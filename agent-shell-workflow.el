;;; agent-shell-workflow.el --- Reusable prompt and agent workflows for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-workflow
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `agent-shell-workflow' registers reusable, parameterized prompt
;; workflows: an optional deterministic pre-operation gathers context
;; (CI logs, diffs, PR comments) before an agent turn; the prompt
;; template renders that context into text; an optional post-operation
;; reacts to turn completion (verification, cleanup, chaining).
;;
;; Workflows dispatch either directly to a live or new `agent-shell'
;; session, or as a queued item in `agent-shell-queue'.  See
;; `register-agent-shell-workflow' to register a workflow and
;; `agent-shell-workflow-dispatch' to run one.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'agent-shell)

(declare-function agent-shell-queue-enqueue-clear "agent-shell-queue")
(declare-function agent-shell-queue--collect-visible-response-text "agent-shell-queue")
(declare-function agent-shell-queue--enqueue-args "agent-shell-queue")
(declare-function agent-shell-queue-register-item-type "agent-shell-queue")
(declare-function agent-shell-queue--agent-shell-buffer-p "agent-shell-queue")
(declare-function agent-shell-queue-item-args "agent-shell-queue")

(defvar agent-shell-workflow-dispatch-target-functions nil
  "Hook run with (SPEC CTX TARGET SUBMIT) to handle custom workflow targets.
Functions are called until one returns non-nil, indicating the dispatch
has been handled.")

;; Data model

(cl-defstruct (agent-shell-workflow-spec
               (:constructor agent-shell-workflow-spec--make)
               (:copier nil))
  "A registered reusable prompt workflow."
  (id nil :documentation "Symbol — unique registry key.")
  (doc nil :documentation "String — one-line description shown in menus.")
  (category nil :documentation "Grouping label shown in menus.")
  (args nil
        :documentation
        "List of (NAME :prompt STRING :type TYPE :optional BOOL) specs.")
  (pre-op nil
          :documentation
          "Function called as (PRE-OP ctx) or (PRE-OP ctx callback) — see
`agent-shell-workflow-exec-pre'.")
  (template nil
            :documentation
            "String with {{key}} placeholders resolved against ctx.")
  (submit nil
          :documentation
          "Non-nil to submit the rendered prompt immediately on insertion.")
  (target nil
          :documentation
          "One of `:session-reuse', `:session-new', `:queue', `:ask'.")
  (post-op nil
           :documentation
           "Function called as (POST-OP shell-buffer ctx response-text)
on turn completion; see `agent-shell-workflow-exec-post'."))

(defvar agent-shell-workflow-registry (make-hash-table :test #'eq)
  "Hash table of symbol id to `agent-shell-workflow-spec'.
Populate via `register-agent-shell-workflow'.")

(defun agent-shell-workflow-get (id)
  "Return the `agent-shell-workflow-spec' registered under ID, or nil."
  (gethash id agent-shell-workflow-registry))

(defun agent-shell-workflow-list ()
  "Return all registered `agent-shell-workflow-spec' values."
  (map-values agent-shell-workflow-registry))

(cl-defun agent-shell-workflow-register (&key id doc category args pre-op template submit target post-op)
  "Register a workflow spec built from ID, DOC, CATEGORY, ARGS, PRE-OP.
TEMPLATE, SUBMIT, TARGET, and POST-OP.  Re-registering an existing ID
replaces the entry."
  (unless id
    (error "Agent-shell-workflow: `:id' is required"))
  (unless template
    (error "Agent-shell-workflow %s: `:template' is required" id))
  (puthash id
           (agent-shell-workflow-spec--make
            :id id :doc doc :category (or category "General") :args args
            :pre-op pre-op :template template :submit submit
            :target (or target :ask) :post-op post-op)
           agent-shell-workflow-registry))

(defmacro register-agent-shell-workflow (id &rest keys)
  "Define and register a prompt workflow named ID.
KEYS is a plist accepting the same keys as `agent-shell-workflow-register'
\(:doc :category :args :pre-op :template :submit :target :post-op).
:args is data (an arg-spec list), not code, and is quoted automatically."
  (declare (indent 1))
  (let ((keys (if (plist-member keys :args)
                  (plist-put (copy-sequence keys) :args (list 'quote (plist-get keys :args)))
                keys)))
    `(agent-shell-workflow-register :id ',id ,@keys)))

;; Argument collection

(defun agent-shell-workflow--arg-key (name)
  "Normalize an arg-spec NAME (bare symbol or keyword) to a plist keyword key."
  (if (keywordp name)
      name
    (intern (format ":%s" name))))

(defun agent-shell-workflow--read-arg (arg-spec)
  "Interactively read one value for ARG-SPEC and return (KEY . VALUE).
ARG-SPEC is (NAME :prompt STRING :type TYPE :optional BOOL); NAME may be
a bare symbol or a keyword — either way KEY is the keyword form used as
the :args plist key."
  (let* ((name (car arg-spec))
         (key (agent-shell-workflow--arg-key name))
         (opts (cdr arg-spec))
         (prompt (or (plist-get opts :prompt) (format "%s: " name)))
         (type (or (plist-get opts :type) 'string))
         (optional (plist-get opts :optional))
         (raw (pcase type
                ('integer (read-number prompt))
                ('symbol (intern (completing-read prompt nil)))
                (_ (read-string prompt)))))
    (when (and (not optional) (equal raw ""))
      (user-error "agent-shell-workflow: %s is required" name))
    (cons key raw)))

(defun agent-shell-workflow--collect-args (spec provided)
  "Return a complete args plist for SPEC.
Reads any missing required keys not found in PROVIDED."
  (seq-reduce
   (lambda (acc arg-spec)
     (let ((key (agent-shell-workflow--arg-key (car arg-spec)))
           (optional (plist-get (cdr arg-spec) :optional)))
       (if (or (plist-member acc key) optional)
           acc
         (let ((pair (agent-shell-workflow--read-arg arg-spec)))
           (plist-put acc (car pair) (cdr pair))))))
   (agent-shell-workflow-spec-args spec)
   (copy-sequence provided)))

;; Template rendering

(defun agent-shell-workflow--stringify (value)
  "Return a display string for VALUE, a template substitution result."
  (cond
   ((stringp value) value)
   ((null value) "")
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun agent-shell-workflow-render (template ctx)
  "Render TEMPLATE, substituting {{key}} placeholders from CTX.
CTX is a plist; {{args.KEY}} looks inside the :args sub-plist, any other
{{key}} looks up the top-level :key entry."
  (replace-regexp-in-string
   "{{[-a-zA-Z0-9.]+}}"
   (lambda (matched)
     ;; `split-string' below uses regexp matching internally, which would
     ;; otherwise clobber the match-data `replace-regexp-in-string' needs
     ;; once this function returns.
     (save-match-data
       (let* ((key (substring matched 2 -2))
              (segments (split-string key "\\."))
              (value (if (and (> (length segments) 1) (string= (car segments) "args"))
                         (plist-get (plist-get ctx :args) (intern (concat ":" (cadr segments))))
                       (plist-get ctx (intern (concat ":" key))))))
         (agent-shell-workflow--stringify value))))
   template t t))

;; Pre/post operation execution

(defun agent-shell-workflow-exec-pre (spec ctx callback)
  "Run SPEC's pre-op against CTX, then call CALLBACK with the updated ctx.
When SPEC has no pre-op, CALLBACK is invoked with CTX unchanged.
A pre-op of arity 1 is treated as synchronous and must return the updated
ctx; a pre-op of arity 2 is treated as asynchronous and must itself call
its callback argument with the updated ctx."
  (let ((pre-op (agent-shell-workflow-spec-pre-op spec)))
    (if (null pre-op)
        (funcall callback ctx)
      (pcase (car (func-arity pre-op))
        (1 (funcall callback (funcall pre-op ctx)))
        (_ (funcall pre-op ctx callback))))))

(defun agent-shell-workflow-exec-post (spec shell-buffer ctx response-text)
  "Run SPEC's post-op with SHELL-BUFFER, CTX, and RESPONSE-TEXT.
Returns the post-op's control-flag result, or `:done' when SPEC has no
post-op.  See `agent-shell-workflow-spec' for the set of recognized flags."
  (if-let* ((post-op (agent-shell-workflow-spec-post-op spec)))
      (funcall post-op shell-buffer ctx response-text)
    :done))

(defun agent-shell-workflow--apply-post-result (result shell-buffer)
  "Act on a post-op RESULT for SHELL-BUFFER.
Handles `:drop-context', `:restart', `:close', and `(:chain ID ARGS)'.
`:done' and any unrecognized value are no-ops."
  (pcase result
    (:drop-context
     (if (fboundp 'agent-shell-queue-enqueue-clear)
         (agent-shell-queue-enqueue-clear shell-buffer)
       (when (fboundp 'agent-shell-clear)
         (agent-shell-clear shell-buffer))))
    (:restart
     (when (buffer-live-p shell-buffer)
       (with-current-buffer shell-buffer
         (agent-shell-interrupt))))
    (:close
     (when (buffer-live-p shell-buffer)
       (kill-buffer shell-buffer)))
    (`(:chain ,next-id ,next-args)
     (agent-shell-workflow-dispatch next-id
                                    :args next-args
                                    :target :session-reuse
                                    :submit t))
    (_ nil)))

;; Dispatch routing

(defun agent-shell-workflow--resolve-target (target)
  "Resolve TARGET keyword `:ask' via `completing-read' into a concrete target."
  (if (eq target :ask)
      (intern (completing-read "Dispatch to: "
                                '(":session-reuse" ":session-new" ":queue")
                                nil t))
    target))

(defun agent-shell-workflow--canonicalize-dir (dir)
  "Return canonical representation of DIR with trailing slash."
  (file-name-as-directory (expand-file-name (or dir default-directory))))

(defun agent-shell-workflow--project-buffers (dir)
  "Return live `agent-shell' buffers whose `default-directory' is under DIR.
Mirrors `agent-shell-menu-project-buffers' rather than calling it directly:
agent-shell-workflow-menu.el (which does depend on agent-shell-menu) wires a
transient entry into agent-shell-menu-dispatch, so this file requiring
agent-shell-menu in turn would be circular."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (string-prefix-p (agent-shell-workflow--canonicalize-dir dir)
                                   (agent-shell-workflow--canonicalize-dir default-directory))))
              (agent-shell-buffers)))

(defun agent-shell-workflow--create-shell (&optional dir)
  "Create and return a new `agent-shell' buffer in DIR."
  (let ((default-directory (or dir default-directory)))
    (agent-shell-new-shell)))

(defun agent-shell-workflow--session-buffer (target)
  "Return a live `agent-shell' buffer for TARGET, creating or prompting if needed.
When TARGET is `:session-new', always create a new shell buffer.
When matching open buffers exist for `default-directory', prompt the user
whether to reuse an existing shell buffer or create a new one.
Otherwise, create a new shell buffer."
  (if (eq target :session-new)
      (agent-shell-workflow--create-shell)
    (let* ((buffers (agent-shell-workflow--project-buffers default-directory))
           (dir-name (file-name-nondirectory (directory-file-name default-directory))))
      (cond
       ((null buffers)
        (agent-shell-workflow--create-shell))
       ((= (length buffers) 1)
        (let ((buf (car buffers)))
          (if (y-or-n-p (format "Reuse open agent-shell %s for %s? "
                                (buffer-name buf) dir-name))
              buf
            (agent-shell-workflow--create-shell))))
       (t
        (let* ((new-option "[New agent-shell]")
               (choices (cons new-option (mapcar #'buffer-name buffers)))
               (choice (completing-read (format "Select agent-shell for %s: " dir-name)
                                        choices nil t)))
          (if (string-equal choice new-option)
              (agent-shell-workflow--create-shell)
            (get-buffer choice))))))))

(defun agent-shell-workflow--dispatch-rendered (spec ctx target submit)
  "Deliver the rendered prompt for SPEC and CTX to TARGET.
When SUBMIT is non-nil, submit the prompt immediately."
  (let* ((target (agent-shell-workflow--resolve-target (or target (agent-shell-workflow-spec-target spec))))
         (submit (if (null submit) (agent-shell-workflow-spec-submit spec) submit)))
    (cond
     ((run-hook-with-args-until-success 'agent-shell-workflow-dispatch-target-functions
                                        spec ctx target submit)
      t)
     ((eq target :queue)
      (user-error "Target `:queue' requested but no queue target handler is registered"))
     (t
      (let* ((shell-buffer (agent-shell-workflow--session-buffer target))
             (insertion (agent-shell-workflow--insert spec ctx shell-buffer submit))
             (resp-start (alist-get :end insertion)))
        (when (agent-shell-workflow-spec-post-op spec)
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'turn-complete
           :callback (lambda ()
                       (let* ((resp (agent-shell-workflow--last-response-text shell-buffer resp-start))
                              (result (agent-shell-workflow-exec-post spec shell-buffer ctx resp)))
                         (agent-shell-workflow--apply-post-result result shell-buffer))))))))))

(defun agent-shell-workflow--insert (spec ctx shell-buffer submit)
  "Render SPEC with CTX and insert it into SHELL-BUFFER.
When SUBMIT is non-nil, submit immediately.  Returns the plist returned by
`agent-shell-insert'."
  (let ((rendered (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) ctx)))
    (agent-shell-insert :text rendered
                        :shell-buffer shell-buffer
                        :submit submit)))

(defun agent-shell-workflow--last-response-text (shell-buffer start-pos)
  "Extract the agent response text in SHELL-BUFFER from START-POS to point-max.
Returns nil when START-POS is nil.
Uses `agent-shell-queue--collect-visible-response-text' when available,
or extracts buffer substring directly."
  (when (and shell-buffer (buffer-live-p shell-buffer) start-pos)
    (if (fboundp 'agent-shell-queue--collect-visible-response-text)
        (agent-shell-queue--collect-visible-response-text shell-buffer start-pos)
      (with-current-buffer shell-buffer
        (buffer-substring-no-properties (max (point-min) start-pos) (point-max))))))

;;;###autoload
(cl-defun agent-shell-workflow-dispatch (id &key args target submit (context-dir default-directory) &allow-other-keys)
  "Instantiate the workflow ID and dispatch it.
ARGS is a plist supplying template values; missing required args
are read interactively.
TARGET overrides the spec's declared target (`:session-reuse',
`:session-new', `:queue', `:ask').
SUBMIT non-nil forces prompt submission regardless of spec default.
CONTEXT-DIR sets `default-directory' for pre-op execution."
  (let* ((spec (or (agent-shell-workflow-get id)
                   (error "Agent-shell-workflow: no workflow registered with id `%s'" id)))
         (dir (or context-dir default-directory))
         (collected-args (agent-shell-workflow--collect-args spec args))
         (initial-ctx (list :args collected-args :target target :submit submit :context-dir dir)))
    (let ((default-directory dir))
      (agent-shell-workflow-exec-pre
       spec initial-ctx
       (lambda (updated-ctx)
         (let* ((rendered (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) updated-ctx))
                (ctx-with-rendered (plist-put updated-ctx :rendered rendered)))
           (agent-shell-workflow--dispatch-rendered spec ctx-with-rendered target submit)))))))

;; Integration with agent-shell-queue

(defun agent-shell-workflow--dispatch-queue-item (item target-buffer)
  "Execute a queued workflow ITEM into TARGET-BUFFER."
  (let* ((plist (read (agent-shell-queue-item-args item)))
         (rendered (plist-get plist :rendered))
         (submit (if (plist-member plist :submit)
                     (plist-get plist :submit)
                   t))
         (buf (if (stringp target-buffer)
                  (get-buffer target-buffer)
                target-buffer)))
    (agent-shell-insert :text rendered
                        :shell-buffer buf
                        :submit submit)))

(defun agent-shell-workflow--queue-target-handler (spec ctx target submit)
  "Target handler for `:queue' target when `agent-shell-queue' is loaded."
  (when (eq target :queue)
    (if (fboundp 'agent-shell-queue--enqueue-args)
        (let ((rendered (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) ctx)))
          (agent-shell-queue--enqueue-args
           (prin1-to-string (list :workflow-id (agent-shell-workflow-spec-id spec)
                                  :rendered rendered
                                  :submit submit))
           'workflow
           nil)
          t)
      (user-error "Target `:queue' requested but `agent-shell-queue' is not available"))))

(defun agent-shell-workflow--setup-queue-integration ()
  "Register `:queue' target handler and item type if queue is available."
  (add-hook 'agent-shell-workflow-dispatch-target-functions
            #'agent-shell-workflow--queue-target-handler)
  (when (fboundp 'agent-shell-queue-register-item-type)
    (agent-shell-queue-register-item-type
     :kind 'workflow
     :label "workflow"
     :buffer-pred #'agent-shell-queue--agent-shell-buffer-p
     :dispatch-fn #'agent-shell-workflow--dispatch-queue-item
     :input-spec '(:kind capture))))

(with-eval-after-load 'agent-shell-queue
  (agent-shell-workflow--setup-queue-integration))

(when (featurep 'agent-shell-queue)
  (agent-shell-workflow--setup-queue-integration))

(provide 'agent-shell-workflow)

;;; agent-shell-workflow.el ends here
