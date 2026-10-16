;;; agent-shell-workflow.el --- Reusable prompt and agent workflows for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-workflow
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1") (transient "0.4") (annotated-completing-read "0.1"))

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
(require 'subr-x)
(require 'agent-shell)

(declare-function agent-shell-queue-enqueue-clear "agent-shell-queue")
(declare-function agent-shell-queue--collect-visible-response-text "agent-shell-queue")
(declare-function agent-shell-queue--enqueue-args "agent-shell-queue")
(declare-function agent-shell-queue-register-item-type "agent-shell-queue")
(declare-function agent-shell-queue--agent-shell-buffer-p "agent-shell-queue")
(declare-function agent-shell-queue-item-args "agent-shell-queue")
(declare-function agent-shell-queue--has-running-item-p "agent-shell-queue")
(declare-function shell-maker-busy "shell-maker")
(declare-function agent-shell--active-requests-p "agent-shell")

(defgroup agent-shell-workflow nil
  "Reusable prompt and agent workflows for agent-shell."
  :group 'tools
  :prefix "agent-shell-workflow-")

(defcustom agent-shell-workflow-clear-commands
  '((omp . "/shake")
    (t . "/clear"))
  "Alist mapping agent identifiers to context-clearing slash commands.
The key `t' provides the fallback command."
  :type '(alist :key-type symbol :value-type string)
  :group 'agent-shell-workflow)

(defcustom agent-shell-workflow-busy-shell-behavior 'queue
  "Action to take when submitting a workflow prompt to a busy shell buffer.

Allowed values:
`queue'  - Add the prompt to `agent-shell-queue' for the target shell buffer.
`ask'    - Prompt the user (y-or-n-p) whether to add the prompt to the queue.
           To avoid hanging after preflight completes, this is asked upfront
           (conditionally if the shell is not yet busy); if declined, fall
           back to `wait'.
`wait'   - Defer prompt insertion and submission until the running turn
           completes.
`insert' - Insert the rendered prompt into the buffer without submitting.
`error'  - Signal a `user-error' that the shell is currently busy."
  :type '(choice (const :tag "Add to queue for the shell" queue)
                 (const :tag "Ask whether to add to queue" ask)
                 (const :tag "Wait for turn to complete" wait)
                 (const :tag "Insert without submitting" insert)
                 (const :tag "Signal an error" error))
  :group 'agent-shell-workflow)

(defvar-local agent-shell-workflow--pending-clear nil
  "Non-nil when a context clearing command is pending in this shell buffer.")

(defun agent-shell-workflow--clear-command-for-buffer (buffer)
  "Resolve the context-clearing command for BUFFER.
Inspects the agent configuration via `agent-shell-get-config' or buffer name.
Matches against `agent-shell-workflow-clear-commands'.
Returns nil if no clear operation is defined for that shell type/agent."
  (let* ((buf (and buffer (get-buffer buffer)))
         (config (and buf (fboundp 'agent-shell-get-config) (agent-shell-get-config buf)))
         (ident (or (and config (map-elt config :identifier))
                    (and buf (string-match-p "\\bomp\\b" (downcase (buffer-name buf))) 'omp)))
         (val agent-shell-workflow-clear-commands)
         (raw (or (and config (or (map-elt config :clear-command)
                                  (map-elt config :clear-operation)))
                  (cond
                   ((functionp val) (funcall val ident))
                   ((listp val)
                    (let ((entry (assq ident val)))
                      (if entry
                          (cdr entry)
                        (cdr (assq t val)))))
                   ((stringp val) val)
                   (t nil)))))
    (if (and (stringp raw) (not (string-empty-p (string-trim raw))))
        (string-trim raw)
      nil)))

(defun agent-shell-workflow--execute-clear (buffer cmd)
  "Execute context-clearing CMD in BUFFER and track its completion."
  (condition-case nil
      (agent-shell-insert :text cmd :shell-buffer buffer :submit t)
    (error nil))
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer (derived-mode-p 'agent-shell-mode))
             (fboundp 'agent-shell-subscribe-to))
    (with-current-buffer buffer
      (setq-local agent-shell-workflow--pending-clear t))
    (let ((sub nil))
      (setq sub
            (condition-case nil
                (agent-shell-subscribe-to
                 :shell-buffer buffer
                 :event 'turn-complete
                 :on-event
                 (lambda (_event)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (setq-local agent-shell-workflow--pending-clear nil)))
                   (when sub
                     (agent-shell-unsubscribe :subscription sub)
                     (setq sub nil))))
              (error
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq-local agent-shell-workflow--pending-clear nil)))
               nil)))))
  t)

(defun agent-shell-workflow--maybe-clear-reused-buffer (buffer &optional spec)
  "Call or prompt context-clearing command in reused BUFFER according to SPEC.
When SPEC is provided and its clear option is nil, does nothing.
When SPEC has a non-nil clear option (such as `t') and BUFFER has a clear
command defined, executes it immediately.
When SPEC clear option is `ask' or SPEC is omitted, prompts the user.
Returns non-nil if a clear command was executed, nil otherwise."
  (let* ((clear-opt (if spec (agent-shell-workflow-spec-clear spec) 'ask))
         (cmd (when clear-opt (agent-shell-workflow--clear-command-for-buffer buffer))))
    (when (and cmd clear-opt)
      (cond
       ((eq clear-opt 'ask)
        (when (y-or-n-p (format "Run %s in %s? " cmd (buffer-name buffer)))
          (agent-shell-workflow--execute-clear buffer cmd)))
       (t
        (agent-shell-workflow--execute-clear buffer cmd))))))

(defvar agent-shell-workflow-dispatch-target-functions nil
  "Hook run with (SPEC CTX TARGET SUBMIT) to handle custom workflow targets.
Functions are called until one returns non-nil, indicating the dispatch
has been handled.")

;; Subprocess execution primitives

(defun agent-shell-workflow--run-process-async (program args dir callback)
  "Run PROGRAM with ARGS in DIR asynchronously.
Calls CALLBACK with the trimmed stdout string on exit.
If execution fails or process exits with non-zero status and empty output,
CALLBACK receives a descriptive error string."
  (let* ((output-buffer (generate-new-buffer " *agent-shell-workflow-proc*"))
         (default-directory (or dir default-directory)))
    (condition-case err
        (make-process
         :name (format "asw-%s" (file-name-nondirectory program))
         :buffer output-buffer
         :command (cons program args)
         :noquery t
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((out (with-current-buffer output-buffer
                          (string-trim (buffer-string))))
                   (status (process-exit-status proc)))
               (when (buffer-live-p output-buffer)
                 (kill-buffer output-buffer))
               (if (and (not (zerop status)) (string-empty-p out))
                   (funcall callback (format "[Command %s exited with status %s]" program status))
                 (funcall callback out))))))
      (error
       (when (buffer-live-p output-buffer)
         (kill-buffer output-buffer))
       (funcall callback (format "[Error executing %s: %s]" program (error-message-string err)))))))

(defun agent-shell-workflow--gather-async (pairs callback-or-dir &optional dir-or-callback)
  "Run shell command PAIRS asynchronously and collect their outputs.
PAIRS is a list of (KEY PROGRAM ARG...).
Calls CALLBACK with an alist of (KEY . OUTPUT) when all commands complete.
CALLBACK-OR-DIR and DIR-OR-CALLBACK may be passed in either order:
(PAIRS CALLBACK &optional DIR) or (PAIRS DIR CALLBACK)."
  (let* ((callback (if (functionp callback-or-dir) callback-or-dir dir-or-callback))
         (dir (if (functionp callback-or-dir) dir-or-callback callback-or-dir))
         (total (length pairs))
         (remaining total)
         (results (make-hash-table :test #'equal)))
    (if (zerop total)
        (when callback (funcall callback nil))
      (dolist (pair pairs)
        (let ((key (car pair))
              (program (cadr pair))
              (args (cddr pair)))
          (agent-shell-workflow--run-process-async
           program args dir
           (lambda (out)
             (puthash key out results)
             (setq remaining (1- remaining))
             (when (zerop remaining)
               (let ((alist (mapcar (lambda (p) (cons (car p) (gethash (car p) results)))
                                    pairs)))
                 (when callback (funcall callback alist)))))))))))

;; Data model

(cl-defstruct (agent-shell-workflow-spec
               (:constructor agent-shell-workflow-spec--make)
               (:copier nil))
  "A registered reusable prompt workflow."
  (id
   nil
   :documentation "Symbol — unique registry key.")
  (doc
   nil
   :documentation "String — one-line description shown in menus.")
  (category
   nil
   :documentation "Grouping label shown in menus.")
  (args
   nil
   :documentation "List of (NAME :prompt STRING :type TYPE :optional BOOL) specs.")
  (pre-op
   nil
   :documentation
   "Function called as (PRE-OP ctx) or (PRE-OP ctx callback).
See `agent-shell-workflow-exec-pre'.")
  (template
   nil
   :documentation "String with {{key}} placeholders resolved against ctx.")
  (submit
   nil
   :documentation "Non-nil to submit the rendered prompt immediately on insertion.")
  (target
   nil
   :documentation "One of `:session-reuse', `:session-new', `:queue', `:ask'.")
  (post-op
   nil
   :documentation
   "Function called as (POST-OP shell-buffer ctx response-text).
Called on turn completion; see `agent-shell-workflow-exec-post'.")
  (clear
   nil
   :documentation
   "Non-nil to execute context-clearing slash command before dispatching prompt.
Requires a clear operation defined for the target shell type/agent."))

(defvar agent-shell-workflow-registry (make-hash-table :test #'eq)
  "Hash table of symbol id to `agent-shell-workflow-spec'.
Populate via `register-agent-shell-workflow'.")

(defun agent-shell-workflow-get (id)
  "Return the `agent-shell-workflow-spec' registered under ID, or nil."
  (or (gethash id agent-shell-workflow-registry)
      (progn
        (require 'agent-shell-workflow-library nil t)
        (gethash id agent-shell-workflow-registry))))

(defun agent-shell-workflow-list ()
  "Return all registered `agent-shell-workflow-spec' values."
  (require 'agent-shell-workflow-library nil t)
  (map-values agent-shell-workflow-registry))

(cl-defun agent-shell-workflow-register (&key id doc category args pre-op template submit target post-op clear clear-before)
  "Register a workflow spec built from ID, DOC, CATEGORY, ARGS, PRE-OP,
TEMPLATE, SUBMIT, TARGET, POST-OP, and CLEAR (or CLEAR-BEFORE).
Re-registering an existing ID replaces the entry."
  (unless id
    (error "Agent-shell-workflow: `:id' is required"))
  (unless template
    (error "Agent-shell-workflow %s: `:template' is required" id))
  (puthash id
           (agent-shell-workflow-spec--make
            :id id :doc doc :category (or category "General") :args args
            :pre-op pre-op :template template :submit submit
            :target (or target :ask) :post-op post-op
            :clear (or clear clear-before))
           agent-shell-workflow-registry))

(defmacro register-agent-shell-workflow (id &rest keys)
  "Define and register a prompt workflow named ID.
KEYS is a plist accepting the same keys as `agent-shell-workflow-register'
\(:doc :category :args :pre-op :template :submit :target :post-op :clear :clear-before).
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
      (let ((arity (func-arity pre-op)))
        (if (or (> (car arity) 1)
                (and (= (car arity) 1) (or (eq (cdr arity) 'many) (>= (cdr arity) 2))))
            (funcall pre-op ctx callback)
          (funcall callback (funcall pre-op ctx)))))))

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
  "Return live `agent-shell' buffers whose project root matches DIR.
Matches buffers where DIR is within the buffer's `default-directory'
or the buffer's `default-directory' is within DIR.
Mirrors `agent-shell-menu-project-buffers' rather than calling it directly:
agent-shell-workflow-menu.el (which does depend on agent-shell-menu) wires a
transient entry into agent-shell-menu-dispatch, so this file requiring
agent-shell-menu in turn would be circular."
  (let ((canon-dir (agent-shell-workflow--canonicalize-dir dir)))
    (seq-filter (lambda (buf)
                  (with-current-buffer buf
                    (let ((buf-dir (agent-shell-workflow--canonicalize-dir default-directory)))
                      (or (equal buf-dir canon-dir)
                          (string-prefix-p buf-dir canon-dir)
                          (string-prefix-p canon-dir buf-dir)))))
                (agent-shell-buffers))))

(defun agent-shell-workflow--create-shell (&optional dir)
  "Create and return a new `agent-shell' buffer in DIR."
  (let* ((canon-dir (agent-shell-workflow--canonicalize-dir (or dir default-directory)))
         (before-bufs (agent-shell-buffers)))
    (let ((default-directory canon-dir))
      (let ((res (agent-shell-new-shell)))
        (cond
         ((and (bufferp res) (buffer-live-p res))
          res)
         ((seq-find (lambda (b) (not (memq b before-bufs)))
                    (agent-shell-buffers)))
         ((seq-first (agent-shell-workflow--project-buffers canon-dir)))
         ((when (derived-mode-p 'agent-shell-mode)
            (current-buffer)))
         ((and res (not (numberp res)))
          res))))))

(defun agent-shell-workflow--session-buffer (target &optional dir spec)
  "Return a live `agent-shell' buffer for TARGET in DIR, creating or prompting.
When TARGET is `:session-new', always create a new shell buffer.
When matching open buffers exist for DIR (default `default-directory'), prompt
the user whether to reuse an existing shell buffer or create a new one.
Otherwise, create a new shell buffer."
  (let ((effective-dir (or dir default-directory)))
    (if (eq target :session-new)
        (agent-shell-workflow--create-shell effective-dir)
      (let* ((buffers (agent-shell-workflow--project-buffers effective-dir))
             (dir-name (file-name-nondirectory (directory-file-name (expand-file-name effective-dir))))
             (reused-buf nil)
             (res-buf nil))
        (cond
         ((null buffers)
          (setq res-buf (agent-shell-workflow--create-shell effective-dir)))
         ((= (length buffers) 1)
          (let ((buf (car buffers)))
            (if (y-or-n-p (format "Reuse open agent-shell %s for %s? "
                                  (buffer-name buf) dir-name))
                (setq reused-buf buf
                      res-buf buf)
              (setq res-buf (agent-shell-workflow--create-shell effective-dir)))))
         (t
          (let* ((new-option "[New agent-shell]")
                 (choices (cons new-option (mapcar #'buffer-name buffers)))
                 (choice (completing-read (format "Select agent-shell for %s: " dir-name)
                                          choices nil t)))
            (if (string-equal choice new-option)
                (setq res-buf (agent-shell-workflow--create-shell effective-dir))
              (setq reused-buf (get-buffer choice)
                    res-buf reused-buf)))))
        (when (and reused-buf (buffer-live-p reused-buf))
          (agent-shell-workflow--maybe-clear-reused-buffer reused-buf spec))
        res-buf))))

(defun agent-shell-workflow--dispatch-rendered (spec ctx target submit &optional dir)
  "Deliver the rendered prompt for SPEC and CTX to TARGET in DIR.
When SUBMIT is non-nil, submit the prompt immediately."
  (let* ((target (agent-shell-workflow--resolve-target (or target (agent-shell-workflow-spec-target spec))))
         (submit (if (null submit) (agent-shell-workflow-spec-submit spec) submit))
         (effective-dir (or dir (plist-get ctx :context-dir) default-directory)))
    (cond
     ((run-hook-with-args-until-success 'agent-shell-workflow-dispatch-target-functions
                                        spec ctx target submit)
      t)
     ((eq target :queue)
      (user-error "Target `:queue' requested but no queue target handler is registered"))
     (t
      (let* ((shell-buffer (or (plist-get ctx :shell-buffer)
                               (agent-shell-workflow--session-buffer target effective-dir spec))))
        (agent-shell-workflow--dispatch-to-buffer spec ctx shell-buffer submit))))))

(defun agent-shell-workflow--buffer-busy-p (buffer)
  "Return non-nil if BUFFER is currently busy executing a turn or command."
  (let ((buf (cond
              ((bufferp buffer) buffer)
              ((stringp buffer) (get-buffer buffer))
              (t nil))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (or (and (boundp 'agent-shell-workflow--pending-clear)
                 agent-shell-workflow--pending-clear)
            (and (boundp 'shell-maker--busy)
                 shell-maker--busy)
            (and (fboundp 'shell-maker-busy)
                 (ignore-errors (shell-maker-busy)))
            (and (boundp 'agent-shell--busy)
                 agent-shell--busy)
            (and (boundp 'agent-shell--state)
                 (fboundp 'agent-shell--active-requests-p)
                 (ignore-errors (agent-shell--active-requests-p agent-shell--state)))
            (and (fboundp 'agent-shell-queue--has-running-item-p)
                 (ignore-errors (agent-shell-queue--has-running-item-p (buffer-name buf)))))))))

(defun agent-shell-workflow--resolve-busy-action-upfront (shell-buffer will-submit)
  "Determine the action to take if SHELL-BUFFER is busy, asked upfront.
Consults `agent-shell-workflow-busy-shell-behavior'.  When set to `ask',
prompts the user with `y-or-n-p' synchronously before preflight starts.
If SHELL-BUFFER is already busy, asks if prompt should be queued when
preflight completes.  If SHELL-BUFFER is not currently busy, asks
conditionally if prompt should be queued if the shell becomes busy.
Returns one of `queue', `wait', `insert', or `error'."
  (when (and will-submit shell-buffer)
    (let* ((buf (cond
                 ((bufferp shell-buffer) shell-buffer)
                 ((stringp shell-buffer) (get-buffer shell-buffer))
                 (t nil)))
           (buf-name (cond
                      ((bufferp buf) (buffer-name buf))
                      ((stringp shell-buffer) shell-buffer)
                      (t (format "%s" shell-buffer))))
           (queue-avail (fboundp 'agent-shell-queue--enqueue-args)))
      (pcase agent-shell-workflow-busy-shell-behavior
        ('ask
         (if queue-avail
             (let* ((busy-now (agent-shell-workflow--buffer-busy-p shell-buffer))
                    (prompt-text
                     (if busy-now
                         (format "Agent shell `%s' is currently busy. Add prompt to queue when preflight completes? "
                                 buf-name)
                       (format "If agent shell `%s' is busy when preflight completes, add prompt to queue? "
                               buf-name))))
               (if (y-or-n-p prompt-text)
                   'queue
                 'wait))
           'wait))
        ('queue
         (if queue-avail
             'queue
           (message "agent-shell-queue not available; falling back to waiting for turn complete")
           'wait))
        (other other)))))

(defun agent-shell-workflow--resolve-busy-action (shell-buffer)
  "Determine the action to take when submitting to busy SHELL-BUFFER.
Consults `agent-shell-workflow-busy-shell-behavior'.  If set to `ask',
prompts the user with `y-or-n-p'.  Returns one of `queue', `wait',
`insert', or `error'."
  (let* ((buf (cond
               ((bufferp shell-buffer) shell-buffer)
               ((stringp shell-buffer) (get-buffer shell-buffer))
               (t nil)))
         (buf-name (cond
                    ((bufferp buf) (buffer-name buf))
                    ((stringp shell-buffer) shell-buffer)
                    (t (format "%s" shell-buffer))))
         (queue-avail (fboundp 'agent-shell-queue--enqueue-args)))
    (pcase agent-shell-workflow-busy-shell-behavior
      ('ask
       (if (and queue-avail
                (y-or-n-p (format "Agent shell `%s' is currently busy. Add prompt to queue? "
                                  buf-name)))
           'queue
         'wait))
      ('queue
       (if queue-avail
           'queue
         (message "agent-shell-queue not available; falling back to waiting for turn complete")
         'wait))
      (other other))))

(defun agent-shell-workflow--enqueue-for-buffer (spec ctx shell-buffer submit)
  "Add prompt for SPEC, CTX, and SUBMIT to `agent-shell-queue' for SHELL-BUFFER.
Signals a `user-error' if `agent-shell-queue' is not available."
  (if (fboundp 'agent-shell-queue--enqueue-args)
      (let* ((buf (cond
                   ((bufferp shell-buffer) shell-buffer)
                   ((stringp shell-buffer) (get-buffer shell-buffer))
                   (t shell-buffer)))
             (buf-name (cond
                        ((bufferp buf) (buffer-name buf))
                        ((stringp shell-buffer) shell-buffer)
                        (t (format "%s" shell-buffer))))
             (rendered (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) ctx)))
        (agent-shell-queue--enqueue-args
         (prin1-to-string (list :workflow-id (agent-shell-workflow-spec-id spec)
                                :rendered rendered
                                :ctx ctx
                                :submit submit))
         'workflow
         buf)
        (message "Added workflow `%s' to queue for %s"
                 (agent-shell-workflow-spec-id spec)
                 buf-name)
        t)
    (user-error "Cannot enqueue prompt: `agent-shell-queue' is not available")))

(defun agent-shell-workflow--defer-until-turn-complete (shell-buffer callback)
  "Defer CALLBACK until turn-complete fires on SHELL-BUFFER.
Clears `agent-shell-workflow--pending-clear' if set."
  (if (and (buffer-live-p shell-buffer)
           (with-current-buffer shell-buffer (derived-mode-p 'agent-shell-mode))
           (fboundp 'agent-shell-subscribe-to))
      (let ((sub nil))
        (setq sub
              (condition-case nil
                  (agent-shell-subscribe-to
                   :shell-buffer shell-buffer
                   :event 'turn-complete
                   :on-event
                   (lambda (_ev)
                     (when sub
                       (agent-shell-unsubscribe :subscription sub)
                       (setq sub nil))
                     (when (buffer-live-p shell-buffer)
                       (with-current-buffer shell-buffer
                         (setq-local agent-shell-workflow--pending-clear nil)))
                     (funcall callback)))
                (error
                 (funcall callback)))))
    (funcall callback)))

(defun agent-shell-workflow--insert-and-post (spec ctx shell-buffer submit)
  "Insert rendered SPEC into SHELL-BUFFER and subscribe to post-op if present."
  (let* ((insertion (agent-shell-workflow--insert spec ctx shell-buffer submit))
         (resp-start (alist-get :end insertion)))
    (when (agent-shell-workflow-spec-post-op spec)
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer
       :event 'turn-complete
       :on-event (lambda (_event)
                   (let* ((start (or resp-start
                                     (with-current-buffer shell-buffer
                                       (when (boundp 'comint-last-input-end)
                                         comint-last-input-end))))
                          (resp (agent-shell-workflow--last-response-text shell-buffer start))
                          (result (agent-shell-workflow-exec-post spec shell-buffer ctx resp)))
                     (agent-shell-workflow--apply-post-result result shell-buffer)))))))

(defun agent-shell-workflow--dispatch-to-buffer (spec ctx shell-buffer submit)
  "Insert and optionally submit prompt in SHELL-BUFFER for SPEC and CTX.
If SUBMIT is non-nil and SHELL-BUFFER is busy, handles the busy state
according to `agent-shell-workflow-busy-shell-behavior' or pre-resolved
`:busy-action' in CTX.
If a context clear is currently pending in SHELL-BUFFER, defers insertion
until turn-complete."
  (let ((do-insert
         (lambda ()
           (agent-shell-workflow--insert-and-post spec ctx shell-buffer submit))))
    (cond
     ((and submit (agent-shell-workflow--buffer-busy-p shell-buffer))
      (pcase (or (plist-get ctx :busy-action)
                 (agent-shell-workflow--resolve-busy-action shell-buffer))
        ('queue
         (agent-shell-workflow--enqueue-for-buffer spec ctx shell-buffer submit))
        ('wait
         (agent-shell-workflow--defer-until-turn-complete shell-buffer do-insert))
        ('insert
         (agent-shell-workflow--insert spec ctx shell-buffer nil))
        ('error
         (let ((name (cond
                      ((bufferp shell-buffer) (buffer-name shell-buffer))
                      ((stringp shell-buffer) shell-buffer)
                      (t (format "%s" shell-buffer)))))
           (user-error "Agent shell `%s' is currently busy" name)))
        (_
         (agent-shell-workflow--enqueue-for-buffer spec ctx shell-buffer submit))))
     ((and (buffer-live-p shell-buffer)
           (buffer-local-value 'agent-shell-workflow--pending-clear shell-buffer)
           (with-current-buffer shell-buffer (derived-mode-p 'agent-shell-mode))
           (fboundp 'agent-shell-subscribe-to))
      (agent-shell-workflow--defer-until-turn-complete shell-buffer do-insert))
     (t
      (funcall do-insert)))))

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
  (let ((start (or start-pos
                   (and (buffer-live-p shell-buffer)
                        (with-current-buffer shell-buffer
                          (when (boundp 'comint-last-input-end)
                            comint-last-input-end))))))
    (when (and shell-buffer (buffer-live-p shell-buffer) start)
      (if (fboundp 'agent-shell-queue--collect-visible-response-text)
          (agent-shell-queue--collect-visible-response-text shell-buffer start)
        (with-current-buffer shell-buffer
          (buffer-substring-no-properties (max (point-min) start) (point-max)))))))

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
         (resolved-target (agent-shell-workflow--resolve-target
                           (or target (agent-shell-workflow-spec-target spec))))
         (effective-submit (if (null submit)
                               (agent-shell-workflow-spec-submit spec)
                             submit))
         (shell-buffer (cond
                        ((memq resolved-target '(:session-reuse :session-new))
                         (agent-shell-workflow--session-buffer resolved-target dir spec))
                        ((bufferp resolved-target)
                         (when (and (buffer-live-p resolved-target) spec (agent-shell-workflow-spec-clear spec))
                           (agent-shell-workflow--maybe-clear-reused-buffer resolved-target spec))
                         resolved-target)
                        ((and (stringp resolved-target) (get-buffer resolved-target))
                         (let ((b (get-buffer resolved-target)))
                           (when (and (buffer-live-p b) spec (agent-shell-workflow-spec-clear spec))
                             (agent-shell-workflow--maybe-clear-reused-buffer b spec))
                           b))
                        (t nil)))
         (busy-action (when (and effective-submit shell-buffer)
                        (agent-shell-workflow--resolve-busy-action-upfront shell-buffer effective-submit)))
         (initial-ctx (list :args collected-args
                            :target resolved-target
                            :shell-buffer shell-buffer
                            :submit effective-submit
                            :busy-action busy-action
                            :context-dir dir)))
    (let ((default-directory dir))
      (agent-shell-workflow-exec-pre
       spec initial-ctx
       (lambda (updated-ctx)
         (let* ((merged-ctx (let ((c (copy-sequence initial-ctx))
                                  (rem updated-ctx))
                              (while rem
                                (setq c (plist-put c (car rem) (cadr rem)))
                                (setq rem (cddr rem)))
                              c))
                (effective-dir (or (plist-get merged-ctx :context-dir) dir))
                (default-directory effective-dir)
                (rendered (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) merged-ctx))
                (ctx-with-rendered (plist-put merged-ctx :rendered rendered)))
           (agent-shell-workflow--dispatch-rendered spec ctx-with-rendered resolved-target effective-submit effective-dir)))))))

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
                target-buffer))
         (workflow-id (plist-get plist :workflow-id))
         (spec (when workflow-id (agent-shell-workflow-get workflow-id)))
         (ctx (plist-get plist :ctx)))
    (if spec
        (agent-shell-workflow--insert-and-post spec ctx buf submit)
      (agent-shell-insert :text rendered
                          :shell-buffer buf
                          :submit submit))))

(defun agent-shell-workflow--queue-target-handler (spec ctx target submit)
  "Target handler for `:queue' target when `agent-shell-queue' is loaded."
  (when (eq target :queue)
    (let ((shell-buf (plist-get ctx :shell-buffer)))
      (agent-shell-workflow--enqueue-for-buffer spec ctx shell-buf submit))))

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

(require 'agent-shell-workflow-library)
(require 'agent-shell-workflow-menu)

;;; agent-shell-workflow.el ends here
