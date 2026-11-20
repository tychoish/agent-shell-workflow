;;; agent-shell-workflow.el --- Reusable prompt and agent workflows for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-workflow
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1") (annotated-completing-read "0.1") (magit "3.3"))

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

(defvar agent-shell-workflow-test-mode nil
  "When non-nil, force interactive agent config selection paths in batch tests.")

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell)
(require 'annotated-completing-read)
(require 'magit-process)

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

(defcustom agent-shell-workflow-debounce-interval 2.0
  "Interval in seconds between dispatching the same workflow in a directory.
Prevents accidental double submissions from repeated clicks or keypresses.
When set to 0 or nil, debouncing is disabled."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'agent-shell-workflow)

(defcustom agent-shell-workflow-repo-groups nil
  "Alist mapping repository group names (symbols) to lists of directory paths.
Each entry is of the form (GROUP-NAME . (DIR1 DIR2 ...)).
Directories are expanded to absolute paths upon registration and resolution."
  :type '(alist :key-type symbol :value-type (repeat directory))
  :group 'agent-shell-workflow)

(defvar agent-shell-workflow--in-flight-dispatches (make-hash-table :test 'equal)
  "Hash table tracking active or recently dispatched workflows.
Keys are composite identifiers `(id dir)', values are dispatch timestamps.")

(defvar-local agent-shell-workflow--pending-clear nil
  "Non-nil when a context clearing command is pending in this shell buffer.")

(defvar-local agent-shell-workflow--new-shell nil
  "Non-nil if this shell buffer was newly created and has not received input.")

(defun agent-shell-workflow--fresh-shell-p (buffer)
  "Return non-nil if BUFFER is a new or fresh agent-shell buffer.
A shell buffer is fresh if it has been marked as new by
`agent-shell-workflow--create-shell', or if it is an `agent-shell-mode'
buffer that has not yet executed any turns or user inputs."
  (when-let* ((buf (if (bufferp buffer) buffer (and (stringp buffer) (get-buffer buffer))))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (cond
       (agent-shell-workflow--new-shell t)
       ((not (derived-mode-p 'agent-shell-mode)) nil)
       ((and (bound-and-true-p shell-maker--current-request-id)
             (not (zerop shell-maker--current-request-id)))
        nil)
       ((ignore-errors
          (when-let* ((state (bound-and-true-p agent-shell--state)))
            (map-nested-elt state '(:session :title))))
        nil)
       ((ignore-errors
          (when-let* ((state (bound-and-true-p agent-shell--state))
                      (usage (map-elt state :usage)))
            (if (fboundp 'agent-shell--usage-has-data-p)
                (agent-shell--usage-has-data-p usage)
              (or (> (or (alist-get :total-tokens usage) 0) 0)
                  (> (or (alist-get :context-size usage) 0) 0)))))
        nil)
       ((and (bound-and-true-p comint-last-input-start)
             (bound-and-true-p comint-last-input-end)
             (< (marker-position comint-last-input-start)
                (marker-position comint-last-input-end)))
        nil)
       (t t)))))

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
    (let ((sub nil)
          (timer nil))
      (let ((clear-fn
             (lambda ()
               (when timer
                 (cancel-timer timer)
                 (setq timer nil))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq-local agent-shell-workflow--pending-clear nil)))
               (when sub
                 (agent-shell-unsubscribe :subscription sub)
                 (setq sub nil)))))
        (setq sub
              (condition-case nil
                  (agent-shell-subscribe-to
                   :shell-buffer buffer
                   :event 'turn-complete
                   :on-event (lambda (_event) (funcall clear-fn)))
                (error
                 (funcall clear-fn)
                 nil)))
        (when sub
          (setq timer (run-with-timer 3.0 nil clear-fn))))))
  t)

(defun agent-shell-workflow--maybe-clear-reused-buffer (buffer &optional spec)
  "Call or prompt context-clearing command in reused BUFFER according to SPEC.
When BUFFER is a new or fresh shell buffer, does nothing.
When SPEC is provided and its clear option is nil, does nothing.
When SPEC has a non-nil clear option (such as `t') and BUFFER has a clear
command defined, executes it immediately.
When SPEC clear option is `ask' or SPEC is omitted, prompts the user.
Returns non-nil if a clear command was executed, nil otherwise."
  (when-let* ((buf (if (bufferp buffer) buffer (and (stringp buffer) (get-buffer buffer))))
              ((buffer-live-p buf))
              ((not (agent-shell-workflow--fresh-shell-p buf)))
              (clear-opt (if spec (agent-shell-workflow-spec-clear spec) 'ask))
              (cmd (agent-shell-workflow--clear-command-for-buffer buf)))
    (when (or (not (eq clear-opt 'ask))
              (y-or-n-p (format "Run %s in %s? " cmd (buffer-name buf))))
      (agent-shell-workflow--execute-clear buf cmd))))

(defvar agent-shell-workflow-dispatch-target-functions nil
  "Hook run with (SPEC CTX TARGET SUBMIT) to handle custom workflow targets.
Functions are called until one returns non-nil, indicating the dispatch
has been handled.")

;; Subprocess execution primitives

(defun agent-shell-workflow--run-git-async (args dir callback)
  "Run Git with ARGS in DIR using Magit, then call CALLBACK with its output."
  (let ((default-directory (or dir default-directory))
        (magit-pre-start-git-hook nil)
        (magit-process-connection-type nil)
        (magit-process-popup-time -1)
        (magit-inhibit-refresh t)
        (output ""))
    (condition-case err
        (let* ((proc (apply #'magit-start-git nil args))
               (filter (process-filter proc))
               (sentinel (process-sentinel proc)))
          (set-process-filter
           proc (lambda (process chunk)
                  (setq output (concat output chunk))
                  (when filter (funcall filter process chunk))))
          (set-process-sentinel
           proc (lambda (process event)
                  (when (memq (process-status process) '(exit signal))
                    (when sentinel
                      (condition-case err
                          (funcall sentinel process event)
                        (error (message "Magit process sentinel failed: %s"
                                        (error-message-string err)))))
                    (let ((out (string-trim output))
                          (status (process-exit-status process)))
                      (funcall callback
                               (if (and (not (zerop status)) (string-empty-p out))
                                   (format "[Command git exited with status %s]" status)
                                 out)))))))
      (error (funcall callback (format "[Error executing git: %s]"
                                       (error-message-string err)))))))

(defun agent-shell-workflow--run-process-async (program args dir callback)
  "Run PROGRAM with ARGS in DIR asynchronously.
Calls CALLBACK with the trimmed stdout string on exit.
If execution fails or process exits with non-zero status and empty output,
CALLBACK receives a descriptive error string."
  (if (equal program "git")
      (agent-shell-workflow--run-git-async args dir callback)
    (let* ((output-buffer (generate-new-buffer " *agent-shell-workflow-proc*"))
         (default-directory (or dir default-directory)))
    (condition-case err
        (make-process
         :name (format "asw-%s" (file-name-nondirectory program))
         :buffer output-buffer
         :command (cons program args)
         :noquery t
         :connection-type 'pipe
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
       (funcall callback (format "[Error executing %s: %s]" program (error-message-string err))))))))

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
   :documentation
   "One of `:session-reuse', `:session-reuse-always', `:session-new',
`:queue', `:ask'.")
  (shell-reuse
   nil
   :documentation
   "Policy for reusing existing shells.
One of \\='always (reuse without prompt, spawn new if none),
\\='prompt (prompt user if shells exist), \\='new (always spawn new),
or nil.")
  (new-shell-providers
   nil
   :documentation
   "List of provider specs to use when creating a new shell.
Each element can be a provider ID symbol/string, a config function,
or a list/plist specifying overrides,
e.g. \\='((antigravity :model \"gemini-2.5-pro\")).")
  (buffer-display
   nil
   :documentation
   "How to display the shell buffer.
One of `:foreground' (display and focus via `pop-to-buffer'),
`:background' (do not display window or focus),
`:display' (display via `display-buffer'), or nil (defaults to `:display').")
  (post-op
   nil
   :documentation
   "Function called as (POST-OP shell-buffer ctx response-text).
Called on turn completion; see `agent-shell-workflow-exec-post'.")
  (clear
   nil
   :documentation
   "Non-nil to execute context-clearing slash command before dispatching prompt.
Requires a clear operation defined for the target shell type/agent.")
  (variants
   nil
   :documentation "List of variant definitions for this workflow."))

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

(defun agent-shell-workflow--normalize-target (target)
  "Normalize TARGET (keyword or quoted symbol) to canonical target symbol."
  (cond
   ((or (null target) (bufferp target) (stringp target))
    target)
   ((symbolp target)
    (let ((name (string-remove-prefix ":" (symbol-name target))))
      (cond
       ((equal name "ask") :ask)
       ((equal name "session-reuse") :session-reuse)
       ((member name '("session-reuse-always" "always-reuse" "session-reuse-or-new" "reuse-always"))
        :session-reuse-always)
       ((member name '("session-new" "new")) :session-new)
       ((equal name "queue") :queue)
       ((keywordp target) target)
       (t (intern (format ":%s" name))))))
   (t target)))

(defun agent-shell-workflow--normalize-display (display)
  "Normalize DISPLAY to canonical display keyword.
DISPLAY can be a keyword, quoted symbol, or string."
  (if (null display)
      :display
    (let ((sym (if (symbolp display)
                   (intern (format ":%s" (string-remove-prefix ":" (symbol-name display))))
                 (intern (format ":%s" (string-remove-prefix ":" (format "%s" display)))))))
      (pcase sym
        (:background :background)
        (:foreground :foreground)
        (_ :display)))))

(defun agent-shell-workflow--normalize-shell-reuse (policy)
  "Normalize shell reuse POLICY to \\='always, \\='prompt, \\='new, or nil.
POLICY can be a keyword or symbol."
  (when policy
    (let ((name (string-remove-prefix ":" (format "%s" policy))))
      (cond
       ((member name '("always" "t")) 'always)
       ((equal name "new") 'new)
       ((equal name "prompt") 'prompt)
       (t (intern name))))))

(cl-defun agent-shell-workflow-register (&key id doc category args pre-op template submit target post-op clear clear-before variants shell-reuse new-shell-providers providers buffer-display display)
  "Register a workflow spec built from ID, DOC, CATEGORY, ARGS, PRE-OP,
TEMPLATE, SUBMIT, TARGET, POST-OP, CLEAR (or CLEAR-BEFORE), and VARIANTS.
SHELL-REUSE specifies reuse behavior (\\='always, \\='prompt, \\='new).
NEW-SHELL-PROVIDERS (or PROVIDERS) specifies preferred/allowed shell providers.
BUFFER-DISPLAY (or DISPLAY) specifies `:foreground', `:background',
or `:display'.
Re-registering an existing ID replaces the entry."
  (unless id
    (error "Agent-shell-workflow: `:id' is required"))
  (unless template
    (error "Agent-shell-workflow %s: `:template' is required" id))
  (let* ((effective-providers (or new-shell-providers providers))
         (effective-display (agent-shell-workflow--normalize-display (or buffer-display display)))
         (effective-target (agent-shell-workflow--normalize-target (or target :ask)))
         (effective-shell-reuse (agent-shell-workflow--normalize-shell-reuse shell-reuse))
         (spec (agent-shell-workflow-spec--make
                :id id :doc doc :category (or category "General") :args args
                :pre-op pre-op :template template :submit submit
                :target effective-target :post-op post-op
                :clear (or clear clear-before)
                :variants variants
                :shell-reuse effective-shell-reuse
                :new-shell-providers effective-providers
                :buffer-display effective-display)))
    (setf (map-elt agent-shell-workflow-registry id) spec)
    (when variants
      (dolist (var-spec variants)
        (let* ((var-id (or (plist-get (cdr var-spec) :id)
                           (intern (format "%s-%s" id (car var-spec)))))
               (var-doc (or (plist-get (cdr var-spec) :doc)
                            (format "%s (%s)" (or doc (symbol-name id)) (car var-spec))))
               (var-tmpl (or (plist-get (cdr var-spec) :template)
                             template))
               (replaces (plist-get (cdr var-spec) :template-replace)))
          (when replaces
            (dolist (pair (if (consp (car-safe replaces)) replaces (list replaces)))
              (setq var-tmpl (replace-regexp-in-string
                              (regexp-quote (car pair))
                              (cdr pair)
                              var-tmpl t t))))
          (agent-shell-workflow-register
           :id var-id
           :doc var-doc
           :category (or (plist-get (cdr var-spec) :category) category)
           :args (or (plist-get (cdr var-spec) :args) args)
           :pre-op (or (plist-get (cdr var-spec) :pre-op) pre-op)
           :template var-tmpl
           :submit (if (plist-member (cdr var-spec) :submit)
                       (plist-get (cdr var-spec) :submit)
                     submit)
           :target (agent-shell-workflow--normalize-target (or (plist-get (cdr var-spec) :target) target))
           :post-op (or (plist-get (cdr var-spec) :post-op) post-op)
           :clear (if (plist-member (cdr var-spec) :clear)
                      (plist-get (cdr var-spec) :clear)
                    (or clear clear-before))
           :shell-reuse (agent-shell-workflow--normalize-shell-reuse
                         (or (plist-get (cdr var-spec) :shell-reuse) shell-reuse))
           :new-shell-providers (or (plist-get (cdr var-spec) :new-shell-providers)
                                    (plist-get (cdr var-spec) :providers)
                                    effective-providers)
           :buffer-display (agent-shell-workflow--normalize-display
                            (or (plist-get (cdr var-spec) :buffer-display)
                                (plist-get (cdr var-spec) :display)
                                effective-display))))))
    spec))

(defmacro register-agent-shell-workflow (id &rest keys)
  "Define and register a prompt workflow named ID.
KEYS is a plist accepting the same keys as `agent-shell-workflow-register'
\(:doc :category :args :pre-op :template :submit :target :post-op
 :clear :clear-before :variants :shell-reuse :new-shell-providers
 :providers :buffer-display :display).
:args, :variants, :target, :shell-reuse, :buffer-display, and
:new-shell-providers accept quoted or bare symbols and are quoted
automatically."
  (declare (indent 1))
  (let ((keys (copy-sequence keys)))
    (when (plist-member keys :args)
      (setq keys (plist-put keys :args (list 'quote (plist-get keys :args)))))
    (when (plist-member keys :variants)
      (setq keys (plist-put keys :variants (list 'quote (plist-get keys :variants)))))
    (when (and (plist-member keys :target)
               (symbolp (plist-get keys :target))
               (not (keywordp (plist-get keys :target))))
      (setq keys (plist-put keys :target (list 'quote (plist-get keys :target)))))
    (when (and (plist-member keys :buffer-display)
               (symbolp (plist-get keys :buffer-display))
               (not (keywordp (plist-get keys :buffer-display))))
      (setq keys (plist-put keys :buffer-display (list 'quote (plist-get keys :buffer-display)))))
    (when (and (plist-member keys :display)
               (symbolp (plist-get keys :display))
               (not (keywordp (plist-get keys :display))))
      (setq keys (plist-put keys :display (list 'quote (plist-get keys :display)))))
    (when (and (plist-member keys :shell-reuse)
               (symbolp (plist-get keys :shell-reuse))
               (not (keywordp (plist-get keys :shell-reuse))))
      (setq keys (plist-put keys :shell-reuse (list 'quote (plist-get keys :shell-reuse)))))
    (when (plist-member keys :new-shell-providers)
      (setq keys (plist-put keys :new-shell-providers (list 'quote (plist-get keys :new-shell-providers)))))
    (when (plist-member keys :providers)
      (setq keys (plist-put keys :providers (list 'quote (plist-get keys :providers)))))
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

(define-error 'agent-shell-workflow-skip
  "Workflow turn skipped by pre-op" 'user-error)

(defun agent-shell-workflow-skip (&optional reason)
  "Abort the current workflow turn cleanly from within a pre-op.
Signals `agent-shell-workflow-skip' carrying REASON.  The dispatcher
reports REASON with `message', never calls the pre-op's callback, and
therefore renders and submits nothing."
  (signal 'agent-shell-workflow-skip (list (or reason "nothing to do"))))

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
            (condition-case skip
                (funcall pre-op ctx callback)
              (agent-shell-workflow-skip
               (message "Workflow skipped: %s" (cadr skip))))
          ;; Only the pre-op itself is guarded; CALLBACK runs outside the
          ;; handler so downstream errors are not mistaken for skips.
          (let ((result (condition-case skip
                            (list (funcall pre-op ctx))
                          (agent-shell-workflow-skip
                           (message "Workflow skipped: %s" (cadr skip))
                           nil))))
            (when result
              (funcall callback (car result)))))))))

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

(defun agent-shell-workflow--canonicalize-dir (dir)
  "Return canonical representation of DIR with trailing slash."
  (file-name-as-directory (file-truename (expand-file-name (or dir default-directory)))))

(defun agent-shell-workflow--project-root (&optional dir)
  "Return the project root for DIR, or canonical DIR if no project detected."
  (let ((d (agent-shell-workflow--canonicalize-dir dir)))
    (or (when-let* ((pr (ignore-errors (project-current nil d))))
          (agent-shell-workflow--canonicalize-dir (project-root pr))))
        (when (fboundp 'vc-root-dir)
          (let ((default-directory d))
            (when-let* ((vc (ignore-errors (vc-root-dir))))
              (agent-shell-workflow--canonicalize-dir vc))))
        d))

(defun agent-shell-workflow--project-buffers (dir)
  "Return live `agent-shell' buffers whose project root matches DIR.
Matches buffers where DIR and buffer share the same project root,
or where either directory is within the other."
  (let* ((canon-dir (agent-shell-workflow--canonicalize-dir dir))
         (target-root (agent-shell-workflow--project-root canon-dir)))
    (seq-filter (lambda (buf)
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (let* ((buf-dir (agent-shell-workflow--canonicalize-dir default-directory))
                             (buf-root (agent-shell-workflow--project-root buf-dir)))
                        (or (equal buf-root target-root)
                            (equal buf-dir canon-dir)
                            (string-prefix-p buf-dir canon-dir)
                            (string-prefix-p canon-dir buf-dir))))))
                (agent-shell-buffers))))

(cl-defun agent-shell-workflow--target-candidates (&optional dir (include-queue (or (bound-and-true-p agent-shell-workflow-dispatch-target-functions)
                                                                                   (fboundp 'agent-shell-queue-enqueue))
                                                                               include-queue-p))
  "Return candidate alist for selecting a dispatch target via ACR.
DIR is the target directory (defaults to `default-directory').
When INCLUDE-QUEUE is non-nil (or omitted when queue integration is
active), an enqueue target option is included.

Each entry is of the form:
  (CANDIDATE-STRING ANNOTATION . TARGET)
Where TARGET is `:session-new', a live shell buffer, or `:queue'."
  (let* ((effective-dir (agent-shell-workflow--canonicalize-dir (or dir default-directory)))
         (dir-name (file-name-nondirectory (directory-file-name effective-dir)))
         (project-buffers (agent-shell-workflow--project-buffers effective-dir))
         (all-buffers (if (fboundp 'agent-shell-buffers) (agent-shell-buffers) nil))
         (other-buffers (seq-remove (lambda (b) (memq b project-buffers)) all-buffers))
         (queue-available (if include-queue-p
                              include-queue
                            (or (bound-and-true-p agent-shell-workflow-dispatch-target-functions)
                                (fboundp 'agent-shell-queue-enqueue))))
         candidates)
    ;; 1. New shell in this directory
    (push (cons (format "New agent-shell in %s" dir-name)
                (cons "[new shell]" :session-new))
          candidates)
    ;; 2. Open shells matching the repository
    (dolist (buf project-buffers)
      (when (buffer-live-p buf)
        (push (cons (buffer-name buf)
                    (cons (format "[%s (project)]" dir-name) buf))
              candidates)))
    ;; 3. Other open agent shells
    (dolist (buf other-buffers)
      (when (buffer-live-p buf)
        (let* ((buf-dir (with-current-buffer buf default-directory))
               (buf-dir-short (file-name-nondirectory (directory-file-name (expand-file-name buf-dir)))))
          (push (cons (buffer-name buf)
                      (cons (format "[open: %s]" buf-dir-short) buf))
                candidates))))
    ;; 4. Queue target (when available)
    (when queue-available
      (push (cons "Enqueue in task queue"
                  (cons "[queue]" :queue))
            candidates))
    (nreverse candidates)))

(defun agent-shell-workflow--pick-dispatch-target (&optional dir prompt)
  "Prompt the user to select a dispatch target via `annotated-completing-read'.
Offers creating a new shell in DIR, any open shell, and task queue when available."
  (let* ((effective-dir (agent-shell-workflow--canonicalize-dir (or dir default-directory)))
         (candidates (agent-shell-workflow--target-candidates effective-dir))
         (prompt-str (or prompt
                         (format "Dispatch workflow to [%s]: "
                                 (file-name-nondirectory (directory-file-name effective-dir)))))
         (choice (annotated-completing-read
                  candidates
                  :prompt prompt-str
                  :category 'agent-shell-target
                  :require-match t
                  :history 'agent-shell-workflow-target-history)))
    (cond
     ((bufferp choice) choice)
     ((memq choice '(:session-new :queue :session-reuse)) choice)
     ((stringp choice)
      (cond
       ((get-buffer choice) (get-buffer choice))
       ((string-prefix-p ":" choice) (intern choice))
       ((string-match-p "\\`\\[?new" (downcase choice)) :session-new)
       ((string-match-p "queue" (downcase choice)) :queue)
       (t choice)))
     (t choice))))

(defun agent-shell-workflow--resolve-target (target &optional dir)
  "Resolve TARGET into a concrete target.
When TARGET is `:ask', prompt the user via `agent-shell-workflow--pick-dispatch-target'.
Otherwise return TARGET normalized."
  (let ((norm (agent-shell-workflow--normalize-target target)))
    (if (eq norm :ask)
        (agent-shell-workflow--pick-dispatch-target dir)
      norm)))

(defun agent-shell-workflow--normalize-provider-spec (item)
  "Normalize provider specification ITEM to (ID . PLIST-OPTIONS).
ITEM may be:
- a symbol or string naming a provider ID, e.g. \\='antigravity
- a list like (ID :model \"...\" :mode \"...\")
- a plist like (:provider ID :model \"...\" :mode \"...\")
- or an agent config alist."
  (cond
   ((and (listp item) (assq :identifier item))
    (cons (cdr (assq :identifier item)) (list :config item)))
   ((and (listp item) (keywordp (car item)))
    (let ((id (or (plist-get item :provider)
                  (plist-get item :identifier)
                  (plist-get item :name)))
          (opts (copy-sequence item)))
      (setq opts (plist-put opts :provider nil))
      (setq opts (plist-put opts :identifier nil))
      (setq opts (plist-put opts :name nil))
      (cons id opts)))
   ((and (consp item) (cdr item) (keywordp (cadr item)))
    (cons (car item) (cdr item)))
   (t
    (cons item nil))))

(defun agent-shell-workflow--apply-provider-overrides (config opts)
  "Apply options in OPTS (such as :model and :mode) to agent CONFIG."
  (let ((model (or (plist-get opts :model) (plist-get opts :default-model-id)))
        (mode (or (plist-get opts :mode) (plist-get opts :default-session-mode-id)))
        (cfg (copy-alist config)))
    (when model
      (let ((model-str (format "%s" model)))
        (map-put! cfg :default-model-id (lambda () model-str))
        (let ((orig-fn (map-elt cfg :default-config-options)))
          (map-put! cfg :default-config-options
                    (lambda ()
                      (let ((existing (if orig-fn (funcall orig-fn) nil)))
                        (cons (cons "model" model-str)
                              (seq-remove (lambda (pair) (equal (car pair) "model")) existing))))))))
    (when mode
      (let ((mode-str (format "%s" mode)))
        (map-put! cfg :default-session-mode-id (lambda () mode-str))
        (let ((orig-fn (map-elt cfg :default-config-options)))
          (map-put! cfg :default-config-options
                    (lambda ()
                      (let ((existing (if orig-fn (funcall orig-fn) nil)))
                        (cons (cons "mode" mode-str)
                              (seq-remove (lambda (pair) (equal (car pair) "mode")) existing))))))))
    cfg))

(defun agent-shell-workflow--resolve-single-provider (item)
  "Resolve provider spec ITEM into an agent-shell config alist with overrides."
  (let* ((norm (agent-shell-workflow--normalize-provider-spec item))
         (id (car norm))
         (opts (cdr norm))
         (direct-cfg (plist-get opts :config)))
    (if direct-cfg
        (agent-shell-workflow--apply-provider-overrides direct-cfg opts)
      (let* ((configs (if (fboundp 'agent-shell--resolved-agent-configs)
                          (agent-shell--resolved-agent-configs)
                        nil))
             (id-str (format "%s" id))
             (base (or (and (fboundp id) (funcall id))
                       (seq-find (lambda (c)
                                   (or (equal (format "%s" (map-elt c :identifier)) id-str)
                                       (string-equal-ignore-case (or (map-elt c :mode-line-name) "") id-str)
                                       (string-equal-ignore-case (or (map-elt c :buffer-name) "") id-str)))
                                 configs)
                       (if (fboundp 'agent-shell-make-agent-config)
                           (agent-shell-make-agent-config :identifier (intern id-str)
                                                          :buffer-name (capitalize id-str))
                         `((:identifier . ,(intern id-str))
                           (:buffer-name . ,(capitalize id-str))))))
             (cfg (copy-alist base)))
        (agent-shell-workflow--apply-provider-overrides cfg opts)))))

(defun agent-shell-workflow--resolve-provider-config (providers)
  "Resolve PROVIDERS list into a single selected agent config alist.
When PROVIDERS has 1 item, returns its resolved config without prompting.
When PROVIDERS has multiple items, prompts user via `completing-read'."
  (when providers
    (let ((list (if (listp providers) providers (list providers))))
      (if (= (length list) 1)
          (agent-shell-workflow--resolve-single-provider (car list))
        (let* ((candidates
                (mapcar (lambda (item)
                          (let* ((norm (agent-shell-workflow--normalize-provider-spec item))
                                 (id (car norm))
                                 (opts (cdr norm))
                                 (model (plist-get opts :model))
                                 (label (if model
                                            (format "%s (model: %s)" id model)
                                          (format "%s" id))))
                            (cons label item)))
                        list))
               (choice (completing-read "Select agent for workflow: "
                                        (mapcar #'car candidates)
                                        nil t nil nil (caar candidates)))
               (selected (cdr (assoc choice candidates))))
          (agent-shell-workflow--resolve-single-provider selected))))))

(defun agent-shell-workflow--match-buffer-provider (buffers providers)
  "Return the first buffer in BUFFERS that matches one of the PROVIDERS."
  (when (and buffers providers)
    (let ((wanted (mapcar (lambda (p)
                            (let ((id (car (agent-shell-workflow--normalize-provider-spec p))))
                              (and id (format "%s" id))))
                          providers)))
      (seq-find
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((id (or (and (boundp 'agent-shell--state)
                                (map-nested-elt agent-shell--state '(:agent-config :identifier)))
                           (and (boundp 'shell-maker--config)
                                (map-elt shell-maker--config :identifier)))))
               (and id (member (format "%s" id) wanted))))))
       buffers))))

(defun agent-shell-workflow--create-shell (&optional dir config display)
  "Create and return a new `agent-shell' buffer in DIR with CONFIG.
When CONFIG is nil, prompts for agent selection via `agent-shell-select-config'
if available, or falls back to `agent-shell-new-shell'.
DISPLAY controls buffer display: `:foreground' (focus via `pop-to-buffer'),
`:background' (no display or focus), or `:display' (via `display-buffer')."
  (let* ((canon-dir (agent-shell-workflow--canonicalize-dir (or dir default-directory)))
         (before-bufs (agent-shell-buffers)))
    (let ((default-directory canon-dir))
      (let ((res (cond
                  (config
                   (if (fboundp 'agent-shell--start)
                       (agent-shell--start :config config
                                           :session-strategy 'new
                                           :new-session t
                                           :no-focus t)
                     (agent-shell-new-shell)))
                  ((and (or (not noninteractive) (bound-and-true-p agent-shell-workflow-test-mode))
                        (fboundp 'agent-shell-select-config)
                        (fboundp 'agent-shell--start))
                   (let ((cfg (agent-shell-select-config :prompt "Start agent for workflow: ")))
                     (if cfg
                         (agent-shell--start :config cfg
                                             :session-strategy 'new
                                             :new-session t
                                             :no-focus t)
                       (agent-shell-new-shell))))
                  (t
                   (agent-shell-new-shell)))))
        (let ((buf (cond
                    ((buffer-live-p res)
                     res)
                    ((seq-find (lambda (b) (not (memq b before-bufs)))
                               (agent-shell-buffers)))
                    ((seq-first (agent-shell-workflow--project-buffers canon-dir)))
                    ((when (derived-mode-p 'agent-shell-mode)
                       (current-buffer)))
                    ((and res (not (numberp res)))
                     res))))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (setq-local agent-shell-workflow--new-shell t))
            (unless noninteractive
              (pcase (agent-shell-workflow--normalize-display display)
                (:background nil)
                (:foreground (pop-to-buffer buf))
                (_ (display-buffer buf)))))
          buf)))))

(defun agent-shell-workflow--call-create-shell (dir &optional config display)
  "Invoke `agent-shell-workflow--create-shell' respecting mocked arities."
  (cond
   ((or config display)
    (condition-case nil
        (agent-shell-workflow--create-shell dir config display)
      (wrong-number-of-arguments
       (condition-case nil
           (agent-shell-workflow--create-shell dir config)
         (wrong-number-of-arguments
          (agent-shell-workflow--create-shell dir))))))
   (t
    (agent-shell-workflow--create-shell dir))))

(defun agent-shell-workflow--session-buffer (target &optional dir spec)
  "Return a live `agent-shell' buffer for TARGET in DIR, creating or prompting.
When TARGET is `:session-new' or `:new', always create a new shell buffer.
When TARGET is `:session-reuse-always' (or SPEC has shell-reuse set to always),
reuse an existing project buffer without prompting; if none exist, create one.
Otherwise, prompt the user whether to reuse an existing buffer or create one.
If declined, signals `user-error'."
  (let* ((effective-dir (or dir default-directory))
         (norm-target (agent-shell-workflow--normalize-target target))
         (reuse-policy (agent-shell-workflow--normalize-shell-reuse
                        (and spec (agent-shell-workflow-spec-shell-reuse spec))))
         (providers (and spec (agent-shell-workflow-spec-new-shell-providers spec)))
         (display (agent-shell-workflow--normalize-display
                   (and spec (agent-shell-workflow-spec-buffer-display spec))))
         (always-reuse (or (eq norm-target :session-reuse-always)
                           (eq reuse-policy 'always))))
    (if (eq norm-target :session-new)
        (let ((cfg (and providers (agent-shell-workflow--resolve-provider-config providers))))
          (agent-shell-workflow--call-create-shell effective-dir cfg display))
      (let* ((buffers (agent-shell-workflow--project-buffers effective-dir))
             (dir-name (file-name-nondirectory (directory-file-name (expand-file-name effective-dir))))
             (reused-buf nil)
             (res-buf nil))
        (cond
         (always-reuse
          (if buffers
              (setq reused-buf (or (agent-shell-workflow--match-buffer-provider buffers providers)
                                   (car buffers))
                    res-buf reused-buf)
            (let ((cfg (and providers (agent-shell-workflow--resolve-provider-config providers))))
              (setq res-buf (agent-shell-workflow--call-create-shell effective-dir cfg display)))))
         ((null buffers)
          (if (y-or-n-p (format "No agent-shell open for %s. Create one? " dir-name))
              (let ((cfg (and providers (agent-shell-workflow--resolve-provider-config providers))))
                (setq res-buf (agent-shell-workflow--call-create-shell effective-dir cfg display)))
            (user-error "Aborted: no agent-shell available for %s" dir-name)))
         ((= (length buffers) 1)
          (let ((buf (car buffers)))
            (if (y-or-n-p (format "Reuse open agent-shell %s for %s? "
                                  (buffer-name buf) dir-name))
                (setq reused-buf buf
                      res-buf buf)
              (if (y-or-n-p (format "Create a new agent-shell for %s? " dir-name))
                  (let ((cfg (and providers (agent-shell-workflow--resolve-provider-config providers))))
                    (setq res-buf (agent-shell-workflow--call-create-shell effective-dir cfg display)))
                (user-error "Aborted: cancelled shell selection")))))
         (t
          (let* ((new-option "[New agent-shell]")
                 (choices (cons new-option (mapcar #'buffer-name buffers)))
                 (choice (completing-read (format "Select agent-shell for %s: " dir-name)
                                          choices nil t)))
            (if (string-equal choice new-option)
                (let ((cfg (and providers (agent-shell-workflow--resolve-provider-config providers))))
                  (setq res-buf (agent-shell-workflow--call-create-shell effective-dir cfg display)))
              (setq reused-buf (get-buffer choice)
                    res-buf reused-buf)))))
        (when (and (buffer-live-p reused-buf)
                   (not (agent-shell-workflow--fresh-shell-p reused-buf)))
          (agent-shell-workflow--maybe-clear-reused-buffer reused-buf spec))
        res-buf))))

(defun agent-shell-workflow--dispatch-rendered (spec ctx target submit &optional dir)
  "Deliver the rendered prompt for SPEC and CTX to TARGET in DIR.
When SUBMIT is non-nil, submit the prompt immediately.
Once the turn has been handed off without error (inserted, queued or
deferred), the function named by CTX's `:after-handoff' (a symbol, so
it survives queue serialization) is called with CTX."
  (let* ((effective-dir (or dir (plist-get ctx :context-dir) default-directory))
         (target (agent-shell-workflow--resolve-target (or target (agent-shell-workflow-spec-target spec)) effective-dir))
         (submit (if (null submit) (agent-shell-workflow-spec-submit spec) submit))
         (result
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
    (when-let* ((after (plist-get ctx :after-handoff))
                ((functionp after)))
      (condition-case err
          (funcall after ctx)
        (error (message "Workflow `%s' after-handoff failed: %s"
                        (agent-shell-workflow-spec-id spec)
                        (error-message-string err)))))
    result))

(defun agent-shell-workflow--buffer-busy-p (buffer)
  "Return non-nil if BUFFER is currently busy executing a turn or command."
  (let ((buf (cond
              ((bufferp buffer) buffer)
              ((stringp buffer) (get-buffer buffer))
              (t nil))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (unless (agent-shell-workflow--fresh-shell-p buf)
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
                   (ignore-errors (agent-shell-queue--has-running-item-p (buffer-name buf))))))))))

(defun agent-shell-workflow--resolve-busy-action-upfront (shell-buffer will-submit)
  "Determine the action to take if SHELL-BUFFER is busy, asked upfront.
Consults `agent-shell-workflow-busy-shell-behavior'.  When set to `ask',
prompts the user synchronously before preflight starts.
If SHELL-BUFFER is already busy, asks if prompt should be queued or if a
different shell should be selected.  If SHELL-BUFFER is not currently busy,
asks conditionally if prompt should be queued if the shell becomes busy.
Returns one of `queue', `wait', `select', `insert', or `error'."
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
         (let ((busy-now (agent-shell-workflow--buffer-busy-p shell-buffer)))
           (if busy-now
               (if (y-or-n-p (format "Agent shell `%s' is currently busy. Add prompt to queue when preflight completes? "
                                     buf-name))
                   'queue
                 (if (y-or-n-p "Select or create a different agent shell? ")
                     'select
                   'wait))
             (if queue-avail
                 (if (y-or-n-p (format "If agent shell `%s' is busy when preflight completes, add prompt to queue? "
                                       buf-name))
                     'queue
                   'wait)
               'wait))))
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

(defun agent-shell-workflow--sanitize-for-serialization (obj)
  "Convert unreadable objects in OBJ to readable representations."
  (cond
   ((bufferp obj) (buffer-name obj))
   ((processp obj) (process-name obj))
   ((markerp obj) (marker-position obj))
   ((consp obj)
    (cons (agent-shell-workflow--sanitize-for-serialization (car obj))
          (agent-shell-workflow--sanitize-for-serialization (cdr obj))))
   ((vectorp obj)
    (vconcat (mapcar #'agent-shell-workflow--sanitize-for-serialization obj)))
   (t obj)))

(defun agent-shell-workflow--enqueue-for-buffer (spec ctx shell-buffer submit)
  "Add prompt for SPEC, CTX, and SUBMIT to `agent-shell-queue'; for SHELL-BUFFER.
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
             (rendered (or (plist-get ctx :rendered)
                        (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) ctx))))
        (agent-shell-queue--enqueue-args
         (prin1-to-string (list :workflow-id (agent-shell-workflow-spec-id spec)
                                :rendered rendered
                                :ctx (agent-shell-workflow--sanitize-for-serialization ctx)
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
      (let ((sub nil)
            (timer nil)
            (called nil))
        (let ((fire (lambda ()
                      (unless called
                        (setq called t)
                        (when timer
                          (cancel-timer timer)
                          (setq timer nil))
                        (when sub
                          (agent-shell-unsubscribe :subscription sub)
                          (setq sub nil))
                        (when (buffer-live-p shell-buffer)
                          (with-current-buffer shell-buffer
                            (setq-local agent-shell-workflow--pending-clear nil)))
                        (funcall callback)))))
          (setq sub
                (condition-case nil
                    (agent-shell-subscribe-to
                     :shell-buffer shell-buffer
                     :event 'turn-complete
                     :on-event (lambda (_ev) (funcall fire)))
                  (error
                   (funcall fire)
                   nil)))
          (when sub
            (setq timer (run-with-timer 5.0 nil fire)))))
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
If a context clear is currently pending in SHELL-BUFFER, defers insertion
until turn-complete.
If SUBMIT is non-nil and SHELL-BUFFER is busy, handles the busy state
according to `agent-shell-workflow-busy-shell-behavior' or pre-resolved
`:busy-action' in CTX."
  (let ((do-insert
         (lambda ()
           (agent-shell-workflow--insert-and-post spec ctx shell-buffer submit))))
    (cond
     ((and (buffer-live-p shell-buffer)
           (buffer-local-value 'agent-shell-workflow--pending-clear shell-buffer)
           (with-current-buffer shell-buffer (derived-mode-p 'agent-shell-mode))
           (fboundp 'agent-shell-subscribe-to))
      (agent-shell-workflow--defer-until-turn-complete shell-buffer do-insert))
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
     (t
      (funcall do-insert)))))

(defun agent-shell-workflow--insert (spec ctx shell-buffer submit)
  "Render SPEC with CTX and insert it into SHELL-BUFFER.
When SUBMIT is non-nil, submit immediately (or prompt if SUBMIT is `:ask'
or `ask').  Returns the plist returned by `agent-shell-insert'."
  (let* ((actual-submit (cond
                         ((memq submit '(:ask ask))
                          (y-or-n-p "Submit rendered prompt? "))
                         (t submit)))
         (rendered (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) ctx))
         (display (agent-shell-workflow--normalize-display
                   (or (plist-get ctx :buffer-display)
                       (and spec (agent-shell-workflow-spec-buffer-display spec))))))
    (when (buffer-live-p shell-buffer)
      (with-current-buffer shell-buffer
        (setq-local agent-shell-workflow--new-shell nil))
      (unless noninteractive
        (pcase display
          (:background nil)
          (:foreground (pop-to-buffer shell-buffer))
          (_ (display-buffer shell-buffer))))
      (message "Submitting prompt for workflow `%s' to `%s'..."
               (agent-shell-workflow-spec-id spec)
               (buffer-name shell-buffer)))
    (agent-shell-insert :text rendered
                        :shell-buffer shell-buffer
                        :submit actual-submit)))

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
(cl-defun agent-shell-workflow-dispatch (id &key args target submit (context-dir default-directory)
                                            shell-reuse new-shell-providers providers
                                            buffer-display display &allow-other-keys)
  "Instantiate the workflow ID and dispatch it.
ARGS is a plist supplying template values; missing required args
are read interactively.
TARGET overrides the spec's declared target (`:session-reuse',
`:session-reuse-always', `:session-new', `:queue', `:ask').
SUBMIT non-nil forces prompt submission regardless of spec default.
CONTEXT-DIR sets `default-directory' for pre-op execution.
SHELL-REUSE overrides reuse behavior (\\='always, \\='prompt, \\='new).
NEW-SHELL-PROVIDERS (or PROVIDERS) overrides shell providers when spawning.
BUFFER-DISPLAY (or DISPLAY) overrides `:foreground', `:background',
or `:display'."
  (let* ((spec (or (agent-shell-workflow-get id)
                   (error "Agent-shell-workflow: no workflow registered with id `%s'" id)))
         (dir (or context-dir default-directory))
         (canon-dir (agent-shell-workflow--canonicalize-dir dir))
         (debounce-key (list id canon-dir))
         (now (float-time))
         (last-time (gethash debounce-key agent-shell-workflow--in-flight-dispatches)))
    (if (and agent-shell-workflow-debounce-interval
             last-time
             (< (- now last-time) agent-shell-workflow-debounce-interval))
        (progn
          (message "Workflow `%s' was just dispatched for %s; ignoring duplicate request"
                   id (file-name-nondirectory (directory-file-name canon-dir)))
          nil)
      (when agent-shell-workflow-debounce-interval
        (puthash debounce-key now agent-shell-workflow--in-flight-dispatches))
      (let* ((raw-target (agent-shell-workflow--normalize-target
                          (or target (agent-shell-workflow-spec-target spec))))
             (effective-display
              (agent-shell-workflow--normalize-display
               (or buffer-display display
                   (and spec (agent-shell-workflow-spec-buffer-display spec)))))
             (effective-shell-reuse
              (agent-shell-workflow--normalize-shell-reuse
               (or shell-reuse
                   (and spec (agent-shell-workflow-spec-shell-reuse spec)))))
             (effective-providers
              (or new-shell-providers providers
                  (and spec (agent-shell-workflow-spec-new-shell-providers spec))))
             (initial-target (agent-shell-workflow--resolve-target raw-target))
             (effective-submit (if (null submit)
                                   (agent-shell-workflow-spec-submit spec)
                                 submit))
             shell-buffer
             chosen-config
             (resolved-target initial-target)
             busy-action)
        ;; 1. Resolve target shell and upfront busy handling before preflight
        (let ((cur-target initial-target)
              (done nil))
          (while (not done)
            (cond
             ((or (memq cur-target '(:session-reuse-always :always-reuse :session-reuse-or-new :reuse-always))
                  (and (memq cur-target '(:session-reuse :reuse))
                       (memq effective-shell-reuse '(always :always t))))
              (let* ((buffers (agent-shell-workflow--project-buffers dir)))
                (if buffers
                    (setq cur-target (or (agent-shell-workflow--match-buffer-provider buffers effective-providers)
                                         (car buffers)))
                  (setq cur-target :session-new))))
             ((memq cur-target '(:session-reuse :reuse))
              (if (memq effective-shell-reuse '(new :new))
                  (setq cur-target :session-new)
                (let* ((buffers (agent-shell-workflow--project-buffers dir))
                       (dir-name (file-name-nondirectory (directory-file-name dir))))
                  (cond
                   ((null buffers)
                    (if (y-or-n-p (format "No agent-shell open for %s. Create one? " dir-name))
                        (progn
                          (setq cur-target :session-new)
                          (setq chosen-config
                                (if effective-providers
                                    (agent-shell-workflow--resolve-provider-config effective-providers)
                                  (when (and (fboundp 'agent-shell-select-config)
                                             (or (not noninteractive) (bound-and-true-p agent-shell-workflow-test-mode)))
                                    (agent-shell-select-config :prompt "Start agent for workflow: ")))))
                      (user-error "Aborted: no agent-shell available for %s" dir-name)))
                   ((= (length buffers) 1)
                    (let ((buf (car buffers)))
                      (if (y-or-n-p (format "Reuse open agent-shell %s for %s? "
                                            (buffer-name buf) dir-name))
                          (setq cur-target buf)
                        (if (y-or-n-p (format "Create a new agent-shell for %s? " dir-name))
                            (progn
                              (setq cur-target :session-new)
                              (setq chosen-config
                                    (if effective-providers
                                        (agent-shell-workflow--resolve-provider-config effective-providers)
                                      (when (and (fboundp 'agent-shell-select-config)
                                                 (or (not noninteractive) (bound-and-true-p agent-shell-workflow-test-mode)))
                                        (agent-shell-select-config :prompt "Start agent for workflow: ")))))
                          (user-error "Aborted: cancelled shell selection")))))
                   (t
                    (let* ((new-option "[New agent-shell]")
                           (choices (cons new-option (mapcar #'buffer-name buffers)))
                           (choice (completing-read (format "Select agent-shell for %s: " dir-name)
                                                    choices nil t nil nil
                                                    (buffer-name (car buffers)))))
                      (if (string-equal choice new-option)
                          (progn
                            (setq cur-target :session-new)
                            (setq chosen-config
                                  (if effective-providers
                                      (agent-shell-workflow--resolve-provider-config effective-providers)
                                    (when (fboundp 'agent-shell-select-config)
                                      (agent-shell-select-config :prompt "Start agent for workflow: ")))))
                        (setq cur-target (get-buffer choice)))))))))
             ((memq cur-target '(:session-new :new))
              (when (null chosen-config)
                (setq chosen-config
                      (if effective-providers
                          (agent-shell-workflow--resolve-provider-config effective-providers)
                        (when (and (fboundp 'agent-shell-select-config)
                                   (or (not noninteractive) (bound-and-true-p agent-shell-workflow-test-mode)))
                          (agent-shell-select-config :prompt "Start agent for workflow: ")))))
              (setq done t))
             ((or (bufferp cur-target) (stringp cur-target))
              (let ((b (if (stringp cur-target) (get-buffer cur-target) cur-target)))
                (if (and effective-submit (bufferp b) (buffer-live-p b))
                    (let ((b-action (agent-shell-workflow--resolve-busy-action-upfront b effective-submit)))
                      (if (eq b-action 'select)
                          (progn
                            (setq cur-target :session-new)
                            (when (null chosen-config)
                              (setq chosen-config
                                    (if effective-providers
                                        (agent-shell-workflow--resolve-provider-config effective-providers)
                                      (when (and (fboundp 'agent-shell-select-config)
                                                 (or (not noninteractive) (bound-and-true-p agent-shell-workflow-test-mode)))
                                        (agent-shell-select-config :prompt "Start agent for workflow: "))))))
                        (setq shell-buffer b
                              busy-action b-action
                              done t)))
                  (setq shell-buffer b
                        done t))))
             (t
              (setq done t))))
          (setq resolved-target cur-target))

        ;; 2. Additional preflight clarifications and argument collection
        (let ((collected-args (agent-shell-workflow--collect-args spec args)))
          ;; 3. Create the shell buffer now if needed
          (if (eq resolved-target :session-new)
              (progn
                (setq shell-buffer (agent-shell-workflow--call-create-shell dir chosen-config effective-display)
                      resolved-target shell-buffer)
                (when (buffer-live-p shell-buffer)
                  (unless noninteractive
                    (pcase effective-display
                      (:background nil)
                      (:foreground (pop-to-buffer shell-buffer))
                      (_ (display-buffer shell-buffer))))
                  (message "Created agent-shell `%s' for workflow `%s'"
                           (buffer-name shell-buffer) id)))
            (when (and (buffer-live-p shell-buffer)
                       (not (eq busy-action 'queue))
                       (agent-shell-workflow-spec-clear spec)
                       (not (agent-shell-workflow--fresh-shell-p shell-buffer)))
              (agent-shell-workflow--maybe-clear-reused-buffer shell-buffer spec)))

          ;; 4. Non-blocking preflight execution
          (message "Running preflight for workflow `%s'..." id)
          (let ((initial-ctx (list :args collected-args
                                   :target resolved-target
                                   :shell-buffer shell-buffer
                                   :submit effective-submit
                                   :busy-action busy-action
                                   :context-dir dir
                                   :buffer-display effective-display
                                   :shell-reuse effective-shell-reuse
                                   :new-shell-providers effective-providers)))
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
                   ;; 5. Insert rendered prompt & submit
                   (agent-shell-workflow--dispatch-rendered spec ctx-with-rendered resolved-target effective-submit effective-dir)))))))))))

;; ─────────────────────────────────────────────────────────────────────────────
;; Repository groups

(defun agent-shell-workflow-register-repo-group (name repos)
  "Register a repository group named NAME with REPOS directory paths.
NAME is a symbol (or string converted to a symbol).
REPOS is a list of directory path strings.
Existing group with NAME is replaced.  Returns NAME."
  (unless (symbolp name)
    (setq name (intern (format "%s" name))))
  (unless (and (listp repos) (seq-every-p #'stringp repos))
    (user-error "REPOS must be a list of directory path strings"))
  (let ((expanded (seq-map #'expand-file-name repos)))
    (setf (map-elt agent-shell-workflow-repo-groups name) expanded)
    name))

(defun agent-shell-workflow-resolve-repo-group (group)
  "Return list of directory paths for GROUP.
GROUP can be a symbol registered in `agent-shell-workflow-repo-groups',
or a list of directory path strings.
Signals `user-error' if GROUP cannot be resolved."
  (cond
   ((and (symbolp group) (map-elt agent-shell-workflow-repo-groups group))
    (map-elt agent-shell-workflow-repo-groups group))
   ((and (listp group) (seq-every-p #'stringp group))
    (seq-map #'expand-file-name group))
   (t
    (user-error "Unknown or invalid repository group: %S" group))))

(cl-defun agent-shell-workflow-dispatch-group (id group &rest keys)
  "Dispatch workflow ID across all repositories in GROUP.
GROUP is a symbol in `agent-shell-workflow-repo-groups' or a list of repository directories.
KEYS are passed to `agent-shell-workflow-dispatch' for each repository, overriding
`:dir' with each repository directory.
Returns an alist mapping each repository directory to its dispatch result."
  (let* ((repos (agent-shell-workflow-resolve-repo-group group))
         results)
    (dolist (dir repos (nreverse results))
      (let* ((dir-keys (copy-sequence keys))
             (effective-keys (plist-put dir-keys :dir dir))
             (res (apply #'agent-shell-workflow-dispatch id effective-keys)))
        (push (cons dir res) results)))))

;; Integration with agent-shell-queue

(defun agent-shell-workflow--dispatch-queue-item (item target-buffer)
  "Execute a queued workflow ITEM into TARGET-BUFFER."
  (let* ((raw (agent-shell-queue-item-args item))
         (clean-raw (if (stringp raw)
                        (replace-regexp-in-string "#<[^>]+>" "\"<unreadable>\"" raw)
                      raw))
         (plist (condition-case nil
                    (if (stringp clean-raw)
                        (read clean-raw)
                      clean-raw)
                  (error nil)))
         (rendered (or (and (listp plist) (plist-get plist :rendered))
                       (if (stringp raw) raw (format "%s" raw))))
         (submit (if (and (listp plist) (plist-member plist :submit))
                     (plist-get plist :submit)
                   t))
         (buf (cond
               ((bufferp target-buffer) target-buffer)
               ((stringp target-buffer) (get-buffer target-buffer))
               (t (current-buffer))))
         (workflow-id (and (listp plist) (plist-get plist :workflow-id)))
         (spec (when workflow-id (agent-shell-workflow-get workflow-id)))
         (ctx (and (listp plist) (plist-get plist :ctx))))
    (if spec
        (let ((effective-ctx (if (listp ctx) (copy-sequence ctx) nil)))
          (setq effective-ctx (plist-put effective-ctx :shell-buffer buf))
          (setq effective-ctx (plist-put effective-ctx :target buf))
          (setq effective-ctx (plist-put effective-ctx :rendered rendered))
          (agent-shell-workflow--insert-and-post spec effective-ctx buf submit))
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

(require 'agent-shell-queue nil t)
(agent-shell-workflow--setup-queue-integration)

(autoload 'agent-shell-workflow-select "agent-shell-workflow-menu" nil t)
(autoload 'agent-shell-workflow-select-group "agent-shell-workflow-menu" nil t)

(provide 'agent-shell-workflow)

;;; agent-shell-workflow.el ends here
