;;; agent-shell-workflow-train.el --- Reusable workflow train templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Versioned workflow templates, frozen train instantiation, named continuation
;; registry, pure-data plan-to-template preview/registration, immutable handoff
;; checkpoints, and MCP prompt delivery bridge for `agent-shell-workflow'.
;;
;; Load-time pure: starts no timers, daemons, or agent work on `require'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-workflow-train-contract)

(declare-function agent-shell-queue-train-create "agent-shell-queue-train" (&rest args))
(declare-function agent-shell-queue-train-dag-cycle-p "agent-shell-queue-train-contract" (adjacency))
(declare-function mcpkit-register-prompt "mcpkit" (name service &rest plist))

;;; Registries

(defvar agent-shell-workflow-train--templates (make-hash-table :test #'equal)
  "Map of (TEMPLATE-ID . VERSION) to immutable template spec plist.")

(defvar agent-shell-workflow-train--continuations (make-hash-table :test #'equal)
  "Map of (HANDLER-NAME . VERSION) to continuation function.")

;;; Template Registration and Lookup

(defun agent-shell-workflow-train-register-template (spec)
  "Validate and register versioned workflow template SPEC plist.
Returns the frozen copy stored in the registry."
  (let ((check (agent-shell-workflow-train-validate-template spec)))
    (unless (plist-get check :ok)
      (user-error "Invalid workflow template: %s"
                  (string-join (plist-get check :errors) "; ")))
    (let* ((id (plist-get spec :id))
           (ver (plist-get spec :version))
           (frozen (copy-tree spec)))
      (setf (map-elt agent-shell-workflow-train--templates (cons id ver)) frozen)
      frozen)))

(defun agent-shell-workflow-train-get-template (id &optional version)
  "Return registered template for ID at VERSION (or highest registered version)."
  (if version
      (map-elt agent-shell-workflow-train--templates (cons id version))
    (let ((best-ver 0)
          (best-spec nil))
      (map-do (lambda (k v)
                (when (and (eq (car k) id) (> (cdr k) best-ver))
                  (setq best-ver (cdr k)
                        best-spec v)))
              agent-shell-workflow-train--templates)
      best-spec)))

(defun agent-shell-workflow-train-list-templates ()
  "Return list of all registered versioned workflow template specs."
  (let (out)
    (map-do (lambda (_k v) (push v out)) agent-shell-workflow-train--templates)
    (nreverse out)))

;;; Named Versioned Continuations

(defun agent-shell-workflow-train-register-continuation (name version handler)
  "Register continuation HANDLER under NAME and positive integer VERSION."
  (unless (and (symbolp name) (integerp version) (> version 0) (functionp handler))
    (user-error "Continuation requires symbol NAME, positive VERSION, and function HANDLER"))
  (setf (map-elt agent-shell-workflow-train--continuations (cons name version)) handler))

(defun agent-shell-workflow-train-resolve-continuation (name version)
  "Resolve registered continuation handler for NAME and VERSION.
Signals an error with migration guidance if the handler is not registered."
  (or (map-elt agent-shell-workflow-train--continuations (cons name version))
      (error "Unknown continuation handler %S v%S; register or migrate train before resuming"
             name version)))

;;; Immutable Handoff Checkpoints

(cl-defun agent-shell-workflow-train-build-handoff
    (train task &key objective expected-revision artifacts previous-outcomes)
  "Build and validate an immutable worker handoff checkpoint for TRAIN and TASK."
  (let* ((limits (plist-get train :limits))
         (usage (plist-get train :usage))
         (remaining
          (list :max-turns (max 0 (- (or (plist-get limits :max-turns) 0)
                                     (or (plist-get usage :turns) 0)))
                :max-retries (max 0 (- (or (plist-get limits :max-retries) 0)
                                       (or (plist-get usage :retries) 0)))
                :max-duration (or (plist-get limits :max-duration) 0)))
         (checkpoint
          (list :schema-version agent-shell-workflow-train-contract-version
                :train-id (plist-get train :id)
                :task-id (plist-get task :id)
                :objective (or objective (plist-get task :objective)
                               (format "Execute task %s" (plist-get task :id)))
                :resource (plist-get train :resource)
                :expected-revision (or expected-revision (plist-get train :revision))
                :artifacts artifacts
                :previous-outcomes previous-outcomes
                :authorization (plist-get train :authorization)
                :remaining-limits remaining
                :deadline (plist-get train :deadline))))
    (let ((check (agent-shell-workflow-train-validate-handoff checkpoint)))
      (unless (plist-get check :ok)
        (error "Invalid handoff checkpoint: %s"
               (string-join (plist-get check :errors) "; ")))
      checkpoint)))

;;; Safe Pure-Data Plan-to-Template Compilation

(defun agent-shell-workflow-train--dag-cycle-p (tasks)
  "Return non-nil if TASKS list contains a dependency cycle."
  (let ((adj (mapcar (lambda (tk) (cons (plist-get tk :id) (plist-get tk :depends-on)))
                     tasks))
        (state (make-hash-table :test #'equal))
        (found nil))
    (cl-labels
        ((visit (node)
           (let ((st (map-elt state node)))
             (cond
              ((eq st 'visiting) (setq found t))
              ((eq st 'visited) nil)
              (t
               (setf (map-elt state node) 'visiting)
               (dolist (dep (cdr (assoc node adj)))
                 (unless found (visit dep)))
               (setf (map-elt state node) 'visited))))))
      (dolist (entry adj)
        (unless found (visit (car entry)))))
    found))

(defun agent-shell-workflow-train-template-preview (source-plan)
  "Compile plain-data SOURCE-PLAN into a validated template preview.
Never evaluates Lisp expressions or note text.  Preserves `:source-note-id'
and computes `:source-digest' for traceability."
  (let* ((id (plist-get source-plan :id))
         (ver (or (plist-get source-plan :version) 1))
         (tasks (plist-get source-plan :tasks))
         (effects (plist-get source-plan :permitted-effects))
         (note-id (plist-get source-plan :source-note-id))
         (digest (secure-hash 'sha256 (prin1-to-string source-plan))))
    (when (agent-shell-workflow-train--dag-cycle-p tasks)
      (user-error "Source plan task graph contains a dependency cycle"))
    (let* ((spec (list :id id
                       :version ver
                       :description (or (plist-get source-plan :description) "")
                       :task-types (mapcar (lambda (tk) (or (plist-get tk :type) 'task)) tasks)
                       :tasks (copy-tree tasks)
                       :permitted-effects effects
                       :declared-branches (or (plist-get source-plan :declared-branches)
                                              '(retry cancel))
                       :default-limits (plist-get source-plan :default-limits)
                       :context-policy (or (plist-get source-plan :context-policy) 'fresh)
                       :source-note-id note-id
                       :source-digest digest))
           (check (agent-shell-workflow-train-validate-template spec)))
      (unless (plist-get check :ok)
        (user-error "Plan preview validation failed: %s"
                    (string-join (plist-get check :errors) "; ")))
      (list :ok t :preview spec :source-digest digest))))

(defun agent-shell-workflow-train-template-register-preview (preview-result)
  "Register a previously reviewed PREVIEW-RESULT as a versioned template."
  (unless (and (listp preview-result) (plist-get preview-result :ok)
               (plist-get preview-result :preview))
    (user-error "Cannot register unvalidated template preview"))
  (agent-shell-workflow-train-register-template (plist-get preview-result :preview)))

;;; Frozen Train Instantiation

(defun agent-shell-workflow-train--resolve-limits (template caller-limits interactive-p)
  "Resolve finite limits from CALLER-LIMITS or TEMPLATE defaults.
If neither sets limits, prompts when INTERACTIVE-P else signals `user-error'."
  (or caller-limits
      (plist-get template :default-limits)
      (if interactive-p
          (list :max-turns (read-number "Max turns: " 6)
                :max-retries (read-number "Max retries: " 4)
                :max-duration (read-number "Max duration (seconds): " 7200))
        (user-error "Template %S defines no default limits; explicit :limits required"
                    (plist-get template :id)))))

(cl-defun agent-shell-workflow-train-create
    (template-id &key version resource inputs limits authorization
                 context-policy interactive-p now state-dir)
  "Instantiate TEMPLATE-ID (at VERSION or latest) into a staged durable train.
Freezes template version and task graph into plain data.  Authorization
defaults to nil (deny-by-default: nothing runs unattended unless granted)."
  (let ((tpl (agent-shell-workflow-train-get-template template-id version)))
    (unless tpl
      (user-error "Workflow template not found: %S (version %S)" template-id version))
    (let* ((eff-limits (agent-shell-workflow-train--resolve-limits tpl limits interactive-p))
           (tasks (copy-tree (or (plist-get tpl :tasks)
                                 (list (list :id "step-1" :depends-on nil
                                             :objective (or (plist-get tpl :description)
                                                            (symbol-name template-id)))))))
           (definition (list :template-id (plist-get tpl :id)
                             :template-version (plist-get tpl :version)
                             :inputs (copy-tree inputs)
                             :context-policy (or context-policy
                                                 (plist-get tpl :context-policy)
                                                 'fresh)
                             :declared-branches (copy-tree (plist-get tpl :declared-branches))
                             :source-note-id (plist-get tpl :source-note-id)
                             :source-digest (plist-get tpl :source-digest))))
      (if (fboundp 'agent-shell-queue-train-create)
          (agent-shell-queue-train-create
           :definition definition
           :resource resource
           :authorization authorization
           :limits eff-limits
           :tasks tasks
           :now now
           :state-dir state-dir)
        (list :outcome 'accepted
              :state 'staged
              :revision 1
              :definition definition
              :resource resource
              :authorization authorization
              :limits eff-limits
              :tasks tasks)))))

;;; Built-In Reusable Templates & MCP Prompt Bridge

(defun agent-shell-workflow-train-register-builtin-templates ()
  "Register built-in v1 workflow templates explicitly."
  (dolist (spec
           (list
            (list :id 'ci-repair :version 1
                  :description "Bounded CI repair, push verification, and strict SHA observation."
                  :task-types '(observe repair verify)
                  :tasks '((:id "observe-initial" :type observe :depends-on nil :read-only t)
                           (:id "repair" :type repair :depends-on ("observe-initial") :repair-p t)
                           (:id "observe-pushed" :type observe :depends-on ("repair") :read-only t))
                  :permitted-effects '(dispatch observe request-review publish-artifact)
                  :declared-branches '(retry revise escalate cancel)
                  :context-policy 'fresh
                  :prompt-arguments '((:name "repository" :description "owner/repo" :required t)
                                      (:name "branch" :description "Topic branch" :required t))
                  :prompt-template "Repair failing CI for {{repository}} on {{branch}}. Submit structured result and end turn."
                  :default-limits '(:max-turns 6 :max-retries 4 :max-duration 7200
                                    :max-repairs 3 :max-observations 240))
            (list :id 'pr-review-cycle :version 1
                  :description "Collect PR review batches, propose responses, and gate external publish."
                  :task-types '(observe plan revise review)
                  :tasks '((:id "collect-feedback" :type observe :depends-on nil :read-only t)
                           (:id "propose-response" :type plan :depends-on ("collect-feedback")))
                  :permitted-effects '(dispatch observe request-review publish-artifact)
                  :declared-branches '(approve revise defer cancel)
                  :context-policy 'fresh
                  :prompt-arguments '((:name "pull_request" :description "PR number" :required t))
                  :prompt-template "Prepare bounded response proposals for PR #{{pull_request}}. Do not publish without approval."
                  :default-limits '(:max-turns 6 :max-retries 3 :max-duration 7200))
            (list :id 'mail-triage :version 1
                  :description "Classify mail batches and extract actionable Org/reply proposals."
                  :task-types '(classify extract review)
                  :tasks '((:id "extract-actionables" :type extract :depends-on nil :read-only t))
                  :permitted-effects '(dispatch request-review publish-artifact)
                  :declared-branches '(approve revise defer cancel)
                  :context-policy 'fresh
                  :prompt-arguments '((:name "account" :description "Mail account root" :required t))
                  :prompt-template "Extract actionable proposals from mail account {{account}} into a reviewable manifest."
                  :default-limits '(:max-turns 4 :max-retries 2 :max-duration 3600))
            (list :id 'plan-review :version 1
                  :description "Review a Denote plan at a frozen content digest."
                  :task-types '(review)
                  :tasks '((:id "review-plan" :type review :depends-on nil :read-only t))
                  :permitted-effects '(dispatch request-review publish-artifact)
                  :declared-branches '(approve revise cancel)
                  :context-policy 'fresh
                  :prompt-arguments '((:name "note_id" :description "Denote note ID" :required t))
                  :prompt-template "Review Denote plan {{note_id}} and submit structured findings."
                  :default-limits '(:max-turns 2 :max-retries 1 :max-duration 1800))
            (list :id 'patch-implement :version 1
                  :description "Implement one bounded task in an isolated worktree."
                  :task-types '(implement)
                  :tasks '((:id "implement" :type implement :depends-on nil))
                  :permitted-effects '(dispatch publish-artifact request-review)
                  :declared-branches '(approve revise retry cancel)
                  :context-policy 'fresh
                  :prompt-arguments '((:name "objective" :description "Task objective" :required t))
                  :prompt-template "Implement bounded task: {{objective}}. Submit structured result and end turn."
                  :default-limits '(:max-turns 4 :max-retries 2 :max-duration 3600))
            (list :id 'independent-review :version 1
                  :description "Independent read-only review of an immutable patch snapshot."
                  :task-types '(review)
                  :tasks '((:id "independent-review" :type review :depends-on nil :read-only t))
                  :permitted-effects '(dispatch publish-artifact request-review)
                  :declared-branches '(approve revise cancel)
                  :context-policy 'fresh
                  :prompt-arguments '((:name "revision" :description "Target Git SHA" :required t))
                  :prompt-template "Perform independent read-only review of commit {{revision}}."
                  :default-limits '(:max-turns 2 :max-retries 1 :max-duration 1800))))
    (agent-shell-workflow-train-register-template spec)))

(defun agent-shell-workflow-train--render-prompt (template args)
  "Render simple {{key}} placeholders in TEMPLATE using ARGS plist."
  (replace-regexp-in-string
   "{{\\([a-zA-Z0-9_-]+\\)}}"
   (lambda (match)
     (let* ((key (match-string 1 match))
            (val (plist-get args (intern (concat ":" key)))))
       (if val (format "%s" val) match)))
   template t t))

;;;###autoload
(defun agent-shell-workflow-train-register-mcp-prompts (service)
  "Expose workflow train templates on MCP SERVICE via `mcpkit-register-prompt'."
  (unless (fboundp 'mcpkit-register-prompt)
    (user-error "mcpkit-register-prompt is not available"))
  (let ((count 0))
    (dolist (tpl (agent-shell-workflow-train-list-templates))
      (let* ((id (plist-get tpl :id))
             (ver (plist-get tpl :version))
             (desc (or (plist-get tpl :description) ""))
             (pargs (plist-get tpl :prompt-arguments))
             (ptpl (or (plist-get tpl :prompt-template) desc))
             (pname (format "asw_%s_v%d" (replace-regexp-in-string "-" "_" (symbol-name id)) ver)))
        (mcpkit-register-prompt
         pname service
         :title (format "ASW: %s (v%d)" id ver)
         :description desc
         :arguments pargs
         :handler (lambda (args)
                    (agent-shell-workflow-train--render-prompt ptpl args)))
        (cl-incf count)))
    count))

(provide 'agent-shell-workflow-train)
;;; agent-shell-workflow-train.el ends here
