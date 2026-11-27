;;; agent-shell-workflow-workspace-actions.el --- Coordinated checkout actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Frozen checkout dispatch and deterministic verification for workspace actions.

;;; Code:

(require 'agent-shell-workflow-operations)
(require 'seq)
(require 'subr-x)

(defvar agent-shell-workflow--workspace-dispatches nil
  "Runtime dispatch options indexed by operation ID.")

(defvar agent-shell-workflow--workspace-owned-shells nil
  "Dedicated newly created buffers indexed by operation ID.")

(defvar-local agent-shell-workflow--workspace-active-turn nil
  "Operation and token owning the current turn, or nil.")

(defun agent-shell-workflow--workspace-turn-start (buffer ctx)
  "Validate CTX's live ownership before inserting a turn into BUFFER."
  (let* ((id (plist-get ctx :operation-id))
         (operation (agent-shell-workflow-operation-read id))
         (args (plist-get ctx :args))
         (token (plist-get args :dispatch-token))
         (ids (plist-get args :workspace-target-ids)))
    (unless (and ids
                 (seq-every-p
                  (lambda (target-id)
                    (let ((member (agent-shell-workflow--workspace-member operation target-id)))
                      (and (equal "running" (plist-get member :state))
                           (equal token (plist-get (plist-get member :details) :dispatch-token)))))
                  ids))
      (user-error "Workspace turn is cancelled, stale or already completed"))
    (when (plist-get ctx :owned-shell)
      (with-current-buffer buffer
        (setq agent-shell-workflow--workspace-active-turn (list id token ids)))
      (setf (map-elt agent-shell-workflow--workspace-owned-shells id)
            (cons (list :buffer buffer :token token :target-ids ids)
                  (map-elt agent-shell-workflow--workspace-owned-shells id))))))

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-interrupt "agent-shell" (&optional force))
(declare-function agent-shell-queue-store-items "agent-shell-queue-core" (store))
(declare-function agent-shell-queue-item-args "agent-shell-queue-core" (item))
(declare-function agent-shell-queue-item-id "agent-shell-queue-core" (item))
(declare-function agent-shell-queue-remove "agent-shell-queue-core" (id))
(defvar agent-shell-queue--store)

(defun agent-shell-workflow--workspace-member (operation target-id)
  "Find TARGET-ID in OPERATION's members."
  (seq-find (lambda (member) (equal target-id (plist-get member :target-id)))
            (plist-get operation :members)))

(defun agent-shell-workflow--workspace-action-pre (context)
  "Require CONTEXT to belong to a frozen workspace operation."
  (unless (and (stringp (plist-get context :operation-id))
               (consp (plist-get context :manifest)))
    (user-error "Use workspace dispatch-worktrees or select-worktrees for this workflow"))
  context)

(defun agent-shell-workflow--workspace-transition (operation-id target-id state &optional details)
  "Record STATE and DETAILS for TARGET-ID in OPERATION-ID using fresh generation."
  (agent-shell-workflow-operation-update
   operation-id target-id
   (plist-get (agent-shell-workflow-operation-read operation-id) :generation)
   state details))

(defun agent-shell-workflow--workspace-process (directory command)
  "Run explicit COMMAND in DIRECTORY, signaling on nonzero exit."
  (unless (and (consp command) (seq-every-p #'stringp command))
    (user-error "Command must be a nonempty list of program and arguments"))
  (let ((default-directory directory))
    (with-temp-buffer
      (let ((status (apply #'process-file (car command) nil t nil (cdr command))))
        (unless (and (integerp status) (zerop status))
          (error "%s failed (%s): %s" (car command) status (buffer-string)))
        (string-trim (buffer-string))))))

(defun agent-shell-workflow--workspace-preflight (target mutating)
  "Validate frozen TARGET and MUTATING checkout requirements."
  (when-let* ((failure (plist-get target :preflight-error)))
    (user-error "%s" failure))
  (unless (and (equal "local" (plist-get target :resource-id))
               (not (file-remote-p (plist-get target :shell-dir)))
               (file-directory-p (plist-get target :shell-dir)))
    (user-error "Workspace shell resource is unavailable"))
  (let ((observed (agent-shell-workflow--workspace-target
                   (plist-get target :worktree-dir))))
    (seq-do (lambda (field)
              (unless (equal (plist-get observed field) (plist-get target field))
                (user-error "Checkout changed %s: %s" field
                            (plist-get target :worktree-dir))))
            '(:id :common-dir :branch :head))
    (when (and mutating (null (plist-get observed :branch)))
      (user-error "Mutation requires a branch: %s" (plist-get target :worktree-dir)))
    (when (plist-get observed :preflight-error)
      (user-error "%s" (plist-get observed :preflight-error)))))

(defun agent-shell-workflow--workspace-ready-p (operation target-id)
  "Return whether OPERATION dependencies permit TARGET-ID to start."
  (let ((dependency (seq-find
                     (lambda (entry) (equal target-id (plist-get entry :target-id)))
                     (plist-get operation :dependencies))))
    (seq-every-p
     (lambda (required)
       (equal "succeeded" (plist-get
                           (agent-shell-workflow--workspace-member operation required)
                           :state)))
     (plist-get dependency :requires))))

;;;###autoload
(defun agent-shell-workflow-dispatch-worktrees (id directories &rest keys)
  "Dispatch workflow ID to exact DIRECTORIES with a durable frozen manifest.
KEYS accepts :workspace, :dependencies, :together and ordinary dispatch options.
Mutation actions require explicit verification and commit arguments.  Ordinary
workflows receive the frozen manifest; their completion remains verifying
until their own verifier records an observed result."
  (let* ((snapshot (agent-shell-workflow-resolve-workspace
                    (plist-get keys :workspace) directories))
         (mutating (or (memq id '(commit-group repair-conflicts))
                       (if (plist-member keys :mutating)
                           (plist-get keys :mutating)
                         (not (memq id '(workspace-inspect group-git-status))))))
         (arguments (plist-get keys :args)))
    (when (plist-get keys :together)
      (unless (= 1 (thread-last (plist-get snapshot :targets)
                                (seq-map (lambda (target) (plist-get target :shell-dir)))
                                seq-uniq length))
        (user-error "Together dispatch requires one explicit shared shell resource")))
    (unless (agent-shell-workflow-get id)
      (user-error "Unknown workflow: %s" id))
    (agent-shell-workflow-operations-setup)
    (add-hook 'agent-shell-workflow-operation-cancel-functions
              #'agent-shell-workflow--workspace-cancel)
    (when mutating
      (unless (and (consp (plist-get arguments :verify-command))
                   (seq-every-p #'stringp (plist-get arguments :verify-command)))
        (user-error "Mutation needs an explicit :verify-command list"))
      (when (memq id '(commit-group repair-conflicts))
        (unless (and
                 (stringp (plist-get arguments :commit-message))
                 (not (string-empty-p (plist-get arguments :commit-message))))
          (user-error "Commit actions need :commit-message")))
      (when (and (eq id 'commit-group)
                 (not (and (consp (plist-get arguments :files))
                           (seq-every-p #'stringp (plist-get arguments :files)))))
        (user-error "Commit preparation needs explicit :files"))
      (when (and (eq id 'repair-conflicts)
                 (not (stringp (plist-get arguments :merge-ref))))
        (user-error "Conflict repair needs :merge-ref"))
      (when (and (plist-get keys :target)
                 (not (eq :session-new (plist-get keys :target))))
        (user-error "Mutation workflows require :target :session-new for owned cancellation")))
    (let* ((operation (agent-shell-workflow-operation-create
                       id snapshot mutating (plist-get keys :dependencies)))
           (operation-id (plist-get operation :operation-id)))
      (setf (map-elt agent-shell-workflow--workspace-dispatches operation-id)
            (copy-tree keys))
      (agent-shell-workflow--workspace-schedule operation-id)
      (agent-shell-workflow-operation-read operation-id))))

(defun agent-shell-workflow--workspace-schedule (operation-id)
  "Dispatch ready pending members of OPERATION-ID independently."
  (let* ((operation (agent-shell-workflow-operation-read operation-id))
         (keys (map-elt agent-shell-workflow--workspace-dispatches operation-id))
         (targets (plist-get operation :targets))
         ready)
    (seq-do
     (lambda (target)
       (let* ((target-id (plist-get target :id))
              (member (agent-shell-workflow--workspace-member
                       (agent-shell-workflow-operation-read operation-id) target-id)))
         (when (and (or (not (plist-get keys :resume-targets))
                        (member target-id (plist-get keys :resume-targets)))
                    (equal "pending" (plist-get member :state))
                    (agent-shell-workflow--workspace-ready-p
                     (agent-shell-workflow-operation-read operation-id) target-id))
           (condition-case failure
               (let* ((args (copy-tree (plist-get keys :args)))
                      (id (intern (plist-get operation :workflow-id))))
                 (agent-shell-workflow--workspace-transition operation-id target-id "preflight")
                 (agent-shell-workflow--workspace-preflight
                  target (eq (plist-get operation :mutating) t))
                 (when (and (eq id 'commit-group)
                            (plist-get (agent-shell-workflow--workspace-target
                                        (plist-get target :worktree-dir)) :unmerged))
                   (user-error "Commit preparation cannot start with unresolved conflicts"))
                 (when (eq id 'repair-conflicts)
                   (let* ((directory (plist-get target :worktree-dir))
                          (details (plist-get member :details))
                          (merge-head (or (plist-get details :merge-head)
                                          (agent-shell-workflow--workspace-git
                                           directory "rev-parse" "--verify"
                                           (concat (plist-get args :merge-ref) "^{commit}")))))
                     (if (plist-get details :merge-started)
                         (unless (equal merge-head
                                        (agent-shell-workflow--workspace-git
                                         directory "rev-parse" "MERGE_HEAD"))
                           (user-error "Recorded merge changed; inspect the checkout"))
                       (agent-shell-workflow--workspace-transition
                        operation-id target-id "preflight"
                        (list :merge-started t :merge-head merge-head))
                       (agent-shell-workflow--workspace-merge
                        target (plist-put args :merge-ref merge-head)))))
                 (push target ready))
             (error
              (agent-shell-workflow--workspace-transition
               operation-id target-id "blocked"
               (list :error (error-message-string failure))))))))
     targets)
    (setq ready (nreverse ready))
    (if (plist-get keys :together)
        (when ready (agent-shell-workflow--workspace-dispatch-targets operation ready keys))
      (let ((shown nil)
            (display (agent-shell-workflow--resolve-display
                      (plist-get keys :buffer-display) (plist-get keys :display)
                      (when-let* ((spec (agent-shell-workflow-get
                                         (intern (plist-get operation :workflow-id)))))
                        (agent-shell-workflow-spec-buffer-display spec)))))
        (seq-do (lambda (target)
                  (let* ((member-keys (copy-tree keys))
                         (member-display (cond ((eq display :background) :background)
                                               (shown :background)
                                               ((eq display :foreground) :display)
                                               (t display))))
                    (setq member-keys (plist-put member-keys :buffer-display member-display))
                    (setq shown t)
                    (agent-shell-workflow--workspace-dispatch-targets
                     operation (list target) member-keys))) ready)))
    (let ((current (agent-shell-workflow-operation-read operation-id)))
      (seq-do
       (lambda (target)
         (let* ((target-id (plist-get target :id))
                (member (agent-shell-workflow--workspace-member current target-id))
                (dependency (seq-find
                             (lambda (entry) (equal target-id (plist-get entry :target-id)))
                             (plist-get current :dependencies)))
                (failed (seq-find
                         (lambda (required)
                           (member (plist-get (agent-shell-workflow--workspace-member current required)
                                              :state) '("blocked" "failed" "cancelled")))
                         (plist-get dependency :requires))))
           (when (and failed (equal "pending" (plist-get member :state)))
             (agent-shell-workflow--workspace-transition
              operation-id target-id "blocked" (list :dependency failed
                                                     :error "Prerequisite did not succeed")))))
       targets))))

(defun agent-shell-workflow--workspace-dispatch-targets (operation selected keys)
  "Dispatch one agent turn for OPERATION's SELECTED targets using KEYS."
  (let* ((operation-id (plist-get operation :operation-id))
         (targets (plist-get operation :targets))
         (primary (car selected))
         (token (secure-hash 'sha256 (format "%s%s%s" operation-id selected (float-time))))
         (args (copy-tree (plist-get keys :args))))
    (unless (seq-every-p (lambda (target)
                           (equal (plist-get primary :shell-dir) (plist-get target :shell-dir))) selected)
      (user-error "Together dispatch requires the same shell resource for all targets"))
    (setq args (plist-put args :dispatch-token token))
    (setq args (plist-put args :workspace-manifest (prin1-to-string targets)))
    (setq args (plist-put args :workspace-target-ids
                          (seq-map (lambda (target) (plist-get target :id)) selected)))
    (seq-do (lambda (target)
              (agent-shell-workflow--workspace-transition
               operation-id (plist-get target :id) "running"
               (list :dispatch-token token :turn-complete nil :error nil))) selected)
    (condition-case failure
        (apply #'agent-shell-workflow-dispatch (intern (plist-get operation :workflow-id))
               :args args :context-dir (plist-get primary :worktree-dir)
               :shell-dir (plist-get primary :shell-dir)
               :repos (seq-map (lambda (target) (plist-get target :worktree-dir)) targets)
               :operation-id operation-id :target-id (plist-get primary :id)
               :manifest targets :target (or (plist-get keys :target) :session-new)
               :operation-generation
               (plist-get (agent-shell-workflow-operation-read operation-id) :generation)
               (seq-mapcat
                (lambda (key)
                  (when (plist-member keys key) (list key (plist-get keys key))))
                '(:submit :shell-reuse :providers :new-shell-providers
                          :buffer-display :display)))
      (error
       (seq-do (lambda (target)
                 (let ((member (agent-shell-workflow--workspace-member
                                (agent-shell-workflow-operation-read operation-id)
                                (plist-get target :id))))
                   (when (equal "running" (plist-get member :state))
                     (agent-shell-workflow--workspace-transition
                      operation-id (plist-get target :id) "blocked"
                      (list :error (error-message-string failure)))))) selected)))))

(defun agent-shell-workflow--workspace-merge (target args)
  "Inspect and perform the requested merge in TARGET using ARGS."
  (let ((directory (plist-get target :worktree-dir))
        (reference (plist-get args :merge-ref)))
    (when (plist-get (agent-shell-workflow--workspace-target directory) :dirty)
      (user-error "Merge requires a clean checkout: %s" directory))
    (agent-shell-workflow--workspace-git directory "rev-parse" "--verify"
                                         (concat reference "^{commit}"))
    (let ((default-directory directory))
      (with-temp-buffer
        (let ((status (process-file "git" nil t nil "merge" "--no-commit" "--no-ff"
                                    "--" reference)))
          (unless (or (equal status 0)
                      (not (string-empty-p
                            (agent-shell-workflow--workspace-git directory "ls-files" "-u"))))
            (error "Merge failed: %s" (buffer-string))))))))

(defun agent-shell-workflow--workspace-action-post (buffer ctx response)
  "Verify CTX after RESPONSE from BUFFER and record actual Git results."
  (let* ((operation (agent-shell-workflow-operation-read (plist-get ctx :operation-id)))
         (ids (or (plist-get (plist-get ctx :args) :workspace-target-ids)
                  (list (plist-get ctx :target-id)))))
    (seq-do
     (lambda (target-id)
       (let* ((target (seq-find (lambda (entry) (equal target-id (plist-get entry :id)))
                                (plist-get operation :targets)))
              (member-ctx (copy-tree ctx)))
         (setq member-ctx (plist-put member-ctx :target-id target-id))
         (setq member-ctx (plist-put member-ctx :context-dir (plist-get target :worktree-dir)))
         (agent-shell-workflow--workspace-verify-member buffer member-ctx response))) ids)
    (when (and (buffer-live-p buffer)
               (equal (car (buffer-local-value 'agent-shell-workflow--workspace-active-turn buffer))
                      (plist-get ctx :operation-id)))
      (with-current-buffer buffer (setq agent-shell-workflow--workspace-active-turn nil))))
  :done)

(defun agent-shell-workflow--workspace-verify-member (_buffer ctx _response)
  "Verify one CTX checkout and record a deterministic commit when requested."
  (let* ((operation-id (plist-get ctx :operation-id))
         (target-id (plist-get ctx :target-id))
         (operation (agent-shell-workflow-operation-read operation-id))
         (member (agent-shell-workflow--workspace-member operation target-id))
         (args (plist-get ctx :args))
         (token (plist-get args :dispatch-token))
         (directory (plist-get ctx :context-dir))
         (id (intern (plist-get operation :workflow-id))))
    (when (and (member (plist-get member :state) '("running" "verifying"))
               (equal token (plist-get (plist-get member :details) :dispatch-token)))
      (condition-case failure
          (progn
            (agent-shell-workflow--workspace-transition
             operation-id target-id "verifying" '(:turn-complete t))
            (agent-shell-workflow--workspace-preflight
             (seq-find (lambda (target) (equal target-id (plist-get target :id)))
                       (plist-get operation :targets))
             (eq (plist-get operation :mutating) t))
            (when (eq (plist-get operation :mutating) t)
              (agent-shell-workflow--workspace-process directory (plist-get args :verify-command))
              (agent-shell-workflow--workspace-preflight
               (seq-find (lambda (target) (equal target-id (plist-get target :id)))
                         (plist-get operation :targets)) t))
            (when (memq id '(commit-group repair-conflicts))
              (unless (string-empty-p
                       (agent-shell-workflow--workspace-git directory "ls-files" "-u"))
                (user-error "Unresolved merge conflicts remain"))
              (when (eq id 'commit-group)
                (unless (string-empty-p
                         (agent-shell-workflow--workspace-git directory "diff" "--cached" "--name-only"))
                  (user-error "Index must be empty before explicit staging"))
                (apply #'agent-shell-workflow--workspace-git directory "add" "--"
                       (plist-get args :files)))
              (agent-shell-workflow--workspace-transition
               operation-id target-id "verifying"
               (list :commit-intent t :starting-head
                     (agent-shell-workflow--workspace-git directory "rev-parse" "HEAD")))
              (agent-shell-workflow--workspace-git directory "commit" "-m"
                                                   (plist-get args :commit-message))
              (agent-shell-workflow--workspace-transition
               operation-id target-id "committed"
               (list :commits (list (agent-shell-workflow--workspace-git
                                     directory "rev-parse" "HEAD")))))
            (agent-shell-workflow--workspace-transition
             operation-id target-id "succeeded"
             (list :observed-head (agent-shell-workflow--workspace-git directory "rev-parse" "HEAD")
                   :status (agent-shell-workflow--workspace-git directory "status" "--short")))
            (agent-shell-workflow--workspace-schedule operation-id))
        (error
         (agent-shell-workflow--workspace-transition
          operation-id target-id "blocked" (list :error (error-message-string failure)))))))
  :done)

(defun agent-shell-workflow--workspace-cancel (operation)
  "Stop OPERATION's dedicated shells after durable cancellation."
  (when (and (boundp 'agent-shell-queue--store)
             agent-shell-queue--store
             (fboundp 'agent-shell-queue-store-items))
    (seq-do
     (lambda (bucket)
       (seq-do
        (lambda (item)
          (condition-case nil
              (let* ((raw (agent-shell-queue-item-args item))
                     (payload (if (stringp raw) (car (read-from-string raw)) raw)))
                (when (equal (plist-get operation :operation-id)
                             (plist-get (plist-get payload :ctx) :operation-id))
                  (agent-shell-queue-remove (agent-shell-queue-item-id item))))
            (error nil)))
        (copy-sequence (cdr bucket))))
     (copy-tree (agent-shell-queue-store-items agent-shell-queue--store))))
  (seq-do (lambda (owned)
            (let ((buffer (plist-get owned :buffer)))
              (when (and (buffer-live-p buffer)
                         (equal (buffer-local-value 'agent-shell-workflow--workspace-active-turn buffer)
                                (list (plist-get operation :operation-id)
                                      (plist-get owned :token) (plist-get owned :target-ids)))
                         (seq-some
                          (lambda (target-id)
                            (equal "cancelled" (plist-get
                                                (agent-shell-workflow--workspace-member operation target-id)
                                                :state)))
                          (plist-get owned :target-ids)))
                (with-current-buffer buffer (agent-shell-interrupt t)))))
          (map-elt agent-shell-workflow--workspace-owned-shells
                   (plist-get operation :operation-id)))
  (setf (map-elt agent-shell-workflow--workspace-owned-shells
                 (plist-get operation :operation-id)) nil)
  (setf (map-elt agent-shell-workflow--workspace-dispatches
                 (plist-get operation :operation-id)) nil))

;;;###autoload
(defun agent-shell-workflow-workspace-preview (directories &optional workspace)
  "Inspect exact DIRECTORIES under WORKSPACE without starting an agent shell."
  (let* ((snapshot (agent-shell-workflow-resolve-workspace workspace directories))
         (operation (agent-shell-workflow-operation-create 'workspace-inspect snapshot))
         (operation-id (plist-get operation :operation-id)))
    (seq-do
     (lambda (target)
       (let ((target-id (plist-get target :id)))
         (condition-case failure
             (progn
               (agent-shell-workflow--workspace-transition operation-id target-id "preflight")
               (agent-shell-workflow--workspace-preflight target nil)
               (agent-shell-workflow--workspace-transition operation-id target-id "running")
               (agent-shell-workflow--workspace-transition operation-id target-id "verifying")
               (agent-shell-workflow--workspace-transition
                operation-id target-id "succeeded"
                (list :status (agent-shell-workflow--workspace-git
                               (plist-get target :worktree-dir) "status" "--short"))))
           (error
            (agent-shell-workflow--workspace-transition
             operation-id target-id "blocked" (list :error (error-message-string failure)))))))
     (plist-get snapshot :targets))
    (agent-shell-workflow-operation-read operation-id)))

;;;###autoload
(defun agent-shell-workflow-workspace-resume (operation-id &rest keys)
  "Reconcile OPERATION-ID, optionally retrying explicit :retry-targets in KEYS.
Without :retry-targets, only observations are refreshed.  Retries require
the original checkout identity and HEAD, fresh :args, and a completed or
never-started turn.  Recorded commits and uncertain commit intents are
never replayed; use the status view to inspect those members."
  (let* ((operation (agent-shell-workflow-operation-resume operation-id))
         (retry (plist-get keys :retry-targets))
         (mutating (eq (plist-get operation :mutating) t))
         (args (plist-get keys :args)))
    (when retry
      (when (and mutating (plist-get keys :target)
                 (not (eq (plist-get keys :target) :session-new)))
        (user-error "Mutation retries require :target :session-new for owned cancellation"))
      (unless (and (listp retry) (seq-every-p #'stringp retry))
        (user-error "Retry targets must be a list of frozen target IDs"))
      (when (and mutating
                 (not (and (consp (plist-get args :verify-command))
                           (seq-every-p #'stringp (plist-get args :verify-command)))))
        (user-error "Mutation retry requires explicit :verify-command"))
      (when (member (plist-get operation :workflow-id) '("commit-group" "repair-conflicts"))
        (unless (and (stringp (plist-get args :commit-message))
                     (not (string-empty-p (plist-get args :commit-message))))
          (user-error "Commit retry requires :commit-message")))
      (when (and (equal (plist-get operation :workflow-id) "commit-group")
                 (not (and (consp (plist-get args :files))
                           (seq-every-p #'stringp (plist-get args :files)))))
        (user-error "Commit retry requires explicit :files"))
      (seq-do
       (lambda (target-id)
         (let* ((member (agent-shell-workflow--workspace-member operation target-id))
                (details (plist-get member :details))
                (target (seq-find (lambda (entry) (equal target-id (plist-get entry :id)))
                                  (plist-get operation :targets))))
           (unless (and member
                        (member (plist-get member :state) '("pending" "blocked" "failed"))
                        (not (plist-get member :commits))
                        (not (plist-get details :commit-intent))
                        (or (not (plist-get details :dispatch-token))
                            (plist-get details :turn-complete)))
             (user-error "Target %s has completed or uncertain work; inspect before retry" target-id))
           (agent-shell-workflow--workspace-preflight target mutating)
           (when (equal (plist-get operation :workflow-id) "repair-conflicts")
             (unless (and (plist-get details :merge-started)
                          (equal (plist-get details :merge-head)
                                 (agent-shell-workflow--workspace-git
                                  (plist-get target :worktree-dir) "rev-parse" "MERGE_HEAD")))
               (user-error "Retry requires the previously recorded merge"))))) retry)
      (setq keys (plist-put keys :resume-targets retry))
      (setf (map-elt agent-shell-workflow--workspace-dispatches operation-id)
            (copy-tree keys))
      (agent-shell-workflow-operations-setup)
      (add-hook 'agent-shell-workflow-operation-cancel-functions
                #'agent-shell-workflow--workspace-cancel)
      (seq-do
       (lambda (target-id)
         (agent-shell-workflow--workspace-transition operation-id target-id "preflight")) retry)
      (seq-do
       (lambda (target-id)
         (let ((record (agent-shell-workflow-operation-read operation-id)))
           (let ((target (seq-find (lambda (entry) (equal target-id (plist-get entry :id)))
                                   (plist-get record :targets))))
             (if (agent-shell-workflow--workspace-ready-p record target-id)
                 (agent-shell-workflow--workspace-dispatch-targets record (list target) keys)
               (agent-shell-workflow--workspace-transition
                operation-id target-id "blocked" '(:reason "Prerequisite has not succeeded")))))) retry))
    (agent-shell-workflow-operation-read operation-id)))

(provide 'agent-shell-workflow-workspace-actions)
;;; agent-shell-workflow-workspace-actions.el ends here
