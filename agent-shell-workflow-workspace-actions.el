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
         (mutating (memq id '(commit-group repair-conflicts)))
         (arguments (plist-get keys :args)))
    (when (plist-get keys :together)
      (unless (= 1 (thread-last (plist-get snapshot :targets)
                     (seq-map (lambda (target) (plist-get target :shell-dir)))
                     seq-uniq length))
        (user-error "Together dispatch requires one explicit shared shell resource")))
    (agent-shell-workflow-operations-setup)
    (add-hook 'agent-shell-workflow-operation-cancel-functions
              #'agent-shell-workflow--workspace-cancel)
    (when mutating
      (unless (and (consp (plist-get arguments :verify-command))
                   (seq-every-p #'stringp (plist-get arguments :verify-command))
                   (stringp (plist-get arguments :commit-message))
                   (not (string-empty-p (plist-get arguments :commit-message))))
        (user-error "Mutation needs :verify-command list and :commit-message"))
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
         (when (and (equal "pending" (plist-get member :state))
                    (agent-shell-workflow--workspace-ready-p
                     (agent-shell-workflow-operation-read operation-id) target-id))
           (condition-case failure
               (let* ((args (copy-tree (plist-get keys :args)))
                      (id (intern (plist-get operation :workflow-id))))
                 (agent-shell-workflow--workspace-transition operation-id target-id "preflight")
                 (agent-shell-workflow--workspace-preflight
                  target (memq id '(commit-group repair-conflicts)))
                 (when (and (eq id 'commit-group) (plist-get target :unmerged))
                   (user-error "Commit preparation cannot start with unresolved conflicts"))
                 (when (eq id 'repair-conflicts)
                   (agent-shell-workflow--workspace-merge target args))
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
         (args (copy-tree (plist-get keys :args)))
         (before (when (fboundp 'agent-shell-buffers) (agent-shell-buffers))))
    (unless (seq-every-p (lambda (target)
                          (equal (plist-get primary :shell-dir) (plist-get target :shell-dir))) selected)
      (user-error "Together dispatch requires the same shell resource for all targets"))
    (setq args (plist-put args :dispatch-token token))
    (setq args (plist-put args :workspace-manifest (prin1-to-string targets)))
    (setq args (plist-put args :workspace-target-ids
                         (seq-map (lambda (target) (plist-get target :id)) selected)))
    (seq-do (lambda (target)
              (agent-shell-workflow--workspace-transition
               operation-id (plist-get target :id) "running" (list :dispatch-token token))) selected)
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
                      (list :error (error-message-string failure)))))) selected)))
    (when (fboundp 'agent-shell-buffers)
      (setf (map-elt agent-shell-workflow--workspace-owned-shells operation-id)
            (append (seq-difference (agent-shell-buffers) before)
                    (map-elt agent-shell-workflow--workspace-owned-shells operation-id))))))

(defun agent-shell-workflow--workspace-merge (target args)
  "Inspect and perform the requested merge in TARGET using ARGS."
  (let ((directory (plist-get target :worktree-dir))
        (reference (plist-get args :merge-ref)))
    (when (plist-get target :dirty)
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
  "Verify CTX's checkout after an agent turn and record actual Git results."
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
         (agent-shell-workflow--workspace-verify-member buffer member-ctx response))) ids))
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
            (agent-shell-workflow--workspace-transition operation-id target-id "verifying")
            (unless (memq id '(commit-group repair-conflicts))
              (agent-shell-workflow--workspace-preflight
               (seq-find (lambda (target) (equal target-id (plist-get target :id)))
                         (plist-get operation :targets)) nil))
            (when (memq id '(commit-group repair-conflicts))
              (unless (string-empty-p
                       (agent-shell-workflow--workspace-git directory "ls-files" "-u"))
                (user-error "Unresolved merge conflicts remain"))
              (agent-shell-workflow--workspace-process directory (plist-get args :verify-command))
              (let ((target (seq-find
                             (lambda (entry) (equal target-id (plist-get entry :id)))
                             (plist-get operation :targets))))
                (agent-shell-workflow--workspace-preflight target t))
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
  (seq-do (lambda (buffer)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (agent-shell-interrupt t))))
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

(provide 'agent-shell-workflow-workspace-actions)
;;; agent-shell-workflow-workspace-actions.el ends here
