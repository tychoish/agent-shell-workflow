;;; agent-shell-workflow-operations.el --- Durable workspace operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Frozen checkout manifests, member state and exclusive mutation ownership.
;; Queue completion is evidence that verification may start, never Git success.

;;; Code:

(require 'agent-shell-workflow-workspaces)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function sprite-state-path "sprite" (name))
(declare-function agent-shell-queue-item-args "agent-shell-queue-core" (item))

(defcustom agent-shell-workflow-operation-directory nil
  "Operation storage directory, or nil to use `sprite-state-path'."
  :type '(choice (const nil) directory)
  :group 'agent-shell-workflow)

(defvar agent-shell-workflow-operation-update-functions nil
  "Functions called with operation ID, target ID, old state, new state, details.
Notifications follow durable commit.  A notification error is logged and cannot
roll back that commit; consumers must reconcile from the stored generation.")

(defvar agent-shell-workflow-operation-cancel-functions nil
  "Functions called with the durable cancelled operation.
Schedulers use this to stop only their owned queue items and processes.")

(defvar agent-shell-queue-item-done-hook)

(defconst agent-shell-workflow--operation-transitions
  '(("pending" "preflight" "blocked" "failed" "cancelled")
    ("preflight" "running" "blocked" "failed" "cancelled")
    ("running" "verifying" "blocked" "failed" "cancelled")
    ("verifying" "committed" "succeeded" "blocked" "failed" "cancelled")
    ("committed" "waiting-ci" "succeeded" "blocked" "failed" "cancelled")
    ("waiting-ci" "succeeded" "blocked" "failed" "cancelled")
    ("blocked" "preflight" "failed" "cancelled")
    ("failed" "preflight" "blocked" "cancelled"))
  "Allowed member state transitions.  Success and cancellation are final.")

(defun agent-shell-workflow--operation-directory ()
  "Return and create the local operation storage directory."
  (let ((directory (or agent-shell-workflow-operation-directory
                       (if (fboundp 'sprite-state-path)
                           (sprite-state-path "workflow-operations/")
                         (user-error "Load sprite or set operation-directory")))))
    (when (file-remote-p directory)
      (user-error "Operation storage must be local"))
    (make-directory directory t)
    (file-name-as-directory (expand-file-name directory))))

(defun agent-shell-workflow--operation-path (id &optional suffix)
  "Return storage path for ID and optional SUFFIX."
  (unless (and (stringp id) (string-match-p "\\`[[:alnum:]-]+\\'" id))
    (user-error "Invalid operation ID: %S" id))
  (expand-file-name (concat id (or suffix ".json"))
                    (agent-shell-workflow--operation-directory)))

(defun agent-shell-workflow--operation-json-data (value)
  "Convert plain Lisp VALUE to JSON data, rejecting live objects."
  (cond
   ((or (null value) (eq value t) (eq value :false)
        (stringp value) (numberp value)) value)
   ((and (listp value) (keywordp (car value)))
    (unless (zerop (% (length value) 2))
      (user-error "Malformed operation plist: %S" value))
    (let ((object (make-hash-table :test #'equal)))
      (seq-do (lambda (pair)
                (unless (keywordp (car pair))
                  (user-error "Operation object keys must be keywords"))
                (setf (map-elt object (substring (symbol-name (car pair)) 1))
                      (agent-shell-workflow--operation-json-data (cadr pair))))
              (seq-partition value 2))
      object))
   ((or (listp value) (vectorp value))
    (vconcat (seq-map #'agent-shell-workflow--operation-json-data value)))
   (t (user-error "Operation data is not JSON-safe: %S" value))))

(defun agent-shell-workflow--operation-write (path value)
  "Atomically replace PATH with plain JSON VALUE."
  (let* ((encoded (json-serialize
                   (agent-shell-workflow--operation-json-data value)
                   :null-object nil :false-object :false))
         (temporary (make-temp-file (concat path ".tmp-")))
         (coding-system-for-write 'utf-8-unix)
         (write-region-inhibit-fsync nil))
    (unwind-protect
        (progn
          (set-file-modes temporary #o600)
          (with-temp-file temporary (insert encoded))
          (rename-file temporary path t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun agent-shell-workflow--operation-read-json (path)
  "Read a plain JSON plist from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-buffer :object-type 'plist :array-type 'list
                       :null-object nil :false-object :false)))

(defun agent-shell-workflow-operation-read (id)
  "Read the durable operation ID as fresh plain data."
  (let ((operation (agent-shell-workflow--operation-read-json
                    (agent-shell-workflow--operation-path id))))
    (unless (and (equal (plist-get operation :version) 1)
                 (equal (plist-get operation :operation-id) id)
                 (integerp (plist-get operation :generation)))
      (user-error "Invalid or unsupported operation manifest: %s" id))
    operation))

(defun agent-shell-workflow--operation-with-lock (id function)
  "Call FUNCTION under ID's exclusive on-disk write lock."
  (let* ((lock (agent-shell-workflow--operation-path id ".lock"))
         (token (list :host (system-name) :pid (emacs-pid)
                       :token (secure-hash 'sha256
                                           (format "%s:%s:%s" id (float-time) (random)))))
         acquired)
    (unwind-protect
        (progn
          (condition-case nil
              (progn (make-directory lock) (setq acquired t))
            (file-already-exists
             (user-error "Operation %s has an active or recoverable writer lock" id)))
          (agent-shell-workflow--operation-write
           (expand-file-name "owner.json" lock) token)
          (funcall function))
      (when acquired (delete-directory lock t)))))

(defun agent-shell-workflow-operation-recover-write-lock (id expected-generation)
  "Recover ID's abandoned local write lock at EXPECTED-GENERATION.
Only a recorded owner on this host whose PID is dead can be recovered.
Foreign-host and incomplete ownership records require operator inspection."
  (let* ((lock (agent-shell-workflow--operation-path id ".lock"))
         (owner (agent-shell-workflow--operation-read-json
                 (expand-file-name "owner.json" lock))))
    (unless (and (equal (plist-get owner :host) (system-name))
                 (integerp (plist-get owner :pid))
                 (not (process-attributes (plist-get owner :pid)))
                 (= expected-generation
                    (plist-get (agent-shell-workflow-operation-read id) :generation)))
      (user-error "Cannot prove operation %s writer is abandoned" id))
    (delete-directory lock t)))

(defun agent-shell-workflow--operation-lease-path (target)
  "Return the checkout lease path for TARGET across all operations."
  (expand-file-name
   (concat "asw-checkout-" (secure-hash 'sha256 (plist-get target :worktree-dir)) ".lock")
   (or (plist-get target :common-dir)
       (agent-shell-workflow--operation-directory))))

(defun agent-shell-workflow--operation-acquire-lease (id target)
  "Acquire TARGET's mutation lease for ID, returning ownership success."
  (let* ((path (agent-shell-workflow--operation-lease-path target))
         (guard (concat path ".guard"))
         (owner-file (expand-file-name "owner.json" path)))
    (condition-case nil
        (make-directory guard)
      (file-already-exists (user-error "Checkout lease is being reconciled: %s" path)))
    (unwind-protect
        (condition-case nil
            (progn
              (make-directory path)
              (agent-shell-workflow--operation-write
               owner-file (list :operation-id id :target-id (plist-get target :id)
                                :host (system-name) :pid (emacs-pid)
                                :storage-directory (agent-shell-workflow--operation-directory)))
              t)
          (file-already-exists
           (when (file-exists-p owner-file)
             (let ((owner (agent-shell-workflow--operation-read-json owner-file)))
               (and (equal id (plist-get owner :operation-id))
                    (equal (agent-shell-workflow--operation-directory)
                           (plist-get owner :storage-directory)))))))
      (delete-directory guard))))

(defun agent-shell-workflow--operation-release-lease (id target)
  "Release TARGET's lease only when owned by ID."
  (let* ((path (agent-shell-workflow--operation-lease-path target))
         (guard (concat path ".guard"))
         (owner-file (expand-file-name "owner.json" path)))
    (condition-case nil
        (make-directory guard)
      (file-already-exists (user-error "Checkout lease is being reconciled: %s" path)))
    (unwind-protect
        (when (file-exists-p owner-file)
          (let ((owner (agent-shell-workflow--operation-read-json owner-file)))
            (when (and (equal id (plist-get owner :operation-id))
                       (equal (agent-shell-workflow--operation-directory)
                              (plist-get owner :storage-directory)))
              (delete-directory path t))))
      (delete-directory guard))))

(defun agent-shell-workflow-operation-inspect-checkout-lease (directory)
  "Inspect DIRECTORY's checkout lease without changing ownership.
Return its :path, :owner, :exists and :owner-missing fields.  An absent
owner file may indicate a crash during acquisition; it is never reclaimed
automatically.  DIRECTORY must still resolve to its original Git checkout."
  (let* ((target (agent-shell-workflow--workspace-target directory))
         (path (agent-shell-workflow--operation-lease-path target))
         (owner-file (expand-file-name "owner.json" path)))
    (unless (plist-get target :common-dir)
      (user-error "Checkout must resolve before inspecting its lease: %s" directory))
    (list :path path :exists (file-directory-p path)
          :owner-missing (and (file-directory-p path) (not (file-exists-p owner-file)))
          :owner (and (file-exists-p owner-file)
                      (agent-shell-workflow--operation-read-json owner-file)))))

(defun agent-shell-workflow-operation-release-checkout-lease
    (directory expected-owner-id &optional quiescent-confirmed)
  "Explicitly release DIRECTORY's abandoned or cancelled checkout lease.
QUIESCENT-CONFIRMED must be t: the caller confirms all owned agent turns and
subprocesses have stopped.  EXPECTED-OWNER-ID must match the inspected owner;
nil acknowledges an ownerless orphan.  Refuse foreign hosts, live unrecorded
owners, and any recorded member outside succeeded, cancelled or failed.
No Git data, worktree, shell or operation manifest is changed."
  (unless (eq quiescent-confirmed t)
    (user-error "Confirm owned agent turns and processes are stopped before release"))
  (let* ((inspection (agent-shell-workflow-operation-inspect-checkout-lease directory))
         (path (plist-get inspection :path))
         (guard (concat path ".guard")))
    (condition-case nil
        (make-directory guard)
      (file-already-exists (user-error "Checkout lease has an active recovery guard")))
    (unwind-protect
        (let* ((current (agent-shell-workflow-operation-inspect-checkout-lease directory))
               (owner (plist-get current :owner))
               (id (plist-get owner :operation-id))
               (storage (plist-get owner :storage-directory)))
          (unless (and (plist-get current :exists) (equal expected-owner-id id))
            (user-error "Checkout lease ownership changed; inspect it again"))
          (when owner
            (unless (and (equal (plist-get owner :host) (system-name))
                         (integerp (plist-get owner :pid))
                         (stringp storage) (stringp id))
              (user-error "Cannot recover foreign-host or incomplete ownership metadata"))
            (let* ((agent-shell-workflow-operation-directory storage)
                   (manifest (agent-shell-workflow--operation-path id)))
              (if (file-exists-p manifest)
                  (agent-shell-workflow--operation-with-lock
                   id
                   (lambda ()
                     (let* ((operation (agent-shell-workflow-operation-read id))
                            (member (seq-find
                                     (lambda (entry)
                                       (equal (plist-get owner :target-id)
                                              (plist-get entry :target-id)))
                                     (plist-get operation :members))))
                       (unless (member (plist-get member :state)
                                       '("succeeded" "failed" "cancelled"))
                         (user-error "Recorded checkout owner is still active"))
                       (delete-directory path t))))
                (when (process-attributes (plist-get owner :pid))
                  (user-error "Unrecorded checkout owner process is still alive"))
                (delete-directory path t))))
          (unless owner (delete-directory path t))
          t)
      (delete-directory guard))))

(defun agent-shell-workflow--operation-check-dependencies (targets dependencies)
  "Validate TARGETS and dependency DAG in DEPENDENCIES without side effects."
  (let ((ids (seq-map (lambda (target) (plist-get target :id)) targets))
        (pending (copy-tree dependencies)))
    (seq-do
     (lambda (entry)
       (unless (and (member (plist-get entry :target-id) ids)
                    (seq-every-p (lambda (required) (member required ids))
                                 (plist-get entry :requires)))
         (user-error "Dependencies reference unknown operation targets")))
     dependencies)
    (seq-do
     (lambda (_target)
       (let ((blocked-ids (seq-map (lambda (entry) (plist-get entry :target-id))
                                    pending)))
         (setq pending
               (seq-filter
                (lambda (entry)
                  (seq-some (lambda (required) (member required blocked-ids))
                            (plist-get entry :requires))) pending))))
     targets)
    (when pending (user-error "Cyclic operation target dependencies"))))

(defun agent-shell-workflow-operation-create
    (workflow-id snapshot &optional mutating dependencies)
  "Persist WORKFLOW-ID with frozen SNAPSHOT and return its operation.
MUTATING reserves each checkout across groups.  DEPENDENCIES is plain
JSON data describing member dependencies; the scheduler interprets it.
Unavailable targets and lease conflicts remain explicit blocked members."
  (let* ((id (concat (format-time-string "%Y%m%dT%H%M%S-")
                     (substring (secure-hash 'sha256
                                              (format "%s:%s:%s" (emacs-pid)
                                                      (float-time) (random))) 0 16)))
         (targets (copy-tree (plist-get snapshot :targets)))
         (operation (list :version 1 :operation-id id :generation 0
                           :workspace (when (plist-get snapshot :workspace)
                                        (format "%s" (plist-get snapshot :workspace)))
                           :workflow-id (if (symbolp workflow-id)
                                            (symbol-name workflow-id) workflow-id)
                           :created-at (format-time-string "%FT%TZ" nil t)
                           :mutating (if mutating t :false)
                           :targets targets :dependencies (copy-tree dependencies)))
         committed)
    (unless (and targets (stringp (plist-get operation :workflow-id))
                 (seq-every-p (lambda (target)
                                (and (stringp (plist-get target :id))
                                     (stringp (plist-get target :worktree-dir)))) targets)
                 (= (length targets)
                    (length (seq-uniq (seq-map (lambda (target) (plist-get target :id))
                                                targets)))))
      (user-error "Operation requires a workflow and distinct target snapshots"))
    (agent-shell-workflow--operation-json-data operation)
    (agent-shell-workflow--operation-check-dependencies targets dependencies)
    (unwind-protect
        (progn
          (setq operation
                (plist-put operation :members
                           (seq-map
                            (lambda (target)
                              (let ((reason
                                     (or (plist-get target :preflight-error)
                                         (and mutating (not (plist-get target :branch))
                                              "Mutation requires an attached branch")
                                         (and mutating
                                              (not (agent-shell-workflow--operation-acquire-lease
                                                    id target))
                                              "Checkout owned by another operation"))))
                                (list :target-id (plist-get target :id)
                                      :state (if reason "blocked" "pending")
                                      :details (and reason (list :reason reason))
                                      :commits nil))) targets)))
          (agent-shell-workflow--operation-with-lock
           id (lambda ()
                (agent-shell-workflow--operation-write
                 (agent-shell-workflow--operation-path id) operation)))
          (setq committed t)
          (copy-tree operation))
      (unless committed
        (seq-do (lambda (target)
                  (agent-shell-workflow--operation-release-lease id target)) targets)))))

(defun agent-shell-workflow--operation-notify (id target-id old-state new-state details)
  "Notify observers of durable ID TARGET-ID state and DETAILS."
  (seq-do (lambda (function)
            (condition-case failure
                (funcall function id target-id old-state new-state (copy-tree details))
              (error (message "Operation %s notification failed: %s"
                              id (error-message-string failure)))))
          agent-shell-workflow-operation-update-functions))

(defun agent-shell-workflow-operation-update
    (id target-id expected-generation new-state &optional details)
  "Commit TARGET-ID's NEW-STATE in ID with DETAILS at EXPECTED-GENERATION.
Reject stale callbacks, invalid transitions and live/non-JSON details.
Commit hashes in DETAILS's :commits are retained separately on the member.
Notifications run only after the atomic write and lock release."
  (agent-shell-workflow--operation-json-data details)
  (let (old-state)
    (let ((operation
           (agent-shell-workflow--operation-with-lock
            id
            (lambda ()
              (let* ((record (agent-shell-workflow-operation-read id))
                     (member (seq-find
                              (lambda (entry) (equal target-id (plist-get entry :target-id)))
                              (plist-get record :members)))
                     (target (seq-find
                              (lambda (entry) (equal target-id (plist-get entry :id)))
                              (plist-get record :targets))))
                (unless (and (integerp expected-generation)
                             (= expected-generation (plist-get record :generation)))
                  (user-error "Stale operation generation for %s" id))
                (unless member (user-error "Unknown operation target: %s" target-id))
                (setq old-state (plist-get member :state))
                (unless (or (equal old-state new-state)
                            (member new-state (cdr (assoc old-state
                                                        agent-shell-workflow--operation-transitions))))
                  (user-error "Invalid operation transition: %s -> %s" old-state new-state))
                (when (and (equal new-state "committed")
                           (not (and (plist-get details :commits)
                                     (seq-every-p #'stringp (plist-get details :commits)))))
                  (user-error "Committed state requires recorded commit hashes"))
                (when (and (equal new-state "preflight")
                           (eq (plist-get record :mutating) t)
                           (not (agent-shell-workflow--operation-acquire-lease id target)))
                  (user-error "Checkout is owned by another operation"))
                (setf (plist-get member :state) new-state)
                (let ((merged (copy-tree (plist-get member :details))))
                  (seq-do (lambda (pair)
                            (setq merged (plist-put merged (car pair) (cadr pair))))
                          (seq-partition details 2))
                  (setf (plist-get member :details) merged))
                (when (and (equal new-state "committed")
                           (plist-get details :commits))
                  (setf (plist-get member :commits)
                        (seq-uniq (append (plist-get member :commits)
                                          (copy-sequence (plist-get details :commits))))))
                (setf (plist-get record :generation) (1+ expected-generation))
                (agent-shell-workflow--operation-write
                 (agent-shell-workflow--operation-path id) record)
                (when (member new-state '("succeeded" "failed" "cancelled"))
                  (agent-shell-workflow--operation-release-lease id target))
                record)))))
      (agent-shell-workflow--operation-notify id target-id old-state new-state details)
      operation)))

(defun agent-shell-workflow-operation-status (operation)
  "Derive group status from every member of OPERATION."
  (let ((states (seq-map (lambda (member) (plist-get member :state))
                          (plist-get operation :members))))
    (cond
     ((seq-every-p (lambda (state) (equal state "succeeded")) states) "succeeded")
     ((seq-every-p (lambda (state) (equal state "cancelled")) states) "cancelled")
     ((seq-some (lambda (state) (member state '("failed" "blocked" "cancelled"))) states)
      (if (seq-every-p (lambda (state) (equal state "blocked")) states)
          "blocked"
        "partial"))
     ((seq-every-p (lambda (state) (equal state "pending")) states) "pending")
     (t "running"))))

(defun agent-shell-workflow-operation-list (&optional incomplete-only)
  "Return stored operations, restricting to recoverable ones if INCOMPLETE-ONLY."
  (let ((operations
         (seq-map (lambda (file)
                    (agent-shell-workflow-operation-read (file-name-base file)))
                  (directory-files (agent-shell-workflow--operation-directory)
                                   t "\\.json\\'"))))
    (if incomplete-only
        (seq-remove (lambda (operation)
                      (member (agent-shell-workflow-operation-status operation)
                              '("succeeded" "cancelled"))) operations)
      operations)))

(defun agent-shell-workflow-operation-resume (id)
  "Reconcile ID's observed checkout state without dispatching or replaying work.
Frozen targets remain unchanged.  Identity, branch, HEAD and resource drift
block unfinished members and retain the observed snapshot in their details."
  (let ((operation (agent-shell-workflow-operation-read id)))
    (seq-do
     (lambda (target)
       (let* ((target-id (plist-get target :id))
              (member (seq-find (lambda (entry)
                                  (equal target-id (plist-get entry :target-id)))
                                (plist-get operation :members)))
              (state (plist-get member :state))
              (observed (agent-shell-workflow--workspace-target
                         (plist-get target :worktree-dir)))
              (expected-head (or (car (last (plist-get member :commits)))
                                  (plist-get target :head)))
              (changed (or (plist-get observed :preflight-error)
                           (not (equal (plist-get observed :head) expected-head))
                           (seq-some
                            (lambda (key) (not (equal (plist-get observed key)
                                                      (plist-get target key))))
                            '(:repo-root :worktree-dir :common-dir :branch))
                           (not (equal (plist-get target :resource-id) "local"))
                           (not (file-directory-p (or (plist-get target :shell-dir)
                                                     (plist-get target :worktree-dir)))))))
         (setq operation
                 (agent-shell-workflow-operation-update
                  id target-id (plist-get operation :generation)
                  (if (and changed (not (member state '("succeeded" "cancelled"))))
                      "blocked"
                    state)
                  (append (list :observed observed :reason
                                (if changed "Checkout or resource changed; review before resuming"
                                  "Reconciled; no work was dispatched"))
                          (and (plist-get member :commits)
                               (list :commits (plist-get member :commits))))))))
     (plist-get operation :targets))
    operation))

(defun agent-shell-workflow-operation-cancel (id)
  "Cancel unfinished members of ID and release only its own checkout leases.
No shell, shared queue or worktree is removed.  Schedulers must cancel their
owned successors using the durable cancellation notification."
  (let ((operation (agent-shell-workflow-operation-read id)))
    (seq-do
     (lambda (member)
       (setq operation
               (agent-shell-workflow-operation-update
                id (plist-get member :target-id) (plist-get operation :generation)
                (if (equal (plist-get member :state) "succeeded")
                    "succeeded"
                  "cancelled")
                (list :reason "Cancellation requested"))))
     (plist-get operation :members))
    (seq-do
     (lambda (function)
       (condition-case failure
           (funcall function (copy-tree operation))
         (error (message "Operation %s cancellation handler failed: %s"
                         id (error-message-string failure)))))
     agent-shell-workflow-operation-cancel-functions)
    operation))

(defun agent-shell-workflow--operation-queue-done (_buffer-name item)
  "Advance serialized workflow ITEM from running to verifying only."
  (condition-case failure
      (let* ((raw (if (fboundp 'agent-shell-queue-item-args)
                      (agent-shell-queue-item-args item)
                    (plist-get item :args)))
             (payload (if (stringp raw) (car (read-from-string raw)) raw))
             (context (plist-get payload :ctx))
             (id (plist-get context :operation-id))
             (target-id (plist-get context :target-id))
             (generation (plist-get context :operation-generation)))
        (when (and (stringp id) (stringp target-id) (integerp generation))
          (let* ((operation (agent-shell-workflow-operation-read id))
                 (member (seq-find
                          (lambda (entry) (equal target-id (plist-get entry :target-id)))
                          (plist-get operation :members))))
            (when (and (= generation (plist-get operation :generation))
                       (equal (plist-get member :state) "running"))
              (agent-shell-workflow-operation-update
               id target-id generation "verifying"
               (list :reason "Queue turn completed; verification required"))))))
    (error (message "Operation queue reconciliation skipped: %s"
                    (error-message-string failure)))))

(defun agent-shell-workflow-operations-setup ()
  "Install durable operation queue integration explicitly and idempotently."
  (add-hook 'agent-shell-queue-item-done-hook
            #'agent-shell-workflow--operation-queue-done))

(provide 'agent-shell-workflow-operations)
;;; agent-shell-workflow-operations.el ends here
