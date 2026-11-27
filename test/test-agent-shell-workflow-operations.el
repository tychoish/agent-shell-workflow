;;; test-agent-shell-workflow-operations.el --- Durable operation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Operation fixtures use real temporary Git checkouts and isolated storage.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'agent-shell-workflow-operations)

(defmacro asw-operation-test-with-repository (&rest body)
  "Run BODY with a real temporary repository and isolated durable storage."
  (declare (indent 0) (debug t))
  `(let* ((fixture (make-temp-file "asw-operation-test-" t))
          (repository (expand-file-name "repo/" fixture))
          (agent-shell-workflow-operation-directory
           (expand-file-name "operations/" fixture))
          (agent-shell-workflow-operation-update-functions nil)
          (agent-shell-workflow-operation-cancel-functions nil))
     (unwind-protect
         (progn
           (make-directory repository)
           (agent-shell-workflow--workspace-git repository "init" "-b" "main")
           (agent-shell-workflow--workspace-git repository "config" "user.name" "Test")
           (agent-shell-workflow--workspace-git repository "config" "user.email" "test@example.invalid")
           (agent-shell-workflow--workspace-git repository "commit" "--allow-empty" "-m" "initial")
           ,@body)
       (delete-directory fixture t))))

(defun asw-operation-test-create (repository &optional mutating)
  "Create a test operation selecting REPOSITORY, optionally MUTATING."
  (agent-shell-workflow-operation-create
   'test-workflow (agent-shell-workflow-resolve-workspace nil (list repository)) mutating))

(defun asw-operation-test-advance (operation states &optional final-details)
  "Advance OPERATION through STATES with optional FINAL-DETAILS."
  (seq-do
   (lambda (state)
     (setq operation
           (agent-shell-workflow-operation-update
            (plist-get operation :operation-id)
            (plist-get (car (plist-get operation :targets)) :id)
            (plist-get operation :generation) state
            (and (equal state (car (last states))) final-details)))) states)
  operation)

(ert-deftest agent-shell-workflow/operation-durable-copy-and-stale-callback ()
  (asw-operation-test-with-repository
    (let* ((operation (asw-operation-test-create repository t))
           (id (plist-get operation :operation-id))
           (target-id (plist-get (car (plist-get operation :targets)) :id))
           (frozen (copy-tree (plist-get operation :targets))))
      (setf (plist-get (car (plist-get operation :targets)) :branch) "changed")
      (should (equal frozen (plist-get (agent-shell-workflow-operation-read id) :targets)))
      (agent-shell-workflow-operation-update id target-id 0 "preflight")
      (should-error (agent-shell-workflow-operation-update id target-id 0 "running")
                    :type 'user-error)
      (should (= 1 (plist-get (agent-shell-workflow-operation-read id) :generation)))
      (should (= 1 (length (agent-shell-workflow-operation-list t)))))))

(ert-deftest agent-shell-workflow/operation-cross-group-checkout-leases ()
  (asw-operation-test-with-repository
    (let* ((first (asw-operation-test-create repository t))
           (second (asw-operation-test-create repository t))
           (second-id (plist-get second :operation-id))
           (target-id (plist-get (car (plist-get second :targets)) :id)))
      (should (equal "blocked" (agent-shell-workflow-operation-status second)))
      (should (equal "pending"
                     (agent-shell-workflow-operation-status
                      (asw-operation-test-create repository))))
      (should-error (agent-shell-workflow-operation-update second-id target-id 0 "preflight")
                    :type 'user-error)
      (agent-shell-workflow-operation-cancel (plist-get first :operation-id))
      (should (equal "preflight"
                     (plist-get (car (plist-get
                                      (agent-shell-workflow-operation-update
                                       second-id target-id 0 "preflight") :members)) :state))))))

(ert-deftest agent-shell-workflow/operation-independent-siblings-partial-success ()
  (asw-operation-test-with-repository
    (let ((other (expand-file-name "other/" fixture)))
      (agent-shell-workflow--workspace-git repository "worktree" "add" "-b" "other" other)
      (let* ((operation
              (agent-shell-workflow-operation-create
               'inspect (agent-shell-workflow-resolve-workspace nil (list repository other))))
             (id (plist-get operation :operation-id))
             (sibling-id (plist-get (cadr (plist-get operation :targets)) :id)))
        (setq operation (asw-operation-test-advance operation '("preflight" "running" "verifying" "succeeded")))
        (setq operation (agent-shell-workflow-operation-update
                         id sibling-id (plist-get operation :generation) "failed"
                         '(:reason "Simulated sibling failure")))
        (should (equal "partial" (agent-shell-workflow-operation-status operation)))
        (should (equal "succeeded" (plist-get (car (plist-get operation :members)) :state)))))))

(ert-deftest agent-shell-workflow/operation-leases-shared-across-instance-storage ()
  (asw-operation-test-with-repository
    (let ((first (asw-operation-test-create repository t)))
      (let ((agent-shell-workflow-operation-directory
             (expand-file-name "other-instance/" fixture)))
        (should (equal "blocked" (agent-shell-workflow-operation-status
                                  (asw-operation-test-create repository t)))))
      (agent-shell-workflow-operation-cancel (plist-get first :operation-id))
      (let ((agent-shell-workflow-operation-directory
             (expand-file-name "other-instance/" fixture)))
        (should (equal "pending" (agent-shell-workflow-operation-status
                                  (asw-operation-test-create repository t))))))))

(ert-deftest agent-shell-workflow/operation-distinct-worktrees-own-distinct-leases ()
  (asw-operation-test-with-repository
    (let ((other (expand-file-name "other/" fixture)))
      (agent-shell-workflow--workspace-git repository "worktree" "add" "-b" "other" other)
      (should (equal "pending" (agent-shell-workflow-operation-status
                                (asw-operation-test-create repository t))))
      (should (equal "pending" (agent-shell-workflow-operation-status
                                (asw-operation-test-create other t)))))))

(ert-deftest agent-shell-workflow/operation-crash-window-notification-cannot-replay-commit ()
  (asw-operation-test-with-repository
    (let* ((operation (asw-operation-test-create repository t))
           (id (plist-get operation :operation-id))
           (target-id (plist-get (car (plist-get operation :targets)) :id)))
      (setq operation (asw-operation-test-advance
                       operation '("preflight" "running" "verifying")
                       '(:dispatch-token "owned-turn")))
      (agent-shell-workflow--workspace-git repository "commit" "--allow-empty" "-m" "actual result")
      (let* ((commit (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD"))
             (generation (plist-get operation :generation))
             (agent-shell-workflow-operation-update-functions
              (list (lambda (&rest _arguments) (error "Observer crashed")))))
        (setq operation (agent-shell-workflow-operation-update
                         id target-id generation "committed" (list :commits (list commit))))
        (should (equal (list commit) (plist-get (car (plist-get operation :members)) :commits)))
        (should-error (agent-shell-workflow-operation-update
                       id target-id generation "committed" (list :commits (list commit)))
                      :type 'user-error)
        (setq operation (agent-shell-workflow-operation-resume id))
        (should (equal "committed" (plist-get (car (plist-get operation :members)) :state)))
        (should (equal "owned-turn"
                       (plist-get (plist-get (car (plist-get operation :members)) :details)
                                  :dispatch-token)))
        (should (equal commit (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD")))))))

(ert-deftest agent-shell-workflow/operation-resume-retains-snapshot-and-observations ()
  (asw-operation-test-with-repository
    (let* ((operation (asw-operation-test-create repository t))
           (id (plist-get operation :operation-id))
           (targets (copy-tree (plist-get operation :targets))))
      (agent-shell-workflow--workspace-git repository "commit" "--allow-empty" "-m" "outside change")
      (setq operation (agent-shell-workflow-operation-resume id))
      (should (equal "blocked" (agent-shell-workflow-operation-status operation)))
      (should (equal targets (plist-get operation :targets)))
      (should (plist-get (plist-get (car (plist-get operation :members)) :details) :observed)))))

(ert-deftest agent-shell-workflow/operation-detached-head-and-missing-directory-blocked ()
  (asw-operation-test-with-repository
    (agent-shell-workflow--workspace-git repository "checkout" "--detach")
    (should (equal "blocked" (agent-shell-workflow-operation-status
                              (asw-operation-test-create repository t))))
    (should (equal "pending" (agent-shell-workflow-operation-status
                              (asw-operation-test-create repository))))
    (should (equal "blocked" (agent-shell-workflow-operation-status
                              (asw-operation-test-create
                               (expand-file-name "missing/" fixture) t))))))

(ert-deftest agent-shell-workflow/operation-refuses-live-data-and-cyclic-dependencies ()
  (asw-operation-test-with-repository
    (let* ((snapshot (agent-shell-workflow-resolve-workspace nil (list repository)))
           (target-id (plist-get (car (plist-get snapshot :targets)) :id)))
      (should-error
       (agent-shell-workflow-operation-create
        'test snapshot nil (list (list :target-id target-id :requires (list target-id))))
       :type 'user-error)
      (let ((operation (asw-operation-test-create repository)))
        (should-error (agent-shell-workflow-operation-update
                       (plist-get operation :operation-id) target-id 0 "preflight"
                       (list :buffer (current-buffer))) :type 'user-error)
        (should (= 0 (plist-get (agent-shell-workflow-operation-read
                                (plist-get operation :operation-id)) :generation)))))))

(ert-deftest agent-shell-workflow/operation-queue-done-only-starts-verification ()
  (asw-operation-test-with-repository
    (let* ((operation (asw-operation-test-advance
                       (asw-operation-test-create repository) '("preflight" "running")))
           (id (plist-get operation :operation-id))
           (target-id (plist-get (car (plist-get operation :targets)) :id))
           (payload (list :ctx (list :operation-id id :target-id target-id
                                      :operation-generation (plist-get operation :generation)))))
      (let ((item (agent-shell-queue--make-item (prin1-to-string payload) nil 'workflow)))
        (agent-shell-workflow--operation-queue-done "unused" item)
        (agent-shell-workflow--operation-queue-done "unused" item))
      (setq operation (agent-shell-workflow-operation-read id))
      (should (equal "verifying" (plist-get (car (plist-get operation :members)) :state)))
      (should (= 3 (plist-get operation :generation)))
      (should-not (plist-get (car (plist-get operation :members)) :commits)))))

(ert-deftest agent-shell-workflow/operation-cancel-keeps-worktrees-and-commits ()
  (asw-operation-test-with-repository
    (let* ((operation (asw-operation-test-create repository t))
           (id (plist-get operation :operation-id))
           (head (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD"))
           notified
           (agent-shell-workflow-operation-cancel-functions
            (list (lambda (record) (setq notified (plist-get record :operation-id))))))
      (should (equal "cancelled" (agent-shell-workflow-operation-status
                                 (agent-shell-workflow-operation-cancel id))))
      (should (equal id notified))
      (should (equal head (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD")))
      (should (file-directory-p repository))
      (should-not (agent-shell-workflow-operation-list t)))))

(ert-deftest agent-shell-workflow/operation-active-write-lock-is-not-stolen ()
  (asw-operation-test-with-repository
    (let* ((operation (asw-operation-test-create repository))
           (id (plist-get operation :operation-id))
           (target-id (plist-get (car (plist-get operation :targets)) :id)))
      (agent-shell-workflow--operation-with-lock
       id
       (lambda ()
         (should-error (agent-shell-workflow-operation-update id target-id 0 "preflight")
                       :type 'user-error)
         (should-error (agent-shell-workflow-operation-recover-write-lock id 0)
                       :type 'user-error)))
      (should (= 0 (plist-get (agent-shell-workflow-operation-read id) :generation))))))

(ert-deftest agent-shell-workflow/operation-lease-explicit-recovery-refuses-active-owner ()
  (asw-operation-test-with-repository
    (let* ((operation (asw-operation-test-create repository t))
           (id (plist-get operation :operation-id))
           (inspection (agent-shell-workflow-operation-inspect-checkout-lease repository)))
      (should (equal id (plist-get (plist-get inspection :owner) :operation-id)))
      (should-error (agent-shell-workflow-operation-release-checkout-lease repository id)
                    :type 'user-error)
      (should-error (agent-shell-workflow-operation-release-checkout-lease repository "stale-owner" t)
                    :type 'user-error)
      (should-error (agent-shell-workflow-operation-release-checkout-lease repository id t)
                    :type 'user-error)
      (should (plist-get (agent-shell-workflow-operation-inspect-checkout-lease repository) :exists)))))

(ert-deftest agent-shell-workflow/operation-lease-ownerless-orphan-explicit-recovery ()
  (asw-operation-test-with-repository
    (let ((path (agent-shell-workflow--operation-lease-path
                 (agent-shell-workflow--workspace-target repository))))
      (make-directory path)
      (should (plist-get (agent-shell-workflow-operation-inspect-checkout-lease repository) :owner-missing))
      (should-error (agent-shell-workflow-operation-release-checkout-lease repository nil)
                    :type 'user-error)
      (should (agent-shell-workflow-operation-release-checkout-lease repository nil t))
      (should-not (file-exists-p path)))))

(ert-deftest agent-shell-workflow/operation-lease-cancelled-owner-quiescent-recovery ()
  (asw-operation-test-with-repository
    (let* ((operation (asw-operation-test-create repository t))
           (id (plist-get operation :operation-id)))
      (setq operation (asw-operation-test-advance
                       operation '("preflight" "running") '(:dispatch-token "owned-turn")))
      (agent-shell-workflow-operation-cancel id)
      (should (plist-get (agent-shell-workflow-operation-inspect-checkout-lease repository) :exists))
      (should (agent-shell-workflow-operation-release-checkout-lease repository id t))
      (should-not (plist-get (agent-shell-workflow-operation-inspect-checkout-lease repository) :exists)))))

(provide 'test-agent-shell-workflow-operations)
;;; test-agent-shell-workflow-operations.el ends here
