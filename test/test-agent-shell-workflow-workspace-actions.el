;;; test-agent-shell-workflow-workspace-actions.el --- Workspace action tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Real temporary Git checkouts with a deterministic fake dispatch provider.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-workflow-workspace-actions)

(defun asw-actions-test--repository (directory)
  "Initialize a real Git repository at DIRECTORY with one tracked file."
  (make-directory directory t)
  (agent-shell-workflow--workspace-git directory "init" "-b" "main")
  (agent-shell-workflow--workspace-git directory "config" "user.email" "test@example.com")
  (agent-shell-workflow--workspace-git directory "config" "user.name" "Workspace Test")
  (with-temp-file (expand-file-name "file.txt" directory) (insert "initial\n"))
  (agent-shell-workflow--workspace-git directory "add" "file.txt")
  (agent-shell-workflow--workspace-git directory "commit" "-m" "initial")
  directory)

(defmacro asw-actions-test--isolated (&rest body)
  "Run BODY with disposable operation storage and repositories."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "asw-actions-" t))
          (agent-shell-workflow-operation-directory (expand-file-name "state" root))
          (agent-shell-workflow--workspace-dispatches nil)
          (agent-shell-workflow--workspace-owned-shells nil)
          (agent-shell-workflow-workspace-policies nil)
          (agent-shell-workflow-repo-groups nil)
          (agent-shell-workflow-operation-cancel-functions nil)
          (agent-shell-queue-item-done-hook nil))
     (unwind-protect (progn ,@body) (delete-directory root t))))

(ert-deftest agent-shell-workflow/workspace-preview-is-read-only-and-partial ()
  (asw-actions-test--isolated
    (let* ((repository (asw-actions-test--repository (expand-file-name "repo" root)))
           (head (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD"))
           (operation (agent-shell-workflow-workspace-preview
                       (list repository (expand-file-name "missing" root)))))
      (should (equal '("succeeded" "blocked")
                     (seq-map (lambda (member) (plist-get member :state))
                              (plist-get operation :members))))
      (should (equal head (agent-shell-workflow--workspace-git
                          repository "rev-parse" "HEAD"))))))

(ert-deftest agent-shell-workflow/workspace-commit-verifies-before-real-commit ()
  (asw-actions-test--isolated
    (let* ((repository (asw-actions-test--repository (expand-file-name "repo" root)))
           captured)
      (with-temp-file (expand-file-name "file.txt" repository) (insert "prepared\n"))
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys)
                   (setq captured keys)
                   (agent-shell-workflow--workspace-action-post nil keys "done"))))
        (let* ((operation (agent-shell-workflow-dispatch-worktrees
                           'commit-group (list repository)
                           :args '(:verify-command ("git" "diff" "--check")
                                   :commit-message "feat: prepared" :files ("file.txt"))))
               (member (car (plist-get operation :members)))
               (head (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD")))
          (should (equal "succeeded" (plist-get member :state)))
          (should (equal (list head) (plist-get member :commits)))
          (should (equal (list (file-name-as-directory repository))
                         (plist-get captured :repos)))
          (should (string-match-p "worktree-dir"
                                  (plist-get (plist-get captured :args) :workspace-manifest)))
          (agent-shell-workflow--workspace-action-post nil captured "duplicate")
          (should (equal head (agent-shell-workflow--workspace-git
                              repository "rev-parse" "HEAD"))))))))

(ert-deftest agent-shell-workflow/workspace-failed-verification-does-not-commit ()
  (asw-actions-test--isolated
    (let* ((repository (asw-actions-test--repository (expand-file-name "repo" root)))
           (head (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD")))
      (with-temp-file (expand-file-name "file.txt" repository) (insert "prepared\n"))
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys)
                   (agent-shell-workflow--workspace-action-post nil keys "done"))))
        (let ((operation (agent-shell-workflow-dispatch-worktrees
                          'commit-group (list repository)
                          :args '(:verify-command ("git" "not-a-command")
                                  :commit-message "feat: prepared" :files ("file.txt")))))
          (should (equal "blocked" (plist-get (car (plist-get operation :members)) :state)))
          (should (equal head (agent-shell-workflow--workspace-git
                              repository "rev-parse" "HEAD"))))))))

(ert-deftest agent-shell-workflow/workspace-independent-siblings-and-dependencies ()
  (asw-actions-test--isolated
    (let* ((first (asw-actions-test--repository (expand-file-name "first" root)))
           (second (asw-actions-test--repository (expand-file-name "second" root)))
           captured)
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys)
                   (push (plist-get keys :context-dir) captured)
                   (if (equal (file-name-as-directory first) (plist-get keys :context-dir))
                       (error "Fake provider failed")
                     (agent-shell-workflow--workspace-action-post nil keys "inspected")))))
        (let ((operation (agent-shell-workflow-dispatch-worktrees
                          'workspace-inspect (list first second))))
          (should (= 2 (length captured)))
          (should (equal '("blocked" "succeeded")
                         (seq-map (lambda (member) (plist-get member :state))
                                  (plist-get operation :members)))))
        (setq captured nil)
        (let* ((snapshot (agent-shell-workflow-resolve-workspace nil (list first second)))
               (targets (plist-get snapshot :targets))
               (dependency (list :target-id (plist-get (cadr targets) :id)
                                 :requires (list (plist-get (car targets) :id))))
               (operation (agent-shell-workflow-dispatch-worktrees
                           'workspace-inspect (list first second) :dependencies (list dependency))))
          (should (= 1 (length captured)))
          (should (equal '("blocked" "blocked")
                         (seq-map (lambda (member) (plist-get member :state))
                                  (plist-get operation :members)))))))))

(ert-deftest agent-shell-workflow/workspace-cancelled-turn-cannot-commit ()
  (asw-actions-test--isolated
    (let* ((repository (asw-actions-test--repository (expand-file-name "repo" root)))
           captured)
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys) (setq captured keys))))
        (let* ((operation (agent-shell-workflow-dispatch-worktrees 'workspace-inspect (list repository)))
               (operation-id (plist-get operation :operation-id)))
          (agent-shell-workflow-operation-cancel operation-id)
          (agent-shell-workflow--workspace-action-post nil captured "late")
          (should (equal "cancelled"
                         (plist-get (car (plist-get
                                          (agent-shell-workflow-operation-read operation-id)
                                          :members)) :state))))))))

(ert-deftest agent-shell-workflow/workspace-mutations-require-explicit-arguments ()
  (asw-actions-test--isolated
    (let ((repository (asw-actions-test--repository (expand-file-name "repo" root))))
      (should-error (agent-shell-workflow-dispatch-worktrees 'commit-group (list repository))
                    :type 'user-error)
      (should-error (agent-shell-workflow-dispatch-worktrees
                     'repair-conflicts (list repository)
                     :args '(:verify-command ("true") :commit-message "fix: merge"))
                    :type 'user-error))))

(ert-deftest agent-shell-workflow/workspace-together-is-one-turn-and-two-commits ()
  (asw-actions-test--isolated
    (let* ((first (asw-actions-test--repository (expand-file-name "first" root)))
           (second (asw-actions-test--repository (expand-file-name "second" root)))
           (turns 0))
      (agent-shell-workflow-register-repo-group 'test (list first second))
      (agent-shell-workflow-register-workspace-policy 'test :shell-directory root)
      (seq-do (lambda (directory)
                (with-temp-file (expand-file-name "file.txt" directory) (insert "prepared\n")))
              (list first second))
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys)
                   (setq turns (1+ turns))
                   (should (= 2 (length (plist-get keys :repos))))
                   (should (= 2 (length (plist-get (plist-get keys :args)
                                                 :workspace-target-ids))))
                   (agent-shell-workflow--workspace-action-post nil keys "prepared"))))
        (let ((operation (agent-shell-workflow-dispatch-worktrees
                          'commit-group (list first second) :workspace 'test :together t
                          :args '(:verify-command ("git" "diff" "--check")
                                  :commit-message "feat: prepared" :files ("file.txt")))))
          (should (= turns 1))
          (should (seq-every-p (lambda (member)
                                (and (equal "succeeded" (plist-get member :state))
                                     (= 1 (length (plist-get member :commits)))))
                              (plist-get operation :members))))))))

(ert-deftest agent-shell-workflow/workspace-real-merge-conflict-requires-resolution ()
  (asw-actions-test--isolated
    (let* ((repository (asw-actions-test--repository (expand-file-name "repo" root)))
           captured)
      (agent-shell-workflow--workspace-git repository "checkout" "-b" "topic")
      (with-temp-file (expand-file-name "file.txt" repository) (insert "topic\n"))
      (agent-shell-workflow--workspace-git repository "commit" "-am" "topic change")
      (agent-shell-workflow--workspace-git repository "checkout" "main")
      (with-temp-file (expand-file-name "file.txt" repository) (insert "main\n"))
      (agent-shell-workflow--workspace-git repository "commit" "-am" "main change")
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys)
                   (setq captured keys)
                   (should-not (string-empty-p
                                (agent-shell-workflow--workspace-git repository "ls-files" "-u")))
                   (agent-shell-workflow--workspace-action-post nil keys "not resolved"))))
        (let ((operation (agent-shell-workflow-dispatch-worktrees
                          'repair-conflicts (list repository)
                          :args '(:verify-command ("git" "diff" "--check")
                                  :commit-message "fix: merge topic" :merge-ref "topic"))))
          (should captured)
          (should (equal "blocked" (plist-get (car (plist-get operation :members)) :state)))
          (should-not (string-empty-p
                       (agent-shell-workflow--workspace-git repository "ls-files" "-u"))))))))

(ert-deftest agent-shell-workflow/workspace-resolved-merge-is-verified-and-committed ()
  (asw-actions-test--isolated
    (let ((repository (asw-actions-test--repository (expand-file-name "repo" root))))
      (agent-shell-workflow--workspace-git repository "checkout" "-b" "topic")
      (with-temp-file (expand-file-name "file.txt" repository) (insert "topic\n"))
      (agent-shell-workflow--workspace-git repository "commit" "-am" "topic change")
      (agent-shell-workflow--workspace-git repository "checkout" "main")
      (with-temp-file (expand-file-name "file.txt" repository) (insert "main\n"))
      (agent-shell-workflow--workspace-git repository "commit" "-am" "main change")
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys)
                   (with-temp-file (expand-file-name "file.txt" repository) (insert "resolved\n"))
                   (agent-shell-workflow--workspace-git repository "add" "file.txt")
                   (agent-shell-workflow--workspace-action-post nil keys "resolved"))))
        (let ((operation (agent-shell-workflow-dispatch-worktrees
                          'repair-conflicts (list repository)
                          :args '(:verify-command ("git" "diff" "--check")
                                  :commit-message "fix: merge topic" :merge-ref "topic"))))
          (should (equal "succeeded" (plist-get (car (plist-get operation :members)) :state)))
          (should (= 3 (length (split-string
                                (agent-shell-workflow--workspace-git
                                 repository "rev-list" "--parents" "-n" "1" "HEAD"))))))))))

(ert-deftest agent-shell-workflow/workspace-resume-retries-frozen-failed-verification ()
  (asw-actions-test--isolated
    (let* ((repository (asw-actions-test--repository (expand-file-name "repo" root)))
           (args '(:verify-command ("false") :commit-message "feat: retry" :files ("file.txt")))
           operation)
      (with-temp-file (expand-file-name "file.txt" repository) (insert "changed\n"))
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys)
                   (agent-shell-workflow--workspace-action-post nil keys "prepared"))))
        (setq operation (agent-shell-workflow-dispatch-worktrees
                         'commit-group (list repository) :args args))
        (should (equal "blocked" (agent-shell-workflow-operation-status operation)))
        (let ((frozen (copy-tree (plist-get operation :targets))))
          (setq operation
                (agent-shell-workflow-workspace-resume
                 (plist-get operation :operation-id)
                 :retry-targets (list (plist-get (car frozen) :id))
                 :args (plist-put (copy-tree args) :verify-command '("git" "diff" "--check"))))
          (should (equal frozen (plist-get operation :targets)))
          (should (equal "succeeded" (agent-shell-workflow-operation-status operation)))
          (should (= 1 (length (plist-get (car (plist-get operation :members)) :commits)))))))))

(ert-deftest agent-shell-workflow/workspace-resume-never-replays-uncertain-commit ()
  (asw-actions-test--isolated
    (let* ((repository (asw-actions-test--repository (expand-file-name "repo" root)))
           (snapshot (agent-shell-workflow-resolve-workspace nil (list repository)))
           (operation (agent-shell-workflow-operation-create 'commit-group snapshot t))
           (id (plist-get operation :operation-id))
           (target-id (plist-get (car (plist-get snapshot :targets)) :id)))
      (seq-do (lambda (state)
                (setq operation (agent-shell-workflow--workspace-transition id target-id state)))
              '("preflight" "running" "verifying"))
      (agent-shell-workflow--workspace-transition
       id target-id "verifying" '(:commit-intent t :turn-complete t))
      (agent-shell-workflow--workspace-git repository "commit" "--allow-empty" "-m" "lost callback")
      (let ((head (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD")))
        (should-error
         (agent-shell-workflow-workspace-resume
          id :retry-targets (list target-id)
          :args '(:verify-command ("true") :commit-message "feat: retry" :files ("file.txt")))
         :type 'user-error)
        (should (equal head (agent-shell-workflow--workspace-git repository "rev-parse" "HEAD")))))))

(ert-deftest agent-shell-workflow/workspace-retry-resets-completion-and-requires-owned-shell ()
  (asw-actions-test--isolated
    (let* ((repository (asw-actions-test--repository (expand-file-name "repo" root)))
           (args '(:verify-command ("false") :commit-message "feat: retry" :files ("file.txt")))
           operation)
      (with-temp-file (expand-file-name "file.txt" repository) (insert "changed\n"))
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                 (lambda (_id &rest keys)
                   (agent-shell-workflow--workspace-action-post nil keys "prepared"))))
        (setq operation (agent-shell-workflow-dispatch-worktrees
                         'commit-group (list repository) :args args)))
      (let* ((id (plist-get operation :operation-id))
             (target-id (plist-get (car (plist-get operation :targets)) :id)))
        (should (plist-get (plist-get (car (plist-get operation :members)) :details) :turn-complete))
        (should-error
         (agent-shell-workflow-workspace-resume
          id :retry-targets (list target-id) :args args :target :session-reuse)
         :type 'user-error)
        (cl-letf (((symbol-function 'agent-shell-workflow-dispatch) #'ignore))
          (setq operation (agent-shell-workflow-workspace-resume
                           id :retry-targets (list target-id) :args args)))
        (should (equal "running" (plist-get (car (plist-get operation :members)) :state)))
        (should-not (plist-get (plist-get (car (plist-get operation :members)) :details)
                               :turn-complete))))))

(provide 'test-agent-shell-workflow-workspace-actions)
;;; test-agent-shell-workflow-workspace-actions.el ends here
