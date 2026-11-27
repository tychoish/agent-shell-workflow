;;; test-agent-shell-workflow-workspace-provider.el --- Workspace provider integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Real dispatch, completion subscriptions, verification, and temporary Git
;; repositories; only the external agent provider is replaced.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'agent-shell-workflow)
(require 'agent-shell-workflow-library)
(require 'agent-shell-workflow-workspace-actions)

(defun asw-provider-test--repository (directory)
  "Initialize DIRECTORY with a committed tracked file and local Git identity."
  (make-directory directory t)
  (agent-shell-workflow--workspace-git directory "init" "-b" "main")
  (agent-shell-workflow--workspace-git directory "config" "user.email" "provider@example.com")
  (agent-shell-workflow--workspace-git directory "config" "user.name" "Provider Test")
  (with-temp-file (expand-file-name "tracked.txt" directory) (insert "initial\n"))
  (agent-shell-workflow--workspace-git directory "add" "tracked.txt")
  (agent-shell-workflow--workspace-git directory "commit" "-m" "initial")
  directory)

(defmacro asw-provider-test--isolated (&rest body)
  "Run BODY with a fake provider around the real workspace dispatch path."
  (declare (indent 0) (debug t))
  `(let* ((provider-root (make-temp-file "asw-provider-" t))
          (provider-repository
           (asw-provider-test--repository (expand-file-name "checkout" provider-root)))
          (provider-resource (expand-file-name "environment" provider-root))
          (provider-shells nil)
          (provider-events nil)
          (provider-interrupts nil)
          (provider-prepare #'ignore)
          (agent-shell-workflow-operation-directory (expand-file-name "state" provider-root))
          (agent-shell-workflow-registry (copy-hash-table agent-shell-workflow-registry))
          (agent-shell-workflow-workspace-policies nil)
          (agent-shell-workflow-repo-groups nil)
          (agent-shell-workflow--workspace-dispatches nil)
          (agent-shell-workflow--workspace-owned-shells nil)
          (agent-shell-workflow-operation-cancel-functions nil)
          (agent-shell-workflow-operation-update-functions nil)
          (agent-shell-queue-item-done-hook nil)
          (agent-shell-queue--store nil)
          (agent-shell-workflow-debounce-interval nil)
          (agent-shell-workflow--in-flight-dispatches (make-hash-table :test #'equal)))
     (make-directory provider-resource t)
     (agent-shell-workflow-register-repo-group 'provider-test (list provider-repository))
     (agent-shell-workflow-register-workspace-policy
      'provider-test :shell-directory provider-resource)
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-workflow--create-shell)
                    (lambda (directory &optional _config _display)
                      (let ((buffer (generate-new-buffer " *asw-fake-provider*")))
                        (with-current-buffer buffer
                          (setq default-directory (file-name-as-directory directory)))
                        (push buffer provider-shells)
                        buffer)))
                   ((symbol-function 'agent-shell-buffers) (lambda () provider-shells))
                   ((symbol-function 'agent-shell-workflow--resolve-provider-config)
                    (lambda (_providers) '((:identifier . fake-provider))))
                   ((symbol-function 'agent-shell-workflow--buffer-busy-p) #'ignore)
                   ((symbol-function 'agent-shell-workflow--show-buffer) #'ignore)
                   ((symbol-function 'agent-shell-insert)
                    (lambda (&rest _keys)
                      (funcall provider-prepare)
                      '((:end . 1))))
                   ((symbol-function 'agent-shell-subscribe-to)
                    (lambda (&rest keys)
                      (push (cons (plist-get keys :shell-buffer)
                                  (plist-get keys :on-event)) provider-events)
                      'fake-subscription))
                   ((symbol-function 'agent-shell-interrupt)
                    (lambda (&optional _force) (push (current-buffer) provider-interrupts))))
           ,@body)
       (seq-do (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) provider-shells)
       (delete-directory provider-root t))))

(ert-deftest agent-shell-workflow/workspace-provider-real-dispatch-verifies-and-commits ()
  (asw-provider-test--isolated
    (let ((starting-head (agent-shell-workflow--workspace-git
                          provider-repository "rev-parse" "HEAD")))
      (setq provider-prepare
            (lambda ()
              (with-temp-file (expand-file-name "tracked.txt" provider-repository)
                (insert "prepared by fake provider\n"))))
      (let* ((operation (agent-shell-workflow-dispatch-worktrees
                         'commit-group (list provider-repository)
                         :workspace 'provider-test :providers '(fake-provider)
                         :args '(:verify-command ("git" "diff" "--check")
                                 :commit-message "feat: provider preparation"
                                 :files ("tracked.txt"))))
             (operation-id (plist-get operation :operation-id)))
        (should (= 1 (length provider-shells)))
        (should (= 1 (length provider-events)))
        (should (equal (file-name-as-directory provider-resource)
                       (buffer-local-value 'default-directory (car provider-shells))))
        (should (equal (file-name-as-directory provider-repository)
                       (plist-get (car (plist-get operation :targets)) :worktree-dir)))
        (should (equal "running" (plist-get (car (plist-get operation :members)) :state)))
        (should (equal starting-head (agent-shell-workflow--workspace-git
                                      provider-repository "rev-parse" "HEAD")))
        (funcall (cdar provider-events) nil)
        (let* ((completed (agent-shell-workflow-operation-read operation-id))
               (member (car (plist-get completed :members)))
               (commit (agent-shell-workflow--workspace-git provider-repository "rev-parse" "HEAD")))
          (should (equal "succeeded" (plist-get member :state)))
          (should-not (equal starting-head commit))
          (should (equal (list commit) (plist-get member :commits)))
          (should (string-empty-p (agent-shell-workflow--workspace-git
                                  provider-repository "status" "--porcelain")))
          (funcall (cdar provider-events) nil)
          (should (equal commit (agent-shell-workflow--workspace-git
                                  provider-repository "rev-parse" "HEAD"))))))))

(ert-deftest agent-shell-workflow/workspace-provider-generic-without-post-completes ()
  (asw-provider-test--isolated
    (agent-shell-workflow-register
     :id 'provider-read-only :template "Inspect {{context-dir}} without modifications."
     :submit t :target :session-new)
    (should-not (agent-shell-workflow-spec-post-op
                 (agent-shell-workflow-get 'provider-read-only)))
    (let* ((operation (agent-shell-workflow-dispatch-worktrees
                       'provider-read-only (list provider-repository)
                       :workspace 'provider-test :providers '(fake-provider) :mutating nil))
           (operation-id (plist-get operation :operation-id)))
      (should (= 1 (length provider-events)))
      (funcall (cdar provider-events) nil)
      (let ((member (car (plist-get (agent-shell-workflow-operation-read operation-id) :members))))
        (should (equal "succeeded" (plist-get member :state)))
        (should (equal (agent-shell-workflow--workspace-git
                        provider-repository "rev-parse" "HEAD")
                       (plist-get (plist-get member :details) :observed-head)))))))

(ert-deftest agent-shell-workflow/workspace-provider-cancel-ignores-repurposed-successful-shell ()
  (asw-provider-test--isolated
    (let* ((operation (agent-shell-workflow-dispatch-worktrees
                       'workspace-inspect (list provider-repository)
                       :workspace 'provider-test :providers '(fake-provider)))
           (operation-id (plist-get operation :operation-id))
           (buffer (car provider-shells))
           (new-owner '("another-operation" "another-token" ("another-target"))))
      (funcall (cdar provider-events) nil)
      (with-current-buffer buffer
        (setq agent-shell-workflow--workspace-active-turn new-owner))
      (agent-shell-workflow-operation-cancel operation-id)
      (should-not provider-interrupts)
      (should (equal new-owner
                     (buffer-local-value 'agent-shell-workflow--workspace-active-turn buffer)))
      (should (equal "succeeded"
                     (plist-get (car (plist-get
                                      (agent-shell-workflow-operation-read operation-id)
                                      :members)) :state))))))

(ert-deftest agent-shell-workflow/workspace-provider-changed-head-blocks-before-verifier ()
  (asw-provider-test--isolated
    (let ((verification-count 0)
          (real-process (symbol-function 'agent-shell-workflow--workspace-process)))
      (cl-letf (((symbol-function 'agent-shell-workflow--workspace-process)
                 (lambda (directory command)
                   (setq verification-count (1+ verification-count))
                   (funcall real-process directory command))))
        (let* ((operation (agent-shell-workflow-dispatch-worktrees
                           'commit-group (list provider-repository)
                           :workspace 'provider-test :providers '(fake-provider)
                           :args '(:verify-command ("git" "diff" "--check")
                                   :commit-message "feat: coordinator commit"
                                   :files ("tracked.txt"))))
               (operation-id (plist-get operation :operation-id)))
          (agent-shell-workflow--workspace-git
           provider-repository "commit" "--allow-empty" "-m" "concurrent writer")
          (let ((concurrent-head (agent-shell-workflow--workspace-git
                                  provider-repository "rev-parse" "HEAD")))
            (funcall (cdar provider-events) nil)
            (should (zerop verification-count))
            (should (equal "blocked"
                           (plist-get (car (plist-get
                                            (agent-shell-workflow-operation-read operation-id)
                                            :members)) :state)))
            (should (equal concurrent-head (agent-shell-workflow--workspace-git
                                            provider-repository "rev-parse" "HEAD")))))))))

(provide 'test-agent-shell-workflow-workspace-provider)
;;; test-agent-shell-workflow-workspace-provider.el ends here
