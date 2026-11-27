;;; test-agent-shell-workflow-workspaces.el --- Workspace cases -*- lexical-binding: t; -*-

;;; Commentary:
;; Real Git fixtures for checkout identities and resource policies.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-workflow-workspaces)

(defmacro asw-workspace-test/with-repo (&rest body)
  "Run BODY with a temporary repository ROOT and checkout WORKTREE."
  (declare (indent 0))
  `(let* ((temporary (make-temp-file "asw-workspaces-" t))
          (root (expand-file-name "repo/" temporary))
          (worktree (expand-file-name "other/" temporary))
          (agent-shell-workflow-repo-groups nil)
          (agent-shell-workflow-workspace-policies nil))
     (unwind-protect
         (progn
           (make-directory root)
           (agent-shell-workflow--workspace-git root "init" "-b" "main")
           (agent-shell-workflow--workspace-git root "config" "user.name" "Test")
           (agent-shell-workflow--workspace-git root "config" "user.email" "test@example.com")
           (agent-shell-workflow--workspace-git root "commit" "--allow-empty" "-m" "initial")
           (agent-shell-workflow--workspace-git root "worktree" "add" "-b" "topic" worktree)
           ,@body)
       (delete-directory temporary t))))

(ert-deftest asw-workspace/identity-aliases-and-worktrees ()
  (asw-workspace-test/with-repo
    (let ((alias (expand-file-name "alias" temporary)))
      (make-symbolic-link worktree alias)
      (let ((targets (plist-get (agent-shell-workflow-resolve-workspace
                                 nil (list root worktree alias)) :targets)))
        (should (= 2 (length targets)))
        (should (equal (plist-get (car targets) :common-dir)
                       (plist-get (cadr targets) :common-dir)))
        (should (equal root (plist-get (cadr targets) :repo-root)))
        (should-not (equal (plist-get (car targets) :id)
                           (plist-get (cadr targets) :id)))))))

(ert-deftest asw-workspace/identity-detached-dirty-and-missing ()
  (asw-workspace-test/with-repo
    (agent-shell-workflow--workspace-git worktree "checkout" "--detach")
    (write-region "dirty" nil (expand-file-name "untracked" worktree) nil 'silent)
    (let* ((snapshot (agent-shell-workflow-resolve-workspace
                      nil (list worktree (expand-file-name "missing" temporary))))
           (targets (plist-get snapshot :targets)))
      (should-not (plist-get (car targets) :branch))
      (should (plist-get (car targets) :head))
      (should (plist-get (car targets) :dirty))
      (should (plist-get (cadr targets) :preflight-error)))))

(ert-deftest asw-workspace/identity-nested-worktree ()
  (asw-workspace-test/with-repo
    (let ((nested (expand-file-name ".wt/nested/" root)))
      (agent-shell-workflow--workspace-git root "worktree" "add" "-b" "nested" nested)
      (let ((targets (plist-get (agent-shell-workflow-resolve-workspace
                                 nil (list root nested)) :targets)))
        (should (= 2 (length targets)))
        (should (equal nested (plist-get (cadr targets) :worktree-dir)))))))

(ert-deftest asw-workspace/policy-precedence-and-snapshot ()
  (asw-workspace-test/with-repo
    (agent-shell-workflow-register-repo-group 'elisp (list root worktree))
    (should
     (eq 'elisp
         (agent-shell-workflow-register-workspace-policy
          'elisp :shell-directory temporary
          :repository-rules (list (cons root (list :resource 'local :shell-directory root)))
          :worktree-rules (list (cons worktree (list :resource 'local :shell-directory worktree))))))
    (let ((snapshot (agent-shell-workflow-resolve-workspace 'elisp)))
      (should (equal root (plist-get (car (plist-get snapshot :targets)) :shell-dir)))
      (should (equal worktree (plist-get (cadr (plist-get snapshot :targets)) :shell-dir)))
      (setf (map-elt agent-shell-workflow-workspace-policies 'elisp) nil)
      (should (= 2 (length (plist-get snapshot :targets)))))))

(ert-deftest asw-workspace/policy-conflicts-and-remote ()
  (asw-workspace-test/with-repo
    (agent-shell-workflow-register-repo-group 'elisp (list root))
    (should-error
     (agent-shell-workflow-register-workspace-policy
      'elisp :shell-directory root
      :worktree-rules (list (cons root (list :shell-directory root))
                            (cons root (list :shell-directory temporary))))
     :type 'user-error)
    (agent-shell-workflow-register-workspace-policy
     'elisp :shell-directory root :resource 'remote)
    (should (plist-get (car (plist-get (agent-shell-workflow-resolve-workspace 'elisp)
                                      :targets)) :preflight-error))))

(ert-deftest asw-workspace/policy-relative-paths-are-frozen ()
  (asw-workspace-test/with-repo
    (let ((default-directory temporary))
      (agent-shell-workflow-register-repo-group 'elisp (list root))
      (agent-shell-workflow-register-workspace-policy
       'elisp :shell-directory "repo"
       :worktree-rules '(("repo" :resource local :shell-directory "other"))))
    (let ((default-directory "/tmp/"))
      (should (equal worktree
                     (plist-get (car (plist-get
                                      (agent-shell-workflow-resolve-workspace 'elisp)
                                      :targets)) :shell-dir))))))

(ert-deftest asw-workspace/remote-resource-never-contacts-transport ()
  (let ((remote "/ssh:unavailable.invalid:/repo/"))
    (cl-letf (((symbol-function 'file-truename)
               (lambda (&rest _) (error "Unexpected filesystem access"))))
      (should (plist-get (car (plist-get
                               (agent-shell-workflow-resolve-workspace nil (list remote))
                               :targets)) :preflight-error)))))

(ert-deftest asw-workspace/separate-git-directory-owns-main-checkout ()
  (asw-workspace-test/with-repo
    (let ((separate (expand-file-name "separate/" temporary))
          (metadata (expand-file-name "metadata" temporary)))
      (make-directory separate)
      (agent-shell-workflow--workspace-git
       separate "init" "--separate-git-dir" metadata "-b" "main")
      (agent-shell-workflow--workspace-git separate "config" "user.name" "Test")
      (agent-shell-workflow--workspace-git separate "config" "user.email" "test@example.com")
      (agent-shell-workflow--workspace-git separate "commit" "--allow-empty" "-m" "initial")
      (should (equal separate (plist-get (agent-shell-workflow--workspace-target separate)
                                         :repo-root))))))

(ert-deftest asw-workspace/policy-malformed-resource-is-user-error ()
  (asw-workspace-test/with-repo
    (agent-shell-workflow-register-repo-group 'elisp (list root))
    (should-error
     (agent-shell-workflow-register-workspace-policy
      'elisp :shell-directory root
      :worktree-rules (list (cons root (list :resource "remote" :shell-directory root))))
     :type 'user-error)))

(provide 'test-agent-shell-workflow-workspaces)
;;; test-agent-shell-workflow-workspaces.el ends here
