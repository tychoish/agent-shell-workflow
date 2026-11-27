;;; test-agent-shell-workflow-routing.el --- Resource routing tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify resource shell placement independently of checkout preflight scope.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'agent-shell-workflow)

(defmacro asw-routing-test/fixture (&rest body)
  "Run BODY with separate target and shell directories and an isolated shell."
  (declare (indent 0))
  `(let ((agent-shell-workflow-registry (make-hash-table :test #'eq))
         (agent-shell-workflow--in-flight-dispatches (make-hash-table :test #'equal)))
    (let* ((root (make-temp-file "asw-routing-" t))
           (target-dir (expand-file-name "target/" root))
           (shell-dir (expand-file-name "resource/" root))
           (shell-buffer (generate-new-buffer " *asw-routing*"))
           (agent-shell-workflow-debounce-interval nil))
      (unwind-protect
          (progn
            (make-directory target-dir)
            (make-directory shell-dir)
            (with-current-buffer shell-buffer
              (setq default-directory shell-dir))
            (agent-shell-workflow-define routing
              :template "{{context-dir}}"
              :target :session-new)
            (cl-letf (((symbol-function 'agent-shell-select-config) #'ignore))
              ,@body))
        (kill-buffer shell-buffer)
        (delete-directory root t)))))

(ert-deftest agent-shell-workflow-routing/separate-preflight-and-startup ()
  "Startup uses the resource while preflight and operation IDs retain scope."
  (asw-routing-test/fixture
    (let (startup preflight received)
      (cl-letf (((symbol-function 'agent-shell-workflow--call-create-shell)
                 (lambda (directory &rest _)
                   (setq startup directory)
                   shell-buffer))
                ((symbol-function 'agent-shell-workflow-exec-pre)
                 (lambda (_spec context callback)
                   (setq preflight default-directory)
                   (funcall callback context)))
                ((symbol-function 'agent-shell-workflow--dispatch-rendered)
                 (lambda (_spec context &rest _)
                   (setq received context))))
        (agent-shell-workflow-dispatch
         'routing :context-dir target-dir :shell-dir shell-dir
         :operation-id "operation" :target-id "target"
         :operation-generation 3 :manifest '(:version 1))
        (should (equal startup shell-dir))
        (should (equal preflight target-dir))
        (should (equal (plist-get received :repos)
                       (list (file-truename target-dir))))
        (should (equal (plist-get received :operation-id) "operation"))
        (should (equal (plist-get received :target-id) "target"))
        (should (= (plist-get received :operation-generation) 3))
        (should (equal (plist-get received :manifest) '(:version 1)))))))

(ert-deftest agent-shell-workflow-routing/workspace-resolves-target-resource ()
  "A direct workspace dispatch preserves all repositories in the context."
  (asw-routing-test/fixture
    (let (received)
      (cl-letf (((symbol-function 'agent-shell-workflow-resolve-workspace)
                 (lambda (workspace directories)
                   (should (eq workspace 'elisp))
                   (should (equal directories (list (file-truename target-dir))))
                   (list :targets (list (list :worktree-dir (file-truename target-dir)
                                             :shell-dir shell-dir)))))
                ((symbol-function 'agent-shell-workflow--call-create-shell)
                 (lambda (directory &rest _)
                   (should (equal directory shell-dir))
                   shell-buffer))
                ((symbol-function 'agent-shell-workflow-exec-pre)
                 (lambda (_spec context callback) (funcall callback context)))
                ((symbol-function 'agent-shell-workflow--dispatch-rendered)
                 (lambda (_spec context &rest _) (setq received context))))
        (agent-shell-workflow-dispatch 'routing :context-dir target-dir :workspace 'elisp)
        (should (equal (plist-get received :context-dir) target-dir))
        (should (equal (plist-get received :shell-dir) shell-dir))))))

(ert-deftest agent-shell-workflow-routing/workspace-preflight-precedes-shell ()
  "An unavailable member prevents the whole selection from spawning shells."
  (asw-routing-test/fixture
    (cl-letf (((symbol-function 'agent-shell-workflow-resolve-workspace)
               (lambda (&rest _)
                 (list :targets
                       (list (list :worktree-dir (file-truename target-dir)
                                   :shell-dir shell-dir)
                             '(:worktree-dir "/missing/" :preflight-error "Missing checkout")))))
              ((symbol-function 'agent-shell-workflow--call-create-shell)
               (lambda (&rest _) (ert-fail "Shell created before membership validation"))))
      (should-error
       (agent-shell-workflow-dispatch 'routing :context-dir target-dir :workspace 'elisp)
       :type 'user-error))))

(ert-deftest agent-shell-workflow-routing/explicit-shell-mismatch ()
  "Selecting a shell outside the resource fails before preflight."
  (asw-routing-test/fixture
    (with-current-buffer shell-buffer (setq default-directory target-dir))
    (cl-letf (((symbol-function 'agent-shell-workflow-exec-pre)
               (lambda (&rest _) (ert-fail "Preflight ran with a mismatched shell"))))
      (should-error
       (agent-shell-workflow-dispatch 'routing :target shell-buffer
                                      :context-dir target-dir :shell-dir shell-dir)
       :type 'user-error))))

(ert-deftest agent-shell-workflow-routing/reuse-excludes-resource-subdirectories ()
  "Explicit resource reuse excludes shells even within resource subdirectories."
  (asw-routing-test/fixture
    (let ((child-dir (expand-file-name "child/" shell-dir)) created)
      (make-directory child-dir)
      (with-current-buffer shell-buffer (setq default-directory child-dir))
      (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list shell-buffer)))
                ((symbol-function 'agent-shell-workflow--call-create-shell)
                 (lambda (directory &rest _) (setq created directory) nil))
                ((symbol-function 'agent-shell-workflow-exec-pre) #'ignore))
        (agent-shell-workflow-dispatch 'routing :target :session-reuse-always
                                       :context-dir target-dir :shell-dir shell-dir)
        (should (equal created shell-dir))))))

(ert-deftest agent-shell-workflow-routing/fallback-uses-resource-directory ()
  "Deferred shell creation preserves the explicit resource after preflight."
  (asw-routing-test/fixture
    (let (received)
      (cl-letf (((symbol-function 'agent-shell-workflow--session-buffer)
                 (lambda (_target directory _spec exact)
                   (should (equal directory shell-dir))
                   (should exact)
                   shell-buffer))
                ((symbol-function 'agent-shell-workflow--dispatch-to-buffer)
                 (lambda (_spec _ctx buffer _submit) (setq received buffer))))
        (agent-shell-workflow--dispatch-rendered
         (agent-shell-workflow-get 'routing)
         (list :context-dir target-dir :shell-dir shell-dir)
         :session-new nil target-dir)
        (should (eq received shell-buffer))))))

(ert-deftest agent-shell-workflow-routing/group-delegates-workspace-selection ()
  "Workspace group dispatch forwards every target and together selection."
  (asw-routing-test/fixture
    (let (received)
      (cl-letf (((symbol-function 'agent-shell-workflow-dispatch-worktrees)
                 (lambda (id directories &rest keys)
                   (setq received (list id directories keys)))))
        (agent-shell-workflow-dispatch-group
         'routing (list target-dir shell-dir) :workspace 'elisp :together t)
        (should (eq (car received) 'routing))
        (should (equal (cadr received) (list target-dir shell-dir)))
        (should (eq (plist-get (caddr received) :workspace) 'elisp))
        (should (plist-get (caddr received) :together))))))

(ert-deftest agent-shell-workflow-routing/relative-resource-resolves-once ()
  (asw-routing-test/fixture
    (let ((default-directory root)
          received)
      (cl-letf (((symbol-function 'agent-shell-workflow--create-shell)
                 (lambda (directory &rest _)
                   (setq received directory)
                   shell-buffer))
                ((symbol-function 'agent-shell-workflow--dispatch-to-buffer) #'ignore))
        (agent-shell-workflow-dispatch 'routing :context-dir target-dir
                                       :shell-dir "resource/"))
      (should (equal shell-dir received)))))

(provide 'test-agent-shell-workflow-routing)
;;; test-agent-shell-workflow-routing.el ends here
