;;; test-agent-shell-workflow-org-dispatch.el --- Org dispatch tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise durable task references and source locations before dispatch.

;;; Code:

(require 'ert)
(require 'agent-shell-workflow-library)
(require 'org-id)

(ert-deftest agent-shell-workflow-org-dispatch/narrowed-source-location ()
  "Source locations are absolute and include the whole subtree."
  (with-temp-buffer
    (org-mode)
    (insert "* Earlier\nText\n* TODO Task\nBody\n** Child\nChild body\n* Later\n")
    (goto-char (point-min))
    (forward-line 2)
    (org-narrow-to-subtree)
    (let ((task (agent-shell-workflow-library--extract-org-task)))
      (should (= 3 (plist-get task :line)))
      (should (= 9 (plist-get task :end-line)))
      (should (string-match-p "Child body" (plist-get task :body)))
      (should (buffer-narrowed-p)))))

(ert-deftest agent-shell-workflow-org-dispatch/durable-file-id ()
  "Preflight creates and saves an ID, preserving it on repeat dispatch."
  (let ((file (make-temp-file "workflow-org-dispatch-" nil ".org"))
        (org-id-track-globally nil)
        buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "* TODO Task\nBody\n* Later\n")
            (goto-char (point-min))
            (let* ((first (agent-shell-workflow-library--org-task-pre-op nil))
                   (identifier (plist-get first :task-id))
                   (again (agent-shell-workflow-library--org-task-pre-op nil)))
              (should (stringp identifier))
              (should-not (string-empty-p identifier))
              (should (equal identifier (org-entry-get nil "ID")))
              (should (equal identifier (plist-get again :task-id)))
              (should (= 1 (plist-get first :task-line)))
              (should (= 5 (plist-get first :task-end-line)))
              (should-not (buffer-modified-p))
              (with-temp-buffer
                (insert-file-contents file)
                (org-mode)
                (goto-char (point-min))
                (should (equal identifier (org-entry-get nil "ID")))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest agent-shell-workflow-org-dispatch/existing-id-read-only ()
  "Read-only tasks with IDs can still be dispatched without changing state."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:ID: existing-task\n:END:\nBody\n")
    (goto-char (point-min))
    (let ((buffer-read-only t))
      (should (equal "existing-task"
                     (plist-get (agent-shell-workflow-library--extract-org-task) :id))))))

(ert-deftest agent-shell-workflow-org-dispatch/agenda-source ()
  "Agenda dispatch uses the source buffer even when that buffer is narrowed."
  (let ((source (generate-new-buffer " *workflow-org-source*")))
    (unwind-protect
        (let ((marker (with-current-buffer source
                        (org-mode)
                        (insert "* First\n* TODO Target\nBody\n* Last\n")
                        (goto-char (point-min))
                        (forward-line 1)
                        (prog1 (point-marker)
                          (goto-char (point-max))
                          (org-narrow-to-subtree)))))
          (with-temp-buffer
            (setq major-mode 'org-agenda-mode)
            (insert (propertize "Target" 'org-marker marker))
            (goto-char (point-min))
            (let ((task (agent-shell-workflow-library--extract-org-task)))
              (should (= 2 (plist-get task :line)))
              (should (equal "Target" (plist-get task :title)))))
          (with-current-buffer source (should (buffer-narrowed-p))))
      (kill-buffer source))))

(ert-deftest agent-shell-workflow-org-dispatch/prompt-and-callback ()
  "Rendered instructions carry source references, delegation and progress."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:ID: prompt-task\n:END:\nBody\n")
    (goto-char (point-min))
    (let (received)
      (agent-shell-workflow-library--org-task-pre-op
       '(:args (:instructions "Extra constraints"))
       (lambda (context) (setq received context)))
      (let ((prompt (agent-shell-workflow-render
                     (agent-shell-workflow-spec-template
                      (agent-shell-workflow-get 'implement-org-task)) received)))
        (should (string-match-p "id:prompt-task" prompt))
        (should (string-match-p ":1-5" prompt))
        (should (string-match-p "sub-agents" prompt))
        (should (string-match-p "original heading" prompt))
        (should (string-match-p "Extra constraints" prompt))))))

(ert-deftest agent-shell-workflow-org-dispatch/source-survives-shell-selection ()
  "Dispatch captures the original source before creating a foreground shell."
  (let ((source (generate-new-buffer " *workflow-dispatch-source*"))
        (shell (generate-new-buffer " *workflow-dispatch-shell*"))
        (original-create (symbol-function 'agent-shell-workflow--call-create-shell))
        (original-deliver (symbol-function 'agent-shell-workflow--dispatch-rendered))
        (agent-shell-workflow-debounce-interval nil)
        delivered)
    (unwind-protect
        (progn
          (fset 'agent-shell-workflow--call-create-shell
                (lambda (&rest _)
                  (set-buffer shell)
                  shell))
          (fset 'agent-shell-workflow--dispatch-rendered
                (lambda (_spec context &rest _)
                  (setq delivered context)))
          (with-current-buffer source
            (org-mode)
            (insert "* TODO Original task\n:PROPERTIES:\n:ID: original-task\n:END:\nBody\n")
            (goto-char (point-min))
            (agent-shell-workflow-dispatch
             'implement-org-task :args '(:instructions "")
             :target '(:session-new . test-config)))
          (should (equal "original-task" (plist-get delivered :task-id)))
          (should (string-match-p "Original task" (plist-get delivered :rendered))))
      (fset 'agent-shell-workflow--call-create-shell original-create)
      (fset 'agent-shell-workflow--dispatch-rendered original-deliver)
      (kill-buffer source)
      (kill-buffer shell))))

(ert-deftest agent-shell-workflow-org-dispatch/dead-source-error ()
  "Dead source markers produce actionable errors."
  (should-error
   (agent-shell-workflow-library--org-task-pre-op
    (list :source-marker (make-marker)))
   :type 'user-error))

(ert-deftest agent-shell-workflow-org-dispatch/read-only-needs-id ()
  "Read-only tasks without IDs fail before any edits."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\nBody\n")
    (goto-char (point-min))
    (let ((buffer-read-only t))
      (should-error (agent-shell-workflow-library--extract-org-task)
                    :type 'user-error))
    (should (equal "* TODO Task\nBody\n" (buffer-string)))))

(ert-deftest agent-shell-workflow-org-dispatch/state-change-preserves-location ()
  "The TODO transition does not corrupt subtree line numbers."
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE"))))
      (org-mode)
      (insert "* TODO Task\n:PROPERTIES:\n:ID: state-task\n:END:\nx\n* Next\ny\n")
      (goto-char (point-min))
      (let ((task (agent-shell-workflow-library--extract-org-task)))
        (should (equal "INPROGRESS" (org-get-todo-state)))
        (should (equal "TODO" (plist-get task :todo)))
        (should (= 5 (plist-get task :end-line)))))))

(defun agent-shell-workflow-org-dispatch-test--save-preamble ()
  "Insert a preamble before the source heading as a save hook would."
  (save-excursion
    (goto-char (point-min))
    (insert "Preamble\n")))

(ert-deftest agent-shell-workflow-org-dispatch/save-hooks-update-location ()
  "Source locations describe the saved file after save hooks run."
  (let ((file (make-temp-file "workflow-org-save-hook-" nil ".org"))
        (org-id-track-globally nil)
        buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "* TODO Task\nBody\n")
            (goto-char (point-min))
            (add-hook 'before-save-hook
                      #'agent-shell-workflow-org-dispatch-test--save-preamble nil t)
            (let ((task (agent-shell-workflow-library--extract-org-task)))
              (should (= 2 (plist-get task :line)))
              (should (= 6 (plist-get task :end-line)))
              (should (equal "Task" (plist-get task :title))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(provide 'test-agent-shell-workflow-org-dispatch)
;;; test-agent-shell-workflow-org-dispatch.el ends here
