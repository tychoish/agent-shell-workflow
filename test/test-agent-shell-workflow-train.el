;;; test-agent-shell-workflow-train.el --- Tests for workflow train templates -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `agent-shell-workflow-train': versioned template isolation,
;; pure-data plan-to-template compilation, immutable handoffs, deny-by-default
;; instantiation, named continuations, and MCP prompt rendering.

;;; Code:

(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'agent-shell-workflow-train)

(defmacro test-asw-train--with-clean-registry (&rest body)
  "Run BODY with isolated template and continuation registries."
  `(let ((agent-shell-workflow-train--templates (make-hash-table :test #'equal))
         (agent-shell-workflow-train--continuations (make-hash-table :test #'equal)))
     ,@body))

(ert-deftest test-asw-train-template-version-isolation ()
  (test-asw-train--with-clean-registry
   (agent-shell-workflow-train-register-template
    '(:id demo :version 1 :description "v1"
      :tasks ((:id "s1" :depends-on nil))
      :permitted-effects (observe)
      :default-limits (:max-turns 2 :max-retries 1 :max-duration 600)))
   (let ((tr-v1 (agent-shell-workflow-train-create
                 'demo :resource "/repo/a")))
     ;; Registering v2 does not mutate already-instantiated v1 train
     (agent-shell-workflow-train-register-template
      '(:id demo :version 2 :description "v2"
        :tasks ((:id "s1" :depends-on nil) (:id "s2" :depends-on ("s1")))
        :permitted-effects (observe dispatch)
        :default-limits (:max-turns 5 :max-retries 2 :max-duration 1200)))
     (should (= (plist-get (plist-get tr-v1 :definition) :template-version) 1))
     (should (= (length (plist-get tr-v1 :tasks)) 1))
     ;; Deny-by-default authorization is nil when not explicitly granted
     (should-not (plist-get tr-v1 :authorization)))))

(ert-deftest test-asw-train-explicit-limits-required-when-no-default ()
  (test-asw-train--with-clean-registry
   (agent-shell-workflow-train-register-template
    '(:id no-lim :version 1 :permitted-effects (observe)))
   ;; Non-interactive caller without :limits is rejected
   (should-error (agent-shell-workflow-train-create 'no-lim :resource "/repo/a")
                 :type 'user-error)
   ;; Supplying explicit :limits succeeds
   (let ((tr (agent-shell-workflow-train-create
              'no-lim :resource "/repo/a"
              :limits '(:max-turns 2 :max-retries 1 :max-duration 300))))
     (should (eq (plist-get tr :state) 'staged)))))

(ert-deftest test-asw-train-plan-preview-rejects-cycles-and-never-evals ()
  (test-asw-train--with-clean-registry
   ;; Cyclic plan rejected
   (should-error
    (agent-shell-workflow-train-template-preview
     '(:id bad-cyc :version 1 :permitted-effects (dispatch)
       :tasks ((:id "a" :depends-on ("b")) (:id "b" :depends-on ("a")))))
    :type 'user-error)
   ;; Plain-data note plan compiles safely without evaluating text
   (let* ((side-effect nil)
          (prev (agent-shell-workflow-train-template-preview
                 '(:id from-note :version 1 :source-note-id "20261126T221549"
                   :description "(setq side-effect t)"
                   :permitted-effects (observe)
                   :tasks ((:id "t1" :depends-on nil))
                   :default-limits (:max-turns 2 :max-retries 1 :max-duration 600))))
          (reg (agent-shell-workflow-train-template-register-preview prev)))
     (should-not side-effect)
     (should (equal (plist-get reg :source-note-id) "20261126T221549"))
     (should (stringp (plist-get reg :source-digest))))))

(ert-deftest test-asw-train-handoff-and-continuations ()
  (test-asw-train--with-clean-registry
   ;; Unknown continuation fails visibly
   (should-error (agent-shell-workflow-train-resolve-continuation 'missing 1))
   (agent-shell-workflow-train-register-continuation 'retry 1 #'identity)
   (should (eq (agent-shell-workflow-train-resolve-continuation 'retry 1) #'identity))
   ;; Handoff checkpoint computes exact remaining limits
   (let* ((train '(:id "tr-1" :revision 2 :resource "/repo/wt"
                   :limits (:max-turns 6 :max-retries 4 :max-duration 3600)
                   :usage (:turns 2 :retries 1)
                   :authorization (:grant-id "g1" :allowed-effects (dispatch))))
          (task '(:id "t1" :objective "Fix test"))
          (cp (agent-shell-workflow-train-build-handoff train task :expected-revision "abc")))
     (should (= (plist-get (plist-get cp :remaining-limits) :max-turns) 4))
     (should (= (plist-get (plist-get cp :remaining-limits) :max-retries) 3))
     (should (equal (plist-get cp :resource) "/repo/wt")))))

(ert-deftest test-asw-train-builtin-templates-and-prompt-render ()
  (test-asw-train--with-clean-registry
   (agent-shell-workflow-train-register-builtin-templates)
   (should (agent-shell-workflow-train-get-template 'ci-repair 1))
   (should (agent-shell-workflow-train-get-template 'pr-review-cycle 1))
   (should (agent-shell-workflow-train-get-template 'mail-triage 1))
   (should (equal (agent-shell-workflow-train--render-prompt
                   "Fix {{repository}} on {{branch}}"
                   '(:repository "tychoish/foo" :branch "main"))
                  "Fix tychoish/foo on main"))))

(provide 'test-agent-shell-workflow-train)
;;; test-agent-shell-workflow-train.el ends here
