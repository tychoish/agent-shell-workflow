;;; test-agent-shell-workflow-train-contract.el --- Tests for ASW train contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `agent-shell-workflow-train-contract' template, review,
;; handoff checkpoint, and strict CI observation contracts.

;;; Code:

(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'agent-shell-workflow-train-contract)

(ert-deftest test-asw-train-contract-template ()
  (let ((tpl '(:id ci-repair
               :version 1
               :task-types (repair observe)
               :permitted-effects (dispatch observe request-review)
               :declared-branches (retry escalate cancel)
               :context-policy fresh
               :default-limits (:max-turns 6 :max-retries 4 :max-duration 7200))))
    (should (plist-get (agent-shell-workflow-train-validate-template tpl) :ok)))
  ;; Unknown effect rejected
  (should-not (plist-get (agent-shell-workflow-train-validate-template
                          '(:id bad :version 1 :permitted-effects (arbitrary-eval)))
                         :ok))
  ;; Version <= 0 rejected
  (should-not (plist-get (agent-shell-workflow-train-validate-template
                          '(:id bad :version 0 :permitted-effects (observe)))
                         :ok)))

(ert-deftest test-asw-train-contract-review-request ()
  (let ((req '(:request-id "rev-1"
               :subject-digest "sha256:deadbeef"
               :review-kind diff-review
               :generation 2
               :allowed-continuations (approve revise cancel)
               :expires-at 2000000000)))
    (should (plist-get (agent-shell-workflow-train-validate-review-request req) :ok)))
  ;; Invalid review action rejected
  (should-not (plist-get (agent-shell-workflow-train-validate-review-request
                          '(:request-id "rev-2" :subject-digest "sha256:1"
                            :review-kind hitl-question :generation 0
                            :allowed-continuations (auto-merge)
                            :expires-at 2000000000))
                         :ok)))

(ert-deftest test-asw-train-contract-handoff ()
  (let ((cp '(:schema-version 1
              :train-id "tr-1"
              :task-id "t-1"
              :objective "Fix failing unit test"
              :resource "/home/tychoish/src/foo/.wt/fix"
              :expected-revision "0123456789abcdef0123456789abcdef01234567"
              :remaining-limits (:max-turns 5 :max-retries 3 :max-duration 3000))))
    (should (plist-get (agent-shell-workflow-train-validate-handoff cp) :ok)))
  (should-not (plist-get (agent-shell-workflow-train-validate-handoff
                          '(:schema-version 99 :train-id "tr-1"))
                         :ok)))

(ert-deftest test-asw-train-contract-ci-observation ()
  (let ((sha "0123456789abcdef0123456789abcdef01234567"))
    (should (plist-get (agent-shell-workflow-train-validate-ci-observation
                        (list :repository "tychoish/foo"
                              :ref "refs/heads/fix"
                              :requested-sha sha
                              :actual-sha sha
                              :policy 'required-at-pr-head
                              :policy-source "branch-protection"
                              :observed-at 1700000000
                              :checks '((:name "ert" :status "completed" :conclusion "success"))))
                       :ok))
    ;; Short SHA rejected
    (should-not (plist-get (agent-shell-workflow-train-validate-ci-observation
                            '(:repository "tychoish/foo" :ref "main"
                              :requested-sha "0123456" :policy all-at-head
                              :observed-at 1700000000 :checks nil))
                           :ok))))

(provide 'test-agent-shell-workflow-train-contract)
;;; test-agent-shell-workflow-train-contract.el ends here
