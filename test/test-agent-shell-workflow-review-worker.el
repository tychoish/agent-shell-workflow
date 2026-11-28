;;; test-agent-shell-workflow-review-worker.el --- Tests for review & worker -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `agent-shell-workflow-review' and `agent-shell-workflow-worker'.

;;; Code:

(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'agent-shell-workflow-review)
(require 'agent-shell-workflow-worker)

(ert-deftest test-asw-review-digest-expiry-and-provenance ()
  (let ((agent-shell-workflow-review--requests (make-hash-table :test #'equal)))
    (agent-shell-workflow-request-review
     :request-id "rev-1" :train-id "tr-1" :task-id "t1"
     :subject-digest "sha256:aaa" :review-kind 'diff-review
     :allowed-continuations '(approve revise cancel)
     :generation 1 :expires-at 2000 :partial-p nil :now 1000)
    ;; Unauthenticated provenance forbidden
    (let ((bad-prov (agent-shell-workflow-respond-review
                     "rev-1" :action 'approve :subject-digest "sha256:aaa"
                     :generation 1 :provenance 'agent-claim :now 1500)))
      (should (eq (plist-get bad-prov :outcome) 'forbidden)))
    ;; Stale subject digest rejected
    (let ((stale (agent-shell-workflow-respond-review
                  "rev-1" :action 'approve :subject-digest "sha256:CHANGED"
                  :generation 1 :provenance 'interactive-human :now 1500)))
      (should (eq (plist-get stale :outcome) 'conflict))
      (should-not (plist-get stale :approved)))
    ;; Non-terminal diff buffer close/save ignored
    (let ((ign (agent-shell-workflow-review-from-diff-event
                "rev-1" 'buffer-saved "sha256:aaa" :provenance 'interactive-human :now 1500)))
      (should (eq (plist-get ign :outcome) 'invalid)))
    ;; Expired review never approves
    (let ((exp (agent-shell-workflow-respond-review
                "rev-1" :action 'approve :subject-digest "sha256:aaa"
                :generation 1 :provenance 'interactive-human :now 2500)))
      (should (eq (plist-get exp :outcome) 'conflict))
      (should-not (plist-get exp :approved)))))

(ert-deftest test-asw-review-partial-and-mcp-origin-proposal ()
  (let ((agent-shell-workflow-review--requests (make-hash-table :test #'equal))
        (agent-shell-workflow-review--proposals (make-hash-table :test #'equal)))
    (agent-shell-workflow-request-review
     :request-id "rev-p" :train-id "tr-1" :task-id "t1"
     :subject-digest "sha256:bbb" :review-kind 'denote-review
     :allowed-continuations '(approve revise cancel)
     :generation 1 :expires-at 2000 :partial-p t :now 1000)
    (let ((ok (agent-shell-workflow-respond-review
               "rev-p" :action 'approve :subject-digest "sha256:bbb"
               :generation 1 :provenance 'authenticated-human
               :accepted-ids '("a1") :rejected-ids '("a2") :now 1500)))
      (should (eq (plist-get ok :outcome) 'accepted))
      (should (plist-get ok :approved)))
    ;; MCP-origin plan stages proposal instead of bypassing origin restriction
    (let ((prop (agent-shell-workflow-review-propose-plan-steps
                 '(:id "plan-1" :metadata (:origin mcp))
                 '((:op fix :target "foo"))
                 :rationale "Add lint step")))
      (should (eq (plist-get prop :outcome) 'accepted))
      (should (plist-get prop :staged-proposal)))))

(ert-deftest test-asw-worker-routing-discovery-and-wait-release ()
  (let ((agent-shell-workflow-worker--bindings (make-hash-table :test #'equal))
        (stopped nil))
    ;; Refuses fallback to coordinator Emacs
    (should-error
     (agent-shell-workflow-worker-bind
      "att-bad" :target-id "main" :checkout "/tmp/wt")
     :type 'user-error)
    ;; Bind isolated sprite
    (agent-shell-workflow-worker-bind
     "att-1" :sprite-name "wt-fix" :target-id "work.6.wt-fix"
     :endpoint "http://127.0.0.1:8765/sprite/work.6.wt-fix/mcp"
     :checkout "/tmp/wt-fix" :generation 1
     :coordinator-endpoint "http://127.0.0.1:8765/mcp")
    ;; Discover tools
    (let ((disc (agent-shell-workflow-worker-discover-capabilities
                 "att-1" (lambda (_tid) '("edit_file" "run_tests")))))
      (should (equal (plist-get disc :capabilities) '("edit_file" "run_tests"))))
    ;; Lost route fails visibly
    (should-error
     (agent-shell-workflow-worker-discover-capabilities "att-1" (lambda (_tid) nil)))
    ;; Entering wait releases idle worker while preserving checkout
    (let ((rel (agent-shell-workflow-worker-release-for-wait
                "att-1" (lambda (tid) (push tid stopped)))))
      (should (plist-get rel :released))
      (should (equal stopped '("work.6.wt-fix")))
      (should (= (agent-shell-workflow-worker-active-count) 0)))
    ;; Documented retain-lease keeps worker alive during wait
    (agent-shell-workflow-worker-bind
     "att-2" :target-id "work.6.wt-lease" :checkout "/tmp/wt-lease" :retain-lease t)
    (let ((kept (agent-shell-workflow-worker-release-for-wait
                 "att-2" (lambda (_tid) (error "Should not stop leased worker")))))
      (should (plist-get kept :retained))
      (should (= (agent-shell-workflow-worker-active-count) 1)))))

(provide 'test-agent-shell-workflow-review-worker)
;;; test-agent-shell-workflow-review-worker.el ends here
