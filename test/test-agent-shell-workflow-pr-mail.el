;;; test-agent-shell-workflow-pr-mail.el --- Tests for PR & mail trains -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `agent-shell-workflow-pr-train' and
;; `agent-shell-workflow-mail-train'.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'agent-shell-workflow-pr-train)
(require 'agent-shell-workflow-mail-train)

(ert-deftest test-asw-pr-train-dedup-stale-approval-and-gates ()
  (let* ((sha1 "1111111111111111111111111111111111111111")
         (sha2 "2222222222222222222222222222222222222222")
         (b1 (agent-shell-workflow-pr-train-make-batch
              :repository "tychoish/foo" :pr-number 42 :head-sha sha1
              :items (list (list :id "c1" :kind 'discussion-comment :body "nit")
                           (list :id "c1" :kind 'discussion-comment :body "dup")
                           (list :id "a1" :kind 'approval :head-sha sha1))
              :now 1000)))
    ;; Deduplicated c1
    (should (= (length (plist-get b1 :items)) 2))
    (should (= (length (plist-get b1 :valid-approvals)) 1))
    ;; Without explicit merge-pr grant, cannot merge even with approval
    (should-not (agent-shell-workflow-pr-train-can-run-effect-p b1 'merge-pr '(publish-comment)))
    (should (agent-shell-workflow-pr-train-can-run-effect-p b1 'merge-pr '(merge-pr)))
    ;; New head SHA invalidates old-head approval while preserving unresolved c1
    (let ((b2 (agent-shell-workflow-pr-train-merge-refresh
               b1 (list (list :id "cr1" :kind 'change-request :body "fix"))
               sha2 1100)))
      (should (null (plist-get b2 :valid-approvals)))
      (should (member "c1" (plist-get b2 :unresolved-ids)))
      (should (member "cr1" (plist-get b2 :unresolved-ids)))
      (should-not (agent-shell-workflow-pr-train-can-run-effect-p b2 'merge-pr '(merge-pr)))
      ;; Revise response creates one fresh bounded task
      (let ((rev-task (agent-shell-workflow-pr-train-propose-revise-task b2 "Address both")))
        (should (eq (plist-get rev-task :context-policy) 'fresh))
        (should (equal (plist-get rev-task :head-sha) sha2))))))

(ert-deftest test-asw-mail-train-partial-acceptance-and-reconciliation ()
  (let* ((m0 (agent-shell-workflow-mail-train-make-manifest
              :account-root "/mail/work" :cursor "cur-1"
              :proposals
              (list (list :message-id "<m1@ex>" :action-kind 'create-org-task
                          :path "/mail/work/cur/m1")
                    ;; Duplicate proposal deduplicated
                    (list :message-id "<m1@ex>" :action-kind 'create-org-task
                          :path "/mail/work/cur/m1")
                    (list :message-id "<m2@ex>" :action-kind 'send-mail
                          :path "/mail/work/cur/m2")
                    (list :message-id "<m3@ex>" :action-kind 'create-org-task
                          :path "/mail/work/cur/m3")
                    (list :message-id "<m4@ex>" :action-kind 'create-org-task
                          :path "/mail/work/cur/m4"))
              :now 1000))
         (keys (mapcar (lambda (p) (plist-get p :proposal-key))
                       (plist-get m0 :proposals))))
    (should (= (length keys) 4))
    (let* ((k-m1 (nth 0 keys))
           (k-m2 (nth 1 keys))
           (k-m3 (nth 2 keys))
           (k-m4 (nth 3 keys))
           ;; Locator says m1 unchanged, m3 moved to archive
           (locator (lambda (mid)
                      (pcase mid
                        ("<m1@ex>" "/mail/work/cur/m1")
                        ("<m2@ex>" "/mail/work/cur/m2")
                        ("<m3@ex>" "/mail/work/archive/m3")
                        (_ nil))))
           (m1 (agent-shell-workflow-mail-train-apply-decision
                m0
                :accepted-keys (list k-m1 k-m2 k-m3)
                :deferred-keys (list k-m4)
                :authorized-actions '(classify-move create-org-task)
                :message-locator-fn locator
                :now 1100))
           (by-key (lambda (k)
                     (seq-find (lambda (p) (equal (plist-get p :proposal-key) k))
                               (plist-get m1 :proposals)))))
      ;; m1 applied with 1 receipt
      (should (eq (plist-get (funcall by-key k-m1) :status) 'applied))
      (should (= (length (plist-get m1 :receipts)) 1))
      ;; m2 send-mail blocked without explicit send-mail grant
      (should (eq (plist-get (funcall by-key k-m2) :status) 'forbidden-needs-grant))
      ;; m3 moved message triggers reconciliation instead of blind apply
      (should (eq (plist-get (funcall by-key k-m3) :status) 'needs-reconcile))
      ;; m4 deferred cleanly
      (should (eq (plist-get (funcall by-key k-m4) :status) 'deferred))
      ;; Re-applying m1 is deduplicated (still 1 receipt)
      (let ((m2 (agent-shell-workflow-mail-train-apply-decision
                 m1 :accepted-keys (list k-m1)
                 :authorized-actions '(create-org-task)
                 :message-locator-fn locator :now 1200)))
        (should (= (length (plist-get m2 :receipts)) 1))))))

(provide 'test-agent-shell-workflow-pr-mail)
;;; test-agent-shell-workflow-pr-mail.el ends here
