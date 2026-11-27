;;; agent-shell-workflow-mail-train.el --- Mail action proposals & review -*- lexical-binding: t; -*-

;;; Commentary:
;; Message-ID + account-root keyed actionable proposal manifests, deduplication
;; of Org tasks and reply drafts, partial human acceptance/deferral without
;; holding an agent session, strict separation of classification acceptance vs
;; mail-send/rule-change authorization, and moved-message reconciliation.
;;
;; Load-time pure: starts no timers or mail queries on `require'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-workflow-train-contract)

(defconst agent-shell-workflow-mail-train-action-kinds
  '(classify-move create-org-task draft-reply send-mail unsubscribe delete-mail change-rule)
  "Declared mail action kinds.")

(defconst agent-shell-workflow-mail-train-privileged-kinds
  '(send-mail unsubscribe delete-mail change-rule)
  "Mail actions requiring explicit authorization beyond classification.")

;;; Proposal Manifest Construction & Deduplication

(defun agent-shell-workflow-mail-train--action-key (proposal)
  "Return stable deduplication key for mail action PROPOSAL."
  (format "%s|%s|%s"
          (plist-get proposal :account-root)
          (plist-get proposal :message-id)
          (plist-get proposal :action-kind)))

(cl-defun agent-shell-workflow-mail-train-make-manifest
    (&key account-root cursor proposals now)
  "Create a deduplicated actionable proposal manifest for ACCOUNT-ROOT."
  (let ((seen (make-hash-table :test #'equal))
        (deduped nil))
    (dolist (p proposals)
      (let* ((kind (plist-get p :action-kind))
             (mid (plist-get p :message-id))
             (norm (plist-put (copy-sequence p) :account-root
                              (or (plist-get p :account-root) account-root)))
             (key (agent-shell-workflow-mail-train--action-key norm)))
        (unless (memq kind agent-shell-workflow-mail-train-action-kinds)
          (user-error "Unknown mail action kind: %S" kind))
        (unless (and (stringp mid) (not (string-empty-p mid)))
          (user-error "Mail proposal requires non-empty :message-id"))
        (unless (map-elt seen key)
          (setf (map-elt seen key) t)
          (push (thread-first norm
                  (plist-put :proposal-key key)
                  (plist-put :status 'pending))
                deduped))))
    (list :schema-version agent-shell-workflow-train-contract-version
          :account-root account-root
          :cursor cursor
          :created-at (or now (float-time))
          :proposals (nreverse deduped)
          :receipts nil)))

;;; Partial Acceptance and Authorized Application

(cl-defun agent-shell-workflow-mail-train-apply-decision
    (manifest &key accepted-keys deferred-keys rejected-keys
              authorized-actions message-locator-fn now)
  "Apply a human partial review decision to MANIFEST.
ACCEPTED-KEYS are applied only if privileged kinds are explicitly listed in
AUTHORIZED-ACTIONS, MESSAGE-LOCATOR-FN confirms message path (moved/missing
marks `needs-reconcile'), and prior receipts are deduplicated."
  (let ((ts (or now (float-time)))
        (receipts (copy-sequence (plist-get manifest :receipts)))
        (applied-set (make-hash-table :test #'equal))
        (updated-proposals nil))
    (dolist (r receipts)
      (setf (map-elt applied-set (plist-get r :proposal-key)) t))
    (dolist (p (plist-get manifest :proposals))
      (let* ((key (plist-get p :proposal-key))
             (kind (plist-get p :action-kind))
             (mid (plist-get p :message-id))
             (orig-path (plist-get p :path)))
        (cond
         ((member key rejected-keys)
          (push (plist-put (copy-sequence p) :status 'rejected) updated-proposals))
         ((member key deferred-keys)
          (push (plist-put (copy-sequence p) :status 'deferred) updated-proposals))
         ((member key accepted-keys)
          (cond
           ((map-elt applied-set key)
            (push (plist-put (copy-sequence p) :status 'duplicate-skipped) updated-proposals))
           ((and (memq kind agent-shell-workflow-mail-train-privileged-kinds)
                 (not (memq kind authorized-actions)))
            (push (plist-put (copy-sequence p) :status 'forbidden-needs-grant)
                  updated-proposals))
           ((and message-locator-fn
                 (let ((cur-path (funcall message-locator-fn mid)))
                   (or (null cur-path)
                       (and orig-path (not (equal cur-path orig-path))))))
            (push (plist-put (copy-sequence p) :status 'needs-reconcile)
                  updated-proposals))
           (t
            (let ((receipt (list :proposal-key key
                                 :message-id mid
                                 :action-kind kind
                                 :applied-at ts)))
              (push receipt receipts)
              (setf (map-elt applied-set key) t)
              (push (plist-put (copy-sequence p) :status 'applied) updated-proposals)))))
         (t
          (push p updated-proposals)))))
    (thread-first (copy-sequence manifest)
      (plist-put :proposals (nreverse updated-proposals))
      (plist-put :receipts receipts))))

(provide 'agent-shell-workflow-mail-train)
;;; agent-shell-workflow-mail-train.el ends here
