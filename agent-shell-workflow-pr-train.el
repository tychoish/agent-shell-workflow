;;; agent-shell-workflow-pr-train.el --- PR feedback batching & handoffs -*- lexical-binding: t; -*-

;;; Commentary:
;; Revision-bound PR review/comment/thread batching, deduplication, stale
;; approval invalidation, bounded revise task creation, and strict gating of
;; externally mutating publish/merge effects.
;;
;; Load-time pure: starts no timers or network requests on `require'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-workflow-train-contract)

(defconst agent-shell-workflow-pr-train-feedback-kinds
  '(discussion-comment change-request approval dismissed-review resolved-thread)
  "Normalized PR feedback observation kinds.")

(defconst agent-shell-workflow-pr-train-external-effects
  '(publish-comment push-branch mark-ready approve-pr merge-pr)
  "External PR effects that always require explicit per-effect authorization.")

;;; Batch Creation and Deduplication

(cl-defun agent-shell-workflow-pr-train-make-batch
    (&key repository pr-number head-sha items now)
  "Create a revision-bound PR feedback batch for HEAD-SHA.
Deduplicates ITEMS by `:id' and separates unresolved actionable feedback
from resolved threads and approvals."
  (let ((seen (make-hash-table :test #'equal))
        (deduped nil))
    (dolist (item items)
      (let* ((id (plist-get item :id))
             (kind (plist-get item :kind))
             (rev (or (plist-get item :head-sha) head-sha)))
        (unless (memq kind agent-shell-workflow-pr-train-feedback-kinds)
          (user-error "Unknown PR feedback kind: %S" kind))
        (unless (map-elt seen id)
          (setf (map-elt seen id) t)
          (push (plist-put (copy-sequence item) :head-sha rev) deduped))))
    (let* ((ordered (nreverse deduped))
           (unresolved
            (seq-filter
             (lambda (it)
               (and (memq (plist-get it :kind) '(discussion-comment change-request))
                    (not (plist-get it :resolved))))
             ordered))
           (approvals
            (seq-filter
             (lambda (it)
               (and (eq (plist-get it :kind) 'approval)
                    (equal (plist-get it :head-sha) head-sha)))
             ordered)))
      (list :schema-version agent-shell-workflow-train-contract-version
            :repository repository
            :pr-number pr-number
            :head-sha head-sha
            :collected-at (or now (float-time))
            :items ordered
            :unresolved-ids (mapcar (lambda (it) (plist-get it :id)) unresolved)
            :valid-approvals approvals))))

(defun agent-shell-workflow-pr-train-merge-refresh (existing new-items new-head-sha &optional now)
  "Merge NEW-ITEMS into EXISTING PR batch at NEW-HEAD-SHA.
Preserves unresolved feedback IDs from EXISTING, deduplicates repeated items,
and invalidates approvals tied to any older head SHA."
  (let* ((old-unresolved
          (seq-filter (lambda (it)
                        (member (plist-get it :id) (plist-get existing :unresolved-ids)))
                      (plist-get existing :items)))
         (combined (append new-items old-unresolved)))
    (agent-shell-workflow-pr-train-make-batch
     :repository (plist-get existing :repository)
     :pr-number (plist-get existing :pr-number)
     :head-sha new-head-sha
     :items combined
     :now now)))

;;; Revise Handoff and External Effect Gate

(defun agent-shell-workflow-pr-train-propose-revise-task (batch human-comments)
  "Build one fresh bounded revise task plist from review BATCH and HUMAN-COMMENTS."
  (list :id (format "pr-%s-revise-%04x" (plist-get batch :pr-number) (random #xffff))
        :type 'revise
        :context-policy 'fresh
        :depends-on nil
        :head-sha (plist-get batch :head-sha)
        :unresolved-ids (copy-sequence (plist-get batch :unresolved-ids))
        :human-comments human-comments
        :objective (format "Address %d unresolved PR #%s review items at %s"
                           (length (plist-get batch :unresolved-ids))
                           (plist-get batch :pr-number)
                           (plist-get batch :head-sha))))

(defun agent-shell-workflow-pr-train-can-run-effect-p (batch effect-kind granted-effects)
  "Return non-nil iff external EFFECT-KIND is explicitly authorized for BATCH.
Ordinary comments and resolved threads never authorize publishing or merging;
merging also requires at least one valid approval at the current `:head-sha'
and zero unresolved change requests."
  (and (memq effect-kind agent-shell-workflow-pr-train-external-effects)
       (memq effect-kind granted-effects)
       (if (eq effect-kind 'merge-pr)
           (and (consp (plist-get batch :valid-approvals))
                (null (seq-filter (lambda (it)
                                    (and (eq (plist-get it :kind) 'change-request)
                                         (not (plist-get it :resolved))))
                                  (plist-get batch :items))))
         t)))

(provide 'agent-shell-workflow-pr-train)
;;; agent-shell-workflow-pr-train.el ends here
