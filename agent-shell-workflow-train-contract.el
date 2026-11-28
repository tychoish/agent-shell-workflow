;;; agent-shell-workflow-train-contract.el --- Workflow train contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-data contracts and validators for workflow templates, declared
;; continuations, review request envelopes, handoff checkpoints, and strict
;; CI observations/policies.
;;
;; Load-time pure; shares schema version 1 with `agent-shell-queue-train-contract'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

;;; Schema and Policy Constants

(defconst agent-shell-workflow-train-contract-version 1
  "Contract schema version matching `agent-shell-queue-train-schema-version'.")

(defconst agent-shell-workflow-train-effect-kinds
  '(dispatch observe request-review publish-artifact apply-authorized-operation)
  "Permitted effect kinds for workflow templates and continuations.")

(defconst agent-shell-workflow-train-review-kinds
  '(hitl-question hitl-plan diff-review denote-review)
  "Supported human review adapter kinds.")

(defconst agent-shell-workflow-train-review-actions
  '(approve revise branch defer retry skip cancel)
  "Declared actions a human review response may select.")

(defconst agent-shell-workflow-train-ci-policies
  '(explicit-checks all-at-head main-workflows latest-branch-run required-at-pr-head)
  "Supported per-repository CI observation policy kinds.")

(defconst agent-shell-workflow-train-context-policies
  '(fresh reuse-within-attempt)
  "Supported worker context reuse policies.")

;;; Internal Helpers

(defun agent-shell-workflow-train--nonempty-string-p (val)
  "Return non-nil when VAL is a non-empty string."
  (and (stringp val) (not (string-empty-p val))))

(defun agent-shell-workflow-train--normalize-sym (val)
  "Normalize string or symbol VAL to a symbol, or nil."
  (cond
   ((symbolp val) val)
   ((stringp val) (intern val))
   (t nil)))

(defun agent-shell-workflow-train--result (errors)
  "Build validation plist from ERRORS."
  (if errors
      (list :ok nil :errors (nreverse errors))
    (list :ok t :errors nil)))

;;; Validators

(defun agent-shell-workflow-train-validate-template (spec)
  "Validate a versioned workflow template SPEC plist.
Required keys: `:id', `:version' (positive integer), `:task-types',
`:permitted-effects', `:declared-branches'.  Optional `:default-limits'
and `:review-batch-window' are validated when supplied."
  (let ((errors nil))
    (if (not (and (listp spec) (cl-evenp (length spec))))
        (push "Template spec must be a plist" errors)
      (unless (agent-shell-workflow-train--normalize-sym (plist-get spec :id))
        (push "Template :id is required" errors))
      (let ((ver (plist-get spec :version)))
        (unless (and (integerp ver) (> ver 0))
          (push "Template :version must be a positive integer" errors)))
      (let ((effects (plist-get spec :permitted-effects)))
        (unless (listp effects)
          (push "Template :permitted-effects must be a list" errors))
        (dolist (eff effects)
          (unless (memq (agent-shell-workflow-train--normalize-sym eff)
                        agent-shell-workflow-train-effect-kinds)
            (push (format "Template declares unknown effect kind: %S" eff) errors))))
      (let ((ctx-pol (plist-get spec :context-policy)))
        (when (and ctx-pol
                   (not (memq (agent-shell-workflow-train--normalize-sym ctx-pol)
                              agent-shell-workflow-train-context-policies)))
          (push (format "Unknown :context-policy: %S" ctx-pol) errors)))
      (let ((limits (plist-get spec :default-limits)))
        (when limits
          (dolist (k '(:max-turns :max-retries :max-duration))
            (let ((v (plist-get limits k)))
              (unless (and (integerp v) (> v 0))
                (push (format "Template :default-limits %s must be a positive integer" k)
                      errors)))))))
    (agent-shell-workflow-train--result errors)))

(defun agent-shell-workflow-train-validate-review-request (req)
  "Validate human review request envelope REQ plist."
  (let ((errors nil))
    (if (not (and (listp req) (cl-evenp (length req))))
        (push "Review request must be a plist" errors)
      (dolist (k '(:request-id :subject-digest))
        (unless (agent-shell-workflow-train--nonempty-string-p (plist-get req k))
          (push (format "Review request %s must be a non-empty string" k) errors)))
      (unless (memq (agent-shell-workflow-train--normalize-sym (plist-get req :review-kind))
                    agent-shell-workflow-train-review-kinds)
        (push (format "Unsupported :review-kind: %S" (plist-get req :review-kind)) errors))
      (let ((gen (plist-get req :generation)))
        (unless (and (integerp gen) (>= gen 0))
          (push "Review request :generation must be a non-negative integer" errors)))
      (let ((actions (plist-get req :allowed-continuations)))
        (if (not (and (listp actions) actions))
            (push "Review request :allowed-continuations must be a non-empty list" errors)
          (dolist (act actions)
            (unless (memq (agent-shell-workflow-train--normalize-sym act)
                          agent-shell-workflow-train-review-actions)
              (push (format "Unknown review action: %S" act) errors)))))
      (let ((exp (plist-get req :expires-at)))
        (unless (numberp exp)
          (push "Review request :expires-at must be a numeric timestamp" errors))))
    (agent-shell-workflow-train--result errors)))

(defun agent-shell-workflow-train-validate-handoff (checkpoint)
  "Validate immutable worker handoff CHECKPOINT plist.
Requires `:train-id', `:task-id', `:objective', `:resource',
`:expected-revision', `:remaining-limits', and `:schema-version'."
  (let ((errors nil))
    (if (not (and (listp checkpoint) (cl-evenp (length checkpoint))))
        (push "Handoff checkpoint must be a plist" errors)
      (let ((sv (plist-get checkpoint :schema-version)))
        (unless (and (integerp sv) (= sv agent-shell-workflow-train-contract-version))
          (push (format "Unsupported handoff :schema-version: %S" sv) errors)))
      (dolist (k '(:train-id :task-id :objective :resource))
        (unless (agent-shell-workflow-train--nonempty-string-p (plist-get checkpoint k))
          (push (format "Handoff %s must be a non-empty string" k) errors)))
      (unless (listp (plist-get checkpoint :remaining-limits))
        (push "Handoff :remaining-limits must be a plist" errors)))
    (agent-shell-workflow-train--result errors)))

(defun agent-shell-workflow-train-validate-ci-observation (obs)
  "Validate strict CI observation OBS plist.
Requires exact full `:requested-sha', `:actual-sha', valid `:policy',
`:repository', `:ref', `:observed-at', and `:checks' list."
  (let ((errors nil))
    (if (not (and (listp obs) (cl-evenp (length obs))))
        (push "CI observation must be a plist" errors)
      (dolist (k '(:repository :ref :requested-sha))
        (unless (agent-shell-workflow-train--nonempty-string-p (plist-get obs k))
          (push (format "CI observation %s must be a non-empty string" k) errors)))
      (let ((req-sha (plist-get obs :requested-sha)))
        (when (and (stringp req-sha)
                   (not (string-match-p "\\`[0-9a-fA-F]\\{40\\}\\'" req-sha)))
          (push "CI observation :requested-sha must be a full 40-char hex SHA" errors)))
      (unless (memq (agent-shell-workflow-train--normalize-sym (plist-get obs :policy))
                    agent-shell-workflow-train-ci-policies)
        (push (format "Unsupported CI :policy: %S" (plist-get obs :policy)) errors))
      (unless (numberp (plist-get obs :observed-at))
        (push "CI observation :observed-at must be a numeric timestamp" errors))
      (unless (listp (plist-get obs :checks))
        (push "CI observation :checks must be a list" errors)))
    (agent-shell-workflow-train--result errors)))

(provide 'agent-shell-workflow-train-contract)
;;; agent-shell-workflow-train-contract.el ends here
