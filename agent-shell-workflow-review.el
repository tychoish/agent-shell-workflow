;;; agent-shell-workflow-review.el --- Human review and plan evolution -*- lexical-binding: t; -*-

;;; Commentary:
;; Revision- and digest-bound human review request envelopes, HITL question
;; restart reconstruction, Diff Review / Denote explicit submit/cancel adapters,
;; partial acceptance validation, and safe proposal-only plan evolution for
;; restricted/MCP-origin plans.
;;
;; Load-time pure: starts no timers or interactive prompts on `require'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-workflow-train-contract)

(declare-function hitl-ask "hitl" (&rest args))
(declare-function hitl-plan-origin-mcp-p "hitl-plan" (plan))
(declare-function hitl-plan-restricted-p "hitl-plan" ())
(declare-function hitl-plan-insert-steps "hitl-plan" (plan before steps))

;;; Review Request Store

(defvar agent-shell-workflow-review--requests (make-hash-table :test #'equal)
  "Map of request-id string to persisted review request envelope plist.")

(defvar agent-shell-workflow-review--proposals (make-hash-table :test #'equal)
  "Map of proposal-id string to staged plan evolution proposal plist.")

(defun agent-shell-workflow-review--gen-id (prefix)
  "Generate opaque ID starting with PREFIX."
  (format "%s-%d-%06x" prefix (floor (float-time)) (random #xffffff)))

;;; Request Creation & Reconstruction

(cl-defun agent-shell-workflow-request-review
    (&key request-id train-id task-id subject-digest review-kind
          allowed-continuations generation expires-at partial-p
          subject-ref prompt-text now)
  "Create and persist a revision-bound human review request envelope.
Validates against `agent-shell-workflow-train-validate-review-request'.
Returns the persisted request plist."
  (let* ((rid (or request-id (agent-shell-workflow-review--gen-id "rev")))
         (ts (or now (float-time)))
         (req (list :request-id rid
                    :train-id train-id
                    :task-id task-id
                    :subject-digest subject-digest
                    :subject-ref subject-ref
                    :review-kind review-kind
                    :allowed-continuations allowed-continuations
                    :generation (or generation 1)
                    :expires-at (or expires-at (+ ts 86400))
                    :partial-p (not (null partial-p))
                    :prompt-text (or prompt-text "")
                    :status 'pending
                    :created-at ts
                    :response nil)))
    (let ((check (agent-shell-workflow-train-validate-review-request req)))
      (unless (plist-get check :ok)
        (user-error "Invalid review request: %s"
                    (string-join (plist-get check :errors) "; "))))
    (setf (map-elt agent-shell-workflow-review--requests rid) req)
    req))

(defun agent-shell-workflow-review-get (request-id)
  "Return stored review request plist for REQUEST-ID, or nil."
  (when-let* ((req (map-elt agent-shell-workflow-review--requests request-id)))
    (copy-sequence req)))

(defun agent-shell-workflow-review-reconstruct-hitl (request-id callback)
  "Recreate a live HITL question for persisted REQUEST-ID after restart.
Attaches fresh CALLBACK.  Returns nil if request is not pending or HITL
is unavailable."
  (when-let* ((req (map-elt agent-shell-workflow-review--requests request-id)))
    (when (and (eq (plist-get req :status) 'pending)
               (fboundp 'hitl-ask))
      (hitl-ask
       :id request-id
       :prompt (plist-get req :prompt-text)
       :options (mapcar #'symbol-name (plist-get req :allowed-continuations))
       :metadata (list :train-id (plist-get req :train-id)
                       :generation (plist-get req :generation)
                       :subject-digest (plist-get req :subject-digest))
       :callback callback))))

;;; Authenticated Human Response Validation

(cl-defun agent-shell-workflow-respond-review
    (request-id &key action subject-digest generation provenance
                accepted-ids rejected-ids comments now)
  "Record a validated human response to REQUEST-ID.
Rejects unauthenticated provenance (must be `interactive-human' or
`authenticated-human'), expired requests (expiry never approves), mismatched
`:subject-digest', stale `:generation', undeclared `:action', or disallowed
partial acceptance."
  (let ((req (map-elt agent-shell-workflow-review--requests request-id))
        (ts (or now (float-time))))
    (cond
     ((null req)
      (list :outcome 'invalid :reason "review-request-not-found"))
     ((not (memq provenance '(interactive-human authenticated-human)))
      (list :outcome 'forbidden :reason "unauthenticated-human-provenance"))
     ((not (eq (plist-get req :status) 'pending))
      (list :outcome 'conflict :reason (format "already-%s" (plist-get req :status))))
     ((> ts (plist-get req :expires-at))
      (setf (map-elt agent-shell-workflow-review--requests request-id)
            (plist-put (copy-sequence req) :status 'expired))
      (list :outcome 'conflict :reason "review-expired" :approved nil))
     ((not (equal subject-digest (plist-get req :subject-digest)))
      (list :outcome 'conflict :reason "stale-subject-digest" :approved nil))
     ((and generation (/= generation (plist-get req :generation)))
      (list :outcome 'conflict :reason "stale-generation" :approved nil))
     ((not (memq action (plist-get req :allowed-continuations)))
      (list :outcome 'invalid :reason (format "undeclared-action:%S" action)))
     ((and (or accepted-ids rejected-ids) (not (plist-get req :partial-p)))
      (list :outcome 'invalid :reason "partial-acceptance-not-allowed"))
     (t
      (let* ((status (if (eq action 'cancel) 'cancelled 'resolved))
             (resp (list :action action
                         :provenance provenance
                         :subject-digest subject-digest
                         :accepted-ids accepted-ids
                         :rejected-ids rejected-ids
                         :comments comments
                         :responded-at ts))
             (updated (thread-first (copy-sequence req)
                        (plist-put :status status)
                        (plist-put :response resp))))
        (setf (map-elt agent-shell-workflow-review--requests request-id) updated)
        (list :outcome 'accepted
              :request-id request-id
              :status status
              :action action
              :approved (eq action 'approve)
              :response resp))))))

;;; Diff Review & Denote Explicit Handoff Adapters

(defun agent-shell-workflow-review-from-diff-event (request-id event-kind head-sha
                                                               &key comments provenance now)
  "Translate an explicit Diff Review submit/cancel event into a review response.
EVENT-KIND must be `review-submitted-approve', `review-submitted-changes', or
`review-cancelled'.  Opening, saving, or closing a buffer is ignored."
  (let ((action (pcase event-kind
                  ('review-submitted-approve 'approve)
                  ('review-submitted-changes 'revise)
                  ('review-cancelled 'cancel)
                  (_ nil))))
    (if (null action)
        (list :outcome 'invalid :reason "non-terminal-buffer-event")
      (agent-shell-workflow-respond-review
       request-id
       :action action
       :subject-digest head-sha
       :provenance provenance
       :comments comments
       :now now))))

(defun agent-shell-workflow-review-from-denote-event (request-id event-kind content-digest
                                                                 &key comments provenance now)
  "Translate an explicit Denote plan review event into a review response.
Requires matching CONTENT-DIGEST; modified note invalidates prior review."
  (let ((action (pcase event-kind
                  ('review-submitted-approve 'approve)
                  ('review-submitted-revise 'revise)
                  ('review-cancelled 'cancel)
                  (_ nil))))
    (if (null action)
        (list :outcome 'invalid :reason "non-terminal-note-event")
      (agent-shell-workflow-respond-review
       request-id
       :action action
       :subject-digest content-digest
       :provenance provenance
       :comments comments
       :now now))))

;;; Safe Plan Evolution (Respecting HITL MCP-Origin Restrictions)

(cl-defun agent-shell-workflow-review-propose-plan-steps (plan steps &key before rationale)
  "Safely propose or insert STEPS into HITL PLAN without bypassing origin rules.
If PLAN is MCP-origin or execution is restricted (`hitl-plan-restricted-p'),
stages a proposal record for human approval instead of calling
`hitl-plan-insert-steps'.  Never weakens HITL origin restrictions."
  (let ((restricted (or (and (fboundp 'hitl-plan-restricted-p)
                             (hitl-plan-restricted-p))
                        (and (fboundp 'hitl-plan-origin-mcp-p)
                             (hitl-plan-origin-mcp-p plan))
                        (eq (plist-get (and (listp plan) (plist-get plan :metadata)) :origin)
                            'mcp))))
    (if restricted
        (let* ((pid (agent-shell-workflow-review--gen-id "prop"))
               (proposal (list :proposal-id pid
                               :plan plan
                               :before before
                               :steps steps
                               :rationale rationale
                               :status 'pending-human-approval)))
          (setf (map-elt agent-shell-workflow-review--proposals pid) proposal)
          (list :outcome 'accepted :staged-proposal t :proposal-id pid))
      (if (fboundp 'hitl-plan-insert-steps)
          (progn
            (hitl-plan-insert-steps plan before steps)
            (list :outcome 'accepted :inserted t))
        (list :outcome 'invalid :reason "hitl-plan-insert-steps-unavailable")))))

(provide 'agent-shell-workflow-review)
;;; agent-shell-workflow-review.el ends here
