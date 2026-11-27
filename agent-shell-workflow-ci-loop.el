;;; agent-shell-workflow-ci-loop.el --- End-to-end bounded CI train -*- lexical-binding: t; -*-

;;; Commentary:
;; Composes the bounded fix/push/observe CI repair loop on top of durable
;; trains, strict exact-SHA CI observation, independent Git reconciliation,
;; worker release during waits, and actionable no-progress checkpoints.
;;
;; Load-time pure: starts no timers, daemons, or agent work on `require'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-workflow-train-contract)
(require 'agent-shell-workflow-train)
(require 'agent-shell-workflow-worker)

;;; Independent Git Reconciliation

(cl-defun agent-shell-workflow-ci-loop-reconcile-git
    (&key previous-sha local-head remote-sha dirty-p)
  "Independently reconcile Git state after a repair attempt.
Returns (:status SYM :new-sha STR-OR-NIL :reason STR) where status is
`advanced', `blocked-dirty', `blocked-unpushed', `blocked-external-move',
or `no-progress'."
  (cond
   (dirty-p
    (list :status 'blocked-dirty :new-sha nil :reason "working-tree-dirty"))
   ((and (not (equal remote-sha previous-sha))
         (not (equal remote-sha local-head)))
    (list :status 'blocked-external-move :new-sha nil
          :reason "remote-branch-moved-externally"))
   ((and (not (equal local-head previous-sha))
         (equal remote-sha previous-sha))
    (list :status 'blocked-unpushed :new-sha nil :reason "commit-not-pushed"))
   ((and (equal local-head remote-sha)
         (not (equal local-head previous-sha)))
    (list :status 'advanced :new-sha local-head :reason "pushed-new-sha"))
   (t
    (list :status 'no-progress :new-sha nil :reason "no-new-commit"))))

;;; Failure Fingerprinting

(defun agent-shell-workflow-ci-loop-failure-fingerprint (observation)
  "Compute deterministic failure fingerprint string from CI OBSERVATION plist."
  (let* ((failing (plist-get observation :failing-checks))
         (names (sort (mapcar (lambda (c)
                                (format "%s:%s"
                                        (plist-get c :name)
                                        (or (plist-get c :conclusion) "failed")))
                              failing)
                      #'string<)))
    (secure-hash 'sha256 (string-join names "|"))))

;;; Bounded CI Loop State Machine

(cl-defun agent-shell-workflow-ci-loop-stage
    (&key id repository branch expected-sha resource policy limits authorization now)
  "Stage a new bounded CI repair loop record (inactive until activated)."
  (let* ((ts (or now (float-time)))
         (eff-limits (or limits
                         '(:max-turns 6 :max-repairs 3 :max-retries 4
                           :max-observations 240 :max-duration 7200))))
    (list :schema-version agent-shell-workflow-train-contract-version
          :id (or id (format "ci-loop-%d-%04x" (floor ts) (random #xffff)))
          :state 'staged
          :revision 1
          :generation 1
          :repository repository
          :branch branch
          :expected-sha expected-sha
          :resource resource
          :policy (or policy 'all-at-head)
          :limits eff-limits
          :authorization authorization
          :usage '(:turns 0 :repairs 0 :observations 0)
          :deadline (+ ts (plist-get eff-limits :max-duration))
          :last-fingerprint nil
          :repeated-failures 0
          :active-attempt nil
          :checkpoint nil
          :history nil)))

(defun agent-shell-workflow-ci-loop-activate (loop &optional now)
  "Explicitly activate a staged CI LOOP."
  (unless (eq (plist-get loop :state) 'staged)
    (user-error "Cannot activate CI loop in state %S" (plist-get loop :state)))
  (thread-first (copy-sequence loop)
    (plist-put :state 'active)
    (plist-put :revision (1+ (plist-get loop :revision)))
    (plist-put :activated-at (or now (float-time)))))

(defun agent-shell-workflow-ci-loop-step-observation (loop observation &optional now)
  "Advance active CI LOOP given strict CI OBSERVATION at clock NOW.
Returns (:loop NEW-LOOP :action SYM :reason STR)."
  (let* ((ts (or now (float-time)))
         (state (plist-get loop :state))
         (expected (plist-get loop :expected-sha))
         (actual (plist-get observation :actual-sha))
         (verdict (plist-get observation :verdict))
         (limits (plist-get loop :limits))
         (usage (copy-sequence (plist-get loop :usage)))
         (obs-count (1+ (or (plist-get usage :observations) 0))))
    (cond
     ((not (eq state 'active))
      (list :loop loop :action 'noop :reason (format "not-active:%s" state)))
     ((> ts (plist-get loop :deadline))
      (let ((l2 (thread-first (copy-sequence loop)
                  (plist-put :state 'expired)
                  (plist-put :revision (1+ (plist-get loop :revision))))))
        (list :loop l2 :action 'expired :reason "deadline-exceeded")))
     ((and (plist-get limits :max-observations)
           (> obs-count (plist-get limits :max-observations)))
      (let ((l2 (thread-first (copy-sequence loop)
                  (plist-put :state 'blocked)
                  (plist-put :revision (1+ (plist-get loop :revision)))
                  (plist-put :checkpoint '(:reason "max-observations-exhausted")))))
        (list :loop l2 :action 'blocked :reason "max-observations-exhausted")))
     (t
      (setq usage (plist-put usage :observations obs-count))
      (let ((base (thread-first (copy-sequence loop)
                    (plist-put :usage usage)
                    (plist-put :revision (1+ (plist-get loop :revision))))))
        (pcase verdict
          ('succeeded
           (if (equal actual expected)
               (list :loop (plist-put base :state 'completed)
                     :action 'succeeded :reason "verified-at-expected-sha")
             (list :loop base :action 'wait :reason "wrong-sha-ignored")))
          ('pending
           (list :loop base :action 'wait :reason "ci-pending"))
          ('failed
           (let* ((fp (agent-shell-workflow-ci-loop-failure-fingerprint observation))
                  (same-fp (equal fp (plist-get loop :last-fingerprint)))
                  (rep-count (if same-fp (1+ (or (plist-get loop :repeated-failures) 0)) 0))
                  (repairs (or (plist-get usage :repairs) 0))
                  (max-rep (or (plist-get limits :max-repairs) 0)))
             (cond
              ((>= rep-count 2)
               (let ((blocked (thread-first base
                                (plist-put :state 'blocked)
                                (plist-put :last-fingerprint fp)
                                (plist-put :repeated-failures rep-count)
                                (plist-put :checkpoint
                                           (list :reason "repeated-failure-fingerprint"
                                                 :fingerprint fp
                                                 :expected-sha expected)))))
                 (list :loop blocked :action 'blocked :reason "repeated-failure-fingerprint")))
              ((>= repairs max-rep)
               (let ((blocked (thread-first base
                                (plist-put :state 'blocked)
                                (plist-put :checkpoint
                                           (list :reason "max-repairs-exhausted"
                                                 :expected-sha expected)))))
                 (list :loop blocked :action 'blocked :reason "max-repairs-exhausted")))
              (t
               (let* ((u2 (thread-first (copy-sequence usage)
                            (plist-put :repairs (1+ repairs))
                            (plist-put :turns (1+ (or (plist-get usage :turns) 0)))))
                      (att-id (format "rep-%s-%d" (plist-get loop :id) (1+ repairs)))
                      (next (thread-first base
                              (plist-put :usage u2)
                              (plist-put :last-fingerprint fp)
                              (plist-put :repeated-failures rep-count)
                              (plist-put :active-attempt att-id))))
                 (list :loop next :action 'dispatch-repair
                       :attempt-id att-id :reason "ci-failed-dispatch-repair"))))))
          (_
           (let ((blocked (thread-first base
                            (plist-put :state 'blocked)
                            (plist-put :checkpoint (list :reason (format "ci-verdict:%s" verdict))))))
             (list :loop blocked :action 'blocked
                   :reason (format "ci-verdict:%s" verdict))))))))))

(defun agent-shell-workflow-ci-loop-after-repair (loop git-state &optional worker-stopper)
  "Reconcile GIT-STATE after repair finishes and release worker for CI wait.
GIT-STATE is a plist (:local-head STR :remote-sha STR :dirty-p BOOL).
Returns (:loop NEW-LOOP :status SYM :reason STR)."
  (let* ((att (plist-get loop :active-attempt))
         (_ (when att
              (agent-shell-workflow-worker-release-for-wait att worker-stopper)))
         (rec (agent-shell-workflow-ci-loop-reconcile-git
               :previous-sha (plist-get loop :expected-sha)
               :local-head (plist-get git-state :local-head)
               :remote-sha (plist-get git-state :remote-sha)
               :dirty-p (plist-get git-state :dirty-p)))
         (st (plist-get rec :status))
         (base (thread-first (copy-sequence loop)
                 (plist-put :active-attempt nil)
                 (plist-put :revision (1+ (plist-get loop :revision))))))
    (if (eq st 'advanced)
        (list :loop (plist-put base :expected-sha (plist-get rec :new-sha))
              :status 'waiting-ci :reason "pushed-new-sha")
      (list :loop (thread-first base
                    (plist-put :state 'blocked)
                    (plist-put :checkpoint
                               (list :reason (plist-get rec :reason)
                                     :expected-sha (plist-get loop :expected-sha))))
            :status 'blocked :reason (plist-get rec :reason)))))

(provide 'agent-shell-workflow-ci-loop)
;;; agent-shell-workflow-ci-loop.el ends here
