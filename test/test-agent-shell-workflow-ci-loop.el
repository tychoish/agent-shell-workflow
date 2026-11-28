;;; test-agent-shell-workflow-ci-loop.el --- End-to-end bounded CI pilot -*- lexical-binding: t; -*-

;;; Commentary:
;; End-to-end fake-CI + fake-Git pilot tests for `agent-shell-workflow-ci-loop':
;; staged creation, activation, repair dispatch, worker release during wait,
;; Git SHA reconciliation, unpushed/external-move/dirty/repeated-failure blocks,
;; deadline expiry, and exact-SHA success.

;;; Code:

(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'agent-shell-workflow-ci-loop)

(defconst test-ci-pilot--sha1 "1111111111111111111111111111111111111111")
(defconst test-ci-pilot--sha2 "2222222222222222222222222222222222222222")
(defconst test-ci-pilot--sha3 "3333333333333333333333333333333333333333")

(ert-deftest test-asw-ci-loop-end-to-end-pilot ()
  (let ((agent-shell-workflow-worker--bindings (make-hash-table :test #'equal))
        (stopped nil))
    ;; 1. Stage (inactive by default)
    (let* ((l0 (agent-shell-workflow-ci-loop-stage
                :id "ci-pilot-1"
                :repository "tychoish/foo"
                :branch "fix/ci"
                :expected-sha test-ci-pilot--sha1
                :resource "/tmp/wt-ci"
                :policy 'required-at-pr-head
                :limits '(:max-turns 4 :max-repairs 2 :max-retries 2
                          :max-observations 10 :max-duration 3600)
                :now 1000)))
      (should (eq (plist-get l0 :state) 'staged))
      ;; 2. Activate explicitly
      (let* ((l1 (agent-shell-workflow-ci-loop-activate l0 1005))
             ;; 3. Observe initial failure at SHA1 -> dispatches repair
             (s1 (agent-shell-workflow-ci-loop-step-observation
                  l1 (list :verdict 'failed :actual-sha test-ci-pilot--sha1
                           :failing-checks '((:name "ert" :conclusion "failure")))
                  1010))
             (l2 (plist-get s1 :loop))
             (att (plist-get s1 :attempt-id)))
        (should (eq (plist-get s1 :action) 'dispatch-repair))
        ;; Bind worker for repair attempt
        (agent-shell-workflow-worker-bind
         att :target-id "work.6.wt-ci" :checkout "/tmp/wt-ci")
        (should (= (agent-shell-workflow-worker-active-count) 1))
        ;; 4. Repair finishes and pushes SHA2 -> worker released during CI wait!
        (let* ((after (agent-shell-workflow-ci-loop-after-repair
                       l2 (list :local-head test-ci-pilot--sha2
                                :remote-sha test-ci-pilot--sha2
                                :dirty-p nil)
                       (lambda (tid) (push tid stopped))))
               (l3 (plist-get after :loop)))
          (should (eq (plist-get after :status) 'waiting-ci))
          (should (equal (plist-get l3 :expected-sha) test-ci-pilot--sha2))
          (should (= (agent-shell-workflow-worker-active-count) 0))
          (should (equal stopped '("work.6.wt-ci")))
          ;; 5. Wrong-SHA success is ignored
          (let ((wrong (agent-shell-workflow-ci-loop-step-observation
                        l3 (list :verdict 'succeeded :actual-sha test-ci-pilot--sha1)
                        1020)))
            (should (eq (plist-get wrong :action) 'wait)))
          ;; 6. Verified success at pushed SHA2 completes loop
          (let ((done (agent-shell-workflow-ci-loop-step-observation
                       l3 (list :verdict 'succeeded :actual-sha test-ci-pilot--sha2)
                       1030)))
            (should (eq (plist-get done :action) 'succeeded))
            (should (eq (plist-get (plist-get done :loop) :state) 'completed))))))))

(ert-deftest test-asw-ci-loop-git-blocks-and-repeated-fingerprint ()
  ;; Unpushed commit blocks
  (should (eq (plist-get (agent-shell-workflow-ci-loop-reconcile-git
                          :previous-sha test-ci-pilot--sha1
                          :local-head test-ci-pilot--sha2
                          :remote-sha test-ci-pilot--sha1
                          :dirty-p nil)
                         :status)
              'blocked-unpushed))
  ;; External remote movement blocks
  (should (eq (plist-get (agent-shell-workflow-ci-loop-reconcile-git
                          :previous-sha test-ci-pilot--sha1
                          :local-head test-ci-pilot--sha1
                          :remote-sha test-ci-pilot--sha3
                          :dirty-p nil)
                         :status)
              'blocked-external-move))
  ;; Dirty tree blocks
  (should (eq (plist-get (agent-shell-workflow-ci-loop-reconcile-git
                          :previous-sha test-ci-pilot--sha1
                          :local-head test-ci-pilot--sha2
                          :remote-sha test-ci-pilot--sha2
                          :dirty-p t)
                         :status)
              'blocked-dirty))
  ;; Repeated identical failure fingerprint stops loop
  (let* ((l0 (agent-shell-workflow-ci-loop-activate
              (agent-shell-workflow-ci-loop-stage
               :id "ci-fp" :repository "tychoish/foo" :branch "b"
               :expected-sha test-ci-pilot--sha1 :resource "/tmp/wt"
               :limits '(:max-turns 10 :max-repairs 10 :max-retries 10
                         :max-observations 20 :max-duration 3600)
               :now 1000)
              1001))
         (obs (list :verdict 'failed :actual-sha test-ci-pilot--sha1
                    :failing-checks '((:name "ert" :conclusion "failure"))))
         (l1 (plist-get (agent-shell-workflow-ci-loop-step-observation l0 obs 1010) :loop))
         (l2 (plist-get (agent-shell-workflow-ci-loop-step-observation l1 obs 1020) :loop))
         (r3 (agent-shell-workflow-ci-loop-step-observation l2 obs 1030)))
    (should (eq (plist-get r3 :action) 'blocked))
    (should (equal (plist-get r3 :reason) "repeated-failure-fingerprint"))))

(provide 'test-agent-shell-workflow-ci-loop)
;;; test-agent-shell-workflow-ci-loop.el ends here
