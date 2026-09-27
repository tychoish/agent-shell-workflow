;;; test-agent-shell-workflow.el --- Tests for agent-shell-workflow -*- lexical-binding: t; no-byte-compile: t; -*-

;; Tests for the prompt registry, template rendering, pre/post
;; execution engine, dispatch routing, queue integration, and the ACR
;; picker added by agent-shell-workflow / agent-shell-workflow-menu /
;; agent-shell-workflow-library.
;;
;; Batch run:
;;   ./scripts/run-tests.el

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'agent-shell nil t)
(require 'agent-shell-workflow)
(require 'agent-shell-workflow-menu)
(require 'agent-shell-workflow-library)

;;; Isolation macro (mirrors asq-types-test/isolate)

(defmacro asw-test/isolate (&rest body)
  "Execute BODY with a clean prompt registry and isolated queue state."
  `(let ((agent-shell-workflow-registry (make-hash-table :test #'eq))
         (agent-shell-queue--store
          (agent-shell-queue--make-store :items nil :format 'plist :file nil))
         (agent-shell-queue--queue
          (agent-shell-queue-queue--make
           :store 'agent-shell-queue--store
           :session-paused nil
           :editing-ids nil
           :interjection-pending nil))
         (agent-shell-queue--loaded t))
     (cl-letf (((symbol-function 'agent-shell-queue--save) #'ignore)
               ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
               ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore))
       ,@body)))

;;; ─────────────────────────────────────────────────────────────
;;; Registry

(ert-deftest agent-shell-workflow/def-registers-spec ()
  "register-agent-shell-workflow registers a retrievable spec."
  (asw-test/isolate
   (register-agent-shell-workflow sample
     :doc "A sample prompt"
     :category "Testing"
     :args ((thing :prompt "Thing: "))
     :template "Do {{args.thing}}"
     :target :session-reuse)
   (let ((spec (agent-shell-workflow-get 'sample)))
     (should spec)
     (should (equal (agent-shell-workflow-spec-doc spec) "A sample prompt"))
     (should (equal (agent-shell-workflow-spec-category spec) "Testing"))
     (should (eq (agent-shell-workflow-spec-target spec) :session-reuse)))))

(ert-deftest agent-shell-workflow/def-defaults-category-and-target ()
  "Category defaults to General and target defaults to :ask when omitted."
  (asw-test/isolate
   (register-agent-shell-workflow bare :template "hi")
   (let ((spec (agent-shell-workflow-get 'bare)))
     (should (equal (agent-shell-workflow-spec-category spec) "General"))
     (should (eq (agent-shell-workflow-spec-target spec) :ask)))))

(ert-deftest agent-shell-workflow/def-requires-template ()
  "Registering without :template signals an error."
  (asw-test/isolate
   (should-error (agent-shell-workflow-register :id 'no-template))))

(ert-deftest agent-shell-workflow/redefine-replaces-entry ()
  "Re-registering an existing id replaces rather than duplicates it."
  (asw-test/isolate
   (register-agent-shell-workflow dup :template "one")
   (register-agent-shell-workflow dup :template "two")
   (should (= 1 (hash-table-count agent-shell-workflow-registry)))
   (should (equal (agent-shell-workflow-spec-template (agent-shell-workflow-get 'dup)) "two"))))

(ert-deftest agent-shell-workflow/list-returns-all-specs ()
  "agent-shell-workflow-list returns every registered spec."
  (asw-test/isolate
   (register-agent-shell-workflow a :template "a")
   (register-agent-shell-workflow b :template "b")
   (should (= 2 (length (agent-shell-workflow-list))))))

;;; ─────────────────────────────────────────────────────────────
;;; Template rendering

(ert-deftest agent-shell-workflow/render-substitutes-args ()
  "{{args.KEY}} resolves against the :args sub-plist."
  (should (equal (agent-shell-workflow-render "Hello {{args.name}}" (list :args (list :name "world")))
                 "Hello world")))

(ert-deftest agent-shell-workflow/render-substitutes-top-level ()
  "{{key}} without an args. prefix resolves against the top-level ctx."
  (should (equal (agent-shell-workflow-render "Log:\n{{ci-log}}" (list :ci-log "boom"))
                 "Log:\nboom")))

(ert-deftest agent-shell-workflow/render-missing-key-is-empty ()
  "An unresolved placeholder renders as an empty string, not an error."
  (should (equal (agent-shell-workflow-render "[{{missing}}]" nil) "[]")))

(ert-deftest agent-shell-workflow/render-multiple-placeholders ()
  "Multiple placeholders in one template all resolve correctly.
Regression test: `split-string' inside the replacer must not clobber the
match-data `replace-regexp-in-string' relies on for subsequent matches."
  (should (equal (agent-shell-workflow-render
                  "{{args.repo}}#{{args.run-id}} {{ci-summary}}"
                  (list :args (list :repo "acme/x" :run-id 42) :ci-summary "OK"))
                 "acme/x#42 OK")))

(ert-deftest agent-shell-workflow/render-numeric-arg-stringified ()
  "A non-string value (e.g. an integer arg) is stringified for substitution."
  (should (equal (agent-shell-workflow-render "run {{args.run-id}}" (list :args (list :run-id 7)))
                 "run 7")))

;;; ─────────────────────────────────────────────────────────────
;;; Argument collection

(ert-deftest agent-shell-workflow/collect-args-skips-provided ()
  "Arguments already present in the provided plist are not re-read."
  (asw-test/isolate
   (register-agent-shell-workflow needs-arg
     :args ((thing :prompt "Thing: "))
     :template "{{args.thing}}")
   (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (error "should not prompt"))))
     (should (equal (agent-shell-workflow--collect-args
                     (agent-shell-workflow-get 'needs-arg) (list :thing "given"))
                    (list :thing "given"))))))

(ert-deftest agent-shell-workflow/collect-args-reads-missing ()
  "A missing argument is read interactively via its declared prompt."
  (asw-test/isolate
   (register-agent-shell-workflow needs-arg
     :args ((thing :prompt "Thing: "))
     :template "{{args.thing}}")
   (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "typed")))
     (should (equal (agent-shell-workflow--collect-args (agent-shell-workflow-get 'needs-arg) nil)
                    (list :thing "typed"))))))

(ert-deftest agent-shell-workflow/collect-args-normalizes-bare-symbol-names ()
  "Bare-symbol arg-spec names normalize to keyword plist keys."
  (asw-test/isolate
   (should (eq (agent-shell-workflow--arg-key 'thing) :thing))
   (should (eq (agent-shell-workflow--arg-key :thing) :thing))))

(ert-deftest agent-shell-workflow/read-arg-integer-type-uses-read-number ()
  "An arg-spec of :type integer reads its value via `read-number'."
  (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 7)))
    (should (equal (agent-shell-workflow--read-arg '(run-id :type integer)) '(:run-id . 7)))))

(ert-deftest agent-shell-workflow/read-arg-symbol-type-uses-completing-read ()
  "An arg-spec of :type symbol reads its value via `completing-read', interned."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "chosen")))
    (should (equal (agent-shell-workflow--read-arg '(mode :type symbol)) '(:mode . chosen)))))

(ert-deftest agent-shell-workflow/read-arg-required-empty-signals-error ()
  "A required (non-optional) arg-spec errors when the typed value is empty."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (should-error (agent-shell-workflow--read-arg '(thing :prompt "Thing: ")) :type 'user-error)))

(ert-deftest agent-shell-workflow/read-arg-optional-empty-is-allowed ()
  "An :optional arg-spec permits an empty typed value without erroring."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (should (equal (agent-shell-workflow--read-arg '(thing :optional t)) '(:thing . "")))))

;;; ─────────────────────────────────────────────────────────────
;;; Pre/post execution engine

(ert-deftest agent-shell-workflow/exec-pre-noop-without-pre-op ()
  "With no pre-op, exec-pre calls back with ctx unchanged."
  (asw-test/isolate
   (register-agent-shell-workflow no-pre :template "x")
   (let (result)
     (agent-shell-workflow-exec-pre (agent-shell-workflow-get 'no-pre) '(:a 1)
                                  (lambda (ctx) (setq result ctx)))
     (should (equal result '(:a 1))))))

(ert-deftest agent-shell-workflow/exec-pre-synchronous ()
  "A one-argument pre-op runs synchronously and its return value is passed on."
  (asw-test/isolate
   (register-agent-shell-workflow sync-pre
     :pre-op (lambda (ctx) (plist-put ctx :extra "added"))
     :template "x")
   (let (result)
     (agent-shell-workflow-exec-pre (agent-shell-workflow-get 'sync-pre) (list :a 1)
                                  (lambda (ctx) (setq result ctx)))
     (should (equal (plist-get result :extra) "added")))))

(ert-deftest agent-shell-workflow/exec-pre-asynchronous ()
  "A two-argument pre-op is treated as async and must invoke its own callback."
  (asw-test/isolate
   (register-agent-shell-workflow async-pre
     :pre-op (lambda (ctx callback) (funcall callback (plist-put ctx :extra "async")))
     :template "x")
   (let (result)
     (agent-shell-workflow-exec-pre (agent-shell-workflow-get 'async-pre) (list :a 1)
                                  (lambda (ctx) (setq result ctx)))
     (should (equal (plist-get result :extra) "async")))))

(ert-deftest agent-shell-workflow/exec-post-defaults-to-done ()
  "With no post-op, exec-post returns :done."
  (asw-test/isolate
   (register-agent-shell-workflow no-post :template "x")
   (should (eq (agent-shell-workflow-exec-post (agent-shell-workflow-get 'no-post) nil nil "resp")
              :done))))

(ert-deftest agent-shell-workflow/exec-post-invokes-post-op ()
  "The post-op is called with shell-buffer, ctx, and response text."
  (asw-test/isolate
   (let (seen)
     (register-agent-shell-workflow with-post
       :post-op (lambda (buf ctx resp) (setq seen (list buf ctx resp)) :close)
       :template "x")
     (should (eq (agent-shell-workflow-exec-post (agent-shell-workflow-get 'with-post) 'buf '(:a 1) "resp")
                :close))
     (should (equal seen (list 'buf '(:a 1) "resp"))))))

(ert-deftest agent-shell-workflow/apply-post-result-drop-context ()
  ":drop-context enqueues a clear command for the shell buffer."
  (asw-test/isolate
   (let ((called nil))
     (cl-letf (((symbol-function 'agent-shell-queue-enqueue-clear)
                (lambda (buf) (setq called buf))))
       (agent-shell-workflow--apply-post-result :drop-context 'my-buf)
       (should (eq called 'my-buf))))))

(ert-deftest agent-shell-workflow/apply-post-result-close-kills-buffer ()
  ":close kills a live shell buffer."
  (asw-test/isolate
   (let ((buf (generate-new-buffer "asw-test-close")))
     (unwind-protect
         (progn
           (agent-shell-workflow--apply-post-result :close buf)
           (should-not (buffer-live-p buf)))
       (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-shell-workflow/apply-post-result-chain-dispatches-next ()
  "(:chain ID ARGS) dispatches the next prompt with session-reuse and submit."
  (asw-test/isolate
   (let (seen)
     (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
                (lambda (id &rest keys) (setq seen (cons id keys)))))
       (agent-shell-workflow--apply-post-result '(:chain next-id (:x 1)) 'buf)
       (should (eq (car seen) 'next-id))
       (should (equal (plist-get (cdr seen) :target) :session-reuse))
       (should (plist-get (cdr seen) :submit))))))

(ert-deftest agent-shell-workflow/apply-post-result-done-is-noop ()
  ":done (and any unrecognized value) does nothing observable."
  (asw-test/isolate
   (should (null (agent-shell-workflow--apply-post-result :done 'buf)))
   (should (null (agent-shell-workflow--apply-post-result :unknown-flag 'buf)))))

;;; ─────────────────────────────────────────────────────────────
;;; Dispatch routing

(ert-deftest agent-shell-workflow/resolve-target-passes-through-concrete-target ()
  "A concrete target resolves to itself without prompting."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "should not prompt"))))
    (should (eq (agent-shell-workflow--resolve-target :session-new) :session-new))))

(ert-deftest agent-shell-workflow/resolve-target-ask-prompts-via-completing-read ()
  ":ask resolves interactively via `completing-read', interned to a keyword."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ":queue")))
    (should (eq (agent-shell-workflow--resolve-target :ask) :queue))))

(ert-deftest agent-shell-workflow/dispatch-unknown-id-errors ()
  "Dispatching an unregistered id signals an error."
  (asw-test/isolate
   (should-error (agent-shell-workflow-dispatch 'does-not-exist))))

(ert-deftest agent-shell-workflow/dispatch-queue-target-enqueues ()
  "A :queue target enqueues a prompt-library item instead of inserting."
  (asw-test/isolate
   (register-agent-shell-workflow queued
     :args ((thing :prompt "Thing: "))
     :template "do {{args.thing}}"
     :target :queue)
   (let (enqueued)
     (cl-letf (((symbol-function 'agent-shell-queue--enqueue-args)
                (lambda (args kind buf) (setq enqueued (list args kind buf)))))
       (agent-shell-workflow-dispatch 'queued :args (list :thing "x"))
       (should (eq (nth 1 enqueued) 'workflow))
       (should (null (nth 2 enqueued)))
       (should (equal (plist-get (read (nth 0 enqueued)) :rendered) "do x"))))))

(ert-deftest agent-shell-workflow/dispatch-session-inserts-rendered-text ()
  "A session target renders the template and inserts it via agent-shell-insert."
  (asw-test/isolate
   (register-agent-shell-workflow direct
     :args ((thing :prompt "Thing: "))
     :template "do {{args.thing}}"
     :target :session-new
     :submit t)
   (let (inserted)
     (cl-letf (((symbol-function 'agent-shell-new-shell) (lambda (&rest _) 'new-buf))
               ((symbol-function 'agent-shell-insert)
                (lambda (&rest keys) (setq inserted keys))))
       (agent-shell-workflow-dispatch 'direct :args (list :thing "x"))
       (should (equal (plist-get inserted :text) "do x"))
       (should (plist-get inserted :submit))
       (should (eq (plist-get inserted :shell-buffer) 'new-buf))))))

(ert-deftest agent-shell-workflow/dispatch-session-reuse-prefers-existing-buffer ()
  ":session-reuse picks an existing directory-scoped buffer over creating one."
  (asw-test/isolate
   (register-agent-shell-workflow reuse-target :template "hi" :target :session-reuse :submit t)
   (let* ((buf (generate-new-buffer "asw-test-reuse"))
          new-shell-called)
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-workflow--project-buffers) (lambda (_dir) (list buf)))
                   ((symbol-function 'agent-shell-new-shell) (lambda (&rest _) (setq new-shell-called t)))
                   ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                   ((symbol-function 'agent-shell-insert) #'ignore))
           (agent-shell-workflow-dispatch 'reuse-target)
           (should-not new-shell-called))
       (kill-buffer buf)))))

(ert-deftest agent-shell-workflow/dispatch-with-post-op-subscribes-using-insertion-end ()
  "When a spec has a post-op, dispatch subscribes to turn-complete with a
:response-start position taken from agent-shell-insert's return value."
  (asw-test/isolate
   (register-agent-shell-workflow with-post
     :template "hi"
     :target :session-new
     :post-op (lambda (_buf _ctx _resp) :done))
   (let (subscribed-args)
     (cl-letf (((symbol-function 'agent-shell-new-shell) (lambda (&rest _) 'new-buf))
               ((symbol-function 'agent-shell-insert) (lambda (&rest _) (list (cons :end 42))))
               ((symbol-function 'agent-shell-subscribe-to)
                (lambda (&rest keys) (setq subscribed-args keys) 'token)))
       (agent-shell-workflow-dispatch 'with-post)
       (should (eq (plist-get subscribed-args :shell-buffer) 'new-buf))
       (should (eq (plist-get subscribed-args :event) 'turn-complete))))))

(ert-deftest agent-shell-workflow/dispatch-without-post-op-does-not-subscribe ()
  "When a spec has no post-op, dispatch never calls agent-shell-subscribe-to."
  (asw-test/isolate
   (register-agent-shell-workflow no-post-dispatch :template "hi" :target :session-new)
   (let (subscribe-called)
     (cl-letf (((symbol-function 'agent-shell-new-shell) (lambda (&rest _) 'new-buf))
               ((symbol-function 'agent-shell-insert) (lambda (&rest _) (list (cons :end 1))))
               ((symbol-function 'agent-shell-subscribe-to)
                (lambda (&rest _) (setq subscribe-called t))))
       (agent-shell-workflow-dispatch 'no-post-dispatch)
       (should-not subscribe-called)))))

(ert-deftest agent-shell-workflow/last-response-text-delegates-to-queue-walker ()
  "agent-shell-workflow--last-response-text reuses the queue's visible-text walk."
  (let ((buf (generate-new-buffer "asw-test-response")) seen)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-queue--collect-visible-response-text)
                   (lambda (sbuf start) (setq seen (list sbuf start)) "captured")))
          (should (equal (agent-shell-workflow--last-response-text buf 7) "captured"))
          (should (equal seen (list buf 7))))
      (kill-buffer buf))))

(ert-deftest agent-shell-workflow/last-response-text-nil-without-start-pos ()
  "With no :response-start (no post-op was registered), no text is captured."
  (let ((buf (generate-new-buffer "asw-test-response-nil")))
    (unwind-protect
        (should (null (agent-shell-workflow--last-response-text buf nil)))
      (kill-buffer buf))))

;;; ─────────────────────────────────────────────────────────────
;;; Queue integration

(ert-deftest agent-shell-workflow/queue-item-type-registered ()
  "The prompt-library item type is registered against agent-shell-queue."
  (should (agent-shell-queue--type-for-kind 'workflow)))

(ert-deftest agent-shell-workflow/dispatch-queue-item-inserts-rendered-text ()
  "Dispatching a queued prompt-library item inserts its stored rendered text."
  (asw-test/isolate
   (let* ((buf (generate-new-buffer "asw-test-queue-item"))
          (item (agent-shell-queue--make-item
                 (prin1-to-string (list :prompt-id 'x :rendered "queued text"))
                 nil 'workflow))
          inserted)
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-insert)
                    (lambda (&rest keys) (setq inserted keys))))
           (agent-shell-workflow--dispatch-queue-item item (buffer-name buf))
           (should (equal (plist-get inserted :text) "queued text"))
           (should (plist-get inserted :submit)))
       (kill-buffer buf)))))

;;; ─────────────────────────────────────────────────────────────
;;; Built-in library prompts

(ert-deftest agent-shell-workflow/library-registers-built-ins ()
  "The example prompt library registers all four built-in workflows."
  (dolist (id '(create-commit fix-ci pr-review-patch expand-coverage refactor-module))
    (should (agent-shell-workflow-get id))))

(ert-deftest agent-shell-workflow/library-diff-summary-short-diff ()
  "Short diff (<= 5 lines) is returned in full."
  (cl-letf (((symbol-function 'agent-shell-workflow-library--git-output)
             (lambda (&rest _) "--- a/f\n+++ b/f\n@@ -1 +1 @@\n-a\n+b")))
    (let ((res (agent-shell-workflow-library--diff-summary '("HEAD") 5)))
      (should (string-match-p "\\+b" res))
      (should-not (string-match-p "Diff exceeds" res)))))

(ert-deftest agent-shell-workflow/library-diff-summary-long-diff-uses-stat ()
  "Long diff (> 5 lines) uses diffstat and note."
  (cl-letf (((symbol-function 'agent-shell-workflow-library--git-output)
             (lambda (&rest args)
               (if (member "--stat" args)
                   " foo.el | 10 +++++-----\n 1 file changed"
                 "line1\nline2\nline3\nline4\nline5\nline6\nline7"))))
    (let ((res (agent-shell-workflow-library--diff-summary '("HEAD") 5)))
      (should (string-match-p "foo\\.el | 10" res))
      (should (string-match-p "Diff exceeds 5 lines" res)))))

(ert-deftest agent-shell-workflow/library-diff-summary-empty ()
  "Empty diff returns (no changes)."
  (cl-letf (((symbol-function 'agent-shell-workflow-library--git-output)
             (lambda (&rest _) "")))
    (should (equal (agent-shell-workflow-library--diff-summary '("HEAD") 5) "(no changes)"))))

(ert-deftest agent-shell-workflow/library-sanitize-name ()
  "agent-shell-workflow-library--sanitize-name strips invalid characters and trims hyphens."
  (should (equal (agent-shell-workflow-library--sanitize-name "feature/cool-branch#42") "feature-cool-branch-42"))
  (should (equal (agent-shell-workflow-library--sanitize-name "---lead-and-trail---") "lead-and-trail"))
  (should (equal (agent-shell-workflow-library--sanitize-name "safe.branch_1") "safe.branch_1"))
  (should (equal (agent-shell-workflow-library--sanitize-name nil) ""))
  (should (equal (agent-shell-workflow-library--sanitize-name "") "")))

(ert-deftest agent-shell-workflow/library-unique-artifact-dir-increments ()
  "agent-shell-workflow-library--unique-artifact-dir increments counter when directory exists."
  (let ((tmp-dir (make-temp-file "asq-uniq-dir-test-" t)))
    (unwind-protect
        (let* ((d1 (agent-shell-workflow-library--unique-artifact-dir tmp-dir "run-99" "main" "20260927"))
               (d2 (agent-shell-workflow-library--unique-artifact-dir tmp-dir "run-99" "main" "20260927"))
               (d3 (agent-shell-workflow-library--unique-artifact-dir tmp-dir "run-99" "main" "20260927"))
               (d-other (agent-shell-workflow-library--unique-artifact-dir tmp-dir "run-99" "feat/x" "20260927")))
          (should (equal (file-name-nondirectory d1) "20260927-run-99-main-1"))
          (should (equal (file-name-nondirectory d2) "20260927-run-99-main-2"))
          (should (equal (file-name-nondirectory d3) "20260927-run-99-main-3"))
          (should (equal (file-name-nondirectory d-other) "20260927-run-99-feat-x-1"))
          (should (file-directory-p d1))
          (should (file-directory-p d2))
          (should (file-directory-p d3))
          (should (file-directory-p d-other)))
      (delete-directory tmp-dir t))))

(ert-deftest agent-shell-workflow/library-fix-ci-pre-op-populates-ctx ()
  "fix-ci's pre-op fetches summary and log text via gh into ctx."
  (let ((tmp-dir (make-temp-file "asq-fix-ci-pre-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-workflow-library--project-root)
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function 'agent-shell-workflow-library--shell)
                   (lambda (&rest args) (mapconcat #'identity args " "))))
          (let ((ctx (agent-shell-workflow-library--fix-ci-pre-op
                      (list :args (list :repo "acme/x" :run-id 9)))))
            (should (string-match-p "acme/x" (plist-get ctx :ci-summary)))
            (should (string-match-p "--log-failed" (plist-get ctx :ci-log)))))
      (delete-directory tmp-dir t))))

(ert-deftest agent-shell-workflow/library-fix-ci-saves-files-and-renders-template ()
  "fix-ci pre-op saves logs/jobs/index to filesystem and renders template with paths."
  (let ((tmp-dir (make-temp-file "asq-fix-ci-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function (quote agent-shell-workflow-library--project-root))
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function (quote agent-shell-workflow-library--shell))
                   (lambda (&rest args)
                     (cond
                      ((member "--log-failed" args)
                       "--- FAIL: TestWidget (0.05s)\n    widget_test.go:42: unexpected nil\nFAIL")
                      ((member "--json" args)
                       "{\"jobs\":[{\"name\":\"test\",\"conclusion\":\"failure\"}]}")
                      (t
                       "X Run #123 (Workflow: CI, Branch: main)")))))
          (let* ((spec (agent-shell-workflow-get (quote fix-ci)))
                 (ctx (agent-shell-workflow-library--fix-ci-pre-op
                       (list :args (list :repo "org/app" :run-id 123))))
                 (log-path (expand-file-name (plist-get ctx :ci-log-file) tmp-dir))
                 (jobs-path (expand-file-name (plist-get ctx :ci-jobs-file) tmp-dir))
                 (index-path (expand-file-name (plist-get ctx :ci-index-file) tmp-dir))
                 (rendered (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) ctx)))
            ;; Files exist on disk
            (should (file-exists-p log-path))
            (should (file-exists-p jobs-path))
            (should (file-exists-p index-path))
            ;; Content was written
            (with-temp-buffer
              (insert-file-contents log-path)
              (should (string-match-p "TestWidget" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents index-path)
              (should (string-match-p "CI Triage Index" (buffer-string))))
            ;; Rendered template refers to files and gives triage guidance
            (should (string-match-p "\\.agent/fix-ci/[^/]+/run-123-logs\\.txt" rendered))
            (should (string-match-p "\\.agent/fix-ci/[^/]+/run-123-jobs\\.json" rendered))
            (should (string-match-p "\\.agent/fix-ci/[^/]+/ci-triage-index\\.md" rendered))
            (should (string-match-p "\\.agent/fix-ci/[^/]+/fix-plan\\.md" rendered))
            (should (string-match-p "Do NOT read the entire log file into context" rendered))
            (should (string-match-p "150-200 lines" rendered))
            ;; Rendered template does NOT inline the entire log body or full page
            (should-not (string-match-p "widget_test\\.go:42" rendered))))
      (delete-directory tmp-dir t))))



(ert-deftest agent-shell-workflow/library-fix-ci-unique-directories-prevent-collision ()
  "fix-ci pre-op creates distinct directories across multiple attempts without overwriting."
  (let ((tmp-dir (make-temp-file "asq-fix-ci-col-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-workflow-library--project-root)
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function 'agent-shell-workflow-library--shell)
                   (lambda (&rest args)
                     (cond
                      ((member "--log-failed" args) "attempt log")
                      ((member "--json" args) "{\"jobs\":[]}")
                      (t "Run summary")))))
          (let* ((ctx1 (agent-shell-workflow-library--fix-ci-pre-op
                        (list :args (list :repo "org/app" :run-id 123 :date "20260927"))))
                 (log1 (expand-file-name (plist-get ctx1 :ci-log-file) tmp-dir))
                 (plan1 (expand-file-name (plist-get ctx1 :ci-plan-file) tmp-dir)))
            ;; Simulate agent writing plan in attempt 1
            (with-temp-file plan1 (insert "plan for attempt 1"))
            ;; Now run fix-ci again for the same run
            (let* ((ctx2 (agent-shell-workflow-library--fix-ci-pre-op
                          (list :args (list :repo "org/app" :run-id 123 :date "20260927"))))
                   (log2 (expand-file-name (plist-get ctx2 :ci-log-file) tmp-dir))
                   (plan2 (expand-file-name (plist-get ctx2 :ci-plan-file) tmp-dir)))
              (should-not (equal (plist-get ctx1 :ci-dir) (plist-get ctx2 :ci-dir)))
              (should (string-match-p "-1$" (plist-get ctx1 :ci-dir)))
              (should (string-match-p "-2$" (plist-get ctx2 :ci-dir)))
              ;; Attempt 1 plan is not overwritten
              (with-temp-buffer
                (insert-file-contents plan1)
                (should (equal (buffer-string) "plan for attempt 1")))
              ;; Attempt 2 plan file is distinct and does not yet exist
              (should-not (file-exists-p plan2))
              (should (file-exists-p log1))
              (should (file-exists-p log2)))))
      (delete-directory tmp-dir t))))

(ert-deftest agent-shell-workflow/library-resolve-ci-run-auto-selects-failing ()
  "Auto-selects the latest run when its conclusion indicates failure."
  (let ((runs '(((databaseId . 34348785571) (displayTitle . "bump: dependencies") (status . "completed") (conclusion . "failure") (headBranch . "main") (headSha . "dd16f2f0107ebda9373d78cda7049397db797605"))
                ((databaseId . 34295328476) (displayTitle . "fix bug") (status . "completed") (conclusion . "success") (headBranch . "main") (headSha . "cdc9b486796226f379f82f32a66b385408cc19be")))))
    (cl-letf (((symbol-function 'agent-shell-workflow-library--fetch-runs) (lambda (&rest _) runs))
              ((symbol-function 'agent-shell-workflow-library--current-branch) (lambda () "main"))
              ((symbol-function 'annotated-completing-read) (lambda (&rest _) (error "should not prompt"))))
      (should (= 34348785571 (agent-shell-workflow-library--resolve-ci-run "acme/x" "main"))))))

(ert-deftest agent-shell-workflow/library-resolve-ci-run-uses-acr-picker-when-not-failing ()
  "Presents an ACR picker when the latest run is not failing."
  (let ((runs '(((databaseId . 111) (displayTitle . "feat: test") (status . "completed") (conclusion . "success") (headBranch . "main") (headSha . "abc1234567") (startedAt . "2026-09-09T10:00:00Z") (updatedAt . "2026-09-09T10:02:00Z") (createdAt . "2026-09-09T10:00:00Z"))
                ((databaseId . 222) (displayTitle . "fix: bug") (status . "completed") (conclusion . "failure") (headBranch . "main") (headSha . "def9876543") (startedAt . "2026-09-09T08:00:00Z") (updatedAt . "2026-09-09T08:01:30Z") (createdAt . "2026-09-09T08:00:00Z"))))
        acr-called)
    (cl-letf (((symbol-function 'agent-shell-workflow-library--fetch-runs) (lambda (&rest _) runs))
              ((symbol-function 'agent-shell-workflow-library--current-branch) (lambda () "main"))
              ((symbol-function 'annotated-completing-read)
               (lambda (table &rest _) (setq acr-called table) (caar table)))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _) (setq acr-called table) (caar table))))
      (should (= 111 (agent-shell-workflow-library--resolve-ci-run "acme/x" "main")))
      (should acr-called)
      (should (string-match-p "success" (cdar acr-called))))))
(ert-deftest agent-shell-workflow/library-gather-runs-each-pair-into-ctx ()
  "agent-shell-workflow-library--gather stores each command's output under its key."
  (cl-letf (((symbol-function 'agent-shell-workflow-library--shell)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-workflow-library--gather
                '(:a 1) (list (list :one "echo" "one") (list :two "echo" "two")))))
      (should (equal (plist-get ctx :one) "echo one"))
      (should (equal (plist-get ctx :two) "echo two"))
      (should (= 1 (plist-get ctx :a))))))

(ert-deftest agent-shell-workflow/library-pr-review-pre-op-populates-ctx ()
  "pr-review-patch's pre-op fetches PR comments via gh into ctx."
  (let ((tmp-dir (make-temp-file "asq-pr-pre-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-workflow-library--project-root)
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function 'agent-shell-workflow-library--shell)
                   (lambda (&rest args) (mapconcat #'identity args " "))))
          (let ((ctx (agent-shell-workflow-library--pr-review-pre-op
                      (list :args (list :pr-number 42)))))
            (should (string-match-p "42" (plist-get ctx :pr-comments)))
            (should (string-match-p "--comments" (plist-get ctx :pr-comments)))))
      (delete-directory tmp-dir t))))

(ert-deftest agent-shell-workflow/library-pr-review-saves-files-and-renders-template ()
  "pr-review-patch pre-op saves markdown/JSON to filesystem and renders template with paths."
  (let ((tmp-dir (make-temp-file "asq-pr-review-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function (quote agent-shell-workflow-library--project-root))
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function (quote agent-shell-workflow-library--shell))
                   (lambda (&rest args)
                     (cond
                      ((member "--json" args)
                       "{\"title\":\"Support Foo\",\"author\":{\"login\":\"alice\"},\"url\":\"https://github.com/org/app/pull/55\",\"reviews\":[{\"author\":{\"login\":\"bob\"},\"state\":\"COMMENTED\",\"body\":\"Please simplify this function.\"}],\"comments\":[]}")
                      ((member "repos/org/app/pulls/55/comments" args)
                       "[{\"user\":{\"login\":\"carol\"},\"path\":\"widget.go\",\"line\":25,\"diff_hunk\":\"@@ -20,4 +20,5 @@\",\"body\":\"Handle error return here.\"}]")
                      ((member "--comments" args)
                       "Review by bob: Please simplify this function.")
                      (t "")))))
          (let* ((spec (agent-shell-workflow-get (quote pr-review-patch)))
                 (ctx (agent-shell-workflow-library--pr-review-pre-op
                       (list :args (list :repo "org/app" :pr-number 55))))
                 (md-path (expand-file-name (plist-get ctx :pr-comments-file) tmp-dir))
                 (json-path (expand-file-name (plist-get ctx :pr-comments-json-file) tmp-dir))
                 (alias-md (expand-file-name "pr-comments.md" (expand-file-name (plist-get ctx :pr-dir) tmp-dir)))
                 (rendered (agent-shell-workflow-render (agent-shell-workflow-spec-template spec) ctx)))
            ;; Files exist on disk
            (should (file-exists-p md-path))
            (should (file-exists-p json-path))
            (should (file-exists-p alias-md))
            ;; Markdown formatted properly
            (with-temp-buffer
              (insert-file-contents md-path)
              (let ((text (buffer-string)))
                (should (string-match-p "PR #55 Comments: Support Foo" text))
                (should (string-match-p "Review by @bob" text))
                (should (string-match-p "Inline Comment by @carol on `widget\\.go`" text))))
            ;; Rendered template directs agent to read files
            (should (string-match-p "\\.agent/pr-comments/[^/]+/pr-55-comments\\.md" rendered))
            (should (string-match-p "\\.agent/pr-comments/[^/]+/pr-55-comments\\.json" rendered))
            (should (string-match-p "\\.agent/pr-comments/[^/]+/review-plan\\.md" rendered))
            (should (string-match-p "Do NOT read all raw comment data into context" rendered))
            (should (string-match-p "change-required" rendered))
            ;; Rendered template does NOT inline the entire comments dump
            (should-not (string-match-p "Please simplify this function" rendered))))
      (delete-directory tmp-dir t))))



(ert-deftest agent-shell-workflow/library-pr-review-unique-directories-prevent-collision ()
  "pr-review-patch pre-op creates distinct directories across multiple attempts without overwriting."
  (let ((tmp-dir (make-temp-file "asq-pr-col-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-workflow-library--project-root)
                   (lambda () (file-name-as-directory tmp-dir)))
                  ((symbol-function 'agent-shell-workflow-library--shell)
                   (lambda (&rest args)
                     (cond
                      ((member "--json" args) "{\"title\":\"Col PR\",\"author\":{\"login\":\"u\"},\"reviews\":[],\"comments\":[]}")
                      (t "")))))
          (let* ((ctx1 (agent-shell-workflow-library--pr-review-pre-op
                        (list :args (list :repo "org/app" :pr-number 42 :date "20260927"))))
                 (plan1 (expand-file-name (plist-get ctx1 :pr-plan-file) tmp-dir)))
            (with-temp-file plan1 (insert "review plan 1"))
            (let* ((ctx2 (agent-shell-workflow-library--pr-review-pre-op
                          (list :args (list :repo "org/app" :pr-number 42 :date "20260927"))))
                   (plan2 (expand-file-name (plist-get ctx2 :pr-plan-file) tmp-dir)))
              (should-not (equal (plist-get ctx1 :pr-dir) (plist-get ctx2 :pr-dir)))
              (should (string-match-p "-1$" (plist-get ctx1 :pr-dir)))
              (should (string-match-p "-2$" (plist-get ctx2 :pr-dir)))
              (with-temp-buffer
                (insert-file-contents plan1)
                (should (equal (buffer-string) "review plan 1")))
              (should-not (file-exists-p plan2)))))
      (delete-directory tmp-dir t))))

(ert-deftest agent-shell-workflow/library-coverage-pre-op-populates-ctx ()
  "expand-coverage's pre-op diffs :file against HEAD into ctx."
  (cl-letf (((symbol-function 'agent-shell-workflow-library--git-output)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-workflow-library--coverage-pre-op
                (list :args (list :file "foo.el")))))
      (should (string-match-p "HEAD -- foo.el" (plist-get ctx :file-diff))))))

(ert-deftest agent-shell-workflow/library-refactor-pre-op-populates-ctx ()
  "refactor-module's pre-op gathers recent git log for :file into ctx."
  (cl-letf (((symbol-function 'agent-shell-workflow-library--git-output)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-workflow-library--refactor-pre-op
                (list :args (list :file "foo.el")))))
      (should (string-match-p "log --oneline -n 5 -- foo.el" (plist-get ctx :recent-history))))))

(ert-deftest agent-shell-workflow/library-git-output-delegates-to-magit ()
  "agent-shell-workflow-library--git-output uses magit-git-output when available."
  (let (called-with)
    (cl-letf (((symbol-function 'magit-git-output)
               (lambda (&rest args) (setq called-with args) " magit-result \n")))
      (should (equal (agent-shell-workflow-library--git-output "status" "--short") "magit-result"))
      (should (equal called-with '("status" "--short"))))))

(ert-deftest agent-shell-workflow/library-git-output-falls-back-to-shell ()
  "agent-shell-workflow-library--git-output falls back to shell when magit is absent."
  (let (called-with)
    (cl-letf (((symbol-function 'magit-git-output) nil)
              ((symbol-function 'agent-shell-workflow-library--shell)
               (lambda (&rest args) (setq called-with args) "shell-result")))
      (should (equal (agent-shell-workflow-library--git-output "status" "--short") "shell-result"))
      (should (equal called-with '("git" "status" "--short"))))))

(ert-deftest agent-shell-workflow/library-create-commit-pre-op-without-files ()
  "create-commit pre-op gathers status, diff HEAD, and recent log without files arg."
  (cl-letf (((symbol-function 'agent-shell-workflow-library--git-output)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-workflow-library--create-commit-pre-op nil)))
      (should (string-match-p "status --short" (plist-get ctx :git-status)))
      (should (string-match-p "HEAD" (plist-get ctx :git-diff)))
      (should (string-match-p "log --oneline -n 5" (plist-get ctx :recent-log))))))

(ert-deftest agent-shell-workflow/library-create-commit-pre-op-with-files ()
  "create-commit pre-op scopes status and diff to specified files."
  (cl-letf (((symbol-function 'agent-shell-workflow-library--git-output)
             (lambda (&rest args) (mapconcat #'identity args " "))))
    (let ((ctx (agent-shell-workflow-library--create-commit-pre-op
                (list :args (list :files "src/lib.el")))))
      (should (string-match-p "status --short -- src/lib.el" (plist-get ctx :git-status)))
      (should (string-match-p "HEAD -- src/lib.el" (plist-get ctx :git-diff)))
      (should (string-match-p "log --oneline -n 5" (plist-get ctx :recent-log))))))

;;; ─────────────────────────────────────────────────────────────
;;; ACR menu

(ert-deftest agent-shell-workflow/select-dispatches-chosen-prompt ()
  "agent-shell-workflow-select dispatches whichever spec the picker returns."
  (asw-test/isolate
   (register-agent-shell-workflow choosable :template "hi")
   (let (dispatched)
     (cl-letf (((symbol-function 'annotated-completing-read)
                (lambda (table &rest _) (cddr (assoc "choosable" table))))
               ((symbol-function 'agent-shell-workflow-dispatch)
                (lambda (id &rest _) (setq dispatched id))))
       (agent-shell-workflow-select)
       (should (eq dispatched 'choosable))))))

(ert-deftest agent-shell-workflow/select-errors-when-registry-empty ()
  "Selecting from an empty registry signals a user-error."
  (asw-test/isolate
   (should-error (agent-shell-workflow-select) :type 'user-error)))

(ert-deftest agent-shell-workflow/select-filters-by-category ()
  "Passing a category to agent-shell-workflow-select narrows the candidates."
  (asw-test/isolate
   (register-agent-shell-workflow in-cat :category "CI/CD" :template "hi")
   (register-agent-shell-workflow other-cat :category "Testing" :template "hi")
   (should (= 1 (length (agent-shell-workflow--candidates "CI/CD"))))))

(ert-deftest agent-shell-workflow/dispatch-menu-key-integrity ()
  "No key in agent-shell-workflow-dispatch-menu conflicts or duplicates."
  (let ((keys (transient-test/collect-keys 'agent-shell-workflow-dispatch-menu)))
    (should (null (transient-test/key-prefix-conflicts keys)))
    (should (null (transient-test/duplicate-keys keys)))))

(ert-deftest agent-shell-workflow/dispatch-menu-suffix-registered ()
  "agent-shell-workflow-dispatch-menu is a transient prefix wrapping the ACR picker."
  (should (get 'agent-shell-workflow-dispatch-menu 'transient--layout))
  (should (commandp 'agent-shell-workflow-select)))

(ert-deftest agent-shell-workflow/create-shell-binds-default-directory ()
  "agent-shell-workflow--create-shell calls agent-shell-new-shell with default-directory set."
  (let ((seen-dir nil))
    (cl-letf (((symbol-function 'agent-shell-new-shell)
               (lambda () (setq seen-dir default-directory) 'new-shell-buffer)))
      (let ((res (agent-shell-workflow--create-shell "/tmp/test-dir/")))
        (should (eq res 'new-shell-buffer))
        (should (equal seen-dir "/tmp/test-dir/"))))))

(ert-deftest agent-shell-workflow/session-buffer-session-new ()
  "agent-shell-workflow--session-buffer creates new shell buffer for :session-new."
  (cl-letf (((symbol-function 'agent-shell-workflow--create-shell)
             (lambda (&optional _dir) 'created-shell)))
    (should (eq (agent-shell-workflow--session-buffer :session-new) 'created-shell))))

(ert-deftest agent-shell-workflow/session-buffer-session-reuse-fallback ()
  "agent-shell-workflow--session-buffer falls back to creating shell when no project buffers exist."
  (cl-letf (((symbol-function 'agent-shell-workflow--project-buffers)
             (lambda (_dir) nil))
            ((symbol-function 'agent-shell-workflow--create-shell)
             (lambda (&optional _dir) 'fallback-shell)))
    (should (eq (agent-shell-workflow--session-buffer :session-reuse) 'fallback-shell))))

(ert-deftest agent-shell-workflow/session-buffer-session-reuse-declined ()
  "agent-shell-workflow--session-buffer creates shell when user declines reuse."
  (let ((buf (generate-new-buffer "asp-reuse-declined")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-workflow--project-buffers)
                   (lambda (_dir) (list buf)))
                  ((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
                  ((symbol-function 'agent-shell-workflow--create-shell)
                   (lambda (&optional _dir) 'declined-shell)))
          (should (eq (agent-shell-workflow--session-buffer :session-reuse) 'declined-shell)))
      (kill-buffer buf))))

(ert-deftest agent-shell-workflow/session-buffer-session-reuse-multiple-prompts ()
  "agent-shell-workflow--session-buffer prompts with completing-read when multiple buffers exist."
  (let ((b1 (generate-new-buffer "asp-reuse-b1"))
        (b2 (generate-new-buffer "asp-reuse-b2"))
        picked)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-workflow--project-buffers)
                   (lambda (_dir) (list b1 b2)))
                  ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _) (setq picked (buffer-name b1)) (buffer-name b1))))
          (let ((res (agent-shell-workflow--session-buffer :session-reuse)))
            (should (eq res b1))
            (should (equal picked (buffer-name b1)))))
      (kill-buffer b1)
      (kill-buffer b2))))


(ert-deftest agent-shell-workflow/dispatch-target-functions-hook ()
  "agent-shell-workflow-dispatch-target-functions intercepts custom targets."
  (asw-test/isolate
   (register-agent-shell-workflow custom-tgt
     :args ((val :prompt "Val: "))
     :template "Custom {{args.val}}"
     :target :custom-worker)
   (let (hook-args)
     (let ((agent-shell-workflow-dispatch-target-functions
            (list (lambda (spec ctx target submit)
                    (when (eq target :custom-worker)
                      (setq hook-args (list spec ctx target submit))
                      t)))))
       (agent-shell-workflow-dispatch 'custom-tgt :args (list :val "123"))
       (should hook-args)
       (should (eq (nth 2 hook-args) :custom-worker))
       (should (equal (plist-get (nth 1 hook-args) :rendered) "Custom 123"))))))

(ert-deftest agent-shell-workflow/dispatch-queue-unregistered-fails ()
  "Dispatching to :queue without a registered handler raises a user-error."
  (asw-test/isolate
   (register-agent-shell-workflow queue-unreg
     :template "Queue this"
     :target :queue)
   (let ((agent-shell-workflow-dispatch-target-functions nil))
     (should-error
      (agent-shell-workflow-dispatch 'queue-unreg)
      :type 'user-error))))

(provide 'test-agent-shell-workflow)

;;; test-agent-shell-workflow.el ends here
