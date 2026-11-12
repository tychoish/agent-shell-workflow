;;; test-agent-shell-workflow-github.el --- PR status sync tests -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-helper)
(require 'agent-shell-workflow)
(require 'agent-shell-workflow-github)

(defun aswg-test/pr (num &rest kw)
  "Build a tracking PR plist for NUM, overridden by KW."
  (let ((tr (list :last-seen "[2026-11-11 Wed 12:45]" :delta-type 'new
                  :delta-reason "newly seen"
                  :action-summary (or (plist-get kw :action) "needs 1 review")
                  :failing-checks nil)))
    (list :number num :repo (or (plist-get kw :repo) "o/r")
          :title (format "Title %d" num) :branch (format "b%d" num)
          :url (format "https://x/%d" num)
          :bucket (or (plist-get kw :bucket) 'reviews) :approvals 0
          :threads-open 0 :tracking tr)))

(defmacro aswg-test/with-file (var content &rest body)
  "Run BODY with VAR bound to a temp file holding CONTENT (nil: absent)."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "aswg" t))
          (,var (expand-file-name "status.org" dir)))
     (unwind-protect
         (progn (when ,content (with-temp-file ,var (insert ,content)))
                ,@body)
       (delete-directory dir t))))

(defun aswg-test/slurp (f)
  (with-temp-buffer (insert-file-contents f) (buffer-string)))

(defun aswg-test/count (string regexp)
  (with-temp-buffer (insert string) (goto-char (point-min)) (how-many regexp)))

(defconst aswg-test/fixture
  "#+title: Open PR status (@me)
#+filetags: :agent:prs:

* Summary
old junk
* Tasks
** TODO 1: needs 1 review
:PROPERTIES:
:REPO: o/r
:PR: 1
:STEP: reviews
:LAST_SEEN: [2026-01-01 Thu 00:00]
:END:
:LOGBOOK:
- Note taken on [2026-01-02 Fri 10:00] \\\\
  keep me
:END:
User note line.
*** Notes and In-Flight Decisions
- my decision
** TODO 2: gone
:PROPERTIES:
:REPO: o/r
:PR: 2
:STEP: ready
:END:
")

(ert-deftest aswg-test/preserves-user-content ()
  (aswg-test/with-file f aswg-test/fixture
    (agent-shell-workflow-github-pr-sync
     (list (aswg-test/pr 1) (aswg-test/pr 3)) f)
    (let ((s (aswg-test/slurp f)))
      (should (string-match-p "keep me" s))
      (should (string-match-p "User note line\\." s))
      (should (string-match-p "- my decision" s))
      (should-not (string-match-p "old junk" s))
      (should (string-match-p "^\\*\\* TODO 3: needs 1 review" s))
      (should (string-match-p ":PR: 3" s))
      (should (string-match-p "^\\*\\*\\* Notes and In-Flight Decisions" s)))))

(ert-deftest aswg-test/step-change-updates-heading ()
  (aswg-test/with-file f aswg-test/fixture
    (agent-shell-workflow-github-pr-sync
     (list (aswg-test/pr 1 :bucket 'ready :action "ready to merge")) f)
    (let ((s (aswg-test/slurp f)))
      (should (string-match-p "^\\*\\* TODO 1: ready to merge$" s))
      (should (string-match-p ":STEP: ready" s))
      (should (string-match-p ":LAST_SEEN: \\[2026-11-11 Wed 12:45\\]" s))
      (should (string-match-p "keep me" s)))))

(ert-deftest aswg-test/closed-pr-done ()
  (aswg-test/with-file f aswg-test/fixture
    (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
    (let ((s (aswg-test/slurp f)))
      (should (string-match-p "^\\*\\* DONE 2: gone" s))
      (should (string-match-p "CLOSED: \\[" s))
      (should (string-match-p "^\\*\\* TODO 1:" s)))))

(ert-deftest aswg-test/summary-table ()
  (aswg-test/with-file f aswg-test/fixture
    (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
    (let ((s (aswg-test/slurp f)))
      (should (string-match-p "Last refreshed: \\[" s))
      (should (string-match-p "| PR +| Repo +| Branch +| Approvals +| CI failing +| Unresolved +| Bucket" s))
      (should (string-match-p "| +1 | o/r +| b1 " s)))))

(ert-deftest aswg-test/skeleton-created ()
  (aswg-test/with-file f nil
    (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 7)) f)
    (let ((s (aswg-test/slurp f)))
      (should (string-prefix-p "#+title: Open PR status (@me)\n#+filetags: :agent:prs:" s))
      (should (string-match-p "^\\* Summary" s))
      (should (string-match-p "^\\* Tasks" s))
      (should (string-match-p "^\\*\\* TODO 7:" s)))))

(ert-deftest aswg-test/idempotent ()
  (aswg-test/with-file f aswg-test/fixture
    (let ((prs (list (aswg-test/pr 1))))
      (agent-shell-workflow-github-pr-sync prs f)
      (let ((a (aswg-test/slurp f)))
        (agent-shell-workflow-github-pr-sync prs f)
        (should (equal (replace-regexp-in-string "Last refreshed.*" "" a)
                       (replace-regexp-in-string "Last refreshed.*" ""
                                                 (aswg-test/slurp f))))))))

(ert-deftest aswg-test/refuses-modified-visiting-buffer ()
  (aswg-test/with-file f aswg-test/fixture
    (let ((buf (find-file-noselect f)))
      (unwind-protect
          (progn (with-current-buffer buf (insert "x"))
                 (should-error (agent-shell-workflow-github-pr-sync nil f)
                               :type 'user-error)
                 (should-not (string-match-p "^x" (aswg-test/slurp f))))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest aswg-test/reverts-visiting-buffer ()
  (aswg-test/with-file f aswg-test/fixture
    (let ((buf (find-file-noselect f)))
      (unwind-protect
          (progn (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 9)) f)
                 (with-current-buffer buf
                   (should (string-match-p "TODO 9:" (buffer-string)))))
        (kill-buffer buf)))))

;;; Workflow pre-op

(defmacro aswg-test/with-mocks (collect &rest body)
  (declare (indent 1))
  `(let ((agent-shell-workflow-github--pr-status-fingerprint nil)
         (committed 0))
     ;; Load the real library first so the pre-op's `require' cannot clobber mocks.
     (require 'magit-dash-gh-pr nil t)
     (cl-letf (((symbol-function 'magit-dash-gh-pr-status-collect-all) ,collect)
               ((symbol-function 'magit-dash-gh-pr-attach-tracking) #'identity)
               ((symbol-function 'magit-dash-gh-pr-commit-tracking)
                (lambda (prs) (cl-incf committed) prs))
               ((symbol-function 'magit-dash-gh-pr-tracking-fingerprint)
                (lambda (prs) (format "%S" (mapcar (lambda (p) (plist-get p :bucket)) prs)))))
       ,@body)))

(ert-deftest aswg-test/custom-todo-keywords-close-and-reopen ()
  "TODO NEXT | DONE CANCELED: closing uses the first done keyword, reopen uses TODO."
  (let ((org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE" "CANCELED"))))
    (aswg-test/with-file f (concat "* Summary\n* Tasks\n"
                                   "** NEXT 1: needs 1 review\n:PROPERTIES:\n:REPO: o/r\n:PR: 1\n:STEP: reviews\n:END:\n"
                                   "** CANCELED 2: was cancelled\n:PROPERTIES:\n:REPO: o/r\n:PR: 2\n:STEP: reviews\n:END:\n"
                                   "** NEXT 3: gone\n:PROPERTIES:\n:REPO: o/r\n:PR: 3\n:STEP: reviews\n:END:\n")
      (agent-shell-workflow-github-pr-sync
       (list (aswg-test/pr 1) (aswg-test/pr 2 :bucket 'ready :action "ready to merge")) f)
      (let ((s (aswg-test/slurp f)))
        ;; In-progress keyword preserved on unchanged-step PR 1.
        (should (string-match-p "^\\*\\* NEXT 1: needs 1 review$" s))
        ;; Step change on a CANCELED task reopens it as TODO.
        (should (string-match-p "^\\*\\* TODO 2: ready to merge$" s))
        ;; Vanished open PR 3 is closed with the first done keyword.
        (should (string-match-p "^\\*\\* DONE 3: gone" s))))))

(ert-deftest aswg-test/custom-keywords-without-todo ()
  "With `NEXT | DONE' (no TODO keyword) sync must still produce valid tasks."
  (let ((org-todo-keywords '((sequence "NEXT" "|" "DONE"))))
    (aswg-test/with-file f nil
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 7)) f)
      (with-temp-buffer
        (insert-file-contents f)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* .*7:")
        (should (member (org-get-todo-state) '("NEXT" "TODO")))
        (should (equal "7: needs 1 review" (org-get-heading t t t t)))))))

(ert-deftest aswg-test/pr-without-notes-child ()
  (aswg-test/with-file f (concat "* Summary\n* Tasks\n"
                                 "** TODO 1: needs 1 review\n:PROPERTIES:\n:REPO: o/r\n:PR: 1\n:STEP: reviews\n:END:\nbody\n"
                                 "** TODO 2: other\n:PROPERTIES:\n:REPO: o/r\n:PR: 2\n:STEP: reviews\n:END:\n")
    (agent-shell-workflow-github-pr-sync
     (list (aswg-test/pr 1 :bucket 'ready :action "ready to merge")
           (aswg-test/pr 2)) f)
    (let ((s (aswg-test/slurp f)))
      (should (string-match-p "^\\*\\* TODO 1: ready to merge$" s))
      (should (string-match-p ":STEP: +ready" s))
      (should (string-match-p "^body$" s))
      ;; No Notes heading is invented for existing entries.
      (should-not (string-match-p "Notes and In-Flight" s)))
    ;; Closing such a PR also works.
    (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 2)) f)
    (should (string-match-p "^\\*\\* DONE 1:" (aswg-test/slurp f)))))

(ert-deftest aswg-test/same-pr-number-in-two-repos ()
  (aswg-test/with-file f nil
    (let ((prs (list (aswg-test/pr 5 :repo "o/a") (aswg-test/pr 5 :repo "o/b"))))
      (agent-shell-workflow-github-pr-sync prs f)
      (let ((s (aswg-test/slurp f)))
        (should (= 2 (aswg-test/count s "^\\*\\* TODO 5:")))
        (should (string-match-p ":REPO: o/a\n:PR: 5" s))
        (should (string-match-p ":REPO: o/b\n:PR: 5" s)))
      ;; Update only o/b; o/a must not change.  Then drop o/a only.
      (agent-shell-workflow-github-pr-sync
       (list (aswg-test/pr 5 :repo "o/a")
             (aswg-test/pr 5 :repo "o/b" :bucket 'ready :action "ready to merge")) f)
      (with-temp-buffer
        (insert-file-contents f) (org-mode)
        (let ((by-repo (org-map-entries
                        (lambda () (cons (org-entry-get nil "REPO") (org-entry-get nil "STEP")))
                        "PR=\"5\"")))
          (should (equal "reviews" (cdr (assoc "o/a" by-repo))))
          (should (equal "ready" (cdr (assoc "o/b" by-repo))))))
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 5 :repo "o/b")) f)
      (with-temp-buffer
        (insert-file-contents f) (org-mode)
        (should (equal '(("o/a" . "DONE") ("o/b" . "TODO"))
                       (org-map-entries
                        (lambda () (cons (org-entry-get nil "REPO") (org-get-todo-state)))
                        "PR=\"5\"")))))))

(ert-deftest aswg-test/empty-pr-list ()
  ;; Absent file: skeleton only, no tasks.
  (aswg-test/with-file f nil
    (agent-shell-workflow-github-pr-sync nil f)
    (let ((s (aswg-test/slurp f)))
      (should (string-match-p "^\\* Summary" s))
      (should (string-match-p "^\\* Tasks" s))
      (should (string-match-p "| PR +|" s))
      (should-not (string-match-p ":PR:" s))))
  ;; An empty list refreshes Summary but leaves open tasks open.
  (aswg-test/with-file f aswg-test/fixture
    (agent-shell-workflow-github-pr-sync nil f)
    (let ((s (aswg-test/slurp f)))
      (should (string-match-p "^\\*\\* TODO 1:" s))
      (should (string-match-p "^\\*\\* TODO 2:" s))
      (should (string-match-p "Last refreshed" s))
      (should (string-match-p "keep me" s))
      (should-not (string-match-p "old junk" s)))))

(ert-deftest aswg-test/missing-summary-section ()
  "No Summary heading: it is created, existing Tasks are preserved."
  (aswg-test/with-file f "#+title: x\n\n* Tasks\n** TODO 1: needs 1 review\n:PROPERTIES:\n:REPO: o/r\n:PR: 1\n:STEP: reviews\n:END:\nmine\n"
    (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
    (let ((s (aswg-test/slurp f)))
      (should (= 1 (aswg-test/count s "^\\* Summary$")))
      (should (= 1 (aswg-test/count s "^\\* Tasks$")))
      (should (string-match-p "^mine$" s))
      (should (string-match-p "Last refreshed" s))
      (should (= 1 (aswg-test/count s "^:PR: 1$"))))))

(ert-deftest aswg-test/malformed-summary-section ()
  "Summary last in the file, without trailing newline, with stray text and a stale table."
  (dolist (content '("* Tasks\n* Summary"
                     "* Tasks\n* Summary\nstale | not a table\n|-\n| junk"
                     "* Summary\n| a | b |\n|-\n| 1 | 2 |\n* Tasks\n** TODO 9: user\n:PROPERTIES:\n:REPO: o/r\n:PR: 9\n:END:\n"
                     "* Summary\n** Sub of summary\ntext\n* Tasks\n"))
    (aswg-test/with-file f content
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
      (let ((s (aswg-test/slurp f)))
        (should (= 1 (aswg-test/count s "^\\* Summary$")))
        (should (= 1 (aswg-test/count s "^\\* Tasks$")))
        (should (string-match-p "Last refreshed" s))
        (should-not (string-match-p "stale\\|| junk\\|| 1 | 2 |" s))
        (should (string-match-p "^\\*\\* TODO 1:" s))
        ;; Second sync is stable.
        (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
        (should (= 1 (aswg-test/count (aswg-test/slurp f) "^:PR: 1$")))))))

(ert-deftest aswg-test/preop-sync-then-skip-when-unchanged ()
  (aswg-test/with-file f nil
    (let ((agent-shell-workflow-pr-status-file f))
      (aswg-test/with-mocks (lambda (&rest _) (list (aswg-test/pr 1)))
        (let ((ctx (agent-shell-workflow-github--pr-status-sync-pre-op nil)))
          (should (equal (plist-get ctx :pr-status-file) f))
          (should (string-match-p "o/r#1 reviews — newly seen — needs 1 review"
                                  (plist-get ctx :pr-status-delta)))
          (should (= committed 0))
          (agent-shell-workflow-github--pr-status-commit ctx)
          (should (= committed 1))
          (should (file-exists-p f)))
        (delete-file f)
        (should-error (agent-shell-workflow-github--pr-status-sync-pre-op nil)
                      :type 'agent-shell-workflow-skip)
        (should-not (file-exists-p f))
        (should (= committed 1))))))

(ert-deftest aswg-test/preop-collection-failure-state ()
  (aswg-test/with-file f nil
    (let ((agent-shell-workflow-pr-status-file f))
      (aswg-test/with-mocks (lambda (&rest _) (error "gh down"))
        (should-error (agent-shell-workflow-github--pr-status-sync-pre-op nil)
                      :type 'agent-shell-workflow-skip)
        (should-not agent-shell-workflow-github--pr-status-fingerprint)
        (should (= committed 0))
        (should-not (file-exists-p f))))))

(ert-deftest aswg-test/exec-pre-skip-does-not-call-callback ()
  (let ((called nil)
        (spec (agent-shell-workflow-spec--make
               :id 'x :pre-op (lambda (_ctx) (agent-shell-workflow-skip "nope")))))
    (agent-shell-workflow-exec-pre spec nil (lambda (_) (setq called t)))
    (should-not called)))

;;; Review-fix regression tests

(defvar magit-dash-gh-pr-status-max-prs)

(defun aswg-test/states (f)
  "Return ((PR . STATE) ...) for entries of file F."
  (with-temp-buffer
    (insert-file-contents f) (org-mode)
    (org-map-entries (lambda () (cons (org-entry-get nil "PR") (org-get-todo-state)))
                     "PR<>\"\"")))

(ert-deftest aswg-test/truncated-list-does-not-close ()
  (aswg-test/with-file f aswg-test/fixture
    (let ((magit-dash-gh-pr-status-max-prs 1))
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
      (should (string-match-p "^\\*\\* TODO 2:" (aswg-test/slurp f))))
    (let ((magit-dash-gh-pr-status-max-prs 2))
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
      (should (string-match-p "^\\*\\* DONE 2:" (aswg-test/slurp f))))))

(ert-deftest aswg-test/preop-empty-after-nonempty-skips ()
  (aswg-test/with-file f nil
    (let ((agent-shell-workflow-pr-status-file f)
          (agent-shell-workflow-github--pr-status-last-count 0)
          (results (list (list (aswg-test/pr 1)) nil)))
      (aswg-test/with-mocks (lambda (&rest _) (pop results))
        (let ((ctx (agent-shell-workflow-github--pr-status-sync-pre-op nil)))
          (agent-shell-workflow-github--pr-status-commit ctx))
        (let ((before (aswg-test/slurp f)))
          (should-error (agent-shell-workflow-github--pr-status-sync-pre-op nil)
                        :type 'agent-shell-workflow-skip)
          (should (equal before (aswg-test/slurp f))))))))

(ert-deftest aswg-test/org-injection-sanitized ()
  (dolist (title '("* evil heading\n" "x\n* evil\n" "#+begin_src\nfoo" ":END:\n* h"
                   "a\n:PROPERTIES:\n:X: y\n:END:\n"))
    (aswg-test/with-file f nil
      (let ((pr (aswg-test/pr 1 :action title)))
        (setq pr (plist-put (plist-put (plist-put pr :title title) :branch title)
                            :url title))
        (agent-shell-workflow-github-pr-sync (list pr) f)
        (let ((s (aswg-test/slurp f)))
          ;; Summary, Tasks, the one task, its single review action.
          (should (= 4 (aswg-test/count s "^\\*+ ")))
          (should (= 2 (aswg-test/count s "^:PROPERTIES:$")))
          (should (= 2 (aswg-test/count s "^:END:$")))
          (should (= 0 (aswg-test/count s "^#\\+begin")))
          (should (= 1 (length (aswg-test/states f)))))))))

(ert-deftest aswg-test/symlinked-status-file-preserved ()
  (aswg-test/with-file f aswg-test/fixture
    (let* ((link (expand-file-name "link.org" (file-name-directory f))))
      (make-symbolic-link f link)
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1) (aswg-test/pr 3)) link)
      (should (file-symlink-p link))
      (should (string-match-p "^\\*\\* TODO 3:" (aswg-test/slurp f))))))

(ert-deftest aswg-test/done-task-reopens-when-pr-present ()
  (aswg-test/with-file f (concat "* Summary\n* Tasks\n"
                                 "** DONE 1: needs 1 review\nCLOSED: [2026-01-01 Thu 00:00]\n:PROPERTIES:\n:REPO: o/r\n:PR: 1\n:STEP: reviews\n:END:\n")
    (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
    (should (equal '(("1" . "TODO")) (aswg-test/states f)))))

(ert-deftest aswg-test/no-log-prompts-with-note-logging ()
  (let ((org-log-done 'note)
        (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d@)"))))
    (aswg-test/with-file f (concat "* Summary\n* Tasks\n"
                                   "** TODO 2: gone\n:PROPERTIES:\n:REPO: o/r\n:PR: 2\n:STEP: reviews\n:END:\n"
                                   "** DONE 1: x\n:PROPERTIES:\n:REPO: o/r\n:PR: 1\n:STEP: reviews\n:END:\n")
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
      (should-not (get-buffer "*Org Note*"))
      (should (equal '(("2" . "DONE") ("1" . "TODO")) (aswg-test/states f)))
      (should (string-match-p "CLOSED: \\[" (aswg-test/slurp f)))
      (should-not (string-match-p ":LOGBOOK:" (aswg-test/slurp f))))))

(ert-deftest aswg-test/no-open-keyword ()
  (let ((org-todo-keywords '((sequence "|" "DONE"))))
    (aswg-test/with-file f "#+TODO: | DONE\n* Summary\n* Tasks\n"
      (agent-shell-workflow-github-pr-sync
       (list (aswg-test/pr 1) (aswg-test/pr 2)) f)
      (should (equal '(("1") ("2")) (mapcar (lambda (x) (list (car x))) (aswg-test/states f))))
      (should (equal '(nil nil) (mapcar #'cdr (aswg-test/states f))))
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1) (aswg-test/pr 2)) f)
      (should (= 2 (aswg-test/count (aswg-test/slurp f) "^:PR: ")))
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
      (should (equal '(("1") ("2" . "DONE")) (aswg-test/states f)))
      (should (string-match-p "CLOSED: \\[" (aswg-test/slurp f)))
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1) (aswg-test/pr 2)) f)
      (should (equal '(("1") ("2")) (aswg-test/states f))))))

(ert-deftest aswg-test/find-heading-tags-and-example-blocks ()
  (aswg-test/with-file f (concat "#+begin_example\n* Tasks\n#+end_example\n"
                                 "* Summary :x:\nold\n* Tasks :y:\n")
    (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 1)) f)
    (let ((s (aswg-test/slurp f)))
      (should (= 1 (aswg-test/count s "^\\* Summary :x:$")))
      (should (= 1 (aswg-test/count s "^\\* Tasks :y:$")))
      (should (= 0 (aswg-test/count s "^\\* Summary$")))
      (should (string-match-p "^#\\+begin_example\n\\* Tasks\n#\\+end_example" s))
      (should (equal '("1") (mapcar #'car (aswg-test/states f)))))))

(ert-deftest aswg-test/preop-commit-deferred-to-handoff ()
  (aswg-test/with-file f nil
    (let ((agent-shell-workflow-pr-status-file f)
          (agent-shell-workflow-github--pr-status-last-count 0))
      (aswg-test/with-mocks (lambda (&rest _) (list (aswg-test/pr 1)))
        (let ((ctx (agent-shell-workflow-github--pr-status-sync-pre-op nil)))
          (should (= committed 0))
          (should-not agent-shell-workflow-github--pr-status-fingerprint)
          (should (plist-get ctx :pr-status-pending-fingerprint))
          (should (functionp (plist-get ctx :after-handoff)))
          ;; Failed dispatch: nothing committed.
          (setq ctx (plist-put ctx :shell-buffer (current-buffer)))
          (cl-letf (((symbol-function 'agent-shell-workflow--resolve-target) #'identity)
                    ((symbol-function 'agent-shell-workflow--dispatch-to-buffer)
                     (lambda (&rest _) (error "no shell"))))
            (should-error (agent-shell-workflow--dispatch-rendered
                           (agent-shell-workflow-spec--make :id 'x) ctx 'buf nil)))
          (should (= committed 0))
          ;; Successful handoff commits.
          (cl-letf (((symbol-function 'agent-shell-workflow--resolve-target) #'identity)
                    ((symbol-function 'agent-shell-workflow--dispatch-to-buffer)
                     (lambda (&rest _) t)))
            (agent-shell-workflow--dispatch-rendered
             (agent-shell-workflow-spec--make :id 'x) ctx 'buf nil))
          (should (= committed 1))
          (should agent-shell-workflow-github--pr-status-fingerprint))))))

(ert-deftest aswg-test/repeated-collection-failures-warn ()
  (aswg-test/with-file f nil
    (let ((agent-shell-workflow-pr-status-file f)
          (agent-shell-workflow-github--pr-status-failures 0)
          msgs warns)
      (aswg-test/with-mocks (lambda (&rest _) (error "gh down"))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest a) (push (apply #'format fmt a) msgs)))
                  ((symbol-function 'display-warning)
                   (lambda (&rest a) (push a warns))))
          (dotimes (_ 3)
            (should-error (agent-shell-workflow-github--pr-status-sync-pre-op nil)
                          :type 'agent-shell-workflow-skip))
          (should (= 2 (length msgs)))
          (should (= 1 (length warns)))
          (should (= 3 agent-shell-workflow-github--pr-status-failures)))))))

;; Periodic timer

(defmacro aswg-test/with-timer-clean (&rest body)
  `(let ((agent-shell-workflow-pr-status--timer nil))
     (unwind-protect (progn ,@body)
       (agent-shell-workflow-pr-status-timer-stop))))

(ert-deftest aswg-test/timer-start-one-timer-with-interval ()
  (aswg-test/with-timer-clean
   (let ((agent-shell-workflow-pr-status-timer-interval 77)
         (before (length timer-list)))
     (agent-shell-workflow-pr-status-timer-start)
     (should (= (length timer-list) (1+ before)))
     (should (timerp agent-shell-workflow-pr-status--timer))
     (should (= (timer--repeat-delay agent-shell-workflow-pr-status--timer) 77)))))

(ert-deftest aswg-test/timer-double-start-no-leak ()
  (aswg-test/with-timer-clean
   (let ((before (length timer-list)))
     (agent-shell-workflow-pr-status-timer-start)
     (agent-shell-workflow-pr-status-timer-start)
     (should (= (length timer-list) (1+ before))))))

(ert-deftest aswg-test/timer-stop-idempotent ()
  (aswg-test/with-timer-clean
   (let ((before (length timer-list)))
     (agent-shell-workflow-pr-status-timer-start)
     (agent-shell-workflow-pr-status-timer-stop)
     (should (= (length timer-list) before))
     (should-not agent-shell-workflow-pr-status--timer)
     (agent-shell-workflow-pr-status-timer-stop)
     (should-not agent-shell-workflow-pr-status--timer))))

(ert-deftest aswg-test/timer-fn-swallows-errors-and-skip ()
  (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
             (lambda (&rest _) (error "boom"))))
    (should-not (agent-shell-workflow-pr-status--dispatch)))
  (let (msgs)
    (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
               (lambda (&rest _) (agent-shell-workflow-skip "idle")))
              ((symbol-function 'message)
               (lambda (f &rest a) (push (apply #'format f a) msgs))))
      (should-not (agent-shell-workflow-pr-status--dispatch)))
    (should (cl-some (lambda (m) (string-match-p "skipped" m)) msgs))
    (should-not (cl-some (lambda (m) (string-match-p "timer:" m)) msgs))))

(ert-deftest aswg-test/run-now-dispatches-once ()
  (let (calls)
    (cl-letf (((symbol-function 'agent-shell-workflow-dispatch)
               (lambda (&rest a) (push a calls))))
      (let ((agent-shell-workflow-pr-status-timer-target 'queue))
        (agent-shell-workflow-pr-status-run-now))
      (should (equal calls '((pr-status-sync :target :queue))))
      (setq calls nil)
      (let ((agent-shell-workflow-pr-status-timer-target 'session))
        (agent-shell-workflow-pr-status-run-now))
      (should (equal calls '((pr-status-sync)))))))

(provide 'test-agent-shell-workflow-github)

;;; Action items and CI preflight

(defun aswg-test/pr* (num &rest kw)
  "Like `aswg-test/pr' but KW plist keys (other than :repo :bucket :action) are set on the PR."
  (let ((pr (aswg-test/pr num :repo (plist-get kw :repo) :bucket (plist-get kw :bucket)
                          :action (plist-get kw :action)))
        (rest kw))
    (while rest
      (unless (memq (car rest) '(:repo :bucket :action))
        (setq pr (plist-put pr (car rest) (cadr rest))))
      (setq rest (cddr rest)))
    pr))

(ert-deftest aswg-test/no-notes-heading-created ()
  (aswg-test/with-file f nil
    (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 7)) f)
    (should-not (string-match-p "Notes and In-Flight" (aswg-test/slurp f)))))

(ert-deftest aswg-test/one-action-per-activity ()
  (aswg-test/with-file f nil
    (agent-shell-workflow-github-pr-sync
     (list (aswg-test/pr* 7 :bucket 'fix-ci :approvals 0 :threads-open 2
                          :changes-requested 1 :merge-state "DIRTY" :base "main"
                          :failing '(("Tests" . "https://ci/1") ("koalaty" . "https://ci/2"))
                          :draft t))
     f)
    (let ((s (aswg-test/slurp f)))
      (dolist (h '("fix failing tests: Tests" "raise koalaty rating"
                   "fix merge conflicts with main" "respond to 2 open review threads"
                   "address changes requested by 1 reviewer" "find 1 more review"
                   "mark ready for review"))
        (should (string-match-p (format "^\\*\\*\\* TODO %s$" (regexp-quote h)) s)))
      (should (string-match-p "- Failing: \\[\\[https://ci/1\\]\\[Tests\\]\\]" s))
      (should (string-match-p "- Prompt: fix-ci workflow, repo o/r" s))
      ;; Action items carry no PR property, so they are never mistaken for PR tasks.
      (should (= 1 (aswg-test/count s "^:PR: ")))
      (should (= 7 (aswg-test/count s "^:ACTION: "))))))

(ert-deftest aswg-test/resolved-actions-close-and-reopen ()
  (aswg-test/with-file f nil
    (let ((bad (aswg-test/pr* 7 :threads-open 1 :approvals 1 :required 1))
          (good (aswg-test/pr* 7 :approvals 1 :required 1)))
      (agent-shell-workflow-github-pr-sync (list bad) f)
      (should (string-match-p "^\\*\\*\\* TODO respond to 1 open review thread$" (aswg-test/slurp f)))
      (agent-shell-workflow-github-pr-sync (list good) f)
      (should (string-match-p "^\\*\\*\\* DONE respond to 1 open review thread$" (aswg-test/slurp f)))
      (agent-shell-workflow-github-pr-sync (list bad) f)
      (let ((s (aswg-test/slurp f)))
        (should (string-match-p "^\\*\\*\\* TODO respond to 1 open review thread$" s))
        (should (= 1 (aswg-test/count s "^\\*\\*\\* .*respond to"))))
      (agent-shell-workflow-github-pr-sync (list (aswg-test/pr 99)) f)
      (should (string-match-p "^\\*\\*\\* DONE respond to" (aswg-test/slurp f))))))

(ert-deftest aswg-test/actions-do-not-break-sibling-tasks ()
  (aswg-test/with-file f nil
    (let ((prs (list (aswg-test/pr* 1 :threads-open 1) (aswg-test/pr* 2 :threads-open 3))))
      (agent-shell-workflow-github-pr-sync prs f)
      (agent-shell-workflow-github-pr-sync prs f)
      (let ((s (aswg-test/slurp f)))
        (should (string-match-p "^\\*\\* TODO 2: " s))
        (should (= 2 (aswg-test/count s "^\\*\\*\\* TODO respond to")))
        (should (= 2 (aswg-test/count s "^:REPO: ")))))))

(ert-deftest aswg-test/ci-fingerprint-change-marks-stale ()
  (aswg-test/with-file f nil
    (let ((agent-shell-workflow-pr-status-ci-preflight nil))
      (agent-shell-workflow-github-pr-sync
       (list (aswg-test/pr* 7 :failing '(("Tests" . "https://ci/1")))) f)
      ;; Pretend logs were fetched for the current failure.
      (let ((fp (agent-shell-workflow-github--ci-fingerprint '(("Tests" . "https://ci/1")))))
        (agent-shell-workflow-github--edit-file
         f (lambda ()
             (goto-char (point-min)) (re-search-forward "^:ACTION: fix-ci")
             (org-entry-put nil "LOGS_FETCHED" "[2026-11-12 Thu 10:00]")
             (org-entry-put nil "LOGS_FINGERPRINT" fp))))
      (agent-shell-workflow-github-pr-sync
       (list (aswg-test/pr* 7 :failing '(("Tests" . "https://ci/1")))) f)
      (should-not (string-match-p ":STALE:" (aswg-test/slurp f)))
      (agent-shell-workflow-github-pr-sync
       (list (aswg-test/pr* 7 :failing '(("Tests" . "https://ci/2")))) f)
      (should (string-match-p ":STALE: +t" (aswg-test/slurp f)))
      (should (string-match-p "\\[\\[https://ci/2\\]\\[Tests\\]\\]" (aswg-test/slurp f))))))

(defconst aswg-test/run-url "https://github.com/o/r/actions/runs/555/job/9")

(ert-deftest aswg-test/preflight-uses-fix-ci-workflow-and-clears-stale ()
  (aswg-test/with-file f nil
    (let* ((calls nil)
           (agent-shell-workflow-pr-status-ci-preflight '(("o/r" :dir "/tmp/aswg-checkout")))
           (agent-shell-workflow-github--preflight-procs (make-hash-table :test #'equal))
           (pr (aswg-test/pr* 7 :branch "feat" :failing `(("Tests" . ,aswg-test/run-url)))))
      (cl-letf (((symbol-function 'agent-shell-workflow-library--fix-ci-pre-op)
                 (lambda (ctx &optional callback)
                   (push ctx calls)
                   (funcall callback
                            (list :ci-dir ".agent/fix-ci/run-555"
                                  :ci-index-file ".agent/fix-ci/run-555/ci-triage-index.md")))))
        (agent-shell-workflow-github-pr-sync (list pr) f))
      (let ((args (plist-get (car calls) :args)))
        (should (equal (plist-get args :repo) "o/r"))
        (should (equal (plist-get args :run-id) 555))
        (should (equal (plist-get args :root) "/tmp/aswg-checkout")))
      (should (= 0 (hash-table-count agent-shell-workflow-github--preflight-procs)))
      (let ((s (aswg-test/slurp f)))
        (should (string-match-p ":RUN_ID: +555" s))
        (should (string-match-p ":LOGS_FETCHED: +\\[" s))
        (should (string-match-p "^- Logs: \\[\\[file:/tmp/aswg-checkout/.agent/fix-ci/run-555" s))
        (should (string-match-p "^- Prompt: fix-ci workflow, repo o/r, run 555" s))
        (should-not (string-match-p ":STALE:" s)))
      ;; Unchanged failure: no second fetch.
      (cl-letf (((symbol-function 'agent-shell-workflow-library--fix-ci-pre-op)
                 (lambda (&rest _) (ert-fail "refetched"))))
        (agent-shell-workflow-github-pr-sync (list pr) f)))))

(ert-deftest aswg-test/preflight-refetches-when-stale ()
  (aswg-test/with-file f nil
    (let* ((n 0)
           (agent-shell-workflow-pr-status-ci-preflight '(("o/r" :dir "/tmp/aswg-checkout")))
           (agent-shell-workflow-github--preflight-procs (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'agent-shell-workflow-library--fix-ci-pre-op)
                 (lambda (_ctx &optional callback)
                   (cl-incf n)
                   (funcall callback (list :ci-dir "d" :ci-index-file "i")))))
        (agent-shell-workflow-github-pr-sync
         (list (aswg-test/pr* 7 :failing `(("Tests" . ,aswg-test/run-url)))) f)
        (agent-shell-workflow-github-pr-sync
         (list (aswg-test/pr* 7 :failing '(("Tests" . "https://github.com/o/r/actions/runs/777/job/1"))))
         f))
      (should (= n 2))
      (let ((s (aswg-test/slurp f)))
        (should (string-match-p ":RUN_ID: +777" s))
        (should-not (string-match-p ":STALE:" s))))))

(ert-deftest aswg-test/preflight-failure-recorded ()
  (aswg-test/with-file f nil
    (let ((agent-shell-workflow-pr-status-ci-preflight '(("o/r" :dir "/tmp/aswg-checkout")))
          (agent-shell-workflow-github--preflight-procs (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'agent-shell-workflow-library--fix-ci-pre-op)
                 (lambda (&rest _) (error "gh exploded"))))
        (agent-shell-workflow-github-pr-sync
         (list (aswg-test/pr* 7 :failing `(("Tests" . ,aswg-test/run-url)))) f))
      (should (= 0 (hash-table-count agent-shell-workflow-github--preflight-procs)))
      (let ((s (aswg-test/slurp f)))
        (should (string-match-p ":PREFLIGHT_ERROR: gh exploded" s))
        (should-not (string-match-p ":LOGS_FETCHED:" s))))))

(ert-deftest aswg-test/preflight-skipped-without-run-id ()
  (aswg-test/with-file f nil
    (let ((agent-shell-workflow-pr-status-ci-preflight '(("o/r" :dir "/tmp/aswg-checkout"))))
      (cl-letf (((symbol-function 'agent-shell-workflow-library--fix-ci-pre-op)
                 (lambda (&rest _) (ert-fail "should not run"))))
        (agent-shell-workflow-github-pr-sync
         (list (aswg-test/pr* 7 :failing '(("Tests" . "https://ci/1")))) f)))))
