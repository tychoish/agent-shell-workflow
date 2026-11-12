;;; agent-shell-workflow-github.el --- GitHub and CI workflows for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-workflow
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; GitHub Actions CI remediation and PR review comment triage workflows.
;; Fetches run logs and PR review discussions via `gh` CLI, parses structured
;; run indices, and formats artifacts for agent turns.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'json)
(require 'agent-shell-workflow)
(require 'agent-shell-workflow-utils)
(eval-when-compile (require 'agent-shell-workflow))

(declare-function magit-get-current-branch "magit-git" ())
(declare-function magit-dash-gh--repo-info "magit-dash" ())
(declare-function magit-dash--repo-at-point "magit-dash" ())
(declare-function magit-dash-repo-name "magit-dash" (repo))
(declare-function annotated-completing-read "annotated-completing-read")

;; GitHub time parsing and repository resolution
(defun agent-shell-workflow-library--iso-to-seconds (iso-str)
  "Convert ISO-STR timestamp string to float seconds."
  (when (and (stringp iso-str) (not (string-empty-p iso-str)))
    (ignore-errors
      (float-time (encode-time (parse-time-string iso-str))))))

(defun agent-shell-workflow-library--format-duration (start-iso end-iso)
  "Format duration between START-ISO and END-ISO string."
  (let ((s (agent-shell-workflow-library--iso-to-seconds start-iso))
        (e (agent-shell-workflow-library--iso-to-seconds end-iso))
        (diff nil))
    (if (and s e)
        (progn
          (setq diff (max 0 (floor (- e s))))
          (cond ((< diff 60) (format "%ds" diff))
                ((< diff 3600) (format "%dm %ds" (/ diff 60) (% diff 60)))
                (t (format "%dh %dm" (/ diff 3600) (/ (% diff 3600) 60)))))
      "n/a")))

(defun agent-shell-workflow-library--format-time-ago (iso-time)
  "Format ISO-TIME string as relative time ago."
  (let ((t-sec (agent-shell-workflow-library--iso-to-seconds iso-time)))
    (if t-sec
        (let ((diff (max 0 (floor (- (float-time) t-sec)))))
          (cond ((< diff 60) "just now")
                ((< diff 3600) (format "%dm ago" (/ diff 60)))
                ((< diff 86400) (format "%dh ago" (/ diff 3600)))
                (t (format "%dd ago" (/ diff 86400)))))
      "n/a")))

(defun agent-shell-workflow-library--resolve-repo-slug (repo)
  "Resolve REPO name or path to an OWNER/NAME GitHub repository slug string."
  (if (and (stringp repo) (string-match-p "/" repo))
      repo
    (or (ignore-errors
          (let ((slug (string-trim (shell-command-to-string "gh repo view --json nameWithOwner --jq .nameWithOwner"))))
            (unless (or (string-empty-p slug) (string-match-p "^error" slug))
              slug)))
        (ignore-errors
          (and (fboundp 'magit-dash-gh--repo-info)
               (when-let* ((info (magit-dash-gh--repo-info))
                           (o (plist-get info :owner))
                           (r (plist-get info :repo)))
                 (format "%s/%s" o r))))
        repo)))

(defun agent-shell-workflow-library--fetch-runs (repo &optional limit)
  "Fetch recent GitHub Actions run records for REPO as a list of alists.
Optional LIMIT sets maximum runs to fetch (defaults to 20)."
  (when-let* ((slug (agent-shell-workflow-library--resolve-repo-slug repo))
              ((executable-find "gh" t)))
    (let* ((lim (number-to-string (or limit 20)))
           (json-str (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "gh" nil t nil "run" "list"
                                       "--repo" slug
                                       "--limit" lim
                                       "--json" "databaseId,displayTitle,status,conclusion,headBranch,headSha,createdAt,updatedAt,startedAt,url"))))
           (parsed (ignore-errors (json-parse-string json-str :object-type 'alist :array-type 'list))))
      (when (listp parsed) parsed))))

(defun agent-shell-workflow-library--current-branch ()
  "Return the current Git branch name, or `main'."
  (or (ignore-errors (magit-get-current-branch)) "main"))

(defun agent-shell-workflow-library--resolve-ci-run (repo &optional target-branch)
  "Return a run-id for REPO and TARGET-BRANCH.
If the latest run on TARGET-BRANCH is failing, return its run-id
automatically.  Otherwise, prompt the user with an ACR picker showing
recent runs."
  (let* ((branch (or target-branch (agent-shell-workflow-library--current-branch)))
         (runs (agent-shell-workflow-library--fetch-runs repo 20))
         (branch-runs (seq-filter (lambda (r) (equal (map-elt r 'headBranch) branch)) runs))
         (target-runs (or branch-runs runs))
         (latest (car target-runs))
         (latest-conclusion (and latest (or (map-elt latest 'conclusion) (map-elt latest 'status))))
         (latest-failing-p (and latest
                                (member latest-conclusion '("failure" "cancelled" "timed_out" "action_required")))))
    (if latest-failing-p
        (map-elt latest 'databaseId)
      (if (null target-runs)
          (user-error "No CI runs found for %s" repo)
        (let* ((items (mapcar
                       (lambda (r)
                         (let* ((id (map-elt r 'databaseId))
                                (title (map-elt r 'displayTitle))
                                (sha (map-elt r 'headSha))
                                (short-sha (if (and (stringp sha) (>= (length sha) 7))
                                               (substring sha 0 7)
                                             (or sha "")))
                                (b (map-elt r 'headBranch))
                                (status (map-elt r 'status))
                                (conclusion (or (map-elt r 'conclusion) status))
                                (dur (agent-shell-workflow-library--format-duration
                                      (map-elt r 'startedAt) (map-elt r 'updatedAt)))
                                (ago (agent-shell-workflow-library--format-time-ago
                                      (or (map-elt r 'updatedAt) (map-elt r 'createdAt))))
                                (cand (format "#%s %s (%s) [%s]" id title short-sha b))
                                (ann (format "%s | %s | %s" conclusion dur ago)))
                           (list cand id ann)))
                       target-runs))
               (table (mapcar (lambda (item) (cons (nth 0 item) (nth 2 item))) items))
               (selected (if (fboundp 'annotated-completing-read)
                             (annotated-completing-read table
                                                        :prompt "Select CI Run: "
                                                        :require-match t
                                                        :history 'agent-shell-workflow-ci-run-history)
                           (completing-read "Select CI Run: " table nil t)))
               (match (assoc selected items)))
          (if match
              (nth 1 match)
            (user-error "No CI run selected")))))))

;; Artifact directories and review comment formatting
(defvar agent-shell-workflow-library-max-branch-length 30
  "Maximum character length for branch names in artifact directories.")

(defun agent-shell-workflow-library--sanitize-name (str &optional max-length)
  "Sanitize and optionally truncate STR for use in directory/file names.
Strips leading `refs/heads/' or `origin/' git prefixes if present.
Replaces non-alphanumeric characters (except `.' and `_') with `-',
collapses consecutive hyphens, and trims leading/trailing delimiter
characters (`-', `.', `_').  When MAX-LENGTH is a positive integer,
truncates STR to at most MAX-LENGTH characters, trimming any resulting
trailing delimiter characters."
  (if (or (null str) (string-empty-p (format "%s" str)))
      ""
    (let* ((s (format "%s" str))
           (s-no-ref (replace-regexp-in-string "\\`\\(?:refs/heads/\\|origin/\\)" "" s))
           (cleaned (replace-regexp-in-string "[^a-zA-Z0-9._]+" "-" s-no-ref))
           (collapsed (replace-regexp-in-string "-+" "-" cleaned))
           (trimmed (string-trim collapsed "[-._]+" "[-._]+")))
      (if (and (integerp max-length) (> max-length 0) (> (length trimmed) max-length))
          (string-trim-right (substring trimmed 0 max-length) "[-._]+")
        trimmed))))

(defun agent-shell-workflow-library--unique-artifact-dir (base-dir &optional ident branch date max-branch-len)
  "Return a unique artifact directory path under BASE-DIR.
Constructs a directory name using DATE (defaults to today as YYYYMMDD),
IDENT (e.g. \"run-123\" or \"pr-55\"), BRANCH, and an incrementing sequence
number starting at 1 (e.g. `<date>-<ident>-<branch>-1').  If candidate
exists, the sequence number increments until an unused directory name is
found.  BRANCH is sanitized and truncated to MAX-BRANCH-LEN (defaults to
`agent-shell-workflow-library-max-branch-length' or 30).
Creates and returns the unused directory path."
  (let* ((d-str (or date (format-time-string "%Y%m%d")))
         (branch-limit (or max-branch-len agent-shell-workflow-library-max-branch-length 30))
         (clean-ident (when ident (agent-shell-workflow-library--sanitize-name ident)))
         (clean-branch (when branch (agent-shell-workflow-library--sanitize-name branch branch-limit)))
         (parts (delq nil (list (and (not (string-empty-p d-str)) d-str)
                                (and clean-ident (not (string-empty-p clean-ident)) clean-ident)
                                (and clean-branch (not (string-empty-p clean-branch)) clean-branch))))
         (base-name (if parts (mapconcat #'identity parts "-") "artifacts"))
         (seq 1)
         candidate)
    (while (file-exists-p (setq candidate (expand-file-name (format "%s-%d" base-name seq) base-dir)))
      (setq seq (1+ seq)))
    (make-directory candidate t)
    candidate))

(defun agent-shell-workflow-library--format-pr-comments-markdown (repo pr-num view-obj inline-comments raw-comments)
  "Format PR review comments into Markdown.
REPO is the repository slug string.  PR-NUM is the PR number string.
VIEW-OBJ is the parsed hash-table from `gh pr view --json ...`.
INLINE-COMMENTS is the parsed vector of hash-tables from `gh api ...`.
RAW-COMMENTS is fallback plain text from `gh pr view --comments`."
  (with-temp-buffer
    (let* ((title (if (hash-table-p view-obj) (or (gethash "title" view-obj) "") ""))
           (author-val (if (hash-table-p view-obj) (gethash "author" view-obj) nil))
           (author (cond ((hash-table-p author-val) (or (gethash "login" author-val) ""))
                         ((stringp author-val) author-val)
                         (t "")))
           (url (if (hash-table-p view-obj) (or (gethash "url" view-obj) "") ""))
           (reviews (if (hash-table-p view-obj) (gethash "reviews" view-obj) nil))
           (comments (if (hash-table-p view-obj) (gethash "comments" view-obj) nil))
           (inline inline-comments))
      (insert (format "# PR #%s Comments" pr-num))
      (unless (string-empty-p title)
        (insert (format ": %s" title)))
      (insert "\n\n")
      (when repo
        (insert (format "- **Repository**: %s\n" repo)))
      (unless (string-empty-p author)
        (insert (format "- **Author**: @%s\n" author)))
      (unless (string-empty-p url)
        (insert (format "- **URL**: %s\n" url)))
      (insert (format "- **Generated**: %s\n" (format-time-string "%Y-%m-%dT%T%z")))
      (insert (format "- **Reviews**: %d\n" (if (vectorp reviews) (length reviews) 0)))
      (insert (format "- **Top-level Comments**: %d\n" (if (vectorp comments) (length comments) 0)))
      (insert (format "- **Inline Comments**: %d\n\n" (if (vectorp inline) (length inline) 0)))

      ;; Reviews
      (when (and (vectorp reviews) (> (length reviews) 0))
        (insert (format "## Reviews (%d)\n\n" (length reviews)))
        (seq-doseq (r reviews)
          (let* ((u-obj (gethash "author" r))
                 (user (if (hash-table-p u-obj) (gethash "login" u-obj) (format "%s" (or u-obj ""))))
                 (state (or (gethash "state" r) ""))
                 (submitted (or (gethash "submittedAt" r) ""))
                 (r-url (or (gethash "url" r) ""))
                 (body (or (gethash "body" r) "")))
            (unless (equal user "github-actions")
              (insert (format "### Review by @%s (%s)\n" (or user "unknown") state))
              (unless (string-empty-p submitted) (insert (format "- **Submitted**: %s\n" submitted)))
              (unless (string-empty-p r-url) (insert (format "- **URL**: %s\n" r-url)))
              (unless (string-empty-p body)
                (insert "\n**Body**:\n")
                (insert body)
                (insert "\n"))
              (insert "\n---\n\n")))))

      ;; Comments
      (when (and (vectorp comments) (> (length comments) 0))
        (insert (format "## Top-level Comments (%d)\n\n" (length comments)))
        (seq-doseq (c comments)
          (let* ((u-obj (gethash "author" c))
                 (user (if (hash-table-p u-obj) (gethash "login" u-obj) (format "%s" (or u-obj ""))))
                 (created (or (gethash "createdAt" c) ""))
                 (c-url (or (gethash "url" c) ""))
                 (body (or (gethash "body" c) "")))
            (unless (equal user "github-actions")
              (insert (format "### Comment by @%s\n" (or user "unknown")))
              (unless (string-empty-p created) (insert (format "- **At**: %s\n" created)))
              (unless (string-empty-p c-url) (insert (format "- **URL**: %s\n" c-url)))
              (unless (string-empty-p body)
                (insert "\n**Body**:\n")
                (insert body)
                (insert "\n"))
              (insert "\n---\n\n")))))

      ;; Inline comments
      (when (and (vectorp inline) (> (length inline) 0))
        (insert (format "## Inline Review Comments (%d)\n\n" (length inline)))
        (seq-doseq (ic inline)
          (let* ((u-obj (gethash "user" ic))
                 (user (if (hash-table-p u-obj) (gethash "login" u-obj) (format "%s" (or u-obj ""))))
                 (path (or (gethash "path" ic) ""))
                 (line (or (gethash "line" ic) (gethash "original_line" ic) 0))
                 (created (or (gethash "created_at" ic) ""))
                 (i-url (or (gethash "html_url" ic) (gethash "url" ic) ""))
                 (diff (or (gethash "diff_hunk" ic) ""))
                 (body (or (gethash "body" ic) "")))
            (unless (equal user "github-actions")
              (insert (format "### Inline Comment by @%s on `%s` (line %s)\n" (or user "unknown") path line))
              (insert (format "- **File**: `%s:%s`\n" path line))
              (unless (string-empty-p created) (insert (format "- **At**: %s\n" created)))
              (unless (string-empty-p i-url) (insert (format "- **URL**: %s\n" i-url)))
              (unless (string-empty-p diff)
                (insert "\n```diff\n")
                (insert diff)
                (insert "\n```\n"))
              (unless (string-empty-p body)
                (insert "\n**Comment**:\n")
                (insert body)
                (insert "\n"))
              (insert "\n---\n\n")))))

      ;; Fallback when structured data is absent
      (when (and (or (null reviews) (= (length reviews) 0))
                 (or (null comments) (= (length comments) 0))
                 (or (null inline) (= (length inline) 0))
                 (and raw-comments (not (string-empty-p raw-comments))))
        (insert "## Comments\n\n")
        (insert raw-comments)
        (insert "\n"))

      (buffer-string))))

;; CI build failure remediation

(defun agent-shell-workflow-library--fix-ci-pre-op (ctx &optional callback)
  "Fetch failing CI artifacts for :repo/:run-id in CTX and save them locally.
Artifacts (failed-step log, jobs metadata JSON, triage index, and fix
plan) are written under a unique directory in <project-root>/.agent/fix-ci/
so the agent can inspect them as files without collisions across runs
or attempts.  When CALLBACK is given, executes asynchronously."
  (let* ((args (plist-get ctx :args))
         (raw-repo (or (plist-get args :repo)
                       (ignore-errors
                         (and (fboundp 'magit-dash--repo-at-point)
                              (when-let* ((r (magit-dash--repo-at-point)))
                                (magit-dash-repo-name r))))
                       (ignore-errors
                         (let ((slug (string-trim (shell-command-to-string "gh repo view --json nameWithOwner --jq .nameWithOwner"))))
                           (unless (or (string-empty-p slug) (string-match-p "^error" slug))
                             slug)))
                       (user-error "No repository specified for fix-ci")))
         (repo-slug (agent-shell-workflow-library--resolve-repo-slug raw-repo))
         (target-branch (or (plist-get args :branch)
                            (agent-shell-workflow-library--current-branch)))
         (run-id (or (plist-get args :run-id)
                     (agent-shell-workflow-library--resolve-ci-run repo-slug target-branch)))
         (run-id-str (when run-id (format "%s" run-id)))
         (updated-args (plist-put (plist-put (copy-sequence args) :repo repo-slug) :run-id run-id))
         (updated-ctx (plist-put (copy-sequence ctx) :args updated-args)))
    (if (and repo-slug run-id-str)
        (let ((finish
               (lambda (ci-summary ci-log ci-jobs)
                 (let* ((root (agent-shell-workflow-library--project-root))
                        (parsed-jobs (ignore-errors (json-parse-string ci-jobs :object-type 'alist :array-type 'list)))
                        (run-branch (or (plist-get args :branch)
                                        (and (listp parsed-jobs) (alist-get 'headBranch parsed-jobs))
                                        target-branch))
                        (base-ci-dir (expand-file-name ".agent/fix-ci" root))
                        (ci-dir (agent-shell-workflow-library--unique-artifact-dir
                                 base-ci-dir
                                 (format "run-%s" run-id-str)
                                 run-branch
                                 (plist-get args :date)))
                        (log-file (expand-file-name (format "run-%s-logs.txt" run-id-str) ci-dir))
                        (jobs-file (expand-file-name (format "run-%s-jobs.json" run-id-str) ci-dir))
                        (index-file (expand-file-name "ci-triage-index.md" ci-dir))
                        (alias-log-file (expand-file-name "ci-logs.txt" ci-dir))
                        (alias-jobs-file (expand-file-name "ci-jobs.json" ci-dir))
                        (plan-file (expand-file-name "fix-plan.md" ci-dir))
                        (rel-ci-dir (file-relative-name ci-dir root))
                        (rel-log-file (file-relative-name log-file root))
                        (rel-jobs-file (file-relative-name jobs-file root))
                        (rel-index-file (file-relative-name index-file root))
                        (rel-plan-file (file-relative-name plan-file root))
                        (log-content (if (and (stringp ci-log) (not (string-empty-p ci-log)))
                                         ci-log
                                       (format "No failed-step logs returned for run #%s.\n\nSummary:\n%s" run-id-str ci-summary)))
                        (jobs-content (if (and (stringp ci-jobs) (not (string-empty-p ci-jobs)))
                                          ci-jobs
                                        "{}"))
                        (index-content
                         (format "# CI Triage Index\n\n- **Repository**: %s\n- **Run ID**: %s\n- **Branch**: %s\n- **Generated**: %s\n- **Log File**: `%s`\n- **Jobs Metadata**: `%s`\n- **Fix Plan**: `%s`\n\n## Summary\n\n```\n%s\n```\n"
                                 repo-slug run-id-str (or run-branch "unknown") (format-time-string "%Y-%m-%dT%T%z") rel-log-file rel-jobs-file rel-plan-file ci-summary)))
                   ;; Write artifacts to local filesystem
                   (agent-shell-workflow-library--write-file log-file log-content)
                   (agent-shell-workflow-library--write-file jobs-file jobs-content)
                   (agent-shell-workflow-library--write-file index-file index-content)
                   (agent-shell-workflow-library--write-file alias-log-file log-content)
                   (agent-shell-workflow-library--write-file alias-jobs-file jobs-content)
                   ;; Populate context
                   (setq updated-ctx (plist-put updated-ctx :ci-summary ci-summary))
                   (setq updated-ctx (plist-put updated-ctx :ci-dir rel-ci-dir))
                   (setq updated-ctx (plist-put updated-ctx :ci-log-file rel-log-file))
                   (setq updated-ctx (plist-put updated-ctx :ci-jobs-file rel-jobs-file))
                   (setq updated-ctx (plist-put updated-ctx :ci-index-file rel-index-file))
                   (setq updated-ctx (plist-put updated-ctx :ci-plan-file rel-plan-file))
                   (setq updated-ctx (plist-put updated-ctx :ci-log ci-log))
                   updated-ctx))))
          (if callback
              (agent-shell-workflow--gather-async
               (list (list :ci-summary "gh" "run" "view" run-id-str "--repo" repo-slug)
                     (list :ci-log "gh" "run" "view" run-id-str "--repo" repo-slug "--log-failed")
                     (list :ci-jobs "gh" "run" "view" run-id-str "--repo" repo-slug "--json" "jobs,conclusion,workflowName,url,displayTitle,headBranch"))
               (lambda (results)
                 (let* ((summary (or (cdr (assq :ci-summary results)) ""))
                        (log (or (cdr (assq :ci-log results)) ""))
                        (jobs (or (cdr (assq :ci-jobs results)) "")))
                   (funcall callback (funcall finish summary log jobs)))))
            ;; Synchronous path
            (let* ((ci-summary (agent-shell-workflow-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug))
                   (ci-log (agent-shell-workflow-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug "--log-failed"))
                   (ci-jobs (agent-shell-workflow-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug "--json" "jobs,conclusion,workflowName,url,displayTitle,headBranch")))
              (funcall finish ci-summary ci-log ci-jobs))))
      (if callback
          (funcall callback updated-ctx)
        updated-ctx))))

(register-agent-shell-workflow fix-ci
  :doc "Download CI artifacts to local filesystem and prompt agent to fix build failure"
  :category "CI/CD"
  :args ((repo :prompt "Repository: " :optional t)
         (run-id :prompt "Run ID: " :type integer :optional t))
  :pre-op #'agent-shell-workflow-library--fix-ci-pre-op
  :template "Investigate and fix the CI failure in {{args.repo}} (run #{{args.run-id}}).

## CI Artifacts:
- Failed step log: `{{ci-log-file}}`
- Failing jobs metadata: `{{ci-jobs-file}}`
- CI triage index: `{{ci-index-file}}`

Do NOT read the entire log file into context. Inspect the logs as files using search or tail inspection (failures typically appear in the last 150-200 lines).

## Instructions:
1. Analyze failure in `{{ci-log-file}}` (search FAIL, errors, panics, or lint failures).
2. Write structured fix plan to `{{ci-plan-file}}` (root cause, action items, verification).
3. Present fix plan to user and ask confirmation before modifying source files.
4. Implement targeted fix and verify with narrow tests."
  :submit t
  :target :session-reuse
  :clear t)

;; PR review comment remediation

(defun agent-shell-workflow-library--pr-review-pre-op (ctx &optional callback)
  "Fetch PR review comments for :pr-number in CTX and save them locally.
Markdown summary and JSON export are saved under a unique directory
under <project-root>/.agent/pr-comments/ so the agent can inspect them
as files without colliding across PRs or review attempts.
When CALLBACK is given, executes asynchronously."
  (let* ((args (plist-get ctx :args))
         (raw-repo (or (plist-get args :repo)
                       (ignore-errors
                         (and (fboundp 'magit-dash--repo-at-point)
                              (when-let* ((r (magit-dash--repo-at-point)))
                                (magit-dash-repo-name r))))
                       (ignore-errors
                         (let ((slug (string-trim (shell-command-to-string "gh repo view --json nameWithOwner --jq .nameWithOwner"))))
                           (unless (or (string-empty-p slug) (string-match-p "^error" slug))
                             slug)))))
         (repo-slug (when raw-repo (agent-shell-workflow-library--resolve-repo-slug raw-repo)))
         (pr-arg (plist-get args :pr-number))
         (pr-number (or (and pr-arg (if (numberp pr-arg) pr-arg (string-to-number (format "%s" pr-arg))))
                        (ignore-errors
                          (let ((val (string-trim (shell-command-to-string "gh pr view --json number --jq .number"))))
                            (when (and val (not (string-empty-p val)) (string-match-p "^[0-9]+$" val))
                              (string-to-number val))))))
         (pr-str (if pr-number (format "%s" pr-number)
                   (user-error "No PR number specified or detected for pr-review-patch")))
         (repo-args (if repo-slug (list "--repo" repo-slug) nil))
         (finish
          (lambda (view-json-raw api-json-raw raw-comments)
            (let* ((view-obj (ignore-errors
                               (json-parse-string view-json-raw :object-type 'hash-table :array-type 'array)))
                   (api-arr (ignore-errors
                              (when (and api-json-raw (not (string-empty-p api-json-raw)))
                                (json-parse-string api-json-raw :object-type 'hash-table :array-type 'array))))
                   (head-ref (when (hash-table-p view-obj) (gethash "headRefName" view-obj)))
                   (branch (or (plist-get args :branch)
                               (and (stringp head-ref) (not (string-empty-p head-ref)) head-ref)
                               (agent-shell-workflow-library--current-branch)))
                   (root (agent-shell-workflow-library--project-root))
                   (base-pr-dir (expand-file-name ".agent/pr-comments" root))
                   (pr-dir (agent-shell-workflow-library--unique-artifact-dir
                            base-pr-dir
                            (format "pr-%s" pr-str)
                            branch
                            (plist-get args :date)))
                   (md-file (expand-file-name (format "pr-%s-comments.md" pr-str) pr-dir))
                   (json-file (expand-file-name (format "pr-%s-comments.json" pr-str) pr-dir))
                   (alias-md-file (expand-file-name "pr-comments.md" pr-dir))
                   (alias-json-file (expand-file-name "pr-comments.json" pr-dir))
                   (plan-file (expand-file-name "review-plan.md" pr-dir))
                   (rel-pr-dir (file-relative-name pr-dir root))
                   (rel-md-file (file-relative-name md-file root))
                   (rel-json-file (file-relative-name json-file root))
                   (rel-plan-file (file-relative-name plan-file root))
                   ;; Render Markdown
                   (md-content (agent-shell-workflow-library--format-pr-comments-markdown
                                repo-slug pr-str view-obj api-arr raw-comments))
                   ;; Build structured JSON
                   (json-content
                    (if (hash-table-p view-obj)
                        (let ((table (make-hash-table :test #'equal)))
                          (puthash "pr" (or pr-number (string-to-number pr-str)) table)
                          (when repo-slug (puthash "repo" repo-slug table))
                          (when-let* ((t-val (gethash "title" view-obj))) (puthash "title" t-val table))
                          (when-let* ((u-val (gethash "url" view-obj))) (puthash "url" u-val table))
                          (puthash "reviews" (or (gethash "reviews" view-obj) []) table)
                          (puthash "comments" (or (gethash "comments" view-obj) []) table)
                          (puthash "inline_comments" (or api-arr []) table)
                          (or (ignore-errors (json-serialize table)) "{}"))
                      (if (and view-json-raw (not (string-empty-p view-json-raw)))
                          view-json-raw
                        "{}")))
                   ;; Summary string
                   (pr-summary
                    (if (hash-table-p view-obj)
                        (let* ((title (or (gethash "title" view-obj) ""))
                               (author-obj (gethash "author" view-obj))
                               (author (if (hash-table-p author-obj) (gethash "login" author-obj) (format "%s" (or author-obj ""))))
                               (reviews (gethash "reviews" view-obj))
                               (comments (gethash "comments" view-obj))
                               (inline api-arr))
                          (format "PR #%s: %s (by @%s) — %d review(s), %d comment(s), %d inline comment(s)"
                                  pr-str title author
                                  (if (vectorp reviews) (length reviews) 0)
                                  (if (vectorp comments) (length comments) 0)
                                  (if (vectorp inline) (length inline) 0)))
                      (format "PR #%s in %s" pr-str (or repo-slug "repository")))))
              ;; Write artifacts to disk
              (agent-shell-workflow-library--write-file md-file md-content)
              (agent-shell-workflow-library--write-file json-file (or json-content "{}"))
              (agent-shell-workflow-library--write-file alias-md-file md-content)
              (agent-shell-workflow-library--write-file alias-json-file (or json-content "{}"))
              ;; Populate context
              (let* ((updated-args (plist-put (copy-sequence args) :pr-number (or pr-number (string-to-number pr-str))))
                     (updated-ctx (plist-put (copy-sequence ctx) :args updated-args)))
                (when repo-slug
                  (setq updated-args (plist-put updated-args :repo repo-slug))
                  (setq updated-ctx (plist-put updated-ctx :args updated-args)))
                (setq updated-ctx (plist-put updated-ctx :pr-summary pr-summary))
                (setq updated-ctx (plist-put updated-ctx :pr-dir rel-pr-dir))
                (setq updated-ctx (plist-put updated-ctx :pr-comments-file rel-md-file))
                (setq updated-ctx (plist-put updated-ctx :pr-comments-json-file rel-json-file))
                (setq updated-ctx (plist-put updated-ctx :pr-plan-file rel-plan-file))
                (setq updated-ctx (plist-put updated-ctx :pr-comments raw-comments))
                updated-ctx)))))
    (if callback
        (agent-shell-workflow--gather-async
         (delq nil
               (list (append (list :view "gh" "pr" "view" pr-str "--json"
                                   "number,title,author,url,reviews,comments,headRefName")
                             repo-args)
                     (when repo-slug
                       (list :api "gh" "api"
                             (format "repos/%s/pulls/%s/comments" repo-slug pr-str)
                             "--paginate"))
                     (append (list :raw "gh" "pr" "view" pr-str "--comments")
                             repo-args)))
         (lambda (results)
           (let ((view-json-raw (or (cdr (assq :view results)) ""))
                 (api-json-raw (or (cdr (assq :api results)) ""))
                 (raw-comments (or (cdr (assq :raw results)) "")))
             (funcall callback (funcall finish view-json-raw api-json-raw raw-comments)))))
      ;; Synchronous path
      (let* ((view-json-raw
              (apply #'agent-shell-workflow-library--shell
                     "gh" "pr" "view" pr-str "--json"
                     "number,title,author,url,reviews,comments,headRefName"
                     repo-args))
             (api-json-raw
              (when repo-slug
                (agent-shell-workflow-library--shell
                 "gh" "api" (format "repos/%s/pulls/%s/comments" repo-slug pr-str)
                 "--paginate")))
             (raw-comments
              (apply #'agent-shell-workflow-library--shell
                     "gh" "pr" "view" pr-str "--comments"
                     repo-args)))
        (funcall finish view-json-raw api-json-raw raw-comments)))))

(register-agent-shell-workflow pr-review-patch
  :doc "Fetch PR review comments to local filesystem and draft remediation patch"
  :category "Code Review"
  :args ((pr-number :prompt "PR number: " :type integer :optional t)
         (repo :prompt "Repository: " :optional t))
  :pre-op #'agent-shell-workflow-library--pr-review-pre-op
  :template "Address the review comments on PR #{{args.pr-number}} in {{args.repo}}.

## PR Summary:
{{pr-summary}}

## Review Comments:
- Markdown summary: `{{pr-comments-file}}`
- Structured JSON: `{{pr-comments-json-file}}`

Do NOT read all raw comment data into context at once. Review comments in `{{pr-comments-file}}` using search or file viewing.

## Instructions:
1. Categorize comments in `{{pr-comments-file}}` (change-required, question, nit, praise, discussion, resolved).
2. Write review plan to `{{pr-plan-file}}` (proposed changes, reviewer replies, user questions).
3. Present summary counts and discussion items to user for confirmation.
4. Apply confirmed changes and verify tests pass."
  :submit t
  :target :session-reuse)

;; PR status synchronizer and `pr-status-sync' workflow

(require 'org)
(require 'subr-x)
(require 'orgx nil t)

(declare-function magit-dash-gh-pr-status-collect-all "magit-dash-gh-pr" (&optional dir))
(declare-function magit-dash-gh-pr-attach-tracking "magit-dash-gh-pr" (pr))
(declare-function magit-dash-gh-pr-commit-tracking "magit-dash-gh-pr" (prs))
(declare-function magit-dash-gh-pr-tracking-fingerprint "magit-dash-gh-pr" (prs))
(defvar org-directory)
(defvar magit-dash-gh-pr-status-max-prs)

(defcustom agent-shell-workflow-pr-status-file
  (expand-file-name "pr-review-status.org"
                    (if (boundp 'org-directory) org-directory "~/org"))
  "Org file kept in sync with the open PR list by `pr-status-sync'."
  :type 'file
  :group 'agent-shell-workflow)

(defvar agent-shell-workflow-github--pr-status-fingerprint nil
  "Fingerprint of the PR set last synced by `pr-status-sync' (in memory only).")

(defvar agent-shell-workflow-github--pr-status-last-count 0
  "Number of PRs in the last committed `pr-status-sync' (in memory only).")

(defvar agent-shell-workflow-github--pr-status-failures 0
  "Consecutive PR collection failures in the `pr-status-sync' pre-op.")

(defconst agent-shell-workflow-github--failure-warn-threshold 3
  "Consecutive collection failures after which a warning replaces a message.")

(defun agent-shell-workflow-github--pr-field (pr key)
  "Return tracking KEY of PR."
  (plist-get (plist-get pr :tracking) key))

(defun agent-shell-workflow-github--pr-step (pr)
  "Return the STEP string (the bucket name) for PR."
  (agent-shell-workflow-github--line (or (plist-get pr :bucket) "unknown")))

(defun agent-shell-workflow-github--line (value)
  "Return VALUE as a single-line string safe to embed in Org.
Control characters (including newlines) collapse to single spaces."
  (string-trim
   (replace-regexp-in-string "[[:cntrl:]]+" " "
                             (format "%s" (if (null value) "" value)))))

(defun agent-shell-workflow-github--pr-heading-text (pr)
  "Return the sanitized task heading text (without stars or keyword) for PR."
  (let ((text (agent-shell-workflow-github--line
               (format "%s: %s" (plist-get pr :number)
                       (or (agent-shell-workflow-github--pr-field pr :action-summary)
                           (plist-get pr :title) "")))))
    ;; A trailing :word: would be parsed as heading tags.
    (string-trim-right
     (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:\\'" "" text))))

(defun agent-shell-workflow-github--body-line (value)
  "Return VALUE as a safe Org list item line, or nil if empty.
The line has no trailing newline."
  (let ((l (agent-shell-workflow-github--line value)))
    (unless (string-empty-p l) (concat "- " l))))

(defun agent-shell-workflow-github--cell (value)
  "Return VALUE formatted as a single-line Org table cell string."
  (replace-regexp-in-string "|" "/" (agent-shell-workflow-github--line value)))

(defun agent-shell-workflow-github--find-heading (title)
  "Return position of the level-1 heading titled TITLE, or nil.
Tolerates a TODO keyword, priority cookie and tags, and ignores
text inside example/src/export/comment blocks."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((re (concat "^\\* +\\(?:[A-Z]+ +\\)?\\(?:\\[#[A-Z0-9]+\\] +\\)?"
                     (regexp-quote title)
                     "\\(?:[ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$"))
         found)
     (while (and (not found) (re-search-forward re nil t))
       (unless (org-in-block-p '("example" "src" "export" "comment"
                                 "verse" "quote" "center"))
         (setq found (line-beginning-position))))
     found)))

(defun agent-shell-workflow-github--ensure-skeleton ()
  "Ensure the current Org buffer has the title, filetags, Summary and Tasks."
  (org-with-wide-buffer
   (when (zerop (buffer-size))
     (insert "#+title: Open PR status (@me)\n#+filetags: :agent:prs:\n\n"))
   (dolist (h '("Summary" "Tasks"))
     (unless (agent-shell-workflow-github--find-heading h)
       (goto-char (point-max))
       (unless (or (bobp) (bolp)) (insert "\n"))
       (insert (format "* %s\n" h))))))

(defun agent-shell-workflow-github--task-entries ()
  "Return an alist ((REPO . PR) . MARKER) of entries with REPO and PR properties."
  (let (res)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (let ((repo (org-entry-get nil "REPO"))
              (pr (org-entry-get nil "PR")))
          (when (and repo pr)
            (push (cons (cons repo pr) (copy-marker (point))) res))))
      t))
    (nreverse res)))

(defun agent-shell-workflow-github--set-prop (prop value pos)
  "Set PROP to single-line VALUE on the entry at POS."
  (let ((value (agent-shell-workflow-github--line value)))
    (if (fboundp 'orgx-entry-set-property)
        (orgx-entry-set-property prop value pos)
      (org-with-point-at pos (org-entry-put nil prop value)))))

(defun agent-shell-workflow-github--write-summary (prs)
  "Replace the body of the Summary section with a refresh line and table of PRS."
  (org-with-wide-buffer
   (goto-char (agent-shell-workflow-github--find-heading "Summary"))
   (forward-line 1)
   ;; The heading may be the last line, with no trailing newline.
   (unless (bolp) (insert "\n"))
   (let ((start (point))
         (end (save-excursion
                (cond ((looking-at org-outline-regexp-bol) (point))
                      ((outline-next-heading) (point))
                      (t (point-max))))))
     (delete-region start end)
     (insert (format "Last refreshed: %s\n\n" (format-time-string "[%F %a %R]")))
     (let ((table-start (point)))
       (insert "| PR | Repo | Branch | Approvals | CI failing | Unresolved | Bucket |\n|-\n")
       (dolist (pr prs)
         (insert (format "| %s |\n"
                         (mapconcat
                          #'agent-shell-workflow-github--cell
                          (list (plist-get pr :number) (plist-get pr :repo)
                                (plist-get pr :branch)
                                (or (plist-get pr :approvals) 0)
                                (length (agent-shell-workflow-github--pr-field
                                         pr :failing-checks))
                                (or (plist-get pr :threads-open) 0)
                                (plist-get pr :bucket))
                          " | "))))
       (save-excursion (goto-char table-start) (org-table-align)))
     (insert "\n"))))

(defun agent-shell-workflow-github--open-keyword ()
  "Return the keyword for an open task, or nil when none is defined.
That is TODO if defined, else the first open keyword.  With a keyword
set such as `#+TODO: | DONE' open tasks carry no keyword; matching and
closing are then done by the REPO and PR properties."
  (if (member "TODO" org-not-done-keywords)
      "TODO"
    (car org-not-done-keywords)))

(defun agent-shell-workflow-github--append-task (pr)
  "Append a new task entry for PR under the Tasks heading."
  (org-with-wide-buffer
   (goto-char (agent-shell-workflow-github--find-heading "Tasks"))
   (let* ((stars (make-string (1+ (org-current-level)) ?*))
          (kw (agent-shell-workflow-github--open-keyword))
          (l #'agent-shell-workflow-github--line))
     (org-end-of-subtree t t)
     (unless (bolp) (insert "\n"))
     (insert
      (format "%s %s%s\n:PROPERTIES:\n:REPO: %s\n:PR: %s\n:STEP: %s\n:LAST_SEEN: %s\n:BRANCH: %s\n:URL: %s\n:END:\n%s%s %s\n"
              stars (if kw (concat kw " ") "")
              (agent-shell-workflow-github--pr-heading-text pr)
              (funcall l (plist-get pr :repo)) (funcall l (plist-get pr :number))
              (agent-shell-workflow-github--pr-step pr)
              (funcall l (agent-shell-workflow-github--pr-field pr :last-seen))
              (funcall l (plist-get pr :branch)) (funcall l (plist-get pr :url))
              (mapconcat (lambda (s) (concat s "\n"))
                         (delq nil
                               (list (agent-shell-workflow-github--body-line
                                      (plist-get pr :title))
                                     (agent-shell-workflow-github--body-line
                                      (agent-shell-workflow-github--pr-field
                                       pr :delta-reason))))
                         "")
              (concat stars "*") "Notes and In-Flight Decisions")))))

(defun agent-shell-workflow-github--set-todo (marker state)
  "Set the TODO state of the entry at MARKER to STATE (\"\" clears it).
No logging, notes, dependency checks or state-change hooks run."
  (let ((org-log-done nil)
        (org-todo-log-states nil)
        (org-enforce-todo-dependencies nil)
        (org-enforce-todo-checkbox-dependencies nil)
        (org-after-todo-state-change-hook nil)
        (org-blocker-hook nil))
    (cl-letf (((symbol-function 'org-add-log-setup) #'ignore)
              ((symbol-function 'org-add-log-note) #'ignore))
      (org-with-point-at marker
        (org-back-to-heading t)
        (org-todo (if (string-empty-p state) 'none state))))))

(defun agent-shell-workflow-github--update-task (marker pr)
  "Update STEP and LAST_SEEN (and the heading on step change) of PR at MARKER.
A task in a done state is reopened since PR is present (open)."
  (let ((step (agent-shell-workflow-github--pr-step pr))
        (state (org-with-point-at marker (org-get-todo-state))))
    (unless (equal step (org-with-point-at marker (org-entry-get nil "STEP")))
      (agent-shell-workflow-github--set-prop "STEP" step marker)
      (org-with-point-at marker
        (org-back-to-heading t)
        (when (looking-at org-complex-heading-regexp)
          (replace-match (agent-shell-workflow-github--pr-heading-text pr)
                         t t nil 4))))
    (when (and state (member state org-done-keywords))
      (agent-shell-workflow-github--set-todo
       marker (or (agent-shell-workflow-github--open-keyword) "")))
    (agent-shell-workflow-github--set-prop
     "LAST_SEEN"
     (agent-shell-workflow-github--pr-field pr :last-seen) marker)))

(defun agent-shell-workflow-github--close-task (marker)
  "Mark the task at MARKER DONE and set CLOSED, without prompts or logging."
  (agent-shell-workflow-github--set-todo
   marker (or (car org-done-keywords) "DONE"))
  (org-with-point-at marker
    (org-back-to-heading t)
    (unless (org-entry-get nil "CLOSED")
      (org-add-planning-info 'closed (org-current-effective-time)))))

(defun agent-shell-workflow-github--close-allowed-p (prs)
  "Return non-nil when PRS is complete enough to close vanished tasks.
An empty or truncated (>= max-prs) list is never trusted."
  (and prs
       (< (length prs)
          (or (bound-and-true-p magit-dash-gh-pr-status-max-prs) 50))))

(defun agent-shell-workflow-github--sync-buffer (prs)
  "Apply PRS to the current Org buffer."
  (agent-shell-workflow-github--ensure-skeleton)
  (agent-shell-workflow-github--write-summary prs)
  (let ((entries (agent-shell-workflow-github--task-entries))
        (seen nil))
    (dolist (pr prs)
      (let* ((key (cons (agent-shell-workflow-github--line (plist-get pr :repo))
                        (agent-shell-workflow-github--line (plist-get pr :number))))
             (hit (assoc key entries)))
        (push key seen)
        (if hit
            (agent-shell-workflow-github--update-task (cdr hit) pr)
          (agent-shell-workflow-github--append-task pr))))
    (when (agent-shell-workflow-github--close-allowed-p prs)
      (dolist (e entries)
        (unless (member (car e) seen)
          (let ((state (org-with-point-at (cdr e) (org-get-todo-state))))
            (when (if state
                      (member state org-not-done-keywords)
                    (null org-not-done-keywords))
              (agent-shell-workflow-github--close-task (cdr e)))))))
    (dolist (e entries) (set-marker (cdr e) nil))))

(defun agent-shell-workflow-github-pr-sync (prs &optional file)
  "Non-destructively sync the PR status Org FILE with PRS.
PRS are PR plists carrying a `:tracking' plist (see
`magit-dash-gh-pr-attach-tracking').  FILE defaults to
`agent-shell-workflow-pr-status-file'.  Only the Summary body is
rewritten; existing task entries are matched by their REPO and PR
properties and get only STEP/LAST_SEEN (and heading on step change)
updated.  PRs no longer open are marked DONE only when PRS is
non-empty and below `magit-dash-gh-pr-status-max-prs' (an empty or
truncated list is not trusted).  A symlinked FILE stays a symlink.
Signals an error when a buffer visiting FILE has unsaved changes.
Return FILE."
  (let* ((file (expand-file-name (or file agent-shell-workflow-pr-status-file)))
         (target (file-truename file))
         (visiting (or (find-buffer-visiting file) (find-buffer-visiting target)))
         (dir (file-name-directory target))
         content mode tmp)
    (when (and visiting (buffer-modified-p visiting))
      (user-error "Buffer %s has unsaved changes; save or revert before syncing PR status"
                  (buffer-name visiting)))
    (make-directory dir t)
    (setq mode (and (file-exists-p target) (file-modes target)))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (inhibit-message t))
        (when (file-exists-p target) (insert-file-contents target))
        (delay-mode-hooks (org-mode))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (agent-shell-workflow-github--sync-buffer prs))
        (setq content (buffer-string))))
    (setq tmp (make-temp-file (expand-file-name ".pr-status-" dir)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil tmp nil 'silent))
          (when mode (set-file-modes tmp mode))
          (rename-file tmp target t)
          (setq tmp nil))
      (when (and tmp (file-exists-p tmp)) (delete-file tmp)))
    (when (and visiting (buffer-live-p visiting))
      (with-current-buffer visiting
        (let ((inhibit-read-only t))
          (revert-buffer t t t))))
    file))

(defun agent-shell-workflow-github--delta-lines (prs)
  "Return compact delta lines for PRS whose delta is not `unchanged'."
  (delq nil
        (mapcar
         (lambda (pr)
           (unless (eq (agent-shell-workflow-github--pr-field pr :delta-type)
                       'unchanged)
             (format "- %s#%s %s — %s — %s"
                     (plist-get pr :repo) (plist-get pr :number)
                     (plist-get pr :bucket)
                     (agent-shell-workflow-github--pr-field pr :delta-reason)
                     (agent-shell-workflow-github--pr-field pr :action-summary))))
         prs)))

(defun agent-shell-workflow-github--note-collection-failure (reason)
  "Record a consecutive collection failure and report REASON (message or warning)."
  (cl-incf agent-shell-workflow-github--pr-status-failures)
  (if (>= agent-shell-workflow-github--pr-status-failures
          agent-shell-workflow-github--failure-warn-threshold)
      (display-warning
       'agent-shell-workflow
       (format "pr-status-sync: %d consecutive collection failures: %s"
               agent-shell-workflow-github--pr-status-failures reason)
       :warning)
    (message "pr-status-sync: %s" reason)))

(defun agent-shell-workflow-github--pr-status-commit (ctx)
  "Commit the PR tracking and fingerprint staged in CTX by the pre-op.
Runs from `:after-handoff' once the turn has been handed off."
  (when-let* ((prs-cell (plist-member ctx :pr-status-pending-prs)))
    (magit-dash-gh-pr-commit-tracking (cadr prs-cell))
    (setq agent-shell-workflow-github--pr-status-fingerprint
          (plist-get ctx :pr-status-pending-fingerprint)
          agent-shell-workflow-github--pr-status-last-count
          (length (cadr prs-cell)))))

(defun agent-shell-workflow-github--pr-status-sync-pre-op (ctx)
  "Collect PR state, sync the status file, and add the delta to CTX.
Skips the turn (via `agent-shell-workflow-skip') when collection fails,
returns nothing after a previously non-empty sync, or the PR
fingerprint is unchanged since the last sync.  Tracking and the
fingerprint are only staged in CTX; `:after-handoff' commits them."
  (require 'magit-dash-gh-pr nil t)
  (let* ((collected
          (condition-case err
              (progn
                (unless (fboundp 'magit-dash-gh-pr-status-collect-all)
                  (error "magit-dash PR tracking is not available"))
                (prog1 (magit-dash-gh-pr-status-collect-all)
                  (setq agent-shell-workflow-github--pr-status-failures 0)))
            (error
             (let ((reason (format "PR collection failed (%s); status file left untouched"
                                   (error-message-string err))))
               (agent-shell-workflow-github--note-collection-failure reason)
               (agent-shell-workflow-skip reason)))))
         (prs (mapcar #'magit-dash-gh-pr-attach-tracking collected))
         (fingerprint (magit-dash-gh-pr-tracking-fingerprint prs)))
    (when (and (null prs)
               agent-shell-workflow-github--pr-status-fingerprint
               (> agent-shell-workflow-github--pr-status-last-count 0))
      (agent-shell-workflow-skip
       "PR collection returned nothing after a non-empty sync; status file left untouched"))
    (when (equal fingerprint agent-shell-workflow-github--pr-status-fingerprint)
      (agent-shell-workflow-skip "PR status unchanged; no turn needed"))
    (let ((file (agent-shell-workflow-github-pr-sync prs))
          (lines (agent-shell-workflow-github--delta-lines prs)))
      (setq ctx (plist-put ctx :pr-status-file file))
      (setq ctx (plist-put ctx :pr-status-pending-prs prs))
      (setq ctx (plist-put ctx :pr-status-pending-fingerprint fingerprint))
      (setq ctx (plist-put ctx :after-handoff
                           'agent-shell-workflow-github--pr-status-commit))
      (plist-put ctx :pr-status-delta
                 (if lines
                     (string-join lines "\n")
                   "- (no per-PR changes; set membership changed)")))))

;; No :model is set: the workflow spec has no model slot, so the model comes
;; from the target session.  Use a small/cheap model session for this turn.
(register-agent-shell-workflow pr-status-sync
  :doc "Sync the PR status Org file and triage only changed PRs (skips when nothing changed)"
  :category "Code Review"
  :pre-op #'agent-shell-workflow-github--pr-status-sync-pre-op
  :template "The PR status file `{{pr-status-file}}` was just updated automatically (summary table, task headings, STEP and LAST_SEEN). Do NOT rewrite or re-read the whole file.

## Changed PRs:
{{pr-status-delta}}

## Instructions:
For each changed PR above, add a short triage note (one or two lines: what changed, what to do next) using the orgx MCP tools:
- `org_heading_append_note` with match_property \"PR\" and match_value the PR number.
- `org_heading_set_property` (same match_property/match_value) only if a property needs correcting.
Keep your output brief."
  :submit t
  :target :session-reuse
  :clear t)

;;; Periodic `pr-status-sync' trigger

(defcustom agent-shell-workflow-pr-status-timer-interval 3600
  "Seconds between automatic `pr-status-sync' dispatches."
  :type 'natnum
  :group 'agent-shell-workflow)

(defcustom agent-shell-workflow-pr-status-timer-target 'queue
  "Where timer-driven `pr-status-sync' runs are dispatched.
`queue' enqueues a background item via agent-shell-queue so no live
session is interrupted; `session' uses the workflow's own target."
  :type '(choice (const :tag "Background queue" queue)
                 (const :tag "Session" session))
  :group 'agent-shell-workflow)

(defvar agent-shell-workflow-pr-status--timer nil
  "Timer driving periodic `pr-status-sync', or nil when stopped.")

(defun agent-shell-workflow-pr-status--dispatch ()
  "Dispatch `pr-status-sync' once.  Never signals; skip is not an error."
  (condition-case err
      (apply #'agent-shell-workflow-dispatch 'pr-status-sync
             (when (eq agent-shell-workflow-pr-status-timer-target 'queue)
               (list :target :queue)))
    (agent-shell-workflow-skip
     (message "pr-status-sync: skipped (%s)" (cadr err)) nil)
    (error
     (message "pr-status-sync timer: %s" (error-message-string err)) nil)))

;;;###autoload
(defun agent-shell-workflow-pr-status-run-now ()
  "Dispatch `pr-status-sync' once, now."
  (interactive)
  (agent-shell-workflow-pr-status--dispatch))

;;;###autoload
(defun agent-shell-workflow-pr-status-timer-stop ()
  "Stop the periodic `pr-status-sync' timer.  Safe when not running."
  (interactive)
  (when (timerp agent-shell-workflow-pr-status--timer)
    (cancel-timer agent-shell-workflow-pr-status--timer))
  (setq agent-shell-workflow-pr-status--timer nil))

;;;###autoload
(defun agent-shell-workflow-pr-status-timer-start ()
  "Start (or restart) the periodic `pr-status-sync' timer.
Uses the current `agent-shell-workflow-pr-status-timer-interval'."
  (interactive)
  (agent-shell-workflow-pr-status-timer-stop)
  (setq agent-shell-workflow-pr-status--timer
        (run-with-timer agent-shell-workflow-pr-status-timer-interval
                        agent-shell-workflow-pr-status-timer-interval
                        #'agent-shell-workflow-pr-status--dispatch)))

(provide 'agent-shell-workflow-github)

;;; agent-shell-workflow-github.el ends here
