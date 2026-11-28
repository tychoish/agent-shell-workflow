;;; agent-shell-workflow-github.el --- GitHub and CI workflows for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-workflow

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

(defun agent-shell-workflow-library--owner-repo-from-url (url)
  "Extract \"OWNER/REPO\" string from a git remote URL."
  (when (stringp url)
    (let ((clean (replace-regexp-in-string "\\.git\\'" "" (string-trim url))))
      (cond
       ((string-match "github\\.com[:/]\\([^/]+\\)/\\([^/]+\\)\\'" clean)
        (format "%s/%s" (match-string 1 clean) (match-string 2 clean)))
       ((string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" clean)
        (format "%s/%s" (match-string 1 clean) (match-string 2 clean)))))))

(defun agent-shell-workflow-library--resolve-repo-slug (&optional repo)
  "Resolve REPO name or path to an OWNER/NAME GitHub repository slug string."
  (let* ((dir (cond ((and (stringp repo) (file-directory-p repo))
                     (file-name-as-directory (expand-file-name repo)))
                    (t default-directory)))
         (default-directory dir)
         (cur-repo-name (file-name-nondirectory (directory-file-name default-directory)))
         (matching-dir-p (or (null repo)
                             (and (stringp repo)
                                  (or (file-directory-p repo)
                                      (equal repo cur-repo-name))))))
    (cond
     ;; Valid OWNER/NAME slug directly provided (and not a local path)
     ((and (stringp repo)
           (not (file-directory-p repo))
           (string-match-p "\\`[a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+\\'" repo))
      repo)
     ;; Git URL directly provided
     ((and (stringp repo)
           (agent-shell-workflow-library--owner-repo-from-url repo)))
     ;; Registered magit-dash repo by name if available
     ((and (stringp repo) (boundp 'magit-dash-repo-list)
           (when-let* ((r (seq-find (lambda (it)
                                      (equal (magit-dash-repo-name it) repo))
                                    magit-dash-repo-list))
                       (raw (or (and (fboundp 'magit-dash-repo-repo) (magit-dash-repo-repo r))
                                (and (fboundp 'magit-dash-repo-clone-url) (magit-dash-repo-clone-url r))
                                (and (fboundp 'magit-dash-repo-upstream-repo) (magit-dash-repo-upstream-repo r))))
                       (slug (agent-shell-workflow-library--owner-repo-from-url raw)))
             slug)))
     ;; Local git remotes: origin or pushdefault (primary for current directory / matching repo)
     ((and matching-dir-p
           (let ((slug (agent-shell-workflow-library--owner-repo-from-url
                        (or (ignore-errors (magit-get "remote" "origin" "url"))
                            (ignore-errors (magit-get "remote" "pushdefault" "url"))))))
             (and slug (not (string-empty-p slug)) slug))))
     ;; gh repo view fallback
     ((ignore-errors
        (let ((slug (string-trim (shell-command-to-string "gh repo view --json nameWithOwner --jq .nameWithOwner"))))
          (unless (or (string-empty-p slug) (string-match-p "^error" slug))
            slug))))
     ;; magit-dash repo-info if loaded
     ((ignore-errors
        (and (fboundp 'magit-dash-gh--repo-info)
             (when-let* ((info (magit-dash-gh--repo-info))
                         (o (plist-get info :owner))
                         (r (plist-get info :repo)))
               (format "%s/%s" o r)))))
     ;; Upstream remote as last resort
     ((and matching-dir-p
           (let ((slug (agent-shell-workflow-library--owner-repo-from-url
                        (ignore-errors (magit-get "remote" "upstream" "url")))))
             (and slug (not (string-empty-p slug)) slug))))
     (t repo))))

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
If a run matching the local HEAD commit is failing, return its run-id.
If the latest run on TARGET-BRANCH is failing, return its run-id
automatically.  Otherwise, prompt the user with an ACR picker showing
recent runs."
  (let* ((branch (or target-branch (agent-shell-workflow-library--current-branch)))
         (head-sha (ignore-errors
                     (let ((sha (agent-shell-workflow-library--git-output "rev-parse" "HEAD")))
                       (unless (string-empty-p sha) sha))))
         (runs (agent-shell-workflow-library--fetch-runs repo 20))
         (sha-run (when head-sha
                    (seq-find (lambda (r)
                                (let ((r-sha (map-elt r 'headSha)))
                                  (and (stringp r-sha)
                                       (or (string-prefix-p r-sha head-sha)
                                           (string-prefix-p head-sha r-sha)))))
                              runs)))
         (sha-conclusion (and sha-run (or (map-elt sha-run 'conclusion) (map-elt sha-run 'status))))
         (sha-failing-p (and sha-run
                             (member sha-conclusion '("failure" "cancelled" "timed_out" "action_required"))))
         (branch-runs (seq-filter (lambda (r) (equal (map-elt r 'headBranch) branch)) runs))
         (target-runs (or branch-runs runs))
         (latest (car target-runs))
         (latest-conclusion (and latest (or (map-elt latest 'conclusion) (map-elt latest 'status))))
         (latest-failing-p (and latest
                                (member latest-conclusion '("failure" "cancelled" "timed_out" "action_required")))))
    (cond
     (sha-failing-p
      (map-elt sha-run 'databaseId))
     (latest-failing-p
      (map-elt latest 'databaseId))
     ((null target-runs)
      (user-error "No CI runs found for %s" repo))
     (t
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
                              (is-head (and head-sha sha
                                            (or (string-prefix-p sha head-sha)
                                                (string-prefix-p head-sha sha))))
                              (cand (format "#%s %s (%s) [%s]%s"
                                            id title short-sha b
                                            (if is-head " [HEAD]" "")))
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
    (let* ((title (or (map-elt view-obj "title") ""))
           (author-val (map-elt view-obj "author"))
           (author (cond ((or (hash-table-p author-val) (consp author-val))
                          (or (map-elt author-val "login") ""))
                         ((stringp author-val) author-val)
                         (t "")))
           (url (or (map-elt view-obj "url") ""))
           (reviews (map-elt view-obj "reviews"))
           (comments (map-elt view-obj "comments"))
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
          (let* ((u-obj (map-elt r "author"))
                 (user (if (or (hash-table-p u-obj) (consp u-obj)) (map-elt u-obj "login") (format "%s" (or u-obj ""))))
                 (state (or (map-elt r "state") ""))
                 (submitted (or (map-elt r "submittedAt") ""))
                 (r-url (or (map-elt r "url") ""))
                 (body (or (map-elt r "body") "")))
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
          (let* ((u-obj (map-elt c "author"))
                 (user (if (or (hash-table-p u-obj) (consp u-obj)) (map-elt u-obj "login") (format "%s" (or u-obj ""))))
                 (created (or (map-elt c "createdAt") ""))
                 (c-url (or (map-elt c "url") ""))
                 (body (or (map-elt c "body") "")))
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
          (let* ((u-obj (map-elt ic "user"))
                 (user (if (or (hash-table-p u-obj) (consp u-obj)) (map-elt u-obj "login") (format "%s" (or u-obj ""))))
                 (path (or (map-elt ic "path") ""))
                 (line (or (map-elt ic "line") (map-elt ic "original_line") 0))
                 (created (or (map-elt ic "created_at") ""))
                 (i-url (or (map-elt ic "html_url") (map-elt ic "url") ""))
                 (diff (or (map-elt ic "diff_hunk") ""))
                 (body (or (map-elt ic "body") "")))
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
or attempts.  ARGS may carry :root, the checkout to use instead of the
project root of `default-directory'.  When CALLBACK is given, executes
asynchronously."
  (let* ((args (plist-get ctx :args))
         (root (if-let* ((r (plist-get args :root)))
                   (file-name-as-directory (expand-file-name r))
                 (agent-shell-workflow-library--project-root)))
         (default-directory (or root default-directory))
         (raw-repo (or (plist-get args :repo)
                       (ignore-errors
                         (and (fboundp 'magit-dash--repo-at-point)
                              (when-let* ((r (magit-dash--repo-at-point)))
                                (or (and (fboundp 'magit-dash-ci--repo-slug)
                                         (magit-dash-ci--repo-slug r))
                                    (and (fboundp 'magit-dash-repo-repo)
                                         (magit-dash-repo-repo r))
                                    (and (fboundp 'magit-dash-repo-clone-url)
                                         (magit-dash-repo-clone-url r))
                                    (magit-dash-repo-name r)))))
                       (agent-shell-workflow-library--resolve-repo-slug)
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
                 (let* ((root (if-let* ((r (plist-get args :root)))
                                         (file-name-as-directory (expand-file-name r))
                                       (agent-shell-workflow-library--project-root)))
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
                   (funcall callback (funcall finish summary log jobs))))
               (plist-get args :root))
            ;; Synchronous path
            (let* ((ci-summary (agent-shell-workflow-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug))
                   (ci-log (agent-shell-workflow-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug "--log-failed"))
                   (ci-jobs (agent-shell-workflow-library--shell "gh" "run" "view" run-id-str "--repo" repo-slug "--json" "jobs,conclusion,workflowName,url,displayTitle,headBranch")))
              (funcall finish ci-summary ci-log ci-jobs))))
      (if callback
          (funcall callback updated-ctx)
        updated-ctx))))

(agent-shell-workflow-define fix-ci
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
  :buffer-display :background
  :clear t)

;; Multi-repository group CI remediation

(defconst agent-shell-workflow-library--failing-conclusions
  '("failure" "cancelled" "timed_out" "action_required")
  "GitHub Actions run conclusions treated as failing CI runs.")

(defun agent-shell-workflow-library--find-failing-run (runs branch &optional head-sha)
  "Return the failing run alist from RUNS for BRANCH or HEAD-SHA, or nil.
Checks a run matching HEAD-SHA first, then the latest run on BRANCH
\(falling back to the latest run in RUNS when BRANCH has no runs)."
  (let* ((sha-run (when (and (stringp head-sha) (not (string-empty-p head-sha)))
                    (seq-find (lambda (r)
                                (let ((r-sha (map-elt r 'headSha)))
                                  (and (stringp r-sha)
                                       (or (string-prefix-p r-sha head-sha)
                                           (string-prefix-p head-sha r-sha)))))
                              runs)))
         (sha-state (and sha-run (or (map-elt sha-run 'conclusion)
                                     (map-elt sha-run 'status))))
         (branch-runs (seq-filter (lambda (r) (equal (map-elt r 'headBranch) branch))
                                  runs))
         (latest (car (or branch-runs runs)))
         (latest-state (and latest (or (map-elt latest 'conclusion)
                                       (map-elt latest 'status)))))
    (cond
     ((and sha-run
           (member sha-state agent-shell-workflow-library--failing-conclusions))
      sha-run)
     ((and latest
           (member latest-state agent-shell-workflow-library--failing-conclusions))
      latest)
     (t nil))))

(defun agent-shell-workflow-library--failing-job-names (ci-jobs-json)
  "Extract failing job names from CI-JOBS-JSON string as a list of strings."
  (when-let* (((and (stringp ci-jobs-json) (not (string-empty-p ci-jobs-json))))
              (parsed (ignore-errors
                        (json-parse-string ci-jobs-json
                                           :object-type 'alist
                                           :array-type 'list)))
              (jobs (and (listp parsed) (alist-get 'jobs parsed))))
    (thread-last jobs
      (seq-filter (lambda (j)
                    (member (or (alist-get 'conclusion j) (alist-get 'status j))
                            agent-shell-workflow-library--failing-conclusions)))
      (seq-map (lambda (j) (alist-get 'name j)))
      (seq-filter #'stringp))))

(defun agent-shell-workflow-library--write-ci-artifacts
    (root repo-slug run-id-str run-branch date ci-summary ci-log ci-jobs)
  "Write CI artifacts under ROOT/.agent/fix-ci/ and return metadata plist.
REPO-SLUG, RUN-ID-STR, RUN-BRANCH, DATE, CI-SUMMARY, CI-LOG, and CI-JOBS
populate the run log, jobs JSON, triage index, and plan paths."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (parsed-jobs (ignore-errors
                        (json-parse-string ci-jobs
                                           :object-type 'alist
                                           :array-type 'list)))
         (effective-branch (or run-branch
                               (and (listp parsed-jobs)
                                    (alist-get 'headBranch parsed-jobs))
                               "main"))
         (base-ci-dir (expand-file-name ".agent/fix-ci" root))
         (ci-dir (agent-shell-workflow-library--unique-artifact-dir
                  base-ci-dir (format "run-%s" run-id-str) effective-branch date))
         (log-file (expand-file-name (format "run-%s-logs.txt" run-id-str) ci-dir))
         (jobs-file (expand-file-name (format "run-%s-jobs.json" run-id-str) ci-dir))
         (index-file (expand-file-name "ci-triage-index.md" ci-dir))
         (alias-log-file (expand-file-name "ci-logs.txt" ci-dir))
         (alias-jobs-file (expand-file-name "ci-jobs.json" ci-dir))
         (plan-file (expand-file-name "fix-plan.md" ci-dir))
         (log-content (if (and (stringp ci-log) (not (string-empty-p ci-log)))
                          ci-log
                        (format "No failed-step logs returned for run #%s.\n\nSummary:\n%s"
                                run-id-str ci-summary)))
         (jobs-content (if (and (stringp ci-jobs) (not (string-empty-p ci-jobs)))
                           ci-jobs
                         "{}"))
         (index-content
          (format "# CI Triage Index\n\n- **Repository**: %s\n- **Run ID**: %s\n- **Branch**: %s\n- **Generated**: %s\n- **Log File**: `%s`\n- **Jobs Metadata**: `%s`\n- **Fix Plan**: `%s`\n\n## Summary\n\n```\n%s\n```\n"
                  repo-slug run-id-str effective-branch
                  (format-time-string "%Y-%m-%dT%T%z")
                  (file-relative-name log-file root)
                  (file-relative-name jobs-file root)
                  (file-relative-name plan-file root)
                  ci-summary)))
    (agent-shell-workflow-library--write-file log-file log-content)
    (agent-shell-workflow-library--write-file jobs-file jobs-content)
    (agent-shell-workflow-library--write-file index-file index-content)
    (agent-shell-workflow-library--write-file alias-log-file log-content)
    (agent-shell-workflow-library--write-file alias-jobs-file jobs-content)
    (list :root root
          :repo repo-slug
          :run-id run-id-str
          :branch effective-branch
          :ci-dir ci-dir
          :log-file log-file
          :jobs-file jobs-file
          :index-file index-file
          :plan-file plan-file
          :failing-jobs (agent-shell-workflow-library--failing-job-names jobs-content))))

(defun agent-shell-workflow-library--group-ci-repo-meta (dir target-branch)
  "Resolve local repository metadata plist for DIR and optional TARGET-BRANCH."
  (let* ((root (file-name-as-directory (expand-file-name dir)))
         (default-directory root)
         (slug (agent-shell-workflow-library--resolve-repo-slug root))
         (branch (or target-branch
                     (ignore-errors
                       (let ((b (agent-shell-workflow-library--git-output
                                 "rev-parse" "--abbrev-ref" "HEAD")))
                         (unless (string-empty-p b) b)))
                     (agent-shell-workflow-library--current-branch)))
         (head-sha (ignore-errors
                     (let ((sha (agent-shell-workflow-library--git-output
                                 "rev-parse" "HEAD")))
                       (unless (string-empty-p sha) sha)))))
    (list :root root :slug slug :branch branch :head-sha head-sha)))

(defun agent-shell-workflow-library--format-group-ci-report (artifacts)
  "Format downloaded CI ARTIFACTS list into a compact Markdown report."
  (string-join
   (seq-map
    (lambda (art)
      (let ((jobs (plist-get art :failing-jobs)))
        (concat
         (format "### %s (`%s`) — run #%s [%s]\n"
                 (plist-get art :repo)
                 (plist-get art :root)
                 (plist-get art :run-id)
                 (plist-get art :branch))
         (when jobs
           (format "- Failing jobs: %s\n" (string-join jobs ", ")))
         (format "- Failed step log: `%s`\n" (plist-get art :log-file))
         (format "- Failing jobs metadata: `%s`\n" (plist-get art :jobs-file))
         (format "- CI triage index: `%s`" (plist-get art :index-file)))))
    artifacts)
   "\n\n"))

(defun agent-shell-workflow-library--group-fix-ci-finalize
    (ctx metas failing-entries healthy-entries date)
  "Populate CTX from METAS, FAILING-ENTRIES, HEALTHY-ENTRIES, and DATE.
Signals `agent-shell-workflow-skip' when no repository has a failing run."
  (when (null failing-entries)
    (agent-shell-workflow-skip
     (format "All %d %s have passing CI"
             (length healthy-entries)
             (if (= (length healthy-entries) 1) "repository" "repositories"))))
  (let* ((primary-root (or (plist-get (car metas) :root)
                           (plist-get ctx :context-dir)
                           default-directory))
         (group-plan-dir (agent-shell-workflow-library--unique-artifact-dir
                          (expand-file-name ".agent/fix-ci" primary-root)
                          "group" nil date))
         (group-plan-file (expand-file-name "fix-plan.md" group-plan-dir))
         (report (agent-shell-workflow-library--format-group-ci-report failing-entries))
         (healthy-text
          (if healthy-entries
              (string-join
               (seq-map (lambda (m)
                          (format "- %s (`%s`)"
                                  (or (plist-get m :slug) (plist-get m :root))
                                  (plist-get m :root)))
                        healthy-entries)
               "\n")
            "(none)"))
         (updated (copy-sequence ctx)))
    (setq updated (plist-put updated :ci-group-report report))
    (setq updated (plist-put updated :ci-group-healthy healthy-text))
    (setq updated (plist-put updated :ci-group-failed-count (length failing-entries)))
    (setq updated (plist-put updated :ci-group-healthy-count (length healthy-entries)))
    (setq updated (plist-put updated :ci-group-plan-file group-plan-file))
    (setq updated (plist-put updated :ci-group-artifacts failing-entries))
    updated))

(defun agent-shell-workflow-library--group-fix-ci-pre-op (ctx &optional callback)
  "Download failing CI artifacts across a repository group into CTX.
Resolves repositories from `:group' in CTX's `:args', else `:repos' in
CTX, else `:context-dir'.  Healthy repositories are skipped so zero
turns are spent on them; if all repositories are healthy, skips the
entire workflow via `agent-shell-workflow-skip'.  When CALLBACK is
non-nil, fetches all repositories in parallel asynchronously."
  (let* ((args (plist-get ctx :args))
         (group (plist-get args :group))
         (target-branch (let ((b (plist-get args :branch)))
                          (when (and (stringp b) (not (string-empty-p b))) b)))
         (date (plist-get args :date))
         (repos (cond
                 ((and group (not (equal group "")))
                  (agent-shell-workflow-resolve-repo-group
                   (if (stringp group) (intern group) group)))
                 ((plist-get ctx :repos)
                  (plist-get ctx :repos))
                 (t
                  (list (or (plist-get ctx :context-dir) default-directory)))))
         (metas (seq-map (lambda (dir)
                           (agent-shell-workflow-library--group-ci-repo-meta
                            dir target-branch))
                         repos))
         (run-fields "databaseId,displayTitle,status,conclusion,headBranch,headSha,createdAt,updatedAt,startedAt,url"))
    (if callback
        ;; Wave 1: query recent runs for every repository in parallel.
        (let ((list-pairs
               (seq-map
                (lambda (m)
                  (list (plist-get m :root)
                        "gh" "run" "list"
                        "--repo" (plist-get m :slug)
                        "--limit" "20"
                        "--json" run-fields))
                metas)))
          (agent-shell-workflow--gather-async
           list-pairs
           (lambda (list-results)
             (let (failing-metas healthy-metas detail-pairs)
               (seq-do
                (lambda (m)
                  (let* ((root (plist-get m :root))
                         (raw (or (cdr (assoc root list-results)) ""))
                         (runs (ignore-errors
                                 (json-parse-string raw
                                                    :object-type 'alist
                                                    :array-type 'list)))
                         (fail-run (and (listp runs)
                                        (agent-shell-workflow-library--find-failing-run
                                         runs (plist-get m :branch) (plist-get m :head-sha)))))
                    (if fail-run
                        (let* ((run-id-str (format "%s" (map-elt fail-run 'databaseId)))
                               (slug (plist-get m :slug))
                               (entry (plist-put (copy-sequence m) :run-id run-id-str)))
                          (push entry failing-metas)
                          (push (list (cons root :summary) "gh" "run" "view" run-id-str "--repo" slug)
                                detail-pairs)
                          (push (list (cons root :log) "gh" "run" "view" run-id-str "--repo" slug "--log-failed")
                                detail-pairs)
                          (push (list (cons root :jobs) "gh" "run" "view" run-id-str "--repo" slug
                                      "--json" "jobs,conclusion,workflowName,url,displayTitle,headBranch")
                                detail-pairs))
                      (push m healthy-metas))))
                metas)
               (setq failing-metas (nreverse failing-metas))
               (setq healthy-metas (nreverse healthy-metas))
               (if (null failing-metas)
                   (condition-case skip
                       (agent-shell-workflow-library--group-fix-ci-finalize
                        ctx metas nil healthy-metas date)
                     (agent-shell-workflow-skip
                      (message "Workflow skipped: %s" (cadr skip))))
                 ;; Wave 2: download failed logs and metadata in parallel.
                 (agent-shell-workflow--gather-async
                  (nreverse detail-pairs)
                  (lambda (detail-results)
                    (let ((artifacts
                           (seq-map
                            (lambda (m)
                              (let* ((root (plist-get m :root))
                                     (summary (or (cdr (assoc (cons root :summary) detail-results)) ""))
                                     (log (or (cdr (assoc (cons root :log) detail-results)) ""))
                                     (jobs (or (cdr (assoc (cons root :jobs) detail-results)) "")))
                                (agent-shell-workflow-library--write-ci-artifacts
                                 root (plist-get m :slug) (plist-get m :run-id)
                                 (plist-get m :branch) date summary log jobs)))
                            failing-metas)))
                      (funcall callback
                               (agent-shell-workflow-library--group-fix-ci-finalize
                                ctx metas artifacts healthy-metas date))))))))))
      ;; Synchronous path
      (let (artifacts healthy-metas)
        (seq-do
         (lambda (m)
           (let* ((slug (plist-get m :slug))
                  (runs (agent-shell-workflow-library--fetch-runs slug 20))
                  (fail-run (agent-shell-workflow-library--find-failing-run
                             runs (plist-get m :branch) (plist-get m :head-sha))))
             (if fail-run
                 (let* ((run-id-str (format "%s" (map-elt fail-run 'databaseId)))
                        (summary (agent-shell-workflow-library--shell
                                  "gh" "run" "view" run-id-str "--repo" slug))
                        (log (agent-shell-workflow-library--shell
                              "gh" "run" "view" run-id-str "--repo" slug "--log-failed"))
                        (jobs (agent-shell-workflow-library--shell
                               "gh" "run" "view" run-id-str "--repo" slug
                               "--json" "jobs,conclusion,workflowName,url,displayTitle,headBranch")))
                   (push (agent-shell-workflow-library--write-ci-artifacts
                          (plist-get m :root) slug run-id-str
                          (plist-get m :branch) date summary log jobs)
                         artifacts))
               (push m healthy-metas))))
         metas)
        (agent-shell-workflow-library--group-fix-ci-finalize
         ctx metas (nreverse artifacts) (nreverse healthy-metas) date)))))

(agent-shell-workflow-define group-fix-ci
  :doc "Download failing CI artifacts across a repository group and fix in one session"
  :category "CI/CD"
  :args ((group :prompt "Repository group (optional): " :type symbol :optional t)
         (branch :prompt "Branch (optional): " :optional t))
  :pre-op #'agent-shell-workflow-library--group-fix-ci-pre-op
  :template "Investigate and fix the failing CI runs across this repository group ({{ci-group-failed-count}} failing, {{ci-group-healthy-count}} passing).

## Failing Repositories & Pre-Downloaded Artifacts:
{{ci-group-report}}

## Passing Repositories (skipped):
{{ci-group-healthy}}

Do NOT read entire log files into context and do NOT re-run `gh run list` or `gh run view` — artifacts are already downloaded above. Inspect all failing logs in parallel using search or tail inspection (failures typically appear in the last 150-200 lines).

## Instructions:
1. Inspect all failing logs in parallel (search `Error:`, `FAILED`, `FAIL`, panics, or lint failures) and note any shared root causes across repositories.
2. Write a unified fix plan to `{{ci-group-plan-file}}` (per-repo root cause, action items, verification).
3. Present the fix plan to the user and ask confirmation before modifying source files.
4. Implement targeted fixes across the failing repositories and verify each with narrow tests or `elisp_validate`."
  :submit t
  :target :session-reuse
  :buffer-display :background
  :clear t)

;;;###autoload
(defun agent-shell-workflow-fix-ci-group (&optional group branch)
  "Dispatch `group-fix-ci' across GROUP in a single session.
When GROUP is nil, prompt from `agent-shell-workflow-repo-groups'.
Optional BRANCH overrides per-repository branch detection."
  (interactive)
  (unless (or group agent-shell-workflow-repo-groups)
    (user-error "No repository groups configured in `agent-shell-workflow-repo-groups'"))
  (let* ((chosen-group
          (or group
              (let* ((table (seq-map
                             (lambda (entry)
                               (let* ((grp-name (car entry))
                                      (grp-repos (cdr entry))
                                      (count (length grp-repos)))
                                 (cons (symbol-name grp-name)
                                       (cons (format "[%d %s: %s]"
                                                     count
                                                     (if (= count 1) "repo" "repos")
                                                     (string-join
                                                      (seq-map #'file-name-nondirectory
                                                               (seq-take grp-repos 3))
                                                      ", "))
                                             grp-name))))
                             agent-shell-workflow-repo-groups)))
                (if (fboundp 'annotated-completing-read)
                    (annotated-completing-read
                     table
                     :prompt "Select repository group for fix-ci: "
                     :category 'agent-shell-repo-group
                     :require-match t
                     :history 'agent-shell-workflow-repo-group-history)
                  (intern (completing-read "Select repository group for fix-ci: "
                                           (mapcar #'car table) nil t))))))
         (group-sym (if (stringp chosen-group) (intern chosen-group) chosen-group))
         (args (when (and (stringp branch) (not (string-empty-p branch)))
                 (list :branch branch))))
    (agent-shell-workflow-dispatch-group
     'group-fix-ci group-sym :together t :args args)))

;; PR review comment remediation

(defun agent-shell-workflow-library--pr-review-pre-op (ctx &optional callback)
  "Fetch PR review comments for :pr-number in CTX and save them locally.
Markdown summary and JSON export are saved under a unique directory
under <project-root>/.agent/pr-comments/ so the agent can inspect them
as files without colliding across PRs or review attempts.
When CALLBACK is given, executes asynchronously."
  (let* ((args (plist-get ctx :args))
         (root (if-let* ((r (plist-get args :root)))
                   (file-name-as-directory (expand-file-name r))
                 (agent-shell-workflow-library--project-root)))
         (default-directory (or root default-directory))
         (raw-repo (or (plist-get args :repo)
                       (ignore-errors
                         (and (fboundp 'magit-dash--repo-at-point)
                              (when-let* ((r (magit-dash--repo-at-point)))
                                (or (and (fboundp 'magit-dash-ci--repo-slug)
                                         (magit-dash-ci--repo-slug r))
                                    (and (fboundp 'magit-dash-repo-repo)
                                         (magit-dash-repo-repo r))
                                    (and (fboundp 'magit-dash-repo-clone-url)
                                         (magit-dash-repo-clone-url r))
                                    (magit-dash-repo-name r)))))
                       (agent-shell-workflow-library--resolve-repo-slug)))
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
                   (head-ref (map-elt view-obj "headRefName"))
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
                          (setf (map-elt table "pr") (or pr-number (string-to-number pr-str)))
                          (when repo-slug (setf (map-elt table "repo") repo-slug))
                          (when-let* ((t-val (map-elt view-obj "title"))) (setf (map-elt table "title") t-val))
                          (when-let* ((u-val (map-elt view-obj "url"))) (setf (map-elt table "url") u-val))
                          (setf (map-elt table "reviews") (or (map-elt view-obj "reviews") []))
                          (setf (map-elt table "comments") (or (map-elt view-obj "comments") []))
                          (setf (map-elt table "inline_comments") (or api-arr []))
                          (or (ignore-errors (json-serialize table)) "{}"))
                      (if (and view-json-raw (not (string-empty-p view-json-raw)))
                          view-json-raw
                        "{}")))
                   ;; Summary string
                   (pr-summary
                    (if (hash-table-p view-obj)
                        (let* ((title (or (map-elt view-obj "title") ""))
                               (author-obj (map-elt view-obj "author"))
                               (author (if (or (hash-table-p author-obj) (consp author-obj)) (map-elt author-obj "login") (format "%s" (or author-obj ""))))
                               (reviews (map-elt view-obj "reviews"))
                               (comments (map-elt view-obj "comments"))
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

(agent-shell-workflow-define pr-review-patch
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

;; Diff-review remediation and autonomous draft critique workflows

(declare-function magit-dash-review-extract-findings "magit-dash-review" (review-file &optional unresolved-only))
(declare-function magit-dash-review--current-review-file "magit-dash-review" ())
(declare-function diff-review-refresh "diff-review" ())

(defun agent-shell-workflow-library--resolve-review-file (&optional review-file root)
  "Resolve REVIEW-FILE path from explicit arg, current diff-review buffer, or ROOT."
  (cond
   ((and (stringp review-file) (not (string-empty-p review-file)))
    (expand-file-name review-file))
   ((and (fboundp 'magit-dash-review--current-review-file)
         (magit-dash-review--current-review-file))
    (expand-file-name (magit-dash-review--current-review-file)))
   ((and (derived-mode-p 'diff-review-mode) buffer-file-name)
    (expand-file-name buffer-file-name))
   (t
    (let* ((dir (or root (agent-shell-workflow-library--project-root) default-directory))
           (git-common (string-trim
                        (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "git" nil t nil "-C" dir "rev-parse" "--git-common-dir")))))
           (review-dir (expand-file-name "review" (expand-file-name git-common dir)))
           (files (and (file-directory-p review-dir)
                       (directory-files review-dir t "\\.json\\'"))))
      (cond
       ((null files)
        (user-error "No diff-review JSON files found in %s" review-dir))
       ((= (length files) 1)
        (car files))
       (t
        (let* ((names (mapcar (lambda (f) (cons (file-name-nondirectory f) f)) files))
               (choice (completing-read "Select review file: " (mapcar #'car names) nil t)))
          (cdr (assoc choice names)))))))))

(defun agent-shell-workflow-library--extract-diff-review-findings (review-file)
  "Extract unresolved non-draft findings from REVIEW-FILE as a list of plists."
  (if (fboundp 'magit-dash-review-extract-findings)
      (magit-dash-review-extract-findings review-file t)
    (with-temp-buffer
      (insert-file-contents review-file)
      (let* ((data (json-parse-string (buffer-string)
                                      :object-type 'hash-table
                                      :array-type 'array
                                      :null-object nil
                                      :false-object :false))
             (root (map-elt data "root"))
             (base (map-elt data "base"))
             (head (map-elt data "head"))
             (md (map-elt data "magit_dash"))
             (worktree (or (and (hash-table-p md) (map-elt md "worktree")) root))
             (comments (append (or (map-elt data "comments") []) nil))
             (unresolved (seq-filter
                          (lambda (c)
                            (and (not (eq (map-elt c "resolved") t))
                                 (not (equal (map-elt c "status") "draft"))))
                          comments)))
        (mapcar
         (lambda (c)
           (let* ((lines (map-elt c "lines"))
                  (line (and (vectorp lines) (>= (length lines) 1) (aref lines 0)))
                  (end-line (and (vectorp lines) (>= (length lines) 2) (aref lines 1)))
                  (path (map-elt c "path"))
                  (hunk (if (and worktree (file-directory-p worktree) path)
                            (let ((default-directory worktree))
                              (string-trim
                               (with-output-to-string
                                 (with-current-buffer standard-output
                                   (if (and base head (not (string-empty-p head)))
                                       (call-process "git" nil t nil "diff" "-U5"
                                                     (format "%s...%s" base head) "--" path)
                                     (call-process "git" nil t nil "diff" "-U5"
                                                   (or base "HEAD") "--" path))))))
                          "")))
             (list :id (map-elt c "id")
                   :path path
                   :lines lines
                   :line line
                   :end-line end-line
                   :author (map-elt c "author")
                   :body (map-elt c "body")
                   :hunk hunk
                   :worktree (or worktree root)
                   :root root
                   :base base
                   :head head
                   :review-file (expand-file-name review-file))))
         unresolved)))))

(defun agent-shell-workflow-library--format-diff-review-findings-md (review-file findings)
  "Render FINDINGS from REVIEW-FILE into a structured Markdown document."
  (let* ((first-f (car findings))
         (worktree (or (plist-get first-f :worktree) default-directory))
         (base (or (plist-get first-f :base) "HEAD"))
         (head (or (plist-get first-f :head) "(working tree)"))
         (idx 0))
    (concat
     (format "# Diff Review Findings\n\n- **Review File**: `%s`\n- **Worktree**: `%s`\n- **Range**: `%s` -> `%s`\n- **Unresolved Findings**: %d\n\n"
             review-file worktree base head (length findings))
     (mapconcat
      (lambda (f)
        (setq idx (1+ idx))
        (let* ((id (or (plist-get f :id) "unknown"))
               (path (or (plist-get f :path) "(review-wide)"))
               (line (plist-get f :line))
               (end-line (plist-get f :end-line))
               (loc (cond
                     ((and line end-line (/= line end-line))
                      (format "%s:%d-%d" path line end-line))
                     (line (format "%s:%d" path line))
                     (t path)))
               (author (or (plist-get f :author) "reviewer"))
               (body (or (plist-get f :body) ""))
               (hunk (or (plist-get f :hunk) "")))
          (concat
           (format "## %d. `%s` (id: `%s`, by @%s)\n\n%s\n"
                   idx loc id author body)
           (if (not (string-empty-p hunk))
               (format "\n### Diff Context\n\n```diff\n%s\n```\n" hunk)
             ""))))
      findings
      "\n"))))

(defun agent-shell-workflow-library--diff-review-remediate-pre-op (ctx &optional callback)
  "Extract unresolved diff-review findings and write Markdown artifact for CTX.
CALLBACK, when non-nil, receives the updated context instead of returning it."
  (let* ((args (plist-get ctx :args))
         (review-file (agent-shell-workflow-library--resolve-review-file
                       (plist-get args :review-file)
                       (plist-get args :root)))
         (findings (or (plist-get args :findings)
                       (agent-shell-workflow-library--extract-diff-review-findings review-file))))
    (unless findings
      (user-error "No unresolved review comments to send"))
    (let* ((first-f (car findings))
           (worktree (file-name-as-directory
                      (expand-file-name
                       (or (plist-get first-f :worktree)
                           (plist-get first-f :root)
                           (agent-shell-workflow-library--project-root)
                           default-directory))))
           (target-slug (agent-shell-workflow-library--sanitize-name
                         (file-name-sans-extension (file-name-nondirectory review-file))))
           (artifact-dir (expand-file-name
                          (format ".agent/diff-review/%s"
                                  (if (string-empty-p target-slug) "review" target-slug))
                          worktree))
           (findings-file (expand-file-name "findings.md" artifact-dir))
           (rel-findings-file (file-relative-name findings-file worktree))
           (md-content (agent-shell-workflow-library--format-diff-review-findings-md
                        review-file findings))
           (updated-args (plist-put (copy-sequence args) :review-file review-file))
           (updated-ctx (plist-put (copy-sequence ctx) :args updated-args)))
      (agent-shell-workflow-library--write-file findings-file md-content)
      (setq updated-ctx (plist-put updated-ctx :review-json-file review-file))
      (setq updated-ctx (plist-put updated-ctx :findings-file rel-findings-file))
      (setq updated-ctx (plist-put updated-ctx :findings-count (length findings)))
      (setq updated-ctx (plist-put updated-ctx :worktree worktree))
      (setq updated-ctx (plist-put updated-ctx :base (or (plist-get first-f :base) "HEAD")))
      (setq updated-ctx (plist-put updated-ctx :head (or (plist-get first-f :head) "WORKTREE")))
      (if callback
          (funcall callback updated-ctx)
        updated-ctx))))

(defun agent-shell-workflow-library--diff-review-remediate-post-op (ctx)
  "Refresh any open diff-review buffer visiting `:review-json-file' in CTX."
  (when-let* ((rf (plist-get ctx :review-json-file))
              (buf (find-buffer-visiting rf)))
    (with-current-buffer buf
      (if (fboundp 'diff-review-refresh)
          (diff-review-refresh)
        (revert-buffer t t t))))
  ctx)

(agent-shell-workflow-define diff-review-remediate
  :doc "Send unresolved diff-review findings and hunks to agent-shell for remediation"
  :category "Code Review"
  :args ((review-file :prompt "Review JSON file: " :optional t))
  :pre-op #'agent-shell-workflow-library--diff-review-remediate-pre-op
  :post-op #'agent-shell-workflow-library--diff-review-remediate-post-op
  :template "Remediate the {{findings-count}} unresolved code review finding(s) in `{{worktree}}`.

## Review Artifacts:
- Findings & diff hunks: `{{findings-file}}`
- Live review JSON store: `{{review-json-file}}`

## Instructions:
1. Read `{{findings-file}}` and inspect each unresolved finding and its diff context.
2. Apply the necessary code fixes and/or regression tests directly in `{{worktree}}`.
3. Run the relevant unit tests to verify every fix passes cleanly.
4. For each addressed finding (`<comment-id>`), append a concise reply to `{{review-json-file}}` via:
   `emacsclient --eval '(magit-dash-review-append-reply \"{{review-json-file}}\" \"<comment-id>\" \"agent-shell\" \"<summary of fix>\")'`"
  :submit t
  :target :session-reuse)

(defun agent-shell-workflow-library--review-draft-pass-pre-op (ctx &optional callback)
  "Prepare diff patch artifact for autonomous `review-draft-pass' in CTX.
CALLBACK, when non-nil, receives the updated context instead of returning it."
  (let* ((args (plist-get ctx :args))
         (review-file (agent-shell-workflow-library--resolve-review-file
                       (plist-get args :review-file)
                       (plist-get args :root)))
         (data (with-temp-buffer
                 (insert-file-contents review-file)
                 (json-parse-string (buffer-string)
                                    :object-type 'hash-table
                                    :array-type 'array
                                    :null-object nil
                                    :false-object :false)))
         (root (map-elt data "root"))
         (base (or (map-elt data "base") "HEAD"))
         (head (map-elt data "head"))
         (md (map-elt data "magit_dash"))
         (worktree (file-name-as-directory
                    (expand-file-name
                     (or (and (hash-table-p md) (map-elt md "worktree"))
                         root
                         (agent-shell-workflow-library--project-root)
                         default-directory))))
         (target-slug (agent-shell-workflow-library--sanitize-name
                       (file-name-sans-extension (file-name-nondirectory review-file))))
         (artifact-dir (expand-file-name
                        (format ".agent/diff-review/%s"
                                (if (string-empty-p target-slug) "review" target-slug))
                        worktree))
         (diff-file (expand-file-name "review-diff.patch" artifact-dir))
         (rel-diff-file (file-relative-name diff-file worktree))
         (diff-text (if (file-directory-p worktree)
                        (let ((default-directory worktree))
                          (with-output-to-string
                            (with-current-buffer standard-output
                              (if (and head (not (string-empty-p head)))
                                  (call-process "git" nil t nil "diff" "-U5"
                                                (format "%s...%s" base head))
                                (call-process "git" nil t nil "diff" "-U5" base)))))
                      ""))
         (focus (or (plist-get args :focus) "correctness, edge cases, and test coverage"))
         (updated-args (plist-put (copy-sequence args) :review-file review-file))
         (updated-ctx (plist-put (copy-sequence ctx) :args updated-args)))
    (agent-shell-workflow-library--write-file diff-file diff-text)
    (setq updated-ctx (plist-put updated-ctx :review-json-file review-file))
    (setq updated-ctx (plist-put updated-ctx :diff-file rel-diff-file))
    (setq updated-ctx (plist-put updated-ctx :worktree worktree))
    (setq updated-ctx (plist-put updated-ctx :base base))
    (setq updated-ctx (plist-put updated-ctx :head (or head "WORKTREE")))
    (setq updated-ctx (plist-put updated-ctx :focus focus))
    (if callback
        (funcall callback updated-ctx)
      updated-ctx)))

(agent-shell-workflow-define review-draft-pass
  :doc "Run an autonomous agent code critique pass and stage draft findings"
  :category "Code Review"
  :args ((review-file :prompt "Review JSON file: " :optional t)
         (focus :prompt "Review focus: " :optional t))
  :pre-op #'agent-shell-workflow-library--review-draft-pass-pre-op
  :post-op #'agent-shell-workflow-library--diff-review-remediate-post-op
  :template "Perform an adversarial code review pass on `{{worktree}}` ({{base}} -> {{head}}).

## Review Context:
- Unified diff artifact: `{{diff-file}}`
- Target review JSON store: `{{review-json-file}}`
- Focus areas: {{focus}}

## Instructions:
1. Inspect the changes in `{{diff-file}}` and read surrounding source files in `{{worktree}}`.
2. Identify high-signal defects, concurrency/state hazards, API contract violations, or missing test coverage.
3. Stage your findings into a `code-review` `hitl-plan` (with operation `draft-finding` or `record-finding` targeting `:review-file \"{{review-json-file}}\"`) so the human reviewer can triage them in `hitl-plan-ui` and `diff-review`."
  :submit t
  :target :session-reuse)

(provide 'agent-shell-workflow-github)

;;; agent-shell-workflow-github.el ends here
