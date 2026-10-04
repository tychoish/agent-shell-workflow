;;; agent-shell-workflow-library.el --- Built-in agent-shell-workflow workflows -*- lexical-binding: t -*-

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

;; Base prompt recipes and shared shell/git utilities for `agent-shell-workflow`.
;; Includes test coverage expansion, refactoring cleanups, and git commit authoring.
;; Also acts as an umbrella loader for domain workflows in `agent-shell-workflow-github'
;; and `agent-shell-workflow-pipeline'.

;;; Code:

(require 'seq)
(require 'agent-shell-workflow)
(eval-when-compile (require 'agent-shell-workflow))

(declare-function magit-git-output "magit-git" (&rest args))

;; Shared shell and git utilities
(defun agent-shell-workflow-library--shell (&rest args)
  "Run ARGS as a shell command in `default-directory` and return its output.
Trailing newline is trimmed.  Errors are captured inline in the result
rather than signaled, so a pre-op can surface tool failures to the agent
instead of aborting the workflow."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (apply #'call-process (car args) nil t nil (cdr args))))))

(defun agent-shell-workflow-library--git-output (&rest args)
  "Run git with ARGS in `default-directory` and return trimmed output.
Uses `magit-git-output` when available, falling back to
`agent-shell-workflow-library--shell`."
  (if (fboundp 'magit-git-output)
      (string-trim (or (apply #'magit-git-output args) ""))
    (apply #'agent-shell-workflow-library--shell "git" args)))

(defun agent-shell-workflow-library--diff-summary (args &optional max-lines)
  "Return git diff for ARGS, truncating to `--stat` if lines exceed MAX-LINES.
MAX-LINES defaults to 5.  When diff has <= MAX-LINES lines, returns full diff.
When diff exceeds MAX-LINES lines, returns `git diff --stat` output with
a note."
  (let* ((limit (or max-lines 5))
         (full-diff (apply #'agent-shell-workflow-library--git-output (append '("diff") args)))
         (trimmed (string-trim full-diff)))
    (if (string-empty-p trimmed)
        "(no changes)"
      (let ((lines (split-string trimmed "\n" t)))
        (if (<= (length lines) limit)
            trimmed
          (let* ((stat (apply #'agent-shell-workflow-library--git-output (append '("diff" "--stat") args)))
                 (trimmed-stat (string-trim (or stat ""))))
            (if (not (string-empty-p trimmed-stat))
                (format "%s\n(Diff exceeds %d lines — run `git diff%s` to view full diff)"
                        trimmed-stat limit
                        (if args (concat " " (mapconcat #'identity args " ")) ""))
              (format "%s\n...\n(Truncated — run `git diff` to view full diff)"
                      (mapconcat #'identity (seq-take lines limit) "\n")))))))))

(defun agent-shell-workflow-library--gather (ctx pairs)
  "Populate CTX with the output of each shell command in PAIRS.
PAIRS is a list of (CTX-KEY COMMAND ARG...) entries; each COMMAND is run
via `agent-shell-workflow-library--shell` and stored under CTX-KEY."
  (dolist (pair pairs ctx)
    (plist-put ctx (car pair) (apply #'agent-shell-workflow-library--shell (cdr pair)))))

;; Local filesystem helpers for prompt artifacts
(defun agent-shell-workflow-library--project-root ()
  "Return the root directory of the current project or repository."
  (file-name-as-directory
   (expand-file-name
    (or (ignore-errors (vc-root-dir))
        (ignore-errors (locate-dominating-file default-directory ".git"))
        default-directory))))

(defun agent-shell-workflow-library--write-file (file-path content)
  "Write CONTENT string to FILE-PATH, creating parent directories as needed."
  (let ((dir (file-name-directory file-path)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file file-path
    (insert (or content ""))))

;; Test coverage expansion

(defun agent-shell-workflow-library--coverage-pre-op (ctx)
  "Diff :file in CTX against HEAD to scope untested edits."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file))
         (diff (agent-shell-workflow-library--diff-summary (list "HEAD" "--" file) 5)))
    (plist-put (copy-sequence ctx) :file-diff diff)))

(register-agent-shell-workflow expand-coverage
  :doc "Analyze uncovered lines and author missing unit tests"
  :category "Testing"
  :args ((file :prompt "File: "))
  :pre-op #'agent-shell-workflow-library--coverage-pre-op
  :template "Review {{args.file}} for untested logic and author missing unit tests.

Diff:
{{file-diff}}"
  :submit t
  :target :session-reuse)

;; Refactor / dead-code cleanup

(defun agent-shell-workflow-library--refactor-pre-op (ctx)
  "Gather git log summary for :file in CTX to scope stale/legacy code."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file))
         (log (agent-shell-workflow-library--git-output "log" "--oneline" "-n" "5" "--" file)))
    (plist-put (copy-sequence ctx) :recent-history (if (string-empty-p log) "(no history)" log))))

(register-agent-shell-workflow refactor-module
  :doc "Clean up dead code and migrate legacy macro forms"
  :category "Refactoring"
  :args ((file :prompt "File: "))
  :pre-op #'agent-shell-workflow-library--refactor-pre-op
  :template "Refactor {{args.file}}: remove dead code and migrate legacy forms to current conventions.

Recent history:
{{recent-history}}"
  :submit t
  :target :session-reuse)

;; Git commit authoring

(defun agent-shell-workflow-library--create-commit-pre-op (ctx)
  "Gather git status, diff against HEAD, and recent log history for CTX.
Uses Magit or Git directly to gather state."
  (let* ((args (plist-get ctx :args))
         (files (plist-get args :files))
         (has-files (and (stringp files) (not (string-empty-p files))))
         (file-args (when has-files (list "--" files)))
         (status (apply #'agent-shell-workflow-library--git-output
                        (append '("status" "--short") file-args)))
         (diff (agent-shell-workflow-library--diff-summary
                (append '("HEAD") file-args) 5))
         (log (agent-shell-workflow-library--git-output "log" "--oneline" "-n" "5"))
         (updated-ctx (copy-sequence ctx)))
    (setq updated-ctx (plist-put updated-ctx :git-status (if (string-empty-p status) "(clean)" status)))
    (setq updated-ctx (plist-put updated-ctx :git-diff diff))
    (setq updated-ctx (plist-put updated-ctx :recent-log (if (string-empty-p log) "(no history)" log)))
    updated-ctx))

(register-agent-shell-workflow create-commit
  :doc "Draft and create a git commit with concise message and attribution"
  :category "Git"
  :args ((files :prompt "Files to commit (optional): " :optional t)
         (instructions :prompt "Additional instructions (optional): " :optional t))
  :pre-op #'agent-shell-workflow-library--create-commit-pre-op
  :template "Create a git commit for the current changes:

## Working Tree Status:
{{git-status}}

## Current Diff:
{{git-diff}}

## Recent Commits (for style reference):
{{recent-log}}

{{args.instructions}}

## Guidelines:
- Subject: Imperative, concise sentence matching repository conventions.
- Body: 2-3 sentences explaining motivation/impact (omit if self-explanatory).
- Attribution: Include `Co-authored-by:` trailer identifying the AI assistant.
- Stage appropriate changes and commit."
  :submit t
  :target :session-reuse)

(provide 'agent-shell-workflow-library)

;; Domain workflow modules
(require 'agent-shell-workflow-github)
(require 'agent-shell-workflow-pipeline)

;;; agent-shell-workflow-library.el ends here
