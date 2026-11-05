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

;; Base prompt recipes for `agent-shell-workflow`.
;; Includes test coverage expansion, refactoring cleanups, and git commit authoring.
;; Also acts as an umbrella loader for domain workflows in `agent-shell-workflow-github'
;; and `agent-shell-workflow-pipeline'.

;;; Code:

(require 'agent-shell-workflow-utils)
(require 'agent-shell-workflow)
(eval-when-compile
  (require 'agent-shell-workflow))

;; Domain workflow modules
(require 'agent-shell-workflow-github)
(require 'agent-shell-workflow-pipeline)

;; Test coverage expansion

(defun agent-shell-workflow-library--coverage-pre-op (ctx &optional callback)
  "Diff :file in CTX against HEAD to scope untested edits."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file)))
    (if callback
        (agent-shell-workflow--run-process-async
         "git" (list "diff" "HEAD" "--" file) nil
         (lambda (out)
           (let ((diff (if (string-empty-p out)
                           "(no diff against HEAD)"
                         out)))
             (funcall callback (plist-put (copy-sequence ctx) :file-diff diff)))))
      (let ((diff (agent-shell-workflow-library--diff-summary (list "HEAD" "--" file) 5)))
        (plist-put (copy-sequence ctx) :file-diff diff)))))

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

(defun agent-shell-workflow-library--refactor-pre-op (ctx &optional callback)
  "Gather git log summary for :file in CTX to scope stale/legacy code."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file)))
    (if callback
        (agent-shell-workflow--run-process-async
         "git" (list "log" "--oneline" "-n" "5" "--" file) nil
         (lambda (out)
           (let ((log (if (string-empty-p out) "(no history)" out)))
             (funcall callback (plist-put (copy-sequence ctx) :recent-history log)))))
      (let ((log (agent-shell-workflow-library--git-output "log" "--oneline" "-n" "5" "--" file)))
        (plist-put (copy-sequence ctx) :recent-history (if (string-empty-p log) "(no history)" log))))))

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

(defun agent-shell-workflow-library--create-commit-pre-op (ctx &optional callback)
  "Gather git status, diff against HEAD, and recent log history for CTX.
Uses Magit to gather state."
  (let* ((finish
          (lambda (status diff log)
            (let ((updated-ctx (copy-sequence ctx)))
              (setq updated-ctx (plist-put updated-ctx :git-status (if (string-empty-p status) "(clean)" status)))
              (setq updated-ctx (plist-put updated-ctx :git-diff (if (string-empty-p diff) "(no diff)" diff)))
              (setq updated-ctx (plist-put updated-ctx :recent-log (if (string-empty-p log) "(no history)" log)))
              updated-ctx))))
    (if callback
        (agent-shell-workflow--gather-async
         (list '(:status "git" "status" "--short")
               '(:diff "git" "diff" "HEAD")
               '(:log "git" "log" "--oneline" "-n" "5"))
         (lambda (results)
           (let ((status (or (cdr (assq :status results)) ""))
                 (diff (or (cdr (assq :diff results)) ""))
                 (log (or (cdr (assq :log results)) "")))
             (funcall callback (funcall finish status diff log)))))
      (let ((status (agent-shell-workflow-library--git-output "status" "--short"))
            (diff (agent-shell-workflow-library--git-output "diff" "HEAD"))
            (log (agent-shell-workflow-library--git-output "log" "--oneline" "-n" "5")))
        (funcall finish status diff log)))))

(register-agent-shell-workflow create-commit
  :doc "Commit all uncommitted changes in a single conventional commit"
  :category "Git"
  :args ((instructions :prompt "Additional instructions (optional): " :optional t))
  :pre-op #'agent-shell-workflow-library--create-commit-pre-op
  :template "Create one git commit containing all uncommitted changes:

## Working Tree Status:
{{git-status}}

## Current Diff:
{{git-diff}}

## Recent Commits (for style reference):
{{recent-log}}

{{args.instructions}}

## Guidelines:
- Treat the supplied Git status, diff, and history as authoritative; do not rerun
  `git status`, `git diff`, or `git log` to check them.
- The diff excludes untracked files; read their contents directly as needed.
- Stage all changes, including untracked and already staged changes, and commit.
- Use conventional commit style: `type(scope): imperative subject` (scope optional).
- Include a short body when the motivation is not self-explanatory.
- Include a `Co-authored-by:` trailer identifying the AI assistant."
  :submit t
  :target :session-reuse)

(register-agent-shell-workflow create-commit-series
  :doc "Organize uncommitted changes into a logical series of conventional commits"
  :category "Git"
  :args ((instructions :prompt "Additional instructions (optional): " :optional t))
  :template "Inspect the uncommitted changes and create a logical series of commits.
Group related changes, preserve existing work, and commit all changes.
Use conventional commit style: `type(scope): imperative subject` (scope optional).
Include a short body when the motivation is not self-explanatory and a
`Co-authored-by:` trailer identifying the AI assistant in each commit.

{{args.instructions}}"
  :submit t
  :target :session-reuse)

(provide 'agent-shell-workflow-library)
;;; agent-shell-workflow-library.el ends here
