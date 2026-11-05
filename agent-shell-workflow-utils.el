;;; agent-shell-workflow-utils.el --- Shared workflow utilities -*- lexical-binding: t -*-

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

;; Shared utilities for built-in workflow modules.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'magit-git)

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
  "Run Git with ARGS using Magit and return its trimmed output."
  (string-trim (or (apply #'magit-git-output args) "")))

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
  (let ((dir (file-name-directory file-path))
        (text (or content "")))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))
    (with-temp-file file-path
      (insert text))))

(provide 'agent-shell-workflow-utils)
;;; agent-shell-workflow-utils.el ends here
