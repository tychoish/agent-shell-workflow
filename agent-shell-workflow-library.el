;;; agent-shell-workflow-library.el --- Built-in agent-shell-workflow workflows -*- lexical-binding: t -*-

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

;; Base prompt recipes for `agent-shell-workflow`.
;; Includes test coverage expansion, refactoring cleanups, and git commit authoring.
;; Also acts as an umbrella loader for domain workflows in `agent-shell-workflow-github'
;; and `agent-shell-workflow-pipeline'.

;;; Code:

(require 'agent-shell-workflow-utils)
(require 'agent-shell-workflow)
(require 'org)
(require 'org-id)
(eval-when-compile
  (require 'agent-shell-workflow))

(declare-function agent-shell-workflow-resolve-repo-group "agent-shell-workflow")
(declare-function org-back-to-heading "org")
(declare-function org-get-heading "org")
(declare-function org-get-todo-state "org")
(declare-function org-get-tags "org")
(declare-function org-end-of-subtree "org")
(declare-function org-todo "org")
(declare-function org-get-at-bol "org-macs")
(defvar org-todo-keywords-1)

;; Domain workflow modules
(require 'agent-shell-workflow-github)
(require 'agent-shell-workflow-pipeline)

;; Test coverage expansion

(defun agent-shell-workflow-library--coverage-pre-op (ctx &optional callback)
  "Diff :file in CTX against HEAD to scope untested edits.
CALLBACK, when non-nil, receives the updated context instead of returning it."
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

(agent-shell-workflow-define expand-coverage
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
  "Gather git log summary for :file in CTX to scope stale/legacy code.
CALLBACK, when non-nil, receives the updated context instead of returning it."
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

(agent-shell-workflow-define refactor-module
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
  "Add Git status, diff against HEAD, and recent commits to a copy of CTX.
Signal `user-error' when there are no changes to commit.
With CALLBACK, gather asynchronously and pass it the updated context.
Otherwise, return the updated context."
  (let* ((finish
          (lambda (status diff log)
            (when (and (or (null status) (string-empty-p (string-trim status)) (equal status "(clean)"))
                       (or (null diff) (string-empty-p (string-trim diff)) (equal diff "(no diff)")))
              (user-error "No diff to commit: working tree is clean"))
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

(agent-shell-workflow-define create-commit
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
  :target :session-reuse
  :variants ((series
              :id create-commit-series
              :doc "Organize uncommitted changes into a logical series of conventional commits"
              :template-replace
              (("Create one git commit containing all uncommitted changes:"
                . "Create a logical series of commits (or a single commit if changes are cohesive; inspect the changes and decide if a series or single commit is appropriate) containing all uncommitted changes:")))))

;; Org task implementation

(defun agent-shell-workflow-library--extract-org-task ()
  "Return metadata for the Org heading at point or its agenda entry.
The plist contains :title, :heading, :file, :todo, :tags, and :body strings,
plus :id and absolute :line and :end-line numbers for the source subtree.
Create a stable Org ID when writable.  Save changed file-backed tasks so
agents can resolve the reference from disk; unsaved buffers retain the ID
in memory.  Read-only headings without IDs signal `user-error'.
Use the buffer name when no file is visited and placeholder text for an
empty body.  The body includes child headings.
When the heading has a TODO keyword, try to set it to INPROGRESS if that
state is defined in `org-todo-keywords-1'.  Preserve the original TODO
state but extract source locations and body after transition/save hooks.
Signal `user-error' outside Org or when an agenda entry has no live marker."
  (let ((marker nil)
        (buf (current-buffer)))
    (cond
     ((derived-mode-p 'org-agenda-mode)
      (setq marker (or (org-get-at-bol 'org-marker)
                       (org-get-at-bol 'org-hd-marker)))
      (unless (and marker (markerp marker) (marker-buffer marker))
        (user-error "No Org task found at point in agenda")))
     ((derived-mode-p 'org-mode)
      nil)
     (t
      (user-error "Current buffer is not an Org or Org-agenda buffer")))
    (let ((target-buf (if marker (marker-buffer marker) buf))
          (target-pos (if marker (marker-position marker) (point))))
      (with-current-buffer target-buf
        (save-excursion
          (save-restriction
            (widen)
            (goto-char target-pos)
            (org-back-to-heading t)
            (unless (or (org-entry-get nil "ID") (not buffer-read-only))
              (user-error "Task needs an Org ID; make the source buffer writable"))
            (let* ((heading-marker (copy-marker (point) t))
                   (todo-state (org-get-todo-state))
                   (identifier (or (org-entry-get nil "ID")
                                   (if buffer-file-name
                                       (org-id-get-create)
                                     (org-entry-put nil "ID" (org-id-new))
                                     (org-entry-get nil "ID")))))
              (when (and (not buffer-read-only) todo-state
                         (boundp 'org-todo-keywords-1)
                         (member "INPROGRESS" org-todo-keywords-1))
                (ignore-errors (org-todo "INPROGRESS")))
              (when (and buffer-file-name (buffer-modified-p) (not buffer-read-only))
                (save-buffer))
              (goto-char heading-marker)
              (org-back-to-heading t)
              (let* ((heading-end (line-end-position))
                     (subtree-end (save-excursion (org-end-of-subtree t t) (point)))
                     (body (string-trim
                            (buffer-substring-no-properties
                             (min (1+ heading-end) subtree-end) subtree-end))))
                (list :title (org-get-heading t t t t)
                      :heading (string-trim
                                (buffer-substring-no-properties
                                 (line-beginning-position) heading-end))
                      :file (or (buffer-file-name) (buffer-name))
                      :id identifier
                      :line (line-number-at-pos (point) t)
                      :end-line (line-number-at-pos
                                 (max heading-end (1- subtree-end)) t)
                      :todo (or todo-state "")
                      :tags (string-join (org-get-tags) " :")
                      :body (if (string-empty-p body) "(no body content)" body))))))))))

(defun agent-shell-workflow-library--org-task-pre-op (ctx &optional callback)
  "Add the current Org task's template values to a copy of CTX.
Populate :task-title, :task-heading, :task-file, :task-todo, :task-tags,
:task-id, :task-line, :task-end-line, and :task-body.  When CTX includes
:source-marker, extract from that location even after shell selection.
Pass the updated context to CALLBACK when non-nil; otherwise return it."
  (let* ((source (plist-get ctx :source-marker))
         (task (if source
                   (progn
                     (unless (and (markerp source) (marker-buffer source))
                       (user-error "Org task source buffer is no longer available"))
                     (with-current-buffer (marker-buffer source)
                       (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char source)
                           (agent-shell-workflow-library--extract-org-task)))))
                 (agent-shell-workflow-library--extract-org-task)))
         (updated (copy-sequence ctx)))
    (setq updated (plist-put updated :task-title (plist-get task :title)))
    (setq updated (plist-put updated :task-heading (plist-get task :heading)))
    (setq updated (plist-put updated :task-file (plist-get task :file)))
    (setq updated (plist-put updated :task-id (plist-get task :id)))
    (setq updated (plist-put updated :task-line (plist-get task :line)))
    (setq updated (plist-put updated :task-end-line (plist-get task :end-line)))
    (setq updated (plist-put updated :task-todo (plist-get task :todo)))
    (setq updated (plist-put updated :task-tags (plist-get task :tags)))
    (setq updated (plist-put updated :task-body (plist-get task :body)))
    (if callback
        (funcall callback updated)
      updated)))

(agent-shell-workflow-define implement-org-task
  :doc "Implement the Org-mode task at point, reporting progress and results"
  :category "Org"
  :args ((instructions :prompt "Additional instructions (optional): " :optional t))
  :pre-op #'agent-shell-workflow-library--org-task-pre-op
  :template "Implement the following task from {{task-file}}:{{task-line}}-{{task-end-line}}.
Stable Org reference: id:{{task-id}} (resolve this ID if line numbers change).

{{task-heading}}

{{task-body}}

{{args.instructions}}

Instructions:
1. Implement the requested changes and address all requirements outlined in this task.
2. Provide progress updates as you proceed with each step. Record progress,
   decisions, blockers, and verification under the original heading identified
   by id:{{task-id}} using native Org tools. Save the source file after updates.
   If the source is an unsaved buffer, report that limitation explicitly.
3. Delegate independent work to sub-agents when available and useful; give each
   a clear scope and integrate and verify their results.
4. Upon completion, summarize changes and verification results in the original
   heading and mark it done only after all requirements are met."
  :submit t
  :target :ask)

;;;###autoload
(defun agent-shell-workflow-dispatch-org-task (&optional target instructions)
  "Dispatch the Org heading at point or its agenda entry for implementation.
TARGET selects the destination; nil or :ask prompts for one.  See
`agent-shell-workflow-dispatch' for supported targets.
Append nonempty INSTRUCTIONS to the task prompt.  Interactively, use a
prefix argument to enter additional instructions."
  (interactive
   (list nil (when current-prefix-arg
               (read-string "Additional instructions (optional): "))))
  (let ((args (when (and instructions (not (string-empty-p instructions)))
                (list :instructions instructions))))
    (agent-shell-workflow-dispatch
     'implement-org-task
     :target (or target :ask)
     :args args)))

;; Multi-repository group workflows

(defun agent-shell-workflow-library--group-status-pre-op (ctx &optional callback)
  "Add a Git status and branch report to a copy of CTX as :group-report.
Use the :group argument in CTX's :args when provided; otherwise use :repos
from CTX.  In per-repository dispatch, :repos contains only that repository.
Pass the updated context to CALLBACK when non-nil; otherwise return it."
  (let* ((group (plist-get (plist-get ctx :args) :group))
         (repos (if group
                    (agent-shell-workflow-resolve-repo-group
                     (if (stringp group) (intern group) group))
                  (plist-get ctx :repos)))
         (reports nil))
    (dolist (dir repos)
      (let* ((dname (file-name-nondirectory (directory-file-name dir)))
             (status (let ((default-directory dir))
                       (or (ignore-errors
                             (agent-shell-workflow-library--git-output "status" "--short"))
                           "(error reading git status)")))
             (branch (let ((default-directory dir))
                       (or (ignore-errors
                             (agent-shell-workflow-library--git-output "rev-parse" "--abbrev-ref" "HEAD"))
                           "unknown")))
             (clean (or (string-empty-p (string-trim status))
                        (equal (string-trim status) "(clean)"))))
        (push (format "### Repository: %s (%s) [%s]\n%s"
                      dname dir (string-trim branch)
                      (if clean "  (clean working tree)" status))
              reports)))
    (let ((report (string-join (nreverse reports) "\n\n")))
      (if callback
          (funcall callback (plist-put (copy-sequence ctx) :group-report report))
        (plist-put (copy-sequence ctx) :group-report report)))))

(agent-shell-workflow-define group-git-status
  :doc "Summarize Git working tree status across repositories"
  :category "Repository Group"
  :args ((group :prompt "Repository group: " :type symbol :optional t))
  :pre-op #'agent-shell-workflow-library--group-status-pre-op
  :template "Review Git working tree status:

{{group-report}}

Identify repositories with uncommitted changes, untracked files, or detached HEAD states."
  :submit t
  :target :session-reuse)

(agent-shell-workflow-define group-exec
  :doc "Run an instruction once per repository in a group (iterate mode)"
  :category "Repository Group"
  :args ((instruction :prompt "Instruction: "))
  :template "Coordinated task, applied separately to each repository in a group.
This session is for the repository at {{context-dir}}.

Instruction:
{{args.instruction}}

Review the current workspace and apply necessary changes for this repository only."
  :submit t
  :target :session-reuse)

(agent-shell-workflow-define group-exec-all
  :doc "Run one instruction across all repositories of a group in a single session"
  :category "Repository Group"
  :args ((instruction :prompt "Instruction: "))
  :template "Coordinated task across these repositories:

{{repos}}

Instruction:
{{args.instruction}}

The working directory is the first repository; read and change the others by
path as the task requires, and keep the changes consistent across them."
  :submit t
  :target :session-reuse)

(provide 'agent-shell-workflow-library)
;;; agent-shell-workflow-library.el ends here
