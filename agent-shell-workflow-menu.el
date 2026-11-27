;;; agent-shell-workflow-menu.el --- ACR picker for agent-shell-workflow -*- lexical-binding: t -*-

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

;; The registry is elastic and often site/config-specific, so the
;; interactive surface is a single `annotated-completing-read' picker
;; (`agent-shell-workflow-select') rather than a bespoke menu per
;; workflow.  Every registered workflow, including `fix-ci',
;; appears in it.

;;; Code:

(require 'cl-lib)
(require 'annotated-completing-read)
(require 'agent-shell-workflow)
(require 'tabulated-list)
(eval-when-compile (require 'agent-shell-workflow))

(declare-function agent-shell-workflow-dispatch-group "agent-shell-workflow")
(declare-function agent-shell-workflow-dispatch-worktrees "agent-shell-workflow-workspace-actions")
(declare-function agent-shell-workflow-workspace-preview "agent-shell-workflow-workspace-actions")
(declare-function agent-shell-workflow-operation-list "agent-shell-workflow-operations")
(declare-function agent-shell-workflow-operation-read "agent-shell-workflow-operations")

(defvar-local agent-shell-workflow--workspace-status-id nil
  "Operation displayed by the current workspace status buffer.")

(defun agent-shell-workflow--workspace-status-refresh ()
  "Refresh the current workspace operation status table."
  (let* ((operation (agent-shell-workflow-operation-read
                     agent-shell-workflow--workspace-status-id))
         (targets (plist-get operation :targets)))
    (setq tabulated-list-entries
          (seq-map
           (lambda (member)
             (let* ((target-id (plist-get member :target-id))
                    (target (seq-find
                             (lambda (entry) (equal target-id (plist-get entry :id))) targets)))
               (list target-id
                     (vector (plist-get target :worktree-dir)
                             (or (plist-get target :branch) "DETACHED")
                             (plist-get member :state)
                             (format "%S" (plist-get member :details))))))
           (plist-get operation :members)))))

(define-derived-mode agent-shell-workflow-workspace-status-mode
  tabulated-list-mode "Workspace Status"
  "Show independently tracked members of a frozen workspace operation."
  (setq tabulated-list-format
        [("Checkout" 48 t) ("Branch" 22 t) ("State" 12 t) ("Details" 0 nil)])
  (add-hook 'tabulated-list-revert-hook
            #'agent-shell-workflow--workspace-status-refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun agent-shell-workflow-workspace-status (&optional operation-id)
  "Display durable per-member progress for OPERATION-ID."
  (interactive)
  (require 'agent-shell-workflow-operations)
  (let* ((selected (or operation-id
                       (completing-read
                        "Operation: "
                        (seq-map (lambda (operation) (plist-get operation :operation-id))
                                 (agent-shell-workflow-operation-list)) nil t)))
         (buffer (get-buffer-create (format "*Workspace %s*" selected))))
    (with-current-buffer buffer
      (agent-shell-workflow-workspace-status-mode)
      (setq agent-shell-workflow--workspace-status-id selected)
      (agent-shell-workflow--workspace-status-refresh)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun agent-shell-workflow-select-worktrees (&optional directories)
  "Preview exact DIRECTORIES before choosing a workspace workflow.
Interactively select one existing checkout directory; callers may supply
multiple selected worktrees, including two from the same repository."
  (interactive)
  (require 'agent-shell-workflow-workspace-actions)
  (let* ((paths (or directories (list (read-directory-name "Exact checkout: " nil nil t))))
         (preview (agent-shell-workflow-workspace-preview paths)))
    (agent-shell-workflow-workspace-status (plist-get preview :operation-id))
    (when (y-or-n-p "Dispatch a workflow to exactly these checkouts? ")
      (let ((id (agent-shell-workflow-spec-id (agent-shell-workflow-read))) args)
        (when (memq id '(commit-group repair-conflicts))
          (setq args (list :verify-command
                           (split-string-and-unquote
                            (read-string "Verification program and arguments: "))
                           :commit-message (read-string "Commit message: ")))
          (if (eq id 'commit-group)
              (setq args (plist-put args :files
                                    (split-string-and-unquote
                                     (read-string "Explicit files relative to each checkout: "))))
            (setq args (plist-put args :merge-ref (read-string "Merge reference: ")))))
        (agent-shell-workflow-dispatch-worktrees id paths :args args)))))

(defun agent-shell-workflow--candidates (&optional category)
  "Return registered workflow specs, filtered to CATEGORY when non-nil."
  (let ((specs (agent-shell-workflow-list)))
    (if category
        (seq-filter (lambda (s) (equal (agent-shell-workflow-spec-category s) category)) specs)
      specs)))

;;;###autoload
(defun agent-shell-workflow-read (&optional category prompt)
  "Read a registered workflow via `annotated-completing-read'; return its spec.
Candidates are annotated with their category and one-line doc.  With
CATEGORY non-nil, only workflows in that category are offered.  PROMPT
defaults to \"Select workflow: \".  Signal `user-error' when no workflow
is registered."
  (let* ((specs (or (agent-shell-workflow--candidates category)
                    (user-error "No workflows registered")))
         (table (seq-map
                 (lambda (spec)
                   (cons (symbol-name (agent-shell-workflow-spec-id spec))
                         (cons (format "[%s] %s"
                                       (agent-shell-workflow-spec-category spec)
                                       (or (agent-shell-workflow-spec-doc spec) ""))
                               spec)))
                 specs)))
    (annotated-completing-read
     table
     :prompt (or prompt "Select workflow: ")
     :category 'agent-shell-workflow
     :require-match t
     :history 'agent-shell-workflow-select)))

;;;###autoload
(defun agent-shell-workflow-select (&optional category arg)
  "Pick a registered workflow via `agent-shell-workflow-read' and dispatch it.
With CATEGORY non-nil, only workflows in that category are offered.
When ARG is non-nil (or invoked with `C-u'), prompt for dispatch target
via ACR (choosing any open shell, a new shell in repo, or the queue)."
  (interactive (list nil current-prefix-arg))
  (let ((id (agent-shell-workflow-spec-id (agent-shell-workflow-read category))))
    (if arg
        (agent-shell-workflow-dispatch id :target :ask)
      (agent-shell-workflow-dispatch id))))

;;;###autoload
(defun agent-shell-workflow-select-group (&optional group category)
  "Select repository GROUP and workflow, then dispatch to all repos in GROUP.
When GROUP is nil, prompt the user via `annotated-completing-read' to select
a group from `agent-shell-workflow-repo-groups'.
Then prompt for a workflow via `annotated-completing-read' (optionally
filtered by CATEGORY).
Dispatches the selected workflow to each repository in the group."
  (interactive)
  (unless agent-shell-workflow-repo-groups
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
                (annotated-completing-read
                 table
                 :prompt "Select repository group: "
                 :category 'agent-shell-repo-group
                 :require-match t
                 :history 'agent-shell-workflow-repo-group-history))))
         (group-sym (if (stringp chosen-group) (intern chosen-group) chosen-group))
         (selected-workflow
          (agent-shell-workflow-read
           category (format "Select workflow for group `%s': " group-sym))))
    (agent-shell-workflow-dispatch-group
     (agent-shell-workflow-spec-id selected-workflow)
     group-sym)))

(provide 'agent-shell-workflow-menu)

;;; agent-shell-workflow-menu.el ends here
