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
(eval-when-compile (require 'agent-shell-workflow))

(declare-function agent-shell-workflow-dispatch-group "agent-shell-workflow")

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
