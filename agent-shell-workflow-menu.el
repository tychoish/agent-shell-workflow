;;; agent-shell-workflow-menu.el --- ACR picker for agent-shell-workflow -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/agent-shell-workflow
;; Package-Requires: ((emacs "29.1") (transient "0.4") (annotated-completing-read "0.1"))

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
;; (`agent-shell-workflow-select') rather than a bespoke transient menu
;; per workflow.  `agent-shell-workflow-dispatch-menu' wires exactly one
;; entry point into `agent-shell-menu-dispatch' that calls the picker.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'annotated-completing-read)
(require 'agent-shell-workflow)
(eval-when-compile (require 'agent-shell-workflow))

(defun agent-shell-workflow--candidates (&optional category)
  "Return registered workflow specs, filtered to CATEGORY when non-nil."
  (let ((specs (agent-shell-workflow-list)))
    (if category
        (seq-filter (lambda (s) (equal (agent-shell-workflow-spec-category s) category)) specs)
      specs)))

;;;###autoload
(defun agent-shell-workflow-select (&optional category)
  "Pick a registered workflow via `annotated-completing-read' and dispatch it.
Candidates are annotated with their category and one-line doc.  With
CATEGORY non-nil, only workflows in that category are offered."
  (interactive)
  (let* ((specs (or (agent-shell-workflow--candidates category)
                    (user-error "No workflows registered")))
         (table (seq-map
                 (lambda (spec)
                   (cons (symbol-name (agent-shell-workflow-spec-id spec))
                         (cons (format "[%s] %s"
                                       (agent-shell-workflow-spec-category spec)
                                       (or (agent-shell-workflow-spec-doc spec) ""))
                               spec)))
                 specs))
         (selected (annotated-completing-read
                    table
                    :prompt "Select workflow: "
                    :category 'agent-shell-workflow
                    :require-match t
                    :history 'agent-shell-workflow-select)))
    (agent-shell-workflow-dispatch (agent-shell-workflow-spec-id selected))))

;;;###autoload
(transient-define-prefix agent-shell-workflow-dispatch-menu ()
  "Single entry point into the `agent-shell-workflow' ACR picker."
  ["Workflows"
   ("w" "Select workflow…" agent-shell-workflow-select)])

(provide 'agent-shell-workflow-menu)

;;; agent-shell-workflow-menu.el ends here
