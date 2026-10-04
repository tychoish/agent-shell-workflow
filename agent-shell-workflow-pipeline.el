;;; agent-shell-workflow-pipeline.el --- Worktree executor and verifier pipeline workflows -*- lexical-binding: t -*-

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

;; Worktree-isolated autonomous execution cascade workflows.  A primary
;; operation runs in an isolated worktree via `sprite-mcp`, followed by a
;; structured verifier step via `gptel`, escalating to `hitl` when necessary.

;;; Code:

(require 'seq)
(require 'json)
(require 'agent-shell-workflow)
(require 'agent-shell-workflow-library)
(eval-when-compile (require 'agent-shell-workflow))

(declare-function sprite-mcp-spawn "sprite-mcp" (&rest args))
(declare-function sprite-mcp-kill "sprite-mcp" (sprite-or-id &rest args))
(declare-function sprite-mcp-worktree "sprite-mcp" (entry))
(declare-function sprite-mcp-branch "sprite-mcp" (entry))
(declare-function sprite-mcp-sprite-id "sprite-mcp" (entry))
(declare-function hitl-ask "hitl" (&rest args))
(declare-function gptel-request "gptel-request" (prompt &rest args))

;; Pipeline executor and verifier workflows

(defconst agent-shell-workflow-library--pipeline-verify-schema
  '(:type "object"
    :properties (:accepted (:type "boolean")
                 :confidence (:type "number")
                 :summary (:type "string")
                 :issues (:type "array" :items (:type "string")))
    :required ["accepted" "summary"])
  "JSON schema for structured diff verification output.")

(defun agent-shell-workflow-library--pipeline-run-pre-op (ctx)
  "Initialize an isolated worktree for the edit operation in CTX.
Uses `sprite-mcp-spawn' when available; falls back to the current repository
root otherwise.  Populates :op-id, :repo-root, :worktree-dir, :context-dir,
:sprite-id, and :branch in CTX."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file))
         (instruction (plist-get args :instruction))
         (repo (or (plist-get args :repo-root) (agent-shell-workflow-library--project-root)))
         (op-id (or (plist-get args :op-id)
                    (format "op-%d" (floor (float-time)))))
         (smcp (when (fboundp 'sprite-mcp-spawn)
                 (condition-case nil
                     (sprite-mcp-spawn :name op-id :repo-root repo)
                   (error nil))))
         (wt-dir (if smcp (sprite-mcp-worktree smcp) repo))
         (smcp-id (when smcp (sprite-mcp-sprite-id smcp)))
         (branch (when smcp (sprite-mcp-branch smcp)))
         (updated-ctx (copy-sequence ctx)))
    (unless (and file (not (string-empty-p (format "%s" file))))
      (user-error "Target file must be specified for executor-pipeline-run"))
    (unless (and instruction (not (string-empty-p (format "%s" instruction))))
      (user-error "Instruction must be specified for executor-pipeline-run"))
    (setq updated-ctx (plist-put updated-ctx :op-id op-id))
    (setq updated-ctx (plist-put updated-ctx :repo-root repo))
    (setq updated-ctx (plist-put updated-ctx :worktree-dir wt-dir))
    (setq updated-ctx (plist-put updated-ctx :context-dir wt-dir))
    (setq updated-ctx (plist-put updated-ctx :sprite-id smcp-id))
    (setq updated-ctx (plist-put updated-ctx :branch branch))
    updated-ctx))

(defun agent-shell-workflow-library--pipeline-run-post-op (_shell-buffer ctx _response-text)
  "Capture git diff from the worktree in CTX and chain to the verifier workflow."
  (let* ((wt-dir (plist-get ctx :worktree-dir))
         (args (plist-get ctx :args))
         (diff (if (and wt-dir (file-directory-p wt-dir))
                   (let ((d (apply #'agent-shell-workflow-library--shell
                                   "git" (list "-C" wt-dir "diff" "HEAD"))))
                     (if (string-empty-p (string-trim d))
                         (apply #'agent-shell-workflow-library--shell
                                "git" (list "-C" wt-dir "diff" "HEAD~1"))
                       d))
                 "")))
    `(:chain executor-pipeline-verify
             (:op-id ,(plist-get ctx :op-id)
              :file ,(plist-get args :file)
              :instruction ,(plist-get args :instruction)
              :diff ,(or diff "")
              :repo-root ,(plist-get ctx :repo-root)
              :worktree-dir ,wt-dir
              :sprite-id ,(plist-get ctx :sprite-id)
              :branch ,(plist-get ctx :branch)))))

(defun agent-shell-workflow-library--pipeline-verify-pre-op (ctx callback)
  "Evaluate worktree diff against the instruction in CTX via `gptel-request'.
Runs asynchronously and invokes CALLBACK with :verify-result populated in CTX."
  (let* ((args (plist-get ctx :args))
         (file (plist-get args :file))
         (instruction (plist-get args :instruction))
         (diff (or (plist-get args :diff) ""))
         (updated-ctx (copy-sequence ctx)))
    (if (string-empty-p (string-trim diff))
        ;; Immediately reject empty diff
        (let ((res (list :accepted nil
                         :confidence 1.0
                         :summary "No code modifications were produced in worktree"
                         :issues '("Empty diff: instruction was not applied"))))
          (funcall callback (plist-put updated-ctx :verify-result res)))
      (if (fboundp 'gptel-request)
          (let ((prompt (format "You are an automated code verification agent. Evaluate whether the following git diff correctly and safely implements the requested instruction for file `%s`.

Instruction: %s

Diff:
%s

Respond with a JSON object containing:
- \"accepted\": boolean (true if diff accurately fulfills instruction without regression)
- \"confidence\": number between 0.0 and 1.0
- \"summary\": concise string explanation
- \"issues\": array of string issue descriptions if any"
                                (or file "unknown") (or instruction "") diff)))
            (condition-case err
                (gptel-request prompt
                  :schema agent-shell-workflow-library--pipeline-verify-schema
                  :callback
                  (lambda (resp _info)
                    (let* ((parsed (when (stringp resp)
                                     (ignore-errors
                                       (json-parse-string resp :object-type 'plist :array-type 'list))))
                           (accepted (and parsed (plist-get parsed :accepted)
                                          (not (eq (plist-get parsed :accepted) :false))))
                           (conf (if parsed (or (plist-get parsed :confidence) 1.0) 0.0))
                           (summary (if parsed (or (plist-get parsed :summary) "")
                                      (or resp "Verification failed to produce valid JSON")))
                           (issues (if parsed (plist-get parsed :issues) nil))
                           (res (list :accepted (and accepted t)
                                      :confidence (float conf)
                                      :summary summary
                                      :issues issues)))
                      (funcall callback (plist-put updated-ctx :verify-result res)))))
              (error
               (let ((res (list :accepted nil
                                :confidence 0.0
                                :summary (format "Verification failed: %s" (error-message-string err))
                                :issues (list (error-message-string err)))))
                 (funcall callback (plist-put updated-ctx :verify-result res))))))
        ;; Fallback when gptel is not loaded
        (let ((res (list :accepted t
                         :confidence 0.5
                         :summary "Unverified: gptel is not loaded in current environment"
                         :issues nil)))
          (funcall callback (plist-put updated-ctx :verify-result res)))))))

(defun agent-shell-workflow-library--pipeline-verify-post-op (_shell-buffer ctx _response)
  "Evaluate verifier result in CTX.
Clean up worktree on success or escalate to `hitl' on failure."
  (let* ((res (plist-get ctx :verify-result))
         (args (plist-get ctx :args))
         (op-id (or (plist-get args :op-id) (plist-get ctx :op-id) "unknown"))
         (file (or (plist-get args :file) ""))
         (diff (or (plist-get args :diff) ""))
         (wt-dir (or (plist-get args :worktree-dir) (plist-get ctx :worktree-dir)))
         (sprite-id (or (plist-get args :sprite-id) (plist-get ctx :sprite-id)))
         (accepted (plist-get res :accepted))
         (confidence (or (plist-get res :confidence) 0.0))
         (summary (or (plist-get res :summary) ""))
         (issues (plist-get res :issues)))
    (if (and accepted (>= confidence 0.85))
        (progn
          (message "Op `%s` verified successfully (confidence %.2f): %s"
                   op-id confidence summary)
          (when (and sprite-id (fboundp 'sprite-mcp-kill))
            (sprite-mcp-kill sprite-id :cleanup-worktree t))
          :done)
      ;; Rejected or low-confidence: escalate to hitl
      (if (fboundp 'hitl-ask)
          (hitl-ask
           :id (format "verify-%s" op-id)
           :prompt (format "Verifier rejected or flagged op `%s` (%s) with confidence %.2f:

Summary: %s
Issues: %s

Diff:
%s

Select action:"
                           op-id file confidence summary
                           (if issues (mapconcat #'identity issues "; ") "none")
                           diff)
           :kind :single-choice
           :options '("Accept and Merge" "Reject and Discard" "Keep Worktree")
           :default-value "Reject and Discard"
           :metadata (list :op-id op-id :worktree-dir wt-dir :sprite-id sprite-id :diff diff)
           :callback
           (lambda (choice _q)
             (pcase choice
               ("Accept and Merge"
                (message "User accepted op %s" op-id)
                (when (and sprite-id (fboundp 'sprite-mcp-kill))
                  (sprite-mcp-kill sprite-id :cleanup-worktree nil)))
               ("Reject and Discard"
                (message "User rejected op %s" op-id)
                (when (and sprite-id (fboundp 'sprite-mcp-kill))
                  (sprite-mcp-kill sprite-id :cleanup-worktree t)))
               ("Keep Worktree"
                (message "Worktree retained at %s for op %s" wt-dir op-id)))))
        (message "Verifier flagged op `%s`: %s (hitl not available)" op-id summary))
      :done)))

(register-agent-shell-workflow executor-pipeline-run
  :doc "Execute an atomic edit operation in an isolated sprite-mcp git worktree"
  :category "Pipeline"
  :args ((file :prompt "Target file: ")
         (instruction :prompt "Instruction: ")
         (op-id :prompt "Operation ID (optional): " :optional t)
         (repo-root :prompt "Repository root (optional): " :optional t))
  :pre-op #'agent-shell-workflow-library--pipeline-run-pre-op
  :template "In file `{{args.file}}`, apply the following instruction:
{{args.instruction}}

Perform all edits in this isolated worktree. Ensure all tests and syntax checks pass. Do not commit or push."
  :submit t
  :target :session-reuse
  :post-op #'agent-shell-workflow-library--pipeline-run-post-op)

(register-agent-shell-workflow executor-pipeline-verify
  :doc "Verify a worktree diff against the instruction using gptel with HITL escalation"
  :category "Pipeline"
  :args ((op-id :prompt "Op ID: ")
         (file :prompt "Target file: ")
         (instruction :prompt "Instruction: ")
         (diff :prompt "Diff: ")
         (worktree-dir :prompt "Worktree directory: ")
         (repo-root :prompt "Repo root: " :optional t)
         (sprite-id :prompt "Sprite ID: " :optional t)
         (branch :prompt "Branch: " :optional t))
  :pre-op #'agent-shell-workflow-library--pipeline-verify-pre-op
  :template "Verification completed for op {{args.op-id}}."
  :submit nil
  :target :session-reuse
  :post-op #'agent-shell-workflow-library--pipeline-verify-post-op)

;; Backward compatibility aliases


(provide 'agent-shell-workflow-pipeline)

;;; agent-shell-workflow-pipeline.el ends here
