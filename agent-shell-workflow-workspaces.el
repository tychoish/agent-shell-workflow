;;; agent-shell-workflow-workspaces.el --- Exact workspace targets -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in resource policies and immutable Git checkout snapshots for workflows.

;;; Code:

(require 'agent-shell-workflow)
(require 'map)
(require 'seq)
(require 'subr-x)

(defvar agent-shell-workflow-workspace-policies nil
  "Alist of named resource policies over existing repository groups.")

(defun agent-shell-workflow--workspace-directory (directory)
  "Normalize DIRECTORY without contacting unsupported remote transports."
  (if (file-remote-p directory)
      (file-name-as-directory (expand-file-name directory))
    (agent-shell-workflow--canonicalize-dir directory)))

(defun agent-shell-workflow--workspace-git (directory &rest arguments)
  "Run Git ARGUMENTS in DIRECTORY, returning trimmed output or signaling."
  (let ((default-directory directory))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (user-error "Git in %s: %s" directory (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun agent-shell-workflow--workspace-target (directory)
  "Snapshot the exact checkout containing DIRECTORY, including failures."
  (let* ((canonical (agent-shell-workflow--workspace-directory directory))
         (target (list :id (secure-hash 'sha256 canonical)
                       :worktree-dir canonical :resource-id "local")))
    (condition-case failure
        (progn
          (when (file-remote-p canonical)
            (user-error "Remote checkout requires a transport adapter: %s" canonical))
          (unless (file-directory-p canonical)
            (user-error "Checkout directory is unavailable: %s" canonical))
          (let* ((checkout (agent-shell-workflow--canonicalize-dir
                            (agent-shell-workflow--workspace-git
                             canonical "rev-parse" "--show-toplevel")))
                 (common (agent-shell-workflow--canonicalize-dir
                          (expand-file-name
                           (agent-shell-workflow--workspace-git
                            checkout "rev-parse" "--git-common-dir") checkout)))
                 (branch (agent-shell-workflow--workspace-git
                          checkout "rev-parse" "--abbrev-ref" "HEAD"))
                 (git-dir (agent-shell-workflow--workspace-directory
                           (expand-file-name
                            (agent-shell-workflow--workspace-git
                             checkout "rev-parse" "--git-dir") checkout))))
            (setq target
                  (list :id (secure-hash 'sha256 checkout)
                        :repo-root
                        (if (equal common git-dir)
                            checkout
                          (agent-shell-workflow--workspace-directory
                           (substring
                            (car (split-string
                                  (agent-shell-workflow--workspace-git
                                   checkout "worktree" "list" "--porcelain" "-z")
                                  "\0"))
                            (length "worktree "))))
                        :common-dir common :worktree-dir checkout
                        :branch (unless (equal branch "HEAD") branch)
                        :head (agent-shell-workflow--workspace-git
                               checkout "rev-parse" "HEAD")
                        :resource-id "local"
                        :dirty (not (string-empty-p
                                     (agent-shell-workflow--workspace-git
                                      checkout "status" "--porcelain")))
                        :unmerged (not (string-empty-p
                                        (agent-shell-workflow--workspace-git
                                         checkout "ls-files" "-u")))))))
      (error (setq target (plist-put target :preflight-error
                                     (error-message-string failure)))))
    target))

;;;###autoload
(cl-defun agent-shell-workflow-register-workspace-policy
    (name &key (group name) (resource 'local) shell-directory
          worktree-rules repository-rules)
  "Register NAME over GROUP with RESOURCE and SHELL-DIRECTORY.
WORKTREE-RULES and REPOSITORY-RULES are alists of exact directories to
resource plists (:resource local :shell-directory DIR).  Conflicting
duplicate rules are rejected.  Return NAME."
  (unless (and name (symbolp name) group resource (symbolp resource)
               (stringp shell-directory))
    (user-error "Policy needs a symbol name, group, resource and shell directory"))
  (agent-shell-workflow-resolve-repo-group group)
  (let ((normalized
         (seq-map
          (lambda (rules)
            (let (seen)
              (seq-do
               (lambda (rule)
                 (unless (and (stringp (car-safe rule))
                              (stringp (plist-get (cdr rule) :shell-directory))
                              (symbolp (or (plist-get (cdr rule) :resource) 'local)))
                   (user-error "Invalid resource rule: %S" rule))
                 (let* ((path (agent-shell-workflow--workspace-directory (car rule)))
                        (value (copy-tree (cdr rule)))
                        (prior (assoc path seen)))
                   (setq value
                         (plist-put value :shell-directory
                                    (agent-shell-workflow--workspace-directory
                                     (plist-get value :shell-directory))))
                   (when (and prior (not (equal (cdr prior) value)))
                     (user-error "Conflicting resource rules for %s" path))
                   (push (cons path value) seen)))
               rules)
              (nreverse seen)))
          (list worktree-rules repository-rules))))
    (setf (map-elt agent-shell-workflow-workspace-policies name)
          (copy-tree (list :group group :resource resource
                           :shell-directory
                           (agent-shell-workflow--workspace-directory shell-directory)
                           :worktree-rules (car normalized)
                           :repository-rules (cadr normalized)))))
  name)

;;;###autoload
(defun agent-shell-workflow-resolve-workspace (workspace &optional directories)
  "Return a fresh normalized WORKSPACE snapshot without starting shells.
DIRECTORIES overrides the existing group membership with exact checkouts.
With nil WORKSPACE, DIRECTORIES supplies the entire selection.  Failures
remain visible as :preflight-error fields; checkout aliases collapse."
  (let* ((policy (and workspace
                      (or (map-elt agent-shell-workflow-workspace-policies workspace)
                          (user-error "Unknown workspace policy: %S" workspace))))
         (paths (or directories
                    (agent-shell-workflow-resolve-repo-group
                     (plist-get policy :group))))
         seen targets)
    (unless paths (user-error "Workspace needs at least one checkout"))
    (seq-do
     (lambda (path)
       (let* ((target (agent-shell-workflow--workspace-target path))
              (checkout (plist-get target :worktree-dir))
              (rule (or
                     (seq-find (lambda (entry)
                                 (equal checkout
                                        (car entry)))
                               (plist-get policy :worktree-rules))
                     (seq-find (lambda (entry)
                                 (equal (plist-get target :repo-root)
                                        (car entry)))
                               (plist-get policy :repository-rules))))
              (resource (or (cdr rule) policy))
              (kind (or (plist-get resource :resource) 'local))
              (shell-dir (agent-shell-workflow--workspace-directory
                          (or (plist-get resource :shell-directory) checkout))))
         (setq target (plist-put target :shell-dir shell-dir))
         (setq target (plist-put target :resource-id (symbol-name kind)))
         (when (or (not (eq kind 'local)) (file-remote-p shell-dir)
                   (not (file-directory-p shell-dir)))
           (setq target (plist-put target :preflight-error
                                   (format "Unavailable resource %s at %s; local directory required"
                                           kind shell-dir))))
         (unless (member (plist-get target :id) seen)
           (push (plist-get target :id) seen)
           (push target targets))))
     paths)
    (list :workspace workspace :targets (nreverse targets))))

(provide 'agent-shell-workflow-workspaces)
;;; agent-shell-workflow-workspaces.el ends here
