;;; agent-shell-workflow-worker.el --- Worker routing and lifecycle -*- lexical-binding: t; -*-

;;; Commentary:
;; Worker routing, capability discovery, coordinator reporting routes, and
;; idle worker release during waits for claimed train attempts.
;; Never falls back to the current coordinator Emacs session for worker
;; editing or testing.
;;
;; Load-time pure: starts no sprites, daemons, or timers on `require'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function sprite-mcp-spawn "sprite-mcp" (&rest args))
(declare-function sprite-mcp-kill "sprite-mcp" (sprite-or-id &rest args))
(declare-function mcpkit-proxy-fetch-tools "mcpkit-proxy" (target-id))

;;; Worker Binding Registry

(defvar agent-shell-workflow-worker--bindings (make-hash-table :test #'equal)
  "Map of attempt-id string to worker binding plist.")

(cl-defun agent-shell-workflow-worker-bind
    (attempt-id &key sprite-name target-id endpoint checkout generation
                coordinator-endpoint capabilities retain-lease)
  "Bind an isolated worker to ATTEMPT-ID.
Refuses binding if TARGET-ID or SPRITE-NAME refers to the current coordinator
Emacs instance (`main' or `nil')."
  (when (or (null target-id)
            (string-empty-p target-id)
            (member target-id '("main" "self" "coordinator")))
    (user-error "Worker binding must target an isolated sprite, never coordinator Emacs"))
  (unless (and (stringp checkout) (file-name-absolute-p checkout))
    (user-error "Worker binding requires an absolute canonical checkout path"))
  (let ((binding (list :attempt-id attempt-id
                       :sprite-name (or sprite-name target-id)
                       :target-id target-id
                       :endpoint endpoint
                       :checkout (directory-file-name (expand-file-name checkout))
                       :generation (or generation 1)
                       :coordinator-endpoint coordinator-endpoint
                       :capabilities (copy-sequence capabilities)
                       :retain-lease (not (null retain-lease))
                       :status 'active
                       :bound-at (float-time))))
    (setf (map-elt agent-shell-workflow-worker--bindings attempt-id) binding)
    binding))

(defun agent-shell-workflow-worker-get (attempt-id)
  "Return worker binding plist for ATTEMPT-ID, or nil."
  (when-let* ((b (map-elt agent-shell-workflow-worker--bindings attempt-id)))
    (copy-sequence b)))

(defun agent-shell-workflow-worker-discover-capabilities (attempt-id &optional fetcher)
  "Discover live worker capabilities for ATTEMPT-ID after startup or route change.
Uses FETCHER (defaults to `mcpkit-proxy-fetch-tools') and updates the binding's
`:capabilities' manifest.  Signals an error if route is lost."
  (let ((binding (map-elt agent-shell-workflow-worker--bindings attempt-id)))
    (unless binding
      (user-error "No worker bound to attempt %s" attempt-id))
    (let* ((tid (plist-get binding :target-id))
           (fn (or fetcher
                   (and (fboundp 'mcpkit-proxy-fetch-tools) #'mcpkit-proxy-fetch-tools)))
           (tools (when fn (funcall fn tid))))
      (unless tools
        (error "Worker route %s returned no tools (route lost or unavailable)" tid))
      (let ((updated (thread-first (copy-sequence binding)
                       (plist-put :capabilities tools)
                       (plist-put :discovered-at (float-time)))))
        (setf (map-elt agent-shell-workflow-worker--bindings attempt-id) updated)
        updated))))

(defun agent-shell-workflow-worker-release-for-wait (attempt-id &optional stopper)
  "Release worker bound to ATTEMPT-ID when entering a wait state.
If `:retain-lease' is non-nil, retains the worker and returns `:retained t'.
Otherwise calls STOPPER (if supplied) without touching checkout or logs,
removes the binding, and returns `:released t'."
  (let ((binding (map-elt agent-shell-workflow-worker--bindings attempt-id)))
    (cond
     ((null binding)
      (list :attempt-id attempt-id :released nil :reason "not-bound"))
     ((plist-get binding :retain-lease)
      (list :attempt-id attempt-id :released nil :retained t
            :target-id (plist-get binding :target-id)))
     (t
      (when stopper
        (funcall stopper (plist-get binding :target-id)))
      (map-delete agent-shell-workflow-worker--bindings attempt-id)
      (list :attempt-id attempt-id :released t :retained nil
            :preserved-checkout (plist-get binding :checkout))))))

(defun agent-shell-workflow-worker-active-count ()
  "Return count of currently bound active workers."
  (hash-table-count agent-shell-workflow-worker--bindings))

(provide 'agent-shell-workflow-worker)
;;; agent-shell-workflow-worker.el ends here
