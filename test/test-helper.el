;;; test-helper.el --- ERT test infrastructure -*- lexical-binding: t; ; no-byte-compile: t; -*-

;;; Commentary:
;; Adds the package root to load-path for batch test runs and defines
;; transient key-integrity helpers used by the menu tests.

;;; Code:

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory test-file))
       (root (file-name-directory (directory-file-name test-dir))))
  (add-to-list 'load-path root))
;; Suppress system desktop notifications during test execution
(require 'alert nil t)
(when (boundp 'alert-default-style)
  (setq alert-default-style 'ignore))

(require 'cl-lib)

;;; Mock agent-shell-queue if not installed

(unless (featurep 'agent-shell-queue)
  (defvar agent-shell-queue--item-types (make-hash-table :test #'eq))
  (cl-defun agent-shell-queue-register-item-type (&key kind label buffer-pred dispatch-fn input-spec)
    (puthash kind (list :kind kind :label label :buffer-pred buffer-pred :dispatch-fn dispatch-fn :input-spec input-spec)
             agent-shell-queue--item-types))
  (defun agent-shell-queue--type-for-kind (kind)
    (gethash kind agent-shell-queue--item-types))
  (defun agent-shell-queue--agent-shell-buffer-p (buf)
    (with-current-buffer buf
      (derived-mode-p 'agent-shell-mode)))
  (cl-defstruct (agent-shell-queue-item
                 (:constructor agent-shell-queue-item--make)
                 (:copier nil))
    id args status kind background created dispatched completed response outcome directory)
  (defun agent-shell-queue--make-item (prompt &optional background kind _delay-before _delay-after)
    (agent-shell-queue-item--make
     :id "test-item-1"
     :args prompt
     :background background
     :kind (or kind 'workflow)
     :status 'queued))
  (cl-defstruct (agent-shell-queue-store
                 (:constructor agent-shell-queue--make-store)
                 (:copier nil))
    items format file)
  (cl-defstruct (agent-shell-queue-queue
                 (:constructor agent-shell-queue-queue--make)
                 (:copier nil))
    store active-buffer-name name head session-paused editing-ids interjection-pending halted-sessions)
  (defvar agent-shell-queue--store nil)
  (defvar agent-shell-queue--queue nil)
  (defvar agent-shell-queue--loaded nil)
  (defun agent-shell-queue-enqueue-item (&rest _) t)
  (defun agent-shell-queue-enqueue-clear (&rest _) t)
  (defun agent-shell-queue--enqueue-args (&rest _) t)
  (defun agent-shell-queue--collect-visible-response-text (&rest _) nil)
  (defun agent-shell-queue--ensure-subscription (&rest _) t)
  (defun agent-shell-queue--save (&rest _) t)
  (defun agent-shell-queue--refresh-buffer (&rest _) t)
  (provide 'agent-shell-queue))

(defun agent-shell-test/suffix-plist (suffix)
  "Extract the plist from a parsed transient suffix spec SUFFIX.
Handles both the (CLASS :key ...) cons format and the
(LEVEL CLASS (:key ...)) tuple format produced by different transient versions."
  (if (integerp (car suffix))
      (nth 2 suffix)
    (cdr suffix)))

;;; Transient key introspection helpers

(defun transient-test/collect-keys (prefix-sym)
  "Return a list of all :key strings in PREFIX-SYM's transient layout."
  (let ((queue (list (get prefix-sym 'transient--layout)))
        keys)
    (while queue
      (let ((node (pop queue)))
        (cond
          ((and (vectorp node) (not (byte-code-function-p node)))
           (setq queue (append (seq-into node 'list) queue)))
          ((and (consp node) (eq (car node) 'transient-suffix))
           (when-let* ((key (plist-get (cdr node) :key)))
             (push key keys)))
          ((consp node)
           (setq queue (append node queue))))))
    (nreverse keys)))

(defun transient-test/key-sequence-prefix-p (short long)
  "Return non-nil if SHORT key string is a strict prefix of LONG."
  (when (not (equal short long))
    (let ((sv (kbd short))
          (lv (kbd long)))
      (and (< (length sv) (length lv))
           (equal sv (substring lv 0 (length sv)))))))

(defun transient-test/key-prefix-conflicts (keys)
  "Return a list of (SHORT LONG) pairs where SHORT is a strict prefix of LONG."
  (let (conflicts)
    (dolist (short keys)
      (dolist (long keys)
        (when (transient-test/key-sequence-prefix-p short long)
          (push (list short long) conflicts))))
    conflicts))

(defun transient-test/duplicate-keys (keys)
  "Return a list of keys that appear more than once in KEYS."
  (let (seen dups)
    (dolist (k keys)
      (if (member k seen)
          (unless (member k dups)
            (push k dups))
        (push k seen)))
    dups))

(provide 'test-helper)
