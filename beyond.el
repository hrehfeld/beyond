;;; beyond-core.el --- An Ergonomic Command State  -*- lexical-binding: t -*-

;;; Commentary:

;; This module sets up the emulation keymaps for each beyond state.
;; Functions to switch to each state is also provided.

;;; Code:

(require 'cl-macs)
(require 'dash)
(require 'subr-x)

(defgroup beyond nil "Beyond" :group 'Editing)


;; (defvar beyond/insert-command-history nil "History of changes in this insertion round.")
;; (defvar beyond/insert-command nil "Command which started the insertion.")
;; ;; (defvar beyond/insert-origin 0 "Point at start of insert state.")

(defvar-local beyond-input-method nil
"The input method to activate
when going to insert state. (When leaving insert state the
input-method is reset to nil.)")

(defvar beyond-insertion-state-begin-hook nil "A list of functions to
run when entering insert state.")


(defun beyond-kill-non-bol-trailing-whitespace ()
  (save-excursion
    (when (not (bolp))
      (let ((insert-point (point)))
        (skip-syntax-forward " " (line-end-position))
        (when (eolp) (delete-region insert-point (point)))))))


(add-hook 'beyond-insertion-state-begin-hook #'beyond-kill-non-bol-trailing-whitespace)

(defvar-local beyond-state-stack nil "Stack of active states.")
;; (setq beyond-state-stack nil)


(defun beyond--active-state ()
  (car beyond-state-stack))

(defun beyond--pop-active-state ()
  (pop beyond-state-stack))

(defun beyond--push-active-state (state)
  (push state beyond-state-stack))

(defun beyond--exit-active-state (&optional no-error)
  (if-let ((state (beyond--pop-active-state)))
      (set state nil)
    (unless no-error
      (error "No beyond state active."))))

(defun beyond--state-set (state-sym state-val)
  (if (symbol-function state-sym)
      ;; call minor mode if state has one
      (funcall state-sym (if state-val 1 -1))
    (set state-sym state-val)))

(defun beyond--switch-state (state)
  (cl-assert (symbolp state) "state %S should be a symbol" state)
  (message "Beyond switching to %S" state)
  (let ((old-state (beyond--active-state)))
    (unless (eq state old-state)
      ;; disable previous state
      (when old-state
        (beyond--state-set old-state nil))
      ;; save and activate new state
      (beyond--push-active-state state)
      (beyond--state-set state t)
      (beyond-update-cursor))))

(defun beyond--switch-back-state ()
  (beyond--pop-active-state)
  (let ((previous-state (beyond--active-state)))
    (message "Beyond switching back to %S" previous-state)
    (beyond--state-set previous-state t)
    (beyond-update-cursor)))


(defvar beyond-motion-state-map (make-sparse-keymap) "Keymap for
motion state.")


(defvar beyond-command-state-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (set-keymap-parent map beyond-motion-state-map)
    map)
  "Keymap for command state.")

(defvar-local beyond-command-state nil "Is command state
currently active?")


(defvar beyond-special-state-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (set-keymap-parent map beyond-motion-state-map)
    map)
  "Keymap for special state.")

(defvar-local beyond-special-state nil "Is special state
currently active?")


(defun beyond--signal-point-read-only ()
  (when (and buffer-read-only (not (beyond-shell-mode-p)))
          (error "Buffer is read only, can't insert in it"))
        (when (get-text-property (point) 'read-only)
          (if-let ((writeable-pos (next-single-property-change (point) 'read-only nil (line-end-position))))
              (progn
                (when (get-text-property writeable-pos 'read-only)
                  (error "Rest of the line is read only"))
                (goto-char writeable-pos))
            (error "Rest of the buffer is read only")))
  )

(define-minor-mode beyond-insertion-state
  "beyond's minor mode for inserting."
  :lighter " by-INS"
  :keymap (make-sparse-keymap)
  (if beyond-insertion-state
      (progn
        (beyond--signal-point-read-only)
        (beyond--switch-state 'beyond-insertion-state)
        (activate-input-method beyond-input-method)
        ;;(deactivate-mark)
        (push-mark)
        )
    (setq-local beyond-input-method current-input-method)
    (deactivate-input-method)
    (push-mark)
    (beyond--switch-back-state)
    ))

(defun beyond-exit-insertion-state () (interactive) (beyond-insertion-state -1))

;; manage keymaps
(defvar beyond-state-map-alist
  (list (cons 'beyond-insertion-state  beyond-insertion-state-map)
        (cons 'beyond-command-state beyond-command-state-map)
        (cons 'beyond-special-state beyond-special-state-map)
        )
  "List of cons pairs `(VAR . MAP)' that will enable beyond's state's keymaps.

Earlier items overwrite bindings in later maps, so make sure
inheriting states appear before their parents -- e.g.
beyond-command-state before beyond-motion-state.")
;;(kill-local-variable 'beyond-state-map-alist)


;; (defcustom beyond-default-cursor-type 'bar "Default `cursor-type', also used for the minibuffer." :group 'beyond :type 'sexp)
;; (defcustom beyond-command-cursor-type 'box "`cursor-type' for command state." :group 'beyond
;; :type 'sexp)
(defvar-local beyond--default-cursor-type 'undefined "The cursor type before insert mode. If this is the symbol `undefined', cursor-type is saved before changing")
(defvar-local beyond--default-cursor-color nil "The cursor color before insert mode")

(defcustom beyond-cursor-types '((beyond-insertion-state bar nil))
  "`cursor-type' for each state. Each list element should be a `(state type color)'. Cursor will be set for active all active states, overwriting previous cursor settings" :group 'beyond :type 'sexp)

(defun beyond-update-cursor ()
  "Update the cursor depending on the current beyond state."
  (with-current-buffer (window-buffer)
    (let ((type-color
           (or (car-safe
                (mapcan
                 (lambda (cursor-type-color)
                   (pcase
                       cursor-type-color
                     (`(,mode ,type ,color)
                      (when (symbol-value mode)
                        (list (list type color))))
                     (error "no match")))
                 beyond-cursor-types))
               (list beyond--default-cursor-type beyond--default-cursor-color))))
      (when (eq beyond--default-cursor-type 'undefined)
        (setq beyond--default-cursor-type cursor-type
              beyond--default-cursor-color (frame-parameter nil 'cursor-color)))
      ;;(message "beyond-update-cursor %S %i" type-color (length type-color))
      (pcase
          type-color
        (`(,type ,color)
         (unless (eq type 'undefined)
           (setq cursor-type type))
         (when color
           (set-cursor-color color))))

      )))
;; (beyond-update-cursor)
(add-hook 'buffer-list-update-hook #'beyond-update-cursor)

;; (defun beyond-interactive-insert (&rest args)
;;   "Beyond insert commands must call this function after `interactive'.
;; The effect of this function is to remember the current command
;; and ARGS so that it can be repeated later by
;; `beyond-set-insert-like-state'.  The current command must take an
;; optional list of changes as its last argument."
;;   (unless beyond/insert-command
;;     (setq beyond/insert-command (cons this-command (-map (lambda (x) (list 'quote x)) args)))))

;; (defun beyond/after-change-hook (begin end old-len)
;;   "Remember the change defined by BEGIN END OLD-LEN in `beyond/insert-command-history'."
;;   (when (and beyond-insert-state (not (bound-and-true-p mc--executing-command-for-fake-cursor)))
;;     ;; (message "bach: %s" beyond/insert-command-history (list begin end old-len))
;;     (cond ((and beyond/insert-command-history
;;                 (string= "" (nth 2 (car beyond/insert-command-history))) ;; no insert
;;                 (eq begin end) ;; no insert
;;                 (eq (+ begin old-len) (+ beyond/insert-origin
;;                                          (car (car beyond/insert-command-history)))))
;;            ;; two consecutive deletes: concat them.
;;            (setq beyond/insert-command-history (cons (list (- begin beyond/insert-origin)
;;                                                          (+ old-len (nth 1 (car beyond/insert-command-history)))
;;                                                          "")
;;                                                    (cdr beyond/insert-command-history))))
;;           ((and beyond/insert-command-history
;;                 (eq 0 (nth 1 (car beyond/insert-command-history))) ;; no delete
;;                 (eq 0 old-len) ;; no delete
;;                 (eq begin (+ beyond/insert-origin
;;                              (car (car beyond/insert-command-history))
;;                              (length (nth 2 (car beyond/insert-command-history))))))
;;            ;; two consecutive inserts: concat them.
;;            (setq beyond/insert-command-history (cons (list (car (car beyond/insert-command-history))
;;                                                          0
;;                                                          (concat (nth 2 (car beyond/insert-command-history)) (buffer-substring-no-properties begin end)))
;;                                                    (cdr beyond/insert-command-history))))
;;           (t
;;            (push (list (- begin beyond/insert-origin) old-len (buffer-substring-no-properties begin end))
;;                  beyond/insert-command-history)))))

;; (defun beyond/replay-changes (changes)
;;   "Replay the CHANGES at the current point."
;;   (let ((p0 (point)))
;;     (setq beyond/insert-command nil) ;; did not go to insert state after all
;;     (dolist (change changes)
;;       (goto-char (+ p0 (nth 0 change)))
;;       (delete-region (+ p0 (nth 0 change)) (+ p0 (nth 0 change) (nth 1 change)))
;;       (insert (nth 2 change)))))

(defcustom beyond-special-state-list
  '(Buffer-menu-mode
    cfw:calendar-mode
    debugger-mode
    ediff-mode
    ediff-meta-mode
    finder-mode
    git-rebase-mode
    mu4e-headers-mode
    mu4e-view-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    notmuch-tree-mode
    org-agenda-mode
    pass-mode
    view-mode)
    "A List of modes which should use `beyond-special-state'."
    :group 'beyond
    :type '(repeat symbol))

(defun beyond-shell-mode-p ()
  "Is the `major-mode' any of the shell modes?"
  (derived-mode-p 'comint-mode 'eshell-mode 'term-mode 'vterm-mode))

;;fixme: make this a list of function syms instead
(defcustom beyond-special-conditions
  '((bound-and-true-p magit-blame-mode))
  "A list of sufficient conditions to trigger special state."
  :group 'beyond :type '(list sexp))

(defcustom beyond-insert-conditions '((eq major-mode 'message-mode))
  "A list of sufficient conditions to start in insert state."
  :group 'beyond :type '(list sexp))

(defun beyond-special-state-p ()
  "Should the mode use `beyond-special-state'?"
  (or (and (eq (get major-mode 'mode-class) 'special)
           (not (beyond-shell-mode-p)))
      (-some 'eval beyond-special-conditions)
      (memq major-mode beyond-special-state-list)))

(defun beyond-insertion-state-p () (-some 'eval beyond-insert-conditions))

;;; Initialisation and activation

(define-minor-mode beyond-local-mode
  "Minor mode for setting up command state in a single buffer."
  :lighter " beyond"
  :keymap nil
  (setq beyond-state-stack nil)
  )


(defun beyond-local-mode--initialize ()
  "Setup beyond in the current buffer.
Should only be used to
initialize `beyond-local-mode' from the globalized minor-mode
`beyond-mode'.  It is called whenever beyond is enabled in a buffer
for the first time or when beyond is active and the `major-mode' of
the buffer changes."
  (unless (minibufferp)
    (cl-pushnew 'beyond-state-map-alist emulation-mode-map-alists)
        ;; (unless (memq 'beyond/after-change-hook after-change-functions)
        ;;   (push 'beyond/after-change-hook after-change-functions))
    (beyond--switch-state
     (cond ((beyond-special-state-p)
            'beyond-special-state)
           ((beyond-insertion-state-p)
            'beyond-insertion-state)
           (t 'beyond-command-state)))))

;;;###autoload (autoload 'beyond-mode "beyond" "Toggle beyond in all buffers" t)
(define-globalized-minor-mode beyond-mode beyond-local-mode beyond-local-mode--initialize :group 'beyond)


(with-eval-after-load 'multiple-cursors
  ;; (defvar mc--default-cmds-to-run-for-all)
  ;; (defvar mc--default-cmds-to-run-once)
  (setq mc--default-cmds-to-run-for-all
        (append '(beyond-beginning-of-expression
                  beyond-beginning-of-line
                  beyond-end-of-expression
                  beyond-end-of-line
                  beyond-end-of-region
                  beyond-find-char-backward
                  beyond-find-char-forward
                  beyond-quote-character
                  beyond-replace-by-character
                  beyond-smarter-backward
                  beyond-smarter-forward
                  beyond-splice
                  beyond-split-line
                  beyond-switch-mark
                  beyond-toggle-character-case
                  beyond-toggle-mark)))
  (setq mc--default-cmds-to-run-once
        (append mc--default-cmds-to-run-once
                '(beyond-adjust-indent
                  beyond-navigate-forward
                  beyond-navigate-backward
                  beyond-drop-mark
                  beyond-enclose
                  beyond-qsearch-next
                  beyond-qsearch-next-at-point
                  beyond-qsearch-previous
                  beyond-qsearch-previous-at-point
                  beyond-query-replace
                  beyond-quit
                  beyond-set-command-state
                  beyond-set-insert-like-state
                  beyond-substitute-region
                  beyond-take-region
                  beyond-toggle-character-case
                  beyond-toggle-case))))

(defun beyond-multiple-cursor-each-command-hook ()
  (mc/create-fake-cursor-at-point))

(define-minor-mode beyond-multiple-cursor-each-command-mode
  "Add a multiple cursor after every command."
  :lighter " ADD MC"
  (if beyond-multiple-cursor-each-command-mode
      (add-hook 'post-command-hook #'beyond-multiple-cursor-each-command-hook)
    (remove-hook 'post-command-hook #'beyond-multiple-cursor-each-command-hook)
      )
  )

(define-key beyond-command-state-map (kbd "u") #'beyond-insertion-state)
(define-key beyond-insertion-state-map (kbd "C-g") #'beyond-exit-insertion-state)

;;; mark functions

(defmacro beyond-mark-helper (goto-beginning &rest goto-end)
  `(progn
     (push-mark)
     (save-excursion
       ,goto-beginning
       (push-mark)
       )
     ,@goto-end
     (activate-mark)))

(defun beyond-mark-char-forward ()
  (interactive)
  (beyond-mark-helper
   (identity nil)
   (forward-char)))

(defun beyond-mark-char-backward ()
  (interactive)
  (beyond-mark-helper
   (identity nil)
   (backward-char)))

(defun beyond-mark-end-of-line ()
  (interactive)
  (push-mark)
  (end-of-line)
  (activate-mark))

(defun beyond-mark-line (char-pos)
  (interactive "d")
  (beyond-mark-helper
   (progn
     (goto-char char-pos)
     (beginning-of-line))
   (end-of-line)
   (forward-char)))

(defcustom beyond-mark-text-units-functions
  '(beyond-mark-char-forward beyond-mark-char-backward
                             beyond-mark-end-of-line
                             beyond-mark-line next-line)
  "List of functions that are consecutively called to expand the
region further.")

(defvar-local beyond--mark-text-units-num-calls 0)
(defvar-local beyond--mark-text-units-start nil)

(defun beyond-mark-text-units (pos-begin pos-end)
  (interactive "r")
  (unless (eq last-command 'beyond-mark-text-units)
    (setq beyond--mark-text-units-num-calls 0
          beyond--mark-text-units-start (point)))
  (let ((num-marks (length mark-ring))
        (num-marks-global (length global-mark-ring))
        (mark-fun (or
                   (nth beyond--mark-text-units-num-calls beyond-mark-text-units-functions)
                   (car (last beyond-mark-text-units-functions)))))
    (goto-char beyond--mark-text-units-start)
    (call-interactively mark-fun)
    (setq last-command this-command)
    (setq beyond--mark-text-units-num-calls (1+ beyond--mark-text-units-num-calls))
    ))



(mapc (lambda (map-binding-list)
        (mapc (lambda (e) (define-key (car map-binding-list) (kbd (car e)) (cdr e)))
              (cdr map-binding-list)))
      `((,beyond-command-state-map
         . (("d" . kill-region)
            ("e" . er/expand-region)
            ("w" . beyond-mark-text-units)))
        (,beyond-motion-state-map
         . (
            ("F" . avy-goto-char)
            ("f" . avy-goto-char-in-line)
            ("j" . avy-goto-word-1)
            ;;("C-M-f" . avy-goto-char-timer)
            (";" . avy-goto-line)
            (":" . avy-goto-end-of-line)
            ("m" . pointless-goto-mark)
            ("i" . scroll-down-command)
            ("o" . scroll-up-command)
            ("O" . scroll-other-window)
            ("I" . scroll-other-window-down)
            ("\\" . ace-window)
            ))))

(eval-after-load 'org-mode
  (progn
    (defvar beyond-org-mode-motion-state-map (make-sparse-keymap))
    (push (cons 'org-mode beyond-org-mode-motion-state-map) beyond-state-map-alist)
    (define-key beyond-org-mode-motion-state-map (kbd "k") #'org-previous-visible-heading)
    (define-key beyond-org-mode-motion-state-map (kbd "k") #'org-next-visible-heading)))

(provide 'beyond)
;;; beyond ends here
