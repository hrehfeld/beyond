;;; beyond.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;; Author: Hauke Rehfeld
;; URL: https://github.com/hrehfeld/beyond
;;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (kolor "0.1.0"))
;;; Commentary:

;; This module sets up the emulation keymaps for each beyond state.
;; Functions to switch to each state is also provided.

;;; Code:

(require 'cl-macs)
(require 'dash)
(require 'advice)
(require 'subr-x)
(require 'kolor)

(defgroup beyond nil "Beyond" :group 'Editing)


(defconst beyond-debug-buffer "*beyond-debug*" "Name of beyond debug message buffer")

(defvar beyond-debug nil "A list of debug types to show messages for.")
(defun beyond-debug (type format-string &rest format-args)
  (cl-check-type type symbol)
  (cl-check-type format-string string)
  (cl-check-type format-args list)
  (when (memq type beyond-debug)
    (let ((buffer-name (buffer-name))
          (buf (get-buffer-create beyond-debug-buffer)))
      (buffer-disable-undo buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-max))
          (insert (apply #'format (append (list (concat "beyond (%s) [%s]: " format-string "\n") type buffer-name) format-args))))))))
;;(setq beyond-debug '(hook mode))
;;(setq-local beyond-debug '(hook mode))
;;(setq beyond-debug '(cursor))
;;(setq beyond-debug nil)
;;(beyond-debug hook t)

;; (defvar beyond/insert-command-history nil "History of changes in this insertion round.")
;; (defvar beyond/insert-command nil "Command which started the insertion.")
;; ;; (defvar beyond/insert-origin 0 "Point at start of insert state.")

(defvar-local beyond-input-method nil
  "The input method to activate
when going to insert state. (When leaving insert state the
input-method is reset to nil.)")


(defvar beyond-state-lighters
  nil
  "Alist of lighters for each state.")

;; manage keymaps
(defvar beyond-state-map-alist
  nil
  "List of cons pairs `(VAR . MAP)' that will enable beyond's state's keymaps.

Every state should be listed here as it is used by
`beyond--all-states'.

Earlier items overwrite bindings in later maps, so make sure
inheriting states appear before their parents - e.g.
beyond-command-state before beyond-motion-state.")
;;(kill-local-variable 'beyond-state-map-alist)

(defun beyond--all-states ()
  (mapcar #'car beyond-state-map-alist))

(defvar-local beyond--buffer-active-state nil "Active state.")
(defun beyond--buffer-active-state () beyond--buffer-active-state)

(defvar-local beyond--buffer-command-state nil "Active state.")
(defun beyond--buffer-command-state () beyond--buffer-command-state)

(defun beyond--set-buffer-command-state (state)
  (setq beyond--buffer-command-state state))

(defun beyond--state-set (state-sym state-val)
  "Enable or disable a state named `STATE-SYM'. When `ENABLE' is t, enable the state.

`STATE' can either be a minor-mode (if bound as a function symbol) or
just a variable."
  (beyond-debug 'mode "Switching variable for  %S to %S" state-sym state-val)
  (set state-sym state-val)
  (when (symbol-function state-sym)
    (beyond-debug 'mode  "calling function for %S" state-sym)
    ;; call minor mode if state has one
    (funcall state-sym (if state-val 1 -1))
    )
)


(defun beyond--sanity-check ()
  ;; only ever one state active
  (cl-assert (= 1 (seq-count #'symbol-value (beyond--all-states)))
             t "%S" (-zip-lists (beyond--all-states) (mapcar #'symbol-value (beyond--all-states))))
  (cl-assert (symbol-value beyond--buffer-active-state) t)
  ;; state's map is in the currently active keymaps
  (let* ((map-sym (intern (concat (symbol-name (beyond--buffer-active-state)) "-map")))
         (map (symbol-value map-sym)))
    (beyond-debug 'sanity "beyond--sanity-check: index of %S: %i" map-sym (cl-position map (current-active-maps)))
    (cl-assert (member map (current-active-maps)) t)
    )
  t)


;; hook infrastructure
(defvar beyond-state-switch-hook nil "Hook called when states are
switched, before state hooks are run. Functions are called with
the state symbol as the first and the previous state symbol as the second argument.

Do NOT put blocking stuff here.")
(defvar beyond-state-hooks nil
  "Alist of `(STATE-SYM . HOOK-SYM)' called when entering `STATE'")
(defvar beyond-state-exit-hooks nil
  "Alist of `(STATE-SYM . HOOK-SYM)' called when exiting `STATE'")

(defun beyond--run-state-hook (state &optional exit?)
  "Run state hooks for `STATE', and also parent state's hooks."
  (cl-check-type state symbol)
  (cl-loop while state
           do (progn
                (let ((hook (alist-get state (if exit? beyond-state-exit-hooks beyond-state-hooks))))
                  (when hook
                    (beyond-debug 'hook "beyond--run-state-hook: running hook %S" hook)
                    (run-hook-with-args hook)))
                (setq state (alist-get state beyond-state-parents)))
           ))


(defun beyond--is-insertion-state (state)
  ;; FIXME: use state-parents to check if a state inherits from insertion-state
  (eq state 'beyond-insertion-state))


(defun beyond--switch-state (state &optional cycle)
  (cl-assert (symbolp state) "state %S should be a symbol" state)
  (let ((old-state beyond--buffer-active-state))
    (unless (eq state old-state)
      (beyond-debug 'mode "Beyond switching from %S to %S in %s" old-state state (buffer-name))
      ;; disable previous state
      (when old-state
        (beyond--state-set old-state nil)
        (beyond--run-state-hook old-state t)
        )
      ;; save and activate new state
      (unless (beyond--is-insertion-state state)
        (beyond--set-buffer-command-state state))
      (setq beyond--buffer-active-state state)
      (beyond--state-set state t)
      (beyond--sanity-check)
      (beyond-debug 'hook "beyond--switch-state: running %S" state)
      (run-hook-with-args 'beyond-state-switch-hook state old-state)
      (beyond--run-state-hook state)
      ))
  (beyond--sanity-check)
  (beyond-debug 'mode "beyond--switch-state: beyond-states: %S %S %S" beyond-command-state beyond-insertion-state (beyond--buffer-command-state)))


(defun beyond-initial-state ()
  "Get the initial beyond state for current buffer."
  (let ((state (if (beyond--buffer-start-in-insertion-state?)
                   (beyond--find-insertion-buffer-state)
                 (beyond--find-command-buffer-state))))
    (if (listp state)
        (car state)
      state)))


(defun beyond--buffer-active-state-map ()
  (alist-get beyond--buffer-active-state beyond-state-map-alist))

(defmacro beyond-def-state-map (map-name &optional state parent-map supress?)
  "Define a keymap for a state.

`MAP-NAME' is the symbol for the keymap.
`STATE' is the symbol for the state.
`PARENT-MAP' is the symbol for the parent keymap.
`SUPRESS?' is a boolean to suppress keymap using `suppress-keymap'."
  (cl-check-type map-name symbol)
  (cl-check-type state symbol)
  (cl-check-type parent-map (or null symbol))
  (cl-assert (or (not parent-map) (boundp parent-map)) t)
  (let ((state-name (symbol-name state)))
    `(defvar ,map-name
       (let ((map (make-sparse-keymap)))
         ,@(let ((ops (list)))
             (when supress?
               (push '(suppress-keymap map t) ops))
             (when parent-map
               (push `(set-keymap-parent map ,parent-map) ops))
             ops)
         map)
       ,(concat "Keymap for `" state-name "'."))))


(defvar beyond-state-parents nil)


(cl-defmacro beyond-def-state (state doc &body minor-mode-body &key lighter-string parent-state parent-state-map major-modes &allow-other-keys)
  "`STATE' should be a symbol for the new state like `beyond-command-state'.

`PARENT-STATE' is a symbol for a parent state or `nil'.

`PARENT-STATE-MAP' is a symbol of the parent keymap, defaults to
`PARENT-STATE' with a \"-map\" suffix or nil if parent-state is
nil.

With `NO-VAR', don't create a state variable (mostly useful if
state is a minor mode and creates a variable anyways).

Do NOT use `minor-mode-body' for state transitions! It will cause
havoc.

With `MAJOR-MODES', set this state as the default state for the
given major modes in `beyond-mode-command-state-alist'.
"
  (cl-check-type state symbol)
  (cl-check-type parent-state (or null symbol))
  (let* ((minor-mode-body
         (let (filtered-body)
           (while minor-mode-body
             (let ((next (pop minor-mode-body)))
               (if (keywordp next)
                   (if (member next '(:lighter-string :parent-state :parent-state-map :major-modes))
                       (progn
                         (unless minor-mode-body
                           (error "Expected argument after keyword %S" next))
                         (pop minor-mode-body))
                     (error "Unexpected keyword %s" next))
                 (push next filtered-body))))
           (nreverse filtered-body)))
         (state-name (symbol-name state))
         (state-map-name (concat state-name "-map"))
         (state-map (intern state-map-name))
         (parent-state-map (or parent-state-map
                               (and parent-state (intern (concat (symbol-name parent-state) "-map")))))
         (enter-hook (intern (concat state-name "-enter-hook")))
         (exit-hook (intern (concat state-name "-exit-hook"))))
    `(progn
       ;; enter-hook
       (defvar ,enter-hook nil ,(format "Hook called when entering `%s', after the usual mode-hook.

Functions are called with the state symbol as the only argument" state-name))
       ;; exit-hook
       (defvar ,exit-hook nil ,(format "Hook called when exiting `%s', after the usual mode-hook.

Functions are called with the state symbol as the only argument" state-name))
       (add-to-list 'beyond-state-hooks '(,state . ,enter-hook))
       (add-to-list 'beyond-state-exit-hooks '(,state . ,exit-hook))

       (add-to-list 'beyond-state-lighters '(,state . ,lighter-string))

       ;; state variable
       (defvar-local ,state nil ,doc)
       ;; keymap
       (defvar ,state-map
         (let ((map (make-sparse-keymap)))
           ,@(when parent-state-map
               `((set-keymap-parent map ,parent-state-map)))
           map))

       ,(when minor-mode-body
         `(defun ,state (&optional mode-compat-arg-enabled)
           (beyond-debug 'mode "body of %S" ',state)
           ,@minor-mode-body
           ))

       ;; parent
       ,@(when parent-state
           `((setq beyond-state-parents (cons '(,state . ,parent-state) (assq-delete-all ',state beyond-state-parents)))))
       ;; enable keymap
       (add-to-list 'beyond-state-map-alist '(,state . ,state-map))

       ,@(cl-loop
          for major-mode in (if (listp major-modes) major-modes (list major-modes))
          collect
          `(add-to-list 'beyond-mode-command-state-alist '(,major-mode . ,state))))))

;;(add-to-list 'beyond-state-map-alist '(beyond-org-mode-command-state . beyond-command-state-map))
;;(setq beyond-state-map-alist nil)
;;(setq beyond-major-mode-states nil)



(defvar beyond-minimal-motion-state-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    map)
  "Base keymap that saves motion commands. Other special state maps inherit from this.")
(defvar beyond-motion-state-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map beyond-minimal-motion-state-map)
    map)
  "Base keymap that saves motion commands. Other special state maps inherit from this.")
;; (pp (macroexpand-1 '(beyond-def-state beyond-command-state "Beyond command mode where keys aren't self-inserting but run commands." "CMD" nil :parent-state-map beyond-motion-state-map)))
(beyond-def-state beyond-command-state
                  "Beyond command mode where keys aren't self-inserting but run commands instead."
                  :lighter-string "CMD"
                  :parent-state-map beyond-motion-state-map)
(beyond-def-state beyond-special-state
                  "Beyond special mode where keys aren't self-inserting but run commands instead while mimicing the original special mode."
                  :lighter-string "SPC"
                  :parent-state-map beyond-minimal-motion-state-map)

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

;; insert state is the only state that actually has a mode
(beyond-def-state beyond-insertion-state
                  "Beyond insertion mode where keys self-insert. Generally behaves very similar to normal emacs."
                  :lighter-string "INS"
                  (if beyond-insertion-state
                      (progn
                        (beyond--signal-point-read-only)
                        (activate-input-method beyond-input-method)
                        ;;(deactivate-mark)
                        (push-mark nil t)
                        )
                    (setq-local beyond-input-method current-input-method)
                    (deactivate-input-method)
                    ;; remove the mark set when entering insertion mode if cursor position didn't change
                    (when (= (mark t) (point))
                      ;;FIXME: for some reaons mark is still set
                      (beyond-debug 'mark "popping mark")
                      (pop-mark))
 ))

(defun beyond-enter-insertion-state () (interactive) (beyond--switch-state 'beyond-insertion-state))
(defun beyond-exit-insertion-state () (interactive) (beyond--switch-state (beyond--buffer-command-state)))
(defun beyond-next-state ()
  "Switch to the next beyond state in the state ring buffer for the current buffer."
  (interactive)
  (let ((states (beyond--find-command-buffer-state)))
    (when (listp states)
      (beyond--switch-state
       (let* ((cell (member (beyond--buffer-command-state) states))
              (next-state
               ;; if there's a following element use next state, otherwise take the first state
               (if (cdr cell) (cadr cell) (car states))))
         next-state)
       t))))





(defvar-local beyond--original-cursor nil "The cursor before switching to beyond-mode.")

(defvar beyond-cursor-type-default 'box
  "The default cursor type.")

(defcustom beyond-cursor-colors '((beyond-insertion-state . "#04f")
                                  (beyond-command-state . "#000"))
  "An alist of `CURSOR-COLOR' for each state.

See `set-cursor-color'."
  :group 'beyond :type 'sexp)
(defcustom beyond-cursor-types '((beyond-insertion-state . (hbar . 5))
                                  (beyond-command-state . box))
  "An alist of `CURSOR-TYPE' for each state.

See `set-cursor-color'."
  :group 'beyond :type 'sexp)

;;(toggle-debug-on-error)

(require 'color)

(defun beyond-color-hex-from-hsl (h s l)
  (apply #'color-rgb-to-hex (color-hsl-to-rgb h s l)))
;;(beyond-color-hex-from-hsl 0 0.5 0.5)

(defun beyond-color-for-string (str sat val)
	(let* ((hue (string-to-number (substring (sha1 str) 0 8) 16))
				 (hue (/ (mod hue 256) 255.0)))
		(funcall #'beyond-color-hex-from-hsl hue sat val)))
;; (beyond-color-for-string "test" 0.5 0.5)

(defun beyond-theme-is-light? ()
  (eq 'light (frame-parameter nil 'background-mode)))

(defcustom beyond-color-cursor-saturation-max 0.7
  "Max saturation for generated cursor colors."
  :group 'beyond :type 'sexp)
(defcustom beyond-color-cursor-saturation-min 0.4
  "Min saturation for generated cursor colors."
  :group 'beyond :type 'sexp)

(defcustom beyond-color-cursor-value-lightness-max 0.7
  "Max lightness for generated cursor colors."
  :group 'beyond :type 'sexp)
(defcustom beyond-color-cursor-value-lightness-min 0.4
  "Min lightness for generated cursor colors."
  :group 'beyond :type 'sexp)


(defun beyond-color-extract-hsl (color)
  (let* ((rgb (color-name-to-rgb color)))
    (apply #'color-rgb-to-hsl rgb)))
;; (beyond-color-extract-hsl "#f0f")

(defun beyond-update-cursor (&optional state old-state)
  "Update the cursor depending on the current beyond state.

Cursor will be set for all active states, overwriting
previous cursor settings.
"
  (with-current-buffer (window-buffer)
    (with-demoted-errors "beyond-update-cursor: %S"
      (let ((state (or state beyond--buffer-active-state)))
        (let ((type (alist-get state beyond-cursor-types))
              (color (alist-get state beyond-cursor-colors)))
          (let ((color (let* ((cursor-color
                               (beyond-color-extract-hsl
                                (cond ((kolor-is-emacs? (face-attribute 'cursor :background)))
                                      ((kolor-is-emacs? (face-attribute 'default :foreground)))
                                      (t (0.5 0.6 0.3)))))
                              (s (nth 1 cursor-color))
                              (l (nth 2 cursor-color))
                              (saturation-lightness
                               (list (min (max s beyond-color-cursor-saturation-min) beyond-color-cursor-saturation-max)
                                     (min (max l beyond-color-cursor-value-lightness-min) beyond-color-cursor-value-lightness-max))

                               ))
                         (beyond-debug 'cursor "cursor-color %S %S %S" cursor-color state saturation-lightness)
                         (if color
                             (apply #'beyond-color-hex-from-hsl (car (beyond-color-extract-hsl color)) saturation-lightness)
                           (apply #'beyond-color-for-string
                                  (cons
                                   (symbol-name state)
                                   ;;beyond-cursor-color-saturation
                                   ;;(if (beyond-theme-is-light?) beyond-cursor-color-value-light beyond-cursor-color-value-dark)))))
                                   saturation-lightness)
                                  ))
                         )))
            (beyond-debug 'cursor "beyond-update-cursor %S %S in %s (on frame %s) with %S"
                          type color (buffer-name)
                          (frame-parameter nil 'width)
                          state)
            (setq cursor-type (or type beyond-cursor-type-default))
            (set-cursor-color color))
          )))))
;; (beyond-update-cursor)




(defvar beyond--update-cursor-scheduled? nil "Is a cursor update scheduled?")

(defun beyond-update-cursor-scheduled ()
  "Run a scheduled cursor update."
  (when beyond--update-cursor-scheduled?
    (setq beyond--update-cursor-scheduled? nil)
    (beyond-update-cursor)))

(defun beyond-update-cursor-schedule (&optional state old-state)
  "Schedule a cursor update."
  (setq beyond--update-cursor-scheduled? t)
  (run-with-idle-timer beyond-cursor-mode-update-time nil #'beyond-update-cursor-scheduled))


(defcustom beyond-cursor-mode-idle-time 0.1
  "Time in seconds to wait before updating the cursor regularly."
  :group 'beyond :type 'sexp)

(defcustom beyond-cursor-mode-update-time 0.01
  "Time in seconds to wait before updating the cursor after a command."
  :group 'beyond :type 'sexp)



(defvar beyond--cursor-mode-timer nil "Timer for updating the cursor.")

(define-minor-mode beyond-cursor-mode
  "Minor mode to update the cursor depending on the current beyond state."
  :lighter nil
  :global t
  (if beyond-cursor-mode
      (progn
        (add-hook 'buffer-list-update-hook #'beyond-update-cursor-schedule)
        (add-hook 'beyond-state-switch-hook #'beyond-update-cursor-schedule)
        (add-hook 'post-command-hook #'beyond-update-cursor-schedule)
        (setq beyond--cursor-mode-timer (run-with-idle-timer beyond-cursor-mode-idle-time t #'beyond-update-cursor-scheduled))
        )
    (remove-hook 'buffer-list-update-hook #'beyond-update-cursor-schedule)
    (remove-hook 'beyond-state-switch-hook #'beyond-update-cursor-schedule)
    (remove-hook 'post-command-hook #'beyond-update-cursor-schedule)
    (cancel-timer beyond--cursor-mode-timer)
    (setq beyond--cursor-mode-timer nil)
    ))


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

(defun beyond-shell-mode-p ()
  "Is the `major-mode' any of the shell modes?"
  (derived-mode-p 'comint-mode 'eshell-mode 'term-mode 'vterm-mode))


(defcustom beyond-mode-command-state-alist
  (append
   '((org-agenda-mode . beyond-special-state))
   (mapcar (lambda (mode) `(,mode . beyond-special-state))
           '(magit-blame-mode
             Buffer-menu-mode
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
             pass-mode
             view-mode)))
  "A alist of `(MODE . BEYOND-STATE)' to trigger a specific command state for that mode.

It actually just checks for a variable of that name to see if that condition triggers."
  :group 'beyond :type '(repeat symbol))

(defun beyond--major-mode-is-special ()
  (when (and (eq (get major-mode 'mode-class) 'special)
                         (not (beyond-shell-mode-p)))
                'beyond-special-state))

(defcustom beyond-command-state-selectors
  (list ;; see major-mode conventions
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html
   #'beyond--major-mode-is-special
   )
  "A list of selector functions to return a command/special state for current buffer.

Each entry is called as a function with no arguments and should
return a beyond state to switch to or nil."
  :group 'beyond :type '(list symbol))

(defcustom beyond-insertion-state-selectors
  nil
  "A list of selector functions to return a insertion state for current buffer.

Each entry is called as a function with no arguments and should
return a beyond state to switch to or nil."
  :group 'beyond :type '(list symbol))

(defun beyond--find-buffer-state (state-selectors mode-state-alist default)
  "Return most appropriate state for current buffer.

First, call each nullary function in `STATE-SELECTORS' and return result if non-nil,
return that.

Otherwise, check any of `local-minor-modes' against `MODE-STATE-ALIST' and return result if non-nil.

Otherwise, check `major-mode' against `MODE-STATE-ALIST' and return result if non-nil.

Last, return `DEFAULT'."
  (or (cl-loop for state-selector in state-selectors
               with state = nil
               do (setq state (funcall state-selector))
               if state return state)
      (cl-loop for minor-mode in local-minor-modes
               with state = nil
               do (setq state (alist-get minor-mode mode-state-alist))
               if state return state)
      (alist-get major-mode mode-state-alist)
      default))

(defun beyond--find-command-buffer-state ()
  (beyond--find-buffer-state beyond-command-state-selectors beyond-mode-command-state-alist 'beyond-command-state))

(defun beyond--find-insertion-buffer-state ()
  (beyond--find-buffer-state beyond-insertion-state-selectors beyond-mode-insertion-state-alist 'beyond-insertion-state))

(defcustom beyond-mode-insertion-state-alist
  nil
  "A alist of `(MODE . BEYOND-STATE)' to trigger a specific insertion state for that mode.

It actually just checks for a variable of that name to see if that condition triggers."
  :group 'beyond :type '(repeat symbol))

(defcustom beyond-start-in-insertion-state?-modes '(message-mode vterm-mode ediff-mode) "A list of modes that trigger insertion state when enabling `beyond-mode'.")

(defcustom beyond-start-in-insertion-state?-selectors
  (list
   (lambda () (when (string-match "COMMIT_EDITMSG" (buffer-name))
                'beyond-insertion-state)))
  "A alist of selector functions to return an insertion state for current buffer.

Each entry is called as a function with no arguments and should
return a beyond state to switch to or nil."
  :group 'beyond :type '(list symbol))

(defun beyond--buffer-start-in-insertion-state? ()
  (beyond--find-buffer-state
     beyond-start-in-insertion-state?-selectors
     (mapcar (lambda (mode) (cons mode mode)) beyond-start-in-insertion-state?-modes) nil))


;;; Initialisation and activation

;; have an extra indirection because emulation-mode-map-alists needs direct references to keymaps,
;; not symbols of keymap-variables.
(defvar beyond-state-map-alist-for-emulation-mode-map-alists nil)

(defun beyond-lighter-string ()
  "Return the lighter string appropriate for the current state."
  (alist-get beyond--buffer-active-state beyond-state-lighters)
  )


(define-minor-mode beyond-mode
  "Minor mode for setting up command state in a single buffer."
  :lighter (:eval (concat " by:" (beyond-lighter-string)))
  :keymap nil
  (if beyond-mode
      (unless (minibufferp)
        ;; save original cursor
        (setq beyond--original-cursor (cons cursor-type (frame-parameter nil 'cursor-color)))
        (cl-delete 'beyond-state-map-alist emulation-mode-map-alists)
        (cl-pushnew 'beyond-state-map-alist-for-emulation-mode-map-alists emulation-mode-map-alists)
        (cl-delete 'beyond--define-key--state-conditional--minor-mode-map-alist emulation-mode-map-alists)
        (cl-pushnew 'beyond--define-key--state-conditional--minor-mode-map-alist emulation-mode-map-alists)
        ;; update the emulation keymaps everytime so that any change is catched by turning it off
        ;; and on again
        (setq beyond-state-map-alist-for-emulation-mode-map-alists
              (cl-loop for (mode-sym . map-sym) in beyond-state-map-alist
                       collect (cons mode-sym (symbol-value map-sym))))
        ;; (unless (memq 'beyond/after-change-hook after-change-functions)
        ;;   (push 'beyond/after-change-hook after-change-functions))
        (setq beyond--buffer-command-state (beyond--find-command-buffer-state))
        (beyond--switch-state
         (beyond-initial-state))
        (beyond-debug 'mode "enabled %S" beyond-mode))
    (setq cursor-type (car beyond--original-cursor))
    (set-frame-parameter nil 'cursor-color (cdr beyond--original-cursor))
    (setq beyond--original-cursor (list cursor-type (frame-parameter nil 'cursor-color)))
    (beyond-debug 'mode "disabled"))
  )

;; make modeline lighter update after each state switch
(add-hook 'beyond-state-switch-hook (lambda (&optional state old-state) (force-mode-line-update)))


(defun beyond-mode--initialize ()
  "Setup beyond in the current buffer.
Should only be used to
initialize `beyond-mode' from the globalized minor-mode
`beyond-global-mode'.  It is called whenever beyond is enabled in a buffer
for the first time or when beyond is active and the `major-mode' of
the buffer changes."
  (beyond-debug 'mode "beyond-global-mode enable %S" (buffer-name))
  (beyond-mode 1))

;;;###autoload (autoload 'beyond-global-mode "beyond" "Toggle beyond in all buffers" t)
(define-globalized-minor-mode beyond-global-mode beyond-mode beyond-mode--initialize :predicate t :group 'beyond)


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


;; key helpers

;;;###autoload
(defmacro beyond-def-key-repeater (key command &optional repeat-command)
  (let ((name (intern (concat "beyond-def-key-repeater--" key "--" (symbol-name command))))
        (repeat-command (or repeat-command command)))
    `(defun ,name ()
      (interactive)
      (call-interactively #',command)
      (set-transient-map (let ((map (make-sparse-keymap)))
                           (define-key map ,key #',repeat-command)
                           map)
                         t))))






;;;###autoload
(defun beyond-quote-keypress (key)
  "Execute the command which would be bound to the KEY if beyond was not enabled."
  (interactive (list (read-char))) ;; use read-char so that multiple-cursors advice kicks in.
  (let* ((keymap (make-composed-keymap
                  (let ((active-beyond-keymaps
                         (mapcar #'symbol-value
                                 (seq-concatenate 'list
                                                  (mapcar #'cdr beyond-state-map-alist)
                                                  (seq-mapcat #'cdr beyond--define-key--state-conditional--map-alist)))))
                    (seq-filter (lambda (keymap) (not (memq keymap active-beyond-keymaps)))
                                (current-active-maps)))))
         (command (lookup-key keymap (vector key)))
         (command (or (command-remapping command nil keymap)
                  command)))
    (setq last-command-event key)
    (message "Executing the command %S bound to %c" command key)
    (call-interactively command nil [key])))


(defun beyond--swap-control-bit (event)
  "Swap the control 'bit' in EVENT, unless C-c <event> is a prefix reserved for modes."
  (interactive (list (read-key)))
  (cond
   ((memq event '(9 13 ?{ ?} ?\[ ?\] ?$ ?& ?= ?< ?> ?: ?\; ?/ ?? ?. ?, ?' ?\" )) event)
   ((<= event 27) (+ 96 event))
   ((not (eq 0 (logand (lsh 1 26) event))) (logxor (lsh 1 26) event))
   (t (list 'control event))))

;;FIXME: make this generic and add wrapper command specifically for C-c
;;;###autoload
(defun beyond--read-key-sequence-control-swapped (arg)
  "Input a key sequence, prepending C- to each key (unless such
key is already reserved for minor mode, see
`beyond--swap-control-bit'), and run the command bound to that
sequence."
  (interactive "P")
  (let ((keys '((control c)))
        (binding (key-binding (kbd "C-c")))
        (key-vector (kbd "C-c"))
        (prompt "C-c-"))
    (while (and binding
                (or (eq binding 'mode-specific-command-prefix)
                    ;; if using universal prefix, the above will happen.
                    (not (commandp binding))))
      (let ((key (read-key (format "%s" prompt))))
        (if (eq key ?h) (describe-bindings key-vector) ;; h -> show help
          (push (beyond--swap-control-bit key) keys)
          (setq key-vector (vconcat (reverse keys)))
          (setq prompt (key-description key-vector))
          (setq binding (key-binding key-vector)))))
    (cond
     ((not binding) (user-error "No command bound to %s" prompt))
     ((commandp binding)
      (let ((current-prefix-arg arg)) (call-interactively binding)))
     (t (user-error "Key not bound to a command: %s" binding)))))


(defvar taps-timeout-default 0.15 "Default timeout for `taps-timeout-keypress' when not overriding specifically.")

(defun taps-timeout-keypress (command alternative-command-keymap timeout prompt &rest format-args)
  "Execute `COMMAND' if no further keysequence found in `ALTERNATIVE-COMMAND-KEYMAP' is pressed within `TIMEOUT' seconds. Otherwise execute command found there.

`PROMPT' will be used as the prompt after `format-args' are applied to it using `format'."
  (let ((prompt (apply #'format prompt format-args))
        (timeout (or timeout 0.15))
        cancelled)
    (cl-check-type timeout number)
    (let* ((idle-timer (run-with-timer timeout nil (lambda () (setq cancelled t))))
           key-sequence
           (command
            (or
             ;; maybe read a complete sequence found in keymap
             (cl-loop with event = nil
                      with command = nil
                      while (not cancelled)
                      ;; TODO: use read-key for non-char key events?
                      do (setq event (read-event prompt t 0.05))
                      if event
                      do (progn
                           (setq key-sequence (vconcat key-sequence (list event))
                                 command (lookup-key alternative-command-keymap key-sequence 'accept-default-binding))
                           ;; (message "read event %S %S" event command)
                           (cl-assert (not (numberp command)) t "Got a long keysequence for some reason!"))
                      if command
                      return command)
             command)))
      (call-interactively command t (vconcat (this-command-keys) key-sequence))
      )))

(defun taps--command-name (base-command)
  (cl-check-type base-command symbol)
  (concat "taps--command--" (symbol-name base-command)))
(defun taps--map-name (base-command)
  (cl-check-type base-command symbol)
  (concat (taps--command-name base-command) "-map"))

(defun taps--command-sym (base-command)
  (intern (taps--command-name base-command)))
(defun taps--map-sym (base-command)
  (intern (taps--map-name base-command)))

(defun taps--command (base-command command-name timeout tap-map)
  (interactive)
  (cl-check-type command-name symbol)
  (cl-check-type tap-map symbol)
  `(defun ,command-name ()
     (interactive)
     (taps-timeout-keypress (quote,base-command) ,tap-map ,timeout ,(format "Continue command %S?" base-command))))

(defun taps--map (map-sym command-sym)
  (interactive)
  (cl-check-type command-sym symbol)
  (cl-check-type map-sym symbol)
  `(defvar ,map-sym (make-sparse-keymap) ,(format "Double-tap keymap for `%s'." (symbol-name command-sym))))

(cl-defmacro taps-def-taps (base-command keys-commands &key command-name timeout tap-map base-map base-key)
  "TODO
Each `command' in `keys-commands' can be a function symbol, a lambda expression or a form, which will be wrapped in a lambda expression."
  (interactive)
  (cl-check-type base-command symbol)
  (cl-check-type command-name (or null symbol))
  (cl-check-type tap-map (or null symbol))
  (cl-check-type keys-commands list)
  (let* ((command-name (or command-name (taps--command-sym base-command)))
         (has-tap-map? tap-map)
         (tap-map (or tap-map (taps--map-sym base-command)))
         (tap-map-def (unless has-tap-map? (taps--map tap-map command-name)))
         (tap-command (taps--command base-command command-name timeout tap-map))
         (command-sym (gensym "taps--command-"))
         (bind (when (and base-map base-key)
                 `(define-key ,base-map ,base-key ,command-sym)))
         )
    `(progn
       ,tap-map-def
       ,@(cl-loop for (key . command) in keys-commands
                  collect (progn
                            (cl-check-type command (or list symbol))
                            (cl-assert command)
                            (let ((command (if (symbolp command)
                                               command
                                             (cond ((eq (car command) 'lambda) command)
                                                   (t `(lambda () (interactive) ,command)))
                                             )))
                              `(define-key ,tap-map ,key (quote ,command)))))
       (let ((,command-sym ,tap-command))
         ,bind
         ;;just return the command
         ,command-sym))))

(cl-defmacro taps-def-double-tap-key (base-map key base-command double-tap-command &key command-name timeout tap-map)
  "TODO"
  (interactive)
  (list #'taps-def-taps base-command (list (cons key double-tap-command))
        :command-name command-name :timeout timeout :tap-map tap-map
        :base-map base-map :base-key key
        ))

;;(macroexpand-1 '(taps--command pointless-jump-mark beyond-def-quick-key-command--pointless-jump-mark 0.5 beyond-def-quick-key-command--pointless-jump-mark-map))
;; (macroexpand '(taps-def-tap pointless-jump-mark :name beyond-def-quick-key-command--pointless-jump-mark :timeout 0.5 :tap-map nil))
;; (macroexpand '(taps-def-taps pointless-jump-mark (("m" . exchange-point-and-mark)) :timeout 0.5 :base-map beyond-command-state :base-key "n"))
;;(taps-def-taps pointless-jump-mark (("m" . exchange-point-and-mark)) :timeout 0.5)

(defmacro beyond-def-taps-key (map key base-command double-tap-command &optional timeout)
  (let* ((double-tap-map (taps--map-name base-command))
         (keys (listify-key-sequence key))
         (key (car (last keys)))
         )
    `(let ((command (taps-def-taps base-command (,key . ,double-tap-command) :timeout ,timeout))
           (define-key ,(intern double-tap-map) command)
           (define-key ,map command)))))

;;(macroexpand-1 '(beyond-def-double-tap-key 'beyond-command-state-map "k" foo bar))


;; motion commands

(defun beyond-call-interactively-with-last-command (command)
  "Call `command' interactively, but set `last-command' to `command' if `last-command' is `this-command'.

The main use of this is where the called function checks if it is
called several times in succession and modifies behavior then.
`next-line''s handling of `temporary-goal-column' is an obvious
example."
  (let ((last-command (if (eq last-command this-command) command last-command)))
    (call-interactively command)
    ))

(defun beyond-next-line ()
  "Like `next-line', but sets `last-command' to `next-line', so `temporary-goal-column' behavior is correct.

`last-command' is checked by `line-move'."
  (interactive)
  (beyond-call-interactively-with-last-command #'next-line))

(defun beyond-previous-line ()
  "Like `previous-line', but sets `last-command' to `previous-line', so `temporary-goal-column' behavior is correct.

`last-command' is checked by `line-move'."
  (interactive)
  (beyond-call-interactively-with-last-command #'previous-line))



;; editing commands

;; TODO: delete space backwards if in front of non-space
;; TODO: smart-hungry
(defun beyond-kill-region-or-line ()
  "Kill region if active, line if at line end, or cycle space."
  (interactive)
  (if (use-region-p)
      (kill-region nil nil t)
    (let ((point (point)))
      (if (eolp)
          (prog1
              (kill-line)
            (message "killed line"))
        ;; kill ws first
        (cycle-spacing)))))

(defun beyond-smart-hungry-delete ()
  "Kill region if active, line if at line end, or cycle space."
  (interactive)
  (if (use-region-p)
      (kill-region nil nil t)
    (let ((forward-skip-syntax " ")
          (backward-skip-syntax " ")
          (insert-char " "))
      (let ((point (point))
            (skip-forward-pos (save-excursion (skip-syntax-forward forward-skip-syntax) (point)))
            (skip-backward-pos (save-excursion (skip-syntax-backward backward-skip-syntax) (point))))
        (cond ((not (equal (point) skip-forward-pos))
               (kill-region (point) skip-forward-pos))
              ((not (equal (point) skip-backward-pos))
               (kill-region (point) skip-backward-pos))
              ((eolp)
               (prog1
                   (kill-line)
                 (message "killed line")))
              ((bolp)
               (backward-delete-char 1))
              (t (insert insert-char))
              )))))

(defvar beyond-syntax-pairs '(("(" ")") ("[" "]") ("<" ">") ("'" "'") ("\"" "\"") ("{" "}")))

(defun beyond--dilate-region (beginning-repl end-repl)
  ""
  (save-excursion (goto-char (region-beginning))
                  (insert beginning-repl))
  (save-excursion (goto-char (region-end))
                  (insert end-repl)))

(defun beyond--syntax-pair-choose (prompt)
  (let* ((keys (mapcar #'car beyond-syntax-pairs))
         (prompt (format prompt (string-join keys " ")))
         ;; fixme: if pairs start with the same char, we have conflicts
         (pair-char (read-char-from-minibuffer
                     prompt
                     (mapcar #'string-to-char keys)))
         (pair-str (char-to-string pair-char))
         (pair (car (seq-filter (lambda (pair) (string-prefix-p pair-str (car pair)))
                                beyond-syntax-pairs))))
    pair))

(defun beyond--erode-region (beginning-repl end-repl)
  ""
  (let* ((backward-pos (if (use-region-p) (region-beginning) (point)))
        (forward-pos (if (use-region-p) (region-end) (point)))
        (beginning backward-pos)
        (end forward-pos))
    (save-excursion
      (goto-char backward-pos)
      (when (search-backward beginning-repl)
        "ttest \"foo"
        (delete-region (match-beginning 0) (match-end 0))
        (setq beginning (point))
      (goto-char forward-pos)
      (when (search-forward end-repl)
        (delete-region (match-beginning 0) (match-end 0))
        (setq end (point)))))
    (list beginning end)))

(defun beyond-erode-region ()
  ""
  (interactive)
  (let* ((pair (beyond--syntax-pair-choose "Erode region by pair %S: ")))
    (apply #'beyond--erode-region pair)))

(defun beyond-dilate-region ()
  ""
  (interactive)
  (let* ((pair (beyond--syntax-pair-choose "Dilate region by pair %S: ")))
    (apply #'beyond--dilate-region pair)))

(defun beyond-syntax-pair-exchange ()
  ""
  (interactive)
  (let* ((pair (beyond--syntax-pair-choose "Erode region by pair %S: "))
         (pair-new (beyond--syntax-pair-choose "Dilate region by pair %S: ")))
    (cl-multiple-value-bind (beginning end) (apply #'beyond--erode-region pair)
      (push-mark end t t)
      (goto-char beginning)
      (apply #'beyond--dilate-region pair-new))))

(defun beyond-mark-surround ()
  "Kill region if active, line if at line end, or cycle space."
  (interactive)
  (if (use-region-p)
      (kill-region nil nil t)
    (let ((forward-skip-syntax " ")
          (backward-skip-syntax " ")
          (insert-char " "))
      (let ((point (point))
            (skip-forward-pos (save-excursion (skip-syntax-forward forward-skip-syntax) (point)))
            (skip-backward-pos (save-excursion (skip-syntax-backward backward-skip-syntax) (point))))
        (cond ((not (equal (point) skip-forward-pos))
               (kill-region (point) skip-forward-pos))
              ((not (equal (point) skip-backward-pos))
               (kill-region (point) skip-backward-pos))
              ((eolp)
               (prog1
                   (kill-line)
                 (message "killed line")))
              ((bolp)
               (backward-delete-char 1))
              (t (insert insert-char))
              )))))




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

(defun beyond-toggle-mark ()
  (interactive)
  ;;(message "mark was %s at %s" (if mark-active "active" "inactive") (mark))
  ;; (let ((overlay (make-overlay (mark) (1+ (mark)))))
  ;;   (overlay-put overlay 'face '(:background "red")))
  (if mark-active
      (progn
        (deactivate-mark t)
        (cl-assert (not mark-active)))
    (activate-mark)
    (cl-assert mark-active))
  (redisplay)
  )


(defun beyond-move-first-moving (point &rest move-funs)
  (interactive "d")
  ;; TODO: make this generate all movement points, check if on any of them, and then go to the next
  (while (and move-funs (= (point) point))
    (message "point %S (point) %S -- %S" point (point) (car move-funs))
    (funcall (car move-funs))
    (setq move-funs (cdr move-funs)))
  )

(defun beyond-back-to-indentation-or-beginning-of-line (point)
  (interactive "d")
  (beyond-move-first-moving point
                            #'back-to-indentation
                            #'beginning-of-line))


(defun beyond-kill-non-bol-trailing-whitespace ()
  "Kill whitespace after point unless at beginning of line."
  (save-excursion
    (when (not (bolp))
      (let ((insert-point (point)))
        (skip-syntax-forward " " (line-end-position))
        (when (eolp) (delete-region insert-point (point)))))))

(add-hook 'beyond-insertion-state-enter-hook #'beyond-kill-non-bol-trailing-whitespace)
;;(remove-hook 'beyond-insertion-state-enter-hook #'beyond-kill-non-bol-trailing-whitespace)

(defvar beyond-easy-bindings nil)



(defun beyond-undo () (interactive)
       (let* ((prefix current-prefix-arg)
              (current-prefix-arg (list (if (use-region-p) 4 prefix))))
         (call-interactively #'undo)))

(defvar beyond--define-key--state-conditional--map-alist nil
  "Alist of `(STATE KEYMAP-SYMBOL ...)' that should be turned on and off in `emulation-mode-map-alists' on state switches.")
;; (setq beyond--define-key--state-conditional--map-alist nil)

(defvar-local beyond--define-key--state-conditional--minor-mode-map-alist nil
  "Alist like `minor-mode-map-alist' but for conditional bindings. Should be added to `emulation-mode-map-alists'")
;; (setq beyond--define-key--state-conditional--minor-mode-map-alist nil)

(defun beyond--minor-mode-from-keymap (keymap)
  "Return the minor mode symbol for KEYMAP from `minor-mode-map-alist'. Return nil if not found."
  (cl-loop for (minor-mode . map) in minor-mode-map-alist
           if (equal map keymap)
           return minor-mode))

(defun beyond--define-key--state-conditional--update (state)
  (cl-check-type state symbol)
  ;; reset first
  ;;(message "beyond--define-key--state-conditional--update RESETTING %S" state)
  (cl-assert (boundp 'beyond--define-key--state-conditional--minor-mode-map-alist))
  (setq beyond--define-key--state-conditional--minor-mode-map-alist nil)
  (let ((state-active? (symbol-value state))
        (conditional-keymap-symbols (alist-get state beyond--define-key--state-conditional--map-alist)))
    ;; add back relevant keymaps
    (cl-loop for conditional-keymap-sym in conditional-keymap-symbols
             do
             (cl-check-type conditional-keymap-sym symbol)
             ;; first, retrieve the minor mode name by looking up the keymap parent in minor-mode-map-alist
             (let* ((conditional-keymap (symbol-value conditional-keymap-sym))
                    (conditional-keymap-parent (keymap-parent conditional-keymap))
                    (conditional-minor-mode-sym (beyond--minor-mode-from-keymap conditional-keymap-parent))
                    (conditional-minor-mode-active (symbol-value conditional-minor-mode-sym))
                    (minor-mode-map-entry (cons conditional-minor-mode-sym conditional-keymap)))
               (progn;;if conditional-minor-mode-active
                 (if state-active?
                     (progn (push minor-mode-map-entry beyond--define-key--state-conditional--minor-mode-map-alist)
                            ;;(message "beyond--define-key--state-conditional--update ACTIVE: %S %S" conditional-minor-mode-sym conditional-minor-mode-active)
                            )
                   ;; (message "beyond--define-key--state-conditional--update NOT ACTIVE: %S %S" conditional-minor-mode-sym conditional-minor-mode-active)
                   )
                 ))))
  ;;(message "beyond--define-key--state-conditional--minor-mode-map-alist %S %S" (buffer-name (current-buffer)) beyond--define-key--state-conditional--minor-mode-map-alist)
  )

(defun beyond--define-key--state-conditional--switch-state-hook (state old-state)
  (cl-check-type state symbol)
  ;; ;; remove other bindings first
  ;; (when old-state
  ;;   (beyond--define-key--state-conditional--update old-state))
  (beyond--define-key--state-conditional--update state))
(add-hook 'beyond-state-switch-hook #'beyond--define-key--state-conditional--switch-state-hook)
;;(remove-hook 'beyond-state-switch-hook #'beyond--define-key--state-conditional--switch-state-hook)

(defun beyond--define-key--state-conditional--update-hook ()
  (when beyond--buffer-active-state
    (beyond--define-key--state-conditional--update beyond--buffer-active-state)))
(add-hook 'window-state-change-hook #'beyond--define-key--state-conditional--update-hook)
;;(remove-hook 'window-state-change-hook #'beyond--define-key--state-conditional--update-hook)
(add-hook 'post-command-hook #'beyond--define-key--state-conditional--update-hook)
;;(remove-hook 'post-command-hook #'beyond--define-key--state-conditional--update-hook)

(defun beyond--define-key--state-conditional--keymap-name (keymap state)
  "Return the auto-generated name for the conditional keymap for `KEYMAP' when `STATE' is active."
  (intern (format "beyond--define-key--state-conditional--keymap--%s--%s" state keymap)))

(defun beyond-define-key (keymap-sym key command &optional remove state)
  "Like `define-key', but also setup the keybinding to be toggled when `STATE' is entered/exited.

`KEYMAP-SYM', `KEY', `COMMAND' are the same as for `define-key'.

With `REMOVE', just remove the keybinding and hook setup."
  (cl-check-type keymap-sym symbol)
  ;;(cl-check-type key key-valid)
  (cl-check-type state (or null symbol) "is not a symbol or nil %S")
  (if remove
      ;; just remove from mapping
      (beyond--define-key-state-conditional-remove keymap-sym key state)
    ;; otherwise add the keybinding
    (when state
      ;;(message "beyond-define-key before wrapper %S %S %S %S" keymap-sym key command state)
      ;; create keymap for state + keymap-sym
      (let ((conditional-keymap-sym (beyond--define-key--state-conditional--keymap-name keymap-sym state)))

        (set conditional-keymap-sym
             (let ((map (or
                         ;; either already exists
                         (and (boundp conditional-keymap-sym) (symbol-value conditional-keymap-sym))
                         ;; or create new map
                         (make-sparse-keymap))))
               ;; set parent either way (shouldn't be necessary, but useful during dev)
               (set-keymap-parent map (symbol-value keymap-sym))
               map))
        (cl-pushnew conditional-keymap-sym (alist-get state beyond--define-key--state-conditional--map-alist))
        (setq keymap-sym conditional-keymap-sym)))
    ;; now define the key
    (let ((command
           ;; check if command needs to be converted to lambda
           ;; is a list and not a lambda
           (if (and (listp command)
                            (not (ad-lambda-p command)))
                       (let ((lambdafied `(lambda ()
                                            (interactive)
                                            ,command)))
                         (progn ;;(message "%S => %S" command lambdafied)
                                (eval `(lambda ()
                                         (interactive)
                                         ,command))))
                     command)))
      ;; define key either in original keymap or state conditional keymap
      (define-key (symbol-value keymap-sym) key command remove))))


(defun beyond--parse-define-key-def (keymap-sym key def)
  "Check DEF and convert it to something that can be bound if necessary.

DEF can be:
- a symbol to a keymap, which will return the keymap,
- a keymap, which will just return the keymap,
- a cons/non-empty list, which will be checked if it's a tap definition:
  - a tap definition looks like:
    `(:tap base-command (key1 command1) (key2 command2) ...)',
  - or otherwise will be converted to a lambda command."
  (cond
   ;; def is a symbol to a keymap
   ((and (symbolp def)
         (boundp def)
         (keymapp (symbol-value def)))
    ;; return the keymap
    (symbol-value def))
   ;; def is a keymap
   ((keymapp def)
    ;; just return the keymap
    def)
   ;; def is a cons/non-empty list
   ((consp def)
    ;; check if it's a tap definition
    ;; a tap definition is `(:tap base-command (key1 command1) (key2 command2) ...)'
    (if (eq (car def) :tap)
        (let ((base-command (cadr def))
              (commands (cddr def)))
          (eval `(taps-def-taps
                  ,base-command
                  ,commands
                  :timeout ,(if (length< commands 2) 0.3 0.5)
                  :base-map ,keymap-sym :base-key ,key
                  ) t))
      ;; otherwise construct lambda
      `(lambda () (interactive) ,def)))
   ;; warn otherwise
   (t
    (progn
      (cl-check-type def function "is not a function")
      def))))

(defun beyond--define-key-add-desc (def desc)
  (if (and def desc) (cons (if (listp desc) (eval desc t) desc) def) def))

(defun beyond-easy-bind (&optional bindings)
  (interactive)
  (let ((bindings (or bindings beyond-easy-bindings)))
    (cl-loop for (keymap-sym . bindings) in bindings
             do
             (let ((map (if (keymapp keymap-sym) keymap-sym (symbol-value keymap-sym))))
               (cl-loop for (key command desc) in bindings
                        do
                        (progn
                          (if command
                              (let* ((command (beyond--parse-define-key-def keymap-sym key command))
                                     (command (beyond--define-key-add-desc command desc)))
                                (define-key map (kbd key) command))
                            ;; handle nil as command as an unbind, because (define-key ... nil) doesn't
                            ;; work for some reason
                            ;;(unbind-key (kbd key) map)
                            (define-key map (kbd key) nil 'remove)
                            )
                          ))))))


(defun beyond--parse-keywords-and-rest (args)
  "Parse ARGS into a list of keywords and the associated value after the keyword, and the rest of the list elements.

Return value is a list of the form (:rest REST . ARGS)."
  (cl-check-type args list)
  (let (rest in-keyword)
    (cl-loop
     for arg in args
     do
     (if in-keyword
         (setq in-keyword nil)
       (if (keywordp arg)
           (setq in-keyword arg)
         (push arg rest)))
     finally (return (cons :rest (cons (nreverse rest) args))))))

(defun beyond-bind (map-bindings)
  (interactive)
  (cl-loop
   for map-binding in map-bindings
   do
   (let ((keymap (plist-get map-binding :map))
         (state (plist-get map-binding :state))
         (bindings (plist-get (beyond--parse-keywords-and-rest map-binding) :rest)))
     (cl-check-type keymap symbol)
     (cl-check-type state (or null symbol))
     (cl-loop
      for binding in bindings
      do
      (let* ((binding (beyond--parse-keywords-and-rest binding))
             (desc (plist-get binding :desc))
             (remove (plist-get binding :remove))
             (rest (plist-get binding :rest))
             (key (pop rest))
             (command (pop rest))
             (desc (or desc (when rest (pop rest))))
             (define-key-args (list keymap (kbd key) command)))
        ;;(message "binding %S %S %S %S %S: %S" keymap key command remove state binding)
        (beyond-define-key keymap (kbd key) command remove state)
        ))))
  t)


;; make insert mode automatically stop after idle time

(defvar beyond-insert-idle-time 2 "Idle time until returning from insert-state.")
(defvar-local beyond-insertion-state-exit-on-idle-timer nil "The current insert timer object.")

(defun beyond-insertion-state-exit-on-idle-action (buffer)
  (cl-check-type buffer buffer)
  (beyond-debug 'hook "exit-on-idle action")
  (when beyond-insertion-state
    (with-current-buffer buffer
      ;; for some reason boon-quit doesn't quit the state
      (call-interactively #'beyond-exit-insertion-state)))
  ;; clean up the timer var
  (setq beyond-insertion-state-exit-on-idle-timer nil))


;; before updating cursor, etc.
(defun beyond-insertion-state-exit-on-idle ()
  ;; (when beyond-insertion-state-exit-on-idle-timer
  ;;   (cancel-timer beyond-insertion-state-exit-on-idle-timer))
  (beyond-debug 'hook "exit-on-idle start")
  (let ((buf (current-buffer)))
    ;; assume after 10 seconds that user forgot he's in insert state
    (if (sit-for 10)
        (beyond-insertion-state-exit-on-idle-action buf)
      (setq beyond-insertion-state-exit-on-idle-timer
            (run-with-idle-timer beyond-insert-idle-time nil (lambda () (beyond-insertion-state-exit-on-idle-action buf))))))
  ;; FIXME: for some reason cursor doesn't uipdate??!
  (beyond-update-cursor))
;;(add-hook 'beyond-insertion-state-enter-hook #'beyond-insertion-state-exit-on-idle)
;;(remove-hook 'beyond-insertion-state-enter-hook #'beyond-insertion-state-exit-on-idle)



;;(defhydra 'beyond-command-state-map (kbd "<SPC> z") '(("z" . redo)))

;;(add-to-list 'pointless-keysets-alist (cons 'pointless-jump-mark (car pointless-jump-keysets)))



(provide 'beyond)

;;; beyond.el ends here
