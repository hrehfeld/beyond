;;; beyond-core.el --- An Ergonomic Command State  -*- lexical-binding: t -*-

;;; Commentary:

;; This module sets up the emulation keymaps for each beyond state.
;; Functions to switch to each state is also provided.

;;; Code:

(require 'cl-macs)
(require 'dash)
(require 'subr-x)

(defgroup beyond nil "Beyond" :group 'Editing)


(defvar beyond-debug nil "A list of debug types.")
(defmacro beyond-debug (type &rest body)
  ;;(message "beyond-debug %S %S %S" type (member type beyond-debug) beyond-debug)
  `(when (memq ',type beyond-debug)
     (progn ,@body)
    ))
;;(setq beyond-debug '(hook mode))
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
`beyond--states'.

Earlier items overwrite bindings in later maps, so make sure
inheriting states appear before their parents - e.g.
beyond-command-state before beyond-motion-state.")
;;(kill-local-variable 'beyond-state-map-alist)

(defun beyond--states ()
  (mapcar #'car beyond-state-map-alist))

;; stack of states infrastructure
(defvar-local beyond-state-stack nil "Stack of active states.")
;; (setq beyond-state-stack nil)

(defun beyond--active-state ()
  (car beyond-state-stack))

(defun beyond--pop-active-state ()
  (pop beyond-state-stack))

(defun beyond--push-active-state (state)
  (push state beyond-state-stack))


(defun beyond--state-set (state-sym state-val)
  "Enable or disable a state named `STATE-SYM'. When `ENABLE' is t, enable the state.

`STATE' can either be a minor-mode (if bound as a function symbol) or
just a variable."
  (beyond-debug mode (message "Switching variable for  %S to %S" state-sym state-val))
  (set state-sym state-val)
  (when (symbol-function state-sym)
    (beyond-debug mode (message "calling function for %S" state-sym))
    ;; call minor mode if state has one
    (funcall state-sym (if state-val 1 -1))
    )
)


(defun beyond--sanity-check ()
  ;; only ever one state active
  (cl-assert (= 1 (seq-count #'symbol-value (beyond--states))) t "%S" (-zip-lists (beyond--states) (mapcar #'symbol-value (beyond--states))))
  (cl-assert (symbol-value (beyond--active-state)) t)
  ;; state's map is in the currently active keymaps
  (let* ((map-sym (intern (concat (symbol-name (beyond--active-state)) "-map")))
         (map (symbol-value map-sym)))
    (beyond-debug mode (message "beyond--sanity-check: index of %S: %i" map-sym (cl-position map (current-active-maps))))
    (cl-assert (member map (current-active-maps)) t)
    )
  t)


;; hook infrastructure
(defvar beyond-state-switch-hook nil "Hook called when states are
switched, before state hooks are run. Functions are called with
the state symbol.

Do NOT put blocking stuff here.")
(defvar beyond-state-hooks nil
  "Alist of `(STATE-SYM . HOOK-SYM)' called when entering `STATE'")
(defvar beyond-state-exit-hooks nil
  "Alist of `(STATE-SYM . HOOK-SYM)' called when exiting `STATE'")

(defun beyond--run-state-hook (state &optional exit?)
  (cl-check-type state symbol)
  (cl-loop while state
           do (progn
                (let ((hook (alist-get state (if exit? beyond-state-exit-hooks beyond-state-hooks))))
                  (when hook
                    (beyond-debug hook (message "beyond--run-state-hook: running hook %S" hook))
                    (run-hook-with-args hook)))
                (setq state (alist-get state beyond-state-parents)))
           ))


(defun beyond--switch-state (state)
  (cl-assert (symbolp state) "state %S should be a symbol" state)
  (let ((old-state (beyond--active-state)))
    (unless (eq state old-state)
      (beyond-debug mode (message "Beyond switching from %S to %S in %s" old-state state (buffer-name)))
      ;; disable previous state
      (when old-state
        (beyond--state-set old-state nil)
        (beyond--run-state-hook old-state t)
        )
      ;; save and activate new state
      (beyond--push-active-state state)
      (beyond--state-set state t)
      (beyond--sanity-check)
      (beyond-debug hook (message "beyond--switch-state: running %S" state))
      (run-hook-with-args 'beyond-state-switch-hook state)
      (beyond--run-state-hook state)
      ))
  (beyond-debug mode (message "beyond-states: %S %S %S" beyond-command-state beyond-insertion-state (beyond--active-state))))

(defun beyond--switch-back-state ()
  (beyond--sanity-check)
  (when (length< beyond-state-stack 2)
    (user-error "beyond: can't exit last state! %S %S %S" beyond-command-state beyond-insertion-state (beyond--active-state)))
  (let ((state (beyond--active-state)))
    ;; exit current state
    (beyond--state-set state nil)
    (beyond--run-state-hook state t)
    (beyond--pop-active-state)
    ;; enter previous state
    (let ((previous-state (beyond--active-state)))
      (cl-assert previous-state t)
      (beyond-debug mode (message "Beyond switching back from %S to %S in %s" state previous-state (buffer-name)))
      (beyond--state-set previous-state t)
      (beyond--run-state-hook previous-state)
      (run-hook-with-args 'beyond-state-switch-hook previous-state)
      )
  (beyond--sanity-check)
  )
  (beyond-debug mode (message "beyond-states: %S %S %S" beyond-command-state beyond-insertion-state (beyond--active-state))))


(defun beyond-initial-state ()
  "Get the initial beyond state for current buffer."

  (or (cl-loop for minor-mode in local-minor-modes
               with state = nil
               do (setq state (alist-get minor-mode beyond-minor-mode-states))
               if state return state)
      (alist-get major-mode beyond-major-mode-states)
      (cl-loop for trigger in beyond-state-trigger
               with state = nil
               do (setq state (funcall trigger))
               if state return state)
      'beyond-command-state
      ))



(defmacro beyond-def-state-map (map-name &optional state parent-map supress?)
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

(cl-defmacro beyond-def-state (state doc lighter-string parent-state &optional parent-state-map &rest minor-mode-body)
  "`STATE' should be a symbol for the new state like `beyond-command-state'.

`PARENT-STATE' is a symbol for a parent state or `nil'.

`PARENT-STATE-MAP' is a symbol of the parent keymap, defaults to
`PARENT-STATE' with a \"-map\" suffix or nil if parent-state is
nil.

With `NO-VAR', don't create a state variable (mostly useful if
state is a minor mode and creates a variable anyways).

Do NOT use `minor-mode-body' for state transitions! It will cause
havoc."
  (cl-check-type state symbol)
  (cl-check-type parent-state (or null symbol))
  (let* ((state-name (symbol-name state))
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
           (beyond-debug mode (message "body of %S" ',state))
           ,@minor-mode-body
           ))

       ;; parent
       ,@(when parent-state
           `((setq beyond-state-parents (cons '(,state . ,parent-state) (assq-delete-all ',state beyond-state-parents)))))
       ;; enable keymap
       (add-to-list 'beyond-state-map-alist '(,state . ,state-map)))))

;;(add-to-list 'beyond-state-map-alist '(beyond-org-mode-command-state . beyond-command-state-map))
;;(setq beyond-state-map-alist nil)
;;(setq beyond-major-mode-states nil)



(defvar beyond-motion-state-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Base keymap that saves motion commands. Other special state maps inherit from this.")
;; (pp (macroexpand-1 '(beyond-def-state beyond-command-state "Beyond command mode where keys aren't self-inserting but run commands." "CMD" nil beyond-motion-state-map)))
(beyond-def-state beyond-command-state "Beyond command mode where keys aren't self-inserting but run commands instead." "CMD" nil beyond-motion-state-map)
(beyond-def-state beyond-special-state "Beyond special mode where keys aren't self-inserting but run commands instead while mimicing the original special mode." "SPC" nil beyond-motion-state-map)

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
(beyond-def-state beyond-insertion-state "Beyond insertion mode where keys self-insert. Generally behaves very similar to normal emacs." "INS" nil nil
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
                      (beyond-debug mark (message "popping mark"))
                      (pop-mark))
 ))

(defun beyond-enter-insertion-state () (interactive) (beyond--switch-state 'beyond-insertion-state))
(defun beyond-exit-insertion-state () (interactive) (beyond--switch-back-state))



(defvar-local beyond--original-cursor 'undefined "The cursor before insert mode. If this is the symbol `undefined', cursor-type is saved before changing")

(defvar beyond-cursor-default '(box "#000") "The default cursor
as `(type color)'. `Color' may be nil")
(defcustom beyond-cursor-types '((beyond-insertion-state . (bar "#770"))
                                 (beyond-command-state . (box "#000")))
  "An alist of `(CURSOR-TYPE CURSOR-COLOR)' for each state.

Cursor will be set for active all active states, overwriting
previous cursor settings.

See `set-cursor-color'."
  :group 'beyond :type 'sexp)

;;(toggle-debug-on-error)

(defun beyond-update-cursor (&optional mode)
  "Update the cursor depending on the current beyond state."
    (with-current-buffer (window-buffer)
    (let ((type-color
           (alist-get (beyond--active-state) beyond-cursor-types)))
      (when (eq beyond--original-cursor 'undefined)
        ;;(message "beyond previous cursor in %s was %S %S" (buffer-name) cursor-type (frame-parameter nil 'cursor-color))
        (setq beyond--original-cursor (list cursor-type (frame-parameter nil 'cursor-color))))
      (beyond-debug cursor (message "beyond-update-cursor %S %i in %s with %S" type-color (length type-color) (buffer-name) (beyond--active-state)))
      (cl-destructuring-bind (type color) (or type-color beyond--original-cursor)
        (unless (eq type 'undefined)
          (setq cursor-type type))
        (when color
          (set-cursor-color color)))

      )))
;; (beyond-update-cursor)
(add-hook 'buffer-list-update-hook #'beyond-update-cursor)
(add-hook 'beyond-state-switch-hook #'beyond-update-cursor)

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


(defcustom beyond-minor-mode-states
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
            view-mode))
  "A alist of `(MINOR-MODE . BEYOND-STATE)' to trigger a specific state for that minor mode.

It actually just checks for a variable of that name to see if
that condition triggers."
  :group 'beyond :type '(repeat symbol))

(defcustom beyond-major-mode-states
  '((message-mode . beyond-insert-state)
    (org-agenda-mode . beyond-special-state))
  "A alist of `(MAJOR-MODE . BEYOND-STATE)' to trigger a specific state for that major mode."
  :group 'beyond :type '(list symbol))

(defcustom beyond-state-trigger
  (list ;; see major-mode conventions
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html
   (lambda () (when (and (eq (get major-mode 'mode-class) 'special)
                         (not (beyond-shell-mode-p)))
                'beyond-special-state))
   (lambda () (when (string-match "COMMIT_EDITMSG" (buffer-name))
                'beyond-insertion-state)))
  "A alist of predicate functions to trigger special state.

Each entry is called as a function with no arguments and should
return a beyond state to switch to or nil."
  :group 'beyond :type '(list symbol))


(defcustom beyond-major-mode-command-statekeymaps
  nil
  "A alist of `(MAJOR-MODE . COMMAND-STATE-KEYMAP)' to override the command-state keymap."
  :group 'beyond :type '(list symbol))


;;; Initialisation and activation

;; have an extra indirection because emulation-mode-map-alists needs direct references to keymaps,
;; not symbols of keymap-variables.
(defvar beyond-state-map-alist-for-emulation-mode-map-alists nil)

(defun beyond-lighter-string ()
  "Return the lighter string appropriate for the current state."
  (alist-get (beyond--active-state) beyond-state-lighters)
  )


(define-minor-mode beyond-mode
  "Minor mode for setting up command state in a single buffer."
  :lighter (:eval (concat " by:" (beyond-lighter-string)))
  :keymap nil
  (if beyond-mode
      (unless (minibufferp)
        (setq beyond-state-stack nil)
        (cl-pushnew 'beyond-state-map-alist-for-emulation-mode-map-alists emulation-mode-map-alists)
        ;; update the emulation keymaps everytime so that any change is catched by turning it off
        ;; and on again
        (setq beyond-state-map-alist-for-emulation-mode-map-alists
              (cl-loop for (mode-sym . map-sym) in beyond-state-map-alist
                       collect (cons mode-sym (symbol-value map-sym))))
        ;; (unless (memq 'beyond/after-change-hook after-change-functions)
        ;;   (push 'beyond/after-change-hook after-change-functions))
        (beyond--switch-state
         (beyond-initial-state)))
    (cl-delete 'beyond-state-map-alist emulation-mode-map-alists))
  )

;; make modeline lighter update after each state switch
(add-hook 'beyond-state-switch-hook #'force-mode-line-update)


(defun beyond-mode--initialize ()
  "Setup beyond in the current buffer.
Should only be used to
initialize `beyond-mode' from the globalized minor-mode
`beyond-global-mode'.  It is called whenever beyond is enabled in a buffer
for the first time or when beyond is active and the `major-mode' of
the buffer changes."
  (beyond-debug mode "beyond-global-mode %S" (buffer-name))
  (unless beyond-mode
    (beyond-mode 1)))

;;;###autoload (autoload 'beyond-global-mode "beyond" "Toggle beyond in all buffers" t)
(define-globalized-minor-mode beyond-global-mode beyond-mode beyond-mode--initialize :group 'beyond)


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
                                 (mapcar #'cdr beyond-state-map-alist))))
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



(defun beyond-timeout-keypress (command alternative-command-keymap timeout prompt &rest format-args)
  "Execute `COMMAND' if no further keysequence found in `ALTERNATIVE-COMMAND-KEYMAP' is pressed within `TIMEOUT' seconds. Otherwise execute command found there.

`PROMPT' will be used as the prompt after `format-args' are applied to it using `format'."
  (let ((prompt (apply #'format prompt format-args))
        cancelled)
    (let* ((idle-timer (run-with-timer timeout nil (lambda () (setq cancelled t))))
           key-sequence
           (command
            (or
             ;; maybe read a complete sequence found in keymap
             (cl-loop with event = nil
                      with command = nil
                      while (not cancelled)
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

(defmacro beyond-def-quick-key-command (name timeout base-command &optional quick-map)
  (interactive)
  (cl-check-type name symbol)
  (cl-check-type timeout number)
  (cl-check-type quick-map (or null symbol))
  (let* ((new-map-sym (intern (concat (symbol-name name) "-map")))
         (new-map-def (unless quick-map
                        `((defvar ,new-map-sym (make-sparse-keymap)))))
         (quick-map (or quick-map new-map-sym)))
    (cl-check-type quick-map symbol)
    `(progn
       ,@new-map-def
       (let ((quick-map ,quick-map))
             (defun ,name ()
               (interactive)
               (beyond-timeout-keypress ,base-command quick-map ,timeout "Continue command %S?" ,base-command))
             #',name))))

(defmacro beyond-def-quick-key-command-with-keys (name timeout base-command &optional quick-map &rest keys-commands)
  (interactive)
  (cl-check-type quick-map (or null symbol))
  `(let* ((quick-command (beyond-def-quick-key-command ,name ,timeout ,base-command ,quick-map))
          (quick-map (symbol-value ,(or quick-map '(intern (concat (symbol-name quick-command) "-map"))))))
     (cl-check-type quick-command symbol)
     (cl-check-type quick-map keymap)
     (cl-assert quick-map t)
     ,@(cl-loop for (key . command) in keys-commands
                collect (progn
                          (cl-check-type key string)
                          (cl-check-type command symbol)
                          `(define-key quick-map ,key (quote ,command))))
     quick-command))
;; (macroexpand-1 '(beyond-def-quick-key-command-with-keys beyond-pointless-jump-mark-quick 0.5 'pointless-jump-mark nil ("m" . exchange-point-and-mark)))
;;(beyond-def-quick-key-command-with-keys beyond-pointless-jump-mark-quick 0.5 'pointless-jump-mark nil ("m" . exchange-point-and-mark))




;; editing commands

(defun beyond-kill-region-or-line ()
  "Kill region if active, line if at line end, or cycle space."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (let ((point (point)))
      (if (eolp)
          (prog1
              (kill-line)
            (message "killed line"))
            ;; kill ws first
        (cycle-spacing)))))


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
  (message "mark was %s at %s" (if mark-active "active" "inactive") (mark))
  (let ((overlay (make-overlay (mark) (1+ (mark)))))
    (overlay-put overlay 'face '(:background "red")))
  (if mark-active
      (progn
        (deactivate-mark t)
        (cl-assert (not mark-active)))
    (activate-mark)
    (cl-assert mark-active))
  (redisplay)
  )


(define-key beyond-command-state-map (kbd "v") #'beyond-enter-insertion-state)
(define-key beyond-insertion-state-map (kbd "C-g") #'beyond-exit-insertion-state)
(define-key beyond-insertion-state-map (kbd "<escape>") #'beyond-exit-insertion-state)

(defun beyond-kill-non-bol-trailing-whitespace ()
  "Kill whitespace after point unless at beginning of line."
  (save-excursion
    (when (not (bolp))
      (let ((insert-point (point)))
        (skip-syntax-forward " " (line-end-position))
        (when (eolp) (delete-region insert-point (point)))))))

(add-hook 'beyond-insertion-state-enter-hook #'beyond-kill-non-bol-trailing-whitespace)
;;(remove-hook 'beyond-insertion-state-enter-hook #'beyond-kill-non-bol-trailing-whitespace)

(let ((end-of-line-key "p")
      (beginning-of-line-key "u"))
  (mapc (lambda (map-binding-list)
          (mapc (lambda (e) (define-key (car map-binding-list) (kbd (car e)) (cdr e)))
                (cdr map-binding-list)))
        `((,beyond-command-state-map
           . (("d" . beyond-kill-region-or-line)
              ("y" . yank)
              ("e" . er/expand-region)
              ("w" . beyond-mark-text-units)
              ("x" . ,ctl-x-map)
              ("<SPC> <SPC>" . set-mark-command)
              ("r" . beyond-toggle-mark)
              ;; see later smartrep definition
              ("/" . undo)
              ("<SPC> z" . ,(beyond-def-key-repeater "z" undo-tree-redo))
              ("<backspace>" . delete-backward-char)
              ("a" . embark-act)
              ;;editing
              ("m" . newline)
              ))
          (,beyond-special-state-map
           . (("x" . ,ctl-x-map)))
          (,beyond-motion-state-map
           ;; should only be right-handed
           . (
              ("c" . beyond--read-key-sequence-control-swapped)
              ("\\" . beyond-quote-keypress)
              ("j" . pointless-jump-char-timeout)
              (";" . pointless-jump-word-beginning)
              (,beginning-of-line-key . ,(beyond-def-quick-key-command-with-keys beyond-pointless-jump-beginning-of-line
                                                                                 0.15 'pointless-jump-beginning-of-line
                                                                                 nil (,beginning-of-line-key . beginning-of-line)))
              ("U" . back-to-indentation)
              (,end-of-line-key . ,(beyond-def-quick-key-command-with-keys beyond-pointless-jump-end-of-line
                                                                           0.15 'pointless-jump-end-of-line
                                                                           nil (,end-of-line-key . end-of-line)))
              ("'" . pointless-jump-sexp)
              ("n" . ,(beyond-def-quick-key-command-with-keys beyond-pointless-jump-mark-quick
                                                              0.15 'pointless-jump-mark
                                                              nil ("m" . exchange-point-and-mark)))
              ("." . xref-find-definitions-other-window)
              ("i" . scroll-down-command)
              ("o" . scroll-up-command)
              ("O" . scroll-other-window)
              ("I" . scroll-other-window-down)
              ("k" . backward-char)
              ("l" . forward-char)
              ("h" . isearch-forward)

              )))))


;; make insert mode automatically stop after idle time

(defvar beyond-insert-idle-time 2 "Idle time until returning from insert-state.")
(defvar-local beyond-insertion-state-exit-on-idle-timer nil "The current insert timer object.")

(defun beyond-insertion-state-exit-on-idle-action (buffer)
  (cl-check-type buffer buffer)
  (beyond-debug hook (message "exit-on-idle action"))
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
  (beyond-debug hook (message "exit-on-idle start"))
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

(eval-after-load 'org-mode
  (progn
    (beyond-def-state beyond-org-mode-command-state "Beyond command state for `org-mode' buffers" "CMD" beyond-command-state)
    (pp (macroexpand-1 '(beyond-def-state beyond-org-mode-command-state "Beyond command state for `org-mode' buffers" "CMD" beyond-command-state)))
    (add-to-list 'beyond-major-mode-states '(org-mode . beyond-org-mode-command-state))

    (defvar beyond-org-mode-command-state-leader-key "<SPC>"
      "Prefix key for `beyond-org-mode-command-state.'")
    ;; (let ((leader (concat beyond-org-mode-command-state-leader-key " ")))
    ;;   (cl-loop for (key . command) in `((,(concat leader "j") . pointless-jump-org-headline))
    ;;            do (define-key beyond-org-mode-command-state-map (kbd key) command)))
    ))

(provide 'beyond)
;;; beyond ends here
