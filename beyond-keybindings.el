(require 'beyond)

(defvar beyond-goto-map (make-keymap) "Keymap with special goto commands")

(beyond-easy-bind
 `((beyond-goto-map
    ("g" ,(if (fboundp #'consult-goto-line) #'consult-goto-line #'goto-line) "Goto Line")
    ("." end-of-buffer "Buffer end")
    ("," beginning-of-buffer "Buffer begin")
    (">" end-of-buffer-other-window "Other buffer end")
    ("<" beginning-of-buffer-other-window "Other buffer begin")
    ("p" previous-buffer "Previous buffer")
    ("n" next-buffer "Next Buffer"))
   (beyond-insertion-state-map
    ("C-r" beyond-exit-insertion-state)
    ("<escape>" beyond-exit-insertion-state))
   (beyond-command-state-map
    . (("f" beyond-enter-insertion-state "Insert")
       ;;("F" beyond-overwrite "Overwrite")
       ("q" quoted-insert "Insert char")
       ;; unbind insertion-state key
       ("C-r" nil)
       ("s" beyond-smart-hungry-delete nil)
       ("d" kill-region)
       ("y" yank nil)
       ("e" er/expand-region nil)
       ("t" copilot-accept-completion)
       ("T" copilot-accept-completion-by-word)
       ("w" beyond-mark-text-units nil)
       ("x" ,ctl-x-map nil)
       ;;("<SPC> <SPC>" set-mark-command nil)
       ("r" beyond-toggle-mark nil)
       ;; see later artrep nil definition
       ("/" beyond-undo nil)
       ;;("<SPC> z" ,(beyond-def-key-repeater "z" undo-tree-redo) nil)
       ("<SPC>" beyond-next-state nil)
       ("<backspace>" delete-backward-char nil)
       ("a" embark-act nil)
       ("?" (which-key-show-keymap (beyond--buffer-active-state-map)))
       ("(" beyond-dilate-region "Dialate with parentheses")
       (")" beyond-erode-region "Erode with parentheses")
       ;; fix aggressive-indent-mode fucking up symbol-replace
       ("C-M-5" (let ((aggressive-indent-mode? aggressive-indent-mode))
                  (aggressive-indent-mode -1)
                  (call-interactively #'highlight-symbol-query-replace)
                  (when aggressive-indent-mode?
                    (aggressive-indent-mode))))
       ))
   (beyond-special-state-map
    . (("x" ,ctl-x-map "C-x")))
   (beyond-minimal-motion-state-map
    ;; should only be right-handed
    . (("\\" beyond-quote-keypress nil)
       ("i" scroll-down-command nil)
       ("o" scroll-up-command nil)
       ("O" scroll-other-window nil)
       ("I" scroll-other-window-down nil)))
   (beyond-motion-state-map
    ;; should only be right-handed
    . (("c" beyond--read-key-sequence-control-swapped nil)
       ("g" beyond-goto-map nil)
       ("'" pointless-jump-sexp nil)
       (";" pointless-jump-word-beginning nil)
       ("j" pointless-jump-char-timeout nil)
       ("J" pointless-resume nil)
       ("C-j" pointless-repeat nil)
       ("." xref-find-definitions nil)
       ("M-." xref-find-definitions-other-window nil)
       ("," xref-pop-marker-stack nil)
       ("k" backward-char nil)
       ("l" forward-char nil)
       ("h" isearch-forward nil)
       ("H" (:tap consult-ripgrep
                  ("f" projectile-find-file)))
       ("u" pointless-jump-beginning-of-line)
       ("p" (:tap pointless-jump-end-of-line ("p" end-of-line)))
       ("n" (:tap nil ("n" exchange-point-and-mark) ("m" pointless-jump-mark)))
       ))))

;;(unbind-key ";" beyond-command-state-map )


(taps-def-double-tap-key beyond-motion-state-map "n" pointless-jump-mark exchange-point-and-mark)
(taps-def-double-tap-key beyond-motion-state-map "b" pointfull-pop-local-mark pointfull-mark)

(taps-def-double-tap-key beyond-command-state-map "m" newline open-line)




(provide 'beyond-keybindings)
