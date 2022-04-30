(require 'beyond)

(setq beyond-easy-bindings
      `((beyond-command-state-map
         . (("s" beyond-kill-region-or-line nil)
            ("d" kill-region)
            ("y" yank nil)
            ("e" er/expand-region nil)
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
            ("?" (which-key-show-keymap (beyond--active-state-map)))
            ))
        (beyond-special-state-map
         . (("x" ,ctl-x-map "C-x")))
        (beyond-motion-state-map
         ;; should only be right-handed
         . (
            ("c" beyond--read-key-sequence-control-swapped nil)
            ("\\" beyond-quote-keypress nil)
            ("'" pointless-jump-sexp nil)
            ("." xref-find-definitions-other-window nil)
            ("i" scroll-down-command nil)
            ("o" scroll-up-command nil)
            ("O" scroll-other-window nil)
            ("I" scroll-other-window-down nil)
            ("k" backward-char nil)
            ("l" forward-char nil)
            ("h" isearch-forward nil)
            ))))

;;(unbind-key "j" beyond-command-state-map )

(beyond-easy-bind beyond-easy-bindings)

(provide 'beyond-keybindings)
