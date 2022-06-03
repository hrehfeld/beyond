(require 'org)
(require 'beyond)
(require 'org-convenience)

(beyond-def-state beyond-org-mode-command-state "Beyond command state for `org-mode' buffers" "oCMD" beyond-command-state)
(beyond-def-state beyond-org-mode-refile-state "Refile headlines in `org-mode' buffers" "oRFI" beyond-org-mode-command-state)
;;(pp (macroexpand-1 '(beyond-def-state beyond-org-mode-command-state "Beyond command state for `org-mode' buffers" "CMD" beyond-command-state)))

(add-to-list 'beyond-mode-command-state-alist
             '(org-mode . (beyond-command-state beyond-org-mode-command-state beyond-org-mode-refile-state)))

(beyond-def-state beyond-org-agenda-mode-command-state "Beyond command state for `org-agenda-mode' buffers" "oaCMD" beyond-command-state)
;;(pp (macroexpand-1 '(beyond-def-state beyond-org-agenda-mode-command-state "Beyond command state for `org-agenda-mode' buffers" "CMD" beyond-command-state)))
(add-to-list 'beyond-mode-command-state-alist '(org-agenda-mode . beyond-org-agenda-mode-command-state))

(setq beyond-org-mode-easy-bindings
      `((beyond-org-mode-refile-state-map
         ("m" (org-convenience-refile-next my/org-main-file nil) (propertize "Main" 'face '(:foreground "#0a0")))
         ("b" (org-convenience-refile-next org-default-notes-file "Notes") (propertize "Note" 'face '(:foreground "#0a0")))
         ("a" (org-convenience-refile-next my/org-always-file nil) "Always")
         ("r" (org-convenience-refile-next org-default-notes-file) "Reference")
				 ("z" (org-convenience-refile-next org-convenience-thoughts-file) "Thoughts")
         ("R" (org-convenience-refile-next org-default-notes-file "Recipes") "Recipes")
				 ("I" (org-convenience-refile-next my/org-inbox-file nil) "Inbox")
				 ("s" (org-convenience-refile-next my/org-someday-file) "Someday")
				 ("v" (org-convenience-refile-next my/org-appointments-file) "Events")
				 ("e" (org-convenience-refile-next my/org-eventually-file) "Eventually")
				 ("T" (org-convenience-refile-next my/org-tickler-file) "Tickle")
				 ("o" (org-convenience-refile-next my/org-read-file) "Read")
				 ("O" (org-convenience-refile-next org-convenience-reddit-notes-file) "Reddit")
				 ("E" (org-convenience-refile-next "emacs.org" "Someday") "Emacs")
				 ("H" (org-convenience-refile-next my/org-habits-file) "Habits")
         )

        (beyond-org-mode-command-state-map
         ("d" org-cut-subtree "Cut Subtree")
         ("\\" org-reddit-archive-uri-at-point "Reddit fetch")
         ("'" org-demote-subtree "Demote Subtree")
         ("\"" org-promote-subtree "Promote Subtree")
         ("q" org-convenience-clipboard-yank-quote "Yank quoted")
         ("." org-refile-goto-last-stored)
         ("<tab>" (progn (org-back-to-heading) (org-cycle)) "Fold")
         ("<backtab>" (progn (org-back-to-heading) (org-shifttab)) "Global Fold")
         ("j" pointless-jump-org-headline "Jump Headline")
         (";" org-convenience-heading-goto-title "Goto Title")
         ("n" org-convenience-forward-heading-same-level-or-up-forward-heading-same-level "Next Headline")
         ("p" org-convenience-backward-heading-same-level-or-up-heading "Previous Headline")
         ("k" org-convenience-up-element-or-backward-heading "Up")
         ("l" org-next-visible-heading "Next")
         ("w" org-refile "org-refile")
         ("1" org-todo "Todo")
         ;; ("2" org-convenience-set-context "context")
         ;; ("3" org-convenience-set-category "category")
         ("4" org-archive-subtree-default "Archive")
         ("5" (org-convenience-toggle-tag "SPRINT") "Sprint?")
         ("[" org-deadline "Deadline")
         ("]" org-schedule "Schedule")
         ("9" org-convenience-priority-up "Prio +1")
         ("0" org-convenience-priority-down "Prio -1")
         ("8" (lambda () (interactive) (org-priority 'remove)))
         (";" org-convenience-set-tags-command-dwim "Tags"))))
(beyond-easy-bind beyond-org-mode-easy-bindings)

(provide 'beyond-org)
