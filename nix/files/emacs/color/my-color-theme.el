;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Nicities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color-theme)
(color-theme-initialize)
(global-font-lock-mode 1)
(color-theme-tango-2)
;Show column numbers
(column-number-mode 1)
(setq-default fill-column 72)
(setq auto-fill-mode 1)
;Show what's being selected
(transient-mark-mode 1)
;Line by line scrolling
(setq scroll-step 1)
(setq inhibit-startup-message t)
;Disable the menubar (promotes good emacs memory :)
;Make page up and page down a whole lot nicer
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)
