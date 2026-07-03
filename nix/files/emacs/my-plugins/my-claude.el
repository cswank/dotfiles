;;; my-c --- Summary
;;; setup c development
;;; Commentary:

;;; Code:
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :custom
  (claude-code-ide-terminal-backend 'eat) ; vterm hangs emacs at 100% CPU during "thinking..."
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools
;;; my-beer.el ends here
