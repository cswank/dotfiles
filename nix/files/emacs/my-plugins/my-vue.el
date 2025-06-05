;;; my-web --- Summary
;;; setup web development
;;; Commentary:

;;; Code:

;; (add-to-list 'load-path "~/.emacs.d/plugins/vue-ts-mode")
;; (require 'vue-ts-mode)

;; (use-package treesit
;;       :mode (("\\.vue\\'" . vue-ts-mode)
;;              ;; More modes defined here...
;;              )
;;       :preface
;;       (defun os/setup-install-grammars ()
;;         "Install Tree-sitter grammars if they are absent."
;;         (interactive)
;;         (dolist (grammar
;;                  '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))))
;;           (add-to-list 'treesit-language-source-alist grammar)
;;           ;; Only install `grammar' if we don't already have it
;;           ;; installed. However, if you want to *update* a grammar then
;;           ;; this obviously prevents that from happening.
;;           (unless (treesit-language-available-p (car grammar))
;;             (treesit-install-language-grammar (car grammar)))))

;;       ;; Optional, but recommended. Tree-sitter enabled major modes are
;;       ;; distinct from their ordinary counterparts.
;;       ;;
;;       ;; You can remap major modes with `major-mode-remap-alist'. Note
;;       ;; that this does *not* extend to hooks! Make sure you migrate them
;;       ;; also
;;       (dolist (mapping
;;                '(
;;                  (typescript-mode . typescript-ts-mode)
;;                  ))
;;         (add-to-list 'major-mode-remap-alist mapping))
;;       :config
;;       (os/setup-install-grammars))

;; (provide 'my-vue)
;;; my-web.el ends here
