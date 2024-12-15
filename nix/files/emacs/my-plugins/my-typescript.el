;;; my-javascript --- Summary
;;; setup for email via mu4e
;;; Commentary:

;;; -*- lexical-binding: t -*-
;;
;; this should be replaced by the built-in modes eventually, but it's not always
;; simple to switch and not every language has a builtin `lang'-ts-mode
(use-package tree-sitter
  :demand t
  :diminish tree-sitter-mode
  :config
  (setq treesit-font-lock-level 4))

(use-package tree-sitter-langs
  :after tree-sitter)

;; ;; seems like it will still throw a warning the first time you visit a file with
;; ;; a treesit major mode if you don't have the grammar installed; manually
;; ;; running M-x lang-ts-mode after waiting a few seconds for this package to
;; ;; install and compile the grammar fixes the warning and you shouldn't see it
;; ;; again afterwards
;; (use-package treesit-auto
;;   :config
;;   (setq treesit-auto-install t))

;; (use-package flymake :ensure nil
;;   :init
;;   (setq-default flymake-no-changes-timeout 1)
;;   :config
;;   (setq flymake-mode-line-format
;;         ;; the default mode line lighter takes up an unnecessary amount of
;;         ;; space, so make it shorter
;;         '(" " flymake-mode-line-exception flymake-mode-line-counters)))

;; ;; use-package for configuring, even though eglot is
;; ;; built-in in Emacs 29, thus :ensure nil
;; (use-package eglot :ensure nil :defer t
;;   :custom-face
;;   ;; personal preference here; I hate it when packages
;;   ;; use the `shadow' face for stuff, it's impossible to read
;;   (eglot-inlay-hint-face
;;    ((t ( :foreground unspecified
;;          :inherit font-lock-comment-face))))
;;   :config
;;   ;; these two lines help prevent lag with the typescript language server. they
;;   ;; might actually be mutually exclusive but I haven't investigated further
;;   (with-eval-after-load 'eglot (fset #'jsonrpc--log-event #'ignore))
;;   (setq eglot-events-buffer-size 0)

;;   ;; just do it, don't prompt me
;;   (setq eglot-confirm-server-initiated-edits nil)
;;   (setq eglot-sync-connect 0)
;;   (setq eglot-autoshutdown t)
;;   (setq rex/language-servers
;;         (list '(tsx-ts-mode "typescript-language-server" "--stdio")
;;               '(php-mode "phpactor" "language-server")))
;;   (dolist (server rex/language-servers)
;;     (add-to-list 'eglot-server-programs server))
;;   :hook
;;   (php-mode . eglot-ensure)
;;   (typescript-ts-mode . eglot-ensure)
;;   (tsx-ts-mode . eglot-ensure)
;;   (eglot-managed-mode
;;    . (lambda () (setq eldoc-documentation-function
;;                       'eldoc-documentation-compose-eagerly))))

;; ;; triggering this manually is imo much nicer than having it pop up
;; ;; automatically and obscure the code once you open it, you can scroll it with
;; ;; C-j and C-k and any other command closes it again, but it can very rarely get
;; ;; stuck on screen, which requires C-g
;; (use-package eldoc-box
;;   :defer t
;;   :config
;;   (defun rex/eldoc-box-scroll-up ()
;;     "Scroll up in `eldoc-box--frame'"
;;     (interactive)
;;     (with-current-buffer eldoc-box--buffer
;;       (with-selected-frame eldoc-box--frame
;;         (scroll-down 3))))
;;   (defun rex/eldoc-box-scroll-down ()
;;     "Scroll down in `eldoc-box--frame'"
;;     (interactive)
;;     (with-current-buffer eldoc-box--buffer
;;       (with-selected-frame eldoc-box--frame
;;         (scroll-up 3))))
;;   ;; this won't work without installing general; I include it as an example
;;   ;; see: https://github.com/skyler544/rex/blob/main/config/rex-keybindings.el
;;   :general
;;   (:keymaps 'eglot-mode-map
;;             "C-k" 'rex/eldoc-box-scroll-up
;;             "C-j" 'rex/eldoc-box-scroll-down
;;             "M-h" 'eldoc-box-help-at-point))

;; ;; these files are started in fundamental mode
;; ;; by default, but conf-mode handles them well
;; (use-package emacs :ensure nil
;;   :mode
;;   ("\\.env.test$" . conf-mode)
;;   ("\\.env.local$" . conf-mode)
;;   ("\\.env.sample$" . conf-mode)
;;   ("\\.env$" . conf-mode))

;; (use-package web-mode
;;   :config
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-markup-indent-offset 2)
;;   :mode
;;   ("\\.html$" . web-mode)
;;   ("\\.twig$" . web-mode))

;; (use-package php-mode
;;   :hook (php-mode . tree-sitter-hl-mode)
;;   :config
;;   ;; can't remember exactly what this is for
;;   (setq php-mode-template-compatibility nil))

;; ;; take advantage of the built-in treesit
;; (use-package emacs :ensure nil
;;   :after treesit
;;   :custom-face
;;   (typescript-ts-jsx-tag-face
;;    ((t ( :inherit font-lock-type-face))))
;;   :mode
;;   ("\\.js$" . js-ts-mode)
;;   ("\\.ts$" . typescript-ts-mode)
;;   ("\\.tsx$" . tsx-ts-mode))

;; ;; If you have multiple projects with multiple node versions, you probably won't
;; ;; want to deal with switching manually via exporting paths in your terminal all
;; ;; the time; this way is nicer but requires the tool nvm. Unfortunately, loading
;; ;; nvm itself takes a second or two, which can be very annoying if you let it
;; ;; live directly in your shell config. Instead, this way you can use it manually
;; ;; when you open up a project that needs it.
;; (use-package nvm
;;   ;; this is not on melpa so you'll need to grab it from rejeep/nvm.el on github
;;   ;; and load it manually or try using an elpaca recipe:
;;   ;; :elpaca (:host github :repo "rejeep/nvm.el")
;;   :init
;;   (setq rex/nvm-enabled nil)
;;   (defun rex/load-nvm ()
;;     "Start nvm."
;;     (interactive)
;;     (setq rex/nvm-enabled t)
;;     ;; ~/.local/bin/load-nvm
;;     ;; export NVM_DIR="$HOME/.nvm"
;;     ;; [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
;;     ;; [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
;;     (async-shell-command "source ~/.local/bin/load-nvm"))
;;   (defun rex/nvm-use ()
;;     "Use the .nvmrc file."
;;     (interactive)
;;     (unless rex/nvm-enabled
;;       (rex/load-nvm))
;;     (nvm-use-for)))

;; (use-package add-node-modules-path
;;   :hook
;;   (tsx-ts-mode . add-node-modules-path)
;;   (typescript-ts-mode . add-node-modules-path))

;; ;; you need to have prettier installed for this to work obviously
;; (use-package prettier-js
;;   :diminish prettier-js-mode
;;   :hook
;;   (tsx-ts-mode . prettier-js-mode)
;;   (typescript-ts-mode . prettier-js-mode))
