(define-configuration buffer
  ((default-modes (append
                   '(emacs-mode)
                   '(reduce-tracking-mode)
                   %slot-default%))))

(load-after-system :nx-search-engines (nyxt-init-file "search.lisp"))
(dolist (file (list (nyxt-init-file "passwd.lisp")
                    (nyxt-init-file "style.lisp")))
  (load file))

