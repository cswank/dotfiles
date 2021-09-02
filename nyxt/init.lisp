(define-configuration buffer
  ((default-modes (append
                   '(emacs-mode)
                   '(reduce-tracking-mode)
                   %slot-default%))))

(dolist (file (list (nyxt-init-file "passwd.lisp")
                    (nyxt-init-file "search.lisp")
                    (nyxt-init-file "style.lisp")))
  (load file))

