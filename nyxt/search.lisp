(in-package #:nyxt-user)

(defvar *my-search-engines* nil)
(setf *my-search-engines*
      (list
       '("gg" "https://www.google.com/search?q=~a" "https://www.google.com/")
       '("go" "https://pkg.go.dev/search?q=~a&m=" "https://golang.org/doc/")))


(define-configuration buffer
  ((search-engines (mapcar (lambda (engine)
                             (apply 'make-search-engine engine))
                           *my-search-engines*))))
