(in-package #:nyxt-user)

(list
 (make-instance 'search-engine :shortcut "wiki" :search-url
                "https://en.wikipedia.org/w/index.php?search=~a" :fallback-url
                (quri.uri:uri "https://en.wikipedia.org/") :completion-function
                (make-search-completion-function :base-url
                                                 "https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=~a"
                                                 :processing-function
                                                 (alexandria:compose #'second
                                                                     #'json:decode-json-from-string)))
 (make-instance 'search-engine :shortcut "gg" :search-url
                "https://www.google.com/search?q=~a" :fallback-url
                (quri.uri:uri "https://google.com/"))
 (make-instance 'search-engine :shortcut "ddg" :search-url
                "https://duckduckgo.com/?q=~a" :fallback-url
                (quri.uri:uri "https://duckduckgo.com/") :completion-function
                (make-search-completion-function :base-url
                                                 "https://duckduckgo.com/ac/?q=~a"
                                                 :processing-function
                                                 #'(lambda (nyxt::results)
                                                     (mapcar #'cdar
                                                             (json:decode-json-from-string
                                                              nyxt::results))))))
