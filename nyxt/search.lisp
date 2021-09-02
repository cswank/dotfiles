(list
 (make-instance 'search-engine
                :shortcut "google"
                :search-url "https://www.google.com/search?q=~a"
                :fallback-url (quri.uri:uri "https://www.google.com/")
                :completion-function (make-search-completion-function
                                      :base-url "https://www.google.com/search?q=~a"
                                      :processing-function #'(lambda (nyxt::results)
                                          (mapcar #'cdar
                                                  (json:decode-json-from-string
                                                   nyxt::results))))))
