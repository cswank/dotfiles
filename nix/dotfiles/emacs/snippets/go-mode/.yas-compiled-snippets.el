;;; Compiled snippets and support files for `go-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("testcases" "testCases := []struct{	\n	$1\n}{\n	{\n    	${1:$(mapconcat (lambda (f) (concat (nth 0 (split-string f)) \":\"))\n   (split-string yas-text \"\\n\") \"\\n\")} $0\n    },\n}\n\nfor i, tc := range testCases {\n	t.Run(fmt.Sprintf(\"%02d\", i), func(t *testing.T) {\n	})\n}" "testcases" nil nil nil "/Users/craigswank/.emacs.d/snippets/go-mode/testcases" nil nil)
                       ("struct" "type $1 struct {\n     $0\n}\n\nfunc New(opts ...func(*$1)) *$1 {\n     x :=&$1{}\n\n     for _, opt := range opts {\n         opt(x)\n     }\n\n     return x\n}" "struct" nil nil nil "/Users/craigswank/.emacs.d/snippets/go-mode/struct" nil nil)
                       ("iferr" "if err != nil {\n	return $1err\n}  \n" "err" nil nil nil "/Users/craigswank/.emacs.d/snippets/go-mode/err" nil nil)))


;;; Do not edit! File generated at Fri Jul 16 06:25:31 2021
