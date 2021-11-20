;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("vfm" "#+TBLFM: @>\\$$1=v${2:$$(yas-choose-value '(\"sum\" \"mean\"))}(@2..@-1);%.2f" "vfm" nil nil nil "/Users/craigswank/.emacs.d/snippets/org-mode/vfm" nil nil)
                       ("unix" "#+TBLFM: \\$$1='(org-sbe unix (ts \\$\\$$2))\n\n#+NAME: unix\n#+BEGIN_SRC sh :results silent sh :var ts=\"\"\nTS=$(($ts / 1000))\nOUT=$(date -r $TS)\necho $OUT\n#+END_SRC\n" "unix" nil nil nil "/Users/craigswank/.emacs.d/snippets/org-mode/unix" nil nil)
                       ("table4" "#+NAME: $1\n| Category | Amount | Tax | Total |\n|----------+--------+-----+-------|\n| $0         |        |     |       |\n|----------+--------+-----+-------|\n| Total:   |        |     | 0.00  |\n#+TBLFM: @>\\$4=vsum(@2..@-1);%.2f\n#+TBLFM: @2\\$4..@-1\\$4=\\$2+\\$3;%.2f" "table4" nil nil nil "/Users/craigswank/.emacs.d/snippets/org-mode/table4" nil nil)
                       ("table2" "#+NAME: $1\n| Category              | Amount |\n|-----------------------+--------|\n| $0                      |        |\n|-----------------------+--------|\n| Total:                |   0.00 |\n#+TBLFM: @>\\$2=vsum(@2..@-1);%.2f" "table2" nil nil nil "/Users/craigswank/.emacs.d/snippets/org-mode/table2" nil nil)
                       ("sql" "#+BEGIN_SRC sql :engine postgres :exports results :cmdline -d `(symbol-value 'database-name)` -h '`(symbol-value 'database-host)`' -U parsyl\n$0\n#+END_SRC" "sql" nil nil nil "/Users/craigswank/.emacs.d/snippets/org-mode/sql" nil nil)
                       ("gotable" "#+NAME: $2\n#+BEGIN_SRC go :var x=0 :results output :imports '(\"fmt\") \n  fmt.Println(x*x)\n#+END_SRC\n\n#+NAME: $3\n| a |  b |\n|---+----|\n| $0 |    |\n|   |    |\n#+TBLFM: \\$2='(org-sbe $2 (x \\$1))\n" "gotable" nil nil nil "/Users/craigswank/.emacs.d/snippets/org-mode/gotable" nil nil)))


;;; Do not edit! File generated at Fri Jul 16 06:25:31 2021
