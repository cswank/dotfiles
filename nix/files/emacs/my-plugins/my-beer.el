;;; my-c --- Summary  -*- lexical-binding: t; -*-
;;; setup c development
;;; Commentary:

;;; Code:
(defun abv (og fg)
  "Alcohol by volume (OG = original gravity, FG = final gravity)."
  (* (- og fg) 131.25)
  )

(provide 'my-beer)
;;; my-beer.el ends here
