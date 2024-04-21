;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--confirm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-arglist (arglist)
  "Parse an argument list into required, optional, and rest sections."
  (let ( (section :required)
         required
         optional
         rest)
    (dolist (arg arglist)
      (cond
        ((eq arg '&optional)
          (setq section :optional))
        ((eq arg '&rest)
          (setq section :rest))
        (t
          (push arg
            (cl-case section
              (:required required)
              (:optional optional)
              (:rest     rest))))))
    `( ,(nreverse required)
       ,(nreverse optional)
       ,(nreverse rest))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (parse-arglist '(x y &optional z)) returns ((x y) (z) nil))
(confirm that (parse-arglist '(x y &optional z &rest rest)) returns ((x y) (z) (rest)))
(confirm that (parse-arglist '(x y &rest rest)) returns ((x y) nil (rest)))
(confirm that (parse-arglist '()) returns (nil nil nil))
(confirm that (parse-arglist '(&optional z &rest rest)) returns (nil (z) (rest)))
(confirm that (parse-arglist '(&optional z)) returns (nil (z) nil))
(confirm that (parse-arglist '(&rest rest)) returns (nil nil (rest)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--parse-arglist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
