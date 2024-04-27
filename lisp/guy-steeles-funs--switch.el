;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `switch' macro presented by Guy Steele, with some minor adjustments.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--confirm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro switch (value &rest body)
  "Steele's `switch' from 'The Evolution of Lisp' (but with the originally-separate
`break' macro refactored to use `cl-macrolet' and a `gensym'-ed block label .

Remember, `break' breaks from the most-local enclosing `switch''s block,
similarily to C's switch."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ( (label    (gensym "switch-"))
          (newbody  (mapcar #'(lambda (clause) `(,(gensym) ,@(rest clause))) body))
          (switcher (cl-mapcar
                      #'(lambda (clause newclause) `(,(first clause) (go ,(first newclause))))
                      body newbody)))
    `(cl-block ,label
       (cl-macrolet ((break () '(cl-return-from ,label)))
         (cl-tagbody (cl-case ,value ,@switcher)
           (break)
           ,@(apply #'nconc newbody))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro break () '(cl-return-from switch))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 0
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (apply #'concat (nreverse res)))
  returns "none")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 1
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (apply #'concat (nreverse res)))
  returns "one too many")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 2
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (apply #'concat (nreverse res)))
  returns "too many")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 3
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (apply #'concat (nreverse res)))
  returns "many")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 4
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (apply #'concat(nreverse res)))
  returns "")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'guy-steeles-funs--switch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
