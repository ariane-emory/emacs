;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
(require 'aris-funs--parse-arglist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prepend-new-args ( prepend-required-args
                          prepend-optional-args
                          prepend-rest-args
                          arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Insert PREPEND-REQUIRED-ARGS, PREPEND-OPTIONAL-ARGS, and PREPEND-REST-ARGS into
ARGLIST, prepnding them to the required, optional, and rest argument sections, respectively."
  (let* ( (parsed    (parse-arglist arglist))
          (required  (first  parsed))
          (optional  (second parsed))
          (rest      (third  parsed))
          (required
            (when (or prepend-required-args required)
              `(,@prepend-required-args ,@required)))
          (optional
            (when (or prepend-optional-args optional)
              `(&optional ,@prepend-optional-args ,@optional)))
          (rest
            (when (or prepend-rest-args rest)
              `(&rest ,@prepend-rest-args ,@rest))))
    (append required optional rest)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (prepend-new-args '(db-sym) '(db-prop) nil '(x y &optional z))
  returns (db-sym x y &optional db-prop z))
(confirm that (prepend-new-args '(db-sym) '(db-prop) nil '(x y &optional z &rest rest))
  returns (db-sym x y &optional db-prop z &rest rest))
(confirm that (prepend-new-args '(db-sym) '(db-prop) nil '(x y &rest rest))
  returns (db-sym x y &optional db-prop &rest rest))
(confirm that (prepend-new-args '(db-sym) '(db-prop) nil '())
  returns (db-sym &optional db-prop))
(confirm that (prepend-new-args '(db-sym) '(db-prop) nil '(&optional z &rest rest))
  returns (db-sym &optional db-prop z &rest rest))
(confirm that (prepend-new-args '(db-sym) '(db-prop) nil '(&optional z))
  returns (db-sym &optional db-prop z))
(confirm that (prepend-new-args '(db-sym) '(db-prop) nil '(&rest rest))
  returns (db-sym &optional db-prop &rest rest))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--prepend-new-args)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
