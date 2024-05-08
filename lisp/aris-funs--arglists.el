;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for processing arglists.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-arglist (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun count-args (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Count the minimum and maximum arguments described by ARGLIST and
return a pair of the form (MIN-ARGS . MAX-ARGS).

This only handles those lambda list keywords native to Emacs Lisp,
i.e. &optional and &rest, but will tolerate default values for optional
args."
  (let ( (min-args 0)
         (max-args 0)
         (seen-optional nil)
         (seen-rest nil))
    (while-let ((popped (pop arglist)))
      (cond
        ((eq popped '&optional)
          (setq seen-optional t))
        ((eq popped '&rest)
          (setq seen-rest t))
        (seen-rest) ;; don't bother counting after &rest.
        (seen-optional
          (cl-incf max-args)) 
        (t
          (cl-incf min-args)
          (cl-incf max-args))))
    (cons min-args (if seen-rest nil max-args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (count-args '(foo &optional bar &rest baz)) returns (1))
(confirm that (count-args '(foo &optional bar baz)) returns (1 . 3))
(confirm that (count-args '(foo bar &optional baz)) returns (2 . 3))
(confirm that (count-args '(foo bar &optional (baz 9))) returns (2 . 3))
(confirm that (count-args '(foo bar &optional baz &rest quux)) returns (2))
(confirm that (count-args '(&rest quux)) returns (0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *arglists--emac-lisp-lambda-list-keywords*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '(&optional &rest)
  " A list of the lambda list keywords native to Emacs Lisp.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun arg-names (arglist)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Return a list of the argument names in ARG-LIST by removing lambda list keywords.
;; This only handles elisp's native lambda list keywords."
;;   (cl-remove-if
;;     (lambda (arg) (memq arg '(&optional &rest))) arglist))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arg-names (arglist &optional user-lambda-list-keywords)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a list of the argument names in ARGLIST. This supports &optional
(with CL-style default values) and &rest."
  (let ((lambda-list-keywords
          (append *arglists--emac-lisp-lambda-list-keywords*
            user-lambda-list-keywords)))
    (mapcar (lambda (e) (or (car-safe e) e))
      (cl-remove-if
        (lambda (e) (memq e lambda-list-keywords))
        arglist))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (arg-names '(foo bar &optional baz &rest quux))
  returns (foo bar baz quux))
(confirm that (arg-names '(foo bar &optional (baz 9) &rest quux))
  returns (foo bar baz quux))
(confirm that
  (arg-names '(foo &my-keyword bar &optional (baz 9) &rest quux) '(&my-keyword))
  returns (foo bar baz quux))
(confirm that (arg-names '(x y &optional z)) returns (x y z))
(confirm that (arg-names '(x y &optional z &rest rest)) returns (x y z rest))
(confirm that (arg-names '(x y &rest rest)) returns (x y rest))
(confirm that (arg-names '()) returns ())
(confirm that (arg-names '(&optional z &rest rest)) returns (z rest))
(confirm that (arg-names '(&optional z)) returns (z))
(confirm that (arg-names '(x y z)) returns (x y z))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compatible-arg-counts? (pattern-arg-count scrutinee-arg-count)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Determine whether SCRUTINEE-ARG-COUNT is 'compatible' with
PATTERN-ARG-COUNT.

SCRUTINEE-ARG-COUNT and PATTERN-ARG-COUNT are pairs of the form
(MIN-ARGS . MAX-ARGS). MAX-ARGS may be nil to indicate 'no maximum number of args',
(i.e., the spec describtes the argument count of an arglist with a &rest lambda
list keyword in it).

SCRUTINEE-ARG-COUNT is considered 'compatible' with
PATTERN-ARG-COUNT when any arg count that would be valid for the pattern
would also be valid for the scrutinee."
  (cond
    ;; if MIN-ARGS doesn't match, they're not compatible:
    ((not (= (car pattern-arg-count) (car scrutinee-arg-count))) nil)
    ;; if scrutinee has no MAX-ARGS, they're compatible:
    ((null (cdr scrutinee-arg-count)) t)
    ;; if pattern has a MAX-ARGS, scrutinees must be GTE:
    ((and (cdr pattern-arg-count)
       (>= (cdr scrutinee-arg-count) (cdr pattern-arg-count)) t))
    ;; otherwise they're not compatible:
    (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (compatible-arg-counts? '(1 . 3) '(1 . 3)) returns t)
(confirm that (compatible-arg-counts? '(1 . 3) '(1 . 4)) returns t)
(confirm that (compatible-arg-counts? '(1 . 4) '(1 . 3)) returns nil)
(confirm that (compatible-arg-counts? '(2 . 3) '(1 . 3)) returns nil)
(confirm that (compatible-arg-counts? '(1 . 3) '(2 . 3)) returns nil)
(confirm that (compatible-arg-counts? '(1 . nil) '(1 . 3)) returns nil)
(confirm that (compatible-arg-counts? '(1 . 3) '(1 . nil)) returns t)
(confirm that (compatible-arg-counts? '(2 . 3) '(2 . 5)) returns t)
(confirm that (compatible-arg-counts? '(2 . 5) '(2 . 3)) returns nil)
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
          (optional  (cadr   parsed))
          (rest      (caddr  parsed))
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
(provide 'aris-funs--arglists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
