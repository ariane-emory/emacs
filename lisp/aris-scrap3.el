;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--some)
(require 'aris-funs--destructuring-match) 
(require 'aris-funs--constant-folding-conditionals) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    `(or (and ,(caar clauses) (progn ,@(cdar clauses)))
	     (cond2 ,@(cdr clauses)))))

(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    (if (cdar clauses)
      `(if ,(caar clauses) (progn ,@(cdar clauses)) (cond2 ,@(cdr clauses)))
      `(or ,(caar clauses) (cond2 ,@(cdr clauses))))))

(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    (let ((clause (car clauses)) (sym (gensym)))
      `(let ((,sym ,(car clause)))
         (if ,sym
           ,(if (null (cdr clause)) sym `(progn ,@(cdr clause)))
           (cond2 ,@(cdr clauses)))))))

(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (cl-macrolet ((let-it (expr &rest body) `(let ((it ,expr)) ,@body)))
    (cond
      ((null clauses) nil)
      ((and (cdar clauses) (cdr clauses))
        `(if ,(caar clauses) ,(macroexp-progn (cdar clauses)) (cond2 ,@(cdr clauses))))
      ((cdar clauses)
        `(when ,(caar clauses) ,(macroexp-progn (cdar clauses))))
      ((cdr clauses) `(or ,(caar clauses) (cond2 ,@(cdr clauses))))
      (t (caar clauses)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cond2 (&rest clauses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Re-implementation of ordinary `cond' with constant folding."
  (when clauses
    (if (cdar clauses)
      `(if* ,(caar clauses) ,(macroexp-progn (cdar clauses)) (cond2 ,@(cdr clauses)))
      `(or* ,(caar clauses) (cond2 ,@(cdr clauses))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x 7)

;; Example usage:
(cond2
  ((= x 1) (message "case 1"))
  (nil :foo)
  (32)
  ((= x 3) (message "case 3"))
  (t (message "default")))

;; Expansion:
(if (= x 1)
  (message "case 1")
  32)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let-it 'foo
  (gensymbolicate- it))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::uniqueify-variables1 (sym thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Make variables in THING unique by symbolicating them with  SYM."
  (cond
    ((ml::variable-p thing) (symbolicate- thing sym))
    ((atom thing) thing)
    (t (cons (ml::uniqueify-variables1 sym (car thing))
         (ml::uniqueify-variables1 sym (cdr thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ml::uniqueify-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Make variables in THING unique, using an identical number to make them unique."
  `(ml::uniqueify-variables1 (gensym "") ,thing))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ml::uniqueify-variables1 (gensym "") (ml::fix-variables '(X foos Bar)))
(ml::uniqueify-variables (ml::fix-variables '(X foos Bar)))
