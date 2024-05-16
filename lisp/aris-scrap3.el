;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--some) ; only used in some tests.
(require 'aris-funs--destructuring-match) ; only used in some tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(some 'string   "foo")
(some #'stringp "foo")


(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    `(or (and ,(caar clauses) (progn ,@(cdar clauses)))
	     (cond2 ,@(cdr clauses)))))

(defmacro my-cond (&rest clauses)
  "Emulate the behavior of the `cond' special form."
  (if clauses
    (let ((clause (car clauses)) (sym (gensym)))
      `(let ((,sym ,(car clause)))
         (if ,sym
           ,(if (null (cdr clause)) sym `(progn ,@(cdr clause)))
           (my-cond ,@(cdr clauses)))))))

(defmacro my-cond (&rest clauses)
  "Emulate the behavior of the `cond' special form."
  (if clauses
    (let ((clause (car clauses)) (sym (gensym)))
      `(let ((,sym ,(car clause)))
         (if ,sym
           ,(if (null (cdr clause)) sym `(progn ,@(cdr clause)))
           (my-cond ,@(cdr clauses)))))))




;; Example usage:
(my-cond
  ((= 2 1) (message "1"))
  ((= 3 3) (message "2"))
  (t (message "default")))


;; Example usage:
(my-cond
  ((= 2 1) (message "1"))
  (32)
  ((= 3 3) (message "2"))
  (t (message "default")))

(my-cond
  ;; ((= 2 1) (message "1"))
  ;; (32)
  ;; ((= 3 3) (message "2"))
  (t (message "default")))

