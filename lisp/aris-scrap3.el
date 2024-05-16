;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--some) ; only used in some tests.
(require 'aris-funs--destructuring-match) ; only used in some tests.
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

;; Example usage:
(cond2
  ((= 2 1) (message "1"))
  ((= 3 3)) ; (message "2")
  (t (message "default")))

;; Example usage:
(cond2
  ((= 2 1) (message "1"))
  (32)
  ((= 3 3) (message "2"))
  (t (message "default")))

(cond2
  ;; ((= 2 1) (message "1"))
  ;; (32)
  ;; ((= 3 3) (message "2"))
  (t (message "default")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant folding:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun macroexp-and/or-args (args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Helper function for `or*'."
  (when  args
    (if (let-it (car args) (or (eq t it) (and (atom it) (not (symbolp it)))))
      (list (car args))
      (cons (car args) (macroexp-and/or-args (cdr args))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro and* (&rest things)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `or'."
  (let ((expr (macroexp-and/or-args things)))
    (if (cdr expr)
      `(and ,@(macroexp-and/or-args things))
      (car expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro or* (&rest things)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `or'."
  (let ((expr (macroexp-and/or-args things)))
    (if (cdr expr)
      `(or ,@(macroexp-and/or-args things))
      (car expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if* (test then &rest else)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `if'."
  (cond
    ((null test) (macroexp-progn else))
    ((or (eq t test) (and (atom test) (not (symbolp test)))) then)
    (t `(if ,test ,then ,@else))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when* (test &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `when'."
  `(if* ,test ,(macroexp-progn body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro unless* (test &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `unless'."
  `(if* ,test nil ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((x 7))
  (confirm that (when* x "foo") returns "foo")             ; (if* x "foo") ; (if x "foo")
  (confirm that (when* nil "foo") returns nil)             ; (if* nil "foo") ; nil
  (confirm that (when* x "foo") returns "foo")             ; (if* x "foo") ; (if x "foo")
  (confirm that (when* t "foo") returns "foo")             ; (if* t "foo") ; "foo"
  (confirm that (when* t "foo" "bar") returns "bar")       ; (if* t (progn "foo" "bar")) ; (progn "foo" "bar")
  (confirm that (unless* x "foo") returns nil)             ; (if* x nil "foo")
  (confirm that (unless* nil "foo") returns "foo")         ; (if* nil nil "foo") ; "foo"
  (confirm that (unless* x "foo") returns nil)             ; (if* x nil "foo") ; (if x nil "foo")
  (confirm that (unless* t "foo") returns nil)             ; (if* t nil "foo") ; nil
  (confirm that (unless* t "foo" "bar") returns nil)       ; (if* t nil "foo" "bar") ; nil
  (confirm that (if* t "foo") returns "foo")               ; (when* t "foo") ; (if* t "foo") ; "foo"
  (confirm that (if* x "foo" "bar" "baz") returns "foo")   ; (if x "foo" "bar" "baz")
  (confirm that (if* t "foo" "bar" "baz") returns "foo")   ; "foo" 
  (confirm that (if* nil "foo" "bar" "baz") returns "baz") ; (progn "bar" "baz")
  ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cond2 (&rest clauses)
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
  (32)
  ((= x 3) (message "case 3"))
  (t (message "default")))

;; Expansion:
(if (= x 1)
  (message "case 1")
  32)

