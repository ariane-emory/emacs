;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro some (type val)
  "Return VAL when it is of type TYPE, otherwise return nil. (Conservative version)"
  `(and (cl-typep ,val ',type) ,val))

(some integer 8)
(some (pair-of integer) '(7 . 8))
(some integer "hello")

;; (cond-let
;;   (((the-integer (some integer x))) (* 2 the-integer))
;;   (((the-string  (some string  x))) (concat "hello " the-string))
;;   (t "no integer or string"))


(internal--build-bindings '((x 1) (y 2)))

;; ((x (and t 1)) (y (and x 2)))



(defmacro cond-let (&rest clauses)
  ;; (debug nil clauses)
  (prn "clauses: %S" clauses)
  (cond
    ((null clauses) nil)
    ((eq (caar clauses) t) (cadar clauses))
    (t `(if-let ,(caar clauses)
	        (progn ,@(cdar clauses))
	        (cond-let ,@(cdr clauses))))))

(defmacro cond-let (&rest clauses)
  ;; (debug nil clauses)
  (prn "clauses: %S" clauses)
  (cond
    ((null clauses) nil)
    ((eq (caar clauses) t)
      ;; (debug nil clauses)
      ;; (cadar clauses)
      `(progn ,@(cdar clauses)))
    (t `(if-let ,(caar clauses)
	        (progn ,@(cdar clauses))
	        (cond-let ,@(cdr clauses))))))

(setq x "world")
(setq x 9)
(setq x nil)

(if-let ((the-integer (some integer x)))
  (progn
    (prn "int case")
    (* 2 the-integer))
  (if-let ((the-string (some string  x)))
    (progn
      (prn "string case")
      (concat "hello " the-string))
    (progn
      (prn "t case")
      "no integer or string")))

(cond-let
  (((the-integer (some integer x))) (prn "int case") (* 2 the-integer))
  (t (prn "t case") "no integer or string"))

(cond-let
  (((the-integer (some integer x))) (prn "int case") (* 2 the-integer))
  (((the-string  (some string  x))) (prn "string case") (concat "hello " the-string))
  (t (prn "t case") "no integer or string"))

(if-let ((the-integer (some integer x)))
  (progn
    (prn "int case")
    (* 2 the-integer))
  (if-let ((the-string (some string x)))
    (progn
      (prn "string case")
      (concat "hello " the-string))    
    (progn
      (prn "t case")
      "no integer or string")))


