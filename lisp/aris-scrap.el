;; -*- lexical-binding: nil; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'peter-norvigs-funs--defun-memo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (div-mod (n : positive-integer) (d : positive-integer)) => (pair-of positive-integer)
  `(,(/ n d) . ,(% n d)))

;; ... expands to:
(defun* div-mod ((n : positive-integer) (d : positive-integer)) => (pair-of positive-integer)
  `(,(/ n d) \,(% n d)))

(div-mod 19 8) ;; => (2 . 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  type both argument and return::
(defun* foo ((n : positive-integer)) => positive-integer (* n n))

(defun foo (n)
  (cl-check-type n positive-integer)
  (let ((foo-return-1126 (* n n)))
    (unless (cl-typep foo-return-1126 'positive-integer)
      (signal 'wrong-type-return (list 'positive-integer foo-return-1126)))
    foo-return-1126))

;; only type return:
(defun* foo (n) => positive-integer (* n n))

(defun foo (n)
  (let ((foo-return-1127 (* n n)))
    (unless (cl-typep foo-return-1127 'positive-integer)
      (signal 'wrong-type-return (list 'positive-integer foo-return-1127)))
    foo-return-1127))

;; only type argument:
(defun* foo ((n : positive-integer)) (* n n))

(defun foo (n)
  (cl-check-type n positive-integer)
  (* n n))

;; type neither:
(defun* foo (n) (* n n))

(defun foo (n)
  (* n n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-memo bar (x y) (prn "Calculate %s x %s.." x y ) (* x y))

(bar 7 8)
(bar 7 8)
(bar 7 9)
;; (bar 7 9 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

