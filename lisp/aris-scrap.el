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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(symbol-function 'bar)
(lambda
  (&rest args)
  (let ((val (gethash args #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()) 'memo-not-found-898)))
    (if (eq val 'memo-not-found-898)
      (setf (gethash args #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()))
        (apply
          (lambda (x y)
            (prn "Calculate %s x %s.." x y)
            (* x y))
          args))
      val)))

(lambda (&rest args)
  (let ((val (gethash args #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()) 'memo-not-found-896)))
    (if (eq val 'memo-not-found-896)
      (setf (gethash args #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()))
        (apply
          (lambda
            (x y)
            (prn "Calculate %s x %s.." x y)
            (* x y))
          args))
      val)))

(lambda (&rest args)
  (let ((val (gethash args #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()) 'memo-not-found-895)))
    (if (eq val 'memo-not-found-895)
      (setf (gethash args #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()))
        (apply
          (lambda (x y)
            (prn "Calculate %s x %s.." x y)
            (* x y))
          args))
      val)))

(lambda (&rest args)
  (let* ( (k args)
          (val (gethash k #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()) 'memo-not-found-893)))
    (if (eq val 'memo-not-found-893)
      (setf (gethash k #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()))
        (apply
          (lambda (x y)
            (prn "Calculate %s x %s.." x y)
            (* x y))
          args))
      val)))
