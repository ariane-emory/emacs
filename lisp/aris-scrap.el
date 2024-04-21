;; -*- lexical-binding: nil; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-fib-benchmarks)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo ((n : positive-integer)) => positive-integer
  (* n n))

(defun foo (n)
  (cl-check-type n positive-integer)
  (let ((foo-return-1126 (* n n)))
    (unless (cl-typep foo-return-1126 'positive-integer)
      (signal 'wrong-type-return (list 'positive-integer foo-return-1126)))
    foo-return-1126))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only type return:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo (n) => positive-integer
  (* n n))

(defun foo (n)
  (let ((foo-return-1127 (* n n)))
    (unless (cl-typep foo-return-1127 'positive-integer)
      (signal 'wrong-type-return (list 'positive-integer foo-return-1127)))
    foo-return-1127))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only type argument:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo ((n : positive-integer))
  (* n n))

(defun foo (n)
  (cl-check-type n positive-integer)
  (* n n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type neither:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo (n)
  (* n n))

(defun foo (n)
  (* n n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;q;;;;;;;;;;;;;;;;;;;;;;;;
(defun-memo bar (x y)
  (prn "Calculate %s x %s.." x y)
  (* x y))

(bar 7 8)
(bar 7 8)
(bar 7 9)

(symbol-function 'bar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (dotimes (n 100)
    (let ((rep-count (1+ n)))
      (clear-memos 'untyped-memoized-until-fib)
      (let ((result (benchmark-run rep-count (untyped-memoized-until-fib 1000))))
        (prn "%3d reps = %.5f seconds." rep-count (first result)))))
  (print-memos 'untyped-memoized-until-fib))


(ignore!
  (progn
    (dotimes (n 100)
      (let ((rep-count (1+ n)))
        (clear-memos 'untyped-memoized-naive-fib)
        (let ((result (benchmark-run rep-count (untyped-memoized-naive-fib 1000))))
          (prn "%3d reps = %.5f seconds." rep-count (first result)))))
    (print-memos 'untyped-memoized-naive-fib)
    )

  (untyped-memoized-naive-fib 500)
  (untyped-memoized-naive-fib 1000)
  (untyped-memoized-naive-fib 1500)
  (untyped-memoized-naive-fib 2000))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-memo ack (m n)
  "Compute the Ackermann function A(m, n)."
  (cond
    ((zerop m) (1+ n))
    ((zerop n) (ack (1- m) 1))
    (t (ack (1- m) (ack m (1- n))))))

(defun-memo ack-iter (m n)
  "Compute the Ackermann function A(m, n)."
  (with-gensyms (block)
    (cl-block block
      (cl-tagbody
        loop
        (cond
          ((zerop m)
            (cl-return-from block (1+ n)))
          ((zerop n)
            (cl-decf m)
            (setq n 1)
            (go loop))
          (t
            (cl-decf m)
            (setq n (ack-iter m (1- n)))
            (go loop)))))))

;; Test the Ack function
(message "Ack-Iter(1, 1) = %d" (ack-iter 1 1))
(message "Ack-Iter(3, 2) = %d" (ack-iter 3 2))
(message "Ack-Iter(4, 1) = %d" (ack-iter 4 1))

;; Test the Ack function
(message "Ack(1, 1) = %d" (ack 1 1))
(message "Ack(3, 2) = %d" (ack 3 2))
(message "Ack(4, 1) = %d" (ack 4 1))

(print-memos 'ack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun flatten (input &optional accumulator)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
This is from Norvig."
  (cond
    ((null input) accumulator)
    ((atom input) (cons input accumulator))
    (t (flatten (first input) (flatten (rest input) accumulator)))))

(flatten '(this (is a) (list (with lots) (of (nested stuff)))))
