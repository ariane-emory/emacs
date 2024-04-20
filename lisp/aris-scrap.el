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


;; φ m n 0 = m + n
;; φ m n 1 = m * n
;; φ m n 2 = m ^ n

;; A 0 n             = n + 1
;; A (m + 1) 0       = A m 1
;; A (m + 1) (n + 1) = A m (A (m + 1) n)

(defun ackermann (m n)
  "Compute the Ackermann function A(m, n)."
  (cond
    ((zerop m) (1+ n))
    ((zerop n) (ackermann (1- m) 1))
    (t (ackermann (1- m) (ackermann m (1- n))))))

;; Test the Ackermann function
(message "Ackermann(1, 1) = %d" (ackermann 1 1))
(message "Ackermann(3, 2) = %d" (ackermann 3 2))
(message "Ackermann(4, 1) = %d" (ackermann 4 1))
