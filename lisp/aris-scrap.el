;; -*- lexical-binding: nil; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-fib-benchmarks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (div-mod (n : integer) (d : integer)) => (pair-of integer)
  `(,(/ n d) . ,(% n d)))

;; ... expands to:
(defun* div-mod ((n : integer) (d : integer)) => (pair-of integer)
  `(,(/ n d) \,(% n d)))

(div-mod 19 8) ;; => (2 . 3)
(div-mod -19 8) ;; => (-2 . -3)
(div-mod 19 -8) ;; => (-2 . 3)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (input &optional accumulator)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
This is from Norvig."
  (prn "(flatten %s %s)" input accumulator)
  (let ((result
          (with-indentation
            (cond
              ((null input) accumulator)
              ((atom input) (cons input accumulator))
              (t (flatten
                   (first input)
                   (flatten (rest input) accumulator)))))))
    (prn "⇒ %s" result)
    result))

(flatten '(this (is a) (list (with lots) (of (nested stuff)))))

(flatten '(one two))

(let ((*pipe--verbose* t))
  (|> '(1 2 3 4 5 (6 7 8) 9 10) cdr cdr cdr car))

(let ((*pipe--verbose* t))
  (|> '(this (is a) (list (with lots) (of (nested stuff))))
    cdr cdr car cdr car car))

(let ((*pipe--verbose* t))
  (|> '(this (is a) (list (with lots) (of (nested stuff))))
    rest rest first rest first first))
