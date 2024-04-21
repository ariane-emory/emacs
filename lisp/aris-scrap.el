;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-fib-benchmarks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def* (div-mod (n : integer) (d : integer)) => (pair-of integer)
;;   `(,(/ n d) . ,(% n d)))

;; ;; ... expands to:
;; (defun* div-mod ((n : integer) (d : integer)) => (pair-of integer)
;;   `(,(/ n d) \,(% n d)))

;; (div-mod 19 8) ;; => (2 . 3)
;; (div-mod -19 8) ;; => (-2 . -3)
;; (div-mod 19 -8) ;; => (-2 . 3)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten1 (input &optional accumulator)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
This is adapted from the version in Peter Norvig's book."
  ;; (prn "(flatten %s %s)" input accumulator)
  (let ((result
          ;; (with-indentation
          (cond
            ((null input) accumulator)
            ((atom input) (cons input accumulator))
            (t (flatten1
                 (first input)
                 (flatten1 (rest input) accumulator)))))) ;)
    ;; (prn "⇒ %s" result)
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten2 (lst)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
I wrote this one myself."
  (when lst
    (if (consp (car lst))
      (append (flatten2 (pop lst)) (flatten2 lst))
      (cons (pop lst) (flatten2 lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(flatten1 '(this (is a) (list (with lots) (of (nested stuff))))) ;; => (this is a list with lots of nested stuff)
(flatten2 '(this (is a) (list (with lots) (of (nested stuff))))) ;; => (this is a list with lots of nested stuff)

(benchmark-run 100000 (flatten1 '(this (is a) (list (with lots) (of (nested stuff)))))) ;; => (3.13811 26 2.360741000000001)
(benchmark-run 100000 (flatten2 '(this (is a) (list (with lots) (of (nested stuff)))))) ;; => (1.2355470000000002 9 0.8095919999999985)



