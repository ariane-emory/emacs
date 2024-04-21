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
(defun norvigs-flatten (input &optional accumulator)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
This is adapted from the version in Peter Norvig's book."
  ;; (prn "(flatten %s %s)" input accumulator)
  (let ((result
          ;; (with-indentation
          (cond
            ((null input) accumulator)
            ((atom input) (cons input accumulator))
            (t (norvigs-flatten
                 (first input)
                 (norvigs-flatten (rest input) accumulator)))))) ;)
    ;; (prn "⇒ %s" result)
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (lst)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
I wrote this one myself."
  (when lst
    (if (consp (car lst))
      (append (flatten (pop lst)) (flatten lst))
      (cons (pop lst) (flatten lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(norvigs-flatten '(this (is a) (list (with lots) (of (nested stuff))))) ;; => (this is a list with lots of nested stuff)
(flatten '(this (is a) (list (with lots) (of (nested stuff))))) ;; => (this is a list with lots of nested stuff)

(benchmark-run 1000 (norvigs-flatten '(this (is a) (list (with lots) (of (nested stuff)))))) ;; => (0.010135 0 0.0)
(benchmark-run 1000 (flatten '(this (is a) (list (with lots) (of (nested stuff)))))) ;; => (0.005893 0 0.0)

