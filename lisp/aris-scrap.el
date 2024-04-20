;; -*- lexical-binding: nil; fill-column: 80; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'aris-funs--alists)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--lists)
(require 'aris-funs--match-pattern)
(require 'aris-funs--match-pattern2)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--stacks)
(require 'aris-funs--unsorted)
(require 'aris-funs--ignorebang)
(require 'aris-types)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; naming examples for an imaginary frobnosticate-widget package:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fw--frobnosticate-widget     ;; a public-facing function in the frobnosticate-widget package.
;; --fw-frobnostication-helper  ;; an internal function in the frobnosticate-widget package.
;; --do-fw-stuff                ;; an internal function in the frobnosticate-widget package.
;; *fw--frobnostication-level*  ;; a public-facing (usually customizable) variable in the frobnosticate-widget package.
;; *--fw-frobnostication-count* ;; an internal variable in the frobnosticate-widget package not meant for customization
;; frobnosticate-widget         ;; either a public-facing function in the frobnosticate-widget package or a convenient
;;                              ;;  alias for fw--frobnosticate-widget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'match 'pcase)
(defalias 'def 'def*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (div-mod (n : positive-integer) (d : positive-integer)) => (pair-of positive-integer)
  `(,(/ n d) . ,(% n d)))

;; ... expands to:
(defun* div-mod ((n : positive-integer) (d : positive-integer)) => (pair-of positive-integer)
  `(,(/ n d) \,(% n d)))

;; ... which expands to:
(defun div-mod
  (n d)
  (cl-check-type n positive-integer)
  (cl-check-type d positive-integer)
  (let ((div-mod-return-1790 (cons (/ n d) (% n d))))
    (unless (cl-typep div-mod-return-1790 '(pair-of positive-integer))
      (signal 'wrong-type-return
        (list 'pair-of positive-integer div-mod-return-17q90)))
    div-mod-return-1790))

(div-mod 19 8) ;; => (2 . 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (def (fib-iter (n : Int) : Int)
    (let loop ((a 0) (b 1) (i n))
      (if (= i 0)
        a
        (loop b (+ a b) (- i 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pd--reset)
(pdef (pattern-dispatch-fib 0) 0)
(pdef (pattern-dispatch-fib 1) 1)
(pdef (pattern-dispatch-fib n) (+ (pattern-dispatch-fib (1- n)) (pattern-dispatch-fib (- n 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (typed-pcase-fib (n : positive-integer)) => positive-integer
  (pcase n
    (0 0)
    (1 1)
    (n (+ (typed-pcase-fib (- n 1)) (typed-pcase-fib (- n 2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (typed-piped-pcase-fib (n : positive-integer)) => positive-integer
  (match n
    (0 0)
    (1 1)
    (n (|> n 1- typed-piped-pcase-fib (+ _ (|> n (- _ 2) typed-piped-pcase-fib))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (typed-piped-iter-fib (n : positive-integer)) => positive-integer
  ;; "Non-recursive version of a pipe-based `fib'."
  (|>
    ;; basically just an env alist:
    `( (a . 0)
       (b . 1)
       (i . ,n))
    'loop
    ;; update the env:
    `( (a . ,(alist-get 'b _))
       (b . ,(+ (alist-get 'a _) (alist-get 'b _)))
       (i . ,(1- (alist-get 'i _))))
    ;; loop until i = 0:
    :unless (zero? (alist-get 'i _)) :go 'loop
    ;; extract return value (else positive-integer return type wouldn't satisy):
    (alist-get 'a _)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun piped-iter-fib (n)
  ;; "Non-recursive version of a pipe-based `fib'."
  (|>
    ;; basically just an env alist:
    `( (a . 0)
       (b . 1)
       (i . ,n))
    'loop
    ;; update the env:
    `( (a . ,(alist-get 'b _))
       (b . ,(+ (alist-get 'a _) (alist-get 'b _)))
       (i . ,(1- (alist-get 'i _))))
    ;; loop until i = 0:
    :unless (zero? (alist-get 'i _)) :go 'loop
    ;; extract return value (else positive-integer return type wouldn't satisy):
    (alist-get 'a _)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun traditional-naive-fib (n)
  (if (<= n 1) n
    (+ (traditional-naive-fib (- n 1)) (traditional-naive-fib (- n 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (typed-naive-fib (n : positive-integer)) => positive-integer
  (if (<= n 1) n
    (+ (typed-naive-fib (- n 1)) (typed-naive-fib (- n 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (setq reps 10)
    (setq n 20)
    (benchmark-run reps (traditional-naive-fib n)) ;; => (0.023791 0 0.0)
    (benchmark-run reps (pattern-dispatch-fib n)) ;; => (283.103026 1793 179.31079300000005)
    (benchmark-run reps (typed-naive-fib n)) ;; => (0.075895 0 0.0)
    (benchmark-run reps (typed-pcase-fib n)) ;; => (0.094119 0 0.0)
    (benchmark-run reps (typed-piped-pcase-fib n)) ;; (188.736017 1369 131.53158799999997)    
    (benchmark-run reps (typed-piped-iter-fib n)) ;; => (0.013819000000000001 0 0.0)
    (benchmark-run reps (piped-iter-fib n)) ;; => (0.113213 1 0.10347000000001572)
    )
  ) ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
