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
(def (match-rec-fib (n : positive-integer)) => positive-integer
  (match n
    (0 0)
    (1 1)
    (n (+ (match-rec-fib (- n 1)) (match-rec-fib (- n 2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (pipe-match-rec-fib (n : positive-integer)) => positive-integer
  (match n
    (0 0)
    (1 1)
    (n (|> n 1- pipe-match-rec-fib (+ _ (|> n (- _ 2) pipe-match-rec-fib))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (pipe-iter-fib (n : positive-integer)) => positive-integer
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
(ignore!
  (progn
    (setq reps 10)
    (setq n 20)
    (benchmark-run reps (match-rec-fib n)) ;; (0.123622 0 0.0)
    (benchmark-run reps (pipe-match-rec-fib n)) ;; (185.330855 1257 129.114517)
    (benchmark-run reps (pipe-iter-fib n))) ;; (0.007254 0 0.0)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
