;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some of these might not really be required (LOL):
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore! ; tophy's `let loop' syntax:
  (def* (fib-iter (n : Int) : Int)
    (let loop ((a 0) (b 1) (i n))
      (if (= i 0)
        a
        (loop b (+ a b) (- i 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pd--reset)
(pd--def (untyped-pattern-dispatch-fib 0) 0)
(pd--def (untyped-pattern-dispatch-fib 1) 1)
(pd--def (untyped-pattern-dispatch-fib n)
  (+ (untyped-pattern-dispatch-fib (1- n))
    (untyped-pattern-dispatch-fib (- n 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun untyped-naive-fib (n)
  (if (<= n 1) n
    (+ (untyped-naive-fib (- n 1)) (untyped-naive-fib (- n 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (typed-naive-fib (n : positive-integer)) => positive-integer
  (if (<= n 1) n
    (+ (typed-naive-fib (- n 1)) (typed-naive-fib (- n 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun untyped-pcase-fib (n)
  (pcase n
    (0 0)
    (1 1)
    (n (+ (untyped-pcase-fib (- n 1)) (untyped-pcase-fib (- n 2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (typed-pcase-fib (n : positive-integer)) => positive-integer
  (pcase n
    (0 0)
    (1 1)
    (n (+ (typed-pcase-fib (- n 1)) (typed-pcase-fib (- n 2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun untyped-piped-pcase-fib (n)
  (pcase n
    (0 0)
    (1 1)
    (n (|> n 1- untyped-piped-pcase-fib
         (+ _ (|> n (- _ 2) untyped-piped-pcase-fib))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (typed-piped-pcase-fib (n : positive-integer)) => positive-integer
  (pcase n
    (0 0)
    (1 1)
    (n (|> n 1- typed-piped-pcase-fib
         (+ _ (|> n (- _ 2) typed-piped-pcase-fib))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun untyped-piped-iter-fib (n)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (typed-piped-iter-fib (n : positive-integer)) => positive-integer
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun untyped-tagbody-fib (n)
  (with-gensyms (block)
    (let ((a 0) (b 1) (i n))
      (cl-block block
        (cl-tagbody
          loop
          (setq
            i (1- i)
            b (+ a b)
            a (- b a))
          (unless (zerop i) (go loop))
          (cl-return-from block a))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (typed-tagbody-fib (n : positive-integer)) => positive-integer
  (with-gensyms (block)
    (let ((a 0) (b 1) (i n))
      (cl-block block
        (cl-tagbody
          loop
          (setq
            i (1- i)
            b (+ a b)
            a (- b a))
          (unless (zerop i) (go loop))
          (cl-return-from block a))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun untyped-until-fib (n)
  (with-gensyms (block)
    (let ((a 0) (b 1) (i n))
      (until (zerop i)
        (setq
          i (1- i)
          b (+ a b)
          a (- b a)))
      a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (typed-until-fib (n : positive-integer)) => positive-integer
  (with-gensyms (block)
    (let ((a 0) (b 1) (i n))
      (until (zerop i)
        (setq
          i (1- i)
          b (+ a b)
          a (- b a)))
      a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore! ;; run the sexps by hand, the file would take to long to load otherwise.
  (progn
    (setq reps 20)
    (setq n 20)
    (benchmark-run reps (untyped-naive-fib n)) ;; => (0.047994999999999996 0 0.0)
    (benchmark-run reps (typed-naive-fib n)) ;; => (0.162711 0 0.0)

    (benchmark-run reps (untyped-pcase-fib n)) ;; => (0.075735 0 0.0)
    (benchmark-run reps (typed-pcase-fib n)) ;; (0.192546 0 0.0)

    (benchmark-run reps (untyped-piped-iter-fib n)) ;; => (0.013763000000000001 0 0.0)
    (benchmark-run reps (typed-piped-iter-fib n)) ;; => (0.020678000000000002 0 0.0)
    
    (benchmark-run reps (untyped-tagbody-fib n)) ;; => (0.000206 0 0.0)
    (benchmark-run reps (typed-tagbody-fib n)) ;; => (0.000183 0 0.0)

    (benchmark-run reps (untyped-until-fib n)) ;; => (0.000137 0 0.0)
    (benchmark-run reps (typed-until-fib n)) ;; => (0.000158 0 0.0)

    (benchmark-run reps (untyped-piped-pcase-fib n)) ;; => (398.437878 2690 276.91953)
    (benchmark-run reps (typed-piped-pcase-fib n)) ;; (188.736017 1369 131.53158799999997)    

    (benchmark-run reps (untyped-pattern-dispatch-fib n)) ;; => (283.103026 1793 179.31079300000005)
    ) ; END OF progn
  ) ; END OF ignore!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
