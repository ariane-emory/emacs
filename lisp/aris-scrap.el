;; -*- lexical-binding: nil; fill-column: 90; lisp-indent-offset: 2; eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'aris-funs--alists)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--lists)
(require 'aris-funs--match-pattern)
(require 'aris-funs--match-pattern2)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--stacks)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATCH2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (match2 '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))

    (let ( (*mp--use-new-pipe-macro* t)
           (*mp--verbose* t)
           (*mp--merge-duplicate-alist-keys* nil)
           (*mp--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))
    
    (let ( (*mp--use-new-pipe-macro* nil)
           (*mp--verbose* t)
           (*mp--merge-duplicate-alist-keys* nil)
           (*mp--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

    (let ( (*mp--use-new-pipe-macro* t)
           (*mp--verbose* t)
           (*mp--merge-duplicate-alist-keys* t)
           (*mp--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))
    
    (let ( (*mp--use-new-pipe-macro* nil)
           (*mp--verbose* t)
           (*mp--merge-duplicate-alist-keys* t)
           (*mp--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))
    
    (merge-duplicate-alist-keys '((a 1) (a 2) (a 3) (a 4) (a 5) (b 8)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PATTERN-DISPATCH and some PIPE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (let ((*wm--depth-indicator-enable*))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Reset the pattern-call dispatcher's alist:
      (pd--reset)

      ;; Define some simple functions:
      (def (doub n) (|> n (+ _ _)))
      (def (sqr y) (|> y (* _ _)))
      (def (doub-sqr y) (doub (sqr y)))

      ;; Define a fib:
      (def (fib 0) 0)
      (def (fib 1) 1)
      (def (fib n)
        (|>
          :(prn "Calculating (fib %d) using a pipe-based fib..." n)
          (|> n
            (- _ 1)
            (fib _))
          (+ _
            (|> n
              (- _ 2)
              (fib _)
              :(prn "Calculated (fib %d) = %d" n _)))))

      ;; Call it with some output commenting on the proceedings:
      (|> 3
        :(prn "Starting out with %d" _)
        (+ _ (|> 2 (+ _ 5)))
        :(prn "Getting the result of (fib %d)" _)
        (fib _)
        :(prn "Result =  %d" _)) ;; ⇒ 55
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      )))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACKS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq stk '(1 2 3 4 5 6 7 8))

(dostack (x stk)
  (prn "Val: %S" x)
  (when (eql? x 5) (stop!)))

(dostack (x stk)
  (prn "Val: %S" x)
  (when (eql? x 5) (return!)))

(dostack (x stk)
  (prn "Val: %S" x)
  (when (eql? x 5) (return! 99)))

(dostack (x '(1 2 3 4 5 6 7 8))
  (prn "Val: %S" x)
  (when (eql? x 5) (stop!)))

(dostack (x '(1 2 3 4 5 6 7 8))
  (prn "Val: %S" x)
  (when (eql? x 5) (return! (x . (list (stack))))))

(--dostack-mini-forth (x '(1 2 :swap 3 4 5 6 7 8))
  (prn "Val: %S" x)
  (when (eql? x 5) (stop!)))


(--dostack-mini-forth (x '(1 2 :swap 3 4 5 6 7 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --dostack-mini-forth (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A dumb little Forth-like stack machine without enough operations to be very useful,
meant mainly for use in unit tests."
  ;; (let (out)
  (--dostack-validate-spec spec)
  (let* ( (out-sym (unless body (gensym "out-")))
          (body
            (or body
              (list (list 'setq out-sym (list 'cons (car spec) out-sym))))))
    `(let (,out-sym)
       (dostack ,spec
         (cond
           ((eq :dup x)    (dup!))
           ((eq :drop x)   (pop!))
           ((eq :over x)   (over!))
           ((eq :return x) (return!))
           ((eq :rotl x)   (rotl!))
           ((eq :rotr x)   (rotr!))
           ((eq :swap x)   (swap!))
           ((eq :stop x)   (stop!))
           (t ,@body)))
       ,out-sym)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(--dostack-mini-forth (x '(1 2 :swap 3 4 5 6 7 8)))

(--dostack-mini-forth (x '(1 2 :swap 3 4 5 6 7 8))
  (prn "%s" x))




