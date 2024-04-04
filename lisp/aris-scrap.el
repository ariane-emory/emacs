;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--alist-funs)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (pd--reset)
    (def (fib 0) 0)
    (def (fib 1) 1)
    (def (fib n)  (+ (fib (- n 1)) (fib (- n 2))))
    (def (double n) (+ n n))
    (def (square y) (* y y))
    ;; (def (double-square y) (double 2 (square y)))
    ;; (double-square 3)

    (prn (make-string 80 ?\=))
    (let ( (*pd--verbose* t)
           (*match-pattern--verbose* nil)
           (*match-pattern2--verbose* nil))
      (error-unless "You broke (fib 4): %s" '(it) (= 3 (fib 4)))
      (error-unless "You broke (fib 10): %s" '(it) (= 55 (fib 10)))
      (error-unless "You broke (double 9): %s" '(it) (= 18 (double 9)))
      (error-unless "You broke (square 7): %s" '(it) (= 49 (square 7)))
      
      (prn "Prnting the table:")
      (pd--prnt-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pd--get-group 'fib)
(pd--print-group (pd--get-group 'fib))
(pd--format-group-as-lines (pd--get-group 'fib))
(pd--format-group-as-string (pd--get-group 'fib))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (match2 '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))

    (let ( (*match-pattern--verbose* t)
           (*match-pattern--merge-duplicate-alist-keys* nil)
           (*match-pattern--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

    (let ( (*match-pattern--verbose* t)
           (*match-pattern--merge-duplicate-alist-keys* t)
           (*match-pattern--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

    (aris-merge-duplicate-alist-keys '((a 1) (a 2) (a 3) (a 4) (a 5) (b 8)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((*with-messages--depth-indicator-enable*))
  ;; Do some simple arithmetic with a pipe:
  (|> 2 -> (+ _ 1) -> (* 3 _)) ;; ⇒ 9

  ;; Reset the pattern-call dispatcher's alist.
  (pd--reset) 

  ;; Define some simple functions::
  (def (double n) (|> n -> (+ _ _)))
  (def (square y) (|> y -> (* _ _)))
  
  ;; Define a fib:
  (def (fib 0) 0)
  (def (fib 1) 1)
  (def (fib n)
    (|> (prn "Calculating (fib %d) using a pipe-based fib..." n)
      (|> n -> (- _ 1) -> (fib _)) ->
      (+ _ (|> n -> (- _ 2) -> (fib _) ->
             (prn "Calculated (fib %d) = %d" n _) _))))

  ;; Call it with some output commenting on the proceedings:
  (|>
    3 -> (prn "Starting out with %d" _) (+ _ (|> 2 -> (+ _ 5))) ->
    (prn "Getting the result of (fib %d)" _) (fib _) ->
    "I'm just a harmless string sitting in the pipe doing doing nothing."
    (prn "Result =  %d" _) _) ;; ⇒ 55

  (|> 5 -> (square _) -> (when (odd? _) (return (double _)) _))
  (|> 6 -> (square _) -> (when (odd? _) (return (double _)) _))
  )

(|> 3)
;; Output that eventually ends with this (with free automatic indentation as a side
;; effect of how my message-printing code works):
;;           Calculating (fib 3)...
;;             Calculating (fib 2)...
;;             Calculated (fib 2) = 0
;;           Calculated (fib 3) = 1
;;         Calculated (fib 5) = 2
;;         Calculating (fib 4)...
;;           Calculating (fib 3)...
;;             Calculating (fib 2)...
;;             Calculated (fib 2) = 0
;;           Calculated (fib 3) = 1
;;           Calculating (fib 2)...
;;           Calculated (fib 2) = 0
;;         Calculated (fib 4) = 1
;;       Calculated (fib 6) = 3
;;     Calculated (fib 8) = 8
;;   Calculated (fib 10) = 21
;; Result =  55


