;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--alists)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (match2 '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))

    (let ( (*mp--verbose* t)
           (*mp--merge-duplicate-alist-keys* nil)
           (*mp--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

    (let ( (*mp--verbose* t)
           (*mp--merge-duplicate-alist-keys* t)
           (*mp--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

    (aris-merge-duplicate-alist-keys '((a 1) (a 2) (a 3) (a 4) (a 5) (b 8)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (let ((*wm--depth-indicator-enable*))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Do some simple arithmetic with a pipe:
      (|> 2 -> (+ _ 1) -> (* 3 _)) ;; ⇒ 9

      ;; Reset the pattern-call dispatcher's alist:
      (pd--reset) 

      ;; Define some simple functions:
      (def (double n) (|> n -> (+ _ _)))
      (def (square y) (|> y -> (* _ _)))
      (def (double-square y) (double (square y)))
      
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
      

      (|> 3)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      )))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(|||> '(a 1 a 2 a 3 b 1 b 2 c 3)
  -> (plist-to-alist _) ;;  ((a . 1) (a . 2) (a . 3) (b . 1) (b . 2) (c . 3))
  -> (merge-duplicate-alist-keys _)
  ;;-> (flatten-alist-values _)
  -> (alist-to-plist _)
  ;;-> _
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arg gen:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic case:
(|||> 5 -> (* _ _) -> (+ _ 8) -> (when (odd? _) (return (* _ 2))))

'((consp-head)
   (car-head)
   (consp-car-head)
   (car-head-length)
   (head . 5)
   (head-is-spec)
   (head-includes-init-form)
   (var . _)
   (init-form . 5)
   (body
     -> (* _ _)
     -> (+ _ 8)
     -> (when
         (odd? _)
         (return
           (* _ 2)))))

;; Named binding:
(|||> ((x)) 5 -> (* x x) -> (+ x 8) -> (when (odd? x) (return (* x 2))))

'((consp-head . t)
   (car-head x)
   (consp-car-head . t)
   (car-head-length . 1)
   (head
     (x))
   (head-is-spec . t)
   (head-includes-init-form)
   (var . x)
   (init-form)
   (body 5
     -> (* x x)
     -> (+ x 8)
     -> (when
         (odd? x)
         (return
           (* x 2)))))

;; Named binding with value:
(|||> ((x 5)) -> (* x x) -> (+ x 8) -> (when (odd? x) (return (* x 2))))

'((consp-head . t)
   (car-head x 5)
   (consp-car-head . t)
   (car-head-length . 2)
   (head
     (x 5))
   (head-is-spec . t)
   (head-includes-init-form . t)
   (var . x)
   (init-form)
   (body
     -> (* x x)
     -> (+ x 8)
     -> (when
         (odd? x)
         (return
           (* x 2)))))


