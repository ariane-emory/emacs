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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arg gen:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BASIC CASES:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pipe-args 5 -> (* _ _) -> (+ _ 8) -> (when (odd? _) (return (* _ 2))))

'( (consp-head)
   (car-head)
   (consp-car-head)
   (car-head-length)
   (head-is-spec)
   (head-is-spec-with-init-form)
   (head . 5)
   (var . _)
   (init-form . 5)
   (body ->
     (* _ _)
     ->
     (+ _ 8)
     ->
     (when
       (odd? _)
       (return
         (* _ 2)))))

;; Named binding:
(pipe-args ((z)) 5 -> (* z z) -> (+ z 8) -> (when (odd? z) (return (* z 2))))

'( (consp-head . t)
   (car-head z)
   (consp-car-head . t)
   (car-head-length . 1)
   (head-is-spec . t)
   (head-is-spec-with-init-form)
   (head
     (z))
   (var . z)
   (init-form . 5)
   (body ->
     (* z z)
     ->
     (+ z 8)
     ->
     (when
       (odd? z)
       (return
         (* z 2)))))

;; Named binding with value:
(pipe-args ((z 5)) -> (* z z) -> (+ z 8) -> (when (odd? z) (return (* z 2))))

'( (consp-head . t)
   (car-head z 5)
   (consp-car-head . t)
   (car-head-length . 2)
   (head-is-spec . t)
   (head-is-spec-with-init-form . t)
   (head
     (z 5))
   (var . z)
   (init-form . 5)
   (body ->
     (* z z)
     ->
     (+ z 8)
     ->
     (when
       (odd? z)
       (return
         (* z 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMPTY BODY CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pipe-args ((z 5))) ;; nothing entered in pipe?

'( (consp-head . t)
   (car-head z 5)
   (consp-car-head . t)
   (car-head-length . 2)
   (head-is-spec . t)
   (head-is-spec-with-init-form . t)
   (head
     (z 5))
   (var . z)
   (init-form . 5)
   (body))

(pipe-args ((z))) ;; nothing entered in pipe

'( (consp-head . t)
   (car-head z)
   (consp-car-head . t)
   (car-head-length . 1)
   (head-is-spec . t)
   (head-is-spec-with-init-form)
   (head
     (z))
   (var . z)
   (init-form)
   (body))

(pipe-args);; nothing entered in pipe

'() ;; ILLEGAL EXPANSION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ONE ARG CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pipe-args ((z 5))) ;; nothing entered in pipe?

'( (consp-head . t)
   (car-head z 5)
   (consp-car-head . t)
   (car-head-length . 2)
   (head-is-spec . t)
   (head-is-spec-with-init-form . t)
   (head
     (z 5))
   (var . z)
   (init-form . 5)
   (body))

(pipe-args ((z))) ;; nothing entered in pipe

'( (consp-head . t)
   (car-head z)
   (consp-car-head . t)
   (car-head-length . 1)
   (head-is-spec . t)
   (head-is-spec-with-init-form)
   (head
     (z))
   (var . z)
   (init-form)
   (body))

(pipe-args 5);; nothing entered in pipe

'( (consp-head)
   (car-head)
   (consp-car-head)
   (car-head-length)
   (head-is-spec)
   (head-is-spec-with-init-form)
   (head . 5)
   (var . _)
   (init-form . 5)
   (body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GOOD EXPANSION:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (pipe--print
    (make-string 80 61))
  (let
    ((last 5)
      (sym '_)
      (_ nil))
    (catch 'return
      (mapcr
        '(->
           (* _ _)
           ->
           (+ _ 8))
        (lambda
          (expr)
          (pipe--print
            (make-string 80 61))
          (pipe--print "Expr: %S" expr)
          (pipe--print "Var:  %S" _)
          (pipe--print "Last: %S" last)
          (cl-flet
            ((expr-fun
               `(lambda
                  (sym)
                  (cl-flet
                    ((return
                       (,sym)
                       (throw 'return ,sym)))
                    (let
                      ((result ,expr))
                      result)))))
            (cond
              ((eq expr '->)
                (setq _ last)
                (setq last nil)
                (pipe--print "Updated by arrow! Var is %S, last is %S" _ last))
              (t
                (setq last
                  (expr-fun _))
                (pipe--print "Updated by call! Var is %S, last is %S" _ last))))))
      (throw 'return
        (progn
          (pipe--print
            (make-string 80 61))
          (pipe--print "Returning: %S"
            (or last _))
          (pipe--print
            (make-string 80 61))
          (pipe--print "Var:  %S" _)
          (pipe--print "Last: %S" last)
          (or last _))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST ARG GEN VERSION:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alist-get 'body (pipe-args 5 -> (* _ _) -> (+ _ 8)))
(|> 5 -> (* _ _) -> (+ _ 8))

(pipe-args 5 -> (* _ _) -> (+ _ 8))
((consp-head)
  (car-head)
  (consp-car-head)
  (car-head-length)
  (head-is-spec)
  (head-is-spec-with-init-form)
  (head . 5)
  (var . _)
  (init-form . 5)
  (body ->
    (* _ _)
    ->
    (+ _ 8)))



