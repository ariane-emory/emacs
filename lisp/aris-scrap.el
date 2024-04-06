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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (let ((*wm--depth-indicator-enable*))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Reset the pattern-call dispatcher's alist:
      (pd--reset)

      ;; Define some simple functions:
      (def (double n) (|> n (+ _ _)))
      (def (square y) (|> y (* _ _)))
      (def (double-square y) (double (square y)))
      
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIPE ARG GEN:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BASIC CASES:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(--pipe-args 5 (* _ _) (+ _ 8) (when (odd? _) (return (* _ 2))))

'( (consp-head)
   (car-head)
   (consp-car-head)
   (car-head-length)
   (head-is-spec)
   (head-is-spec-with-init-form)
   (head . 5)
   (var . _)
   (init-form . 5)
   (body 
     (* _ _)
     (+ _ 8)
     (when
       (odd? _)
       (return
         (* _ 2)))))

;; Named binding:
(--pipe-args ((z)) 5 (* z z) (+ z 8) (when (odd? z) (return (* z 2))))

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
   (body 
     (* z z)
     (+ z 8)
     (when
       (odd? z)
       (return
         (* z 2)))))

;; Named binding with value:
(--pipe-args ((z 5)) (* z z) (+ z 8) (when (odd? z) (return (* z 2))))

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
   (body 
     (* z z)
     (+ z 8)
     (when
       (odd? z)
       (return
         (* z 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMPTY BODY CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(--pipe-args ((z 5))) ;; nothing entered in pipe?

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

(--pipe-args ((z))) ;; nothing entered in pipe

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

(--pipe-args);; nothing entered in pipe

'() ;; ILLEGAL EXPANSION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ONE ARG CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(--pipe-args ((z 5))) ;; nothing entered in pipe?

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

(--pipe-args ((z))) ;; nothing entered in pipe

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

(--pipe-args 5);; nothing entered in pipe

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
;; TEST ARG GEN VERSION:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(|> 8 :(prn "Hello"))
(|> 8 :(when (eql _ 8) (return (* 7 6))))
(|> 8 :?(when (eql _ 8) (* 7 6)))
(|> 8 :?(when (eql _ 7) (* 7 6)))

(|> 5 6)
(|> ((x)) 5 6)
(|> ((x 5)) 6)
(--pipe-args 5 6)
(--pipe-args ((x)) 5 6)
(--pipe-args ((x 5)) 6)

(|> 5 (* _ _) (+ _ 8))
(|> ((z)) 5 (* z z) (+ z 8))
(|> ((z 5)) (* z z) (+ z 8))

(--pipe-args 5 (* _ _) (+ _ 8))
(--pipe-args ((z)) 5 (* z z) (+ z 8))
(--pipe-args ((z 5)) (* z z) (+ z 8))

(setq y 10)

(|> ((e)) 5 (+ e 7) double (+ e 3) neg)
(|> ((e 5)) (+ e 7) double (+ e 3) neg (lambda (x) (* x 3)) (return 999) :(when (< e 40) 1) :(prn "Done!") (+ y e))
(|> 2  :(prn "hello") (+ 2 _) :?(when t 99))

;; (defun double (x) (* x 2))
(|> 5 (+ _ 7) double :(prn "hello") (+ _ 3) neg :?(when (negative? _) (neg _)) :(when (> _ 20) (return 11)) 1)
(|> 5 (+ _ 7) :(prn  "hello") (+ _ 3) (return 99) neg :?(when (negative? _) (neg _)) :(when (> _ 20) (return 11)) 1)
(|> 5 (+ _ 7) double :(prn "hello") (+ _ 3) (return 99) neg :?(when (negative? _) (neg _)) :(when (> _ 20) (return 11)) 1)

(|> 5 (+ _ 7) double :(prn "hello") (+ _ 3) neg :when? negative? neg :when? (> _ 20) (return 11) 1)
(|> 5 (+ _ 7) :(prn  "hello") (+ _ 3) (return 99) neg :when? negative? neg :when? (> _ 20) (return 11) 1)
(|> 5 (+ _ 7) double :(prn "hello") (+ _ 3) (return 99) neg :when? negative? neg :when? (> _ 20) (return 11) 1)

;;;;;;;;;;;
(|> ((x)) 5 (+ x 7) double :(prn "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return 11)) 1)
(|> ((x)) 5 (+ x 7) :(prn "hello") (+ x 3) (return 99) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return 11)) 1)
(|> ((x)) 5 (+ x 7) double :(prn "hello") (+ x 3) (return 99) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return 11)) 1)

(|> ((x)) 5 (+ x 7) double :(prn "hello") (+ x 3) neg :when? negative? neg :when? (> x 20) (return 11) 1)
(|> ((x)) 5 (+ x 7) :(prn "hello") (+ x 3) (return 99) neg :when? negative? neg :when? (> x 20) (return 11) 1)
(|> ((x)) 5 (+ x 7) double :(prn "hello") (+ x 3) (return 99) neg :when? negative? neg :when? (> x 20) (return 11) 1)

;;;;;;;;;;;
(|> ((x 5)) (+ x 7) double :(prn "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return 11)) 1)
(|> ((x 5)) (+ x 7) :(prn "hello") (+ x 3) (return 99) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return 11)) 1)
(|> ((x 5)) (+ x 7) double :(prn "hello") (+ x 3) (return 99) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return 11)) 1)

(|> ((x 5)) (+ x 7) double :(prn "hello") (+ x 3) neg :when? negative? neg :when? (> x 20) (return 11) 1)
(|> ((x 5)) (+ x 7) :(prn "hello") (+ x 3) (return 99) neg :when? negative? neg :when? (> x 20) (return 11) 1)
(|> ((x 5)) (+ x 7) double :(prn "hello") (+ x 3) (return 99) neg :when? negative? neg :when? (> x 20) (return 11) 1)

