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

(confirm that (dostack (x stk) (when (eql? x 5) (stop!))) returns (6 7 8))
(confirm that stk returns (6 7 8))
(confirm that (dostack (x stk) (when (eql? x 5) (stop!))) returns nil)

(setq stk '(1 2 3 4 5 6 7 8))

(confirm that (dostack (x stk) (when (eql? x 5) (return!))) returns 5)




stk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(doforth (x '(1 2 :swap 3 4 5 6 :drop 7 8 9)))

(doforth (x '(1 2 :swap 3 4 5 6 :drop 7 8 9))
  (prn "item: %S" x)
  (push-out! x))

;; (let ( (*wm--divider-width* 50)
;;        (count 0))
;;   (doforth (x '(:dup 7 :mul 3 )) ;; (:plus 1 2 :over 3 :plus 4 5 6))
;;     (cond
;;       ;; ((eql? x 9) (stop!))
;;       ((eq? x :plus)
;;         (let* ( (left   (pop!))
;;                 (right  (pop!))
;;                 (result (+ left right)))
;;           ;; (push! result)
;;           (nconc (stack) (list result))
;;           (prn "added %d and %d to get %d" left right result)))
;;       ((eq? x :mul)
;;         (let* ( (left   (pop!))
;;                 (right  (pop!))
;;                 (result (* left right)))
;;           ;; (push! result)
;;           (prn "stack before concing %S: %S" (list result) (stack))
;;           (push-back! result)
;;           (prn "stack afte concing %S: %S" (list result) (stack))
;;           (prn "added %d and %d to get %d" left right result)))
;;       (t
;;         (push-back! x)
;;         (cl-incf count)
;;         (when (eql 10 count)
;;           (prn "STOP!")
;;           (stop!))))))

