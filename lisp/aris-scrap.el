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

(doforthy (x '(1 2 :swap 3 4 5 6 :drop 7 8 9))
  (cond
    ((eql? 3 x) (push-out! (* x 100)))
    ((odd? x)   (push-out! x))))

(|> ((e)) 5 (+ e 7) double (+ e 3) neg (lambda (n) (* 3 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full expansion of dostack-lite/pipe:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let
  ((final
     (let ( (e        nil)
            (var-sym 'e)
            (flag     nil))
       (cl-labels ( (flag-is? (test-flag)
                      (eq flag (--valid-pipe-flag test-flag)))
                    (set-flag! (new-flag &optional force)
                      (let ((new-flag (--valid-pipe-flag new-flag t)))
                        (cond
                          ((and flag new-flag (not force))
                            (error "Cannot set flag to %S when flag is already set to %S."
                              new-flag flag))
                          (force
                            (--pipe-print "FORCING FLAG FROM %S TO %S." flag new-flag)
                            (setq flag new-flag))
                          (t
                            (--pipe-print "Setting flag from %S to %S%s." flag new-flag
                              (if force " (forced)" ""))))
                        (setq flag new-flag)))
                    (unset-flag! nil
                      (when flag
                        (--pipe-print "Unsetting flag %S." flag)
                        (set-flag! nil)))
                    (store! (value)
                      (prog1
                        (setq e value)
                        (--pipe-print "Updated %S to %S." var-sym e)))
                    (labeled-print (label value)
                      (let* ((label   (format "%s:" label))
                              (whites (make-string (- 21 (length label)) 32))
                              (label  (concat label whites)))
                        (--pipe-print "%s%S" label value))))
         (prndiv)
         (--pipe-print "START")
         (prndiv)
         (catch 'return-3260


           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (let ((stack-4156 '(5 (+ e 7) double (+ e 3) neg (lambda (n) (* 3 n)))))
             (cl-labels
               ((len nil
                  (length stack-4156))
                 (stack nil
                   stack-4156)
                 (set-stack!
                   (new-stack)
                   (setq stack-4156 new-stack))
                 (push!
                   (val)
                   (push val stack-4156))
                 (require-len>=
                   (len)
                   (unless (length> stack-4156 (1- len))
                     (signal 'stack-underflow (list 'stack-4156))))
                 (pop! nil
                   (require-len>= 1)
                   (pop stack-4156)))
               (while stack-4156
                 (let ((expr (pop!)))
                   (prndiv)
                   (prn "dostack: %S" expr)


                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; Conent of the pipe macro's dostack body:
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   (prndiv)
                   (labeled-print "Current" expr)
                   (labeled-print "Remaining" (stack))
                   (labeled-print var-sym e)
                   (labeled-print "Flag" flag)
                   (if (--is-pipe-command? expr)
                     (set-flag! (alist-get expr *--pipe-commands-to-flags*))
                     (let ((result (eval (if (fun? expr)
                                           (list expr var-sym)
                                           (let ((return-label 'return-3260))
                                             `(cl-flet ((return! (value)
                                                          (throw ',return-label value)))
                                                ,expr))))))
                       (labeled-print "Expr result" result)
                       (cl-flet ((drop-next! nil
                                   (let ((next (pop!)))
                                     (--pipe-print "Popped 1st %S from %S." next
                                       (stack))
                                     (when
                                       (memq next *--pipe--arity-1-commands*)
                                       (--pipe-print "Popped command's argument %S from %S."
                                         (pop!)
                                         (stack)))
                                     (when (memq next *--pipe--arity-2-commands*)
                                       (error "Ignoring the %S command is not yet supported." next)))))
                         (cond
                           ((flag-is? :IGNORE)
                             (--pipe-print "Not setting %S because %S." result flag))
                           ((flag-is? :WHEN)
                             (when (not result)
                               (drop-next!)))
                           ((flag-is? :UNLESS)
                             (when result
                               (drop-next!)))
                           ((flag-is? :RETURN)
                             (--pipe-print "Returning due to command: %S" result)
                             (throw 'return-3260 result))
                           ((and (flag-is? :MAYBE) result)
                             (store! result))
                           ((and (flag-is? :MAYBE) (not result))
                             (--pipe-print "Ignoring %S." result))
                           (t
                             (store! result)))
                         (unset-flag!))))))))
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           
           (throw 'return-3260
             (progn
               (prndiv)
               (--pipe-print "Returning this because stack is empty: %S" e)
               (prndiv)
               e)))))))
  (--pipe-print "Pipe's final return: %S" final)
  final)

(let ((ctr 0))
  (doforthy (x '(1 2 3 4 5 6 7 8 9))
    (cl-incf ctr)
    (prndiv)
    (prn "ctr:     %S" ctr)
    (prn "current: %S" x)
    (prn "ahead:   %S" (stack))
    (when (eql? ctr 50) (return! 111))
    (cond
      ((even? x)  (push-back! x)))
    (prn "after:   %S" (stack))
    ))


(defvar *toggle-print* nil)

(defmacro toggled-print (first &rest rest)
  (when *toggle-print*
    `((prn ,first ,@rest))))


(defmacro use-toggled-print (&rest body)
  `(progn
     (prn "before")
     ,@'(1 2 3)
     ,@(toggled-print ,@body)
     ,@'(4 5 )
     (prn "after")))

(use-toggled-print "Hello %s %s" "World" "!")

(progn
  (prn "before")
  1 2 3 1 2 3
  (prn "after"))




