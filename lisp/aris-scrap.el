;; -*- lexical-binding: nil; fill-column: 100; lisp-indent-offset: 2; eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'aris-funs--alists)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--lists)
(require 'aris-funs--match-pattern)
(require 'aris-funs--match-pattern2)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--stacks)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq foo 7)
(setq ty 'integer)
(maybe 'integer foo)  
;; (maybe integer foo) 
(maybe ty foo)        

(if-let ((x (when (integerp foo) foo)))
  (prn "yes: %S" x)
  (prn "no!"))

(if-let ((x (maybe 'integer foo)))
  (prn "yes: %S" x)
  (prn "no!"))

;; (if-let ((x (maybe integer foo)))
;;   (prn "yes: %S" x)
;;   (prn "no!"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a function with type checks using the macro:
(defun* foo ((num : number) (exp : integer) &optional print-message)
  "A silly function to raise the number NUM to the integral power EXP.

This is marked as interactive for no good reason other than to test if
INTERACTIVE-FORM is handled properly when defun* builds NEW-BODY and is
marked pure mainly to test if DECLARE-FORM is handled properly."
  (declare (pure t))
  (interactive)
  (let ((res (expt num exp)))
    (when print-message (message "%s to the power of %d is %s." num exp res))
    res))

;; try it out;
(foo 2.5 3 t) ;; ⇒ 15.625 and also prints "2.5 to the power of 3 is 15.625.".
(foo 2.5 3.5 t) ;; signals (wrong-type-argument integer 3.5 pow).
(if-let ((res (maybe integer (foo 4 3 t))))
  (message "Result %S is an integer." res)
  (message "Result was not an integer.")) ;; prints "Result 64 is an integer."

(defun* pow ((num : number) (exp : integer))
  (expt num exp))

(dolist (num '(3 3.5 "foo"))
  (if-let ((res (maybe integer (pow num 3))))
    (message "%d^3 is the integer %d." num res)
    (message "%s^3 is not an integer." num)))

;; prints:
;; 3^3 is the integer 27.
;; 3.5^3 is not an integer.
;; and then signals (wrong-type-argument number "foo" num).

;; imaginary &rest syntax:
(defun* pow ((num : number) (exp : integer) (nums : &rest integer))
  (expt num exp))
(defun* pow ((num : number) (exp : integer) (&rest nums : integer))
  (expt num exp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming (newer version without dostack)."
  (let* ( (args            (eval `(--pipe-make-args ,head ,@tail)))
          (return-label `',(gensym "return-"))
          (var             (alist-get 'var  args))
          (body         `',(alist-get 'body args)))
    (prn "args is %S" args)
    `(let ( (,var      nil)
            (var-sym ',var)
            (body     ,body)
            (flag      nil))
       (cl-labels ( (pop! ()
                      (unless (length> ,body 0) (signal 'stack-underflow (list 'body)))
                      (pop body))
                    (drop-next! () 
                      (let ((next (pop!)))
                        (--pipe-print "Popped 1st %S from %S." next body)
                        (when (memq next *--pipe--arity-1-commands*)
                          (let ((popped (pop!)))
                            (--pipe-print "Popped command's argument %S from %S."
                              popped body)))
                        (when (memq next *--pipe--arity-2-commands*)
                          (error "Ignoring the %S command is not yet supported." next))))
                    (store! (value)
                      (prog1
                        (setq ,var value)
                        (--pipe-print "Updated %S to %S." var-sym ,var)))
                    (flag-is? (test-flag)
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
                    (unset-flag! ()
                      (when flag
                        (--pipe-print "Unsetting flag %S." flag)
                        (set-flag! nil)))
                    (labeled-print (label value)
                      (let* ( (label  (format "%s:" label))
                              (whites (make-string (- 21 (length label)) ?\ ))
                              (label  (concat label whites)))
                        (--pipe-print "%s%S" label value))))
         (--pipe-prndiv)
         (--pipe-print "START")
         (--pipe-prndiv)
         (catch ,return-label                
           (while body
             (let ((expr (pop!)))
               (--pipe-prndiv)
               (labeled-print "Current" expr)
               (labeled-print "Remaining" body)
               (labeled-print var-sym ,var)
               (labeled-print "Flag" flag)
               (if (--is-pipe-command? expr)
                 (set-flag! (alist-get expr *--pipe-commands-to-flags*))
                 (let ((result
                         (eval (if (fun? expr)
                                 `(,expr ,var-sym)
                                 (let ((return-label ,return-label))
                                   `(cl-flet ((return! (value)
                                                (--pipe-print "Throwing %S." ',return-label)
                                                (throw ',return-label value)))
                                      ,expr))))))
                   (labeled-print "Expr result" result)
                   (cond
                     ((flag-is? :IGNORE)
                       (--pipe-print "Not setting %S because %S." result flag))
                     ((flag-is? :WHEN)
                       (when (not result) (drop-next!)))
                     ((flag-is? :UNLESS)
                       (when result (drop-next!)))
                     ((flag-is? :RETURN)
                       (--pipe-print "Returning due to command: %S" result)
                       (throw ,return-label result))
                     ((and (flag-is? :MAYBE) result)
                       (store! result))
                     ((and (flag-is? :MAYBE) (not result))
                       (--pipe-print "Ignoring %S." result))
                     (t (store! result)))
                   (unset-flag!)))))
           ;; For clarity, explicitly throw the return value if we run out of stack items:
           (throw ,return-label
             (progn
               (--pipe-prndiv)
               (--pipe-print "Returning this because stack is empty: %S" ,var)
               (--pipe-prndiv)
               ,var))
           ) ;; END OF CATCH.
         ) ;; END OF CL-LABELS.
       ) ;; END OF LET (AND OF EXPANDED MACRO CONTENT).
    ) ;; END OF LET*. 
  ) ;; END OF DEFMACRO.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *pipe--verbose* t)
(|> ((z)) 5 (* z z) (+ z 8) double) ;; => 66
(|> ((z 5)) (* z z) (+ z 8) double) ;; => 66
(|> 5 (* _ _) (+ _ 8) double) ;; => 66

(|> ((z 5)) (* z z) :return 9 (+ z 8) double) ;; => 9
(|> ((z 5)) (* z z) (return! 9) (+ z 8) double) ;; => 9


