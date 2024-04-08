;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(progn
  (setq myfun (lambda () xxx))
  (let ((xxx 5))
    (funcall myfun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(|> ((x 2)) (+ 3 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(|> ((e)) 5 double (lambda (n) (+ e 2)) (* e 2))

(let ((final
        (let ( (body '(5 double (lambda (n) (+ e 2)) (return e) (* e 2)))
               (e nil)
               (var-sym 'e)
               (flag nil))
          (cl-labels
            ((flag-is? (test-flag)
               (eq flag (--valid-pipe-flag-or-nil test-flag)))
              (set-flag! (new-flag force)
                (let ((new-flag (--valid-pipe-flag-or-nil new-flag t)))
                  (cond
                    ((and flag new-flag (not force))
                      (error "Cannot set flag to %S when flag is already set to %S." new-flag flag))
                    (force
                      (--pipe--print "FORCING FLAG FROM %S TO %S." flag new-flag)
                      (setq flag new-flag))
                    (t
                      (--pipe--print "Setting flag from %S to %S%s." flag new-flag
                        (if force " (forced)" ""))))
                  (setq flag new-flag)))
              (store! (value)
                (setq e value))
              (unset-flag! nil
                (set-flag! nil nil)))
            (--pipe--print (make-string 80 61))
            (--pipe--print "START")
            (--pipe--print (make-string 80 61))
            (catch 'return
              (dostack (expr body)
                (--pipe--print (make-string 80 61))
                (--pipe--print "Current:             %S" expr)
                (--pipe--print "Remaining:           %S" stack)
                (--pipe--print "Var:                 %S" e)
                (--pipe--print "Flag:                %S" flag)
                (if (--is-pipe-command? expr)
                  (set-flag! (alist-get expr *--pipe-commands-to-flags*) nil)
                  (cl-flet ((ignore-next-and-unset-flag! (bool)
                              (if bool
                                (let ((next (pop!)))
                                  (--pipe--print "Popped 1st %S from %S." next body)
                                  (when (memq next *--pipe--arity-2-commands*)
                                    (error "Ignoring the %S command is not yet supported." next))
                                  (when
                                    (memq next *--pipe--arity-1-commands*)
                                    (--pipe--print "Popped 1st %S from %S."
                                      (pop!) body))
                                  (unset-flag!))
                                (--pipe--print "Next command will be processed."))
                              (unset-flag!))
                             ;; ((return (value) (throw 'return value)))
                             ;; (expr-fun ;; HERE=============================================
                             ;;   `(lambda (expr ,var-sym)
                             ;;      (cl-flet ((return (value) (throw 'return value)))
                             ;;        (--pipe--print "Evaluating expr:     %S." expr)
                             ;;        ,expr)))
                             )
                    (let ((result
                            (if (fun? expr)
                              (eval (list expr 'e))
                              (eval
                                `(cl-flet ((return (value) (throw 'return value)))
                                   ,expr ))
                              ))) ;; HERE=================================
                      (--pipe--print "Expr result:         %S" result)
                      (cond
                        ((flag-is? :RETURN)
                          (--pipe--print "Returning due to command: %S" result)
                          (throw 'return result))
                        ((flag-is? :UNLESS)
                          (ignore-next-and-unset-flag! result))
                        ((flag-is? :WHEN)
                          (ignore-next-and-unset-flag! (not result)))
                        ((and (flag-is? :MAYBE) result)
                          (--pipe--print "Updating var to %S and unsetting the %S flag." e flag)
                          (store! result)
                          (unset-flag!))
                        ((and (flag-is? :MAYBE) (not result))
                          (--pipe--print "Ignoring %S and unsetting the %S flag." result flag)
                          (unset-flag!))
                        ((flag-is? :IGNORE)
                          (--pipe--print "Not setting %S because %S and unsetting the flag." result flag)
                          (unset-flag!))
                        (t
                          (store! result)
                          (--pipe--print "Updating var to %S." e)))))))
              (throw 'return
                (progn
                  (--pipe--print (make-string 80 61))
                  (--pipe--print "Because empty stack: %S" e)
                  (--pipe--print (make-string 80 61))
                  e)))))))
  (--pipe--print "Pipe's final return: %S" final)
  final)
