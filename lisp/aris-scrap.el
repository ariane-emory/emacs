;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--lust-style-syntax)
(require 'aris-funs--error-when-and-error-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin
  ;; Test defining a function:
  ;; Reset the pattern dispatch table first so that we won't get
  ;; 'Pattern blah already defined' errors if we evaluate this
  ;; buffer twice:
  (setq *lust-style-syntax--pattern-dispatch-table* nil)
  ;;(setq *lust-style-syntax--verbose* nil)

  (def p 6)
  (def w 12)
  (def p 18)
  (def x (1+ w))
  (def y '(w x))
  (def z (list w x))
  (def (fib 0) 0)
  (def (fib 1) 1)
  (def (fib n) (+ (fib (- n 1)) (fib (- n 2)))) 
  (def qqq 444)
  (def qqq '(333 444))
  (def (double n) (+ n n))


  (fib 0)
  (fib 1)
  (fib 2)
  (fib 3)
  (fib 4)
  (fib 5) 
  (fib 6)
  (fib 7)
  (fib 8)
  (fib 9)
  (fib 10)
  (fib 10)

  (def result
    (list
      (fib (car z))
      (fib (cadr z))
      (fib 10)))
  ) ;; (144 233 55)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


*lust-style-syntax--pattern-dispatch-table*
(fib 10)
(symbol-plist 'fib)
(symbol-plist 'double)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "An internal helper function to bind the pattern dispatcher function to symbols that's used by def."
  (aris-lust-syle-defs--use-print
    (print (make-string 80 ?\=))
    (print "Binding dispatch fun for '%s!" symbol)

    ;; SYMBOL must be a symbol:
    (error-unless "%s is not a symbol." '(symbol)
      (symbolp symbol))
    
    ;; If SYMBOL is already bound and it doesn't look like we did it,
    ;; raise an error.
    (let ((already-bound (fboundp symbol)))
      (when (and
              already-bound
              (not (let ((existing-group-label (get symbol :PATTERN-DISPATCHER-GROUP)))
                   (print "'%s already has group label '%s."
                     symbol existing-group-label)
                   (eq existing-group-label symbol))))
        (error
          (concat
            "Logic error: symbol '%s already bound to a function and "
            "it doesn't look like it was bound by this function."
            "fmakunbound it first if you really want to re-bind it!" symbol)))
      (print "%sinding dispatch fun for '%s!"
        (if already-bound "Reb" "B") symbol))

    ;; Attach our handler function to SYMBOL's function cell:
    (fset symbol (lust-style-syntax--make-pattern-dispatcher-fun symbol))

    ;; Stash the group label in a property on SYMBOL:
    (put symbol :PATTERN-DISPATCHER-GROUP symbol)

    ;; Make sure the label was set properly and then return SYMBOL's plist:
    (let ( (group-label (get symbol :PATTERN-DISPATCHER-GROUP))
           (plist (symbol-plist symbol)))
      ;; Sanity check:
      (error-unless
        (format (concat
                  "After setting field to %s, its value is %s. "
                  "Something has gone wrong.")
          symbol group-label)
        (eq symbol group-label))
      (print "Marked symbol %s with group label %s, its plist is now: %s."
        symbol group-label plist)
      ;; Finally, return SYMBOL's modified plist:
      plist)))

(lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun 'foo)

(symbol-plist 'foo)

