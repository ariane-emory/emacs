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
    (print "BINDING DISPATCH FUN FOR %s!" symbol)
    (error-unless "%s is not a symbol." '(symbol)
      (symbolp symbol))
    
    (let ((already-bound (fboundp symbol)))
      (when (and
              already-bound
              (not (let ((got-symbol (get (function symbol) :PATTERN-DISPATCHER-GROUP)))
                   (print "Retrieved %s for %s." got-symbol symbol)
                   got-symbol)))
        (error "Logic error: function %s already bound elsewherm, fmakunbound it first!" symbol))
      (print "%sBINDING DISPATCH FUN FOR %s!"
        (if already-bound "RE" "") symbol))
    
    (fset symbol
      (lust-style-syntax--make-pattern-dispatcher-fun symbol))

    (put symbol :PATTERN-DISPATCHER-GROUP symbol)

    (print "MARKED SYMBOL %s WITH LABEL %s." symbol
      (get
        symbol
        :PATTERN-DISPATCHER-GROUP))))

(lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun 'foo)
