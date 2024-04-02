;; -*- lexical-binding: t; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
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

(lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun 'foo)

(symbol-plist 'foo)

*lust-style-syntax--pattern-dispatch-table*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--make-pattern-dispatcher-fun (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Factory function for pattern dispatch handler functions. The reason we construct new ones each time is
because we're gong to be stshing stuff in their symbol properties."
  (aris-lust-syle-defs--use-print
    (print "Making dispatcher for %s..." symbol)
    (let ((symbol symbol))
      (lambda (&rest args)
        (print "Doing dispatch for %s..." symbol)
        (let* ( (group-symbol (get symbol :PATTERN-DISPATCHER-GROUP))
                (group (lust-style-syntax--get-patterns-for-group group-symbol))
                (call-pattern (cons symbol args)))
          ;;       (aris-lust-syle-defs--use-print
          ;;         (print "Looking for group %s..." symbol)
          ;;         (lust-style-syntax--eval-match-result
          ;;           (aris-lust-syle-defs--match-call-pattern-in-group call-pattern group)))))
          )))))

(progn
(defun lust-style-syntax--make-pattern-dispatcher-fun-2 (symbol)
  (aris-lust-syle-defs--use-print
    (lambda (&rest args)
      (let ((symbol symbol))
        (message "Doing dispatch for %s..." (symbol-name symbol))))))

  (funcall (lust-style-syntax--make-pattern-dispatcher-fun-2 'boop) 8))

