;; -*- lexical-binding: t; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--lust-style-syntax)
(require 'aris-funs--error-when-and-error-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
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


  (message "%s" (fib 0))
  (message "%s" (fib 1))
  (message "%s" (fib 2))
  (message "%s" (fib 3))
  (message "%s" (fib 4))
  (message "%s" (fib 5))
  (message "%s" (fib 6))
  (message "%s" (fib 7))
  (message "%s" (fib 8))
  (message "%s" (fib 9))
  (message "%s" (fib 10))

  ;; (def result
  ;;   (list
  ;;     (fib (car z))
  ;;     (fib (cadr z))
  ;;     (fib 10)))
  ) ;; (144 233 55)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



*lust-style-syntax--pattern-dispatch-table*
;;(fib 10)
(symbol-plist 'fib)
(symbol-plist 'double)

(lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun 'foo)

(symbol-plist 'foo)

*lust-style-syntax--pattern-dispatch-table*

(progn
  (pd-reset)
  (def (boop x y) (* x y))
  ;; (message "%s" (funcall (lust-style-syntax--make-pattern-dispatcher-fun 'boop) 8 9))
  )
