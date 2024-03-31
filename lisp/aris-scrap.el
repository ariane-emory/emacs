;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (emacs-lisp-mode)
  (variable-pitch-mode -1)
  (aris-prettify-symbols-lisp)
  (display-fill-column-indicator-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test defining a function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset the pattern dispatch table first so that we won't get
;; 'Pattern blah already defined' errors if we evaluate this
;; buffer twice:
(progn
  (setq *lust-style-def--pattern-dispatch-table* nil)

  (def w 8)
  (def x (1+ w))
  (def y '(w x))
  (def z (list w x))
  (def (fib 0) 0)
  (def (fib 1) 1)
  (def (fib n) (+ (fib (- n 1)) (fib (- n 2)))) 

  (setq result
    (list
      (fib (car z))
      (fib (cadr z))
      (fib 10)))) ;; => (21 34 55)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratch ends here:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
