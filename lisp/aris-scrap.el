;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--lust-style-syntax)
(require 'aris-funs--when-and-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin
  ;; Test defining a function:
  ;; Reset the pattern dispatch table first so that we won't get
  ;; 'Pattern blah already defined' errors if we evaluate this
  ;; buffer twice:
  (setq *lust-style-syntax--pattern-dispatch-table* nil)
  (setq *lust-style-syntax--verbose* t)

  (def p 6)
  (def w 12)
  (def p 18)
  (def x (1+ w))
  (def y '(w x))
  (def z (list w x))
  (def (fib 0) 0)
  (def (fib 1) 1)
  (def (fib n) (+ (fib (- n 1)) (fib (- n 2)))) 
  
  (def result
    (list
      (fib (car z))
      (fib (cadr z))
      (fib 10)))
  ) ;; (144 233 55)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def qqq 444)
(def qqq '(333 444))

(def (double n) (+ n n))


(error-when nil "This should not raise an error: %s" it)
(error-when t "This should raise an error: %s" it)
(error-unless t "This should not raise an error: %s")
(error-unless nil "This should raise an error: %s")


;; (font-lock-add-keywords nil
;; 	'( ("(\\(error-when\\_>\\)" . 1))
;; 	'prepend)
;; (font-lock-add-keywords nil
;;   '(("error-when" . font-lock-warning-face)) 'prepend)
