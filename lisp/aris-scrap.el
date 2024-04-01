;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--lust-style-syntax)
(require 'aris-funs--when-and-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (def qqq 444)
  (def qqq '(333 444))
  (def (double n) (+ n n))
  
  (def result
    (list
      (fib (car z))
      (fib (cadr z))
      (fib 10)))
  ) ;; (144 233 55)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(error-unless "This should not raise an error" t)
(error-unless "This should raise an error." nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-is-format-string-p (str)
  "Check if STR contains format specifiers."
  (string-match-p "%[a-zA-Z]" str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error-unless (error-message form)
  "Assert that FORM is not nil. If it is, signal an error with ERROR-MESSAGE and
ERROR-FMT-ARGS ."
  `(unless ,form (error ,error-message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error-when (error-message &rest format-args-and-body)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Assert that FORM is nil. If it is not, signal an error with ERROR-MESSAGE and
ERROR-FMT-ARGS in a scope where `it' is bound to the result of FORM."
  (if (string-is-format-string-p error-message)
    `(let ((it (progn ,@(car format-args-and-body))))
       (when it
         (error ,error-message ,@(cdr format-args-and-body))))
    `(let ((it (progn ,@format-args-and-body)))
       (when it
         (error ,error-message)))))


(error-when "This should not raise an error: %s" '(it) nil)
(error-when "This should raise an error: %s" '(it) t)
