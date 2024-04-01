;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
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


;; (error-unless "This should not raise an error" t)
;; (error-unless "This should raise an error." nil)
;; (error-when "This should raise an error: %s" '(it) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-is-format-string-p (str)
  "Check if STR contains format specifiers."
  (string-match-p "%[a-zA-Z]" str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(error-when "This should raise an error because condition is non-nil." "this string is true")

(defmacro error-when (error-message &rest format-args-and-body)
  "Assert that the last expression in FORMAT-ARGS-AND-BODY is nil. If it is not,
signal an error with ERROR-MESSAGE and FORMAT-ARGS-AND-BODY."
  (unless format-args-and-body
    (error "error-when: No body provided."))
  (if (not (string-is-format-string-p error-message))
    (let ((body `(progn ,@format-args-and-body)))
      `(when ,body (error ,error-message)))
    (let ( (body `(progn ,@(cdr format-args-and-body)))
           (format-args `(list ,@(cadar format-args-and-body))))
      `(let ((it ,body))
         (when it (apply #'error ,error-message ,format-args))))))

(defmacro error-unless (error-message &rest format-args-and-body)
  "Assert that the last expression in FORMAT-ARGS-AND-BODY is not nil. If it is not,
signal an error with ERROR-MESSAGE and FORMAT-ARGS-AND-BODY."
  (let ( (body (if (not (string-is-format-string-p error-message))
                 format-args-and-body
                 (cdr format-args-and-body)))
         (format-args (if (string-is-format-string-p error-message)
                        `(list ,@(cadar format-args-and-body))
                        nil)))
    `(error-when ,error-message (not ,body) ,format-args)))



(error-when "This should raise an error because condition is non-nil: %s %s %s" '(it 8 (+ 2 3)) 1 nil "THIS STRING IS TRUE")
(error-when "This should NOT raise an error because condition is nil: %s %s %s" '(it 8 (+ 2 3)) 1 nil nil)
(error-when "This should raise an error because condition is non-nil." 1 nil "THIS STRING IS TRUE")
(error-when "This should NOT raise an error because condition is nil." 1 nil nil)









