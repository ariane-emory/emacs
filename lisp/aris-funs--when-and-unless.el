;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-is-format-string-p (str)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Check if STR contains format specifiers."
  (string-match-p "%[a-zA-Z]" str))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error-when (error-message &rest format-args-and-body)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error-unless (error-message &rest format-args-and-body)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Assert that the last expression in FORMAT-ARGS-AND-BODY is not nil. If it is not,
signal an error with ERROR-MESSAGE and FORMAT-ARGS-AND-BODY."
  (let* ( (string-is-format-string (string-is-format-string-p error-message))
          (body (if string-is-format-string
                  (cdr format-args-and-body)
                  format-args-and-body))
          (format-args (when string-is-format-string (car format-args-and-body))))
    `(error-when ,error-message ,format-args (not (progn ,@body)))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (error-when "This should raise an error because condition is non-nil: %s %s %s"
    '(it 8 (+ 2 3)) 1 nil "THIS STRING IS TRUE")
  (error-when "This should NOT raise an error because condition is nil: %s %s %s"
    '(it 8 (+ 2 3)) 1 nil nil)
  (error-when "This should raise an error because condition is non-nil."
    1 nil "THIS STRING IS TRUE")
  (error-when "This should NOT raise an error because condition is nil."
    1 nil nil)
  (error-unless "This should NOT raise an error because condition is non-nil: %s %s %s"
    '(it 8 (+ 2 3)) 1 nil "THIS STRING IS TRUE")
  (error-unless "This should raise an error because condition is nil: %s %s %s"
    '(it 8 (+ 2 3)) 1 nil nil)
  (error-unless "This should NOT raise an error because condition is non-nil."
    1 nil "THIS STRING IS TRUE")
  (error-unless "This should raise an error because condition is nil."
    1 nil nil))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(font-lock-add-keywords 'emacs-lisp-mode '(("error-when" . font-lock-warning-face)))
(font-lock-add-keywords 'emacs-lisp-mode '(("error-unless" . font-lock-warning-face)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--when-and-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
