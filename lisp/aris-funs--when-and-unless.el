;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro error-when (form error-message &rest error-fmt-args)
  "Assert that FORM is nil. If it is not, signal an error with ERROR-MESSAGE and
ERROR-FMT-ARGS in a scope where `it' is bound to the result of FORM."
  `(let ((it ,form))
     (unless (not it)
       (error ,error-message ,@error-fmt-args))))

(defmacro error-unless (form error-message &rest error-fmt-args)
  "Assert that FORM is not nil. If it is, signal an error with ERROR-MESSAGE and
ERROR-FMT-ARGS ."
  `(unless ,form
     (error ,error-message ,@error-fmt-args)))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("error-when" . font-lock-warning-face)))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("error-unless" . font-lock-warning-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--when-and-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
