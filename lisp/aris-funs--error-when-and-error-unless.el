;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-is-format-string-p (string)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "True when STRING is a string and contains format specifiers."
  (and (stringp string) (string-match-p "%[a-zA-Z]" string)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro no-name (expr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(progn (unless format-args-and-body
            (error "error-when: No body provided."))
     (let* ( (string-is-format-string
               (string-is-format-string-p error-message))
             (body
               (if string-is-format-string
                 (cdr format-args-and-body)
                 format-args-and-body))
             (format-args
               (when string-is-format-string
                 (let ((unquoted-format-args (cadar format-args-and-body)))
                   `(list ,@unquoted-format-args)))))
       `(let ((it (progn ,@body)))
          (if ,',expr
            (apply #'error ,error-message ,format-args)
            it)))))
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error-unless (error-message &rest format-args-and-body)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Assert that the last expression in FORMAT-ARGS-AND-BODY is not nil. If it is not,
signal an error with ERROR-MESSAGE and FORMAT-ARGS-AND-BODY.

When ERROR-MESSAGE is a format string, the car of FORMAT-ARGS-AND-BODY is
expected to evaluate to a list of format arguments to be passed to `format'
when signaling an error.

NOTE:
Because whether or not ERROR-MESSAGE contains format specifiers is judged at
expansion time in order to decide how to slice up FORMAT-ARGS-AND-BODY, if
format specifiers are used, then ERROR-MESSAGE must be a string at expansion
time so that `error-when' can up slice  FORMAT-ARGS-AND-BODY. If you violate
this, odds are you'll get a \"Not enough arguments for format string\" error.

Acceptable cases:
  Error message is not a format string;
  (error-when \"Raise an error because the condition is true.\" 1 nil t)

  Error message is a format string and formet arguments are passed;
  (error-when \"Raise an %s, %s,  %s\" '(\"error because the condition\" it \"is true.\")
    2 (progn nil) (not nil))

  Error message is not string, but evaluates to a string:
  (error-when
    (concat \"Raise an \" \"error because \" \"the condition \" \"is true.\")
    (progn 5) (progn 6) 'true)

  Error message is not string, but evaluates to a string and refernces IT:
  (error-when
    (concat \"Raise an \" \"error because \" \"the condition, \" (format \"%s\" it) \", is true.\")
    (progn 3) 4 \"true\")

  Same thing as the last example, basically:
  (error-when
    (format \"Raise an %s because the condition, %s, is true.\" \"error\" it)
    \"false\")

Unacceptable case:
  (error-when
    (concat \"Raise an %s because \" \"the %s is true.\") '(\"error\" \"condition\")
    (not nil))
"
  (no-name (not it)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (error-unless "This should NOT raise an error because condition is non-nil: %s %s %s" '(it 8 (+ 2 3)) 1 nil "THIS STRING IS TRUE")
  (error-unless "This should raise an error because condition is nil: %s %s %s" '(it 8 (+ 2 3)) 1 nil nil)
  (error-unless "This should NOT raise an error because condition is non-nil." 1 nil "THIS STRING IS TRUE")
  (error-unless "This should raise an error because condition is nil." 1 nil nil)
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error-when (error-message &rest format-args-and-body)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Signal an error unless the last expression in FORMAT-ARGS-AND-BODY evaluates
to nil. Ifit is not, signal an error with ERROR-MESSAGE and
FORMAT-ARGS-AND-BODY.

See `error-unless' for caveats about the use of format speciiers."
  (no-name it))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (error-when "This should raise an error because condition is non-nil: %s %s %s" '(it 8 (+ 2 3)) 1 nil "THIS STRING IS TRUE")
  (error-when "This should NOT raise an error because condition is nil: %s %s %s" '(it 8 (+ 2 3)) 1 nil nil)
  (error-when "This should raise an error because condition is non-nil." 1 nil "THIS STRING IS TRUE")
  (error-when "This should NOT raise an error because condition is nil." 1 nil nil)
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\(error-when\\_>\\)" . font-lock-warning-face)))
(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\(error-unless\\_>\\)" . font-lock-warning-face)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--error-when-and-error-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
