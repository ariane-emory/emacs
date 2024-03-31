;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *with-messages-indent* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-message (&rest args)
  "Print `message-string' before evaluating `body', returning the result of the
last expression in `body'."
  (declare (indent 1) (debug t))
  `(with-messages :just ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-messages (&rest args)
  "Print `message-string' at the before evaluating `body' and a variant
afterwards, returning the result of the last expression in `body'."
  (declare (indent 1) (debug t))
  (let* ((1st-is-just-kw (eql :just (car args)))
          (is-double-message (and (not 1st-is-just-kw) (stringp (cadr args))))
          (message-string (if 1st-is-just-kw (cadr args) (car args)))
          (second-message-string (when is-double-message (cadr args)))
          (body (if 1st-is-just-kw (cddr args) (cdr args)))
          (indent-str (make-string (* 2 *with-messages-indent*) ?\ ))
          (message-string (if (stringp message-string) message-string (eval message-string)))
          (message-string-head (substring message-string 0 1))
          (message-string-tail (substring message-string 1))
          (second-message-string second-message-string)
          (second-message-string-head (substring second-message-string 0 1))
          (second-message-string-tail (substring second-message-string 1))
          (start-message-string
            (concat
              indent-str
              (upcase message-string-head)
              message-string-tail
              (if 1st-is-just-kw "." "...")))
          (start-message-expr (list `(message ,start-message-string)))
          (end-message-string
            (cond 
              ((not 1st-is-just-kw)
                (concat
                  indent-str
                  "Done "
                  (downcase message-string-head)
                  message-string-tail
                  "."))
              (is-double-message
                (concat
                  indent-str
                  (upcase second-message-string-head)
                  second-message-string-tail
                  "."))))
          (end-message-expr  (unless 1st-is-just-kw (list `(message "%s" ,end-message-string)))))
    `(let ((*with-messages-indent* (1+ *with-messages-indent*)))
       ,@start-message-expr
       (let ((result (progn ,@body)))
         ,@end-message-expr
         result))))
(with-messages "doing stuff" "that" (message "bang"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro use-package-with-messages (&rest args)
  `(with-messages (format "using %s" ',(car args))
     (use-package ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro use-package-with-message (&rest args)
  `(with-message (format "using %s" ',(car args))
     (use-package ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indented-message (fmt &rest rest)
  (let ((indent-str (make-string (* 2 *with-messages-indent*) ?\ )))
    (apply 'message (concat indent-str fmt) rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--with-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
