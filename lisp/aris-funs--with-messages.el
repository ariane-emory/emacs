;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup with-messages nil "The `with-messages' macro and its relatives.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *with-messages--indent-char* ?\,  ;; DO NOT NEGLECT THE SPACE!
  "The character used for indentation in `with-messages'."
  :group 'with-messages
  :type 'character)

(defcustom *with-messages--indent-size* 2
  "The number of `*with-messages--indent-char*' characters used for each level of indentation in `with-messages'."
  :group 'with-messages
  :type 'integer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *with-messages--indent* 0 "The current indentation level in `with-messages'.
This variable is not meant to be customized." )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-indentation (&rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate body with one more level of indentation."
  `(let ((*with-messages--indent* (1+ *with-messages--indent*))) ,@body))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-messages (&rest args)
  "Print `message-string' before evaluating `body', returning the result of the
last expression in `body' and printing a variant message afterwards."
  (let* ( (1st-is-just-kw (eql :just (car args)))
          (is-double-message (and (not 1st-is-just-kw) (stringp (cadr args))))
          (message-string (if 1st-is-just-kw (cadr args) (car args)))
          (second-message-string (when is-double-message (cadr args)))
          (message-string
            (if (stringp message-string) message-string (eval message-string)))
          (message-string-head (substring message-string 0 1))
          (message-string-tail (substring message-string 1))
          (second-message-string-head
            (when is-double-message(substring second-message-string 0 1)))
          (second-message-string-tail
            (when is-double-message(substring second-message-string 1)))
          (start-message-string
            (concat
              (upcase message-string-head)
              message-string-tail
              (if 1st-is-just-kw "." "...")))
          (end-message-fmt-args
            (cond 
              (is-double-message
                (list (downcase second-message-string-head) second-message-string-tail))
              ((not 1st-is-just-kw)
                (list (downcase message-string-head) message-string-tail))))
          (end-message-expr
            (when end-message-fmt-args
              (list `(apply #'message "%sDone %s%s." indent-string ',end-message-fmt-args))))
          (body
            (cond
              (is-double-message (cddr args))
              (1st-is-just-kw (cddr args))
              (t (cdr args)))))
    `(let ((indent-string
             (make-string
               (* *with-messages--indent* *with-messages--indent-size*)
               *with-messages--indent-char*))
            (*with-messages--indent* (1+ *with-messages--indent*)))
       (unwind-protect
         (progn
           (message "%s%s" indent-string ,start-message-string)
           ,@body)
         ,@end-message-expr))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-message (&rest args)
  "Print `message-string' before evaluating `body', returning the result of the
last expression in `body'."
  `(with-messages :just ,@args))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indented-message (fmt &rest rest)
  (let ((indent-string
          (format "%d%s" *with-messages--indent*
            (make-string (* *with-messages--indent* *with-messages--indent-size*) *with-messages--indent-char*))))
    (apply 'message (concat indent-string fmt) rest)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro use-package-with-messages (&rest args)
  `(with-messages (format "using %s" ',(car args))
     (use-package ,@args)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro use-package-with-message (&rest args)
  `(with-message (format "using %s" ',(car args))
     (use-package ,@args)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--with-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
