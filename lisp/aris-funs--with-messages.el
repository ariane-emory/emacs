;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup with-messages nil "The `with-messages' macro and its relatives.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *wm--indent-char* ?\  ;; DO NOT NEGLECT THE SPACE!
  "The character used for indentation in `with-messages'."
  :group 'with-messages
  :type 'character)

(defcustom *wm--indent-size* 2
  "The number of `*wm--indent-char*' characters used for each level of indentation in `with-messages'."
  :group 'with-messages
  :type 'integer)

(defcustom *wm--depth-indicator-enable* nil
  "Whether to display the current indentation level in `with-messages'."
  :group 'with-messages
  :type 'boolean)

(defcustom *wm--depth-indicator-format* "[%2d] "
  "The format string used to display the current indentation level in `with-messages'."
  :group 'with-messages
  :type 'string)

(defcustom *wm--divider-width* 60
  "The width of the divider line in `prndiv'."
  :group 'with-messages
  :type 'integer)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *wm--indent* 0 "The current indentation level in `with-messages'.
This variable is not meant to be customized but can be safely dynamically shadowed." )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-indentation (&rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate body with one more level of indentation."
  `(let ((*wm--indent* (1+ *wm--indent*))) ,@body))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-messages (&rest args)
  "Print `message-string' before evaluating `body', returning the result of the
last expression in `body' and printing a variant message afterwards."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
            (format "%s%s%s"
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
              (list `(apply #'message "%sDone %s%s."
                       indent-string ',end-message-fmt-args))))
          (body
            (cond
              (is-double-message (cddr args))
              (1st-is-just-kw (cddr args))
              (t (cdr args)))))
    `(let ( (indent-string (indent-string))
            (*wm--indent* (1+ *wm--indent*)))
       (unwind-protect
         (progn
           (message "%s%s" indent-string ,start-message-string)
           ,@body)
         ,@end-message-expr))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-message (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Print `message-string' before evaluating `body', returning the result of the
last expression in `body'."
  `(with-messages :just ,@args))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indented-message (fmt &rest rest)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Print a message with the current indentation level."
  (apply 'message (format "%s%s" (indent-string) fmt) rest)
  nil)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-string ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a string of *WM--INDENT-CHAR*s corresponding to the current indentation level."
  (let ((indent-string
          (make-string
            (* *wm--indent* *wm--indent-size*)
            *wm--indent-char*))
         (depth-indicator
           (if *wm--depth-indicator-enable*
             (format  *wm--depth-indicator-format*
               *wm--indent*) "")))
    (format "%s%s" depth-indicator indent-string)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro use-package-with-messages (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(with-messages (format "using %s" ',(car args))
     (use-package ,@args)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro use-package-with-message (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(with-message (format "using %s" ',(car args))
     (use-package ,@args)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'prn 'indented-message)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun prndiv (&optional (char ?\=) (count *wm--divider-width*))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Print a divider line with the current indentation level."
  (indented-message "%s" (make-string count char)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--with-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
