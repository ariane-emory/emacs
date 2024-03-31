;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when t
  (require 'aris-funs--lust-style-syntax)
  (begin
    ;; Test defining a function:
    ;; Reset the pattern dispatch table first so that we won't get
    ;; 'Pattern blah already defined' errors if we evaluate this
    ;; buffer twice:
    (setq *lust-style-syntax--pattern-dispatch-table* nil)

    (def p 4)
    (def w 5)
    (def p 6)
    (def x (1+ w))
    (def y '(w x))
    (def z (list w x))
    (def (fib 0) 0)
    (def (fib 1) 1)
    (def (fib 8) w)
    (def (fib n) (+ (fib (- n 1)) (fib (- n 2)))) 

    w
    
    (def result
      (list
        (fib (car z))
        (fib (cadr z))
        (fib 10)))
    )
  ) ;; => (169 273 39)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *with-messages-indent-char* ?\  "The character used for indentation in `with-messages'.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indented-message (fmt &rest rest)
  (let ((indent-str (make-string (* 2 *with-messages-indent*) *with-messages-indent-char*)))
    (apply 'message (concat indent-str fmt) rest)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-message (&rest args)
  "Print `message-string' before evaluating `body', returning the result of the
last expression in `body'."
  `(with-messages :just ,@args))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-messages (&rest args)
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
            (unless 1st-is-just-kw
              (list `(apply #'message "%sDone %s%s." indent-str ',end-message-fmt-args))))
          (body
            (cond
              (is-double-message (cddr args))
              (1st-is-just-kw (cddr args))
              (t (cdr args)))))
    `(let ( (indent-str
              (make-string (* 2 *with-messages-indent*) *with-messages-indent-char*))
            (*with-messages-indent* (1+ *with-messages-indent*)))
       (unwind-protect
         (progn
           (message "%s%s" indent-str ,start-message-string)
           ,@body)
         ,@end-message-expr))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(insert
  (pp (macroexpand-all
        '(with-messages "doing things" "doing the things"
           (with-messages "doing stuff" "doing the stuff"
             (indented-message "boom")
             (indented-message "bang"))))))
(let
  ((indent-str
     (make-string
       (* 2 *with-messages-indent*)
       *with-messages-indent-char*))
    (*with-messages-indent*
      (1+ *with-messages-indent*)))
  (unwind-protect
    (progn
      (message "%s%s" indent-str "Doing things...")
      (let
        ((indent-str
           (make-string
             (* 2 *with-messages-indent*)
             *with-messages-indent-char*))
          (*with-messages-indent*
            (1+ *with-messages-indent*)))
        (unwind-protect
          (progn
            (message "%s%s" indent-str "Doing stuff...")
            (indented-message "boom")
            (indented-message "bang"))
          (apply #'message "%sDone %s%s." indent-str
            '("d" "oing the stuff")))))
    (apply #'message "%sDone %s%s." indent-str
      '("d" "oing the things"))))










