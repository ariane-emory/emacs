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


;; (insert
;;   (pp (macroexpand-all
;;         '(with-messages-2 "doing stuff" "the stuff"
;;            (with-message-2 "doing things" "the things"
;;              (indented-message-2 "boom"))))))

;; (insert
;;   (pp (macroexpand-all
;;         '(with-messages-2 "screwing around"
;;            (with-message-2 "doing things" 
;;              (with-messages-2 "doing stuff" "doing the stuff"
;;                (indented-message-2 "boom")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indented-message-2 (fmt &rest rest)
  (let ((indent-str (make-string (* 2 *with-messages-indent*) ?\ )))
    (apply 'message
      (concat indent-str fmt) rest)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-message-2 (&rest args)
  "Print `message-string' before evaluating `body', returning the result of the
last expression in `body'."
  (declare (indent 1) (debug t))
  `(with-messages :just ,@args))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-messages-2 (&rest args)
  "Print `message-string' at the before evaluating `body' and a variant
afterwards, returning the result of the last expression in `body'."
  (declare (indent 1) (debug t))
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
          (end-message-string
            (cond 
              (is-double-message
                (concat
                  "Done "
                  (downcase second-message-string-head)
                  second-message-string-tail
                  "."))
              ((not 1st-is-just-kw)
                (concat
                  "Done "
                  (downcase message-string-head)
                  message-string-tail
                  "."))))
          (end-message-expr
            (unless 1st-is-just-kw
              (list `(message "%s%s" indent-str ,end-message-string))))
          (body
            (cond
              (is-double-message (cddr args))
              (1st-is-just-kw (cddr args))
              (t (cdr args)))))
    `(let ( (indent-str (make-string (* 2 *with-messages-indent*) ?\ ))
            (*with-messages-indent* (1+ *with-messages-indent*)))
       (unwind-protect
         (cl-flet ((foo () 88))
           (message "%s%s" indent-str ,start-message-string)
           ,@body)
         ,@end-message-expr))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(insert
  (pp (macroexpand-all
        '(with-messages-2 "doing things" "doing the things"
           (with-messages-2 "doing stuff" "doing the stuff"
             (indented-message-2 "boom")
             (indented-message-2 "bang"))))))


