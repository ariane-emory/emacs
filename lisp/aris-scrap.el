;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
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

    (def p 13)
    (def w p)
    (def p 20)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
          (indent-str (make-string (* 2 *with-messages-indent*) ?_))
          (message-string (if (stringp message-string) message-string (eval message-string)))
          (message-string-head (substring message-string 0 1))
          (message-string-tail (substring message-string 1))
          (second-message-string-head (when is-double-message(substring second-message-string 0 1)))
          (second-message-string-tail (when is-double-message(substring second-message-string 1)))
          (start-message-string
            (concat
              (format "[%s]" *with-messages-indent*)
              indent-str
              (upcase message-string-head)
              message-string-tail
              (if 1st-is-just-kw "." "...")))
          (start-message-expr (list `(message ,start-message-string)))
          (end-message-string
            (cond 
              (is-double-message
                (concat
                  (format "[%s]" *with-messages-indent*)
                  indent-str
                  "Done "
                  (downcase second-message-string-head)
                  second-message-string-tail
                  "."))
              ((not 1st-is-just-kw)
                (concat
                  (format "[%s]" *with-messages-indent*)
                  indent-str
                  "Done "
                  (downcase message-string-head)
                  message-string-tail
                  "."))))
          (end-message-expr  (unless 1st-is-just-kw (list `(message "%s" ,end-message-string)))))
    `(let ((*with-messages-indent* (1+ *with-messages-indent*)))
       ,@start-message-expr
       (let ((result (progn ,@body)))
         ,@end-message-expr
         result))))

(insert (pp (macroexpand-all
              '(with-messages "doing stuff" "that"
                 (with-messages "doing things" "those"
                   (with-messages "doing whatever" "these" (indented-message "boom")))))))
(let
  ((*with-messages-indent*
     (1+ *with-messages-indent*)))
  (message "[0]Doing stuff...")
  (let
    ((result
       (progn "that"
         (let
           ((*with-messages-indent*
              (1+ *with-messages-indent*)))
           (message "[0]Doing things...")
           (let
             ((result
                (progn "those"
                  (let
                    ((*with-messages-indent*
                       (1+ *with-messages-indent*)))
                    (message "[0]Doing whatever...")
                    (message "Indent is %s" *with-messages-indent*)
                    (let
                      ((result
                         (progn "these"
                           (indented-message "boom"))))
                      (message "%s" "[0]Done these.")
                      result)))))
             (message "%s" "[0]Done those.")
             result)))))
    (message "%s" "[0]Done that.")
    result))



(require 'pp)


