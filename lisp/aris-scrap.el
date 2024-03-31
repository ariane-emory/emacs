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


(insert
  (pp (macroexpand-all
        '(with-messages-2 "doing things" "doing the things"
           (with-messages-2 "doing stuff" "doing the stuff"
             (indented-message-2 "boom")
             (indented-message-2 "bang"))))))

(let ( (indent-str (make-string (* 2 *with-messages-indent*) 32))
       (*with-messages-indent* (1+ *with-messages-indent*)))
  (let* ((--cl-indented-message-2--
           #'(lambda (fmt &rest rest)
               "\n\n(fn FMT &rest REST)"
               (let ((indent-str (make-string (* 2 *with-messages-indent*) 32)))
                 (apply 'message (concat indent-str fmt) rest)))))
    (progn
      (unwind-protect
        (progn
          (message "%s%s" indent-str "Doing things...")
          (let ( (indent-str (make-string (* 2 *with-messages-indent*) 32))
                 (*with-messages-indent* (1+ *with-messages-indent*)))
            (let* ((--cl-indented-message-2--
                     #'(lambda (fmt &rest rest)
                         "\n\n(fn FMT &rest REST)"
                         (let ((indent-str (make-string (* 2 *with-messages-indent*) 32)))
                           (apply 'message (concat indent-str fmt) rest)))))
              (progn
                (unwind-protect
                  (progn
                    (message "%s%s" indent-str "Doing stuff...")
                    (funcall --cl-indented-message-2-- "boom")
                    (funcall --cl-indented-message-2-- "bang"))
                  (message "%s%s" indent-str "Done doing the stuff."))))))
        (message "%s%s" indent-str "Done doing the things.")))))





