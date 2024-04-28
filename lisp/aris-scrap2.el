;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (thing '(1 2 ,3 4 ,@5 #'6 '7 `8))
  (cond
    ((eq 'quote (car-safe thing)) (prn "This one is kind of special: %s" (cadr thing)))
    ((eq '\, (car-safe thing)) (prn "This one is special: %s" (cadr thing)))
    ((eq '\,@ (car-safe thing)) (prn "This one is very special: %s" (cadr thing)))
    ((eq 'function (car-safe thing)) (prn "This one is super special: %s" (cadr thing)))
    ((eq '\` (car-safe thing)) (prn "This one is extra special: %s" (cadr thing)))
    (t (prn "%s" thing))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manually tweaked expansion with a private method:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let
;;   ((interest-rate 0.06))
;;   (a:ensure-generic-fn 'is?)
;;   (mapcar #'a:ensure-generic-fn '(withdraw deposit balance name privcall interest))
;;   (cl-defun account (name &optional (balance 0.0))
;;     #'(lambda (message)
;;         (cl-flet ((private-method () name))
;;           (cl-case message
;;             (is? #'(lambda (class) (eq class 'account)))
;;             (withdraw #'(lambda (amt)
;;                           (if (<= amt balance)
;;                             (cl-decf balance amt)
;;                             :INSUFFICIENT-FUNDS)))
;;             (deposit #'(lambda (amt) (cl-incf balance amt)))
;;             (balance #'(lambda nil balance))
;;             (name #'(lambda nil name))
;;             (privcall #'(lambda nil (private-method)))
;;             (interest #'(lambda nil (cl-infc balance (* balance interest-rate)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun foo (bar &rest baz)
  baz)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count-args (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Count the minimum and maximum arguments described by ARGLIST and
return a pair of the form (MIN-ARGS . MAX-ARGS).

This only handles those lambda list keywords native to Emacs Lisp,
i.e. &optional and &rest."
  (let ( (min-args 0)
         (max-args 0)
         (seen-optional nil)
         (rest-arg nil))
    (while-let ((popped (pop arglist)))
      (cond
        ((eq popped '&optional)
          (setq seen-optional t))
        ((eq popped '&rest)
          (setq rest-arg t))
        (seen-optional
          (cl-incf max-args))
        (rest-arg) ;; don't bother counting after &rest.
        (t
          (cl-incf min-args)
          (cl-incf max-args))))
    (cons min-args (if rest-arg nil max-args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (count-args '(foo &optional bar &rest baz)) returns (1))
(confirm that (count-args '(foo &optional bar baz)) returns (1 . 3))
(confirm that (count-args '(foo bar &optional baz)) returns (2 . 3))
(confirm that (count-args '(foo bar &optional baz &rest quux)) returns (2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
