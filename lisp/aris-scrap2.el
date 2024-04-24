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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account (name &optional (balance 0.00))
  ((interest-rate .06))
  (withdraw (amt) (if (<= amt balance) (cl-decf balance amt) :INSUFFICIENT-FUNDS))
  (deposit (amt) (cl-incf balance amt))
  (balance () balance)
  (name () name)
  (interest () (cl-infc balance (* balance interest-rate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf acct2 (account "A. User" 2000.00))
(deposit acct2 42.00)
(withdraw acct2 200.00)
(symbol-plist 'acct2)
(symbol-plist 'withdraw)
(is? acct2 'account)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account-with-password (password &delegee (account acct)) ()
  (change-password (pass new-pass)
    (if (equal pass password)
      (setf password new-pass)
      :WRONG-PASSWORD))
  (otherwise (pass &rest args)
    (if (equal pass password)
      (apply message acct args)
      :WRONG-PASSWORD)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf acct3 (account-with-password "secret" acct2))
(withdraw acct3 "guess" 2000.00)
(withdraw acct3 "secret" 200.00)
(is? acct3 'account-with-password) ;; t
(is? acct3 'account) ;; nil
(class-name acct3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account-with-limit (limit &delegee (account acct)) ()
  (withdraw (amt)
    (if ( > amt limit)
      :OVER-LIMIT
      (withdraw acct amt)))
  (otherwise (&rest args)
    (apply message acct args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf acct4
  (account-with-password "pass"
    (account-with-limit 100.00
      (account "A. Thrifty Spender" 500.00))))
(withdraw acct4 "pass" 200.00)
(withdraw acct4 "pass" 20.00)
(withdraw acct4 "guess" 20.00)
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
