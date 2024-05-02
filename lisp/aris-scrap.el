;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(aos:defclass barclass (num &parent (parent fooclass fakeclass)) ()
  (bar () (format "BAR! %d" num)))

(aos:defclass bazclass (num &parent (parent fooclass barclass) &rest things) ()
  (baz () (format "BAZ! %d" num)))

(setq foo  (make-fooclass 8))
(setq bar  (make-barclass 4 foo))
(setq baz  (make-bazclass 7 foo))
(setq baz2 (make-bazclass 6 bar))
;; (setq bad-bar (barclass 9 baz)) ;; this shouldn't work.

(foo (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "FOO! 2"
(bar (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "BAR! 3"
(baz (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "BAZ! 5"

(class-names foo)
(class-names bar)
(class-names baz) ;; (bazclass fooclass) WHY ISN'T this (bazclass barclass fooclass)?

(field-names basic-acct)
(field-names passwd-acct)

(setq basic-acct (make-basic-account "A. User" 2000.00))
(field-values basic-acct) ;; => ("A. User" 2000.0)

(aos:is-object? (alist-get 'acct (field-values passwd-acct)))

(repr basic-acct)
(repr passwd-acct)
(repr limit-acct)

(prepr limit-acct)
(strepr limit-acct)

(field-values basic-acct)
(field-values passwd-acct)

(change-password passwd-acct "this" "that")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun weighted-pick (weighted-list)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Select the cdr of an element from WEIGHTED-LIST based on the weight in its car."
  (let* ( (total-weight (apply #'+ (mapcar #'car weighted-list)))
          (random-weight (random total-weight))
          (cumulative-weight 0))
    (catch 'return
      (while-let ((item (pop weighted-list)))
        (setq cumulative-weight (+ cumulative-weight (car item)))
        (when (< random-weight cumulative-weight)
          (throw 'return (cdr item)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dotimes (_ 20)
  (message "Selected element: %S" (weighted-pick weighted-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I have a list like this:
'( (3 (prn "Chose the likely case") :LIKELY)
   (2 . :UNLIKELY)
   (1 . :RARE))

;; I would like to randomly select the cdr of an element in this list, using the car as a
;; weighting of how likely it is to pick that cdr.

;; So, for that list, 3/6ths of the time it should select ((prn "Chose the likely case") :LIKELY).
;; 2/6ths of the time it should select :UNLIKELY.
;; 1/6th of the time it should select :RARE.
