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



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro switch (value &rest body)
;;   "Steele's `switch' from 'The Evolution of Lisp'."
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (let* ( (newbody (cl-mapcar #'(lambda (clause)
;;                                   `(,(gensym) ,@(rest clause)))
;;                      body))
;;           (switcher (cl-mapcar #'(lambda (clause newclause)
;;                                    `(,(first clause) (go ,(first newclause))))
;;                       body newbody)))
;;     `(cl-block switch
;;        (cl-tagbody (cl-case ,value ,@switcher)
;;          (break)
;;          ,@(apply #'nconc newbody)))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro break () '(cl-return-from switch))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that
;;   (let (res)
;;     (switch 0
;;       (0 (push "none" res) (break))
;;       (1 (push "one " res))
;;       (2 (push "too " res))
;;       (3 (push "many" res)))
;;     (nreverse res))
;;   returns ("none"))
;; (confirm that
;;   (let (res)
;;     (switch 1
;;       (0 (push "none" res) (break))
;;       (1 (push "one " res))
;;       (2 (push "too " res))
;;       (3 (push "many" res)))
;;     (nreverse res))
;;   returns ("one " "too " "many"))
;; (confirm that
;;   (let (res)
;;     (switch 2
;;       (0 (push "none" res) (break))
;;       (1 (push "one " res))
;;       (2 (push "too " res))
;;       (3 (push "many" res)))
;;     (nreverse res))
;;   returns ("too " "many"))
;; (confirm that
;;   (let (res)
;;     (switch 3
;;       (0 (push "none" res) (break))
;;       (1 (push "one " res))
;;       (2 (push "too " res))
;;       (3 (push "many" res)))
;;     (nreverse res))
;;   returns ("many"))
;; (confirm that
;;   (let (res)
;;     (switch 4
;;       (0 (push "none" res) (break))
;;       (1 (push "one " res))
;;       (2 (push "too " res))
;;       (3 (push "many" res)))
;;     (nreverse res))
;;   returns nil)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let (res)  
;;   (cl-block switch
;;     (cl-tagbody
;;       (cl-case 1
;;         (0 (go g1471))
;;         (1 (go g1472))
;;         (2 (go g1473))
;;         (3 (go g1474)))
;;       (break)
;;       g1471
;;       (push "none" res)
;;       (break)
;;       g1472
;;       (push "one " res)
;;       g1473
;;       (push "too " res)
;;       g1474
;;       (push "many" res)))
;;   (nreverse res))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro switch (value &rest body)
  "Steele's `switch' from 'The Evolution of Lisp' (but with the originally-separate
`break' macro refactored to use `cl-macrolet' and a `gensym'-ed block label ."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ( (newbody  (mapcar #'(lambda (clause) `(,(gensym) ,@(rest clause))) body))
          (switcher (cl-mapcar
                      #'(lambda (clause newclause) `(,(first clause) (go ,(first newclause))))
                      body newbody))
          (switch (gensym "switch-"))
          (foo (apply #'nconc newbody)))
    `(cl-block ,switch
       (cl-macrolet ((break () '(cl-return-from ,switch)))
         (cl-tagbody (cl-case ,value ,@switcher)
           (break)
           ,@foo)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro break () '(cl-return-from switch))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 0
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns ("none"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 1
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns ("one " "too " "many"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 2
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns ("too " "many"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 3
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns ("many"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 4
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


