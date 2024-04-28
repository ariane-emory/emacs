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




(setq methods '((balance nil balance)
                 (class-name nil class-name)
                 (class-names nil
                   (if-let
                     ((par
                        (parent self)))
                     (cons class-name
                       (class-names par))
                     (list class-name)))
                 (deposit
                   (amt)
                   (cl-incf balance amt))
                 (field-names nil
                   (sort
                     (copy-sequence
                       (if-let
                         ((par
                            (parent self)))
                         (cl-union field-names
                           (field-names par))
                         field-names))
                     #'string<))
                 (field-values nil
                   (sort-symbol-keyed-alist
                     (cl-pairlis field-names
                       (list name balance))))
                 (implements?
                   (iface)
                   (let
                     ((interface
                        (get iface 'aos-interface)))
                     (when interface
                       (cl-every
                         (lambda
                           (method)
                           (responds-to? self method))
                         interface))))
                 (interest nil
                   (cl-infc balance
                     (* balance interest-rate)))
                 (is?
                   (class)
                   (or
                     (eq class class-name)
                     (when-let
                       ((par
                          (parent self)))
                       (is? par class))))
                 (method-names nil
                   (sort
                     (copy-sequence
                       (if-let
                         ((par
                            (parent self)))
                         (cl-union method-names
                           (method-names par))
                         method-names))
                     #'string<))
                 (name nil name)
                 (parent nil nil)
                 (prepr nil
                   (prn
                     (strepr self)))
                 (repr nil
                   (cons
                     (cons 'class
                       (class-name self))
                     (mapr
                       (field-values self)
                       (lambda
                         (kvp)
                         (let-kvp kvp
                           (cons \.key
                             (a:maybe-repr \.val)))))))
                 (responds-to?
                   (msg)
                   (or
                     (not
                       (null
                         (memq msg method-names)))
                     (when-let
                       ((par
                          (parent self)))
                       (responds-to? par msg))))
                 (strepr nil
                   (trim-trailing-whitespace
                     (pp-to-string
                       (repr self))))
                 (withdraw
                   (amt)
                   (if
                     (<= amt balance)
                     (cl-decf balance amt)
                     :INSUFFICIENT-FUNDS))))


(mapcar (lambda (method) (list (first method) (count-args (second method)))) methods)
( (balance (0 . 0))
  (class-name (0 . 0))
  (class-names (0 . 0))
  (deposit (1 . 1))
  (field-names (0 . 0))
  (field-values (0 . 0))
  (implements? (1 . 1))
  (interest (0 . 0))
  (is? (1 . 1))
  (method-names (0 . 0))
  (name (0 . 0))
  (parent (0 . 0))
  (prepr (0 . 0))
  (repr (0 . 0))
  (responds-to? (1 . 1))
  (strepr (0 . 0))
  (withdraw (1 . 1)))



