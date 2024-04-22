;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class account (name &optional (balance 0.00))
  ((interest-rate .06))
  (withdraw (amt) (if (<= amt balance) (cl-decf balance amt) :INSUFFICIENT-FUNDS))
  (deposit (amt) (cl-incf balance amt))
  (balance () balance)
  (name () name)
  (privcall () :BAR)
  (interest () (cl-infc balance (* balance interest-rate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manually tweaked expansion with a private method:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let
;;   ((interest-rate 0.06))
;;   (ensure-generic-fn 'is?)
;;   (mapcar #'ensure-generic-fn '(withdraw deposit balance name privcall interest))
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
(setf acct2 (account "A. User" 2000.00))
(privcall acct2)

(deposit acct2 42.00)
(withdraw acct2 200.00)
(symbol-plist 'acct2)
(symbol-plist 'withdraw)
(is? acct2 'account)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class password-account (password acct) ()
  (change-password (pass new-pass)
    (if (equal pass password)
      (setf password new-pass)
      :WRONG-PASSWORD))
  (otherwise (pass &rest args)
    (if (equal pass password)
      (apply message acct args)
      :WRONG-PASSWORD)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf acct3 (password-account "secret" acct2))
(withdraw acct3 "guess" 2000.00)
(withdraw acct3 "secret" 2000.00)
(is? acct3 'password-account) ;; t
(is? acct3 'account) ;; nil
(class-name acct3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class limited-account (limit acct) ()
  (withdraw (amt)
    (if ( > amt limit)
      :OVER-LIMIT
      (withdraw acct amt)))
  (otherwise (&rest args)
    (apply message acct args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf acct4
  (password-account "pass"
    (limited-account 100.00
      (account "A. Thrifty Spender" 500.00)))) 
(withdraw acct4 "pass" 200.00)
(withdraw acct4 "pass" 20.00)
(withdraw acct4 "guess" 20.00)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(company-quickhelp-mode -1)
(dolist (thing '(1 2 ,3 4 ,@5 #'6 '7 `8))
  (cond
    ((eq 'quote (car-safe thing)) (prn "This one is kind of special: %s" (cadr thing)))
    ((eq '\, (car-safe thing)) (prn "This one is special: %s" (cadr thing)))
    ((eq '\,@ (car-safe thing)) (prn "This one is very special: %s" (cadr thing)))
    ((eq 'function (car-safe thing)) (prn "This one is super special: %s" (cadr thing)))
    ((eq '\` (car-safe thing)) (prn "This one is extra special: %s" (cadr thing)))
    (t (prn "%s" thing))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class integer (value) nil
  (val () value)
  (add (other) (integer (+ value (val other))))
  (sub (other) (integer (- value (val other))))
  (mul (other) (integer (* value (val other))))
  (div (other) (integer (/ value (val other))))
  (rem (other) (integer (% value (val other)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (val (mul (integer 2) (add (integer 5) (integer 7))))
  returns 24)
(confirm that (val (rem (mul (integer 2) (add (integer 5) (integer 7))) (integer 5)))
  returns 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform-tree (pred? fun tree)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Replace atoms matching PRED? in TREE with the result of applying FUN to them."
  (unless (list? tree) (error "TREE must be a list"))
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (fun? fun)   (error "FUN must be a function"))
  (when tree
    (let (result tail)
      (while tree
        (let* ((head (pop tree))
                (new-tail
                  (list
                    (cond
                      ((cons? head) (transform-tree pred? fun head))
                      ((funcall pred? head) (funcall fun head))
                      (else         head)))))
          (if result
            (setcdr tail new-tail)
            (setq result new-tail))
          (setq tail new-tail)))
      result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform-tree (pred? fun tree)
  "Replace atoms matching PRED? in TREE with the result of applying FUN to them."
  (cond
    ((listp tree)
      (mapcar (lambda (item)
                (if (listp item)
                  (transform-tree pred? fun item)
                  (cond
                    ((funcall pred? item) (funcall fun item))
                    (t item))))
        tree))
    (t tree)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform-tree (pred? fun tree)
  "Replace atoms matching PRED? in TREE with the result of applying FUN to them."
  (if (listp tree)
    (mapcar (lambda (item)
              (if (listp item)
                (transform-tree pred? fun item)
                (if (funcall pred? item)
                  (funcall fun item)
                  item)))
      tree)
    tree))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun op-to-name (op)
  (cl-case op
    ('+ 'add)
    ('- 'sub)
    ('* 'mul)
    ('/ 'div)
    ('% 'rem)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (transform-tree #'even? #'double '(1 2 3 4 5 6 7 (8 5 9 5 10)))
  returns (1 4 3 8 5 12 7 (16 5 9 5 20)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(transform-tree #'symbolp #'op-to-name '(2 + (3 * 4) / 5 % 6)) ;; (2 add (3 mul 4) div 5 rem 6)

