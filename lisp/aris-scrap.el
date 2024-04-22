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


(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let* ((length (cl-reduce #'+ things
                   :key (lambda (x) (length (string x)))))
          (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
                (len (length x)))
          (cl-replace name x :start1 index)
          (cl-incf index len))))))

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let* ((length (apply #'+ (mapcar #'length (mapcar #'symbol-name things))))
          (name (make-string length 0)))
    (let ((index 0))
      (dolist (thing things (intern name))
        (let* ((x (symbol-name thing))
                (len (length x)))
          (setq name (replace-regexp-in-string "\\cc" x name nil t index))
          (setq index (+ index len)))))))

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let* ((length (apply #'+ (mapcar #'(lambda (x)
                                        (if (symbolp x)
                                          (length (symbol-name x))
                                          (length x)))
                              things)))
          (name (make-string length 0)))
    (let ((index 0))
      (dolist (thing things (intern name))
        (let* ((x (if (symbolp thing)
                    (symbol-name thing)
                    thing))
                (len (length x)))
          (setq name (replace-regexp-in-string "\\cc" x name nil t index))
          (setq index (+ index len)))))))

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (intern
    (apply #'concat
      (mapcar (lambda (thing)
                (if (symbolp thing)
                  (symbol-name thing)
                  thing))
        things))))

(symbolicate2 'asd "bar")

(defun symbolicate2 (&rest things)
  (string-join (mapcar (lambda (x) (format "%s" x)) things)))
