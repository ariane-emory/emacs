;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ignore!
  (prn "VAL:          %S" val)
  (prn "a:is-object?: %S" (a:is-object? val))
  (push (format "%s:%s %s" key padding
          (if (a:is-object? val)
            "foo" ; (fmt val)
            val))
    lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(a:defclass barclass (num &delegee (parent fooclass)) ()
  (bar () (format "BAR! %d" num)))

(a:defclass bazclass (num &delegee (parent barclass)) ()
  (baz () (format "BAZ! %d" num)))

(a:defclass bazclass (num &delegee (parent barclass) &rest things) ()
  (baz () (format "BAZ! %d" num)))


(foo (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "FOO! 2"
(bar (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAR! 3"
(baz (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAZ! 5"

(field-names acct)
(field-names passwd-acct)

;; (format acct)


(setq acct (account "A. User" 2000.00))
(field-values acct) ;; => ("A. User" 2000.0)

(a:is-object? (alist-get 'acct (field-values passwd-acct)))

;; (fmt passwd-acct)
;; (fmt-as-lines acct)
;; (fmt acct)
;; (fmt passwd-acct)
;; (fmt-as-lines passwd-acct)

;; (describe acct)
;; (describe limit-acct)
;; (describe passwd-acct)

(repr acct)
(repr passwd-acct)
(repr limit-acct)

(prepr limit-acct)
(strepr limit-acct)

(field-values acct)

((balance . 1924.0)
  (name . "A. User"))

(field-values passwd-acct)

((acct closure
   ((self closure #2
      (message)
      (progn '(declare (aos-class 'account)) nil)
      (cond
        ((eql message 'class-name) #'(lambda nil class-name))
        ((eql message 'method-names) #'(lambda nil method-names))
        ((eql message 'field-names) #'(lambda nil field-names))
        ((eql message 'is?) #'(lambda (class) (eq class class-name)))
        ((eql message 'responds-to?) #'(lambda (method) (not (null (memq method method-names)))))
        ((eql message 'prepr) #'(lambda nil (prn (strepr self))))
        ((eql message 'strepr) #'(lambda nil (trim-trailing-whitespace (pp-to-string (repr self)))))
        ((eql message 'repr) #'(lambda nil (cons (cons 'class (class-name self)) (mapr (field-values self) #'(lambda (kvp) (let ((key (car kvp)) (val (cdr kvp))) (cons key (if (a:is-object? val) (repr val) val))))))))
        ((eql message 'field-values) #'(lambda nil (sort-symbol-keyed-alist (cl-pairlis field-names (list name balance)))))
        ((eql message 'withdraw) #'(lambda (amt) (if (<= amt balance) (setq balance (- balance amt)) :INSUFFICIENT-FUNDS)))
        ((eql message 'deposit) #'(lambda (amt) (setq balance (+ balance amt))))
        ((eql message 'balance) #'(lambda nil balance))
        ((eql message 'name) #'(lambda nil name))
        ((eql message 'interest) #'(lambda nil (cl-infc balance (* balance interest-rate))))))
     (balance . 500.0)
     (name . "A. User")
     (interest-rate . 0.06)
     (method-names balance class-name deposit field-names field-values interest is? method-names name prepr repr responds-to? strepr withdraw)
     (field-names name balance)
     (class-name . account))
   (message)
   (progn '(declare (aos-class 'account)) nil)
   (cond
     ((eql message 'class-name) #'(lambda nil class-name))
     ((eql message 'method-names) #'(lambda nil method-names))
     ((eql message 'field-names) #'(lambda nil field-names))
     ((eql message 'is?) #'(lambda (class) (eq class class-name)))
     ((eql message 'responds-to?) #'(lambda (method) (not (null (memq method method-names)))))
     ((eql message 'prepr) #'(lambda nil (prn (strepr self))))
     ((eql message 'strepr) #'(lambda nil (trim-trailing-whitespace (pp-to-string (repr self)))))
     ((eql message 'repr) #'(lambda nil (cons (cons 'class (class-name self)) (mapr (field-values self) #'(lambda (kvp) (let ((key (car kvp)) (val (cdr kvp))) (cons key (if (a:is-object? val) (repr val) val))))))))
     ((eql message 'field-values) #'(lambda nil (sort-symbol-keyed-alist (cl-pairlis field-names (list name balance)))))
     ((eql message 'withdraw) #'(lambda (amt) (if (<= amt balance) (setq balance (- balance amt)) :INSUFFICIENT-FUNDS)))
     ((eql message 'deposit) #'(lambda (amt) (setq balance (+ balance amt))))
     ((eql message 'balance) #'(lambda nil balance))
     ((eql message 'name) #'(lambda nil name))
     ((eql message 'interest) #'(lambda nil (cl-infc balance (* balance interest-rate))))))
  (password . "secret"))
