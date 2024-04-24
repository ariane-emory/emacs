;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ari's lambda-based objects, inspired by Peter Norvig's book.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--aliases)
(require 'aris-funs--basic-preds)
(require 'aris-funs--lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:universal-methods*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '( (class-name () class-name)
     (dir () method-names)
     (is? (class) (eq class class-name))
     (responds-to? (method) (not (null (memq method method-names)))))
  "Methods possessed by all of Ari's variant of Norvig-style objects.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro a:defclass (class instance-vars class-vars &rest user-methods)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a class for object-oriented programming."
  (let* ( (instance-vars (first (a:extract-delegee-arg instance-vars)))
          (methods (append *a:universal-methods* user-methods))
          (method-names (sort (mapcar #'first methods) #'string<))
          (method-clauses (mapcar #'a:make-method-clause methods)))
    `(let ( (class-name   ',class)
            (method-names ',method-names)
            ,@class-vars)
       ;; Define generic functions for the methods and a constructor for the class:
       (mapc #'a:ensure-generic-fun method-names)
       (cl-defun ,class ,instance-vars
         #'(lambda (message)
             (declare (norvig-object-class ',class))
             (cl-case message ,@method-clauses))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:is-object? (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when THING is a Norvig-style object."
  (let ((declarations (cdadar-safe (cdadddr-safe thing))))
    (cl-some (lambda (form) (equal (car form) 'norvig-object-class)) declarations)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:is? (thing class)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when THING is a Norvig-style object of class CLASS.

In contrast to (is? thing class), this function is not a generic function, so it will
simply return nil if THING is not an object instead of causing an error due to
trying to send a message to a non-object."
  (when (a:is-object? thing)
    (a:send-message thing 'is? class)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:must-be-object! (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Signal an error if THING is not a Norvig-style object."
  (unless (a:is-object? thing)
    (error "Not a Norvig-style object: %s" thing)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:get-method (object message)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return the method that implements message for this object."
  (a:must-be-object! object)
  (funcall object message))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:make-method-clause (clause)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Translate a message from a:defclass into a case clause.

(a:make-clause '(name () name)) ⇒ (name #'(lambda 0 name))"
  `(,(first clause) #'(lambda ,@(rest clause))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:send-message (object message &rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the function to implement the message, and apply the function to the args."
  (a:must-be-object! object)
  (apply (a:get-method object message) args))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:ensure-generic-fun (message)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari commented out a couple of lines such that this will always re-bind the named
  ;;   function.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define an object-oriented dispatch function for a message, unless it has a1ready been defined as one."
  ;;(unless (a:generic-fun-p message)
  (let ((fun #'(lambda (object &rest args)
                 (apply #'a:send-message object message args))))
    (setf (symbol-function message) fun)
    (setf (get message 'generic-fun) fun))) ; )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:generic-fun-p (fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Is this a generic function?"
  (and
    (fboundp fun-name)
    (eq (get fun-name 'generic-fun) (symbol-function fun-name))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:extract-delegee-arg (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Extract the delegee argument from an arglist ARGLIST, returning a list whose first
element is the modified arglist and whose second element is the delegee argument as
a list of the form (CLASS SYMBOL).

This function adds a new lambda list keyword, &delegee. When used, the &delegee
keyword follow all required parameters and precede any &optional and &rest
parameters. The &delegee keyword must be followed by a specifier for the delegee,
which may be either a symbol (meaning the delegee is bound to that symbol and
may be of any class) or a list of length two whose first element is the required
class of the delegee and whose second element is the symbol to bind the delegee to.

Examples of use:
(a:extract-delegee-arg '(password &delegee (account acct) &rest things)) ⇒
 ((password account &rest things) (acct . account))
(a:extract-delegee-arg '(password &delegee acct &optional thing)) ⇒
 ((password nil &optional thing) (acct))
(a:extract-delegee-arg '(password &delegee (account acct))) ⇒
 ((password account) (acct . account))
(a:extract-delegee-arg '(password &delegee acct)) ⇒
 ((password nil) (acct))

Examples of mis-use:
(a:extract-delegee-arg '(password &delegee) ;; malformed ARGLIST, nothing after &delegee.
(a:extract-delegee-arg '(password &delegee &optional foo)) ;; malformed ARGLIST, &delegee immediately followed by &optional.
(a:extract-delegee-arg '(password &optional thing &delegee acct)) ;; malformed ARGLIST, &optional preceded belegee."
  (if (not (memq '&delegee arglist))
    (list arglist)
    (let (new-arglist-segment delegee)
      (while-let ( (top (first arglist))
                   (_ (not (eq '&delegee top))))
        (push (pop arglist) new-arglist-segment))
      (pop arglist)
      (unless arglist
        (error "Malformed ARGLIST, nothing after &delegee."))
      (let ((top (pop arglist)))
        (when (memq top '(&optional &rest))
          (error "Malformed ARGLIST, &delegee immediately followed by %s." top))
        (unless (or (symbol? top)
                  (and (double? top) (symbol? (first top)) (symbol? (second top))))
          (error (concat "Malformed ARGLIST, &delegee must be followed by a "
                   "symbol or a list of length two.")))
        (setq delegee
          (if (symbol? top)
            (list top)
            (nreverse top))))
      (push (first delegee) new-arglist-segment) ; add delegee's symbol.
      (list (append (nreverse new-arglist-segment) arglist) delegee))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (a:extract-delegee-arg '(password &delegee (account acct) &rest things))
  returns ((password acct &rest things) (acct account)))
(confirm that (a:extract-delegee-arg '(password &delegee acct &optional thing))
  returns ((password acct &optional thing) (acct)))
(confirm that (a:extract-delegee-arg '(password &delegee (account acct)))
  returns ((password acct) (acct account)))
(confirm that (a:extract-delegee-arg '(password &delegee acct))
  returns ((password acct) (acct)))
(confirm that (a:extract-delegee-arg '(password &rest things))
  returns ((password &rest things)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account (name &optional (balance 0.00))
  ((interest-rate .06))
  (withdraw (amt) (if (<= amt balance) (cl-decf balance amt) :INSUFFICIENT-FUNDS))
  (deposit (amt) (cl-incf balance amt))
  (balance () balance)
  (name () name)
  (interest () (cl-infc balance (* balance interest-rate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (a:is-object? (setf normal-acct (account "A. User" 2000.00))) returns t)
(confirm that (class-name normal-acct) returns account)
(confirm that (a:is? normal-acct 'account) returns t)
(confirm that (is? normal-acct 'account) returns t)
(confirm that (deposit normal-acct 42.00) returns 2042.0)
(confirm that (deposit normal-acct 82.00) returns 2124.0)
(confirm that (withdraw normal-acct 200.00) returns 1924.0)
(confirm that (balance normal-acct) returns 1924.0)
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
(confirm that (a:is-object?
                (setf passwd-acct
                  (account-with-password "secret" (account "A. User" 2000.00))))
  returns t)
(confirm that (class-name passwd-acct) returns account-with-password)
(confirm that (a:is? passwd-acct 'account-with-password) returns t)
(confirm that (is? passwd-acct 'account-with-password) returns t)
(confirm that (withdraw passwd-acct "guess" 2000.00) returns :WRONG-PASSWORD)
(confirm that (withdraw passwd-acct "secret" 1500.00) returns 500.0)
(confirm that (withdraw passwd-acct "secret" 1500.00) returns :INSUFFICIENT-FUNDS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account-with-limit (limit &delegee (account acct)) ()
  (withdraw (amt)
    (if (> amt limit)
      :OVER-LIMIT
      (withdraw acct amt)))
  (otherwise (&rest args)
    (apply message acct args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (a:is-object?
                (setf limit-acct
                  (account-with-password "pass"
                    (account-with-limit 100.00
                      (account "A. Thrifty Spender" 500.00)))))
  returns t)
(confirm that (class-name limit-acct) returns account-with-password)
(confirm that (a:is? limit-acct 'account-with-password) returns t) ; because of ordering
(confirm that (is? limit-acct 'account-with-password) returns t); because of ordering
(confirm that (withdraw limit-acct "pass" 200.00) returns :OVER-LIMIT)
(confirm that (withdraw limit-acct "pass" 20.00) returns 480.0)
(confirm that (withdraw limit-acct "guess" 20.00) returns :WRONG-PASSWORD)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
