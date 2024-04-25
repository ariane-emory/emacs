;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ari's lambda-based objects, inspired by Peter Norvig's book.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--aliases)
(require 'aris-funs--alists)
(require 'aris-funs--basic-preds)
(require 'aris-funs--lists)
(require 'aris-funs--strings)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO:
;;  - Come up with something!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:cl-lambda-list-keywords*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '(&optional &key &rest)
  ;; we're not concerned with &body for now but in the future we should forbidd it.
  "Keywords that can appear in a `defclass' lambda list, which are CL's lambda
list keywords excluding &aux.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:cl-lambda-list-keywords-other-than-&optional*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cl-remove '&optional *a:cl-lambda-list-keywords*)
  "Keywords that can appear in a lambda list other than &optional and &aux.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:defclass-lambda-list-keywords*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cons '&delegee *a:cl-lambda-list-keywords*)
  "Keywords that can appear in a:defclass' lambda list.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:universal-methods*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '( (class-name   ()      class-name)
     (method-names ()      method-names)
     (field-names  ()      field-names)
     (is?          (class) (eq class class-name))
     (responds-to? (msg)   (not (null (memq msg method-names))))
     (prepr        ()      (prn (strepr self)))
     (strepr       ()      (trim-trailing-whitespace (pp-to-string (repr self))))
     (repr         ()
       (cons (cons 'class (class-name self)) 
         (mapr (field-values self)
           (lambda (kvp)
             (let ((key (car kvp)) (val (cdr kvp)))
               (cons key (a:maybe-repr val))))))))
  ;; Note that all objects also have a `field-values' method but, since it needs to
  ;; access instance variables, it is synthesized in `defclass' in order to resolve them
  ;; in the right lexical scope.
  "Methods possessed by all objects in Ari's variant of Norvig-style objects.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro a:defclass (class arglist class-vars &rest user-methods)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a class for object-oriented programming."
  (let ((parsed-arglist  (a:extract-delegee-arg arglist)))
    (let-alist parsed-arglist
      (let* ( ;; synthesize this method so we can inject it into the `cl-defun' in 
              ;; our expansion so that it can access the instance's arglist:
              (field-values-method
                `(field-values ()
                   (sort-symbol-keyed-alist
                     (cl-pairlis field-names (list ,@.field-names)))))
              ;; end of synthesized method(s).
              (synthesized-methods (list field-values-method))
              (methods (append *a:universal-methods* synthesized-methods user-methods)))
        (when-let ((method (or (assoc 'delegate methods)
                             (assoc 'method-not-found methods))))
          (setf (car method) 'otherwise))
        (when (and .delegee-sym (not (assoc 'otherwise methods)))
          (nconc methods `((otherwise (&rest args) (apply message ,.delegee-sym args)))))
        (let ( (method-names   (sort (mapcar #'first methods) #'string<))
               (method-clauses (mapcar #'a:make-method-clause methods))
               (delegee-test
                 (when .delegee-classes
                   `((unless ; warapped in a list for splicing.
                       (and (a:is-object? ,.delegee-sym)
                         (memq (class-name ,.delegee-sym) ',.delegee-classes))
                       (error "Delegee class is not %s%s: %S."
                         (empty-string-unless (rest ',.delegee-classes) "one of ")
                         (apply #'pp-things-to-string :or ',.delegee-classes)
                         (a:maybe-repr ,.delegee-sym)))))))
          ;; `let' class variables:
          `(let ( (class-name   ',class)
                  (field-names  ',.field-names)
                  (method-names ',method-names)
                  ,@class-vars)
             ;; define generic functions for the methods:
             (mapc #'a:ensure-generic-fun method-names)
             ;; define a constructor for the class:
             (cl-defun ,class ,.arglist
               ,@delegee-test
               (let (self)
                 ;; bind SELF lexically so that the object can reference itself:
                 (setq self
                   ;; the object itself:
                   #'(lambda (message)
                       (declare (aos-class ',class))
                       (cl-case message ,@method-clauses)))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:make-method-clause (expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Translate an element of a:defclass METHODS argument into a case clause.

Example:
(a:make-clause '(name (x &rest ys) name)) ⇒ (name #'(lambda (x &rest ys) name))"
  `(,(first expr) #'(lambda ,@(rest expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:is-object? (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when THING is a Norvig-style object."
  (let ((declarations (cdadar-safe (cdadddr-safe thing))))
    (cl-some (lambda (form) (equal (car form) 'aos-class)) declarations)))
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
  "Return the method that implements MESSAGE for OBJECT."
  (a:must-be-object! object)
  ;; (prn "get: Getting method for %s." message)
  ;; (let ((res (with-indentation (funcall object message))))
  ;;   (prn "get: Got method for %s: %s." message res)
  (let ((res (funcall object message)))
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:send-message (object message &rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the method from OBJECT to handle MESSAGE, and apply the method to ARGS ."
  (a:must-be-object! object)
  ;; (prn "send: Getting method for %s..." message)
  (if-let ((method (a:get-method object message)))
    (progn
      ;; (prn "send: Got method for %s." message)
      ;; (with-indentation
      (apply method args)) ; )
    (error "send: No method for %s." message)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:ensure-generic-fun (message)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari commented out a couple of lines such that this will always re-bind the named
  ;;   function.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a generic dispatch function for MESSAGE, unless it has a1ready been defined as one."
  ;;(unless (a:generic-fun-p message)
  (let ((fun #'(lambda (object &rest args)
                 (apply #'a:send-message object message args))))
    (setf (symbol-function message) fun)
    (setf (get message 'generic-fun) fun))) ; )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:maybe-repr (val)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "If VAL is an object, return its `repr'; otherwise, return VAL."
  (if (a:is-object? val) (repr val) val))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:generic-fun-p (fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when the symbol FUN-NAME it bound to a generic function."
  (and
    (fboundp fun-name)
    (eq (get fun-name 'generic-fun) (symbol-function fun-name))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'a:generic-fun? 'a:generic-fun-p)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:extract-delegee-arg (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Extract the delegee argument from an arglist ARGLIST, returning an alist.

This function adds a new lambda list keyword, &delegee. When used, the &delegee
keyword must precede any &rest, &key or &aux parameters but may be &optional.
The &delegee keyword must be followed by a specifier for the delegee, which may
be either a symbol (meaning the delegee is bound to that symbol and may be of any
class) or a list whose first element is the symbol to bind the delegee to and whose tail
is a list of possible classes for the delegee.

Examples of use:
(see unit tests)

Examples of mis-use:
(a:extract-delegee-arg '(password &delegee) ;; malformed ARGLIST, nothing after &delegee.
;; malformed ARGLIST, &rest precedes &delegee:
(a:extract-delegee-arg '(password &rest thing &delegee acct))"
  (when (memq '&aux arglist)
    (error "Malformed ARGLIST, &aux is not supported."))
  (let ((alist (make-empty-alist
                 arglist field-names delegee-sym delegee-classes delegee-is-optional)))
    (cl-flet ((extract-field-names (arglist)
                (mapcar (lambda (x) (or (car-safe x) x))
                  (cl-remove-if (lambda (x) (memq x *a:defclass-lambda-list-keywords*))
                    arglist))))
      (if (not (memq '&delegee arglist))
        (progn 
          (alist-put! 'arglist alist arglist)
          (alist-put! 'field-names alist
            (extract-field-names (alist-get 'arglist alist))))
        (let (new-arglist-segment delegee-is-optional)
          (while-let ( (popped (pop arglist))
                       (_ (not (eq popped '&delegee))))
            (when (eq popped '&optional)
              (alist-put! 'delegee-is-optional alist t))
            (when (memq popped *a:cl-lambda-list-keywords-other-than-&optional*)
              (error "Malformed ARGLIST, %s before &delegee." top))
            (push popped new-arglist-segment))
          (unless arglist
            (error "Malformed ARGLIST, nothing after &delegee."))
          (let ((popped (pop arglist)))
            (when (memq popped *a:defclass-lambda-list-keywords*)
              (error "Malformed ARGLIST, &delegee immediately followed by %s." popped))
            (unless
              (or (symbol? popped)
                (and (proper-list? popped)
                  (length> popped 1)
                  (cl-every #'symbol? popped)))
              (error (concat "Malformed ARGLIST, &delegee must be followed by a "
                       "symbol or a list of 2 or more symbols.")))
            (let ((delegee-sym (if (symbol? popped) popped (first popped))))
              (alist-put! 'delegee-sym alist delegee-sym)
              (push delegee-sym new-arglist-segment))
            (alist-put! 'delegee-classes alist (when (not (symbol? popped)) (rest popped))))
          (alist-put! 'arglist alist (nconc (reverse new-arglist-segment) arglist))
          (alist-put! 'field-names alist
            (extract-field-names (alist-get 'arglist alist)))
          alist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mandatory delegate and an &optional case:
(confirm that (a:extract-delegee-arg '(password &delegee acct &optional thing))
  returns ( (arglist password acct &optional thing)
            (field-names password acct thing)
            (delegee-sym . acct)
            (delegee-classes)
            (delegee-is-optional)))
;; typed delegee first case:
(confirm that (a:extract-delegee-arg '(&delegee (acct account) password))
  returns ( (arglist acct password)
            (field-names acct password)
            (delegee-sym . acct)
            (delegee-classes account)
            (delegee-is-optional)))
;; typed delegee with two classes first case:
(confirm that (a:extract-delegee-arg '(&delegee (acct account account2) password))
  returns ( (arglist acct password)
            (field-names acct password)
            (delegee-sym . acct)
            (delegee-classes account account2)
            (delegee-is-optional)))
;; typed delegee case:
(confirm that (a:extract-delegee-arg '(password &delegee (acct account)))
  returns ( (arglist password acct)
            (field-names password acct)
            (delegee-sym . acct)
            (delegee-classes account)
            (delegee-is-optional)))
;; un-typed delegee case:
(confirm that (a:extract-delegee-arg '(password &delegee acct))
  returns ( (arglist password acct)
            (field-names password acct)
            (delegee-sym . acct)
            (delegee-classes)
            (delegee-is-optional)))
;; optional delegee case case:
(confirm that (a:extract-delegee-arg '(password &optional thing &delegee acct))
  returns ( (arglist password &optional thing acct)
            (field-names password thing acct)
            (delegee-sym . acct)
            (delegee-classes)
            (delegee-is-optional . t)))
;; optional delegee case 2:
(confirm that (a:extract-delegee-arg '(password &optional &delegee acct thing))
  returns ( (arglist password &optional acct thing)
            (field-names password acct thing)
            (delegee-sym . acct)
            (delegee-classes)
            (delegee-is-optional . t)))
;; optional delegee case 3:
(confirm that (a:extract-delegee-arg '(password &optional &delegee (acct account) thing))
  returns ( (arglist password &optional acct thing)
            (field-names password acct thing)
            (delegee-sym . acct)
            (delegee-classes account)
            (delegee-is-optional . t)))
;; optional delegee case 4:
(confirm that
  (a:extract-delegee-arg '(password &optional (foo 5) &delegee (acct account) bar))
  returns ( (arglist password &optional (foo 5) acct bar)
            (field-names password foo acct bar)
            (delegee-sym . acct)
            (delegee-classes account)
            (delegee-is-optional . t)))
;; mandatory delegee and an &rest case:
(confirm that (a:extract-delegee-arg '(password &delegee (acct account) &rest things))
  returns ( (arglist password acct &rest things)
            (field-names password acct things)
            (delegee-sym . acct)
            (delegee-classes account)
            (delegee-is-optional)))
;; 'do nothing' case:
(confirm that (a:extract-delegee-arg '(password &rest things))
  returns ( (arglist password &rest things)
            (field-names password things)
            (delegee-sym)
            (delegee-classes)
            (delegee-is-optional)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account (name &optional (balance 0.00))
  ((interest-rate .06))
  (withdraw (amt) (if (<= amt balance) (cl-decf balance amt) :INSUFFICIENT-FUNDS))
  (deposit  (amt) (cl-incf balance amt))
  (balance  ()    balance)
  (name     ()    name)
  (interest ()    (cl-infc balance (* balance interest-rate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (a:is-object? (setq acct (account "A. User" 2000.00))) returns t)
(confirm that (class-name acct) returns account) 
(confirm that (method-names acct) returns
  ( balance class-name deposit field-names field-values interest is? method-names name
    prepr repr responds-to? strepr withdraw))
(confirm that (field-names acct) returns (name balance))
(confirm that (a:is? acct 'account) returns t)
(confirm that (is? acct 'account) returns t)
(confirm that (deposit acct 42.00) returns 2042.0)
(confirm that (deposit acct 82.00) returns 2124.0)
(confirm that (withdraw acct 200.00) returns 1924.0)
(confirm that (balance acct) returns 1924.0)
(confirm that (repr acct) returns
  ((class . account)
    (balance . 1924.0)
    (name . "A. User")))
(confirm that (strepr acct) returns
  "((class . account)\n  (balance . 1924.0)\n  (name . \"A. User\"))")
;; (makunbound 'acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account-with-password (password &delegee acct) ()
  (check-password (attempted-password)
    (equal attempted-password password))
  (change-password (attempted-password new-password)
    (if (check-password self attempted-password)
      (setf password new-password)
      :WRONG-PASSWORD))
  (delegate (attempted-password &rest args)
    (if (check-password self attempted-password)
      (apply message acct args)
      :WRONG-PASSWORD)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (a:is-object?
    (setq passwd-acct (account-with-password "secret" (account "A. User" 2000.00))))
  returns t)
(confirm that (class-name passwd-acct) returns account-with-password)
(confirm that (method-names passwd-acct) returns
  ( change-password check-password class-name field-names field-values is? method-names
    otherwise prepr repr responds-to? strepr))
(confirm that (field-names passwd-acct) returns (password acct))
(confirm that (a:is? passwd-acct 'account-with-password) returns t)
(confirm that (is? passwd-acct 'account-with-password) returns t)
(confirm that (withdraw passwd-acct "guess" 2000.00) returns :WRONG-PASSWORD)
(confirm that (withdraw passwd-acct "secret" 1500.00) returns 500.0)
(confirm that (withdraw passwd-acct "secret" 1500.00) returns :INSUFFICIENT-FUNDS)
(confirm that (repr passwd-acct) returns
  ((class . account-with-password)
    (acct
      (class . account)
      (balance . 500.0)
      (name . "A. User"))
    (password . "secret")))
(confirm that (strepr passwd-acct) returns
  "((class . account-with-password)\n  (acct\n    (class . account)\n    (balance . 500.0)\n    (name . \"A. User\"))\n  (password . \"secret\"))")
;; (makunbound 'passwd-acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account-with-limit (limit &delegee (acct account)) ()
  (withdraw (amt)
    (if (> amt limit)
      :OVER-LIMIT
      (withdraw acct amt))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (a:is-object?
    (setq limit-acct
      (account-with-password "pass"
        (account-with-limit 100.00
          (account "A. Thrifty Spender" 500.00)))))
  returns t)
(confirm that (class-name limit-acct) returns account-with-password)
(confirm that (method-names limit-acct) returns
  ( change-password check-password class-name field-names field-values is? method-names
    otherwise prepr repr responds-to? strepr))
(confirm that (field-names limit-acct) returns (password acct))
(confirm that (a:is? limit-acct 'account-with-password) returns t) ; because of ordering
(confirm that (is? limit-acct 'account-with-password) returns t); because of ordering
(confirm that (withdraw limit-acct "pass" 200.00) returns :OVER-LIMIT)
(confirm that (withdraw limit-acct "pass" 20.00) returns 480.0)
(confirm that (withdraw limit-acct "guess" 20.00) returns :WRONG-PASSWORD)
(confirm that (repr limit-acct) returns
  ((class . account-with-password)
    (acct
      (class . account-with-limit)
      (acct
        (class . account)
        (balance . 480.0)
        (name . "A. Thrifty Spender"))
      (limit . 100.0))
    (password . "pass")))
(confirm that (strepr limit-acct) returns
  "((class . account-with-password)\n  (acct\n    (class . account-with-limit)\n    (acct\n      (class . account)\n      (balance . 480.0)\n      (name . \"A. Thrifty Spender\"))\n    (limit . 100.0))\n  (password . \"pass\"))")
;; (makunbound 'limit-acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
