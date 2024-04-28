;; -*- lexical-binding: t; fill-column: 85; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ari's lambda-based objects, inspired by Peter Norvig's book.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--aliases)
(require 'aris-funs--arglists)
(require 'aris-funs--alists)
(require 'aris-funs--basic-preds)
(require 'aris-funs--let-kvp)
(require 'aris-funs--lists)
(require 'aris-funs--strings)
(require 'aris-funs--symbolicate)
(require 'aris-funs--unsorted)
(require 'aris-funs--with-gensyms)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO:
;;  - private methods + interfaces preds = traits?
;;  - &get / &set / &getset.
;;  - 'is' types, 'satisfies' types and 'implements types'?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *aos:cl-lambda-list-keywords*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '(&optional &key &rest)
  ;; we're not concerning ourselves with &body and &aux for now, but in the
  ;; future we should probably forbid them.
  " CL's lambda list keywords excluding &aux and &body.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *aos:cl-lambda-list-keywords-other-than-&optional*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cl-remove '&optional *aos:cl-lambda-list-keywords*)
  "Keywords that can appear in a lambda list other than &optional.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *aos:defclass-lambda-list-keywords*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cons '&parent *aos:cl-lambda-list-keywords*)
  "Keywords that can appear in `aos:defclass' lambda list.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (sort-symbol-keyed-alist (cl-union '((a . 1) (b. 2) (c . 3)) '((c . 33) (d . 4)) :test (lambda (x y) (eq (car x) (car y)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *aos:universal-methods*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '( (class-name   ()      class-name)
     (class-names  ()      (if-let ((par (parent self)))
                             (cons class-name (class-names par))
                             (list class-name)))
     ;;-------------------------------------------------------------------------------
     (signature    (msg)   (or (assoc msg method-signatures)
                             (when-let ((par (parent self)))
                               (signature par msg))))     
     (signatures   ()      (sort-symbol-keyed-alist
                             (if-let ((par (parent self)))
                               (cl-union method-signatures
                                 (signatures par) :test
                                 (lambda (x y) (eq (car x) (car y))))
                               method-signatures)))
     ;;-------------------------------------------------------------------------------
     (responds-to? (msg)   (not (null (signature self msg))))
     ;;-------------------------------------------------------------------------------
     (method-names ()      (mapcar #'first (signatures self)))
     ;;-------------------------------------------------------------------------------
     (field-names  ()      (sort-with-string<
                             (if-let ((par (parent self)))
                               (cl-union field-names (field-names par))
                               field-names)))
     ;;-------------------------------------------------------------------------------
     (implements?  (iface) (let ((interface (get iface 'aos:interface)))
                             (when interface
                               (with-gensyms (implements?-dolist)
                                 (catch implements?-dolist
                                   (dolist (sig interface)
                                     (let ((mysig (signature self (car sig))))
                                       (unless
                                         (and mysig 
                                           (compatible-arg-counts? (second sig)
                                             (second mysig)))
                                         (throw implements?-dolist nil))))
                                   t)))))
     ;;-------------------------------------------------------------------------------
     (is?          (class) (or (eq class class-name)
                             (when-let ((par (parent self)))
                               (is? par class))))
     ;;-------------------------------------------------------------------------------
     (prepr        ()      (prn (strepr self)))
     (strepr       ()      (trim-trailing-whitespace
                             (pp-to-string (repr self))))
     (repr         ()      (cons (cons 'class (class-name self)) 
                             (rmapcar (field-values self)
                               (lambda (kvp)
                                 (let-kvp kvp
                                   (cons .key (aos:maybe-repr .val))))))))
  ;;----------------------------------------------------------------------------------
  ;; Note that all objects also have a `field-values' and possibly a `parent'
  ;; method but, since they need to access instance variables, they are
  ;; synthesized in `defclass' in order to resolve them in the right lexical
  ;; scope.
  "Methods possessed by all objects in Ari's variant of Norvig-style objects.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aos:defclass (class arglist class-vars &rest user-methods)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a class for object-oriented programming."
  (let-alist (aos:parse-defclass-args arglist)
    (let* ( (constructor-name (symbolicate 'make class))
            ;; synthesize these methods so we can inject them into the
            ;; `cl-defun' in the expansion so that it can access the instance's
            ;; arglist:
            (field-values-method
              `(field-values ()
                 (sort-symbol-keyed-alist
                   (cl-pairlis field-names (list ,@.field-names)))))
            (parent-method
              `(parent () ,(if .parent-sym .parent-sym nil)))
            ;; end of synthesized methods.
            (synthesized-methods (list field-values-method parent-method))
            ;; extract the otherwise method:
            (otherwise-assoc (or (assoc 'delegate user-methods)
                               (assoc 'otherwise user-methods)))
            ;; remove the otherwise method, we'll tack one onto the end of the
            ;; list later:
            (user-methods (cl-remove otherwise-assoc user-methods))
            (methods (append *aos:universal-methods* synthesized-methods
                       user-methods))
            ;; sort methods by name:
            (methods (sort-symbol-keyed-alist methods))
            (method-names (mapcar #'first methods))
            (method-signatures
              (mapcar (lambda (method) (list (first method) (count-args (second method))))
                methods))
            (methods (if (null .parent-sym)
                       methods
                       ;; because .parent-sym, we need an 'otherwise' method:
                       (append methods
                         (list (cons 'otherwise
                                 (or
                                   ;; either use a supplied 'otherwise' method:
                                   (cdr otherwise-assoc)
                                   ;; or synthesize one:
                                   `((&rest args)
                                      (apply message ,.parent-sym args))))))))
            (method-clauses (mapcar #'aos:make-method-clause methods))
            (parent-test-expr
              (when .parent-classes
                ;; we need a class test for the parent:
                `((unless ; warapped in a list for splicing.
                    (and (aos:is-object? ,.parent-sym)
                      (memq (class-name ,.parent-sym) ',.parent-classes))
                    (error "Parent class is not %s%s: %S."
                      (empty-string-unless (rest ',.parent-classes) "one of ")
                      (apply #'pp-things-to-string :or ',.parent-classes)
                      (aos:maybe-repr ,.parent-sym)))))))
      ;; `let' class variables:
      `(let ( (class-name        ',class)
              (field-names       ',.field-names)
              (method-signatures ',method-signatures)
              ,@class-vars)
         ;; define generic functions for the methods:
         (mapc #'aos:ensure-generic-fun ',method-names)
         ;; define a constructor for the class:
         (cl-defun ,constructor-name ,.arglist
           ,@parent-test-expr
           (let (self)
             ;; bind SELF lexically so that the object can reference itself:
             (setq self
               ;; the object itself:
               #'(lambda (message)
                   (declare (aos:class ',class))
                   (cl-case message ,@method-clauses)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:parse-defclass-args (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Extract the parent argument from an arglist ARGLIST, returning an alist.

This function adds a new lambda list keyword, &parent. When used, the &parent
keyword must precede any &rest, &key or &aux parameters but may be &optional.
The &parent keyword must be followed by a specifier for the parent, which may
be either a symbol (meaning the parent is bound to that symbol and may be of any
class) or a list whose first element is the symbol to bind the parent to and whose tail
is a list of possible classes for the parent.

Examples of use:
(see unit tests)

Examples of mis-use:
(aos:parse-defclass-args '(password &parent) ;; malformed ARGLIST, nothing after &parent.
;; malformed ARGLIST, &rest precedes &parent:
(aos:parse-defclass-args '(password &rest thing &parent acct))"
  (when (or (memq '&aux arglist) (memq '&aux arglist))
    (error "Malformed ARGLIST, &aux and &body are not supported."))
  (let ((alist (make-empty-alist arglist field-names
                 parent-sym parent-classes parent-is-optional)))
    (alist-put! 'field-names alist (arg-names arglist '(&parent)))
    (if (not (memq '&parent arglist))
      (alist-put! 'arglist alist arglist)
      (let (new-arglist-segment)
        (while-let ( (popped (pop arglist))
                     (_ (not (eq popped '&parent))))
          (when (eq popped '&optional)
            (alist-put! 'parent-is-optional alist t))
          (when (memq popped *aos:cl-lambda-list-keywords-other-than-&optional*)
            (error "Malformed ARGLIST, %s before &parent." top))
          (push popped new-arglist-segment))
        (unless arglist
          (error "Malformed ARGLIST, nothing after &parent."))
        (let ((popped (pop arglist)))
          (when (memq popped *aos:defclass-lambda-list-keywords*)
            (error "Malformed ARGLIST, &parent immediately followed by %s."
              popped))
          (unless
            (or (symbol? popped)
              (and (proper-list? popped)
                (length> popped 1)
                (cl-every #'symbol? popped)))
            (error (concat "Malformed ARGLIST, &parent must be followed by a "
                     "symbol or a list of 2 or more symbols.")))
          (let ((parent-sym (if (symbol? popped) popped (first popped))))
            (alist-put! 'parent-sym alist parent-sym)
            (push parent-sym new-arglist-segment))
          (alist-put! 'parent-classes alist
            (when (not (symbol? popped)) (rest popped))))
        (alist-put! 'arglist alist
          (nconc (reverse new-arglist-segment) arglist))))
    alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case with untyped mandatory un-typed parent PAR first:
(confirm that (aos:parse-defclass-args '(&parent account password))
  returns ( (arglist account password)
            (field-names account password)
            (parent-sym . account)
            (parent-classes)
            (parent-is-optional)))
;; case with mandatory untyped parent PAR and an &optional THING:
(confirm that (aos:parse-defclass-args '(password &parent par &optional thing))
  returns ( (arglist password par &optional thing)
            (field-names password par thing)
            (parent-sym . par)
            (parent-classes)
            (parent-is-optional)))
;; case with typed parent PAR first:
(confirm that (aos:parse-defclass-args '(&parent (par account) password))
  returns ( (arglist par password)
            (field-names par password)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional)))
;; case with typed parent PAR with two class variants first:
(confirm that (aos:parse-defclass-args '(&parent (par account account2) password))
  returns ( (arglist par password)
            (field-names par password)
            (parent-sym . par)
            (parent-classes account account2)
            (parent-is-optional)))
;; case with typed parent PAR:
(confirm that (aos:parse-defclass-args '(password &parent (par account)))
  returns ( (arglist password par)
            (field-names password par)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional)))
;; case with un-typed parent PAR:
(confirm that (aos:parse-defclass-args '(password &parent par))
  returns ( (arglist password par)
            (field-names password par)
            (parent-sym . par)
            (parent-classes)
            (parent-is-optional)))
;; case with optional parent PAR:
(confirm that (aos:parse-defclass-args '(password &optional thing &parent par))
  returns ( (arglist password &optional thing par)
            (field-names password thing par)
            (parent-sym . par)
            (parent-classes)
            (parent-is-optional . t)))
;; case with optional parent PAR:
(confirm that (aos:parse-defclass-args '(password &optional &parent par thing))
  returns ( (arglist password &optional par thing)
            (field-names password par thing)
            (parent-sym . par)
            (parent-classes)
            (parent-is-optional . t)))
;; case with optional typed parent PAR:
(confirm that (aos:parse-defclass-args '(password &optional &parent (par account) thing))
  returns ( (arglist password &optional par thing)
            (field-names password par thing)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional . t)))
;; case with optional typed parent:
(confirm that
  (aos:parse-defclass-args
    '(password &optional (foo 5) &parent (par account) bar))
  returns ( (arglist password &optional (foo 5) par bar)
            (field-names password foo par bar)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional . t)))
;; case with mandatory parent PAR and a &rest THINGS:
(confirm that
  (aos:parse-defclass-args '(password &parent (par account) &rest things))
  returns ( (arglist password par &rest things)
            (field-names password par things)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional)))
;; 'do nothing' case:
(confirm that (aos:parse-defclass-args '(password &rest things))
  returns ( (arglist password &rest things)
            (field-names password things)
            (parent-sym)
            (parent-classes)
            (parent-is-optional)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:make-method-clause (expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Translate an element of aos:defclass METHODS argument into a case clause.

Example:
(aos:make-clause '(name (x &rest ys) name)) ⇒ (name #'(lambda (x &rest ys) name))"
  `(,(first expr) #'(lambda ,@(rest expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:is-object? (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when THING is a Norvig-style object."
  (let ((declarations (cdadar-safe (cdadddr-safe thing))))
    (cl-some (lambda (form) (equal (car form) 'aos:class)) declarations)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:is? (thing class)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when THING is a Norvig-style object of class CLASS.

In contrast to (is? thing class), this function is not a generic function, so it will
simply return nil if THING is not an object instead of causing an error due to
trying to send a message to a non-object."
  (when (aos:is-object? thing)
    (aos:send-message thing 'is? class)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:must-be-object! (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Signal an error if THING is not a Norvig-style object."
  (unless (aos:is-object? thing)
    (error "Not a Norvig-style object: %s" thing)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:get-method (object message)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return the method that implements MESSAGE for OBJECT."
  (aos:must-be-object! object)
  (funcall object message))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:send-message (object message &rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the method from OBJECT to handle MESSAGE, and apply the method to ARGS ."
  (aos:must-be-object! object)
  ;; (prn "send: Getting method for %s..." message)
  (if-let ((method (aos:get-method object message)))
    (progn
      ;; (prn "send: Got method for %s." message)
      ;; (with-indentation
      (apply method args)) ; )
    (error "send: No method for %s." message)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:ensure-generic-fun (message)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari commented out a couple of lines such that this will always re-bind the
  ;;   named function.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a generic dispatch function for MESSAGE, unless it has a1ready been defined as one."
  ;;(unless (aos:generic-fun-p message)
  (let ((fun #'(lambda (object &rest args)
                 (apply #'aos:send-message object message args))))
    (setf (symbol-function message) fun)
    (setf (get message 'aos:generic-fun) fun))) ; )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:maybe-repr (val)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "If VAL is an object, return its `repr'; otherwise, return VAL."
  (if (aos:is-object? val) (repr val) val))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:generic-fun-p (fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when the symbol FUN-NAME it bound to a generic function."
  (and
    (fboundp fun-name)
    (eq (get fun-name 'aos:generic-fun) (symbol-function fun-name))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'aos:generic-fun? 'aos:generic-fun-p)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro aos:definterface (name method-names)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Define an interface with NAME and METHOD-NAMES."
;;   (unless (symbolp name)
;;     (error "Interface name must be a symbol."))
;;   (unless (cl-every #'symbolp method-names)
;;     (error "All method names must be symbols."))
;;   (let ((method-names (sort-with-string< method-names)))
;;     `(setf (get ',name 'aos:interface) ',method-names)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that (aos:definterface account (balance deposit name interest withdraw))
;;   returns (balance deposit interest name withdraw))
;; (confirm that (aos:definterface account (withdraw balance name deposit interest))
;;   returns (balance deposit interest name withdraw))
;; (confirm that (get 'account 'aos:interface) returns
;;   (balance deposit interest name withdraw))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aos:definterface (name specs)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define an interface. SPECS take the form (METHOD-NAME (MIN-ARGS . MAX-ARGS))."
  (unless (symbolp name)
    (error "NAME must be a symbol."))
  (cl-flet
    ((valid-spec? (spec)
       (unless
         (and 
           (proper-list-p spec)
           (length= spec 2)
           (consp (second spec))
           (integerp (car (second spec)))
           (or (null (cdr (second spec)))
             (integerp (cdr (second spec)))))
         (error
           "All SPECS must have the form (METHOD-NAME (MIN-ARGS . MAX-ARGS)): %S."
           spec))))
    (when (length= specs 0)
      (error "SPECS may not be empty."))
    (mapc #'valid-spec? specs)
    (let ((specs (sort-symbol-keyed-alist specs)))
      `(setf (get ',name 'aos:interface) ',specs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (aos:definterface account
    ( (balance  (0 . 0))
      (deposit  (1 . 1))
      (name     (0 . 0))
      (interest (0 . 0))
      (withdraw (1 . 1))))
  returns
  ( (balance  (0 . 0))
    (deposit  (1 . 1))
    (interest (0 . 0))
    (name     (0 . 0))
    (withdraw (1 . 1))))
(confirm that
  (aos:definterface account
    ( (deposit  (1 . 1))
      (name     (0 . 0))
      (balance  (0 . 0))
      (withdraw (1 . 1))
      (interest (0 . 0))))
  returns
  ( (balance  (0 . 0))
    (deposit  (1 . 1))
    (interest (0 . 0))
    (name     (0 . 0))
    (withdraw (1 . 1))))
(confirm that (get 'account 'aos:interface)
  returns
  ( (balance  (0 . 0))
    (deposit  (1 . 1))
    (interest (0 . 0))
    (name     (0 . 0))
    (withdraw (1 . 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:defclass basic-account (name &optional (balance 0.00))
  ((interest-rate .06))
  (withdraw (amt) (if (<= amt balance) (cl-decf balance amt) :INSUFFICIENT-FUNDS))
  (deposit  (amt) (cl-incf balance amt))
  (balance  ()    balance)
  (name     ()    name)
  (interest ()    (cl-infc balance (* balance interest-rate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (aos:is-object? (setq basic-acct (make-basic-account "A. User" 2000.00)))
  returns t)
(confirm that (class-name basic-acct) returns basic-account) 
(confirm that (method-names basic-acct) returns
  ( balance class-name class-names deposit field-names field-values implements?
    interest is? method-names name parent prepr repr responds-to? signature
    signatures strepr withdraw))
(confirm that (signatures basic-acct) returns
  ((balance (0 . 0))
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
    (signature (1 . 1))
    (signatures (0 . 0))
    (strepr (0 . 0))
    (withdraw (1 . 1))))
(confirm that (signature basic-acct 'withdraw) returns (withdraw (1 . 1)))
(confirm that (signature basic-acct 'deposit)  returns (deposit  (1 . 1)))
(confirm that (signature basic-acct 'balance)  returns (balance  (0 . 0)))
(confirm that (responds-to? basic-acct 'withdraw) returns t)
(confirm that (field-names basic-acct) returns (balance name))
(confirm that (aos:is? basic-acct 'basic-account) returns t)
(confirm that (is? basic-acct 'basic-account) returns t)
(confirm that (implements? basic-acct  'account) returns t)
(confirm that (parent basic-acct) returns nil)
(confirm that (deposit basic-acct 42.00) returns 2042.0)
(confirm that (deposit basic-acct 82.00) returns 2124.0)
(confirm that (withdraw basic-acct 200.00) returns 1924.0)
(confirm that (balance basic-acct) returns 1924.0)
(confirm that (repr basic-acct) returns
  ((class . basic-account)
    (balance . 1924.0)
    (name . "A. User")))
(confirm that (strepr basic-acct) returns
  "((class . basic-account)\n  (balance . 1924.0)\n  (name . \"A. User\"))")
;; (makunbound 'basic-acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:defclass account-with-password (password &parent acct) ()
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (aos:is-object?
    (setq passwd-acct
      (make-account-with-password "secret"
	      (make-basic-account "A. User" 2000.00))))
  returns t)
(confirm that (class-name passwd-acct) returns account-with-password)
(confirm that (class-names passwd-acct)
  returns (account-with-password basic-account))
(confirm that (responds-to? passwd-acct 'withdraw) returns t)
(confirm that (responds-to? passwd-acct 'balance) returns t)
(confirm that (method-names passwd-acct) returns
  ( balance change-password check-password class-name class-names deposit
    field-names field-values implements? interest is? method-names name parent
    prepr repr responds-to? signature signatures strepr withdraw))
(confirm that (field-names passwd-acct) returns (acct balance name password))
(confirm that (aos:is? passwd-acct 'account-with-password) returns t)
(confirm that (is? passwd-acct 'account-with-password) returns t)
(confirm that (aos:is? passwd-acct 'basic-account) returns t)
(confirm that (is? passwd-acct 'basic-account) returns t)
(confirm that (implements? passwd-acct 'account) returns t)
(confirm that (repr (parent passwd-acct))
  returns ((class . basic-account)
            (balance . 2000.0)
            (name . "A. User")))
(confirm that (withdraw passwd-acct "guess" 2000.00) returns :WRONG-PASSWORD)
(confirm that (withdraw passwd-acct "secret" 1500.00) returns 500.0)
(confirm that (withdraw passwd-acct "secret" 1500.00)
  returns :INSUFFICIENT-FUNDS)
(confirm that (repr passwd-acct) returns
  ((class . account-with-password)
    (acct
      (class . basic-account)
      (balance . 500.0)
      (name . "A. User"))
    (password . "secret")))
(confirm that (strepr passwd-acct) returns
  "((class . account-with-password)\n  (acct\n    (class . basic-account)\n    (balance . 500.0)\n    (name . \"A. User\"))\n  (password . \"secret\"))")
;; (makunbound 'passwd-acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:defclass account-with-limit (limit &parent (acct basic-account)) ()
  (withdraw (amt)
    (if (> amt limit)
      :OVER-LIMIT
      (withdraw acct amt))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (aos:is-object?
    (setq limit-acct
      (make-account-with-password "pass"
        (make-account-with-limit 100.00
          (make-basic-account "A. Thrifty Spender" 500.00)))))
  returns t)
(confirm that (class-name limit-acct) returns account-with-password)
(confirm that (method-names limit-acct) returns 
  ( balance change-password check-password class-name class-names deposit
    field-names field-values implements? interest is? method-names name parent
    prepr repr responds-to? signature signatures strepr withdraw))
(confirm that (field-names limit-acct) returns
  (acct balance limit name password))
(confirm that (aos:is? limit-acct 'account-with-password) returns t)
(confirm that (is? limit-acct 'account-with-password) returns t)
(confirm that (is? limit-acct 'basic-account) returns t)
(confirm that (implements? limit-acct 'account) returns t)
(confirm that (implements? limit-acct 'nope) returns nil)
(confirm that (repr (parent limit-acct))
  returns ((class . account-with-limit)
            (acct (class . basic-account)
              (balance . 500.0) (name . "A. Thrifty Spender"))
            (limit . 100.0)))
(confirm that (withdraw limit-acct "pass"  200.00) returns :OVER-LIMIT)
(confirm that (withdraw limit-acct "pass"  20.00)  returns 480.0)
(confirm that (withdraw limit-acct "guess" 20.00)  returns :WRONG-PASSWORD)
(confirm that (repr limit-acct) returns
  ((class . account-with-password)
    (acct
      (class . account-with-limit)
      (acct
        (class . basic-account)
        (balance . 480.0)
        (name . "A. Thrifty Spender"))
      (limit . 100.0))
    (password . "pass")))
(confirm that (strepr limit-acct) returns
  "((class . account-with-password)\n  (acct\n    (class . account-with-limit)\n    (acct\n      (class . basic-account)\n      (balance . 480.0)\n      (name . \"A. Thrifty Spender\"))\n    (limit . 100.0))\n  (password . \"pass\"))")
;; (makunbound 'limit-acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aos:satisfies? (thing pred-or-preds)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when THING satisfies all of the predicates in PRED-OR-PREDS.

PRED-OR-PREDS may be a single predicate or a list of predicates.

Not too confident in this one yet!"
  (if (listp pred-or-preds)
    (cl-every (lambda (pred) (funcall pred thing)) pred-or-preds)
    (funcall pred-or-preds thing)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:satisfies? basic-acct #'aos:is-object?)
(aos:satisfies? basic-acct '(aos:is-object? (lambda (o) (aos:is-object? o))))
(aos:satisfies? basic-acct '((lambda (o) (aos:is-object? o))))
;; This should ideally work, but it doesn't right now:
;; (aos:satisfies? basic-acct '(lambda (o) (aos:is-object? o))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
