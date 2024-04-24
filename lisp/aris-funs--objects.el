;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ari's lambda-based objects, inspired by Peter Norvig's book.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--aliases)
(require 'aris-funs--alists)
(require 'aris-funs--basic-preds)
(require 'aris-funs--lists)
(require 'aris-funs--strings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO:
;;  - tests for `field-values', `fmt', `fmt-as-lines'.
;;  - ability to specify multiple delegee class possibilities.
;;  - &get, &set, &getset.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro a:defclass (class arglist class-vars &rest user-methods)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a class for object-oriented programming."
  (let* ( (field-names     (a:extract-field-names arglist))
          (parsed-arglist  (a:extract-delegee-arg arglist))
          (fields          (first parsed-arglist))
          (delegee-spec    (second parsed-arglist))
          (delegee-sym     (first delegee-spec))
          (delegee-class   (second delegee-spec))
          (delegee-test    (when delegee-class ; wrapped in a list. v
                             `((unless (a:is? ,delegee-sym ',delegee-class)
                                 (error "Delegee is not of class '%s: %S."
                                   ',delegee-class ,delegee-sym)))))
          ;; synthesize these method(s) and inject them into the `cl-defun' so 
          ;; that they can access the instance's fields:
          (field-values-method
            `(field-values ()
               (sort-symbol-keyed-alist (cl-pairlis field-names (list ,@field-names)))))
          ;; end of synthesized method(s).
          (synthesized-methods
            (list field-values-method))
          (methods (append *a:universal-methods* synthesized-methods user-methods)))
    
    ;; (prndiv)
    ;; (prn "defclass %s:" class)
    ;; (prndiv)
    ;; (prn "*a:universal-methods*:")
    ;; (prndiv)
    ;; (prn "%s" (indent-string-lines (substring (pp-to-string *a:universal-methods*) 0 -1)))
    ;; (prndiv)
    ;; (prn "synthesized-methods:")
    ;; (prndiv)
    ;; (prn "%s" (indent-string-lines (substring (pp-to-string synthesized-methods) 0 -1)))
    ;; (prndiv)
    ;; (prn "user-methods:")
    ;; (prndiv)
    ;; (prn "%s" (indent-string-lines (substring (pp-to-string user-methods) 0 -1)))
    ;; (prndiv)
    ;; (prn "methods:")
    ;; (prndiv)
    ;; (prn "%s" (indent-string-lines (substring (pp-to-string methods) 0 -3)) 0 -1)
    ;; (prndiv)

    (when-let ((method (or (assoc 'delegate methods) (assoc 'method-not-found methods))))
      (setf (car method) 'otherwise))
    (when (and delegee-spec (not (alist-has? 'otherwise methods)))
      (nconc methods `((otherwise (&rest args) (apply message ,delegee-sym args)))))
    (let ( (method-names   (sort (mapcar #'first methods) #'string<))
           (method-clauses (mapcar #'a:make-method-clause methods)))
      `(let ( (class-name   ',class)
              (field-names  ',field-names)
              (method-names ',method-names)
              ,@class-vars)
         ;; define generic functions for the methods and a constructor for the class:
         (mapc #'a:ensure-generic-fun method-names)
         (cl-defun ,class ,fields
           ,@delegee-test
           (let (self)
             ;; bind self lexically so object can reference itself:
             (setq self
               #'(lambda (message)
                   (declare (aos-class ',class))
                   (cl-case message ,@method-clauses)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:make-method-clause (clause)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Translate a message from a:defclass into a case clause.

(a:make-clause '(name () name)) ⇒ (name #'(lambda 0 name))"
  `(,(first clause) #'(lambda ,@(rest clause))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:universal-methods*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '( (class-name   () class-name)
     (describe     ()
       (prndiv)
       (mapc #'prn (fmt-as-lines self))
       (prndiv))
     (dir          () method-names)
     (field-names  () field-names)
     (fmt          () (join-string-lines (fmt-as-lines self)))
     (fmt-as-lines ()
       (let ((fmt-values
               (cons (cons 'class-name (symbol-name class-name))
                 (field-values self))))
         (let ((max-len 0))
           ;; measure the max key length so we can line things up;
           (dolist (kvp fmt-values)
             ;; (prn "kvp: %s" kvp)
             (let* ( (key (car kvp))
                     (key-length (length (symbol-name key))))
               (when (> key-length max-len)
                 (setq max-len key-length))))
           ;; print the padded key-value pairs:
           (let (lines)
             (dolist (kvp fmt-values)
               (let* ( (key (car kvp))
                       (val (cdr kvp))
                       (key-length (length (symbol-name key)))
                       (padding (make-string (- max-len key-length) ?\ )))
                 ;; (prn "VAL:          %S" val)
                 ;; (prn "a:is-object?: %S" (a:is-object? val))
                 (push (format "%s:%s %s" key padding
                         (if (a:is-object? val)
                           (fmt val)
                           val)
                         )
                   lines)))
             (nreverse lines)))))
     (is?          (class)  (eq class class-name))
     (responds-to? (method) (not (null (memq method method-names)))))
  ;; Note that all objects also have a `field-values' method but, since
  ;; it need to access instance variables, it is synthesized in `defclass'.
  "Methods possessed by all objects in Ari's variant of Norvig-style objects.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:cl-lambda-list-keywords*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '(&optional &key &rest &aux)
  ;; we're not concerned with &body for now but in the future we should forbidd it.
  "Keywords that can appear in a lambda list.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:cl-lambda-list-keywords-other-than-&optional*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cl-remove '&optional *a:cl-lambda-list-keywords*)
  "Keywords that can appear in a lambda list other than &optional.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *a:defclass-lambda-list-keywords*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cons '&delegee *a:cl-lambda-list-keywords*)
  "Keywords that can appear in a:defclass' lambda list.")
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
  "Return the method that implements message for this object."
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
  "Get the function to implement the message, and apply the function to the args."
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
(defalias 'a:generic-fun? 'a:generic-fun-p)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:extract-delegee-arg (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Extract the delegee argument from an arglist ARGLIST, returning a list whose first
element is the modified arglist and whose second element is the delegee argument as
a list of the form (CLASS SYMBOL IS-OPTIONAL).

This function adds a new lambda list keyword, &delegee. When used, the &delegee
keyword must precede any &rest, &key or &auxparameters but may be &optional.
The &delegee keyword must be followed by a specifier for the delegee, which may
be either a symbol (meaning the delegee is bound to that symbol andmay be of any
class) or a list of length two whose first element is the required class of the delegee
and whose second element is the symbol to bind the delegee to.

Examples of use:
(a:extract-delegee-arg '(password &delegee (acct account) &rest things)) ⇒
 ((password account &rest things) (acct . account))
(a:extract-delegee-arg '(password &delegee acct &optional thing)) ⇒
 ((password nil &optional thing) (acct))
(a:extract-delegee-arg '(password &delegee (acct account))) ⇒
 ((password account) (acct . account))
(a:extract-delegee-arg '(password &delegee acct)) ⇒
 ((password nil) (acct))

Examples of mis-use:
(a:extract-delegee-arg '(password &delegee) ;; malformed ARGLIST, nothing after &delegee.
(a:extract-delegee-arg '(password &rest thing &delegee acct)) ;; malformed ARGLIST, &rest precedes &delegee."
  (if (not (memq '&delegee arglist))
    (list arglist nil)
    (let (new-arglist-segment delegee-is-optional)
      (while-let ( (popped (pop arglist))
                   (_ (not (eq popped '&delegee))))
        (when (eq popped '&optional)
          (setq delegee-is-optional t))
        (when (memq popped *a:cl-lambda-list-keywords-other-than-&optional*)
          (error "Malformed ARGLIST, %s before &delegee." top))
        (push popped new-arglist-segment))
      ;;(pop arglist)
      (unless arglist
        (error "Malformed ARGLIST, nothing after &delegee."))
      (let ((top (pop arglist)))
        (when (memq top *a:defclass-lambda-list-keywords*)
          (error "Malformed ARGLIST, &delegee immediately followed by %s." top))
        (unless (or (symbol? top)
                  (and (double? top) (symbol? (first top)) (symbol? (second top))))
          (error (concat "Malformed ARGLIST, &delegee must be followed by a "
                   "symbol or a list of length two.")))
        (setq delegee
          (if (symbol? top)
            (list top nil delegee-is-optional)
            (append top (list delegee-is-optional)))))
      (push (first delegee) new-arglist-segment) ; add delegee's symbol.
      (list (append (nreverse new-arglist-segment) arglist) delegee))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delegee first case:
(confirm that (a:extract-delegee-arg '(&delegee (acct account) password))
  returns ((acct password) (acct account nil)))
;; typed delegee case:
(confirm that (a:extract-delegee-arg '(password &delegee (acct account)))
  returns ((password acct) (acct account nil)))
;; un-typed delegee case:
(confirm that (a:extract-delegee-arg '(password &delegee acct))
  returns ((password acct) (acct nil nil)))
;; optional delegee case case:
(confirm that (a:extract-delegee-arg '(password &optional thing &delegee acct))
  returns ((password &optional thing acct) (acct nil t)))
;; optional delegee case 2:
(confirm that (a:extract-delegee-arg '(password &optional &delegee acct thing))
  returns ((password &optional acct thing) (acct nil t)))
;; optional delegee case 3:
(confirm that (a:extract-delegee-arg '(password &optional &delegee (acct account) thing))
  returns ((password &optional acct thing) (acct account t)))
;; optional delegee case 4:
(confirm that (a:extract-delegee-arg '(password &optional foo &delegee (acct account) bar))
  returns ((password &optional foo acct bar) (acct account t)))
;; mandatory delegee and an &rest case:
(confirm that (a:extract-delegee-arg '(password &delegee (acct account) &rest things))
  returns ((password acct &rest things) (acct account nil)))
;; mandatory delegate and an &optional case:
(confirm that (a:extract-delegee-arg '(password &delegee acct &optional thing))
  returns ((password acct &optional thing) (acct nil nil)))
;; 'do nothing' case:
(confirm that (a:extract-delegee-arg '(password &rest things))
  returns ((password &rest things) nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:extract-field-names (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Reduce an 'a:defclass' arglist to a list of field names, stripping out the
default values of  &optional arguments and removing &aux arguments."
  (let (without-aux-args)
    (while-let ( (popped (pop arglist))
                 (_ (not (eq '&aux popped))))
      (unless (memq popped *a:defclass-lambda-list-keywords*)
        (push (if (consp popped) (first popped) popped) without-aux-args)))
    (nreverse without-aux-args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (a:extract-field-names
    '( acct &delegee (acct account) &optional baz &rest things
       &key (foo 42) &aux bar (baz quux)))
  returns (acct acct baz things foo))
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
;; (confirm that (dir acct)
;;   returns (balance class-name deposit fmt dir field-names interest is? name responds-to? withdraw))
(confirm that (field-names acct) returns (name balance))
(confirm that (a:is? acct 'account) returns t)
(confirm that (is? acct 'account) returns t)
(confirm that (deposit acct 42.00) returns 2042.0)
(confirm that (deposit acct 82.00) returns 2124.0)
(confirm that (withdraw acct 200.00) returns 1924.0)
(confirm that (balance acct) returns 1924.0)
;; (makunbound 'acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account-with-password (password &delegee acct) ()
  (change-password (pass new-pass)
    (if (equal pass password)
      (setf password new-pass)
      :WRONG-PASSWORD))
  (delegate (pass &rest args)
    (if (equal pass password)
      (apply message acct args)
      :WRONG-PASSWORD)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (a:is-object?
    (setq passwd-acct
      (account-with-password "secret" (account "A. User" 2000.00))))
  returns t)
(confirm that (class-name passwd-acct) returns account-with-password)
;; (confirm that (dir passwd-acct)
;;   returns (change-password class-name fmt dir field-names is? otherwise responds-to?))
(confirm that (field-names passwd-acct) returns (password acct))
(confirm that (a:is? passwd-acct 'account-with-password) returns t)
(confirm that (is? passwd-acct 'account-with-password) returns t)
(confirm that (withdraw passwd-acct "guess" 2000.00) returns :WRONG-PASSWORD)
(confirm that (withdraw passwd-acct "secret" 1500.00) returns 500.0)
(confirm that (withdraw passwd-acct "secret" 1500.00) returns :INSUFFICIENT-FUNDS)
;; (makunbound 'passwd-acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass account-with-limit (limit &delegee (acct account)) ()
  (withdraw (amt)
    (if (> amt limit)
      :OVER-LIMIT
      (withdraw acct amt)))
  (delegate (&rest args)
    (apply message acct args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (a:is-object?
    (setq limit-acct
      (account-with-password "pass"
        (account-with-limit 100.00
          (account "A. Thrifty Spender" 500.00)))))
  returns t)
(confirm that (class-name limit-acct) returns account-with-password)
;; (confirm that (dir limit-acct)
;;   returns (change-password class-name fmt dir field-names is? otherwise responds-to?))
(confirm that (field-names limit-acct) returns (password acct))
(confirm that (a:is? limit-acct 'account-with-password) returns t) ; because of ordering
(confirm that (is? limit-acct 'account-with-password) returns t); because of ordering
(confirm that (withdraw limit-acct "pass" 200.00) returns :OVER-LIMIT)
(confirm that (withdraw limit-acct "pass" 20.00) returns 480.0)
(confirm that (withdraw limit-acct "guess" 20.00) returns :WRONG-PASSWORD)
;; (makunbound 'limit-acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


