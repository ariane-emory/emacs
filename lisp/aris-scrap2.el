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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a:parse-defclass-args2 (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(a:parse-defclass-args2 '(password &parent) ;; malformed ARGLIST, nothing after &parent.
;; malformed ARGLIST, &rest precedes &parent:
(a:parse-defclass-args2 '(password &rest thing &parent acct))"
  (when (memq '&aux arglist)
    (error "Malformed ARGLIST, &aux is not supported."))
  (let ((alist (make-empty-alist arglist field-names
                 parent-sym parent-classes parent-is-optional)))
    (alist-put! 'field-names alist
      (mapcar (lambda (x) (or (car-safe x) x))
        (cl-remove-if
          (lambda (x) (memq x *a:defclass-lambda-list-keywords*))
          arglist)))
    (if (not (memq '&parent arglist))
      (alist-put! 'arglist alist arglist)
      (let (new-arglist-segment)
        (while-let ( (popped (pop arglist))
                     (_ (not (eq popped '&parent))))
          (when (eq popped '&optional)
            (alist-put! 'parent-is-optional alist t))
          (when (memq popped *a:cl-lambda-list-keywords-other-than-&optional*)
            (error "Malformed ARGLIST, %s before &parent." top))
          (push popped new-arglist-segment))
        (unless arglist
          (error "Malformed ARGLIST, nothing after &parent."))
        (let ((popped (pop arglist)))
          (when (memq popped *a:defclass-lambda-list-keywords*)
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
          (nconc (reverse new-arglist-segment) arglist))
        alist))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case with untyped mandatory un-typed parent PAR first:
(confirm that (a:parse-defclass-args2 '(&parent account password))
  returns ( (arglist account password)
            (field-names account password)
            (parent-sym . account)
            (parent-classes)
            (parent-is-optional)))
;; case with mandatory untyped parent PAR and an &optional THING:
(confirm that (a:parse-defclass-args2 '(password &parent par &optional thing))
  returns ( (arglist password par &optional thing)
            (field-names password par thing)
            (parent-sym . par)
            (parent-classes)
            (parent-is-optional)))
;; case with typed parent PAR first:
(confirm that (a:parse-defclass-args2 '(&parent (par account) password))
  returns ( (arglist par password)
            (field-names par password)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional)))
;; case with typed parent PAR with two class variants first:
(confirm that (a:parse-defclass-args2 '(&parent (par account account2) password))
  returns ( (arglist par password)
            (field-names par password)
            (parent-sym . par)
            (parent-classes account account2)
            (parent-is-optional)))
;; case with typed parent PAR:
(confirm that (a:parse-defclass-args2 '(password &parent (par account)))
  returns ( (arglist password par)
            (field-names password par)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional)))
;; case with un-typed parent PAR:
(confirm that (a:parse-defclass-args2 '(password &parent par))
  returns ( (arglist password par)
            (field-names password par)
            (parent-sym . par)
            (parent-classes)
            (parent-is-optional)))
;; case with optional parent PAR:
(confirm that (a:parse-defclass-args2 '(password &optional thing &parent par))
  returns ( (arglist password &optional thing par)
            (field-names password thing par)
            (parent-sym . par)
            (parent-classes)
            (parent-is-optional . t)))
;; case with optional parent PAR:
(confirm that (a:parse-defclass-args2 '(password &optional &parent par thing))
  returns ( (arglist password &optional par thing)
            (field-names password par thing)
            (parent-sym . par)
            (parent-classes)
            (parent-is-optional . t)))
;; case with optional typed parent PAR:
(confirm that (a:parse-defclass-args2 '(password &optional &parent (par account) thing))
  returns ( (arglist password &optional par thing)
            (field-names password par thing)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional . t)))
;; case with optional typed parent:
(confirm that
  (a:parse-defclass-args2
    '(password &optional (foo 5) &parent (par account) bar))
  returns ( (arglist password &optional (foo 5) par bar)
            (field-names password foo par bar)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional . t)))
;; case with mandatory parent PAR and a &rest THINGS:
(confirm that
  (a:parse-defclass-args2 '(password &parent (par account) &rest things))
  returns ( (arglist password par &rest things)
            (field-names password par things)
            (parent-sym . par)
            (parent-classes account)
            (parent-is-optional)))
;; 'do nothing' case:
(confirm that (a:parse-defclass-args2 '(password &rest things))
  returns ( (arglist password &rest things)
            (field-names password things)
            (parent-sym)
            (parent-classes)
            (parent-is-optional)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ((needle 'f))
  (memq needle (cdr (memq needle '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))) ;; nil

(let ((needle 'f))
  (member needle (cdr (member needle '(a b c d e f g h i j k l m n o p q r s t u f v w x y z))))) ;; (f v w x y z)

(defun count>= (needle lst count)
  "t when COUNT or more instances of NEEDLE are found in LST."
  (prndiv)
  (prn "needle: %s" needle)
  (prn "lst:    %s" lst)
  (prn "count:  %s" count)
  (when (< count 0)
    (error "COUNT must be a non-negative integer."))
  (if (and (= count 0) lst)
    t
    (when (> count 0)
      (debug)
      (count>= needle (cdr (member needle lst)) (1- count)))))

(count>= 'f '(a b c d e f g h i j k l m n o p q r s t u f v w x y z) 1)
(count>= 'f '(a b c d e f g h i j k l m n o p q r s t u f v w x y z) 2) ;; t
(count>= 'f '(a b c d e f g h i j k l m n o p q r s t u f v w x y z) 3)c ;; nil

