;; -*- lexical-binding: nil; fill-column: 90; lisp-indent-offset: 2; eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'aris-funs--alists)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--lists)
(require 'aris-funs--match-pattern)
(require 'aris-funs--match-pattern2)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--stacks)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maybe macro:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro maybe (type val)
  "Return VAL when it is of type TYPE, otherwise return nil."
  `(when (cl-typep ,val ,type)
     ,val))

(defmacro maybe (type val)
  "Return VAL when it is of type TYPE, otherwise return nil."
  (let ((type-form
          (if (and (symbolp type) (get type 'cl-deftype-satisfies))
            `',type
            type)))
    `(when (cl-typep ,val ,type-form)
       ,val)))

(defmacro maybe (type val)
  "Return VAL when it is of type TYPE, otherwise return nil."
  `(when (cl-typep ,val
           ,(if (and (symbolp type) (get type 'cl-deftype-satisfies))
              `',type
              type))
     ,val))

(setq foo 7)
(setq ty 'integer)
(maybe 'integer foo)  ; Works
(maybe integer foo)   ; Works
(maybe ty foo)        ; Works

(if-let ((x (when (integerp foo) foo)))
  (prn "yes: %S" x)
  (prn "no!"))

(if-let ((x (maybe 'integer foo)))
  (prn "yes: %S" x)
  (prn "no!"))

(if-let ((x (maybe integer foo)))
  (prn "yes: %S" x)
  (prn "no!"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defunt tsqr (('integer x))
  "Square an integer."
  (* x x))

(defun tsqr (x)
  "Square an integer."
  (let
    ((x (progn (cl-check-type x integer) x)))
    (* x x)))

(setq x 7)
(cl-check-type 7 integer)
(tsqr 3)
(tsqr "3")

(defun lambda-list-keyword-p (symbol)
  "Check if SYMBOL is a lambda list keyword."
  (member symbol '(&rest &optional &key &allow-other-keys)))

(type-check-for-arg (integer x))
(type-check-for-arg x)

(defmacro defunt (name arglist &rest rest)
  (let (new-arglist type-checks)
    (dolist (arg arglist)
      (prn "defunt: arg is %S." arg)
      (let ((ty (car-safe arg)))
        (prn "defunt: ty is %S." ty)
        (if (and ty (symbolp ty))
          (progn
            (prn "defunt: It's a non-nil symbol.")
            `(cl-check-type ,(cadr arg) ,ty))
          (prn "defunt: It's NOT a non-nil symbol."))))
    (let ( (new-arglist (nreverse new-arglist))
           (type-checks (nreverse type-checks)))
      `(list
         ,new-arglist  
         ,type-checks))))

(defmacro type-check-for-arg (arg)
  "Check the type of a single typed arg."
  (prn "tcfa: arg is %S." arg)
  (let ((ty (car-safe arg)))
    (prn "tcfa: ty is %S." ty)
    (if-let ( (__ (and ty (symbolp ty)))
              (var (cadr arg)))
      `(cl-check-type ,var ,ty))))

(defmacro defunt (name arglist &rest rest)
  (let (new-arglist type-checks)
    (dolist (arg arglist)
      (prn "defunt: arg is %S." arg)
      (let ((ty (car-safe arg)))
        (prn "defunt: ty is %S." ty)
        (if (and ty (symbolp ty))
          (progn
            (prn "defunt: ty is a non-nil symbol.")
            (push `(cl-check-type ,(cadr arg) ,ty) type-checks))
          (prn "defunt: ty is NOT a non-nil symbol."))))
    
    (let ( (new-arglist (nreverse new-arglist))
           (type-checks (nreverse type-checks)))
      `(list
         ,new-arglist  
         ,type-checks))))

(defunt foo ((integer x) y))

(defunt foo (x y))

