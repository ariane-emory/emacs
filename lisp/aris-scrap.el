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
  (if-let
    ((x (maybe 'integer x)))
    (progn (* x x))
    (error "type error")))

(tsqr 3)
(tsqr "3")

(defun tsqr (x)
  "Square an integer."
  (let
    ((x (if (cl-typep x 'integer)
          x 
          (signal 'wrong-type-argument (list 'integerp x)))))
    (* x x)))

(defmacro expand-one-typed-binding (binding)
  "Expand a single typed binding."
  (if (not (and (consp binding) (length= binding 2)))
    binding 
    (let* ( (typ (car binding))
            (var (cadr binding))
            (prd  (get typ 'cl-deftype-satisfies))
            )
      `(,typ ,var ,prd))))

(expand-one-typed-binding ('integer x))


(expand-one-typed-binding x)



