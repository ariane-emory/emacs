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
  `(when
     (cl-typep ,val
       ,(if (and (symbolp type) (get type 'cl-deftype-satisfies)) `',type type))
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
;; define a function with type checks using the macro:
(defun* foo ((num : number) (exp : integer) &optional print-message)
  "A silly function to raise the number NUM to the integral power EXP.

This is marked as interactive for no good reason other than to test if
INTERACTIVE-FORM is handled properly when defun* builds NEW-BODY and is
marked pure mainly to test if DECLARE-FORM is handled properly."
  (declare (pure t))
  (interactive)
  (let ((res (expt num exp)))
    (when print-message (message "%s to the power of %d is %s." num exp res))
    res))

;; try it out;
(foo 2.5 3 t) ;; ⇒ 15.625 and also prints "2.5 to the power of 3 is 15.625.".
(foo 2.5 3.5 t) ;; signals (wrong-type-argument integer 3.5 pow).
(if-let ((res (maybe integer (foo 4 3 t))))
  (message "Result %S is an integer." res)
  (message "Result was not an integer.")) ;; prints "Result 64 is an integer."


(defun* foo ((num : number) (exp : integer)) (expt num exp))
