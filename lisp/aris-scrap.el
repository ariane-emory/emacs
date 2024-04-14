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
;; STACKS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(doforthy (x '(1 2 :swap 3 4 5 6 :drop 7 8 9))
  (cond
    ((eql? 3 x) (push-out! (* x 100)))
    ((odd? x)   (push-out! x))))

(|> ((e)) 5 (+ e 7) double (+ e 3) neg (lambda (n) (* 3 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((ctr 0))
  (doforthy (x '(1 2 3 4 5 6 7 8 9))
    (cl-incf ctr)
    (prndiv)
    (prn "ctr:     %S" ctr)
    (prn "current: %S" x)
    (prn "ahead:   %S" (stack))
    (when (eql? ctr 50) (stop!)) ;;(return! 111))
    (cond
      ((even? x)  (push-out! x)))
    (prn "after:   %S" (stack))
    (unless (stack) (stop!))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maybe macro:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defmacro maybe (type val)
  "Return VAL when it is of type TYPE, otherwise return nil."
  `(when (cl-typep ,val ,type)
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
(dostack (expr '(1 2 3 4 5 6 7 8 9 10))
  (when (oddp expr)
    (pop!))
  (message "expr: %S" expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(|> ((e)) 5 (+ e 7) double (+ e 3) neg (lambda (n) (* 3 n)))


