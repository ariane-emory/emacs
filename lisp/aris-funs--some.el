;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `some' macro:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-types) ; used in `confirm' tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun some (type val)
;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;   "Return VAL when it is of type TYPE, otherwise return nil. (Conservative version)"
;;   (and (cl-typep val type) val))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro some (type val)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return VAL when it is of type TYPE, otherwise return nil. (Conservative version)"
  `(and (cl-typep ,val ,type) ,val))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ( (foo 7) (ty 'integer))
  (confirm that (some '(satisfies integerp) foo) returns 7)
  (confirm that (some 'integer foo) returns 7)
  (confirm that (some ty foo) returns 7)
  (confirm that (some 'integer "nope") returns nil)
  (confirm that (some ty "nope") returns nil)
  (confirm that (some '(pair-of integer) '(3 . 4)) returns (3 . 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun some* (type val)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  "Return VAL when it is of type TYPE, otherwise return nil. (Buggy version)"
  (cond
    ((and (symbolp type) (get type 'cl-deftype-satisfies))
      ;; (debug nil 1 type)
      (and (cl-typep val type) val))
    ((functionp type)
      ;; (debug nil 2 type)
      (and (funcall type val) val))
    (t
      ;; (debug nil 3 type)
      (and (cl-typep val type) val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((foo 7) (ty 'integer) (tyfun (lambda (x) (integerp x))))
  (confirm that (some* 'integer foo) returns 7)
  (confirm that (some* #'integerp foo) returns 7)
  (confirm that (some* tyfun foo) returns 7)
  (confirm that (some* ty foo) returns 7)
  (confirm that (some* 'integer "nope") returns nil)
  (confirm that (some* ty "nope") returns nil)
  (confirm that (some* '(pair-of integer) '(3 . 4)) returns (3 . 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--some)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
