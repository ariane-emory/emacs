;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-types) ; used in `confirm' tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun some* (type val)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  "Return VAL when it is of type TYPE, otherwise return nil. (Conservative version)"
  (cond
    ((functionp type) (and (funcall type val)  val))
    (t (and (cl-typep val type) val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((foo 7) (ty 'integer) (tyfun (lambda (x) (integerp x))))
  (confirm that (some* 'integer foo) returns 7)
  (confirm that (some* #'integerp foo) returns 7)
  (confirm that (some* tyfun foo) returns 7)
  (confirm that (some* ty foo) returns 7)
  (confirm that (some* 'integer "nope") returns nil)
  (confirm that (some* ty "nope") returns nil)
  (confirm that (some* '(pair-of integer) '(3 . 4)) returns (3 . 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
