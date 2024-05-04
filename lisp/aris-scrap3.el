;; -*- lexical-binding: t; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-putnew(key new-val alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Put NEW-VAL into ALIST as the association of KEY , throwing
'no-match an association for KEY is already present in ALIST with a
different (by `equal') value."
  (when-let ( (assoc (assoc key alist))
              (neq   (not (equal (cdr assoc) new-val))))
    (throw 'no-match nil))
  (cl-pushnew (cons key new-val) alist :test #'equal))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a . 1) (b . 2)))) (alist-putnew 'c 3 alist)) ;; new key/val.
(let ((alist '((a . 1) (b . 2)))) (alist-putnew 'b 2 alist)) ;; existing key/value.
;; duplicate key, un-equal value:
(let ((alist '((a . 1) (b . 2)))) (alist-putnew 'b 3 alist)) 



(defun foo-throws-bar-p ()
  (cl-block 'blk
    (catch 'bar
      (foo)
      (cl-return-from 'blk nil))
    (cl-return-from 'blk t)))

(defun foo () (throw 'bar 123))
(foo-throws-bar-p) ;; t

(defun foo () (throw 'bar nil))
(foo-throws-bar-p) ;; t

(defun foo () 123)
(foo-throws-bar-p) ;; nil 
