;; -*- lexical-binding: t; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun expr-throws-sym-p (throw-sym expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when evaluating expr throws THROW-SYM."
  (with-gensyms (blk)
    (cl-block blk
      (catch throw-sym
        (eval expr)
        (cl-return-from blk nil))
      (cl-return-from blk t))))
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
;; new key/val:
(confirm that (let ((alist '((a . 1) (b . 2)))) (alist-putnew 'c 3 alist))
  returns ((c . 3) (a . 1) (b . 2)))
(confirm that (expr-throws-sym-p 'no-match '(let ((alist '((a . 1) (b . 2)))) (alist-putnew 'c 3 alist)))
  returns nil)
;; existing key, equal value:
(confirm that (let ((alist '((a . 1) (b . 2)))) (alist-putnew 'b 2 alist))
  returns ((a . 1) (b . 2)))
(confirm that (expr-throws-sym-p 'no-match '(let ((alist '((a . 1) (b . 2)))) (alist-putnew 'b 2 alist)))
  returns nil)
;; duplicate key, un-equal value:
(confirm that (expr-throws-sym-p 'no-match '(let ((alist '((a . 1) (b . 2)))) (alist-putnew 'b 3 alist)))
  returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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
