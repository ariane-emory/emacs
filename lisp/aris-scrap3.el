;; -*- lexical-binding: t; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun expr-throws-sym-p (thrown-sym expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when evaluating expr throws THROWN-SYM."
  (with-gensyms (blk)
    (cl-block blk
      (catch thrown-sym
        (eval expr)
        (cl-return-from blk nil))
      (cl-return-from blk t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun alist-putunique(key new-val alist &optional (throw-sym 'unequal-duplicate))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Put NEW-VAL into ALIST as the association of KEY, throwing
THROWSYM if an association for KEY is already present in ALIST with a
different (by `equal') value."
  (let ((assoc (assoc key alist)))
    (cond
      ((and assoc (equal (cdr assoc) new-val)) alist) ;; just return alist.
      (assoc (throw throw-sym nil))   
      (t (cons (cons key new-val) alist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new key/val:
(confirm that
  (let ((alist '((a . 1) (b . 2)))) (alist-putunique 'c 3 alist))
  returns ((c . 3) (a . 1) (b . 2)))
(confirm that
  (expr-throws-sym-p 'unequal-duplicate
    '(let ((alist '((a . 1) (b . 2)))) (alist-putunique 'c 3 alist)))
  returns nil)
;; existing key, equal value:
(confirm that
  (let ((alist '((a . 1) (b . 2)))) (alist-putunique 'b 2 alist))
  returns ((a . 1) (b . 2)))
(confirm that
  (expr-throws-sym-p 'unequal-duplicate '
    (let ((alist '((a . 1) (b . 2)))) (alist-putunique 'b 2 alist)))
  returns nil)
;; duplicate key, un-equal value. this one SHOULD throw, so we don't `confirm' it's return value,
;; we just `confirm' if it threw:
(confirm that
  (expr-throws-sym-p 'unequal-duplicate
    '(let ((alist '((a . 1) (b . 2)))) (alist-putunique 'b 3 alist)))
  returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
