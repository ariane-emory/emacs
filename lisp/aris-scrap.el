;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ignore!
  (prn "VAL:          %S" val)
  (prn "a:is-object?: %S" (a:is-object? val))
  (push (format "%s:%s %s" key padding
          (if (a:is-object? val)
            "foo" ; (fmt val)
            val))
    lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(a:defclass barclass (num &delegee (parent fooclass fakeclass)) ()
  (bar () (format "BAR! %d" num)))

(a:defclass bazclass (num &delegee (parent fooclass barclass) &rest things) ()
  (baz () (format "BAZ! %d" num)))

(setq foo  (fooclass 8))
(setq bar  (barclass 4 foo))
(setq baz  (bazclass 7 foo))
(setq baz2 (bazclass 6 bar))
;; (setq bad-bar (barclass 9 baz)) ;; this shouldn't work...

(foo (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "FOO! 2"
(bar (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAR! 3"
(baz (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAZ! 5"

(field-names acct)
(field-names passwd-acct)

;; (format acct)

(setq acct (account "A. User" 2000.00))
(field-values acct) ;; => ("A. User" 2000.0)

(a:is-object? (alist-get 'acct (field-values passwd-acct)))

;; (fmt passwd-acct)
;; (fmt-as-lines acct)
;; (fmt acct)
;; (fmt passwd-acct)
;; (fmt-as-lines passwd-acct)

;; (describe acct)
;; (describe limit-acct)
;; (describe passwd-acct)

(repr acct)
(repr passwd-acct)
(repr limit-acct)

(prepr limit-acct)
(strepr limit-acct)

(field-values acct)
(field-values passwd-acct)

(change-password passwd-acct "this" "that")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(a:defclass up-to-gen (from &optional to step) ()
  (next () (catch 'stop
             (let ((next (+ from step)))
               (if (< next to)
                 (setq from next)
                 (throw 'stop nil)))))
  (each (f)
    (while-let ((val (next self)))
      (funcall f val))))

(defun a:generator? (val)
  (and (a:is-object? val)
    (responds-to? val 'next)
    (responds-to? val 'each)))
  
(let ((gen (up-to-gen 1 10 2)))
  (while-let ((n (next gen)))
    (prn n)))

(let ((g (up-to-gen 1 10 2)))
  (if (a:generator? g)
    (each g #'prn)
    (prn "Not a generator")))

(each g #'prn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unrepeat* (lst)
  "Recursively remove sequentially repeated items from LST."
  (let (res last)
    (while lst
      (let ((popped (pop lst)))
        (when (consp popped)
          (setq popped (unrepeat popped)))
        (when (not (equal popped last))
          (push popped res)
          (setq last popped))))
    (reverse res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (unrepeat* '(a a b a b b c a a (b b c) (b b c) nil nil (b b (c d d e))))
  returns (a b a b c a (b c) nil (b (c d e))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unrepeat (lst)
  "Remove sequentially repeated items from LST."
  (let (res last)
    (while lst
      (let ((popped (pop lst)))
        (when (not (equal popped last))
          (push popped res)
          (setq last popped))))
    (reverse res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (unrepeat '(a a b a b b c a a (b b c) (b b c) nil nil (b b (c d d e))))
  returns (a b a b c a (b b c) nil (b b (c d d e))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

