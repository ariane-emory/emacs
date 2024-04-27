;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; (setq bad-bar (barclass 9 baz)) ;; this shouldn't work.

(foo (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "FOO! 2"
(bar (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAR! 3"
(baz (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAZ! 5"

(field-names acct)
(field-names passwd-acct)

(setq acct (account "A. User" 2000.00))
(field-values acct) ;; => ("A. User" 2000.0)

(a:is-object? (alist-get 'acct (field-values passwd-acct)))

(repr acct)
(repr passwd-acct)
(repr limit-acct)

(prepr limit-acct)
(strepr limit-acct)

(field-values acct)
(field-values passwd-acct)

(change-password passwd-acct "this" "that")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

