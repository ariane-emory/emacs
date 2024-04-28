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
  ;; should probably go in a parent class:
  (each (f)
    (while-let ((val (next self)))
      (funcall f val))))

(a:definterface generator (next each))

(let ((gen (make-up-to-gen 1 10 2)))
  (while-let ((n (next gen)))
    (prn n)))

(let ((g (make-up-to-gen 1 10 2)))
  (if (a:implements? g 'generator)
    (each g #'prn)
    (prn "Not a generator")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(a:defclass barclass (num &parent (parent fooclass fakeclass)) ()
  (bar () (format "BAR! %d" num)))

(a:defclass bazclass (num &parent (parent fooclass barclass) &rest things) ()
  (baz () (format "BAZ! %d" num)))

(setq foo  (make-fooclass 8))
(setq bar  (make-barclass 4 foo))
(setq baz  (make-bazclass 7 foo))
(setq baz2 (make-bazclass 6 bar))
;; (setq bad-bar (barclass 9 baz)) ;; this shouldn't work.

(foo (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "FOO! 2"
(bar (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "BAR! 3"
(baz (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "BAZ! 5"

(field-names basic-acct)
(field-names passwd-acct)

(setq basic-acct (make-basic-account "A. User" 2000.00))
(field-values basic-acct) ;; => ("A. User" 2000.0)

(a:is-object? (alist-get 'acct (field-values passwd-acct)))

(repr basic-acct)
(repr passwd-acct)
(repr limit-acct)

(prepr limit-acct)
(strepr limit-acct)

(field-values basic-acct)
(field-values passwd-acct)

(change-password passwd-acct "this" "that")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(a:definterface generator (next each))
(setf (get 'generator 'aos-interface) '(next each))

(symbol-plist 'generator)

(a:implements? 'generator (make-up-to-gen 1 10 2))
(a:implements? 'generator foo)

(symbol-plist 'foo)
