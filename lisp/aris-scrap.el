;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(a:defclass barclass (num &delegee (parent fooclass)) ()
  (bar () (format "BAR! %d" num)))

(a:defclass bazclass (num &delegee (parent barclass)) ()
  (baz () (format "BAZ! %d" num)))

(a:defclass bazclass (num &delegee (parent barclass) &rest things) ()
  (baz () (format "BAZ! %d" num)))


(foo (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "FOO! 2"
(bar (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAR! 3"
(baz (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAZ! 5"

(field-names acct)
(field-names passwd-acct)

;; (format acct)


(setq acct (account "A. User" 2000.00))
(field-values acct) ;; => ("A. User" 2000.0)

(fmt acct)
(fmt passwd-acct)


