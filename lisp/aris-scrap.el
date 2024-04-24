;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (a:generic-fun-p 'dir)
;; (a:generic-fun-p 'get-self)
;; (a:get-method acct 'method-not-found)
;; ;; (get-self acct)
;; (dir limit-acct)
;; (a:send-message limit-acct 'method-not-found "pass" 2)
;; (a:get-method limit-acct 'method-not-found)

(a:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(a:defclass barclass (num parent) ()
  (bar () (format "BAR! %d" num))
  (method-not-found (&rest args)
    (apply message parent args)))

(a:defclass bazclass (num parent) ()
  (baz () (format "BAZ! %d" num))
  (method-not-found (&rest args)
    (apply message parent args)))

(foo (barclass 3 (fooclass 2)))
(method-not-found (barclass 3 (fooclass 2)))
