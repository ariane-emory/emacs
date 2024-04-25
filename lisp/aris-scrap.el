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

(a:defclass barclass (num &delegee (parent fooclass)) ()
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

(with-messages "doing the thing" "it is done"
  (prn "pow")
  (with-indentation
    (prn "zowie"))
  (prn "kapow"))

(with-messages "doing the other thing" 
  (prn "Pow and %s, %s." "kablam" 3)
  (with-indentation
    (prn "zowie"))
  (prn "kapow"))
(pprn "nice")
(pprn "nice!")
(pprn "nice... %s!" 2)
