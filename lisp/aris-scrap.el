;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(aos:defclass barclass (num &parent (parent fooclass fakeclass)) ()
  (bar () (format "BAR! %d" num)))

(aos:defclass bazclass (num &parent (parent fooclass barclass) &rest things) ()
  (baz () (format "BAZ! %d" num)))

(setq foo  (make-fooclass 8))
(setq bar  (make-barclass 4 foo))
(setq baz  (make-bazclass 7 foo))
(setq baz2 (make-bazclass 6 bar))
;; (setq bad-bar (barclass 9 baz)) ;; this shouldn't work.

(foo (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "FOO! 2"
(bar (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "BAR! 3"
(baz (make-bazclass 5 (make-barclass 3 (make-fooclass 2)))) ;; => "BAZ! 5"

(class-names foo)
(class-names bar)
(class-names baz) ;; (bazclass fooclass) WHY ISN'T this (bazclass barclass fooclass)?

(field-names basic-acct)
(field-names passwd-acct)

(setq basic-acct (make-basic-account "A. User" 2000.00))
(field-values basic-acct) ;; => ("A. User" 2000.0)

(aos:is-object? (alist-get 'acct (field-values passwd-acct)))

(repr basic-acct)
(repr passwd-acct)
(repr limit-acct)

(prepr limit-acct)
(strepr limit-acct)

(field-values basic-acct)
(field-values passwd-acct)

(change-password passwd-acct "this" "that")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:definterface generator
  ((next (0 . 0))
    (each (1 . 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:defclass up-to-gen (from &optional to step) ()
  (next () (catch 'stop
             (let ((next (+ from step)))
               (if (< next to)
                 (setq from next)
                 (throw 'stop nil)))))
  ;; should probably go in a parent class:
  (each (f &optional y)
    (let ((y (or y 0)))
      (prn "Y: %s" y)
      (while-let ((val (next self)))
        (funcall f (+ y val))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((g (make-up-to-gen 1 10 2)))
  (if (implements? g 'generator)
    (each g #'prn 10)
    (prn "Not a generator")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let ((gen (make-up-to-gen 1 10 2)))
;;   (while-let ((n (next gen)))
;;     (prn n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(symbol-plist 'generator)

(implements? (make-up-to-gen 1 10 2) 'generator)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
