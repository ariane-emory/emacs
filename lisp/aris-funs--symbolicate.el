;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--confirm)
(require 'aris-funs--lists) ; for `compact'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun symbolicate- (&rest things)
  "A simplified substitute for Alexandria's `symbolicate' function: stringify the THINGS, join them with \"-\" and
intern them to make a symbol, ignoring nils in THINGS."
  (intern (string-join (mapcar (lambda (x) (format "%s" x)) (compact things)) "-")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (symbolicate- "foo") returns foo)
(confirm that (symbolicate- "foo" 'bar 1) returns foo-bar-1)
(confirm that (symbolicate- "foo" 'bar 3.14) returns foo-bar-3.14)
(confirm that (symbolicate- "foo" 'bar "baz quux") returns foo-bar-baz\ quux)
(confirm that (symbolicate- "foo" 'bar nil 1) returns foo-bar-1)
(confirm that (symbolicate- "asd" "" 1) returns asd--1)
(confirm that (symbolicate- "asd" nil 1) returns asd-1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun symbolicate (&rest things)
  "A simplified substitute for Alexandria's `symbolicate' function: stringify the THINGS, join them with \"-\" and
intern them to make a symbol, ignoring nils in THINGS."
  (intern (apply #'concat (mapcar (lambda (x) (format "%s" x)) (compact things)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (symbolicate "foo") returns foo)
(confirm that (symbolicate "foo" 'bar 1) returns foobar1)
(confirm that (symbolicate "foo" 'bar 3.14) returns foobar3.14)
(confirm that (symbolicate "foo" 'bar "baz quux") returns foobarbaz\ quux)
(confirm that (symbolicate "foo" 'bar nil 1) returns foobar1)
(confirm that (symbolicate "asd" "" 1) returns asd1)
(confirm that (symbolicate "asd" nil 1) returns asd1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gensymbolicate- (&rest things)
  "`symbolicate' THINGS and use the result as the argument to `gensym'.
Example:
(gensymbolicate \"foo\" 'bar 1) ⇒ foo-bar-11533"
  (gensym (apply #'symbolicate `(,@things ""))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gensymbolicate (&rest things)
  "`symbolicate' THINGS and use the result as the argument to `gensym'.
Example:
(gensymbolicate \"foo\" 'bar 1) ⇒ foo-bar-11533"
  (gensym (apply #'symbolicate `(,@things ""))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--symbolicate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
