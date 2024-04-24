;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(a:defclass barclass (num &delegee (fooclass parent)) ()
  (bar () (format "BAR! %d" num))
  (method-not-found (&rest args)
    (apply message parent args)))

(a:defclass bazclass (num &delegee (barclass parent)) ()
  (baz () (format "BAZ! %d" num))
  (method-not-found (&rest args)
    (apply message parent args)))

(foo (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "FOO! 2"
(bar (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAR! 3"
(baz (bazclass 5 (barclass 3 (fooclass 2)))) ;; => "BAZ! 5"

(a:get-method (fooclass 2) 'method-not-found)
(a:get-method (barclass 3 (fooclass 2)) 'method-not-fjkj)

'(closure
   ( (message . method-not-found)
     (parent closure
       ((num . 2) (method-names class-name dir foo is? responds-to?) (class-name . fooclass))
       (message)
       (progn '(declare (aos-class 'fooclass)) nil)
       (cond
         ((eql message 'class-name) #'(lambda nil class-name))
         ((eql message 'dir) #'(lambda nil method-names))
         ((eql message 'is?) #'(lambda (class) (eq class class-name)))
         ((eql message 'responds-to?) #'(lambda (method) (not (null (memq method method-names)))))
         ((eql message 'foo) #'(lambda nil (format "FOO! %d" num)))))
     )
   (&rest args)
   (apply message parent args))
