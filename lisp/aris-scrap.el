;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(a:defclass fooclass (num) ()
  (foo () (format "FOO! %d" num)))

(let ( (class-name 'fooclass)
       (method-names '(class-name dir foo is? nil responds-to?)))
  (mapc #'a:ensure-generic-fun method-names)
  (cl-defun fooclass (num)
    (let (self)
      (setq self
        #'(lambda (message)
            (declare (aos-class 'fooclass))
            (cl-case message
              (class-name #'(lambda nil class-name))
              (dir #'(lambda nil method-names))
              (is? #'(lambda (class) (eq class class-name)))
              (responds-to? #'(lambda (method) (not (null (memq method method-names)))))
              (foo #'(lambda nil (format "FOO! %d" num)))
              (nil #'(lambda))))))))



(a:defclass barclass (num &delegee (fooclass parent)) ()
  (bar () (format "BAR! %d" num))
  (method-not-found (&rest args)
    (apply message parent args)))


(a:defclass bazclass (num &delegee (barclass parent)) ()
  (baz () (format "BAZ! %d" num)))




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
