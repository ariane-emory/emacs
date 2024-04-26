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
(defmacro switch (value &rest body)
  "Steele's `switch' from 'The Evolution of Lisp'."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ( (newbody (cl-mapcar #'(lambda (clause)
                                  `(,(gensym) ,@(rest clause)))
                     body))
          (switcher (cl-mapcar #'(lambda (clause newclause)
                                   `(,(first clause) (go ,(first newclause))))
                      body newbody)))
    `(cl-block switch
       (cl-tagbody (cl-case ,value ,@switcher)
         (break)
         ,@(apply #'nconc newbody)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro break () '(cl-return-from switch))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let (res)
    (switch 0
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns ("none"))
(confirm that
  (let (res)
    (switch 1
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns ("one " "too " "many"))
(confirm that
  (let (res)
    (switch 2
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns ("too " "many"))
(confirm that
  (let (res)
    (switch 3
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns ("many"))
(confirm that
  (let (res)
    (switch 4
      (0 (push "none" res) (break))
      (1 (push "one " res))
      (2 (push "too " res))
      (3 (push "many" res)))
    (nreverse res))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let (res)  
  (cl-block switch
    (cl-tagbody
      (cl-case 1
        (0 (go g1471))
        (1 (go g1472))
        (2 (go g1473))
        (3 (go g1474)))
      (break)
      g1471
      (push "none" res)
      (break)
      g1472
      (push "one " res)
      g1473
      (push "too " res)
      g1474
      (push "many" res)))
  (nreverse res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
