;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n:defclass n:integer (value) nil
  (val ()      value)
  (fmt ()      (format "(n:integer %d)" value))
  (add (other) (n:integer (+    value (val other))))
  (sub (other) (n:integer (-    value (val other))))
  (mul (other) (n:integer (*    value (val other))))
  (div (other) (n:integer (/    value (val other))))
  (rem (other) (n:integer (%    value (val other))))
  (pow (other) (n:integer (expt value (val other)))))

(val (n:integer 666))
(dir (n:integer 666))

(responds-to? (n:integer 666) 'rem)
(responds-to? (n:integer 666) 'foo)

(is? (n:integer 666) 'n:integer)
(is? (n:integer 666) 'nope)

(setq q (n:integer 444))
(setq r (lambda (foo) :foo))

(n:is-object? q)
(n:is-object? r)
(n:is-object? nil)

(n:is? 2 'n:integer)
(n:is? q 'n:integer)
(n:is? r 'n:integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extracting delegee arg:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (extract-delegee '(password &delegee (account acct)))
;;   ⇒ '((password acct) (account . acct))
;; (extract-delegee '(password &delegee acct))
;;   ⇒ '((password acct) (nil . acct)
;; (extract-delegee '(password &delegee (account acct) &rest things))
;;   ⇒ '((password acct &rest things) (account . acct))
;; (extract-delegee '(password &delegee acct &rest things))
;;   ⇒ '((password acct &rest things) (nil . acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (extract-delegee '(password &delegee acct))
