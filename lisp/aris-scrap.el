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
;; (extract-delegee '(password &delegee acct &optional thing))
;;   ⇒ '((password acct &optional thing) (nil . acct)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (extract-delegee '(password &delegee acct &optional foo))


(defun extract-delegee (arglist)
  "Extract the delegee argument from an arglist ARGLIST, returning a list whose first
element is the modified arglist and whose second element is the delegee argument.

This adds a new lambda list keyword, &delegee. When used, the &delegee keyword
follow all required parameters and precede any &optional and &rest parameters.
The &delegee keyword must be followed by a specifier for the delegee, which may
be either a symbol (meaning the delegee is bound to that symbol and may be of any class)
or a list of length two whose first element is the required class of the delegee and whose
second element is the symbol to bind the delegee to.

Examples of use:
(extract-delegee '(password &delegee (account acct)))
  ⇒ '((password acct) (account . acct))
(extract-delegee '(password &delegee acct))
  ⇒ '((password acct) (nil . acct)
(extract-delegee '(password &delegee (account acct) &rest things))
  ⇒ '((password acct &rest things) (account . acct))
(extract-delegee '(password &delegee acct &optional thing))
  ⇒ '((password acct &optional thing) (nil . acct)

Examples of mis-use:
(extract-delegee '(password &delegee) ;; malformed ARGLIST, nothing after &delegee.
(extract-delegee '(password &delegee &optional foo)) ;; malformed ARGLIST, &delegee immediately followed by &optional.
(extract-delegee '(password &optional thing &delegee acct)) ;; malformed ARGLIST, &optional preceded belegee."
  (if (not (memq '&delegee arglist))
    arglist
    (let (new-arglist delegee)
      (while-let ( (top (first arglist))
                   (_ (not (eq '&delegee top))))
        (prn "top:         %s" top)
        (push (pop arglist) new-arglist))
      (pop arglist)
      (prn "new-arglist: %s" new-arglist)
      (prn "arglist:     %s" arglist)
      (unless arglist
        (error "Malformed ARGLIST, nothing after &delegee."))
      (let ((top (pop arglist)))
        (when (memq top '(&optional &rest))
          (error "Malformed ARGLIST, &delegee immediately followed by %s." top))
        (unless (or
                  (symbol? top)
                  (and (double? top) (symbol? (first top)) (symbol? (second top))))
          (error
            (concat "Malformed ARGLIST, &delegee must be followed by a "
              "symbol or a list of length two.")))
        (setq delegee
          (if (symbol? top)
            (cons nil top)
            (cons (first top) (second top)))))

      (prn "delegee:      %s" delegee)
      (while arglist
        (push (pop arglist) new-arglist))
      (setq new-arglist (append (nreverse new-arglist) (list (cdr delegee))))
      (list new-arglist delegee)
      )))




(confirm that (extract-delegee '(password &delegee (account acct)))
  returns ((password acct) (account . acct)))
(confirm that (extract-delegee '(password &delegee acct))
  returns ((password acct) (nil . acct)))
(confirm that (extract-delegee '(password &delegee (account acct) &rest things))
  returns '((password acct &rest things) (account . acct)))
(confirm that (extract-delegee '(password &delegee acct &optional thing))
   returns '((password acct &optional thing) (nil . acct))
