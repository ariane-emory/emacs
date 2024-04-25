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

(defun pp-things (&rest things)
  (let (pieces)
    (dolist (thing things)
      (push (format "%s" thing) pieces))
    (setq pieces (nconc (intercalate ", " pieces) (list ".")))
    (apply #'concat pieces)))


(cl-defun pp-things (&rest things &key con)
  "Con should be either 'and or 'or."
  (let (pieces)
    (dolist (thing things)
      (push (format "%s" thing) pieces))
    (setq pieces (nconc (intercalate ", " pieces) (list ".")))
    (apply #'concat pieces)))


;;(cl-defun pp-things (&rest things &key (con nil) &allow-other-keys)
(cl-defun pp-things (&rest things &key (con nil) &allow-other-keys)
  "Con should be either 'and or 'or."
  (unless (or (null con) (memq con '(and or)))
    (error "Invalid value for :con argument. Should be 'and or 'or."))
  (let (pieces)
    (dolist (thing things)
      (push (format "%s" thing) pieces))
    (setq pieces (nreverse (intercalate ", " pieces)))
    (prn "things: %s" things)
    (prn "con: %s" con)
    (if con
      (setq pieces (nconc pieces))
      (setq pieces (nconc pieces (cdr con))))
    (setq pieces (nconc pieces (list ".")))
    (apply #'concat pieces)))





(cl-defun pp-things (&rest things)
  "Con should be either 'and or 'or."
  (let (connective (connectives '((:and . and) (:or . or))))
    (when-let ((entry (assoc (car things) connectives)))
      (setq connective (format " %s " (cdr entry)))
      (setq things (cdr things)))
    (setq things (intercalate ", " things))
    (when (and connective (length> things 1))
      (let ((last (car (last things))))
        (setq things (nconc (butlast things 2) (list connective last))))))
  (apply #'concat (mapcar (lambda (x) (format "%s" x)) (nconc things (list ".")))))

(pp-things :and 1 "foo" 'bar) ;; "1, foo and bar."
(pp-things :or 1 "foo" 'bar) ;; "1, foo or bar."
(pp-things 1 "foo" 'bar) ;; "1, foo, bar."

(pp-things 'and 1 "foo") ;; "and, 1, foo."
(pp-things 'or 1 "foo") ;; "or, 1, foo."
(pp-things 1 "foo") ;; "1, foo."

(pp-things 'and 1) ;; "and, 1."
(pp-things 1) ;; "1."
(pp-things 'or 1) ;; "or, 1."

