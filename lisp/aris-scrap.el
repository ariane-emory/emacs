;; -*- lexical-binding: nil; fill-column: 120; lisp-indent-offset: 2; eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'aris-funs--alists)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--lists)
(require 'aris-funs--match-pattern)
(require 'aris-funs--match-pattern2)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--stacks)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; naming examples for an imaginary frobnosticate-widget package:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fw--frobnosticate-widget     ;; a public-facing function in the frobnosticate-widget package.
;; --fw-frobnostication-helper  ;; an internal function in the frobnosticate-widget package.
;; --do-fw-stuff                ;; an internal function in the frobnosticate-widget package.
;; *fw--frobnostication-level*  ;; a public-facing (usually customizable) variable in the frobnosticate-widget package.
;; *--fw-frobnostication-count* ;; an internal variable in the frobnosticate-widget package not meant for customization
;; frobnosticate-widget         ;; either a public-facing function in the frobnosticate-widget package or a convenient
;;                              ;;  alias for fw--frobnosticate-widget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((foo 7))
  (if-let ((x (maybe 'integer foo)))
    (prn "yes: %S" x)
    (prn "no!")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *pipe--verbose* t)
(|> ((e)) 5 (* e e) (+ e 8) double) ;; => 66
(|> ((e 5)) (* e e) (+ e 8) double) ;; => 66
(|> 5 (* _ _) (+ _ 8) double) ;; => 66

(|> ((e 5)) (* e e) :return 9 (+ e 8) double) ;; => 9
(|> ((e 5)) (* e e) (return! 9) (+ e 8) double) ;; => 9

(|> 5 :when odd? 100)
(|> 6 :when odd? 100)
(|> 5 :unless odd? 100)
(|> 6 :unless odd? 100)
(|> 5 :when odd? 101 :unless odd? 200)
(|> 6 :when odd? 101 :unless odd? 200)

;; breaking cases, genuinely malformed:
(|> 1 :unless t)
(|> 1 :return)

;; not detected:
(|> 1 :unless t :return) 
(|> 1 :unless t :when nil 3)
(|> 1 :unless :unless t 2 3)

;; surprisingly this works:
(|> ((e 9)) (* e e) :when even? :when (> e 50) :return 9 (+ e 8) double)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a function with type checks using the defun* macro:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo ((num : number) (exp : integer) &optional print-message)
  "A silly function to raise the number NUM to the integral power EXP.

This is marked as interactive for no good reason other than to test if
INTERACTIVE-FORM is handled properly when defun* builds NEW-BODY and is
marked pure mainly to test if DECLARE-FORM is handled properly."
  (declare (pure t))
  (interactive)
  (let ((res (expt num exp)))
    (when print-message (message "%s to the power of %d is %s." num exp res))
    res))

;; try it out;
(foo 2.5 3 t) ;; ⇒ 15.625 and also prints "2.5 to the power of 3 is 15.625.".
(foo 2.5 3.5 t) ;; signals (wrong-type-argument integer 3.5 pow).
(if-let ((res (maybe integer (foo 4 3 t))))
  (message "Result %S is an integer." res)
  (message "Result was not an integer.")) ;; prints "Result 64 is an integer."

(defun* pow ((num : number) (exp : integer))
  (expt num exp))

(dolist (num '(3 3.5 "foo"))
  (if-let ((res (maybe 'integer (pow num 3))))
    (message "%d^3 is the integer %d." num res)
    (message "%s^3 is not an integer." num)))

;; prints:
;; 3^3 is the integer 27.
;; 3.5^3 is not an integer.
;; and then signals (wrong-type-argument number "foo" num).

;; imaginary &rest/optional syntax:
(defun* pow ((num : number) (exp : integer) (nums : &rest integer))
  (expt num exp))
(defun* pow ((num : number) (exp : integer) (&rest nums : integer))
  (expt num exp))
(defun* pow ((num : number) (exp : &optional integer))
  (expt num (or exp 2)))
(defun* pow ((num : number) (&optional exp : integer))
  (expt num (or exp 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun grok/pcase (obj)
  (pcase obj
    ((or
       (and
         (pred stringp)
         (pred (string-match "^key:\\([[:digit:]]+\\)$"))
         (app (match-string 1) val))
       (let val (list "149" 'default)))
      val)))

(`(,key . ,val))

(cl-deftype list-of-length (n)
  "Type specifier for lists of length N."
  `(and list (satisfies (lambda (lst) (= (length lst) ,n)))))

(cl-typep '(1 2 3 4) '(list-of-length 4))

(let ((foo '(this 1 2 3 4))) 
  (pcase-let ((`(this ,foo ,bar ,baz) foo))
    (list foo bar baz))) ;; => (1 2 3)

(let ((foo '(this 1 2 3 4))) 
  (pcase-let ((`(this ,foo ,bar ,baz . _) foo))
    (list foo bar baz))) ;; => (1 2 3)

(let ((foo '(this 1 2 3 4))) 
  (pcase-let ((`(this ,foo ,bar ,baz . ,_) foo))
    (list foo bar baz))) ;; => (1 2 3)

(let ((target '(this 1 2 3 4))) 
  (pcase-let (((and `(this ,foo ,bar ,baz . ,stop) (guard (null stop))) target))
    (list foo bar baz stop))) ;; => (1 2 3 (4))

(let ((target '(this 1 2 3 4))) 
  (pcase-let (((and `(this ,foo ,bar ,baz . ,stop) (guard (null stop))) target))
    (list foo bar baz stop))) ;; => (1 2 3 nil)

(let ((target '(this 1 2 3 4))) 
  (pcase-let ((`(this ,foo ,bar ,baz . ,stop) target))
    (guard (null stop))
    (list foo bar baz stop)))

(let ((target '(this 1 2 3 4))) 
  (pcase-let ((`(this ,foo ,bar ,baz . ,stop) target))
    (guard (null stop))
    (list foo bar baz stop)))

(let ((target '(this 1 2 3 4))) 
  (pcase-let (((and `(this ,foo ,bar ,baz . ,stop) (guard (null stop))) target))
    (list foo bar baz stop))) ;; => (1 2 3 (4))

There must be something funny about pcase-let. It seems to be a bit more lenient than normal pcase.


Okay, so I'm trying to learn my way around `pcase` and its friends...

(pcase '(foo bar baz quux)
  (`(foo ,bar ,baz) (list bar baz))) ;; => nil, this makes sense, the scrutinee has more elements than the pattern

(pcase-let ((`(foo ,bar ,baz) '(foo bar baz quux)))
  (list bar baz)) ;; => (bar baz), wait, what, why didn't match fail?1

(pcase--macroexpand '(cl-type integer))
(pcase--macroexpand '(cl-type (integer 0 10)))

(defun* foo ((bar : (list-of-length 3)))
  bar)

(foo '(1 2 3))

(cl-defun foo ((a b &optional (c 9)) d)
  (list b c))

(setq baz 777)
(foo `(1 ,baz) 4)

(cl-defun foo ((a &rest as) (b &rest bs))
  (list as bs))

(cl-defun foo ((a . b) &rest (cs . ds))
  (list a b cs ds))

(foo '(1 . 2) '((4 . 5) (6 . 7) (8 . 9))) ;; => (1 2 ((4 . 5) (6 . 7) (8 . 9)) nil)

(foo '(1 . 2) '((4 . 5) (6 . 7) (8 . 9)) 'additional-arg1 'additional-arg2)

(--> 5 (+ 3 it))


'(-as-> 5 x (+ 3 x) (* 6 x) (neg x))

(macroexpand-all '(-as-> 5 x (+ 3 x) (* 6 x) (neg x)))
(let ((x 5))
  (let ((x (+ 3 x)))
    (let ((x (* 6 x)))
      (neg x))))

(|> ((it)) 5 (* it 3) (+ it 8) neg)

(--> 5 (* it 3) (+ it 8) neg (lambda (x) (* x 10)))

(-as-> 5 it
  (* it 3)
  (+ it 8)
  neg)

(defmacro -as-> (value variable &rest forms)
  "Starting with VALUE, thread VARIABLE through FORMS.

In the first form, bind VARIABLE to VALUE.  In the second form, bind
VARIABLE to the result of the first form, and so forth."
  (declare (debug (form symbolp body)))
  (if (null forms)
    `,value
    `(let ((,variable ,value) (foo 1))
       (-as-> ,(if (symbolp (car forms))
                 (list (car forms) variable)
                 (car forms))
         ,variable
         ,@(cdr forms)))))

(macroexpand-all '(-as-> 5 it ((lambda (x) (* x 10)) it)))

;; expands to:
(let ((it 5) (foo 1))
  (let ((x it)) (* x 10)))


(let ((it 5))
  (let ((x it))
    (* x 10)))

(macroexpand-all '(-as-> 5 it (* it 3) (+ it 8) neg ((lambda (x) (* x 10)) it)))

;;; Expands to:
(let ((it 5))
  (let ((it (* it 3)))
    (let ((it (+ it 8)))
      (let ((it (neg it)))
        (let ((x it))
          (* x 10)))))) ;; => -230

(pcase-let ((`(foo ,bar ,baz) '(foo bar baz quux)))
  (list bar baz))

(pcase-let ((`(foo ,bar ,baz) '(foop bar baz quux)))
  (list bar baz))

(pcase '(foo bar baz quux) (`(foo ,bar ,baz) (list bar baz)))

(pcase-when-let (`(foo ,bar ,baz ,quux) '(foo bar baz quux))
  (prn "message")
  (list bar baz))

(pcase-if-let
  (`(foo ,bar ,baz ,quux)
    '(foo bar baz quux))
  (list bar baz))





