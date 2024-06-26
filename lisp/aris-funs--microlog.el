;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--aliases)
(require 'aris-funs--alists)
(require 'aris-funs--destructuring-match)
(require 'aris-funs--lists)
(require 'aris-funs--unification)
(require 'aris-funs--strings)
(require 'aris-funs--sym-db)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup microlog nil
  "Ari's miniature Prolog.")
;;-----------------------------------------------------------------------------------------
(defcustom *ml:verbose* t
  "Whether or not Microlog should print verbose messages."
  :group 'microlog
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::prn (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *ml:verbose* (apply #'prn args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ml::prn1 (fmt val &rest args) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  `(let ((val ,val))
     (if *ml:verbose* (prn1 ,fmt val ,@args) val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::prnl ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *ml:verbose* (prnl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::prndiv (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:verbose* (apply #'prndiv args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::symbol-is-variable-p (symbol)
  "Check if SYMBOL starts with a capital letter."
  (let ((first-char (substring (symbol-name symbol) 0 1)))
    (equal (upcase first-char) first-char)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::variable-p (thing)
  "Check if THING is a symbol that starts with a capital letter."
  (and (symbolp thing) (ml::symbol-is-variable-p thing)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ml::variable-p 'Foo)      returns t)
(confirm that (ml::variable-p 'foo)      returns nil)
(confirm that (ml::variable-p  nil)      returns nil)
(confirm that (ml::variable-p  666)      returns nil)
(confirm that (ml::variable-p  '(1 2 3)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::fix-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn capitalized variable designators Foo symbols like \?Foo anywhere in THING
(including improper tails)."
  ;; (prn "thing: %S" thing)
  (cond
    ((ml::variable-p thing) (intern (concat "?" (symbol-name thing))))
    ((atom thing) thing)
    (t (cons (ml::fix-variables (car thing)) (ml::fix-variables (cdr thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ml::fix-variables '(foo Bar)) returns (foo \?Bar))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::uniqueify-variables1 (sym thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Make variables in THING unique by symbolicating them with SYM."
  (cond
    ((ml::variable-p thing) (symbolicate- thing sym))
    ((atom thing) thing)
    (t (cons (ml::uniqueify-variables1 sym (car thing))
         (ml::uniqueify-variables1 sym (cdr thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ml::uniqueify-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Make variables in THING unique, using an identical number to make them unique."
  `(ml::uniqueify-variables1 (gensym "") ,thing))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::ununiqueify-symbol (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Remove the numeric suffix from THING if present."
  (if (symbolp thing)
    (let ((name (symbol-name thing)))
      (if (string-match "^\\(.*\\)-[0-9]+$" name)
        (intern (match-string 1 name))
        thing))
    thing))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ml::ununiqueify-symbol 'Foo-1234) returns Foo)
(confirm that (ml::ununiqueify-symbol 'Foo) returns Foo)
(confirm that (ml::ununiqueify-symbol '(1 2 3)) returns (1 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::ununiqueify-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Make variables in THING unique by symbolicating them with SYM."
  (cond
    ((ml::variable-p thing) (ml::ununiqueify-symbol thing))
    ((atom thing) thing)
    (t (cons (ml::ununiqueify-variables (car thing))
         (ml::ununiqueify-variables (cdr thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ml::ununiqueify-variables '\?Foo-1234)
  returns \?Foo)
(confirm that (ml::ununiqueify-variables '(\?Foo-1234 \?Bar-1234))
  returns (\?Foo \?Bar))
(confirm that 
  (ml::ununiqueify-variables '(1 \?Foo-1234 (2 \?Bar-1234 3)))
  returns (1 \?Foo (2 \?Bar 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::fmt-pretty-vars (string)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Replace occurrences of \\? with ? in STRING."
  (replace-regexp-in-string "\\\\\\?" "?" string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::unfix-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    ((u::variable-p thing) (make-symbol (substring (symbol-name thing) 1)))
    ((atom thing) thing)
    (t (cons (ml::unfix-variables (car thing)) (ml::unfix-variables (cdr thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml:prn-world ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (ml::prndiv)
  (dm::prn "DB:")
  (with-indentation
    (let ((alist (nreverse (db-to-alist '*ml*))))
      (dolist (rule alist)
        (let ((str
                (concat 
                  (substring
                    (format "%S"
                      (ml::unfix-variables (car rule)))
                    1 -1)
                  (cond
                    ((null (cdr rule)) ".")
                    ((null (cddr rule))
                      (concat " :- "
                        (substring
                          (format "%S"
                            (ml::unfix-variables (cadr rule)))
                          1 -1)
                        "."))
                    (t " :- ")))))
          (prn str)
          (when (cddr rule) ; (ml:prn-world)
            (with-indentation
              (let ((pos (cdr rule)))
                (while pos
                  (let ((clause (car pos)))
                    (prn ; (ml::fmt-pretty-vars
                      (concat (substring
                                (format "%S"
                                  (ml::unfix-variables clause))
                                1 -1)
                        (if (cdr pos) "," ".")))) ; )
                  (pop pos)))))))))
  (ml::prndiv)
  ;; (ml::prnl)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml:reset ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (ensure-db! '*ml*)
  (ml::prndiv)
  (ml::prn "Resetting database!")
  (ml::prndiv)
  (ml::prnl)
  (clear-db   '*ml*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun <- (fact)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (:- fact))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro <- (&rest fact)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(:- (,@fact)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun :- (consequent &rest antecedents)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (unless (consp consequent)
;;     (error "CONSEQUENT should be a cons or a symbol."))
;;   (when-let ((bad-antecedent
;;                (cl-find-if-not (lambda (a) (consp a)) antecedents)))
;;     (error "ANTECEDENTS should be conses, got: %S." bad-antecedent))
;;   (ensure-db! '*ml*)
;;   (let* ( (consequent     (u::fix-variables consequent))
;;           (antecedents    (mapcar #'u::fix-variables antecedents))
;;           (lookup         (db-get '*ml* consequent))
;;           (was-found      (cdr lookup))
;;           (found          (car lookup))
;;           (found-is-equal (and was-found (equal antecedents found))))    
;;     (cond
;;       ((not was-found) ; new rule, add it.
;;         (ml::prndiv)
;;         (let ((rule antecedents))
;;           (ml::prn "Adding rule %S." (cons consequent antecedents))
;;           (db-put '*ml* consequent rule)))
;;       (found-is-equal found) ; repeated (but `equal' rule, do nothing.
;;       (t (error "Already have a rule for %S: %S" consequent found)))
;;     (db-to-alist '*ml*)
;;     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro :- (consequent &rest antecedents)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (unless (consp consequent)
    (error "CONSEQUENT should be a cons or a symbol."))
  (when-let ((bad-antecedent
               (cl-find-if-not (lambda (a) (consp a)) antecedents)))
    (error "ANTECEDENTS should be conses, got: %S." bad-antecedent))
  (let ( (fixed-antecedents (mapcar #'ml::fix-variables antecedents))
         (fixed-consequent  (ml::fix-variables consequent)))
    `(progn
       (ensure-db! '*ml*)
       (let* ( (consequent        ',consequent)
               (antecedents       ',antecedents)
               (fixed-consequent  ',fixed-consequent) 
               (fixed-antecedents ',fixed-antecedents)
               (lookup            (db-get '*ml* fixed-consequent))
               (was-found         (cdr lookup))
               (found             (car lookup))
               (found-is-equal    (and was-found (equal fixed-antecedents found))))    
         (cond
           ((not was-found) ; new rule, add it.
             (ml::prndiv)
             (ml::prn "Adding rule %S." (cons consequent antecedents))
             (db-put '*ml* fixed-consequent fixed-antecedents))
           (found-is-equal found) ; repeated (but `equal' rule, do nothing.
           (t (error "Already have a rule for %S: %S" fixed-consequent found)))
         (db-to-alist '*ml*)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (hash-table-p (ml:reset)) returns t)
(confirm that (hash-table-empty-p (ml:reset)) returns t)
(confirm that (<- ari likes eating hamburger)
  returns (((ari likes eating hamburger))))
;; repeated rule does nothing:
(confirm that (<- ari likes eating hamburger)
  returns (((ari likes eating hamburger))))
(confirm that (:-
                (Person would eat a Food)
                (Person is a person)
                (Food is a food)
                (Person likes eating Food))
  returns ( ((\?Person would eat a \?Food)
              (\?Person is a person)
              (\?Food is a food)
              (\?Person likes eating \?Food))
            ((ari likes eating hamburger))))
;; repeated rule does nothing:
(confirm that (:-
                (Person would eat a Food)
                (Person is a person)
                (Food is a food)
                (Person likes eating Food))
  returns ( ((\?Person would eat a \?Food)
              (\?Person is a person)
              (\?Food is a food)
              (\?Person likes eating \?Food))
            ((ari likes eating hamburger))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--microlog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(ml:reset)
(<- ari likes eating hamburger)
(<- ari is a person)
(<- hamburger is a food)
(<- ari has beef)
(:-
  (Person would eat a Food)
  (Person is a person)
  (Food is a food)
  (Person likes eating Food))
(:- (Person could eat a Food)
  (Person is a person)
  (Food is a food)
  (Person would eat a Food)
  (Person can cook a Food))
(:- (Person can cook a hamburger)
  (Person has beef))
(ml:prn-world)
(ml:reset)

;; real Prolog: mortal(Who) :- man(Who).
(:- (Who is mortal) (Who is a man))
;; real Prolog: man(socrates).
(<- socrates is a man)
(ml:prn-world)
(ml:reset)

;; real Prolog in the comments, my Lisp stuff in the code:
;;  is-a-man(Thing) :- is-featherless(Thing), is-bipedal(Thing).
(:- (Thing is a man) (Thing is featherless) (Thing is bipedal))
;;  is-featherless(Thing) :- is-plucked(Thing).
(:- (Thing is featherless) (Thing is plucked))
;;  is-bipedal(Thing) :- is-a-chicken(Thing).
(:- (Thing is bipedal) (Thing is a chicken))
;;  is-a-chicken(some-chicken).
(<- some-chicken is a chicken)
;;  is-plucked(some-chicken).
(<- some-chicken is plucked)
(ml:prn-world)
(ml:reset)

(<-  member Item (Item  . Rest))
(:- (member Item (Thing . Rest)) (member Item Rest))
(ml:prn-world)
(ml:reset)
