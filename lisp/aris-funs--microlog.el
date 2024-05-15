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
(defvar *ml:db* nil
  "The database of rules and facts.")
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
(defun ml::fmt-pretty-vars (string)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Replace occurrences of \\? with ? in STRING."
  (replace-regexp-in-string "\\\\\\?" "?" string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml::prettify-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    ((u::variable-p thing) (make-symbol (capitalize1 (substring (symbol-name thing) 1))))
    ((atom thing) thing)
    (t (cons (ml::prettify-variables (car thing)) (ml::prettify-variables (cdr thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml:prn-world ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (ml::prndiv)
  (dm::prn "DB:")
  (with-indentation
    (let ((alist (db-to-alist '*ml*)))
      (dolist (rule alist)
        (let ((str
                (concat 
                  (substring (format "%S" (ml::prettify-variables (car rule))) 1 -1)
                  (cond
                    ((null (cdr rule)) ".")
                    ((null (cddr rule))
                      (concat " :- "
                        (substring
                          (format "%S" (ml::prettify-variables (cadr rule))) 1 -1)
                        "."))
                    (t " :- ")))))
          (prn str)
          (when (cddr rule) ; (ml:prn-world)
            (with-indentation
              (let ((pos (cdr rule)))
                (while pos
                  (let ((clause (car pos)))
                    (prn (ml::fmt-pretty-vars
                           (concat (substring (format "%S"
                                                (ml::prettify-variables clause))
                                     1 -1)
                             (if (cdr pos) "," ".")))))
                  (pop pos)))))))))
  (ml::prndiv)
  (ml::prnl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ml:reset ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (ensure-db! '*ml*)
  (clear-db   '*ml*)
  ;; (setf *ml:db* nil)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun <- (fact)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (:- fact))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro <- (&rest fact)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(:- (,@fact)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun :- (consequent &rest antecedents)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  `(progn
     (ensure-db! '*ml*)
     (let* ( (consequent     (u::fix-variables ',consequent))
             (antecedents    (mapcar #'u::fix-variables ',antecedents))
             (lookup         (db-get '*ml* consequent))
             (was-found      (cdr lookup))
             (found          (car lookup))
             (found-is-equal (and was-found (equal antecedents found))))    
       (cond
         ((not was-found) ; new rule, add it.
           (ml::prndiv)
           (let ((rule antecedents))
             (ml::prn "Adding rule %S." (cons consequent antecedents))
             (db-put '*ml* consequent rule)))
         (found-is-equal found) ; repeated (but `equal' rule, do nothing.
         (t (error "Already have a rule for %S: %S" consequent found)))
       (db-to-alist '*ml*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (hash-table-p (ml:reset)) returns t)
(confirm that (hash-table-empty-p (ml:reset)) returns t)
(confirm that (<- ari likes eating hamburger)
  returns (((ari likes eating hamburger))))
;; repeated rule does nothing:
(confirm that (<- ari likes eating hamburger)
  returns (((ari likes eating hamburger))))
(confirm that (:-
                (,Person would eat a ,Food)
                (,Person is a person)
                (,Food is a food)
                (,Person likes eating ,Food))
  returns ( ((\?Person would eat a \?Food)
              (\?Person is a person)
              (\?Food is a food)
              (\?Person likes eating \?Food))
            ((ari likes eating hamburger))))
;; repeated rule does nothing:
(confirm that (:-
                (,Person would eat a ,Food)
                (,Person is a person)
                (,Food is a food)
                (,Person likes eating ,Food))
  returns ( ((\?Person would eat a \?Food)
              (\?Person is a person)
              (\?Food is a food)
              (\?Person likes eating \?Food))
            ((ari likes eating hamburger))))
;; This should (and does) signal an error:
;; (confirm that (:- '(,Person would eat a ,Food) '(,Person likes ,Food))
;;   returns ( ( ((\, Person) would eat a (\, Food))
;;               ((\, Person) likes eating (\, Food)))
;;             ( (ari likes eating hamburger))))
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
(provide 'aris-funs--microlog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ml::prettify-variables '(x (\?foo \?bar)))
(u::variable-p '\?foo)
(<- ari is a person)
(<- hamburger is a food)
(<- ari has beef)
(:-
  (,Person could eat a ,Food)
  (,Person is a person)
  (,Food is a food)
  (,Person would eat a ,Food)
  (,Person can cook a ,Food))
(:-
  (ari can cook a hamburger)
  (ari has beef))


(ml:prn-world)


