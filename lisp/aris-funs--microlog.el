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
(defun ml:prn-world ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (ml::prndiv)
  (dm::prn "DB:")
  (with-indentation
    (ml::prn (trim-trailing-whitespace (pp-to-string-without-offset *ml:db*))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun <- (fact)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (:- fact))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun :- (consequent &rest antecedents)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (unless (or (consp consequent) (symbolp consequent))
    (error "CONSEQUENT should be a cons or a symbol."))
  (when-let ((bad-antecedent
               (cl-find-if-not (lambda (a) (or (consp a) (symbolp a))) antecedents)))
    (error "ANTECEDENTS should be a consses or symbols, got: %S." bad-antecedent))
  (ensure-db! '*ml*)
  (let* ( (consequent     (u::fix-variables consequent))
          (antecedents    (mapcar #'u::fix-variables antecedents))
          (lookup         (db-get '*ml* consequent))
          (was-found      (cdr lookup))
          (found          (car lookup))
          (found-is-equal (and was-found (equal antecedents (cdr found)))))    
    (cond
      ((not found) ; new rule, add it.
        (ml::prndiv)
        (let ((rule (cons consequent antecedents)))
          (db-put '*ml* consequent (ml::prn1 "Adding rule %S." rule))))
      (found-is-equal found) ; repeated (but `equal' rule, do nothing.
      (t (error "Already have a rule for %S: %S" consequent found)))
    (get '*ml* 'db)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(get '*ml* 'db)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (hash-table-p (ml:reset)) returns t)
(confirm that (hash-table-empty-p (ml:reset)) returns t)
(confirm that (<- '(kiki likes eating hamburgers))
  returns ((kiki likes eating hamburgers)))
;; repeated rule does nothing:
(confirm that (<- '(kiki likes eating hamburgers))
  returns (((kiki likes eating hamburgers))))
(confirm that (:- '(,person would eat a ,food) '(,person likes eating ,food))
  returns ( ((\?person would eat a \?food) (\?person likes eating \?food))
            ((kiki likes eating hamburgers))))
;; repeated rule does nothing:
(confirm that (:- '(,person would eat a ,food) '(,person likes eating ,food))
  returns ( ((\?person would eat a \?food) (\?person likes eating \?food))
            ((kiki likes eating hamburgers))))
;; This should (and does) signal an error:
;; (confirm that (:- '(,person would eat a ,food) '(,person likes ,food))
;;   returns ( ( ((\, person) would eat a (\, food))
;;               ((\, person) likes eating (\, food)))
;;             ( (kiki likes eating hamburgers))))
(ml:prn-world)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--microlog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ml:reset)
;; (:- '(foo) '(bar baz quux) '(shprungy))

;; (pat-match '(?x + ?y) '(2 + 1)) ;; => ((?X . 2) (?Y . 1))
(dm:match     '(,x + ,y) '(2 + 1)) ;; => (( x . 2) ( y . 1))

;; (unify    '(?x + 1) '(2 + ?y))  ;; => ((?Y . 1) (?X . 2))
;; (dm:unify '(,x + 1) '(2 + ,y))  ;; => (( y . 1) ( x . 2))

