;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--destructuring-match-aux)
(require 'aris-funs--trees)
(require 'trace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup unify nil
  "Ari's unification functions.")
;;---------------------------------------------------------------------------------------------------
(defcustom *u:verbose* t
  "Whether or not functions in the 'unify' group should print verbose messages."
  :group 'unify
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *u:test* t
  ;; (setq *u:test* nil)
  ;; (setq *u:test* t)
  "Whether or not unit test are enables."
  :group 'unify
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *u:div-width* 90
  "Div width used by functions in the 'unify' group."
  :group 'unify
  :type 'integer)
;;---------------------------------------------------------------------------------------------------
(defcustom *u:occurs-check* t
  "Whether to perform the occurs check, can be nil, t or :soft."
  :group 'unify
  :type 'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *u:unifier-simplifies* nil
  "Whether `u::unifier' simplifies BINDINGS before substituting."
  :group 'unify
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *u:bind-conses* t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print helpers:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::prn (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *u:verbose* (apply #'prn args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::prnl (&optional count)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *u:verbose* (prnl count)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::prndiv (&optional char)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *u:verbose* (funcall #'prndiv (or char ?\=) *u:div-width*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formatting helper:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::style (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    ((u::variable-p thing) (format "%S" thing))
    ((atom thing) (format-message "`%S'" thing))
    (t (format "%S" thing))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::reuse-cons (x y x-y)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return (cons x y), or u::reuse x-y if it is equal to (cons x y). Norvig's."
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
    x-y
    (cons x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::variable-p (x)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) ?\?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::fix-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn comma-prefixed variable designators like ,X / (\, X) into symbols like \?X anywhere in THING
(including improper tails)."
  (cond
    ((atom thing) thing)
    ((and (consp thing) (eq '\, (car thing)) (cdr thing) (not (cddr thing)) (symbolp (cadr thing)))
      (intern (concat "?" (symbol-name (cadr thing)))))
    (t (cons (u::fix-variables (car thing)) (u::fix-variables (cdr thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::equivalent-bindings? (bindings1 bindings2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when BINDINGS1 and BINDINGS2 are equivalent. This is mainly intended for use in the unit tests:
it won't detect every case of equivalency (such as recursive cases), but should handle the simple cases that
are relevant to the unit tests correctly."
  (cond
    ((and (eq t bindings1) (eq t bindings2)))
    ((or (eq t bindings1) (eq t bindings2)) nil)
    (t 
      (catch 'not-equivalent
        (dolist (ordering (list (list bindings1 bindings2) (list bindings2 bindings1)))
          (dolist (pair (car ordering))
            (unless (u::variable-p  (car pair))
              (error "malformed bindings, cars in each pair should be variables: %s" pair))
            (let ( (assoc  (assoc  (car pair) (cadr ordering)))
                   (rassoc (rassoc (car pair) (cadr ordering))))
              (cond
                ((and assoc  (equal (cdr assoc)  (cdr pair))))
                ((and rassoc (equal (car rassoc) (cdr pair))))
                (t (throw 'not-equivalent nil))))))
        t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *u:test*
  (confirm that
    (u::equivalent-bindings?
      (u::fix-variables '(((\, c) . 1) ((\, b) . (\, a))))
      (u::fix-variables '(((\, c) . 1) ((\, a) . (\, b)))))
    returns t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::subst-bindings (bindings x)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Substitute the value of variables in bindings into x,
 taking recursively bound variables into account. Norvig's with tweaks."
  ;; (u::prndiv)
  ;; (debug bindings x)
  ;; (u::prn "x:        %s" x)
  (let
    ((res 
       (cond
         ((null bindings)
           ;; (u::prn "case 1")
           nil)
         ((eq bindings t)
           ;; (u::prn "case 2")
           x)
         ((and (u::variable-p x) (assoc x bindings))
           ;; (u::prn "case 3")
           (with-indentation (u::subst-bindings bindings (cdr (assoc x bindings)))))
         ((atom x) x)
         (t
           ;; (u::prn "case 4")
           (u::reuse-cons
             (with-indentation (u::subst-bindings bindings (car x)))
             (with-indentation (u::subst-bindings bindings (cdr x)))
             x)))))
    ;; (u::prn "result:   %s" res)
    ;; (u::prndiv)
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::occurs (bindings var expr)
  "Doe VAR occur anywhere inside EXPR?"
  (u::prndiv ?\-)
  (u::prn "check if %s occurs in %s" (u::style var) (u::style expr))
  (debug nil bindings var expr)
  (let
    ((res
       (if (u::variable-p expr)
         (cond
           ((and (u::variable-p var) (eq (cadr expr) (cadr var))))
           ((assoc expr bindings)
             (with-indentation
               (u::prndiv ?\-)
               (u::occurs bindings var (cdr (assoc expr bindings)))))
           (t nil))
         (cond
           ((consp expr) (or
                           (with-indentation
                             (u::prndiv ?\-)
                             (u::occurs bindings var (car expr)))
                           (with-indentation
                             (u::prndiv ?\-)
                             (u::occurs bindings var (cdr expr)))))
           ((eq var expr))))))
    (u::prn "%s %s in %s"
      (u::style var)
      (if res "         occurs" "DOES NOT occur ")
      (u::style expr))
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::occurs (bindings var expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Does VAR occur anywhere inside EXPR according to BINDINGS?"
  (cond
    ((eq var expr) t)
    ((and (u::variable-p expr) (assoc expr bindings))
      (u::occurs bindings var (cdr (assoc expr bindings))))
    ((consp expr)
      (or (u::occurs bindings var (car expr))
        (u::occurs bindings var (cdr expr))))
    (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *u:test*
  (confirm that
    (u::occurs
      (u::fix-variables '(((\, x) f (\, y))))
      (u::fix-variables '(\, y))
      (u::fix-variables '(f (\, x))))
    returns t)
  (confirm that (u::occurs nil 'x '(f (\, x))) returns t)
  (confirm that (u::occurs '(((\, x) f (\, y))) 'x '(f (\, x))) returns t)
  (confirm that (u::occurs '(((\, x) f (\, y))) 'z '(f (\, x))) returns nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify-variable-with-value1 (bindings pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: when this recurses, it might swap which tail was PAT1 and which was PAT2, but that seems to be fine."
  (let* ( (variable (car pat1))
          (value    (car pat2))
          (binding  (assoc variable bindings)))
    (u::prn "try to unify the variable %s and with the value %s."
      (u::style variable)
      (u::style value))
    (cond
      ((and *u:occurs-check* (u::occurs bindings variable value))
        (u::prn "occurs check failed, not unifying.")
        (when (eq *u:occurs-check* :soft)
          (u::prn ":soft occurs check enabled, skip binding but continue unifying...")
          (u::unify1 bindings (cdr pat1) (cdr pat2))))
      ((and binding (not (equal value (cdr binding))))
        (u::prn "variable %s is already bound to a different value, %s."
          (u::style variable)
          (u::style (cdr binding)))
        (if (not (u::variable-p (cdr binding)))
          nil
          (u::unify-variable-with-value1 bindings (cons (cdr binding) (cdr pat1)) pat2)))
      (binding
        (u::prn "variable %s is already bound to the same value, %s."
          (u::style variable)
          (u::style (cdr binding)))
        (with-indentation (u::unify1 bindings (cdr pat1) (cdr pat2))))
      (t
        (u::prn "binding variable %s to value %s." (u::style variable)
          (u::style value))
        (with-indentation
          (u::unify1 (cons (cons variable value) bindings) (cdr pat1)
            (cdr pat2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify-variable-with-variable2 (bindings pat1 pat2)
  ;; if we entered this fun we already know that (car pat1) is unbound and that (car pat2) is a
  ;; bound variable.
  (let ((binding2 (assoc (car pat2) bindings)))
    (if (and (u::variable-p (car pat2)) binding2)
      (progn
        (u::prn "avoid circular binding, unify %s with %s's value %s instead!"
          (u::style (car pat1))
          (u::style (car pat2))
          (u::style (cdr binding2)))
        (with-indentation
          (u::unify1 bindings pat1 (cons (cdr binding2) (cdr pat2)))))
      (u::prn "variable %s is unbound, unifying it with %s."
        (u::style (car pat1))
        (u::style (car pat2)))
      (with-indentation
        (u::unify1 (cons (cons (car pat1) (car pat2)) bindings) (cdr pat1) (cdr pat2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify1 (bindings pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (u::unify1 nil '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (u::prndiv)
  (u::prn "pat1:     %S" pat1)
  (u::prn "pat2:     %S" pat2)
  (u::prn "bindings: %S" bindings)
  (u::prndiv ?\-)
  ;; uncurl tails:
  (let ((pat1-tail-type
          (and pat1 (atom pat1)))
         (pat2-tail-type
           (and pat2 (atom pat2))))
    (when (and pat1-tail-type pat2-tail-type)      
      (setf pat1 (list pat1))
      (setf pat2 (list pat2))
      (u::prn "pat1*:    %S" pat1)
      (u::prn "pat2*:    %S" pat2)))
  (cond
    ;;----------------------------------------------------------------------------------------------
    ((not (or pat1 pat2))
      (u::prn "PAT1 and PAT2 both ran out.")
      (or bindings t))
    ;;----------------------------------------------------------------------------------------------
    ((not pat1)
      (u::prn "PAT1 ran out.")
      nil)
    ;;----------------------------------------------------------------------------------------------
    ((not pat2)
      (u::prn "PAT2 ran out.")
      nil)
    ;;----------------------------------------------------------------------------------------------
    ;; `eql' atoms on both sides:
    ((and (atom (car pat1)) (atom (car pat2)) (eql (car pat1) (car pat2)))
      (u::prn "%s and %s are eql atoms." (u::style (car pat1)) (u::style (car pat2)))
      (with-indentation (u::unify1 bindings (cdr pat1) (cdr pat2))))
    ;;----------------------------------------------------------------------------------------------
    ;; same variable:
    ((and (u::variable-p (car pat1)) (u::variable-p (car pat2))
       (eq (car pat1) (car pat2)))
      (u::prn "%s and %s are the same variable." (u::style (car pat1)) (u::style (car pat2)))
      (with-indentation (u::unify1 bindings (cdr pat1) (cdr pat2))))
    ;;----------------------------------------------------------------------------------------------
    ;; both different variables:
    ((and (u::variable-p (car pat1)) (u::variable-p (car pat2)))
      (u::prn "both PAT1 elem %s and PAT2 elem %s are variables."
        (u::style (car pat1))
        (u::style (car pat2)))
      (let ( (binding1 (assoc (car pat1) bindings))
             (binding2 (assoc (car pat2) bindings)))
        (u::prn "%s = %s" (u::style (car pat1))
          (if binding1 (u::style (cdr binding1)) "<unbound>"))
        (u::prn "%s = %s" (u::style (car pat2))
          (if binding2 (u::style (cdr binding2)) "<unbound>"))
        (cond
          ;;-----------------------------------------------------------------------------------------
          ;; bound to un-`equal' values:
          ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
            (u::prn "already bound to different terms.")
            nil)
          ;;-----------------------------------------------------------------------------------------
          ;; neither bound:
          ((not (or binding1 binding2))
            (u::prn "both unbound.")
            (with-indentation
              (u::unify1 (cons (cons (car pat1) (car pat2)) bindings) (cdr pat1) (cdr pat2))))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1's var not bound, unify with PAT2's var:
          ((not binding1)
            (u::unify-variable-with-variable2 bindings pat1 pat2))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT2's var not bound, unify with PAT1's var:
          ((not binding2)
            (u::unify1 bindings pat2 pat1))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1 and PAT2's vars are bound to the same value, unify them destructively.
          ;; not totally sure about this but it seems to work, so far.
          (t (u::prn
               (concat "PAT1's var %s and PAT2's var %s are bound to the same value, "
                 "unifying them destructively.")
               (u::style (car pat1)) (u::style (car pat2)))
            (setcdr binding1 (car binding2))
            (with-indentation (u::unify1 bindings (cdr pat1) (cdr pat2)))))))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT1, value on PAT2:
    ((and (u::variable-p (car pat1)) (or *u:bind-conses* (atom (car pat2))))
      (with-indentation (u::unify-variable-with-value1 bindings pat1 pat2)))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT2, value on PAT1:
    ((and (u::variable-p (car pat2)) (or *u:bind-conses* (atom (car pat1))))
      (with-indentation (u::unify1 bindings pat2 pat1))
      ;; (u::unify-variable-with-value1 bindings pat2 pat1)
      )
    ;;----------------------------------------------------------------------------------------------
    ;; conses on both sides:
    ((and (consp (car pat1)) (consp (car pat2)))
      ;; (debug nil :conses (car pat1) (car pat2))
      (u::unify1 (u::unify1 bindings (car pat1) (car pat2)) (cdr pat1) (cdr pat2)))
    ;;----------------------------------------------------------------------------------------------
    (t nil ;; (debug nil t (car pat1) (car pat2))
      (u::prn "unhandled"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *u:test*
  (confirm that (u::unify1 nil (u::fix-variables '(,x + 1))
                  (u::fix-variables '(2 + 1)))
    returns ((\?x . 2)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,w ,x ,y))
                  (u::fix-variables '(,x ,y ,w)))
    returns ((\?x . \?y) (\?w . \?x))) 
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,w ,x ,y ,z))
                  (u::fix-variables '(,x ,y ,z ,w)))
    returns ((\?y . \?z) (\?x . \?y) (\?w . \?x)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(333 + ,x))
                  (u::fix-variables '(,x + 333)))
    returns ((\?x . 333)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(2 + 1))
                  (u::fix-variables '(2 + 1)))
    returns t)
  (confirm that (u::unify1 nil
                  (u::fix-variables '(2 + 1))
                  (u::fix-variables '(,x + 1)))
    returns ((\?x . 2)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,y + 1))
                  (u::fix-variables '(,x + ,x)))
    returns ((\?x . 1) (\?y . \?x)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x + 1))
                  (u::fix-variables '(2 + ,y)))
    returns ((\?y . 1) (\?x . 2)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x + 1))
                  (u::fix-variables '(2 + ,y + 1)))
    returns nil)
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x + 1 + 2))
                  (u::fix-variables '(2 + ,y + 3)))
    returns nil)
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x + 1 + ,a))
                  (u::fix-variables '(2 + ,y + ,a)))
    returns ((\?y . 1) (\?x . 2)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x + 1 + ,a))
                  (u::fix-variables '(2 + ,y + ,b)))
    returns ((\?b . \?a)
              (\?y . 1) (\?x . 2)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x + 1 + ,a))
                  (u::fix-variables '(2 + ,y + ,b + 3)))
    returns nil)
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x + 1))
                  (u::fix-variables '(2 + ,x)))
    returns nil)
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x + 1 + ,a + 8))
                  (u::fix-variables '(2 + ,y + ,a + ,a)))
    returns ((\?a . 8) (\?y . 1) (\?x . 2)))
  ;;---------------------------------------------------------------------------------------------------
  ;; uncurl tails tests:
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x ,y . (,z . ,zz)))
                  (u::fix-variables '(1 2 . (3 . 4))))
    returns ((\?zz . 4) (\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x ,y . (,z . ,zz)))
                  (u::fix-variables '(1 2 . (3 . 4))))
    returns ((\?zz . 4) (\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x ,y . ,z))
                  (u::fix-variables '(1 2 . 3)))
    returns ((\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (u::unify1 nil
                  (u::fix-variables '(,x ,y   ,z))
                  (u::fix-variables '(1 2   3)))
    returns ((\?z . 3) (\?y . 2) (\?x . 1)))
  ;;---------------------------------------------------------------------------------------------------
  ;; occurs check tests:
  (confirm that
    (let ((*u:occurs-check* nil))
      (u::unify1 nil
        (u::fix-variables '(,x ,y))
        (u::fix-variables '((f ,y) (f ,x)))))
    returns ((\?y f \?x) (\?x f \?y)))
  (confirm that
    (let ((*u:occurs-check* :soft))
      (u::unify1 nil
        (u::fix-variables '(,x ,y))
        (u::fix-variables '((f ,y) (f ,x)))))
    returns ((\?x f \?y)))
  (confirm that
    (let ((*u:occurs-check* t))
      (u::unify1 nil
        (u::fix-variables '(,x ,y))
        (u::fix-variables '((f ,y) (f ,x)))))
    returns nil)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun u:unify (thing1 thing2)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Unidy THING1 and THING2 and produce an alist of bindings."
;;   (u::unify1 nil (u::fix-variables thing1) (u::fix-variables thing2))
;;   ;; (u::unify1 nil thing1 thing2)
;;   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro u:unify (thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Unidy THING1 and THING2 and produce an alist of bindings."
  `(u::unify1 nil (u::fix-variables ,thing1) (u::fix-variables ,thing2))
  ;; (u::unify1 nil thing1 thing2)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *u:test*
  (confirm that (u:unify '(,w ,x ,y) '(,x ,y ,w))
    returns ((\?x . \?y) (\?w . \?x)))
  (confirm that (u:unify '(,w ,x ,y ,z) '(,x ,y ,z ,w))
    returns ((\?y . \?z) (\?x . \?y) (\?w . \?x)))
  (confirm that (u:unify '(333 + ,x + ,x) '(,z + 333 + ,z))
    returns ((\?x . \?z) (\?z . 333)))
  (confirm that (u:unify '(333 + ,x) '(,x + 333))
    returns ((\?x . 333)))
  (confirm that (u:unify '(2 + 1) '(2 + 1))
    returns t)
  (confirm that (u:unify '(,x + 1) '(2 + 1))
    returns ((\?x . 2)))
  (confirm that (u:unify '(2 + 1) '(,x + 1))
    returns ((\?x . 2)))
  (confirm that (u:unify '(,y + 1) '(,x + ,x))
    returns ((\?x . 1) (\?y . \?x)))
  (confirm that (u:unify '(,x + 1) '(2 + ,y))
    returns ((\?y . 1) (\?x . 2)))
  (confirm that (u:unify '(,x + 1) '(2 + ,y + 1))
    returns nil)
  (confirm that (u:unify '(,x + 1 + 2) '(2 + ,y + 3))
    returns nil)
  (confirm that (u:unify '(,x + 1 + ,a) '(2 + ,y + ,a))
    returns ((\?y . 1) (\?x . 2)))
  (confirm that (u:unify '(,x + 1 + ,a) '(2 + ,y + ,b))
    returns ((\?b . \?a) (\?y . 1) (\?x . 2)))
  (confirm that (u:unify '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
    returns nil)
  (confirm that (u:unify '(,x + 1) '(2 + ,x))
    returns nil)
  (confirm that (u:unify '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))
    returns ((\?a . 8) (\?y . 1) (\?x . 2)))
  ;;---------------------------------------------------------------------------------------------------
  ;; uncurl tails tests:
  (confirm that (u:unify '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
    returns ((\?zz . 4) (\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (u:unify '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
    returns ((\?zz . 4) (\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (u:unify '(,x ,y . ,z) '(1 2 . 3))
    returns ((\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (u:unify '(,x ,y   ,z) '(1 2   3))
    returns ((\?z . 3) (\?y . 2) (\?x . 1)))
  ;;---------------------------------------------------------------------------------------------------
  ;; occurs check tests:
  (confirm that
    (let ((*u:occurs-check* nil))
      (u:unify '(,x ,y) '((f ,y) (f ,x))))
    returns ((\?y f \?x) (\?x f \?y)))
  (confirm that
    (let ((*u:occurs-check* :soft))
      (u:unify '(,x ,y) '((f ,y) (f ,x))))
    returns ((\?x f \?y)))
  (confirm that
    (let ((*u:occurs-check* t))
      (u:unify '(,x ,y) '((f ,y) (f ,x))))
    returns nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::simplify-bindings (bindings)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Simplify BINDINGS by finding anything bound directly to another variable and rebinding it to what that variable is
bound to."
  (u::prn "simplifying       %s." bindings)
  (dolist (pair1 bindings bindings)
    (dolist (pair2 bindings)
      (when (equal (cdr pair2) (car pair1))
        (setcdr pair2 (cdr pair1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *u:test*
  (confirm that
    (u::simplify-bindings (u::unify1 nil
                            (u::fix-variables '(,v ,u ,w ,x))
                            (u::fix-variables '(,u ,x ,v 333))))
    returns ((\?x . 333) (\?w . 333) (\?u . 333) (\?v . 333))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u:unifier (pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "My version of Norvig's `n:unifier'."
  (let* ( (bindings (u:unify pat1 pat2))
          (bindings (if *u:unifier-simplifies*
                      (u::simplify-bindings bindings)
                      bindings)))
    (u::prn "unified bindings: %s" bindings)
    (let ((expr (u::subst-bindings bindings (u::fix-variables pat1))))
      (u::prn "substituted expr: %s" expr)
      (u::prndiv)
      (u::prnl)
      expr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *u:test*
  (confirm that (u:unifier '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
    returns (2 + 1 + 2 + 2))
  (confirm that (u:unifier '(,x + 1 + ,a) '(2 + ,y + ,a))
    returns (2 + 1 + \?a))
  (confirm that (u:unifier '(333 + ,x + ,x) '(,z + 333 + ,z))
    returns (333 + 333 + 333))
  (confirm that
    (u:unifier '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))
    returns (2 + 1 + 8 + 8))
  (confirm that
    (let ( (*u:verbose* t)
           (*u:unifier-simplifies* nil))
      (u:unifier '(,v ,u ,w ,x) '(,u ,x ,v 333)))
    returns (333 333 333 333))
  (confirm that
    (let ( (*u:verbose* t)
           (*u:unifier-simplifies* t))
      (u:unifier 
        '(,v ,u ,w ,x)
        '(,u ,x ,v 333)))
    returns (333 333 333 333))
  ;; uncurl tails:
  (confirm that (u:unifier '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
    returns (1 2 3 . 4))
  (confirm that (u:unifier '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
    returns (1 2 3 . 4))
  (confirm that (u:unifier '(,x ,y . ,z) '(1 2 . 3))
    returns (1 2 . 3))
  (confirm that (u:unifier '(,x ,y   ,z) '(1 2   3))
    returns (1 2 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ignore!
  (let ((reps 5000))
    (list
      (benchmark-run reps
        (let ( (*u:verbose* nil)
               (*u:unifier-simplifies* nil))
          (u:unifier 
            '(,u ,v ,w ,x)
            '(,x ,u ,v 333)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Norvigian version follows:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *n:occurs-check* t)
(defvar n::fail '(nil . nil))
(defvar n::no-bindings '((t . t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::extend-bindings (bindings var expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Add a (VAR . EXPR) pair to BINDINGS."
  (cons (cons var expr)
    ;; Once we add a "real" binding,
    ;; we can get rid of the dummy n::no-bindings
    (if (eq bindings n::no-bindings) nil bindings)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::subst-bindings (bindings expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Substitute the value of variables in BINDINGS into EXPR,
 taking recursively bound variables into account."
  (cond
    ((eq bindings n::fail) n::fail)
    ((eq bindings n::no-bindings) expr)
    ((and (u::variable-p expr) (assoc expr bindings))
      (n::subst-bindings bindings (cdr (assoc expr bindings))))
    ((atom expr) expr)
    (t (u::reuse-cons (n::subst-bindings bindings (car expr))
         (n::subst-bindings bindings (cdr expr)) expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::unify-variable (bindings var expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Unify VAR with EXPR, using (and maybe extending) BINDINGS."
  (cond
    ((assoc var bindings)
      (n::unify1 bindings (cdr (assoc var bindings)) expr))
    ((and (u::variable-p expr) (assoc expr bindings))
      (n::unify1 bindings var (cdr (assoc expr bindings))))
    ((and *n:occurs-check* (u::occurs bindings var expr))
      n::fail)
    (t (n::extend-bindings bindings var expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::unify1 (bindings thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "See if THING1 and THING2 match with given BINDINGS."
  (let ((bindings (or bindings n::no-bindings)))
    (cond
      ((eq bindings n::fail)
        ;; (debug nil 1)
        n::fail)
      ((eql thing1 thing2)
        ;; (debug nil 2)
        bindings)
      ((u::variable-p thing1)
        ;; (debug nil 3)
        (n::unify-variable bindings thing1 thing2))
      ((u::variable-p thing2)
        ;; (debug nil 3)
        (n::unify-variable bindings thing2 thing1))
      ((and (consp thing1) (consp thing2))
        ;; (debug nil 3)
        (n::unify1 (n::unify1 bindings (car thing1) (car thing2))
          (cdr thing1)
          (cdr thing2)))
      (t
        ;; (debug nil 5)
        n::fail))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun n:unify (thing1 thing2)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Unidy THING1 and THING2 and produce an alist of bindings."
;;   (n::unify1 nil (u::fix-variables thing1) (u::fix-variables thing2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n:unify (thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Unidy THING1 and THING2 and produce an alist of bindings."
  `(n::unify1 nil (u::fix-variables ,thing1) (u::fix-variables ,thing2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n:unifier (thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return something that unifies with both THING1 and THING2 (or n::fail)."
  (n::subst-bindings (n:unify thing2 thing1) (u::fix-variables thing2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; junk:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (progn
      (trace-function #'u:unifier)
      (trace-function #'u::unify1)
      (trace-function #'u:unify)
      (trace-function #'u::reuse-cons)
      (trace-function #'u::occurs)
      (trace-function #'u::unify-variable-with-value1)
      (trace-function #'u::unify-variable-with-variable2)
      (trace-function #'u::simplify-bindings)
      (trace-function #'u::subst-bindings))

    (untrace-all)

    (let ((*u:verbose* t))
      (u:unifier '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2)))

    (let ((*u:verbose* t))
      (u:unifier '(333 + ,x + ,x) '(,z + 333 + ,z)))

    (let ((reps 1000))
      (benchmark-run reps
        (let ((*u:verbose* nil)) (u:unifier '(333 + ,x + ,x) '(,z + 333 + ,z)))))

    (u:unify '(,x ,x) '(,y ,y))
    (u:unify '(,x ,y a) '(,y ,x ,x))
    (u:unify '(,x ,y) '(,y ,x)) ;; => ((,X . ,Y))
    (u:unify '(,x ,y a) '(,y ,x ,x)) ;; => ((,Y . A) (,X . ,Y))

    (u:unify '(,y ,x) '(7 (f ,x)))
    (u:unifier '(,y ,x) '(7 (f ,x)))
    (u:unifier '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))

    (let ((*u:occurs-check* nil))
      (u:unify '(,x ,y) '((f ,y) (f ,x)))) ; (((\, y) f (\, x)) ((\, x) f (\, y)))
    (let ((*u:occurs-check* :soft))
      (u:unify '(,x ,y) '((f ,y) (f ,x)))) ; (((\, x) f (\, y)))
    (let ((*u:occurs-check* t))
      (u:unify '(,x ,y) '((f ,y) (f ,x)))) ; nil

    (ignore!
      (variable variable) ; bind var...
      (variable atom)     ; bind var...
      (atom     variable) ; bind var...
      (variable cons)     ; ???
      (cons     variable) ; ???
      (cons     cons)     ; recurse!
      (atom     cons)     ; FAIL.
      (cons     atom)     ; FAIL.
      (atom     atom))     ; `eql'.
    


    (n::unify1 nil '(x) '(11))

    ;; (trace-function #'n::unify)
    ;; (trace-function #'n::unify1)
    ;; (trace-function #'n:unifier)
    ;; (trace-function #'n::extend-bindings3)
    ;; (trace-function #'u::fix-variables)
    ;; (trace-function #'n::subst-bindings)
    ;; (untrace-all)

    (n::unify1 nil 'x 11) 
    (n::unify1 nil '(x y z) '(11 22 33)) 

    (n::unify1 nil (u::fix-variables '(,x ,y ,z)) '(11 22 33))
    (n::unify1 nil (u::fix-variables '(,x ,y ,z)) (u::fix-variables '(,x ,y ,z)))

    (n::unify1 nil  (u::fix-variables '(,x ,y (,z 8 ,b . ,c))) '(1 2 (3 8 4 . 5)))
    (u::variable-p (car (u::fix-variables '(,x ,y (,z 8 ,b . \?c)))))

    (n:unify
      '(,u ,v ,w ,x)
      '(,x ,u ,v 333))

    (n::unify1
      nil
      (u::fix-variables '(,u ,v ,w ,x))
      (u::fix-variables '(,x ,u ,v 333)))


    (u:unify '(,w ,x ,y ,z) '(,x ,y ,z ,w))
    (n:unify  '(,w ,x ,y ,z) '(,x ,y ,z ,w))

    (n:unify
      '((,a * ,x ^ 2) + (,b * ,x) + ,c)
      '(,z + (4 * 5) + 3))
    (u:unify
      '((,a * ,x ^ 2) + (,b * ,x) + ,c)
      '(,z + (4 * 5) + 3))

    (n:unify
      '(,u ,v 333 ,w)
      '(,x ,u ,v  ,v))

    (u:unify
      '(,u ,v 333 ,w)
      '(,x ,u ,v  ,v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--unification)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
