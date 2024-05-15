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
(defcustom *uu:verbose* t
  "Whether or not functions in the 'unify' group should print verbose messages."
  :group 'unify
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *uu:test* t
  ;; (setq *uu:test* nil)
  ;; (setq *uu:test* t)
  "Whether or not unit test are enables."
  :group 'unify
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *uu:div-width* 90
  "Div width used by functions in the 'unify' group."
  :group 'unify
  :type 'integer)
;;---------------------------------------------------------------------------------------------------
(defcustom *uu:occurs-check* t
  "Whether to perform the occurs check, can be nil, t or :soft."
  :group 'unify
  :type 'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *uu:unifier-simplifies* nil
  "Whether `uu::unifier' simplifies BINDINGS before substituting."
  :group 'unify
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *uu:bind-conses* t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print helpers:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::prn (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *uu:verbose* (apply #'prn args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::prnl (&optional count)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *uu:verbose* (prnl count)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::prndiv (&optional char)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *uu:verbose* (funcall #'prndiv (or char ?\=) *uu:div-width*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formatting helper:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::style (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    ((atom thing) (format-message "`%S'" thing))
    ((dm::pat-elem-is-a-variable? thing) (format "%S" thing))
    (t (format "%S" thing))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::reuse-cons (x y x-y)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return (cons x y), or uu::reuse x-y if it is equal to (cons x y). Norvig's."
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
    x-y
    (cons x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::variable-p (x)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) ?\?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::fix-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn comma-prefixed variable designators like ,X / (\, X) into symbols like \?X anywhere in THING
(including improper tails)."
  (cond
    ((atom thing) thing)
    ((and (consp thing) (eq '\, (car thing)) (cdr thing) (not (cddr thing)) (symbolp (cadr thing)))
      (intern (concat "?" (symbol-name (cadr thing)))))
    (t (cons (uu::fix-variables (car thing)) (uu::fix-variables (cdr thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::equivalent-bindings? (bindings1 bindings2)
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
            (unless (uu::variable-p  (car pair))
              (error "malformed bindings, cars in each pair should be variables: %s" pair))
            (let ( (assoc  (assoc  (car pair) (cadr ordering)))
                   (rassoc (rassoc (car pair) (cadr ordering))))
              (cond
                ((and assoc  (equal (cdr assoc)  (cdr pair))))
                ((and rassoc (equal (car rassoc) (cdr pair))))
                (t (throw 'not-equivalent nil))))))
        t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *uu:test*
  (confirm that
    (uu::equivalent-bindings?
      (uu::fix-variables '(((\, c) . 1) ((\, b) . (\, a))))
      (uu::fix-variables '(((\, c) . 1) ((\, a) . (\, b)))))
    returns t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::subst-bindings (bindings x)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Substitute the value of variables in bindings into x,
 taking recursively bound variables into account. Norvig's with tweaks."
  ;; (uu::prndiv)
  ;; (debug bindings x)
  ;; (uu::prn "x:        %s" x)
  (let
    ((res 
       (cond
         ((null bindings)
           ;; (uu::prn "case 1")
           nil)
         ((eq bindings t)
           ;; (uu::prn "case 2")
           x)
         ((and (uu::variable-p x) (assoc x bindings))
           ;; (uu::prn "case 3")
           (with-indentation (uu::subst-bindings bindings (cdr (assoc x bindings)))))
         ((atom x) x)
         (t
           ;; (uu::prn "case 4")
           (uu::reuse-cons
             (with-indentation (uu::subst-bindings bindings (car x)))
             (with-indentation (uu::subst-bindings bindings (cdr x)))
             x)))))
    ;; (uu::prn "result:   %s" res)
    ;; (uu::prndiv)
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::occurs (bindings var expr)
  "Doe VAR occur anywhere inside EXPR?"
  (uu::prndiv ?\-)
  (uu::prn "check if %s occurs in %s" (uu::style var) (uu::style expr))
  (debug nil bindings var expr)
  (let
    ((res
       (if (uu::variable-p expr)
         (cond
           ((and (uu::variable-p var) (eq (cadr expr) (cadr var))))
           ((assoc expr bindings)
             (with-indentation
               (uu::prndiv ?\-)
               (uu::occurs bindings var (cdr (assoc expr bindings)))))
           (t nil))
         (cond
           ((consp expr) (or
                           (with-indentation
                             (uu::prndiv ?\-)
                             (uu::occurs bindings var (car expr)))
                           (with-indentation
                             (uu::prndiv ?\-)
                             (uu::occurs bindings var (cdr expr)))))
           ((eq var expr))))))
    (uu::prn "%s %s in %s"
      (uu::style var)
      (if res "         occurs" "DOES NOT occur ")
      (uu::style expr))
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::occurs (bindings var expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Does VAR occur anywhere inside EXPR according to BINDINGS?"
  (cond
    ((eq var expr) t)
    ((and (uu::variable-p expr) (assoc expr bindings))
      (n::occurs-check bindings var (cdr (assoc expr bindings))))
    ((consp expr)
      (or (n::occurs-check bindings var (car expr))
        (n::occurs-check bindings var (cdr expr))))
    (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *uu:test*
  (confirm that
    (uu::occurs
      (uu::fix-variables '(((\, x) f (\, y))))
      (uu::fix-variables '(\, y))
      (uu::fix-variables '(f (\, x))))
    returns t)
  (confirm that (uu::occurs nil 'x '(f (\, x))) returns t)
  (confirm that (uu::occurs '(((\, x) f (\, y))) 'x '(f (\, x))) returns t)
  (confirm that (uu::occurs '(((\, x) f (\, y))) 'z '(f (\, x))) returns nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::unify-variable-with-value1 (bindings pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: when this recurses, it might swap which tail was PAT1 and which was PAT2, but that seems to be fine."
  (let* ( (variable (car pat1))
          (value    (car pat2))
          (binding  (assoc variable bindings)))
    (uu::prn "try to unify the variable %s and with the value %s."
      (uu::style variable)
      (uu::style value))
    (cond
      ((and *uu:occurs-check* (uu::occurs bindings variable value))
        (uu::prn "occurs check failed, not unifying.")
        (when (eq *uu:occurs-check* :soft)
          (uu::prn ":soft occurs check enabled, skip binding but continue unifying...")
          (uu::unify1 bindings (cdr pat1) (cdr pat2))))
      ((and binding (not (equal value (cdr binding))))
        (uu::prn "variable %s is already bound to a different value, %s."
          (uu::style variable)
          (uu::style (cdr binding)))
        (if (not (uu::variable-p (cdr binding)))
          nil
          (uu::unify-variable-with-value1 bindings (cons (cdr binding) (cdr pat1)) pat2)))
      (binding
        (uu::prn "variable %s is already bound to the same value, %s."
          (uu::style variable)
          (uu::style (cdr binding)))
        (with-indentation (uu::unify1 bindings (cdr pat1) (cdr pat2))))
      (t
        (uu::prn "binding variable %s to value %s." (uu::style variable)
          (uu::style value))
        (with-indentation
          (uu::unify1 (cons (cons variable value) bindings) (cdr pat1)
            (cdr pat2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::unify-variable-with-variable2 (bindings pat1 pat2)
  ;; if we entered this fun we already know that (car pat1) is unbound and that (car pat2) is a
  ;; bound variable.
  (let ((binding2 (assoc (car pat2) bindings)))
    (if (and (uu::variable-p (cdr binding2)))
      (progn
        (uu::prn "avoid circular binding, unify %s with %s's value %s instead!"
          (uu::style (car pat1))
          (uu::style (car pat2))
          (uu::style (cdr binding2)))
        (with-indentation
          (uu::unify1 bindings pat1 (cons (cdr binding2) (cdr pat2))))
        ;; (uu::unify1 bindings (cdr pat1) (cdr pat2))
        )
      (uu::prn "variable %s is unbound, unifying it with %s."
        (uu::style (car pat1))
        (uu::style (car pat2)))
      (with-indentation
        (uu::unify1 (cons (cons (car pat1) (car pat2)) bindings) (cdr pat1) (cdr pat2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::unify1 (bindings pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (uu::unify1 nil '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (uu::prndiv)
  (uu::prn "pat1:     %S" pat1)
  (uu::prn "pat2:     %S" pat2)
  (uu::prn "bindings: %S" bindings)
  (uu::prndiv ?\-)
  ;; uncurl tails:
  (let ((pat1-tail-type
          (cond
            ((and pat1 (atom pat1)) :tail)
            ;; ((and (eq (car pat1) '\,) (not (cddr pat1))) :wayward-comma)
            ))
         (pat2-tail-type
           (cond
             ((and pat2 (atom pat2)) :tail)
             ;; ((and (eq (car pat2) '\,) (not (cddr pat2))) :wayward-comma)
             )))
    (uu::prn "tt1:     %S" pat1-tail-type)
    (uu::prn "tt2:     %S" pat2-tail-type)
    (when (and pat1-tail-type pat2-tail-type)      
      (cond
        ((eq pat1-tail-type :tail) (setf pat1 (list pat1)))
        ((eq pat1-tail-type :wayward-comma) (setf pat1 (list (list '\, (cadr pat1))))))
      (cond
        ((eq pat2-tail-type :tail) (setf pat2 (list pat2)))
        ((eq pat2-tail-type :wayward-comma) (setf pat2 (list (list '\, (cadr pat2))))))
      (uu::prn "pat1*:    %S" pat1)
      (uu::prn "pat2*:    %S" pat2)
      ))
  
  (cond
    ;;----------------------------------------------------------------------------------------------
    ((not (or pat1 pat2))
      (uu::prn "PAT1 and PAT2 both ran out.")
      (or bindings t))
    ;;----------------------------------------------------------------------------------------------
    ((not pat1)
      (uu::prn "PAT1 ran out.")
      nil)
    ;;----------------------------------------------------------------------------------------------
    ((not pat2)
      (uu::prn "PAT2 ran out.")
      nil)
    ;;----------------------------------------------------------------------------------------------
    ;; equal atoms:
    ((and (atom (car pat1)) (atom (car pat2)) (eql (car pat1) (car pat2)))
      (uu::prn "%s and %s are eql atoms." (uu::style (car pat1)) (uu::style (car pat2)))
      (with-indentation (uu::unify1 bindings (cdr pat1) (cdr pat2))))
    ;;----------------------------------------------------------------------------------------------
    ;; same variable:
    ((and (uu::variable-p (car pat1)) (uu::variable-p (car pat2))
       (eq (car pat1) (car pat2)))
      (uu::prn "%s and %s are the same variable." (uu::style (car pat1)) (uu::style (car pat2)))
      (with-indentation (uu::unify1 bindings (cdr pat1) (cdr pat2))))
    ;;----------------------------------------------------------------------------------------------
    ;; both different variables:
    ((and (uu::variable-p (car pat1)) (uu::variable-p (car pat2)))
      (uu::prn "both PAT1 elem %s and PAT2 elem %s are variables."
        (uu::style (car pat1))
        (uu::style (car pat2))
        )
      (let ( (binding1 (assoc (car pat1) bindings))
             (binding2 (assoc (car pat2) bindings)))
        (uu::prn "%s = %s" (uu::style (car pat1))
          (if binding1 (uu::style (cdr binding1)) "<unbound>"))
        (uu::prn "%s = %s" (uu::style (car pat2))
          (if binding2 (uu::style (cdr binding2)) "<unbound>"))
        (cond
          ;;-----------------------------------------------------------------------------------------
          ;; bound to un-`equal' values:
          ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
            (uu::prn "already bound to different terms.")
            nil)
          ;;-----------------------------------------------------------------------------------------
          ;; neither bound:
          ((not (or binding1 binding2))
            (uu::prn "both unbound.")
            (with-indentation
              (uu::unify1 (cons (cons (car pat1) (car pat2)) bindings) (cdr pat1) (cdr pat2))))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1's var not bound, unify with PAT2's var:
          ((not binding1)
            ;; (uu::unify-variable-with-value1 pat1 (car pat1) (car pat2) pat2 bindings)
            (uu::unify-variable-with-variable2 bindings pat1 pat2)
            )
          ;;-----------------------------------------------------------------------------------------
          ;; PAT2's var not bound, unify with PAT1's var:
          ((not binding2)
            ;; (uu::unify-variable-with-value1 pat2 (car pat2) (car pat1) pat1 bindings)
            (uu::unify-variable-with-variable2 bindings pat2 pat1)
            )
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1 and PAT2's vars are bound to the same value, unify them destructively.
          ;; not totally sure about this but it seems to work, so far.
          (t (uu::prn
               (concat "PAT1's var %s and PAT2's var %s are bound to the same value, "
                 "unifying them destructively.")
               (uu::style (car pat1)) (uu::style (car pat2)))
            (setcdr binding1 (car binding2))
            (with-indentation (uu::unify1 bindings (cdr pat1) (cdr pat2)))))))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT1, value on PAT2:
    ((and (uu::variable-p (car pat1)) (or *uu:bind-conses* (atom (car pat2))))
      (uu::unify-variable-with-value1 bindings pat1 pat2))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT2, value on PAT1:
    ((and (uu::variable-p (car pat2)) (or *uu:bind-conses* (atom (car pat1))))
      (uu::unify-variable-with-value1 bindings pat2 pat1))
    ;;----------------------------------------------------------------------------------------------
    (t nil (uu::prn "unhandled"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *uu:test*
  (confirm that (uu::unify1 nil (uu::fix-variables '(,x + 1))
                  (uu::fix-variables '(2 + 1)))
    returns ((\?x . 2)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,w ,x ,y))
                  (uu::fix-variables '(,x ,y ,w)))
    returns ((\?x . \?y) (\?w . \?x))) 
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,w ,x ,y ,z))
                  (uu::fix-variables '(,x ,y ,z ,w)))
    returns ((\?y . \?z) (\?x . \?y) (\?w . \?x)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(333 + ,x))
                  (uu::fix-variables '(,x + 333)))
    returns ((\?x . 333)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(2 + 1))
                  (uu::fix-variables '(2 + 1)))
    returns t)
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(2 + 1))
                  (uu::fix-variables '(,x + 1)))
    returns ((\?x . 2)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,y + 1))
                  (uu::fix-variables '(,x + ,x)))
    returns ((\?x . 1) (\?y . \?x)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x + 1))
                  (uu::fix-variables '(2 + ,y)))
    returns ((\?y . 1) (\?x . 2)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x + 1))
                  (uu::fix-variables '(2 + ,y + 1)))
    returns nil)
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x + 1 + 2))
                  (uu::fix-variables '(2 + ,y + 3)))
    returns nil)
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x + 1 + ,a))
                  (uu::fix-variables '(2 + ,y + ,a)))
    returns ((\?y . 1) (\?x . 2)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x + 1 + ,a))
                  (uu::fix-variables '(2 + ,y + ,b)))
    returns ((\?b . \?a)
              (\?y . 1) (\?x . 2)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x + 1 + ,a))
                  (uu::fix-variables '(2 + ,y + ,b + 3)))
    returns nil)
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x + 1))
                  (uu::fix-variables '(2 + ,x)))
    returns nil)
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x + 1 + ,a + 8))
                  (uu::fix-variables '(2 + ,y + ,a + ,a)))
    returns ((\?a . 8) (\?y . 1) (\?x . 2)))
  ;;---------------------------------------------------------------------------------------------------
  ;; uncurl tails tests:
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x ,y . (,z . ,zz)))
                  (uu::fix-variables '(1 2 . (3 . 4))))
    returns ((\?zz . 4) (\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x ,y . (,z . ,zz)))
                  (uu::fix-variables '(1 2 . (3 . 4))))
    returns ((\?zz . 4) (\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x ,y . ,z))
                  (uu::fix-variables '(1 2 . 3)))
    returns ((\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (uu::unify1 nil
                  (uu::fix-variables '(,x ,y   ,z))
                  (uu::fix-variables '(1 2   3)))
    returns ((\?z . 3) (\?y . 2) (\?x . 1)))
  ;;---------------------------------------------------------------------------------------------------
  ;; occurs check tests:
  (confirm that
    (let ((*uu:occurs-check* nil))
      (uu::unify1 nil
        (uu::fix-variables '(,x ,y))
        (uu::fix-variables '((f ,y) (f ,x)))))
    returns ((\?y f \?x) (\?x f \?y)))
  (confirm that
    (let ((*uu:occurs-check* :soft))
      (uu::unify1 nil
        (uu::fix-variables '(,x ,y))
        (uu::fix-variables '((f ,y) (f ,x)))))
    returns ((\?x f \?y)))
  (confirm that
    (let ((*uu:occurs-check* t))
      (uu::unify1 nil
        (uu::fix-variables '(,x ,y))
        (uu::fix-variables '((f ,y) (f ,x)))))
    returns nil)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::unify (thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Unidy THING1 and THING2 and produce an alist of bindings."
  (uu::unify1 nil (uu::fix-variables thing1) (uu::fix-variables thing2))
  ;; (uu::unify1 nil thing1 thing2)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *uu:test*
  (confirm that (uu::unify '(,w ,x ,y) '(,x ,y ,w))
    returns ((\?x . \?y) (\?w . \?x)))
  (confirm that (uu::unify '(,w ,x ,y ,z) '(,x ,y ,z ,w))
    returns ((\?y . \?z) (\?x . \?y) (\?w . \?x)))
  (confirm that (uu::unify '(333 + ,x + ,x) '(,z + 333 + ,z))
    returns ((\?x . \?z) (\?z . 333)))
  (confirm that (uu::unify '(333 + ,x) '(,x + 333))
    returns ((\?x . 333)))
  (confirm that (uu::unify '(2 + 1) '(2 + 1))
    returns t)
  (confirm that (uu::unify '(,x + 1) '(2 + 1))
    returns ((\?x . 2)))
  (confirm that (uu::unify '(2 + 1) '(,x + 1))
    returns ((\?x . 2)))
  (confirm that (uu::unify '(,y + 1) '(,x + ,x))
    returns ((\?x . 1) (\?y . \?x)))
  (confirm that (uu::unify '(,x + 1) '(2 + ,y))
    returns ((\?y . 1) (\?x . 2)))
  (confirm that (uu::unify '(,x + 1) '(2 + ,y + 1))
    returns nil)
  (confirm that (uu::unify '(,x + 1 + 2) '(2 + ,y + 3))
    returns nil)
  (confirm that (uu::unify '(,x + 1 + ,a) '(2 + ,y + ,a))
    returns ((\?y . 1) (\?x . 2)))
  (confirm that (uu::unify '(,x + 1 + ,a) '(2 + ,y + ,b))
    returns ((\?b . \?a) (\?y . 1) (\?x . 2)))
  (confirm that (uu::unify '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
    returns nil)
  (confirm that (uu::unify '(,x + 1) '(2 + ,x))
    returns nil)
  (confirm that (uu::unify '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))
    returns ((\?a . 8) (\?y . 1) (\?x . 2)))
  ;;---------------------------------------------------------------------------------------------------
  ;; uncurl tails tests:
  (confirm that (uu::unify '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
    returns ((\?zz . 4) (\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (uu::unify '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
    returns ((\?zz . 4) (\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (uu::unify '(,x ,y . ,z) '(1 2 . 3))
    returns ((\?z . 3) (\?y . 2) (\?x . 1)))
  (confirm that (uu::unify '(,x ,y   ,z) '(1 2   3))
    returns ((\?z . 3) (\?y . 2) (\?x . 1)))
  ;;---------------------------------------------------------------------------------------------------
  ;; occurs check tests:
  (confirm that
    (let ((*uu:occurs-check* nil))
      (uu::unify '(,x ,y) '((f ,y) (f ,x))))
    returns ((\?y f \?x) (\?x f \?y)))
  (confirm that
    (let ((*uu:occurs-check* :soft))
      (uu::unify '(,x ,y) '((f ,y) (f ,x))))
    returns ((\?x f \?y)))
  (confirm that
    (let ((*uu:occurs-check* t))
      (uu::unify '(,x ,y) '((f ,y) (f ,x))))
    returns nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu::simplify-bindings (bindings)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Simplify BINDINGS by finding anything bound directly to another variable and rebinding it to what that variable is
bound to."
  (uu::prn "simplifying       %s." bindings)
  (dolist (pair1 bindings bindings)
    (dolist (pair2 bindings)
      (when (equal (cdr pair2) (car pair1))
        (setcdr pair2 (cdr pair1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *uu:test*
  (confirm that
    (uu::simplify-bindings (uu::unify1 nil
                             (uu::fix-variables '(,v ,u ,w ,x))
                             (uu::fix-variables '(,u ,x ,v 333))))
    returns ((\?x . 333) (\?w . 333) (\?u . 333) (\?v . 333))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uu:unifier (pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "My version of Norvig's `n:unifier'."
  (let* ( (bindings (uu::unify pat1 pat2))
          (bindings (if *uu:unifier-simplifies*
                      (uu::simplify-bindings bindings)
                      bindings)))
    (uu::prn "unified bindings: %s" bindings)
    (let ((expr (uu::subst-bindings bindings (uu::fix-variables pat1))))
      (uu::prn "substituted expr: %s" expr)
      (uu::prndiv)
      (uu::prnl)
      expr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *uu:test*
  (confirm that (uu:unifier '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
    returns (2 + 1 + 2 + 2))
  (confirm that (uu:unifier '(,x + 1 + ,a) '(2 + ,y + ,a))
    returns (2 + 1 + \?a))
  (confirm that (uu:unifier '(333 + ,x + ,x) '(,z + 333 + ,z))
    returns (333 + 333 + 333))
  (confirm that
    (uu:unifier '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))
    returns (2 + 1 + 8 + 8))
  (confirm that
    (let ( (*uu:verbose* t)
           (*uu:unifier-simplifies* nil))
      (uu:unifier '(,v ,u ,w ,x) '(,u ,x ,v 333)))
    returns (333 333 333 333))
  (confirm that
    (let ( (*uu:verbose* t)
           (*uu:unifier-simplifies* t))
      (uu:unifier '(,v ,u ,w ,x) '(,u ,x ,v 333)))
    returns (333 333 333 333))
  ;; uncurl tails:
  (confirm that (uu:unifier '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
    returns (1 2 3 . 4))
  (confirm that (uu:unifier '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
    returns (1 2 3 . 4))
  (confirm that (uu:unifier '(,x ,y . ,z) '(1 2 . 3))
    returns (1 2 . 3))
  (confirm that (uu:unifier '(,x ,y   ,z) '(1 2   3))
    returns (1 2 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ignore!
  (let ((reps 5000))
    (list
      (benchmark-run reps
        (let ( (*uu:verbose* nil)
               (*uu:unifier-simplifies* nil))
          (uu:unifier 
            '(,u ,v ,w ,x)
            '(,x ,u ,v 333)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--unification)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; junk:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (trace-function #'uu:unifier)
    (trace-function #'uu::unify1)
    (trace-function #'uu::reuse-cons)
    (trace-function #'uu::occurs)
    (trace-function #'uu::unify-variable-with-value1)
    (trace-function #'uu::unify-variable-with-variable2)
    (trace-function #'uu::simplify-bindings)
    (trace-function #'uu::subst-bindings))

  (untrace-all)

  (let ((*uu:verbose* t))
    (uu:unifier '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2)))

  (let ((*uu:verbose* t))
    (uu:unifier '(333 + ,x + ,x) '(,z + 333 + ,z)))

  (let ((reps 1000))
    (benchmark-run reps
      (let ((*uu:verbose* nil)) (uu:unifier '(333 + ,x + ,x) '(,z + 333 + ,z)))))

  (uu::unify1 nil '(,x ,x) '(,y ,y))
  (uu::unify1 nil '(,x ,y a) '(,y ,x ,x))
  (uu::unify1 nil '(,x ,y) '(,y ,x)) ;; => ((,X . ,Y))
  (uu::unify1 nil '(,x ,y a) '(,y ,x ,x)) ;; => ((,Y . A) (,X . ,Y))

  (uu::unify1 nil '(,y ,x) '(7 (f ,x)))
  (uu:unifier '(,y ,x) '(7 (f ,x)))
  (uu:unifier '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))

  (let ((*uu:occurs-check* nil))
    (uu::unify1 nil '(,x ,y) '((f ,y) (f ,x)))) ; (((\, y) f (\, x)) ((\, x) f (\, y)))
  (let ((*uu:occurs-check* :soft))
    (uu::unify1 nil '(,x ,y) '((f ,y) (f ,x)))) ; (((\, x) f (\, y)))
  (let ((*uu:occurs-check* t))
    (uu::unify1 nil '(,x ,y) '((f ,y) (f ,x)))) ; nil

  (variable variable) ; bind var...
  (variable atom)     ; bind var...
  (atom     variable) ; bind var...
  (variable cons)     ; ???
  (cons     variable) ; ???
  (cons     cons)     ; recurse!
  (atom     cons)     ; FAIL.
  (cons     atom)     ; FAIL.
  (atom     atom)     ; `eql'.
  )





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
    ((and (uu::variable-p expr) (assoc expr bindings))
      (n::subst-bindings bindings (cdr (assoc expr bindings))))
    ((atom expr) expr)
    (t (uu::reuse-cons (n::subst-bindings bindings (car expr))
         (n::subst-bindings bindings (cdr expr)) expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::occurs-check (bindings var expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Does VAR occur anywhere inside EXPR according to BINDINGS?"
  (cond ((eq var expr) t)
    ((and (uu::variable-p expr) (assoc expr bindings))
      (n::occurs-check bindings var (cdr (assoc expr bindings))))
    ((consp expr)
      (or (n::occurs-check bindings var (car expr))
        (n::occurs-check bindings var (cdr expr))))
    (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::unify-variable (bindings var expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Unify VAR with EXPR, using (and maybe extending) BINDINGS."
  (cond
    ((assoc var bindings)
      (n::unify1 bindings (cdr (assoc var bindings)) expr))
    ((and (uu::variable-p expr) (assoc expr bindings))
      (n::unify1 bindings var (cdr (assoc expr bindings))))
    ((and *n:occurs-check* (n::occurs-check bindings var expr))
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
      ((uu::variable-p thing1)
        ;; (debug nil 3)
        (n::unify-variable bindings thing1 thing2))
      ((uu::variable-p thing2)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::unify (thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Unidy THING1 and THING2 and produce an alist of bindings."
  (n::unify1 nil (uu::fix-variables thing1) (uu::fix-variables thing2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n:unifier (thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return something that unifies with both THING1 and THING2 (or n::fail)."
  (n::subst-bindings (n::unify thing2 thing1) (uu::fix-variables thing2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(n::unify1 nil '(x) '(11))

;; (trace-function #'n::unify)
;; (trace-function #'n::unify1)
;; (trace-function #'n:unifier)
;; (trace-function #'n::extend-bindings3)
;; (trace-function #'uu::fix-variables)
;; (trace-function #'n::subst-bindings)
;; (untrace-all)

(n::unify1 nil 'x 11) 
(n::unify1 nil '(x y z) '(11 22 33)) 

(n::unify1 nil (uu::fix-variables '(,x ,y ,z)) '(11 22 33))
(n::unify1 nil (uu::fix-variables '(,x ,y ,z)) (uu::fix-variables '(,x ,y ,z)))

(n::unify1 nil  (uu::fix-variables '(,x ,y (,z 8 ,b . ,c))) '(1 2 (3 8 4 . 5)))
(uu::variable-p (car (uu::fix-variables '(,x ,y (,z 8 ,b . \?c)))))

(n::unify
  '(,u ,v ,w ,x)
  '(,x ,u ,v 333))

(n::unify1
  nil
  (uu::fix-variables '(,u ,v ,w ,x))
  (uu::fix-variables '(,x ,u ,v 333)))

(n::unify
  '((,a * ,x ^ 2) + (,b * ,x) + ,c)
  '(,lz + (4 * 5) + 3))

(uu::unify1 nil '(,w ,x ,y ,z) '(,x ,y ,z ,w))
(n::unify  '(,w ,x ,y ,z) '(,x ,y ,z ,w))

(n:unifier
  '((,a * ,x ^ 2) + (,b * ,x) + ,c)
  '(,z + (4 * 5) + 3))

(n::unify
  '((,a * ,x ^ 2) + (,b * ,x) + ,c)
  '(,z + (4 * 5) + 3))
_
