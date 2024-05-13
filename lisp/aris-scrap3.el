;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--destructuring-match-aux)
(require 'aris-funs--trees)
(require 'trace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: `u::unify1's circular binding avoidance and use of the occurs check isn't up to snuff
;; compared to `u::unify2'.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::style (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    ((atom thing) (format-message "`%S'" thing))
    ((dm::pat-elem-is-a-variable? thing) (format "%S" thing))
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
         ((and (dm::pat-elem-is-a-variable? x) (assoc x bindings))
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
(defun u::occurs (var expr bindings)
  "Doe VAR occur anywhere inside EXPR?"
  (u::prndiv ?\-)
  (u::prn "check if %s occurs in %s" (u::style var) (u::style expr))
  (let
    ((res
       (if (dm::pat-elem-is-a-variable? expr)
         (cond
           ((and (dm::pat-elem-is-a-variable? var) (eq (cadr expr) (cadr var))))
           ((assoc expr bindings)
             (with-indentation (u::occurs var (cdr (assoc expr bindings)) bindings)))
           (t nil))
         (cond
           ((consp expr) (or
                           (with-indentation
                             (u::prndiv ?\-)
                             (u::occurs var (car expr) bindings))
                           (with-indentation
                             (u::prndiv ?\-)
                             (u::occurs var (cdr expr) bindings))))
           ((eq var expr))))))
    (u::prn "%s %s in %s"
      (u::style var)
      (if res "         occurs" "DOES NOT occur ")
      (u::style expr))
    (u::prndiv ?\-)
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (u::unify2 '(,x ,y) '((f ,y) (f ,x)) nil)
(confirm that (u::occurs '(\, y) '(f (\, x)) '(((\, x) f (\, y)))) returns t)
(u::occurs 'x '(f (\, x)) nil)
(u::occurs 'x '(f (\, x)) '(((\, x) f (\, y))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro u::unify-variable-with-value (variable-pat value-pat)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Not portable: expects BINDING in surrounding env."
  (let ( (variable-pat-name (upcase (symbol-name variable-pat)))
         (value-pat-name    (upcase (symbol-name value-pat))))
    `(progn
       (u::prn "%s elem is a variable" ,variable-pat-name)
       (let ((binding (assoc (car ,variable-pat) bindings)))
         (u::prn "%s elem is the variable %s and %s elem is the value %s."
           ,variable-pat-name
           (u::style (car ,variable-pat))
           ,value-pat-name
           (u::style (car ,value-pat)))
         (cond
           ((and binding (not (eql (car ,value-pat) (cdr binding)))) ;; should this be `equal'?
             ;; ADD OCCURS CHECK!
             (u::prn "%s elem %s is already bound to a different value, %s."
               ,variable-pat-name
               (u::style (car ,variable-pat))
               (u::style (cdr binding)))
             (throw 'not-unifiable nil))
           (binding
             (u::prn "%s elem %s is already bound to the same value, %s."
               ,variable-pat-name
               (u::style (car ,variable-pat))
               (u::style (cdr binding))))
           ((not (or *u:bind-conses* (atom (car ,value-pat))))
             (throw 'not-unifiable nil))
           (t
             (u::prn "binding variable %s to atom %s."
               (u::style (car ,variable-pat))
               (u::style (car ,value-pat)))
             (push (cons (car ,variable-pat) (car ,value-pat)) bindings)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify1 (pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (u::unify1 '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (let (bindings)
    (catch 'not-unifiable
      (while (and pat1 pat2)
        (u::prndiv)
        (u::prn "pat1: %s" pat1)
        (u::prn "pat2: %s" pat2)
        (let ( (pat1-elem (car pat1))
               (pat2-elem (car pat2)))
          (u::prn "pat1-elem: %s" pat1-elem)
          (u::prn "pat2-elem: %s" pat2-elem)
          (u::prndiv ?\-)
          ;;-----------------------------------------------------------------------------------------
          (cond
            ;;---------------------------------------------------------------------------------------
            ;; same variable:
            ((and (dm::pat-elem-is-a-variable? pat1-elem)
               (dm::pat-elem-is-a-variable? pat2-elem)
               (eq (dm::pat-elem-var-sym pat1-elem)
                 (dm::pat-elem-var-sym pat2-elem)))
              (u::prn "%s and %s are the same variable." (u::style pat1-elem) (u::style pat2-elem)))
            ;;---------------------------------------------------------------------------------------
            ;; both different variables:
            ((and (dm::pat-elem-is-a-variable? pat1-elem) (dm::pat-elem-is-a-variable? pat2-elem))
              (u::prn "both PAT1 elem and PAT2 elem are variables.")
              (let ( (binding1 (assoc pat1-elem bindings))
                     (binding2 (assoc pat2-elem bindings)))
                (u::prn "%s = %s" (u::style pat1-elem)
                  (if binding1 (u::style (cdr binding1)) "<unbound>"))        
                (u::prn "%s = %s" (u::style pat2-elem)
                  (if binding2 (u::style (cdr binding2)) "<unbound>"))
                (cond
                  ;;-----------------------------------------------------------------------------------------
                  ;; bound to un-`equal' values:
                  ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
                    (u::prn "already bound to different terms")
                    (throw 'not-unifiable nil))
                  ;;-----------------------------------------------------------------------------------------
                  ;; neither bound:
                  ((not (or binding1 binding2))
                    (u::prn "both unbound.")
                    (push (cons pat1-elem pat2-elem) bindings))
                  ;;-----------------------------------------------------------------------------------------
                  ;; PAT1's var not bound, unify with PAT2's var:
                  ((not binding1)
                    (u::prn "PAT1 elem %s is unbound, unifying it with %s."
                      (u::style (car pat1))
                      (u::style (car binding2)))
                    (push (cons pat1-elem pat2-elem) bindings))
                  ;;-----------------------------------------------------------------------------------------
                  ;; PAT2's var not bound, unify with PAT1's var:
                  ((not binding2)
                    (u::prn "PAT2 elem %s is unbound, unifying it with %s."
                      (u::style (car pat2))
                      (u::style (car binding1)))
                    (push (cons pat2-elem pat1-elem) bindings))
                  ;;-----------------------------------------------------------------------------------------
                  ;; PAT1 and PAT2's vars are bound to the same value, unify them destructively.
                  ;; not totally sure about this but it seems to work, so far.
                  (t
                    (u::prn
                      (concat "pat1-elem and pat2-elem are bound to the same value, "
                        "unifying destructively."))
                    (setcdr binding1 (car binding2))))))
            ;;----------------------------------------------------------------------------------------------
            ;; variable on PAT1, value on PAT2:
            ((dm::pat-elem-is-a-variable? pat1-elem)
              (u::unify-variable-with-value pat1 pat2))
            ;;----------------------------------------------------------------------------------------------
            ;; variable on PAT2, value on PAT1:
            ((dm::pat-elem-is-a-variable? pat2-elem)
              (u::unify-variable-with-value pat2 pat1))
            ((equal pat1-elem pat2-elem)
              (u::prn "pat1-elem and pat2-elem are equal"))
            (t (u::prn "pat1-elem and pat2-elem are not equal")            
              (throw 'not-unifiable nil))))
        (pop pat1)
        (pop pat2)
        (u::prn "bindings: %s" bindings)
        ;;(debug pat1 pat2 bindings)
        ))
    (prog1
      (if (or pat1 pat2)
        ;; not unifiable, return no bindings:
        (progn
          (u::prndiv ?\-)
          (u::prn "pat1: %s" pat1)
          (u::prn "pat2: %s" pat2)
          (u::prn "not unifiable, return no bindings")
          nil)
        (or bindings t))
      (u::prndiv))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(ignore!
(confirm that (u::unify1 '(333 + ,x + ,x) '(,z + 333 + ,z))
  returns (((\, x) \, z) ((\, z) . 333)))
(confirm that (u::unify1 '(333 + ,x) '(,x + 333))
  returns (((\, x) . 333)))
(confirm that (u::unify1 '(2 + 1) '(2 + 1))
  returns t)
(confirm that (u::unify1 '(,x + 1) '(2 + 1))
  returns (((\, x) . 2)))
(confirm that (u::unify1 '(2 + 1) '(,x + 1))
  returns (((\, x) . 2)))
(confirm that (u::unify1 '(,y + 1) '(,x + ,x))
  returns (((\, x) . 1) ((\, y) \, x)))
(confirm that (u::unify1 '(,x + 1) '(2 + ,y))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify1 '(,x + 1) '(2 + ,y + 1))
  returns nil)
(confirm that (u::unify1 '(,x + 1 + 2) '(2 + ,y + 3))
  returns nil)
(confirm that (u::unify1 '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify1 '(,x + 1 + ,a) '(2 + ,y + ,b))
  returns (((\, a) \, b) ((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify1 '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
  returns nil)
(confirm that (u::unify1 '(,x + 1) '(2 + ,x))
  returns nil) ;; )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify-variable-with-value2 (pat1 pat2 bindings)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: when this recurses, it might swap which tail was PAT1 and which was PAT2, but that seems to be fine."
  (let* ( (variable (car pat1))
          (value    (car pat2))
          (binding  (assoc variable bindings)))
    (u::prn "try to unify the variable %s and with the value %s."
      (u::style variable)
      (u::style value))
    (cond
      ((and *u:occurs-check* (u::occurs variable value bindings))
        (u::prn "occurs check failed, not unifying.")
        (when (eq *u:occurs-check* :soft)
          (u::prn ":soft occurs check enabled, skip binding but continue unifying...")
          ;; (debug)
          (u::unify2 (cdr pat1) (cdr pat2) bindings)))
      ((and binding (not (eql value (cdr binding))))
        (u::prn "variable %s is already bound to a different value, %s."
          (u::style variable)
          (u::style (cdr binding)))
        (if (not (dm::pat-elem-is-a-variable? (cdr binding)))
          nil
          ;; (debug (cdr binding) variable value pat1 pat2 bindings)
          (u::unify-variable-with-value2 (cdr binding) value pat1 pat2 bindings)))
      (binding
        (u::prn "variable %s is already bound to the same value, %s."
          (u::style variable)
          (u::style (cdr binding)))
        (with-indentation (u::unify2 (cdr pat1) (cdr pat2) bindings)))
      (t
        (u::prn "binding variable %s to value %s." (u::style variable)
          (u::style value))
        (with-indentation
          (u::unify2 (cdr pat1) (cdr pat2)
            (cons (cons variable value) bindings)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify-variable-with-variable (pat1 pat2 bindings)
  ;; if we entered this fun we already know that (car pat1) is unbound and that (car pat2) is a
  ;; bound variable.
  (let ((binding2 (assoc (car pat2) bindings)))
    (if (and (dm::pat-elem-is-a-variable? (cdr binding2))
          ;; (u::occurs (car pat1) (cdr binding2) bindings)
          )
      (progn
        (u::prn "avoid circular binding, unify %s with %s's value %s instead!"
          (u::style (car pat1))
          (u::style (car pat2))
          (u::style (cdr binding2)))
        (with-indentation
          (u::unify2 pat1 (cons (cdr binding2) (cdr pat2)) bindings))
        ;; (u::unify2 (cdr pat1) (cdr pat2) bindings)
        )
      (u::prn "variable %s is unbound, unifying it with %s."
        (u::style (car pat1))
        (u::style (car pat2)))
      (with-indentation
        (u::unify2 (cdr pat1) (cdr pat2) (cons (cons (car pat1) (car pat2)) bindings))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify2 (pat1 pat2 bindings)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A tail recursive variation on `u::unify1'. Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (u::unify2 '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (u::prndiv)
  (u::prn "pat1:     %S" pat1)
  (u::prn "pat2:     %S" pat2)
  (u::prn "bindings: %S" bindings)
  (u::prndiv ?\-)
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
    ;; equal atoms:
    ((and (atom (car pat1)) (atom (car pat2)) (eql (car pat1) (car pat2)))
      (u::prn "%s and %s are eql atoms." (u::style (car pat1)) (u::style (car pat2)))
      (with-indentation (u::unify2 (cdr pat1) (cdr pat2) bindings)))
    ;;----------------------------------------------------------------------------------------------
    ;; same variable:
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (dm::pat-elem-is-a-variable? (car pat2))
       (eq (dm::pat-elem-var-sym (car pat1)) (dm::pat-elem-var-sym (car pat2))))
      (u::prn "%s and %s are the same variable." (u::style (car pat1)) (u::style (car pat2)))
      (with-indentation (u::unify2 (cdr pat1) (cdr pat2) bindings)))
    ;;----------------------------------------------------------------------------------------------
    ;; both different variables:
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (dm::pat-elem-is-a-variable? (car pat2)))
      (u::prn "both PAT1 elem %s and PAT2 elem %s are variables."
        (u::style (car pat1))
        (u::style (car pat2))
        )
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
              (u::unify2 (cdr pat1) (cdr pat2) (cons (cons (car pat1) (car pat2)) bindings))))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1's var not bound, unify with PAT2's var:
          ((not binding1)
            ;; (u::unify-variable-with-value2 (car pat1) (car pat2) pat1 pat2 bindings)
            (u::unify-variable-with-variable pat1 pat2 bindings)
            )
          ;;-----------------------------------------------------------------------------------------
          ;; PAT2's var not bound, unify with PAT1's var:
          ((not binding2)
            ;; (u::unify-variable-with-value2 (car pat2) (car pat1) pat2 pat1 bindings)
            (u::unify-variable-with-variable pat2 pat1 bindings)
            )
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1 and PAT2's vars are bound to the same value, unify them destructively.
          ;; not totally sure about this but it seems to work, so far.
          (t (u::prn
               (concat "PAT1's var %s and PAT2's var %s are bound to the same value, "
                 "unifying them destructively.")
               (u::style (car pat1)) (u::style (car pat2)))
            (setcdr binding1 (car binding2))
            (with-indentation (u::unify2 (cdr pat1) (cdr pat2) bindings))))))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT1, value on PAT2:
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (or *u:bind-conses* (atom (car pat2))))
      (u::unify-variable-with-value2 pat1 pat2 bindings))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT2, value on PAT1:
    ((and (dm::pat-elem-is-a-variable? (car pat2)) (or *u:bind-conses* (atom (car pat1))))
      (u::unify-variable-with-value2 pat2 pat1 bindings))
    ;;----------------------------------------------------------------------------------------------
    (t nil (ignore! (error "unhandled")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (u::unify2 '(,w ,x ,y) '(,x ,y ,w) nil)
  returns (((\, x) \, y) ((\, w) \, x)))
(confirm that (u::unify2 '(,w ,x ,y ,z) '(,x ,y ,z ,w) nil)
  returns (((\, y) \, z) ((\, x) \, y) ((\, w) \, x)))
(confirm that (u::unify2 '(333 + ,x + ,x) '(,z + 333 + ,z) nil)
  returns (((\, x) \, z) ((\, z) . 333)))
(confirm that (u::unify2 '(333 + ,x) '(,x + 333) nil)
  returns (((\, x) . 333)))
(confirm that (u::unify2 '(2 + 1) '(2 + 1) nil)
  returns t)
(confirm that (u::unify2 '(,x + 1) '(2 + 1) nil)
  returns (((\, x) . 2)))
(confirm that (u::unify2 '(2 + 1) '(,x + 1) nil)
  returns (((\, x) . 2)))
(confirm that (u::unify2 '(,y + 1) '(,x + ,x) nil)
  returns (((\, x) . 1) ((\, y) \, x)))
(confirm that (u::unify2 '(,x + 1) '(2 + ,y) nil)
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 '(,x + 1) '(2 + ,y + 1) nil)
  returns nil)
(confirm that (u::unify2 '(,x + 1 + 2) '(2 + ,y + 3) nil)
  returns nil)
(confirm that (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,a) nil)
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,b) nil)
  returns (((\, b) \, a) ((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,b + 3) nil)
  returns nil)
(confirm that (u::unify2 '(,x + 1) '(2 + ,x) nil)
  returns nil)
(confirm that (u::unify2 '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a) nil)
  returns (((\, a) . 8) ((\, y) . 1) ((\, x) . 2)))
;;---------------------------------------------------------------------------------------------------
;; occurs check tests:
(confirm that
  (let ((*u:occurs-check* nil))
    (u::unify2 '(,x ,y) '((f ,y) (f ,x)) nil))
  returns (((\, y) f (\, x)) ((\, x) f (\, y))))
(confirm that
  (let ((*u:occurs-check* :soft))
    (u::unify2 '(,x ,y) '((f ,y) (f ,x)) nil))
  returns (((\, x) f (\, y))))
(confirm that
  (let ((*u:occurs-check* t))
    (u::unify2 '(,x ,y) '((f ,y) (f ,x)) nil))
  returns nil)
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
(confirm that
  (u::simplify-bindings (u::unify2
                          '(,v ,u ,w ,x)
                          '(,u ,x ,v 333) nil))
  returns (((\, x) . 333) ((\, w) . 333) ((\, u) . 333) ((\, v) . 333)))
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
            (unless (dm::pat-elem-is-a-variable? (car pair))
              (error "malformed bindings, cars in each pair should be variables: %s" pair))
            (let ( (assoc  (assoc  (car pair) (cadr ordering)))
                   (rassoc (rassoc (car pair) (cadr ordering))))
              (cond
                ((and assoc  (equal (cdr assoc)  (cdr pair))))
                ((and rassoc (equal (car rassoc) (cdr pair))))
                (t (throw 'not-equivalent nil))))))
        t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (u::equivalent-bindings?
    '(((\, c) . 1) ((\, b) . (\, a))) '(((\, c) . 1) ((\, a) . (\, b))))
  returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comparison tests between `u::unify1' and `u::unify2'.
;; NOTE: `u::unify1' does not have occurs checking yet, so comparisons related to the occurs check
;;   are not included.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ((*u:bind-conses* t))
    (u::equivalent-bindings?
      (u::unify1 '(1 + ,x) '(,y + (2 . 3)))
      (u::unify2 '(1 + ,x) '(,y + (2 . 3)) nil)))
  returns t)
(confirm that
  (let ((*u:bind-conses* nil))
    (u::equivalent-bindings?
      (u::unify1 '(1 + ,x) '(,y + (2 . 3)))
      (u::unify2 '(1 + ,x) '(,y + (2 . 3)) nil)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(333 + ,x + ,x) '(,z + 333 + ,z))
    (u::unify2 '(333 + ,x + ,x) '(,z + 333 + ,z) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(333 + ,x) '(,x + 333))
    (u::unify2 '(333 + ,x) '(,x + 333) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(2 + 1) '(2 + 1))
    (u::unify2 '(2 + 1) '(2 + 1) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1) '(2 + 1))
    (u::unify2 '(,x + 1) '(2 + 1) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(2 + 1) '(,x + 1))
    (u::unify2 '(2 + 1) '(,x + 1) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,y + 1) '(,x + ,x))
    (u::unify2 '(,y + 1) '(,x + ,x) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1) '(2 + ,y))
    (u::unify2 '(,x + 1) '(2 + ,y) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1) '(2 + ,y + 1))
    (u::unify2 '(,x + 1) '(2 + ,y + 1) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1 + 2) '(2 + ,y + 3))
    (u::unify2 '(,x + 1 + 2) '(2 + ,y + 3) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1 + ,a) '(2 + ,y + ,a))
    (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,a) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
    (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,b + 3) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1) '(2 + ,x))
    (u::unify2 '(,x + 1) '(2 + ,x) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))
    (u::unify2 '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a) nil))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 '(,x + 1 + ,a) '(2 + ,y + ,b))
    (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,b) nil))
  returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u:unifier (fun pat1 pat2 &rest rest )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Norvig's  unifier."
  (let* ( (bindings (apply fun pat1 pat2 rest))
          (bindings (if *u:unifier-simplifies*
                      (u::simplify-bindings bindings)
                      bindings)))
    (u::prn "unified bindings: %s" bindings)
    (let ((expr (u::subst-bindings bindings pat1)))
      (u::prn "substituted expr: %s" expr)
      (u::prndiv)
      (u::prnl)
      expr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ignore!
(confirm that (u:unifier #'u::unify1 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
  returns (2 + 1 + 2 + 2))
(confirm that (u:unifier #'u::unify1 '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (2 + 1 + (\, a)))
(confirm that (u:unifier #'u::unify1 '(333 + ,x + ,x) '(,z + 333 + ,z))
  returns (333 + 333 + 333))
(confirm that (u:unifier #'u::unify2 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2) nil)
  returns (2 + 1 + 2 + 2))
(confirm that (u:unifier #'u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,a) nil)
  returns (2 + 1 + (\, a))) ; )
(confirm that (u:unifier #'u::unify2 '(333 + ,x + ,x) '(,z + 333 + ,z) nil)
  returns (333 + 333 + 333))
(confirm that
  (u:unifier #'u::unify2 '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a) nil)
  returns (2 + 1 + 8 + 8))

(ignore!
  (let ((reps 10000))
    (list
      (benchmark-run reps
        (let ( (*u:verbose* nil)
               (*u:unifier-simplifies* nil))
          (u:unifier #'u::unify2
            '(,v ,u ,w ,x)
            '(,u ,x ,v 333) nil)))
      (benchmark-run reps
        (let ( (*u:verbose* nil)
               (*u:unifier-simplifies* t))
          (u:unifier #'u::unify2
            '(,v ,u ,w ,x)
            '(,u ,x ,v 333) nil)))))

  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--unification)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; junk:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (trace-function #'u:unifier)
    (trace-function #'u::unify1)
    (trace-function #'u::unify2)
    (trace-function #'u::reuse-cons)
    (trace-function #'u::occurs)
    (trace-function #'u::unify-variable-with-value2)
    (trace-function #'u::unify-variable-with-variable)
    (trace-function #'u::simplify-bindings)
    (trace-function #'u::subst-bindings))

  (untrace-all)

  (let ((*u:verbose* t))
    (u:unifier #'u::unify2 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2) nil))

  (let ((*u:verbose* t))
    (u:unifier #'u::unify2 '(333 + ,x + ,x) '(,z + 333 + ,z) nil))

  (list
    (benchmark-run 10000 (u:unifier #'u::unify1 '(333 + ,x + ,x) '(,z + 333 + ,z)))
    (benchmark-run 10000 (u:unifier #'u::unify2 '(333 + ,x + ,x) '(,z + 333 + ,z) nil)))

  (u::unify2 '(,x ,x) '(,y ,y) nil)
  (u::unify2 '(,x ,y a) '(,y ,x ,x) nil)
  (u::unify2 '(,x ,y) '(,y ,x) nil) ;; => ((,X . ,Y))
  (u::unify2 '(,x ,y a) '(,y ,x ,x) nil) ;; => ((,Y . A) (,X . ,Y))

  (u::unify2 '(,y ,x) '(7 (f ,x)) nil)
  (u:unifier #'u::unify2 '(,y ,x) '(7 (f ,x)) nil)
  (u:unifier #'u::unify2 '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a) nil)

  (u::unify2 '(,x ,y) '((f ,y) (f ,x)) nil) ;; => ((,y f ,x) (,x f ,y))

  (let ((*u:occurs-check* :soft))
    (u::unify2 '(,x ,y) '((f ,y) (f ,x)) nil))  ;; => (((\, x) f (\, y)))
  )

