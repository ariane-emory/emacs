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
(defun u::occurs (bindings var expr)
  "Doe VAR occur anywhere inside EXPR?"
  (u::prndiv ?\-)
  (u::prn "check if %s occurs in %s" (u::style var) (u::style expr))
  (let
    ((res
       (if (dm::pat-elem-is-a-variable? expr)
         (cond
           ((and (dm::pat-elem-is-a-variable? var) (eq (cadr expr) (cadr var))))
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
      ;; (if res "occurs" "DOES NOT occur ")
      (u::style expr))
    ;; (u::prndiv ?\-)
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (u::occurs '(((\, x) f (\, y))) '(\, y) '(f (\, x))) returns t)
(confirm that (u::occurs nil 'x '(f (\, x))) returns nil)
(confirm that (u::occurs '(((\, x) f (\, y))) 'x '(f (\, x))) returns nil)
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
(defun u::unify1 (bindings pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (u::unify1 nil '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
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
    (u::prndiv)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(ignore!
(confirm that (u::unify1 nil '(333 + ,x + ,x) '(,z + 333 + ,z))
  returns (((\, x) \, z) ((\, z) . 333)))
(confirm that (u::unify1 nil '(333 + ,x) '(,x + 333))
  returns (((\, x) . 333)))
(confirm that (u::unify1 nil '(2 + 1) '(2 + 1))
  returns t)
(confirm that (u::unify1 nil '(,x + 1) '(2 + 1))
  returns (((\, x) . 2)))
(confirm that (u::unify1 nil '(2 + 1) '(,x + 1))
  returns (((\, x) . 2)))
(confirm that (u::unify1 nil '(,y + 1) '(,x + ,x))
  returns (((\, x) . 1) ((\, y) \, x)))
(confirm that (u::unify1 nil '(,x + 1) '(2 + ,y))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify1 nil '(,x + 1) '(2 + ,y + 1))
  returns nil)
(confirm that (u::unify1 nil '(,x + 1 + 2) '(2 + ,y + 3))
  returns nil)
(confirm that (u::unify1 nil '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify1 nil '(,x + 1 + ,a) '(2 + ,y + ,b))
  returns (((\, a) \, b) ((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify1 nil '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
  returns nil)
(confirm that (u::unify1 nil '(,x + 1) '(2 + ,x))
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
      ((and *u:occurs-check* (u::occurs bindings variable value))
        (u::prn "occurs check failed, not unifying.")
        (when (eq *u:occurs-check* :soft)
          (u::prn ":soft occurs check enabled, skip binding but continue unifying...")
          ;; (debug)
          (u::unify2 bindings (cdr pat1) (cdr pat2))))
      ((and binding (not (eql value (cdr binding))))
        (u::prn "variable %s is already bound to a different value, %s."
          (u::style variable)
          (u::style (cdr binding)))
        (if (not (dm::pat-elem-is-a-variable? (cdr binding)))
          nil
          ;; (debug (cdr binding) variable value pat1 pat2 bindings)
          (debug)
          ;; This is wrong:
          (u::unify-variable-with-value2 (cdr binding) value pat1 pat2 bindings)))
      (binding
        (u::prn "variable %s is already bound to the same value, %s."
          (u::style variable)
          (u::style (cdr binding)))
        (with-indentation (u::unify2 bindings (cdr pat1) (cdr pat2))))
      (t
        (u::prn "binding variable %s to value %s." (u::style variable)
          (u::style value))
        (with-indentation
          (u::unify2 (cons (cons variable value) bindings) (cdr pat1)
            (cdr pat2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify-variable-with-variable2 (bindings pat1 pat2)
  ;; if we entered this fun we already know that (car pat1) is unbound and that (car pat2) is a
  ;; bound variable.
  (let ((binding2 (assoc (car pat2) bindings)))
    (if (and (dm::pat-elem-is-a-variable? (cdr binding2))
          ;; (u::occurs bindings (car pat1) (cdr binding2))
          )
      (progn
        (u::prn "avoid circular binding, unify %s with %s's value %s instead!"
          (u::style (car pat1))
          (u::style (car pat2))
          (u::style (cdr binding2)))
        (with-indentation
          (u::unify2 bindings pat1 (cons (cdr binding2) (cdr pat2))))
        ;; (u::unify2 bindings (cdr pat1) (cdr pat2))
        )
      (u::prn "variable %s is unbound, unifying it with %s."
        (u::style (car pat1))
        (u::style (car pat2)))
      (with-indentation
        (u::unify2 (cons (cons (car pat1) (car pat2)) bindings) (cdr pat1) (cdr pat2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify2 (bindings pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A tail recursive variation on `u::unify1'. Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (u::unify2 nil '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (u::prndiv)
  (u::prn "pat1:     %S" pat1)
  (u::prn "pat2:     %S" pat2)
  (u::prn "bindings: %S" bindings)
  (u::prndiv ?\-)
  ;; uncurl tails:
  (let ((pat1-tail-type
          (cond
            ((and pat1 (atom pat1)) :tail)
            ((and (eq (car pat1) '\,) (not (cddr pat1))) :wayward-comma)))
         (pat2-tail-type
           (cond
             ((and pat2 (atom pat2)) :tail)
             ((and (eq (car pat2) '\,) (not (cddr pat2))) :wayward-comma))))
    (u::prn "tt1:     %S" pat1-tail-type)
    (u::prn "tt2:     %S" pat2-tail-type)
    (when (and pat1-tail-type pat2-tail-type)
      
      (cond
        ((eq pat1-tail-type :tail) (setf pat1 (list pat1)))
        ((eq pat1-tail-type :wayward-comma) (setf pat1 (list (list '\, (cadr pat1))))))
      (cond
        ((eq pat2-tail-type :tail) (setf pat2 (list pat2)))
        ((eq pat2-tail-type :wayward-comma) (setf pat2 (list (list '\, (cadr pat2))))))
      (u::prn "pat1*:    %S" pat1)
      (u::prn "pat2*:    %S" pat2)
      ))
  
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
      (with-indentation (u::unify2 bindings (cdr pat1) (cdr pat2))))
    ;;----------------------------------------------------------------------------------------------
    ;; same variable:
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (dm::pat-elem-is-a-variable? (car pat2))
       (eq (dm::pat-elem-var-sym (car pat1)) (dm::pat-elem-var-sym (car pat2))))
      (u::prn "%s and %s are the same variable." (u::style (car pat1)) (u::style (car pat2)))
      (with-indentation (u::unify2 bindings (cdr pat1) (cdr pat2))))
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
              (u::unify2 (cons (cons (car pat1) (car pat2)) bindings) (cdr pat1) (cdr pat2))))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1's var not bound, unify with PAT2's var:
          ((not binding1)
            ;; (u::unify-variable-with-value2 (car pat1) (car pat2) pat1 pat2 bindings)
            (u::unify-variable-with-variable2 bindings pat1 pat2)
            )
          ;;-----------------------------------------------------------------------------------------
          ;; PAT2's var not bound, unify with PAT1's var:
          ((not binding2)
            ;; (u::unify-variable-with-value2 (car pat2) (car pat1) pat2 pat1 bindings)
            (u::unify-variable-with-variable2 bindings pat2 pat1)
            )
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1 and PAT2's vars are bound to the same value, unify them destructively.
          ;; not totally sure about this but it seems to work, so far.
          (t (u::prn
               (concat "PAT1's var %s and PAT2's var %s are bound to the same value, "
                 "unifying them destructively.")
               (u::style (car pat1)) (u::style (car pat2)))
            (setcdr binding1 (car binding2))
            (with-indentation (u::unify2 bindings (cdr pat1) (cdr pat2)))))))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT1, value on PAT2:
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (or *u:bind-conses* (atom (car pat2))))
      (u::unify-variable-with-value2 pat1 pat2 bindings))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT2, value on PAT1:
    ((and (dm::pat-elem-is-a-variable? (car pat2)) (or *u:bind-conses* (atom (car pat1))))
      (u::unify-variable-with-value2 pat2 pat1 bindings))
    ;;----------------------------------------------------------------------------------------------
    (t nil (u::prn "unhandled"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (u::unify2 nil '(,w ,x ,y) '(,x ,y ,w))
  returns (((\, x) \, y) ((\, w) \, x)))
(confirm that (u::unify2 nil '(,w ,x ,y ,z) '(,x ,y ,z ,w))
  returns (((\, y) \, z) ((\, x) \, y) ((\, w) \, x)))
(confirm that (u::unify2 nil '(333 + ,x + ,x) '(,z + 333 + ,z))
  returns (((\, x) \, z) ((\, z) . 333)))
(confirm that (u::unify2 nil '(333 + ,x) '(,x + 333))
  returns (((\, x) . 333)))
(confirm that (u::unify2 nil '(2 + 1) '(2 + 1))
  returns t)
(confirm that (u::unify2 nil '(,x + 1) '(2 + 1))
  returns (((\, x) . 2)))
(confirm that (u::unify2 nil '(2 + 1) '(,x + 1))
  returns (((\, x) . 2)))
(confirm that (u::unify2 nil '(,y + 1) '(,x + ,x))
  returns (((\, x) . 1) ((\, y) \, x)))
(confirm that (u::unify2 nil '(,x + 1) '(2 + ,y))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 nil '(,x + 1) '(2 + ,y + 1))
  returns nil)
(confirm that (u::unify2 nil '(,x + 1 + 2) '(2 + ,y + 3))
  returns nil)
(confirm that (u::unify2 nil '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 nil '(,x + 1 + ,a) '(2 + ,y + ,b))
  returns (((\, b) \, a) ((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 nil '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
  returns nil)
(confirm that (u::unify2 nil '(,x + 1) '(2 + ,x))
  returns nil)
(confirm that (u::unify2 nil '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))
  returns (((\, a) . 8) ((\, y) . 1) ((\, x) . 2)))
;;---------------------------------------------------------------------------------------------------
;; uncurl tails tests:
(confirm that (u::unify2 nil '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
  returns (((\, zz) . 4) ((\, z) . 3) ((\, y) . 2) ((\, x) . 1)))
(confirm that (u::unify2 nil '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
  returns (((\, zz) . 4) ((\, z) . 3) ((\, y) . 2) ((\, x) . 1)))
(confirm that (u::unify2 nil '(,x ,y . ,z) '(1 2 . 3))
  returns (((\, z) . 3) ((\, y) . 2) ((\, x) . 1)))
(confirm that (u::unify2 nil '(,x ,y   ,z) '(1 2   3))
  returns (((\, z) . 3) ((\, y) . 2) ((\, x) . 1)))
;;---------------------------------------------------------------------------------------------------
;; occurs check tests:
(confirm that
  (let ((*u:occurs-check* nil))
    (u::unify2 nil '(,x ,y) '((f ,y) (f ,x))))
  returns (((\, y) f (\, x)) ((\, x) f (\, y))))
(confirm that
  (let ((*u:occurs-check* :soft))
    (u::unify2 nil '(,x ,y) '((f ,y) (f ,x))))
  returns (((\, x) f (\, y))))
(confirm that
  (let ((*u:occurs-check* t))
    (u::unify2 nil '(,x ,y) '((f ,y) (f ,x))))
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
  (u::simplify-bindings (u::unify2 nil '(,v ,u ,w ,x) '(,u ,x ,v 333)))
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
      (u::unify1 nil '(1 + ,x) '(,y + (2 . 3)))
      (u::unify2 nil '(1 + ,x) '(,y + (2 . 3)))))
  returns t)
(confirm that
  (let ((*u:bind-conses* nil))
    (u::equivalent-bindings?
      (u::unify1 nil '(1 + ,x) '(,y + (2 . 3)))
      (u::unify2 nil '(1 + ,x) '(,y + (2 . 3)))))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(333 + ,x + ,x) '(,z + 333 + ,z))
    (u::unify2 nil '(333 + ,x + ,x) '(,z + 333 + ,z)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(333 + ,x) '(,x + 333))
    (u::unify2 nil '(333 + ,x) '(,x + 333)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(2 + 1) '(2 + 1))
    (u::unify2 nil '(2 + 1) '(2 + 1)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1) '(2 + 1))
    (u::unify2 nil '(,x + 1) '(2 + 1)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(2 + 1) '(,x + 1))
    (u::unify2 nil '(2 + 1) '(,x + 1)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,y + 1) '(,x + ,x))
    (u::unify2 nil '(,y + 1) '(,x + ,x)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1) '(2 + ,y))
    (u::unify2 nil '(,x + 1) '(2 + ,y)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1) '(2 + ,y + 1))
    (u::unify2 nil '(,x + 1) '(2 + ,y + 1)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1 + 2) '(2 + ,y + 3))
    (u::unify2 nil '(,x + 1 + 2) '(2 + ,y + 3)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1 + ,a) '(2 + ,y + ,a))
    (u::unify2 nil '(,x + 1 + ,a) '(2 + ,y + ,a)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
    (u::unify2 nil '(,x + 1 + ,a) '(2 + ,y + ,b + 3)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1) '(2 + ,x))
    (u::unify2 nil '(,x + 1) '(2 + ,x)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))
    (u::unify2 nil '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a)))
  returns t)
(confirm that
  (u::equivalent-bindings?
    (u::unify1 nil '(,x + 1 + ,a) '(2 + ,y + ,b))
    (u::unify2 nil '(,x + 1 + ,a) '(2 + ,y + ,b)))
  returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u:unifier (fun pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "My version of Norvig's unifier."
  (let* ( (bindings (funcall fun nil pat1 pat2))
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
(confirm that (u:unifier #'u::unify2 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
  returns (2 + 1 + 2 + 2))
(confirm that (u:unifier #'u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (2 + 1 + (\, a)))
(confirm that (u:unifier #'u::unify2 '(333 + ,x + ,x) '(,z + 333 + ,z))
  returns (333 + 333 + 333))
(confirm that
  (u:unifier #'u::unify2 '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a))
  returns (2 + 1 + 8 + 8))
(confirm that
  (let ( (*u:verbose* t)
         (*u:unifier-simplifies* nil))
    (u:unifier #'u::unify2
      '(,v ,u ,w ,x)
      '(,u ,x ,v 333)))
  returns (333 333 333 333))
(confirm that
  (let ( (*u:verbose* t)
         (*u:unifier-simplifies* t))
    (u:unifier #'u::unify2
      '(,v ,u ,w ,x)
      '(,u ,x ,v 333)))
  returns (333 333 333 333))
(ignore!
  (let ((reps 50000))
    (list
      (benchmark-run reps
        (let ( (*u:verbose* nil)
               (*u:unifier-simplifies* nil))
          (u:unifier #'u::unify2
            '(,u ,v ,w ,x)
            '(,x ,u ,v 333))))
      (benchmark-run reps
        (let ( (*u:verbose* nil)
               (*u:unifier-simplifies* t))
          (u:unifier #'u::unify2
            '(,u ,v ,w ,x)
            '(,x ,u ,v 333)))))))
;; uncurl tails:
(confirm that (u:unifier #'u::unify2 '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
  returns (1 2 3 . 4))
(confirm that (u:unifier #'u::unify2 '(,x ,y . (,z . ,zz)) '(1 2 . (3 . 4)))
  returns (1 2 3 . 4))
(confirm that (u:unifier #'u::unify2 '(,x ,y . ,z) '(1 2 . 3))
  returns (1 2 . 3))
(confirm that (u:unifier #'u::unify2 '(,x ,y   ,z) '(1 2   3))
  returns (1 2 3))
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
    (trace-function #'u::unify-variable-with-variable2)
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

  (u::unify2 nil '(,x ,x) '(,y ,y))
  (u::unify2 nil '(,x ,y a) '(,y ,x ,x))
  (u::unify2 nil '(,x ,y) '(,y ,x)) ;; => ((,X . ,Y))
  (u::unify2 nil '(,x ,y a) '(,y ,x ,x)) ;; => ((,Y . A) (,X . ,Y))

  (u::unify2 nil '(,y ,x) '(7 (f ,x)))
  (u:unifier #'u::unify2 '(,y ,x) '(7 (f ,x)) nil)
  (u:unifier #'u::unify2 '(,x + 1 + ,a + 8) '(2 + ,y + ,a + ,a) nil)

  (u::unify2 nil '(,x ,y) '((f ,y) (f ,x))) ;; => ((,y f ,x) (,x f ,y))

  (let ((*u:occurs-check* :soft))
    (u::unify2 nil '(,x ,y) '((f ,y) (f ,x))))  ;; => (((\, x) f (\, y)))

  (variable variable) ; bind var...
  (variable atom)     ; bind var...
  (atom     variable) ; bind var...
  (variable cons)     ; ???
  (cons     variable) ; ???
  (cons     cons)     ; recurse!
  (atom     cons)     ; FAIL.
  (cons     atom)     ; FAIL.
  (atom     atom)     ; `eql'.

  (defun unify (x y &optional (bindings no-bindings))
    "See if x and y match with given bindings."
    (cond
      ((eq bindings fail) fail)
      ((eql x y) bindings) ;*** moved this line
      ((variable-p x) (unify-variable x y bindings))
      ((variable-p y) (unify-variable y x bindings))
      ((and (consp x) (consp y))
        (unify (rest x) (rest y)
          (unify (first x) (first y) bindings)))
      (t fail)))

  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Norvigian version follows:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *n:occurs-check* t)
(defvar n::fail '(nil . nil))
(defvar n::no-bindings '((t . t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::unify-variable (bindings var expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Unify VAR with EXPR, using (and maybe extending) BINDINGS."
  (cond
    ((assoc var bindings)
      (n::unify1 (cdr (assoc var bindings)) expr bindings))
    ((and (n::variable-p expr) (assoc expr bindings))
      (n::unify1 var (cdr (assoc expr bindings)) bindings))
    ((and *n:occurs-check* (n::occurs-check bindings var expr))
      n::fail)
    (t (n::extend-bindings var expr bindings))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::occurs-check (bindings var expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Does VAR occur anywhere inside EXPR according to BINDINGS?"
  (cond ((eq var expr) t)
    ((and (n::variable-p expr) (assoc expr bindings))
      (n::occurs-check bindings var (cdr (assoc expr bindings))))
    ((consp expr)
      (or (n::occurs-check bindings var (car expr))
        (n::occurs-check bindings var (cdr expr))))
    (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::extend-bindings (var expr bindings)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Add a (VAR . EXPR) pair to BINDINGS."
  (cons (cons var expr)
    ;; Once we add a "real" binding,
    ;; we can get rid of the dummy n::no-bindings
    (if (eq bindings n::no-bindings) nil bindings)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::subst-bindings (bindings x)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Substitute the value of variables in bindings into x,
 taking recursively bound variables into account. Norvig's with tweaks."
  (cond
    ((null bindings) nil)
    ((eq bindings t) x)
    ((and (dm::pat-elem-is-a-variable? x) (assoc x bindings))
      (u::subst-bindings bindings (cdr (assoc x bindings))))
    ((atom x) x)
    (t
      (u::reuse-cons
        (with-indentation (u::subst-bindings bindings (car x)))
        (with-indentation (u::subst-bindings bindings (cdr x)))
        x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::subst-bindings (bindings expr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Substitute the value of variables in BINDINGS into EXPR,
 taking recursively bound variables into account."
  (cond
    ((eq bindings n::fail) n::fail)
    ((eq bindings n::no-bindings) expr)
    ((and (n::variable-p expr) (assoc expr bindings))
      (n::subst-bindings bindings (cdr (assoc expr bindings))))
    ((atom expr) expr)
    (t (u::reuse-cons (n::subst-bindings bindings (car expr))
         (n::subst-bindings bindings (cdr expr)) expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n:unifier (thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return something that unifies with both THING1 and THING2 (or n::fail)."
  (let ( (thing1 (n::fix-variables thing1))
         (thing2 (n::fix-variables thing2)))
    (n::subst-bindings (n::unify1 thing2 thing1) thing2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::variable-p (x)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) ?\?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::fix-variables (thing)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn comma-prefixed variable designators like ,X / (\, X) into symbols like \?X anywhere in THING
(including improper tails)."
  (cond
    ((atom thing) thing)
    ((and (consp thing) (eq '\, (car thing)) (cdr thing) (not (cddr thing)) (symbolp (cadr thing)))
      (intern (concat "?" (symbol-name (cadr thing)))))
    (t (cons (n::fix-variables (car thing)) (n::fix-variables (rest thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::unify1 (thing1 thing2 &optional bindings)
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
      ((n::variable-p thing1)
        ;; (debug nil 3)
        (n::unify-variable bindings thing1 thing2))
      ((n::variable-p thing2)
        ;; (debug nil 3)
        (n::unify-variable bindings thing2 thing1))
      ((and (consp thing1) (consp thing2))
        ;; (debug nil 3)
        (n::unify1 (cdr thing1) (cdr thing2)
          (n::unify1 (car thing1) (car thing2) bindings)))
      (t
        ;; (debug nil 5)
        n::fail))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n::unify (thing1 thing2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "See if THING1 and THING2 match with given BINDINGS."
  (n::unify1 (n::fix-variables thing1) (n::fix-variables thing2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(n::unify1 '(x) '(11))

(trace-function #'n::unify)
(trace-function #'n::unify1)
(trace-function #'n:unifier)
(trace-function #'n::extend-bindings3)
(trace-function #'n::fix-variables)
(trace-function #'n::subst-bindings)
;; (untrace-all)

(n::unify1 'x 11) 
(n::unify1 '(x y z) '(11 22 33)) ;; sus.

(n::unify1 (n::fix-variables '(,x ,y ,z)) '(11 22 33))
(n::unify1 (n::fix-variables '(,x ,y ,z)) (n::fix-variables '(,x ,y ,z)))

(n::unify1 (n::fix-variables '(,x ,y (,z 8 ,b . ,c))) '(1 2 (3 8 4 . 5)))
(n::variable-p (car (n::fix-variables '(,x ,y (,z 8 ,b . \?c)))))

(n::unify
  (n::fix-variables '(,u ,v ,w ,x))
  (n::fix-variables '(,x ,u ,v 333)))

(n::unify1
  (n::fix-variables '(,u ,v ,w ,x))
  (n::fix-variables '(,x ,u ,v 333))
  nil)

(n::unify
  (n::fix-variables '((,a * ,x ^ 2) + (,b * ,x) + ,c))
  (n::fix-variables '(,z + (4 * 5) + 3)))


(u::unify2 nil '(,w ,x ,y ,z) '(,x ,y ,z ,w))
(n::unify  '(,w ,x ,y ,z) '(,x ,y ,z ,w))

(n:unifier
  '((,a * ,x ^ 2) + (,b * ,x) + ,c)
  '(,z + (4 * 5) + 3))

(n::unify
  '((,a * ,x ^ 2) + (,b * ,x) + ,c)
  '(,z + (4 * 5) + 3))

