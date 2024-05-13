;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--destructuring-match-aux)
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
  "Whether to perform the occurs check."
  :group 'unify
  :type 'boolen)
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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun u::unify1 (pat1 pat2)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Recursively unify two patterns, returning an alist of variable bindings.

;; Example:
;;   (u::unify1 '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
;;   (let (bindings)
;;     (catch 'not-unifiable
;;       (while (and pat1 pat2)
;;         (u::prndiv)
;;         (u::prn "pat1: %s" pat1)
;;         (u::prn "pat2: %s" pat2)
;;         (let ( (pat1-elem (car pat1))
;;                (pat2-elem (car pat2)))
;;           (u::prn "pat1-elem:  %s" pat1-elem)
;;           (u::prn "pat2-elem: %s" pat2-elem)
;;           (u::prndiv ?\-)
;;           (cond
;;             ((and (dm::pat-elem-is-a-variable? pat1-elem)
;;                (dm::pat-elem-is-a-variable? pat2-elem)
;;                (eq (dm::pat-elem-var-sym pat1-elem)
;;                  (dm::pat-elem-var-sym pat2-elem)))
;;               (u::prn "pat1-elem and pat2-elem are the same variable"))
;;             ((and (dm::pat-elem-is-a-variable? pat1-elem) (dm::pat-elem-is-a-variable? pat2-elem))
;;               (u::prn "both pat1-elem and pat2-elem are variables")
;;               (let ( (binding1 (assoc pat1-elem bindings))
;;                      (binding2 (assoc pat2-elem bindings)))
;;                 (cond
;;                   ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
;;                     (u::prn "already bound to different variables")
;;                     (throw 'not-unifiable nil))
;;                   ((not binding1)
;;                     (push (cons pat1-elem pat2-elem) bindings))
;;                   ((not binding2)
;;                     (push (cons pat2-elem pat1-elem) bindings))
;;                   (t
;;                     (u::prn
;;                       (concat "pat1-elem and pat2-elem are bound to the same value, "
;;                         "unifying destructively."))
;;                     (setcdr binding1 (car binding2))))))
;;             ((dm::pat-elem-is-a-variable? pat1-elem)
;;               (u::prn "pat1-elem is a variable")
;;               (let ((binding (assoc pat1-elem bindings)))
;;                 (cond
;;                   ((and binding (not (eql pat2-elem (cdr binding))))
;;                     (u::prn "pat1-elem is already bound to a different value")
;;                     (throw 'not-unifiable nil))
;;                   (binding
;;                     (u::prn "pat1-elem is already bound to the same value"))
;;                   ((not (or *u:bind-conses* (atom pat2-elem)))
;;                     (throw 'not-unifiable nil))
;;                   (t (push (cons pat1-elem pat2-elem) bindings)))))
;;             ((dm::pat-elem-is-a-variable? pat2-elem)
;;               (u::prn "pat2-elem is a variable")
;;               (let ((binding (assoc pat2-elem bindings)))
;;                 (cond
;;                   ((and binding (not (eql pat1-elem (cdr binding))))
;;                     (u::prn "pat2-elem is already bound to a different value")
;;                     (throw 'not-unifiable nil))
;;                   (binding
;;                     (u::prn "pat2-elem is already bound to the same value"))
;;                   ((not (or *u:bind-conses* (atom pat1-elem)))
;;                     (throw 'not-unifiable nil))
;;                   (t (push (cons pat2-elem pat1-elem) bindings)))))
;;             ((equal pat1-elem pat2-elem)
;;               (u::prn "pat1-elem and pat2-elem are equal"))
;;             (t (u::prn "pat1-elem and pat2-elem are not equal")            
;;               (throw 'not-unifiable nil))))
;;         (pop pat1)
;;         (pop pat2)
;;         (u::prn "bindings: %s" bindings)
;;         ;;(debug pat1 pat2 bindings)
;;         ))
;;     (prog1
;;       (if (or pat1 pat2)
;;         ;; not unifiable, return no bindings:
;;         (progn
;;           (u::prndiv ?\-)
;;           (u::prn "pat1: %s" pat1)
;;           (u::prn "pat2: %s" pat2)
;;           (u::prn "not unifiable, return no bindings")
;;           nil)
;;         (or bindings t))
;;       (u::prndiv))))


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
           ((and binding (not (eql (car ,value-pat) (cdr binding))))
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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro u::unify-variable-with-value2 (variable-pat value-pat)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Note: when this recurses, it might swap which tail was PAT1 and which was PAT2, but that seems to be fine."
;;   (let ( (variable-pat-name (upcase (symbol-name variable-pat)))
;;          (value-pat-name    (upcase (symbol-name value-pat))))
;;     `(let ((binding (assoc (car ,variable-pat) bindings)))
;;        (u::prn "%s elem is the variable %s and %s elem is the value %s."
;;          ,variable-pat-name
;;          (u::style (car ,variable-pat))
;;          ,value-pat-name
;;          (u::style (car ,value-pat)))
;;        (cond
;;          ((and binding (not (eql (car ,value-pat) (cdr binding))))
;;            (u::prn "%s elem %s is already bound to a different value, %s."
;;              ,variable-pat-name
;;              (u::style (car ,variable-pat))
;;              (u::style (cdr binding)))
;;            nil)
;;          (binding
;;            (u::prn "%s elem %s is already bound to the same value, %s."
;;              ,variable-pat-name
;;              (u::style (car ,variable-pat))
;;              (u::style (cdr binding)))
;;            (with-indentation (u::unify2 (cdr ,variable-pat) (cdr ,value-pat) bindings)))
;;          (t
;;            (u::prn "binding variable %s to atom %s." (u::style (car ,variable-pat))
;;              (u::style (car ,value-pat)))
;;            (with-indentation
;;              (u::unify2 (cdr ,variable-pat) (cdr ,value-pat)
;;                (cons (cons (car ,variable-pat) (car ,value-pat)) bindings))))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify-variable-with-value2 (variable value pat1 pat2 bindings)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: when this recurses, it might swap which tail was PAT1 and which was PAT2, but that seems to be fine."
  (let ((binding (assoc variable bindings)))
    (u::prn "try to unify the variable %s and with the value %s."
      (u::style variable)
      (u::style value))
    (cond
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
(defmacro u::unify-variable-with-variable (binding pat1 pat2 bindings)
  (let ( (pat1-name (upcase (symbol-name pat1)))
         (pat2-name (upcase (symbol-name pat2))))
    `(progn
       (if (equal (car ,pat1) (cdr (assoc (car ,pat2) bindings)))
         (progn
           (u::prn "avoid circular binding!")
           ;; (debug (car ,pat1) (car ,pat2) bindings)
           (u::unify2 (cdr ,pat1) (cdr ,pat2) bindings))
         (u::prn "%s elem %s is unbound, unifying it with %s." ,pat1-name (u::style (car ,pat1))
           (u::style (car ,binding)))
         (with-indentation
           (u::unify2 (cdr ,pat1) (cdr ,pat2) (cons (cons (car ,pat1) (car ,pat2)) ,bindings)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro u::unify-variable-with-variable (variable pat1 pat2 bindings)
  (let ( (pat1-name (upcase (symbol-name pat1)))
         (pat2-name (upcase (symbol-name pat2))))
    `(progn
       (if (equal (car ,pat1) (cdr (assoc (car ,pat2) bindings)))
         (progn
           (u::prn "avoid circular binding!")
           ;; (debug (car ,pat1) (car ,pat2) bindings)
           (u::unify2 (cdr ,pat1) (cdr ,pat2) bindings))
         (u::prn "%s elem %s is unbound, unifying it with %s." ,pat1-name (u::style (car ,pat1))
           (u::style variable))
         (with-indentation
           (u::unify2 (cdr ,pat1) (cdr ,pat2) (cons (cons (car ,pat1) (car ,pat2)) ,bindings)))))))
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
      (u::prn "both PAT1 elem and PAT2 elem are variables.")
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
            (u::unify-variable-with-variable (car binding2) pat1 pat2 bindings))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT2's var not bound, unify with PAT1's var:
          ((not binding2)
            (u::unify-variable-with-variable (car binding1) pat2 pat1 bindings))
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
      (u::unify-variable-with-value2 (car pat1) (car pat2) pat1 pat2 bindings))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT2, value on PAT1:
    ((and (dm::pat-elem-is-a-variable? (car pat2)) (or *u:bind-conses* (atom (car pat1))))
      (u::unify-variable-with-value2 (car pat2) (car pat1) pat2 pat1 bindings))
    ;;----------------------------------------------------------------------------------------------
    (t nil (ignore! (error "unhandled")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comparison tests:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ((*u:bind-conses* t))
    (equal
      (u::unify1 '(1 + ,x) '(,y + (2 . 3)))
      (u::unify2 '(1 + ,x) '(,y + (2 . 3)) nil)))
  returns t)
(confirm that
  (let ((*u:bind-conses* nil))
    (equal
      (u::unify1 '(1 + ,x) '(,y + (2 . 3)))
      (u::unify2 '(1 + ,x) '(,y + (2 . 3)) nil)))
  returns t)
(confirm that
  (let ((*u:bind-conses* nil))
    (u::unify1 '(1 + ,x) '(,y + (2 . 3))))
  returns nil)
(confirm that
  (let ((*u:bind-conses* t))
    (u::unify1 '(1 + ,x) '(,y + (2 . 3))))
  returns (((\, x) 2 . 3) ((\, y) . 1)))
(confirm that
  (let ((*u:bind-conses* nil))
    (u::unify2 '(1 + ,x) '(,y + (2 . 3)) nil))
  returns nil)
(confirm that
  (let ((*u:bind-conses* t))
    (u::unify2 '(1 + ,x) '(,y + (2 . 3)) nil))
  returns (((\, x) 2 . 3) ((\, y) . 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u:unifier (fun pat1 pat2 &rest rest )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Norvig's  unifier."
  (let ((bindings (apply fun pat1 pat2 rest)))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ((*u:verbose* t))
  (u:unifier #'u::unify2 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2) nil))

(let ((*u:verbose* t))
  (u:unifier #'u::unify2 '(333 + ,x + ,x) '(,z + 333 + ,z) nil))

(ignore!
  (list
    (benchmark-run 10000 (u:unifier #'u::unify1 '(333 + ,x + ,x) '(,z + 333 + ,z)))
    (benchmark-run 10000 (u:unifier #'u::unify2 '(333 + ,x + ,x) '(,z + 333 + ,z) nil)))
  )


(u::unify2 '(,x ,x) '(,y ,y) nil)

(u::unify2 '(,x ,y a) '(,y ,x ,x) nil)
(u::unify2 '(,x ,y) '(,y ,x) nil) ;; => ((,X . ,Y))
(u::unify2 '(,x ,y a) '(,y ,x ,x) nil) ;; => ((,Y . A) (,X . ,Y))


(u::unify2 '(,x) '((f ,x)) nil)
