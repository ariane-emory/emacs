;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup unify nil
  "Ari's unification functions.")
;;---------------------------------------------------------------------------------------------------
(defcustom *u:verbose* nil
  "Whether or not functions in the 'unify' group should print verbose messages."
  :group 'unify
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *u:div-width* 90
  "Div width used by functions in the 'unify' group."
  :group 'unify
  :type 'integer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *u:bind-conses* nil)
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
(defun u::reuse-cons (x y x-y)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return (cons x y), or u::reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
    x-y
    (cons x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::subst-bindings (bindings x)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Substitute the value of variables in bindings into x,
 taking recursively bound variables into account."
  ;;
  (u::prndiv)
  ;; (debug bindings x)
  (u::prn "x:        %s" x)
  (let
    ((res 
       (cond
         ((null bindings)
           (u::prn "case 1")
           nil)
         ((eq bindings t)
           (u::prn "case 2")
           x)
         ((and (dm::pat-elem-is-a-variable? x) (assoc x bindings))
           (u::prn "case 3")
           (with-indentation (u::subst-bindings bindings (cdr (assoc x bindings)))))
         ((atom x) x)
         (t
           (u::prn "case 4")
           (u::reuse-cons
             (with-indentation (u::subst-bindings bindings (car x)))
             (with-indentation (u::subst-bindings bindings (cdr x)))
             x)))))
    (u::prn "result:   %s" res)
    (u::prndiv)
    res))
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
          (u::prn "pat1-elem:  %s" pat1-elem)
          (u::prn "pat2-elem: %s" pat2-elem)
          (u::prndiv ?\-)
          (cond
            ((and (dm::pat-elem-is-a-variable? pat1-elem)
               (dm::pat-elem-is-a-variable? pat2-elem)
               (eq (dm::pat-elem-var-sym pat1-elem)
                 (dm::pat-elem-var-sym pat2-elem)))
              (u::prn "pat1-elem and pat2-elem are the same variable"))
            ((and (dm::pat-elem-is-a-variable? pat1-elem) (dm::pat-elem-is-a-variable? pat2-elem))
              (u::prn "both pat1-elem and pat2-elem are variables")
              (let ( (binding1 (assoc pat1-elem bindings))
                     (binding2 (assoc pat2-elem bindings)))
                (cond
                  ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
                    (u::prn "already bound to different variables")
                    (throw 'not-unifiable nil))
                  ((not binding1)
                    (push (cons pat1-elem pat2-elem) bindings))
                  ((not binding2)
                    (push (cons pat2-elem pat1-elem) bindings))
                  (t ; do nothing?)
                    ))))
            ((dm::pat-elem-is-a-variable? pat1-elem)
              (u::prn "pat1-elem is a variable")
              (let ((binding (assoc pat1-elem bindings)))
                (cond
                  ((and binding (not (eql pat2-elem (cdr binding))))
                    (u::prn "pat1-elem is already bound to a different value")
                    (throw 'not-unifiable nil))
                  (binding
                    (u::prn "pat1-elem is already bound to the same value"))
                  ((not (or *u:bind-conses* (atom pat2-elem)))
                    (throw 'not-unifiable nil))
                  (t (push (cons pat1-elem pat2-elem) bindings)))))
            ((dm::pat-elem-is-a-variable? pat2-elem)
              (u::prn "pat2-elem is a variable")
              (let ((binding (assoc pat2-elem bindings)))
                (cond
                  ((and binding (not (eql pat1-elem (cdr binding))))
                    (u::prn "pat2-elem is already bound to a different value")
                    (throw 'not-unifiable nil))
                  (binding
                    (u::prn "pat2-elem is already bound to the same value"))
                  ((not (or *u:bind-conses* (atom pat1-elem)))
                    (throw 'not-unifiable nil))
                  (t (push (cons pat2-elem pat1-elem) bindings)))))
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
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u::unify2 (pat1 pat2 &optional bindings)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A tail recursive variation on `u::unify1'. Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (u::unify2 '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (u::prndiv)
  (u::prn "pat1:     %s" pat1)
  (u::prn "pat2:     %s" pat2)
  (u::prn "bindings: %s" bindings)
  (cond
    ((not (or  pat1 pat2)) (or bindings t))
    ((not pat1)
      (u::prn "pat1 ran out.")
      nil)
    ((not pat2)
      (u::prn "pat2 ran out.")
      nil)
    ((and (atom (car pat1)) (atom (car pat2)) (eql (car pat1) (car pat2)))
      (u::prn "%s and %s are eql atoms." (car pat1) (car pat2))
      (with-indentation
        (u::unify2 (cdr pat1) (cdr pat2) bindings)))
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (dm::pat-elem-is-a-variable? (car pat2))
       (eq (dm::pat-elem-var-sym (car pat1)) (dm::pat-elem-var-sym (car pat2))))
      (u::prn "%s and %s are the same variable" (car pat1) (car pat2))
      (with-indentation
        (u::unify2 (cdr pat1) (cdr pat2) bindings)))
    ((and (dm::pat-elem-is-a-variable? (car pat1))
       (dm::pat-elem-is-a-variable? (car pat2))
       (eq (dm::pat-elem-var-sym (car pat1))
         (dm::pat-elem-var-sym (car pat2))))
      (u::prn "pat1-elem and pat2-elem are the same variable")
      (u::unify2 (cdr pat1) (cdr pat2) bindings))
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (dm::pat-elem-is-a-variable? (car pat2)))
      (u::prn "both pat1-elem and pat2-elem are variables")
      (let ( (binding1 (assoc (car pat1) bindings))
             (binding2 (assoc (car pat2) bindings)))
        (u::prn "%s = %s" (car pat1) (cdr binding1))
        (u::prn "%s = %s" (car pat2) (cdr binding2))
        (cond
          ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
            (u::prn "already bound to different terms")
            nil)
          ((not (or binding1 binding2))
            (u::prn "both unbound")
            (with-indentation
              (u::unify2 (cdr pat1) (cdr pat2)
                (cons (cons (car pat1) (car pat2))
                  bindings))))
          ((not binding1)
            (u::prn "pat1-elem is unbound")
            (with-indentation
              (u::unify2 (cdr pat1) (cdr pat2)
                (cons (cons (car pat1) (car pat2))
                  bindings)))
            )
          ((not binding2)
            (u::prn "pat2-elem is unbound")
            (with-indentation
              (u::unify2 (cdr pat1) (cdr pat2)
                (cons (cons (car pat2) (car pat1))
                  bindings)))
            )
          ;; may be impossible?
          (t (debug)
            (with-indentation (u::unify2 (cdr pat1) (cdr pat2) bindings))
            )
          )))
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (or *u:bind-conses* (atom (car pat2))))
      (u::prn "pat1-elem is the variable %s and pat2-elem is atom %s" (car pat1) (car pat2))
      (let ((binding (assoc (car pat1) bindings)))
        (cond
          ((and binding (not (eql (car pat2) (cdr binding))))
            (u::prn "pat1-elem is already bound to a different value")
            nil)
          (binding
            (u::prn "pat1-elem is already bound to the same value")
            (with-indentation (u::unify2 (cdr pat1) (cdr pat2) bindings)))
          (t
            (u::prn "binding variaable %s to atom %s" (car pat1) (car pat2))
            (with-indentation
              (u::unify2 (cdr pat1) (cdr pat2) (cons (cons (car pat1) (car pat2)) bindings)))))))
    ((and (dm::pat-elem-is-a-variable? (car pat2)) (or *u:bind-conses* (atom (car pat1))))
      (u::prn "pat1-elem is the atom %s pat2-elem is the variable" (car pat1) (car pat2))
      (let ((binding (assoc (car pat2) bindings)))
        (cond
          ((and binding (not (eql (car pat1) (cdr binding))))
            (u::prn "pat2-elem is already bound to a different value")
            nil)
          (binding
            (u::prn "pat2-elem is already bound to the same value")
            (with-indentation (u::unify2 (cdr pat1) (cdr pat2) bindings)))
          (t
            (u::prn "binding variaable %s to atom %s" (car pat2) (car pat1))
            (with-indentation
              (u::unify2 (cdr pat1) (cdr pat2) (cons (cons (car pat2) (car pat1)) bindings)))))))
    (t nil ;; (error "unhandled")
      )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (u::unify2 '(2 + 1) '(2 + 1))
  returns t)
(confirm that (u::unify2 '(,x + 1) '(2 + 1))
  returns (((\, x) . 2)))
(confirm that (u::unify2 '(2 + 1) '(,x + 1))
  returns (((\, x) . 2)))
(confirm that (u::unify2 '(,y + 1) '(,x + ,x))
  returns (((\, x) . 1) ((\, y) \, x)))
(confirm that (u::unify2 '(,x + 1) '(2 + ,y))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 '(,x + 1) '(2 + ,y + 1))
  returns nil)
(confirm that (u::unify2 '(,x + 1 + 2) '(2 + ,y + 3))
  returns nil)
(confirm that (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,b))
  returns (((\, a) \, b) ((\, y) . 1) ((\, x) . 2)))
(confirm that (u::unify2 '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
  returns nil)
(confirm that (u::unify2 '(,x + 1) '(2 + ,x))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comparison tests:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ((*u:bind-conses* t))
    (equal
      (u::unify1 '(1 + ,x) '(,y + (2 . 3)))
      (u::unify2 '(1 + ,x) '(,y + (2 . 3)))))
  returns t)
(confirm that
  (let ((*u:bind-conses* nil))
    (equal
      (u::unify1 '(1 + ,x) '(,y + (2 . 3)))
      (u::unify2 '(1 + ,x) '(,y + (2 . 3)))))
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
    (u::unify2 '(1 + ,x) '(,y + (2 . 3))))
  returns nil)
(confirm that
  (let ((*u:bind-conses* t))
    (u::unify2 '(1 + ,x) '(,y + (2 . 3))))
  returns (((\, x) 2 . 3) ((\, y) . 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun u:unifier (fun pat1 pat2)
  (let ((bindings (funcall fun pat1 pat2)))
    (u::prn "bindings: %s" bindings)
    (u::prndiv)
    (u::prnl)
    (u::subst-bindings bindings pat1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (u:unifier #'u:unify1 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
  returns (2 + 1 + 2 + 2))
(confirm that (u:unifier #'u:unify1 '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (2 + 1 + (\, a)))
(confirm that (u:unifier #'u:unify2 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
  returns (2 + 1 + 2 + 2))
(confirm that (u:unifier #'u:unify2 '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (2 + 1 + (\, a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(u:unifier #'u:unify1 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
(u:unifier #'u:unify2 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
