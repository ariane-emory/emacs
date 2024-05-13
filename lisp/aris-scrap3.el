;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (dolist (thing  '(  aa
;;                    ,bb    ,(bb t)
;;                    ,@cc  ,@(cc t)
;;                    #'dd  #'(dd t)
;;                    'ee    '(ee t)
;;                    `ff    `(ff t)
;;                    ))
;;   (cond
;;     ((eq '\, (car-safe thing)) (prn "This one is special: %s" thing))
;;     ((eq '\,@ (car-safe thing)) (prn "This one is very special: %s" thing))
;;     ((eq 'function (car-safe thing)) (prn "This one is super special: %s" thing))
;;     ((eq 'quote (car-safe thing)) (prn "This one is kind of special: %s" thing))
;;     ((eq '\` (car-safe thing)) (prn "This one is extra special: %s" thing))
;;     ;; (t (prn "%s" thing))
;;     ))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dm::pat-elem-is-a-variable? ',x) ;; => t
(dm::pat-elem-is-a-variable? 'x)  ;; => nil
(dm::pat-elem-is-a-variable?  1)  ;; => nil


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unify1 (pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (unify1 '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (let (bindings)
    (catch 'not-unifiable
      (while (and pat1 pat2)
        (prndiv)
        (prn "pat1: %s" pat1)
        (prn "pat2: %s" pat2)
        (let ( (pat1-elem (car pat1))
               (pat2-elem (car pat2)))
          (prn "pat1-elem:  %s" pat1-elem)
          (prn "pat2-elem: %s" pat2-elem)
          (prndiv ?\-)
          (cond
            ((and (dm::pat-elem-is-a-variable? pat1-elem)
               (dm::pat-elem-is-a-variable? pat2-elem)
               (eq (dm::pat-elem-var-sym pat1-elem)
                 (dm::pat-elem-var-sym pat2-elem)))
              (prn "pat1-elem and pat2-elem are the same variable"))
            ((and (dm::pat-elem-is-a-variable? pat1-elem)
               (dm::pat-elem-is-a-variable? pat2-elem)
               (prn "both pat1-elem and pat2-elem are variables")
               (let ( (binding1 (assoc pat1-elem bindings))
                      (binding2 (assoc pat2-elem bindings)))
                 (cond
                   ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
                     (prn "already bound to different variables")
                     (throw 'not-unifiable nil))
                   ((not (or binding1 binding2))
                     (push (cons pat1-elem pat2-elem) bindings))
                   (t ; do nothing?)
                     )))))
            ((dm::pat-elem-is-a-variable? pat1-elem)
              (prn "pat1-elem is a variable")
              (let ((binding (assoc pat1-elem bindings)))
                (cond
                  ((and binding (not (eql pat2-elem (cdr binding))))
                    (prn "pat1-elem is already bound to a different value")
                    (throw 'not-unifiable nil))
                  (binding
                    (prn "pat1-elem is already bound to the same value"))
                  (t (push (cons pat1-elem pat2-elem) bindings)))))
            ((dm::pat-elem-is-a-variable? pat2-elem)
              (prn "pat2-elem is a variable")
              (let ((binding (assoc pat2-elem bindings)))
                (cond
                  ((and binding (not (eql pat1-elem (cdr binding))))
                    (prn "pat2-elem is already bound to a different value")
                    (throw 'not-unifiable nil))
                  (binding
                    (prn "pat2-elem is already bound to the same value"))
                  (t (push (cons pat2-elem pat1-elem) bindings)))))
            ((equal pat1-elem pat2-elem)
              (prn "pat1-elem and pat2-elem are equal"))
            (t (prn "pat1-elem and pat2-elem are not equal")            
              (setq bindings nil))))
        (pop pat1)
        (pop pat2)
        (prn "bindings: %s" bindings)
        ;;(debug pat1 pat2 bindings)
        ))
    (prog1
      (if (or pat1 pat2)
        ;; not unifiable, return no bindings:
        (progn
          (prndiv ?\-)
          (prn "pat1: %s" pat1)
          (prn "pat2: %s" pat2)
          (prn "not unifiable, return no bindings")
          nil)
        (or bindings t))
      (prndiv))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (unify1 '(2 + 1) '(2 + 1))
  returns t)
(confirm that (unify1 '(,x + 1) '(2 + 1))
  returns (((\, x) . 2)))
(confirm that (unify1 '(2 + 1) '(,x + 1))
  returns (((\, x) . 2)))
(confirm that (unify1 '(,y + 1) '(,x + ,x))
  returns (((\, x) . 1) ((\, y) \, x)))
(confirm that (unify1 '(,x + 1) '(2 + ,y))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (unify1 '(,x + 1) '(2 + ,y + 1))
  returns nil)
(confirm that (unify1 '(,x + 1 + 2) '(2 + ,y + 3))
  returns nil)
(confirm that (unify1 '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (unify1 '(,x + 1 + ,a) '(2 + ,y + ,b))
  returns (((\, a) \, b) ((\, y) . 1) ((\, x) . 2)))
(confirm that (unify1 '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
  returns nil)
(confirm that (unify1 '(,x + 1) '(2 + ,x))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reuse-cons (x y x-y)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
    x-y
    (cons x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subst-bindings (bindings x)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Substitute the value of variables in bindings into x,
 taking recursively bound variables into account."
  ;;
  (prndiv)
  ;; (debug bindings x)
  (prn "x:        %s" x)
  (let
    ((res 
       (cond
         ((null bindings)
           (prn "case 1")
           nil)
         ((eq bindings t)
           (prn "case 2")
           x)
         ((and (dm::pat-elem-is-a-variable? x) (assoc x bindings))
           (prn "case 3")
           (with-indentation (subst-bindings bindings (cdr (assoc x bindings)))))
         ((atom x) x)
         (t
           (prn "case 4")
           (reuse-cons
             (with-indentation (subst-bindings bindings (car x)))
             (with-indentation (subst-bindings bindings (cdr x)))
             x)))))
    (prn "result:   %s" res)
    (prndiv)
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unifier (pat1 pat2)
  (let ((bindings (unify1 pat1 pat2)))
    (prn "bindings: %s" bindings)
    (prndiv)
    (prnl)
    (subst-bindings bindings pat1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (unifier '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
  returns (2 + 1 + 2 + 2))
(confirm that (unifier '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (2 + 1 + (\, a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unify2 (pat1 pat2 &optional bindings)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A tail recursive variation on `unify1'. Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (unify2 '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (prndiv)
  (prn "pat1:     %s" pat1)
  (prn "pat2:     %s" pat2)
  (prn "bindings: %s" bindings)
  (cond
    ((not (or  pat1 pat2)) (or bindings t))
    ((not pat1)
      (prn "pat1 ran out.")
      nil)
    ((not pat2)
      (prn "pat2 ran out.")
      nil)
    ((and (atom (car pat1)) (atom (car pat2)) (eql (car pat1) (car pat2)))
      (prn "%s and %s are eql atoms." (car pat1) (car pat2))
      (with-indentation
        (unify2 (cdr pat1) (cdr pat2) bindings)))
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (dm::pat-elem-is-a-variable? (car pat2))
       (eq (dm::pat-elem-var-sym (car pat1)) (dm::pat-elem-var-sym (car pat2))))
      (prn "%s and %s are the same variable" (car pat1) (car pat2))
      (with-indentation
        (unify2 (cdr pat1) (cdr pat2) bindings)))
    ((and (dm::pat-elem-is-a-variable? (car pat1))
       (dm::pat-elem-is-a-variable? (car pat2))
       (eq (dm::pat-elem-var-sym (car pat1))
         (dm::pat-elem-var-sym (car pat2))))
      (prn "pat1-elem and pat2-elem are the same variable")
      (unify2 (cdr pat1) (cdr pat2) bindings))
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (dm::pat-elem-is-a-variable? (car pat2)))
      (prn "both pat1-elem and pat2-elem are variables")
      (let ( (binding1 (assoc (car pat1) bindings))
             (binding2 (assoc (car pat2) bindings)))
        (prn "%s = %s" (car pat1) (cdr binding1))
        (prn "%s = %s" (car pat2) (cdr binding2))
        (cond
          ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
            (prn "already bound to different terms")
            nil)
          ((not (or binding1 binding2))
            (with-indentation
              (unify2 (cdr pat1) (cdr pat2)
                (cons (cons (car pat1) (car pat2))
                  bindings))))
          (t (with-indentation (unify2 (cdr pat1) (cdr pat2) bindings))))))
    ((and (dm::pat-elem-is-a-variable? (car pat1)) (atom (car pat2)))
      (prn "pat1-elem is the variable %s and pat2-elem is atom %s" (car pat1) (car pat2))
      (let ((binding (assoc (car pat1) bindings)))
        (cond
          ((and binding (not (eql (car pat2) (cdr binding))))
            (prn "pat1-elem is already bound to a different value")
            nil)
          (binding
            (prn "pat1-elem is already bound to the same value")
            (with-indentation (unify2 (cdr pat1) (cdr pat2) bingins)))
          (t
            (prn "binding variaable %s to atom %s" (car pat1) (car pat2))
            (with-indentation
              (unify2 (cdr pat1) (cdr pat2) (cons (cons (car pat1) (car pat2)) bindings)))))))
    ((and (dm::pat-elem-is-a-variable? (car pat2)) (atom (car pat1)))
      (prn "pat1-elem is the atom %s pat2-elem is the variable" (car pat1) (car pat2))
      (let ((binding (assoc (car pat2) bindings)))
        (cond
          ((and binding (not (eql (car pat1) (cdr binding))))
            (prn "pat2-elem is already bound to a different value")
            nil)
          (binding
            (prn "pat2-elem is already bound to the same value")
            (with-indentation (unify2 (cdr pat1) (cdr pat2) bindings)))
          (t
            (prn "binding variaable %s to atom %s" (car pat2) (car pat1))
            (with-indentation
              (unify2 (cdr pat1) (cdr pat2) (cons (cons (car pat2) (car pat1)) bindings)))))))
    (t nil ;; (error "unhandled")
      )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (unify2 '(2 + 1) '(2 + 1))
  returns t)
(confirm that (unify2 '(,x + 1) '(2 + 1))
  returns (((\, x) . 2)))
(confirm that (unify2 '(2 + 1) '(,x + 1))
  returns (((\, x) . 2)))
(confirm that (unify2 '(,y + 1) '(,x + ,x))
  returns (((\, x) . 1) ((\, y) \, x)))
(confirm that (unify2 '(,x + 1) '(2 + ,y))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (unify2 '(,x + 1) '(2 + ,y + 1))
  returns nil)
(confirm that (unify2 '(,x + 1 + 2) '(2 + ,y + 3))
  returns nil)
(confirm that (unify2 '(,x + 1 + ,a) '(2 + ,y + ,a))
  returns (((\, y) . 1) ((\, x) . 2)))
(confirm that (unify2 '(,x + 1 + ,a) '(2 + ,y + ,b))
  returns (((\, a) \, b) ((\, y) . 1) ((\, x) . 2)))
(confirm that (unify2 '(,x + 1 + ,a) '(2 + ,y + ,b + 3))
  returns nil)
(confirm that (unify2 '(,x + 1) '(2 + ,x))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
