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

(defun fun (expr)
  (dolist* (thing pos expr)
    (prndiv)
    (prn "pos:   %s" pos)
    (prn "thing: %s" thing)
    ;; (debug)
    (when (consp thing) (with-indentation (fun thing)))))

;; (fun '(,(x integer?) ,(y integer? (< x _ . 333))))

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
        bindings)
      (prndiv))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unify1 '(,x + 1) '(2 + ,y))
(unify1 '(,x + 1) '(2 + ,y + 1))
(unify1 '(,x + 1 + 2) '(2 + ,y + 3))
(unify1 '(,x + 1 + ,a) '(2 + ,y + ,a)) 
(unify1 '(,x + 1 + ,a) '(2 + ,y + ,b))
(unify1 '(,x + 1 + ,a) '(2 + ,y + ,b + 3)) 
(unify1 '(,x + 1) '(2 + ,x)) ; bad case.


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
  ;; (debug bindings x)
  (cond
    ((null bindings) nil)
    ((eq bindings t) x)
    ((and (dm::pat-elem-is-a-variable? x) (assoc x bindings))
      (subst-bindings bindings (cdr (assoc x bindings))))
    ((atom x) x)
    (t (reuse-cons (subst-bindings bindings (car x))
         (subst-bindings bindings (cdr x)) x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unifier (pat1 pat2)
  (let ((bindings (unify1 pat1 pat2)))
    (subst-bindings bindings pat1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (unify1 '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))
(unifier '(,x + 1 + ,a + ,x) '(2 + ,y + ,x + 2))

;; (unifier '(,x + 1 + ,a) '(2 + ,y + ,a))
