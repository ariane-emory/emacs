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
    (while (or pat1 pat2)
      (prndiv)
      (prn "pat1: %s" pat1)
      (prn "pat2: %s" pat2)
      (let ( (pat1-elem (car pat1))
             (pat2-elem (car pat2)))
        (prn "pat1-elem:  %s" pat1-elem)
        (prn "pat2-elem: %s" pat2-elem)
        (prndiv ?\-)
        (cond
          ((not (and pat1 pat2))
            ;; not unifiable, return no bindings:
            (prn "not unifiable, return no bindings")
            (setq pat1 nil)
            (setq pat2 nil)
            (setq bindings nil))
          ((and (dm::pat-elem-is-a-variable? pat1-elem)
             (dm::pat-elem-is-a-variable? pat2-elem)
             (eq (dm::pat-elem-var-sym pat1-elem)
               (dm::pat-elem-var-sym pat2-elem))
             )
            (prn "pat1-elem and pat2-elem are the same variable")
            (pop pat1)
            (pop pat2))
          ((and (dm::pat-elem-is-a-variable? pat1-elem)
             (dm::pat-elem-is-a-variable? pat2-elem)
             ;; (not (eq (dm::pat-elem-var-sym pat1-elem)
             ;;      (dm::pat-elem-var-sym pat2-elem)))
             )
            (prn "both pat1-elem and pat2-elem are variables")
            (push (cons pat1-elem pat2-elem) bindings)
            (pop pat1)
            (pop pat2))
          ((dm::pat-elem-is-a-variable? pat1-elem)
            (prn "pat1-elem is a variable")
            (push (cons pat1-elem pat2-elem) bindings)
            (pop pat1)
            (pop pat2))
          ((dm::pat-elem-is-a-variable? pat2-elem)
            (prn "pat2-elem is a variable")
            (push (cons pat2-elem pat1-elem) bindings)
            (pop pat1)
            (pop pat2))
          ((equal pat1-elem pat2-elem)
            (prn "pat1-elem and pat2-elem are equal")
            (pop pat1)
            (pop pat2))
          (t (prn "pat1-elem and pat2-elem are not equal")
            (setq pat1 nil)
            (setq pat2 nil)
            (setq bindings nil))))
      (prn "bindings: %s" bindings)
      ;;(debug pat1 pat2 bindings)
      )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unify1 '(,x + 1) '(2 + ,y))
(prnl 2)
(unify1 '(,x + 1) '(2 + ,y + 1))
(unify1 '(,x + 1 + 2) '(2 + ,y + 3))
(unify1 '(,x + 1 + ,a) '(2 + ,y + ,a)) 
(unify1 '(,x + 1 + ,a) '(2 + ,y + ,b)) 
