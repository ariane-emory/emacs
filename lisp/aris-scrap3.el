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
    (while (and pat1 pat2)
      (prndiv)
      (prn "pat:  %s" pat1)
      (prn "pat2: %s" pat2)
      (let ( (pat-elem (car pat1))
             (pat2-elem (car pat2)))
        (prn "pat-elem:  %s" pat-elem)
        (prn "pat2-elem: %s" pat2-elem)
        (prndiv ?\-)
        (cond
          ((dm::pat-elem-is-a-variable? pat-elem)
            (prn "pat-elem is a variable")
            (push (cons pat-elem pat2-elem) bindings)
            (setq pat1 (cdr pat1))
            (setq pat2 (cdr pat2)))
          ((dm::pat-elem-is-a-variable? pat2-elem)
            (prn "pat2-elem is a variable")
            (push (cons pat2-elem pat-elem) bindings)
            (setq pat1 (cdr pat1))
            (setq pat2 (cdr pat2)))
          ((equal pat-elem pat2-elem)
            (prn "pat-elem and pat2-elem are equal")
            (setq pat1 (cdr pat1))
            (setq pat2 (cdr pat2)))
          (t (prn "pat-elem and pat2-elem are not equal")
            (setq pat1 nil)
            (setq pat2 nil))))
      (prn "bindings: %s" bindings))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unify1 '(,x + 1) '(2 + ,y))
(prnl 2)
(unify1 '(,x + 1) '(2 + ,y + 1))

