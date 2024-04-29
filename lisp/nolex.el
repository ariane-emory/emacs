;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar swap-syms
  '( (i . you)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-sym (sym)
  (let ((res
          (cond
            ((assoc sym swap-syms) (cdr (assoc sym swap-syms)))
            ((rassoc sym swap-syms) (cdr (rassoc sym swap-syms)))
            (t sym))))
    (prn "swap %s for %s" sym res)
    res))
(swap-sym 'i)
(swap-sym 'you)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((pattern '(i ,verb1 ,desig ,modal-verb ,verb2 a ,thing)))
  (dolist (target
            '( (i think i would like a smoke)
               (i know i could have a smoke)
               (i believe i have seen a ghost)
               (i suspect you have (never seen) a (red car))))
    ;; (prn "pattern: %s" pattern)
    ;; (prn "target:  %s" target)
    (when-let-alist (easy-match pattern target)
      (prn
        (flatten `(do you really ,.verb1 that ,(swap-sym .desig) ,.modal-verb ,.verb2 a ,.thing \?))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
