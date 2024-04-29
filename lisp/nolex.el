;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-sym (sym)
  (let ((res
          (cond
            ((assoc sym swap-syms) (cdr (assoc sym swap-syms)))
            ((rassoc sym swap-syms) (car (rassoc sym swap-syms)))
            (t sym))))
    ;; (prn "swap %s for %s" sym res)
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar swap-syms
  '( (i . you)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar patterns
  '( (,desig1 ,verb1 that ,desig2 ,modal-verb never ,verb2 a ,noun)
     (,desig1 ,verb1 that ,desig2 ,modal-verb ,verb2 a ,noun)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prndiv)
(let ((pattern '(,desig1 ,verb1 that ,desig2 ,modal-verb ,verb2 a ,noun)))
  (dolist (target
            '( (i think that i would like a smoke)
               (i think that you would like a smoke)
               (i know that i could have a smoke)
               (i believe that you have seen a ghost)
               (you believe that i have seen a ghost)
               (i suspect that you have never seen a zebra)))
    (catch 'matched
      (dolist (pattern patterns)
        (when-let-alist (ap:match pattern target)
          (prn
            `( do ,(swap-sym .desig1) really ,.verb1 that ,(swap-sym .desig2)
               ,.modal-verb ,.verb2 a ,.noun \?))
          (throw 'matched nil))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

