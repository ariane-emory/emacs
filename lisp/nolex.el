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
(defvar rules
  '( ((,desig1 ,verb1 that ,desig2 ,modal-verb never ,verb2 a ,noun) .
       ( do ,desig1 really ,verb1 that ,desig2 ,modal-verb ,verb2 a ,noun \?))
     ;; (,desig1 ,verb1 that ,desig2 ,modal-verb ,verb2 a ,noun)
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prndiv)
(dolist (target
          '( (i think that i would like a smoke)
             (i think that you would like a smoke)
             (i know that i could have a smoke)
             (i believe that you have seen a ghost)
             (you believe that i have seen a ghost)
             (i suspect that you have never seen a zebra)))
  (catch 'matched
    (dolist (pair rules)
      (let ((pattern (car pair)) (out-pattern (cdr pair)))
        (when-let ((alist (ap:match pattern target)))
          (prn
            (ap:fill out-pattern (rmapcar alist (lambda (kvp)
                                                  (prn "kvp %s" kvp)
                                                  (cons (car kvp)
                                                    (swap-sym (cdr kvp))))))
            ;; `( do ,(swap-sym .desig1) really ,.verb1 that ,(swap-sym .desig2)
            ;;    ,.modal-verb ,.verb2 a ,.noun \?)
            )
          (throw 'matched nil))
        ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

