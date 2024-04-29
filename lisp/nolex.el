;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *swap-words*
  '( (i . you)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ( (,desig1 ,verb1 that ,desig2 ,modal-verb never ,verb2 a ,noun) .
       ( do ,desig1 really ,verb1 that ,desig2 ,modal-verb never ,verb2 a ,noun \?))
     ( (,desig1 ,verb1 that ,desig2 ,modal-verb ,verb2 a ,noun) .
       ( do ,desig1 really ,verb1 that ,desig2 ,modal-verb  ,verb2 a ,noun \?))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-word (sym)
  (cond
    ((assoc sym  *swap-words*) (cdr (assoc sym *swap-words*)))
    ((rassoc sym *swap-words*) (car (rassoc sym *swap-words*)))
    (t sym)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun proc-input (target)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (prndiv)
  (prn "target: %s" target)
  (catch 'matched
    (dolist (pair *rules*)
      (let ((pattern (car pair)) (out-pattern (cdr pair)))
        ;;(prn "try pat: %s" pattern)
        (when-let ((alist (ap:match pattern target)))
          (prn
            (ap:fill out-pattern (rmapcar alist
                                   (lambda (kvp)
                                     (cons (car kvp) (swap-word (cdr kvp)))))))
          (throw 'matched t))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prndiv)
(prn "START:")
(dolist (target
          '( (i think that i would like a smoke)
             (i think that you would like a smoke)
             (i know that i could have a smoke)
             (i believe that you have seen a ghost)
             (you believe that i have seen a ghost)
             (i suspect that you have never seen a zebra)))
  (proc-input target))
(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

