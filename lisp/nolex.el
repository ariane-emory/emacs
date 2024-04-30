;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  "Return the swapped word for SYM found in *SWAP-WORDS*, or SYM if none."
  (cond
    ((assoc sym  *swap-words*) (cdr (assoc sym *swap-words*)))
    ((rassoc sym *swap-words*) (car (rassoc sym *swap-words*)))
    (t sym)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun proc-input (target)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Transform INPUT according to *RULES*, returning nil if none match."
  (prndiv)
  (prn "target: %s" target)
  (if-let ((res
             (catch 'result
               (dolist (pair *rules*)
                 (let ((pattern (car pair)) (out-pattern (cdr pair)))
                   ;;(prn "try pat: %s" pattern)
                   (when-let ((alist (ap:match pattern target)))
                     (throw 'result
                       (ap:fill out-pattern
                         (mapcdar #'swap-word alist)))))))))
    res
    '(i don\'t understand \!)))
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
             (i suspect that you have never seen a zebra)
             (foo bar baz)))
  (prn (proc-input target)))
(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


