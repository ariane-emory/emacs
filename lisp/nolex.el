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
        (flatten `(do you really ,.verb1 that ,.desig ,.modal-verb ,.verb2 a ,.thing \?))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
