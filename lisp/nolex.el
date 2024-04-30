;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--easy-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *swap-words*
  '( (i . you)
     (do . don\'t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ( (,subj ,bar ,baz) .
       (fine \, ,subj ,bar ,baz \, so what \?))
     ( (,subj ,modal-verb ,verb a ,thing) .
       (so just go ,verb a ,thing \!))
     ( (,subj ,modal-verb never ,verb a ,thing) .
       (,subj ,modal-verb ,verb a ,thing \!))
     ( (you ,do-don\'t ,verb \!) .
       (you ,do-don\'t ,verb \!))
     ( (,subj ,do-don\'t ,verb \!) .
       (,subj ,do-don\'t ,verb \!))
     ( (,subj ,verb that ,subj-2 ,modal-verb never ,verb-2 a ,noun) .
       ( come on \, ,subj can\'t really ,verb that
         ,subj-2 ,modal-verb never ,verb-2 a ,noun \!))
     ( (,subj ,verb that ,subj-2 ,modal-verb ,verb-2 a ,noun) .
       ( do ,subj really ,verb that ,subj-2 ,modal-verb ,verb-2 a ,noun \?))
     ( t . (i don\'t understand \!))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-word (sym)
  "Return the swapped word for SYM found in *SWAP-WORDS*, or SYM if none."
  (let ((res
          (cond
            ((assoc sym  *swap-words*) (cdr (assoc sym *swap-words*)))
            ((rassoc sym *swap-words*) (car (rassoc sym *swap-words*)))
            (t sym))))
    ;; (prn "swap %s for %s" sym res)
    res
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-response (input)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Transform INPUT according to *RULES*, returning nil if none match."
  (catch 'result
    (dolist (pair *rules*)
      (let ((pattern (car pair)) (response (cdr pair)))
        (if (eq pattern t)
          (throw 'result response)
          ;; (prn "  try:     %s" pattern)
          (when-let ((alist (with-indentation (ap:match pattern input))))
            (throw 'result
              (ap:fill response (mapcdar #'swap-word alist)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prndiv)
(prn "START:")
(dolist (input
          '( (i think that i would like a smoke)
             (i think that you would like a smoke)
             (i think that you should have a smoke)
             (i know that i could have a smoke)
             (i believe that you have seen a ghost)
             (you believe that i have seen a ghost)
             (i suspect that you have never seen a zebra)
             (i know that you have never eaten a hamburger)
             (you would never eat a hamburger)
             (you should never eat a hamburger)
             (i could eat a hamburger)
             (foo bar baz)
             (you don\'t understand)
             (i don\'t understand \!)
             (you eat chickens)
             (you suck ass \!)))
  (prndiv)
  (prn "INPUT:     %s" input)
  (prn "RESPONSE:  %s" (get-response input)))
(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
