;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (thing  '(  aa
                   ,bb    ,(bb t)
                   ,@cc  ,@(cc t)
                   #'dd  #'(dd t)
                   'ee    '(ee t)
                   `ff    `(ff t)
                   ))
  (cond
    ((eq '\, (car-safe thing)) (prn "This one is special: %s" thing))
    ((eq '\,@ (car-safe thing)) (prn "This one is very special: %s" thing))
    ((eq 'function (car-safe thing)) (prn "This one is super special: %s" thing))
    ((eq 'quote (car-safe thing)) (prn "This one is kind of special: %s" thing))
    ((eq '\` (car-safe thing)) (prn "This one is extra special: %s" thing))
    ;; (t (prn "%s" thing))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


