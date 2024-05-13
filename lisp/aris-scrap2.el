;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'aris-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:definterface generator
  ( (next (0 . 0))
    (each (1 . 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aos:defclass up-to-gen (from &optional to step) ()
  (next () (catch 'stop
             (let ((next (+ from step)))
               (if (< next to)
                 (setq from next)
                 (throw 'stop nil)))))
  ;; should probably go in a parent class:
  (each (f)
    (while-let ((val (next self)))
      (funcall f val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((g (make-up-to-gen 1 10 2)))
  (if (implements? g 'generator)
    (each g #'prn)
    (prn "Not a generator")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let ((gen (make-up-to-gen 1 10 2)))
;;   (while-let ((n (next gen)))
;;     (prn n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(symbol-plist 'generator)

(implements? (make-up-to-gen 1 10 2) 'generator)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(aos:implements? (make-up-to-gen 1 10 2) 'generator)
(aos:implements? 7 'generator)
