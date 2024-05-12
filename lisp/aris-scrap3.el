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

;; (dm:match '(,(x integer?) ,(y integer? (< x _))) '(7 9))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro walk* (lst &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Walk a possibly improper list."
;;   `(let ((pos ,lst))
;;      (while pos
;;        ,@body
;;        (setf pos (if (atom pos) nil (cdr pos))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fun (expr)
  (dolist* (thing pos expr)
    (prndiv)
    (prn "pos:   %s" pos)
    (prn "thing: %s" thing)
    ;; (debug)
    (when (consp thing) (with-indentation (fun thing)))))

(fun '(,(x integer?) ,(y integer? (< x _ . 333))))

