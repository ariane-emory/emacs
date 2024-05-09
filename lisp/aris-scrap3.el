;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (thing  '( aa
                   ,bb
                   ,(bb t)
                   ,@cc
                   ,@(cc t)
                   #'dd
                   #'(dd t)
                   'ee
                   '(ee t)
                   `ff
                   `(ff t)
                   )) ; '(1 2 ,3 4 ,@5 #'6 '7 `8))
  (cond
    ((eq '\, (car-safe thing)) (prn "This one is special: %s" thing))
    ((eq '\,@ (car-safe thing)) (prn "This one is very special: %s" thing))
    ((eq 'function (car-safe thing)) (prn "This one is super special: %s" thing))
    ((eq 'quote (car-safe thing)) (prn "This one is kind of special: %s" thing))
    ((eq '\` (car-safe thing)) (prn "This one is extra special: %s" thing))
    ;; (t (prn "%s" thing))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro dolist2 (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

\(fn (VAR LIST [RESULT]) BODY...)"
  (unless (consp spec)     (signal 'wrong-type-argument (list 'consp spec)))
  (unless (length= spec 2) (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  (let ( (tail (gensym "tail-"))
         (var  (car (cdr spec)))
         (lst  (car spec)))
    `(let ((,tail ,var))
       (while ,tail
         (let ((,lst (car ,tail)))
           ,@body
           (setq ,tail (cdr ,tail)))))))


(dolist2 (n '(1 2 3 4 5)) (prn "%s" n))


(let ((tail-1655 '(1 2 3 4 5)))
  (while tail-1655
    (let ((n (car tail-1655)))
      (prn "%s" n)
      (setq tail-1655 (cdr tail-1655)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((tail '(1 2 3 4 . 5)))
  (while tail
    (let ((n (if (consp tail) (car tail) tail)))
      (prn "%s" n)
      (setq tail (if (consp tail) (cdr tail) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((lst '(1 2 3 4 . 5))
       (result '()))
  (while lst
    (setq result (cons (if (consp lst) (car lst) lst) result))
    (setq lst (if (consp lst) (cdr lst) nil)))
  (message "Result: %s" (nreverse result)))


