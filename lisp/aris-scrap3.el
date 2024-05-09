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





(dolist* (n '(1 2 3 4 . 5)) (prn "%s" n))


(let ((tail-1666 '(1 2 3 4 . 5)))
  (while tail-1666
    (let* ( (consp (consp tail-1666))
            (n (if consp (car tail-1666) tail-1666)))
      (prn "%s" n)
      (setq tail-1666 (if consp (cdr tail-1666) nil)))))



(dolist* (n '(1 2 3 4 5)) (prn "%s" n))
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


'\?x x?
(list ?x?x?x)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doconses (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Loop over a list's heads
Evaluate BODY with VAR bound to each cons from LIST and
CAR bound to each cons' `car', in turn.
Then evaluate RESULT to get return value, default nil.

This is a tiny, two-line modification of `dolist'.

 (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 3 (length spec) 4)
    (signal 'wrong-number-of-arguments (list '(3 . 4) (length spec))))
  (let ( (tail     (gensym "tail-"))
         (head     (car spec))
         (position (cadr spec))
         (lst      (caddr spec)))
    `(let ((,tail ,lst))
       (while ,tail
         (let ( (,head     (car ,tail))
                (,position ,tail))
           ,@body
           (setq ,tail (cdr ,tail))))
       ,@(cdr (cdr (cdr spec))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ((lst '(a 2 b 4 c 6 d 8)))
    (doconses (head pos lst lst)
      (when (eq 'c head)
        (setcar pos (cons head (cadr pos)))
        (setcdr pos (cddr pos)))))
  returns (a 2 b 4 (c . 6) d 8))
(confirm that 
  (let ((lst '(a 2 b 4 c 6 d 8)))
    (doconses (head pos lst lst)
      (setcar pos (cons head (cadr pos)))
      (setcdr pos (cddr pos))))
  returns ((a . 2) (b . 4) (c . 6) (d . 8)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let (toggle (lst '((a . 2) (b . 4) (c . 6) (d . 8))))
  (doconses (head pos lst lst)
    (when (not toggle)
      (let ((new (cons (cdr head) (cdr pos))))
        (setcar pos (car head))
        (setcdr pos new)
        (prn "%s" lst)
        (prn "new %s" new)
        (debug)))
    (setf toggle (not toggle))))
