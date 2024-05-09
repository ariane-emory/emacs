;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(dolist* (n '(1 2 3 4 . 5)) (prn "%s" n))


(let ((tail-1666 '(1 2 3 4 . 5)))
  (while tail-1666
    (let* ( (consp (consp tail-1666))
            (n (if consp (car tail-1666) tail-1666)))
      (prn "%s" n)
      (setq tail-1666 (if consp (cdr tail-1666) nil)))))



(dolist* (n '(1 2 3 4 5)) (prn "%s" n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((tail '(1 2 3 4 . 5)))
  (while tail
    (let ((n (if (consp tail) (car tail) tail)))
      (prn "%s" n)
      (setq tail (if (consp tail) (cdr tail) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((lst '(1 2 3 4 . 5))
       (result '()))
  (while lst
    (setq result (cons (if (consp lst) (car lst) lst) result))
    (setq lst (if (consp lst) (cdr lst) nil)))
  (message "Result: %s" (nreverse result)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doconses (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    `(let ((,position ,lst))
       (while ,position
         (let ( (,head     (car ,position))
                ;; (,position ,tail)
                )
           ,@body
           (setq ,position (cdr ,position))))
       ,@(cdr (cdr (cdr spec))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that 
  (let ((lst '(a 2 b 4 c 6 d 8)))
    (doconses (head pos lst lst)
      (when (eq 'c head)
        (setcar pos (cons head (cadr pos)))
        (setcdr pos (cddr pos)))))
  returns (a 2 b 4 (c . 6) d 8))
(confirm that ; plist to alist
  (let ((lst '(a 2 b 4 c 6 d 8)))
    (doconses (head pos lst lst)
      (setcar pos (cons head (cadr pos)))
      (setcdr pos (cddr pos))))
  returns ((a . 2) (b . 4) (c . 6) (d . 8)))
(confirm that ; plist to let varlist
  (let ((lst '(a 2 b 4 c 6 d 8)))
    (doconses (head pos lst lst)
      (setcar pos (list head (cadr pos)))
      (setcdr pos (cddr pos))))
  returns ((a 2) (b 4) (c 6) (d 8)))
(confirm that ; alist to plist 
  (let ((lst '((a . 2) (b . 4) (c . 6) (d . 8))))
    (doconses (head pos lst lst)
      (let ((new (cons (cdr head) (cdr pos))))
        (setcar pos (car head))
        (setcdr pos new)
        (setf pos (cdr pos)))))
  returns (a 2 b 4 c 6 d 8))
(confirm that ; plist to let varlist
  (let ((lst '((a . 2) (b . 4) (c . 6) (d . 8))))
    (doconses (head pos lst lst)
      (let ((new (cons (cdr head) (cdr pos))))
        (setcar pos (list (car head) (cdr head))))))
  returns ((a 2) (b 4) (c 6) (d 8)))
(confirm that
  (let ((lst '(,x . ,y)))
    (doconses (head pos lst lst)
      (when (eq '\, head)
        (setcar pos (list head (cadr pos)))
        (setcdr pos (cddr pos)))))
  returns ((\, x) (\, y)))
(confirm that
  (let ((lst '(,x . ,y)))
    (doconses (head pos lst lst)
      (when (eq '\, head)
        (let ((new (list (list head (cadr pos)))))
          (setcar pos '\.)
          (setcdr pos new)
          (setf pos (cdr pos))))))
  returns ((\, x) \. (\, y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; properize a pattern
(let* ( (lst '(,x ,y . ,z))
        (tail (last lst 2)))
  (when (eq (car tail)  '\,)
    (setcar tail '\.)
    (setcdr tail (list (list '\, (cadr tail)))))
  lst) ; ((\, x) (\, y) \. (\, z))

;; properize a target 
(let ((lst '(2 4 . 6)))
  (when (and (listp lst) (not (proper-list-p lst)))
    (let ((last (last lst)))
      (prn "last %s" last)
      (setcdr last (list '\. (cdr last)))))
  lst) ; (2 4 \. 6)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun properize-pattern (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Properize a pattern."
  (unless (listp lst)
    (error "LST is not a list: %s" lst))
  (let* ((tail (last lst 2)))
    (when (eq (car tail)  '\,)
      (setcar tail '\.)
      (setcdr tail (list (list '\, (cadr tail)))))
    lst)) ; ((\, x) (\, y) \. (\. z))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (properize-pattern '(,x . ,y)) returns ((\, x) \. (\, y)))
(confirm that (properize-pattern '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
(confirm that (properize-pattern '(,x ,y ,z)) returns ((\, x) (\, y) (\, z)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


