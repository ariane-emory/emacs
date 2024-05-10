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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun properize (lst &optional rec improper-indicator)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively make a proper list from an improper list, recursively if REC,
optionally inserting an IMPROPER-INDICATOR before the last element of
lists that were originally improper."
  (cond
    ((atom lst) lst)
    ((and (cdr lst) (atom (cdr lst)))
      (cons
        (if rec
          (properize (car lst) rec improper-indicator)
          (car lst))
        (append
          (when improper-indicator (list improper-indicator))
          (list (cdr lst)))))
    (t
      (cons
        (if rec (properize (car lst) rec improper-indicator) (car lst))
        (properize (cdr lst) rec improper-indicator)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (properize nil) returns nil)
(confirm that (properize '(1 2 3)) returns (1 2 3))
(confirm that (properize '(1 2 . 3)) returns (1 2 3))
(confirm that (properize '(1 2 . 3) t '\.) returns (1 2 \. 3))
(confirm that (properize '(1 (2 3 . 4) . 5)) returns (1 (2 3 . 4) 5))
(confirm that (properize '(1 (2 3 . 4) . 5) t) returns (1 (2 3 4) 5))
(confirm that (properize '(1 (2 3 . 4) . 5) t '\.) returns (1 (2 3 \. 4) \. 5))
(confirm that (properize '(1 (2 3 . 4) . 5) nil '\.) returns (1 (2 3 . 4) \. 5))
(confirm that (properize '((0 . 1) (2 3 . 4) . 5) t '\.)
  returns ((0 \. 1) (2 3 \. 4) \. 5))
(confirm that (properize '((0 . 1)  (2 3 . 4) . 5) nil '\.)
  returns ((0 . 1) (2 3 . 4) \. 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun properize! (lst &optional rec improper-indicator)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively make a proper list from an improper list, recursively if REC."
  (doconses (head pos lst lst)
    (when (and rec (consp head))
      (setcar pos (properize! head rec improper-indicator)))
    (when (and (cdr pos) (atom (cdr pos)))
      (setcdr pos
        (append
          (when improper-indicator (list improper-indicator))
          (list (cdr pos))))
      (setf pos (cdr pos)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (properize! nil) returns nil)
(confirm that (properize! '(1 2 3)) returns (1 2 3))
(confirm that (properize! '(1 2 . 3)) returns (1 2 3))
(confirm that (properize! '(1 (2 3 . 4) . 5)) returns (1 (2 3 . 4) 5))
(confirm that (properize! '(1 (2 3 . 4) . 5) t) returns (1 (2 3 4) 5))
(confirm that (properize! '((0 . 1) (2 3 . 4) . 5) t '\.)
  returns ((0 \. 1) (2 3 \. 4) \. 5))
(confirm that (properize! '((0 . 1)  (2 3 . 4) . 5) nil '\.)
  returns ((0 . 1) (2 3 . 4) \. 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


