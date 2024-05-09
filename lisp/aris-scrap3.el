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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun properize-target (lst &optional no-test)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Properize a target."
  (unless (or no-test (listp lst)) (error "LST is not a list: %s" lst))
  (when   (or no-test (not (proper-list-p lst)))
    (let ((last (last lst)))
      (setcdr last (list '\. (cdr last)))))
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (properize-target  nil)       returns nil)
(confirm that (properize-target '(2))       returns (2))
(confirm that (properize-target '(2 . 4))   returns (2 \. 4))
(confirm that (properize-target '(2 4   6)) returns (2 4 6))
(confirm that (properize-target '(2 4 . 6)) returns (2 4 \. 6))
;; (dont confirm that (properize-target '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun properize-pattern (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Properize a pattern."
  (unless (listp lst) (error "LST is not a list: %s" lst))
  (if (not (proper-list-p lst))
    (properize-target lst t) ; target fun works in this case.
    (let ((tail (last lst 2)))
      (when (eq (car tail)  '\,) ; will also need to detect UNSPLICE!
        (setcar tail '\.)
        (setcdr tail (list (list '\, (cadr tail))))))
    lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (properize-pattern  nil)          returns nil)
(confirm that (properize-pattern '(,x))         returns ((\, x)))
(confirm that (properize-pattern '(,x . y))     returns ((\, x) \. y))
(confirm that (properize-pattern '(,x .,y))     returns ((\, x) \. (\, y)))
(confirm that (properize-pattern '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
(confirm that (properize-pattern '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
(confirm that (properize-pattern '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
;; (dont confirm that (properize-pattern '(,x . ,y ,z)) ... ; bulshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


