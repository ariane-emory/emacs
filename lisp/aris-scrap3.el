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

(dm:match '(foo '(,bar ,baz)) '(foo '(,bar ,baz)))


'(foo '(,bar ,baz))


'(foo (,bar ,baz))

(length '(foo bar))
(quote (foo bar))

(dm:match '(,x (,y . ,z)) '(1 (2 . 3)))

(listp '(2 . 3))


(progn
  (setq il '(1 2 3 4 . 5))
  (prndiv)
  (while il
    (let ((head (if (listp (cdr il)) (car il) (car il))))
      (prn "il:   %s" il)
      (prn "head: %s" head)
      (if (listp (cdr il))
        (setf il (cdr il))
        (setf il nil))
      (debug))))


(let ((lst '(1 2 3 4 . 5))
       (result '()))
  (while (consp lst)
    (setq result (cons (car lst) result))
    (setq lst (cdr lst)))
  (message "Result: %s" (nreverse result)))

(let ((lst '(1 2 3 4 . 5))
       (result '()))
  (while lst
    (push (if (consp lst) (car lst) lst) result)
    (setf lst (when (consp lst) (cdr lst))))
  (message "Result: %s" (nreverse result)))
