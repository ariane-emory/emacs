;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (emacs-lisp-mode)
  (variable-pitch-mode -1)
  (aris-prettify-symbols-lisp)
  (display-fill-column-indicator-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (require 'aris-funs--lust-style-syntax)
  (begin
    ;; Test defining a function:
    ;; Reset the pattern dispatch table first so that we won't get
    ;; 'Pattern blah already defined' errors if we evaluate this
    ;; buffer twice:
    (setq *lust-style-syntax--pattern-dispatch-table* nil)

    (def w 8)
    (def x (1+ w))
    (def y '(w x))
    (def z (list w x))
    (def (fib 0) 0)
    (def (fib 1) 1)
    (def (fib n) (+ (fib (- n 1)) (fib (- n 2)))) 

    (def result
      (list
        (fib (car z))
        (fib (cadr z))
        (fib 10))))) ;; => (21 34 55)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dash)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapr (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over LST."
  (mapcar fn lst))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(merge-duplicate-alist-keys '((v . 1) (w . 2) (w . 3) (x . 4) (y . 5) (y . 6) (y . 7) (z . 8) (x . 9)) nil)
((v 1) (w 2 3) (x 4 9) (y 5 6 7) (z 8))

(merge-duplicate-alist-keys '((v . 1) (w . 2) (w . 3) (x . 4) (y . 5) (y . 6) (y . 7) (z . 8) (x . 9)) t)
((v . 1) (w 2 3) (x 4 9) (y 5 6 7) (z . 8))

(merge-duplicate-alist-keys '((v 1) (w 2) (w 3) (x 4) (y 5) (y 6) (y 7) (z 8) (x 9)) nil)
(v 1) (w 2 3) (x 4 9) (y 5 6 7) (z 8))

(merge-duplicate-alist-keys '((v 1) (w 2) (w 3) (x 4) (y 5) (y 6) (y 7) (z 8) (x 9)) t)
((v . 1) (w 2 3) (x 4 9) (y 5 6 7) (z . 8))

(message "======")




