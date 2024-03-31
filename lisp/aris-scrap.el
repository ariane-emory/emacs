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
(defun merge-duplicate-alist-keys (alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A helper function used by aris-match-pattern to merge the values of duplicate
 ALIST.

Example:
(aris-match-pattern--merge-duplicate-alist-keys '((y . 22) (x . 66) (w . 3) (w .) ⇒
  '((y . 22) (x . 66) (w 3 2 )) (v . 77))"
  (let (result)
    (dolist (pair alist)
      (let* ( (key (car pair))
              (value (cdr pair))
              (existing (assoc key result)))
        (if existing
          (setcdr existing (append (cdr existing) (list value)))
          (push (cons key (list value)) result))))
    (nreverse
      (mapcar
        (lambda (pair)
          (if (= (length (cdr pair)) 1)
            (cons (car pair) (car (cdr pair)))
            pair))
        result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(merge-duplicate-alist-keys '((v . 1) (w . 2)  (w . 3) (x . 4) (y . 5) (y . 6) (y . 6) (z . 7)))
;; ((v . 1) (w 2 3) (x . 4) (y 5 6 6) (z . 7))
(merge-duplicate-alist-keys '((v 1) (w 2)  (w 3) (x 4) (y 5) (y 6) (y 6) (z 7)))
;; ((v 1) (w (2) (3)) (x 4) (y (5) (6) (6)) (z 7))


