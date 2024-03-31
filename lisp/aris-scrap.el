;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (emacs-lisp-mode)
  (variable-pitch-mode -1)
  (aris-prettify-symbols-lisp)
  (display-fill-column-indicator-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when t
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
        (fib 10))))
  ) ;; => (21 34 55)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dash)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-flatten-alist-keys (alist)
  "Flatten the keys of ALIST if they are lists.

Examples:
(`aris-flatten-alist-keys') '((a 1) (b 2 2) (c 3) (d 4 5 6)))
⇒ ((a . 1) (b . 2) (b . 2) (c . 3) (d . 4) (d . 5) (d . 6))

(`aris-add-dots-to-alist'
  (`aris-flatten-alist-keys' '((a 1) (b 2 2) (c 3) (d 4 5 6))))
⇒ ((a . 1) (b . 2) (b . 2) (c . 3) (d . 4) (d . 5) (d . 6))"
  (let (result)
    (dolist (pair alist)
      (let* ( (key (car pair))
              (values (cdr pair))
              (values (if (proper-list-p values) values (list values))))
        (dolist (value values)
          (push (cons key value) result))))
    (nreverse result)))


((a . 1) (b . 2) (b . 2) (c . 3) (d . 4) (d . 5) (d . 6))
