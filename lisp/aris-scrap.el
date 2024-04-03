;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--alist-funs)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--error-when-and-error-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (pd--reset)
  (def (fib 0) 0)
  (def (fib 1) 1)
  (def (fib n)  (+ (fib (- n 1)) (fib (- n 2))))
  (def (double n) (+ n n))
  (def (square y) (* y y))
  ;; (def (double-square y) (double 2 (square y)))
  ;; (double-square 3)

  (let ( (*match-pattern--verbose* nil))
    (error-unless "You broke (fib 4): %s" '(it) (= 3 (fib 4)))
    (error-unless "You broke (fib 10): %s" '(it) (= 55 (fib 10)))
    (error-unless "You broke (double 9): %s" '(it) (= 18 (double 9)))
    (error-unless "You broke (square 7): %s" '(it) (= 49 (square 7))))
  
  (message "Printing the table:")
  (pd--print-table))

;; todo:
;;   continue match2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pd--get-group 'fib)
(pd--print-group (pd--get-group 'fib))
(pd--format-group-as-lines (pd--get-group 'fib))
(pd--format-group-as-string (pd--get-group 'fib))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (match2 '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))

  (let ( (*match-pattern2--verbose* t)
         (*match-pattern2--merge-duplicate-alist-keys* nil))
    (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

  (let ( (*match-pattern2--verbose* t)
         (*match-pattern2--merge-duplicate-alist-keys* t))
    (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

  (aris-add-dots-to-alist
    (aris-merge-duplicate-alist-keys
      '( (a . 1)
         (a . 2)
         (a . 3)
         (a . 4)
         (a . 5)
         (b . 8)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
