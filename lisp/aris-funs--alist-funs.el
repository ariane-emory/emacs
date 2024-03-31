;; -*- fill-column: 80;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dash)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-dots-to-alist (alist)
  "Turn length 2 lists in ALIST into dotted pairs.

Examples:
(add-dots-to-alist '((a 1) (b 2) (c 3)))
⇒ ((a . 1) (b . 2) (c . 3))

(add-dots-to-alist '((a . 1) (b . 2) (c . 3)))
⇒ ((a . 1) (b . 2) (c . 3))

(add-dots-to-alist '((a . 1) (b 2) (c . 3)))
⇒ ((a . 1) (b . 2) (c . 3))"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (mapr
    alist
    (lambda (pair)
      (cond
        ((-cons-pair? pair) pair)
        ((> (length (cdr pair)) 1) pair)
        (t (cons (car pair) (cadr pair)))))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun merge-duplicate-alist-keys (alist &optional (use-dots t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A helper function used by aris-match-pattern to merge the values of duplicate
 ALIST.

Examples:
(merge-duplicate-alist-keys '((v . 1) (w . 2) (w . 3) (x . 4) (y . 5) (y . 6) (y . 7) (z . 8) (x . 9)) nil)
⇒ ((v 1) (w 2 3) (x 4 9) (y 5 6 7) (z 8))

(merge-duplicate-alist-keys '((v . 1) (w . 2) (w . 3) (x . 4) (y . 5) (y . 6) (y . 7) (z . 8) (x . 9)) t)
⇒ ((v . 1) (w 2 3) (x 4 9) (y 5 6 7) (z . 8))

(merge-duplicate-alist-keys '((v 1) (w 2) (w 3) (x 4) (y 5) (y 6) (y 7) (z 8) (x 9)) nil)
⇒ ((v 1) (w 2 3) (x 4 9) (y 5 6 7) (z 8))

(merge-duplicate-alist-keys '((v 1) (w 2) (w 3) (x 4) (y 5) (y 6) (y 7) (z 8) (x 9)) t)
⇒ ((v . 1) (w 2 3) (x 4 9) (y 5 6 7) (z . 8))"
  (let (result)
    (dolist (pair alist)
      (let* ( (key (car pair))
              (is-dotted (-cons-pair? pair))
              (tail (if is-dotted (list (cdr pair)) (cdr pair)))
              (existing (assoc key result)))
        (if existing
          (setcdr existing (nconc (cdr existing) tail))
          (push (cons key tail) result))))
    (nreverse
      (if (not use-dots)
        result
        (mapr
          result
          (lambda (pair)
            (if (length> (cdr pair) 1)
              pair
              (cons (car pair) (cadr pair)))))))))
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--alist-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
