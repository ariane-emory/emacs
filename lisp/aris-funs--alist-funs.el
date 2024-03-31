;; -*- fill-column: 80;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dash)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-add-dots-to-alist (alist)
  "Turn length 2 lists in ALIST into dotted pairs.

Examples:
(`aris-add-dots-to-alist'
  '((a 1) (b 2) (c 3))) ⇒
   ((a . 1) (b . 2) (c . 3))

(`aris-add-dots-to-alist'
  '((a . 1) (b . 2) (c . 3))) ⇒
   ((a . 1) (b . 2) (c . 3))

(`aris-add-dots-to-alist'
  '((a . 1) (b 2) (c . 3))) ⇒
    ((a . 1) (b . 2) (c . 3))"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (unless (proper-list-p alist)
    (error "Not a proper list!"))
  (mapr
    alist
    (lambda (pair)
      (cond
        ((-cons-pair? pair) pair)
        ((> (length (cdr pair)) 1) pair)
        (t (cons (car pair) (cadr pair)))))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-remove-dots-from-alist (alist)
  "Turn  dotted pairs in ALIST into length 2 lists.

Examples:
(`aris-remove-dots-from-alist'
  '((a . 1) (b . 2) (c . 3) (d . 4))) ⇒
    ((a 1) (b 2) (c 3) (d 4))

(`aris-remove-dots-from-alist'
  '((a . 1) (b 2 2) (c . 3) (d 4))) ⇒
    ((a 1) (b 2 2) (c 3) (d 4))

(`aris-remove-dots-from-alist'
  '((a 1) (b 2 2) (c 3) (d 4))) ⇒
    ((a 1) (b 2 2) (c 3) (d 4))

(`aris-remove-dots-from-alist'
  (`aris-add-dots-to-alist'
    (`aris-remove-dots-from-alist'
      (`aris-add-dots-to-alist'
        '((a 1) (b 2) (c 3)))))) ⇒
         ((a 1) (b 2) (c 3))

(`aris-remove-dots-from-alist'
  (`aris-remove-dots-from-alist'
    (`aris-add-dots-to-alist'
      (`aris-add-dots-to-alist'
       '((a 1) (b 2) (c 3)))))) ⇒
        ((a 1) (b 2) (c 3))"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (unless (proper-list-p alist)
    (error "Not a proper list!"))
  (mapr
    alist
    (lambda (pair)
      (message "pair %s"pair)
      (cond
        ((-cons-pair? pair)
          (cons (car pair) (list (cdr pair))))
        ((atom pair) (error "Improper alist."))
        (t pair)))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-merge-duplicate-alist-keys (alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A helper function used by `aris-match-pattern' to merge the values of duplicate
 ALIST. This turns dotted pairs into length 2 lists in the process: if you preferred
the dotted pairs, apply `aris-add-dots-to-alist' to the result to restore them.

Examples:
(`aris-merge-duplicate-alist-keys'
  '((v . 1) (w . 2) (w . 3) (x . 4) (y . 5)
    (y . 6) (y . 7) (z . 8) (x . 9))) ⇒
     ((v 1) (w 2 3) (x 4 9) (y 5 6 7) (z 8))

(`aris-merge-duplicate-alist-keys'
  '((v 1) (w 2) (w 3) (x 4) (y 5)
    (y 6) (y 7) (z 8) (x 9))) ⇒
     ((v 1) (w 2 3) (x 4 9) (y 5 6 7) (z 8))

(`aris-remove-dots-from-alist'
  (`aris-add-dots-to-alist'
    (`aris-remove-dots-from-alist'
      (`aris-add-dots-to-alist'
        '((a 1) (b 2) (c 3)))))) ⇒ ((a 1) (b 2) (c 3))

(`aris-remove-dots-from-alist'
  (`aris-remove-dots-from-alist'
    (`aris-add-dots-to-alist'
      (`aris-add-dots-to-alist' '((a 1) (b 2) (c 3))))))"
  (unless (proper-list-p alist)
    (error "Not a proper list!"))
  (let (result)
    (dolist (pair alist)
      (let* ( (key (car pair))
              (is-dotted (-cons-pair? pair))
              (tail (if is-dotted (list (cdr pair)) (cdr pair)))
              (existing (assoc key result)))
        (if existing
          (setcdr existing (nconc (cdr existing) tail))
          (push (cons key tail) result))))
    (nreverse result)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-flatten-alist-values (alist)
  "Flatten the values of ALIST if they are lists.

Examples:
(`aris-flatten-alist-values')
  '((a 1) (b 2 2) (c 3) (d 4 5 6))) ⇒
    ((a . 1) (b . 2) (b . 2) (c . 3) (d . 4) (d . 5) (d . 6))

(`aris-add-dots-to-alist'
  (`aris-flatten-alist-values'
   '((a 1) (b 2 2) (c 3) (d 4 5 6)))) ⇒
     ((a . 1) (b . 2) (b . 2) (c . 3) (d . 4) (d . 5) (d . 6))"
  (unless (proper-list-p alist)
    (error "Not a proper list!"))
  (let (result)
    (dolist (pair alist)
      (let* ( (key (car pair))
              (values (cdr pair))
              (values (if (proper-list-p values) values (list values))))
        (dolist (value values)
          (push (cons key value) result))))
    (nreverse result)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--alist-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
