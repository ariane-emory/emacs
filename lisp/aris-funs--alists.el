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
(`add-dots-to-alist'
  '((a 1) (b 2) (c 3))) ⇒
   ((a . 1) (b . 2) (c . 3))

(`add-dots-to-alist'
  '((a . 1) (b . 2) (c . 3))) ⇒
   ((a . 1) (b . 2) (c . 3))

(`add-dots-to-alist'
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
(defun remove-dots-from-alist (alist)
  "Turn  dotted pairs in ALIST into length 2 lists.

Examples:
(`remove-dots-from-alist'
  '((a . 1) (b . 2) (c . 3) (d . 4))) ⇒
    ((a 1) (b 2) (c 3) (d 4))

(`remove-dots-from-alist'
  '((a . 1) (b 2 2) (c . 3) (d 4))) ⇒
    ((a 1) (b 2 2) (c 3) (d 4))

(`remove-dots-from-alist'
  '((a 1) (b 2 2) (c 3) (d 4))) ⇒
    ((a 1) (b 2 2) (c 3) (d 4))

(`remove-dots-from-alist'
  (`add-dots-to-alist'
    (`remove-dots-from-alist'
      (`add-dots-to-alist'
        '((a 1) (b 2) (c 3)))))) ⇒
         ((a 1) (b 2) (c 3))

(`remove-dots-from-alist'
  (`remove-dots-from-alist'
    (`add-dots-to-alist'
      (`add-dots-to-alist'
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
(defun merge-duplicate-alist-keys (alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A helper function used by `match-pattern' to merge the values of duplicate
 ALIST. This turns dotted pairs into length 2 lists in the process: if you preferred
the dotted pairs, apply `add-dots-to-alist' to the result to restore them.

Examples:
(`merge-duplicate-alist-keys'
  '((v . 1) (w . 2) (w . 3) (x . 4) (y . 5)
    (y . 6) (y . 7) (z . 8) (x . 9))) ⇒
     ((v 1) (w 2 3) (x 4 9) (y 5 6 7) (z 8))

(`merge-duplicate-alist-keys'
  '((v 1) (w 2) (w 3) (x 4) (y 5)
    (y 6) (y 7) (z 8) (x 9))) ⇒
     ((v 1) (w 2 3) (x 4 9) (y 5 6 7) (z 8))

(`remove-dots-from-alist'
  (`add-dots-to-alist'
    (`remove-dots-from-alist'
      (`add-dots-to-alist'
        '((a 1) (b 2) (c 3)))))) ⇒ ((a 1) (b 2) (c 3))

(`remove-dots-from-alist'
  (`remove-dots-from-alist'
    (`add-dots-to-alist'
      (`add-dots-to-alist' '((a 1) (b 2) (c 3))))))"
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
(defun flatten-alist-values (alist)
  "Flatten the values of ALIST if they are lists.

Examples:
(`flatten-alist-values')
  '((a 1) (b 2 2) (c 3) (d 4 5 6))) ⇒
    ((a . 1) (b . 2) (b . 2) (c . 3) (d . 4) (d . 5) (d . 6))

(`add-dots-to-alist'
  (`flatten-alist-values'
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
(defun alist-to-plist (alist)
  "Convert a dotted alist ALIST to a plist."
  (unless (list? alist) (error "ALIST must be a list"))
  (let (result tail)
    (while alist
      (let* ((pair  (pop alist))
              (key   (car pair))
              (value (cdr pair)))
        (let ((new-tail (list key value)))
          (if tail
            (rplacd! tail new-tail)
            (setq result new-tail))
          (setq tail (cdr new-tail))
          )))
    result))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--alists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
