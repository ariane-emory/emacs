;; -*- fill-column: 80;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'dash)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-set (key alist value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively set a KEY in ALIST to VALUE by building a new alist in
which KEY is set to VALUE, adding a new key/value pair if it wasn't already present.

This is meant for non-dotted ALISTs and might produce unexpexted results if
applied to dotted ALISTs. As usual, `add-dots-to-alist'/`remove-dots-from-alist'
may be applied before or after to get your desired result."
  (unless (proper-list-p alist)
    (error "ALIST must be a proper list, not %S!" alist))
  (let (result key-found)
    (dolist (pair alist)
      (if (not (eq (car pair) key))
        (push pair result)
        (push (cons key value) result)
        (setq key-found t)))
    (unless key-found
      (push (cons key value) result))
    (nreverse result)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq alist '((a . 1) (b . 2) (c . 3)))
;; (alist-set 'b alist 4)
;; (alist-set 'd alist 5)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alist-set! (key alist value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively set a KEY in ALIST to VALUE by modifying the alist in
place, adding a new key/value pair if it wasn't already present."
  `(setf ,alist (alist-set ,key ,alist ,value)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq alist '((a . 1) (b . 2) (c . 3) (d (e . 4) (f . 5))))
;; (alist-set! 'b alist 20)
;; (alist-get 'd alist)
;; (alist-set! 'e (alist-get 'd alist) 40)
;; alist  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-remove (key alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively remove a KEY from ALIST to VALUE by building a new alist in
which KEY is not present, doing nothing if it was not already present."
  (unless (proper-list-p alist)
    (error "ALIST must be a proper list, not %S!" alist))
  (let (result)
    (dolist (pair alist)
      (unless (eq (car pair) key)
        (push pair result)))
    (nreverse result)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq alist '((a . 1) (b . 2) (c . 3)))
;; (alist-remove 'b alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alist-remove! (key alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively remove a KEY from ALIST to VALUE by modifying the alist in
place."
  `(setf ,alist (alist-remove ,key ,alist)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq alist '((a . 1) (b . 2) (c . 3) (d (e . 4) (f . 5))))
;; (alist-remove! 'b alist)
;; (alist-get 'd alist)
;; (alist-remove! 'e (alist-get 'd alist))
;; (alist-remove! 'f (alist-get 'd alist))
;; alist  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alist-remove-empty (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively remove all empty values from ALIST by building a new alist
in which they are not present."
  `(cl-remove-if (lambda (pair) (null (cdr pair))) ,alist))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq alist '((a 1) (b 2) (c) (d (e 4) (f 5))))
;; (alist-remove-empty alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alist-remove-empty! (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively remove all empty values from ALIST by modifying it in place."
  `(setf ,alist (alist-remove-empty ,alist))
  ;; `(setf ,alist (cl-remove-if (lambda (pair) (null (cdr pair))) ,alist))
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq alist '((a 1) (b 2) (c 3) (d (e 4) (f 5))))
;; (alist-remove! 'e (alist-get 'd alist))
;; (alist-remove! 'f (alist-get 'd alist))
;; (alist-remove-empty! alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-dots-to-alist (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; (setq alist '((a 1) (b 2) (c 3)))
;; (add-dots-to-alist alist)
;; (setq alist '((a . 1) (b . 2) (c . 3)))
;; (add-dots-to-alist alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-dots-from-alist (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; (setq alist '((a 1) (b 2) (c 3)))
;; (remove-dots-from-alist alist)
;; (setq alist '((a . 1) (b . 2) (c . 3)))
;; (remove-dots-from-alist alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun merge-duplicate-alist-keys (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; (setq alist '((v . 1) (w . 2) (w . 3) (x . 4) (y . 5) (y . 6) (z . 7)))
;; (merge-duplicate-alist-keys alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten-alist-values (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Flatten the values of ALIST if they are lists - in other words, unmerge keys
whose values are lists and turn them into duplicated keys.

Examples:
(`flatten-alist-values'
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
;; (setq alist '((a 1) (b 2 2) (c 3) (d 4 5 6)))
;; (flatten-alist-values alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-to-plist (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Convert a dotted alist ALIST to a plist.

Note that the result is structured slightly differently depending on whether
ALIST used dotted lists or not:

(setq alist '((a 1) (b 2 2) (c 3) (d 4 5 6)))
(`alist-to-plist' alist) ⇒ (a (1) b (2 2) c (3) d (4 5 6))
(setq alist '((a . 1) (b 2 2) (c . 3) (d 4 5 6)))
(`alist-to-plist' alist) ⇒ (a 1 b (2 2) c 3 d (4 5 6))

`add-dots-to-alist'/`remove-dots-from-alist' may be applied beforehand in order
to achieve your preferred structure."
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
;; (setq alist '((a 1) (b 2 2) (c 3) (d 4 5 6)))
;; (alist-to-plist alist) ⇒ (a (1) b (2 2) c (3) d (4 5 6))
;; (setq alist '((a . 1) (b 2 2) (c . 3) (d 4 5 6)))
;; (alist-to-plist alist) ⇒ (a 1 b (2 2) c 3 d (4 5 6))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--alists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
