;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alist processing functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dash)
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
(require 'aris-funs--unsorted)
(require 'aris-funs--expr-throws-sym-p)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'make-alist 'cl-pairlis)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (make-alist '(a b c) '(1 2 3)) returns ((a . 1) (b . 2) (c . 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-has? (key alist)
  "t when ALIST contains KEY."
  (let ((entry (assoc key alist)))
    (not (null entry))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((my-alist '((a . t) (b . nil))))
  (confirm that (alist-has? 'a my-alist) returns t)
  (confirm that (alist-has? 'b my-alist) returns t)
  (confirm that (alist-has? 'c my-alist) returns nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-lacks? (key alist)
  "t unless ALIST contains KEY."
  (not (alist-has? key alist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((my-alist '((a . t) (b . nil))))
  (confirm that (alist-lacks? 'a my-alist) returns nil)
  (confirm that (alist-lacks? 'b my-alist) returns nil)
  (confirm that (alist-lacks? 'c my-alist) returns t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-keys (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Extract all keys from ALIST."
  (mapcar #'car alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (alist-keys '((a . 1) (b . 2) (c . 3))) returns (a b c))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-values (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Extract all values from ALIST."
  (mapcar #'cdr alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (alist-values '((a . 1) (b . 2) (c . 3))) returns (1 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-put (key alist value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively set a KEY in ALIST to VALUE by building a new alist in
which KEY is set to VALUE, adding a new key/value pair if it wasn't already present.

This is meant for non-dotted ALISTs and might produce unexpexted results if
applied to dotted ALISTs. As usual, `add-dots-to-alist'/`remove-dots-from-alist'
may be applied before or after to get your desired result."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a . 1) (b . 2) (c . 3))))
  (confirm that (alist-put 'b alist 4) returns ((a . 1) (b . 4) (c . 3)))
  (confirm that (alist-put 'd alist 5) returns ((a . 1) (b . 2) (c . 3) (d . 5))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alist-put! (key alist value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively set a KEY in ALIST to VALUE by modifying the alist in place, adding a new key/value pair if it wasn't already present."
  `(progn
     (let ((entry (assoc ,key ,alist)))
       (if entry
         (setcdr entry ,value)
         (setf ,alist (cons (cons ,key ,value) ,alist))))
     ,alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq alist '((a . 1) (b . 2) (c . 3) (d (e . 4) (f . 5))))
(confirm that (alist-put! 'b alist 20)
  returns ((a . 1) (b . 20) (c . 3) (d (e . 4) (f . 5))))
(confirm that (alist-get 'd alist) returns ((e . 4) (f . 5)))
(confirm that (alist-put! 'e (alist-get 'd alist) 40) returns ((e . 40) (f . 5)))
(confirm that alist returns ((a . 1) (b . 20) (c . 3) (d (e . 40) (f . 5))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-remove (key alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively remove a KEY from ALIST to VALUE by building a new alist in
which KEY is not present, doing nothing if it was not already present."
  (unless (proper-list-p alist)
    (error "ALIST must be a proper list, not %S!" alist))
  (let (result)
    (dolist (pair alist)
      (unless (eq (car pair) key)
        (push pair result)))
    (nreverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a . 1) (b . 2) (c . 3))))
  (confirm that (alist-remove 'b alist) returns ((a . 1) (c . 3))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alist-remove! (key alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively remove a KEY from ALIST to VALUE by modifying the alist in
place."
  `(setf ,alist (alist-remove ,key ,alist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a . 1) (b . 2) (c . 3) (d (e . 4) (f . 5)))))
  (confirm that (alist-remove! 'b alist)
    returns ((a . 1) (c . 3) (d (e . 4) (f . 5))))
  (confirm that (alist-get 'd alist) returns ((e . 4) (f . 5)))
  (confirm that (alist-remove! 'e (alist-get 'd alist)) returns ((f . 5)))
  (confirm that (alist-remove! 'f (alist-get 'd alist)) returns nil)
  (confirm that alist returns ((a . 1) (c . 3) (d))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alist-remove-empty (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively remove all empty values from ALIST by building a new alist
in which they are not present."
  `(cl-remove-if (lambda (pair) (null (cdr pair))) ,alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a 1) (b 2) (c) (d (e 4) (f 5)))))
  (confirm that (alist-remove-empty alist) returns ((a 1) (b 2) (d (e 4) (f 5)))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alist-remove-empty! (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively remove all empty values from ALIST by modifying it in place."
  `(setf ,alist (alist-remove-empty ,alist))
  ;; `(setf ,alist (cl-remove-if (lambda (pair) (null (cdr pair))) ,alist))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a 1) (b 2) (c 3) (d (e 4) (f 5)))))
  (confirm that (alist-remove! 'e (alist-get 'd alist)) returns ((f 5)))
  (confirm that (alist-remove! 'f (alist-get 'd alist)) returns nil)
  (confirm that (alist-remove-empty! alist) returns ((a 1) (b 2) (c 3))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-dots-to-alist (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (unless (proper-list-p alist)
    (error "Not a proper list!"))
  (rmapcar
    alist
    (lambda (pair)
      (cond
        ((-cons-pair? pair) pair)
        ((> (length (cdr pair)) 1) pair)
        (t (cons (car pair) (cadr pair)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a 1) (b 2) (c 3))))
  (confirm that (add-dots-to-alist alist) returns ((a . 1) (b . 2) (c . 3)))
  (confirm that (setq alist '((a . 1) (b . 2) (c . 3))) returns ((a . 1) (b . 2) (c . 3)))
  (confirm that (add-dots-to-alist alist) returns ((a . 1) (b . 2) (c . 3))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-dots-from-alist (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (unless (proper-list-p alist)
    (error "Not a proper list!"))
  (rmapcar
    alist
    (lambda (pair)
      ;; (message "pair %s"pair)
      (cond
        ((-cons-pair? pair)
          (cons (car pair) (list (cdr pair))))
        ((atom pair) (error "Improper alist."))
        (t pair)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a 1) (b 2) (c 3))))
  (confirm that (remove-dots-from-alist alist) returns ((a 1) (b 2) (c 3)))
  (confirm that (setq alist '((a . 1) (b . 2) (c . 3))) returns ((a . 1) (b . 2) (c . 3)))
  (confirm that (remove-dots-from-alist alist) returns ((a 1) (b 2) (c 3))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun merge-duplicate-alist-keys (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((v . 1) (w . 2) (w . 3) (x . 4) (y . 5) (y . 6) (z . 7))))
  (confirm that (merge-duplicate-alist-keys alist)
    returns ((v 1) (w 2 3) (x 4) (y 5 6) (z 7))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten-alist-values (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a 1) (b 2 2) (c 3) (d 4 5 6))))
  (confirm that (flatten-alist-values alist)
    returns ((a . 1) (b . 2) (b . 2) (c . 3) (d . 4) (d . 5) (d . 6))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-to-plist (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Convert a dotted alist ALIST to a plist.

Note that the result is structured slightly differently depending on whether
ALIST used dotted lists or not:

(setq alist '((a 1) (b 2 2) (c 3) (d 4 5 6)))
(`alist-to-plist' alist) ⇒ (a (1) b (2 2) c (3) d (4 5 6))
(setq alist '((a . 1) (b 2 2) (c . 3) (d 4 5 6)))
(`alist-to-plist' alist) ⇒ (a 1 b (2 2) c 3 d (4 5 6))

`add-dots-to-alist'/`remove-dots-from-alist' may be applied beforehand in order
to achieve your preferred structure."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (unless (list? alist) (error "ALIST must be a list"))
  (let (result tail)
    (while alist
      (let* ( (pair  (pop alist))
              (key   (car pair))
              (value (cdr pair)))
        (let ((new-tail (list key value)))
          (if tail
            (setcdr tail new-tail)
            (setq result new-tail))
          (setq tail (cdr new-tail))
          )))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((alist '((a 1) (b 2 2) (c 3) (d 4 5 6))))
  (confirm that (alist-to-plist alist) returns (a (1) b (2 2) c (3) d (4 5 6))))
(let ((alist '((a . 1) (b 2 2) (c . 3) (d 4 5 6))))
  (confirm that (alist-to-plist alist) returns (a 1 b (2 2) c 3 d (4 5 6))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sort-symbol-keyed-alist (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Sort ALIST, all of whose keys must be symbols, alphabetically by its keys."
  (cl-sort (copy-sequence alist)
    (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (sort-symbol-keyed-alist '((c . 3) (b . 2) (a . 1)))
  returns ((a . 1) (b . 2) (c . 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-alist (keys &optional values)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Create an alist with KEYS as keys and VALUES, if supplied, as values."
  (if values
    (cl-pairlis keys values)
    (mapcar #'list keys)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (make-alist '(a b c)) returns ((a) (b) (c)))
(confirm that (make-alist '(a b c) '(1 2 3)) returns ((a . 1) (b . 2) (c . 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro make-empty-alist (&rest keys)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Create an alist with KEYS as keys and nil for initial values."
;;   `(make-alist ',keys))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (benchmark-run 1000000
;;   (progn
;;     (confirm that (make-empty-alist a b c)
;;       returns ((a) (b) (c)))
;;     (confirm that (make-empty-alist a b c (d e))
;;       returns ((a) (b) (c) ((d e))))))
;; ;; (2.45471 35 1.7879000000000076)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro merge-alists (&rest alists)
  "Merge ALISTS into a single alist."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((res (gensym)))
    `(let ((,res ()))
       (dolist (alist (list ,@alists) ,res)
         (dolist (pair alist)
           (if (assoc (car pair) ,res)
             (alist-put! (car pair) ,res (cdr pair))
             (push pair ,res))))
       ,res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (merge-alists '((a . 1) (b. 2) (c . 3)) '((c . 33) (d . 4)) '((d . 44) (e . 5)))
  returns ((e . 5) (d . 44) (c . 33) (b. 2) (a . 1)))
(confirm that
  (merge-alists '((a . 1) (b. 2) (c . 3)) '((c . 33) (d . 4)) nil '((d . 44) (e . 5)))
  returns ((e . 5) (d . 44) (c . 33) (b. 2) (a . 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcdar (fun alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FUN over the cdr of each element of ALIST."
  (mapcar (lambda (kvp) (cons (car kvp) (funcall fun (cdr kvp)))) alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (mapcdar #'double '((a . 1) (b . 2) (c . 3)))
  returns ((a . 2) (b . 4) (c . 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rmapcdar (alist fun)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FUN over the cdr of each element of ALIST (with reversed
 parameter order)."
  (mapcdar fun alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (rmapcdar '((a . 1) (b . 2) (c . 3)) #'double)
  returns ((a . 2) (b . 4) (c . 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-empty-alist (&rest keys)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Create an alist with KEYS as keys and nil for initial values."
  `(mapcar #'list ',keys))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (make-empty-alist a b c)
  returns ((a) (b) (c)))
(confirm that (make-empty-alist a b c (d e))
  returns ((a) (b) (c) ((d e))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-empty-alist-2 (keys)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Create an alist with KEYS as keys and nil for initial values."
  (mapcar #'list keys))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (make-empty-alist-2 '(a b c d)) returns ((a) (b) (c) (d)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fill-in-missing-alist-keys (keys alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Fill in missing keys in ALIST with KVPs whose value is nil (without
maintaining ordering). Sketchy?"
  (dolist (key keys)
    (unless (assoc key alist)
      (setf alist (cons (list key) alist))))
  alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (sort-symbol-keyed-alist (fill-in-missing-alist-keys '(c d e) '((a . 1) (b . 2) (c . 3))))
  returns ((a . 1) (b . 2) (c . 3) (d) (e)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun alist-putunique(key new-val alist &optional (throw-sym 'unequal-duplicate))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Put NEW-VAL into ALIST as the association of KEY, throwing
THROW-SYM if an association for KEY is already present in ALIST with a
different (by `equal') value (or return nil, if THROW-SYM is nil)."
  (let ((assoc (assoc key alist)))
    (cond
      ((and assoc (equal (cdr assoc) new-val)) alist) ;; just return alist.
      (assoc (if throw-sym (throw throw-sym nil) nil))
      (t (cons (cons key new-val) alist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new key/val:
(confirm that
  (let ((alist '((a . 1) (b . 2)))) (alist-putunique 'c 3 alist))
  returns ((c . 3) (a . 1) (b . 2)))
(confirm that
  (expr-throws-sym-p 'unequal-duplicate
    '(let ((alist '((a . 1) (b . 2)))) (alist-putunique 'c 3 alist)))
  returns nil)
;; existing key, equal value:
(confirm that
  (let ((alist '((a . 1) (b . 2)))) (alist-putunique 'b 2 alist))
  returns ((a . 1) (b . 2)))
(confirm that
  (expr-throws-sym-p 'unequal-duplicate
    '(let ((alist '((a . 1) (b . 2)))) (alist-putunique 'b 2 alist)))
  returns nil)
;; duplicate key, un-equal value. this one SHOULD throw, so we don't `confirm' it's return
;; value, we just `confirm' if it threw:
(confirm that
  (expr-throws-sym-p 'unequal-duplicate
    '(let ((alist '((a . 1) (b . 2)))) (alist-putunique 'b 3 alist)))
  returns t)
;; since THROW-SYM is specified as nil, this one should not throw:
(confirm that
  (let ((alist '((a . 1) (b . 2)))) (alist-putunique 'b 3 alist nil))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(makunbound 'alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--alists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
