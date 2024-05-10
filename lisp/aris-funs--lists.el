;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General purpose list-manipulation functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dash)
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
(require 'aris-funs--ignorebang)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assert-list! (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Signal an error unless LST is a list."  
  (if (symbolp lst)
    (let ((name (upcase (symbol-name lst))))
      `(unless (listp ,lst) (error "%s is not a list: %s" ,name ,lst)))
    `(unless (listp ,lst)   (error "Not a list: %s" ,lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\(assert-list\\!\\_>\\)" . font-lock-warning-face)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doconses (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Loop over a list's heads and conses. Evaluate BODY with
VAR bound to each cons from LIST and POSITION bound
to each cons cell, in turn. Then evaluate RESULT to get
return value, default nil.

This is a a simple modification of `dolist'.

 (VAR POS LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 3 (length spec) 4)
    (signal 'wrong-number-of-arguments (list '(3 . 4) (length spec))))
  (let ( (var      (car spec))
         (position (cadr spec))
         (lst      (caddr spec)))
    `(let ((,position ,lst))
       (assert-list! ,position)
       (while ,position
         (let ((,var (car ,position)))
           ,@body
           (setq ,position (cdr ,position))))
       ,@(cdr (cdr (cdr spec))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that 
  (let ((lst '(a 2 b 4 c 6 d 8)))
    (doconses (head pos lst lst)
      (when (eq 'c head)
        (setcar pos (cons head (cadr pos)))
        (setcdr pos (cddr pos)))))
  returns (a 2 b 4 (c . 6) d 8))
(confirm that ; plist to alist
  (let ((lst '(a 2 b 4 c 6 d 8)))
    (doconses (head pos lst lst)
      (setcar pos (cons head (cadr pos)))
      (setcdr pos (cddr pos))))
  returns ((a . 2) (b . 4) (c . 6) (d . 8)))
(confirm that ; plist to let varlist
  (let ((lst '(a 2 b 4 c 6 d 8)))
    (doconses (head pos lst lst)
      (setcar pos (list head (cadr pos)))
      (setcdr pos (cddr pos))))
  returns ((a 2) (b 4) (c 6) (d 8)))
(confirm that ; alist to plist 
  (let ((lst '((a . 2) (b . 4) (c . 6) (d . 8))))
    (doconses (head pos lst lst)
      (let ((new (cons (cdr head) (cdr pos))))
        (setcar pos (car head))
        (setcdr pos new)
        (setf pos (cdr pos)))))
  returns (a 2 b 4 c 6 d 8))
(confirm that ; plist to let varlist
  (let ((lst '((a . 2) (b . 4) (c . 6) (d . 8))))
    (doconses (head pos lst lst)
      (let ((new (cons (cdr head) (cdr pos))))
        (setcar pos (list (car head) (cdr head))))))
  returns ((a 2) (b 4) (c 6) (d 8)))
(confirm that
  (let ((lst '(,x . ,y)))
    (doconses (head pos lst lst)
      (when (eq '\, head)
        (setcar pos (list head (cadr pos)))
        (setcdr pos (cddr pos)))))
  returns ((\, x) (\, y)))
(confirm that
  (let ((lst '(,x . ,y)))
    (doconses (head pos lst lst)
      (when (eq '\, head)
        (let ((new (list (list head (cadr pos)))))
          (setcar pos '\.)
          (setcdr pos new)
          (setf pos (cdr pos))))))
  returns ((\, x) \. (\, y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pick (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Pick a random element from LST."
  (assert-list! lst)
  (elt lst (random (length lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun weighted-pick (weighted-lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Select the cdr of an element from WEIGHTED-LST based on the weight in its car."
  (assert-list! weighted-lst)
  (let* ( (total-weight (apply #'+ (mapcar #'car weighted-lst)))
          (random-weight (random total-weight))
          (cumulative-weight 0))
    (catch 'return
      (while-let ((item (pop weighted-lst)))
        (setq cumulative-weight (+ cumulative-weight (car item)))
        (when (< random-weight cumulative-weight)
          (throw 'return (cdr item)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all (pred? lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when all elems in LST? are PRED?"
  (unless (fun? pred?) (error "PRED? must be a function"))
  (while (and lst (funcall pred? (car lst)))
    (pop lst))
  (nil? lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (all (lambda (x) (even? x)) '(2 4 6 8 10)) returns t)
(confirm that (all (lambda (x) (even? x)) '(2 4 6 7 8 10)) returns nil)
(confirm that (all 'even? '(2 4 6 8 10)) returns t)
(confirm that (all 'even? '(2 4 6 7 8 10)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun any (pred? lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when any elem in LST? is PRED?."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (let (result)
    (while (and lst (not result))
      (setq result (funcall pred? (pop lst))))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (any 'even? '(1 3 5 7 9 10)) returns t)
(confirm that (any 'even? '(1 3 5 7 9 11)) returns nil)
(confirm that (any (lambda (x) (even? x)) '(1 3 5 7 9 10)) returns t)
(confirm that (any (lambda (x) (even? x)) '(1 3 5 7 9 11)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-list (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Take a shallow copy of LST."
  (unless (list? lst) (error "LST must be a list"))
  (when lst
    (let* ( (result (list (pop lst)))
            (tail result))
      (while lst
        (let ((new-tail (list (pop lst))))
          (setcdr tail new-tail)
          (setq tail new-tail)))
      result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (copy-list '(1 2 3 4 5)) returns (1 2 3 4 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun depth (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the depth of a nested list structure."
  (unless (list? lst) (error "LST must be a list"))
  (let ( (stack (list (cons lst 1))) ; Stack with initial list and depth of 1
         (max-depth 0))
    (while stack
      (let* ( (current (pop stack))
              (current-list (car current))
              (current-depth (cdr current)))
        (if (> current-depth max-depth)
          (setq max-depth current-depth))
        (mapc (lambda (item)
                (when (list? item)
                  (push (cons item (1+ current-depth)) stack)))
          current-list)))
    max-depth))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (depth '(1 2 3 (4 5 (6 7 8) 9) 10)) returns 3)
(confirm that (depth nil) returns 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter (pred? lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a list containing those members of lst satisfying pred?."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (assert-list! lst)
  (let (result tail)
    (while lst
      (let ((head (pop lst)))
        (if (funcall pred? head)
          (let ((new-tail (list head)))
            (if tail
              (progn
                (setcdr tail new-tail)
                (setq   tail new-tail))
              (setq
                result  new-tail
                tail    result))))))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (filter 'even? '(1 2 3 4 5 6 7 8 9 10)) returns (2 4 6 8 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (lst)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
I wrote this one myself."
  (when lst
    (if (consp (car lst))
      (append (flatten (pop lst)) (flatten lst))
      (cons (pop lst) (flatten lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (flatten '(this (is a) (list (with lots) (of (nested stuff)))))
  returns (this is a list with lots of nested stuff))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten--norvig (input &optional accumulator)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
This is adapted from the version in Peter Norvig's book."
  ;; (prn "(flatten %s %s)" input accumulator)
  (let ((result
          ;; (with-indentation
          (cond
            ((null input) accumulator)
            ((atom input) (cons input accumulator))
            (t (flatten--norvig
                 (first input)
                 (flatten--norvig (rest input) accumulator)))))) ;)
    ;; (prn "⇒ %s" result)
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (flatten--norvig '(this (is a) (list (with lots) (of (nested stuff)))))
  returns (this is a list with lots of nested stuff))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun heads (lsts)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a list of the heads of the lists in LSTS."
  (unless (list? lsts)     (error "LSTS must be a list of lists"))
  (unless (all 'list? lsts) (error "LSTS must be a list of lists"))
  (let* ( (result (list (car (pop lsts))))
          (tail   result))
    (while lsts
      (let ((new-tail (list (car (pop lsts)))))
        (setq tail (setcdr tail new-tail))))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (heads '((1 2 3) (4 5 6) (7 8 9))) returns (1 4 7))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tails (lsts)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a list of the tails of the lists in LSTS."
  (unless (list? lsts)     (error "LSTS must be a list of lists"))
  (unless (all 'list? lsts) (error "LSTS must be a list of lists"))
  (let* ((result (list (cdr (pop lsts))))
          (tail   result))
    (while lsts
      (let ((new-tail (list (cdr (pop lsts)))))
        (setq tail (setcdr tail new-tail))))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (tails '((1 2 3) (4 5 6) (7 8 9))) returns ((2 3) (5 6) (8 9)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intercalate (intercalated lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Intercalate INTERCALATED between items in LST."
  (unless (list? lst) (error "LST must be a list"))
  (when lst
    (let* ( (result (list (car lst)))
            (tail   result))
      (setq lst (cdr lst))
      (while lst
        (let* ( (head     (pop lst))
                (new-tail (list intercalated head)))
          (setcdr tail new-tail)
          (setq tail (cdr new-tail))))
      result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (intercalate 'x '(1 2 3 4 5)) returns (1 x 2 x 3 x 4 x 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-list (pred? lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructivly split LST into two sublists:
1. The longest initial sublist of elements satisfying PRED?
2. The rest of the elements."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (when lst
    (let ( prev
           (current lst))
      (while (and current (funcall pred? (car current)))
        (setq
          prev    current
          current (cdr current)))
      (if prev
        (progn
          (setcdr prev nil)
          (list lst current))
        (list nil lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (split-list 'even? '(2 4 6 7 8 10)) returns ((2 4 6) (7 8 10)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caaaaar (lst) (car (caaaar lst)))
(defun caaaadr (lst) (car (caaadr lst)))
(defun caaadar (lst) (car (caadar lst)))
(defun caaaddr (lst) (car (caaddr lst)))
(defun caadaar (lst) (car (cadaar lst)))
(defun caadadr (lst) (car (cadadr lst)))
(defun caaddar (lst) (car (caddar lst)))
(defun caadddr (lst) (car (cadddr lst)))
(defun cadaaar (lst) (car (cdaaar lst)))
(defun cadaadr (lst) (car (cdaadr lst)))
(defun cadadar (lst) (car (cdadar lst)))
(defun cadaddr (lst) (car (cdaddr lst)))
(defun caddaar (lst) (car (cddaar lst)))
(defun caddadr (lst) (car (cddadr lst)))
(defun cadddar (lst) (car (cddddr lst)))
(defun caddddr (lst) (car (cddddr lst)))
(defun cdaaaar (lst) (cdr (caaaar lst)))
(defun cdaaadr (lst) (cdr (caaadr lst)))
(defun cdaadar (lst) (cdr (caadar lst)))
(defun cdaaddr (lst) (cdr (caaddr lst)))
(defun cdadaar (lst) (cdr (cadaar lst)))
(defun cdadadr (lst) (cdr (cadadr lst)))
(defun cdaddar (lst) (cdr (caddar lst)))
(defun cdadddr (lst) (cdr (cadddr lst)))
(defun cddaaar (lst) (cdr (cdaaar lst)))
(defun cddaadr (lst) (cdr (cdaadr lst)))
(defun cddadar (lst) (cdr (cdadar lst)))
(defun cddaddr (lst) (cdr (cdaddr lst)))
(defun cdddaar (lst) (cdr (cddaar lst)))
(defun cdddadr (lst) (cdr (cddadr lst)))
(defun cddddar (lst) (cdr (cdddar lst)))
(defun cdddddr (lst) (cdr (cddddr lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (car-safe Car)))
(defun cadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (car-safe Cdr)))
(defun cdar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdr-safe Car)))
(defun cddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdr-safe Cdr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caar-safe Car)))
(defun caadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caar-safe Cdr)))
(defun cadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cadr-safe Car)))
(defun caddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cadr-safe Cdr)))
(defun cdaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdar-safe Car)))
(defun cdadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdar-safe Cdr)))
(defun cddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cddr-safe Car)))
(defun cdddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cddr-safe Cdr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caaaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caaar-safe Car)))
(defun caaadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caaar-safe Cdr)))
(defun caadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caadr-safe Car)))
(defun caaddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caadr-safe Cdr)))
(defun cadaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cadar-safe Car)))
(defun cadadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cadar-safe Cdr)))
(defun caddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caddr-safe Car)))
(defun cadddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caddr-safe Cdr)))
(defun cdaaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdaar-safe Car)))
(defun cdaadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdaar-safe Cdr)))
(defun cdadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdadr-safe Car)))
(defun cdaddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdadr-safe Cdr)))
(defun cddaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cddar-safe Car)))
(defun cddadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cddar-safe Cdr)))
(defun cdddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdddr-safe Car)))
(defun cddddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdddr-safe Cdr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caaaaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caaaar-safe Car)))
(defun caaaadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caaaar-safe Cdr)))
(defun caaadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caaadr-safe Car)))
(defun caaaddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caaadr-safe Cdr)))
(defun caadaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caadar-safe Car)))
(defun caadadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caadar-safe Cdr)))
(defun caaddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caaddr-safe Car)))
(defun caadddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caaddr-safe Cdr)))
(defun cadaaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cadaar-safe Car)))
(defun cadaadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cadaar-safe Cdr)))
(defun cadadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cadadr-safe Car)))
(defun cadaddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cadadr-safe Cdr)))
(defun caddaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caddar-safe Car)))
(defun caddadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caddar-safe Cdr)))
(defun cadddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cadddr-safe Car)))
(defun caddddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cadddr-safe Cdr)))
(defun cdaaaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdaaar-safe Car)))
(defun cdaaadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdaaar-safe Cdr)))
(defun cdaadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdaadr-safe Car)))
(defun cdaaddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdaadr-safe Cdr)))
(defun cdadaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdadar-safe Car)))
(defun cdadadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdadar-safe Cdr)))
(defun cdaddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdaddr-safe Car)))
(defun cdadddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdaddr-safe Cdr)))
(defun cddaaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cddaar-safe Car)))
(defun cddaadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cddaar-safe Cdr)))
(defun cddadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cddadr-safe Car)))
(defun cddaddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cddadr-safe Cdr)))
(defun cdddaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdddar-safe Car)))
(defun cdddadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdddar-safe Cdr)))
(defun cddddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cddddr-safe Car)))
(defun cdddddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cddddr-safe Cdr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (retrieving by position):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun first    (lst)                    (car lst))
(defun second   (lst)                   (cadr lst))
(defun third    (lst)                  (caddr lst))
(defun fourth   (lst)                 (cadddr lst))
(defun fifth    (lst)                (caddddr lst))
(defun sixth    (lst)           (car (cdddddr lst)))
(defun seventh  (lst)          (cadr (cdddddr lst)))
(defun eighth   (lst)         (caddr (cdddddr lst)))
(defun ninth    (lst)        (cadddr (cdddddr lst)))
(defun tenth    (lst)       (caddddr (cdddddr lst)))
(defun eleventh (lst)  (car (cdddddr (cdddddr lst))))
(defun twelfth  (lst) (cadr (cdddddr (cdddddr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (first    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 1)
(confirm that (second   '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 2)
(confirm that (third    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 3)
(confirm that (fourth   '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 4)
(confirm that (fifth    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 5)
(confirm that (sixth    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 6)
(confirm that (seventh  '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 7)
(confirm that (eighth   '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 8)
(confirm that (ninth    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 9)
(confirm that (tenth    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 10)
(confirm that (eleventh '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 11)
(confirm that (twelfth  '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 12)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun first-safe    (lst)                               (car-safe lst))
(defun second-safe   (lst)                              (cadr-safe lst))
(defun third-safe    (lst)                             (caddr-safe lst))
(defun fourth-safe   (lst)                            (cadddr-safe lst))
(defun fifth-safe    (lst)                           (caddddr-safe lst))
(defun sixth-safe    (lst)                 (car-safe (cdddddr-safe lst)))
(defun seventh-safe  (lst)                (cadr-safe (cdddddr-safe lst)))
(defun eighth-safe   (lst)               (caddr-safe (cdddddr-safe lst)))
(defun ninth-safe    (lst)              (cadddr-safe (cdddddr-safe lst)))
(defun tenth-safe    (lst)             (caddddr-safe (cdddddr-safe lst)))
(defun eleventh-safe (lst)   (car-safe (cdddddr-safe (cdddddr-safe lst))))
(defun twelfth-safe  (lst)  (cadr-safe (cdddddr-safe (cdddddr-safe lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (first-safe    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 1)
(confirm that (second-safe   '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 2)
(confirm that (third-safe    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 3)
(confirm that (fourth-safe   '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 4)
(confirm that (fifth-safe    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 5)
(confirm that (sixth-safe    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 6)
(confirm that (seventh-safe  '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 7)
(confirm that (eighth-safe   '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 8)
(confirm that (ninth-safe    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 9)
(confirm that (tenth-safe    '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 10)
(confirm that (eleventh-safe '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 11)
(confirm that (twelfth-safe  '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 12)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun nthcdr (index lst)
;;   "Get the INDEXth cdr of LST."
;;   (unless (and (integer? index) (positive? index)) (error "N must be a positive integer"))
;;   (unless (list? lst)                              (error "LST must be a list"))
;;   (unless (>= index 0)                              (error "INDEX must be non-negative"))
;;   (until (zero? index)
;;     (setq lst (cdr lst))
;;     (decr index))
;;   lst)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that (nthcdr 2 '(1 2 3 4 5)) returns (3 4 5))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rmapc (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over LST, discarding the result and returning LST."
  (mapc fn lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (rmapc (list 1 2 3) (lambda (x) (* 2 x))) returns (1 2 3))
(confirm that (rmapc '(1 2 3) (lambda (x) (* 2 x))) returns (1 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rmapcar (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over LST."
  (mapcar fn lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (rmapcar (list 1 2 3) #'1+) returns (2 3 4))
(confirm that (rmapcar     '(1 2 3) #'1+) returns (2 3 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rmapl (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over the conses in LST."
  (cl-mapl fn lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (rmapl (list 1 2 3) (lambda (c) (setcar c (1+ (car c))))) returns (2 3 4))
(confirm that (rmapl     '(1 2 3) (lambda (c) (setcar c (1+ (car c))))) returns (2 3 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rmaplist (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over the conses in LST."
  (cl-maplist fn lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (rmaplist (list 1 2 3 4 5) (lambda (c) (cons (car c) (reverse (cdr c)))))
  returns ((1 5 4 3 2) (2 5 4 3) (3 5 4) (4 5) (5)))
(confirm that (rmaplist '(1 2 3 4 5) (lambda (c) (cons (car c) (reverse (cdr c)))))
  returns ((1 5 4 3 2) (2 5 4 3) (3 5 4) (4 5) (5)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rmapcan (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over the conses in LST."
  (mapcan fn lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (rmapcan (list 1 2 3 4 5) (lambda (n) (list n n))) returns  (1 1 2 2 3 3 4 4 5 5))
(confirm that (rmapcan '(1 2 3 4 5) (lambda (n) (list n n))) returns  (1 1 2 2 3 3 4 4 5 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rmapcon (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over the conses in LST."
  (cl-mapcon fn lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (rmapcon (list 1 2 3 4 5) (lambda (l) (cl-copy-list l))) returns
  (1 2 3 4 5 2 3 4 5 3 4 5 4 5 5))
(confirm that (rmapcon '(1 2 3 4 5) (lambda (l) (cl-copy-list l))) returns
  (1 2 3 4 5 2 3 4 5 3 4 5 4 5 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count>= (count needle lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return t when COUNT or more instances of NEEDLE are found in LST."
  (when (< count 0) (error "COUNT must be a non-negative integer."))
  (if (and (= count 0) lst)
    t
    (let ((lst (member needle lst)))
      (when (and (> count 0) lst)
        (count>= (1- count) needle (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (count>= 1 'f '(a b c d e f g h i j k l m n o p q r s t u f v w x y z))
  returns t)
(confirm that
  (count>= 2 'f '(a b c d e f g h i j k l m n o p q r s t u f v w x y z))
  returns t)
(confirm that
  (count>= 3 'f '(a b c d e f g h i j k l m n o p q r s t u f v w x y z))
  returns nil)
(confirm that
  (count>= 4 'f '(a b c d e f g h i j k l m n o p q r s t u f v w x y z))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-if-member (exclude lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Remove elements of EXCLUDE from LST."
  (cl-remove-if (lambda (x) (member x exclude)) lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (remove-if-member '(&optional &rest) '(foo bar &optional baz &rest quux))
  returns (foo bar baz quux))
(confirm that
  (remove-if-member '(&optional &rest) '(foo bar baz quux))
  returns (foo bar baz quux))
(confirm that
  (remove-if-member '(&optional &rest) '(&optional &rest))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sort-with-string< (lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively sort LST with `string<'."
  (sort (copy-sequence lst) #'string<))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (sort-with-string< '("a" "B" "A" "c" "b" "D" "e" "F"))
  returns ("A" "B" "D" "F" "a" "b" "c" "e"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old functions using dash. These are a bit stale, some of them should probably be
;; renamed, updated or discarded:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -rmapcar (l fun)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (-map fun l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -rmapc (l &rest funs)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (-map (eval (cons #'-compose funs)) l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -rmapcr (l &rest funs)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Reverse the order of funs, compose them and then map them over 𝒍."
  ((eval (cons #'-compose (nreverse funs))) l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dolist* (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A version of `dolist` that also handles improper lists."
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 2 (length spec) 3)
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  (let ( (tail (gensym "tail-"))
         (lst  (car (cdr spec)))
         (elem (car spec)))
    `(let* ((,tail ,lst))
       (assert-list! ,tail)
       (while ,tail
         (let* ( (consp (consp ,tail))
                 (,elem (if consp (car ,tail) ,tail)))
           ,@body
           (setq ,tail (if consp (cdr ,tail) nil))))
       ,@(cdr (cdr spec)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ((res 0))
    (dolist* (n '(1 2 3 4 5 6 7 8 9 . 10) res)
      (incf res n)))
  returns 55)
(confirm that
  (let ((res 0))
    (dolist* (n '(1 2 3 4 5 6 7 8 9 10) res)
      (incf res n)))
  returns 55)
(confirm that
  (let ((res 0))
    (dolist* (n nil res)
      (incf res n)))
  returns 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun length* (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the length of a (possibly improper) list."
  (assert-list! lst)
  (if (proper-list-p lst)
    (length lst)
    (let ((count 0)) 
      (dolist* (n lst count) (cl-incf count)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (length* '(1 2 . 3)) returns 3)
(confirm that (length* '(1 2 3)) returns 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun properize (lst &optional rec improper-indicator)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively make a proper list from an improper list, recursively if REC,
optionally inserting an IMPROPER-INDICATOR before the last element of
lists that were originally improper."
  (cond
    ((atom lst) lst)
    ((and (cdr lst) (atom (cdr lst)))
      ;; found the improper tail.
      (cons
        (if rec (properize (car lst) rec improper-indicator) (car lst))
        (append (when improper-indicator (list improper-indicator))
          (list (cdr lst)))))
    (t (cons
         (if rec (properize (car lst) rec improper-indicator) (car lst))
         (properize (cdr lst) rec improper-indicator)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (properize nil) returns nil)
(confirm that (properize '(1 2 3)) returns (1 2 3))
(confirm that (properize '(1 2 . 3)) returns (1 2 3))
(confirm that (properize '(1 2 . 3) t '\.) returns (1 2 \. 3))
(confirm that (properize '(1 (2 3 . 4) . 5)) returns (1 (2 3 . 4) 5))
(confirm that (properize '(1 (2 3 . 4) . 5) t) returns (1 (2 3 4) 5))
(confirm that (properize '(1 (2 3 . 4) . 5) t '\.) returns (1 (2 3 \. 4) \. 5))
(confirm that (properize '(1 (2 3 . 4) . 5) nil '\.) returns (1 (2 3 . 4) \. 5))
(confirm that (properize '((0 . 1) (2 3 . 4) . 5) t '\.)
  returns ((0 \. 1) (2 3 \. 4) \. 5))
(confirm that (properize '((0 . 1)  (2 3 . 4) . 5) nil '\.)
  returns ((0 . 1) (2 3 . 4) \. 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun properize! (lst &optional rec improper-indicator)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively make a proper list from an improper list, recursively if REC."
  (doconses (head pos lst lst)
    (when (and rec (consp head))
      (setcar pos (properize! head rec improper-indicator)))
    (when (and (cdr pos) (atom (cdr pos)))
      ;; found the improper tail.
      (setcdr pos
        (append
          (when improper-indicator (list improper-indicator))
          (list (cdr pos))))
      (setf pos (cdr pos)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (properize! nil) returns nil)
(confirm that (properize! '(1 2 3)) returns (1 2 3))
(confirm that (properize! '(1 2 . 3)) returns (1 2 3))
(confirm that (properize! '(1 2 . 3) t '\.) returns (1 2 \. 3))
(confirm that (properize! '(1 (2 3 . 4) . 5)) returns (1 (2 3 . 4) 5))
(confirm that (properize! '(1 (2 3 . 4) . 5) t) returns (1 (2 3 4) 5))
(confirm that (properize! '(1 (2 3 . 4) . 5) t '\.) returns (1 (2 3 \. 4) \. 5))
(confirm that (properize! '(1 (2 3 . 4) . 5) nil '\.) returns (1 (2 3 . 4) \. 5))
(confirm that (properize! '((0 . 1) (2 3 . 4) . 5) t '\.)
  returns ((0 \. 1) (2 3 \. 4) \. 5))
(confirm that (properize! '((0 . 1)  (2 3 . 4) . 5) nil '\.)
  returns ((0 . 1) (2 3 . 4) \. 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun old-compact (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Filter nil items from LST."
  (assert-list! lst)
  (while (and lst (nil? (car lst)))
    (pop lst))
  (when lst
    (let* ( (res  (list (pop lst)))
            (tail res))
      (while lst
        (let ((head (pop lst)))
          (unless (nil? head)
            (setq tail (setcdr tail (list head))))))
      res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (old-compact '(1 nil 2 nil 3 nil 4 nil 5 nil)) returns (1 2 3 4 5))
(confirm that (old-compact '(nil)) returns nil)
(confirm that (old-compact nil) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun compact (lst)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Filter nil items from LST."
;;   (assert-list! lst)
;;   (nreverse
;;     (let (res)
;;       (dolist (head lst res)
;;         (unless (null head)
;;           (push head res))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that (compact '(1 nil 2 nil 3 nil 4 nil 5 nil)) returns (1 2 3 4 5))
;; (confirm that (compact '(nil)) returns nil)
;; (confirm that (compact nil) returns nil)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compact (lst &optional rec)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Filter nil items from LST, recursively when REC."
  (assert-list! lst)
  (nreverse
    (let (res)
      (dolist (head lst res)
        (cond
          ((null head)) ; do nothing
          ((and rec (consp head))
            (when-let ((compacted-head (compact head rec)))
              (push compacted-head res)))
          (t (push head res)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (compact '(1 nil 2 nil (3 nil (4 (nil) 5 nil))) t) returns (1 2 (3 (4 5))))
(confirm that (compact '(1 nil 2 nil (3 nil (4 (nil) 5 nil))))
  returns (1 2 (3 nil (4 (nil) 5 nil))))
(confirm that (compact '(1 nil 2 nil 3 nil 4 nil 5 nil)) returns (1 2 3 4 5))
(confirm that (compact '(nil)) returns nil)
(confirm that (compact nil) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unrepeat (lst &optional rec)
  "Remove sequentially repeated items from LST, recursively if REC."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let (res (last (gensym)))
    (while lst
      (let ((popped (pop lst)))
        (when (and rec (consp popped))
          (setq popped (unrepeat popped rec)))
        (when (not (equal popped last))
          (push popped res)
          (setq last popped))))
    (reverse res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (unrepeat '(a (b b c) (b c c)) t) returns (a (b c)))
(confirm that (unrepeat '(a a b a b b c a a (b b c) (b b c) nil nil (b b (c d d e))) t)
  returns (a b a b c a (b c) nil (b (c d e))))
(confirm that (unrepeat '(a a b a b b c a a (b b c) (b b c) nil nil (b b (c d d e))))
  returns (a b a b c a (b b c) nil (b b (c d d e))))
(confirm that (unrepeat '(nil a nil)) returns (nil a nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore! ;; this version is a bit slower than `unrepeat':
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun unrepeat2 (lst &optional rec)
    "Remove sequentially repeated items from LST, recursively if REC."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (cdr
      (nreverse
        (let ((res (list (gensym))))
          (doconses (head pos lst res)
            (when (and rec (listp head))
              (setq head (unrepeat2 head rec)))
            (when (not (equal head (car res)))
              (push head res)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (confirm that (unrepeat2 '(a (b b c) (b c c)) t) returns (a (b c)))
  (confirm that (unrepeat2 '(a a b a b b c a a (b b c) (b b c) nil nil (b b (c d d e))) t)
    returns (a b a b c a (b c) nil (b (c d e))))
  (confirm that (unrepeat2 '(a a b a b b c a a (b b c) (b b c) nil nil (b b (c d d e))))
    returns (a b a b c a (b b c) nil (b b (c d d e))))
  (confirm that (unrepeat2 '(nil a nil)) returns (nil a nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
