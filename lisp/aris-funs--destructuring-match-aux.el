;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions used by `dm:match', my destructuring pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that follow are not yet used! They are intended for the future handling of improper
;; lists as patterns/targets and are not quite finished (for example, they will need to handle
;; UNSPLICE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *dm::improper-indicator* '\.
  "The symbol used to indicate that a list has been properized."
  :group  'destructuring-match
  :type  'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:test-aux* t
  "Whether or not test of destructuring match's auxiliary functions  are enabled."
  :group 'destructuring-match
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; properize targets:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro possibly (fun thing &rest phrase)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((pred (car (last phrase)))
          (phrase (butlast phrase))
          (mode (cond
                  ((equal '(if it is) phrase) :WHEN)
                  ((equal '(if it isnt) phrase) :UNLESS)
                  ((equal '(if it isnt a) phrase) :UNLESS)
                  ((equal '(if it isnt an) phrase) :UNLESS)
                  ((equal '(if it is not) phrase) :UNLESS)
                  ((equal '(if it is not a) phrase) :UNLESS)
                  ((equal '(if it is not an) phrase) :UNLESS)
                  ((equal '(unless it is) phrase) :UNLESS)
                  ((equal '(unless it is a) phrase) :UNLESS)
                  ((equal '(unless it is an) phrase) :UNLESS)
                  (t (error "Unrecognized phrase %s" phrase))))
          (expr `(,pred ,thing))
          (expr (if (eq :UNLESS mode) `(not ,expr) expr))
          (sym (gensym)))
    `(let ((,sym ,thing))
       (if ,expr (,fun ,sym) ,sym))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (possibly double 7 if it is odd?) returns 14)
(confirm that (possibly double 7 if it isnt odd?) returns 7)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-target! (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (properize! lst t *dm::improper-indicator*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-target!  nil)       returns nil)
  (confirm that (dm::properize-target! '(2))       returns (2))
  (confirm that (dm::properize-target! '(2 . 4))   returns (2 \. 4))
  (confirm that (dm::properize-target! '(2 4   6)) returns (2 4 6))
  (confirm that (dm::properize-target! '(2 4 . 6)) returns (2 4 \. 6))
  (let ((lst '(2 4 . 6)))
    (confirm that (dm::properize-target! lst) returns (2 4 \. 6))
    (confirm that lst returns (2 4 \. 6))))
;; don't confirm: (dm::properize-target! '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-target (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (properize lst t *dm::improper-indicator*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-target  nil)       returns nil)
  (confirm that (dm::properize-target '(2))       returns (2))
  (confirm that (dm::properize-target '(2 . 4))   returns (2 \. 4))
  (confirm that (dm::properize-target '(2 4   6)) returns (2 4 6))
  (confirm that (dm::properize-target '(2 4 . 6)) returns (2 4 \. 6)))
;; don't confirm: (dm::properize-target '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; properize patterns:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern!* (lst &optional not-first) ; deeply recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (cond
    ((atom lst) lst)
    ((and (cdr lst) (atom (cdr lst)))
      ;; found an improper tail, properize it:
      (setcar lst (dm::properize-pattern!* (car lst) nil))
      (setcdr lst (list *dm::improper-indicator* (cdr lst)))
      lst)
    ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
      ;; found a wayward comma, fix it:
      (setcar lst *dm::improper-indicator*)
      (setcdr lst (list (list '\, (dm::properize-pattern!* (cadr lst) nil)))))
    ((consp (car lst))
      (setcar lst (dm::properize-pattern!* (car lst) nil))
      (dm::properize-pattern!* (cdr lst) t))
    (t ; (atom (car lst))
      (dm::properize-pattern!* (cdr lst) t)))
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-pattern!*  nil)          returns nil)
  (confirm that (dm::properize-pattern!* '(,x))         returns ((\, x)))
  (confirm that (dm::properize-pattern!* '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern!* '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern!* '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern!* '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern!* '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern!* '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern!* '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern!* '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern!* '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern!* '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern!* due to the way ,z is used in the innermost sub-expression,
  ;; but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure it doesn't try: 
  (confirm that (dm::properize-pattern!* '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?))))
  (let ((lst  '(,v (,w ,x . ,y) . ,z)))
    (confirm that (dm::properize-pattern!* lst)
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
    (confirm that lst
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z))))
  (let ((lst '(,x .  y)))
    (confirm that (dm::properize-pattern!* lst)        returns ((\, x) \. y))
    (confirm that lst                                  returns ((\, x) \. y))))
;; don't confirm: (dm::properize-pattern!* '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern* (lst &optional not-first) ; deeply recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (cond
    ((atom lst) lst)
    ((and (cdr lst) (atom (cdr lst)))
      ;; found an improper tail, properize it:
      (list (dm::properize-pattern* (car lst) nil) *dm::improper-indicator* (cdr lst)))
    ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
      ;; found a wayward comma, fix it:
      (list *dm::improper-indicator*
        (list '\, (dm::properize-pattern* (cadr lst) nil))))
    ((consp (car lst))
      (cons
        (dm::properize-pattern* (car lst) nil)
        (dm::properize-pattern* (cdr lst) t)))
    (t
      (cons
        (car lst)
        (dm::properize-pattern* (cdr lst) t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-pattern*  nil)          returns nil)
  (confirm that (dm::properize-pattern* '(,x))         returns ((\, x)))
  (confirm that (dm::properize-pattern* '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern* '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern* '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern* '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern* '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern* '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern* '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern* '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern* '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern* '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern due to the way ,z is used in the innermost sub-expression,
  ;; but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure it doesn't try: 
  (confirm that (dm::properize-pattern* '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?)))))
;; don't confirm: (dm::properize-pattern* '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern! (lst) ; shallowly recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (let (not-first)
    (doconses (head pos lst lst)
      (cond
        ((and (cdr pos) (atom (cdr pos)))
          ;; found an improper tail, properize it:
          (setcar pos (possibly dm::properize-pattern! head unless it is an atom))
          (setcdr pos (list *dm::improper-indicator* (cdr pos)))
          (setf pos nil))
        ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
          ;; found a wayward comma, fix it:
          (setcar pos *dm::improper-indicator*)
          (setcdr pos
            (list (list '\,
                    (possibly dm::properize-pattern! (cadr pos) unless it is an atom))))
          (setf pos nil))
        ((consp head) (setcar pos (dm::properize-pattern! head))))
      (setf not-first t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-pattern!  nil)          returns nil)
  (confirm that (dm::properize-pattern! '(,x))         returns ((\, x)))
  (confirm that (dm::properize-pattern! '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern! '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern! '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern! '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern! '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern! '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern! '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern! '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern! '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern! '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern! due to the way ,z is used in the innermost sub-expression,
  ;; but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure it doesn't try: 
  (confirm that (dm::properize-pattern! '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?))))
  (let ((lst  '(,v (,w ,x . ,y) . ,z)))
    (confirm that (dm::properize-pattern! lst)
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
    (confirm that lst
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z))))
  (let ((lst '(,x .  y)))
    (confirm that (dm::properize-pattern! lst)         returns ((\, x) \. y))
    (confirm that lst                                  returns ((\, x) \. y))))
;; don't confirm: (dm::properize-pattern! '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern (lst) ; shallowly recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  ;; (prndiv)
  (nreverse
    (let (not-first res)
      (doconses (head pos lst res)
        (cond
          ((and (cdr pos) (atom (cdr pos)))
            ;; found an improper tail, properize it.
            ;; this isn't sexy, but it turns out that nested `cons'es and a conjoined
            ;; `setf' is very slightly faster than the alternatives:
            (setq res
              (cons (cdr pos)
                (cons *dm::improper-indicator*
                  (cons (possibly dm::properize-pattern head unless it is an atom)
                    res)))
              pos nil))
          ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
            ;; found a wayward comma, fix it:
            (setq res (cons
                        (list '\, (possibly dm::properize-pattern (cadr pos) unless it is an atom))
                        (cons *dm::improper-indicator* res)))
            (setq pos nil))
          ((atom head) (push head res))
          (t (push (dm::properize-pattern head) res)))
        (setq not-first t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-pattern  nil)          returns nil)
  (confirm that (dm::properize-pattern '(,x))         returns ((\, x)))
  (confirm that (dm::properize-pattern '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern due to the way ,z is used in the innermost sub-expression,
  ;; but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure it doesn't try: 
  (confirm that (dm::properize-pattern '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?)))))
;; don't confirm: (dm::properize-pattern '(,x . ,y ,z)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern!*2 (lst &optional not-first) ; deeply recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (cond
    ((atom lst) ;; (and lst (atom lst))
      ;;(prn "case 1")
      ;;(debug lst)
      )
    ((and (cdr lst) (atom (cdr lst)))
      ;;(prn "case 2")
      ;; found an improper tail, properize it:
      (setcdr lst (list *dm::improper-indicator* (cdr lst))))
    ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
      ;;(prn "case 3")
      ;; found a wayward comma, fix it:
      (setcar lst *dm::improper-indicator*)
      (setcdr lst (list (list '\,
                          (dm::properize-pattern!*2 (cadr lst) nil)))))
    ((consp (car lst))
      ;;(prn "case 4")
      (setcar lst (dm::properize-pattern!*2 (car lst) nil))
      (dm::properize-pattern!*2 (cdr lst) t))
    ((cdr lst) ; (atom (car lst))
      ;;(prn "case 5: %s" (cdr lst))
      (dm::properize-pattern!*2 (cdr lst) t)))
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-pattern!*2 '(,x))         returns ((\, x)))
  ;; (confirm that (dm::properize-pattern!*2  nil)          returns nil)
  (confirm that (dm::properize-pattern!*2 '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern!*2 '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern!*2 '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern!*2 '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern!*2 '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern!*2 '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern!*2 '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern!*2 '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern!*2 '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern!*2 '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern!*2 due to the way ,z is used in the innermost sub-expression,
  ;; but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure it doesn't try: 
  (confirm that (dm::properize-pattern!*2 '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?))))
  (let ((lst  '(,v (,w ,x . ,y) . ,z)))
    (confirm that (dm::properize-pattern!*2 lst)
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
    (confirm that lst
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z))))
  (let ((lst '(,x .  y)))
    (confirm that (dm::properize-pattern!*2 lst)        returns ((\, x) \. y))
    (confirm that lst                                  returns ((\, x) \. y))
    ))
;; don't confirm: (dm::properize-pattern!*2 '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (setq reps 100000)

    (list
      (benchmark-run reps (dm::properize-pattern!*2 '(,v (,w ,x (a . (b c . d)) (,a b ,c (,d . e)) ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern!*  '(,v (,w ,x (a . (b c . d)) (,a b ,c (,d . e)) ,y) . ,z)))
      ;; (benchmark-run reps (dm::properize-pattern!   '(,v (,w ,x (a . (b c . d)) (,a b ,c (,d . e)) ,y) . ,z)))
      ;; (benchmark-run reps (dm::properize-pattern*   '(,v (,w ,x (a . (b c . d)) (,a b ,c (,d . e)) ,y) . ,z)))
      ;; (benchmark-run reps (dm::properize-pattern    '(,v (,w ,x (a . (b c . d)) (,a b ,c (,d . e)) ,y) . ,z)))
      )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )
  ((1.11989 0 0.0)
    (1.191337 0 0.0))
  ((1.117509 0 0.0)
    (1.1906510000000001 0 0.0))

  ((1.1211820000000001 0 0.0)
    (1.200364 0 0.0))
  ((1.124367 0 0.0)
    (1.19235 0 0.0))
  ((1.124477 0 0.0)
    (1.191989 0 0.0))

  ((1.119714 0 0.0)
    (1.190704 0 0.0))

  ((1.149295 0 0.0)
    (1.189412 0 0.0))

  ((1.131025 0 0.0)
    (1.1760709999999999 0 0.0)
    (1.256165 0 0.0)
    (1.572902 8 0.5307849999999945)
    (1.749189 7 0.45411700000001076))

  ((1.1555549999999999 0 0.0)
    (1.1782789999999999 0 0.0)
    (1.260386 0 0.0)
    (1.4768379999999999 7 0.44884700000000066)
    (1.808689 8 0.5166719999999998))

  ((1.117588 0 0.0)
    (1.190702 0 0.0)
    (1.2648709999999999 0 0.0)
    (1.4856530000000001 7 0.44911899999999605)
    (1.812742 8 0.5165060000000068))
  ((1.157605 0 0.0)
    (1.189629 0 0.0)
    (1.264866 0 0.0)
    (1.487328 7 0.4490239999999943)
    (1.810676 8 0.5164430000000095))

  ((1.152385 0 0.0)
    (1.196218 0 0.0)
    (1.262898 0 0.0)
    (1.503993 7 0.46370499999999026)
    (1.823433 8 0.5258280000000042))

  ((1.1111010000000001 0 0.0)
    (1.1921460000000002 0 0.0)
    (1.2676619999999998 0 0.0)
    (1.573158 8 0.5317229999999995)
    (1.761906 7 0.46225600000000355))

  ((1.085514 0 0.0)
    (1.194597 0 0.0)
    (1.2645099999999998 0 0.0)
    (1.495363 7 0.45707799999999565)
    (1.8259429999999999 8 0.5281590000000023))

  ((1.108143 0 0.0)
    (1.190237 0 0.0)
    (1.268068 0 0.0)
    (1.565256 8 0.5282460000000029)
    (1.7550210000000002 7 0.4594579999999979))

  ;; 250k:
  ((2.745476 0 0.0)
    (2.984801 0 0.0)
    (3.167918 0 0.0)
    (3.8508139999999997 19 1.2566699999999997)
    (4.435362 18 1.1894869999999997))

  ((2.7469099999999997 0 0.0)
    (2.9828040000000002 0 0.0)
    (3.165054 0 0.0)
    (3.848503 19 1.252804999999995)
    (4.512521 19 1.265744000000005))


  ((2.756316 0 0.0)
    (2.981145 0 0.0)
    (3.1631709999999997 0 0.0)
    (3.852269 19 1.2525680000000001)
    (4.491689999999999 19 1.2460780000000007))

  ((2.999263 0 0.0)
    (2.9799499999999997 0 0.0)
    (3.1813960000000003 0 0.0)
    (3.8475949999999997 19 1.2499000000000002)
    (4.499066 19 1.2528980000000018))

  ((2.991221 0 0.0)
    (2.992302 0 0.0)
    (3.165289 0 0.0)
    (3.8324510000000003 19 1.239767999999998)
    (4.511541 19 1.260944000000002))

  ((2.72603 0 0.0)
    (2.980267 0 0.0)
    (3.161387 0 0.0)
    (3.8817589999999997 20 1.2833929999999993)
    (4.474498 19 1.2224160000000008))

  ((3.241275 0 0.0)
    (3.16588 0 0.0)
    (3.830754 19 1.2302300000000006)
    (4.499739 19 1.2378339999999994))

  ((3.263362 0 0.0)
    (3.163977 0 0.0)
    (3.908312 17 1.1300460000000072)
    (4.372341 17 1.1301349999999957))
  ((3.534412 0 0.0)
    (3.176284 0 0.0)
    (3.9534949999999998 17 1.1753929999999997)
    (4.43239 17 1.1783140000000003))

  ((3.255306 0 0.0)
    (3.165536 0 0.0)
    (3.9446260000000004 17 1.1695180000000107)
    (4.497001 18 1.2412149999999968))

  ((3.5435890000000003 0 0.0)
    (3.1610300000000002 0 0.0)
    (3.969637 17 1.1800970000000035)
    (4.4320319999999995 17 1.1753900000000073))

  
  ((3.331073 0 0.0)
    (3.00392 0 0.0)
    (3.770642 17 1.1825889999999504)
    (4.282983 17 1.1891800000000217))

  ((3.249873 0 0.0)
    (2.962156 0 0.0)
    (3.838126 16 1.1245470000000068)
    (4.245882000000001 16 1.1115419999999858))

  ((2.970695 0 0.0)
    (2.729214 0 0.0)
    (3.430835 15 1.0401960000000372)
    (3.893268 15 1.0412259999999947))

  ((3.0867210000000003 0 0.0)
    (2.758928 0 0.0)
    (3.2976360000000002 16 1.1116509999999948)
    (3.7530099999999997 15 1.0466620000000262))

  ((3.046049 0 0.0)
    (2.765993 0 0.0)
    (3.258903 15 1.0381800000000112)
    (3.7419520000000004 15 1.0366539999999986))

  ((12.469066 0 0.0)
    (11.265325 0 0.0)
    (13.28118 64 4.3886309999999895)
    (15.409806 63 4.358108999999999))

  ((11.251618 0 0.0)
    (12.432974 0 0.0)
    (15.454493000000001 64 4.404036000000005)
    (13.275089999999999 64 4.377848999999998))

  ((11.278935 0 0.0)
    (12.435295 0 0.0)
    (13.297673999999999 63 4.3912249999999915)
    (15.484722 64 4.425944999999999))

  ((12.486452 0 0.0)
    (11.258434 0 0.0)
    (13.276031999999999 64 4.372042999999991)
    (15.357441 63 4.3088809999999995))

  ((1.755002 0 0.0)
    (1.7676530000000001 8 0.623942999999997)
    (1.551321 0 0.0)
    (2.058687 8 0.6167419999999879)
    (2.059972 8 0.6209509999999909))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


