;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions used by `dm:match', my destructuring pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--lists)
(require 'aris-funs--sym-db)
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
    ((and (cdr lst) (atom (cdr lst)))
      ;;(prn "case 2")
      ;; found an improper tail, properize it:
      (when (consp (car lst))
        (setcar lst (dm::properize-pattern!* (car lst) nil)))
      (setcdr lst (list *dm::improper-indicator* (cdr lst))))
    ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
      ;;(prn "case 3")
      ;; found a wayward comma, fix it:
      ;; (debug (cadr lst))
      (let ((indic (car lst)))
        (setcar lst *dm::improper-indicator*)
        (setcdr lst (list (list indic
                            (cadr lst)
                            ;; (if (consp (cadr lst))
                            ;;   (dm::properize-pattern!* (cadr lst) nil)
                            ;;   (cadr lst))
                            )))))
    ((consp (car lst))
      ;;(prn "case 4")
      (unless (eq '\, (caar lst))
        (setcar lst (dm::properize-pattern!* (car lst) nil)))
      (dm::properize-pattern!* (cdr lst) t))
    ((cdr lst) ; (atom (car lst))
      ;;(prn "case 5: %s" (cdr lst))
      (dm::properize-pattern!* (cdr lst) t)))
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  ;; (unless (eq '\, (caar lst))
  (confirm that (dm::properize-pattern!* '(,w ,(x integer? . foo) . ,(y integer? . foo)))
    returns ((\, w) (\,(x integer? . foo)) \. (\,(y integer? . foo))))
  (confirm that (dm::properize-pattern!* '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern!* '(,x))         returns ((\, x)))
  ;; (confirm that (dm::properize-pattern!*  nil)          returns nil)
  (confirm that (dm::properize-pattern!* '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern!* '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern!* '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern!* '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern!* '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern!* '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern!* '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern!* '((w y) .  z)) returns ((w y) \. z))
  (confirm that (dm::properize-pattern!* '((w . y) .  z)) returns ((w \. y) \. z))
  (confirm that (dm::properize-pattern!* '((w y) .  ,z)) returns ((w y) \. (\, z)))
  (confirm that (dm::properize-pattern!* '((w . y) .  ,z)) returns ((w \. y) \. (\, z)))
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
    (confirm that lst                                   returns ((\, x) \. y))
    ))
;; don't confirm: (dm::properize-pattern!* '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern* (lst &optional not-first) ; deeply recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (cond
    ((null lst) lst)
    ((and (cdr lst) (atom (cdr lst)))
      ;; found an improper tail, properize it:
      (list
        (if (consp (car lst))
          (dm::properize-pattern* (car lst) nil)
          (car lst))
        *dm::improper-indicator* (cdr lst)))
    ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
      ;; found a wayward comma, fix it:
      (list *dm::improper-indicator*
        (list (car lst)
          (cadr lst)
          ;; (if (consp (cadr lst))
          ;;   (dm::properize-pattern* (cadr lst) nil)
          ;;   (cadr lst))
          )))
    ((consp (car lst))
      (cons
        (if (eq '\, (caar lst))
          (car lst)
          (dm::properize-pattern* (car lst) nil))
        (dm::properize-pattern* (cdr lst) t)))
    (t
      (cons
        (car lst)
        (dm::properize-pattern* (cdr lst) t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  ;; (unless (eq '\, (caar lst))
  (confirm that (dm::properize-pattern* '(,w ,(x integer? . foo) . ,(y integer? . foo)))
    returns ((\, w) (\,(x integer? . foo)) \. (\,(y integer? . foo))))
  (confirm that (dm::properize-pattern* '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern* '(,x))        returns ((\, x)))  
  (confirm that (dm::properize-pattern*  nil)          returns nil)
  (confirm that (dm::properize-pattern* '(,x))         returns ((\, x)))
  (confirm that (dm::properize-pattern* '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern* '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern* '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern* '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern* '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern* '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern* '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern* '((w y) .  z)) returns ((w y) \. z))
  (confirm that (dm::properize-pattern* '((w . y) .  z)) returns ((w \. y) \. z))
  (confirm that (dm::properize-pattern* '((w y) .  ,z)) returns ((w y) \. (\, z)))
  (confirm that (dm::properize-pattern* '((w . y) .  ,z)) returns ((w \. y) \. (\, z)))
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
            (list (list head
                    (cadr pos)
                    ;; (possibly dm::properize-pattern! (cadr pos) unless it is an atom)
                    )))
          (setf pos nil))
        ((consp head)
          (unless (eq '\, (car head))
            (setcar pos (dm::properize-pattern! head)))))
      (setf not-first t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  ;; (unless (eq '\, (caar lst))
  (confirm that (dm::properize-pattern! '(,w ,(x integer? . foo) . ,(y integer? . foo)))
    returns ((\, w) (\,(x integer? . foo)) \. (\,(y integer? . foo))))
  (confirm that (dm::properize-pattern! '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern! '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern!  nil)          returns nil)
  (confirm that (dm::properize-pattern! '(,x))         returns ((\, x)))
  (confirm that (dm::properize-pattern! '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern! '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern! '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern! '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern! '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern! '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern! '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern! '((w y) .  z)) returns ((w y) \. z))
  (confirm that (dm::properize-pattern! '((w . y) .  z)) returns ((w \. y) \. z))
  (confirm that (dm::properize-pattern! '((w y) .  ,z)) returns ((w y) \. (\, z)))
  (confirm that (dm::properize-pattern! '((w . y) .  ,z)) returns ((w \. y) \. (\, z)))
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
                        (list head
                          (cadr pos)
                          ;; (possibly dm::properize-pattern (cadr pos) unless it is an atom)
                          )
                        (cons *dm::improper-indicator* res)))
            (setq pos nil))
          ((atom head) (push head res))
          ((consp head)
            (push (if (eq '\, (car head)) head (dm::properize-pattern head)) res))
          (t (push (dm::properize-pattern head) res)))
        (setq not-first t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  ;; (unless (eq '\, (caar lst))
  (confirm that (dm::properize-pattern '(,w ,(x integer? . foo) . ,(y integer? . foo)))
    returns ((\, w) (\,(x integer? . foo)) \. (\,(y integer? . foo))))
  (confirm that (dm::properize-pattern '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern  nil)          returns nil)
  (confirm that (dm::properize-pattern '(,x))         returns ((\, x)))
  (confirm that (dm::properize-pattern '(,x .  y))    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern '(,x . ,y))    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern '(,x . (y z))) returns ((\, x) y z))
  (confirm that (dm::properize-pattern '(,x  y .  z)) returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern '((w y) .  z)) returns ((w y) \. z))
  (confirm that (dm::properize-pattern '((w . y) .  z)) returns ((w \. y) \. z))
  (confirm that (dm::properize-pattern '((w y) .  ,z)) returns ((w y) \. (\, z)))
  (confirm that (dm::properize-pattern '((w . y) .  ,z)) returns ((w \. y) \. (\, z)))
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
;; Benchmark;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (setq reps 100000)

    (list
      (benchmark-run reps (dm::properize-pattern!*  '(,v (,w ,x (a . (b c . d)) (,a b ,c ,(d integer? . e)) ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern!   '(,v (,w ,x (a . (b c . d)) (,a b ,c ,(d integer? . e)) ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern*   '(,v (,w ,x (a . (b c . d)) (,a b ,c ,(d integer? . e)) ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern    '(,v (,w ,x (a . (b c . d)) (,a b ,c ,(d integer? . e)) ,y) . ,z)))
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )
  ((0.6473749999999999 0 0.0)
    (0.6357649999999999 0 0.0)
    (0.864646 4 0.27644699999999034)
    (0.976884 4 0.26985899999999674))
  ((0.774393 0 0.0)
    (0.789689 0 0.0)
    (1.0495340000000002 5 0.3593659999999943)
    (1.2390130000000001 5 0.3422210000000092))
  ((1.075528 0 0.0)
    (1.278024 0 0.0)
    (1.510217 7 0.47671199999999914)
    (1.868028 8 0.5601659999999988))
  ((1.078125 0 0.0)
    (1.277194 0 0.0)
    (1.583483 8 0.548649000000001)
    (1.7798720000000001 7 0.4769609999999993))
  ((1.091493 0 0.0)
    (1.275776 0 0.0)
    (1.51251 7 0.4795790000000011)
    (1.861343 8 0.5510710000000003))
  ((1.063638 0 0.0)
    (1.264557 0 0.0)
    (1.575855 8 0.5448719999999998)
    (1.782602 7 0.47675100000000015))
  ((1.05327 0 0.0)
    (1.270699 0 0.0)
    (1.589 8 0.5424309999999988)
    (1.8295130000000002 8 0.5262330000000013))
  ((1.063021 0 0.0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::reset ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Clear any interned patterns."
  (ensure-db! '*dm*)
  (clear-db   '*dm*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (hash-table-p (dm::reset)) returns t)
(confirm that (length (symbol-plist '*dm*)) returns 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::intern-pattern (unsplice ellipsis dont-care pat)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Store properized patterns in a hashtable to avoid repeatetly properizing the same pattern."
  (ensure-db! '*dm*)
  (let ((existing (db-get '*dm* pat)))
    (if (cdr existing)
      (car existing)
      ;; `dm::prn' not available here! move it to a new file so it is!
      (prn "Interning pattern %s" pat)
      (db-put '*dm* (list unsplice ellipsis dont-care pat) (dm::properize-pattern* pat)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ((pat '(,w ,(x integer? . foo) . ,(y integer? . foo))))
    (dm::intern-pattern '\,@ '... '_ pat))
  returns ((\, w) (\, (x integer? . foo)) \. (\, (y integer? . foo))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::unproperize!* (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn lists with *dm::improper-indicator* in their second last position back into improper lists."
  (let ((pos lst))
    (prndiv)
    (while (consp pos)
      (if (atom pos)
        (prn "atom:     %s"  pos)
        (prn "head:     %s" (car pos))
        (when (consp (car pos))
          (with-indentation
            (dm::unproperize!* (car pos))))
        (when (eq (cadr-safe pos) *dm::improper-indicator*)
          (when (or (cadddr pos) (not (cddr pos)))
            (error "properize indicator in unexpected position: %s" lst))
          (setcdr pos (caddr pos))
          (setf pos nil))
        (pop pos))))
  (prn "lst:      %s" lst)
  (prndiv)
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADD TESTS!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


