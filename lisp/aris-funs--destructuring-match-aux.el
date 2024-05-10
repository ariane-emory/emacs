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
;; properize targets:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-target! (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (properize! lst t '\.))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-target!  nil)       returns nil)
(confirm that (dm::properize-target! '(2))       returns (2))
(confirm that (dm::properize-target! '(2 . 4))   returns (2 \. 4))
(confirm that (dm::properize-target! '(2 4   6)) returns (2 4 6))
(confirm that (dm::properize-target! '(2 4 . 6)) returns (2 4 \. 6))
(let ((lst '(2 4 . 6)))
  (confirm that (dm::properize-target! lst) returns (2 4 \. 6))
  (confirm that lst returns (2 4 \. 6)))
;; don't confirm: (dm::properize-target! '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-target (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (properize lst t '\.))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-target  nil)       returns nil)
(confirm that (dm::properize-target '(2))       returns (2))
(confirm that (dm::properize-target '(2 . 4))   returns (2 \. 4))
(confirm that (dm::properize-target '(2 4   6)) returns (2 4 6))
(confirm that (dm::properize-target '(2 4 . 6)) returns (2 4 \. 6))
;; don't confirm: (dm::properize-target '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; properize patterns:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern!* (lst &optional not-first) ; deeply recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (if (atom lst)
    lst
    (let ((improper-indicator '\.))
      (cond
        ;; ((atom lst) lst)
        ((and (cdr lst) (atom (cdr lst)))
          ;; found an improper tail, properize it:
          (setcar lst (dm::properize-pattern!* (car lst) nil))
          (setcdr lst (list improper-indicator (cdr lst)))
          lst)
        ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
          ;; found a wayward comma, fix it:
          (setcar lst improper-indicator)
          (setcdr lst (list  (list '\, (dm::properize-pattern!* (cadr lst) nil)))))
        ((atom (car lst))
          (setcdr lst (dm::properize-pattern!* (cdr lst) t)))
        (t (setcar lst (dm::properize-pattern!* (car lst) nil))
          (setcdr lst (dm::properize-pattern!* (cdr lst) t))))
      lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-pattern!*  nil)          returns nil)
(confirm that (dm::properize-pattern!* '(,x))         returns ((\, x)))
(confirm that (dm::properize-pattern!* '(,x .  y))    returns ((\, x) \. y))
(confirm that (dm::properize-pattern!* '(,x . ,y))    returns ((\, x) \. (\, y)))
(confirm that (dm::properize-pattern!* '(,x . (y z))) returns ((\, x) y z))
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
  (confirm that lst                                  returns ((\, x) \. y)))
;; don't confirm: (dm::properize-pattern!* '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern! (lst) ; shallowly recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (let ( (improper-indicator '\.)
         (not-first nil))
    (doconses (head pos lst lst)
      (cond
        ((and (cdr pos) (atom (cdr pos)))
          ;; found an improper tail, properize it:
          (setcar pos (dm::properize-pattern! head))
          (setcdr pos (list improper-indicator (cdr pos)))
          (setf pos nil))
        ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
          ;; found a wayward comma, fix it:
          (setcar pos improper-indicator)
          (setcdr pos
            (list (list '\,
                    (if (listp (cadr pos)) (dm::properize-pattern! (cadr pos)) (cadr pos)))))
          (setf pos nil))
        ;; ((atom head))
        ((consp head) (setcar pos (dm::properize-pattern! head)))
        )
      (setf not-first t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-pattern!  nil)          returns nil)
(confirm that (dm::properize-pattern! '(,x))         returns ((\, x)))
(confirm that (dm::properize-pattern! '(,x .  y))    returns ((\, x) \. y))
(confirm that (dm::properize-pattern! '(,x . ,y))    returns ((\, x) \. (\, y)))
(confirm that (dm::properize-pattern! '(,x . (y z))) returns ((\, x) y z))
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
  (confirm that lst                                  returns ((\, x) \. y)))
;; don't confirm: (dm::properize-pattern! '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern* (lst &optional not-first) ; deeply recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (if (atom lst)
    lst
    (let ((improper-indicator '\.))
      (cond
        ((and (cdr lst) (atom (cdr lst)))
          ;; found an improper tail, properize it:
          (list (dm::properize-pattern* (car lst) nil) improper-indicator (cdr lst)))
        ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
          ;; found a wayward comma, fix it:
          (list improper-indicator
            (list '\, (dm::properize-pattern* (cadr lst) nil))))
        (t (cons
             (dm::properize-pattern* (car lst) nil)
             (dm::properize-pattern* (cdr lst) t)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-pattern*  nil)          returns nil)
(confirm that (dm::properize-pattern* '(,x))         returns ((\, x)))
(confirm that (dm::properize-pattern* '(,x .  y))    returns ((\, x) \. y))
(confirm that (dm::properize-pattern* '(,x . ,y))    returns ((\, x) \. (\, y)))
(confirm that (dm::properize-pattern* '(,x . (y z))) returns ((\, x) y z))
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
  returns ((\, x) (\, y) \. (\,((\, z) integer?))))
;; don't confirm: (dm::properize-pattern* '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern (lst) ; shallowly recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  ;; (prndiv)
  (nreverse
    (let ( (improper-indicator '\.)
           (not-first nil)
           (res nil))
      (doconses (head pos lst res)
        (cond
          ((and (cdr pos) (atom (cdr pos)))
            ;; found an improper tail, properize it:
            (push (dm::properize-pattern head) res)
            (push improper-indicator res)
            (push (cdr pos) res)
            (setf pos nil))
          ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
            ;; found a wayward comma, fix it:
            (push improper-indicator res) 
            (push
              (list '\,
                (let ((next (cadr pos)))
                  (if (atom next) next (dm::properize-pattern next))))
              res)
            (setf pos nil))
          ((listp head) (push (dm::properize-pattern head) res))
          (t (push head res)))
        (setf not-first t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-pattern  nil)          returns nil)
(confirm that (dm::properize-pattern '(,x))         returns ((\, x)))
(confirm that (dm::properize-pattern '(,x .  y))    returns ((\, x) \. y))
(confirm that (dm::properize-pattern '(,x . ,y))    returns ((\, x) \. (\, y)))
(confirm that (dm::properize-pattern '(,x . (y z))) returns ((\, x) y z))
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
  returns ((\, x) (\, y) \. (\,((\, z) integer?))))
;; don't confirm: (dm::properize-pattern '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (setq reps 1000000)

    (list
      (benchmark-run reps (dm::properize-pattern!* '(,v (,w ,x . ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern! '(,v (,w ,x . ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern* '(,v (,w ,x . ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern '(,v (,w ,x . ,y) . ,z)))
      )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )

  ((8.051288 0 0.0)
    (6.346171 0 0.0)
    (7.693087 32 2.2990800000000036)
    (8.291834 33 2.392645999999985))

  ((9.259978 0 0.0)
    (6.363157 0 0.0)
    (7.7342439999999995 32 2.3366629999999873)
    (8.251667 32 2.3615139999999997))

  ((9.255557 0 0.0)
    (6.365731 0 0.0)
    (7.764772 32 2.365460999999982)
    (8.169539 31 2.2713640000000055))

  ((9.275749999999999 0 0.0)
    (6.361552 0 0.0)
    (7.723659 32 2.3289210000000082)
    (8.233366 32 2.341226000000006))

  ((9.251829 0 0.0)
    (6.517494 0 0.0)
    (7.714694 32 2.322127999999992)
    (8.251237999999999 32 2.3479090000000156))

  ((9.279052 0 0.0)
    (6.536695 0 0.0)
    (7.761265 32 2.33556200000001)
    (8.226158 32 2.3267370000000085))

  ((9.266738 0 0.0)
    (6.792406 0 0.0)
    (7.824925 33 2.415692000000007)
    (8.225909999999999 32 2.3338090000000022))

  
  
  ( (9.265348 0 0.0)
    (6.789523 0 0.0)
    (7.749393 32 2.3355499999999836)
    (8.230195 32 2.338384000000019))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
