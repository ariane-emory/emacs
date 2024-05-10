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
(defvar *dm::improper-indicator* '\. "The symbol used to indicate that a list has been properized.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; properize targets:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-target! (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (properize! lst t *dm::improper-indicator*))
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
  (properize lst t *dm::improper-indicator*))
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
    (cond
      ;; ((atom lst) lst)
      ((and (cdr lst) (atom (cdr lst)))
        ;; found an improper tail, properize it:
        (setcar lst (dm::properize-pattern!* (car lst) nil))
        (setcdr lst (list *dm::improper-indicator* (cdr lst)))
        lst)
      ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
        ;; found a wayward comma, fix it:
        (setcar lst *dm::improper-indicator*)
        (setcdr lst (list (list '\, (dm::properize-pattern!* (cadr lst) nil)))))
      ((consp (car lst)) (setcar lst (dm::properize-pattern!* (car lst) nil))
        (setcdr lst (dm::properize-pattern!* (cdr lst) t)))
      (t ; (atom (car lst))
        (setcdr lst (dm::properize-pattern!* (cdr lst) t))))
    lst))
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
  (let (not-first)
    (doconses (head pos lst lst)
      (cond
        ((and (cdr pos) (atom (cdr pos)))
          ;; found an improper tail, properize it:
          (setcar pos (dm::properize-pattern! head))
          (setcdr pos (list *dm::improper-indicator* (cdr pos)))
          (setf pos nil))
        ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
          ;; found a wayward comma, fix it:
          (setcar pos *dm::improper-indicator*)
          (setcdr pos
            (list (list '\,
                    (if (listp (cadr pos)) (dm::properize-pattern! (cadr pos)) (cadr pos)))))
          (setf pos nil))
        ((consp head) (setcar pos (dm::properize-pattern! head))))
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
    (cond
      ((and (cdr lst) (atom (cdr lst)))
        ;; found an improper tail, properize it:
        (list (dm::properize-pattern* (car lst) nil) *dm::improper-indicator* (cdr lst)))
      ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
        ;; found a wayward comma, fix it:
        (list *dm::improper-indicator*
          (list '\, (dm::properize-pattern* (cadr lst) nil))))
      (t (cons
           (dm::properize-pattern* (car lst) nil)
           (dm::properize-pattern* (cdr lst) t))))))
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
    (let (not-first res)
      (doconses (head pos lst res)
        (cond
          ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
            ;; found a wayward comma, fix it:
            (push *dm::improper-indicator* res) 
            (push
              (list '\,
                (let ((next (cadr pos)))
                  (if (atom next) next (dm::properize-pattern next))))
              res)
            (setf pos nil))
          ((atom head) (push head res))
          ;; now we know head's a list.
          ((and (cdr pos) (atom (cdr pos)))
            ;; found an improper tail, properize it:
            (push (dm::properize-pattern head) res)
            (push *dm::improper-indicator* res)
            (push (cdr pos) res)
            (setf pos nil))
          (t (push (dm::properize-pattern head) res)))
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

  ((6.9353869999999995 0 0.0)
    (6.265205000000001 0 0.0)
    (7.010495 32 2.341769999999997)
    (7.611699 32 2.3612980000000334))

  ((6.931694 0 0.0)
    (6.261959 0 0.0)
    (7.016323 32 2.3485039999999913)
    (7.605618 32 2.355562999999961))

  ((6.9367909999999995 0 0.0)
    (6.137823999999999 0 0.0)
    (7.0196439999999996 32 2.3485959999999864)
    (8.013167 32 2.3482890000000225))

  ((6.913105 0 0.0)
    (6.162233 0 0.0)
    (7.016597 32 2.3439649999999688)
    (8.174329 32 2.3671600000000126))


  ((6.92939 0 0.0)
    (6.119363 0 0.0)
    (6.993239 32 2.329365999999993)
    (8.028267000000001 32 2.3547770000000128))

  ((8.076667 0 0.0)
    (6.393187999999999 0 0.0)
    (7.821287 33 2.4203449999999975)
    (8.231491 32 2.3303220000000238))

  ((8.070197 0 0.0)
    (6.3960870000000005 0 0.0)
    (7.737962 32 2.3395270000000323)
    (8.260914999999999 32 2.3539009999999507))

  ((8.078796 0 0.0)
    (6.362006 0 0.0)
    (7.7201319999999996 32 2.3274169999999685)
    (8.326423 33 2.4143770000000018))

  ((8.067729 0 0.0)
    (6.367405 0 0.0)
    (7.726325999999999 32 2.3221679999999765)
    (8.281357 32 2.3650180000000205))

  ((8.080079000000001 0 0.0)
    (6.357424999999999 0 0.0)
    (7.754121 32 2.334134000000006)
    (8.228990000000001 32 2.334557000000018))

  ((8.045619 0 0.0)
    (6.384291 0 0.0)
    (7.747304 32 2.3294369999999844)
    (8.211765 32 2.3107270000000426))

  ((8.07165 0 0.0)
    (6.4332530000000006 0 0.0)
    (7.731896000000001 32 2.3331150000000207)
    (8.239099000000001 32 2.335113999999976))

  ((8.051624 0 0.0)
    (6.339232 0 0.0)
    (7.741712000000001 32 2.3394069999999942)
    (8.237836 32 2.3376510000000508))

  
  ((8.050831 0 0.0)
    (6.359554 0 0.0)
    (7.782384 33 2.382068000000004)
    (8.212067 32 2.3109899999999755))

  ((8.051288 0 0.0)
    (6.346171 0 0.0)
    (7.693087 32 2.2990800000000036)
    (8.291834 33 2.392645999999985))




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
