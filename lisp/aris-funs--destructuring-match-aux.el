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
  (confirm that lst                                  returns ((\, x) \. y)))
;; don't confirm: (dm::properize-pattern!* '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
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
      ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
        ;; found a wayward comma, fix it:
        (list *dm::improper-indicator*
          (list '\, (dm::properize-pattern* (cadr lst) nil))))
      ((and (cdr lst) (atom (cdr lst)))
        ;; found an improper tail, properize it:
        (list (dm::properize-pattern* (car lst) nil) *dm::improper-indicator* (cdr lst)))
      (t (cons
           (dm::properize-pattern* (car lst) nil)
           (dm::properize-pattern* (cdr lst) t))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  returns ((\, x) (\, y) \. (\,((\, z) integer?))))
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
          (setcar pos (if (atom head) head (dm::properize-pattern! head)))
          (setcdr pos (list *dm::improper-indicator* (cdr pos)))
          (setf pos nil))
        ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
          ;; found a wayward comma, fix it:
          (setcar pos *dm::improper-indicator*)
          (setcdr pos
            (list (list '\,
                    (if (consp (cadr pos)) (dm::properize-pattern! (cadr pos)) (cadr pos)))))
          (setf pos nil))
        ((consp head) (setcar pos (dm::properize-pattern! head))))
      (setf not-first t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (confirm that lst                                  returns ((\, x) \. y)))
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
          ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
            ;; found a wayward comma, fix it:
            (push *dm::improper-indicator* res) 
            (push
              (list '\,
                (let ((next (cadr pos)))
                  (if (atom next) next (dm::properize-pattern next))))
              res)
            (setf pos nil))
          ((and (cdr pos) (atom (cdr pos)))
            ;; found an improper tail, properize it:
            (push (if (atom head) head (dm::properize-pattern head)) res)
            (push *dm::improper-indicator* res)
            (push (cdr pos) res)
            (setf pos nil))
          ((atom head) (push head res))
          (t (push (dm::properize-pattern head) res)))
        (setf not-first t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  returns ((\, x) (\, y) \. (\,((\, z) integer?))))
;; don't confirm: (dm::properize-pattern '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (setq reps 500000)

    (list
      (benchmark-run reps (dm::properize-pattern!* '(,v (,w ,x . ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern* '(,v (,w ,x . ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern! '(,v (,w ,x . ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern '(,v (,w ,x . ,y) . ,z)))
      )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )

  ((3.481385 0 0.0)
    (3.488989 17 1.2106969999999997)
    (3.112445 0 0.0)
    (3.9265920000000003 16 1.13579))



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
