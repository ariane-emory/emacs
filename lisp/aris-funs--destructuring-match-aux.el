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
;; (dont confirm that (dm::properize-target! '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
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
;; (dont confirm that (dm::properize-target '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern! (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (if (not (proper-list-p lst))
    (dm::properize-target! lst) ; target fun works in this case.
    (let ((tail (last lst 2)))
      (when (eq (car tail)  '\,) ; will also need to detect UNSPLICE!
        (setcar tail '\.)
        (setcdr tail (list (list '\, (cadr tail))))))
    lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern! (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (let ((improper-indicator '\.))
    (doconses (head pos lst lst)
      (when (consp head)
        (setcar pos (dm::properize-pattern! head)))
      (when (and (cdr pos) (atom (cdr pos)))
        ;; found the improper tail.
        (setcdr pos
          (append
            (when improper-indicator (list improper-indicator))
            (list (cdr pos))))
        (setf pos (cdr pos)))))

  ;; (if (not (proper-list-p lst))
  ;;   (dm::properize-target! lst) ; target fun works in this case.
  ;;   (let ((tail (last lst 2)))
  ;;     (when (eq (car tail)  '\,) ; will also need to detect UNSPLICE!
  ;;       (setcar tail '\.)
  ;;       (setcdr tail (list (list '\, (cadr tail))))))
  ;;   lst)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-pattern!  nil)          returns nil)
(confirm that (dm::properize-pattern! '(,x))         returns ((\, x)))
(confirm that (dm::properize-pattern! '(,x .  y))    returns ((\, x) \. y))
(confirm that (dm::properize-pattern! '(,x . ,y))    returns ((\, x) \. (\, y)))
(confirm that (dm::properize-pattern! '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
(confirm that (dm::properize-pattern! '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
(confirm that (dm::properize-pattern! '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
(confirm that (dm::properize-pattern! '(,x ,y . ,(z  integer?)))
  returns ((\, x) (\, y) \. (\, (z integer?)))) 
;; (dont confirm that (dm::properize-pattern '(,x . ,y ,z)) ... ; bulshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern (lst)
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
          ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
            (push improper-indicator res) 
            (push
              (list '\, (let ((next (cadr pos))) (if (atom next) next (dm::properize-pattern next))))
              res)
            (setf pos nil))
          ((not (listp (cdr pos)))
            (push (dm::properize-pattern head) res)
            (push improper-indicator res)
            (push (cdr pos) res)
            (setf pos nil))
          ((listp head) (push (dm::properize-pattern head) res))
          (t (push head res)))
        (setf not-first t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern (lst &optional not-first)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (let ((improper-indicator '\.))
    (cond
      ((atom lst) lst)
      ((and (cdr lst) (atom (cdr lst)))
        (cons
          (dm::properize-pattern (car lst) nil)
          (append (when improper-indicator (list improper-indicator))
            (list (cdr lst)))))
      ((and not-first (not (cddr lst)) (eq '\, (car lst)))
        (list improper-indicator (list '\, (dm::properize-pattern (cadr lst) nil))))
      (t (cons
           (dm::properize-pattern (car lst) nil)
           (dm::properize-pattern (cdr lst) t))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-pattern  nil)          returns nil)
(confirm that (dm::properize-pattern '(,x))         returns ((\, x)))
(confirm that (dm::properize-pattern '(,x .  y))    returns ((\, x) \. y))
(confirm that (dm::properize-pattern '(,x . ,y))    returns ((\, x) \. (\, y)))
(confirm that (dm::properize-pattern '(,x . (y z))) returns ((\, x) y z))
(confirm that (dm::properize-pattern '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
(confirm that (dm::properize-pattern '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
(confirm that (dm::properize-pattern '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
(confirm that (dm::properize-pattern '((,w . ,y). ,z))
  returns (((\, w) \. (\, y)) \. (\, z)))
(confirm that (dm::properize-pattern '(,v (,w ,x . ,y). ,z))
  returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
(confirm that (dm::properize-pattern '(,x ,y . ,(z  integer?)))
  returns ((\, x) (\, y) \. (\, (z integer?))))
;; this one would not be a legal pattern due to the way ,z is used in the innermost sub-expression,
;; but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure it doesn't try: 
(confirm that (dm::properize-pattern '(,x ,y . ,(,z integer?)))
  returns ((\, x) (\, y) \. (\,((\, z) integer?))))
;; (dont confirm that (dm::properize-pattern '(,x . ,y ,z)) ... ; bulshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
