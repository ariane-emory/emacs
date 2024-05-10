;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `dm:match', my destructuring pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-target! (lst &optional no-test)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (assert-list! lst)
  (when   (or no-test (not (proper-list-p lst)))
    (let ((last (last lst)))
      (setcdr last (list '\. (cdr last)))))
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-target!  nil)       returns nil)
(confirm that (dm::properize-target! '(2))       returns (2))
(confirm that (dm::properize-target! '(2 . 4))   returns (2 \. 4))
(confirm that (dm::properize-target! '(2 4   6)) returns (2 4 6))
(confirm that (dm::properize-target! '(2 4 . 6)) returns (2 4 \. 6))
;; (dont confirm that (dm::properize-target! '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern! (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (assert-list! lst)
  (if (not (proper-list-p lst))
    (dm::properize-target! lst t) ; target fun works in this case.
    (let ((tail (last lst 2)))
      (when (eq (car tail)  '\,) ; will also need to detect UNSPLICE!
        (setcar tail '\.)
        (setcdr tail (list (list '\, (cadr tail))))))
    lst))
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
(defun dm::properize-target (lst &optional no-test)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (assert-list! lst)
  (let (res)
    (nreverse
      (catch 'result
        (doconses (head pos lst res)
          (push head res)
          (unless (listp (cdr pos))
            (push '\. res)
            (push (cdr pos) res)
            (throw 'result res)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-target  nil)       returns nil)
(confirm that (dm::properize-target '(2))       returns (2))
(confirm that (dm::properize-target '(2 . 4))   returns (2 \. 4))
(confirm that (dm::properize-target '(2 4   6)) returns (2 4 6))
(confirm that (dm::properize-target '(2 4 . 6)) returns (2 4 \. 6))
;; (dont confirm that (dm::properize-target '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (assert-list! lst)
  (let (res)
    (nreverse
      (catch 'result
        (doconses (head pos lst res)
          (cond
            ((and (eq '\, head) (not (cddr pos)))
              (push '\.                   res)
              (push (list '\, (cadr pos)) res)
              (throw 'result res))
            ((not (listp (cdr pos)))
              (push head res)
              (push '\.       res)
              (push (cdr pos) res)
              (throw 'result res))
            (t (push head res))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::properize-pattern  nil)          returns nil)
(confirm that (dm::properize-pattern '(,x))         returns ((\, x)))
(confirm that (dm::properize-pattern '(,x .  y))    returns ((\, x) \. y))
(confirm that (dm::properize-pattern '(,x . ,y))    returns ((\, x) \. (\, y)))
(confirm that (dm::properize-pattern '(,x ,y   ,z)) returns ((\, x) (\, y) (\, z)))
(confirm that (dm::properize-pattern '(,x ,y .  z)) returns ((\, x) (\, y) \. z))
(confirm that (dm::properize-pattern '(,x ,y . ,z)) returns ((\, x) (\, y) \. (\, z)))
(confirm that (dm::properize-pattern '(,x ,y . ,(z  integer?)))
  returns ((\, x) (\, y) \. (\, (z integer?)))) 
;; (dont confirm that (dm::properize-pattern '(,x . ,y ,z)) ... ; bulshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
