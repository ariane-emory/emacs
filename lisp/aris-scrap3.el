;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--some) ; only used in some tests.
(require 'aris-funs--destructuring-match) ; only used in some tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(some 'string   "foo")
(some #'stringp "foo")


(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    `(or (and ,(caar clauses) (progn ,@(cdar clauses)))
	     (cond2 ,@(cdr clauses)))))

(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    (let ((clause (car clauses)) (sym (gensym)))
      `(let ((,sym ,(car clause)))
         (if ,sym
           ,(if (null (cdr clause)) sym `(progn ,@(cdr clause)))
           (cond2 ,@(cdr clauses)))))))


(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    (if (cdar clauses)
      `(if ,(caar clauses) (progn ,@(cdar clauses)) (cond2 ,@(cdr clauses)))
      `(or ,(caar clauses) (cond2 ,@(cdr clauses))))))




;; Example usage:
(cond2
  ((= 2 1) (message "1"))
  ((= 3 3) (message "2"))
  (t (message "default")))


;; Example usage:
(cond2
  ((= 2 1) (message "1"))
  (32)
  ((= 3 3) (message "2"))
  (t (message "default")))

(if (= 2 1)
  (progn (message "1"))
  (or 32
    (if (= 3 3)
      (progn (message "2"))
      (if t
        (progn (message "default"))
        nil))))


(cond2
  ;; ((= 2 1) (message "1"))
  ;; (32)
  ;; ((= 3 3) (message "2"))
  (t (message "default")))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cond-let2 (&rest clauses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate the first clause whose `let' bindings aren't nil or a final t clause (if present)."
  (cond
    ((null clauses) nil)
    ((atom (car clauses))
      (error "Malformed CLAUSES, clauses must be conses, got: %S." (car clauses)))
    ((and (atom (caar clauses) (cdr clauses)))
      (error "Malformed CLAUSES, clause with atomic head %s precedes %S."))
    (t
      (let ((sym (if (atom (caar clauses)) 'if 'if-let)))
        `(,sym ,(caar clauses)
           (progn ,@(cdar clauses))
           (cond-let2 ,@(cdr clauses)))))))

(let ((x 22))
  (cond-let2
    (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
    ((the-string   (some 'string  x)) (concat "hello " the-string))
    ((the-bindings
       (and (consp x)
         (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
      (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
    (:otherwise "foo")))

(let ((x '(foo quux ((zot 4 5 6) shprungy qwib poof) 1 2 3 . zap)))
  (cond-let2
    (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
    (:otherwise)))

;; (cond2
'( ((= 2 1) (message "1"))
   ((= 3 3) (message "2"))
   (t (message "default")))
