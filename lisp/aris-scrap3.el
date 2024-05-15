;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro some (type val)
  "Return VAL when it is of type TYPE, otherwise return nil. (Conservative version)"
  `(and (cl-typep ,val ',type) ,val))

(some integer 8)
(some (pair-of integer) '(7 . 8))
(some integer "hello")

(defmacro cond-let (&rest clauses)
  (cond
    ((null clauses) nil)
    ((and (symbolp (caar clauses)) (not (null (caar clauses))))
      (when (cdr clauses) (error "Malformed CLAUSES, t clause precedes %S." (cdr clauses)))
      (macroexp-progn (cdar clauses)))
    ((not (consp (caar clauses)))
      (error "Malformed CLAUSES, clause heads must be conses or non-null symbols, found: %S."
        (caar clauses)))
    ((cdr clauses)
      `(if-let ,(caar clauses)
         ,(macroexp-progn (cdar clauses))
	       (cond-let ,@(cdr clauses))))))

(setq x 9)

(let ((x '(foo quux ((zot 4 5 6) shprungy qwib poof) 1 2 3)))
  (cond-let
    (((the-integer (some integer x)) (_ (> the-integer 5))) (* 2 the-integer))
    ((the-string   (some string  x))  (concat "hello " the-string))
    ((the-bindings (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ...) x))
      (let-alist the-bindings (cons .bar .bazes)))
    (otherwise "no matching clause"))) ;; (quux shprungy qwib poof)

;; expands into:
(if-let
  ((the-integer (some integer x)) (_ (> the-integer 5)))
  (* 2 the-integer)
  (if-let (the-string (some string x))
    (concat "hello " the-string)
    "no integer > 5 or string"))

