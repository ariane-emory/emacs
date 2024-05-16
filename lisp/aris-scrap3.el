;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro some (type val)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return VAL when it is of type TYPE, otherwise return nil. (Conservative version)"
  `(when (cl-typep ,val ',type) ,val))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (some integer 8) returns 8)
(confirm that (some (pair-of integer) '(7 . 8)) returns (7 . 8))
(confirm that (some integer "hello") returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cond-let (&rest clauses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate the first clause whose `let' bindings aren't nil or a final t clause (if present)."
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq x 9)

(let ((x '(foo quux ((zot 4 5 6) shprungy qwib poof) 1 2 3 . zap)))
  (dm::clear-compiled-patterns)
  (cond-let
    (((the-integer (some integer x)) (_ (> the-integer 5))) (* 2 the-integer))
    ((the-string   (some string  x)) (concat "hello " the-string))
    ((the-bindings
       (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x))
      (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
    (otherwise "no matching clause"))) ; (quux zap poof qwib shprungy)

(ignore!
  
  (let ((x '(foo quux ((zot 4 5 6) shprungy qwib poof) 1 2 3 . zap)))
    (dm::clear-compiled-patterns)
    (cond-let
      ((let (the-integer (some integer x)) (_ (> the-integer 5)))
        (* 2 the-integer))
      ((let (the-string   (some string  x)))
        (concat "hello " the-string)
        ((match (foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle))
          `(,.bar ,.needle ,@(nreverse .bazes)))
        ((if (< 8 9)) :foo)
        (otherwise "no matching clause"))))

  )
