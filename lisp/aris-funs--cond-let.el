;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--some) ; only used in some tests.
(require 'aris-funs--destructuring-match) ; only used in some tests.
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
(let ((*dm:verbose* nil))
  (confirm that
    (let ((x '(foo quux ((zot 4 5 6) shprungy qwib poof) 1 2 3 . zap)))
      (cond-let
        (((the-integer (some* 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
        ((the-string   (some* 'string  x)) (concat "hello " the-string))
        ((the-bindings
           (and (consp x)
             (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
          (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
        (otherwise "no matching clause")))
    returns (quux zap poof qwib shprungy))
  (confirm that
    (let ((x 8))
      (cond-let
        (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
        ((the-string   (some 'string  x)) (concat "hello " the-string))
        ((the-bindings
           (and (consp x)
             (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
          (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
        (otherwise "no matching clause")))
    returns 16)
  (confirm that
    (let ((x "world"))
      (cond-let
        (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
        ((the-string   (some 'string  x)) (concat "hello " the-string))
        ((the-bindings
           (and (consp x)
             (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
          (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
        (otherwise "no matching clause")))
    returns "hello world")
  (confirm that
    (let ((x :foo))
      (cond-let
        (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
        ((the-string   (some 'string  x)) (concat "hello " the-string))
        ((the-bindings
           (and (consp x)
             (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
          (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
        (otherwise "no matching clause")))
    returns "no matching clause")
  (confirm that
    (let ((x (list 2 3 4)))
      (cond-let
        (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
        ((the-string   (some 'string  x)) (concat "hello " the-string))
        ((the-bindings
           (and (consp x)
             (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
          (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
        (otherwise "no matching clause")))
    returns "no matching clause"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A hypothetical `super-cond' of some sort that I probably shouldn't actually build:
(ignore!
  (let ((x '(foo quux ((zot 4 5 6) shprungy qwib poof) 1 2 3 . zap)))
    (super-cond
      ((let (the-integer (some integer x)) (_ (> the-integer 5)))
        (* 2 the-integer))
      ((let (the-string (some string x)))
        (concat "hello " the-string))
      ((match (foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle))
        `(,.bar ,.needle ,@(nreverse .bazes)))
      ((if (< 8 9)) :foo)
      (otherwise "no matching clause"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--cond-let)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
