;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--some)
(require 'aris-funs--destructuring-match) 
(require 'aris-funs--constant-folding-conditionals) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq foo 8)

(defmacro setq* (var val)
  (with-gensyms (sym)
    `(let ((,sym ,val))
       (prog1
         ,sym
         (setq ,var ,sym)))))

(defmacro setq* (var val)
  (with-gensyms (old new)
    `(let ( (,old ,var)
            (,new ,val))
       (setq ,var ,new)
       ,old)))

(setq* foo 6)
(setq* foo 10)

(let ((sym-1366 10))
  (prog1 sym-1366
    (setq foo sym-1366)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    `(or (and ,(caar clauses) (progn ,@(cdar clauses)))
	     (cond2 ,@(cdr clauses)))))

(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (if clauses
    (if (cdar clauses)
      `(if ,(caar clauses) (progn ,@(cdar clauses)) (cond2 ,@(cdr clauses)))
      `(or ,(caar clauses) (cond2 ,@(cdr clauses))))))

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
  (cl-macrolet ((let-it (expr &rest body) `(let ((it ,expr)) ,@body)))
    (cond
      ((null clauses) nil)
      ((and (cdar clauses) (cdr clauses))
        `(if ,(caar clauses) ,(macroexp-progn (cdar clauses)) (cond2 ,@(cdr clauses))))
      ((cdar clauses)
        `(when ,(caar clauses) ,(macroexp-progn (cdar clauses))))
      ((cdr clauses) `(or ,(caar clauses) (cond2 ,@(cdr clauses))))
      (t (caar clauses)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cond2 (&rest clauses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Re-implementation of ordinary `cond' with constant folding."
  (when* clauses ; added *
    (if* (cdar clauses) ; added *
      `(if* ,(caar clauses) ,(macroexp-progn (cdar clauses)) (cond2 ,@(cdr clauses)))
      `(or* ,(caar clauses) (cond2 ,@(cdr clauses))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x 7)

;; Example usage:
(cond2
  ((= x 1) (message "case 1"))
  (nil :foo)
  (32)
  ((= x 3) (message "case 3"))
  (t (message "default")))

;; Expansion:
(if (= x 1)
  (message "case 1")
  32)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let-it 'foo
  (gensymbolicate- it))



(ml::uniqueify-variables1 (gensym "") (ml::fix-variables '(X foos Bar)))
(ml::uniqueify-variables (ml::fix-variables '(X foos Bar)))


(u:unify '(\?X-1279 foos three) '(one foos \?Bar-1263))

(u:unify
  (ml::uniqueify-variables (ml::fix-variables '(Foo two three)))
  (ml::uniqueify-variables (ml::fix-variables '(one two Foo)))) ; ((\?Foo-1328 . three) (\?Foo-1327 . one))

(u:unifier
  (ml::uniqueify-variables (ml::fix-variables '(Foo two three)))
  (ml::uniqueify-variables (ml::fix-variables '(one two Foo)))) ; (one two three)



(ml::uniqueify-variables (ml::fix-variables '(Foo bar Baz))) ; => (\?Foo-1293 bar \?Baz-1293)


(gensymbolicate- 'foo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl-find 2 '(1 2 3) :test #'eql) ; 2
(cl-find 2 '((1 . a) (2 . c) (3 . c)) :test (lambda (x y) (eql x (car y)))) ; (2 . c)

(defun* foo ((ignored : integer)) ; return type not specified.
  (bar "3" "4"))

(defun* bar ((strint : string) (strint2 : string)) => (pair-of integer)
  (cons (read strint) (read strint2)))

(foo 1) ; (3 . 4)


(defconst *blah* nil)

(defun quux (x)
  (cond
    ((or *blah* t) 123)
    (x x)
    (t "456")))

(defconst *blah* nil)

(defun quux (x)
  (cond
    ((not *blah*) 123)
    (x x)
    (t "456")))
(quux 3)

(let ((*blah* t)) (quux "not an int"))

(ignore!
  (defconst *int-type*
    (cond
      ((32-bit-system?) 'i32)
      ((64-bit-system?) 'i64)))

  (defun portable-multiply ((x : *int-type*) (x : *int-type*))
    (cond
      ((eq *int-type* 'i32) (mul32 x y))
      ((eq *int-type* 'i64) (mul64 x y)))))


(defun u::unify1 (bindings pat1 pat2)
  (cond
    ((not (or pat1 pat2)) (or bindings t))
    ((not (and pat1 pat2)) nil)
    ((or (atom pat1) (atom pat2)) (u::unify1 bindings (list pat1) (list pat2)))
    ((and (atom (car pat1)) (atom (car pat2)) (eql (car pat1) (car pat2)))
      (u::unify1 bindings (cdr pat1) (cdr pat2)))
    ((and (u::variable-p (car pat1)) (u::variable-p (car pat2))
       (eq (car pat1) (car pat2)))
      (u::unify1 bindings (cdr pat1) (cdr pat2)))
    ((and (u::variable-p (car pat1)) (u::variable-p (car pat2)))
      (let ( (binding1 (assoc (car pat1) bindings))
             (binding2 (assoc (car pat2) bindings)))
        (cond
          ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2)))) nil)
          ((not (or binding1 binding2))
            (u::unify1 (cons (cons (car pat1) (car pat2)) bindings)
              (cdr pat1) (cdr pat2)))
          ((not binding1) (u::unify-variable-with-variable bindings pat1 pat2))
          ((not binding2) (u::unify1 bindings pat2 pat1))
          (t (setcdr binding1 (car binding2))
            (u::unify1 bindings (cdr pat1) (cdr pat2))))))
    ((and (u::variable-p (car pat1)) (or *u:bind-conses* (atom (car pat2))))
      (u::unify-variable-with-value1 bindings pat1 pat2))
    ((and (u::variable-p (car pat2)) (or *u:bind-conses* (atom (car pat1))))
      (u::unify1 bindings pat2 pat1))
    ((and (consp (car pat1)) (consp (car pat2)))
      (u::unify1 (u::unify1 bindings (car pat1) (car pat2)) (cdr pat1) (cdr pat2)))
    (t nil)))
