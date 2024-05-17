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
  (when clauses
    (if (cdar clauses)
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
(defun u::unify1 (bindings pat1 pat2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Recursively unify two patterns, returning an alist of variable bindings.

Example:
  (u::unify1 nil '(x + 1) '(,2 + ,y)) => '((x . 2) (y . 1)"
  (u::prndiv)
  (u::prn "pat1:     %S" pat1)
  (u::prn "pat2:     %S" pat2)
  (u::prn "bindings: %S" bindings)
  (u::prndiv ?\-)
  (cond
    ;;----------------------------------------------------------------------------------------------
    ((not (or pat1 pat2))
      (u::prn "PAT1 and PAT2 both ran out.")
      (or bindings t))
    ;;----------------------------------------------------------------------------------------------
    ((not pat1)
      (u::prn "PAT1 ran out.")
      nil)
    ;;----------------------------------------------------------------------------------------------
    ((not pat2)
      (u::prn "PAT2 ran out.")
      nil)
    ;;----------------------------------------------------------------------------------------------
    ;; uncurl improper tails:
    ((or (atom pat1) (atom pat2))
      (u::unify1 bindings (list pat1) (list pat2)))
    ;;----------------------------------------------------------------------------------------------
    ;; `eql' atoms on both sides:
    ((and (atom (car pat1)) (atom (car pat2)) (eql (car pat1) (car pat2)))
      (u::prn "%s and %s are eql atoms." (u::style (car pat1)) (u::style (car pat2)))
      (with-indentation (u::unify1 bindings (cdr pat1) (cdr pat2))))
    ;;----------------------------------------------------------------------------------------------
    ;; same variable:
    ((and (u::variable-p (car pat1)) (u::variable-p (car pat2))
       (eq (car pat1) (car pat2)))
      (u::prn "%s and %s are the same variable." (u::style (car pat1)) (u::style (car pat2)))
      (with-indentation (u::unify1 bindings (cdr pat1) (cdr pat2))))
    ;;----------------------------------------------------------------------------------------------
    ;; both different variables:
    ((and (u::variable-p (car pat1)) (u::variable-p (car pat2)))
      (u::prn "both PAT1 elem %s and PAT2 elem %s are variables."
        (u::style (car pat1))
        (u::style (car pat2)))
      (let ( (binding1 (assoc (car pat1) bindings))
             (binding2 (assoc (car pat2) bindings)))
        (u::prn "%s = %s" (u::style (car pat1))
          (if binding1 (u::style (cdr binding1)) "<unbound>"))
        (u::prn "%s = %s" (u::style (car pat2))
          (if binding2 (u::style (cdr binding2)) "<unbound>"))
        (cond
          ;;-----------------------------------------------------------------------------------------
          ;; bound to un-`equal' values:
          ((and binding1 binding2 (not (equal (cdr binding1) (cdr binding2))))
            (u::prn "already bound to different terms.")
            nil)
          ;;-----------------------------------------------------------------------------------------
          ;; neither bound:
          ((not (or binding1 binding2))
            (u::prn "both unbound.")
            (with-indentation
              (u::unify1 (cons (cons (car pat1) (car pat2)) bindings) (cdr pat1) (cdr pat2))))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1's var not bound, unify with PAT2's var:
          ((not binding1)
            (u::unify-variable-with-variable2 bindings pat1 pat2))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT2's var not bound, unify with PAT1's var:
          ((not binding2)
            (u::unify1 bindings pat2 pat1))
          ;;-----------------------------------------------------------------------------------------
          ;; PAT1 and PAT2's vars are bound to the same value, unify them destructively.
          ;; not totally sure about this but it seems to work, so far.
          (t (u::prn
               (concat "PAT1's var %s and PAT2's var %s are bound to the same value, "
                 "unifying them destructively.")
               (u::style (car pat1)) (u::style (car pat2)))
            (setcdr binding1 (car binding2))
            (with-indentation (u::unify1 bindings (cdr pat1) (cdr pat2)))))))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT1, value on PAT2:
    ((and (u::variable-p (car pat1)) (or *u:bind-conses* (atom (car pat2))))
      (with-indentation (u::unify-variable-with-value1 bindings pat1 pat2)))
    ;;----------------------------------------------------------------------------------------------
    ;; variable on PAT2, value on PAT1:
    ((and (u::variable-p (car pat2)) (or *u:bind-conses* (atom (car pat1))))
      (with-indentation (u::unify1 bindings pat2 pat1))
      ;; (u::unify-variable-with-value1 bindings pat2 pat1)
      )
    ;;----------------------------------------------------------------------------------------------
    ;; conses on both sides:
    ((and (consp (car pat1)) (consp (car pat2)))
      ;; (debug nil :conses (car pat1) (car pat2))
      (u::unify1 (u::unify1 bindings (car pat1) (car pat2)) (cdr pat1) (cdr pat2)))
    ;;----------------------------------------------------------------------------------------------
    (t nil ;; (debug nil t (car pat1) (car pat2))
      (u::prn "unhandled"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(u:unify '(one two three) '(,first . ,rest))
(u:unify '(,first . ,rest) '(one two three))
(u:unify '(,one ,two) '(first . rest))
(u:unify '(first . rest) '(,one ,two))

