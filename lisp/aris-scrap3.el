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




(defmacro let-it (expr &rest body)
  `(let ((it ,expr)) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cond-let2 (&rest clauses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate the first clause whose `let' bindings aren't nil or a final t clause (if present)."
  (cl-macrolet ((let-it (expr &rest body) `(let ((it ,expr)) ,@body)))
    (cond
      ((null clauses) nil)
      ((atom (car clauses))
        (error "Malformed CLAUSES, clauses must be conses, got: %S." (car clauses)))
      ((and (atom (caar clauses)) (cdr clauses))
        (error "Malformed CLAUSES, clause with atomic head %s precedes %S."))
      ((let-it (caar clauses) (and (atom it) (or (eq t it) (keywordp it) (not (symbolp it)))))
        (macroexp-progn (cdar clauses)))
      ((cdr clauses)
        `(if-let ,(caar clauses)
           ,(macroexp-progn (cdar clauses))
           (cond-let2 ,@(cdr clauses))))
      (t `(when-let ,(caar clauses)
            ,(macroexp-progn (cdar clauses)))))))


(let-it 5 (+ 3 it))

(let ((x 2))
  (cond-let2
    (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
    ((the-string   (some 'string  x)) (concat "hello " the-string))
    ((the-bindings
       (and (consp x)
         (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
      (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
    (:otherwise "foo")))


(setq x 7)
(cond-let2
  (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
  ((the-string   (some 'string  x)) (concat "hello " the-string))
  ((the-bindings
     (and (consp x)
       (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
    (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
  (:otherwise "foo"))

;; (if-let ((the-integer (some 'integer x)) (_ (> the-integer 5)))
;;   (* 2 the-integer)
;;   (if-let (the-string (some 'string x))
;;     (concat "hello " the-string)
;;     (if-let
;;       (the-bindings
;;         (and (consp x)
;;           (dm:match '(foo (\,(bar symbolp (not (null bar)))) (_ (\,@ bazes)) \... \, needle) x)))
;;       (let-alist the-bindings
;;         `(,\.bar ,\.needle ,@(nreverse \.bazes)))
;;       (if :otherwise "foo"
;;         nil))))

(macroexp-if :otherwise "foo" nil)
