;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--some) ; only used in some tests.
(require 'aris-funs--destructuring-match) ; only used in some tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(some 'string   "foo")
(some #'stringp "foo")

(defmacro let-it (expr &rest body)
  `(let ((it ,expr)) ,@body))

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


;; Example usage:
(cond2
  ((= 2 1) (message "1"))
  ((= 3 3)) ; (message "2")
  (t (message "default")))

;; Example usage:
(cond2
  ((= 2 1) (message "1"))
  (32)
  ((= 3 3) (message "2"))
  (t (message "default")))

(cond2
  ;; ((= 2 1) (message "1"))
  ;; (32)
  ;; ((= 3 3) (message "2"))
  (t (message "default")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (let-it 5 (+ 3 it))

;; (let ((x 2))
;;   (cond-let2
;;     (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
;;     ((the-string   (some 'string  x)) (concat "hello " the-string))
;;     ((the-bindings
;;        (and (consp x)
;;          (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
;;       (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
;;     (:otherwise "foo")))


;; (setq x 7)
;; (cond-let2
;;   (((the-integer (some 'integer x)) (_ (> the-integer 5))) (* 2 the-integer))
;;   ((the-string   (some 'string  x)) (concat "hello " the-string))
;;   ((the-bindings
;;      (and (consp x)
;;        (dm:match '(foo ,(bar symbolp (not (null bar))) (_ ,@bazes) ... . ,needle) x)))
;;     (let-alist the-bindings `(,.bar ,.needle ,@(nreverse .bazes))))
;;   (:otherwise "foo"))

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

;; (macroexp-if :otherwise "foo" nil)

;; (defun when* (test &rest body)
;;   (cond
;;     ((or (eq t test) (and (atom test) (not (symbolp test)))) (macroexp-progn body))
;;     (t `(when* ,test ,@body))))

;; (defun if* (test then &rest else)
;;   (cond
;;     ((null else) (macroexp-when test then))
;;     ((or (eq t test) (and (atom test) (not (symbolp test)))) then)
;;     (t `(if ,test ,then ,@else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if* (test then &rest else)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `if'."
  (cond
    ((null test) (macroexp-progn else))
    ((or (eq t test) (and (atom test) (not (symbolp test)))) then)
    (t `(if ,test ,then ,@else))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when* (test &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `when'."
  `(if* ,test ,(macroexp-progn body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro unless* (test &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `unless'."
  `(if* ,test nil ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((x 7))
  (confirm that (when* x "foo") returns "foo")             ; (if* x "foo") ; (if x "foo")
  (confirm that (when* nil "foo") returns nil)             ; (if* nil "foo") ; nil
  (confirm that (when* x "foo") returns "foo")             ; (if* x "foo") ; (if x "foo")
  (confirm that (when* t "foo") returns "foo")             ; (if* t "foo") ; "foo"
  (confirm that (when* t "foo" "bar") returns "bar")       ; (if* t (progn "foo" "bar")) ; (progn "foo" "bar")
  (confirm that (unless* x "foo") returns nil)             ; (if* x nil "foo")
  (confirm that (unless* nil "foo") returns "foo")         ; (if* nil nil "foo") ; "foo"
  (confirm that (unless* x "foo") returns nil)             ; (if* x nil "foo") ; (if x nil "foo")
  (confirm that (unless* t "foo") returns nil)             ; (if* t nil "foo") ; nil
  (confirm that (unless* t "foo" "bar") returns nil)       ; (if* t nil "foo" "bar") ; nil
  (confirm that (if* t "foo") returns "foo")               ; (when* t "foo") ; (if* t "foo") ; "foo"
  (confirm that (if* x "foo" "bar" "baz") returns "foo")   ; (if x "foo" "bar" "baz")
  (confirm that (if* t "foo" "bar" "baz") returns "foo")   ; "foo" 
  (confirm that (if* nil "foo" "bar" "baz") returns "baz") ; (progn "bar" "baz")
  ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmacro cond2 (&rest clauses)
  "Re-implementation of ordinary `cond'."
  (when* clauses
    (if* (cdar clauses)
      `(if* ,(caar clauses) ,(macroexp-progn (cdar clauses)) (cond2 ,@(cdr clauses)))
      `(or ,(caar clauses) (cond2 ,@(cdr clauses))))))


;; Example usage:
(if
  (= 2 1)
  (message "1")
  (or 32
    (if
      (= 3 3)
      (message "2")
      (message "default"))))




;; Example usage:
(cond2
  ((= 2 1) (message "1"))
  (32)
  ((= 3 3) (message "2"))
  (t (message "default")))

(cond2
  ((= 2 1) (message "1"))
  ((= 3 3)) ; (message "2")
  (t (message "default")))

(cond2
  ;; ((= 2 1) (message "1"))
  ;; (32)
  ;; ((= 3 3) (message "2"))
  (t (message "default")))


(setq x 1 y 2 z 3)

(defmacro let-it (expr &rest body)
  `(let ((it ,expr)) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun macroexp-or-args (args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Helper function for `or*'."
  (when  args
    (if (let-it (car args) (or (eq t it) (and (atom it) (not (symbolp it)))))
      (list (car args))
      (cons (car args) (macroexp-or-args (cdr args))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro or* (&rest things)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Constant folded `or'."
  `(or ,@(macroexp-or-args things)))


(macroexp-or-args '(x y 32 z))

(or* x y 32 z)










