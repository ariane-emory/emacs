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

;; ;; (cond-let
;; ;;   (((the-integer (some integer x))) (* 2 the-integer))
;; ;;   (((the-string  (some string  x))) (concat "hello " the-string))
;; ;;   (t "no integer or string"))


;; (internal--build-bindings '((x 1) (y 2)))

;; ;; ((x (and t 1)) (y (and x 2)))
;; (if-let ((the-integer (some integer x)))
;;   (progn
;;     (prn "int case")
;;     (* 2 the-integer))
;;   (if-let ((the-string (some string  x)))
;;     (progn
;;       (prn "string case")
;;       (concat "hello " the-string))
;;     (progn
;;       (prn "t case")
;;       "no integer or string")))


;; (defmacro cond-let (&rest clauses)
;;   ;; (debug nil clauses)
;;   (prn "clauses: %S" clauses)
;;   (cond
;;     ((null clauses) nil)
;;     ((eq (caar clauses) t) (cadar clauses))
;;     (t `(if-let ,(caar clauses)
;; 	        (progn ,@(cdar clauses))
;; 	        (cond-let ,@(cdr clauses))))))

;; (defmacro cond-let (&rest clauses)
;;   ;; (debug nil clauses)
;;   (prn "clauses: %S" clauses)
;;   (cond
;;     ((null clauses) nil)
;;     ((eq (caar clauses) t)
;;       (macroexp-progn `,(cdar clauses)))
;;     (t `(if-let ,(caar clauses)
;;           ,(macroexp-progn (cdar clauses)) ; (progn ,@(cdar clauses))
;; 	        (cond-let ,@(cdr clauses))))))

;; (defmacro cond-let (&rest clauses)
;;   (cond
;;     ((null clauses) nil)
;;     ((eq (caar clauses) t)
;;       (macroexp-progn (cdar clauses)))
;;     ((cdr clauses)
;;       `(if-let ,(caar clauses)
;;          ,(macroexp-progn (cdar clauses))
;; 	       (cond-let ,@(cdr clauses))))))

;; (setq x 9)
;; (setq x nil)
;; (setq x "world")

;; (cond-let
;;   (((the-integer (some integer x))) (prn "int case") (* 2 the-integer))
;;   (((the-string  (some string  x))) (prn "string case") (concat "hello " the-string))
;;   (t (prn "t case") "no integer or string"))

;; (if-let ((the-integer (some integer x)))
;;   (progn
;;     (prn "int case")
;;     (* 2 the-integer))
;;   (if-let
;;     ((the-string (some string x)))
;;     (progn
;;       (prn "string case")
;;       (concat "hello " the-string))
;;     (progn
;;       (prn "t case")
;;       "no integer or string")))

;; (cond-let
;;   (((the-integer (some integer x))) (* 2 the-integer))
;;   (((the-string  (some string  x))) (concat "hello " the-string))
;;   (t "no integer or string"))

;; (if-let ((the-integer (some integer x)))
;;   (* 2 the-integer)
;;   (if-let
;;     ((the-string (some string x)))
;;     (concat "hello " the-string)
;;     "no integer or string"))


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

(setq x '(foo quux (zot shprungy qwib poof)))

(cond-let
  (((the-integer (some integer x)) (_ (> the-integer 5))) (* 2 the-integer))
  ((the-string  (some string  x))  (concat "hello " the-string))
  ((bindings (dm:match '(foo ,bar (_ ,@bazes)) x)) (let-alist bindings (cons .bar .bazes)))
  (otherwise "no matching clause")) ;; (quux shprungy qwib poof)

;; expands into:
(if-let
  ((the-integer (some integer x)) (_ (> the-integer 5)))
  (* 2 the-integer)
  (if-let (the-string (some string x))
    (concat "hello " the-string)
    "no integer > 5 or string"))

