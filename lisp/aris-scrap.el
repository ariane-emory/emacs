;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-fib-benchmarks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def* (div-mod (n : integer) (d : integer)) => (pair-of integer)
  `(,(/ n d) . ,(% n d)))

;; ... expands to:
(defun* div-mod ((n : integer) (d : integer)) => (pair-of integer)
  `(,(/ n d) \,(% n d)))

(div-mod 19 8) ;; => (2 . 3)
(div-mod -19 8) ;; => (-2 . -3)
(div-mod 19 -8) ;; => (-2 . 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  type both argument and return::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo ((n : positive-integer)) => positive-integer
  (* n n))

(defun foo (n)
  (cl-check-type n positive-integer)
  (let ((foo-return-1126 (* n n)))
    (unless (cl-typep foo-return-1126 'positive-integer)
      (signal 'wrong-type-return (list 'positive-integer foo-return-1126)))
    foo-return-1126))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only type return:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo (n) => positive-integer
  (* n n))

(defun foo (n)
  (let ((foo-return-1127 (* n n)))
    (unless (cl-typep foo-return-1127 'positive-integer)
      (signal 'wrong-type-return (list 'positive-integer foo-return-1127)))
    foo-return-1127))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only type argument:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo ((n : positive-integer))
  (* n n))

(defun foo (n)
  (cl-check-type n positive-integer)
  (* n n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type neither:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* foo (n)
  (* n n))

(defun foo (n)
  (* n n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (input &optional accumulator)
  "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) dl))) => (a b c d).
This is from Norvig."
  (prn "(flatten %s %s)" input accumulator)
  (let ((result
          (with-indentation
            (cond
              ((null input) accumulator)
              ((atom input) (cons input accumulator))
              (t (flatten
                   (first input)
                   (flatten (rest input) accumulator)))))))
    (prn "⇒ %s" result)
    result))

(flatten '(this (is a) (list (with lots) (of (nested stuff)))))

(flatten '(one (two three)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-db (db-sym &optional db-prop)
  (let ( (db-prop (or db-prop 'db))
         (table (make-hash-table :test #'equal)))
    (setf (get db-sym db-prop) table)))
(create-db 'foo)
(create-db 'foo 'alt)
(get 'foo 'db)

(defun db-get (db-sym key &optional db-prop)
  (with-gensyms (db-not-found)
    (let* ( (db-prop (or db-prop 'db))
            (db (get db-sym db-prop))
            (got (gethash key db db-not-found))
            (found (not (eq got db-not-found)))
            (val (when found got)))
      (cons found val))))
(db-get 'foo 'bar)
(db-get 'foo 'bar 'alt)

(defun db-put (db-sym key val &optional db-prop)
  (let* ( (db-prop (or db-prop 'db))
          (db (get db-sym db-prop)))
    (setf (gethash key db) val)))
(db-put 'foo 'bar 777)
(db-put 'foo 'bar 888 'alt)

(defun db-clear (db-sym &optional db-prop)
  (let* ( (db-prop (or db-prop 'db))
          (db (get db-sym db-prop)))
    (clrhash db)))
(db-clear 'foo)
(db-clear 'foo 'alt)


(let* ((arglist '(x y &optional z)) (pos arglist))
  (while pos
    (prn "%s %s" (car pos) (cadr pos))
    (pop pos)))

(let* ((arglist '(x y &optional z)) (pos arglist) tail)
  (while pos
    (prn "%s %s" (car pos) (cadr pos))
    (when (eq (cadr pos) '&optional)
      (setq tail (cadr pos))
      (setcdr pos nil))
    (pop pos))
  (prn "arglist %s" arglist)
  (prn "tail %s" tail))

(let* ((arglist '(x y &optional z)) 
        (pos arglist) 
        tail)
  (dolist (arg arglist)
    (when (eq arg '&optional)
      (setq tail (cdr pos))
      (setcdr pos nil))
    (setq pos (cdr pos)))
  (prn "arglist %s" arglist)
  (prn "tail %s" tail))


(let* ((arglist '(x y &optional z)) (pos arglist) tail)
  (while pos
    (when (eq (second pos) '&optional)
      (setq tail (cddr pos))
      (setcdr pos nil))
    (pop pos))
  (prn "arglist %s" arglist)
  (prn "tail %s" tail))
