;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((seed (car (current-time))))
  (defun myrand (limit)
    "Not a very good random number generator."
    (lsh (1- (% (setq seed (logand #xffff
                             ((lambda (z) (if (positive? z) z (+ z -32768)))
                               (* seed 899.))))
               (lsh limit 1)))
      -1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (let ((limit 10)
         (counts '( (0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0)
                    (5 . 0) (6 . 0) (7 . 0) (8 . 0) (9 . 0))))
    (dotimes (x 2000000)
      (let ((roll (myrand limit)))
        (alist-put! roll counts (1+ (alist-get roll counts 0)))))
    (dolist (count counts)
      (prn "%s" count))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(n:defclass n:integer (value) nil
  (val ()      value)
  (fmt ()      (format "(n:integer %d)" value))
  (add (other) (n:integer (+    value (val other))))
  (sub (other) (n:integer (-    value (val other))))
  (mul (other) (n:integer (*    value (val other))))
  (div (other) (n:integer (/    value (val other))))
  (rem (other) (n:integer (%    value (val other))))
  (pow (other) (n:integer (expt value (val other)))))

(let ( (class-name   'n:integer)
       (method-names '(add class-name dir div fmt is? mul pow rem responds-to? sub val)))
  (mapc #'n:ensure-generic-fun method-names)
  (cl-defun n:integer (value)
    #'(lambda (message)
        (declare (norvig-object-class 'n:integer))
        (cl-case message
          (class-name #'(lambda nil class-name))
          (is? #'(lambda (class) (eq class class-name)))
          (dir #'(lambda nil method-names))
          (responds-to? #'(lambda (method) (not (null (memq method method-names)))))
          (val #'(lambda nil value))
          (fmt #'(lambda nil (format "(n:integer %d)" value)))
          (add #'(lambda (other) (n:integer (+ value (val other)))))
          (sub #'(lambda (other) (n:integer (- value (val other)))))
          (mul #'(lambda (other) (n:integer (* value (val other)))))
          (div #'(lambda (other) (n:integer (/ value (val other)))))
          (rem #'(lambda (other) (n:integer (% value (val other)))))
          (pow #'(lambda (other) (n:integer (expt value (val other)))))))))

(let ( (class-name 'n:integer)
       (method-names '(add class-name dir div fmt is? mul pow rem responds-to? sub val))
       (foo 77))
  (mapc #'n:ensure-generic-fun method-names)
  (cl-defun n:integer (value)
    #'(lambda (message)
        (declare (norvig-object-class 'n:integer))
        (cl-case message
          (class-name #'(lambda nil class-name))
          (is? #'(lambda (class) (eq class class-name)))
          (dir #'(lambda nil method-names))
          (responds-to? #'(lambda (method) (not (null (memq method method-names)))))
          (val #'(lambda nil value))
          (fmt #'(lambda nil (format "(n:integer %d)" value)))
          (add #'(lambda (other) (n:integer (+ value (val other)))))
          (sub #'(lambda (other) (n:integer (- value (val other)))))
          (mul #'(lambda (other) (n:integer (* value (val other)))))
          (div #'(lambda (other) (n:integer (/ value (val other)))))
          (rem #'(lambda (other) (n:integer (% value (val other)))))
          (pow #'(lambda (other) (n:integer (expt value (val other)))))))))





(val (n:integer 666))
(dir (n:integer 666))

(responds-to? (n:integer 666) 'rem)
(responds-to? (n:integer 666) 'foo)

(is? (n:integer 666) 'n:integer)
(is? (n:integer 666) 'nope)

(setq q (n:integer 444))
(setq r (lambda (foo) :foo))

(eq (car-safe q) 'closure)

(n:is-object? q)
(n:is-object? r)
(n:is-object? nil)

(n:is? 2 'n:integer)
(n:is? q 'n:integer)
(n:is? r 'n:integer)

