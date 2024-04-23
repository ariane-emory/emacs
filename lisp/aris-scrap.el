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


(val (n:integer 666))
(methods (n:integer 666))

(responds-to? (n:integer 666) 'rem)

(is? (n:integer 666) 'n:integer)


(defun nmapcar (fun lst)
  "A 'non-consing', destructive version of `mapcar'."
  (let ((tail lst))
    (while tail
      (setcar tail (funcall fun (car tail)))
      (setq tail (cdr tail))))
  lst)


(benchmark-run 10000
  (let ((lst '(1 2 3 4 5 6 7 8 9 10)))
    (nmapcar #'double lst)
    lst)) ;; => (0.038106999999999995 0 0.0)

(benchmark-run 10000
  (let ((lst '(1 2 3 4 5 6 7 8 9 10)))
    (mapcar #'double lst))) ;; => (0.008199000000000001 0 0.0)

