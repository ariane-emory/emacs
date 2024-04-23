;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((seed (car (current-time))))
  (defun myrand (limit)
    (% (setq seed (logand #xffff
                    ((lambda (z) (if (positive? z) z (+ z -32768)))
                      (* seed 899.))))
      limit)))

(let ((seed (car (current-time))))
  (defun myrand (limit)
    (lsh (1- (% (setq seed (logand #xffff
                             ((lambda (z) (if (positive? z) z (+ z -32768)))
                               (* seed 899.))))
               (lsh limit 1)))
      -1)))

(let ((limit 10)
       (counts '( (0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0)
                  (5 . 0) (6 . 0) (7 . 0) (8 . 0) (9 . 0))))
  (dotimes (x 1000000)
    (let ((roll (myrand limit)))
      (alist-put! roll counts (1+ (alist-get roll counts 0)))))
  (prn "Counts: %s" counts))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *xorshift64-seed* (now-us))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xorshift64 ()
  "Generate a pseudo-random positive integer."
  (when (zero? *xorshift64-seed*) (setq *xorshift64-seed* (now-us)))  
  (setq
    *xorshift64-seed* (^ *xorshift64-seed* (<< *xorshift64-seed* 13))
    *xorshift64-seed* (^ *xorshift64-seed* (>> *xorshift64-seed* 7))
    *xorshift64-seed* (^ *xorshift64-seed* (<< *xorshift64-seed* 17)))
  (abs *xorshift64-seed*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun random args
  "Return a psuedo-random integer between MIN and MAX inclusive."
  (unless (or (nil? args) (nil? (cddr args)))
    (error "random takes either 0, 1 or 2 arguments"))
  (unless (or (nil? (car args)) (integer? (car args)))
    (error "If provided, first argument must be an integer"))
  (unless (or (nil? (cdr args)) (integer? (cadr args)))
    (error "If provided, second argument must be an integer"))
  (let ((randval (xorshift64)))
    (if args
      (let* ((arg1 (if (cadr args) (car  args) 0))
              (arg2 (if (cadr args) (cadr args) (car args)))
              (min (min arg1 arg2))
              (max (max arg1 arg2))
              (range (1+ (- max min))))
        (+ min (mod (xorshift64) range)))
      randval)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'random)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
