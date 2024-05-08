;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(cl-macrolet ((foo () (prn "Foo!")))
  (defun bar ()
    (foo)))

(bar)

(let ((x 333)) (eval x))





;; => ((1.23023 0 0.0)
;;     (1.642874 0 0.0)
;;     (1.463898 0 0.0)
;;     (0.477198 0 0.0))



(let ((reps 10000000))
  (list
    (benchmark-run reps (* (% (random) 32) (% (random) 32)))
    (benchmark-run reps (eval (* (% (random) 32) (% (random) 32))))
    (let* ((n 32) (fun (eval `(lambda () (* (% (random) ,n) (% (random) ,n)))))) (benchmark-run reps (funcall fun)))
    (let* ((n 32) (expr `(progn (* (% (random) ,n) (% (random) ,n))))) (benchmark-run reps (eval expr)))
    (let* ((n 32) (expr `(* (% (random) ,n) (% (random) ,n)))) (benchmark-run reps (eval expr)))
    (let* ((n 32) (expr `(let ((n ,n)) (* (% (random) n) (% (random) n))))) (benchmark-run reps (eval expr)))
    (let* ((n 32) (expr (byte-compile `(* (% (random) ,n) (% (random) ,n))))) (benchmark-run reps (eval expr)))
    (let* ((n 32) (fun (byte-compile `(lambda () (* (% (random) ,n) (% (random) ,n)))))) (benchmark-run reps (funcall fun)))
    (let* ((n 32) (fun (byte-compile `(lambda (n) (* (% (random) ,n) (% (random) ,n)))))) (benchmark-run reps (funcall fun n)))
    ))

((1.218481 0 0.0)
  (1.629738 0 0.0)
  (1.4719 0 0.0)
  (1.777008 0 0.0)
  (1.6759249999999999 0 0.0)
  (2.05312 0 0.0)
  (5.680541 64 3.505743999999993)
  (0.73609 0 0.0)
  (0.8553850000000001 0 0.0))


;;  => ( (1.221757 0 0.0)
;;       (1.628193 0 0.0)
;;       (1.4766890000000001 0 0.0)
;;       (1.783931 0 0.0)
;;       (1.6634120000000001 0 0.0)
;;       (2.056215 0 0.0)
;;       (5.615627 62 3.4255270000000166)
;;       (0.733064 0 0.0))




(defmacro doconses (spec &rest body)
  "Loop over a list's heads
Evaluate BODY with VAR bound to each cons from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

 (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 2 (length spec) 3)
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  (let ((tail (make-symbol "tail")))
    `(let ((,tail ,(nth 1 spec)))
       (while ,tail
         (let ( (car (car ,tail))
                (,(car spec) ,tail))
           ,@body
           (setq ,tail (cdr ,tail))))
       ,@(cdr (cdr spec)))))

(setf ls '(1 2 3 4 5))
(doconses (l ls ls)
  (setcar l (* 2 car))
  (prn "-> %s" l))


(cl-mapl (lambda (x) (setcar x (* 2 (car x)))) ls)
