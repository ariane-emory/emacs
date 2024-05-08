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
    ))
;;  => ( (1.221757 0 0.0)
;;       (1.628193 0 0.0)
;;       (1.4766890000000001 0 0.0)
;;       (1.783931 0 0.0)
;;       (1.6634120000000001 0 0.0)
;;       (2.056215 0 0.0)
;;       (5.615627 62 3.4255270000000166)
;;       (0.733064 0 0.0))



