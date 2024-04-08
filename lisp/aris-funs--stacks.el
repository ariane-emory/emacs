;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General purpose stack-manipulation functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
(require 'aris-funs--with-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dostack (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Iterate through a stack, executing the body of code for each element in the
stack in a scope where STACK is bound to the remaining stack items and the following
stack operators are defined: `push!', `pop!', `swap!', `dup!', `rotl!', `rotr!',
`over!', `stack-len'."
  (unless (cons? spec)
    (signal 'wrong-type-argument (list 'cons? spec)))
  (unless (= 2 (length spec))
    (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  (let* ( (val-sym (car spec))
          (stack (nth 1 spec))
          (stack-sym (gensym "stack-"))
          ;;(stack-sym (if (symbolp stack) stack (gensym "stack-")))
          )
    `(let ((,stack-sym ,stack))
       (cl-labels (
                    (update-binding ()
                      (setq stack ,stack-sym)
                      nil)
                    (stack-len () (length ,stack-sym))
                    (push! (val)
                      (push val ,stack-sym)
                      (update-binding))
                    (--dostack-require-len>= (len)
                      (unless (length> ,stack-sym (1- len))
                        (signal 'stack-underflow (list ',stack-sym))))
                    (pop! ()
                      (prog1
                        (pop ,stack-sym)
                        (update-binding)))
                    (swap! ()
                      (--dostack-require-len>= 2)
                      (let* ( (top  (pop!))
                              (next (pop!)))
                        (push! top)
                        (push! next)
                        (update-binding)))
                    (dup! ()
                      (--dostack-require-len>= 1)
                      (let ((val (pop!)))
                        (push! val)
                        (push! val)
                        (update-binding)))
                    (rotl! ()
                      (--dostack-require-len>= 3)
                      (let* ( (top  (pop!))
                              (next (pop!))
                              (far  (pop!)))
                        (push! top)
                        (push! far)
                        (push! next)
                        (update-binding)))
                    (rotr! ()
                      (--dostack-require-len>= 3)
                      (let* ( (top  (pop!))
                              (next (pop!))
                              (far  (pop!)))
                        (push! next)
                        (push! top)
                        (push! far)
                        (update-binding)))
                    (over! ()
                      (--dostack-require-len>= 2)
                      (let* ( (top  (pop!))
                              (next (pop!)))
                        (push! next)
                        (push! top)
                        (push! next)
                        (update-binding))))
         (while ,stack-sym
           (let* ( (,val-sym (pop ,stack-sym))
                   (stack ,stack-sym))
             ;;(prn "PROCESS %s" ,val-sym)
             ,@body))
         ;;(prn "FINAL   %s" ,stack-sym)
         ))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --dostack-mini-forth (stack)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A dumb little Forth-like stack machine without enough operations to be very useful,
meant mainly for use in unit tests."
  (let (out)
    (dostack (x stack)
      ;;(prn (make-string 80 ?\=))
      ;;(prn "Processing command: %S" x)
      ;;(prn "Items remaining:    %S" (stack-len))
      ;;(prn "Stack remaining:    %S" stack)
      (cond
        ((eq :drop x) (pop!))
        ((eq :dup x)  (dup!))
        ((eq :over x) (over!))
        ((eq :rotl x) (rotl!))
        ((eq :rotr x) (rotr!))
        ((eq :swap x) (swap!))
        (t (setq out (cons x out)))))
    ;; (prn "Out: %S" out)
    out))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dostack--run-tests ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Run the unit tests for the `dostack' function."
  (confirm that (--dostack-mini-forth '(:drop 3 2 1)) returns (1 2))
  (confirm that (--dostack-mini-forth '(3 :drop 2 1)) returns (1 3))
  (confirm that (--dostack-mini-forth '(3 2 :drop 1)) returns (2 3))

  (confirm that (--dostack-mini-forth '(:dup 3 2 1)) returns (1 2 3 3))
  (confirm that (--dostack-mini-forth '(3 :dup 2 1)) returns (1 2 2 3))
  (confirm that (--dostack-mini-forth '(3 2 :dup 1)) returns (1 1 2 3))

  (confirm that (--dostack-mini-forth '(:over 3 2 1)) returns (1 2 3 2))
  (confirm that (--dostack-mini-forth '(:over 3 2 1 :over 5 4)) returns (4 5 4 1 2 3 2))

  (confirm that (--dostack-mini-forth '(:rotl 4 3 2 1)) returns (1 4 2 3))
  (confirm that (--dostack-mini-forth '(4 :rotl 3 2 1)) returns (3 1 2 4))

  (confirm that (--dostack-mini-forth '(:rotr 4 3 2 1)) returns (1 3 4 2))
  (confirm that (--dostack-mini-forth '(4 :rotr 3 2 1)) returns (2 3 1 4))

  (confirm that (--dostack-mini-forth '(:swap 3 2 1)) returns (1 3 2))
  (confirm that (--dostack-mini-forth '(3 :swap 2 1)) returns (2 1 3))

  (confirm that
    (--dostack-mini-forth '(:over 1 :rotl 2 3 4 :drop 100 5 :swap 9 :rotr 8 10 :dup twice))
    returns (twice twice 8 9 10 5 4 2 3 1))

  (confirm that (--dostack-mini-forth '(9 :dup 8 :swap 7 :drop 6 :over 5 :rotl 4 :rotr 3 2 1))
    returns (1 3 4 2 5 6 8 8 9))

  (prn "Ran all dostack test cases.")
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dostack--run-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun stackmapc (fun stack)
;;   "Map FUN over the elements of STACK using a stack-based approach, discarding the
;; rresults."
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;   (stackmaprc stack fun))
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro stackmaprc (stack fun)
;;   "Map FUN over the elements of STACK using a stack-based approach, discarding the
;; rresults with a reversed parameter order compared to `stackmapc'."
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   `(let ((stk ,stack))
;;      (cl-labels ( (pop! () (pop stk))
;;                   (push! (val) (push val stk))
;;                   (swap! ()
;;                     (let* ( (top (pop!))
;;                             (next (pop!)))
;;                       (push! top)
;;                       (push! next)))
;;                   (dup! ()
;;                     (let ((val (pop!)))
;;                       (push! val)
;;                       (push! val)))
;;                   (rotl! ()
;;                     (let* ( (top  (pop!))
;;                             (next (pop!))
;;                             (far  (pop!)))
;;                       (push! top)
;;                       (push! far)
;;                       (push! next)))
;;                   (rotr! ()
;;                     (let* ( (top  (pop!))
;;                             (next (pop!))
;;                             (far  (pop!)))
;;                       (push! next)
;;                       (push! top)
;;                       (push! far)))
;;                   (over! ()
;;                     (let* ( (top  (pop!))
;;                             (next (pop!)))
;;                       (push! next)
;;                       (push! top)
;;                       (push! next))))
;;        (while stk
;;          (funcall ,fun (pop!)))))
;;   ;; `(let ((stk ,stack))
;;   ;;    (cl-flet ((pop! () (pop stk)))
;;   ;;      (while stk
;;   ;;        (funcall ,fun (pop!)))))
;;   )
;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--stacks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
