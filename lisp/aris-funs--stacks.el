;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General purpose stack-manipulation functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'aris-funs--aliases)
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
  (let ( (val-sym (car spec))
         (stack-sym (gensym "stack-")))
    `(let ((,stack-sym ,(nth 1 spec)))
       (cl-labels (
                    (stack-len () (length ,stack-sym))
                    (push! (val) (push val ,stack-sym))
                    (pop! () (pop ,stack-sym))
                    (swap! ()
                      (let* ( (top  (pop!))
                              (next (pop!)))
                        (push! top)
                        (push! next)))
                    (dup! ()
                      (let ((val (pop!)))
                        (push! val)
                        (push! val)))
                    (rotl! ()
                      (let* ( (top  (pop!))
                              (next (pop!))
                              (far  (pop!)))
                        (push! top)
                        (push! far)
                        (push! next)))
                    (rotr! ()
                      (let* ( (top  (pop!))
                              (next (pop!))
                              (far  (pop!)))
                        (push! next)
                        (push! top)
                        (push! far)))
                    (over! ()
                      (let* ( (top  (pop!))
                              (next (pop!)))
                        (push! next)
                        (push! top)
                        (push! next))))
         (while ,stack-sym
           (let* ( (stack ,stack-sym)
                   (,val-sym (pop ,stack-sym)))
             ,@body))))))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun stackmapc (fun stack)
  "Map FUN over the elements of STACK using a stack-based approach, discarding the
rresults."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (stackmaprc stack fun))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro stackmaprc (stack fun)
  "Map FUN over the elements of STACK using a stack-based approach, discarding the
rresults with a reversed parameter order compared to `stackmapc'."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(let ((stk ,stack))
     (cl-labels ( (pop! () (pop stk))
                  (push! (val) (push val stk))
                  (swap! ()
                    (let* ( (top (pop!))
                            (next (pop!)))
                      (push! top)
                      (push! next)))
                  (dup! ()
                    (let ((val (pop!)))
                      (push! val)
                      (push! val)))
                  (rotl! ()
                    (let* ( (top  (pop!))
                            (next (pop!))
                            (far  (pop!)))
                      (push! top)
                      (push! far)
                      (push! next)))
                  (rotr! ()
                    (let* ( (top  (pop!))
                            (next (pop!))
                            (far  (pop!)))
                      (push! next)
                      (push! top)
                      (push! far)))
                  (over! ()
                    (let* ( (top  (pop!))
                            (next (pop!)))
                      (push! next)
                      (push! top)
                      (push! next))))
       (while stk
         (funcall ,fun (pop!)))))
  ;; `(let ((stk ,stack))
  ;;    (cl-flet ((pop! () (pop stk)))
  ;;      (while stk
  ;;        (funcall ,fun (pop!)))))
  )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--stacks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
