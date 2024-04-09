;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General purpose stack-manipulation functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'cl-lib)
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --dostack-validate-spec (spec)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (unless (cons? spec)
    (signal 'wrong-type-argument (list 'cons? spec)))
  (unless (= 2 (length spec))
    (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec)))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dostack (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Iterate through a stack, executing the body of code for each element in the
stack in a scope where STACK is bound to the remaining stack items and the
followingstack operators are defined: `push!', `pop!', `swap!', `dup!', `rotl!',
`rotr!', `over!', `stack-len'."
  (--dostack-validate-spec spec)
  (let* ( (val-sym (car spec))
          (stack (nth 1 spec))
          (return-label `',(gensym "return-"))
          (stack-is-sym (symbolp stack))
          (stack-sym (if stack-is-sym stack (gensym "stack-")))
          (varlist (list (unless stack-is-sym `((,stack-sym ,stack))))))
    `(catch ,return-label
       (let ,@varlist
         (cl-labels ( (--require-len>= (len)
                        (unless (length> ,stack-sym (1- len))
                          (signal 'stack-underflow (list ',stack-sym))))
                      ;; (--update-binding ()
                      ;;   (setq stack ,stack-sym)
                      ;;   nil)
                      (dup! ()
                        (--require-len>= 1)
                        (let ((val (pop!)))
                          (push! val)
                          (push! val)
                          ;; (--update-binding)
                          ))
                      (over! ()
                        (--require-len>= 2)
                        (let* ( (top  (pop!))
                                (next (pop!)))
                          (push! next)
                          (push! top)
                          (push! next)
                          ;; (--update-binding)
                          ))
                      (len ()
                        (length ,stack-sym))
                      (pop! ()
                        (prog1
                          (pop ,stack-sym)
                          ;; (--update-binding)
                          ))
                      (push! (&optional val)
                        (push (or val ,val-sym) ,stack-sym)
                        ;; (--update-binding)
                        )
                      (return! (&optional val)
                        (throw ,return-label (or val ,val-sym)))
                      (stop! ()
                        (throw ,return-label ,stack-sym))
                      (rotl! ()
                        (--require-len>= 3)
                        (let* ( (top  (pop!))
                                (next (pop!))
                                (far  (pop!)))
                          (push! top)
                          (push! far)
                          (push! next)
                          ;; (--update-binding)
                          ))
                      (rotr! ()
                        (--require-len>= 3)
                        (let* ( (top  (pop!))
                                (next (pop!))
                                (far  (pop!)))
                          (push! next)
                          (push! top)
                          (push! far)
                          ;; (--update-binding)
                          ))
                      (stack ()
                        ,stack-sym)
                      (set-stack! (new-stack)
                        (setq ,stack-sym new-stack))
                      (push-back! (value)
                        (set-stack! (nconc ,stack-sym (list value))))
                      (swap! ()
                        (--require-len>= 2)
                        (let* ( (top  (pop!))
                                (next (pop!)))
                          (push! top)
                          (push! next)
                          ;; (--update-binding)
                          )))
           (while ,stack-sym
             (let ((,val-sym (pop!)))
               (prndiv)
               (prn "dostack: %S" ,val-sym)
               ,@body)))
         ;; Return whatever part of the stack remains;
         ;; (prn "Remaining: %S" ,stack-sym)
         ,stack-sym))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doforth (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A dumb little Forth-like stack machine without enough operations to be very useful,
meant mainly for use in dostack's unit tests."
  ;; (let (out)
  (--dostack-validate-spec spec)
  (let* ( (val-sym   (car spec))
          (out-sym   (gensym "out-"))
          ;;(out-sym   (unless body      (gensym "out-")))
          (body      (or     body     `((setq ,out-sym (cons ,(car spec) ,out-sym)))))
          ;;(body      (append body `((setq ,out-sym (cons ,(car spec) ,out-sym)))))
          ;;(tail-expr (and    out-sym  `((reverse ,out-sym))))
          ;;(tail-expr                  `((reverse ,out-sym)))
          ;; (varlist   (when   out-sym  `(,out-sym)))
          )
    `(let (,out-sym) ;; (,@varlist)
       (dostack ,spec
         (prn "doforth: %S with %S ahead." ,val-sym (stack))
         (cond
           ((eq? :dup    ,val-sym) (dup!))
           ((eq? :drop   ,val-sym) (pop!))
           ((eq? :over   ,val-sym) (over!))
           ((eq? :return ,val-sym) (return!))
           ((eq? :rotl   ,val-sym) (rotl!))
           ((eq? :rotr   ,val-sym) (rotr!))
           ((eq? :swap   ,val-sym) (swap!))
           ((eq? :stop   ,val-sym) (stop!))
           (t ,@body))
         (prn "after: %S" (stack)))
       ;; ,@tail-expr
       (reverse ,out-sym))))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dostack--run-tests ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Run the unit tests for the `dostack' function."
  (confirm that (doforth (_ '(:drop 3 2 1))) returns (2 1))
  (confirm that (doforth (_ '(3 :drop 2 1))) returns (3 1))
  (confirm that (doforth (_ '(3 2 :drop 1))) returns (3 2))

  (confirm that (doforth (_ '(:dup 3 2 1))) returns (3 3 2 1))
  (confirm that (doforth (_ '(3 :dup 2 1))) returns (3 2 2 1))
  (confirm that (doforth (_ '(3 2 :dup 1))) returns (3 2 1 1))

  (confirm that (doforth (_ '(:over 3 2 1))) returns (2 3 2 1))
  (confirm that (doforth (_ '(:over 3 2 1 :over 5 4))) returns (2 3 2 1 4 5 4))

  (confirm that (doforth (_ '(:rotl 4 3 2 1))) returns (3 2 4 1))
  (confirm that (doforth (_ '(4 :rotl 3 2 1))) returns (4 2 1 3))

  (confirm that (doforth (_ '(:rotr 4 3 2 1))) returns (2 4 3 1))
  (confirm that (doforth (_ '(4 :rotr 3 2 1))) returns (4 1 3 2))

  (confirm that (doforth (_ '(:swap 3 2 1))) returns (2 3 1))
  (confirm that (doforth (_ '(3 :swap 2 1))) returns (3 1 2))

  (confirm that
    (doforth (_ '(:over 1 :rotl 2 3 4 :drop 100 5 :swap 9 :rotr 8 10 :dup twice))) 
    returns (1 3 2 4 5 10 9 8 twice twice))

  (confirm that (doforth (_ '(9 :dup 8 :swap 7 :drop 6 :over 5 :rotl 4 :rotr 3 2 1))) 
    returns (9 8 8 6 5 2 4 3 1))

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
