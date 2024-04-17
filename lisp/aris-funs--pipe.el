;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'cl-lib)
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
(require 'aris-funs--unsorted)
(require 'aris-funs--with-messages)
(require 'aris-funs--alists)
(require 'aris-funs--lists)
(require 'aris-funs--stacks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pipe (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming.
(pipe 
  8
  (+ 3 it)
  :(message \"A message! it = %s\" it)
  (* 2 it)
  (- it 1))"
  (let* ( (head-is-spec
            (and
              (cons? head)
              (cons? (car head))
              (length> (car head) 0)
              (length< (car head) 3)))
          (var (if head-is-spec (caar head) *pipe--default-var-sym*))
          (init-form (when head-is-spec (cadar head)))
          (body (if head-is-spec tail (cons head tail))))
    body
    `(let ( (,var ,init-form)
            (ignore-flag nil))
       (maprc ',body
         (lambda (expr)
           (cond
             ((eq : expr)
               (setq ignore-flag t))
             (ignore-flag
               (eval expr)
               (setq ignore-flag nil))
             (t
               (setq ,var (eval expr))
               (setq ignore-flag nil)))))
       ,var)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup pipe nil
  "Elixir-style pipe operator.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *pipe--verbose* nil
  "Whether the pipe operator should print verbose messages."
  :group 'pipe
  :type 'boolean)

(defcustom *pipe--print-fun* 'indented-message
  "The function to use to print messages."
  :group 'pipe
  :type 'function)

(defcustom *pipe--print-divider-fun* 'prndiv
  "The function to use to print dividers."
  :group 'pipe
  :type 'function)

(defcustom *pipe--default-var-sym* '_
  "The default symbol to use for the pipe operator."
  :group 'pipe
  :type 'symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *--pipe--commands*
  '(
     (:       . (1 . :IGNORE))
     (:?      . (1 . :MAYBE))
     (:ignore . (1 . :IGNORE))
     (:maybe  . (1 . :MAYBE))
     (:return . (1 . :RETURN))
     (:unless . (2 . :UNLESS))
     (:when   . (2 . :WHEN))
     )
  "An alist mapping commands to their flags and arities. This is not meant to be customized.")

(defvar *--pipe-flags* (cl-remove-duplicates (map #'cdr (alist-values *--pipe--commands*)))
  "A list of flags that can be set by the pipe operator. This is not meant to be customized.")

(defvar *--pipe-commands-to-flags* (mapr *--pipe--commands* (lambda (x) (cons (car x) (cddr x))))
  "An alist mapping commands to flags. This is not meant to be customized.")

(defvar *--pipe--arity-1-commands*
  (mapr (cl-remove-if (lambda (x) (not (= 1 (cadr x)))) *--pipe--commands*) #'car)
  "Commands that take one argument. This is not meant to be customized.")

(defvar *--pipe--arity-2-commands*
  (mapr (cl-remove-if (lambda (x) (not (= 2 (cadr x)))) *--pipe--commands*) #'car)
  "Commands that take two arguments. This is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe-print (first &rest rest)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pipe--print-fun*"
  `(when *pipe--verbose* (ignore (funcall *pipe--print-fun* ,first ,@rest))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe-prndiv ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pipe--print-divider-fun*"
  `(when *pipe--verbose* (ignore (funcall *pipe--print-divider-fun*))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe-make-args (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ( (head-is-cons (cons? head))
          (car-head (when head-is-cons (car head)))
          (car-head-is-cons (when head-is-cons (cons? car-head)))
          (car-head-length (when car-head-is-cons (length car-head)))
          (head-is-spec
            (and
              car-head-is-cons
              (> car-head-length 0)
              (< car-head-length 3)))
          (head-is-spec-with-init-form (eql car-head-length 2))
          (var (if head-is-spec (car car-head) *pipe--default-var-sym*))
          (body
            (cond
              (head-is-spec-with-init-form (cons (cadr car-head) tail))
              (head-is-spec tail)
              (t (cons head tail))))
          (alist
            `'( ;; (head-is-cons . ,head-is-cons)
                ;; (car-head . ,car-head)
                ;; (car-head-is-cons . ,car-head-is-cons)
                ;; (car-head-length . ,car-head-length)
                ;; (head-is-spec . ,head-is-spec)
                ;; (head-is-spec-with-init-form . ,head-is-spec-with-init-form)
                ;; (head . ,head)
                (var . ,var)
                (body . ,body))))
    alist))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --valid-pipe-flag (kw &optional or-nil)
  (when (not (or (and or-nil (nil? kw)) (memq kw *--pipe-flags*)))
    (error "Invalid pipe flag: %S. Must be one of %S." kw *--pipe-flags*))
  kw)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --is-pipe-command? (kw) 
  (and (keyword? expr) (assoc expr *--pipe-commands-to-flags*)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro |> (head &rest tail)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "`pipe' with optional let-like binding/symbol naming (old-version using dostack)."
;;   (let* ( (args            (eval `(--pipe-make-args ,head ,@tail)))
;;           (return-label `',(gensym "return-"))
;;           (var             (alist-get 'var  args))
;;           (body         `',(alist-get 'body args)))
;;     `(let
;;        ((final
;;           (let ( (,var      nil)
;;                  (var-sym ',var)
;;                  (flag      nil))
;;             (cl-labels ( (flag-is? (test-flag)
;;                            (eq flag (--valid-pipe-flag test-flag)))
;;                          (set-flag! (new-flag &optional force)
;;                            (let ((new-flag (--valid-pipe-flag new-flag t)))
;;                              (cond
;;                                ((and flag new-flag (not force))
;;                                  (error "Cannot set flag to %S when flag is already set to %S."
;;                                    new-flag flag))
;;                                (force
;;                                  (--pipe-print "FORCING FLAG FROM %S TO %S." flag new-flag)
;;                                  (setq flag new-flag))
;;                                (t
;;                                  (--pipe-print "Setting flag from %S to %S%s." flag new-flag
;;                                    (if force " (forced)" ""))))
;;                              (setq flag new-flag)))
;;                          (unset-flag! ()
;;                            (when flag
;;                              (--pipe-print "Unsetting flag %S." flag)
;;                              (set-flag! nil)))
;;                          (store! (value)
;;                            (prog1
;;                              (setq ,var value)
;;                              (--pipe-print "Updated %S to %S." var-sym ,var)))
;;                          (labeled-print (label value)
;;                            (let* ( (label (format "%s:" label))
;;                                    (whites (make-string (- 21 (length label)) ?\ ))
;;                                    (label (concat label whites)))
;;                              (--pipe-print "%s%S" label value))))
;;               (--pipe-prndiv)
;;               (--pipe-print "START")
;;               (--pipe-prndiv)
;;               (catch ,return-label

;;                 ;; BEGINNING OF DOSTACK INVOCATION:
;;                 (dostack-lite (expr ,body)
;;                   (--pipe-prndiv)
;;                   (labeled-print "Current" expr)
;;                   (labeled-print "Remaining" (stack))
;;                   (labeled-print var-sym ,var)
;;                   (labeled-print "Flag" flag)
;;                   (if (--is-pipe-command? expr)
;;                     (set-flag! (alist-get expr *--pipe-commands-to-flags*))
;;                     (let ((result
;;                             (eval (if (fun? expr)
;;                                     (list expr var-sym)
;;                                     (let ((return-label ,return-label))
;;                                       `(cl-flet ((return! (value)
;;                                                    (throw ',return-label value)))
;;                                          ,expr))))))
;;                       (labeled-print "Expr result" result)
;;                       ;; Because drop-next! calls pop, this flet has to be inside of the dostack.
;;                       (cl-flet ((drop-next! () 
;;                                   (let ((next (pop!)))
;;                                     (--pipe-print "Popped 1st %S from %S." next (stack))
;;                                     (when (memq next *--pipe--arity-1-commands*)
;;                                       (let ((popped (pop!)))
;;                                         (--pipe-print "Popped command's argument %S from %S."
;;                                           popped (stack))))
;;                                     (when (memq next *--pipe--arity-2-commands*)
;;                                       (error "Ignoring the %S command is not yet supported." next)))))
;;                         (cond
;;                           ((flag-is? :IGNORE)
;;                             (--pipe-print "Not setting %S because %S." result flag))
;;                           ((flag-is? :WHEN)
;;                             (when (not result) (drop-next!)))
;;                           ((flag-is? :UNLESS)
;;                             (when result (drop-next!)))
;;                           ((flag-is? :RETURN)
;;                             (--pipe-print "Returning due to command: %S" result)
;;                             (throw ,return-label result))
;;                           ((and (flag-is? :MAYBE) result)
;;                             (store! result))
;;                           ((and (flag-is? :MAYBE) (not result))
;;                             (--pipe-print "Ignoring %S." result))
;;                           (t (store! result)))
;;                         (unset-flag!)))))
;;                 ;; END OF DOSTACK BODY ARGUMENT.

;;                 ;; For clarity, explicitly throw the return value if we run out of stack items:
;;                 (throw ,return-label
;;                   (progn
;;                     (--pipe-prndiv)
;;                     (--pipe-print "Returning this because stack is empty: %S" ,var)
;;                     (--pipe-prndiv)
;;                     ,var)))))))
;;        (--pipe-print "Pipe's final return: %S" final)
;;        final)))
;;        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming (alternate version with no inner var-sym and an ugly ,, in the eval)."
  (let* ( (args            (eval `(--pipe-make-args ,head ,@tail)))
          (return-label `',(gensym "return-"))
          (var             (alist-get 'var  args))
          (body         `',(alist-get 'body args))  
          (var-sym      `',var))
    (--pipe-print "return-label is %S" return-label)
    (--pipe-print "args is %S" args)
    `(let ( (,var      nil)
            (body     ,body)
            (flag      nil))
       (cl-labels ( (pop! ()
                      (unless (length> body 0) (signal 'stack-underflow (list 'body)))
                      (pop body))
                    (drop-next! () 
                      (let ((next (pop!)))
                        (--pipe-print "Popped 1st %S from %S." next body)
                        (when (memq next *--pipe--arity-1-commands*)
                          (let ((popped (pop!)))
                            (--pipe-print "Popped command's argument %S from %S."
                              popped body)))
                        (when (memq next *--pipe--arity-2-commands*)
                          (error "Ignoring the %S command is not yet supported." next))))
                    (store! (value)
                      (prog1
                        (setq ,var value)
                        (--pipe-print "Updated %S to %S." ,var-sym ,var)))
                    (flag-is? (test-flag)
                      (eq flag (--valid-pipe-flag test-flag)))
                    (set-flag! (new-flag &optional force)
                      (let ((new-flag (--valid-pipe-flag new-flag t)))
                        (cond
                          ((and flag new-flag (not force))
                            (error "Cannot set flag to %S when flag is already set to %S."
                              new-flag flag))
                          (force
                            (--pipe-print "FORCING FLAG FROM %S TO %S." flag new-flag)
                            (setq flag new-flag))
                          (t
                            (--pipe-print "Setting flag from %S to %S%s." flag new-flag
                              (if force " (forced)" ""))))
                        (setq flag new-flag)))
                    (unset-flag! ()
                      (when flag
                        (--pipe-print "Unsetting flag %S." flag)
                        (set-flag! nil)))
                    (labeled-print (label value)
                      (let* ( (label  (format "%s:" label))
                              (whites (make-string (- 21 (length label)) ?\ ))
                              (label  (concat label whites)))
                        (--pipe-print "%s%S" label value))))
         (--pipe-prndiv)
         (--pipe-print "START")
         (--pipe-prndiv)
         (catch ,return-label                
           (while body
             (let ((expr (pop!)))
               (--pipe-prndiv)
               (labeled-print "Current" expr)
               (labeled-print "Remaining" body)
               (labeled-print ,var-sym ,var)
               (labeled-print "Flag" flag)
               (if (--is-pipe-command? expr)
                 (set-flag! (alist-get expr *--pipe-commands-to-flags*))
                 (let ((result
                         (eval (if (fun? expr)
                                 `(,expr ,,var-sym)
                                 (let ((return-label ,return-label))
                                   `(cl-flet ((return! (value)
                                                (--pipe-print "Throwing %S." ',return-label)
                                                (throw ',return-label value)))
                                      ,expr))))))
                   (labeled-print "Expr result" result)
                   (cond
                     ((flag-is? :IGNORE)
                       (--pipe-print "Not setting %S because %S." result flag))
                     ((flag-is? :WHEN)
                       (when (not result) (drop-next!)))
                     ((flag-is? :UNLESS)
                       (when result (drop-next!)))
                     ((flag-is? :RETURN)
                       (--pipe-print "Returning due to command: %S" result)
                       (throw ,return-label result))
                     ((and (flag-is? :MAYBE) result)
                       (store! result))
                     ((and (flag-is? :MAYBE) (not result))
                       (--pipe-print "Ignoring %S." result))
                     (t (store! result)))
                   (unset-flag!)))))
           ;; For clarity, explicitly throw the return value if we run out of stack items:
           (throw ,return-label
             (progn
               (--pipe-prndiv)
               (--pipe-print "Returning this because stack is empty: %S" ,var)
               (--pipe-prndiv)
               ,var))
           ) ;; END OF CATCH.
         ) ;; END OF CL-LABELS.
       ) ;; END OF LET (AND OF EXPANDED MACRO CONTENT).
    ) ;; END OF LET*. 
  ) ;; END OF DEFMACRO.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming (newer version without dostack)."
  (let* ( (args            (eval `(--pipe-make-args ,head ,@tail)))
          (return-label `',(gensym "return-"))
          (var             (alist-get 'var  args))
          (body         `',(alist-get 'body args)))
    (--pipe-print "return-label is %S" return-label)
    (--pipe-print "args is %S" args)
    `(let ( (,var      nil)
            (var-sym ',var)
            (body     ,body)
            (flag      nil))
       (cl-labels ( (pop! ()
                      (unless (length> body 0) (signal 'stack-underflow (list 'body)))
                      (pop body))
                    (drop-next! () 
                      (let ((next (pop!)))
                        (--pipe-print "Popped 1st %S from %S." next body)
                        (when (memq next *--pipe--arity-1-commands*)
                          (let ((popped (pop!)))
                            (--pipe-print "Popped command's argument %S from %S."
                              popped body)))
                        (when (memq next *--pipe--arity-2-commands*)
                          (error "Ignoring the %S command is not yet supported." next))))
                    (store! (value)
                      (prog1
                        (setq ,var value)
                        (--pipe-print "Updated %S to %S." var-sym ,var)))
                    (flag-is? (test-flag)
                      (eq flag (--valid-pipe-flag test-flag)))
                    (set-flag! (new-flag &optional force)
                      (let ((new-flag (--valid-pipe-flag new-flag t)))
                        (cond
                          ((and flag new-flag (not force))
                            (error "Cannot set flag to %S when flag is already set to %S."
                              new-flag flag))
                          (force
                            (--pipe-print "FORCING FLAG FROM %S TO %S." flag new-flag)
                            (setq flag new-flag))
                          (t
                            (--pipe-print "Setting flag from %S to %S%s." flag new-flag
                              (if force " (forced)" ""))))
                        (setq flag new-flag)))
                    (unset-flag! ()
                      (when flag
                        (--pipe-print "Unsetting flag %S." flag)
                        (set-flag! nil)))
                    (labeled-print (label value)
                      (let* ( (label  (format "%s:" label))
                              (whites (make-string (- 21 (length label)) ?\ ))
                              (label  (concat label whites)))
                        (--pipe-print "%s%S" label value))))
         (--pipe-prndiv)
         (--pipe-print "START")
         (--pipe-prndiv)
         (catch ,return-label                
           (while body
             (let ((expr (pop!)))
               (--pipe-prndiv)
               (labeled-print "Current" expr)
               (labeled-print "Remaining" body)
               (labeled-print var-sym ,var)
               (labeled-print "Flag" flag)
               (if (--is-pipe-command? expr)
                 (set-flag! (alist-get expr *--pipe-commands-to-flags*))
                 (let ((result
                         (eval (if (fun? expr)
                                 `(,expr ,var-sym)
                                 (let ((return-label ,return-label))
                                   `(cl-flet ((return! (value)
                                                (--pipe-print "Throwing %S." ',return-label)
                                                (throw ',return-label value)))
                                      ,expr))))))
                   (labeled-print "Expr result" result)
                   (cond
                     ((flag-is? :IGNORE)
                       (--pipe-print "Not setting %S because %S." result flag))
                     ((flag-is? :WHEN)
                       (when (not result) (drop-next!)))
                     ((flag-is? :UNLESS)
                       (when result (drop-next!)))
                     ((flag-is? :RETURN)
                       (--pipe-print "Returning due to command: %S" result)
                       (throw ,return-label result))
                     ((and (flag-is? :MAYBE) result)
                       (store! result))
                     ((and (flag-is? :MAYBE) (not result))
                       (--pipe-print "Ignoring %S." result))
                     (t (store! result)))
                   (unset-flag!)))))
           ;; For clarity, explicitly throw the return value if we run out of stack items:
           (throw ,return-label
             (progn
               (--pipe-prndiv)
               (--pipe-print "Returning this because stack is empty: %S" ,var)
               (--pipe-prndiv)
               ,var))
           ) ;; END OF CATCH.
         ) ;; END OF CL-LABELS.
       ) ;; END OF LET (AND OF EXPANDED MACRO CONTENT).
    ) ;; END OF LET*. 
  ) ;; END OF DEFMACRO.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIT TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pipe--run-tests ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Run the unit tests for the `pipe' function."
  ;; With no spec:
  (confirm that (|> 1)

    returns 1)
  (confirm that (|> 1 2) returns 2)
  (confirm that (|> 1 2 3) returns 3)
  (confirm that (|> :return 1 2 3) returns 1)
  (confirm that (|> 1 :return 2 3) returns 2)
  (confirm that (|> 1 2 :return 3) returns 3)
  
  (confirm that (|> : 1 2) returns 2)
  (confirm that (|> 1 : 2) returns 1)
  (confirm that (|> :ignore 1 2) returns 2)
  (confirm that (|> 1 :ignore 2) returns 1)
  
  (confirm that (|> (return! 1) 2 3) returns 1)
  (confirm that (|> 1 (return! 2) 3) returns 2)
  (confirm that (|> 1 2 (return! 3)) returns 3)
  
  (confirm that (|> 1 :when odd? 100) returns 100)
  (confirm that (|> 2 :when odd? 100) returns 2)
  (confirm that (|> 1 :when odd? :return 100) returns 100)
  (confirm that (|> 2 :when odd? :return 100) returns 2)
  (confirm that (|> 1 :when odd? (return! 100)) returns 100)
  (confirm that (|> 2 :when odd? (return! 100)) returns 2)
  
  (confirm that (|> 2 :unless odd? 100) returns 100)
  (confirm that (|> 1 :unless odd? 100) returns 1)
  (confirm that (|> 2 :unless odd? :return 100) returns 100)
  (confirm that (|> 1 :unless odd? :return 100) returns 1)
  (confirm that (|> 2 :unless odd? (return! 100)) returns 100)
  (confirm that (|> 1 :unless odd? (return! 100)) returns 1)

  (confirm that (|> 1 (when (odd? _) 100)) returns 100)
  (confirm that (|> 2 (when (odd? _) 100)) returns nil)
  (confirm that (|> 1 :?(when (odd? _) 100)) returns 100)
  (confirm that (|> 2 :?(when (odd? _) 100)) returns 2)

  (confirm that (|> 1 :? t) returns t)
  (confirm that (|> 1 :? nil) returns 1)
  (confirm that (|> 1 :?(= _ 1)) returns t)
  (confirm that (|> 2 :?(= _ 1)) returns 2)
  (confirm that (|> 1 :maybe t) returns t)
  (confirm that (|> 1 :maybe nil) returns 1)
  (confirm that (|> 1 :maybe(= _ 1)) returns t)
  (confirm that (|> 2 :maybe(= _ 1)) returns 2)

  ;; With spec without value:
  (confirm that (|> ((x)) 1) returns 1)
  (confirm that (|> ((x)) 1 2) returns 2)
  (confirm that (|> ((x)) 1 2 3) returns 3)
  (confirm that (|> ((x)) :return 1 2 3) returns 1)
  (confirm that (|> ((x)) 1 :return 2 3) returns 2)
  (confirm that (|> ((x)) 1 2 :return 3) returns 3)
  
  (confirm that (|> ((x)) : 1 2) returns 2)
  (confirm that (|> ((x)) 1 : 2) returns 1)
  (confirm that (|> ((x)) :ignore 1 2) returns 2)
  (confirm that (|> ((x)) 1 :ignore 2) returns 1)

  (confirm that (|> ((x)) (return! 1) 2 3) returns 1)
  (confirm that (|> ((x)) 1 (return! 2) 3) returns 2)
  (confirm that (|> ((x)) 1 2 (return! 3)) returns 3)

  (confirm that (|> ((x)) 1 :when odd? 100) returns 100)
  (confirm that (|> ((x)) 2 :when odd? 100) returns 2)
  (confirm that (|> ((x)) 1 :when odd? :return 100) returns 100)
  (confirm that (|> ((x)) 2 :when odd? :return 100) returns 2)
  (confirm that (|> ((x)) 1 :when odd? (return! 100)) returns 100)
  (confirm that (|> ((x)) 2 :when odd? (return! 100)) returns 2)

  (confirm that (|> ((x)) 2 :unless odd? 100) returns 100)
  (confirm that (|> ((x)) 1 :unless odd? 100) returns 1)
  (confirm that (|> ((x)) 2 :unless odd? :return 100) returns 100)
  (confirm that (|> ((x)) 1 :unless odd? :return 100) returns 1)
  (confirm that (|> ((x)) 2 :unless odd? (return! 100)) returns 100)
  (confirm that (|> ((x)) 1 :unless odd? (return! 100)) returns 1)

  (confirm that (|> ((x)) 1 (when (odd? x) 100)) returns 100)
  (confirm that (|> ((x)) 2 (when (odd? x) 100)) returns nil)
  (confirm that (|> ((x)) 1 :?(when (odd? x) 100)) returns 100)
  (confirm that (|> ((x)) 2 :?(when (odd? x) 100)) returns 2)

  (confirm that (|> ((x)) 1 :? t) returns t)
  (confirm that (|> ((x)) 1 :? nil) returns 1)
  (confirm that (|> ((x)) 1 :?(= x 1)) returns t)
  (confirm that (|> ((x)) 2 :?(= x 1)) returns 2)
  (confirm that (|> ((x)) 1 :maybe t) returns t)
  (confirm that (|> ((x)) 1 :maybe nil) returns 1)
  (confirm that (|> ((x)) 1 :maybe(= x 1)) returns t)
  (confirm that (|> ((x)) 2 :maybe(= x 1)) returns 2)

  ;; With spec including value:
  (confirm that (|> ((x 1))) returns 1)
  (confirm that (|> ((x 1)) 2) returns 2)
  (confirm that (|> ((x 1)) 2 3) returns 3)
  (confirm that (|> ((x :return)) 1 2 3) returns 1)
  (confirm that (|> ((x 1)) :return 2 3) returns 2)
  (confirm that (|> ((x 1)) 2 :return 3) returns 3)

  (confirm that (|> ((x :)) 1 2) returns 2)
  (confirm that (|> ((x 1)) : 2) returns 1)
  (confirm that (|> ((x :ignore)) 1 2) returns 2)
  (confirm that (|> ((x 1)) :ignore 2) returns 1)

  (confirm that (|> ((x (return! 1))) 2 3) returns 1)
  (confirm that (|> ((x 1)) (return! 2) 3) returns 2)
  (confirm that (|> ((x 1)) 2 (return! 3)) returns 3)

  (confirm that (|> ((x 1)) :when odd? 100) returns 100)
  (confirm that (|> ((x 2)) :when odd? 100) returns 2)
  (confirm that (|> ((x 1)) :when odd? :return 100) returns 100)
  (confirm that (|> ((x 2)) :when odd? :return 100) returns 2)
  (confirm that (|> ((x 1)) :when odd? (return! 100)) returns 100)
  (confirm that (|> ((x 2)) :when odd? (return! 100)) returns 2)

  (confirm that (|> ((x 2)) :unless odd? 100) returns 100)
  (confirm that (|> ((x 1)) :unless odd? 100) returns 1)
  (confirm that (|> ((x 2)) :unless odd? :return 100) returns 100)
  (confirm that (|> ((x 1)) :unless odd? :return 100) returns 1)
  (confirm that (|> ((x 2)) :unless odd? (return! 100)) returns 100)
  (confirm that (|> ((x 1)) :unless odd? (return! 100)) returns 1)

  (confirm that (|> ((x 1)) (when (odd? x) 100)) returns 100)
  (confirm that (|> ((x 2)) (when (odd? x) 100)) returns nil)
  (confirm that (|> ((x 1)) :?(when (odd? x) 100)) returns 100)
  (confirm that (|> ((x 2)) :?(when (odd? x) 100)) returns 2)

  (confirm that (|> ((x 1)) :? t) returns t)
  (confirm that (|> ((x 1)) :? nil) returns 1)
  (confirm that (|> ((x 1)) :?(= x 1)) returns t)
  (confirm that (|> ((x 2)) :?(= x 1)) returns 2)
  (confirm that (|> ((x 1)) :maybe t) returns t)
  (confirm that (|> ((x 1)) :maybe nil) returns 1)
  (confirm that (|> ((x 1)) :maybe(= x 1)) returns t)
  (confirm that (|> ((x 2)) :maybe(= x 1)) returns 2)

  (confirm that (|> 7 :(when (eql _ 8) (return! (* 7 6)))) returns 7)
  (confirm that (|> 8 :(when (eql _ 8) (return! (* 7 6)))) returns 42)

  (confirm that (|> 8 :?(when (eql _ 8) (* 7 6))) returns 42)
  (confirm that (|> 8 :?(when (eql _ 7) (* 7 6))) returns 8)
  (confirm that (|> 7 :?(when (eql _ 8) (* 7 6))) returns 7)
  (confirm that (|> 7 :?(when (eql _ 7) (* 7 6))) returns 42)

  (confirm that (|> 5 (* _ _) (+ _ 8)) returns 33)
  (confirm that (|> ((z)) 5 (* z z) (+ z 8)) returns 33)
  (confirm that (|> ((z 5)) (* z z) (+ z 8)) returns 33)

  (confirm that (|> ((e)) 5 (+ e 7) double (+ e 3) neg) returns -27)

  (confirm that
    (let ((y 20))
      (|> ((e 5)) (+ e 7) double (+ e 3) neg (lambda (x) (* x 3)) :(when (< e 40) 1) (+ y e)))
    returns -61)

  (confirm that (|> (+ 3 4) (* _ 5)) returns 35)
  (confirm that (|> ((e)) (+ 3 4) (* e 5)) returns 35)
  (confirm that (|> ((e (+ 3 4))) (* e 5)) returns 35)

  ;;;;;;;;;;;
  (confirm that
    (|> 5 (+ _ 7) double :(ignore "hello") (+ _ 3) neg :?(when (negative? _) (neg _)) :(when (> _ 20) (return! 11)) 1)
    returns 11)
  (confirm that
    (|> 5 (+ _ 7) double :(ignore "hello") (+ _ 3) neg :?(when (negative? _) (neg _)) :(when (> _ 20) (return! 11)))
    returns 11)
  (confirm that
    (|> 5 (+ _ 7) :(ignore  "hello") (+ _ 3) neg :?(when (negative? _) (neg _)) :(when (> _ 20) (return! 11)) 1)
    returns 1)
  (confirm that
    (|> 5 (+ _ 7) :(ignore  "hello") (+ _ 3) neg :?(when (negative? _) (neg _)) :(when (> _ 20) (return! 11)))
    returns 15)

  (confirm that
    (|> 5 (+ _ 7) double :(ignore "hello") (+ _ 3) neg :when negative? neg :when (> _ 20) (return! 11) 1)
    returns 11)
  (confirm that
    (|> 5 (+ _ 7) :(ignore  "hello") (+ _ 3) neg :when negative? neg :when (> _ 20) (return! 11) 1)
    returns 1)
  (confirm that
    (|> 5 (+ _ 7) double :(ignore "hello") (+ _ 3) neg :when negative? neg :when (> _ 20) (return! 11))
    returns 11)
  (confirm that
    (|> 5 (+ _ 7) :(ignore  "hello") (+ _ 3) neg :when negative? neg :when (> _ 20) (return! 11))
    returns 15)

  ;;;;;;;;;;;
  (confirm that
    (|> ((x)) 5 (+ x 7) double :(ignore "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return! 11)) 1)
    returns 11)
  (confirm that
    (|> ((x)) 5 (+ x 7) :(ignore "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return! 11)) 1)
    returns 1)
  (confirm that
    (|> ((x)) 5 (+ x 7) double :(ignore "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return! 11)))
    returns 11)
  (confirm that
    (|> ((x)) 5 (+ x 7) :(ignore "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return! 11)))
    returns 15)

  (confirm that
    (|> ((x)) 5 (+ x 7) double :(ignore "hello") (+ x 3) neg :when negative? neg :when (> x 20) (return! 11) 1)
    returns 11)
  (confirm that
    (|> ((x)) 5 (+ x 7) :(ignore "hello") (+ x 3) neg :when negative? neg :when (> x 20) (return! 11) 1)
    returns 1)
  (confirm that
    (|> ((x)) 5 (+ x 7) double :(ignore "hello") (+ x 3) neg :when negative? neg :when (> x 20) (return! 11))
    returns 11)
  (confirm that
    (|> ((x)) 5 (+ x 7) :(ignore "hello") (+ x 3) neg :when negative? neg :when (> x 20) (return! 11))
    returns 15)

  ;;;;;;;;;;;
  (confirm that
    (|> ((x 5)) (+ x 7) double :(ignore "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return! 11)) 1)
    returns 11)
  (confirm that
    (|> ((x 5)) (+ x 7) :(ignore "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return! 11)) 1)
    returns 1)
  (confirm that
    (|> ((x 5)) (+ x 7) double :(ignore "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return! 11)))
    returns 11)
  (confirm that
    (|> ((x 5)) (+ x 7) :(ignore "hello") (+ x 3) neg :?(when (negative? x) (neg x)) :(when (> x 20) (return! 11)))
    returns 15)

  (confirm that
    (|> ((x 5)) (+ x 7) double :(ignore "hello") (+ x 3) neg :when negative? neg :when (> x 20) (return! 11) 1)
    returns 11)
  (confirm that
    (|> ((x 5)) (+ x 7) :(ignore "hello") (+ x 3) neg :when negative? neg :when (> x 20) (return! 11) 1)
    returns 1)
  (confirm that
    (|> ((x 5)) (+ x 7) double :(ignore "hello") (+ x 3) neg :when negative? neg :when (> x 20) (return! 11))
    returns 11)
  (confirm that
    (|> ((x 5)) (+ x 7) :(ignore "hello") (+ x 3) neg :when negative? neg :when (> x 20) (return! 11))
    returns 15)
  
  (--pipe-print "Ran all pipe test cases.")
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pipe--run-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Scrap tests, PATTERN-DISPATCH and some PIPE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (let ((*wm--depth-indicator-enable*))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Reset the pattern-call dispatcher's alist:
      (pd--reset)

      ;; Define some simple functions:
      (def (doub n) (|> n (+ _ _)))
      (def (sqr y) (|> y (* _ _)))
      (def (doub-sqr y) (doub (sqr y)))

      ;; Define a fib:
      (def (fib 0) 0)
      (def (fib 1) 1)
      (def (fib n)
        (|>
          :(prn "Calculating (fib %d) using a pipe-based fib..." n)
          (|> n
            (- _ 1)
            (fib _))
          (+ _
            (|> n
              (- _ 2)
              (fib _)
              :(prn "Calculated (fib %d) = %d" n _)))))

      ;; Call it with some output commenting on the proceedings:
      (|> 3
        :(prn "Starting out with %d" _)
        (+ _ (|> 2 (+ _ 5)))
        :(prn "Getting the result of (fib %d)" _)
        (fib _)
        :(prn "Result =  %d" _)) ;; ⇒ 55
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      )))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
