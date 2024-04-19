;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
(require 'aris-funs--unsorted)
(require 'aris-funs--with-messages)
(require 'aris-funs--alists)
(require 'aris-funs--lists)
(require 'aris-funs--stacks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup pipe nil
  "Elixir-style pipe operator.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *pipe--verbose* nil
  "Whether the pipe operator should print verbose messages."
  :group 'pipe
  :type 'boolean)

(defcustom *pipe--print-fun* 'prn
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pipe (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe-prn (first &rest rest)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pipe--print-fun*"
  `(when *pipe--verbose* (ignore (funcall *pipe--print-fun* ,first ,@rest))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe-prndiv ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pipe--print-divider-fun*"
  `(when *pipe--verbose* (ignore (funcall *pipe--print-divider-fun*))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *--pipe-commands*
  '( (:       . (1 . :IGNORE))
     (:ignore . (1 . :IGNORE))
     (:?      . (1 . :MAYBE))
     (:go     . (1 . nil)) ;; doesn't use a flag right now.
     (:maybe  . (1 . :MAYBE))
     (:return . (1 . :RETURN))
     (:unless . (2 . :UNLESS))
     (:when   . (2 . :WHEN))
     )
  "An alist mapping commands to their flags and arities. This is not meant to be customized.")

(defvar *--pipe-flags*
  (compact (cl-remove-duplicates (map #'cdr (alist-values *--pipe-commands*))))
  "A list of flags that can be set by the pipe operator. This is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --get-pipe-command (kw)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Retrieve the command associated with a keyword KW, or nil if KW is not a command."
  (alist-get kw *--pipe-commands*))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --get-pipe-command-arity (command)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the arity of a pipe command COMMAND or 0 if COMMAND is not a pipe command."
  (if-let ( (alist-value (alist-get command *--pipe-commands*))
            (arity (car alist-value)))
    arity
    0))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --valid-pipe-flag (kw &optional or-nil)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return KW if it is a valid pipe flag, or signal an error. If OR-NIL is t, return nil if KW is nil."
  (when (not (or (and or-nil (nil? kw)) (memq kw *--pipe-flags*)))
    (error "Invalid pipe flag: %S. Must be one of %S." kw *--pipe-flags*))
  kw)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --make-pipe-args (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
            `'( (var . ,var)
                (body . ,body))))
    alist))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming (newer version without dostack)."
  (let* ( (args            (eval `(--make-pipe-args ,head ,@tail)))
          (return-label `',(gensym "return-"))
          (var             (alist-get 'var  args))
          (body         `',(alist-get 'body args)))
    ;; (--pipe-prn "return-label is %S" return-label)
    ;; (--pipe-prn "args is %S" args)
    `(let* ( (,var             nil)
             (var-sym        ',var)
             (body            ,body)
             (remaining-body   body)
             (flag            nil))
       (cl-labels ( (length>= (len)
                      (length> remaining-body (1- len)))
                    (set-remaining-body! (new-body)
                      (--pipe-prn "Setting remaining-body to %S." new-body)
                      (setq remaining-body new-body))
                    (pop! ()
                      (unless (length>= 1) (signal 'stack-underflow (list 'remaining-body)))
                      (pop remaining-body))
                    (drop-next! ()
                      (let ((drop-count 1) poppeds)
                        (until (zero? drop-count)
                          (--pipe-prn "Drop count is %S" drop-count)
                          (cl-decf drop-count)
                          (let* ( (popped (pop!))
                                  (popped-things-arity (--get-pipe-command-arity popped))
                                  (next-drop-count (+ drop-count popped-things-arity)))
                            (unless (length>= next-drop-count)
                              (error "malformed pipe body detected during drop"))
                            (--pipe-prn
                              (concat "Just dropped %S, adding %d to drop-count, "
                                "new drop-count is %s")
                              popped popped-things-arity next-drop-count)
                            (setq drop-count next-drop-count)))))
                    (store! (value)
                      (prog1
                        (setq ,var value)
                        (--pipe-prn "Updated %S to %S." var-sym ,var)))
                    (flag-is? (tested-flag)
                      (eq flag (--valid-pipe-flag tested-flag)))
                    (set-flag! (new-flag &optional force)
                      (let ((new-flag (--valid-pipe-flag new-flag t)))
                        (cond
                          ((and (not force) flag new-flag)
                            (error "Cannot set flag to %S when flag is already set to %S."
                              new-flag flag))
                          (force
                            (--pipe-prn "FORCING FLAG FROM %S TO %S." flag new-flag))
                          (t
                            (--pipe-prn "Setting flag from %S to %S%s." flag new-flag
                              (if force " (forced)" ""))))
                        (setq flag new-flag)))
                    (unset-flag! ()
                      (when flag
                        (--pipe-prn "Unsetting flag %S." flag)
                        (set-flag! nil)))
                    (labeled-print (label value)
                      (let* ( (label  (format "%s:" label))
                              (whites (make-string (- 21 (length label)) ?\ ))
                              (label  (concat label whites)))
                        (--pipe-prn "%s%S" label value))))
         (--pipe-prndiv)
         (--pipe-prn "START WITH %s!" remaining-body)
         (--pipe-prndiv)
         (catch ,return-label                
           (while remaining-body
             (let ((expr (pop!)))
               (--pipe-prndiv)
               (labeled-print "Current" expr)
               (labeled-print "Remaining" remaining-body)
               (labeled-print var-sym ,var)
               (labeled-print "Flag" flag)
               (if-let ( (command (--get-pipe-command expr))
                         (command-arity (car command))
                         ;; named so as to not shadow flag in set-flag:
                         (command-flag (cdr command)))
                 (progn
                   (unless (length>= command-arity)
                     (error "malformed pipe body"))
                   (set-flag! command-flag))
                 (pcase expr
                   ;; ('nil (error "impossible, expr is %s?" expr))
                   (`',label (message "Skip past label %s..." label))
                   ;; maybe this should be handled inside _ case like other flagged items
                   ;; so as to eval the label:
                   (:go
                     (unless (length>= (--get-pipe-command-arity expr))
                       (error "malformed pipe body"))
                     (let ((go-label (pop!)))
                       (message "This is a :go to %s" go-label)
                       (set-remaining-body! (cdr body)))
                     )
                   (_ (let
                        ((result
                           (eval (if (fun? expr)
                                   `(,expr ,var-sym)
                                   (let ((return-label ,return-label))
                                     `(cl-flet
                                        ((return! (value)
                                           (--pipe-prn "Throwing %S." ',return-label)
                                           (throw ',return-label value)))
                                        ,expr))))))
                        (labeled-print "Expr result" result)
                        (cond
                          ((flag-is? :IGNORE)
                            (--pipe-prn "Not setting %S because %S." result flag))
                          ((flag-is? :WHEN)
                            (when (not result) (drop-next!)))
                          ((flag-is? :UNLESS)
                            (when result (drop-next!)))
                          ((flag-is? :RETURN)
                            (--pipe-prn "Returning due to command: %S" result)
                            (throw ,return-label result))
                          ((and (flag-is? :MAYBE) result)
                            (store! result))
                          ((and (flag-is? :MAYBE) (not result))
                            (--pipe-prn "Ignoring %S." result))
                          (t (store! result)))
                        (unset-flag!)))))))
           ;; For clarity, explicitly throw the return value if we run out of stack items:
           (throw ,return-label
             (progn
               (--pipe-prndiv)
               (--pipe-prn "Returning this because stack is empty: %S" ,var)
               (--pipe-prndiv)
               ,var))
           ) ;; END OF CATCH.
         ) ;; END OF CL-LABELS.
       ) ;; END OF LET (AND OF EXPANDED MACRO CONTENT).
    ) ;; END OF LET*. 
  ) ;; END OF DEFMACRO.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIT TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pipe--run-tests ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Run the unit tests for the `pipe' function."
  (confirm that (--valid-pipe-flag :IGNORE) returns :IGNORE)
  (confirm that (--valid-pipe-flag :MAYBE)  returns :MAYBE)
  (confirm that (--valid-pipe-flag :RETURN) returns :RETURN)
  (confirm that (--valid-pipe-flag :UNLESS) returns :UNLESS)
  (confirm that (--valid-pipe-flag :WHEN)   returns :WHEN)
  (confirm that (--valid-pipe-flag nil t)   returns nil)
  
  (confirm that
    (condition-case _
      (--valid-pipe-flag :foo)
      (error (cadr _)))
    returns
    "Invalid pipe flag: :foo. Must be one of (:IGNORE :MAYBE :RETURN :UNLESS :WHEN).")
  
  (confirm that
    (condition-case _
      (--valid-pipe-flag nil)
      (error (cadr _)))
    returns
    "Invalid pipe flag: nil. Must be one of (:IGNORE :MAYBE :RETURN :UNLESS :WHEN).")
  
  (confirm that (--get-pipe-command :) returns (1 . :IGNORE))
  (confirm that (--get-pipe-command :ignore) returns (1 . :IGNORE))
  (confirm that (--get-pipe-command :?) returns (1 . :MAYBE))
  (confirm that (--get-pipe-command :maybe) returns (1 . :MAYBE))
  (confirm that (--get-pipe-command :return) returns (1 . :RETURN))
  (confirm that (--get-pipe-command :unless) returns (2 . :UNLESS))
  (confirm that (--get-pipe-command :when)  returns (2 . :WHEN))
  (confirm that (--get-pipe-command :foo) returns nil)
  (confirm that (--get-pipe-command :7) returns nil)
  
  (confirm that (--get-pipe-command-arity :) returns 1)
  (confirm that (--get-pipe-command-arity :?) returns 1)
  (confirm that (--get-pipe-command-arity :ignore) returns 1)
  (confirm that (--get-pipe-command-arity :maybe) returns 1)
  (confirm that (--get-pipe-command-arity :return) returns 1)
  (confirm that (--get-pipe-command-arity :unless) returns 2)
  (confirm that (--get-pipe-command-arity :when) returns 2)
  (confirm that (--get-pipe-command-arity :foo) returns 0)
  (confirm that (--get-pipe-command-arity 7) returns 0)

  ;; With no spec:
  (confirm that (|> 1) returns 1)
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
  
  (--pipe-prn "Ran all pipe test cases.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pipe--run-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Scrap tests, PATTERN-DISPATCH and some PIPE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (let ((*wm--depth-indicator-enable*))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Reset the pattern-call dispatcher's alist:
      (pd--reset)

      ;; Define some simple functions:
      (pdef (doub n) (|> n (+ _ _)))
      (pdef (sqr y) (|> y (* _ _)))
      (pdef (doub-sqr y) (doub (sqr y)))

      ;; Define a fib:
      (pdef (fib 0) 0)
      (pdef (fib 1) 1)
      (pdef (fib n)
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
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      )))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
