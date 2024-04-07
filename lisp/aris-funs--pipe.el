;; -*- lexical-binding: nil; fill-column: 100;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'aris-funs--aliases)
(require 'aris-funs--unsorted)
(require 'aris-funs--with-messages)
(require 'aris-funs--alists)
(require 'aris-funs--lists)
(require 'aris-funs--stacks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro pipe (initial &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Older, simple version of `pipe'."
;;   `(let ((_ ,initial))
;;      (mapc (lambda (expr) (setq _ (eval expr))) ',body)
;;      _))
;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro pipe (init &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Codex's unattractive `pipe'."
;;   `(let ((_ ,init))
;;      (dolist (expr ',body)
;;        (let ((fun `(lambda (_) ,expr)))
;;          (setq _ (funcall fun _))))
;;      _))
;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(defcustom *pipe--verbose* t
  "Whether the pipe operator should print verbose messages."
  :group 'pipe
  :type 'boolean)

(defcustom *pipe--print-fun* 'indented-message
  "The function to use to print messages."
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

(defvar *--pipe--commands-to-flags* (mapr *--pipe--commands* (lambda (x) (cons (car x) (cddr x))))
  "An alist mapping commands to flags. This is not meant to be customized.")

(defvar *--pipe--arity-1-commands*
  (mapr (cl-remove-if (lambda (x) (not (= 1 (cadr x)))) *--pipe--commands*) #'car)
  "Commands that take one argument. This is not meant to be customized.")

(defvar *--pipe--arity-2-commands*
  (mapr (cl-remove-if (lambda (x) (not (= 2 (cadr x)))) *--pipe--commands*) #'car)
  "Commands that take two arguments. This is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe--print (first &rest rest)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pipe--print-fun*"
  (when *pipe--verbose*
    `(progn
       (funcall *pipe--print-fun* ,first ,@rest)
       nil)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe--make-args (head &rest tail)
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
            `'( (head-is-cons . ,head-is-cons)
                (car-head . ,car-head)
                (car-head-is-cons . ,car-head-is-cons)
                (car-head-length . ,car-head-length)
                (head-is-spec . ,head-is-spec)
                (head-is-spec-with-init-form . ,head-is-spec-with-init-form)
                (head . ,head)
                (var . ,var)
                (body . ,body))))
    alist))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --valid-pipe-flag-or-nil (kw &optional or-nil)
  (when (not (or (and or-nil (nil? kw)) (memq kw *--pipe-flags*)))
    (error "Invalid pipe flag: %S. Must be one of %S." kw *--pipe-flags*))
  kw)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --is-pipe-command? (kw) 
  (and (keyword? expr) (assoc expr *--pipe--commands-to-flags*)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming."
  (let* ( (args (eval `(--pipe--make-args ,head ,@tail)))
          (var  (alist-get 'var  args))
          (body `',(alist-get 'body args)))
    `(let ((final
             (let ( (body ,body)
                    (,var nil)
                    (flag nil))
               (cl-labels ( (flag-is? (test-flag)
                              (eq flag (--valid-pipe-flag-or-nil test-flag)))
                            (set-flag! (new-flag force)
                              (let ((new-flag (--valid-pipe-flag-or-nil new-flag t)))
                                (cond
                                  ((and flag new-flag (not force))
                                    (error "Cannot set flag to %S when flag is already set to %S."
                                      new-flag flag))
                                  (force
                                    (--pipe--print "FORCING FLAG FROM %S TO %S." flag new-flag)
                                    (setq flag new-flag))
                                  (t
                                    (--pipe--print "Setting flag from %S to %S%s." flag new-flag
                                      (if force " (forced)" ""))))
                                (setq flag new-flag)))
                            (store! (value)
                              (setq ,var value))
                            (unset-flag! ()
                              (set-flag! nil nil)))
                 (--pipe--print (make-string 80 ?\=))
                 (--pipe--print "START")
                 (--pipe--print (make-string 80 ?\=))
                 (catch 'return
                   (dostack (expr body)
                     (--pipe--print (make-string 80 ?\=))
                     (--pipe--print "Current:             %S" expr)
                     (--pipe--print "Remaining:           %S" stack)
                     (--pipe--print "Var:                 %S" ,var)
                     (--pipe--print "Flag:                %S" flag)
                     (if (--is-pipe-command? expr)
                       (let ((new-flag (alist-get expr *--pipe--commands-to-flags*)))
                         (set-flag! new-flag nil))
                       (cl-flet ( (ignore-next-and-unset-flag! (bool)
                                    (if bool
                                      (let ((next (pop!)))
                                        (--pipe--print "Popped 1st %S from %S." next body)
                                        (when (memq next *--pipe--arity-2-commands*)
                                          (error
                                            "Ignoring the %S command is not yet supported." next))
                                        (when (memq next *--pipe--arity-1-commands*)
                                          ;; pop the unary command's argument:
                                          (--pipe--print "Popped 1st %S from %S." (pop!) body)) 
                                        (unset-flag!))
                                      (--pipe--print "Next command will be processed."))
                                    (unset-flag!))
                                  (expr-fun
                                    `(lambda (expr ,',var)
                                       (cl-flet ((return (value) (throw 'return value)))
                                         (--pipe--print "Evaluating expr:     %S." expr)
                                         ,expr))))
                         (let ((result (if (fun? expr)
                                         (eval (list expr ',var)) ;; unsure about this quote.
                                         (expr-fun expr ,var))))
                           (--pipe--print "Expr result:         %S" result)
                           (cond
                             ((flag-is? :RETURN)
                               (--pipe--print "Returning due to command: %S" result)
                               (throw 'return result))
                             ((flag-is? :UNLESS)
                               (ignore-next-and-unset-flag! result))
                             ((flag-is? :WHEN)
                               (ignore-next-and-unset-flag! (not result)))
                             ((and (flag-is? :MAYBE) result)
                               (--pipe--print "%s: Updating var to %S and unsetting the %S flag."
                                 flag ,var flag)
                               (store! result)
                               (unset-flag!))
                             ((and (flag-is? :MAYBE) (not result))
                               (--pipe--print "%S: Ignoring %S and unsetting the %S flag."
                                 flag result flag)
                               (unset-flag!))
                             ((flag-is? :IGNORE)
                               (--pipe--print "Not setting %S because %S and unsetting the flag."
                                 result flag)
                               (unset-flag!))
                             (t 
                               (store! result)
                               (--pipe--print "Updating var to %S." ,var)))))))
                   ;; For clarity, explicitly throw the return value if we run out of stack items:
                   (throw 'return
                     (progn
                       (--pipe--print (make-string 80 ?\=))
                       (--pipe--print "Because empty stack: %S" ,var)
                       (--pipe--print (make-string 80 ?\=))
                       ,var)))))))
       (--pipe--print "Pipe's final return: %S" final)
       final)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pipe--run-tests ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Run the unit tests for the `pipe' function."
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

  (confirm that (|> (return 1) 2 3) returns 1)
  (confirm that (|> 1 (return 2) 3) returns 2)
  (confirm that (|> 1 2 (return 3)) returns 3)

  (confirm that (|> 1 :when odd? 100) returns 100)
  (confirm that (|> 2 :when odd? 100) returns 2)
  (confirm that (|> 1 :when odd? :return 100) returns 100)
  (confirm that (|> 2 :when odd? :return 100) returns 2)
  (confirm that (|> 1 :when odd? (return 100)) returns 100)
  (confirm that (|> 2 :when odd? (return 100)) returns 2)

  (confirm that (|> 2 :unless odd? 100) returns 100)
  (confirm that (|> 1 :unless odd? 100) returns 1)
  (confirm that (|> 2 :unless odd? :return 100) returns 100)
  (confirm that (|> 1 :unless odd? :return 100) returns 1)
  (confirm that (|> 2 :unless odd? (return 100)) returns 100)
  (confirm that (|> 1 :unless odd? (return 100)) returns 1)

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

  (confirm that (|> ((x)) (return 1) 2 3) returns 1)
  (confirm that (|> ((x)) 1 (return 2) 3) returns 2)
  (confirm that (|> ((x)) 1 2 (return 3)) returns 3)

  (confirm that (|> ((x)) 1 :when odd? 100) returns 100)
  (confirm that (|> ((x)) 2 :when odd? 100) returns 2)
  (confirm that (|> ((x)) 1 :when odd? :return 100) returns 100)
  (confirm that (|> ((x)) 2 :when odd? :return 100) returns 2)
  (confirm that (|> ((x)) 1 :when odd? (return 100)) returns 100)
  (confirm that (|> ((x)) 2 :when odd? (return 100)) returns 2)

  (confirm that (|> ((x)) 2 :unless odd? 100) returns 100)
  (confirm that (|> ((x)) 1 :unless odd? 100) returns 1)
  (confirm that (|> ((x)) 2 :unless odd? :return 100) returns 100)
  (confirm that (|> ((x)) 1 :unless odd? :return 100) returns 1)
  (confirm that (|> ((x)) 2 :unless odd? (return 100)) returns 100)
  (confirm that (|> ((x)) 1 :unless odd? (return 100)) returns 1)

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

  (confirm that (|> ((x (return 1))) 2 3) returns 1)
  (confirm that (|> ((x 1)) (return 2) 3) returns 2)
  (confirm that (|> ((x 1)) 2 (return 3)) returns 3)

  (confirm that (|> ((x 1)) :when odd? 100) returns 100)
  (confirm that (|> ((x 2)) :when odd? 100) returns 2)
  (confirm that (|> ((x 1)) :when odd? :return 100) returns 100)
  (confirm that (|> ((x 2)) :when odd? :return 100) returns 2)
  (confirm that (|> ((x 1)) :when odd? (return 100)) returns 100)
  (confirm that (|> ((x 2)) :when odd? (return 100)) returns 2)

  (confirm that (|> ((x 2)) :unless odd? 100) returns 100)
  (confirm that (|> ((x 1)) :unless odd? 100) returns 1)
  (confirm that (|> ((x 2)) :unless odd? :return 100) returns 100)
  (confirm that (|> ((x 1)) :unless odd? :return 100) returns 1)
  (confirm that (|> ((x 2)) :unless odd? (return 100)) returns 100)
  (confirm that (|> ((x 1)) :unless odd? (return 100)) returns 1)

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

  (confirm that (|> 7 :(when (eql _ 8) (return (* 7 6)))) returns 7)
  (confirm that (|> 8 :(when (eql _ 8) (return (* 7 6)))) returns 42)

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

  (confirm that (|> (+ 3 4) (* _ 3)) returns 21)
  (confirm that (|> ((e)) (+ 3 4) (* e 3)) returns 21)
  (confirm that (|> ((e (+ 3 4))) (* e 3)) returns 21)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pipe--run-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
