;; -*- lexical-binding: nil; fill-column: 100;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'aris-funs--aliases)
(require 'aris-funs--unsorted)
(require 'aris-funs--with-messages)
(require 'aris-funs--lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(defmacro pipe--print (first &rest rest)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pipe--print-fun*"
  (when *pipe--verbose*
    `(progn
       (funcall *pipe--print-fun* ,first ,@rest)
       nil)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro pipe (initial &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Older, simple version of `pipe'."
;;   `(let ((_ ,initial))
;;      (mapc (lambda (expr) (setq _ (eval expr))) ',body)
;;      _))
;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro pipe (init &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Codex's unattractive `pipe'."
;;   `(let ((_ ,init))
;;      (dolist (expr ',body)
;;        (let ((fun `(lambda (_) ,expr)))
;;          (setq _ (funcall fun _))))
;;      _))
;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe-args (head &rest tail)
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
(defvar *--pipe--arity-1-commands*
  '(
     :?
     :no-set
     :maybe
     :return
     )
  "Commands that take one argument. This is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *--pipe--arity-2-commands*
  '(
     :unless
     :when
     )
  "Commands that take two arguments. This is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *--pipe--commands-to-flags*
  '(
     (: . :NO-SET)
     (:? . :MAYBE)
     (:no-set . :NO-SET)
     (:maybe . :MAYBE)
     (:return . :RETURN)
     (:unless . :UNLESS)
     (:when . :WHEN)
     )
  "An alist mapping commands to flags. This is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *--pipe-flags*
  '(
     ;; :IGNORE is the only flag thas is not explicitly set by a command (it may be set as
     ;; a consequence of :when and :unless commands instead) and that is handled outside
     ;; the t case of the outer cond.
     :IGNORE
     :MAYBE
     :NO-SET
     :RETURN
     :UNLESS
     :WHEN
     )
  "A list of flags that can be set by the pipe operator. This is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --valid-pipe-flag (kw &optional or-nil)
  (when (not (or (and or-nil (nil? kw)) (memq kw *--pipe-flags*)))
    (error "Invalid pipe flag: %S. Must be one of %S." kw *--pipe-flags*))
  kw)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming."
  (let* ( (args      (eval `(--pipe-args ,head ,@tail)))
          (var       (alist-get 'var  args))
          (body      (alist-get 'body args)))
    `(let ( (,var nil)
            (flag nil))
       (pipe--print (make-string 80 ?\=))
       (pipe--print "START")
       (pipe--print (make-string 80 ?\=))
       (catch 'return
         (cl-labels ( (flag-is? (test-flag) 
                        (eq flag (--valid-pipe-flag test-flag)))
                      (store! (value)
                        (setq ,var value))
                      (set-flag! (new-flag force)
                        (let ((new-flag (--valid-pipe-flag new-flag t)))
                          (cond
                            ((and flag new-flag (not force))
                              (error "Cannot set flag to %S when flag is already set to %S."
                                new-flag flag))
                            (force
                              (pipe--print "FORCING FLAG FROM %S TO %S." flag new-flag)
                              (setq flag new-flag))
                            (t
                              (pipe--print "Setting flag from %S to %S%s." flag new-flag
                                (if force " (forced)" ""))))
                          (setq flag new-flag)))
                      (unset-flag! ()
                        (set-flag! nil nil)))
           (maprc ',body
             (lambda (expr)
               (pipe--print (make-string 80 ?\=))
               (pipe--print "Expr:           %S" expr)
               (pipe--print "Var:            %S" ,var)
               (pipe--print "Flag:           %S" flag)
               (cond
                 ((and (flag-is? :IGNORE) (memq expr *--pipe--arity-2-commands*))
                   (error "Ignoring the %S command because %S is not yet supported."
                     expr flag))
                 ((and (flag-is? :IGNORE) (memq expr *--pipe--arity-1-commands*))
                   (pipe--print "Do nothing for expr %S because %S." expr flag))
                 ((flag-is? :IGNORE)
                   (pipe--print "Ignoring expr %S because %S and unsetting the flag."
                     expr flag)
                   (unset-flag!))
                 ((and (keywordp expr) (assoc expr *--pipe--commands-to-flags*))
                   (let ((new-flag (alist-get expr *--pipe--commands-to-flags*)))
                     ;; (pipe--print "Setting flag from %S to %S by alist entry for %S."
                     ;;   flag new-flag expr)
                     (set-flag! new-flag nil)))
                 (t
                   (cl-flet ((expr-fun
                               `(lambda (expr ,',var)
                                  (cl-flet ((return (value) (throw 'return value)))
                                    (pipe--print "Evaluated expr: %S." expr)
                                    ,expr))))
                     (let* ((result (if (fun? expr)
                                      (eval (list expr ',var)) ;; unsure about this quote.
                                      (expr-fun expr ,var))))
                       (cond
                         ((flag-is? :RETURN)
                           (pipe--print "Returning: %S" result)
                           (throw 'return result)
                           (unset-flag!))
                         ((flag-is? :WHEN)
                           (if result
                             (progn
                               (pipe--print "Next command will be processed.")
                               (unset-flag!))
                             (pipe--print "Next command will be ignored.")
                             (set-flag! :IGNORE t)))
                         ((flag-is? :UNLESS)
                           (if result
                             (progn
                               (pipe--print "Next command will be ignored.")
                               (set-flag! :IGNORE t))
                             (pipe--print "Next command will be processed.")
                             (unset-flag!)))
                         ((flag-is? :MAYBE)
                           (if (not result)
                             (pipe--print "%S: Ignoring %S and unsetting the %S flag."
                               flag result flag)
                             (pipe--print "%s: Updating var to %S and unsetting the %S flag."
                               flag ,var flag)
                             (setq ,var result))
                           (unset-flag!))
                         ((flag-is? :NO-SET)
                           (pipe--print "Not setting %S because %S and unsetting the flag."
                             result flag)
                           (unset-flag!))
                         ;; ((flag-is? :IGNORE)
                         ;;   (pipe--print "Ignoring %S because %S and unsetting the flag."
                         ;;     result flag)
                         ;;   (unset-flag!))
                         (t 
                           (setq ,var result)
                           (pipe--print "Updating var to %S and last to %S." ,var result)
                           ))))))))
           (throw 'return
             (progn
               (pipe--print (make-string 80 ?\=))
               (pipe--print "Returning:      %S" ,var)
               (pipe--print (make-string 80 ?\=))
               ,var)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
