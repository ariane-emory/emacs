;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--unsorted)
(require 'aris-funs--with-messages)
(require 'aris-funs--plists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup pipe nil
  "Elixir-style pipe operator.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pipe--print (first &rest rest)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pipe--print-fun*"
  (when *pipe--verbose*
    `(progn
       (funcall *pipe--print-fun* ,first ,@rest)
       nil)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro pipe (initial &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Older, simple version of `pipe'."
;;   `(let ((_ ,initial))
;;      (mapc (lambda (expr) (setq _ (eval expr))) ',body)
;;      _))
;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro pipe (init &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Codex's unattractive `pipe'."
;;   `(let ((_ ,init))
;;      (dolist (expr ',body)
;;        (let ((fun `(lambda (_) ,expr)))
;;          (setq _ (funcall fun _))))
;;      _))
;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (let* ((head-is-spec
           (and
             (consp head)
             (consp (car head))
             (length> (car head) 0)
             (length< (car head) 3)))
          (sym (if head-is-spec (caar head) *pipe--default-var-sym*))
          (init-form (when head-is-spec (cadar head)))
          (body (if head-is-spec tail (cons head tail))))
    ;;(debug)
    ;; (message "head: %s" head)
    ;; (message "head-is-spec: %s" head-is-spec)
    ;; (message "sym: %s" sym)
    ;; (message "init-form: %s" init-form)
    ;; (message "body: %s" body)
    body
    `(let ( (,sym ,init-form)
            (ignore-flag nil))
       (mapr ',body
         (lambda (expr)
           (cond
             ((eq : expr) (setq ignore-flag t))
             (ignore-flag
               (eval expr)
               (setq ignore-flag nil))
             (t
               (setq ,sym (eval expr))
               (setq ignore-flag nil)))))
       ,sym)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro |> (head &rest tail)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "`pipe' with optional let-like binding/symbol naming."
;;   (let* ( (args      (eval `(pipe-args ,head ,@tail)))
;;           (sym       (alist-get 'var  args))
;;           (var       (alist-get 'var  args))
;;           (body      (alist-get 'body args)))
;;     `(progn
;;        (pipe--print (make-string 80 ?\=))
;;        (let ( (last nil)
;;               (sym ',sym)
;;               (,var nil)
;;               (ignore-flag nil))
;;          (catch 'return
;;            (mapcr ',body
;;              (lambda (expr)
;;                (pipe--print (make-string 80 ?\=))
;;                (pipe--print "Expr:   %S" expr)
;;                (pipe--print "Var:    %S" ,var)
;;                (pipe--print "Last:   %S" last)
;;                (pipe--print "Ignore: %S" ignore-flag)

;;                (cl-flet ((expr-fun
;;                            `(lambda (,sym)
;;                               (cl-flet ((return (,sym) (throw 'return ,sym)))
;;                                 (pipe--print "Evaluated expr %S." ',expr)
;;                                 ,expr))))
;;                  (cond
;;                    ((eq expr '->)
;;                      (setq ,var last)
;;                      ;;(setq last nil) ;; Probably don't do this.
;;                      (pipe--print "Piping last %S! Var is %S, last is %S" last ,var last)
;;                      )
;;                    ((eq expr ':)
;;                      (pipe--print "Setting ignore flag.")
;;                      (setq ignore-flag t))
;;                    (t
;;                      (let ((result (expr-fun ,var)))
;;                        (if ignore-flag
;;                          (progn
;;                            (pipe--print "Ignoring %S and unsetting ignore flag." result)
;;                            (setq ignore-flag nil))
;;                          (setq last result)
;;                          (pipe--print "Updating var to %S and last to %S." ,var result)
;;                          )))))))
;;            (throw 'return
;;              (progn
;;                (pipe--print (make-string 80 ?\=))
;;                (pipe--print "Returning: %S" (or last ,var))
;;                (pipe--print (make-string 80 ?\=))
;;                (pipe--print "Var:  %S" ,var)
;;                (pipe--print "Last: %S" last)
;;                ;;last ;; maybe ,var?
;;                ,var
;;                )))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pipe-args (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming."
  (let* ( (consp-head (consp head))
          (car-head (when consp-head (car head)))
          (consp-car-head (when consp-head (consp car-head)))
          (car-head-length (when consp-car-head (length car-head)))
          (head-is-spec
            (and
              consp-car-head
              (> car-head-length 0)
              (< car-head-length 3)))
          (head-is-spec-with-init-form (eql car-head-length 2))
          (var (if head-is-spec (car car-head) *pipe--default-var-sym*))
          (body
            (cond
              (head-is-spec-with-init-form (cons (cadr car-head) tail))
              (head-is-spec tail)
              (t (cons head tail))))
          ;;(body (append body '(->)))
          (alist
            `'( (consp-head . ,consp-head)
                (car-head . ,car-head)
                (consp-car-head . ,consp-car-head)
                (car-head-length . ,car-head-length)
                (head-is-spec . ,head-is-spec)
                (head-is-spec-with-init-form . ,head-is-spec-with-init-form)
                (head . ,head)
                (var . ,var)
                (body . ,body))))
    alist))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming."
  (let* ( (args      (eval `(pipe-args ,head ,@tail)))
          (sym       (alist-get 'var  args))
          (var       (alist-get 'var  args))
          (body      (alist-get 'body args)))
    `(progn
       (pipe--print (make-string 80 ?\=))
       (let ( (,var nil)
              (flag nil))
         (cl-flet ((set-flag (new-flag)
                     (when flag 
                       (error "Cannot set flag to %S when flag is already set to %S." new-flag flag))
                     (pipe--print "Setting flag to %S." new-flag)
                     (setq flag new-flag)))
           (catch 'return
             (mapcr ',body
               (lambda (expr)
                 (pipe--print (make-string 80 ?\=))
                 (pipe--print "Expr:   %S" expr)
                 (pipe--print "Var:    %S" ,var)
                 (pipe--print "Flag:   %S" flag)

                 (cond
                   ((eq expr ':)
                     (set-flag :IGNORE))
                   ((eq expr ':?)
                     (set-flag :MAYBE))
                   ((eq expr ':!)
                     (set-flag :MAYBE-NOT))
                   (t
                     (cl-flet ((expr-fun
                                 `(lambda (expr ,',var)
                                    (cl-flet ((return (value) (throw 'return value)))
                                      (pipe--print "Evaluated expr %S." expr)
                                      ,expr))))
                       (let* ((result (if (fun? expr)
                                        (eval (list expr ,var))
                                        (expr-fun expr ,var))))
                         (cond
                           ((eq flag :MAYBE)
                             (progn
                               (if (not result)
                                 (pipe--print "%S: Ignoring %S and unsetting the %S flat." flag result flag)
                                 (pipe--print "%s: Updating var to %S and unsetting the %S flag." flag ,var flag))
                               (setq ,var result)
                               (setq flag nil)))
                           ((eq flag :IGNORE)
                             (progn 
                               (pipe--print "Ignoring %S and unsetting ignore flag." result)
                               (setq flag nil)))
                           (t 
                             (progn
                               (setq ,var result)
                               (pipe--print "Updating var to %S and last to %S." ,var result)
                               )))))))))
             (throw 'return
               (progn
                 (pipe--print (make-string 80 ?\=))
                 (pipe--print "Returning: %S" ,var)
                 (pipe--print (make-string 80 ?\=))
                 ,var))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
