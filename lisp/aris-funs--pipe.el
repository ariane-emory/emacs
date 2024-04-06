;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'aris-funs--aliases)
(require 'aris-funs--unsorted)
(require 'aris-funs--with-messages)
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
       (mapr ',body
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
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pipe-args (head &rest tail)
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
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming."
  (let* ( (args      (eval `(--pipe-args ,head ,@tail)))
          (var       (alist-get 'var  args))
          (body      (alist-get 'body args)))
    `(let ( (,var nil)
            (flag nil))
       (pipe--print (make-string 80 ?\=))
       (catch 'return
         (cl-flet ((set-flag (new-flag)
                     (when flag 
                       (error "Cannot set flag to %S when flag is already set to %S." new-flag flag))
                     (pipe--print "Setting flag to %S." new-flag)
                     (setq flag new-flag)))
           (mapcr ',body
             (lambda (expr)
               (pipe--print (make-string 80 ?\=))
               (pipe--print "Expr:           %S" expr)
               (pipe--print "Var:            %S" ,var)
               (pipe--print "Flag:           %S" flag)
               (cond
                 ((eq expr :)
                   (set-flag :IGNORE))
                 ((eq expr :?)
                   (set-flag :WHEN-EXPR))
                 ((eq expr :when?)
                   (set-flag :WHEN-CMD))
                 ((eq expr :unless?)
                   (set-flag :UNLESS-CMD))
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
                         ((eq flag :WHEN-CMD)
                           (if result
                             (progn
                               (pipe--print "Next command will be processed.")
                               (setq flag nil))
                             (progn
                               (pipe--print "Next command will be ignored.")
                               (setq flag nil)
                               (set-flag :IGNORE))))
                         ((eq flag :WHEN-EXPR)
                           (if (not result)
                             (pipe--print "%S: Ignoring %S and unsetting the %S flag." flag result flag)
                             (pipe--print "%s: Updating var to %S and unsetting the %S flag." flag ,var flag)
                             (setq ,var result))
                           (setq flag nil))
                         ((eq flag :UNLESS-CMD)
                           (if result
                             (progn
                               (pipe--print "Next command will be ignored.")
                               (setq flag nil)
                               (set-flag :IGNORE))
                             (progn
                               (pipe--print "Next command will be processed.")
                               (setq flag nil))))
                         ((eq flag :IGNORE)
                           (pipe--print "Ignoring %S and unsetting ignore flag." result)
                           (setq flag nil))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
