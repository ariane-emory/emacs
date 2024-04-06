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
(defcustom *pipe--verbose* nil
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
;; ;; EXAMPLE:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (while nil
;;   (progn 
;;     (let ( (*pipe--verbose* t)
;;            (*wm--depth-indicator-enable* nil))
;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ;; Do some simple arithmetic with a pipe:
;;       (|> 2 -> (+ _ 1) -> (* 3 _)) ;; ⇒ 9

;;       ;; Reset the pattern-call dispatcher's alist:
;;       (pd--reset) 

;;       ;; Define some simple functions:
;;       (def (double n) (|> n -> (+ _ _)))
;;       (def (square y) (|> y -> (* _ _)))
;;       (def (double-square y) (double (square y)))

;;       ;; Define a fib:
;;       (def (fib 0) 0)
;;       (def (fib 1) 1)
;;       (def (fib n)
;;         (|> (pipe--print "Calculating (fib %d) using a pipe-based fib..." n)
;;           (|> n -> (- _ 1) -> (fib _)) ->
;;           (+ _ (|> n -> (- _ 2) -> (fib _) ->
;;                  (pipe--print "Calculated (fib %d) = %d" n _) _))))

;;       ;; Call it with some output commenting on the proceedings:
;;       (|>
;;         3 -> (pipe--print "Starting out with %d" _) (+ _ (|> 2 -> (+ _ 5))) ->
;;         (pipe--print "Getting the result of (fib %d)" _) (fib _) ->
;;         "I'm just a harmless string sitting in the pipe doing doing nothing."
;;         (pipe--print "Result =  %d" _) _) ;; ⇒ 55

;;       (|> 5 -> (square _) -> (when (odd? _) (return (double _)) _))
;;       (|> 6 -> (square _) -> (when (odd? _) (return (double _)) _))


;;       (|> 3)
;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       )))
;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
              (head-is-spec-with-init-form
                (cons (cadr car-head) (cons '-> tail)))
              (head-is-spec
                tail)
              (t (cons head tail))))
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
       (let ( (last nil)
              (sym ',sym)
              (,var nil))
         (catch 'return
           (mapcr ',body
             (lambda (expr)
               ;; (pipe--print (make-string 80 ?\=))
               ;; (pipe--print "Expr: %S" expr)
               ;; (pipe--print "Var:  %S" ,var)
               ;; (pipe--print "Last: %S" last)

               (cl-flet ((expr-fun
                           `(lambda (,sym)
                              (cl-flet ((return (,sym) (throw 'return ,sym)))
                                ;;(pipe--print "Eval: %S" ',expr)
                                ,expr))))
                 (cond
                   ((eq expr '->)
                     (setq ,var last)
                     (setq last nil)
                     ;; (pipe--print "Updated by arrow! Var is %S, last is %S" ,var last)
                     )
                   (t
                     (setq last (expr-fun ,var))
                     ;; (pipe--print "Updated by call! Var is %S, last is %S" ,var last)
                     )))))
           (throw 'return
             (progn
               ;; (pipe--print (make-string 80 ?\=))
               ;; (pipe--print "Returning: %S" (or last ,var))
               ;; (pipe--print (make-string 80 ?\=))
               ;; (pipe--print "Var:  %S" ,var)
               ;; (pipe--print "Last: %S" last)
               last ;; maybe ,var?
               )))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
