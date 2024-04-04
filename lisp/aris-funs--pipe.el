;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--unsorted)
(require 'aris-funs--with-messages)
(require 'aris-funs--plist-funs)
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *pipe--print-fun* 'indented-message
  "The function to use to print messages."
  :group 'pipe
  :type 'function)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pipe--print (first &rest rest)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pipe--print-fun*"
  `(when *pipe--verbose*     
     (funcall *pipe--print-fun* ,first ,@rest)
     nil))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |> (head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming."
  (let* ((head-is-spec
           (and
             (consp head)
             (consp (car head))
             (length> (car head) 0)
             (length< (car head) 3)))
          (var (if head-is-spec (caar head) pipe-default-var-sym))
          (init-form (if head-is-spec (cadar head) head))
          (body tail))

    (pipe--print (make-string 80 ?\=))
    (pipe--print "PIPE CALLED")
    (pipe--print (make-string 80 ?\=))
    (pipe--print "head: %s" head)
    (pipe--print "head-is-spec: %s" head-is-spec)
    (pipe--print "var: %s" var)
    (pipe--print "init-form: %s" init-form)
    (pipe--print "body: %s" body)

    `(progn
       (pipe--print (make-string 80 ?\=))
       (let ( (last ,init-form)
              (sym ',var)
              (,var nil))
         (catch 'return
           (mapcr ',body
             (lambda (expr)
               (pipe--print (make-string 80 ?\=))
               (pipe--print "Expr: %S" expr)
               (pipe--print "Var:  %S" ,var)
               (pipe--print "Last: %S" last)

               (cl-flet ((expr-fun
                           `(lambda (sym)
                              (cl-flet ((return (,sym) (throw 'return ,sym)))
                                ;;(pipe--print "Eval: %S" ',expr)
                                (let ((result ,expr))
                                  ;;(pipe--print "Next: %S" result)
                                  result)))))
                 (cond
                   ((eq expr '->)
                     (setq ,var last)
                     (setq last nil)
                     (pipe--print "Updated by arrow! Var is %S, last is %S" ,var last)
                     )
                   (t (setq last (expr-fun ,var))
                     (pipe--print "Updated by call! Var is %S, last is %S" ,var last))))))
           (throw 'return
             (progn
               (pipe--print (make-string 80 ?\=))
               (pipe--print "Returning: %S" (or last ,var))
               (pipe--print (make-string 80 ?\=))
               ;;(pipe--print "Expr: %S" expr)
               (pipe--print "Var:  %S" ,var)
               (pipe--print "Last: %S" last)
               (or last ,var))))))))
               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro plist-put! (plist-symbol key value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Add or updated a key-value pair in a plist."
  `(setq ,plist-symbol (plist-put ,plist-symbol ,key ,value)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro plist-remove!(plist-symbol key)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Remove a key and it's associated value - this actually removes it, it doesn't just set it to nill"
  `(let (new-plist (old-plist ,plist-symbol))
     (while old-plist
       (let ((k (car old-plist))
              (v (cadr old-plist)))
         (unless (eq k ,key)
           (plist-put! new-plist k v))
         (setq old-plist (cddr old-plist))))
     (setq ,plist-symbol new-plist)))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro plist-sort! (plist-symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Sort a plist by key."
  `(setq ,plist-symbol
     (apply 'append
       (sort (cl-loop for (key value) on ,plist-symbol by #'cddr
               collect (list key value))
         (lambda (a b) (string< (car a) (car b)))))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (setq plist  nil)
    (progn
      (plist-put! plist 'a 1) 
      (plist-put! plist 'b 2)
      (plist-put! plist 'c 3))
    (plist-sort! plist)
    (plist-remove! plist 'a)
    (plist-remove! plist 'b)
    (plist-remove! plist 'c)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ( (*pipe--verbose* nil)
       (*with-messages--depth-indicator-enable* nil))
  ;; Do some simple arithmetic with a pipe:
  (|> 2 -> (+ _ 1) -> (* 3 _)) ;; ⇒ 9

  ;; Reset the pattern-call dispatcher's alist.
  (pd--reset) 

  ;; Define a fib:
  (def (fib 0) 0)
  (def (fib 1) 1)
  (def (fib n)
    (|> (pipe--print "Calculating (fib %d) using a pipe-based fib..." n)
      (|> n -> (- _ 1) -> (fib _)) ->
      (+ _ (|> n -> (- _ 2) -> (fib _) ->
             (pipe--print "Calculated (fib %d) = %d" n _) _))))

  ;; Call it with some output commenting on the proceedings:
  (|>
    3 -> (pipe--print "Starting out with %d" _) (+ _ (|> 2 -> (+ _ 5))) ->
    (pipe--print "Getting the result of (fib %d)" _) (fib _) ->
    "I'm just a harmless string sitting in the pipe doing doing nothing."
    (pipe--print "Result =  %d" _) _)) ;; ⇒ 55
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
