;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    ;; (prn (make-string 80 ?\=))
    ;; (prn "PIPE CALLED")
    ;; (prn (make-string 80 ?\=))
    ;; (prn "head: %s" head)
    ;; (prn "head-is-spec: %s" head-is-spec)
    ;; (prn "var: %s" var)
    ;; (prn "init-form: %s" init-form)
    ;; (prn "body: %s" body)
    `(progn
       ;; (prn (make-string 80 ?\=))
       (let ( (last ,init-form)
              (sym ',var)
              (,var nil))
         (catch 'return
           (mapcr ',body
             (lambda (expr)
               ;; (prn (make-string 80 ?\=))
               ;; (prn "Expr: %S" expr)
               ;; (prn "Var:  %S" ,var)
               ;; (prn "Last: %S" last)
               (cl-flet ((expr-fun
                           `(lambda (sym)
                              (cl-flet ((return (,sym)
                                          (throw 'return ,sym)))
                                ;; (prn "Eval: %S" ',expr)
                                (let ((result ,expr))
                                  ;; (prn "Next: %S" result)
                                  result)))))
                 (cond
                   ((eq expr '->)
                     (setq ,var last)
                     (setq last nil)
                     ;; (prn "Updated! Var is %S, last is %S" ,var last)
                     )
                   (t (setq last (expr-fun ,var)))))))
           (throw 'return last))))))
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((*with-messages--depth-indicator-enable*))
  ;; Do some simple arithmetic with a pipe:
  (|> 2 -> (+ _ 1) -> (* 3 _)) ;; ⇒ 9

  ;; Reset the pattern-call dispatcher's alist.
  (pd--reset) 

  ;; Define a fib:
  (def (fib 0) 0)
  (def (fib 1) 1)
  (def (fib n)
    (|> (prn "Calculating (fib %d) using a pipe-based fib..." n)
      (|> n -> (- _ 1) -> (fib _)) ->
      (+ _ (|> n -> (- _ 2) -> (fib _) ->
             (prn "Calculated (fib %d) = %d" n _) _))))

  ;; Call it with some output commenting on the proceedings:
  (|>
    3 -> (prn "Starting out with %d" _) (+ _ (|> 2 -> (+ _ 5))) ->
    (prn "Getting the result of (fib %d)" _) (fib _) ->
    "I'm just a harmless string sitting in the pipe doing doing nothing."
    (prn "Result =  %d" _) _)) ;; ⇒ 55
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
