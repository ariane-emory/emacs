;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my integer class:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class integer (value) nil
  (val () value)
  (fmt () (format "(integer %d)" value))
  (add (other) (integer (+ value (val other))))
  (sub (other) (integer (- value (val other))))
  (mul (other) (integer (* value (val other))))
  (div (other) (integer (/ value (val other))))
  (rem (other) (integer (% value (val other)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (val (mul (integer 2) (add (integer 5) (integer 7))))
  returns 24)
(confirm that (val (rem (mul (integer 2) (add (integer 5) (integer 7))) (integer 5)))
  returns 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun op-to-method-name (op)
  "If OP is an operator namin one of integer's methods, return the method name."
  (cl-case op
    (+ 'add)
    (- 'sub)
    (* 'mul)
    (/ 'div)
    (% 'rem)))

(defun transform-fun (expr)
  (if-let ((method-name (op-to-method-name expr)))
    method-name ; `(method-name ,method-name)
    expr))

(defmacro infix (&rest args)
  (prndiv)
  (prn "START: %s" args)
  (prndiv)
  (let ((transformed (transform-tree #'always #'transform-fun args)))
    `(infix-helper ,@transformed)))

(defmacro infix-helper (&rest args)
  (let (it)
    (while args
      (let ((expr (pop args)))
        (prndiv)
        (prn "it        = %s" it)
        (prn "expr      = %s" expr)
        (cond
          ((integerp expr)
            (if (fun? it)
              (setq it (funcall it (integer expr)))
              (setq it (integer expr))))
          ((symbolp expr)
            (let ((method (get-method it expr)))
              (prn "pushing %s onto %s" method it)
              (setq it method)
              (prn "pushed method")))
          (t (error "error"))
          )))
    (prndiv)
    (prn "final it  = %s" it)
    (prndiv)))

;; (infix 4 * 5 + 3)

