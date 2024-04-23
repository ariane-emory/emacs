;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my integer class:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(n:define-class integer (value) nil
  (val () value)
  (fmt () (format "(integer %d)" value))
  (add (other) (integer (+ value (val other))))
  (sub (other) (integer (- value (val other))))
  (mul (other) (integer (* value (val other))))
  (div (other) (integer (/ value (val other))))
  (pow (other) (integer (expt value (val other))))
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
    (+ ''add)
    (- ''sub)
    (* ''mul)
    (/ ''div)
    (^ ''pow)
    (% ''rem)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infix-transform-fun (expr)
  (if-let ((method-name (op-to-method-name expr)))
    method-name
    expr))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (transform-tree #'always #'infix-transform-fun '(2 + (3 * y)))
  returns (2 'add (3 'mul y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ## (&rest args)
  (prndiv)
  (prn "START: %s" args)
  (let ((transformed (transform-tree #'always #'infix-transform-fun args)))
    `(with-indentation (val (infix-helper ',transformed)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infix-helper (lst)
  (let (res)
    (prndiv)
    (prn "HELP:  %s" lst)
    (prndiv)    
    (with-indentation
      (while lst
        (let ((expr (pop lst)))
          (prn "RES:  %s" res)
          (prn "EXPR: %s" expr)
          (cond
            ((and (integer? expr) res)
              (prn "EXPR IS INTEGER AND WE'RE HOLDING A METHOD!")
              (setq res (funcall res (integer expr))))
            ((integer? expr)
              (prn "EXPR IS INTEGER!")
              (setq res (integer expr)))
            ((and (list? expr) (eq (car expr) 'quote))
              (prn "EXPR IS METHOD NAME %s!" (second expr))
              (setq res (n:get-method res (second expr))))
            ((and (list? expr) res)
              (prn "EXPR IS LIST AND WE'RE HOLDING A METHOD!")
              (setq res (funcall res (infix-helper expr))))
            ((list? expr)
              (prn "EXPR IS LIST!")
              (prn "CAR IS %s!" (car expr))
              (setq res (infix-helper expr))
              (prn "RES: %s" res))
            (res
              (prn "EXPR IS OTHER %s!" expr)
              (setq res (funcall res (integer (symbol-value expr)))))
            (t
              (setq res (integer (symbol-value expr))))))
        (prndiv))
      res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq y 80)
(setq z 2)
(## (y + 3) + (7 ^ (z + 1))) ;; => 426




