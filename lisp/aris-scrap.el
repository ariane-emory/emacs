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
  (let ((transformed (transform-tree #'always #'transform-fun args)))
    `(infix-helper ,@transformed)))

(defmacro infix-helper (&rest args)
  (let (it)
    (while args
      (let ((expr (pop args)))
        (prndiv)
        (prn "it       = %s" it)
        ;; (cond
        ;;   ((and it (is? it 'integer)) (prn "it (int) = %s" (val it)))
        ;;   (t (prn "it       = %s" it)))
        (prn "expr     = %s" expr)
        (cond
          ((integerp expr) (setq it (integer expr)))
          ((symbolp expr)
            (setq it (get-method it expr)))
          (t (error "error"))
          )))))

;; (infix 4 * 5)

(get-method (integer 4) 'mul)
(fmt (integer 4))
