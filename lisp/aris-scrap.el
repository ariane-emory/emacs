;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-fib-benchmarks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun munge-arglist (prepend-required-args prepend-optional-args arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (prndiv)
  (let* ( (arglist (cons :DUMMY arglist))
          (pos arglist)
          optionals)
    (while pos
      (when (eq (second pos) '&optional) ; no case for &rest yet, avoid it for now.
        (setq optionals (cddr pos)) ; snip into two separate lists. 
        (setcdr pos nil))
      (pop pos))
    (pop arglist) ; pop :DUMMY.
    (let* ((arglist `(db-sym ,@arglist &optional db-prop ,@optionals)))
      arglist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; working cases:
(munge-arglist '(db-sym) '(db-prop) '(x y &optional z)) ;; => (db-sym x y &optional db-prop z)
(munge-arglist '(db-sym) '(db-prop) '(x y)) ;; => (db-sym x y &optional db-prop)
(munge-arglist '(db-sym) '(db-prop) '(&optional z)) ;; => (db-sym &optional db-prop z)

;; non-working cases:
(munge-arglist '(db-sym) '(db-prop) '(x y &optional z &rest body)) ;; => (db-sym db-sym &optional db-prop)
