;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-fib-benchmarks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun munge-arglist (prepend-required-args prepend-optional-args arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (prndiv)
  (prn "initial arglist: %s" arglist)
  (let* ( (arglist (cons :DUMMY arglist))
          (pos arglist)
          optionals
          rest)
    (while pos
      (cond
        ((eq (second pos) '&optional)
          (setq optionals (cddr pos))
          (setcdr pos nil)) ; snip into two separate lists. 
        ((eq (second pos) '&rest)
          (setq rest (cdr pos)) 
          (setcdr pos nil))) ; snip into two separate lists. 
      (pop pos))
    (pop arglist) ; pop :DUMMY.
    (prndiv)
    (prn "required:        %s" arglist)
    (prn "optionals:       %s" optionals)
    (prn "rest:            %s" rest)
    (let* ( (optionals (when (or prepend-optional-args optionals)
                         `(&optional ,@prepend-optional-args ,@optionals)))
            (arglist `( ,@prepend-required-args
                        ,@arglist
                        ,@optionals
                        ,@rest)))
      (prn "final:           %s" arglist)
      arglist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; working cases:
;; (munge-arglist '(db-sym) '(db-prop) '(x y &optional z)) ;; => (db-sym x y &optional db-prop &optional z)
;; (munge-arglist '(db-sym) '(db-prop) '(x y)) ;; => (db-sym x y &optional db-prop)
;; (munge-arglist '(db-sym) '(db-prop) '(&optional z)) ;; => (db-sym &optional db-prop &optional z)
;; (munge-arglist '(db-sym) '(db-prop) '(x y &optional z &rest body)) ;; => (db-sym x y &optional db-prop &optional z &rest body)
;; (munge-arglist '(db-sym) '(db-prop) '(x y &rest body)) ;; => (db-sym x y &optional db-prop &rest body)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (munge-arglist nil nil '(x y &rest body)) ;; => (x y &optional &rest body)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-arglist (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ( (arglist (cons :DUMMY arglist))
          (pos arglist)
          required
          optional
          rest)
    (prn "")
    (prn "initial arglist: %s" arglist)
    
    (while pos
      (let ((head (first pos)))
        (prndiv)
        (prn "head:     %s" head)
        (prn "pos:      %s" pos)
        (prn "required: %s" required)
        (prn "optional: %s" optional)
        (prn "rest:     %s" rest)
        
        ;; (cond
        ;;   ((eq (second pos) '&optional)
        ;;     (setq required arglist)
        ;;     (setq optional (cddr pos))
        ;;     (setcdr pos optional)) ; snip into two separate lists. 
        ;;   ((eq (second pos) '&rest)
        ;;     (setq rest (cddr pos)) 
        ;;     (setcdr pos nil))) ; snip into two separate lists. 
        (pop pos)))
    (pop required) ; pop :DUMMY.
    (prndiv)
    ;; (prn "required:        %s" arglist)
    ;; (prn "optional:        %s" optional)
    ;; (prn "rest:            %s" rest)
    (list required optional rest)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parse-arglist '(x y)) ;; (nil nil nil)
(parse-arglist '(x y &optional z &rest rest)) ;; (nil nil nil)
(parse-arglist '(x y &optional z)) ;; (nil nil nil)
(parse-arglist '(x y &rest rest)) ;; (nil nil nil)
(parse-arglist '()) ;; (nil nil nil)
(parse-arglist '(&optional z &rest rest)) ;; (nil nil nil)
(parse-arglist '(&optional z)) ;; (nil nil nil)
(parse-arglist '(&rest rest)) ;; (nil nil nil)

