;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun munge-arglist (prepend-required-args prepend-optional-args arglist)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (prndiv)
;;   (prn "initial arglist: %s" arglist)
;;   (let* ( (arglist (cons :DUMMY arglist))
;;           (pos arglist)
;;           optionals
;;           rest)
;;     (while pos
;;       (cond
;;         ((eq (second pos) '&optional)
;;           (setq optionals (cddr pos))
;;           (setcdr pos nil)) ; snip into two separate lists. 
;;         ((eq (second pos) '&rest)
;;           (setq rest (cdr pos)) 
;;           (setcdr pos nil))) ; snip into two separate lists. 
;;       (pop pos))
;;     (pop arglist) ; pop :DUMMY.
;;     (prndiv)
;;     (prn "required:        %s" arglist)
;;     (prn "optionals:       %s" optionals)
;;     (prn "rest:            %s" rest)
;;     (let* ( (optionals (when (or prepend-optional-args optionals)
;;                    ~/^      `(&optional ,@prepend-optional-args ,@optionals)))
;;             (arglist `( ,@prepend-required-args
;;                         ,@arglist
;;                         ,@optionals
;;                         ,@rest)))
;;       (prn "final:           %s" arglist)
;;       arglist)))
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
  "Parse an argument list into required, optional, and rest sections."
  (let ( (section 'required)
         (required nil)
         (optional nil)
         (rest nil))
    (dolist (arg arglist)
      (cond
        ((eq arg '&optional)
          (setq section 'optional))
        ((eq arg '&rest)
          (setq section 'rest))
        (t
          (push arg
            (cl-case section
              (required required)
              (optional optional)
              (rest rest))))))
    `( ,(nreverse required)
       ,(nreverse optional)
       ,(nreverse rest))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(confirm that (parse-arglist '(x y &optional z)) returns ((x y) (z) nil))
(confirm that (parse-arglist '(x y &optional z &rest rest)) returns ((x y) (z) (rest)))
(confirm that (parse-arglist '(x y &rest rest)) returns ((x y) nil (rest)))
(confirm that (parse-arglist '()) returns (nil nil nil))
(confirm that (parse-arglist '(&optional z &rest rest)) returns (nil (z) (rest)))
(confirm that (parse-arglist '(&optional z)) returns (nil (z) nil))
(confirm that (parse-arglist '(&rest rest)) returns (nil nil (rest)))

