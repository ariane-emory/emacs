;; -*- lexical-binding: nil; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun munge-arglist (prepend-required-args prepend-optional-args arglist)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (prndiv)
;;   (prn "initial arglist: %s" arglist)
;;   (let* ( (arglist (cons :DUMMY arglist))
;;           (pos arglist)
;;           optional
;;           rest)
;;     (while pos
;;       (cond
;;         ((eq (second pos) '&optional)
;;           (setq optional (cddr pos))
;;           (setcdr pos nil)) ; snip into two separate lists. 
;;         ((eq (second pos) '&rest)
;;           (setq rest (cdr pos)) 
;;           (setcdr pos nil))) ; snip into two separate lists. 
;;       (pop pos))
;;     (pop arglist) ; pop :DUMMY.
;;     (prndiv)
;;     (prn "required:        %s" arglist)
;;     (prn "optional:       %s" optional)
;;     (prn "rest:            %s" rest)
;;     (let* ( (optional (when (or prepend-optional-args optional)
;;                    ~/^      `(&optional ,@prepend-optional-args ,@optional)))
;;             (arglist `( ,@prepend-required-args
;;                         ,@arglist
;;                         ,@optional
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
(defun munge-arglist (prepend-required-args prepend-optional-args prepend-rest-args arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ( (parsed    (parse-arglist arglist))
          (required  (first parsed))
          (optional  (second parsed))
          (rest      (third parsed))
          (required
            (if (or prepend-required-args required)
              `(,@prepend-required-args ,@required)))
          (optional
            (if (or prepend-optional-args optional)
              `(&optional ,@prepend-optional-args ,@optional)))
          (rest
            (if (or prepend-rest-args rest)
              `(&rest ,@prepend-rest-args ,@rest))))
    (prndiv)
    (prn "arglist:   %s" arglist)
    (prn "parsed:    %s" parsed)
    (prn "required:  %s" required)
    (prn "optional: %s" optional)
    (prn "rest:      %s" rest)
    (let ((res (append required optional rest)))
      (prn "res:       %s" res)
      res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (munge-arglist '(db-sym) '(db-prop) nil '(x y &optional z))
  returns (db-sym x y &optional db-prop z))
(confirm that (munge-arglist '(db-sym) '(db-prop) nil '(x y &optional z &rest rest))
  returns (db-sym x y &optional db-prop z &rest rest))
(confirm that (munge-arglist '(db-sym) '(db-prop) nil '(x y &rest rest))
  returns (db-sym x y &optional db-prop &rest rest))
(confirm that (munge-arglist '(db-sym) '(db-prop) nil '())
  returns (db-sym &optional db-prop))
(confirm that (munge-arglist '(db-sym) '(db-prop) nil '(&optional z &rest rest))
  returns (db-sym &optional db-prop z &rest rest))
(confirm that (munge-arglist '(db-sym) '(db-prop) nil '(&optional z))
  returns (db-sym &optional db-prop z))
(confirm that (munge-arglist '(db-sym) '(db-prop) nil '(&rest rest))
  returns (db-sym &optional db-prop &rest rest))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
