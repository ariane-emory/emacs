;; -*- lexical-binding: nil; fill-column: 90; lisp-indent-offset: 2; eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'aris-funs--alists)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--lists)
(require 'aris-funs--match-pattern)
(require 'aris-funs--match-pattern2)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--stacks)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maybe macro:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro maybe (type val)
  "Return VAL when it is of type TYPE, otherwise return nil."
  `(when
     (cl-typep ,val
       ,(if (and (symbolp type) (get type 'cl-deftype-satisfies)) `',type type))
     ,val))

(setq foo 7)
(setq ty 'integer)
(maybe 'integer foo)  ; Works
(maybe integer foo)   ; Works
(maybe ty foo)        ; Works

(if-let ((x (when (integerp foo) foo)))
  (prn "yes: %S" x)
  (prn "no!"))

(if-let ((x (maybe 'integer foo)))
  (prn "yes: %S" x)
  (prn "no!"))

(if-let ((x (maybe integer foo)))
  (prn "yes: %S" x)
  (prn "no!"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun* (name arglist &rest body)
  "Like defun, but with the option of type checking (but only for the mandatory
parameters, for the moment ."
  (let* ( new-arglist type-checks
          (remaining-arglist arglist)
          (remaining-arglist
            (catch 'break-loop
              (while remaining-arglist
                ;; peek and bail if head is a lambda list keyword:
                (when (member (car remaining-arglist) '(&optional &key &allow-other-keys))
                  (throw 'break-loop remaining-arglist))
                ;; pop the head and examine it:
                (let ((arg (pop remaining-arglist)))                                  
                  (if-let ( (var (car-safe arg))
                            (_ (and var
                                 (symbolp var)
                                 (length= arg 3)
                                 (eq : (nth 1 arg))))
                            (ty (nth 2 arg))
                            (_ (and ty (symbolp ty))))
                    ;; then add a type checking form to TYPE-CHECKS:
                    (progn
                      (push `(cl-check-type ,var ,ty) type-checks)
                      (push var new-arglist))
                    ;; else just add the arg to NEW-ARGLIST:
                    (push arg new-arglist)))))))
    ;; if any TYPE-CHECKS were found...
    (if (not type-checks)
      ;; then expand into a normal defun:
      `(defun ,name ,arglist ,@body)
      ;; else tamper with the body before expansion to prepend TYPE-CHECKS onto BODY:
      (let* ( (new-arglist (append (nreverse new-arglist) remaining-arglist))
              (type-checks (nreverse type-checks))
              (parse (byte-run--parse-body body t))
              (docstring (nth 0 parse))
              (declare-form (nth 1 parse))
              (interactive-form (nth 2 parse))
              (body (nth 3 parse))
              (warnings (nth 4 parse))
              (new-body ( append
                          (when docstring (list docstring))
                          (when declare-form (list declare-form))
                          (when interactive-form (list interactive-form))
                          (when warnings (list warnings))
                          type-checks
                          body)))
        `(defun ,name 
           ,new-arglist  
           ,@new-body)))))

;; define a function with type checks using the macro:
(defun* foo ((num : number) (pow : integer) &optional print-message)
  "A silly function to raise the number NUM to the integral power POW.

This is marked as interactive for no good reason other than to test if
INTERACTIVE-FORM is handled properly when defun* builds NEW-BODY and is
marked pure mainly to test if DECLARE-FORM is handled properly."
  (declare (pure t))
  (interactive)
  (let ((res (expt num pow)))
    (when print-message (message "%s to the power of %d is %s." num pow res))
    res))

;; try it out;
(foo 2.5 3 t) ;; ⇒ 15.625 and also prints "2.5 to the power of 3 is 15.625.".
(foo 2.5 3.5 t) ;; signals (wrong-type-argument integer 3.5 pow).
(if-let ((res (maybe integer (foo 4 3 t))))
  (message "Result %S is an integer." res)
  (message "Result was not an integer.")) ;; prints "Result 64 is an integer."
