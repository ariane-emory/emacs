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
  `(when (cl-typep ,val ,type)
     ,val))

(defmacro maybe (type val)
  "Return VAL when it is of type TYPE, otherwise return nil."
  (let ((type-form
          (if (and (symbolp type) (get type 'cl-deftype-satisfies))
            `',type
            type)))
    `(when (cl-typep ,val ,type-form)
       ,val)))

(defmacro maybe (type val)
  "Return VAL when it is of type TYPE, otherwise return nil."
  `(when (cl-typep ,val
           ,(if (and (symbolp type) (get type 'cl-deftype-satisfies))
              `',type
              type))
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
(defunt tsqr (('integer x))
  "Square an integer."
  (* x x))

(defun tsqr (x)
  "Square an integer."
  (let
    ((x (progn (cl-check-type x integer) x)))
    (* x x)))

(setq x 7)
(cl-check-type 7 integer)
(tsqr 3)
(tsqr "3")

(type-check-for-arg (integer x))
(type-check-for-arg x)

(defmacro defunt (name arglist &rest rest)
  (let (new-arglist type-checks)
    (dolist (arg arglist)
      (prn "defunt: arg is %S." arg)
      (let ((ty (car-safe arg)))
        (prn "defunt: ty is %S." ty)
        (if (and ty (symbolp ty))
          (progn
            (prn "defunt: It's a non-nil symbol.")
            `(cl-check-type ,(cadr arg) ,ty))
          (prn "defunt: It's NOT a non-nil symbol."))))
    (let ( (new-arglist (nreverse new-arglist))
           (type-checks (nreverse type-checks)))
      `(list
         ,new-arglist  
         ,type-checks))))

(defmacro type-check-for-arg (arg)
  "Check the type of a single typed arg."
  (prn "tcfa: arg is %S." arg)
  (let ((ty (car-safe arg)))
    (prn "tcfa: ty is %S." ty)
    (if-let ( (__ (and ty (symbolp ty)))
              (var (cadr arg)))
      `(cl-check-type ,var ,ty))))

(defun lambda-list-keyword-p (symbol)
  "Check if SYMBOL is a lambda list keyword."
  (member symbol '(&rest &optional &key &allow-other-keys)))

(defmacro defun* (name arglist &rest body)
  "Like defun, but with the option of type checking (only for the mandatory parameters, for the moment ."
  (let* (new-arglist type-checks (remaining-arglist arglist)
          (remaining-arglist (catch 'break-loop
                               (while remaining-arglist
                                 ;; peek and bail if head is a lambda list keyword:
                                 (when (lambda-list-keyword-p (car remaining-arglist))
                                   (throw 'break-loop remaining-arglist))
                                 ;; pop the head and examine it:
                                 (let ((arg (pop remaining-arglist)))                                  
                                   (prn "defun*: arg is %S." arg)                                
                                   (if-let ( (var (car-safe arg))
                                             (_ (and var
                                                  (symbolp var)
                                                  (length= arg 3)
                                                  (eq : (nth 1 arg))))
                                             (ty (nth 2 arg))
                                             (_ (and ty (symbolp ty))))
                                     ;; add a type checking form to TYPE-CHECKS:
                                     (progn
                                       (prn "defun*: ty is %S." ty)
                                       (prn "defun*: ty is a non-nil symbol.")
                                       (push `(cl-check-type ,var ,ty) type-checks)
                                       (push var new-arglist))
                                     ;; else just add the arg to NEW-ARGLIST:
                                     (prn "defun*: ty is NOT a non-nil symbol.")
                                     (push arg new-arglist)))))))
    (prn "new-arglist    is %S" new-arglist)
    (prn "remaining-arglist is %S" remaining-arglist)
    (if (not type-checks)
      ;; then expand into a normal defun:
      `(defun ,name ,arglist ,@body)
      ;; else tamper with the body to prepend TYPE-CHECKS:
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
        (prn "===========================================")
        (prn "defun*: new-arglist      is %S" new-arglist)
        (prn "defun*: type-checks      is %S" type-checks)
        (prn "defun*: pase             is %S" parse)
        (prn "defun*: docstring        is %S" docstring)
        (prn "defun*: declare-form     is %S" declare-form)
        (prn "defun*: interactive-form is %S" interactive-form)
        (prn "defun*: body             is %S" body)
        (prn "defun*: warnings         is %S" warnings)
        (prn "defun*: new-body         is %S" new-body)
        `(defun ,name 
           ,new-arglist  
           ,@new-body)))))

(defun* foo ((x : integer) y &optional z)
  "My docstring."
  (interactive)
  (* x y z))


;; expands into: 
(defun foo
  (x y &optional z)
  "My docstring."
  (interactive)
  (cl-check-type x integer)
  (* x y z))

(defun* foo (x y)
  (* x y))

;; expands into a normal defun:
(defun foo
  (x y)
  (* x y))




