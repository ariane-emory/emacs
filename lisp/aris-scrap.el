;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--alist-funs)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--error-when-and-error-unless)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (pd--reset)
  (def (fib 0) 0)
  (def (fib 1) 1)
  (def (fib n)  (+ (fib (- n 1)) (fib (- n 2))))
  (def (double n) (+ n n))
  (def (square y) (* y y))
  ;; (def (double-square y) (double 2 (square y)))
  ;; (double-square 3)

  (let ( (*match-pattern--verbose* t)
         (*match-pattern2--verbose* t))
    ;; (error-unless "You broke (fib 4): %s" '(it) (= 3 (fib 4)))
    ;; (error-unless "You broke (fib 10): %s" '(it) (= 55 (fib 10)))
    ;; (error-unless "You broke (double 9): %s" '(it) (= 18 (double 9)))
    (error-unless "You broke (square 7): %s" '(it) (= 49 (square 7))))
  
  (message "Printing the table:")
  (pd--print-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pd--get-group 'fib)
(pd--print-group (pd--get-group 'fib))
(pd--format-group-as-lines (pd--get-group 'fib))
(pd--format-group-as-string (pd--get-group 'fib))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (match2 '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))

    (let ( (*match-pattern--verbose* t)
           (*match-pattern--merge-duplicate-alist-keys* nil)
           (*match-pattern--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

    (let ( (*match-pattern--verbose* t)
           (*match-pattern--merge-duplicate-alist-keys* t)
           (*match-pattern--use-dotted-pairs-in-result* nil))
      (match2 '((* . a) 6 7 (even? . b)) '(1 2 3 4 5 6 7 8)))

    (aris-merge-duplicate-alist-keys '((a 1) (a 2) (a 3) (a 4) (a 5) (b 8)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (=> 8
;;   -> (+ 3 _)
;;   (message "It's %s" _)
;;   -> (* 2 _)
;;   (message "Now it's %s" _)
;;   (if (> _ 25) (return 100))
;;   (message "And now it's %s" _)
;;   (return (+ _ 50))
;;   (message "Finally it's %s" _)
;;   ->
;;   (- _ 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pipe 8
  (+ 3 _)
  :
  (message "A message! _ = %s" _)
  (* 2 _)
  (- _ 1))

(let ((x 5))
  (pipe ((x (+ 3 x)))
    (+ 3 x)
    (* 2 x)
    (- x 1)))

(pipe ((x))
  8 
  (+ 3 x)
  (* 2 x)
  (- x 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro |(head &rest tail)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "`pipe' with optional let-like binding/symbol naming.
(pipe 
  8
  (+ 3 it)
  :(message \"A message! it = %s\" it)
  (* 2 it)
  (- it 1))"
  (let* ((head-is-spec
           (and
             (consp head)
             (consp (car head))
             (length> (car head) 0)
             (length< (car head) 3)))
          (sym (if head-is-spec (caar head) pipe-default-var-sym))
          (init-form (when head-is-spec (cadar head)))
          (body (if head-is-spec tail (cons head tail))))
    ;;(debug)
    ;; (message "head: %s" head)
    ;; (message "head-is-spec: %s" head-is-spec)
    ;; (message "sym: %s" sym)
    ;; (message "init-form: %s" init-form)
    ;; (message "body: %s" body)
    body
    `(let ( (,sym ,init-form)
            (ignore-flag nil))
       ;;(fset 'return (lambda () (throw 'return nil)))
       (catch 'return
         (mapr ',body
           (lambda (expr)
             (cond
               ((eq expr :) (setq ignore-flag t))
               ;;((eq expr 'return) (throw 'return nil))
               (ignore-flag
                 (let ((fun
                         `(lambda (_)
                            (cl-flet ((return (_) (throw 'return _))) ,expr))))
                   (funcall fun _))
                 (setq ignore-flag nil))
               (t
                 ;;(throw 'bang nil)
                 (let ((fun
                         `(lambda (_)
                            (cl-flet ((return (_) (throw 'return _))) ,expr))))
                   (setq ,sym (funcall fun _)))
                 (setq ignore-flag nil)))))
         (throw 'return ,sym)))))


(| ((x 2))
  8
  (+ 3 x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(|
  8
  (+ 3 _)
  :(message "It's %s" _) 
  (* 2 _)
  :(message "Now it's %s" _)
  :(if (> _ 25) (return 100))
  :(message "And now it's %s" _)
  ;;:(return (+ _ 50))
  :(message "Finally it's %s" _)
  (- _ 1))

