;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--pattern-dispatch)
(require 'aris-funs--error-when-and-error-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pd--reset)
(def (fib 0) 0)
(def (fib 1) 1)
(def (fib n) (message "hello") (+ (fib (- n 1)) (fib (- n 2))))
(def (double n) (+ n n))
(def (square y) (* y y))
;; (def result
;;   (list
;;     (fib 4)
;;     (fib 6)
;;     (fib 8)))
(square 7)
(funcall #'square 8)
*pd--pattern-dispatch-table*
(message "Printing the table:")
(pd--print-table)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pd--get-group 'fib)
'(fib
   ((fib 0)
     0)
   ((fib 1)
     1)
   ((fib n)
     (message "hello")
     (+
       (fib
         (- n 1))
       (fib
         (- n 2)))))


(let* ( (group (pd--get-group 'fib))
        (group-name (car group))
        (group-rows (cdr group)))
  (pd--print "Group: %s" group-rows)
  (pd--print "Group name: %s" group-name)
  (pd--print "Group rows: %s" group-rows)
  (pd--print "[%s]" group-name)
  (dolist (row group-rows)
    (let ((pattern (car row)))
      (pd--print "  %s ⇒" pattern)
      (let ((lines (butlast (split-string (pp-to-string (cdr row)) "\n"))))
        (dolist (line lines)
          (pd--print "    %s" line)))
      
      ;; (let ( (pattern (car row))
      ;;        (body (cdr row)))
      ;;   (pd--print "   ⇒ %s" pattern))
      )))



(pd--print (pd--format-group-as-string (pd--get-group 'fib) ?\. 2))








