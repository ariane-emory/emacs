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
(def (fib n) (+ (fib (- n 1)) (fib (- n 2))))
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

;; (indent-string-lines (format-group-as-string (pattern-dispatch--get-group 'fib)))

;; (indent-string-lines (print-group (format-group-as-string (pattern-dispatch--get-group 'fib))))
;; (fib 8)
;; (symbol-function 'square)
;; (symbol-plist 'square)
;; (print-group )
