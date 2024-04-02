;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); eval: (company-posframe-mode -1) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pp)
(require 'cl-lib)
(require 'aris-funs--lust-style-syntax)
(require 'aris-funs--error-when-and-error-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pd-reset)

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

*lust-style-syntax--pattern-dispatch-table*

;; ((square
;;    ((square y)
;;      (* y y)))
;;   (double
;;     ((double n)
;;       (+ n n)))
;;   (fib
;;     ((fib 0)
;;       0)
;;     ((fib 1)
;;       1)
;;     ((fib n)
;;       (+
;;         (fib
;;           (- n 1))
;;         (fib
;;           (- n 2))))))

;; (fib 0) ⇒
;;   (0)
;; (fib 1) ⇒
;;   (1)
;; (fib n) ⇒
;;   ((+
;;      (fib
;;        (- n 1))
;;      (fib
;;        (- n 2))))

(defun print-group (group-symbol)
  (print "Table: %s" *lust-style-syntax--pattern-dispatch-table*)
  (let* ((group (lust-style-syntax--get-patterns-for-group group-symbol)))
    (print "Looked up group for '%s and found:" group-symbol)
    (with-message-indent
      (dolist (row group)
        (print "%s ⇒" (string-trim (pp-to-string (car row))))
        (let ( (lines
                 (butlast (split-string (pp-to-string (cdr row)) "\n"))))
          (print "  %s" (car lines))
          (dolist (line (cdr lines))
            (print "  %s" line)))))))


(print-group 'fib)

(fib 8)
(symbol-function 'square)
(symbol-plist 'square)
