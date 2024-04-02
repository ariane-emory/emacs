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

(cl-defun indent-string-lines (str &optional (n 2) (char ?\ ))
  "Indent each line in a multiline string by N CHARs."
  (let ( (lines (split-string str "\n"))
         (indent (make-string n char)))
    (mapconcat (lambda (line) (concat indent line)) lines "\n")))

(defun format-group-as-lines (group)
  (let (result)
    (dolist (row group)
      (push (format "%s ⇒" (string-trim (pp-to-string (car row)))) result)
      (let ((lines (butlast (split-string (pp-to-string (cdr row)) "\n"))))
        (push (format "  %s" (car lines)) result)
        (dolist (line (cdr lines))
          (push (format "  %s" line) result))))
    (nreverse result)))

(defun format-table-as-lines ()
  (let (result)
    (dolist (group *lust-style-syntax--pattern-dispatch-table*)
      (dolist (line (format-group-as-lines (cdr group)))
        (push (format "  %s" line) result)))
    (nreverse result)))
(format-table-as-lines)


(defun format-group-as-string (group)
  (string-join (format-group-as-lines group) "\n"))

(defun print-group (group)
  (print (format-group-as-string group))
  nil)

(defun print-table ()
  (dolist (line (format-table-as-lines))
    (print line)))

(print-table)

;; (indent-string-lines (format-group-as-string (lust-style-syntax--get-group 'fib)))

;; (indent-string-lines (print-group (format-group-as-string (lust-style-syntax--get-group 'fib))))
;; (fib 8)
;; (symbol-function 'square)
;; (symbol-plist 'square)
;; (print-group )
