;; -*- lexical-binding: t; fill-column: 90; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--trees)
(require 'peter-norvigs-funs--objects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((seed (car (current-time))))
  (defun myrand (limit)
    (% (setq seed (logand #xffff
                    ((lambda (z) (if (positive? z) z (+ z -32768)))
                      (* seed 899.))))
      limit)))

(let (alist)
  (dotimes (x 20)
    (prn "%d" (myrand 3))))


(defun alist-has? (key alist)
  "Check if ALIST contains KEY."
  (let ((entry (assoc key alist)))
    (not (null entry))))

;; Example usage:
(setq my-alist '((a . t) (b . nil)))

(alist-has? 'a my-alist) ; Returns t
(alist-has? 'b my-alist) ; Returns t
(alist-has? 'c my-alist) ; Returns nil
