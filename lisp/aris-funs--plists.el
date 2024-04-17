;; -*- lexical-binding: nil; fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'cl-lib)
(require 'aris-funs--unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-plist (keys vals)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Build a plist from KEYS and VALS."
  (unless (list? keys) (error "KEYS must be a list."))
  (unless (list? vals) (error "VALS must be a list."))
  (unless (>= (length keys) (length vals)) (error "KEYS must be at least as long as VALS."))
  (when keys
    (let* ( (result (list (pop keys) (pop vals)))
            (tail (cdr result)))
      (while keys
        (let ((new-tail (list (pop keys) (pop vals))))
          (setq tail (cdr (rplacd! tail new-tail)))))
      result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (make-plist '(d a c b) '(4 1 3 2)) returns (d 4 a 1 c 3 b 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro plist-put! (plist-symbol key value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Add or update a key-value pair in a plist."
  `(setq ,plist-symbol (plist-put ,plist-symbol ,key ,value)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((plist (make-plist '(d a c b) '(4 1 3 2))))
  (confirm that (plist-put! plist 'a 10) returns (d 4 a 10 c 3 b 2))
  (confirm that (plist-put! plist 'b 20) returns (d 4 a 10 c 3 b 20))
  (confirm that plist returns (d 4 a 10 c 3 b 20)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro plist-remove!(plist-symbol key)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Remove a key and it's associated value - this actually removes it, it doesn't just set it to nill"
  `(let (new-plist (old-plist ,plist-symbol))
     (while old-plist
       (let ((k (car old-plist))
              (v (cadr old-plist)))
         (unless (eq k ,key)
           (plist-put! new-plist k v))
         (setq old-plist (cddr old-plist))))
     (setq ,plist-symbol new-plist)))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq plist (make-plist '(d a c b) '(4 1 3 2)))
;; (plist-remove! plist 'a)
;; (plist-remove! plist 'b)
;; plist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-remove (plist key)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Remove a key and it's associated value. This version returns a new plist and
does not modify the original."
  (let (new-plist (old-plist plist))
    (while old-plist
      (let ((k (car old-plist))
             (v (cadr old-plist)))
        (unless (eq k key)
          (plist-put! new-plist k v))
        (setq old-plist (cddr old-plist))))
    new-plist))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq plist (make-plist '(d a c b) '(4 1 3 2)))
;; (plist-remove plist 'a)
;; (plist-remove (plist-remove plist 'c) 'b)
;; plist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro plist-sort! (plist-symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Sort a plist by key."
  `(setq ,plist-symbol
     (apply 'append
       (sort (cl-loop for (key value) on ,plist-symbol by #'cddr
               collect (list key value))
         (lambda (a b) (string< (car a) (car b)))))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq plist (make-plist '(d a c b) '(4 1 3 2)))
;; (plist-sort! plist)
;; plist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-sort (plist-symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Sort a plist by key. This version returns a new plist and does not modify the original."
  (apply 'append
    (sort (cl-loop for (key value) on plist-symbol by #'cddr
            collect (list key value))
      (lambda (a b) (string< (car a) (car b))))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq plist (make-plist '(d a c b) '(4 1 3 2)))
;; (plist-sort plist)
;; plist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-keys (plist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Extracts the keys from a plist PLIST."
  (unless (listp plist) (error "PLIST must be a list"))
  (when plist
    (let* ( (result (list (car plist)))
            (tail result))
      (setq plist (cddr plist))
      (while plist
        (let ((new-tail (list (pop plist))))
          (setq tail (setcdr tail new-tail)))
        (pop plist))
      result)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq plist (make-plist '(d a c b) '(4 1 3 2)))
;; (plist-keys plist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-vals (plist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Extracts the values from a plist PLIST."
  (unless (listp plist)          (error "PLIST must be a list"))
  (unless (even? (length plist)) (error "PLIST must have an even number of elements"))
  (plist-keys (cdr plist)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq plist (make-plist '(d a c b) '(4 1 3 2)))
;; (plist-vals plist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plist-to-alist (plist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Convert a plist PLIST to a dotted alist. If the number of elements in plist
is odd, the last cons in the resulting alist's value cell will be nil."
  (unless (list? plist)          (error "PLIST must be a list"))
  (when plist
    (let* ( (result (list (cons (car plist) (cadr plist))))
            (tail   result)
            (plist  (cddr plist)))
      (while plist
        (let ((new-alist-item (list (cons (pop plist) (pop plist)))))
          (setq tail (rplacd! tail new-alist-item))))
      result)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq plist (make-plist '(d a c b) '(4 1 3 2)))
;; (plist-to-alist plist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (progn
    (if (not (boundp 'plist))
      (setq plist nil))
    (plist-put! plist 'a 1)
    (plist-put! plist 'b 2)
    (plist-put! plist 'c 3)
    (plist-remove! plist 'a)
    (plist-remove! plist 'b)
    (plist-remove! plist 'c)
    (plist-put! plist 'c 3)
    (plist-put! plist 'a 1)
    (plist-put! plist 'b 2)
    (plist-sort! plist)
    (plist-keys plist)
    (plist-vals plist)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--plists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
