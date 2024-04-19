;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun non-nil-symbol-p (x)
  "Return non-nil if X is a symbol that is not nil."
  (and (symbolp x) (not (null x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (non-nil-symbol-p 'foo) returns t)
(confirm that (non-nil-symbol-p nil) returns nil)
(confirm that (non-nil-symbol-p 3) returns nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-deftype non-nil-symbol ()
  "Type specifier for non-nil symbols."
  '(satisfies non-nil-symbol-p))

(cl-deftype seq-of-length (n)
  "Type specifier for things of length N."
  `(and sequence (satisfies (lambda (seq) (length= seq ,n)))))

;; (cl-deftype pair-of-integers ()
;;   "Type specifier for cons pairs of integers."
;;   '(and pair (satisfies (lambda (x) (and (integerp (car x)) (integerp (cdr x)))))))

(cl-deftype seq-of-ty (ty)
  "Type specifier for things with elements of type TY."
  `(and sequence (satisfies (lambda (seq) (cl-every (lambda (x) (cl-typep x ',ty)) seq)))))

(cl-deftype vec-of-2-integers ()
  "Type specifier for lists of length 2 containing integers."
  '(and vector (seq-of-length 2) (seq-of-ty integer)))

(cl-deftype pair ()
  "Type specifier for dotted cons pairs."
  '(and cons (not (satisfies (lambda (o) (listp (cdr o)))))))

(cl-deftype pair-of (ty1 &optional ty2)
  "Type specifier for dotted cons pairs."
  `(and pair
     (satisfies (lambda (o) (and
                         (cl-typep (car o) ',ty1)
                         (cl-typep (cdr o) ',(if (eq '* ty2) ty1 ty2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(confirm that (cl-typep '(1 2)       'pair-of-integers) returns nil)
(confirm that (cl-typep '(1 . 2)     'pair-of-integers) returns t)
(confirm that (cl-typep '(1 . 2.0)   'pair-of-integers) returns nil)
(confirm that (cl-typep '(1 . foo)   '(pair-of integer non-nil-symbol)) returns t)
(confirm that (cl-typep '(1 . 3)     '(pair-of integer non-nil-symbol)) returns nil) 
(confirm that (cl-typep '(1 . 3)     '(pair-of integer)) returns t)
(confirm that (cl-typep '(foo . bar) '(pair-of integer)) returns nil)
(confirm that (cl-typep '(foo . bar) '(pair-of non-nil-symbol)) returns t)
(confirm that (cl-typep '(foo . nil) '(pair-of non-nil-symbol)) returns nil)
(confirm that (cl-typep '(foo . bar) '(pair-of symbol)) returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-types)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

