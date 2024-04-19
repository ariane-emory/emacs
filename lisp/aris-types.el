;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-deftype seq-of-length (n)
  "Type specifier for things of length N."
  `(and sequence (satisfies (lambda (seq) (length= seq ,n)))))

(cl-deftype seq-of-ty (ty)
  "Type specifier for things with elements of type TY."
  `(and sequence (satisfies (lambda (seq) (cl-every (lambda (x) (cl-typep x ',ty)) seq)))))

(cl-deftype vec-of-2-integers ()
  "Type specifier for lists of length 2 containing integers."
  `(and vector (seq-of-length 2) (seq-of-ty integer)))

(cl-deftype pair-of-integers ()
  "Type specifier for cons pairs of integers."
  `(and cons (satisfies (lambda (x) (and (integerp (car x)) (integerp (cdr x)))))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-types)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
