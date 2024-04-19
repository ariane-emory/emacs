;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-deftype seq-of-length (n)
  "Type specifier for things of length N."
  `(and sequence (satisfies (lambda (seq) (length= seq ,n)))))

(cl-deftype pair-of-integers ()
  "Type specifier for cons pairs of integers."
  '(and pair (satisfies (lambda (x) (and (integerp (car x)) (integerp (cdr x)))))))

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
     (satisfies (lambda (o)
                  (not (null (and (cl-typep (car o) ',ty1)
                             (cl-typep (cdr o) ',(if (eq '* ty2) ty1 ty2)))))))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-typep '(1 . foo) '(pair-of integer symbol)) ;; t
(cl-typep '(1 . 3)   '(pair-of integer symbol)) ;; nil
(cl-typep '(1 . 3)   '(pair-of integer)) ;; t
(cl-typep '(foo . bar) '(pair-of symbol)) ;; (wrong-type-argument number-or-marker-p bar)

(symbol-plist 'pair-of)
;; =>
(cl-deftype-handler
  (lambda
    (ty1 &rest --cl-rest--)
    "Type specifier for dotted cons pairs.\n\n(fn TY1 &optional TY2)"
    (let*
      ((ty2
         (if --cl-rest--
           (car-safe
             (prog1 --cl-rest--
               (setq --cl-rest--
                 (cdr --cl-rest--))))
           '*)))
      (progn
        (if --cl-rest--
          (signal 'wrong-number-of-arguments
            (list nil
              (+ 2
                (length --cl-rest--)))))
        (list 'and 'pair
          (list 'satisfies
            (list 'lambda
              '(o)
              (list 'not
                (list 'null
                  (list 'and
                    (list 'cl-typep
                      '(car o)
                      (list 'quote ty1))
                    (list 'cl-typep
                      '(cdr o)
                      (list 'quote
                        (or ty2 ty1))))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (confirm that (cl-typep '(1 2) 'pair-of-integers) returns nil)
  (confirm that (cl-typep '(1 . 2) 'pair-of-integers) returns t)
  (confirm that (cl-typep '(1 . 2.0) 'pair-of-integers) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (provide 'aris-types)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
