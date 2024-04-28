;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for processing arglists.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count-args (arglist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Count the minimum and maximum arguments described by ARGLIST and
return a pair of the form (MIN-ARGS . MAX-ARGS).

This only handles those lambda list keywords native to Emacs Lisp,
i.e. &optional and &rest, but will tolerate default values for optional
args."
  (let ( (min-args 0)
         (max-args 0)
         (seen-optional nil)
         (rest-arg nil))
    (while-let ((popped (pop arglist)))
      (cond
        ((eq popped '&optional)
          (setq seen-optional t))
        ((eq popped '&rest)
          (setq rest-arg t))
        (rest-arg)
        (seen-optional
          (cl-incf max-args)) ;; don't bother counting after &rest.
        (t
          (cl-incf min-args)
          (cl-incf max-args))))
    (cons min-args (if rest-arg nil max-args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (count-args '(foo &optional bar &rest baz)) returns (1))
(confirm that (count-args '(foo &optional bar baz)) returns (1 . 3))
(confirm that (count-args '(foo bar &optional baz)) returns (2 . 3))
(confirm that (count-args '(foo bar &optional (baz 9))) returns (2 . 3))
(confirm that (count-args '(foo bar &optional baz &rest quux)) returns (2))
(confirm that (count-args '(&rest quux)) returns (0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *arglists--emac-lisp-lambda-list-keywords*
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  '(&optional &rest)
  " A list of the lambda list keywords native to Emacs Lisp.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arg-names (arglist &optional user-lambda-list-keywords)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a list of the argument names in ARGLIST. This supports &optional
(with CL-style default values) and &rest."
  (let ((lambda-list-keywords
          (append *arglists--emac-lisp-lambda-list-keywords*
            user-lambda-list-keywords)))
    (mapcar (lambda (e) (or (car-safe e) e))
      (cl-remove-if
        (lambda (e) (memq e lambda-list-keywords))
        arglist))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (arg-names '(foo bar &optional baz &rest quux))
  returns (foo bar baz quux))
(confirm that (arg-names '(foo bar &optional (baz 9) &rest quux))
  returns (foo bar baz quux))
(confirm that
  (arg-names '(foo &my-keyword bar &optional (baz 9) &rest quux) '(&my-keyword))
  returns (foo bar baz quux))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compatible-arg-count-specs (pattern-arg-count-spec scrutinee-arg-count-spec)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Determine whether SCRUTINEE-ARG-COUNT-SPEC is 'compatible' with
PATTERN-ARG-COUNT-SPEC.

SCRUTINEE-ARG-COUNT-SPEC and PATTERN-ARG-COUNT-SPEC are pairs of the form
(MIN-ARGS . MAX-ARGS). MAX-ARGS may be nil to indicate 'no maximum number of args',
(i.e., the spec describtes the argument count of an arglist with a &rest lambda
list keyword in it).

SCRUTINEE-ARG-COUNT-SPEC is considered 'compatible' with
PATTERN-ARG-COUNT-SPEC when any arg count that would be valid for the pattern
would also be valid for the scrutinee."
  (cond
    ;; if MINARGS doesn't match, they're not compatible:
    ((not (= (car pattern-arg-count-spec) (car scrutinee-arg-count-spec))) nil)
    ;; if scrutinee has no MAXARGS, they're compatible:
    ((null (cdr scrutinee-arg-count-spec)) t)
    ;; if pattern has a MAXARGS, scrutinees must be GTE:
    ((and (cdr pattern-arg-count-spec)
       (>= (cdr scrutinee-arg-count-spec) (cdr pattern-arg-count-spec)) t))
    ;; otherwise they're not compatible:
    (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (compatible-arg-count-specs '(1 . 3) '(1 . 3)) returns t)
(confirm that (compatible-arg-count-specs '(1 . 3) '(1 . 4)) returns t)
(confirm that (compatible-arg-count-specs '(1 . 4) '(1 . 3)) returns nil)
(confirm that (compatible-arg-count-specs '(2 . 3) '(1 . 3)) returns nil)
(confirm that (compatible-arg-count-specs '(1 . 3) '(2 . 3)) returns nil)
(confirm that (compatible-arg-count-specs '(1 . nil) '(1 . 3)) returns nil)
(confirm that (compatible-arg-count-specs '(1 . 3) '(1 . nil)) returns t)
(confirm that (compatible-arg-count-specs '(2 . 3) '(2 . 5)) returns t)
(confirm that (compatible-arg-count-specs '(2 . 5) '(2 . 3)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--arglists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
