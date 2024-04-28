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
i.e. &optional and &rest."
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
        (seen-optional
          (cl-incf max-args))
        (rest-arg) ;; don't bother counting after &rest.
        (t
          (cl-incf min-args)
          (cl-incf max-args))))
    (cons min-args (if rest-arg nil max-args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (count-args '(foo &optional bar &rest baz)) returns (1))
(confirm that (count-args '(foo &optional bar baz)) returns (1 . 3))
(confirm that (count-args '(foo bar &optional baz)) returns (2 . 3))
(confirm that (count-args '(foo bar &optional baz &rest quux)) returns (2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--arglists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
