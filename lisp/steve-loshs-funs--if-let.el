;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steve Losh's CL implementations of if-let/when-let, translated into emacs lisp by
;; ariane-emory. This was mostly just a translation exercise and the macro names are
;; capitalized so as not to shadow emacs' own versions (which should be preferred).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--with-gensyms)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmacro When-let* (bindings &body body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Bind `bindings` serially and execute `body`, short-circuiting on `nil`.
See: https://stevelosh.com/blog/2018/07/fun-with-macros-if-let/ "
  (with-gensyms (block)
    `(cl-block ,block
       (let* ,(cl-loop for (symbol value) in bindings
                collect `(,symbol (or ,value (cl-return-from ,block nil))))
         ,@body))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmacro If-let* (bindings then &body else)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Bind `bindings` serially and execute `then` if all are true, or `else` otherwise.
See: https://stevelosh.com/blog/2018/07/fun-with-macros-if-let/"
  (with-gensyms (outer inner)
    `(cl-block ,outer
       (cl-block ,inner
         (let* ,(cl-loop for (symbol value) in bindings
                  collect `(,symbol (or ,value (cl-return-from ,inner nil))))
           (cl-return-from ,outer ,then)))
       ,@else)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmacro When-let (bindings &body body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Bind `bindings` in parallel and execute `body`, short-circuiting on `nil`.
See: https://stevelosh.com/blog/2018/07/fun-with-macros-if-let/ "
  (with-gensyms (block) 
    `(cl-block ,block
       (let ,(cl-loop for (symbol value) in bindings
               collect `(,symbol (or ,value (cl-return-from ,block nil))))
         ,@body))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmacro If-let (bindings then &body else)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Bind `bindings` in parrallel and execute `then` if all are true, or `else` otherwise.
See: https://stevelosh.com/blog/2018/07/fun-with-macros-if-let/"
  (with-gensyms (outer inner) 
    `(cl-block ,outer
       (cl-block ,inner
         (let ,(cl-loop for (symbol value) in bindings
                 collect `(,symbol (or ,value (cl-return-from ,inner nil))))
           (cl-return-from ,outer ,then)))
       ,@else)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'steve-loshs-funs--if-let)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
