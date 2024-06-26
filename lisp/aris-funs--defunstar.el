;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My `defun*' macro which adds optional type checking of the arguments and return type of
;; a function to a `defun'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun* (name arglist &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Like defun, but with the option of type checking (but only for the mandatory
parameters, for the moment)."
  (let* ( new-arglist
          type-checks
          (remaining-arglist arglist)
          (remaining-arglist
            (catch 'break-loop
              (while remaining-arglist
                ;; peek and bail if head is a lambda list keyword:
                (when (member (car remaining-arglist) '(&optional &key &allow-other-keys))
                  (throw 'break-loop remaining-arglist))
                ;; pop the head and examine it:
                (let ((arg (pop remaining-arglist)))
                  (if-let ( (var (car-safe arg))
                            (_ (and var
                                 (symbolp var)
                                 (length= arg 3)
                                 (eq : (nth 1 arg))))
                            (ty (nth 2 arg))
                            ;; (_ (and ty (symbolp ty)))
                            )
                    ;; then add a type checking form to TYPE-CHECKS:
                    (progn
                      (push `(cl-check-type ,var ,ty) type-checks)
                      (push var new-arglist))
                    ;; else just add the arg to NEW-ARGLIST:
                    (push arg new-arglist)))))))
    ;; (prn "TYPE-CHECKS is %S." type-checks)
    ;; if any TYPE-CHECKS were found...
    (if (and (not type-checks) (not (eq (car-safe body) '=>)))
      ;; then expand into a normal defun:
      `(defun ,name ,arglist ,@body)
      ;; else tamper with the body before expansion to prepend TYPE-CHECKS onto BODY:
      (let* ( (new-arglist (append (nreverse new-arglist) remaining-arglist))
              (type-checks (nreverse type-checks))q
              (parse (byte-run--parse-body body t))
              (docstring (nth 0 parse))
              (declare-form (nth 1 parse))
              (interactive-form (nth 2 parse))
              (body (nth 3 parse))
              (return-type (when (eq '=> (first body)) (second body)))
              (return-sym (when return-type (gensym (format "%s-return-" name))))
              (body
                (if (null return-type)
                  body
                  `((let ((,return-sym ,@(cddr body)))
                      (unless (cl-typep ,return-sym ',return-type)
                        (signal 'wrong-type-return (list ',return-type ,return-sym)))
                      ,return-sym))))
              (warnings (nth 4 parse))
              (new-body (append
                          (when docstring (list docstring))
                          (when declare-form (list declare-form))
                          (when interactive-form (list interactive-form))
                          (when warnings (list warnings))
                          type-checks
                          body))
              (defun-expr `(defun ,name ,new-arglist ,@new-body)))
        defun-expr))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def* (spec &rest body)
  (pcase spec
    (`(,name . ,arglist)
      (if-let ( (_ (eq '=> (car (last (butlast arglist)))))
                (return-type (car (last arglist))))
        `(defun* ,name ,(cl-subseq arglist 0 -2) => ,return-type ,@body)
        `(defun* ,name ,arglist ,@body)))
    (_ (error "bad spec %S" spec))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'def 'def*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--defunstar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aris-funs--defunstar.el ends here
