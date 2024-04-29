;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `easy-match', a very basic pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--confirm)
(require 'aris-funs--lists)
(require 'aris-funs--when-let-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fill-pattern (pat alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (rmapcar pat
    (lambda (thing)
      (cond
        ((if (eq '\, (car-safe thing))
           (if-let ((kvp (assoc (cadr thing) alist)))
             (let-kvp kvp
               .val)
             (error "var %s not found" (cadr thing)))))
        (t thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fill-pattern '(x ,y z) '((y . 999)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun easy-match (pat targ)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      "A very rudimentary pattern matching fun."
      (catch 'no-match
        (let (alist)
          (while-let ( (pat-head  (pop pat))
                       (targ-head (pop targ)))
            (cond
              ((eq '\, (car-safe pat-head)) ; pat-head is a variable.
                (setf alist (cons (cons (cadr pat-head) targ-head) alist)))
              ((proper-list-p pat-head) ; recurse and merge.
                (setf alist (merge-alists alist (easy-match pat-head targ-head))))
              ((equal pat-head targ-head)) ; do nothing.
              (t (throw 'no-match nil))))
          (unless (or pat targ) (nreverse alist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (easy-match
    '(one (this that) (,two three (,four ,five) ,six))
    '(one (this that) (2 three (4 5) 6)))
  returns ( (two . 2)
            (four . 4)
            (five . 5)
            (six . 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist (easy-match
                    '(i ,modal-verb ,verb a ,thing)
                    '(i have (never seen) a (red car)))
    (flatten `(Do you really believe that you ,.modal-verb ,.verb a ,.thing \?)))
  returns (Do you really believe that you have never seen a red car \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist (easy-match
                    '(i ,verb that ,noun ,con ,thing)
                    '(i think that dogs are dumb))
    (flatten `(Why do you ,.verb that ,.noun ,.con ,.thing \?)))
  returns (Why do you think that dogs are dumb \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist
    (easy-match '(i ,modal-verb ,verb a ,thing) '(i have (never seen) a (red car)))
    (flatten `(why do you think that you ,.modal-verb ,.verb a ,.thing \?)))
  returns (why do you think that you have never seen a red car \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--easy-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
