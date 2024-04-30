;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `ap:match', a very basic pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--confirm)
(require 'aris-funs--lists)
(require 'aris-funs--when-let-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ap::merge-2-alists (alist-1 alist-2)
  "Merge two alists into a single alist, maintaining their relative key order
 and signaling an eror upon encountering a duplicate key. Results are returned
in reverse order."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (prn "merge this: %s" alist-1)
  ;; (prn "and this:   %s" alist-2)
  (let ((alist (nreverse alist-1)))
    (dolist (kvp alist-2 alist)
      (if (assoc (car kvp) alist-1)
        (error "duplicate key %s" (car kvp))
        (push kvp alist)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ap::merge-2-alists '((a . 1) (b. 2) (c . 3)) '((d . 4) (e . 5)))
  returns ((e . 5) (d . 4) (c . 3) (b. 2) (a . 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ap:match (pat targ)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A very rudimentary pattern matching/destructuring fun."
  (catch 'no-match
    (if (eq pat t)
      '();; t matches anything and returns an empty alist.
      (let (alist)
        (while (and pat targ)
          (let ( (pat-head  (pop pat))
                 (targ-head (pop targ)))
            (cond
              ((eq '\, (car-safe pat-head)) ; pat-head is a variable.
                (when (assoc (cadr pat-head) alist)
                  (error "duplicate key %s" (cadr pat-head)))
                (setf alist (cons (cons (cadr pat-head) targ-head) alist)))
              ((proper-list-p pat-head) ; recurse and merge.
                (setf alist (ap::merge-2-alists alist
                              (with-indentation (ap:match pat-head targ-head)))))
              ((equal pat-head targ-head)) ; do nothing.
              (t (throw 'no-match nil)))))
        (unless (or pat targ) (nreverse alist))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ap:match '(,y (,y)) '(2 (3))) ; duplicate key!
;; (ap:match '(,y ,z) '(2 (3))) ; duplicate key!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ap:match '(x ,y ,z) '(x 2 (3 4 5))) returns ((y . 2) (z 3 4 5)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ap:match '(,a ,b ,c \!) '(1 2 3)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (ap:match
    '(one (this that) (,two three (,four ,five) ,six))
    '(one (this that) (2 three (4 5) 6)))  
  returns ( (two . 2)
            (four . 4)
            (five . 5)
            (six . 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist (ap:match
                    '(i ,modal-verb ,verb a ,thing)
                    '(i have (never seen) a (red car)))
    (flatten `(Do you really believe that you ,.modal-verb ,.verb a ,.thing \?)))
  returns (Do you really believe that you have never seen a red car \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist (ap:match
                    '(i ,verb that ,noun ,con ,thing)
                    '(i think that dogs are dumb))
    (flatten `(Why do you ,.verb that ,.noun ,.con ,.thing \?)))
  returns (Why do you think that dogs are dumb \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist
    (ap:match '(i ,modal-verb ,verb a ,thing) '(i have (never seen) a (red car)))
    (flatten `(why do you think that you ,.modal-verb ,.verb a ,.thing \?)))
  returns (why do you think that you have never seen a red car \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ap:fill (pat alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Fill in the variables in PAT with the values from ALIST."
  (if (eq pat t)
    (error "You can't fill %s" pat)
    (rmapcar pat
      (lambda (thing)
        (cond
          ((if (eq '\, (car-safe thing))
             (if-let ((kvp (assoc (cadr thing) alist)))
               (cdr kvp)
               (error "var %s not found" (cadr thing)))))
          ((proper-list-p thing) (ap:fill thing alist))
          (t thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ap:fill '(,w x ,y z) '((w . 666) (y . 999)))
  returns (666 x 999 z))
(confirm that (ap:fill '(,w x ,y (,y , y) z ,w) '((y . 999) (w . (333 666))))
  returns ((333 666) x 999 (999 999) z (333 666)))
(confirm that (ap:fill '(a ,b (,c ,d)) (ap:match '(a ,b (,c ,d)) '(a 2 (3 4))))
  returns (a 2 (3 4)))
(confirm that (ap:fill '(a ,b (,c ,d))
                (ap:match '(a ,b (,c ,d))
                  (ap:fill '(a ,b (,c ,d))
                    (ap:match '(a ,b (,c ,d))
                      '(a 2 (3 4))))))
  returns (a 2 (3 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--easy-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


