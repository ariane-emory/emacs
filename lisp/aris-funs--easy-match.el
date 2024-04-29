;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `easy-match', a very basic pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--confirm)
(require 'aris-funs--when-let-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun easy-match (pat targ)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A rudimentary pattern matching fun."
  ;; (prndiv)
  ;; (prn "pat:        %s" pat)
  ;; (prn "targ:       %s" targ)
  (let (alist)
    (catch 'no-match
      (while-let ( (pat-head  (pop pat))
                   (targ-head (pop targ)))
        ;; (prn "pat-head:   %s" pat-head)
        ;; (prn "targ-head:  %s" targ-head)
        (cond
          ((eq '\, (car-safe pat-head)) ; pat-head is a variable.
            (setf alist (cons (cons (cadr pat-head) targ-head) alist)))
          ((proper-list-p pat-head) ; recurse and merge.
            (setf alist (merge-alists alist (easy-match pat-head targ-head))))
          ((equal pat-head targ-head)) ; do nothing.
          (t (throw 'no-match nil)))))
    (let ((res (unless (or pat targ) (nreverse alist))))
      ;; (prn "res:   %s" res)
      res)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when-let-alist (easy-match '(i ,modal-verb ,verb a ,thing) '(i have (never seen) a (red car)))
  (prndiv)
  (prn (flatten `(why do you think that you ,.modal-verb ,.verb a ,.thing \?))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (pair
          '( ((i ,modal-verb ,verb a ,thing) . (i would like a smoke))
             ((i ,modal-verb ,verb a ,thing) . (i could have a smoke))
             ((i ,modal-verb ,verb a ,thing) . (i have seen a ghost))
             ((i ,modal-verb ,verb a ,thing) . (i have (never seen) a (red car)))))
  (let-kvp pair
    (prndiv)
    (prn "pat:   %s" .key)
    (prn "targ:  %s" .val)
    (when-let-alist (easy-match .key .val)
      (prn
        (flatten `(Do you really believe that you ,.modal-verb ,.verb a ,.thing \?))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--easy-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
