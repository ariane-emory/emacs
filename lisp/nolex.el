(defun foo (pat targ)
  "A very bad pattern matching fun."
  (prndiv)
  (prn "pat:        %s" pat)
  (prn "targ:       %s" targ)
  (let (alist)
    (catch 'no-match
      (while-let ( (pat-head  (pop pat))
                   (targ-head (pop targ)))
        (prn "pat-head:   %s" pat-head)
        (prn "targ-head:  %s" targ-head)
        (cond
          ((eq '\, (car-safe pat-head)) ; pat-head is a variable.
            (setf alist (cons (cons (cadr pat-head) targ-head) alist)))
          ((proper-list-p pat-head) ; recurse and merge.
            (setf alist
              (merge-alists alist
                (with-indentation (foo pat-head targ-head)))))
          ((equal pat-head targ-head)) ; do nothing.
          (t (throw 'no-match nil)))))
    (let ((res (unless (or pat targ) (nreverse alist))))
      (prn "res:   %s" res)
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(foo '(one ,two ,three ,four) '(one 2 3 4))
(foo '(one (,two three (,four ,five) ,six)) '(one (2 three (4 5) 6)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (when-let-alist (foo '(i ,modal-verb ,verb a ,thing) '(i have (never seen) a (red car)))
;;   (prndiv)
;;   (prn (flatten `(why do you think that you ,.modal-verb ,.verb a ,.thing \?))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (dolist (pair
;;           '( ((i ,modal-verb ,verb a ,thing) . (i would like a smoke))
;;              ((i ,modal-verb ,verb a ,thing) . (i could have a smoke))
;;              ((i ,modal-verb ,verb a ,thing) . (i have seen a ghost))
;;              ((i ,modal-verb ,verb a ,thing) . (i have (never seen) a (red car)))))
;;   (let-kvp pair
;;     (prndiv)
;;     (prn "pat:   %s" .key)
;;     (prn "targ:  %s" .val)
;;     (when-let-alist (foo .key .val)
;;       (prn
;;         (flatten `(Do you really believe that you ,.modal-verb ,.verb a ,.thing \?))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



