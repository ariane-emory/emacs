(defun foo (pat target)
  "A very bad pattern matching fun."
  (prn "pat:   %s" .key)
  (prn "targ:  %s" .val)
  (let (alist)
    (while-let ( (pat-head  (pop pat))
                 (targ-head (pop targ))
                 (_ (or (equal pat-head targ-head) (eq '\, (car-safe pat-head)))))
      (when (eq '\, (car-safe pat-head)) ; pat-head is a variable
        (setf alist (cons (cons (cadr pat-head) targ-head) alist))))
    (unless (or pat targ) (nreverse alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(foo '(one (,two ,three) ,four) '(1 (2 3) 4))
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
