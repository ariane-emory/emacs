(defmacro foo (pat target)
  (let (alist)
    (prndiv)
    (prn "alist: %s" alist)
    (while-let ( (pat-head  (pop pat))
                 (targ-head (pop target))
                 (_ (or (eq pat-head targ-head) (eq '\, (car-safe pat-head)))))
      (when (eq '\, (car-safe pat-head))
        (alist-put! (cadr pat-head) alist targ-head)))
    `',(nreverse alist)))

(prn "got: %s" (foo (hello ,you this is ,me) (hello bob this is kate)))

(prn "got: %s" (foo (hello ,you this is ,me) (this is some dumb junk)))

(if-let ((alist (foo (i would like a ,thing) (i would like a smoke))))
  (let-alist alist
    `(you should have a ,.thing)))
