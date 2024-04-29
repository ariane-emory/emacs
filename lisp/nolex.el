(defun foo (pat target)
  (let (alist)
    (while-let ( (pat-head  (pop pat))
                 (targ-head (pop target))
                 (_ (or (equal pat-head targ-head) (eq '\, (car-safe pat-head)))))
      (when (eq '\, (car-safe pat-head))
        (setf alist (cons (cons (cadr pat-head) targ-head) alist))))
    (unless (or pat target) (nreverse alist))))

(when-let ((alist (foo '(i ,blah ,verb a ,thing) '(i would like a smoke))))
  (let-alist alist
    (prn `(why do you think that you ,.blah ,.verb a ,.thing \?))))

(when-let ((alist (foo '(i ,blah ,verb a ,thing) '(i could have a smoke))))
  (let-alist alist
    (prn `(why do you think that you ,.blah ,.verb a ,.thing \?))))

(when-let ((alist (foo '(i ,blah ,verb a ,thing) '(i have seen a ghost))))
  (let-alist alist
    (prn `(why do you think that you ,.blah ,.verb a ,.thing \?))))

(when-let-alist (foo '(i ,blah ,verb a ,thing) '(i have (never seen) a (red car)))
  (prn (flatten `(why do you think that you ,.blah ,.verb a ,.thing \?))))



