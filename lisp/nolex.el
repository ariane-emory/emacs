(defmacro foo (pat target)
  (let (alist)
    (while pat
      (let* ( (pat-head  (pop pat))
              (targ-head (pop target))
              (pat-head-is-var (eq '\, (car-safe pat-head))))
        (prndiv)
        (prn "phead: %s" pat-head)
        (prn "thead: %s" targ-head)
        (when pat-head-is-var
          (alist-put! (cadr pat-head) alist targ-head))))
    (prn "alist: %s" alist)
    (quote alist)))


(foo (1 ,y 3) (1 2 3))
