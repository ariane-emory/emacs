;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (thing  '(  aa
                   ,bb    ,(bb t)
                   ,@cc  ,@(cc t)
                   #'dd  #'(dd t)
                   'ee    '(ee t)
                   `ff    `(ff t)
                   ))
  (cond
    ((eq '\, (car-safe thing)) (prn "This one is special: %s" thing))
    ((eq '\,@ (car-safe thing)) (prn "This one is very special: %s" thing))
    ((eq 'function (car-safe thing)) (prn "This one is super special: %s" thing))
    ((eq 'quote (car-safe thing)) (prn "This one is kind of special: %s" thing))
    ((eq '\` (car-safe thing)) (prn "This one is extra special: %s" thing))
    ;; (t (prn "%s" thing))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lst '(1 2 3 4 5 6 7 8 9))
(setq al (dm:match '(,x ,@ys) lst))
(setcar (alist-get 'ys al) 999)
(setq lst '(my name is ari))
(setq al (dm:match '(my name is ,@name) lst))
(setcar (alist-get 'name al) 'bob)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dm::reset)

(dm:match
  '(,a ,b (,c . ,d) . ,e)
  (dm::properize-target '(1 2 (3 . 4) . 5)))
(symbol-plist '*dm*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::unproperize!* (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn lists with *dm::improper-indicator* in their second last position back into improper lists."
  (let ((pos lst))
    (dm::prndiv)
    (while (consp pos)
      (if (atom pos)
        (dm::prn "atom:     %s"  pos)
        (dm::prn "head:     %s" (car pos))
        (when (consp (car pos))
          (with-indentation
            (dm::unproperize!* (car pos))))
        (when (eq (cadr-safe pos) *dm::improper-indicator*)
          (when (cadddr pos)
            (error "properize indicator in unexpected position: %s" lst))
          (setcdr pos (caddr pos))
          (setf pos nil))
        (pop pos))))
  (dm::prn "lst:      %s" lst)
  (dm::prndiv)
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dm::unproperize!* '(1 2 3 (4 (a b c (d \. e)) 5 6 \. 7) \. 8))


(let ((pat '(,a ,b . ,c)))
  (dm::prnl)
  (dm::prnl)
  ;; this doesn't look correct intuitively due to wayward comma but i'm pretty sure it actually is:
  (equal pat (dm::unproperize!* (cl-copy-list (dm::intern-pattern pat)))))

(let ((pat ;; '(,w ,(x integer? . foo) . ,(y integer? . foo))
        '(,a ,(b integer? . foo) . ,c)
        ;; '(,a ,(b integer? foo) . ,(c integer? foo))
        ))
  (dm::prnl)
  (dm::prnl)
  (equal pat (dm::unproperize!* (cl-copy-list (dm::intern-pattern pat)))))




(progn
  (prn "%s" " ")
  (prn "%s" "  ")
  (prn "%s" "   "))

