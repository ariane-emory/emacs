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
and signaling an eror upon encountering a duplicate key. Result is returned
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
(cl-defun ap:match (pattern target &optional (dont-care '_) (ellipsis '...))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A very rudimentary pattern matching/destructuring fun."
  (with-gensyms (no-match-tag)
    (prndiv)
    (prn "GENERATED:    %s" no-match-tag)
    (catch no-match-tag
      (with-indentation
        (ap::match1 pattern target dont-care ellipsis no-match-tag)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is fine:
;; (ap:match '(,x ...) '(1 2 3))
;; but it would be nice if this matched:
;; (ap:match '(,x ...) '(1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ap::match1 (pattern target dont-care ellipsis no-match-tag)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (prndiv)
  (prn "MATCHING %S AGAINST %S" pattern target)
  (prn "no-match-tag:   %s" no-match-tag)
  ;; (prndiv)
  (let (alist)
    (while (and pattern target)
      (prndiv)
      (prn "pattern:       %s" pattern)
      (prn "target:        %s" target)
      (let ( (pat-head  (pop pattern))
             (targ-head (pop target)))
        (prn "pat-head:      %s" pat-head)
        (prn "targ-head:     %s" targ-head)
        (cond
          ((equal pat-head targ-head)) ; do nothing.
          ;; do nothing, maybe this should only match atoms? dunno:
          ((and dont-care (eq pat-head dont-care))) 
          ((and ellipsis (eq pat-head ellipsis) )
            (unless (null pattern)
              (error "ellipsis must be the last element in the pattern"))
            ;; nullify TARGET and PATTERN to break the loop 'successfully':
            (setf target nil)
            (setf pattern nil))
          ((eq '\, (car-safe pat-head)) ; pat-head is a variable.
            (let ((var (cadr pat-head)))
              (when (assoc var alist)
                (error "duplicate key %s" var))
              (setf alist (cons (cons var targ-head) alist))
              (prn "ALIST:         %s" alist)
              ))
          ((and (proper-list-p pat-head)
             (proper-list-p targ-head))
            (setf alist ; recurse and merge:
              (ap::merge-2-alists alist
                (with-indentation
                  (ap::match1 pat-head targ-head dont-care ellipsis no-match-tag)))))
          (t
            (prn "THROWING %s!" no-match-tag)
            (throw no-match-tag nil))))) ;; end of (while (and pattern target).
    (prndiv)
    (prn "final pattern: %s" pattern)
    (prn "final target:  %s" target)
    ;; (unless (or (null pattern) (and (eq ellipsis (car pattern)) (null (cdr pattern))))
    ;;   (prn "THROWING %s!" no-match-tag)

    ;; it pattern ran out before target, no match:
    (when (and target (null pattern))
      (prn "THROWING %s!" no-match-tag)
      (throw no-match-tag nil))
    
    ;;   (throw no-match-tag nil))
    (when (and target (not (equal (pat (list ellipsis)))))
      
      (throw no-match-tag nil))

    (let ((res (nreverse alist)))
      (prn "RESULT:        %s" res)
      res)

    ;; ugly hack to handle cases like (ap::match1 '(,x ...) '(1)) follows.
    ;; if not for the '... in final position case, this could really just be
    ;; (unless (or pattern target) (nreverse alist)), which looks much nicer.
    ;; (let ((res
    ;;         (unless
    ;;           (or target
    ;;             (and pattern
    ;;               (not (and ellipsis
    ;;                    (eq ellipsis (car pattern ))
    ;;                    (progn
    ;;                      (when (cdr pattern)
    ;;                        (error "ellipsis must be the last element in the pattern.")
    ;;                        t))))))
    ;;           (nreverse alist))))
    ;;   (prn "RESULT:        %s" res)
    ;;   res)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ap:match '(,y (,y)) '(2 (3))) ; duplicate key!
;; (ap:match '(,y ,z) '(2 (3))) ; duplicate key!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ap:match '(x ,y ,z) '(x 2 (3 4 5))) returns ((y . 2) (z 3 4 5)))
(confirm that (ap:match '(,a ,b ,c \!) '(1 2 3)) returns nil)
(confirm that (ap:match '(foo _ ,baz) '(foo quux poop)) returns ((baz . poop)))
(confirm that (ap:match '(foo _ ,baz) '(foo (2 . 3) poop)) returns ((baz . poop)))
(confirm that (ap:match '(1 2 (,x b ...) 4 ,y) '(1 2 (a b c) 4 5)) returns
  ((x . a) (y . 5)))
(confirm that (ap:match '(1 2 (,x b ...) 4 ,y ...) '(1 2 (a b c) 4 5 6 7 8 9)) returns
  ((x . a) (y . 5)))
(confirm that (ap:match '(,x ,y (,z 4) ) '(1 2 a (3 4) a)) returns nil)
;; (confirm that (ap:match '(,x 2 (...) 3 ,y) '(1 2 () 3 4)) returns
;;   ((x . 1) (y . 4))) ; elippsis needs alterations for this one to work!
(confirm that (ap:match '(,x 2 (,p ...) 3 ,y) '(1 2 (q r) 3 4)) returns
  ((x . 1) (p . q) (y . 4)))
;; don't allow partially match:
(confirm that (ap:match '(,x (,p ...) ,y) '(1 (q) 2)) returns nil)
(confirm that (ap:match '(,x (,p) ,y) '(1 () 2)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that (ap:match t '(1 2 3)) returns nil) ; no longer legal!
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
(defun ap:fill (pattern alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Fill in the variables in PATTERN with the values from ALIST."
  (if (eq pattern t)
    (error "You can't fill %s" pattern)
    (rmapcar pattern
      (lambda (thing)
        (cond
          ((if (eq '\, (car-safe thing))
             (if-let ((kvp (assoc (cadr thing) alist)))
               (cdr kvp)
               (error "var %s not found" (cadr thing)))))
          ((proper-list-p thing) (ap:fill thing alist))
          (t thing))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that (ap:fill '(,w x ,y z) '((w . 666) (y . 999)))
;;   returns (666 x 999 z))
;; (confirm that (ap:fill '(,w x ,y (,y , y) z ,w) '((y . 999) (w . (333 666))))
;;   returns ((333 666) x 999 (999 999) z (333 666)))
;; (confirm that (ap:fill '(a ,b (,c ,d)) (ap:match '(a ,b (,c ,d)) '(a 2 (3 4))))
;;   returns (a 2 (3 4)))
;; (confirm that (ap:fill '(a ,b (,c ,d))
;;                 (ap:match '(a ,b (,c ,d))
;;                   (ap:fill '(a ,b (,c ,d))
;;                     (ap:match '(a ,b (,c ,d))
;;                       '(a 2 (3 4))))))
;;   returns (a 2 (3 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--easy-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUGS, FIX THESE!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



