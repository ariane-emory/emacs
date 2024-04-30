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
(defgroup ap:match nil
  "Ari's destructuring pattern matcher.")
;;-----------------------------------------------------------------------------------------
(defcustom *ap:match--verbose* t
  "Whether `match-pattern' should print verbose messages."
  :group 'ap:match
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ap::prn (&rest args)
  "Internal print helper function."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when *ap:match--verbose*
    (apply #'prn args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ap::prndiv ()
  "Internal print helper function."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when *ap:match--verbose*
    (prndiv)))
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
  "A simple pattern matching/destructuring fun."
  (with-gensyms (no-match-tag)
    (ap::prndiv)
    (ap::prn "GENERATED:    %s" no-match-tag)
    (catch no-match-tag
      (with-indentation
        (ap::match1 pattern target dont-care ellipsis no-match-tag)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ap::match1 (pattern target dont-care ellipsis no-match-tag)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal function used by `ap:match'."
  (ap::prndiv)
  (ap::prn "MATCHING %S AGAINST %S" pattern target)
  (ap::prn "no-match-tag:   %s" no-match-tag)
  ;; (ap::prndiv)
  (let (alist)
    (while (and pattern target)
      (let ( (pat-head  (pop pattern))
             (targ-head (pop target)))
        (ap::prndiv)
        (ap::prn "pattern:       %s" pattern)
        (ap::prn "target:        %s" target)
        (ap::prn "pat-head:      %s" pat-head)
        (ap::prn "targ-head:     %s" targ-head)
        (cond
          ((and dont-care (eq pat-head dont-care))) ; DONT-CARE, do nothing.
          ;; When PAT-HEAD is an ELLIPSIS, nullify TARGET and PATTERN to break the
          ;; loop successfully:
          ((and ellipsis (eq pat-head ellipsis) )
            (when pattern
              (error "ellipsis must be the last element in the pattern"))
            (setf target  nil)
            (setf pattern nil))
          ;; When PAT-HEAD is a variable, stash TARG-HEAD in ALIST:
          ((eq '\, (car-safe pat-head)) 
            (let ((var (cadr pat-head)))
              (when (assoc var alist)
                (error "duplicate key %s" var))
              (setf alist (cons (cons var targ-head) alist))
              (ap::prn "ALIST:         %s" alist)))
          ;; When PAT-HEAD is a list, recurse and merge the result into ALIST:
          ((and (proper-list-p pat-head)
             (proper-list-p targ-head))
            (setf alist 
              (ap::merge-2-alists alist
                (with-indentation
                  (ap::match1 pat-head targ-head dont-care ellipsis no-match-tag)))))
          ((equal pat-head targ-head)) ; equal literals, do nothing. 
          ;; When the heads aren't equal and we didn't have either a DONT-CARE, an
          ;; ELLIPSIS, a variable, or a list in PAT-HEAD, no match
          (t 
            (ap::prn "THROWING %s!" no-match-tag)
            (throw no-match-tag nil))))) ;; end of (while (and pattern target).
    ;; If we got this far, either PATTERN, TARGET or both are nil.
    (ap::prndiv)
    (ap::prn "final pattern: %s" pattern)
    (ap::prn "final target:  %s" target)
    ;; When TARGET isn't nil, then PATTERN must have ran out before TARGET, no match:
    (when target
      (ap::prn "THROWING %s!" no-match-tag)
      (throw no-match-tag nil))
    ;; By this line, TARGET must be nil. Unless PATTERN is also nil, it had better
    ;; just contain an ELLIPSIS:
    (unless (or (null pattern) (equal pattern (list ellipsis)))
      (throw no-match-tag nil))
    ;; In the future, we could return t here for empty-but-successful matches?
    (let ((res (nreverse alist)))
      (ap::prn "RESULT:        %s" res)
      res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These two are just examples of error cases:
;; (ap:match '(,y (,y)) '(2 (3))) ; duplicate key!
;; (ap:match '(,y ,z) '(2 (3))) ; duplicate key!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (ap:match '(x ,y ,z) '(x 2 (3 4 5))) returns ((y . 2) (z 3 4 5)))
(confirm that (ap:match '(,a ,b ,c \!) '(1 2 3)) returns nil)
(confirm that (ap:match '(,a ,b ,c) '(1 2 3)) returns ((a . 1) (b . 2) (c . 3)))
(confirm that (ap:match '(foo _ ,baz) '(foo quux poop)) returns ((baz . poop)))
(confirm that (ap:match '(foo _ ,baz) '(foo (2 . 3) poop)) returns ((baz . poop)))
(confirm that (ap:match '(,x ...) '(1 2 3)) returns ((x . 1)))
(confirm that (ap:match '(,x ...) '(1)) returns ((x . 1)))
(confirm that (ap:match '(1 2 (,x b ...) 4 ,y) '(1 2 (a b c) 4 5))
  returns ((x . a) (y . 5)))
(confirm that (ap:match '(1 2 (,x b ...) 4 ,y ...) '(1 2 (a b c) 4 5 6 7 8 9))
  returns ((x . a) (y . 5)))
(confirm that (ap:match '(,x ,y (,z 4) ) '(1 2 a (3 4) a)) returns nil)
(confirm that (ap:match '(,x 2 (...) 3 ,y) '(1 2 () 3 4)) returns ((x . 1) (y . 4)))
(confirm that (ap:match '(,x 2 (...) 3 ,y) '(1 2 (a b c) 3 4)) returns ((x . 1) (y . 4)))
(confirm that (ap:match '(,x 2 (,p ...) 3 ,y) '(1 2 (q r) 3 4))
  returns ((x . 1) (p . q) (y . 4)))
(confirm that (ap:match '(1 (,foo _) 2) '(1 (,foo _) 2)) returns ((foo \, foo)))
;; don't allow these to partially match;
(confirm that (ap:match '(,x (,p ...) ,y) '(1 (q r) 2)) returns ((x . 1) (p . q) (y . 2)))
(confirm that (ap:match '(,x (,p) ,y) '(1 (q r) 2)) returns nil)
(confirm that (ap:match '(,x (,p) ,y) '(1 () 2)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (ap:match
    '(one (this that) (,two three (,four ,five) ,six))
    '(one (this that) (2 three (4 5) 6)))  
  returns ( (two . 2)
            (four . 4)
            (five . 5)
            (six . 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUGS, FIX THESE!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




