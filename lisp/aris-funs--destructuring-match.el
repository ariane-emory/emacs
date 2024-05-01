;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `dm:match', a very basic pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--lists)
(require 'aris-funs--when-let-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup destructuring-match nil
  "Ari's destructuring pattern matcher.")
;;-----------------------------------------------------------------------------------------
(defcustom *dm:match-verbose* t
  "Whether `match-pattern' should print verbose messages."
  :group 'destructuring-match
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prn (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:match-verbose* (apply #'prn args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prndiv (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:match-verbose* (apply #'prndiv args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::merge-2-alists (alist-1 alist-2)
  "Merge two alists into a single alist, maintaining their relative key order
and signaling an eror upon encountering a duplicate key. Result is returned
in reverse order."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (prn "merge this: %s" alist-1)
  ;; (prn "and this:   %s" alist-2)
  (let ((alist (nreverse alist-1)))
    (dolist (kvp alist-2 alist)
      (dm::require-non-duplicate-key! (car kvp) alist-1)
      (push kvp alist))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::merge-2-alists '((a . 1) (b. 2) (c . 3)) '((d . 4) (e . 5)))
  returns ((e . 5) (d . 4) (c . 3) (b. 2) (a . 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun dm:match (pattern target &optional (dont-care '_) (ellipsis '...) (unsplice '\,@))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A simple pattern matching/destructuring fun."
  (with-gensyms (no-match-tag)
    (dm::prndiv)
    (dm::prn "GENERATED:     %s" no-match-tag)
    (let ((res
            (catch no-match-tag
              (with-indentation
                (dm::match1 pattern target dont-care ellipsis unsplice no-match-tag)))))
      (dm::prndiv)
      (dm::prn "FINAL RESULT:  %s" res) 
      (dm::prndiv)
      res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::require-non-duplicate-key! (key alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Signal an error if KEY is already in ALIST."
  (when (assoc key alist)
    (error "duplicate key %s." key)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::match1 (pattern target dont-care ellipsis unsplice no-match-tag)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal function used by `dm:match'."
  (dm::prndiv)
  (dm::prn "MATCHING:       %S" pattern)
  (dm::prn "AGAINST:        %S" target)
  (dm::prndiv ?\-)
  (dm::prn "no-match-tag:   %s" no-match-tag)
  (let (alist)
    (while (and pattern target)
      (let ( (pat-head  (pop pattern))
             (targ-head (pop target)))
        (dm::prndiv)
        (dm::prn "pattern:       %s" pattern)
        (dm::prn "target:        %s" target)
        (dm::prn "pat-head:      %s" pat-head)
        (dm::prn "targ-head:     %s" targ-head)
        (cond
          ((and dont-care (eq pat-head dont-care))) ; DONT-CARE, do nothing.
          ;; When PAT-HEAD is an ELLIPSIS, nullify TARGET and PATTERN to break the
          ;; loop successfully:
          ((and ellipsis (eq pat-head ellipsis) )
            (when pattern (error "ellipsis must be the last element in the pattern."))
            ;; nullify TARGET and PATTERN:
            (setf target  nil)
            (setf pattern nil))
          ;; When PAT-HEAD is an UNSPLICE, nullify TARGET and PATTERN to break the
          ;; loop successfully:
          ((and unsplice (eq unsplice (car-safe pat-head)))
            (when pattern (error "unsplice must be the last element in the pattern."))
            (let ((var (cadr pat-head)))
              ;; (debug)
              (dm::require-non-duplicate-key! var alist)
              ;; put the remainder of TARGET in VAR's key in ALIST:
              (setf alist (cons (cons var (cons targ-head target)) alist))
              ;; nullify TARGET and PATTERN:
              (setf target  nil)
              (setf pattern nil)))
          ;; When PAT-HEAD is a variable, stash TARG-HEAD in ALIST:
          ((eq '\, (car-safe pat-head)) 
            (let ((var (cadr pat-head)))
              (dm::require-non-duplicate-key! var alist)
              ;; (when (assoc var alist)
              ;;   (error "duplicate key %s" var))
              (setf alist (cons (cons var targ-head) alist))
              (dm::prn "ALIST:         %s" alist)))
          ;; When PAT-HEAD is a list, recurse and merge the result into ALIST (unless
          ;; the result was just t because the pattern being recursed over contained no
          ;; variables):
          ((and (proper-list-p pat-head) (proper-list-p targ-head))
            (let ((res 
                    (with-indentation
                      (dm::match1 pat-head targ-head
                        dont-care ellipsis unsplice no-match-tag))))
              (unless (eq res t) (setf alist (dm::merge-2-alists alist res)))))
          ((equal pat-head targ-head)) ; equal literals, do nothing. 
          ;; When the heads aren't equal and we didn't have either a DONT-CARE, an
          ;; ELLIPSIS, a variable, or a list in PAT-HEAD, no match
          (t 
            (dm::prn "THROWING %s!" no-match-tag)
            (throw no-match-tag nil))))) ;; end of (while (and pattern target).
    ;; If we got this far, either PATTERN, TARGET or both are nil.
    (dm::prndiv)
    (dm::prn "final pattern: %s" pattern)
    (dm::prn "final target:  %s" target)
    ;; When TARGET isn't nil, then PATTERN must have ran out before TARGET, no match:
    (when target
      (dm::prn "THROWING %s!" no-match-tag)
      (throw no-match-tag nil))
    ;; By this line, TARGET must be nil. Unless PATTERN is also nil, it had better
    ;; contain an ELLIPSIS or an UNSPLICE:
    (cond
      ((null pattern)) ;; don't need to do anything.
      ((and ellipsis (equal (car pattern) ellipsis))
        ;; don't need to do anything other than check for well formednessl'
        (when (cdr pattern) (error "ellipsis must be the last element in the pattern."))) 
      ;; if PATTERN's head is an UNSPLICE, since there's no TARGET left we just need
      ;; to set the var in ALIST to nil:
      ((and unsplice (equal (car-safe (car pattern)) unsplice))
        (when (cdr pattern) (error "unsplice must be the last element in the pattern."))
        (let ((var (cadar pattern)))
          (dm::require-non-duplicate-key! var alist)
          (setf alist (cons (cons var nil) alist))))
      (t (throw no-match-tag nil)))    
    ;; return either the ALIST or just t:
    (if alist
      (let ((res (nreverse alist)))
        (dm::prn "RESULT:        %s" res)
        res)
      t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These two are just examples of error cases:
;; (dm:match '(,y (,y)) '(2 (3))) ; duplicate key in merge!
;; (dm:match '(,y ,y) '(2 3)) ; duplicate key in set!
;; (dm:match '(,y ,@y) '(2 3 4 5)) ; duplicate key in UNSPLICE!
;; (dm:match '(,y ,@zs ...) '(2)) ; malformed, elem after UNSPLICE.
;; (dm:match '(,y ... ,@zs) '(2)) ; malformed, elem after ELLIPSIS.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm:match '(x ,y ,z) '(x 2 (3 4 5))) returns ((y . 2) (z 3 4 5)))
(confirm that (dm:match '(,a ,b ,c \!) '(1 2 3)) returns nil)
(confirm that (dm:match '(,a ,b ,c) '(1 2 3)) returns ((a . 1) (b . 2) (c . 3)))
(confirm that (dm:match '(foo _ ,baz) '(foo quux poop)) returns ((baz . poop)))
(confirm that (dm:match '(foo _ ,baz) '(foo (2 . 3) poop)) returns ((baz . poop)))
(confirm that (dm:match '(,x ...) '(1 2 3)) returns ((x . 1)))
(confirm that (dm:match '(,x ...) '(1)) returns ((x . 1)))
(confirm that (dm:match '(1 2 (,x b ...) 4 ,y) '(1 2 (a b c) 4 5))
  returns ((x . a) (y . 5)))
(confirm that (dm:match '(1 2 (,x b ...) 4 ,y ...) '(1 2 (a b c) 4 5 6 7 8 9))
  returns ((x . a) (y . 5)))
(confirm that (dm:match '(,x ,y (,z 4) ) '(1 2 a (3 4) a)) returns nil)
(confirm that (dm:match '(,x 2 (...) 3 ,y) '(1 2 () 3 4)) returns ((x . 1) (y . 4)))
(confirm that (dm:match '(,x 2 (...) 3 ,y) '(1 2 (a b c) 3 4)) returns ((x . 1) (y . 4)))
(confirm that (dm:match '(,x 2 (,p ...) 3 ,y) '(1 2 (q r) 3 4))
  returns ((x . 1) (p . q) (y . 4)))
(confirm that (dm:match '(1 (,foo _) 2) '(1 (,foo _) 2)) returns ((foo \, foo)))
;; don't allow these to partially match;
(confirm that (dm:match '(,x (,p ...) ,y) '(1 (q r) 2)) returns ((x . 1) (p . q) (y . 2)))
(confirm that (dm:match '(,x (,p) ,y) '(1 (q r) 2)) returns nil)
(confirm that (dm:match '(,x (,p) ,y) '(1 () 2)) returns nil)
(confirm that (dm:match '(,x ,@ys) '(1 2 3 4)) returns ((x . 1) (ys 2 3 4)))
(confirm that
  (dm:match '(,v _ ,w (,x (p q) ,@ys) ,z ...) '(foo bar 1 (2 (p q) 3 4) 5 6 7 8))
  returns ((w . 1) (v . foo) (x . 2) (ys 3 4) (z . 5)))
(confirm that (dm:match '(,x ,@ys) '(1)) returns ((x . 1) (ys)))
(confirm that (dm:match '(1 2 3) '(1 2 3)) returns t)
(confirm that (dm:match '(,1 2 3) '(1 2 3)) returns ((1 . 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (dm:match
    '(one (this that) (,two three (,four ,five) ,six))
    '(one (this that) (2 three (4 5) 6)))  
  returns ( (two . 2)
            (four . 4)
            (five . 5)
            (six . 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist (dm:match
                    '(i ,modal-verb ,verb a ,thing)
                    '(i have (never seen) a (red car)))
    (flatten `(Do you really believe that you ,.modal-verb ,.verb a ,.thing \?)))
  returns (Do you really believe that you have never seen a red car \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist (dm:match
                    '(i ,verb that ,noun ,con ,thing)
                    '(i think that dogs are dumb))
    (flatten `(Why do you ,.verb that ,.noun ,.con ,.thing \?)))
  returns (Why do you think that dogs are dumb \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (when-let-alist
    (dm:match '(i ,modal-verb ,verb a ,thing) '(i have (never seen) a (red car)))
    (flatten `(why do you think that you ,.modal-verb ,.verb a ,.thing \?)))
  returns (why do you think that you have never seen a red car \?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun dm:fill (pattern alist &optional (unsplice '\,@))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Fill in the variables in PATTERN with the values from ALIST."
  (dm::prndiv)
  (dm::prn "FILL:")
  (let (res)
    (while pattern
      (let ((thing (pop pattern)))
        (dm::prndiv)
        (dm::prn "thing:   %s" thing)
        (dm::prn "alist:   %s" alist)
        (push (cond
                ((eq '\, (car-safe thing))
                  (if-let ((assoc (assoc (cadr thing) alist)))
                    (cdr assoc)
                    (error "var %s not found." (cadr thing))))
                ((eq unsplice (car-safe thing))
                  (if-let ((assoc (assoc (cadr thing) alist)))
                    (cdr assoc)
                    (error "var %s not found." (cadr thing))))
                ((proper-list-p thing) (dm:fill thing alist))
                (t thing))
          res)))
    (nreverse res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dm:fill '(,w ,y) '((w . 666) (y 1 2 3 4)))

;; (rmapcar pattern
;;   (lambda (thing)
;;     (cond
;;       ((if (eq '\, (car-safe thing))
;;          (if-let ((assoc (assoc (cadr thing) alist)))
;;            (cdr assoc)
;;            (error "var %s not found." (cadr thing)))))
;;       ((proper-list-p thing) (dm:fill thing alist))
;;       (t thing))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm:fill '(,w x ,y z) '((w . 666) (y . 999)))
  returns (666 x 999 z))
(confirm that (dm:fill '(,w x ,y (,y , y) z ,w) '((y . 999) (w . (333 666))))
  returns ((333 666) x 999 (999 999) z (333 666)))
(confirm that (dm:fill '(a ,b (,c ,d)) (dm:match '(a ,b (,c ,d)) '(a 2 (3 4))))
  returns (a 2 (3 4)))
(confirm that (dm:fill '(a ,b (,c ,d))
                (dm:match '(a ,b (,c ,d))
                  (dm:fill '(a ,b (,c ,d))
                    (dm:match '(a ,b (,c ,d))
                      '(a 2 (3 4))))))
  returns (a 2 (3 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(dm:match '(_ ,x ...) '(foo (2 . 3) (4 . 5)))
