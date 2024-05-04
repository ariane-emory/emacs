;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `dm:match', a very basic pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--lists)
(require 'aris-funs--strings)
(require 'aris-funs--unsorted)
(require 'aris-funs--when-let-alist) ; Only used by some tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO:
;; - lookahead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup destructuring-match nil
  "Ari's destructuring pattern matcher.")
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:verbose* t
  "Whether or not dm:match should print verbose messages."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:tests-enabled* t
  "Whether or not dm:match's unit tests are enabled."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:label-width* 21
  "Label width used by functions in the 'destructuring-match' group."
  :group 'destructuring-match
  :type 'integer)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:default-dont-care* '_
  "dm:match's default DONT-CARE indicator."
  :group 'destructuring-match
  :type 'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:default-ellipsis* '...
  "dm:match's default ELLIPSIS indicator."
  :group 'destructuring-match
  :type 'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:default-unsplice* '\,@
  "dm:match's default UNSPLICE indicator."
  :group 'destructuring-match
  :type 'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:enforce-final-position* t
  "Whether or not dm:match should only allow ELLIPSIS and UNSPLICE in a pattern's final position."
  :group 'destructuring-match
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prn (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:verbose* (apply #'prn args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prnl ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:verbose* (prnl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prndiv (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:verbose* (apply #'prndiv args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmacro dm::prn-labeled (var &optional (extra "") (width *dm:label-width*))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Print VAR with a label at a given WIDTH, optionally prefixed by LABEL."
  (let* ( (label  (concat (upcase (symbol-name var)) ":"))
          (extra  (if (string-equal "" extra) extra (capitalize1 (concat extra " "))))
          (spaces (make-string (max 1 (- width (+ (length extra) (length label)))) ?\ ))
          (fmt    (format "%s%s%s%%s" extra label spaces)))
    `(dm::prn ,fmt ,var)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (let ( (foo    123)
         (barbaz 456)
         (barbazbarbazbarbazbarbaz 789))
    (dm::prn-labeled foo)
    (dm::prn-labeled barbaz)
    (dm::prn-labeled barbaz "last")
    (dm::prn-labeled barbazbarbazbarbazbarbaz)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these expand to:
;; (dm::prn "FOO:            %s" foo)
;; (dm::prn "BARBAZ:         %s" barbaz)
;; and print:
;; FOO:            123
;; BARBAZ:         456
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prn-pp-alist (alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Pretty print ALIST."
  (when *dm:verbose*
    (if-not (consp alist)
      (dm::prn-labeled alist)
      (let ((pp-str (indent-string-lines
                      (trim-trailing-whitespace
                        (pp-to-string-without-offset alist)))))
        (if (<= (count-string-lines pp-str) 1)
          (dm::prn-labeled alist)
          (dm::prn "ALIST:")
          (mapc #'prn (string-lines pp-str)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun dm:match ( pattern target
                     &optional
                     (dont-care *dm:default-dont-care*)
                     (ellipsis  *dm:default-ellipsis*)
                     (unsplice  *dm:default-unsplice*))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A simple pattern matching/destructuring fun."
  (unless (and (listp pattern)
            (listp target)
            (symbolp dont-care)
            (symbolp ellipsis)
            (symbolp unsplice))
    (error (concat "PATTERN and TARGET must be lists, "
             "DONT-CARE, ELLIPSIS and UNSPLICE must be symbols.")))
  (dm::prndiv)
  (dm::prn "BEGIN MATCH:          %S" pattern)
  (dm::prn "AGAINST:              %S" target)
  (let* ( (result (with-indentation (dm::match1 pattern target dont-care ellipsis unsplice nil)))
          (result (if (listp result) (nreverse result) result)))
    (dm::prndiv)
    (dm::prn-labeled result "FINAL")
    (dm::prndiv)
    (dm::prnl)
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::match1 (pattern target dont-care ellipsis unsplice alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal function used by `dm:match'."
  (cl-macrolet ((NO-MATCH! ()
                  `(progn
                     (dm::prn "THROWING %s!" 'no-match)
                     (throw 'no-match nil))))
    ;;-----------------------------------------------------------------------------------------------
    (catch 'no-match
      (dm::prndiv)
      (dm::prn-labeled pattern "MATCHING")
      (dm::prn-labeled target  "AGAINST ")
      ;; Just rename these because it reads better:
      (let ( (pat-tail  pattern)
             (targ-tail target))
        ;;===========================================================================================
        (progn ;; < useless `progn' for organization.
          (while (and pat-tail targ-tail)
            (let ( (pat-head  (pop pat-tail))
                   (targ-head (pop targ-tail)))
              (dm::prndiv)
              (dm::prn-pp-alist alist)
              (dm::prn-labeled  pat-head)
              (dm::prn-labeled  pat-tail)
              (dm::prn-labeled  targ-head)
              (dm::prn-labeled  targ-tail)
              (dm::prndiv ?\-)
              (cond
                ;; When PAT-HEAD is DONT-CARE, do nothing:
                ((and dont-care (eq pat-head dont-care))
                  (dm::prn "DONT-CARE, doing nothing."))
                ;; When PAT-HEAD is an ELLIPSIS, nullify TARG-TAIL and PAT-TAIL to break 
                ;; the loop successfully:
                ((and ellipsis (eq pat-head ellipsis))
                  (when (and *dm:enforce-final-position* pat-tail)
                    (error "ELLIPSIS may only be the final element in PATTERN."))
                  ;; `let' TARGET just to print it in this message:
                  (let ((target (cons targ-head targ-tail))) 
                    (dm::prn-labeled target "discarding"))
                  ;; Nullify TARG-TAIL and PAT-TAIL:
                  (setf targ-tail nil)
                  (setf pat-tail  nil))
                ;; When PAT-HEAD is an UNSPLICE, nullify TARG-TAIL and PAT-TAIL to break 
                ;; the loop successfully:
                ((and unsplice (eq unsplice (car-safe pat-head)))
                  (when (and *dm:enforce-final-position* pat-tail)
                    (error "UNSPLICE may only be the final element in PATTERN."))
                  (let ( (var (cadr pat-head)))
                    ;; `let' ASSOC just to print it in this message:
                    (let ((assoc (cons var target)) )
                      (dm::prn-labeled assoc "unsplicing as"))
                    ;; Put the remainder of TARG-TAIL in VAR's key in ALIST:
                    (let ((target (cons targ-head targ-tail)))
                      (setf alist (alist-putunique var target alist 'no-match)))
                    ;; Nullify TARG-TAIL and PAT-TAIL:
                    (setf targ-tail nil)
                    (setf pat-tail  nil)))
                ;; When PAT-HEAD is a variable, stash TARG-HEAD in ALIST:
                ((eq '\, (car-safe pat-head)) 
                  (let ((var (cadr pat-head)))
                    ;; `let' ASSOC just to print it in this message:
                    (let ((assoc (cons var targ-head))) 
                      (dm::prn-labeled assoc "take var as"))
                    (setf alist (alist-putunique var targ-head alist 'no-match))))
                ;; When PAT-HEAD is a list, recurse and accumulate the result into ALIST
                ;; (unless the result was just t because the pat-tail being recursed over
                ;; contained no variables):
                ((and (proper-list-p pat-head) (proper-list-p targ-head))
                  (dm::prn "PAT-HEAD is a list, recurse:")
                  ;; (dm::prndiv)
                  (let ((res (with-indentation
                               (dm::match1 pat-head targ-head
                                 dont-care ellipsis unsplice alist))))
                    (cond
                      ((eq res t)) ; do nothing.
                      ((eq res nil) (NO-MATCH!)) ;; Sub-pattern's tail didn't match.
                      ;; Since`dm::match1' only returns t or lists, so we'll assume it's 
                      ;; now a list.
                      (t (setf alist res)))))
                ;; When PAT-HEAD and TARG-HEAD are equal literals, do nothing:
                ((equal pat-head targ-head)
                  (dm::prn "Equal literals, doing nothing."))
                ;; When the heads aren't equal and we didn't have either a DONT-CARE, an
                ;; ELLIPSIS, a variable, or a list in PAT-HEAD, then no match:
                (t (NO-MATCH!))))
            ;; (debug)
            ) ;; End of (while (and pat-tail targ-tail).
          ;; If we got this far, either PAT-TAIL, TARG-TAIL or both are nil.
          (dm::prndiv)
          (dm::prn-labeled pat-tail  "final")
          (dm::prn-labeled targ-tail "final")
          (cond
            ;; When TARG-TAIL isn't nil, then PAT-TAIL must have ran out before TARG-TAIL,
            ;; no match:
            (targ-tail (NO-MATCH!))
            ;; By this line, TARG-TAIL must be nil. Unless PAT-TAIL is also nil, it had 
            ;; better contain an ELLIPSIS or an UNSPLICE.
            ((null pat-tail)) ;; Don't need to do anything.
            ((and ellipsis (equal (car pat-tail) ellipsis))
              ;; Don't need to do anything other than check for well formedness:
              (when (and *dm:enforce-final-position* (cdr pat-tail))
                (error "ELLIPSIS may only be the final element in PATTERN."))) 
            ;; If PAT-TAIL's head is an UNSPLICE, since there's no TARG-TAIL left we just 
            ;; need to set the var in ALIST to nil:
            ((and unsplice (equal (car-safe (car pat-tail)) unsplice))
              (when (and *dm:enforce-final-position* (cdr pat-tail))
                (error "UNSPLICE may only be the final element in PATTERN."))
              (let ((var (cadar pat-tail)))
                (setf alist (alist-putunique var nil alist 'no-match))))
            ;; It was something else, no match;
            (t (NO-MATCH!))) 
          (dm::prn-labeled alist "final")
          ;; Return either the ALIST or just t:
          (or alist t))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dm:match '(,x ,@ys) '(1 2 3 4))
;; (dm:match '(,x ...) '(1 2 3 4))
;; (dm:match '(,x ... ,z) '(X 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are now all legal:
;; (dm:match '(,y (,y)) '(2 (3))) ; duplicate key in merge!
;; (dm:match '(,y ,y) '(2 3)) ; duplicate key in set!
;; (dm:match '(,y ,@y) '(2 3 4 5)) ; duplicate key in UNSPLICE!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These two are just examples of error cases:
;; (dm:match '(,y ,@zs ...) '(2)) ; malformed, elem after UNSPLICE.
;; (dm:match '(,y ... ,@zs) '(2)) ; malformed, elem after ELLIPSIS.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:tests-enabled*
  (confirm that (dm:match '(w ,x ,y ,z) '(w 1 2 3)) returns ((x . 1) (y . 2) (z . 3)))
  (confirm that (dm:match '(x ,y ,z) '(x 2 3)) returns ((y . 2) (z . 3)))
  (confirm that (dm:match '(x ,y ,z) '(x 2 (3 4 5))) returns ((y . 2) (z 3 4 5)))
  (confirm that (dm:match '(,a ,b ,c \!) '(1 2 3)) returns nil)
  (confirm that (dm:match '(,a ,b ,c) '(1 2 3)) returns ((a . 1) (b . 2) (c . 3)))
  (confirm that (dm:match '(foo _ ,baz) '(foo quux poop)) returns ((baz . poop)))
  (confirm that (dm:match '(foo _ ,baz) '(foo (2 . 3) poop)) returns ((baz . poop)))
  (confirm that (dm:match '(,x ...) '(1 2 3)) returns ((x . 1)))
  (confirm that (dm:match '(,x ...) '(1)) returns ((x . 1)))
  (confirm that (dm:match '(1 2 (,x b ...) 4 ,y) '(1 2 (a b c) 4 5)) returns ((x . a) (y . 5)))
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
    returns ((v . foo) (w . 1) (x . 2) (ys 3 4) (z . 5)))
  (confirm that (dm:match '(,x ,@ys) '(1)) returns ((x . 1) (ys)))
  (confirm that (dm:match '(1 2 3) '(1 2 3)) returns t)
  (confirm that (dm:match '(,1 2 3) '(1 2 3)) returns ((1 . 1)))
  (confirm that (dm:match '(_ ,x ...) '(foo (2 . 3) (4 . 5))) returns ((x 2 . 3)))
  ;; duplicate var examples:
  (confirm that (dm:match '(,x y ,x) '(8 y 8)) returns ((x . 8)))
  (confirm that (dm:match '(,x ,@x) '((1 2 3) 1 2 3)) returns ((x 1 2 3)))
  (confirm that (dm:match '(foo ,x (bar ,x)) '(foo 8 (bar 8))) returns ((x . 8)))
  (confirm that (dm:match '(,x y ,x) '((8 9) y (8 9))) returns ((x 8 9)))
  (confirm that (dm:match '(,x ,y ,x) '((7 8 . 9) 2 (7 8 . 9))) returns ((x 7 8 . 9) (y . 2)))
  (confirm that (dm:match '(,x y ,x) '(8 y 9)) returns nil)
  (confirm that (dm:match '(,x 2 3 ,x) '(nil 2 3 nil)) returns ((x)))
  (confirm that (dm:match '(,x 2 3 ,x) '(1 2 3 nil)) returns nil)
  (confirm that (dm:match '(,x 2 3 ,x) '(nil 2 3 4)) returns nil)
  (confirm that (dm:match '(,a ,b (,c ,d (,f ,g))) '(A B (C D (F G))))
    returns ((a . A) (b . B) (c . C) (d . D) (f . F) (g . G)))
  (confirm that
    (dm:match
      '(one (this that) (,two three (,four ,five) ,six))
      '(one (this that) (2 three (4 5) 6)))  
    returns ((two . 2) (four . 4) (five . 5) (six . 6)))
  (confirm that
    (when-let-alist
      (dm:match
        '(i ,modal-verb ,verb a ,thing)
        '(i have (never seen) a (red car)))
      (flatten `(Do you really believe that you ,.modal-verb ,.verb a ,.thing \?)))
    returns (Do you really believe that you have never seen a red car \?))
  (confirm that
    (when-let-alist
      (dm:match
        '(i ,verb that ,noun ,con ,thing)
        '(i think that dogs are dumb))
      (flatten `(Why do you ,.verb that ,.noun ,.con ,.thing \?)))
    returns (Why do you think that dogs are dumb \?))
  (confirm that
    (when-let-alist
      (dm:match '(i ,modal-verb ,verb a ,thing) '(i have (never seen) a (red car)))
      (flatten `(why do you think that you ,.modal-verb ,.verb a ,.thing \?)))
    returns (why do you think that you have never seen a red car \?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun dm:fill (pattern alist &optional (splice '\,@))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Fill in the variables in PATTERN with the values from ALIST.

This behaves very similarly to quasiquote."
  (dm::prndiv)
  (dm::prn "FILL:")
  (let (res)
    (while pattern
      (let ((thing (pop pattern)))
        (dm::prndiv)
        (dm::prn "thing:   %s" thing)
        (dm::prn "alist:   %s" alist)
        (cond
          ((eq '\, (car-safe thing))
            (let ((var (cadr thing)))
              (if-let ((assoc (assoc var alist)))
                (push (cdr assoc) res)
                (error "var %s not found." var))))
          ((eq splice (car-safe thing))
            (let ((var (cadr thing)))
              (dm::prn "VAR:     %s" var)
              (if-let ((assoc (assoc (cadr thing) alist)))
                (let ((val (cdr assoc)))
                  (dm::prn "VAL:     %s" val)
                  (unless (proper-list-p val)
                    (error "var %s's value %s cannot be spliced, not a list."))
                  (dolist (elem val)
                    (push elem res)))
                (error "var %s not found." (cadr thing)))))
          ((proper-list-p thing) (push (with-indentation (dm:fill thing alist)) res))
          (t (push thing res)))))
    (nreverse res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:tests-enabled*
  (confirm that (dm:fill '(,w x ,y z) '((w . 666) (y . 999)))
    returns (666 x 999 z))
  (confirm that (dm:fill
                  '(,w x ,y (,y , y) z ,w)
                  '((y . 999) (w . (333 666))))
    returns ((333 666) x 999 (999 999) z (333 666)))
  (confirm that (dm:fill '(a ,b (,c ,d))
                  (dm:match '(a ,b (,c ,d)) '(a 2 (3 4))))
    returns (a 2 (3 4)))
  (confirm that
    (dm:fill '(a ,b (,c ,d))
      (dm:match '(a ,b (,c ,d))
        (dm:fill '(a ,b (,c ,d))
          (dm:match '(a ,b (,c ,d))
            '(a 2 (3 4))))))
    returns (a 2 (3 4)))
  (confirm that
    (dm:match '(a ,b (,c ,d))
      (dm:fill '(a ,b (,c ,d))
        (dm:match '(a ,b (,c ,d))
          (dm:fill '(a ,b (,c ,d))
            (dm:match '(a ,b (,c ,d))
              '(a 2 (3 4)))))))
    returns ((b . 2) (c . 3) (d . 4)))
  (confirm that
    (let ( (pattern '(a ,b (,c ,d (,e ,@fs   ))))
           (target  '(a  2 ( 3  4 ( 5   6 7 8)))))
      (dm:fill pattern
        (dm:match pattern
          (dm:fill pattern
            (dm:match pattern
              target)))))
    returns (a 2 (3 4 (5 6 7 8))))
  (confirm that
    (let ( (pattern '(a ,b (,c ,d (,e ,@fs   ))))
           (target  '(a  2 ( 3  4 ( 5   6 7 8)))))
      (dm:match pattern
        (dm:fill pattern
          (dm:match pattern
            (dm:fill pattern
              (dm:match pattern
                target))))))
    returns ((b . 2) (c . 3) (d . 4) (e . 5) (fs 6 7 8))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
