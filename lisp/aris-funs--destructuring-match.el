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
  "Whether or not functions in the 'destructuring-match' group should print verbose messages."
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
(cl-defmacro dm::prn-labeled (var &optional extra (width *dm:label-width*))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Print VAR with a label at a given WIDTH, optionally prefixed by LABEL."
  (let* ( (label  (concat (upcase (symbol-name var)) ":"))
          (extra  (if (null extra) "" (capitalize1 (concat extra " "))))
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
    (if (null alist)
      (dm::prn-labeled alist)
      (let ((pp-str (indent-string-lines
                      (trim-trailing-whitespace
                        (pp-to-string-without-offset alist)))))
        ;; (if (<= (count-string-lines pp-str) 1)
        ;;   (dm::prn-labeled alist)
        (dm::prn "ALIST:")
        (mapc #'prn (string-lines pp-str)))))) ; )
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
    (error
      "PATTERN and TARGET must be lists, DONT-CARE, ELLIPSIS and UNSPLICE must be symbols."))
  (dm::prnl)
  (dm::prndiv)
  (dm::prn "BEGIN MATCH:          %S" pattern)
  (dm::prn "AGAINST:              %S" target)
  ;; (dm::prndiv)
  ;; (dm::prnl)
  (let* ( (result (with-indentation (dm::match1 pattern target dont-care ellipsis unsplice nil)))
          (result (if (listp result) (nreverse result) result)))
    ;; (dm::prndiv)
    (dm::prn-labeled result "FINAL")
    (dm::prndiv)
    ;; (dm::prnl)
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::match1 (pattern target dont-care ellipsis unsplice alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal function used by `dm:match'."
  (cl-flet ((NO-MATCH! (fmt &rest args)
              (let ((str (apply #'format fmt args)))
                (dm::prn "No match because %s!" str)
                (throw 'no-match nil))))
    ;;-----------------------------------------------------------------------------------------------
    (catch 'no-match
      (while target
        (unless pattern (NO-MATCH! "pattern ran out before TARGET"))
        (dm::prndiv)
        (dm::prn-pp-alist alist)
        (dm::prndiv ?\-)
        (when *dm:verbose*
          (let ((pattern (if (cdr pattern) ; let PATTERN just for printing.
                           (format "%-7s . %s" (car pattern) (cdr pattern))
                           (format "%s" (car pattern)))))
            (dm::prn-labeled pattern))
          (let ((target  (if (cdr target)
                           (format "%-7s . %s" (car target) (cdr target))
                           (format "%s" (car target)))))
            (dm::prn-labeled target)))
        (dm::prndiv ?\-)
        (cond
          ;; ----------------------------------------------------------------------------------------
          ;; Case 1: When PATTERN's head is DONT-CARE, just `pop' the heads off:
          ;; ----------------------------------------------------------------------------------------
          ((and dont-care (eq (car pattern) dont-care))
            (dm::prn "DONT-CARE, discarding %s." (car target))
            (pop pattern)
            (pop target))
          ;; ----------------------------------------------------------------------------------------
          ;; Case 2: When PATTERN's head is an ELLIPSIS, nullify TARGET and PATTERN to break 
          ;; the loop successfully:
          ;; ----------------------------------------------------------------------------------------
          ((and ellipsis (eq (car pattern) ellipsis))
            (when (and *dm:enforce-final-position* (cdr pattern))
              (error "ELLIPSIS may only be the final element in PATTERN."))
            (dm::prn-labeled target "discarding")
            ;; Nullify TARGET and PATTERN:
            (setf pattern nil)
            (setf target  nil))
          ;; ----------------------------------------------------------------------------------------
          ;; Case 3: When PATTERN's head is an UNSPLICE, nullify TARGET and PATTERN to break 
          ;; the loop successfully:
          ;; ----------------------------------------------------------------------------------------
          ((and unsplice (eq unsplice (car-safe (car pattern))))
            (when (and *dm:enforce-final-position* (cdr pattern))
              (error "UNSPLICE may only be the final element in PATTERN."))
            (let* ( (unsplice-var-spec (car  pattern))
                    (unsplice-var-name (cadr unsplice-var-spec))
                    (unsplice-var-val  target)
                    ;; `let' ASSOC just to print it in the message:
                    (assoc (cons unsplice-var-name unsplice-var-val)))
              (dm::prn-labeled assoc "unsplicing as")
              ;; Put UNSPLICE-VAR-VAL in UNSPLICE-VAR-NAME's key in ALIST:
              (setf alist (alist-putunique unsplice-var-name unsplice-var-val alist 'no-match)))
            ;; Nullify TARGET and PATTERN:
            (setf pattern nil)
            (setf target  nil))
          ;; ----------------------------------------------------------------------------------------
          ;; Case 4: When PATTERN's head is a variable, put TARGET's head in ALIST:
          ;; ----------------------------------------------------------------------------------------
          ((eq '\, (car-safe (car pattern)))
            (let* ( (var-spec (car  pattern))
                    (var-val  (car  target))
                    (var-name (cadr var-spec))
                    ;; `let' ASSOC just to print it in the message:
                    (assoc    (cons var-name var-val))) 
              (dm::prn-labeled assoc "take var as")
              (setf alist (alist-putunique var-name var-val alist 'no-match))
              (pop  pattern)
              (pop  target)))
          ;; ----------------------------------------------------------------------------------------
          ;; Case 5: When PATTERN's head is a list, recurse and accumulate the result into 
          ;; ALIST, unless the result was just t because the sub-pattern being recursed over
          ;; contained no variables:
          ;; ----------------------------------------------------------------------------------------
          ((and (proper-list-p (car pattern)) (proper-list-p (car target)))
            (let ( (sub-pattern (car pattern))
                   (sub-target  (car target)))
              (dm::prn "Recursively match %s against %s because PATTERN's head is a list:"
                sub-pattern sub-target)
              ;; (dm::prndiv)
              (let ((res (with-indentation
                           (dm::match1 sub-pattern sub-target dont-care ellipsis unsplice alist))))
                (cond
                  ((eq res t)) ; do nothing.
                  ((eq res nil) (NO-MATCH! "sub-pattern didn't match"))
                  ;; `dm::match1' only returns t or lists, so we'll now assume it's a list.
                  (t (setf alist res))))
              (pop pattern)
              (pop target)))
          ;; ----------------------------------------------------------------------------------------
          ;; Case 6: When PATTERN's head and TARG-HEAD are equal literals, just `pop' the heads off:
          ;; ----------------------------------------------------------------------------------------
          ((equal (car pattern) (car target))
            (dm::prn "Equal literals, discarding %s." (car target))
            (pop pattern)
            (pop target))
          ;; ----------------------------------------------------------------------------------------
          ;; Otherwise: When the heads aren't equal and we didn't have either a DONT-CARE, an
          ;; ELLIPSIS, a variable, or a list in PATTERN's head, then no match:
          ;; ----------------------------------------------------------------------------------------
          (t (NO-MATCH! "expected %s but found %s" (car pattern) (car target)))) ; End of `cond'.
        ;; ----------------------------------------------------------------------------------------
        (dm::prndiv)
        (dm::prnl)
        );; End of (while target ...), if we got this far TARGET is nil!
      (dm::prndiv)
      (dm::prn-labeled pattern  "final")
      (dm::prn-labeled target "final")
      (cond
        ;; By this line, TARGET must be nil. Unless PATTERN is also nil, it had 
        ;; better contain an ELLIPSIS or an UNSPLICE.
        ((null pattern)) ;; Don't need to do anything.
        ((and ellipsis (equal (car pattern) ellipsis))
          ;; Don't need to do anything other than check for well formedness:
          (when (and *dm:enforce-final-position* (cdr pattern))
            (error "ELLIPSIS may only be the final element in PATTERN (case #2)."))) 
        ;; If PATTERN's head is an UNSPLICE, since there's no TARGET left we just 
        ;; need to set the var in ALIST to nil:
        ((and unsplice (equal (car-safe (car pattern)) unsplice))
          (when (and *dm:enforce-final-position* (cdr pattern))
            (error "UNSPLICE may only be the final element in PATTERN (case #2)\."))
          (let ((var (cadar pattern)))
            (setf alist (alist-putunique var nil alist 'no-match))))
        ;; It was something else, no match;
        (t (NO-MATCH! "expected %s but target is empty" pattern))) 
      (dm::prn-labeled alist "final")
      ;; Return either the ALIST or just t:
      (or alist t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (dm:match '(,x ,@ys 4) '(1 2 3 4))
;; (dm:match '(,x ,@ys) '(1 2 3 4))
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
;; (when *dm:tests-enabled*
;;   (confirm that (dm:fill '(,w x ,y z) '((w . 666) (y . 999)))
;;     returns (666 x 999 z))
;;   (confirm that (dm:fill
;;                   '(,w x ,y (,y , y) z ,w)
;;                   '((y . 999) (w . (333 666))))
;;     returns ((333 666) x 999 (999 999) z (333 666)))
;;   (confirm that (dm:fill '(a ,b (,c ,d))
;;                   (dm:match '(a ,b (,c ,d)) '(a 2 (3 4))))
;;     returns (a 2 (3 4)))
;;   (confirm that
;;     (dm:fill '(a ,b (,c ,d))
;;       (dm:match '(a ,b (,c ,d))
;;         (dm:fill '(a ,b (,c ,d))
;;           (dm:match '(a ,b (,c ,d))
;;             '(a 2 (3 4))))))
;;     returns (a 2 (3 4)))
;;   (confirm that
;;     (dm:match '(a ,b (,c ,d))
;;       (dm:fill '(a ,b (,c ,d))
;;         (dm:match '(a ,b (,c ,d))
;;           (dm:fill '(a ,b (,c ,d))
;;             (dm:match '(a ,b (,c ,d))
;;               '(a 2 (3 4)))))))
;;     returns ((b . 2) (c . 3) (d . 4)))
;;   (confirm that
;;     (let ( (pattern '(a ,b (,c ,d (,e ,@fs   ))))
;;            (target  '(a  2 ( 3  4 ( 5   6 7 8)))))
;;       (dm:fill pattern
;;         (dm:match pattern
;;           (dm:fill pattern
;;             (dm:match pattern
;;               target)))))
;;     returns (a 2 (3 4 (5 6 7 8))))
;;   (confirm that
;;     (let ( (pattern '(a ,b (,c ,d (,e ,@fs   ))))
;;            (target  '(a  2 ( 3  4 ( 5   6 7 8)))))
;;       (dm:match pattern
;;         (dm:fill pattern
;;           (dm:match pattern
;;             (dm:fill pattern
;;               (dm:match pattern
;;                 target))))))
;;     returns ((b . 2) (c . 3) (d . 4) (e . 5) (fs 6 7 8))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(dm:match '(1 (2 3)) '(1 2))
