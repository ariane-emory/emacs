;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `dm:match', my destructuring pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--lists)
(require 'aris-funs--strings)
(require 'aris-funs--unsorted)
(require 'aris-funs--when-let-alist) ; Only used by some tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup destructuring-match nil
  "Ari's destructuring pattern matcher.")
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:enable-list-vars* t
  "Describe this."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:verbose* t
  "Whether or not functions in the 'destructuring-match' group should print verbose messages."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:debug* nil 
  "Whether or not the debug breakpoints in 'destructuring-match' are enabled."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:warn-on-consecutive-flexible-elements* t
  "Whether or not 'destructuring-match' should warn if it encounters consecutive flexible pattern elements."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:test-match* t 
  "Whether or not dm:match's unit tests are enabled."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:test-fill* t
  "Whether or not dm:fill's unit tests are enabled."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:label-width* 23
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
  "Print VAR with a label at a given WIDTH, optionally prefixed by EXTRA."
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
(defmacro dm::prn-pp-alist (alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Pretty print ALIST."
  `(when *dm:verbose*
     (let ((alist ,alist))
       (if (null alist)
         (dm::prn-labeled alist)
         (let ((pp-str (indent-string-lines
                         (trim-trailing-whitespace
                           (pp-to-string-without-offset alist)))))
           (if (<= (count-string-lines pp-str) 1)
             (dm::prn-labeled alist)
             (dm::prn "%s:" (upcase (symbol-name ',alist)))
             (mapc #'prn (string-lines pp-str))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::prn-pp-labeled-list (lst-sym &optional extra)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Pretty print LST-SYM with a label."
  `(let* ((,lst-sym (if (cdr ,lst-sym) 
                      (format "%-7s . %s" (car ,lst-sym) (cdr ,lst-sym))
                      (format "%s"        (car ,lst-sym)))))
     (dm::prn-labeled ,lst-sym ,extra)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::log-pop* (&rest lsts)
  "Pop LSTS, logging what was popped from each and returning the value popped from the last list in LSTs."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (macroexp-progn
    (rmapcar lsts
      (lambda (lst)
        `(let ((popped (pop ,lst)))
           (dm::prn "Popped %s from %s, remaining: %s" popped ',lst ,lst)
           popped)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::log-setf-alist-putunique! (key val alist reference-alist)
  "Set KEY to VAL in ALIST, logging the operation and throwing 'no-match if
KEY has a non-`equal' VAL in REFERENCE-ALIST."
  `(prog1
     (setf ,alist (alist-putunique ,key ,val ,alist ,reference-alist 'no-match))
     ;; (dm::prn "Set %s to %s in %s: %s." ,key ,val ',alist ,alist)
     (dm::prn "Set %s to %s in %s." ,key ,val ',alist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ( (al '((b . 2) (c . 3)))
         (*dm:verbose* nil))
    (dm::log-setf-alist-putunique! 'a 123 al al))
  returns ((a . 123) (b . 2) (c . 3)))
(confirm that
  (let ( (al '((a . 123) (b . 2) (c . 3)))
         (*dm:verbose* nil))
    (dm::log-setf-alist-putunique! 'a 123 al al))
  returns ((a . 123) (b . 2) (c . 3)))
(confirm that
  (let ( (al '((b . 2) (c . 3)))
         (ref '((a . 123)))
         (*dm:verbose* nil))
    (dm::log-setf-alist-putunique! 'a 123 al ref))
  returns ((b . 2) (c . 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::pat-elem-is-the-symbol? (symbol pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when SYMBOL is non-nil and `eq' to PAT-ELEM."
  `(and ,symbol (eq ,symbol ,pat-elem)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::pat-elem-is-a-variable? (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when PAT-ELEM describes a variable."
  (let ((comma '\,))
    `(eq ',comma (car-safe ,pat-elem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::pat-elem-is-an-unsplice? (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Expects to be expanded in an environment where UNSPLICE is bound."
  `(and unsplice (eq unsplice (car-safe ,pat-elem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::pat-elem-is-flexible? (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Expects to be expanded in an environment where ELLIPSIS is bound."
  `(or (dm::pat-elem-is-an-unsplice? ,pat-elem) (dm::pat-elem-is-the-symbol? ellipsis ,pat-elem)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::pat-elem-var-sym (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "If PAT-ELEM is a variable / unscplice, return it's name, otherwise return nil."
  `(car-safe (cdr-safe ,pat-elem)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun dm:match ( pattern target
                     &optional
                     (dont-care *dm:default-dont-care*)
                     (ellipsis  *dm:default-ellipsis*)
                     (unsplice  *dm:default-unsplice*))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "My pattern matching/destructuring function."
  (unless (and (listp pattern)
            (listp target)
            (symbolp dont-care)
            (symbolp ellipsis)
            (symbolp unsplice))
    (error
      "PATTERN and TARGET must be lists, DONT-CARE, ELLIPSIS and UNSPLICE must be symbols."))
  (dm::prnl)
  (dm::prndiv)
  (dm::prn "BEGIN MATCHING:       %S" pattern)
  (dm::prn "AGAINST:              %S" target)
  (let* ( (result (with-indentation
                    (dm::match1 pattern pattern target dont-care ellipsis unsplice nil nil)))
          (result (if (listp result) (nreverse result) result)))
    (dm::prn-labeled result "FINAL")
    (dm::prndiv)
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::match1 (initial-pattern pattern target dont-care ellipsis unsplice alist reference-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal function used by `dm:match'."
  ;;-------------------------------------------------------------------------------------------------
  (catch 'no-match
    (setf alist
      (catch 'match
        (dm::prn-labeled pattern "initial")
        (dm::prn-labeled target  "initial")
        ;;-------------------------------------------------------------------------------------------
        (cl-macrolet ((recurse (pattern target alist reference-alist)
                        `(with-indentation
                           (dm::match1 initial-pattern ,pattern ,target
                             dont-care ellipsis unsplice ,alist ,reference-alist)))
                       (NO-MATCH! (fmt &rest args)
                         `(progn
                            (dm::prn "No match because %s!" (format ,fmt ,@args))
                            (throw 'no-match nil)))
                       (warn-when-consecutive-flexible-elements-in-pattern ()
                         ;; Due to `dm::match1's recursive calls to perform lookahead and match 
                         ;; sub-patterns, a call to `dm:match' may trigger this warning more than 
                         ;; once: preventing this wasn't worth the bother, just fix your crappy
                         ;; PATTERN!
                         `(when (and *dm:warn-on-consecutive-flexible-elements*
                                  last-pattern-elem-was-flexible)
                            (let ((warn-msg (format
                                              (concat
                                                "WARNING: Using consecutive flexible elements "
                                                "generally does not make sense, pattern was: %s")
                                              initial-pattern)))
                              (let ( (*dm:verbose* t)
                                     (*wm:indent* 0))
                                (dm::prn warn-msg))
                              (warn warn-msg)))))
          ;;-----------------------------------------------------------------------------------------
          (let (last-pattern-elem-was-flexible)
            ;;---------------------------------------------------------------------------------------
            (while target
              (unless pattern (NO-MATCH! "pattern ran out before TARGET: %s" target))
              (dm::prndiv)
              (dm::prn-pp-alist alist)
              (dm::prn-pp-labeled-list pattern)
              (dm::prn-pp-labeled-list target)
              (let ((target (if (cdr target)
                              (format "%-7s . %s" (car target) (cdr target))
                              (format "%s" (car target))))))
              (dm::prndiv ?\-)
              ;; ------------------------------------------------------------------------------------
              (cond ;; Enter the big `cond'!
                ;; ----------------------------------------------------------------------------------
                ;; Case 1: When PATTERN's head is DONT-CARE, just `pop' the heads off:
                ;; ----------------------------------------------------------------------------------
                ((dm::pat-elem-is-the-symbol? dont-care (car pattern))
                  (setf last-pattern-elem-was-flexible nil)
                  (dm::prn "DONT-CARE, discarding %s." (car target))
                  (dm::log-pop* pattern target))
                ;; ----------------------------------------------------------------------------------
                ;; Case 2: When PATTERN's head is flexible, collect items:
                ;; ----------------------------------------------------------------------------------
                ((dm::pat-elem-is-flexible? (car pattern))
                  (warn-when-consecutive-flexible-elements-in-pattern)
                  (setf last-pattern-elem-was-flexible t)
                  (let ((remaining-non-flexible (cdr pattern)))
                    (while (dm::pat-elem-is-flexible? (car remaining-non-flexible))
                      (dm::log-pop* remaining-non-flexible))
                    ;; If there are no more non-flexible elements in PATTERN we can return early:
                    (unless remaining-non-flexible
                      ;; We know that (car pattern) is flexible, so if it has a var-sym then it
                      ;; must be an UNSPLICE.
                      (when-let ((var-sym (dm::pat-elem-var-sym (dm::log-pop* pattern))))
                        (dm::log-setf-alist-putunique! var-sym target alist alist))
                      (when pattern
                        (dm::prn "RUNNING OUT REMAINING PATTERN: %s" pattern)
                        (dolist (pat-elem pattern)
                          (dm::prn "Running out elem: %s" pat-elem)
                          (warn-when-consecutive-flexible-elements-in-pattern)
                          ;; We know it's flexible, so if it has a var-sym it must be an UNSPLICE.
                          (when-let ((var-sym (dm::pat-elem-var-sym pat-elem)))
                            (dm::log-setf-alist-putunique! var-sym nil alist alist))))
                      (dm::prn-labeled alist "early return")
                      (throw 'match alist))
                    )
                  (dm::prn "Collecting flexible element...")
                  (with-indentation
                    (let (collect) 
                      (catch 'stop-collecting
                        (while t
                          (dm::prndiv)
                          (dm::prn-labeled         collect "pre")
                          (dm::prn-pp-labeled-list pattern)
                          (dm::prn-pp-labeled-list target)                      
                          (let ( (look-0
                                   (let (*dm:verbose*)
                                     (recurse (cdr pattern)      target  nil alist)))
                                 (look-1
                                   (let (*dm:verbose*)
                                     (recurse (cdr pattern) (cdr target) nil alist))))
                            (dm::prndiv ?\-)
                            (dm::prn-labeled look-0)
                            (dm::prn-labeled look-1)
                            (dm::prndiv ?\-)
                            (cond
                              ((null target)
                                (dm::prn "Emptied TARGET, stop.")
                                (throw 'stop-collecting nil))
                              ((and look-0 (not look-1))
                                ;; If LOOK-0 matched the whole TARGET then we can munge
                                ;; LOOK-0 and ALIST into the right shape and return successfully
                                ;; immediately:
                                (setf alist
                                  (nconc
                                    (when (listp look-0) look-0)
                                    (when (dm::pat-elem-is-an-unsplice? (car pattern))
                                      (list
                                        (cons (dm::pat-elem-var-sym (car pattern))
                                          (nreverse collect))))
                                    alist))
                                (dm::prn "CASE 1: Stopping with munged %s!" alist)
                                (throw 'match alist))
                              (t
                                (dm::prn "CASE 2: Nothing else applies, munch %s." (car target))
                                (push (dm::log-pop* target) collect))))
                          (dm::prn-labeled collect "post")
                          (when *dm:debug* (debug 'unsplicing))
                          (dm::prndiv)
                          (dm::prnl)
		                      ) ;; END OF `while' t.
                        ) ;; END OF `catch' STOP-COLLECTING.
                      ;; We already know that (car pattern) is flexible, if it has a var name then
                      ;; it must be an UNSPLICE.
                      (when-let ((var (dm::pat-elem-var-sym (car pattern))))
                        (dm::log-setf-alist-putunique! var (nreverse collect) alist alist))
                      (dm::log-pop* pattern)
                      ) ; end of `let' COLLECT.
                    ) ; end of `with-indentation'.
                  ) ; end of `dm::pat-elem-is-flexible?'s case.
                ;; ----------------------------------------------------------------------------------
                ;; Case 3: When PATTERN's head is a variable, put TARGET's head in ALIST:
                ;; ----------------------------------------------------------------------------------
                ((dm::pat-elem-is-a-variable? (car pattern))
                  (let* ( (var-sym (dm::pat-elem-var-sym (car pattern)))
                          (var-val  (car target))
                          ;; `let' ASSOC just to print it in the message:
                          (assoc    (cons var-sym var-val))) 
                    (setf last-pattern-elem-was-flexible nil)
                    (dm::log-setf-alist-putunique! var-sym var-val alist alist)
                    (dm::prn-labeled assoc "take var as")
                    (dm::log-pop* pattern target)))
                ;; ----------------------------------------------------------------------------------
                ;; Case 4: When PATTERN's head is a list, recurse and accumulate the result into 
                ;; ALIST, unless the result was just t because the sub-pattern being recursed over
                ;; contained no variables:
                ;; ----------------------------------------------------------------------------------
                ((and (proper-list-p (car pattern)) (proper-list-p (car target)))
                  (setf last-pattern-elem-was-flexible nil)
                  (dm::prn "Recursively match %s against %s because PATTERN's head is a list:"
                    (car pattern) (car target))
                  (let ((res (recurse (car pattern) (car target) alist alist)))
                    (cond
                      ((eq res t)) ; do nothing.
                      ((eq res nil) (NO-MATCH! "sub-pattern didn't match"))
                      ;; `dm::match1' only returns t or lists, so by now it must a list.
                      (t (setf alist res))))
                  (dm::log-pop* pattern target))
                ;; ----------------------------------------------------------------------------------
                ;; Case 5: When PATTERN's head and TARG-HEAD are equal literals, just `pop' the 
                ;; heads off:
                ;; ----------------------------------------------------------------------------------
                ((equal (car pattern) (car target))
                  (setf last-pattern-elem-was-flexible nil)
                  (dm::prn "Equal literals, discarding %s." (car target))
                  (dm::log-pop* pattern target))
                ;; ----------------------------------------------------------------------------------
                ;; Otherwise: When the heads aren't `equal' and we didn't have either a DONT-CARE, 
                ;; an ELLIPSIS, a variable, or a list in PATTERN's head, then no match:
                ;; ----------------------------------------------------------------------------------
                (t (NO-MATCH! "expected %s but found %s" (car pattern) (car target)))
                ;; ----------------------------------------------------------------------------------
                ) ; End of the big `cond'.
              ;; ------------------------------------------------------------------------------------
              (dm::prndiv)
              (dm::prnl)
              ;; ------------------------------------------------------------------------------------
              ) ;  end of (while target ...
            ;; --------------------------------------------------------------------------------------
            (dm::prndiv)
            (dm::prn-labeled pattern "final")
            (dm::prn-labeled target  "final")
            ;; --------------------------------------------------------------------------------------
            ;; By this line, TARGET must be nil. Unless PATTERN is also nil, it had better only
            ;; contain ELLIPSISes and UNSPLICEs. Run out the remainder of PATTERN:
            ;; --------------------------------------------------------------------------------------
            (when pattern
              (dm::prn "RUNNING OUT REMAINING PATTERN: %s" pattern)
              (dolist (pat-elem pattern)
                (dm::prn "Running out elem: %s" pat-elem)
                (unless (dm::pat-elem-is-flexible? pat-elem)
                  (NO-MATCH! "expected %s but target is empty" pattern))
                (warn-when-consecutive-flexible-elements-in-pattern)
                (setf last-pattern-elem-was-flexible t)
                ;; We know it's flexible, so, if it has a var-sym now, it must be an UNSPLICE.
                (when-let ((var-sym (dm::pat-elem-var-sym pat-elem)))
                  (dm::log-setf-alist-putunique! var-sym nil alist alist))))))
        alist) ; end of `catch' MATCH.
      ) ; end of `setf' ALIST.
    ;; ----------------------------------------------------------------------------------------------
    ;; Return either the ALIST or just t:
    ;; ----------------------------------------------------------------------------------------------
    (let ((match1-result (or alist t)))
      (dm::prn-labeled match1-result)
      match1-result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-match*
(let ((*dm:verbose* t))
  (confirm that (dm:match '(w ,x ,y ,z)      '(w 1 2 3))         returns ((x . 1) (y . 2) (z . 3)))
  (confirm that (dm:match '(x ,y ,z)         '(x 2 3))           returns ((y . 2) (z . 3)))
  (confirm that (dm:match '(x ,y ,z)         '(x 2 (3 4 5)))     returns ((y . 2) (z 3 4 5)))
  (confirm that (dm:match '(,a ,b ,c \!)     '(1 2 3))           returns nil)
  (confirm that (dm:match '(,a ,b ,c)        '(1 2 3))           returns ((a . 1) (b . 2) (c . 3)))
  (confirm that (dm:match '(foo _ ,baz)      '(foo quux bar))    returns ((baz . bar)))
  (confirm that (dm:match '(foo _ ,baz)      '(foo (2 . 3) bar)) returns ((baz . bar)))
  (confirm that (dm:match '(,x (...))        '(1 nil))           returns ((x . 1)))
  (confirm that (dm:match '(,x ...)          '(1 2 3))           returns ((x . 1)))
  (confirm that (dm:match '(,x ...)          '(1))               returns ((x . 1)))
  (confirm that (dm:match '(,x ,y (,z 4) )   '(1 2 a (3 4) a))   returns nil)
  (confirm that (dm:match '(,x 2 (...) 3 ,y) '(1 2 () 3 4))      returns ((x . 1) (y . 4)))
  (confirm that (dm:match '(,x 2 (...) 3 ,y) '(1 2 (a b c) 3 4)) returns ((x . 1) (y . 4)))
  (confirm that (dm:match '(1 (,foo _) 2)    '(1 (,foo _) 2))    returns ((foo \, foo)))
  (confirm that (dm:match '(,x (,p ...) ,y)  '(1 (q r) 2))       returns ((x . 1) (p . q) (y . 2)))
  (confirm that (dm:match '(,x (,p) ,y)      '(1 (q r) 2))       returns nil)
  (confirm that (dm:match '(,x (,p) ,y)      '(1 () 2))          returns nil)
  (confirm that (dm:match '(,x ,@ys)         '(1 2 3 4))         returns ((x . 1) (ys 2 3 4)))
  (confirm that (dm:match '(,x ,@ys)         '(1))               returns ((x . 1) (ys)))
  (confirm that (dm:match '(1 2 3)           '(1 2 3))           returns t)
  (confirm that (dm:match '(,1 2 3)          '(1 2 3))           returns ((1 . 1)))
  (confirm that (dm:match '(_ ,x ...)        '(foo (2 . 3) 4))   returns ((x 2 . 3)))
  (confirm that (dm:match '(1 2 (,x b ...) 4 ,y) '(1 2 (a b c) 4 5))
    returns ((x . a) (y . 5)))
  (confirm that (dm:match '(1 2 (,x b ...) 4 ,y ...) '(1 2 (a b c) 4 5 6 7 8 9))
    returns ((x . a) (y . 5)))
  (confirm that (dm:match '(,x 2 (,p ...) 3 ,y) '(1 2 (q r) 3 4))
    returns ((x . 1) (p . q) (y . 4)))
  (confirm that
    (dm:match '(,v _ ,w (,x (p q) ,@ys) ,z ...) '(foo bar 1 (2 (p q) 3 4) 5 6 7 8))
    returns ((v . foo) (w . 1) (x . 2) (ys 3 4) (z . 5)))
  ;; duplicate var examples:
  (confirm that (dm:match '(,x y ,x)   '(8 y 8))                 returns ((x . 8)))
  (confirm that (dm:match '(,x ,@x)    '((1 2 3) 1 2 3))         returns ((x 1 2 3)))
  (confirm that (dm:match '(,x y ,x)   '((8 9) y (8 9)))         returns ((x 8 9)))
  (confirm that (dm:match '(,x ,y ,x)  '((7 8 . 9) 2 (7 8 . 9))) returns ((x 7 8 . 9) (y . 2)))
  (confirm that (dm:match '(,x y ,x)   '(8 y 9))                 returns nil)
  (confirm that (dm:match '(,x 2 3 ,x) '(nil 2 3 nil))           returns ((x)))
  (confirm that (dm:match '(,x 2 3 ,x) '(1 2 3 nil))             returns nil)
  (confirm that (dm:match '(,x 2 3 ,x) '(nil 2 3 4))             returns nil)
  (confirm that (dm:match '(foo ,x (bar ,x)) '(foo 8 (bar 8)))   returns ((x . 8)))
  ;; Greediness test cases:
  (confirm that (dm:match '(,x ,@ys foo)     '(1 foo))           returns ((x . 1) (ys)))
  (confirm that (dm:match '(,x ,@ys foo)     '(1 2 3 foo))       returns ((x . 1) (ys 2 3)))
  (confirm that (dm:match '(,x ,@ys)         '(1 2 3 4))         returns ((x . 1) (ys 2 3 4)))
  (confirm that (dm:match '(,x ,@ys)         '(1))               returns ((x . 1) (ys)))
  (confirm that (dm:match '(,x ,@ys)         '(1))               returns ((x . 1) (ys)))
  (confirm that (dm:match '(,x ,@ys)         '(1 2 3 4))         returns ((x . 1) (ys 2 3 4)))
  (confirm that (dm:match '(,x ...)          '(1 2 3 4))         returns ((x . 1)))
  (confirm that (dm:match '(,x ... ,z)       '(X 2 3))           returns ((x . X) (z . 3)))
  ;; stupid-but-legal, consecutive flexible elements:
  (let (*dm:warn-on-consecutive-flexible-elements*)
    (confirm that (dm:match '(,@as ,@bs ,@cs)   '(1 2))          returns ((as 1 2) (bs) (cs)))
    (confirm that (dm:match '(,x ,@ys ,@zs foo) '(1 2 3 foo))    returns ((x . 1) (ys 2 3) (zs)))
    (confirm that (dm:match '(,w ,@xs ,@ys foo ,@zs) '(1 foo))   returns ((w . 1) (xs) (ys) (zs)))
    (confirm that (dm:match '(,w ,@xs foo ,@ys ,@zs) '(1 foo))   returns ((w . 1) (xs) (ys) (zs)))
    (confirm that (dm:match '(,x ,@ys ,@zs) '(1))                returns ((x . 1) (ys) (zs))))
  (confirm that (dm:match '(xx ,@xxs xx)    '(xx xx xx xx xx))   returns ((xxs xx xx xx)))
  (confirm that (dm:match '(... the ,x ...) '(this is your prize))  returns nil)
  (confirm that (dm:match '(... the ,x ...) '(this is the prize))   returns ((x . prize)))
  (confirm that (dm:match '(... the ,x ...) '(the prize is yours))  returns ((x . prize)))
  (confirm that (dm:match '(... the ,x ...) '(here is the prize you found it))
    returns ((x . prize)))
  (confirm that (dm:match '(,foo ... bar _ ... ,baz) '(FOO 1 2 3 4 bar 111)) returns nil)
  (confirm that (dm:match '(,foo ... bar _ ... ,baz) '(FOO 1 2 3 4 bar quux 111))
    returns ((foo . FOO) (baz . 111)))
  (confirm that (dm:match '(,foo ... bar _ ... ,baz) '(FOO 1 2 3 4 bar quux sprungy 111))
    returns ((foo . FOO) (baz . 111)))
  (confirm that (dm:match '(,w ,@xs foo ,@ys bar ,@zs) '(1 2 3 foo bar 8 9))
    returns ((w . 1) (xs 2 3) (ys) (zs 8 9)))
  (confirm that (dm:match '(,w ,@xs foo ,@ys bar ,@zs) '(1 2 3 foo 4 5 6 7 bar 8 9))
    returns ((w . 1) (xs 2 3) (ys 4 5 6 7) (zs 8 9)))
  
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
    returns (why do you think that you have never seen a red car \?))
  (confirm that (dm:match '(,a ,b (,c ,d (,f ,g))) '(A B (C D (F G))))
    returns ((a . A) (b . B) (c . C) (d . D) (f . F) (g . G)))
  ;; parse fun bodies:
  (confirm that
    (dm:match '(,form ,name (,@args) ,@body)
      '(defmacro foo () (prn "foo") :FOO (car bazes)))
    returns ( (form . defmacro) (name . foo) (args)
              (body
                (prn "foo")
                :FOO
                (car bazes))))
  (confirm that
    (dm:match '(,form ,name (,@args) ,@body) '(defmacro foo nil (prn "foo") :FOO :BAR))
    returns ( (form . defmacro) (name . foo) (args)
              (body
                (prn "foo")
                :FOO :BAR)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun dm:fill (pattern alist
                    &optional
                    (unsplice *dm:default-unsplice*)
                    (ellipsis  *dm:default-ellipsis*)
                    (dont-care *dm:default-dont-care*))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Fill in the variables in PATTERN with the values from ALIST.

This behaves very similarly to quasiquote."  
  (dm::prndiv)
  (dm::prn "FILL:")
  (let (res)
    (while pattern
      (let ((thing (dm::log-pop* pattern)))
        (dm::prndiv)
        (dm::prn "thing:   %s" thing)
        (dm::prn "alist:   %s" alist)
        (cond
          ((dm::pat-elem-is-the-symbol? dont-care thing)
            (push (when alist (cdr-safe (pick alist))) res))
          ((dm::pat-elem-is-the-symbol? ellipsis thing)
            (let (random-var-values)
              (while (= 0 (% (random) 2))
                (push (when alist (cdr-safe (pick alist))) random-var-values))
              (dolist (elem random-var-values)
                (push elem res))))
          ((dm::pat-elem-is-a-variable? thing)
            (if-let ((assoc (assoc (dm::pat-elem-var-sym thing) alist)))
              (push (cdr assoc) res)
              (error "var %s not found." (dm::pat-elem-var-sym thing))))
          ((dm::pat-elem-is-an-unsplice? thing)
            (let ((var (dm::pat-elem-var-sym thing)))
              (dm::prn "VAR:     %s" var)
              (if-let ((assoc (assoc (cadr thing) alist)))
                (let ((val (cdr assoc)))
                  (dm::prn "VAL:     %s" val)
                  (unless (proper-list-p val)
                    (error "var %s's value %s cannot be spliced, not a list."))
                  (dolist (elem val)
                    (push elem res)))
                (error "var %s not found." (cadr thing)))))
          ((proper-list-p thing)
            (push (with-indentation (dm:fill thing alist unsplice ellipsis dont-care)) res))
          (t (push thing res)))))
    (nreverse res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-fill*
  (let (*dm:verbose*)
    (confirm that (dm:fill '(,w x ,@ys z) '((w . 666) (ys 999 888 777)))
      returns (666 x 999 888 777 z))
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
      (let ( (pattern '(a ,b (,c ,d (,e ,@fs    ))))
             (target  '(a  2 ( 3  4 ( 5   6 7 8)))))
        (dm:match pattern
          (dm:fill pattern
            (dm:match pattern
              (dm:fill pattern
                (dm:match pattern
                  target))))))
      returns ((b . 2) (c . 3) (d . 4) (e . 5) (fs 6 7 8)))
    (confirm that
      (dm:fill '(defmacro ,name (,arg) `(progn ,@body))
        (dm:match '(,form ,name (,arg) ,@body)
          '(defun foo (bar) (prn "foo") :FOO (car bar))))
      returns (defmacro foo (bar)
                `(progn
                   (prn "foo")
                   :FOO
                   (car bar))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dm:match '(foo ,(bar baz) quux) '(foo qqq quux))
