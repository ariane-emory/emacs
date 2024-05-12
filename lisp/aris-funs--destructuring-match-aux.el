;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions used by `dm:match', my destructuring pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--lists)
(require 'aris-funs--sym-db)
(require 'aris-funs--destructuring-match-vars)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print helpers:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prn (&rest args)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:verbose* (apply #'prn args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prnl (&optional count)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:verbose* (prnl count)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::prndiv (&optional char)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Internal print helper function."
  (when *dm:verbose* (funcall #'prndiv (or char ?\=) *dm:div-width*)))
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
                      (format "%-12s . %s" (car ,lst-sym) (cdr ,lst-sym))
                      (format "%s"        (car ,lst-sym)))))
     (dm::prn-labeled ,lst-sym ,extra)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pat element testing/accessor macros::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro dm::pat-elem-is-the-symbol? (symbol pat-elem)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "t when SYMBOL is non-nil and `eq' to PAT-ELEM."
;;   `(and ,symbol (eq ,symbol ,pat-elem)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::pat-elem-is-the-symbol? (symbol pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when SYMBOL is non-nil and `eq' to PAT-ELEM."
  (and symbol (eq symbol pat-elem)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::pat-elem-is-the-symbol? 'foo 'foo) returns t)
(confirm that (dm::pat-elem-is-the-symbol? 'foo 1234) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro dm::pat-elem-is-a-variable? (pat-elem)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "t when PAT-ELEM describes a variable."
;;   (let ((comma '\,))
;;     `(eq ',comma (car-safe ,pat-elem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::pat-elem-is-a-variable? (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when PAT-ELEM describes a variable."
  (let ((comma '\,))
    (eq comma (car-safe pat-elem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::pat-elem-is-an-unsplice? (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when PAT-ELEM is an unsplice."
  `(and unsplice (eq unsplice (car-safe ,pat-elem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that (dm::pat-elem-is-an-unsplice? '\,@ '(\,@ 1 2 3)) returns t)
;; (confirm that (dm::pat-elem-is-an-unsplice? '\,@ '(1 2 3)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dm::pat-elem-is-flexible? (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Expects to be expanded in an environment where ELLIPSIS is bound."
  `(or (dm::pat-elem-is-an-unsplice? ,pat-elem) (dm::pat-elem-is-the-symbol? ellipsis ,pat-elem)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro dm::pat-elem-var-sym (pat-elem)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "If PAT-ELEM is a variable / unsplice, return it's name, otherwise return nil."
;;   `(let ((2nd (car-safe (cdr-safe ,pat-elem))))
;;      (if (atom 2nd)
;;        2nd
;;        (car 2nd))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::pat-elem-var-sym (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "If PAT-ELEM is a variable / unsplice, return it's name, otherwise return nil."
  (let ((2nd (car-safe (cdr-safe pat-elem))))
    (if (atom 2nd)
      2nd
      (car 2nd))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::pat-elem-var-sym  123)        returns nil)
(confirm that (dm::pat-elem-var-sym  ',x)        returns x)
(confirm that (dm::pat-elem-var-sym  ',(x y z))  returns x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro dm::pat-elem-var-preds (pat-elem)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "If PAT-ELEM is a variable / unsplice, return it's predicates, otherwise return nil."
;;   `(let ((2nd (car-safe (cdr-safe ,pat-elem))))
;;      (if (atom 2nd)
;;        nil
;;        (cdr 2nd))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::pat-elem-var-preds (pat-elem)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "If PAT-ELEM is a variable / unsplice, return it's predicates, otherwise return nil."
  (let ((2nd (car-safe (cdr-safe pat-elem))))
    (if (atom 2nd)
      nil
      (cdr 2nd))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (dm::pat-elem-var-preds 123)       returns nil)
(confirm that (dm::pat-elem-var-preds ',x)       returns nil)
(confirm that (dm::pat-elem-var-preds ',(x y z)) returns (y z))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logged actions:
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
;; properize targets:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-target! (improper-indicator lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (properize! lst t improper-indicator))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-target! *dm:default-improper-indicator* nil)  returns nil)
  (confirm that (dm::properize-target! *dm:default-improper-indicator* '(2)) returns (2))
  (confirm that (dm::properize-target! *dm:default-improper-indicator* '(2 . 4))   returns (2 \. 4))
  (confirm that (dm::properize-target! *dm:default-improper-indicator* '(2 4   6)) returns (2 4 6))
  (confirm that (dm::properize-target! *dm:default-improper-indicator* '(2 4 . 6)) returns (2 4 \. 6))
  (let ((lst '(2 4 . 6)))
    (confirm that (dm::properize-target! *dm:default-improper-indicator* lst) returns (2 4 \. 6))
    (confirm that lst returns (2 4 \. 6))))
;; don't confirm: (dm::properize-target! '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-target (improper-indicator lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a target by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  (properize lst t improper-indicator))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  (confirm that (dm::properize-target *dm:default-improper-indicator*  nil)       returns nil)
  (confirm that (dm::properize-target *dm:default-improper-indicator* '(2))       returns (2))
  (confirm that (dm::properize-target *dm:default-improper-indicator* '(2 . 4))   returns (2 \. 4))
  (confirm that (dm::properize-target *dm:default-improper-indicator* '(2 4   6)) returns (2 4 6))
  (confirm that (dm::properize-target *dm:default-improper-indicator* '(2 4 . 6)) returns (2 4 \. 6)))
;; don't confirm: (dm::properize-target '(2 . 4 6)) ... ; bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; properize patterns:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern! (improper-indicator ellipsis dont-care unsplice lst) ; shallowly recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (let (not-first)
    (dolist* (head pos lst lst)
      (cond
        ((and (cdr pos) (atom (cdr pos)))
          ;; found an improper tail, properize it:
          (setcar pos
            (if (atom head)
              head
              (dm::properize-pattern! improper-indicator ellipsis dont-care unsplice head)))
          (setcdr pos (list improper-indicator (cdr pos)))
          (setf pos nil))
        ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
          ;; found a wayward comma, fix it:
          (setcar pos *dm:default-improper-indicator*)
          (setcdr pos (list (list head (cadr pos))))
          (setf pos nil))
        ((consp head)
          (unless (eq '\, (car head))
            (setcar pos (dm::properize-pattern!
                          improper-indicator ellipsis dont-care unsplice head)))))
      (setf not-first t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  ;; (unless (eq '\, (caar lst))
  (confirm that
    (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
      *dm:default-dont-care* *dm:default-unsplice* 
      '(,w ,(x integer? . foo) . ,(y integer? . foo)))
    returns ((\, w) (\,(x integer? . foo)) \. (\,(y integer? . foo))))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*   nil)
    returns nil)
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x))
    returns ((\, x)))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x .  y))
    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x . ,y))
    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x . (y z)))
    returns ((\, x) y z))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x  y .  z))
    returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x ,y   ,z))
    returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x ,y .  z))
    returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x ,y . ,z))
    returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '((w y) .  z))
    returns ((w y) \. z))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '((w . y) .  z))
    returns ((w \. y) \. z))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '((w y) .  ,z))
    returns ((w y) \. (\, z)))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '((w . y) .  ,z))
    returns ((w \. y) \. (\, z)))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern! due to the way ,z is used in the innermost
  ;;sub-expression,but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure
  ;; it doesn't try: 
  (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?))))
  (let ((lst  '(,v (,w ,x . ,y) . ,z)))
    (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                    *dm:default-dont-care* *dm:default-unsplice*  lst)
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
    (confirm that lst
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z))))
  (let ((lst '(,x .  y)))
    (confirm that (dm::properize-pattern! *dm:default-improper-indicator* *dm:default-ellipsis*
                    *dm:default-dont-care* *dm:default-unsplice*  lst)
      returns ((\, x) \. y))
    (confirm that lst                                  returns ((\, x) \. y))))
;; don't confirm: (dm::properize-pattern! *dm:default-improper-indicator* '(,x . ,y ,z)) ...
;; ^ bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; improper-indicator ellipsis dont-care unsplice
;; *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care* *dm:default-unsplice*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern (improper-indicator ellipsis dont-care unsplice lst) ; shallowly recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  ;; (dm::prndiv)
  (let (not-first res)
    (dolist* (head pos lst (nreverse res))
      (cond
        ((and (cdr pos) (atom (cdr pos)))
          ;; found an improper tail, properize it.
          ;; this isn't sexy, but it turns out that nested `cons'es and a conjoined
          ;; `setf' is very slightly faster than the alternatives:
          (setq res
            (cons (cdr pos)
              (cons improper-indicator
                (cons
                  (if (atom head)
                    head
                    (dm::properize-pattern improper-indicator ellipsis dont-care unsplice head))
                  res)))
            pos nil))
        ((and not-first (eq '\, head) (cdr pos) (not (cddr pos)))
          ;; found a wayward comma, fix it:
          (setq res (cons (list head (cadr pos)) (cons improper-indicator res)))
          (setq pos nil))
        ((atom head) (push head res))
        ((consp head)
          (push (if (eq '\, (car head))
                  head
                  (dm::properize-pattern improper-indicator ellipsis dont-care unsplice head))
            res))
        (t (push (dm::properize-pattern improper-indicator ellipsis dont-care unsplice head) res)))
      (setq not-first t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  ;; (unless (eq '\, (caar lst))
  (confirm that
    (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
      *dm:default-dont-care* *dm:default-unsplice* 
      '(,w ,(x integer? . foo) . ,(y integer? . foo)))
    returns ((\, w) (\,(x integer? . foo)) \. (\,(y integer? . foo))))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice*  nil)
    returns nil)
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x))
    returns ((\, x)))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x .  y))
    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x . ,y))
    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x . (y z)))
    returns ((\, x) y z))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x  y .  z))
    returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x ,y   ,z))
    returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x ,y .  z))
    returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x ,y . ,z))
    returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '((w y) .  z))
    returns ((w y) \. z))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '((w . y) .  z))
    returns ((w \. y) \. z))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '((w y) .  ,z))
    returns ((w y) \. (\, z)))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '((w . y) .  ,z))
    returns ((w \. y) \. (\, z)))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern due to the way ,z is used in the innermost sub-expression,
  ;; but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure it doesn't try: 
  (confirm that (dm::properize-pattern *dm:default-improper-indicator* *dm:default-ellipsis*
                  *dm:default-dont-care* *dm:default-unsplice* '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?)))))
;; don't confirm: (dm::properize-pattern *dm:default-improper-indicator* '(,x . ,y ,z)) ...
;; ^ bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern!* (improper-indicator ellipsis dont-care unsplice lst &optional not-first) ; deeply recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (cond
    ((and (cdr lst) (atom (cdr lst)))
      ;;(dm::prn "case 2")
      ;; found an improper tail, properize it:
      (when (consp (car lst))
        (setcar lst (dm::properize-pattern!* improper-indicator ellipsis dont-care unsplice (car lst) nil)))
      (setcdr lst (list improper-indicator (cdr lst))))
    ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
      ;;(dm::prn "case 3")
      ;; found a wayward comma, fix it:
      ;; (debug (cadr lst))
      (let ((comma (car lst)))
        (setcar lst improper-indicator)
        (setcdr lst (list (list comma (cadr lst))))))
    ((consp (car lst))
      ;;(dm::prn "case 4")
      (unless (eq '\, (caar lst))
        (setcar lst (dm::properize-pattern!* improper-indicator ellipsis dont-care unsplice (car lst) nil)))
      (dm::properize-pattern!* improper-indicator ellipsis dont-care unsplice (cdr lst) t))
    ((cdr lst) ; (atom (car lst))
      ;;(dm::prn "case 5: %s" (cdr lst))
      (dm::properize-pattern!* improper-indicator ellipsis dont-care unsplice (cdr lst) t)))
  lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  ;; (unless (eq '\, (caar lst))
  (confirm that
    (dm::properize-pattern!*
      *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
      *dm:default-unsplice* '(,w ,(x integer? . foo) . ,(y integer? . foo)))
    returns ((\, w) (\,(x integer? . foo)) \. (\,(y integer? . foo))))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x))
    returns ((\, x)))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice*  nil)
    returns nil)
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x .  y))
    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x . ,y))
    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x . (y z)))
    returns ((\, x) y z))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x  y .  z))
    returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y   ,z))
    returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y .  z))
    returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y . ,z))
    returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((w y) .  z))
    returns ((w y) \. z))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((w . y) .  z))
    returns ((w \. y) \. z))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((w y) .  ,z))
    returns ((w y) \. (\, z)))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((w . y) .  ,z))
    returns ((w \. y) \. (\, z)))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern!* due to the way ,z is used in the innermost
  ;; sub-expression, but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure
  ;; it doesn't try: 
  (confirm that (dm::properize-pattern!*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?))))
  (let ((lst  '(,v (,w ,x . ,y) . ,z)))
    (confirm that (dm::properize-pattern!*
                    *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                    *dm:default-unsplice* lst)
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
    (confirm that lst
      returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z))))
  (let ((lst '(,x .  y)))
    (confirm that (dm::properize-pattern!*
                    *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                    *dm:default-unsplice* lst)
      returns ((\, x) \. y))
    (confirm that lst                                   returns ((\, x) \. y))))
;; don't confirm: (dm::properize-pattern!* *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care* *dm:default-unsplice* '(,x . ,y ,z)) ...
;; ^ bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::properize-pattern* (improper-indicator ellipsis dont-care unsplice lst
                                &optional not-first) ; deeply recursive version.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Non-destructively properize a pattern by inserting a 'properize symbol', '\."
  ;; flexible pattern elements in final position will need to dodge the properize symbol '\.
  ;; flexible elements following the properize symbol should be illegal?
  (cond
    ((null lst) lst)
    ((and (cdr lst) (atom (cdr lst)))
      ;; found an improper tail, properize it:
      (list
        (if (consp (car lst))
          (dm::properize-pattern* improper-indicator ellipsis dont-care unsplice (car lst) nil)
          (car lst))
        improper-indicator (cdr lst)))
    ((and not-first (eq '\, (car lst)) (cdr lst) (not (cddr lst)))
      ;; found a wayward comma, fix it:
      (list improper-indicator (list (car lst) (cadr lst))))
    ((consp (car lst))
      (cons
        (if (eq '\, (caar lst))
          (car lst)
          (dm::properize-pattern* improper-indicator ellipsis dont-care unsplice (car lst) nil))
        (dm::properize-pattern* improper-indicator ellipsis dont-care unsplice (cdr lst) t)))
    (t
      (cons
        (car lst)
        (dm::properize-pattern* improper-indicator ellipsis dont-care unsplice (cdr lst) t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when *dm:test-aux*
  ;; (unless (eq '\, (caar lst))
  (confirm that
    (dm::properize-pattern*
      *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
      *dm:default-unsplice* '(,w ,(x integer? . foo) . ,(y integer? . foo)))
    returns ((\, w) (\,(x integer? . foo)) \. (\,(y integer? . foo))))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,w ,(x integer? . foo)))
    returns ((\, w) (\,(x integer? . foo))))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x))
    returns ((\, x)))  
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice*  nil)
    returns nil)
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x))
    returns ((\, x)))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x .  y))
    returns ((\, x) \. y))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x . ,y))
    returns ((\, x) \. (\, y)))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x . (y z)))
    returns ((\, x) y z))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x  y .  z))
    returns ((\, x) y \. z))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y   ,z))
    returns ((\, x) (\, y) (\, z)))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y .  z))
    returns ((\, x) (\, y) \. z))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y . ,z))
    returns ((\, x) (\, y) \. (\, z)))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((w y) .  z))
    returns ((w y) \. z))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((w . y) .  z))
    returns ((w \. y) \. z))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((w y) .  ,z))
    returns ((w y) \. (\, z)))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((w . y) .  ,z))
    returns ((w \. y) \. (\, z)))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '((,w . ,y) . ,z))
    returns (((\, w) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,v (,w ,x . ,y) . ,z))
    returns ((\, v) ((\, w) (\, x) \. (\, y)) \. (\, z)))
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y . ,(z  integer?)))
    returns ((\, x) (\, y) \. (\, (z integer?))))
  ;; this one would not be a legal pattern due to the way ,z is used in the innermost sub-expression,
  ;; but it isn't `dm::properize-pattern's job to try to fix, it, so let's make sure it doesn't try: 
  (confirm that (dm::properize-pattern*
                  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
                  *dm:default-unsplice* '(,x ,y . ,(,z integer?)))
    returns ((\, x) (\, y) \. (\,((\, z) integer?)))))
;; don't confirm: (dm::properize-pattern* *dm:default-improper-indicator* '(,x . ,y ,z)) ... ;bullshit input, invalid read syntax!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro dm::walk* (spec &rest body)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "A version of `dolist` that also handles improper lists."
;;   (unless (consp spec)
;;     (signal 'wrong-type-argument (list 'consp spec)))
;;   (unless (<= 3 (length spec) 4)
;;     (signal 'wrong-number-of-arguments (list '(3 . 4) (length spec))))
;;   (let* ( (var (car spec))
;;           (pos (car (cdr spec)))
;;           (lst (car (cdr (cdr spec)))))
;;     `(let ((,pos ,lst))
;;        (while ,pos
;;          (let ((,var (if (consp pos) (car ,pos) ,pos)))
;;            ,@body
;;            (setf ,pos (if (consp pos) (cdr ,pos) nil))))
;;        ,@(cdr (cdr (cdr spec))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unproperize things:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun dm::unproperize!* (improper-indicator lst)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Turn lists with *dm:default-improper-indicator* in their second last position back into improper lists."
;;   (let ((pos lst))
;;     (dm::prndiv)
;;     (while (consp pos)
;;       (if (atom pos)
;;         (dm::prn "atom:     %s"  pos)
;;         (dm::prn "head:     %s" (car pos))
;;         (when (consp (car pos))
;;           (with-indentation
;;             (dm::unproperize!* improper-indicator (car pos))))
;;         (when (eq (cadr-safe pos) improper-indicator)
;;           (when (or (cadddr pos) (not (cddr pos)))
;;             (error "properize indicator in unexpected position: %s" lst))
;;           (setcdr pos (caddr pos))
;;           (setf pos nil))
;;         (pop pos))))
;;   (dm::prn "lst:      %s" lst)
;;   ;; (dm::prndiv)
;;   lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::unproperize!* (improper-indicator lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn lists with *dm:default-improper-indicator* in their second last position back into improper lists."
  (dolist* (elem pos lst lst)
    (dm::prndiv)
    ;; (prn "pos:   %S" pos)
    ;; (prn "elem:  %S" elem)
    (when (consp elem)
      (with-indentation
        (dm::unproperize!* improper-indicator elem)))
    (when (eq improper-indicator (cadr-safe pos))
      (when (or (cadddr pos) (not (cddr pos)))
        (error "properize indicator in unexpected position: %S" lst))
      (setcdr pos (caddr pos)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADD TESTS!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmark;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore!
  (progn
    (setq reps 100000)

    (list
      (benchmark-run reps (dm::properize-pattern!*
                            *dm:default-improper-indicator* *dm:default-ellipsis*
                            *dm:default-dont-care* *dm:default-unsplice* 
                            '(,v (,w ,x (a . (b c . d)) (,a b ,c ,(d integer? . e)) ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern!
                            *dm:default-improper-indicator* *dm:default-ellipsis*
                            *dm:default-dont-care* *dm:default-unsplice* 
                            '(,v (,w ,x (a . (b c . d)) (,a b ,c ,(d integer? . e)) ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern*
                            *dm:default-improper-indicator* *dm:default-ellipsis*
                            *dm:default-dont-care* *dm:default-unsplice* 
                            '(,v (,w ,x (a . (b c . d)) (,a b ,c ,(d integer? . e)) ,y) . ,z)))
      (benchmark-run reps (dm::properize-pattern
                            *dm:default-improper-indicator* *dm:default-ellipsis*
                            *dm:default-dont-care* *dm:default-unsplice* 
                            '(,v (,w ,x (a . (b c . d)) (,a b ,c ,(d integer? . e)) ,y) . ,z)))
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )
  ((0.798995 0 0.0)
    (0.762206 0 0.0)
    (0.961459 4 0.2660809999999998)
    (1.123289 5 0.3341390000000004))
  ((0.805455 0 0.0)
    (0.764429 0 0.0)
    (0.971856 4 0.2731279999999998)
    (1.057006 4 0.2683070000000001))

  ((0.763871 0 0.0)
    (0.76459 0 0.0)
    (0.957627 4 0.26417999999999964)
    (1.057983 4 0.26854900000000015))
  ((0.7910579999999999 0 0.0)
    (0.764218 0 0.0)
    (0.965691 4 0.2686710000000012)
    (1.120076 5 0.333124999999999))

  
  ((0.6473749999999999 0 0.0)
    (0.6357649999999999 0 0.0)
    (0.864646 4 0.27644699999999034)
    (0.976884 4 0.26985899999999674))
  ((0.774393 0 0.0)
    (0.789689 0 0.0)
    (1.0495340000000002 5 0.3593659999999943)
    (1.2390130000000001 5 0.3422210000000092))
  ((1.075528 0 0.0)
    (1.278024 0 0.0)
    (1.510217 7 0.47671199999999914)
    (1.868028 8 0.5601659999999988))
  ((1.078125 0 0.0)
    (1.277194 0 0.0)
    (1.583483 8 0.548649000000001)
    (1.7798720000000001 7 0.4769609999999993))
  ((1.091493 0 0.0)
    (1.275776 0 0.0)
    (1.51251 7 0.4795790000000011)
    (1.861343 8 0.5510710000000003))
  ((1.063638 0 0.0)
    (1.264557 0 0.0)
    (1.575855 8 0.5448719999999998)
    (1.782602 7 0.47675100000000015))
  ((1.05327 0 0.0)
    (1.270699 0 0.0)
    (1.589 8 0.5424309999999988)
    (1.8295130000000002 8 0.5262330000000013))
  ((1.063021 0 0.0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern cache management funs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::clear-compiled-patterns ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Clear any interned patterns."
  (ensure-db! '*dm*)
  (clear-db   '*dm*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (hash-table-p (dm::clear-compiled-patterns)) returns t)
;; (confirm that (length (symbol-plist '*dm*)) returns 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro walk* (lst &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Walk a possibly improper list."
;;   `(let ((pos ,lst))
;;      (while pos
;;        ,@body
;;        (setf pos (if (atom pos) nil (cdr pos))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun dm::validate-pattern (improper-indicator ellipsis dont-care unsplice pat &optional inner)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Look for patterrns with flexible elemends in an improper tail."
;;   (let ((pos pat) not-first)    
;;     (dm::prndiv)
;;     (while pos
;;       (if (atom pos)
;;         (progn
;;           (when (eq ellipsis pos)
;;             (error "ELLIPSIS %s in pattern's tailtip is not permitted" ellipsis))
;;           (dm::prn "tail tip: %s"  pos)
;;           (setf pos nil))
;;         (progn
;;           (when (and not-first (eq (car-safe pos) unsplice) (not (cdr (cdr-safe pos))))
;;             (error "UNSPLICE %s in pattern's tailtip is not permitted" unsplice))
;;           (dm::prn "pos:      %s" pos)
;;           (dm::prn "head:     %s" (car pos))
;;           (dm::prndiv ?\-)
;;           (when (and (listp (car pos)) (not (eq '\, (caar pos))))
;;             (with-indentation
;;               (dm::validate-pattern improper-indicator ellipsis dont-care unsplice (car pos) t)))
;;           (pop pos)))
;;       (setq not-first t)))
;;   (unless inner (dm::prndiv) (dm::prnl)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun dm::validate-pattern (improper-indicator ellipsis dont-care unsplice pat &optional inner)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Look for patterrns with flexible elemends in an improper tail."
;;   (let ((pos pat) not-first)    
;;     (dm::prndiv)
;;     (while pos
;;       (dm::prn "pos:      %s" pos)
;;       (cond
;;         ((eq ellipsis pos)
;;           (error "ELLIPSIS %s in pattern's tailtip is not permitted" ellipsis))
;;         ((and not-first (consp pos) (eq (car-safe pos) unsplice) (not (cdr (cdr-safe pos))))
;;           (error "UNSPLICE %s in pattern's tailtip is not permitted" unsplice))
;;         ((and (consp pos) (listp (car-safe pos)) (not (eq '\, (car (car-safe pos)))))
;;           (with-indentation
;;             (dm::validate-pattern improper-indicator ellipsis dont-care unsplice (car pos) t))))
;;       (dm::prndiv ?\-)
;;       (setq not-first t)
;;       (setf pos (if (atom pos) nil (cdr pos)))))
;;   (unless inner (dm::prndiv) (dm::prnl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::validate-pattern (improper-indicator ellipsis dont-care unsplice pat)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Look for patterrns with flexible elemends in an improper tail."
  (let (not-first)
    (dolist* (elem pos pat)
      ;; (dm::prndiv)
      ;; (dm::prn "pos:      %s" pos)
      ;; (dm::prn "elem:     %s" elem)
      (cond
        ((eq ellipsis pos)
          (error "ELLIPSIS %s in pattern's tailtip is not permitted" ellipsis))
        ((and not-first (consp pos) (eq elem unsplice) (not (cdr-safe (cdr pos))))
          (error "UNSPLICE %s in pattern's tailtip is not permitted" unsplice))
        ((and (consp pos) (consp elem) (not (eq '\, (car (car pos)))))
          (with-indentation
            (dm::validate-pattern improper-indicator ellipsis dont-care unsplice elem))))
      (dm::prndiv ?\-)
      (setq not-first t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enter at base case:
(dm::validate-pattern *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
  *dm:default-unsplice* nil)
;; boring:
(dm::validate-pattern
  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
  *dm:default-unsplice*
  '(1 2 (3 . 4)))
;; legal:
(dm::prndiv)
(dm::prn "LEGAL:")
(dm::validate-pattern
  *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
  *dm:default-unsplice*
  '(1 2 (3 ,(x . ...) ,@things)))
;; illegal, this will properly signal:
(ignore!
  (dm::prndiv)
  (dm::prn "ILLEGAL:")
  (dm::validate-pattern
    *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
    *dm:default-unsplice*
    '(1 2 (3 . ,@things))))
;; illegal, this will properly signal:
(ignore!
  (dm::validate-pattern
    *dm:default-improper-indicator* *dm:default-ellipsis* *dm:default-dont-care*
    *dm:default-unsplice*
    '(1 2 (3 . ...))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun dm::compile-pattern (improper-indicator ellipsis dont-care unsplice pat)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Store properized patterns in a hashtable to avoid repeatetly properizing the same pattern."
;;   (ensure-db! '*dm*)
;;   (let* ( (key (list improper-indicator ellipsis dont-care unsplice pat))
;;           (existing (db-get '*dm* key)))
;;     (if (cdr existing)
;;       (car existing)
;;       (let ((*dm:verbose* t))
;;         (dm::prndiv ?\-)
;;         (dm::prn "Compiling pattern: %S." pat)
;;         (let ((*dm:verbose* nil))
;;           (dm::validate-pattern improper-indicator ellipsis dont-care unsplice pat))
;;         (let ((compiled (dm::properize-pattern* improper-indicator ellipsis dont-care unsplice pat)))
;;           (dm::prn "Into compiled:     %S." compiled)
;;           (dm::prn "Under key:         %S" key)
;;           (dm::prndiv ?\-)
;;           (db-put '*dm* key compiled))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that
;;   (let ((pat '(,w ,(x integer? . foo) . ,(y integer? . foo))))
;;     (dm::compile-pattern '\. '... '_ '\,@ pat))
;;   returns ((\, w) (\, (x integer? . foo)) \. (\, (y integer? . foo))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::compile-pattern (improper-indicator ellipsis dont-care unsplice pat)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "\='Compile' a pattern without interning it."
  (let ((*dm:verbose* t))
    (dm::prn "Compiling pattern: %S." pat)
    (let ((*dm:verbose* nil))
      (dm::validate-pattern improper-indicator ellipsis dont-care unsplice pat))
    (let ((compiled (dm::properize-pattern* improper-indicator ellipsis dont-care unsplice pat)))
      (dm::prn "Into compiled:     %S." compiled)
      compiled)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ((pat '(,w ,(x integer? . foo) . ,(y integer? . foo))))
    (dm::compile-pattern '\. '... '_ '\,@ pat))
  returns ((\, w) (\, (x integer? . foo)) \. (\, (y integer? . foo))))
(confirm that
  (let ((pat '(,a ,(b integer? . foo) . ,c)))
    (equal pat
      (dm::unproperize!* '\.
        (dm::compile-pattern '\. '... '_ '\,@  
          (dm::unproperize!* '\. 
            (dm::compile-pattern '\. '... '_ '\,@  
              (dm::unproperize!* '\. 
                (dm::compile-pattern '\. '... '_ '\,@   pat))))))))
  returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dm::intern-pattern (improper-indicator ellipsis dont-care unsplice pat)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Store properized patterns in a hashtable to avoid repeatetly properizing the same pattern."
  (ensure-db! '*dm*)
  (let* ( (key (list improper-indicator ellipsis dont-care unsplice pat))
          (existing (db-get '*dm* key)))
    (if (cdr existing)
      (car existing)
      (let ((*dm:verbose* t))
        (dm::prndiv ?\-)
        (let ((compiled (dm::compile-pattern improper-indicator ellipsis dont-care unsplice pat)))
          (dm::prn "Storing under key: %S" key)
          (dm::prndiv ?\-)
          (db-put '*dm* key compiled))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let ((pat '(,w ,(x integer? . foo) . ,(y integer? . foo))))
    (dm::intern-pattern '\. '... '_ '\,@ pat))
  returns ((\, w) (\, (x integer? . foo)) \. (\, (y integer? . foo))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-aux)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


