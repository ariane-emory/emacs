;; -*- fill-column: 105;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'dash)
(require 'aris-funs--alist-funs)
(require 'aris-funs--with-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lexical-binding nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup match-pattern nil
  "Pattern matching inspired by the MATCH6 function from StevenTanimoto's book `The Elements of
Artificial Intelligence' but several improvements.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *match-pattern--init-fun* nil
  "The function `match-pattern' calls to initialize a new match."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--merge-duplicate-alist-keys* t
  "Whether `match-pattern' should merge the values of duplicate keys in the result alist."
  :group 'match-pattern
  :type 'boolean)

(defcustom *match-pattern--kleene-tag* '*
  "The symbol used by `match-pattern' to represent a Kleene star, matching 0 or more times."
  :group 'match-pattern
  :type 'symbol)

;; DO NOT NEGLECT THE SPACE AFTER THE ? ON THE NEXT LINE:
(defcustom *match-pattern--anything-tag* '?  ;; << CRITICAL SPACE AFTER THE ? !!!
  "The symbol used by `match-pattern' to represent a wildcard, matching any single item in
TARGET."
  :group 'match-pattern
  :type 'symbol)

(defcustom *match-pattern--get-capture-symbol-fun* #'cdr
  "The function used by `match-pattern' to extract the symbol from a capture."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--get-capture-tag-fun* #'car
  "The function used by `match-pattern' to extract the 'tag' from a capture element."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--capture-can-be-predicate* t
  "Whether a capture's 'tag' in th PATTERN argument to `match-pattern' is
allowed to be a predicate function."
  :group 'match-pattern
  :type 'boolean)

(defcustom *match-pattern--capture-element?* #'-cons-pair?
  "The function used by `match-pattern' to determine if a PATTERN element
represents a capture. By default, true pairs are considered captures."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--verbatim-element?* nil 
  "The function used by `match-pattern' to determine if a PATTERN element is a verbatim
(non-capturing).  element. By default any element that isn't a capture element is a
verbatim element."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--invalid-element?* nil
  "The function used by `match-pattern' to determine if a PATTERN element is an illegal
element. By default, any element that is neither a capture element or a verbatim
element is an invalid element."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--target-elements-must-be-verbatim* t
  "Whether the elements of  the TARGET argument to `match-pattern' must be verbatim elements."
  :group 'match-pattern
  :type 'boolean)

(defcustom *match-pattern--signal-error-if-target-elements-is-not-verbatim* t
  "Whether `match-pattern' shoud signal an error (or merely fail to match) if a
non-verbatim TARGET element is encountered. This setting only applies when
*MATCH-PATTERN--TARGET-ELEMENTS-MUST-BE-VERBATIM*."
  :group 'match-pattern
  :type 'boolean)

(defcustom *match-pattern--use-dotted-pairs-in-result* t
  "Whether `match-pattern' should use dotted pairs in the result alist."
  :group 'match-pattern
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-match-pattern--match-pattern (pattern target)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Match a PATTERN list against a TARGET list.

This is inspired by MATCH6 function from Steven Tanimoto's book `The Elements of Artificial
Intelligence' but with several improvements.

Examples:
  (`aris-match-pattern--match-pattern' '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))
  ⇒ ((v . 77) (w 3 2 1) (x . 66) (y . 22))

  (`aris-match-pattern--match-pattern' '(77 1 2 3 4 5 66 22) '(77 1 2 3 4 5 66 22))
  ⇒ t

  (setq *match-pattern--use-dotted-pairs-in-result* nil)

  (`aris-match-pattern--match-pattern' '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))
  ⇒ ((v 77) (w 3 2 1) (x 66) (y 22))

  (`aris-match-pattern--match-pattern' '(77 1 2 3 4 5 66 22) '(77 1 2 3 4 5 66 22))
  ⇒ t"
  (let ( (original-indent *aris-indent*)
         (*aris-indent--char* ?\-)
         (*aris-indent--size* 3))
    (cl-letf (((symbol-function 'print) #'indented-message))
      (print "MATCHING PATTERN %S AGAINST TARGET %s!" pattern target)
      (let ((*aris-indent* (1+ *aris-indent*)))
        (when *match-pattern--init-fun*
          (funcall *match-pattern--init-fun*))
        (cl-labels
          ((matchrec (pattern target depth accumulator)
             (let  ( (pattern-head (car pattern))
                     (pattern-tail (cdr pattern))
                     (target-head  (car target))
                     (target-tail  (cdr target)))
               (cl-flet ((plural? (string) ;; pure.
                           (equal "s" (substring string -1)))
                          (string-head (string) ;; pure.
                            (substring string 0 1))
                          (string-tail (string) ;; pure.
                            (substring string 1))              
                          (transform-string-head (string fun) ;; pure.
                            (concat (funcall fun (string-head string)) (string-tail string)))
                          (capitalize (string) ;; pure.
                            (transform-string-head string #'upcase))
                          (uncapitalize (string) ;; pure.
                            (transform-string-head string #'downcase)))
                 (cl-flet* ( (fail-to-match         () (cons nil accumulator))
                             (match-successfully    () (cons t accumulator))
                             (pattern-head-is-atom? () (atom pattern-head))
                             (elem-is-of-elem-type? (elem label preferred-p inverse-p) ;; semi-pure.
                               (let ((result
                                       ;; If `preferred-p' isn't set, but `inverse-p' is, we assume
                                       ;; that anything that's not inverse-p must be have the other
                                       ;; elem type.
                                       (cond
                                         (preferred-p (funcall preferred-p elem))
                                         (inverse-p (not (funcall inverse-p elem)))
                                         (t nil))))
                                 ;; (print "elem %s is a %s element? %s." elem label result)
                                 result))
                             (elem-is-verbatim? (elem) ;; semi-pure.
                               (elem-is-of-elem-type? elem "verbatim"
                                 *match-pattern--verbatim-element?*
                                 *match-pattern--capture-element?*))
                             (elem-is-capture? (elem) ;; semi-pure.
                               (elem-is-of-elem-type? elem "capture"
                                 *match-pattern--capture-element?*
                                 *match-pattern--verbatim-element?*))
                             (heads-are-equal? ()
                               (print "compare %s with %s..." pattern-head target-head)
                               (equal pattern-head target-head))
                             (pattern-head-is-capture? ()
                               (elem-is-capture? pattern-head))
                             (pattern-head-is-verbatim? ()
                               (elem-is-verbatim? pattern-head))
                             (assert-pattern-head-is-capture! ()
                               (unless (pattern-head-is-capture?)
                                 (error "Logic error: pattern-head '%s' is not a capture."
                                   pattern-head)))
                             (capture-field-of-pattern-head (fun)
                               (assert-pattern-head-is-capture!)
                               (funcall fun pattern-head))
                             (capture-symbol-of-pattern-head ()
                               (capture-field-of-pattern-head
                                 *match-pattern--get-capture-symbol-fun*))
                             (capture-tag-of-pattern-head ()
                               (capture-field-of-pattern-head
                                 *match-pattern--get-capture-tag-fun*))
                             (pattern-head-is-invalid? ()
                               (if *match-pattern--invalid-element?*
                                 (funcall *match-pattern--invalid-element?* pattern-head)
                                 (not (or (pattern-head-is-verbatim?) (pattern-head-is-capture?)))))
                             (capture-at-pattern-head-has-tag? (tag)
                               (when tag
                                 (assert-pattern-head-is-capture!)
                                 (eq tag (capture-tag-of-pattern-head))))
                             (make-kvp (value)
                               (cons (capture-symbol-of-pattern-head) (list value)))
                             (continue (pattern target &optional (value :NOT-SUPPLIED))
                               (let ((*aris-indent* (1+ *aris-indent*)))
                                 (matchrec pattern target depth
                                   (if (eq value :NOT-SUPPLIED)
                                     accumulator
                                     (let ((kvp (make-kvp value)))
                                       (print "Accumulating %s." kvp)
                                       (cons kvp accumulator))))))
                             (lookahead (target label)
                               (print "Looking ahead to see if %s match%s..."
                                 label
                                 (if (plural? label) "" "es"))
                               (let ( ;; (depth (1+ depth))
                                      (matched (car (continue pattern-tail target)))
                                      (string label))
                                 ;; (setq depth (1- depth))
                                 (print
                                   (if matched "%s matched!" "%s didn't match!")
                                   string)
                                 matched))
                             (pattern-tail-matches-target? ()
                               (lookahead target "pattern-tail"))
                             (pattern-tail-matches-target-tail? ()
                               (lookahead target-tail "tails")))
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; Body of matchrec:
                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   (print "matching %s against %s with acc %s..." pattern target accumulator)
                   (let ((*aris-indent* (1+ *aris-indent*)))
                     (print "target-head %s is %sa verbatim element." target-head
                       (if (elem-is-verbatim? target-head) "" "not "))
                     (cl-macrolet ((indent (&body body)
                                     `(let ((*aris-indent* (1+ *aris-indent*))) ,@body)))
                       (cl-macrolet ((case (index descr &body body)
                                       `(progn
                                          (print "trying case %s: %s..." ,index ,descr)
                                          (let ((result
                                                  (indent ,@body)))
                                            (if result
                                              (print "case %s applies!" ,index)
                                              (print "case does not apply."))
                                            ;; (print "case %s does not apply." ,index)
                                            result))))
                         (cond
                           ;; If `pattern' is null, match successfully when `target' is null too:
                           ((case 1 "Empty pattern" (null pattern))
                             (indent
                               (print "pattern is null and %s match!"
                                 (if target
                                   "target is not, so no"
                                   "so is target,"))
                               (if (null target)
                                 (match-successfully)
                                 (fail-to-match))))
                           ;; Fail to match if `target' is null and `pattern' isn't:
                           ((case 2 "Empty target" (null target))
                             (indent
                               (print "target is null and pattern isn't, no match!")
                               (fail-to-match)))
                           ;; If `pattern-head' is a verbatim element, match if it's equal to (car
                           ;; `target'):
                           ((case 3 "Verbatim element" (pattern-head-is-verbatim?))
                             (indent
                               ;;(print "pattern-head %s is a verbatim element." pattern-head)
                               (if (heads-are-equal?)
                                 (continue pattern-tail target-tail)
                                 (fail-to-match))))
                           ;; If `*match-pattern--target-elements-must-be-verbatim*' is set, then signal 
                           ;; an error if `target-head' isn't a verbatim element:
                           ((case 4 "Error case: non-verbatim target element"
                              (and
                                *match-pattern--target-elements-must-be-verbatim*
                                (not (elem-is-verbatim? target-head))))
                             (indent
                               (let ((complaint
                                       (format "target-head %s is not a verbatim element."
                                         target-head)))
                                 (if *match-pattern--signal-error-if-target-elements-is-not-verbatim*
                                   (error complaint)
                                   (print complaint)
                                   (fail-to-match)))))
                           ;; If `pattern-head' isn't either a verbatim element or a capture,
                           ;; something has gone wrong:
                           ((case 5 "Error case: invalid pattern element" (pattern-head-is-invalid?))
                             (indent
                               (error "pattern-head '%s' is an invalid element." pattern-head)))
                           ;; From here on, we know that `pattern-head' must be a capture.
                           ;; Case when `pattern-head' is tagged with the "anything" tag:
                           ((case 6 "`anything' pattern element"
                              (capture-at-pattern-head-has-tag? *match-pattern--anything-tag*))
                             (indent
                               (print "head of pattern has 'anything' tag.")
                               (continue pattern-tail target-tail target-head)))
                           ;; Case when `pattern-head' is tagged with the Kleene tag:
                           ((case 7 "Kleene pattern element"
                              (capture-at-pattern-head-has-tag? *match-pattern--kleene-tag*))
                             (indent
                               (print "head of pattern has Kleene tag.")
                               (cond
                                 ((case 101 "Kleene: pattern-tail matches target-tail?"
                                    (pattern-tail-matches-target-tail?))
                                   (indent
                                     (print
                                       (concat
                                         "pattern-pattern tail matches the target-tail "
                                         "target, so we'll take pattern-head as a Kleene item.")))
                                   (continue pattern-tail target-tail target-head))
                                 ((case 102 "Kleene: pattern-tail matches target?"
                                    (pattern-tail-matches-target?))
                                   (indent
                                     (print
                                       (concat
                                         "pattern-tail matches the entire target, so the "
                                         "Kleene item is nil."))
                                     (continue pattern-tail target-tail nil)))
                                 ((case 103 "Kleene: consume any Kleene item." t)
                                   (indent
                                     (print "taking target-head as a Kleene item.")
                                     (continue pattern target-tail target-head))))))
                           ;; Case when `pattern-head' starts with predicate form:
                           ((case 8 "Predicate pattern element"
                              (and
                                *match-pattern--capture-can-be-predicate*
                                (apply (capture-tag-of-pattern-head) (list target-head))))
                             (indent
                               (continue pattern-tail target-tail target-head)))
                           ;; Some unimplemented case happened, signal an error:
                           ((case 9 "Error case: this case should be unrachable" t)
                             (indent
                               (error "Unhandled case! Double-check your configuration."))))))))))))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Leave body of matchrec.
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (let ((*aris-indent* 0))
            (let ((match-result (matchrec pattern target 0 nil)))
              (print "Match result is %s." match-result)
	            (when (car match-result)
	              (let ((match-result (cdr match-result)))
	                ;;(print "Cdr of match result is %s." match-result)
	                (if (not match-result)
		                ;; If the match succeeded but there were no captures, just return t:
		                t
		                (print "Extracted match result %s." match-result)
		                (let ((match-result
			                      (if *match-pattern--merge-duplicate-alist-keys*
			                        (nreverse (aris-merge-duplicate-alist-keys match-result))
			                        match-result)))
		                  (print "Post-merge match result %s." match-result)
		                  (if *match-pattern--use-dotted-pairs-in-result*
		                    (aris-add-dots-to-alist match-result)
		                    match-result))))))))))))
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'match-pattern 'aris-match-pattern--match-pattern)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just a quick test that should match successfully with the default configuration:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when t
  (setq *match-pattern--use-dotted-pairs-in-result* t)
  
  (aris-match-pattern--match-pattern '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))
  ;; ⇒ ((v 77) (w 3 2 1) (x 66) (y 22))

  (aris-match-pattern--match-pattern '(77 1 2 3 4 5 66 22) '(77 1 2 3 4 5 66 22))
  ;; ⇒ t

  (setq *match-pattern--use-dotted-pairs-in-result* nil)

  (aris-match-pattern--match-pattern '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))
  ;; ⇒ ((v 77) (w 3 2 1) (x 66) (y 22))

  (aris-match-pattern--match-pattern '(77 1 2 3 4 5 66 22) '(77 1 2 3 4 5 66 22))
  ;; ⇒ t
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--match-pattern)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
