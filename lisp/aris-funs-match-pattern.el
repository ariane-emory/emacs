;; -*- fill-column: 100;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'dash)
(require 'aris-funs-with-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lexical-binding nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup match-pattern nil
  "Pattern matching inspired by the MATCH6 function from StevenTanimoto's book `The Elements of
Artificial Intelligence' but several improvements..")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *match-pattern--init-fun* nil
  "The function to call to initialize a new match."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--merge-duplicate-alist-keys* t
  "Whether to merge the values of duplicate keys in the result alist."
  :group 'match-pattern
  :type 'boolean)

(defcustom *match-pattern--kleene-tag* '*
  "The symbol used by match-pattern to represent a Kleene star, matching 0 or more times."
  :group 'match-pattern
  :type 'symbol)

;; DO NOT NEGLECT THE SPACE AFTER THE ? ON THE NEXT LINE:
(defcustom *match-pattern--anything-tag* '? 
  "The symbol used by match-pattern to represent a wildcard, matching any single item in
TARGET."
  :group 'match-pattern
  :type 'symbol)

(defcustom *match-pattern--get-capture-symbol-fun* #'cdr
  "The function used by match-pattern to extract the symbol from a capture."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--get-capture-tag-fun* #'car
  "The function used by match-pattern to extract the 'tag' from a capture element."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--capture-can-be-predicate* t
  "Whether a capture's 'tag' is allowed to be a predicate function."
  :group 'match-pattern
  :type 'boolean)

(defcustom *match-pattern--capture-element?* #'-cons-pair?
  "The function used by match-pattern to determine if a PATTERN element
represents a capture. By default, true pairs are considered captures."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--verbatim-element?* nil 
  "The function used by match-pattern to determine if a PATTERN element is a verbatim
(non-capturing).  element. By default any element that isn't a capture element is a
verbatim element."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--invalid-element?* nil
  "The function used by match-pattern to determine if a PATTERN element is an illegal
element. By default, any element that is neither a capture element or a verbatim
element is an invalid element."
  :group 'match-pattern
  :type 'function)

(defcustom *match-pattern--target-elements-must-be-verbatim* t
  "Whether the elements of  TARGET list must be verbatim elements."
  :group 'match-pattern
  :type 'boolean)

(defcustom *match-pattern--signal-error-if-target-elements-is-not-verbatim* t
  "Whether to signal an error (or merely fail to match) if a non-verbatim TARGET
element is encountered. This setting only applies when
*MATCH-PATTERN--TARGET-ELEMENTS-MUST-BE-VERBATIM*."
  :group 'match-pattern
  :type 'boolean)

(defcustom *match-pattern--use-dotted-pairs-in-result* t
  "Whether to use dotted pairs in the result alist."
  :group 'match-pattern
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-match-pattern--merge-duplicate-alist-keys (alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A helper function used by aris-match-pattern to merge the values of duplicate keys in
 ALIST.

Example:
(aris-match-pattern--merge-duplicate-alist-keys '((y . 22) (x . 66) (w . 3) (w . 2) (w . 1) (v . 77))) ⇒
  '((y . 22) (x . 66) (w 3 2 )) (v . 77))"
  (let (result)
    (dolist (pair alist)
      (let* ( (key (car pair))
              (value (cdr pair))
              (existing (assoc key result)))
        (if existing
          (setcdr existing (append (cdr existing) (list value)))
          (push (cons key (list value)) result))))
    (nreverse
      (mapcar
        (lambda (pair)
          (if (= (length (cdr pair)) 1)
            (cons (car pair) (car (cdr pair)))
            pair))
        result))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-match-pattern--match-pattern (pattern target)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Match a PATTERN list against a TARGET list.

This is inspired by MATCH6 function from Steven Tanimoto's book `The Elements of Artificial
Intelligence' but with several improvements."
  (message "MATCHING PATTERN %S AGAINST TARGET %s!" pattern target)
  (when *match-pattern--init-fun* (funcall *match-pattern--init-fun*))
  ;; flet the pure/semi-pure functions:
  (cl-flet* ( (string-head (string) ;; pure.
                (substring string 0 1))
              (string-tail (string) ;; pure.
                (substring string 1))
              (transform-string-head (string fun) ;; pure.
                (concat (funcall fun (string-head string)) (string-tail string)))
              (capitalize (string) ;; pure.
                (transform-string-head string #'upcase))
              (uncapitalize (string) ;; pure.
                (transform-string-head string #'downcase))
              (plural? (string) ;; pure.
                (equal "s" (substring string -1))))
    (cl-labels
      ((matchrec (pattern target depth accumulator)
         (let ( (pattern-head (car pattern))
                (pattern-tail (cdr pattern))
                (target-head  (car target))
                (target-tail  (cdr target)))
           ;; flet functions that rely only upon matchrec's argument and the enclosing
           ;; let's bindings:
           (cl-flet ( (indent-string ()
                        (make-string depth ?\ ))
                      (fail-to-match ()
                        (cons nil accumulator))
                      (match-successfully ()
                        (cons t accumulator))
                      (pattern-head-is-atom? ()
                        (atom pattern-head))
                      (heads-are-equal? ()
                        (message "Compare %s with %s..." pattern-head target-head)
                        (equal pattern-head target-head)))
             (cl-flet* ((message (&rest args)
                          (message "%s%s" (indent-string)
                            (apply #'format (car args) (cdr args))))
                         (elem-is-of-elem-type? (elem label preferred-p inverse-p) ;; semi-pure.
                           (let ((result
                                   ;; If `preferred-p' isn't set, but `inverse-p' is, we'll assume
                                   ;; anything that's not inverse-p must be have the other elem type.
                                   (cond
                                     (preferred-p (funcall preferred-p elem))
                                     (inverse-p (not (funcall inverse-p elem)))
                                     (t nil))))
                             (message "Elem %s is a %s element? %s." elem label result)
                             result))
                         (elem-is-verbatim? (elem) ;; semi-pure.
                           (elem-is-of-elem-type? elem "verbatim"
                             *match-pattern--verbatim-element?* *match-pattern--capture-element?*))
                         (elem-is-capture? (elem) ;; semi-pure.
                           (elem-is-of-elem-type? elem "cature"
                             *match-pattern--capture-element?* *match-pattern--verbatim-element?*))
                         (pattern-head-is-capture? ()
                           (elem-is-capture? pattern-head))
                         (pattern-head-is-verbatim? ()
                           (elem-is-verbatim? pattern-head))
                         (assert-pattern-head-is-capture! ()
                           (unless (pattern-head-is-capture?)
                             (error
                               "Logic error: PATTERN-HEAD '%s' is not a capture."
                               pattern-head)))
                         (capture-field-of-pattern-head (fun)
                           (assert-pattern-head-is-capture!)
                           (funcall fun pattern-head))
                         (capture-symbol-of-pattern-head ()
                           (capture-field-of-pattern-head *match-pattern--get-capture-symbol-fun*))
                         (capture-tag-of-pattern-head ()
                           (capture-field-of-pattern-head *match-pattern--get-capture-tag-fun*))
                         (pattern-head-is-invalid? ()
                           (if *match-pattern--invalid-element?*
                             (funcall *match-pattern--invalid-element?* pattern-head)
                             (not (or (pattern-head-is-verbatim?) (pattern-head-is-capture?)))))
                         (capture-at-pattern-head-has-tag? (tag)
                           (when tag
                             (assert-pattern-head-is-capture!)
                             (eq tag (capture-tag-of-pattern-head))))
                         (make-kvp (value)
                           (cons (capture-symbol-of-pattern-head)
                             (if *match-pattern--use-dotted-pairs-in-result*
                               value
                               (list value))))
                         (continue (pattern target &optional (value :NOT-SUPPLIED))
                           (matchrec pattern target depth
                             (if (eq value :NOT-SUPPLIED)
                               accumulator
                               (let ((kvp (make-kvp value)))
                                 (message "Accumulating %s." kvp)
                                 (cons kvp accumulator)))))
                         (lookahead (target label)
                           (message "Looking ahead to see if %s match%s..."
                             label
                             (if (plural? label) "" "es"))
                           (let ( (depth (1+ depth))
                                  (matched (car (continue pattern-tail target)))
                                  (string (capitalize label)))
                             (message
                               (if matched "%s matched!" "%s didn't match!")
                               string)
                             matched))
                         (pattern-tail-matches-target? ()
                           (lookahead target "PATTERN-TAIL"))
                         (pattern-tail-matches-target-tail? ()
                           (lookahead target-tail "tails")))
               ;; Body of matchrec:
               (message "Matching %s against %s with acc %s..." pattern target accumulator)
               (if (elem-is-verbatim? target-head)
                 (message "TARGET-HEAD %s is a verbatim element." target-head)
                 (message "TARGET-HEAD %s is not a verbatim element." target-head))
               (setq depth (1+ depth))
               (cl-macrolet ((case (index &rest rest)
                               (let ((message (format "Trying case %s" index)))
                                 `(with-messages ,message
                                    ,@rest))))
                 (cond
                   ;; If `pattern' is null, match successfully when `target' is null too:
                   ((case 1
                      (let ((depth (1+ depth)))
                        (null pattern)))
                     (message "PATTERN is null and %s match!"
                       (if target
                         "TARGET is not, sono"
                         "so is TARGET,"))
                     (if (null target)
                       (match-successfully)
                       (fail-to-match)))
                   ;; Fail to match if `target' is null and `pattern' isn't:
                   ((case 2
                      (let ((depth (1+ depth)))
                        (null target)))
                     (message "TARGET is null and PATTERN isn't, no match!")
                     (fail-to-match))
                   ;; If `pattern-head' is a verbatim element, match if it's equal to (car
                   ;; `target'):
                   ((case 3
                      (let ((depth (1+ depth)))
                        (pattern-head-is-verbatim?)))
                     (message "PATTERN-HEAD %s is a verbatim element." pattern-head)
                     (if (heads-are-equal?)
                       (continue pattern-tail target-tail)
                       (fail-to-match)))
                   ;; If `*match-pattern--target-elements-must-be-verbatim*' is set, then signal 
                   ;; an error if `target-head' isn't a verbatim element:
                   ((case 4
                      (let ((depth (1+ depth)))
                        (and
                          *match-pattern--target-elements-must-be-verbatim*
                          (not (elem-is-verbatim? target-head)))))
                     (let ((complaint
                             (format "TARGET-HEAD %s is not a verbatim element."
                               target-head)))
                       (if *match-pattern--signal-error-if-target-elements-is-not-verbatim*
                         (error complaint)
                         (message complaint)
                         (fail-to-match))) )
                   ;; If `pattern-head' isn't either a verbatim element or a capture,
                   ;; something has gone wrong:
                   ((case 5
                      (let ((depth (1+ depth)))
                        (pattern-head-is-invalid?)))
                     (error
                       "PATTERN-HEAD '%s' is an invalid element."
                       pattern-head))
                   ;; From here on, we know that `pattern-head' must be a capture.
                   ;; Case when `pattern-head' is tagged with the "anything" tag:
                   ((case 6
                      (let ((depth (1+ depth)))
                        (capture-at-pattern-head-has-tag? *match-pattern--anything-tag*)))
                     (message "Head of PATTERN has 'anything' tag.")
                     (continue pattern-tail target-tail target-head))
                   ;; Case when `pattern-head' is tagged with the Kleene tag:
                   ((case 7
                      (let ((depth (1+ depth)))
                        (capture-at-pattern-head-has-tag? *match-pattern--kleene-tag*)))
                     (message "Head of PATTERN has Kleene tag.")
                     (cond
                       ((pattern-tail-matches-target-tail?)
                         (message (concat
                                    "Kleene case 1: The rest of PATTERN matches the "
                                    "rest of TARGET, so we'll take LIST_HEAD as a Kleene item."))
                         (continue pattern-tail target-tail target-head))
                       ((pattern-tail-matches-target?)
                         (message (concat
                                    "Kleene case 2: The rest of PATTERN matches the "
                                    "entire TARGET, so the Kleene item is nil."))
                         (continue pattern-tail target-tail nil))
                       (t
                         (message "Kleene case 3: Take LIST head as a Kleene item.")
                         (continue pattern target-tail target-head))))
                   ;; Case when `pattern-head' starts with predicate form:
                   ((case 8
                      (let ((depth (1+ depth)))
                        (and
                          *match-pattern--capture-can-be-predicate*
                          (apply (capture-tag-of-pattern-head) (list target-head)))))
                     (continue pattern-tail target-tail target-head))
                   ;; Some unimplemented case happened, signal an error:
                   ((with-messace "Case t" t)
                     (error "Unhandled case! Double-check your configuration.")))))))))
      (let ((match-result (matchrec pattern target 0 nil)))
        (message "Match result is %s." match-result)
        (when (car match-result)
          (let ((match-result (cdr match-result)))
            (message "Cdr of match result is %s." match-result)
            (if (not match-result)
              ;; If the match succeeded but there were no captures, just return t:
              t
              (message "Post-processing match result %s." match-result)
              (if *match-pattern--merge-duplicate-alist-keys*
                (nreverse (aris-match-pattern--merge-duplicate-alist-keys match-result))
                match-result))
            ))))))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just a quick test that should match successfully with the default configuration:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'match-pattern 'aris-match-pattern--match-pattern)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when t
  (aris-match-pattern--match-pattern
    '(77 1 2 3 4 5 66 22) '(77 1 2 3 4 5 66 22))
  (aris-match-pattern--match-pattern
    '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))
  (match-pattern
    '(77 1 2 3 4 5 66 22) '(77 1 2 3 4 5 66 22))
  (match-pattern
    '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs-match-pattern)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
