;; -*- lexical-binding: t; fill-column: 95;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lust-style function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def (fib 0) 0)
;; (def (fib 1) 1)
;; (def (fib n) (+ (fib (- n 1)) (fib (- n 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--match-pattern)
(require 'aris-funs--with-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup lust-style-syntax nil
  "Dispatch calls to pseudo-functions using pattern matchings.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *lust-style-syntax--verbose* t
  "Whether the pseudo-function dispatcher should print verbose messages."
  :group 'lust-style-syntax
  :type 'boolean)

(defvar *lust-style-syntax--counter* 0
  "This is part of an ugly hack and is not meant to be customized.")

(defvar *lust-style-syntax--pattern-dispatch-table* nil
  "This is where the pattern dispatch table is stored as an alis of alists and is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just a quick test here:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (match-pattern '(fib 1 n z) '(fib 1 5 8))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aris-lust-syle-defs--print (first &rest rest)
  "Do this dumb hack to prevent apostrophes from being turned into single quotes."
  `(when *lust-style-syntax--verbose*
     (indented-message ,first ,@rest)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'print 'aris-lust-syle-defs--print)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--get-patterns-for-group (group-symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cdr (assoc group-symbol *lust-style-syntax--pattern-dispatch-table*)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "An internal helper function to bind the pattern dispatcher function to symbols that's used by def."
  (aris-lust-syle-defs--use-print
    (print (make-string 80 ?\=))
    (print "Binding dispatch fun for '%s!" symbol)

    ;; SYMBOL must be a symbol:
    (error-unless "%s is not a symbol." '(symbol)
      (symbolp symbol))
    
    ;; If SYMBOL is already bound and it doesn't look like we did it,
    ;; raise an error.
    (let ((already-bound (fboundp symbol)))
      (error-when
        (concat
          "Logic error: symbol '%s already bound to a function and "
          "it doesn't look like it was bound by this function."
          "fmakunbound it first if you really want to re-bind it!" symbol)
        (and
          already-bound
          (not (let ((existing-group-label (get symbol :PATTERN-DISPATCHER-GROUP)))
               (print "'%s already has group label '%s."
                 symbol existing-group-label)
               (or (not existing-group-label) (eq existing-group-label symbol))))))

      (print "%sinding dispatch fun for '%s!"
        (if already-bound "Reb" "B") symbol))

    ;; Attach our handler function to SYMBOL's function cell:
    (fset symbol (lust-style-syntax--make-pattern-dispatcher-fun symbol))

    ;; Stash the group label in a property on SYMBOL:
    (put symbol :PATTERN-DISPATCHER-GROUP symbol)

    ;; Make sure the label was set properly and then return SYMBOL's plist:
    (let ( (group-label (get symbol :PATTERN-DISPATCHER-GROUP))
           (plist (symbol-plist symbol)))
      ;; Sanity check:
      (error-unless
        "After setting field to '%s, its value is '%s. Something has gone wrong."
        '(symbol group-label)
        (eq symbol group-label))
      (print "Marked symbol '%s with group label '%s, its plist is now: '%s."
        symbol group-label plist)
      ;; Finally, return SYMBOL's modified plist:
      plist)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--eval-match-result (match-result)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate the MATCH-RESULT for a call pattern."
  (aris-lust-syle-defs--use-print
    (print "Evaluating match result '%s" match-result)
    (cond
      ((symbolp (cadr match-result))
        (let ((eval-result (eval (cadr match-result))))
          (print
            "Match result '%s's pattern case is a symbol, evaluating '%s and returning %s."
            match-result (cadr match-result) eval-result)
          eval-result))
      ((atom (cadr match-result))
        (progn
          (print "Match result '%s's pattern case is an atom, returning %s."
            match-result (cadr match-result))
          (cadr match-result)))
      (t
        (let ((decorated-result (cons 'let match-result)))
          (print "Evaluating decorated pattern case '%s." decorated-result)
          (let ((result (eval decorated-result)))
            (print "Match result '%s's pattern case's body returned %s."
              match-result result)
            result))))))
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-lust-syle-defs--match-call-pattern-in-group (call-pattern group)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Find the pattern case in group that matches the call pattern.."
  (aris-lust-syle-defs--use-print
    (error-unless "Invalid call, no pattern group for '%s." '(call-pattern) group)    
    (PRINT-DIVIDER)
    (let (result)
      (catch 'matched
        (dolist (pattern-case group)
          (let ( (pattern (car pattern-case))
                 (*match-pattern--init-fun*
                   (lambda () (setq *lust-style-syntax--counter* 0)))
                 (*match-pattern--merge-duplicate-alist-keys* nil)
                 (*match-pattern--kleene-tag* nil)
                 (*match-pattern--anything-tag* 'anything)
                 (*match-pattern--verbatim-element?* nil)
                 (*match-pattern--capture-element?*
                   (lambda (elem)
                     (if (> *lust-style-syntax--counter* 1)
                       (symbolp elem)
                       (setq *lust-style-syntax--counter*
                         (1+ *lust-style-syntax--counter*))
                       nil)))
                 (*match-pattern--get-capture-symbol-fun* (lambda (e) e))
                 (*match-pattern--get-capture-tag-fun* (lambda (e) 'anything))
                 (*match-pattern--capture-can-be-predicate* nil)
                 (*match-pattern--target-elements-must-be-verbatim* nil)
                 (*match-pattern--use-dotted-pairs-in-result* nil))
            (print "Trying pattern '%s on target '%s..." pattern call-pattern)
            (let ((match-result (match-pattern pattern call-pattern)))
              ;; (print "Match result reveived ist: %s" match-result)
              (when match-result
                (throw 'matched
                  (setq result (cons match-result (cdr pattern-case)))))))))
      (error-unless "No pattern case found for '%s." '(call-pattern)
        result)
      (PRINT-DIVIDER)
      (print "Found match for '%s = '%s" call-pattern result)
      (PRINT-DIVIDER)
      result)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--make-pattern-dispatcher-fun (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Factory function for pattern call dispatch handler functions. The reason we construct new ones each time is
because we're gong to be stshing stuff in their symbol properties."
  (aris-lust-syle-defs--use-print
    (print "Making dispatch handler for '%s..." symbol)
    (unless (symbolp symbol)
      (error "Symbol must be a symbol, but got '%s." symbol))
    (let ((symbol symbol))
      (lambda (&rest args)
        "Pattern call dispatch hander function to call into the pattern group SYMBOL with ARGs."
        (aris-lust-syle-defs--use-print
          (print "Doing dispatch for '%s..." symbol)
          (with-message-indent
            (let* ( (group-symbol (get symbol :PATTERN-DISPATCHER-GROUP))
                    (group (lust-style-syntax--get-patterns-for-group group-symbol))
                    (call-pattern (cons symbol args)))
              (print "Looked up group for '%s and found:" symbol)
              (with-message-indent
                (dolist (row group)
                  (print "%s ⇒" (string-trim (pp-to-string (car row))))
                  (let ( (lines
                           (butlast (split-string (pp-to-string (cdr row)) "\n"))))
                    (print "  %s" (car lines))
                    (dolist (line (cdr lines))
                      (print "  %s" line)))))
              (lust-style-syntax--eval-match-result
                (aris-lust-syle-defs--match-call-pattern-in-group
                  call-pattern group)))))))))
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-lust-syle-syntax--reset ()
  "Reset the dispatch table and unbind bound functions."
  (aris-lust-syle-defs--use-print
    (PRINT-DIVIDER)
    (print "RESET!")
    (PRINT-DIVIDER)
    (dolist (group *lust-style-syntax--pattern-dispatch-table*)
      (let ((group-symbol (car group)))
        (print "Unbinding %s..." group-symbol)
        (fmakunbound group-symbol)
        (print "Props before: %s" (symbol-plist group-symbol))
        (put group-symbol :PATTERN-DISPATCHER-GROUP nil)
        (print "Props after: %s" (symbol-plist group-symbol))))
    (setq *lust-style-syntax--pattern-dispatch-table* nil)
    (PRINT-DIVIDER)))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define functions with a Lust-style syntax:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro lust-style-syntax--def (pattern-or-symbol &rest def-body) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a function call pattern case or a variable using a Lust-style syntax."
  (let* ( (is-variable-definition (symbolp pattern-or-symbol))
          (is-function-definition (proper-list-p pattern-or-symbol))
          (is-illegal-definition  (not (or is-variable-definition is-function-definition))))
    (error-when "DEF: PATTERN-OR-SYMBOL must be either a symbol or a proper list."
      is-illegal-definition)
    (if is-variable-definition
      (let ( (symbol pattern-or-symbol)
             (value-expr (car def-body))
             (is-illegal-definition (cdr def-body)))
        (error-when "DEF: Variable definition's body must be a single value."
          is-illegal-definition)
        `(aris-lust-syle-defs--use-print
           (print "DEF: Defining variable '%s." ',symbol)
           (setq ,symbol ,value-expr)))
      (let* ( (pattern pattern-or-symbol)
              (group (car pattern))
              (is-illegal-definition (not (or (proper-list-p def-body) (atom def-body)))))
        (error-when "DEF: Function definition's body must be either an atom or a proper list."
          is-illegal-definition)
        `(aris-lust-syle-defs--use-print
           (let ()
             ;;(debug)
             (print "DEF: Defining pattern '%s in group '%s." ',pattern ',group)
             (lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun ',group) ;; ',(car pattern)
             (let ((group (assoc ',group *lust-style-syntax--pattern-dispatch-table*)))
               (if (not group)
                 (let ((group (cons ',(car pattern) (list (cons ',pattern ',def-body)))))
                   (print "DEF:   Added new group '%s" group)
                   (push group *lust-style-syntax--pattern-dispatch-table*))
                 (let ((pattern-case (assoc ',pattern (cdr group))))
                   (when pattern-case
                     (print "DEF:   Found pattern-case '%s in group '%s.")
                     ;; (or pattern-case "<none>") group)
                     (error-when "DEF:   Pattern %s already defined." '(',pattern)
                       pattern-case))
                   (setcdr group (nconc (cdr group) (list (cons ',pattern ',def-body))))
                   (print
                     "DEF: Added pattern case for pattern '%s to group '%s." ',pattern group)))
               (print (string-trim (pp-to-string *lust-style-syntax--pattern-dispatch-table*)))
               *lust-style-syntax--pattern-dispatch-table*)))))))
               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "An internal helper function to bind the pattern dispatcher function to symbols that's used by def."
  (aris-lust-syle-defs--use-print
    (print (make-string 80 ?\=))
    (print "Binding dispatch fun for '%s!" symbol)

    ;; SYMBOL must be a symbol:
    (error-unless "%s is not a symbol." '(symbol)
      (symbolp symbol))
    
    ;; If SYMBOL is already bound and it doesn't look like we did it,
    ;; raise an error.
    (let ((already-bound (fboundp symbol)))
      (error-when
        (concat
          "Logic error: symbol '%s already bound to a function and "
          "it doesn't look like it was bound by this function."
          "fmakunbound it first if you really want to re-bind it!" symbol)
        (and
          already-bound
          (not (let ((existing-group-label (get symbol :PATTERN-DISPATCHER-GROUP)))
               (print "'%s already has group label '%s."
                 symbol existing-group-label)
               (or (not existing-group-label) (eq existing-group-label symbol))))))

      (print "%sinding dispatch fun for '%s!"
        (if already-bound "Reb" "B") symbol))

    ;; Attach our handler function to SYMBOL's function cell:
    (fset symbol (lust-style-syntax--make-pattern-dispatcher-fun symbol))

    ;; Stash the group label in a property on SYMBOL:
    (put symbol :PATTERN-DISPATCHER-GROUP symbol)

    ;; Make sure the label was set properly and then return SYMBOL's plist:
    (let ( (group-label (get symbol :PATTERN-DISPATCHER-GROUP))
           (plist (symbol-plist symbol)))
      ;; Sanity check:
      (error-unless
        "After setting field to '%s, its value is '%s. Something has gone wrong."
        '(symbol group-label)
        (eq symbol group-label))
      (print "Marked symbol '%s with group label '%s, its plist is now: '%s."
        symbol group-label plist)
      ;; Finally, return SYMBOL's modified plist:
      plist)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun PRINT-DIVIDER (&optional (char ?\=))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Print a divider line."
  (aris-lust-syle-defs--use-print
    (print (make-string 80 char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'def 'lust-style-syntax--def)
(defalias 'begin 'progn)
(defalias 'bind ' lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun)
(defalias 'pd-reset 'aris-lust-syle-syntax--reset)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--lust-style-syntax)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
