;; -*- lexical-binding: nil; fill-column: 95;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lust-style function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def (fib 0) 0)
;; (def (fib 1) 1)
;; (def (fib n) (+ (fib (- n 1)) (fib (- n 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--match-pattern)
(require 'aris-funs--match-pattern2)
(require 'aris-funs--with-messages)
(require 'aris-funs--error-when-and-error-unless)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup pattern-dispatch nil
  "Dispatch calls to pseudo-functions using pattern matchings.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *pd--verbose* nil
  "Whether the pseudo-function dispatcher should print verbose messages."
  :group 'pattern-dispatch
  :type 'boolean)

(defcustom *pd--print-fun* 'prn
  "The function to use to print messages."
  :group 'pattern-dispatch 
  :type 'function)

(defcustom *pd--allow-match-fallback* t
  "(TEMPORARY) Whether to fall back to match when match2 fails."
  :group 'pattern-dispatch
  :type 'boolean)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *pd--handler-count* 0
  "A serial number used by dispatch handler functions, not meant to be customized")

(defvar *pd--pattern-dispatch-table* nil
  "This is where the pattern dispatch table is stored as an alist of alists and is not meant to be customized.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pd-prn (first &rest rest)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Wrap *pd--print-fun*"
  `(when *pd--verbose* (ignore (funcall *pd--print-fun* ,first ,@rest))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun --pd-prndiv (&optional (char ?\=))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Print a divider line."
  (--pd-prn (make-string 80 char))
  nil)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --pd-get-group (group-symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (assoc group-symbol *pd--pattern-dispatch-table*))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --pd-eval-match-result (match-result)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate the MATCH-RESULT for a call pattern."
  (--pd-prn "Evaluating match result '%s" match-result)
  (cond
    ((symbolp (cadr match-result))
      (let ((eval-result (eval (cadr match-result))))
        (--pd-prn
          "Match result '%s's pattern case is a symbol, evaluating '%s and returning %s."
          match-result (cadr match-result) eval-result)
        eval-result))
    ((atom (cadr match-result))
      (progn
        (--pd-prn "Match result '%s's pattern case is an atom, returning %s."
          match-result (cadr match-result))
        (cadr match-result)))
    (t
      (let ((decorated-result (cons 'let match-result)))
        (--pd-prn "Evaluating decorated pattern case '%s." decorated-result)
        (let ((result (eval decorated-result)))
          (--pd-prn "Match result '%s's pattern case's body returned %s."
            match-result result)
          result)))))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --pd-match-call-pattern-in-group (call-pattern group)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Find the pattern case in group that matches the call pattern.."
  (error-unless "Invalid call, no pattern group for '%s." '(call-pattern) group)    
  (--pd-prndiv)
  (let (result)
    (catch 'matched
      (dolist (pattern-case group)
        (let ( (pattern (car pattern-case))
               (*mp--anything-tag* 'anything)
               (*mp--capture-can-be-predicate* nil)
               (*mp--capture-element?* 'symbolp)
               (*mp--get-capture-symbol-fun* (lambda (e) e))
               (*mp--get-capture-tag-fun* (lambda (e) 'anything))
               (*mp--kleene-tag* nil)
               (*mp--merge-duplicate-alist-keys* nil)
               (*mp--target-elements-must-be-verbatim* nil)
               (*mp--use-dotted-pairs-in-result* nil)
               (*mp--verbatim-element?* nil)
               ;; temporary:
               (*match-pattern2--anything-tag* 'anything)
               (*match-pattern2--capture-can-be-predicate* nil)
               (*match-pattern2--capture-element?* 'symbolp)
               (*match-pattern2--get-capture-symbol-fun* (lambda (e) e))
               (*match-pattern2--get-capture-tag-fun* (lambda (e) 'anything))
               (*match-pattern2--kleene-tag* nil)
               (*match-pattern2--merge-duplicate-alist-keys* nil)
               (*match-pattern2--target-elements-must-be-verbatim* nil)
               (*match-pattern2--use-dotted-pairs-in-result* nil)
               (*match-pattern2--verbatim-element?* nil))
          (--pd-prn "Trying pattern '%s on target '%s..." pattern call-pattern)
          (let ( (match-result (match2 pattern call-pattern)))
            (when *pd--allow-match-fallback*
              (--pd-prn "MATCH2 FAILED, FALLING BACK TO MATCH!"))
            (let ((match-result (or match-result
                                  (and *pd--allow-match-fallback* (match pattern call-pattern)))))
              (when match-result
                (throw 'matched
                  (setq result (cons match-result (cdr pattern-case))))))))))
    (error-unless "No pattern case found for '%s." '(call-pattern)
      result)
    (--pd-prndiv)
    (--pd-prn "Found match for '%s = '%s" call-pattern result)
    (--pd-prndiv)
    result))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro --pd-make-dispatcher-fun (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Factory function for pattern call dispatch handler functions. The reason we construct new ones each time is
because we're gong to be stshing stuff in their symbol properties."
  (--pd-prn "MAKE: Making dispatch handler for '%s..." symbol)
  (unless (symbolp symbol)
    (error "MAKE: Symbol must be a symbol, but got %S." symbol))
  `(lambda (&rest args)
     "Pattern call dispatch hander function to call into the pattern group SYMBOL with ARGs."
     (--pd-prndiv)
     (--pd-prn "MAKE: Doing dispatch for '%s..." ',symbol)
     (with-indentation
       (let* ( (group-symbol (get ',symbol :PD-GROUP))
               (group (--pd-get-group group-symbol))
               (group-rows (cdr group))
               (call-pattern args))
         (--pd-prn "MAKE: Looked up group for '%s and found:" ',symbol)
         (--pd-prndiv)
         (pd--prn-group group)
         (--pd-eval-match-result
           (--pd-match-call-pattern-in-group
             call-pattern group-rows))))))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --pd-bind (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "An internal helper function to bind the pattern dispatcher function to symbols that's used by def."
  (--pd-prndiv ?\#)
  (--pd-prn "BIND: Preparing to bind dispatch fun for '%s..." symbol)
  ;; SYMBOL must be a symbol:
  (error-unless "BIND: %s is not a symbol." '(symbol) (symbolp symbol))
  ;; If SYMBOL is already bound and it doesn't look like we did it,
  ;; raise an error.
  (let ((already-bound (fboundp symbol)))
    (error-when
      (concat
        "BIND: Logic error: symbol '%s already bound to a function and "
        "it doesn't look like it was bound by this function."
        "fmakunbound it first if you really want to re-bind it!" symbol)
      (and
        already-bound
        (not (let ((existing-group-label (get symbol :PD-GROUP)))
             (--pd-prn "BIND: '%s already has group label '%s."
               symbol existing-group-label)
             (or (not existing-group-label) (eq existing-group-label symbol))))))      
    (--pd-prn "BIND: '%s isn't bound or was bound by us, we can %sbind it."
      symbol (if already-bound "re" "")))
  ;; Attach our handler function to SYMBOL's function cell:
  (fset symbol (eval `(--pd-make-dispatcher-fun ,symbol)))
  ;; Stash the group label and a serial numbe in properties on SYMBOL:
  (put symbol :PD-GROUP symbol)
  (setq *pd--handler-count* (1+ *pd--handler-count*))
  (put symbol :PD-COUNT *pd--handler-count*)
  ;; Make sure the label was set properly and then return SYMBOL's plist:
  (let ( (group-label (get symbol :PD-GROUP))
         (plist (symbol-plist symbol)))
    ;; Sanity check:
    (error-unless
      "BIND: After setting field to '%s, its value is '%s. Something has gone wrong."
      '(symbol group-label)
      (eq symbol group-label))
    (--pd-prn "BIND: Marked '%s with group label '%s, its plist is now:"
      symbol group-label)
    (dolist (line (butlast (split-string (pp-to-string plist ) "\n")))
      (--pd-prn "      %s" line))
    ;; Finally, return SYMBOL's modified plist:
    ;;plist
    ;; Finally, return :
    nil
    ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pd--def (pattern-or-symbol &rest def-body) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a function call pattern case or a variable using a Lust-style syntax."
  (let* ( (is-variable-definition (symbolp pattern-or-symbol))
          (is-function-definition (proper-list-p pattern-or-symbol))
          (is-illegal-definition  (not (or is-variable-definition is-function-definition))))
    (error-when "DEF:  PATTERN-OR-SYMBOL must be either a symbol or a proper list."
      is-illegal-definition)
    (if is-variable-definition
      ;; Variable definition case:
      (let ( (symbol pattern-or-symbol)
             (value-expr (car def-body))
             (is-illegal-definition (cdr def-body)))
        (error-when "DEF:  Variable definition's body must be a single value."
          is-illegal-definition)
        `(progn
           (--pd-prn "DEF: Defining variable '%s." ',symbol)
           (setq ,symbol ,value-expr)))
      ;; Pseudo-function definition case:
      (let* ( (full-pattern-including-group-symbol pattern-or-symbol)
              (group-symbol (car full-pattern-including-group-symbol))
              (pattern-without-group-symbol (cdr full-pattern-including-group-symbol))
              (is-illegal-definition (not (proper-list-p def-body))))
        (error-when "DEF:  Function definition's body must a proper list."
          is-illegal-definition)
        `(progn
           ;;(debug)
           (--pd-prndiv ?\#)
           (--pd-prn "DEF:  Defining pattern '%s in group '%s."
             ',pattern-without-group-symbol ',group-symbol)
           (--pd-bind ',group-symbol)
           ;; Look up existing group and add new case to it if it exists.
           (let ( (new-pattern-case (list (cons ',pattern-without-group-symbol ',def-body)))
                  (group-alist (assoc ',group-symbol *pd--pattern-dispatch-table*)))
             (if (not group-alist)
               (let ((group-alist (cons ',group-symbol new-pattern-case)))
                 (push group-alist *pd--pattern-dispatch-table*)
                 (--pd-prn "DEF:  Added new group:")
                 (--pd-prndiv)
                 (pd--prn-group group-alist))
               (let* ( (existing-pattern-case
                         (assoc ',pattern-without-group-symbol (cdr group-alist))))
                 (when existing-pattern-case
                   (--pd-prn "DEF:  Found existing-pattern-case '%s in group '%s."
                     existing-pattern-case group-alist)
                   ;; (or existing-pattern-case "<none>") group)
                   (error-when "DEF:  Pattern %s already defined in group '%s."
                     '(',full-pattern-including-group-symbol ',group-symbol)
                     existing-pattern-case))
                 (setcdr group-alist (nconc (cdr group-alist) new-pattern-case))
                 (--pd-prndiv)
                 (--pd-prn "DEF:  Added pattern case for pattern '%s to group '%s:"
                   ',full-pattern-including-group-symbol group-alist)
                 (pd--prn-group group-alist)))
             (--pd-prndiv ?\#)
             nil))))))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pd--reset ()
  "Reset the dispatch table and unbind bound functions."
  (--pd-prndiv)
  (--pd-prn "RESET PATTERN DISPATCHER!")
  (--pd-prndiv)
  (dolist (group *pd--pattern-dispatch-table*)
    (let ((group-symbol (car group)))
      (--pd-prn "Unbinding %s..." group-symbol)
      (fmakunbound group-symbol)
      (with-indentation
        (--pd-prn "Props before: %s" (symbol-plist group-symbol))
        (put group-symbol :PD-GROUP nil)
        (put group-symbol :PD-COUNT nil)
        (--pd-prn "Props after:  %s" (symbol-plist group-symbol)))))
  (setq *pd--pattern-dispatch-table* nil)
  (--pd-prndiv))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting functions that aren't documented yet:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun pd--format-group-as-lines (group &optional (indent 0) (indent-char ?\ ))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;(--pd-prn "Formatting group as lines '%s..." group)
  (let* ( result
          (group-name (car group))
          (group-rows (cdr group)))
    (push (format "[%s]" group-name) result)
    (dolist (row group-rows)
      (let ((pattern (car row)))
        (push (format "%s%s ⇒" (make-string (+ indent 2) indent-char) pattern) result)
        (let ((lines (butlast (split-string (pp-to-string (cdr row)) "\n"))))
          (dolist (line lines)
            (push (format "%s%s" (make-string (+ indent 4) indent-char) line) result)))))
    (nreverse result)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun pd--format-group-as-string (group &optional (indent 0) (indent-char ?\ ))
  (string-join (pd--format-group-as-lines group indent indent-char) "\n"))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pd--format-table-as-lines ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let (result)
    (dolist (group *pd--pattern-dispatch-table*)
      (dolist (line (pd--format-group-as-lines group))
        (push (format "%s" line) result)))
    (nreverse result)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pd--format-table-as-string ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (string-join (pd--format-table-as-lines) "\n"))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pd--prn-group (group)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (dolist (line (pd--format-group-as-lines group))
    (--pd-prn line)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pd--prn-table ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (dolist (line (pd--format-table-as-lines))
    (--pd-prn line)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'def 'pd--def)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn these into `confirm' tests:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
  (progn
    (let ( (*pd--verbose* t)
           (*mp--verbose* nil)
           (*match-pattern2--verbose* nil))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (pd--reset)
      (def (fib 0) 0)
      (def (fib 1) 1)
      (def (fib n)  (+ (fib (- n 1)) (fib (- n 2))))
      (def (double n) (+ n n))
      (def (square y) (* y y))
      (def (double-square y) (double (square y)))
      (double-square 3)

      (prndiv)
      (error-unless "You broke (fib 4): %s" '(it) (= 3 (fib 4)))
      (error-unless "You broke (fib 10): %s" '(it) (= 55 (fib 10)))
      (error-unless "You broke (double 9): %s" '(it) (= 18 (double 9)))
      (error-unless "You broke (square 7): %s" '(it) (= 49 (square 7)))
      
      (prn "Printing the table:")
      (pd--prn-table)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      )))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--pattern-dispatch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

