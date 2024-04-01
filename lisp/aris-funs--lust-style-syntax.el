;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lust-style function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def (fib 0) 0)
;; (def (fib 1) 1)
;; (def (fib n) (+ (fib (- n 1)) (fib (- n 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--match-pattern)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup lust-style-syntax nil
  "Dispatch calls to pseudo-functions using pattern matchings.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom *lust-style-syntax--verbose* nil
  "Whether the pseudo-function dispatcher should print verbose messages."
  :group 'lust-style-syntax
  :type 'boolean)

(defvar *lust-style-syntax--counter* 0
  "This is part of an ugly hack and is not meant to be customized.")

(defvar *lust-style-syntax--pattern-dispatch-table* nil
  "This is where the pattern dispatch table is stored as an alis of alists and is not meant to be customized.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just a quick test here:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (match-pattern '(fib 1 n z) '(fib 1 5 8))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aris-lust-syle-defs--use-print (&rest body)
  "Helper macro to use `indented-message' more easily."
  `(cl-letf (((symbol-function 'print) 
               (if *lust-style-syntax--verbose* #'indented-message #'ignore)))
     (progn ,@body)))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define functions with a Lust-style syntax:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro lust-style-syntax--def (pattern &rest body) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a function call pattern case or a variable using a Lust-style syntax."
  (aris-lust-syle-defs--use-print
    (if (symbolp pattern)
      `(progn
         (when ,(cdr body)
           (error "DEF: Variable definition's body must be a single value."))
         (message "DEF: Defining variable %s." ',pattern)
         (set ',pattern ,(car body))
         ,(car body))
      `(progn
         (message "DEF: Defining pattern %s." ',pattern)
         (lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun  ',(car pattern))
         (let ((group (assoc ',(car pattern) *lust-style-syntax--pattern-dispatch-table*)))
           (if group
             (progn
               (let ((pattern-case (assoc ',pattern (cdr group))))
                 (message "DEF:   Found pattern-case %s in group %s"
                   (or pattern-case "<none>") group)
                 (if pattern-case
                   (error "DEF:   Pattern %s already defined." ',pattern)))
               (setcdr group (nconc (cdr group) (list (cons ',pattern ',body))))
               (message "DEF: Added pattern-case %s to group %s" ',pattern group))
             (progn
               (message "DEF:   Adding pattern group %s" ',(car pattern))
               (push (cons ',(car pattern) (list (cons ',pattern ',body)))
                 *lust-style-syntax--pattern-dispatch-table*))))
         *lust-style-syntax--pattern-dispatch-table*))))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'def 'lust-style-syntax--def)
(defalias 'begin 'progn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--get-patterns-for-group (group-symbol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cdr (assoc group-symbol *lust-style-syntax--pattern-dispatch-table*)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--bind-group-symbol-to-pattern-dispatcher-fun (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "An internal helper function to bind the pattern dispatcher function to symbols that's used by def."
  (aris-lust-syle-defs--use-print
    (print "BINDING DISPATCH FUN FOR %s!" symbol)
    (unless (symbolp symbol)
      (error "%s is not a symbol." symbol))
    (let* ((already-bound (fboundp symbol)))
      (when (and
              already-bound
              (not (let ((got-symbol (get (function symbol) :PATTERN-DISPATCHER-FUN)))
                   (print "Retrieved %s for %s." got-symbol symbol)
                   got-symbol)))
        (error (concat
                 "Logic error: function %s already bound "
                 "elsewherm, fmakunbound it first!")
          symbol))
      (print "%sBINDING DISPATCH FUN FOR %s!"
        (if already-bound "RE" "") symbol)
      (fset symbol
        (lust-style-syntax--make-pattern-dispatcher-fun symbol))
      (put
        (function symbol)
        :PATTERN-DISPATCHER-FUN
        symbol)
      (print "Marked fun with %s / %s / %s." symbol (function symbol)
        (get
          (function symbol)
          :PATTERN-DISPATCHER-FUN)))))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--eval-match-result (match-result)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate the MATCH-RESULT for a call pattern."
  (aris-lust-syle-defs--use-print
    (print "Evaluating match result %s" match-result)
    (cond
      ((symbolp (cadr match-result))
        (let ((eval-result (eval (cadr match-result))))
          (print
            "Match result %s's pattern case is a symbol, evaluating %s and returning %s."
            match-result (cadr match-result) eval-result)
          eval-result))
      ((atom (cadr match-result))
        (progn
          (print "Match result %s's pattern case is an atom, returning %s."
            match-result (cadr match-result))
          (cadr match-result)))
      (t
        (let ((decorated-result (cons 'let match-result)))
          (print "Evaluating decorated pattern case %s." decorated-result)
          (let ((result (eval decorated-result)))
            (print "Match result %s's pattern case's body returned %s."
              match-result result)
            result))))))
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-lust-syle-defs--match-call-pattern-in-group (call-pattern group)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Find the pattern case in group that matches the call pattern.."
  (aris-lust-syle-defs--use-print
    (when (not group)
      (error "Invalid call, no pattern group for %s." call-pattern))
    (print
      (concat
        "================================================"
        "================================================"))
    (print ">> Dispatch table for %s:" call-pattern)
    (dolist (pattern-case group)
      (print "  %s" pattern-case))
    (print
      (concat
        "================================================"
        "================================================"))
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
            (print "Trying pattern %s on target %s?" pattern call-pattern)
            (let ((match-result (match-pattern pattern call-pattern)))
              ;; (print "Match result reveived ist: %s" match-result)
              (when match-result
                (throw 'matched
                  (setq result (cons match-result (cdr pattern-case)))))))))
      (if result
        (progn
          (print
            (concat
              "================================================"
              "================================================"))
          (print "Found match for %s = %s" call-pattern result)
          (print
            (concat
              "================================================"
              "================================================"))
          result)
        (error "No pattern case found for %s." call-pattern)))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-syntax--make-pattern-dispatcher-fun (symbol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Factory function for pattern dispatch handler functions. The reason we construct new ones each time is
because we're gong to be stshing stuff in their symbol properties."
  (lambda (&rest args)
    (let* ( (function (function symbol))
            (symbol (get function :PATTERN-DISPATCHER-FUN))
            (group (lust-style-syntax--get-patterns-for-group symbol))
            (call-pattern (cons symbol args)))
      (lust-style-syntax--eval-match-result
        (aris-lust-syle-defs--match-call-pattern-in-group call-pattern group)))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--lust-style-syntax)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
