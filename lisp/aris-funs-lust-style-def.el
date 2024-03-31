;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lust-style function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def (fib 0) 0)
;; (def (fib 1) 1)
;; (def (fib n) (+ (fib (- n 1)) (fib (- n 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs-match-pattern)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *lust-style-def--counter* 0
  "This is part of an ugly hack and is not meant to be customized.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *lust-style-def--pattern-dispatch-table* nil
  "This is where the pattern dispatch table is stored as an alis of alists and is not meant to be customized.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just a quick test here:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (match-pattern '(fib 1 n z) '(fib 1 5 8))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define functions with a Lust-style syntax:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro lust-style-def--def (pattern &rest body) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a function call pattern case using a Lust-style syntax."
  `(progn
     (message "DEF: Defining pattern %s." ',pattern)
     (lust-style-def--bind-symbol-to-pattern-dispatcher-fun  ',(car pattern))
     (let ((group (assoc ',(car pattern) *lust-style-def--pattern-dispatch-table*)))
       (if group
         (let ((pattern-case (assoc ',pattern (cdr group))))
           (message "DEF:   Found pattern-case %s in group %s"
             (or pattern-case "<none>") group)
           (if pattern-case
             (error "DEF:   Pattern %s already defined." ',pattern))
           (setcdr group (nconc (cdr group) (list (cons ',pattern ',body))))
           ;;           (setcdr group (cons (cons ',pattern ',body) (cdr group)))
           (message "DEF: Added pattern-case %s to group %s" ',pattern group))
         (progn
           (message "DEF:   Adding pattern group %s" ',(car pattern))
           (push (cons ',(car pattern) (list (cons ',pattern ',body)))
             *lust-style-def--pattern-dispatch-table*))))
     *lust-style-def--pattern-dispatch-table*))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'def 'lust-style-def--def)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-def--bind-symbol-to-pattern-dispatcher-fun (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "An internal helper function to bind the pattern dispatcher function to symbols that's used by def."
  (message "BINDING DISPATCH FUN FOR '%s!" symbol)
  (unless (symbolp symbol)
    (error "%s is not a symbol." symbol))
  (let* ((already-bound (fboundp symbol)))
    (when (and
            already-bound
            (not (get (function symbol) :PATTERN-DISPATCHER-FUN)))
      (error (concat
               "Logic error: function %s already bound "
               "elsewherm, fmakunbound it first!")
        symbol))
    (message "%sBINDING DISPATCH FUN FOR '%s!"
      (if already-bound "RE" "") symbol)
    (fset symbol
      (lust-style-def--make-pattern-dispatcher-fun symbol))
    (put
      (function symbol)
      :PATTERN-DISPATCHER-FUN
      symbol)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-def--get-patterns-for-symbol (symbol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cdr (assoc symbol *lust-style-def--pattern-dispatch-table*)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-def--make-pattern-dispatcher-fun (symbol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (lambda (&rest args)
    "Factory function for pattern dispatch handler functions. The reason we construct new ones each time is
because we're gong to be stshing stuff in their symbol properties."
    (let* ((function (function symbol))
            (symbol (get function :PATTERN-DISPATCHER-FUN))
            (group (lust-style-def--get-patterns-for-symbol symbol))
            (call-pattern (cons symbol args)))
      (lust-style-def--eval-match-result
        (aris-lust-syle-defs--match-call-pattern-in-group call-pattern group)))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lust-style-def--eval-match-result (result)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate the result of a match."
  (message "Evaluating match result %s" result)
  (if (atom (cadr result))
    (progn
      (message "Match result is an atom, returning %s" (cadr result))
      (cadr result))
    (let ((result (cons 'let result)))
      (message "Evaluating augmented match result %s" result)
      (let ((result (eval result)))
        (message "Evaluated match result %s" result)
        result))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-lust-syle-defs--match-call-pattern-in-group (call-pattern group)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Find the pattern case in group that matches the call pattern.."
  (when (not group)
    (error "Invalid call, no pattern group for %s." call-patternl))

  (message ">> Dispatch table for %s:" call-pattern)
  (dolist (pattern-case group)
    (message "  %s" pattern-case))

  (let (result)
    (catch 'matched
      (dolist (pattern-case group)
        (let ( (pattern (car pattern-case))
               (*match-pattern--init-fun*
                 (lambda () (setq *lust-style-def--counter* 0)))
               (*match-pattern--merge-duplicate-alist-keys* nil)
               (*match-pattern--kleene-tag* nil)
               (*match-pattern--anything-tag* 'anything)
               (*match-pattern--verbatim-element?* nil)
               (*match-pattern--capture-element?*
                 (lambda (elem)
                   (if (> *lust-style-def--counter* 1)
                     (symbolp elem)
                     (setq *lust-style-def--counter*
                       (1+ *lust-style-def--counter*))
                     nil)))
               (*match-pattern--get-capture-symbol-fun* (lambda (e) e))
               (*match-pattern--get-capture-tag-fun* (lambda (e) 'anything))
               (*match-pattern--capture-can-be-predicate* nil)
               (*match-pattern--target-elements-must-be-verbatim* nil)
               (*match-pattern--use-dotted-pairs-in-result* nil))
          (message "Trying #pattern %s on target %s?" pattern call-pattern)
          (let ((match-result (match-pattern pattern call-pattern)))
            ;; (message "Match result reveived ist: %s" match-result)
            (when match-result
              (throw 'matched
                (setq result (cons match-result (cdr pattern-case)))))))))
    (if result
      (progn
        (message "=====================================================================")
        (message "Found match for %s = %s" call-pattern result)
        (message "=====================================================================")
        result)
      (error "No pattern case found for %s." call-pattern))))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs-lust-style-def)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
