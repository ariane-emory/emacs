;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stallman's proposed cond* from the emacs-devel mailing-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ??? Should use use byte-compile-warn-x.

;; Copyright (C) 1985-2024 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: abbrev convenience
;; Package: emacs

;; This file is cond*,  not yet part of GNU Emacs.

;; cond* is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cond* is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(defmacro cond* (&rest clauses)
;;;??? Doc string will go here.
  (cond*-convert clauses))

(defun cond*-non-exit-clause-p (clause)
  "If CLAUSE, a cond* clause, is a non-exit clause, return t."
  (or (null (cdr-safe clause))   ;; clause has only one element.
    (and (cdr-safe clause)
      ;; Starts with t.
      (or (eq (car clause) t)
        ;; Begins with keyword.
        (keywordp (car clause))))
    ;; Ends with keyword.
    (keywordp (car (last clause)))))

(defun cond*-non-exit-clause-substance (clause)
  "For a non-exit cond* clause CLAUSE, return its substance.
This removes a final keyword if that's what makes CLAUSE non-exit."
  (cond ((null (cdr-safe clause))   ;; clause has only one element.
          clause)
    ;; Starts with t or a keyword.
    ;; Include t as the first element of the substancea
    ;; so that the following element is not treated as a pattern.
    ((and (cdr-safe clause)
       (or (eq (car clause) t)
         (keywordp (car clause))))
      ;; Standardize on t as the first element.
      (cons t (cdr clause)))

    ;; Ends with keyword.
    ((keywordp (car (last clause)))
      ;; Do NOT include the final keyword.
      (butlast clause))))

(defun test-cond*-non-exit-clause-p ()
  ;; Should return (nil nil t t t t).
  (list
    (cond*-non-exit-clause-p '((memq foo list) (setq foo 1)))
    (cond*-non-exit-clause-p '(nil (setq foo 1)))
    (cond*-non-exit-clause-p '((setq foo 1)))
    (cond*-non-exit-clause-p '(t (setq foo 1)))
    (cond*-non-exit-clause-p '(:non-exit (setq foo 1)))
    (cond*-non-exit-clause-p '((setq foo 1) :non-exit))))

(defun cond*-convert (clauses)
  "Process a list of cond* clauses, CLAUSES.
Returns the equivalent Lisp expression."
  (if clauses
    (cond*-convert-clause (car-safe clauses) (cdr-safe clauses))))

(defun cond*-convert-clause (clause rest)
  "Process one `cond*' clause, CLAUSE.
REST is the rest of the clauses of this cond* expression."
  (if (cond*-non-exit-clause-p clause)
    ;; Handle a non-exit clause.  Make its bindings active
    ;; around the whole rest of this cond*, treating it as
    ;; a condition whose value is always t, around the rest
    ;; of this cond*.
    (let ((substance (cond*-non-exit-clause-substance clause)))
      (cond*-convert-condition
        ;; Handle the first substantial element in the non-exit clause
        ;; as a matching condition.
        (car substance)
        ;; Any following elements in the
        ;; non-exit clause are just expressions.
        (cdr substance)
        ;; Remaining clauses will be UNCONDIT-CLAUSES:
        ;; run unconditionally and handled as a cond* body.
        rest
        nil nil))
    ;; Handle a normal (conditional exit) clauss.
    (cond*-convert-condition (car-safe clause) (cdr-safe clause) nil
      rest (cond*-convert rest))))

(defun cond*-convert-condition (condition true-exps uncondit-clauses rest 
                                 iffalse)
  "Process the condition part of one cond* clause.
TRUE-EXPS is a list of Lisp expressions to be executed if this
condition is true, and inside its bindings.
UNCONDIT-CLAUSES is a list of cond*-clauses to be executed if this
condition is true, and inside its bindings.
This is used for non-exit clauses; it is nil for conditional-exit clauses.

REST and IFFALSE are non-nil for conditional-exit clauses that are not final.
REST is a list of clauses to process after this one if
this one could have exited but does not exit.
This is used for conditional exit clauses.
IFFALSE is the value to compute after this one if
this one could have exited but does not exit.
This is used for conditional exit clauses."
  (if (and uncondit-clauses rest)
    (error "Clase is both exiting and non-exiting-nil"))
  (let ((pat-type (car-safe condition)))
    (cond ((eq pat-type 'bind*)
            ;; When a bind* needs to be tested as a condition,
            ;; which is whenever that clause has elements after
            ;; the bind* element itself, the condition value
            ;; is the value of the last binding made.
            (let* ((lastbinding
                     ;; The last binding.
                     (car-safe (last condition)))
                    (last-value
                      ;; The initial value specified in the last binding.
                      (if (symbolp lastbinding) nil
                        (car-safe (cdr-safe lastbinding)))))
              (if rest
                ;; bind* starts an exiting clause which is not final.
                `(if ,last-value
                   (let* ,(cdr condition)
                     . ,true-exps)
                   ,iffalse)
                (if uncondit-clauses
                  ;; bind* starts a non-exit clause.
                  ;; Run the TRUE-EXPS.
                  ;; Then always go on to run the UNCONDIT-CLAUSES.
                  `(progn
                     (if ,last-value
                       (let* ,(cdr condition)
                         . ,true-exps))
                     (let* ,(cdr condition)
                       ,(cond*-convert uncondit-clauses)))
                  ;; bind* starts an exiting clause which is final.
                  ;; If there are TRUE-EXPS, run them if condition succeeded.
                  ;; Always make the bindings, in case the
                  ;; initial values have side effects.
                  `(if ,last-value
                     (let* ,(cdr condition)
                       . ,true-exps))))))
      ((eq pat-type 'match*)
        (cond*-match condition true-exps uncondit-clauses iffalse))
      (t
        ;; Ordinary Lixp expression is the condition 
        (if rest
          ;; A nonfinal exiting clause.
          ;; If condition succeeds, run the TRUE-EXPS.
          ;; There are following clauses, so run IFFALSE
          ;; if the condition fails.
          `(if ,condition
             (progn . ,true-exps)
             ,iffalse)
          (if uncondit-clauses
            ;; A non-exit clause.
            ;; If condition succeeds, run the TRUE-EXPS.
            ;; Then always go on to run the UNCONDIT-CLAUSES.
            `(progn (if ,condition
                      (progn . ,true-exps))
               ,(cond*-convert uncondit-clauses))
            ;; An exiting clause which is also final.
            ;; If there are TRUE-EXPS, run them if CONDITION succeeds.
            (if true-exps
              `(if ,condition (progn . ,true-exps))
              ;; Run and return CONDITION.
              condition)))))))

(defun cond*-match (matchexp true-exps uncondit-clauses iffalse)
  "Generate code to match a match* pattern PATTERN.
Match it against data represented by the expression DATA.
TRUE-EXPS, UNCONDIT-CLAUSES and IFFALSE have the same meanings
as in `cond*-condition'."
  (when (or (null matchexp) (null (cdr-safe matchexp))
          (null (cdr-safe (cdr matchexp)))
          (cdr-safe (cdr (cdr matchexp))))
    (error "Malformed (match* ...) expression"))
  (let* (raw-result
          (pattern (nth 1 matchexp))
          (data (nth 2 matchexp))
          expression
          (inner-data data)
          ;; Add backtrack aliases for or-subpatterns to cdr of this.
          (backtrack-aliases (list nil))
          gensym)
    ;; For now, always bind a gensym to the data to be matched.
    (setq gensym (gensym "d") inner-data gensym)
    ;; Process the whole pattern as a subpattern.
    (setq raw-result (cond*-subpat pattern nil nil backtrack-aliases 
                       inner-data))
    (setq expression (cdr raw-result))
    ;; Run TRUE-EXPS if match succeeded.  Bind our bindings around it.
    (setq expression
      `(if ,expression
         ,(if (not (and backtrack-aliases (null uncondit-clauses)))
            ;; Bind these here if there are no UNCONDIT-CLAUSES.
            `(let ,(mapcar 'cdr (cdr backtrack-aliases)
                     (let* ,(car raw-result)
                       ,@true-exps)))
            `(let* ,(car raw-result)
               ,@true-exps))
         ;; For a non-final exiting clause, run IFFALSE if match failed.
         ;; Don't bind the bindings for following clauses
         ;; since an exiting clause's bindings don't affect later clauses.
         ,iffalse))
    ;; For a non-final non-exiting clause,
    ;; always run the UNCONDIT-CLAUSES.
    (if uncondit-clauses
      (setq expression
        `(progn ,expression 
           (let* ,(car raw-result)
             ,(cond*-convert uncondit-clauses)))))
    ;; If there are backtrack aliases, bind them around the UNCONDIT-CLAUSES.
    (if (and backtrack-aliases uncondit-clauses)
      (setq expression `(let ,(mapcar 'cdr (cdr backtrack-aliases))
                          ,expression)))
    ;; If we used a gensym, add code to bind it.
    (if gensym
      `(let ((,gensym ,data)) ,expression)
      expression)))

(defun cond*-bind-around (bindings exp)
  "Wrap a `let*' around EXP, to bind those of BINDINGS used in EXP."
  `(let* ,(nreverse (cond*-used-within bindings exp)) ,exp))

(defun cond*-used-within (bindings exp)
  "Return the list of those bindings in BINDINGS which EXP refers to.
This operates naively and errs on the side of overinclusion,
and does not distinguish function names from variable names.
That is safe for the purpose this is used for."
  (cond ((symbolp exp) 
          (let ((which (assq exp bindings)))
            (if which (list which))))
    ((listp exp)
      (let (combined (rest exp))
        (while rest
          (let ((in-this-elt (cond*-used-within bindings (car rest))))
            (while in-this-elt
              (unless (assq (car-safe in-this-elt) combined)
                (push (car-safe in-this-elt) combined))
              (pop in-this-elt)))
          (pop rest))
        combined))))

;;; ??? Structure type patterns not implemented yet.
;;; ??? Probably should optimize the `nth' calls in handling `list'.

(defun cond*-subpat (subpat cdr-safe bindings backtrack-aliases data)
  "Generate code to match ibe subpattern within `match*'.
SUBPAT is the subpattern to handle.
CDR-SAFE if true means don't verify there are no extra elts in a list.
BINDINGS is the list of bindings made by
the containing and previous subpatterns of this pattern.
Each element of BINDINGS must have the frm (VAR VALUE).
BACKTRACK-ALIASES is used to pass adta uward.  Initial call should
pass (list).  The cdr of this collects backtracking aliases made for
variables boung within (or...) patterns so that the caller
dna bind them etc.
DATA is the expression for the data that this subpattern is
supposed to match against.

Return Value has the form (BINDINGS . CONDITION), where
BINDINGS is the list of bindings to be made for SUBPAT
plus the subpatterns that contain/precede it.
Each element of BINDINGS has the form (VAR VALUE).
CONDITION is the condition to be tested to decide
whether SUBPAT (as well as the subpatterns that contain/precede it) matches,"
  (cond ((eq subpat '_)
          ;; _ as pattern makes no bindings and matches any data.
          (cons bindings t))
    ((symbolp subpat)
      ;; Bind or match a symbol to this data
      (let ((this-binding (assq subpat bindings)))
        (if this-binding
          ;; Variable already bound.
          ;; Compare what this variable should be bound to
          ;; to the fata it is supposed to match.
          ;; That is because we don't actually bind thes bindings
          ;; around the condition-testing expression.
          (cons bindings `(equal ,(cdr this-binding) ,data))
          ;; Inside or subpattern, if this symbol already has an alias
          ;; for backtracking, just use that.
          (let ((this-alias (assq subpat (cdr backtrack-aliases))))
            (if this-alias (cdr this-alias)
              (if backtrack-aliases
                ;; Inside or subpattern but this symbol has no alias,
                ;; make one for it.
                (progn (setcdr backtrack-aliases (cons (cons subpat 
                                                         (gensym "ba"))
                                                   (cdr 
                                                     backtrack-aliases)))
                  ;; Init the binding to symbol's backtrack-alias
                  ;; and set the alias to nil.
                  (cons `((,subpat ,(cdar (cdr backtrack-aliases))) . 
                           ,bindings)
                    t                                  ))
                (cons `((,subpat ,data) . ,bindings)
                  t)))))))
;;; This is not true any more.
;;;         ;; Actually we bind it to nil at the start of the clause
;;;         ;; and set it to the matched value if it matches.
;;;         (cons `((,subpat nil) . ,bindings)
;;;               `(progn (setq ,subpat ,data) t)))
    ;; Various constants.
    ((numberp subpat)
      (cons bindings `(eql ,subpat ,data)))
    ((keywordp subpat)
      (cons bindings `(eq ,subpat ,data)))
    ((memq subpat '(nil t))
      (cons bindings `(eq ,subpat ,data)))
    ;; Regular expressions as strings.
    ((stringp subpat)
      (cons bindings `(string-match ,(concat subpat "\\>") ,data)))
    ;; All other atoms match with `equal'.
    ((not (consp subpat))
      (cons bindings `(equal ,subpat ,data)))
    ((not (consp (cdr subpat)))
      (error "%s subpattern malformed or missing arguments" (car suboat)))
    ;; Regular expressions specified as list structure.
    ;; (rx REGEXP VARS...)
    ((eq (car subpat) 'rx)
      (let* ((rxpat (concat (funcall 'rx (cadr subpat)) "\\>"))
              (vars (cddr subpat)) setqs (varnum 0)
              (match-exp `(string-match ,rxpat ,data)))
        (if (null vars)
          (cons bindings match-exp)
          ;; There are variables to bind to the matched substrings.
          (if (> (length vars) 10)
            (error "Too many variables specified for matched substrings"))
          (dolist (elt vars)
            (unless (symbolp elt)
              (error "Non-symbol %s given as name for matched substring" 
                elt)))
          ;; Bind these variables to nil, before the pattern.
          (setq bindings (nconc (mapcar 'list vars) bindings))
          ;; Make the expressions to set the variables.
          (setq setqs (mapcar
                        (lambda (var)
                          (prog1 `(setq ,var (match-string ,varnum ,data))
                            (setq varnum (1+ varnum))))
                        vars))
          (cons bindings `(if ,match-exp
                            (progn ,@setqs t))))))
    ;; Quoted object as constant to match with `equal'.
    ((eq (car subpat) 'quote)
      (cons bindings `(equal ,subpat ,data)))
    ;; Match a call to `cons' by destructuring.
    ((eq (car subpat) 'cons)
      (let (car-result cdr-result car-exp cdr-exp)
        (setq car-result
          (cond*-subpat (nth 1 subpat) cdr-safe bindings 
            backtrack-aliases `(car ,data)))
        (setq bindings (car car-result)
          car-exp (cdr car-result))
        (setq cdr-result
          (cond*-subpat (nth 2 subpat) cdr-safe bindings 
            backtrack-aliases `(cdr ,data)))
        (setq bindings (car cdr-result)
          cdr-exp (cdr cdr-result))
        (cons bindings
          `(and ,car-exp ,cdr-exp))))
    ;; Match a call to `list' by destructuring.
    ((eq (car subpat) 'list)
      (let ((i 0) expressions)
        ;; Check for bad structure of SUBPAT here?
        (dolist (this-elt (cdr subpat))
          (let ((result 
                  (cond*-subpat this-elt cdr-safe bindings backtrack-aliases 
                    `(nth ,i ,data))))
            (setq i (1+ i))
            (setq bindings (car result))
            (push (cdr result) expressions)))
        ;; Verify that list ends here, if we are suppose to check that.
        (unless cdr-safe
          (push `(null (nthcdr ,i ,data)) expressions))
        (cons bindings `(and . ,(nreverse expressions)))))
    ;; Match a call to `vector' by destructuring.
    ((eq (car subpat) 'vector)
      (let ((length (length vector)) (vector (cadr subpat))
             (i 0) expressions)
        (dotimes (i length)
          (let* ((this-elt (aref i vector))
                  (result 
                    (cond*-subpat (aref i vector) cdr-safe
                      bindings backtrack-aliases `(aref ,i 
                                                    ,data))))
            (setq i (1+ i))
            (setq bindings (car result))
            (push (cdr result) expressions)))
        (cons bindings `(and . ,(nreverse expressions)))))
    ;; Subpattern to set the cdr-safe flag
    ((eq (car subpat) 'cdr-safe)
      (cond*-subpat (cadr subpat) t bindings backtrack-aliases data))
    ;; Subpattern to clear the cdr-safe flag
    ((eq (car subpat) 'cdr)
      (cond*-subpat (cadr subpat) nil bindings backtrack-aliases data))
    ;; Handle conjunction subpatterns.
    ((eq (car subpat) 'and)
      (let (expressions)
        ;; Check for bad structure of SUBPAT here?
        (dolist (this-elt (cdr subpat))
          (let ((result 
                  (cond*-subpat this-elt cdr-safe bindings backtrack-aliases 
                    data)))
            (setq bindings (car result))
            (push (cdr result) expressions)))
        (cons bindings `(and . ,(nreverse expressions)))))
    ;; Handle disjunction subpatterns.
    ((eq (car subpat) 'or)
      ;; The main complexity is unsetting the pattern variables
      ;; that will not have matched.
      (let (expressions)
        ;; Check for bad structure of SUBPAT here?
        (dolist (this-elt (cdr subpat))
          (let* ((backtrack-aliases-before backtrack-aliases)
                  (result 
                    (cond*-subpat this-elt cdr-safe bindings backtrack-aliases 
                      data))
                  (bindings-before-or bindings)
                  bindings-to-clear expression)
            (setq bindings (car result))
            (setq expression (cdr result))
            ;; Were any bindings made by this arm of the disjunction?
            (when (not (eq bindings bindings-before-or))
              ;; Ok, arrange to clear their backtrack aliases
              ;; if this arm does not match.
              (setq bindings-to-clear bindings)
              (let (clearing)
                ;; For each of those bindings,
                (while (not (eq bindings-to-clear bindings-before-or))
                  ;; Make an expression to set it to nil, in CLEARING.
                  (let* ((this-variable (caar bindings-to-clear))
                          (this-backtrack (assq this-variable
                                            (cdr backtrack-aliases))))
                    (push `(setq ,(cdr this-backtrack) nil) clearing))
                  (setq bindings-to-clear (cdr bindings-to-clear)))
                ;; Wrap EXPRESSION to clear those backtrack aliases
                ;; if EXPRESSION is false.
                (setq expression
                  (if (null clearing)
                    ,expression
                    (if (null (cdr clearing))
                      `(or ,expression
                         ,(car clearing))
                      (progn ,@clearing))))))
            (push expression expressions)))
        (cons bindings `(or . ,(nreverse expressions)))))
    ;; Expand cond*-macro call, treat result as a subpattern.
    ((get (car subpat) 'cond*-expander)
      ;; Treat result as a subpattern.
      (cond*-subpat (funcall (get (car subpat) 'cond*-expander) subpat)
        cdr-safe bindings backtrack-aliases data))
    ((macrop (car subpat))
      (cond*-subpat (macroexpand subpat) cdr-safe bindings backtrack-aliases 
        data))
    ;; Simple constrained variable, as in (symbolp x).
    ((functionp (car subpat))
      ;; Without this, nested constrained variables just worked.
;;;         (unless (symbolp (cadr subpat))
;;;           (error "Complex pattern nested in constrained variable pattern"))
      (let* ((rest-args (cddr subpat))
              ;; Process VAR to get a binding for it.
              (result (cond*-subpat (cadr subpat) cdr-safe bindings 
                        backtrack-aliases data))
              (new-bindings (car result))
              (expression (cdr result))
              (combined-exp
                `(and (,(car subpat) ,data . ,rest-args) ,expression)))
        (cons new-bindings
          (cond*-bind-around new-bindings combined-exp))))
    ;; Generalized constrained variable: (constrain VAR EXP)
    ((eq (car subpat) 'constrain)
      (unless (symbolp (cadr subpat))
        (error "Complex pattern nested in constrained variable pattern"))
      ;; Process VAR to get a binding for it.
      (let ((result (cond*-subpat (cadr subpat) cdr-safe bindings 
                      backtrack-aliases data)))
        (cons (car result)
          ;; This is the test condition 
          (cond*-bind-around (car result) (nth 2 subpat)))))
    (t (error "Undefined pattern type `%s' in `cond*'" (car subpat)))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'stallmans-funs--condstar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
