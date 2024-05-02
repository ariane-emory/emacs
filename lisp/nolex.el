;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--destructuring-match)
(require 'aris-funs--lists)
(require 'aris-funs--plists)
(require 'aris-funs--with-gensyms)
(require 'aris-funs--symbolicate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun new-var-name? (symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Remove the exclamation mark from SYMBOL if it ends with one, otherwise return nil."
  (when (and (symbolp symbol)          ; Check if it's a symbol
          (string-suffix-p "!" (symbol-name symbol)))  ; Check if it ends with "!"
    (intern (substring (symbol-name symbol) 0 -1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (new-var-name? 'foo) returns nil)
(confirm that (new-var-name? 'bar!) returns bar)
(confirm that (new-var-name? 'baz!!!) returns baz!!)
(confirm that (new-var-name? 7) returns nil)
(confirm that (new-var-name? '(1 2 3)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun make-member-sym-p (lst)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Generate a membership predicate fun for LST."
;;   (lambda (thing &rest _) (and (symbolp thing) (member thing lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-member-sym-p (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Generate a membership predicate fun for LST."
  `(lambda (thing &rest _) (and (symbolp thing) (member thing ,lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (not (null (member 'a '(a b c)))) returns t)
(confirm that (not (null (member 'b '(a b c)))) returns t)
(confirm that (not (null (member 'c '(a b c)))) returns t)
(confirm that (not (null (member 'd '(a b c)))) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-pick (lst)
  `(lambda (&rest _)
     (let ((lst ,lst))
       ;; (prn "pick: %s" lst)
       (elt lst (random (length lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun make-pick (lst)
;;   (lambda (&rest _)
;;     ;; (prn "pick: %s" lst)
;;     (elt lst (random (length lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (not (null (member (funcall (make-pick '(a b c))) '(a b c)))) returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'pick-qty           (make-pick '(some many enough)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'nil/t?             (make-member-sym-p '(nil t)))
(defalias 'am/are?            (make-member-sym-p '(am are)))
(defalias 'a/an?              (make-member-sym-p '(a an)))
(defalias 'a/an/the?          (make-member-sym-p '(a an the)))
(defalias 'had/have?          (make-member-sym-p '(had have)))
(defalias 'do/does?           (make-member-sym-p '(do does)))
(defalias 'a/another/to?      (make-member-sym-p '(a another to)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'do/would?          (make-member-sym-p '(do would)))
(defalias 'pick-do/would?     (make-pick '(do would)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar   *desire-words*     '(like need want))
(defalias 'desire?            (make-member-sym-p '(like need want)))
(defalias 'pick-desire        (make-pick *desire-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar    *subject-words*   '(i you))
(defalias 'subject?           (make-member-sym-p *subject-words*))
(defalias 'pick-subject       (make-pick *subject-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar    *plural-subject-words*  '(we they))
(defalias 'plural-subject?          (make-member-sym-p *plural-subject-words*))
(defalias 'pick-plural-subject      (make-pick *plural-subject-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar    *us-them*               '(them us))
(defalias 'plural-subject2?         (make-member-sym-p *us-them*))
(defalias 'pick-plural-subject2     (make-pick *us-them*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar   *modal-words*      '( would
                                should
                                could
                                will
                                can
                                do
                                must))
(defvar   *modal-plus*        (append *modal-words* '(have haven\'t)))
(defvar   *neg-modal-words*  '( ;; don\'t
                                ;; do\ not
                                wouldn\'t
                                would\ never
                                would\ not
                                shouldn\'t
                                should\ never
                                should\ not
                                couldn\'t
                                could\ never
                                could\ not
                                won\'t
                                will\ never
                                will\ not
                                can\'t
                                can\ never
                                can\ not
                                musn\'t
                                must\ never
                                must\ not
                                ;; haven\'t
                                cannot))
(defvar   *modal-pairs*       (cl-pairlis *modal-words* *neg-modal-words*))
(defvar   *all-modal-words*   (cons 'might ;; v include *modal-words* thrics for probability:
                                (append *modal-words* *modal-words* *modal-words*
                                  *neg-modal-words*)))
(defalias 'modal?             (make-member-sym-p *modal-words*))
(defalias 'modal-plus?        (make-member-sym-p *modal-plus*))
(defalias 'neg-modal?         (make-member-sym-p *neg-modal-words*))
(defalias 'any-modal?         (make-member-sym-p *all-modal-words*))
(defalias 'pick-modal         (make-pick (cl-remove 'have *modal-words*)))
(defalias 'pick-neg-modal     (make-pick *neg-modal-words*))
(defalias 'pick-any-modal     (make-pick *all-modal-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar   *epistemic-words*  '(know believe suspect think))
(defalias 'epistemic?         (make-member-sym-p *epistemic-words*))
(defalias 'pick-epistemic     (make-pick *epistemic-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'pick-possibility
  (make-pick '(do don\'t sometimes\ do always never might would wouldn\'t)))
(defalias 'pick-obviousness
  (make-pick '(nil nil nil clearly plainly actually secretly obviously)))
(defalias 'pick-insult-adj
  (make-pick '(brainded stupid silly dumb ridiculous demented deranged assinine idiotic)))
(defalias 'pick-insult-noun
  (make-pick '(idiot moron nincompoop fool imbecile jackass knucklehead nitwit dumbass)))
(defalias 'pick-i-am/you-are (make-pick '(i\ am you\ are)))
(defalias 'pick-certainty    (make-pick '(certain sure convinced)))
(defalias 'pick-maybe-that   (make-pick '(that nil)))
(defalias 'pick-maybe-not    (make-pick '(not nil)))
(defalias 'pick-probably-not (make-pick '(not not nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (not (null (subject? 'i))) returns t)
(confirm that (not (null (subject? 'you))) returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *swap-words*
  '( (i . you)
     (am . are)
     (we . they)
     (do . does)
     (my . your)
     (them . us)
     (think . know)
     (need . want)
     (had . have)
     (think . believe)
     (know . suspect)
     (always . never)
     (should . shouldn\'t)
     (could . couldn\'t)
     (will . won\'t)
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-swapper (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (lambda (val var var-alist)
    "Return the swapped word for VAL found in ALIST, or VAL if none."
    (cond
      ((assoc  val alist) (cdr (assoc  val alist)))
      ((rassoc val alist) (car (rassoc val alist)))
      (t val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-swapper (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(lambda (val var var-alist)
     "Return the swapped word for VAL found in ALIST, or VAL if none."
     (cond
       ((assoc  val ,alist) (cdr (assoc  val ,alist)))
       ((rassoc val ,alist) (car (rassoc val ,alist)))
       (t val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun swap-word (var val var-alist)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Return the swapped word for VAL found in *SWAP-WORDS*, or VAL if none."
;;   (cond
;;     ((assoc  val *swap-words*) (cdr (assoc  val *swap-words*)))
;;     ((rassoc val *swap-words*) (car (rassoc val *swap-words*)))
;;     (t val)))
(defalias 'swap-word (make-swapper *swap-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (swap-word 'i 'x '((x . i))) returns you)
(confirm that (swap-word 'you 'x '((x . you))) returns i)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun dup-var (var val var-alist)
;;   (cl-loop for n from 1
;;     for new-var = (intern (format "$%d" n))
;;     until (not (assoc new-var var-alist))
;;     finally (nconc var-alist (list (cons new-var val))))
;;   nil)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dup-var (val var var-alist)
  ;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Duplicate a VAR in VAR-ALIST with a new name."
  (cl-loop for suffix from 0
    for new-var = (intern (concat (symbol-name var) (make-string suffix ?*)))
    until (not (assoc new-var var-alist))
    finally (nconc var-alist (list (cons new-var val))))
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (let ((var-alist '((x . i) (x* . j))))
                (dup-var 'i 'x var-alist)
                var-alist)
  returns ((x . i) (x* . j) (x** . i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun repeat-word (val var var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Double a symbol with a hyphen in between the two, foo ⇒ foo-foo."
  ;; (prn "THESE: %s %s %s" var val var-alist)
  (symbolicate- val val))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (repeat-word 'bo 'x '((x . bo))) returns bo-bo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun have-to-know/knew (val var var-alist)
  (if (eq val 'have)
    'know\ that
    'knew\ that))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun i-me (val var var-alist)
  (if (eq val 'i)
    'me
    'i))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun i-you (val var var-alist)
  (if (eq val 'i)
    'you
    'i))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun i-to-know/knew (val var var-alist)
  (if (eq val 'i)
    'already\ knew\ that
    'already\ knew\ that
    ;; 'know\ that ; neutered for a moment
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun proc-tests (var-testses var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (null var-testses)
    t
    (with-gensyms (result)
      (catch result
        (dolist (var-tests var-testses)
          (let* ( (var   (car var-tests))
                  (tests (cdr var-tests))
                  ;; when var is _, the test must just be examining at VAR-LIST:
                  (is-no-var-test (eq '_ var))
                  (assoc (if is-no-var-test (cons '_ nil) (assoc var var-alist)))
                  (val   (cdr assoc)))
            (unless assoc (error "missing var %s" var))
            (dolist (test tests)
              (let ((res
                      ;; Not sure if this list case is a good idea yet:
                      ;; (if (consp val) ;; don't use listp here!
                      ;;   (cl-every (lambda (x) (funcall test x var var-alist)) val)
                      (funcall test val var var-alist)
                      ;;)
                      ))
                (unless res
                  (throw result nil))))))
        t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (proc-tests '((subj subject?)) '((subj . i) (bar . think) (baz . you)))
  returns t)
(confirm that (proc-tests '((subj subject?)) '((subj . x) (bar . think) (baz . you)))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun proc-funs (var-funses var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cl-flet ( (prn2    (&rest args) (when *proc-funs-verbose* (apply #'prn    args)))
             (prndiv2 (&rest args) (when *proc-funs-verbose* (apply #'prndiv nil))))
    (dolist (var-funs var-funses)
      (let ( (var     (car var-funs))
             (funs    (cdr var-funs)))
        (when (eq '_! var) (error "illegal var %s" var))
        (let* ( (is-discard (eq '_ var))
                (new-var    (unless is-discard (new-var-name? var)))
                (var        (if new-var new-var var))
                (assoc      (unless new-var (assoc var var-alist))))
          (cond
            (is-discard) ;; do nothing.
            ((and new-var assoc)         (error "key %s already taken" var))
            ((and (not new-var) (not assoc)) (error "missing var %s"       var)))
          (cond
            (is-discard
              (setf assoc     (cons var nil))) ; left unattached to VAR-ALIST!
            (new-var
              (setf assoc     (cons var nil)) ; this  used to set it to t...
              (setf var-alist (cons assoc var-alist))
              (prndiv2)
              (prn2 "NEW-VAR:   %s" var)))
          (dolist (fun funs)
            (prndiv2)
            (prn2 "var:       %s" var)
            (let ((val (cdr assoc)))
              (prn2 "val:       %s" val)
              (prn2 "fun:       %s" fun)
              (prn2 "VAR-ALIST:")
              (let (lisp-indent-offset)
                (prn2 "%s" (trim-trailing-whitespace (pp-to-string var-alist))))
              ;; This doesn't seem necessary (with lexical-binding off:
              ;; (when (consp fun)
              ;;   (setf fun (eval fun)))
              ;; (prn2 "fun2:      %s" fun)
              (let ((res
                      (if (consp val) ;; don't use listp here!
                        (rmapcar val (lambda (x) (funcall fun x var var-alist)))
                        (funcall fun val var var-alist))))
                (prn2 "funres:    %s" res)
                ;; (when res 
                (setf (cdr assoc) res)
                ;; ) ;; end of when res
                ))
            (prn2 "VAR-ALIST2:")
            (let (lisp-indent-offset)
              (prn2 "%s" (trim-trailing-whitespace (pp-to-string var-alist))))
            ))))
    (prndiv2)
    (prn2 "DONE PROC FUNS.")
    (prndiv2)
    var-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *proc-funs-verbose* t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (proc-funs
                '((subj swap-word) (subj-2 swap-word))
                '((subj . i) (subj-2 . you) (baz . you)))
  returns ((subj . you) (subj-2 . i) (baz . you)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fill-in-missing-rule-keys (rule)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: this also converts RULE from a plist to an alist."
  (fill-in-missing-alist-keys *fillable-rule-keys* (plist-to-alist rule)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *fillable-rule-keys* '(:input-pattern :var-tests :responses))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (fill-in-missing-rule-keys
    '( :input-pattern ( this is the ,@things)
       :responses
       ((:response-pattern ( 18 ,persp not really ,certainty if this is ,@things )))))
  returns ( (:var-tests)
            (:input-pattern this is the (\,@ things))
            (:responses
              (:response-pattern (18 (\, persp) not really (\, certainty) if this is (\,@ things))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let-rule (rule &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(let-alist (fill-in-missing-rule-keys ,rule) ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let-rule
    '( :input-pattern ( this is the ,@things)
       :responses
       ((:response-pattern ( 18 ,persp not really ,certainty if this is ,@things ))))
    (list .:input-pattern .:var-tests .:responses))
  returns
  ( (this is the (\,@ things))
    nil
    ((:response-pattern (18 (\, persp) not really (\, certainty) if this is (\,@ things))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun select-response (var-alist responses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (prn "s-a-f-r's var-alist: %s" var-alist)
  ;; (prn "s-a-f-r's responses: %s" responses)
  (cl-flet ( (prn2    (&rest args) (when *select-response-verbose* (apply #'prn    args)))
             (prndiv2 (&rest args) (when *select-response-verbose* (apply #'prndiv nil))))
    (let ((response (pick responses)))
      ;; (prn "s-a-f-r   picked:    %s" response)
      (let-response response
        (setf var-alist (proc-funs .:var-funs var-alist))
        (throw 'result (dm:fill .:response-pattern var-alist))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *select-response-verbose* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fill-in-missing-response-keys (response)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: this also converts RESPONSE from a plist to an alist."
  (fill-in-missing-alist-keys *fillable-response-keys* (plist-to-alist response)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *fillable-response-keys* '(:var-funs :response-pattern))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (fill-in-missing-response-keys
    '(:response-pattern ( 18 ,persp not really ,certainty if this is ,@things )))
  returns ( (:var-funs)
            (:response-pattern 18 (\, persp) not really (\, certainty) if this is (\,@ things))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let-response (response &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(let-alist (fill-in-missing-response-keys ,response) ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let-response '(:response-pattern ( 18 ,persp not really ,certainty if this is ,@things ))
    (list .:var-funs .:response-pattern))
  returns ( nil
            (18 (\, persp) not really (\, certainty) if this is (\,@ things))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-response (input)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Transform INPUT according to *RULES*, returning nil if none match."
  ;; (prn "get-response INPUT: %s" input)
  (cl-flet ( (prn2    (&rest args) (when *get-response-verbose* (apply #'prn    args)))
             (prndiv2 (&rest args) (when *get-response-verbose* (apply #'prndiv nil))))
    (catch 'result
      (dolist (rule *rules*)
        (let-rule rule
          (catch 'continue
            (prn2 "try:       %s" .:input-pattern)
            (if (eq t .:input-pattern)
              ;; t matches any input and fills using an empty list:
              (throw 'result (select-response nil .:responses))
              (when-let ((var-alist (dm:match .:input-pattern input)))
                (let ((var-alist (if (eq t var-alist) nil var-alist)))
                  (unless (proc-tests .:var-tests var-alist) (throw 'continue nil))
                  (prn2 "MATCHED:   %s" .:input-pattern)
                  (throw 'result (select-response var-alist .:responses)))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *get-response-verbose* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun converse ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Have a conversation with the bot. Enter 'bye' to exit."
  (interactive)
  (catch 'exit
    (while t
      (let ((input (read)))
        (when (member input '(bye (bye)))
          (throw 'exit nil))
        (prn "INPUT:    %s" input)        
        (let ((response
                (if (proper-list-p input)
                  (get-response input)
                  '(sorry \, I didn\'t hear you \!))))
          (prn "RESPONSE: %s" (prettify-sentence response t)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun punctuation? (sym)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (not (null (member sym '(! \? \, "!" "?" "," ".")))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (punctuation? '!) returns t)
(confirm that (punctuation? '\?) returns t)
(confirm that (punctuation? 'foo) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prettify-sentence (lst &optional drop-first)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Render a Nolex input/output sentence (a list of symbols) as a string.
This was very quick 'n' dirty and could probably be a lot cleaner."
  (let ((lst (if *prettify-sentence-strip*
               (cl-remove-if #'nil/t? (if drop-first (cdr lst) lst))
               lst)))
    (wm::capitalize
      (let ((lst (if (punctuation? (car (last lst))) lst (append lst (list ".")))))
        (apply #'concat
          (cons (format "%s" (car lst))
            (rmapcar (cdr lst)
              (lambda (e)
                ;; (prn "this: %s" e)
                (format
                  (if (punctuation? e) "%s" " %s")
                  (if (eq 'i e) 'I e))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *prettify-sentence-strip* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ;;==============================================================================================
     ( :input-pattern    ( ,subject ,had/have ,a/an ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (had/have        had/have?)
                           (a/an            a/an?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         dup-var dup-var)
                               (had/have        swap-word)
                               (subject*        i-to-know/knew)
                               (subject**       swap-word)
                               (things          swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 1 ,subject ,subject*  ,subject** ,had/have ,a/an ,@things \!))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject  ,epistemic that ,subject-2 ,modal ,verb-2
                           ,a/an ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (epistemic       epistemic?)
                           (subject-2       subject?)
                           (modal           modal?)
                           (a/an            a/an?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (epistemic       pick-epistemic)
                               (subject         swap-word)
                               (subject-2       swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 2 do ,subject really ,epistemic that ,subject-2 ,modal
                               ,verb-2 ,a/an ,@things \?))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject  ,am/are ,a/an/the ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (am/are          am/are?)
                           (a/an/the        a/an/the?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         dup-var)
                               (iadj!           pick-insult-adj)
                               (subject*        swap-word)
                               (am/are          dup-var)
                               (am/are*         swap-word)
                               (obv!            pick-obviousness))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 3 don\'t be ,iadj \, ,subject* ,am/are* not ,a/an/the
                               ,@things \, ,subject ,am/are ,obv the ,@things \!))))
     ;;==============================================================================================
     ( ;;--------------------------------------------------------------------------------------------
       :input-pattern    ( ,subject would ,desire many ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (desire          desire?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word)
                               (qty! pick-qty))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 4 don\'t ,subject have ,qty ,@things already \?))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject would like ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 5 why do you think that ,subject would like ,@things \?))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,desire ,a/an ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (desire          desire?)
                           (a/an            a/an?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word)
                               (desire          swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 6 do ,subject really ,desire ,a/an ,@things \?))))
     ;;==============================================================================================
     ( :input-pattern    ( ,do/would ,subject ,desire ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (do/would        do/would?)
                           (subject         subject?)
                           (desire          desire?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word)
                               (desire          pick-desire)
                               (poss!           pick-possibility))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 7 ,subject ,poss ,desire ,@things))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,bar ,baz)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 8 fine \, ,subject ,bar ,baz \, so what \?))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,modal ,verb ,a/an/the ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (modal           modal?)
                           ;; (_ (lambda (val var var-alist)
                           ;;      (prn "THIS HAPPENED! %s" var-alist)
                           ;;      t))
                           (a/an/the        a/an/the?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word)
                               (subject-2!      pick-subject)
                               (epistemic!      pick-epistemic)
                               (maybe-that!     pick-maybe-that)
                               ;; (_ (lambda (val var var-alist)
                               ;;      (prn "THIS ALSO HAPPENED! %s" var-alist)
                               ;;      t))
                               (modal-2!        pick-modal))
           ;;------------------------------------------------------------------------------------------
           :response-pattern ( 9 ,subject-2 ,epistemic ,maybe-that ,subject
                               ,modal-2 ,verb ,a/an/the ,@things))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,neg-modal ,verb ,a/an/the ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (neg-modal       neg-modal?)
                           (a/an/the        a/an/the?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word)
                               (subject-2!      pick-subject)
                               (epistemic!      pick-epistemic)
                               (maybe-that!     pick-maybe-that)
                               (neg-modal-2!    pick-neg-modal))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( '9B ,subject-2 ,epistemic ,maybe-that ,subject
                               ,neg-modal-2 ,verb ,a/an/the ,@things))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,modal never ,verb a ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (modal           modal?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 10 ,subject ,modal ,verb a ,@things \!))))
     ;;==============================================================================================
     ( :input-pattern    ( you ,foo ,baz \!)
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :response-pattern ( 11 no \, it is you who ,foo ,baz \!))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,epistemic that ,subject-2 ,modal-plus never ,verb a ,noun)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (epistemic       epistemic?)
                           (subject-2       subject?)
                           (modal-plus      modal-plus?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word)
                               (subject-2       swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 12 come on \, ,subject can\'t really ,epistemic
                               that ,subject-2 ,modal-plus never ,verb a ,noun \!))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,epistemic that ,subject-2 ,desire ,a/n ,noun)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (epistemic       epistemic?)
                           (subject-2       subject?)
                           (desire          desire?)
                           (a/n             a/an?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word)
                               (subject-2       swap-word)
                               (epistemic       swap-word)
                               (desire          swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 13 after this conversation \, ,subject
                               ,epistemic that ,subject-2
                               ,desire ,a/n ,noun \!))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,desire to ,verb ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject          subject?)
                           (desire           desire?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject          swap-word)
                               (subject-2!       pick-subject)
                               (desire           swap-word)
                               (epistemic!       pick-epistemic))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 14 ,subject-2 don\'t ,epistemic that ,subject
                               really ,desire to ,verb ,@things))))
     ;;==============================================================================================
     ( :input-pattern    ( ,plural-subject are ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (plural-subject  plural-subject?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (plural-subject  dup-var)
                               (plural-subject* swap-word)
                               (adj!            pick-insult-adj)
                               (noun!           pick-insult-noun)
                               (obv!            pick-obviousness))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 15 You ,adj ,noun \,
                               ,plural-subject are not the ,@things \,
                               it is ,obv ,plural-subject* who are
                               the ,@things \!))))
     ;;==============================================================================================
     ( :input-pattern    ( i wish that you were a ,@things)
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs        ( (adj!            pick-insult-adj)
                              (epistemic!      pick-epistemic)
                              (noun!           pick-insult-noun))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 16 you ,adj ,noun \, I already ,epistemic
                               that you want a ,@things))))
     ;;==============================================================================================
     ( :input-pattern    ( these are ,@things)
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (persp!          pick-i-am/you-are)
                               (certainty!      pick-certainty)
                               (probably-not!   pick-probably-not))
           ;;------------------------------------------------------------------------------------------
           :response-pattern ( 17 ,persp ,probably-not really ,certainty that these are ,@things ))))
     ;;==============================================================================================
     ( :input-pattern    ( this is the ,@things)
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (persp!          pick-i-am/you-are)
                               (certainty!      pick-certainty))
           ;;------------------------------------------------------------------------------------------
           ( :response-pattern ( 18 ,persp not really ,certainty if this is ,@things )))))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,epistemic ,plural-subject ,modal ,verb ,them-us ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tasts        ( (subject         subject?)
                           (epistemic       epistemic?)
                           (plural-subject  plural-subject?)
                           (modal           modal?)
                           (them-us         them-us?))
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (subject         swap-word)
                               (plural-subject  swap-word)
                               ;; (modal (make-pick '(foo bar baz)))
                               (modal           pick-any-modal)
                               (them-us         swap-word))
           ;;----------------------------------------------------------------------------------------
           :response-pattern ( 19 ,plural-subject ,modal ,verb ,them-us ,@things \!))))
     ;;==============================================================================================
     ( :input-pattern    ( trigger )
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs         ( (adj!            pick-insult-adj)
                               (noun!           pick-insult-noun))
           ;;------------------------------------------------------------------------------------------
           ( :response-pattern ( 98 yes \, here we are you ,adj ,noun)))))
     ;;==============================================================================================
     ( :input-pattern    t
       ;;--------------------------------------------------------------------------------------------
       :responses
       ( ;;------------------------------------------------------------------------------------------
         ( :response-pattern (99 i don\'t understand \!))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prndiv)
(prn "START:")
(dolist (input
          '(
             ;; 99
             (i don\'t understand \!)
             (foo bar baz)
             (foo bar baz quux)

             ;; 1
             (i have an apple tree)
             ;; (you have a dollar)
             ;; (i had a dollar)
             ;; (you had a dollar)
             ;; (you had a coin in your pocket)
             ;; (you have a coin in your pocket)
             ;; (i have a fly on my arm)

             ;; 2
             (i think that i would like a smoke)
             ;; (i think that you would like a smoke)
             ;; (i think that you should have a smoke)
             ;; (i know that i could have a smoke)
             ;; (i believe that you have seen a ghost)
             ;; (you believe that i have seen a ghost)
             ;; (i think that you should eat a bag of dicks)
             ;; (you know that you must eat a bag of dicks)

             ;; ;; 3
             (you are an asshole)
             ;; (you are a particularly stupid asshole)
             ;; (i am the King of France)
             ;; (i am an evil robot in disguise as a human)
             
             ;; ;; 4
             (i would like many hamburgers with cheese and bacon)
             ;; (i would like many hamburgers with cheese and bacon)
             ;; (i would like many hamburgers with cheese and bacon)
             ;; (i would need many orange cats)
             ;; (i would need many orange cats)
             ;; (i would need many orange cats)
             
             ;; ;; 5
             ;; (i would like a hamburger with cheese and bacon)
             ;; (i would like an orange cat)
             ;; (you would like a hamburger with cheese and bacon)

             ;; ;; 6
             ;; (i want a hamburger with cheese and bacon)
             ;; (i need a hamburger with cheese and bacon)
             ;; (you want a hamburger with cheese and bacon)
             ;; (you need a hamburger with cheese and bacon)

             ;; ;; 7
             ;; (do you like spicy tacos)
             ;; (do you like spicy tacos)
             ;; (do you like spicy tacos)
             ;; (do you like spicy tacos)
             ;; (would you like spicy tacos)
             ;; (would you like spicy tacos)
             ;; (would you like a cigarette)
             ;; (would you like a cigarette)
             ;; (would you like a cigarette)
             ;; (would you like a cigarette)
             ;; (would you like another cigarette)
             ;; (would you like another cigarette)
             ;; (would you like another cigarette)
             ;; (would you like another cigarette)
             
             ;; ;; 8
             ;; (you don\'t understand)
             ;; (dogs eat chickens)
             ;; (you eat chickens)

             ;; ;; 9
             ;; (i could eat a hamburger and some fries)
             ;; (i would climb a tall tree)
             ;; (you would climb a tall tree)
             ;; (you should have a cigarette)

             ;; ;; 10
             ;; (you would never eat a cold hamburger)
             ;; (you should never eat a cold hamburger)
             ;; (you could never eat a cold hamburger)
             ;; (i could never eat a cold hamburger)

             ;; ;; 11
             ;; (you are stupid \!)
             ;; (you suck ass \!)

             ;; ;; 12
             ;; (i know that you have never eaten a hamburger)
             ;; (i suspect that you have never seen a zebra)

             ;; ;; 14
             ;; (i think that you need a drink)
             ;; (you think that i need a drink)
             ;; (i think that i need a drink)
             ;; (you think that you need a drink)

             ;; ;; 14
             ;; (you want to smoke a fat joint)
             ;; (you need to smoke a fat joint)
             ;; (i want to smoke a fat joint)
             ;; (i need to smoke a fat joint)
             ;; (i want to dance in the moonlight)

             ;; ;; 16
             ;; (we are aliens in disguise as humans)
             ;; (we are aliens in disguise as humans)
             ;; (we are aliens in disguise as humans)
             ;; (they are the cutest kittens in the world)
             ;; (they are the cutest kittens in the world)
             ;; (they are the cutest kittens in the world)

             ;; ;; 17
             ;; (i wish that you were a fluffy cat)
             ;; (i wish that you were a duck wearing a tophat)
             ;; (these are the voyages of the starship Enterprise)
             ;; (these are the voyages of the starship Enterprise)
             ;; (these are the voyages of the starship Enterprise)
             ;; (these are the voyages of the starship Enterprise)
             ;; (this is the worst thing ever)
             ;; (i know we could beat them at soccer)
             ;; (i know we could beat them at soccer)
             ;; (i know we could beat them at soccer)
             ;; (you know they could beat us any day)
             ;; (you know they could beat us any day)
             ;; (you know they could beat us any day)
             ;; (you know they can find us)
             ;; (you know they can find us)
             ;; (you know they can find us)

             ;; ;; 9
             ;; (i could eat a hamburger and some fries)
             ;; (i could eat a hamburger and some fries)
             ;; (i could eat a hamburger and some fries)
             ;; (i could eat a hamburger and some fries)
             ;; (you must conquer the empire of the necromancers)
             ;; (you cannot conquer the empire of the necromancers)
             ;; (you won\'t conquer the empire of the necromancers)
             ;; (i must devour the souls of the innocent)
             ;; (i must devour the souls of the innocent)
             ;; (i must devour the souls of the innocent)
             ))
  
  (prndiv)
  (prn "INPUT:     %s" (prettify-sentence input))
  (let ((response (get-response input)))
    (prn "CASE:      %s" (car response))
    (prn "RESPONSE:  %s" (prettify-sentence response t))))

(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;===================================================================================================
;; CONSTRUCTION ZONE:
;;===================================================================================================


