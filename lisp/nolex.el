;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--destructuring-match)
(require 'aris-funs--lists)
(require 'aris-funs--plists)
(require 'aris-funs--with-gensyms)
(require 'aris-funs--symbolicate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar last-result 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-member-sym-p2 (lst)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Generate a membership predicate fun for LST."
  (lambda (thing &rest _) (and (symbolp thing) (member thing lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-member-sym-p (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Generate a membership predicate fun for LST."
  `(lambda (thing &rest _) (and (symbolp thing) (member thing ,lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-weighted-member-sym-p (lst)
  `(make-member-sym-p (tails ,lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (not (null (member 'a '(a b c)))) returns t)
(confirm that (not (null (member 'b '(a b c)))) returns t)
(confirm that (not (null (member 'c '(a b c)))) returns t)
(confirm that (not (null (member 'd '(a b c)))) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-pick          (lst) `(lambda (&rest _) (pick          ,lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-weighted-pick (lst) `(lambda (&rest _) (weighted-pick ,lst)))
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
(defalias 'was/is?            (make-member-sym-p '(was is)))
(defalias 'result/total?      (make-member-sym-p '(result total)))
(defalias 'am/are?            (make-member-sym-p '(am are)))
(defalias 'a/an?              (make-member-sym-p '(a an)))
(defalias 'a/the?             (make-member-sym-p '(a the)))
(defalias 'a/an/the?          (make-member-sym-p '(a an the)))
(defalias 'had/have?          (make-member-sym-p '(had have)))
(defalias 'do/does?           (make-member-sym-p '(do does)))
(defalias 'a/another/to?      (make-member-sym-p '(a another to)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'do/would?          (make-member-sym-p '(do would)))
(defalias 'pick-do/would?     (make-pick '(do would)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar   *desire-words*     '(need want))
(defalias 'desire?            (make-member-sym-p '(like need want))) ;; includes 'like.
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
(defalias 'us-them?               (make-member-sym-p *us-them*))
(defalias 'plural-subject2?         (make-member-sym-p *us-them*))
(defalias 'pick-plural-subject2     (make-pick *us-them*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar   *modal-words*      '( would
                                should
                                could
                                will
                                can
                                ;; do
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
(defvar   *epistemic-words*  '((2 . know) (2 . believe) (2 . think) (1 . suspect)))
;; (defalias 'epistemic?         (make-member-sym-p (tails *epistemic-words*)))
(defalias 'epistemic?         (make-weighted-member-sym-p  *epistemic-words*))
(defalias 'pick-epistemic     (make-weighted-pick *epistemic-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'pick-possibility
  (make-pick '(do don\'t sometimes\ do always never would wouldn\'t)))
(defalias 'pick-obviousness
  (make-pick '(nil nil nil clearly plainly actually secretly obviously evidently)))
(defalias 'pick-insult-adj
  (make-pick '(brainded stupid silly dumb ridiculous demented deranged assinine idiotic)))
(defalias 'pick-insult-noun
  (make-pick '(idiot moron nincompoop fool imbecile jackass knucklehead nitwit dumbass)))
(defalias 'pick-i-am/you-are (make-pick '(i\ am you\ are)))
(defalias 'pick-certainty    (make-pick '(certain sure convinced)))
(defalias 'pick-maybe-that   (make-pick '(that nil)))
(defalias 'pick-maybe-really (make-pick '(really actually nil nil)))
(defalias 'pick-really       (make-pick '(really actually)))
(defalias 'pick-maybe-not    (make-pick '(never not not nil nil nil)))
(defalias 'pick-probably-not (make-pick '(never not not nil nil nil)))
(defalias 'pick-maybe-actually (make-pick '(actually\, nil nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (not (null (subject? 'i))) returns t)
(confirm that (not (null (subject? 'you))) returns t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun add-ing (sym)
  (symbolicate sym 'ing))
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
      ((assq  val alist) (cdr (assq  val alist)))
      ((rassq val alist) (car (rassq val alist)))
      (t val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-swapper (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(lambda (val var var-alist)
     "Return the swapped word for VAL found in ALIST, or VAL if none."
     (cond
       ((assq  val ,alist) (cdr (assq  val ,alist)))
       ((rassq val ,alist) (car (rassq val ,alist)))
       (t val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun swap-word (var val var-alist)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Return the swapped word for VAL found in *SWAP-WORDS*, or VAL if none."
;;   (cond
;;     ((assq  val *swap-words*) (cdr (assq  val *swap-words*)))
;;     ((rassq val *swap-words*) (car (rassq val *swap-words*)))
;;     (t val)))
(defalias 'swap-word (make-swapper *swap-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (swap-word 'i 'x '((x . i))) returns you)
(confirm that (swap-word 'you 'x '((x . you))) returns i)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun dup-var (var val var-alist)
;;   (cl-loop for n from 1
;;     for new-var = (intern (format "$%d" n))
;;     until (not (assq new-var var-alist))
;;     finally (nconc var-alist (list (cons new-var val))))
;;   nil)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dup-var (val var var-alist)
  ;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Duplicate a VAR in VAR-ALIST with a new name."
  (cl-loop for suffix from 0
    for new-var = (intern (concat (symbol-name var) (make-string suffix ?*)))
    until (not (assq new-var var-alist))
    finally (nconc var-alist (list (cons new-var val))))
  val)
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
                  (kvp (if is-no-var-test (cons '_ nil) (assq var var-alist)))
                  (val   (cdr kvp)))
            (unless kvp (error "missing var %s" var))
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
(defun proc-funs (var-funses var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cl-labels ( (prn2    (&rest args) (when *proc-funs-verbose* (apply #'prn    args)))
               (prndiv2 (&rest args) (when *proc-funs-verbose* (apply #'prndiv nil)))
               (prn-var-alist (&optional (ix ""))
                 (prn2 "VAR-ALIST%s:" ix)
                 (let (lisp-indent-offset)
                   (prn2 "%s" (trim-trailing-whitespace (pp-to-string var-alist))))))
    (dolist (var-funs var-funses)
      (let ( (var     (car var-funs))
             (funs    (cdr var-funs)))
        (when (eq '_! var) (error "illegal var %s" var))
        (let* ( (is-discard (eq '_ var))
                (new-var    (unless is-discard (new-var-name? var)))
                (var        (if new-var new-var var))
                (kvp        (unless new-var (assq var var-alist))))
          (cond
            (is-discard) ;; do nothing.
            ((and new-var kvp)         (error "key %s already taken" var))
            ((and (not new-var) (not kvp)) (error "missing var %s"       var)))
          (cond
            (is-discard
              (setf kvp       (cons var nil))) ; left unattached to VAR-ALIST!
            (new-var
              (setf kvp       (cons var nil)) ; this used to set it to t...
              (setf var-alist (cons kvp var-alist)) ; attach to VAR-ALIST.
              (prndiv2)
              (prn2 "NEW-VAR:   %s" var)))
          (dolist (fun funs)
            (prndiv2)
            (prn2 "var:       %s" var)
            (let ((val (cdr kvp)))
              (prn2 "val:       %s" val)
              (prn2 "fun:       %s" fun)
              (prn-var-alist)
              (let ((res
                      ;; auto-map is probably a bad idea:
                      ;; (if (consp val) ;; don't use listp here!
                      ;; (rmapcar val (lambda (x) (funcall fun x var var-alist)))
                      (funcall fun val var var-alist))) ; )
                (prn2 "funres:    %s" res)
                (setf (cdr kvp) res)))
            (prn-var-alist 2)))))
    (prndiv2)
    (prn2 "DONE PROC FUNS.")
    (prndiv2)
    var-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *proc-funs-verbose* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((*proc-funs-verbose* nil))
  (confirm that (proc-funs
                  '((subj swap-word) (subj-2 swap-word))
                  '((subj . i) (subj-2 . you) (baz . you)))
    returns ((subj . you) (subj-2 . i) (baz . you))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fill-in-missing-rule-keys (rule)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: this also converts RULE from a plist to an alist."
  (unless (member :input: rule)
    (error "malformed INPUT, :input: missing."))
  (unless (member :responses: rule)
    (error "malformed RESPONSE, :responses: missing."))
  (dolist (key (plist-keys rule))
    (unless (memq key *rule-keys*)
      (error "unrecognized rule key %s in %s" key rule)))
  (fill-in-missing-alist-keys *rule-keys* (plist-to-alist rule)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rule-keys* '(:input: :var-tests: :responses:))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (fill-in-missing-rule-keys
    '( :input: ( this is the ,@things)
       :responses:
       ((:response: ( 18 ,persp not really ,certainty if this is ,@things )))))
  returns ( (:var-tests:)
            (:input: this is the (\,@ things))
            (:responses:
              (:response: (18 (\, persp) not really (\, certainty) if this is (\,@ things))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let-rule (rule &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(let-alist (fill-in-missing-rule-keys ,rule) ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let-rule
    '( :input: ( this is the ,@things)
       :responses:
       ((:response: ( 18 ,persp not really ,certainty if this is ,@things ))))
    (list .:input: .:var-tests: .:responses:))
  returns
  ( (this is the (\,@ things))
    nil
    ((:response: (18 (\, persp) not really (\, certainty) if this is (\,@ things))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun select-response (var-alist responses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (prn "s-a-f-r's var-alist: %s" var-alist)
  ;; (prn "s-a-f-r's responses: %s" responses)
  (unless responses
    (error "can't select from empty RESPONSES"))
  (cl-flet ( (prn2    (&rest args) (when *select-response-verbose* (apply #'prn    args)))
             (prndiv2 (&rest args) (when *select-response-verbose* (apply #'prndiv nil))))
    (let ((response (pick responses)))
      ;; (prn "s-a-f-r   picked:    %s" response)
      (let-response response
        (setf var-alist (proc-funs .:var-funs: var-alist))
        (let ((res
                (let (*dm:verbose*) ; shadow!
                  (dm:fill .:response: var-alist))))
          (throw 'result res))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *select-response-verbose* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fill-in-missing-response-keys (response)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: this also converts RESPONSE from a plist to an alist."
  (unless (member :response: response)
    (error "malformed RESPONSE, :response: missing."))
  (dolist (key (plist-keys response))
    (unless (memq key *response-keys*)
      (error "unrecognized response key %s in %s" key response)))
  (fill-in-missing-alist-keys *fillable-response-keys* (plist-to-alist response)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *fillable-response-keys* '(:var-funs:)) ; :response: is not fillable!
(defvar *response-keys*          '(:var-funs: :response:)) ; :response: is not fillable!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (fill-in-missing-response-keys
    '(:response: ( 18 ,persp not really ,certainty if this is ,@things )))
  returns ( (:var-funs:)
            (:response: 18 (\, persp) not really (\, certainty) if this is (\,@ things))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let-response (response &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(let-alist (fill-in-missing-response-keys ,response) ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let-response '(:response: ( 18 ,persp not really ,certainty if this is ,@things ))
    (list .:var-funs: .:response:))
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
            (prn2 "try:       %s" .:input:)
            (if (eq t .:input:)
              (progn
                (prn2 "MATCHED T:   %s" .:input:)
                ;; t matches any input and fills using an empty list:
                (throw 'result (select-response nil .:responses:)))
              (when-let ((var-alist
                           (let ((*dm:verbose* nil)) ; shadow!
                             (dm:match .:input: input))))
                (let ((var-alist (if (eq t var-alist) nil var-alist)))
                  (unless (proc-tests .:var-tests: var-alist) (throw 'continue nil))
                  (prn2 "MATCHED:   %s" .:input:)
                  (throw 'result (select-response var-alist .:responses:)))))))))))
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
(confirm that (punctuation? '!)   returns t)
(confirm that (punctuation? '\?)  returns t)
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
(defvar *prettify-sentence-strip* t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(had/have had/have?) ,(a/an a/an?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         dup-var dup-var)
                          (had/have        swap-word)
                          (subject*        i-to-know/knew)
                          (subject**       swap-word)
                          (things          swap-word))
           :response:   ( 1 ,subject ,subject*  ,subject** ,had/have ,a/an ,@things \!))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(epistemic epistemic?) that ,(subject-2 subject?)
                          ,(modal modal?) ,verb-2 ,(a/an a/an?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (epistemic       pick-epistemic)
                          (maybe-really!   pick-maybe-really)
                          (subject         swap-word)
                          (subject-2       swap-word))
           :response:   ( 2 do ,subject ,maybe-really ,epistemic that ,subject-2 ,modal
                          ,verb-2 ,a/an ,@things \?))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (epistemic       pick-epistemic)
                          (maybe-really!   pick-maybe-really)
                          (subject         swap-word)
                          (subject-2       swap-word))
           :response:   ( 2 do ,subject ,epistemic that ,subject-2 ,maybe-really ,modal
                          ,verb-2 ,a/an ,@things \?))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (epistemic       pick-epistemic)
                          (maybe-really!   pick-maybe-really)
                          (subject         swap-word)
                          (subject-2       swap-word))
           :response:   ( 2 do ,subject ,epistemic that ,subject-2 ,modal ,maybe-really
                          ,verb-2 ,a/an ,@things \?))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?)  ,(am/are am/are?) ,(a/an/the a/an/the?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         dup-var)
                          (iadj!           pick-insult-adj)
                          (subject*        swap-word)
                          (am/are          dup-var)
                          (am/are*         swap-word)
                          (maybe-really!   pick-maybe-really)
                          (obv!            pick-obviousness))
           :response:   ( 3 don\'t be ,iadj \, ,subject* ,am/are* not ,maybe-really ,a/an/the
                          ,@things \, ,subject ,am/are ,obv the ,@things \!))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) would ,(desire desire?) many ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (qty!            pick-qty))
           :response:   ( 4 don\'t ,subject have ,qty ,@things already \?))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) would like ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word))
           :response:   ( 5 why do you think that ,subject would like ,@things \?))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (maybe-really!   pick-maybe-really))
           :response:   ( 5 ,subject ,maybe-really would like ,@things ))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (maybe-really!   pick-maybe-really))
           :response:   ( 5 ,subject would ,maybe-really like ,@things ))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(desire desire?) ,(a/an a/an?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (desire          swap-word)
                          (really!         pick-really))
           :response:   ( 6 do ,subject ,really ,desire ,a/an ,@things \?))))
     ;;==============================================================================================
     ( :input:          ( ,(do/would do/would?) ,(subject subject?) ,(desire desire?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (desire          pick-desire)
                          (maybe-really!   pick-maybe-really)
                          (poss!           pick-possibility))
           :response:   ( 7 ,subject ,poss ,maybe-really ,desire ,@things))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (desire          pick-desire)
                          (maybe-really!   pick-maybe-really)
                          (poss!           pick-possibility))
           :response:   ( 7 ,subject ,maybe-really ,poss ,desire ,@things))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,bar ,baz)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word))
           :response:   ( 8 fine \, ,subject ,bar ,baz \, so what \?))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(modal modal?) ,@verb
                          ,(a/an/the a/an/the?) ,@things)
       :var-tests:      ( (verb (lambda (v _ _) (length< v 3))))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (subject-2!      pick-subject)
                          (epistemic!      pick-epistemic)
                          (maybe-that!     pick-maybe-that)
                          (maybe-really!   pick-maybe-really)
                          (modal-2!        pick-modal))
           :response:   ( 9 ,subject-2 ,epistemic ,maybe-that ,subject
                          ,maybe-really ,modal-2 ,@verb ,a/an/the ,@things))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (modal-2!        pick-modal)
                          (maybe-really!   pick-maybe-really))
           :response:   ( 9 ,subject ,maybe-really ,modal-2 ,@verb ,a/an/the ,@things))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(neg-modal neg-modal?) ,@verb
                          ,(a/an/the a/an/the?) ,@things)
       :var-tests:      ( (verb (lambda (v _ _) (length< v 3))))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (subject-2!      pick-subject)
                          (epistemic!      pick-epistemic)
                          (maybe-that!     pick-maybe-that)
                          (maybe-really!   pick-maybe-really)
                          (neg-modal-2!    pick-neg-modal))
           :response:   ( 9B ,subject-2 ,maybe-really ,epistemic ,maybe-that ,subject
                          ,neg-modal-2 ,@verb ,a/an/the ,@things))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (subject-2!      pick-subject)
                          (epistemic!      pick-epistemic)
                          (maybe-that!     pick-maybe-that)
                          (maybe-really!   pick-maybe-really)
                          (neg-modal-2!    pick-neg-modal))
           :response:   ( 9B ,subject ,maybe-really ,neg-modal-2 ,@verb ,a/an/the ,@things))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(modal modal?) never
                          ,@verb about ,(a/the a/the?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (maybe-really!   pick-maybe-really)
                          (maybe-actually! pick-maybe-actually)
                          (modal           pick-modal)
                          (verb            (lambda (v _ _) (car (last v)))))
           :response:   ( 10A ,maybe-actually ,subject ,maybe-really ,modal
                          ,verb about ,a/the ,@things \!))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (am/are!         (lambda (_ _ vars)
                                             (let-alist vars (if (eq 'i .subject) 'am 'are))))
                          (maybe-really!   pick-maybe-really)
                          (maybe-actually! pick-maybe-actually)
                          (verb            (lambda (v _ _) (add-ing (car (last v))))))
           :response:   ( 10A ,maybe-actually ,subject ,am/are ,maybe-really
                          ,verb about ,a/the ,@things right now \!))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(modal modal?) never ,@verb at
                          ,(a/the a/the?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (maybe-really!   pick-maybe-really)
                          (maybe-actually! pick-maybe-actually)
                          (modal           pick-modal)
                          (verb            (lambda (v _ _) (car (last v)))))
           :response:   ( 10B ,maybe-actually ,subject ,maybe-really ,modal
                          ,verb at ,a/the ,@things \!))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (am/are!         (lambda (_ _ vars)
                                             (let-alist vars (if (eq 'i .subject) 'am 'are))))
                          (maybe-really!   pick-maybe-really)
                          (maybe-actually! pick-maybe-actually)
                          (verb            (lambda (v _ _) (add-ing (car (last v))))))
           :response:   ( 10B ,maybe-actually ,subject ,maybe-really ,am/are
                          ,verb at ,a/the ,@things right now\!))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(modal modal?) never
                          ,@verb ,(a/the a/the?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (maybe-really!   pick-maybe-really)
                          (maybe-actually! pick-maybe-actually)
                          (modal           pick-modal)
                          (verb            (lambda (v _ _) (car (last v)))))
           :response:   ( 10C ,maybe-actually ,subject ,maybe-really ,modal
                          ,verb ,a/the ,@things \!))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (am/are!         (lambda (_ _ vars)
                                             (let-alist vars (if (eq 'i .subject) 'am 'are))))
                          (maybe-really!   pick-maybe-really)
                          (maybe-actually! pick-maybe-actually)
                          (verb            (lambda (v _ _) (add-ing (car (last v))))))
           :response:   ( 10C ,maybe-actually ,subject ,maybe-really ,am/are
                          ,verb ,a/the ,@things right now \!))))
     ;;==============================================================================================
     ( :input:          ( you ,foo ,baz \!)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :response:   ( 11 no \, it is you who ,foo ,baz \!))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(epistemic epistemic?) that ,(subject-2 subject?)
                          ,(modal-plus modal-plus?) never ,verb a ,noun)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (subject-2       swap-word))
           :response:   ( 12 come on \, ,subject can\'t really ,epistemic
                          that ,subject-2 ,modal-plus never ,verb a ,noun \!))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(epistemic epistemic?) that ,(subject-2 subject?)
                          ,(desire desire?) ,(a/an a/an?) ,noun)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (subject-2       swap-word)
                          (epistemic       swap-word)
                          (desire          swap-word))
           :response:   ( 13 after this conversation \, ,subject
                          ,epistemic that ,subject-2
                          ,desire ,a/an ,noun \!))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(desire desire?) to ,verb ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject          swap-word)
                          (subject-2!       pick-subject)
                          (desire           pick-desire)
                          (epistemic!       pick-epistemic))
           :response:   ( 14 ,subject-2 don\'t ,epistemic that ,subject
                          really ,desire to ,verb ,@things))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject          swap-word)
                          (desire           pick-desire))
           :response:   ( 14B ,subject don\'t ,desire to ,verb ,@things))))
     ;;==============================================================================================
     ( :input:          ( ,(plural-subject plural-subject?) are ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (plural-subject  dup-var)
                          (plural-subject* swap-word)
                          (adj!            pick-insult-adj)
                          (noun!           pick-insult-noun)
                          (maybe-really!   pick-maybe-really)
                          (obv!            pick-obviousness))
           :response:   ( 15 You ,adj ,noun \, ,plural-subject are not ,maybe-really the ,@things \,
                          it is ,obv ,plural-subject* who are the ,@things \!))
         ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (plural-subject  dup-var)
                          (plural-subject* swap-word)
                          (adj!            pick-insult-adj)
                          (noun!           pick-insult-noun)
                          (maybe-really!   pick-maybe-really)
                          (obv!            pick-obviousness))
           :response:   ( 15 You ,adj ,noun \, ,plural-subject are ,obv not the ,@things \,
                          it is ,maybe-really ,plural-subject* who are the ,@things \!))))
     ;;==============================================================================================
     ( :input:          ( i wish that you were a ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (adj!            pick-insult-adj)
                          (epistemic!      pick-epistemic)
                          (noun!           pick-insult-noun))
           :response:   ( 16 you ,adj ,noun \, I already ,epistemic
                          that you want a ,@things))))
     ;;==============================================================================================
     ( :input:          ( these are ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (persp!          pick-i-am/you-are)
                          (certainty!      pick-certainty)
                          (probably-not!   pick-probably-not))
           :response:   ( 17 ,persp ,probably-not really ,certainty that these are ,@things ))))
     ;;==============================================================================================
     ( :input:          ( this is the ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (persp!          pick-i-am/you-are)
                          (maybe-really!   pick-maybe-really)
                          (certainty!      pick-certainty))
           :response: ( 18 ,persp not ,maybe-really ,certainty that this is the ,@things ))))
     ;;==============================================================================================
     ( :input:          ( ,(subject subject?) ,(epistemic epistemic?)
                          ,(plural-subject plural-subject?) ,(modal modal?) ,verb
                          ,(us-them us-them?) ,@things)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (subject         swap-word)
                          (plural-subject  swap-word)
                          (modal           pick-any-modal)
                          (maybe-really!   pick-maybe-really)
                          (us-them         swap-word))
           :response:   ( 19 ,plural-subject ,maybe-really ,modal ,verb ,us-them ,@things \!))))
     ;;==============================================================================================
     ( :input:          ( trigger )
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (adj!            pick-insult-adj)
                          (noun!           pick-insult-noun))
           :response:   ( 98 yes \, here we are you ,adj ,noun))))
     ;;==============================================================================================
     ( :input:          ( ,@foo is the same as ,@foo )
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :response:   ( 20 that\'s ,@foo for you))))
     ;;==============================================================================================
     ( :input:          ( add ,(addend integer?) to ,(addend-2 integer?))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (let-alist vars 
                                    (setf last-result (+ .addend .addend-2))))))
           :response:   ( 21 the result is ,sum))))
     ;;==============================================================================================
     ( :input:          ( subtract ,(subtrahend integer?) from ,(subtrahend-2 integer?))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (let-alist vars 
                                    (setf last-result (- .subtrahend-2 .subtrahend))))))
           :response:   ( 21 the result is ,sum))))
     ;;==============================================================================================
     ( :input:          ( multiply ,(left integer?) by ,(right integer?))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (let-alist vars 
                                    (setf last-result (* .left .right))))))
           :response:   ( 21 the result is ,sum))))
     ;;==============================================================================================
     ( :input:          ( divide ,(left integer?) by ,(right integer?))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (let-alist vars 
                                    (setf last-result (/ .right .left))))))
           :response:   ( 21 the result is ,sum))))
     ;;==============================================================================================
     ( :input:          ( add ,(left integer?) to it)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (let-alist vars 
                                    (setf last-result (+ .left last-result))))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( subtract ,(left integer?) from it)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (let-alist vars 
                                    (setf last-result (- last-result .left))))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( multiply it by ,(left integer?))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (let-alist vars 
                                    (setf last-result (* last-result .left))))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( increment it)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ _)
                                  (cl-incf last-result))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( decrement it)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ _)
                                  (cl-decf last-result))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( double it)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ _)
                                  (setf last-result (* last-result 2)))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( triple it)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ _)
                                  (setf last-result (* last-result 3)))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( square it)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (setf last-result (* last-result last-result)))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( halve it)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ _)
                                  (setf last-result (/ last-result 2)))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( divide it by ,(left integer?))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (sum! (lambda (_ _ vars)
                                  (let-alist vars 
                                    (setf last-result (/ last-result .left))))))
           :response:   ( 22 okay \, now we\'ve got ,sum))))
     ;;==============================================================================================
     ( :input:          ( tell me the ,(result/total result/total?) again)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (last-result! (lambda (_ _ _) last-result)))
           :response:   ( 22 the ,result/total was ,last-result))))
     ;;==============================================================================================
     ( :input:          ( tell me the ,(result/total result/total?))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (last-result! (lambda (_ _ _) last-result)))
           :response:   ( 22 the ,result/total is ,last-result))))
     ;;==============================================================================================
     ( :input:          ( what ,(was/is was/is?) the ,(result/total result/total?) again)
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (last-result! (lambda (_ _ _) last-result)))
           :response:   ( 22 the ,result/total ,was/is ,last-result))))
     ;;==============================================================================================
     ( :input:          ( what ,(was/is was/is?) the ,(result/total result/total?))
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (last-result! (lambda (_ _ _) last-result)))
           :response:   ( 22 the ,result/total ,was/is ,last-result))))
     ;;==============================================================================================
     ( :input:          ( what ,(was/is was/is?) it again )
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :var-funs:   ( (last-result! (lambda (_ _ _) last-result)))
           :response:   ( 23 it ,was/is ,last-result))))
     ;;==============================================================================================
     ( :input: t
       :responses:
       ( ;;------------------------------------------------------------------------------------------
         ( :response: (99 i don\'t understand \!))
         ;;------------------------------------------------------------------------------------------
         ( :response: (99 sorry \, i didn\'t hear you properly))
         ;;------------------------------------------------------------------------------------------
         ( :response: (99 what was that \?))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prndiv)
(prn "START:")
(dolist (input
          '( ;; 99
             (i don\'t understand \!)
             (foo bar baz)
             (foo bar baz)
             (foo bar baz)
             (foo bar baz)
             (foo bar baz)
             (foo bar baz quux)
             (foo bar baz quux)
             (foo bar baz quux)
             (foo bar baz quux)
             (foo bar baz quux)

             ;; 1
             (i have an apple tree)
             (you have a dollar)
             (i had a dollar)
             (you had a dollar)
             (you had a coin in your pocket)
             (you have a coin in your pocket)
             (i have a fly on my arm)

             ;; 2
             (i think that i would like a smoke)
             (i think that you would like a smoke)
             (i think that you should have a smoke)
             (i know that i could have a smoke)
             (i believe that you have seen a ghost)
             (i believe that you have seen a ghost)
             (i believe that you have seen a ghost)
             (you believe that i have seen a ghost)
             (you believe that i have seen a ghost)
             (you believe that i have seen a ghost)
             (i think that you should eat a bag of dicks)
             (i think that you should eat a bag of dicks)
             (i think that you should eat a bag of dicks)
             (you know that you must eat a bag of dicks)
             (you know that you must eat a bag of dicks)
             (you know that you must eat a bag of dicks)

             ;; 3
             (you are an asshole)
             (you are a particularly stupid asshole)
             (i am the King of France)
             (i am the King of France)
             (i am the King of France)
             (i am an evil robot in disguise as a human)
             (i am an evil robot in disguise as a human)
             (i am an evil robot in disguise as a human)
             
             ;; 4
             (i would like many hamburgers with cheese and bacon)
             (i would like many hamburgers with cheese and bacon)
             (i would like many hamburgers with cheese and bacon)
             (i would need many orange cats)
             (i would need many orange cats)
             (i would need many orange cats)
             
             ;; 5
             (i would like a hamburger with cheese and bacon)
             (i would like an orange cat)
             (you would like a hamburger with cheese and bacon)

             ;; 6
             (i want a hamburger with cheese and bacon)
             (i need a hamburger with cheese and bacon)
             (i want a hamburger with cheese and bacon)
             (i need a hamburger with cheese and bacon)
             (you want a hamburger with cheese and bacon)
             (you need a hamburger with cheese and bacon)
             (you want a hamburger with cheese and bacon)
             (you need a hamburger with cheese and bacon)

             ;; 7
             (do you like spicy tacos)
             (do you like spicy tacos)
             (do you like spicy tacos)
             (do you like spicy tacos)
             (would you like spicy tacos)
             (would you like spicy tacos)
             (would you like a cigarette)
             (would you like a cigarette)
             (would you like a cigarette)
             (would you like a cigarette)
             (would you like another cigarette)
             (would you like another cigarette)
             (would you like another cigarette)
             (would you like another cigarette)
             
             ;; 8
             (you don\'t understand)
             (dogs eat chickens)
             (you eat chickens)

             ;; 9
             (i could eat a hamburger and some fries)
             (i would climb a tall tree)
             (you would climb a tall tree)
             (you should have a cigarette)

             ;; 10
             (you would never eat a cold hamburger)
             (you should never eat a cold hamburger)
             (you could never eat a cold hamburger)
             (i could never eat a cold hamburger)

             ;; 11
             (you are stupid \!)
             (you suck ass \!)

             ;; 12
             (i know that you have never eaten a hamburger)
             (i suspect that you have never seen a zebra)

             ;; 14
             (i think that you need a drink)
             (you think that i need a drink)
             (i think that i need a drink)
             (you think that you need a drink)

             ;; 14
             (you want to smoke a fat joint)
             (you need to smoke a fat joint)
             (i want to smoke a fat joint)
             (i need to smoke a fat joint)
             (i want to dance in the moonlight)
             (i want to dance in the moonlight)
             (i want to dance in the moonlight)
             (i want to dance in the moonlight)
             (i want to dance in the moonlight)
             (i want to dance in the moonlight)

             ;; 16
             (we are aliens in disguise as humans)
             (we are aliens in disguise as humans)
             (we are aliens in disguise as humans)
             (they are the cutest kittens in the world)
             (they are the cutest kittens in the world)
             (they are the cutest kittens in the world)

             ;; 17
             (i wish that you were a fluffy cat)
             (i wish that you were a duck wearing a tophat)
             (these are the voyages of the starship Enterprise)
             (these are the voyages of the starship Enterprise)
             (these are the voyages of the starship Enterprise)
             (these are the voyages of the starship Enterprise)
             (i know we could beat them at soccer)
             (i know we could beat them at soccer)
             (i know we could beat them at soccer)
             (you know they could beat us any day)
             (you know they could beat us any day)
             (you know they could beat us any day)
             (you know they can find us)
             (you know they can find us)
             (you know they can find us)
             (you know they can find us)
             (you know they can find us)
             (you know they can find us)
             (i know we can find them)
             (i know we can find them)
             (i know we can find them)
             (i know we can find them)
             (i know we can find them)
             (i know we can find them)

             
             ;; 9
             (i could eat a hamburger and some fries)
             (i could eat a hamburger and some fries)
             (i could eat a hamburger and some fries)
             (i could eat a hamburger and some fries)
             (you must conquer the empire of the necromancers)
             (you must conquer the empire of the necromancers)
             (you must conquer the empire of the necromancers)
             (you cannot conquer the empire of the necromancers)
             (you cannot conquer the empire of the necromancers)
             (you cannot conquer the empire of the necromancers)
             (you won\'t conquer the empire of the necromancers)
             (you won\'t conquer the empire of the necromancers)
             (you won\'t conquer the empire of the necromancers)
             (i must devour the souls of the innocent)
             (i must devour the souls of the innocent)
             (i must devour the souls of the innocent)             
             (i must hungrily devour the souls of the innocent)
             (i must hungrily devour the souls of the innocent)
             (i must hungrily devour the souls of the innocent)
             (i must hungrily and angrily devour the souls of the innocent) ;; shouldn't match.
             (this is the worst thing ever)
             (this is the worst thing ever)
             (this is the worst thing ever)
             (would you like food)
             (would you like food)
             (would you like food)
             (would you like food)
             (would you like lots of food)
             (would you like lots of food)
             (would you like lots of food)
             (would you like lots of food)
             (would you want lots of food)
             (would you want lots of food)
             (would you want lots of food)
             (would you want lots of food)
             (quux is the same as quux)
             (sprungy quux is the same as sprungy quux)
             (add 32 to 87)
             (tell me the result again)
             (add 45 to it)
             (tell me the total)
             (subtract 15 from it)
             (subtract 15 from 100)
             (what was it again)
             (multiply 7 by 6)
             (multiply it by 2)
             (divide it by 3)
             (what was the result again)
             (multiply it by 10)
             (what is the total)
             (what was the result)
             (double it)
             (triple it)
             (halve it)
             (square it)
             (increment it)
             (you could never even eat a carrot)
             (you could never even eat a carrot)
             (you would never even look at a carrot)
             (you would never even look at a carrot)
             (you would never even think about a carrot)
             (you would never even think about a carrot)
             (you could never really look at the sun)
             (you could never really look at the sun)
             (i could never even eat a carrot)
             (i could never even eat a carrot)
             (i would never even look at a carrot)
             (i would never even look at a carrot)
             (i would never even think about a carrot)
             (i would never even think about a carrot)
             (i could never really look at the sun)
             (i could never really look at the sun)
             ))
  
  (prndiv)
  (prn "INPUT:     %s" (prettify-sentence input))
  (let ((response (get-response input)))
    (prn "CASE:      %s" (car response))
    (prn "RESPONSE:  %s" (prettify-sentence response t))))

(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(dm:match '(,(subject subject?) ,(modal modal?) never ,@verb about ,(a/the a/the?) ,@things)
;;'(you would never think about a dog))
(add-ing 'look)
