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
;; (defun make-member-sym-p (lst)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Generate a membership predicate fun for LST."
;;   (lambda (thing) (and (symbolp thing) (member thing lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-member-sym-p (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Generate a membership predicate fun for LST."
  `(lambda (thing) (and (symbolp thing) (member thing ,lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (not (null (member 'a '(a b c)))) returns t)
(confirm that (not (null (member 'b '(a b c)))) returns t)
(confirm that (not (null (member 'c '(a b c)))) returns t)
(confirm that (not (null (member 'd '(a b c)))) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-pick (lst)
  `(lambda (&rest _)
     ;; (prn "pick: %s" lst)
     (let ((lst ,lst))
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
                                ;; have
                                must))
(defvar   *modal-plus*        (append *modal-words* '(have haven\'t)))
(defvar   *neg-modal-words*  '( wouldn\'t
                                shouldn\'t
                                couldn\'t
                                won\'t
                                can\'t
                                cannot
                                ;; haven\'t
                                musn\'t))
(defvar   *modal-pairs*       (cl-pairlis *modal-words* *neg-modal-words*))
(defvar   *all-modal-words*   (cons 'might (append *modal-words* *neg-modal-words*)))
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
(make-pick '(do don\'t sometimes\ do always never might would wouldn\'t))
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
     (think . thought)
     (do . don\'t)
     (always . never)
     (should . shouldn\'t)
     (could . couldn\'t)
     (will . won\'t)
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-swapper (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (lambda (var val var-alist)
    "Return the swapped word for VAL found in ALIST, or VAL if none."
    (cond
      ((assoc  val alist) (cdr (assoc  val alist)))
      ((rassoc val alist) (car (rassoc val alist)))
      (t val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-swapper (alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(lambda (var val var-alist)
     "Return the swapped word for VAL found in ALIST, or VAL if none."
     (cond
       ((assoc  val ,alist) (cdr (assoc  val ,alist)))
       ((rassoc val ,alist) (car (rassoc val ,alist)))
       (t val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-word (var val var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return the swapped word for VAL found in *SWAP-WORDS*, or VAL if none."
  (cond
    ((assoc  val *swap-words*) (cdr (assoc  val *swap-words*)))
    ((rassoc val *swap-words*) (car (rassoc val *swap-words*)))
    (t val)))
(defalias 'swap-word (make-swapper *swap-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (swap-word 'x 'i '((x . i))) returns you)
(confirm that (swap-word 'x 'you '((x . you))) returns i)
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
(defun dup-var (var val var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Duplicate a VAR in VAR-ALIST with a new name."
  (cl-loop for suffix from 0
    for new-var = (intern (concat (symbol-name var) (make-string suffix ?*)))
    until (not (assoc new-var var-alist))
    finally (nconc var-alist (list (cons new-var val))))
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (let ((var-alist '((x . i) (x* . j))))
                (dup-var 'x 'i var-alist)
                var-alist)
  returns ((x . i) (x* . j) (x** . i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun repeat-word (var val var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Double a symbol with a hyphen in between the two, foo ⇒ foo-foo."
  ;; (prn "THESE: %s %s %s" var val var-alist)
  (symbolicate- val val))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (repeat-word 'x 'bo '((x . bo))) returns bo-bo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun have-to-know/knew (var val var-alist)
  (if (eq val 'have)
    'know\ that
    'knew\ that))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun i-me (var val var-alist)
  (if (eq val 'i)
    'me
    'i))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun i-you (var val var-alist)
  (if (eq val 'i)
    'you
    'i))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun i-to-know/knew (var val var-alist)
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
                  (assoc (assoc var var-alist)))
            (unless assoc (error "missing var %s" var))
            (dolist (test tests)
              (unless (funcall test (cdr assoc))
                (throw result nil)))))
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
  (dolist (var-funs var-funses)
    (let* ( (var     (car var-funs))
            (funs    (cdr var-funs))
            (new-var (new-var-name? var))
            (var     (if new-var new-var var))
            (assoc   (unless new-var (assoc var var-alist))))
      (cond
        ((and new-var assoc)         (error "key %s already taken" var))
        ((and (not new-var) (not assoc)) (error "missing var %s" var)))
      (when new-var
        (setf assoc (cons var t))
        (setf var-alist (cons assoc var-alist)))
      ;; (prn "NEW-VAR:   %s" var)
      ;; (prn "VAR-ALIST: %s" var-alist)
      (dolist (fun funs)
        ;; (prndiv)
        ;; (prn "var: %s" var)
        (let ((val (cdr assoc)))
          ;; (prn "val: %s" val)
          ;; (prn "fun:  %s" fun)
          (when (consp fun)
            (setf fun (eval fun)))
          ;; (prn "fun2: %s" fun)
          (when-let ((res
                       (if (listp val)
                         (rmapcar val (lambda (x) (funcall fun var x var-alist)))
                         (funcall fun var val var-alist))))
            ;; (prn "funres: %s" res)
            (setf (cdr assoc) res)))
        ;; (prn "ALIST: %s" var-alist)
        )))
  var-alist) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (proc-funs
                '((subj swap-word) (subj-2 swap-word))
                '((subj . i) (subj-2 . you) (baz . you)))
  returns ((subj . you) (subj-2 . i) (baz . you)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rule-keys*
  '( :input-pattern    
     :response-pattern 
     :var-tests
     :var-funs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fill-in-missing-rule-keys (rule)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Note: this also converts RULE from a plist to an alist."
  (fill-in-missing-alist-keys *rule-keys* (plist-to-alist rule)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (fill-in-missing-rule-keys
    '( :input-pattern (,subj ,bar ,baz)
       :response-pattern (fine \, ,subj ,bar ,baz \, so what \?)))
  returns ( (:var-funs)
            (:var-tests)
            (:input-pattern (\, subj) (\, bar) (\, baz))
            (:response-pattern fine \,(\, subj) (\, bar) (\, baz) \, so what \?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let-rule (rule &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(let-alist (fill-in-missing-rule-keys ,rule) ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (let-rule '( :input-pattern    (,subj ,bar ,baz)
               :response-pattern (fine \, ,subj ,bar ,baz \, so what \?)
               :var-tests        ((subj subject?))
               :var-funs         ((subj swap-word)))
    (list .:input-pattern .:response-pattern .:var-tests .:var-funs))
  returns ( ((\, subj) (\, bar) (\, baz))
            (fine \,(\, subj) (\, bar) (\, baz) \, so what \?)
            ((subj subject?))
            ((subj swap-word))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-response (input)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Transform INPUT according to *RULES*, returning nil if none match."
  ;; (prn "get-response INPUT: %s" input)
  (catch 'result
    (dolist (rule *rules*)
      (let-rule rule
        (catch 'continue
          (if (eq t .:input-pattern)
            ;; t matches any input and throws it's .:RESPONSE-PATTERN:
            (throw 'result .:response-pattern)
            (when-let ((var-alist (dm:match .:input-pattern input)))
              (let ((var-alist (if (eq t var-alist) nil var-alist)))
                (unless (proc-tests .:var-tests var-alist) (throw 'continue nil))
                (setf var-alist (proc-funs .:var-funs var-alist))
                (throw 'result (dm:fill .:response-pattern var-alist))))))))))
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
  (let* ( (lst (cl-remove-if #'nil/t? (if drop-first (cdr lst) lst)))
          (str (wm::capitalize
                 (let* ( (res lst)
                         (res (if (punctuation? (car (last res)))
                                res
                                (append res (list ".")))))
                   (apply #'concat
                     (cons (format "%s" (car res))
                       (rmapcar (cdr res)
                         (lambda (e)
                           ;; (prn "this: %s" e)
                           (format (if (punctuation? e) "%s" " %s")
                             (if (eq 'i e) 'I e)
                             )))))))))
    str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ;;==============================================================================================
     ( :input-pattern    ( ,subject  ,had/have ,a/an ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (had/have        had/have?)
                           (a/an            a/an?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         dup-var dup-var)
                           (had/have        swap-word)
                           (subject*        i-to-know/knew)
                           (subject**       swap-word)
                           (things          swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 1 ,subject ,subject*  ,subject** ,had/have ,a/an ,@things \!))
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
       :var-funs         ( (epistemic       pick-epistemic)
                           (subject         swap-word)
                           (subject-2       swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 2 do ,subject really ,epistemic that ,subject-2 ,modal
                           ,verb-2 ,a/an ,@things \?))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject  ,am/are ,a/an/the ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (am/are          am/are?)
                           (a/an/the        a/an/the?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         dup-var)
                           (iadj!           pick-insult-adj)
                           (subject*        swap-word)
                           (am/are          dup-var)
                           (am/are*         swap-word)
                           (obv!            pick-obviousness))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 3 don\'t be ,iadj \, ,subject* ,am/are* not ,a/an/the
                           ,@things \, ,subject ,am/are ,obv the ,@things \!))
     ;;==============================================================================================
     ( ;;--------------------------------------------------------------------------------------------
       :input-pattern    ( ,subject would ,desire many ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (desire          desire?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word)
                           (qty! pick-qty))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 4 don\'t ,subject have ,qty ,@things already \?))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject would like ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 5 why do you think that ,subject would like ,@things \?))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,desire ,a/an ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (desire          desire?)
                           (a/an            a/an?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word)
                           (desire          swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 6 do ,subject really ,desire ,a/an ,@things \?))
     ;;==============================================================================================
     ( :input-pattern    ( ,do/would ,subject ,desire ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (do/would        do/would?)
                           (subject         subject?)
                           (desire          desire?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word)
                           (desire          pick-desire)
                           (poss!           pick-possibility))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 7 ,subject ,poss ,desire ,@things))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,bar ,baz)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 8 fine \, ,subject ,bar ,baz \, so what \?))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,any-modal ,verb ,a/an/the ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (any-modal       any-modal?)
                           (a/an/the        a/an/the?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word)
                           (subject-2!      pick-subject)
                           (epistemic!      pick-epistemic)
                           (maybe-that!     pick-maybe-that)
                           (modal-2!        pick-modal))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 9 ,subject-2 ,epistemic ,maybe-that ,subject
                           ,modal-2 ,verb ,a/an/the ,@things))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,modal never ,verb a ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (modal           modal?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 10 ,subject ,modal ,verb a ,@things \!))
     ;;==============================================================================================
     ( :input-pattern    ( you ,foo ,baz \!)
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 11 no \, it is you who ,foo ,baz \!))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,epistemic that ,subject-2 ,modal-plus never ,verb a ,noun)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (epistemic       epistemic?)
                           (subject-2       subject?)
                           (modal-plus      modal-plus?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word)
                           (subject-2       swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 12 come on \, ,subject can\'t really ,epistemic
                           that ,subject-2 ,modal-plus never ,verb a ,noun \!))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,epistemic that ,subject-2 ,desire ,a/n ,noun)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject         subject?)
                           (epistemic       epistemic?)
                           (subject-2       subject?)
                           (desire          desire?)
                           (a/n             a/an?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word)
                           (subject-2       swap-word)
                           (epistemic       swap-word)
                           (desire          swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 13 after this conversation \, ,subject
                           ,epistemic that ,subject-2
                           ,desire ,a/n ,noun \!))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,desire to ,verb ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (subject          subject?)
                           (desire           desire?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject          swap-word)
                           (subject-2!       pick-subject)
                           (desire           swap-word)
                           (epistemic!       pick-epistemic))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 14 ,subject-2 don\'t ,epistemic that ,subject really ,desire to ,verb ,@things))
     ;;==============================================================================================
     ( :input-pattern    ( ,plural-subject are ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tests        ( (plural-subject  plural-subject?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (plural-subject  dup-var)
                           (plural-subject* swap-word)
                           (adj!            pick-insult-adj)
                           (noun!           pick-insult-noun)
                           (obv!            pick-obviousness))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 15 You ,adj ,noun \,
                           ,plural-subject are not the ,@things \,
                           it is ,obv ,plural-subject* who are
                           the ,@things \!))
     ;;==============================================================================================
     ( :input-pattern    ( i wish that you were a ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (adj!            pick-insult-adj)
                           (epistemic!      pick-epistemic)
                           (noun!           pick-insult-noun))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 16 you ,adj ,noun \, I already ,epistemic
                           that you want a ,@things))
     ;;==============================================================================================
     ( :input-pattern    ( these are ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (persp!          pick-i-am/you-are)
                           (certainty!      pick-certainty))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 17 ,persp not really ,certainty that these are ,@things ))
     ;;==============================================================================================
     ( :input-pattern    ( this is the ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (persp!          pick-i-am/you-are)
                           (certainty!      pick-certainty))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 18 ,persp not really ,certainty if this is ,@things ))
     ;;==============================================================================================
     ( :input-pattern    ( trigger )
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (adj!            pick-insult-adj)
                           (noun!           pick-insult-noun))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 19 yes \, here we are you ,adj ,noun))
     ;;==============================================================================================
     ( :input-pattern    ( ,subject ,epistemic ,plural-subject ,modal ,verb ,them-us ,@things)
       ;;--------------------------------------------------------------------------------------------
       :var-tasts        ( (subject         subject?)
                           (epistemic       epistemic?)
                           (plural-subject  plural-subject?)
                           (modal           modal?)
                           (them-us         them-us?))
       ;;--------------------------------------------------------------------------------------------
       :var-funs         ( (subject         swap-word)
                           (plural-subject  swap-word)
                           ;;(modal (make-pick '(foo bar baz)))
                           (modal           pick-any-modal)
                           (them-us         swap-word))
       ;;--------------------------------------------------------------------------------------------
       :response-pattern ( 20 ,plural-subject ,modal ,verb ,them-us ,@things \!))
     ;;==============================================================================================
     ( :input-pattern    t
       ;;--------------------------------------------------------------------------------------------
       :response-pattern (99 i don\'t understand \!))))
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
             (you believe that i have seen a ghost)
             (i think that you should eat a bag of dicks)
             (you know that you must eat a bag of dicks)

             ;; 3
             (you are an asshole)
             (you are a particularly stupid asshole)
             (i am the King of France)
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
             (this is the worst thing ever)
             (i know we could beat them at soccer)
             (i know we could beat them at soccer)
             (i know we could beat them at soccer)
             (you know they could beat us any day)
             (you know they could beat us any day)
             (you know they could beat us any day)
             (you know they can find us)
             (you know they can find us)
             (you know they can find us)

             ;; 9
             (i could eat a hamburger and some fries)
             (i could eat a hamburger and some fries)
             (i could eat a hamburger and some fries)
             (i could eat a hamburger and some fries)
             (you must conquer the empire of the necromancers)
             (you cannot conquer the empire of the necromancers)
             (you won\'t conquer the empire of the necromancers)
             ))
  
  (prndiv)
  (let ((response (get-response input)))
    (prn "CASE:      %s" (car response))
    (prn "INPUT:     %s" (prettify-sentence input))
    (prn "RESPONSE:  %s" (prettify-sentence response t))))

(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


