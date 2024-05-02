;; -*- lexical-binding: t; fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--destructuring-match)
(require 'aris-funs--lists)
(require 'aris-funs--plists)
(require 'aris-funs--with-gensyms)
(require 'aris-funs--symbolicate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-member-sym-p (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Generate a membership predicate fun for LST."
  `(lambda (thing) (and (symbolp thing) (member thing ,lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-pick (lst)
  (lambda (&rest _)
    (elt lst (random (length lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'pick-qty           (make-pick '(some many enough)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'am/are?            (make-member-sym-p '(am are)))
(defalias 'a/an?              (make-member-sym-p '(a an)))
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
(defvar    *modal-words*     '( would
                                should
                                could
                                will
                                can
                                have))
(defvar    *neg-modal-words* '( wouldn\'t
                                shouldn\'t
                                couldn\'t
                                won\'t
                                can\'t
                                haven\'t))
(defvar    *modal-pairs* (cl-pairlis *modal-words* *neg-modal-words*))
(defvar    *all-modal-words*  (cons 'might (append *modal-words* *neg-modal-words*)))
(defalias 'modal?             (make-member-sym-p *modal-words*))
(defalias 'neg-modal?         (make-member-sym-p *neg-modal-words*))
(defalias 'any-modal?         (make-member-sym-p *any-modal-words*))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (subject? 'i) returns (i you))
(confirm that (subject? 'you) returns (you))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *swap-words*
  '( (i . you)
     (am . are)
     (do . does)
     (my . your)
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
  (lambda (var val var-alist)
    "Return the swapped word for VAL found in ALIST, or VAL if none."
    (cond
      ((assoc  val alist) (cdr (assoc  val alist)))
      ((rassoc val alist) (car (rassoc val alist)))
      (t val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-word (var val var-alist)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dup-var (var val var-alist)
  (cl-loop for n from 1
    for new-var = (intern (format "$%d" n))
    until (not (assoc new-var var-alist))
    finally (nconc var-alist (list (cons new-var val))))
  nil)
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
(defvar *rule-keys*
  '( :input-pattern    
     :response-pattern 
     :var-tests
     :var-funs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ( :input-pattern    ( ,subject  ,had/have ,a/an ,@things)
       :var-tests        ( (subject   subject?)
                           (had/have  had/have?)
                           (a/an      a/an?))
       :var-funs         ( (subject   dup-var dup-var swap-word)
                           (had/have  swap-word)
                           ($2        i-to-know/knew)
                           (things    swap-word))
       :response-pattern ( 1 ,$1 ,$2   ,subject ,had/have ,a/an ,@things \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject  ,epistemic that ,subject-2 ,modal ,verb-2 a ,noun)
       :var-tests        ( (subject   subject?)
                           (epistemic epistemic?)
                           (subject-2 subject?)
                           (modal     modal?))
       :var-funs         ( (subject   swap-word)
                           (subject-2 swap-word))
       :response-pattern ( 2 do ,subject really ,epistemic that ,subject-2 ,modal ,verb-2 a ,noun \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject  ,am/are ,a/an ,@things)
       :var-tests        ( (subject   subject?)
                           (am/are    am/are?)
                           (a/an      a/an?))
       :var-funs         ( (subject   dup-var)
                           (am/are    dup-var)
                           ($1        swap-word)
                           ($2        swap-word))
       :response-pattern ( 3 don\'t be silly \, ,$1 ,$2 not ,a/an ,@things \, ,subject ,am/are
                           the ,@things \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject would ,desire many ,@things)
       :var-tests        ( (subject   subject?)
                           (desire    desire?))
       :var-funs         ( (subject   swap-word)
                           (desire    dup-var)
                           ($1 pick-qty))
       :response-pattern ( 4 don\'t ,subject have ,$1 ,@things already \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject would like ,@things)
       :var-tests        ( (subject   subject?))
       :var-funs         ( (subject   swap-word))
       :response-pattern ( 5 why do you think that ,subject would like ,@things \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject ,desire ,a/an ,@things)
       :var-tests        ( (subject   subject?)
                           (desire    desire?)
                           (a/an      a/an?))
       :var-funs         ( (subject   swap-word)
                           (desire    swap-word))
       :response-pattern ( 6 do ,subject really ,desire ,a/an ,@things \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,do/would ,subject ,desire ,@things)
       :var-tests        ( (do/would  do/would?)
                           (subject   subject?)
                           (desire    desire?))
       :var-funs         ( (subject   swap-word dup-var)
                           (desire    pick-desire)
                           ($1        pick-possibility))
       :response-pattern ( 7 ,subject ,$1 ,desire ,@things))
     

     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject ,bar ,baz)
       :var-tests        ( (subject   subject?))
       :var-funs         ( (subject   swap-word))
       :response-pattern ( 8 fine \, ,subject ,bar ,baz \, so what \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject  ,modal ,verb ,a/an ,@things)
       :var-tests        ( (subject   subject?)
                           (modal     modal?)
                           (a/an      a/an?))
       :var-funs         ( (subject   dup-var dup-var dup-var swap-word)
                           ($1        pick-subject)
                           ($2        pick-epistemic)
                           ($3        pick-any-modal))
       :response-pattern ( 9 ,$1 ,$2 ,subject ,$3 ,verb ,a/an ,@things))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject ,modal never ,verb a ,@things)
       :var-tests        ( (subject   subject?)
                           (modal     modal?))
       :var-funs         ( (subject   swap-word))
       :response-pattern ( 10 ,subject ,modal ,verb a ,@things \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( you ,foo ,baz \!)
       :response-pattern ( 11 no \, it is you who ,foo ,baz \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject ,epistemic that ,subject-2 ,modal never ,verb a ,noun)
       :var-tests        ( (subject   subject?)
                           (epistemic epistemic?)
                           (subject-2 subject?)
                           (modal     modal?))
       :var-funs         ( (subject   swap-word)
                           (subject-2 swap-word))
       :response-pattern ( 12 come on \, ,subject can\'t really ,epistemic
                           that ,subject-2 ,modal never ,verb a ,noun \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject ,epistemic that ,subject-2 ,desire ,a/n ,noun)
       :var-tests        ( (subject   subject?)
                           (epistemic epistemic?)
                           (subject-2 subject?)
                           (desire    desire?)
                           (a/n       a/an?))
       :var-funs         ( (subject   swap-word)
                           (subject-2 swap-word)
                           (epistemic swap-word)
                           (desire    swap-word))
       :response-pattern ( 13 after this conversation \, ,subject
                           ,epistemic that ,subject-2
                           ,desire ,a/n ,noun \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject ,desire to ,verb ,@things)
       :var-tests        ( (subject   subject?)
                           (desire    desire?))
       :var-funs         ( (subject   swap-word dup-var)
                           ($1        pick-subject)
                           (desire    swap-word dup-var)
                           ($2        pick-epistemic))
       :response-pattern ( 14 ,$1 don\'t ,$2 that ,subject really ,desire to ,verb ,@things))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    t
       :response-pattern (99 i don\'t understand \!))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-var-tests (var-testses var-alist)
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
(confirm that (run-var-tests '((subj subject?)) '((subj . i) (bar . think) (baz . you)))
  returns t)
(confirm that (run-var-tests '((subj subject?)) '((subj . x) (bar . think) (baz . you)))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-var-funs (var-funses var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (dolist (var-funs var-funses)
    (let* ( (var   (car var-funs))
            (funs  (cdr var-funs))
            (assoc (assoc var var-alist))
            (val   (cdr-safe assoc)))
      (unless assoc (error "missing var %s" var))
      (dolist (fun funs)
        ;; (prndiv)
        ;; (prn "var: %s" var)
        ;; (prn "val: %s" val)
        ;; (prn "fun: %s" fun)
        (when-let ((res
                     (if (listp val)
                       (compact (rmapcar val (lambda (x) (funcall fun var x var-alist))))
                       (funcall fun var val var-alist))))
          (setf (cdr assoc) res))
        ;; (prn "ALIST: %s" var-alist)
        )))
  var-alist) ; return value is only used by a unit test right now.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (run-var-funs
                '((subj swap-word) (subj-2 swap-word))
                '((subj . i) (subj-2 . you) (baz . you)))
  returns ((subj . you) (subj-2 . i) (baz . you)))
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
  (with-gensyms (result continue)
    (catch result
      (dolist (rule *rules*)
        (let-rule rule
          (catch continue
            (if (eq t .:input-pattern)
              ;; t matches any input and throws it's .:RESPONSE-PATTERN:
              (throw result .:response-pattern)
              (when-let ((var-alist (dm:match .:input-pattern input)))
                (unless (run-var-tests .:var-tests var-alist) (throw continue nil))
                (run-var-funs .:var-funs var-alist)
                (throw result (dm:fill .:response-pattern var-alist))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun converse ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
          (prn "RESPONSE: %s" response))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun punctuation? (sym)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (not (null (member sym '(! \? "!" "." "?")))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (punctuation? '!) returns t)
(confirm that (punctuation? '\?) returns t)
(confirm that (punctuation? 'foo) returns nil)
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
             (i suspect that you have never seen a zebra)

             ;; 3
             (you are an asshole)
             (you are a particularly stupid asshole)
             (i am a bitch)

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
             (i want to dance in the moonlight)))
  
  (prndiv)
  (prn "INPUT:     %s" input)
  (let ((str (wm::capitalize
               (let* ( (res (cdr (get-response input)))
                       (res (if (punctuation? (car (last res)))
                              res
                              (nconc res (list "."))))
                       )
                 (apply #'concat
                   (cons (format "%s" (car res))
                     (rmapcar (cdr res)
                       (lambda (e) (format (if (punctuation? e) "%s" " %s") e) ))))))))
    (prn "RESPONSE:  %s" str)))

(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

