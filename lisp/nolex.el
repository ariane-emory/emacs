;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--destructuring-match)
(require 'aris-funs--lists)
(require 'aris-funs--plists)
(require 'aris-funs--with-gensyms)
(require 'aris-funs--symbolicate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *swap-words*
  '( (i . you)
     (am . are)
     (my . your)
     (had . have)
     (think . thought)
     (do . don\'t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-word (var val var-alist)
  "Return the swapped word for VAL found in *SWAP-WORDS*, or VAL if none."
  (cond
    ((assoc  val *swap-words*) (cdr (assoc  val *swap-words*)))
    ((rassoc val *swap-words*) (car (rassoc val *swap-words*)))
    (t val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (swap-word 'x 'i '((x . i))) returns you)
(confirm that (swap-word 'x 'you '((x . you))) returns i)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-member-sym-p (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Generate a membership predicate fun for LST."
  `(lambda (thing) (and (symbolp thing) (member thing ,lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'am/are?   (make-member-sym-p '(am are)))
(defalias 'a/an?     (make-member-sym-p '(a an)))
(defalias 'had/have? (make-member-sym-p '(had have))) 
(defalias 'subject?  (make-member-sym-p '(i you)))
(defalias 'modal?  (make-member-sym-p '(would should could)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (subject? 'i) returns (i you))
(confirm that (subject? 'you) returns (you))
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
(defun i-to-know/knew (var val var-alist)
  (if (eq val 'i)
    'knew\ that
    'knew\ that
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
(defun mapswap (var val var-alist)
  ;; (debug)
  (let ((res (rmapcar val (lambda (v) (swap-word var v var-alist)))))
    ;; (debug)
    res))
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
                           (things    mapswap))
       :response-pattern ( ,$1 ,$2   ,subject ,had/have ,a/an ,@things \! ))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject  ,am/are ,a/an ,thing)
       :var-tests        ( (subject   subject?)
                           (am/are    am/are?)
                           (a/an      a/an?))
       :var-funs         ( (subject   dup-var)
                           (am/are    dup-var)
                           ($1        swap-word)
                           ($2        swap-word))
       :response-pattern ( don\'t be silly \, ,$1 ,$2 not ,a/an ,thing \, ,subject ,am/are
                           the real ,thing \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subject would like many ,@things)
       :var-tests        ((subject subject?))
       :var-funs         ((subject swap-word))
       :response-pattern ( don\'t ,subject have enough ,@things already \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subject would like ,@things)
       :var-tests        ((subject subject?))
       :var-funs         ((subject swap-word))
       :response-pattern ( why do you think that ,subject would like ,@things \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,bar ,baz)
       :var-tests        ((subj subject?))
       :var-funs         ((subj swap-word))
       :response-pattern ( fine \, ,subj ,bar ,baz \, so what \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subject ,modal ,verb a ,@things)
       :var-tests        ((subject subject?) (modal modal?))
       :var-funs         ((things mapswap))
       :response-pattern ( so just go ,verb a ,@things \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,modal never ,verb a ,thing)
       :var-funs         ((subj swap-word))
       :response-pattern (,subj ,modal ,verb a ,thing \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( you ,foo ,baz \!)
       :response-pattern ( no \, it is you who ,foo ,baz \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,verb that ,subj-2 ,modal never ,verb-2 a ,noun)
       :var-tests        ((subj subject?)  (subj-2 subject?))
       :var-funs         ((subj swap-word) (subj-2 swap-word))
       :response-pattern ( come on \, ,subj can\'t really ,verb
                           that ,subj-2 ,modal never ,verb-2 a ,noun \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,verb that ,subj-2 ,modal ,verb-2 a ,noun)
       :var-tests        ((subj subject?)  (subj-2 subject?))
       :var-funs         ((subj swap-word) (subj-2 swap-word))
       :response-pattern ( do ,subj really ,verb that ,subj-2 ,modal ,verb-2 a ,noun \?))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    t
       :response-pattern (i don\'t understand \!))))
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
            (assoc (assoc var var-alist)))
      (unless assoc (error "missing var %s" var))
      (dolist (fun funs)
        (when-let ((res (funcall fun var (cdr assoc) var-alist)))
          (setf (cdr assoc) res))
        ;; (prn "ALIST: %s" var-alist)
        )))
  var-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (run-var-funs '((subj swap-word) (subj-2 swap-word))
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
(prndiv)
(prn "START:")
(dolist (input
          '( (i think that i would like a smoke)
             (i think that you would like a smoke)
             (i think that you should have a smoke)
             (i know that i could have a smoke)
             (i believe that you have seen a ghost)
             (you believe that i have seen a ghost)
             (i suspect that you have never seen a zebra)
             (i know that you have never eaten a hamburger)
             (you would never eat a hamburger)
             (you should never eat a hamburger)
             (foo bar baz)
             (foo bar baz quux)
             (you don\'t understand)
             (i don\'t understand \!)
             (you eat chickens)
             (dogs eat chickens)
             (you are stupid \!)
             (you suck ass \!)
             (you are an asshole)
             (i am a bitch)
             (i have an orange)
             (you have a dollar)
             (i had a dollar)
             (you had a dollar)
             (you had a coin in your pocket)
             (you have a coin in your pocket)
             (i could eat a hamburger and some fries)
             (i have a fly on my arm)
             (i would like a hamburger with cheese and bacon)
             (you would like a hamburger with cheese and bacon)
             (i would like many hamburgers with cheese and bacon)
             ))
  (prndiv)
  (prn "INPUT:     %s" input)
  (prn "RESPONSE:  %s" (get-response input)))
(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





