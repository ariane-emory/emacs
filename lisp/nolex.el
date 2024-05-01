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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (subject? 'i) returns (i you))
(confirm that (subject? 'you) returns (you))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *swap-words*
  '( (i . you)
     (am . are)
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
(defun dup-var (var val var-alist)
  (let* ( (assoc (assoc var var-alist))
          (new (cons (symbolicate (car assoc) '*) (cdr assoc))))
    (nconc var-alist (list new)))
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dup-var2 (var val var-alist)
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
(defvar *rule-keys*
  '( :input-pattern    
     :response-pattern 
     :var-tests
     :var-funs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun have-to-know/knew (var val var-alist)
  (if (eq val 'have) 'know\ that 'knew))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ( :input-pattern    ( ,subject  ,had/have ,a/an ,thing)
       :var-tests        ( (subject   subject?)
                           (had/have  had/have?)
                           (a/an      a/an?))
       :var-funs         ( (subject   dup-var2 swap-word)
                           (had/have  dup-var2)
                           ($2 have-to-know/knew)
                           )
       :response-pattern (  ,$1 ,$2  ,subject ,had/have ,a/an ,thing ))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( ,subject  ,am/are ,a/an ,thing)
       :var-tests        ( (subject   subject?)
                           (am/are    am/are?)
                           (a/an      a/an?))
       :response-pattern (  don\'t be ridiculous \, ,subject ,am/are the real ,thing \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,bar ,baz)
       :response-pattern ( fine \, ,subj ,bar ,baz \, so what \?)
       :var-tests        ((subj subject?))
       :var-funs         ((subj swap-word)))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,modal-verb ,verb a ,thing)
       :response-pattern ( so just go ,verb a ,thing \!)
       :var-tests        ((subj subject?)))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,modal-verb never ,verb a ,thing)
       :response-pattern (,subj ,modal-verb ,verb a ,thing \!)
       :var-funs         ((subj swap-word)))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    ( you ,foo ,baz \!)
       :response-pattern ( no \, it is you who ,foo ,baz \!))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,verb that ,subj-2 ,modal-verb never ,verb-2 a ,noun)
       :response-pattern ( come on \, ,subj can\'t really ,verb
                           that ,subj-2 ,modal-verb never ,verb-2 a ,noun \!)
       :var-tests        ((subj subject?)  (subj-2 subject?))
       :var-funs         ((subj swap-word) (subj-2 swap-word)))
     ;;----------------------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,verb that ,subj-2 ,modal-verb ,verb-2 a ,noun)
       :response-pattern ( do ,subj really ,verb that ,subj-2 ,modal-verb ,verb-2 a ,noun \?)
       :var-funs         ((subj swap-word) (subj-2 swap-word)))
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
        (prn "ALIST: %s" var-alist)
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
             (i could eat a hamburger)
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
             (i had a dollar)))
  (prndiv)
  (prn "INPUT:     %s" input)
  (prn "RESPONSE:  %s" (get-response input)))
(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


