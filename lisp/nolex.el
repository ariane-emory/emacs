;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--alists)
(require 'aris-funs--easy-match)
(require 'aris-funs--lists)
(require 'aris-funs--plists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-member-p (lst)
  `(lambda (x)
     ;; (prn "Check if %s is in %s!" x ,lst)
     (member x ,lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *subject-words*
  '(i you))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'subject? (make-member-p *subject-words*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *swap-words*
  '( (i . you)
     (do . don\'t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rule-keys*
  '( :input-pattern    
     :response-pattern 
     :var-funs
     :var-tests))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ( :input-pattern    (,subj ,bar ,baz)
       :response-pattern (fine \, ,subj ,bar ,baz \, so what \?)
       :var-tests        ((subj subject?))
       :var-funs         ((subj swap-word)))
     ;;-----------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,modal-verb ,verb a ,thing)
       :response-pattern (so just go ,verb a ,thing \!)
       :var-tests        ((subj subject?)))
     ;;-----------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,modal-verb never ,verb a ,thing)
       :response-pattern (,subj ,modal-verb ,verb a ,thing \!)
       :var-funs         ((subj swap-word)))
     ;;-----------------------------------------------------------------------------------
     ( :input-pattern    (you ,foo ,baz \!)
       :response-pattern (no \, it is you who ,foo ,baz \!))
     ;;-----------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,verb that ,subj-2 ,modal-verb never ,verb-2 a ,noun)
       :response-pattern ( come on \, ,subj can\'t really ,verb
                           that ,subj-2 ,modal-verb never ,verb-2 a ,noun \!)
       :var-funs         ((subj swap-word) (subj-2 swap-word)))
     ;;-----------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,verb that ,subj-2 ,modal-verb ,verb-2 a ,noun)
       :response-pattern ( do ,subj really ,verb that ,subj-2 ,modal-verb ,verb-2 a ,noun \?)
       :var-funs         ((subj swap-word) (subj-2 swap-word)))
     ;;-----------------------------------------------------------------------------------
     ( :input-pattern    t
       :response-pattern (i don\'t understand \!))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun repeat-word (word-sym)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (symbolicate word-sym word-sym))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-word (word-sym)
  "Return the swapped word for WORD-SYM found in *SWAP-WORDS*, or WORD-SYM if none."
  (let ((res
          (cond
            ((assoc word-sym  *swap-words*) (cdr (assoc word-sym *swap-words*)))
            ((rassoc word-sym *swap-words*) (car (rassoc word-sym *swap-words*)))
            (t word-sym))))
    ;; (prn "swap %s for %s" sym res)
    res
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-var-tests (var-testses var-alist)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (null var-testses)
    t
    (with-indentation
      (with-gensyms (my-result)
        (catch my-result
          (with-indentation
            (while-let ( (var-tests (pop var-testses))
                         (var   (car var-tests))
                         (tests (cdr var-tests))
                         (value (alist-get var var-alist)))
              (with-indentation
                (dolist (test tests)
                  (unless (funcall test value)
                    (throw my-result nil))))))
          t)))))
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
      (dolist (fun funs)
        (setf (cdr assoc) (funcall fun (cdr assoc))))))
  var-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (run-var-funs '((subj swap-word) (subj-2 swap-word)) '((subj . i) (subj-2 . you) (baz . you)))
  returns ((subj . you) (subj-2 . i) (baz . you)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fill-in-missing-rule-keys (rule)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (fill-in-missing-alist-keys *rule-keys* (plist-to-alist rule)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that
  (fill-in-missing-rule-keys
    '( :input-pattern (,subj ,bar ,baz)
       :response-pattern (fine \, ,subj ,bar ,baz \, so what \?)))
  returns ( (:var-tests)
            (:var-funs)
            (:input-pattern (\, subj) (\, bar) (\, baz))
            (:response-pattern fine \,(\, subj) (\, bar) (\, baz) \, so what \?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let-rule (rule &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  `(let-alist (fill-in-missing-rule-keys ,rule)
     ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-response (input)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Transform INPUT according to *RULES*, returning nil if none match."
  (with-gensyms (result continue)
    (catch result
      (dolist (rule *rules*)
        (let-rule rule
          (with-indentation
            (catch continue
              (if (eq t .:input-pattern)
                                        ; t matches any input and throws it's .:RESPONSE-PATTERN:
                (throw result .:response-pattern)
                (when-let ((var-alist (ap:match .:input-pattern input)))
                  (unless (run-var-tests .:var-tests var-alist) (throw continue nil))
                  (run-var-funs .:var-funs var-alist)
                  (throw result (ap:fill .:response-pattern var-alist)))))))))))
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
             (you suck ass \!)))
  (prndiv)
  (prn "INPUT:     %s" input)
  (prn "RESPONSE:  %s" (get-response input)))
(prndiv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

