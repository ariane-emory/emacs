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
     :var-preds))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *rules*
  '( ( :input-pattern    (,subj ,bar ,baz)
       :response-pattern (fine \, ,subj ,bar ,baz \, so what \?)
       :var-preds        ((subj subject?)))
     ;;-----------------------------------------------------------------------------------
     ( :input-pattern    (,subj ,modal-verb ,verb a ,thing)
       :response-pattern (so just go ,verb a ,thing \!)
       :var-preds        ((subj subject?)))
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
(defun get-response (input)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Transform INPUT according to *RULES*, returning nil if none match."
  (with-gensyms (result continue)
    (catch result
      (dolist (rule *rules*)
        ;; (prn "ALIST:  %s" (plist-to-alist rule))
        ;; (prn "FILLED: %s" (fill-in-missing-alist-keys *rule-keys* (plist-to-alist rule)))
        (catch continue
          (with-indentation
            (let ( ;; (input-pattern    (plist-get rule :input-pattern))
                   ;; (response-pattern (plist-get rule :response-pattern))
                   ;; (var-preds        (plist-get rule :var-preds))
                   ;; (var-funs         (plist-get rule :var-funs))
                   )
              (let-alist (fill-in-missing-alist-keys *rule-keys* (plist-to-alist rule))
                (if (eq t .:input-pattern)
                  (throw result .:response-pattern) ; t matches any input.
                  (when-let ((var-alist (ap:match .:input-pattern input)))
                    (when .:var-preds
                      (let ((tests-result (run-var-tests var-alist .:var-preds)))
                        (unless tests-result (throw continue nil))))
                    (run-var-funs var-alist .:var-funs)
                    (throw result (ap:fill .:response-pattern var-alist))))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-var-tests (var-alist var-testses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (with-indentation
    (with-gensyms (my-result)
      (catch my-result
        (dolist (var-tests var-testses)
          (with-indentation
            (let* ( (var   (car var-tests))
                    (value (alist-get var var-alist))
                    (tests (cdr var-tests)))
              (with-indentation
                (dolist (test tests)
                  (let ((test-result (not (null (funcall test value)))))
                    (unless test-result
                      (throw my-result nil))))))))
        t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (run-var-tests '((subj . i) (bar . think) (baz . you)) '((subj subject?)))
  returns t)
(confirm that (run-var-tests '((subj . x) (bar . think) (baz . you)) '((subj subject?)))
  returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-var-funs (var-alist var-funses)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (with-indentation
    (dolist (var-funs var-funses)
      (with-indentation
        (let* ( (var   (car var-funs))
                (value (alist-get var var-alist))
                (funs (cdr var-funs)))
          (with-indentation
            (dolist (fun funs)
              (let ((fun-result (funcall fun value)))
                (alist-put! var var-alist fun-result)))))))))
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




(fill-in-missing-alist-keys *rule-keys* (plist-to-alist (car *rules*)))
