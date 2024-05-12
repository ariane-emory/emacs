;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (dolist (thing  '(  aa
;;                    ,bb    ,(bb t)
;;                    ,@cc  ,@(cc t)
;;                    #'dd  #'(dd t)
;;                    'ee    '(ee t)
;;                    `ff    `(ff t)
;;                    ))
;;   (cond
;;     ((eq '\, (car-safe thing)) (prn "This one is special: %s" thing))
;;     ((eq '\,@ (car-safe thing)) (prn "This one is very special: %s" thing))
;;     ((eq 'function (car-safe thing)) (prn "This one is super special: %s" thing))
;;     ((eq 'quote (car-safe thing)) (prn "This one is kind of special: %s" thing))
;;     ((eq '\` (car-safe thing)) (prn "This one is extra special: %s" thing))
;;     ;; (t (prn "%s" thing))
;;     ))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ignore!
;;   (db #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;           ( (\. \... _ \,@ ((\,(subject subject?)) (\,(had/have had/have?)) (\,(a/an a/an?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(had/have had/have?)) (\,(a/an a/an?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(epistemic epistemic?)) that (\,(subject-2 subject?)) (\,(modal modal-plus?)) (\, verb-2) (\,(a/an a/an?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(epistemic epistemic?)) that (\,(subject-2 subject?)) (\,(modal modal-plus?)) (\, verb-2) (\,(a/an a/an?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(am/are am/are?)) (\,(a/an/the a/an/the?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(am/are am/are?)) (\,(a/an/the a/an/the?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) would (\,(desire desire?)) (\,(_ many/more?)) (\,@ things) with (\,@ things2)))
;;             ((\,(subject subject?)) would (\,(desire desire?)) (\,(_ many/more?)) (\,@ things) with (\,@ things2))
;;             (\. \... _ \,@ ((\,(subject subject?)) would (\,(desire desire?)) (\,(_ many/more?)) (\,@ things)))
;;             ((\,(subject subject?)) would (\,(desire desire?)) (\,(_ many/more?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) would like (\,@ things)))
;;             ((\,(subject subject?)) would like (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(desire desire?)) (\,(a/an a/an?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(desire desire?)) (\,(a/an a/an?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(do/would do/would?)) (\,(subject subject?)) (\,(desire desire?)) (\,@ things)))
;;             ((\,(do/would do/would?)) (\,(subject subject?)) (\,(desire desire?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\, bar) (\, baz)))
;;             ((\,(subject subject?)) (\, bar) (\, baz))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(modal modal?)) never (\,@ verb) (\,(at/about (memq _ '(at about for with)))) (\,(a/the a/the?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(modal modal?)) never (\,@ verb) (\,(at/about (memq _ '(at about for with)))) (\,(a/the a/the?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(modal modal?)) (\,@ verb) (\,(a/an/the a/an/the?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(modal modal?)) (\,@ verb) (\,(a/an/the a/an/the?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(neg-modal neg-modal?)) (\,@ verb) (\,(a/an/the a/an/the?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(neg-modal neg-modal?)) (\,@ verb) (\,(a/an/the a/an/the?)) (\,@ things))
;;             (\. \... _ \,@ (you (\, foo) (\, baz) !))
;;             (you (\, foo) (\, baz) !)
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(epistemic epistemic?)) that (\,(subject-2 subject?)) (\,(modal-plus modal-plus?)) never (\, verb) a (\, noun)))
;;             ((\,(subject subject?)) (\,(epistemic epistemic?)) that (\,(subject-2 subject?)) (\,(modal-plus modal-plus?)) never (\, verb) a (\, noun))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(epistemic epistemic?)) that (\,(subject-2 subject?)) (\,(desire desire?)) (\,(a/an a/an?)) (\,@ noun)))
;;             ((\,(subject subject?)) (\,(epistemic epistemic?)) that (\,(subject-2 subject?)) (\,(desire desire?)) (\,(a/an a/an?)) (\,@ noun))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(desire desire?)) to (\,@ verb) (\,(a a/an/another?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(desire desire?)) to (\,@ verb) (\,(a a/an/another?)) (\,@ things))
;;             (\. \... _ \,@ ((\,(plural-subject plural-subject?)) are (\,@ things)))
;;             ((\,(plural-subject plural-subject?)) are (\,@ things))
;;             (\. \... _ \,@ (i wish that you were a (\,@ things)))
;;             (i wish that you were a (\,@ things))
;;             (\. \... _ \,@ (these are (\,@ things)))
;;             (these are (\,@ things))
;;             (\. \... _ \,@ (this is the (\,@ things)))
;;             (this is the (\,@ things))
;;             (\. \... _ \,@ ((\,(subject subject?)) (\,(epistemic epistemic?)) (\,(plural-subject plural-subject?)) (\,(modal modal?)) (\, verb) (\,(us-them us-them?)) (\,@ things)))
;;             ((\,(subject subject?)) (\,(epistemic epistemic?)) (\,(plural-subject plural-subject?)) (\,(modal modal?)) (\, verb) (\,(us-them us-them?)) (\,@ things))
;;             (\. \... _ \,@ (trigger))
;;             (trigger)
;;             (\. \... _ \,@ ((\,@ foo) is the same as (\,@ foo)))
;;             ((\,@ foo) is the same as (\,@ foo))
;;             (\. \... _ \,@ (add (\,(left integer?)) to (\,(right integer?))))
;;             (add (\,(left integer?)) to (\,(right integer?)))
;;             (\. \... _ \,@ (subtract (\,(left integer?)) from (\,(right integer?))))
;;             (subtract (\,(left integer?)) from (\,(right integer?)))
;;             (\. \... _ \,@ (multiply (\,(left integer?)) by (\,(right integer?))))
;;             (multiply (\,(left integer?)) by (\,(right integer?)))
;;             (\. \... _ \,@ (divide (\,(left integer?)) by (\,(right integer?))))
;;             (divide (\,(left integer?)) by (\,(right integer?)))
;;             (\. \... _ \,@ (add (\,(left integer?)) to it))
;;             (add (\,(left integer?)) to it)
;;             (\. \... _ \,@ (subtract (\,(left integer?)) from it))
;;             (subtract (\,(left integer?)) from it)
;;             (\. \... _ \,@ (multiply it by (\,(left integer?))))
;;             (multiply it by (\,(left integer?)))
;;             (\. \... _ \,@ (increment it))
;;             (increment it)
;;             (\. \... _ \,@ (decrement it))
;;             (decrement it)
;;             (\. \... _ \,@ (double it))
;;             (double it)
;;             (\. \... _ \,@ (triple it))
;;             (triple it)
;;             (\. \... _ \,@ (square it))
;;             (square it)
;;             (\. \... _ \,@ (halve it))
;;             (halve it)
;;             (\. \... _ \,@ (divide it by (\,(left integer?))))
;;             (divide it by (\,(left integer?)))
;;             (\. \... _ \,@ (tell me the (\,(result/total (memq _ '(result total)))) again))
;;             (tell me the (\,(result/total (memq _ '(result total)))) again)
;;             (\. \... _ \,@ (tell me the (\,(result/total result/total?))))
;;             (tell me the (\,(result/total result/total?)))
;;             (\. \... _ \,@ (what (\,(was/is was/is?)) the (\,(result/total result/total?)) again))
;;             (what (\,(was/is was/is?)) the (\,(result/total result/total?)) again)
;;             (\. \... _ \,@ (what (\,(was/is was/is?)) the (\,(result/total result/total?))))
;;             (what (\,(was/is was/is?)) the (\,(result/total result/total?)))
;;             (\. \... _ \,@ (what (\,(was/is was/is?)) it again))
;;             (what (\,(was/is was/is?)) it again)
;;             (\. \... _ \,@ (\... i \... (\,(feel (memq _ '(feel feeling)))) (\,@ things)))
;;             (\... i \... (\,(feel (memq _ '(feel feeling)))) (\,@ things))
;;             (\. \... _ \,@ (w (\, x) (\, y) (\, z)))
;;             (w (\, x) (\, y) (\, z))
;;             (\. \... _ \,@ (x (\, y) (\, z)))
;;             (x (\, y) (\, z))
;;             (\. \... _ \,@ ((\, a) (\, b) (\, c) !))
;;             ((\, a) (\, b) (\, c) !)
;;             (\. \... _ \,@ ((\, a) (\, b) (\, c)))
;;             ((\, a) (\, b) (\, c))
;;             (\. \... _ \,@ (foo _ (\, baz)))
;;             (foo _ (\, baz))
;;             (\. \... _ \,@ (foo (\, _) (\, baz)))
;;             (foo (\, _) (\, baz))
;;             (\. \... _ \,@ (foo (\,(_)) (\, baz)))
;;             (foo (\,(_)) (\, baz))
;;             (\. \... _ \,@ ((\, x) (\...)))
;;             ((\, x) (\...))
;;             (\. \... _ \,@ ((\, x) \...))
;;             ((\, x) \...)
;;             (\. \... _ \,@ ((\, x) (\, y) ((\, z) 4)))
;;             ((\, x) (\, y) ((\, z) 4))
;;             (\. \... _ \,@ ((\, x) 2 (\...) 3 (\, y)))
;;             ((\, x) 2 (\...) 3 (\, y))
;;             (\. \... _ \,@ (1 ((\, foo) _) 2))
;;             (1 ((\, foo) _) 2)
;;             (\. \... _ \,@ ((\, x) ((\, p) \...) (\, y)))
;;             ((\, x) ((\, p) \...) (\, y))
;;             (\. \... _ \,@ ((\, x) ((\, p)) (\, y)))
;;             ((\, x) ((\, p)) (\, y))
;;             (\. \... _ \,@ ((\, x) (\,@ ys)))
;;             ((\, x) (\,@ ys))
;;             (\. \... _ \,@ (1 2 3))
;;             (1 2 3)
;;             (\. \... _ \,@ ((\, 1) 2 3))
;;             ((\, 1) 2 3)
;;             (\. \... _ \,@ (_ (\, x) \...))
;;             (_ (\, x) \...)
;;             (\. \... _ \,@ (1 2 ((\, x) b \...) 4 (\, y)))
;;             (1 2 ((\, x) b \...) 4 (\, y))
;;             (\. \... _ \,@ (1 2 ((\, x) b \...) 4 (\, y) \...))
;;             (1 2 ((\, x) b \...) 4 (\, y) \...)
;;             (\. \... _ \,@ ((\, x) 2 ((\, p) \...) 3 (\, y)))
;;             ((\, x) 2 ((\, p) \...) 3 (\, y))
;;             (\. \... _ \,@ ((\, v) _ (\, w) ((\, x) (p q) (\,@ ys)) (\, z) \...))
;;             ((\, v) _ (\, w) ((\, x) (p q) (\,@ ys)) (\, z) \...)
;;             (\. \... _ \,@ ((\, x) y (\, x)))
;;             ((\, x) y (\, x))
;;             (\. \... _ \,@ ((\, x) (\,@ x)))
;;             ((\, x) (\,@ x))
;;             (\. \... _ \,@ ((\, x) (\, y) (\, x)))
;;             ((\, x) (\, y) (\, x))
;;             (\. \... _ \,@ ((\, x) 2 3 (\, x)))
;;             ((\, x) 2 3 (\, x))
;;             (\. \... _ \,@ (foo (\, x) (bar (\, x))))
;;             (foo (\, x) (bar (\, x)))
;;             (\. \... _ \,@ ((\,(needle-1 symbol?)) \... ((\, needle-1) (\,@ needles))))
;;             ((\,(needle-1 symbol?)) \... ((\, needle-1) (\,@ needles)))
;;             (\. \... _ \,@ ((\,(needle-1 symbol?)) \... ((\, needle-2) (\,@ needles))))
;;             ((\,(needle-1 symbol?)) \... ((\, needle-2) (\,@ needles)))
;;             (\. \... _ \,@ ((\,(needle-1 symbol?)) ((\, needle-2) (\,@ needles))))
;;             ((\,(needle-1 symbol?)) ((\, needle-2) (\,@ needles)))
;;             (\. \... _ \,@ (foo (\,(bar)) quux))
;;             (foo (\,(bar)) quux)
;;             (\. \... _ \,@ (foo (\,(bar integer? odd?)) quux))
;;             (foo (\,(bar integer? odd?)) quux)
;;             (\. \... _ \,@ (foo (\,(bar integer? odd? (lambda (n) (> n 4)))) quux))
;;             (foo (\,(bar integer? odd? (lambda (n) (> n 4)))) quux)
;;             (\. \... _ \,@ ((\,(x integer?)) (\,(y integer? (lambda (n) (> x n))))))
;;             ((\,(x integer?)) (\,(y integer? (lambda (n) (> x n)))))
;;             (\. \... _ \,@ ((\,(x integer?)) (\,(y integer? (> x _)))))
;;             ((\,(x integer?)) (\,(y integer? (> x _))))
;;             (\. \... _ \,@ ((\,(x integer?)) (foo (\,(y integer? (> _ x))))))
;;             ((\,(x integer?)) (foo (\,(y integer? (> _ x)))))
;;             (\. \... _ \,@ (foo (\,(bar integer? odd?)) (\, bar) quux))
;;             (foo (\,(bar integer? odd?)) (\, bar) quux)
;;             (\. \... _ \,@ (foo (\, bar) (\,(bar integer? odd?)) quux))
;;             (foo (\, bar) (\,(bar integer? odd?)) quux)
;;             (\. \... _ \,@ (foo (\,(bar integer?)) (\,(bar odd?)) quux))
;;             (foo (\,(bar integer?)) (\,(bar odd?)) quux)
;;             (\. \... _ \,@ (\... (\,(bar integer? (lambda (n) (> n 4)))) \...))
;;             (\... (\,(bar integer? (lambda (n) (> n 4)))) \...)
;;             (\. \... _ \,@ (\... (\,(_ integer? (lambda (n) (> n 4)))) \...))
;;             (\... (\,(_ integer? (lambda (n) (> n 4)))) \...)
;;             (\. \... _ \,@ (\... (\,(a integer? (lambda (n) (> n 4)))) \...))
;;             (\... (\,(a integer? (lambda (n) (> n 4)))) \...)
;;             (\. \... _ \,@ (\... (\,(_ (= _ 8))) \... (\,(x integerp cl-oddp)) \...))
;;             (\... (\,(_ (= _ 8))) \... (\,(x integerp cl-oddp)) \...)
;;             (\. \... _ \,@ (\... (\,(_ integer? (> _ (* 2 2)))) (\,(_ integer? (> _ (+ 4 2)))) (\,(x integer? (> _ (+ 4 2)))) \...))
;;             (\... (\,(_ integer? (> _ (* 2 2)))) (\,(_ integer? (> _ (+ 4 2)))) (\,(x integer? (> _ (+ 4 2)))) \...)
;;             (\. \... _ \,@ (\... (\,(sym symbolp)) \... (\,(_ integer? odd? (> _ 4))) \... (\,(_ integer? odd?)) \... (\,(x integer? odd?)) \...))
;;             (\... (\,(sym symbolp)) \... (\,(_ integer? odd? (> _ 4))) \... (\,(_ integer? odd?)) \... (\,(x integer? odd?)) \...)
;;             (\. \... _ \,@ (\... (\,(_ integer? odd? (> _ 4))) \... (\,(_ integer? odd?)) \... (\,(x integer? odd?)) \...))
;;             (\... (\,(_ integer? odd? (> _ 4))) \... (\,(_ integer? odd?)) \... (\,(x integer? odd?)) \...)
;;             (\. \... _ \,@ (\... (\,(_ integer? (lambda (n) (> n 4)))) (\,(x integer? (lambda (n) (> n 4)))) (\,(_ integer? (lambda (n) (> n 4)))) \...))
;;             (\... (\,(_ integer? (lambda (n) (> n 4)))) (\,(x integer? (lambda (n) (> n 4)))) (\,(_ integer? (lambda (n) (> n 4)))) \...)
;;             (\. \... _ \,@ (\... (\,(a integer? (lambda (n) (> n 4)))) \... (\,(b integer? (lambda (n) (> n 4)))) \...))
;;             (\... (\,(a integer? (lambda (n) (> n 4)))) \... (\,(b integer? (lambda (n) (> n 4)))) \...)
;;             (\. \... _ \,@ (\... (\,(x integer? (lambda (n) (> n 4)))) \... (\,(y integer? (lambda (n) (> n 4)))) \...))
;;             (\... (\,(x integer? (lambda (n) (> n 4)))) \... (\,(y integer? (lambda (n) (> n 4)))) \...)
;;             (\. \... _ \,@ (\... (\,(x integer? (lambda (n) (> n 4)))) (\,(y integer? (lambda (n) (> n 4)))) \...))
;;             (\... (\,(x integer? (lambda (n) (> n 4)))) (\,(y integer? (lambda (n) (> n 4)))) \...)
;;             (\. \... _ \,@ (\... (\,(_ integer? odd? (> _ 4))) \... (\,(_ integer? odd? (> _ 4))) \... (\,(_ integer? odd? (> 4))) \... (\,(needle-1 symbol?)) \... (_ (\,@ needle-2)) _))
;;             (\... (\,(_ integer? odd? (> _ 4))) \... (\,(_ integer? odd? (> _ 4))) \... (\,(_ integer? odd? (> 4))) \... (\,(needle-1 symbol?)) \... (_ (\,@ needle-2)) _)
;;             (\. \... _ \,@ ((\, x) (\,@ ys) foo))
;;             ((\, x) (\,@ ys) foo)
;;             (\. \... _ \,@ ((\, x) \... (\, z)))
;;             ((\, x) \... (\, z))
;;             (\. \... _ \,@ ((\,(x integer?)) \,(y integer?)))
;;             ((\,(x integer?)) \. (\,(y integer?)))
;;             (\. \... _ \,@ ((\,@ things) \, thing))
;;             ((\,@ things) \. (\, thing))
;;             (\. \... _ \,@ ((\,@ things) . four))
;;             ((\,@ things) \. four)
;;             (\. \... _ \,@ ((\,@ things) \, four))
;;             ((\,@ things) \. (\, four))
;;             (\. \... _ \,@ ((\,@ things)))
;;             ((\,@ things))
;;             (\. \... _ \,@ ((\,@ things) . _))
;;             ((\,@ things) \. _)
;;             (\. \... _ \,@ (\... \, x))
;;             (\... \. (\, x))
;;             (\. \... _ \,@ ((\,@ things) three (\, four)))
;;             ((\,@ things) three (\, four))
;;             (\. \... _ \,@ ((\,@ as) (\,@ bs) (\,@ cs)))
;;             ((\,@ as) (\,@ bs) (\,@ cs))
;;             (\. \... _ \,@ ((\,@ as) (\,@ bs) (\,@ cs) foo))
;;             ((\,@ as) (\,@ bs) (\,@ cs) foo)
;;             (\. \... _ \,@ ((\, x) (\,@ ys) (\,@ zs) foo))
;;             ((\, x) (\,@ ys) (\,@ zs) foo)
;;             (\. \... _ \,@ ((\, w) (\,@ xs) (\,@ ys) foo (\,@ zs)))
;;             ((\, w) (\,@ xs) (\,@ ys) foo (\,@ zs))
;;             (\. \... _ \,@ ((\, w) (\,@ xs) foo (\,@ ys) (\,@ zs)))
;;             ((\, w) (\,@ xs) foo (\,@ ys) (\,@ zs))
;;             (\. \... _ \,@ ((\, x) (\,@ ys) (\,@ zs)))
;;             ((\, x) (\,@ ys) (\,@ zs))
;;             (\. \... _ \,@ (xx (\,@ xxs) xx))
;;             (xx (\,@ xxs) xx)
;;             (\. \... _ \,@ (\... the (\, x) \...))
;;             (\... the (\, x) \...)
;;             (\. \... _ \,@ ((\, foo) \... bar _ \... (\, baz)))
;;             ((\, foo) \... bar _ \... (\, baz))
;;             (\. \... _ \,@ ((\, w) (\,@ xs) foo (\,@ ys) bar (\,@ zs)))
;;             ((\, w) (\,@ xs) foo (\,@ ys) bar (\,@ zs))
;;             (\. \... _ \,@ (one (this that) ((\, two) three ((\, four) (\, five)) (\, six))))
;;             (one (this that) ((\, two) three ((\, four) (\, five)) (\, six)))
;;             (\. \... _ \,@ (i (\, modal-verb) (\, verb) a (\, thing)))
;;             (i (\, modal-verb) (\, verb) a (\, thing))
;;             (\. \... _ \,@ (i (\, verb) that (\, noun) (\, con) (\, thing)))
;;             (i (\, verb) that (\, noun) (\, con) (\, thing))
;;             (\. \... _ \,@ ((\, a) (\, b) ((\, c) (\, d) ((\, f) (\, g)))))
;;             ((\, a) (\, b) ((\, c) (\, d) ((\, f) (\, g))))
;;             (\. \... _ \,@ ((\, form) (\, name) ((\,@ args)) (\,@ body)))
;;             ((\, form) (\, name) ((\,@ args)) (\,@ body))
;;             (\. \... _ \,@ (a (\, b) ((\, c) (\, d))))
;;             (a (\, b) ((\, c) (\, d)))
;;             (\. \... _ \,@ (a (\, b) ((\, c) (\, d) ((\, e) (\,@ fs)))))
;;             (a (\, b) ((\, c) (\, d) ((\, e) (\,@ fs))))
;;             (\. \... _ \,@ ((\, form) (\, name) ((\, arg)) (\,@ body)))
;;             ((\, form) (\, name) ((\, arg)) (\,@ body))
;;             ))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (let ((x 88) (_ 99)) (< x _))

;; (dm:match '(,(x integer?) ,(y integer? (< x _))) '(7 9))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro walk* (lst &rest body)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Walk a possibly improper list."
;;   `(let ((pos ,lst))
;;      (while pos
;;        ,@body
;;        (setf pos (if (atom pos) nil (cdr pos))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro walk2* (spec &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "A version of `dolist` that also handles improper lists."
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 3 (length spec) 4)
    (signal 'wrong-number-of-arguments (list '(3 . 4) (length spec))))
  (let* ( (var         (car spec))
          (pos         (car (cdr spec)))
          (lst         (car (cdr (cdr spec))))
          (var-is-cons (symbolicate var "-is-cons"))
          (pos-is-cons (symbolicate pos "-is-cons")))
    `(let ((,pos ,lst))
       (while ,pos
         (let* ((,var (if (consp pos) (car ,pos) ,pos)))
           ,@body
           (setf ,pos (if (consp pos) (cdr ,pos) nil))))
       ,@(cdr (cdr (cdr spec))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun fun (expr)
  (walk2* (thing pos expr)
    (prndiv)
    (prn "pos:   %s" pos)
    (prn "thing: %s" thing)
    ;; (debug)
    (when (consp thing) (with-indentation (fun thing)))))


;; (fun expr)
(fun '(1 2 3 (4 5) . 6))

(fun '(,(x integer?) ,(y integer? (< x _ . 333))))
