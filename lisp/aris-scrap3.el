;; -*- lexical-binding: nil; fill-column: 100; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (thing  '(  aa
                   ,bb    ,(bb t)
                   ,@cc  ,@(cc t)
                   #'dd  #'(dd t)
                   'ee    '(ee t)
                   `ff    `(ff t)
                   ))
  (cond
    ((eq '\, (car-safe thing)) (prn "This one is special: %s" thing))
    ((eq '\,@ (car-safe thing)) (prn "This one is very special: %s" thing))
    ((eq 'function (car-safe thing)) (prn "This one is super special: %s" thing))
    ((eq 'quote (car-safe thing)) (prn "This one is kind of special: %s" thing))
    ((eq '\` (car-safe thing)) (prn "This one is extra special: %s" thing))
    ;; (t (prn "%s" thing))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(setq lst '(1 2 3 4 5 6 7 8 9))
(setq al (dm:match '(,x ,@ys) lst))
(setcar (alist-get 'ys al) 999)
(setq lst '(my name is ari))
(setq al (dm:match '(my name is ,@name) lst))
(setcar (alist-get 'name al) 'bob)


(dm::clear-compiled-patterns)

(dm:match
  '(,a ,b (,c . ,d) . ,e)
  (dm::properize-target '\. '(1 2 (3 . 4) . 5)))

(symbol-plist '*dm*)


(dm::unproperize!* '\. '(1 2 3 (4 (a b c (d \. e)) 5 6 \. 7) \. 8))


(let ((pat '(,a ,b . ,c)))
  (prnl 2)
  ;; this doesn't look correct intuitively due to wayward comma but i'm pretty sure it actually is:
  (equal pat (dm::unproperize!* '\. (cl-copy-list (dm::compile-pattern '\. '\,@ '... '_ pat)))))

(let ((pat ;; '(,w ,(x integer? . foo) . ,(y integer? . foo))
        ;; '(,a ,(b integer? . foo) . ,c)
        '(,a ,(b integer? foo) . ,(c integer? foo))
        ))
  (prnl 2)
  (equal pat
    (dm::unproperize!* '\. 
      (cl-copy-list
        (dm::compile-pattern '\. '\,@ '... '_
          (dm::unproperize!* '\. 
            (cl-copy-list
              (dm::compile-pattern '\. '\,@ '... '_ 
                (dm::unproperize!* '\. 
                  (cl-copy-list
                    (dm::compile-pattern '\. '\,@ '... '_ pat)))))))))))

(symbol-plist '*dm*)

(let ((pat ;; '(,w ,(x integer? . foo) . ,(y integer? . foo))
        '(,a ,(b integer? . foo) . ,c)
        ;; '(,a ,(b integer? foo) . ,(c integer? foo))
        ))
  (prnl 2)
  (equal pat
    (dm::unproperize!* '\.
      (dm::compile-pattern '\. '\,@ '... '_ 
        (dm::unproperize!* '\. 
          (dm::compile-pattern '\. '\,@ '... '_ 
            (dm::unproperize!* '\. 
              (dm::compile-pattern '\. '\,@ '... '_  pat))))))))


(dm::clear-compiled-patterns)
(dm::compile-pattern '\. '\,@ '... '_ '(a b . c))
(dm::unproperize!* '\. (dm::compile-pattern '\,@ '... '_ '\.  '(a b . c)))
(dm::compile-pattern '\. '\,@ '... '_  '(a b . c))

(dm:match '(,@things . ,thing) '(one two three . four))


(dm:match '(,@things . four)  '(one two three . four))
(dm:match '(,@things four)  '(one two three four))

(dm::clear-compiled-patterns)
(symbol-plist '*dm*)

(dm:match '(,@things) '(one two three . four))
(dm:match '(,@things . _) '(one two three . four))
(dm:match '(... . ,x) '(one two three . four))

;; this seems okay to permit?
(dm:match '(,@things . (three four))  '(one two three four))

;; make these illegal, probably?
(dm:match '(,@things . ,@zs) '(one two three four))
(dm:match '(,@things . ,@zs) '(one two three . four))

(dm:match '(,@things . ...)  '(one two three four))
(dm:match '(,@things . ...)  '(one two three . four))

;; these seem like they could be maee sense of, but also like a shitty way to write them.
(dm:match '(one two . ,@zs)  '(one two three four))
(dm:match '(one two . ,@zs)  '(one two three . four))

(dm:match '(one two . ...)   '(one two three four))
(dm:match '(one two . ...)   '(one two three . four))

(dm::clear-compiled-patterns)
