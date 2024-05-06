((and unsplice (eq unsplice (car-safe pat-head)))
  (dm::prn "TRY UNSPLICE!")
  (when t ; pat-tail
    (if *dm:enforce-final-position*
      (error "UNSPLICE may only be the final element in PATTERN.")
      (dm::prndiv)
      (dm::prn "UNSPLICING...")
      (with-indentation
        (let (collect)
          (catch 'stop

            (while t
              (dm::prndiv)
              (dm::prn-labeled         collect)
              (dm::prn-pp-labeled-list pattern)
              (dm::prn-pp-labeled-list target)
              
              (let ( (match-targ-tail
                       (let ((*dm:verbose* t))
                         (with-indentation
                           (dm::match1 pat-tail targ-tail
                             dont-care ellipsis unsplice alist))))
                     (match-targ-tail-tail
                       (let ((*dm:verbose* t))
                         (with-indentation
                           (dm::match1 pat-tail (cdr targ-tail)
                             dont-care ellipsis unsplice alist)))))
                (dm::prn-labeled match-targ-tail)
                (dm::prn-labeled match-targ-tail-tail)
                (dm::prndiv ?\-)

                (cond
                  ((null targ-tail)
                    (dm::prn "Out of TARGET, stop.")
                    (throw 'stop nil))
                  ((and (not match-targ-tail) match-targ-tail-tail)
                    (dm::prn "CASE 1: Pushing %s and continuing!" targ-head)
                    (push targ-head collect)
                    (setf targ-head (pop targ-tail)))
                  ((and match-targ-tail (not match-targ-tail-tail))
                    (dm::prn "CASE 2: Pushing %s and stopping!" targ-head)
                    (push targ-head collect)
                    ;;(setf targ-head (pop targ-tail))
                    (dm::prn "THROWING 'stop!")
                    (throw 'stop nil))
                  (t
                    (dm::prn "CASE 3: Nothing else applies, munch %s." targ-head)
                    (push targ-head collect)
                    (setf targ-head (pop targ-tail))
                    (dm::prn "THROWING 'stop!")
                    ;; (throw 'stop nil)
                    )))
              
              (dm::prn-labeled collect)

              (when *dm:debug* (debug 1))
              ) ;; END OF `while'.
            ) ;; END OF `catch'.
          
          (dm::prn-labeled collect "final")
          (setf alist
            (alist-putunique (cadr pat-head) (nreverse collect) alist 'no-match))

          (when *dm:debug* (debug 2))
          ) ; end of `let' COLLECT.
        ) ; end of `with-indentation'.
      ) ; end of `if' *dm:enforce-final-position*.
    ) ; end of `when' pat-tail.
  ) 
