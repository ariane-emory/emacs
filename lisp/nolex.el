(defun foo (x y)
  (list x y))

(cdr (symbol-function 'foo))  ;; ((x y) (list x y))
(cddr (symbol-function 'foo)) ;; ((list x y))

(fset 'poop (cons 'lambda (cdr (symbol-function 'foo))))

(poop 'd 'e)


(cl-defun foo (xxx (yyy zzz))
  (list xxx yyy zzz))

(defun foo
  (xxx &rest --cl-rest--)
  "\n\n(fn XXX (YYY ZZZ))"
  (let*
    ( (--cl-rest--
        (if (= (length --cl-rest--) 1)
          (car-safe --cl-rest--)
          (signal 'wrong-number-of-arguments (list 'foo (length --cl-rest--)))))
      (yyy
        (if (= (length --cl-rest--) 2)
          (pop --cl-rest--)
          (signal 'wrong-number-of-arguments (list 'foo (length --cl-rest--)))))
      (zzz (car-safe --cl-rest--)))
    (cl-block foo
      (list xxx yyy zzz))))


(setq x 8)

(foo 2 (list 3 x))
