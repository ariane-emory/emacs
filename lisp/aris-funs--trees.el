;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--confirm)
(require 'aris-funs--unsorted)
(require 'aris-funs--basic-preds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform-tree (pred? fun tree &optional swap-test-order)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Replace atoms matching PRED? in TREE with the result of applying FUN to them."
  (let (result tail)
    (while tree
      (let* ( (head (pop tree))
              (new-tail
                (list
                  (cond
                    ((and swap-test-order (funcall pred? head))
                      (funcall fun head))
                    ((cons? head)
                      (transform-tree pred? fun head swap-test-order))
                    ((and (not swap-test-order) (funcall pred? head))
                      (funcall fun head))
                    (t head)))))
        (if result
          (setcdr tail new-tail)
          (setq result new-tail))
        (setq tail new-tail)))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (transform-tree #'even? #'double '(1 2 3 4 5 6 7 (8 5 9 5 10)))
  returns (1 4 3 8 5 12 7 (16 5 9 5 20)))
(confirm that
  (transform-tree
    (lambda (x) (equal x '(1 2 3)))
    (lambda (x) '(a b c))
    '(1 2 3 4 5 6 (1 2 3) 7 8 9 10)
    t)
  returns (1 2 3 4 5 6 (a b c) 7 8 9 10))
(confirm that ; do nothing case
  (transform-tree
    (lambda (x) (equal x '(1 2 3)))
    (lambda (x) '(a b c))
    '(1 2 3 4 5 6 (1 2 3) 7 8 9 10)
    nil)
  returns (1 2 3 4 5 6 (1 2 3) 7 8 9 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform-tree2 (pred? fun tree &optional swap-test-order)
  "Replace atoms matching PRED? in TREE with the result of applying FUN to them.

This version is slower than `transform-tree'."
  (if (listp tree)
    (rmapcar tree
      (lambda (item)
        (cond
          ((and swap-test-order (funcall pred? item))
            (funcall fun item))
          ((cons? item)
            (transform-tree2 pred? fun item swap-test-order))
          ((and (not swap-test-order) (funcall pred? item))
            (funcall fun item))
          (t item))))
    tree))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (transform-tree2 #'even? #'double '(1 2 3 4 5 6 7 (8 5 9 5 10)))
  returns (1 4 3 8 5 12 7 (16 5 9 5 20)))
(confirm that
  (transform-tree2
    (lambda (x) (equal x '(1 2 3)))
    (lambda (x) '(a b c))
    '(1 2 3 4 5 6 (1 2 3) 7 8 9 10)
    t)
  returns (1 2 3 4 5 6 (a b c) 7 8 9 10))
(confirm that ; do nothing case
  (transform-tree2
    (lambda (x) (equal x '(1 2 3)))
    (lambda (x) '(a b c))
    '(1 2 3 4 5 6 (1 2 3) 7 8 9 10)
    nil)
  returns (1 2 3 4 5 6 (1 2 3) 7 8 9 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--trees)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
