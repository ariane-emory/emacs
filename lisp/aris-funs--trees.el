;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--confirm)
(require 'aris-funs--unsorted)
(require 'aris-funs--basic-preds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform-tree (pred? fun tree)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Replace atoms matching PRED? in TREE with the result of applying FUN to them."
  (let (result tail)
    (while tree
      (let* ((head (pop tree))
              (new-tail
                (list
                  (cond
                    ((cons? head) (transform-tree4 pred? fun head))
                    ((funcall pred? head) (funcall fun head))
                    (t         head)))))
        (if result
          (setcdr tail new-tail)
          (setq result new-tail))
        (setq tail new-tail)))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (transform-tree #'even? #'double '(1 2 3 4 5 6 7 (8 5 9 5 10)))
  returns (1 4 3 8 5 12 7 (16 5 9 5 20)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform-tree2 (pred? fun tree)
  "Replace atoms matching PRED? in TREE with the result of applying FUN to them.

This version is slower than `transform-tree'."
  (if (listp tree)
    (mapr tree
      (lambda (item)
        (if (listp item)
          (transform-tree2 pred? fun item)
          (if (funcall pred? item)
            (funcall fun item)
            item))))
    tree))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (transform-tree2 #'even? #'double '(1 2 3 4 5 6 7 (8 5 9 5 10)))
  returns (1 4 3 8 5 12 7 (16 5 9 5 20)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--trees)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
