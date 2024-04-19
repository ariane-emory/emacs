;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General purpose list-manipulation functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dash)
(require 'aris-funs--aliases)
(require 'aris-funs--confirm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun depth (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the depth of a nested list structure."
  (unless (list? lst) (error "LST must be a list"))
  (let ( (stack (list (cons lst 1))) ; Stack with initial list and depth of 1
         (max-depth 0))
    (while stack
      (let* ( (current (pop stack))
              (current-list (car current))
              (current-depth (cdr current)))
        (if (> current-depth max-depth)
          (setq max-depth current-depth))
        (mapc (lambda (item)
                (when (list? item)
                  (push (cons item (1+ current-depth)) stack)))
          current-list)))
    max-depth))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (depth '(1 2 3 (4 5 (6 7 8) 9) 10)) returns 3)
(confirm that (depth nil) returns 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compact (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Filter nil items from LST."
  (unless (list? lst) (error "LST must be a list")) 
  (while (and lst (nil? (car lst)))
    (pop lst)) ;; (setq lst (cdr lst)))
  (when lst
    (let* ((result (list (pop lst)))
            (tail result))
      (while lst
        (let ((head (pop lst)))
          (unless (nil? head)
            (setq tail (rplacd! tail (list head))))))
      result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (compact '(1 nil 2 nil 3 nil 4 nil 5 nil)) returns (1 2 3 4 5))
(confirm that (compact '(nil)) returns nil)
(confirm that (compact nil) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter (pred? lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a list containing those members of lst satisfying pred?."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (let (result tail)
    (while lst
      (let ((head (pop lst)))
        (if (funcall pred? head)
          (let ((new-tail (list head)))
            (if tail
              (progn
                (rplacd! tail new-tail)
                (setq   tail new-tail))
              (setq
                result new-tail
                tail   result))))))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (filter 'even? '(1 2 3 4 5 6 7 8 9 10)) returns (2 4 6 8 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-list (lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Take a shallow copy of LST."
  (unless (list? lst) (error "LST must be a list"))
  (when lst
    (let* ( (result (list (pop lst)))
            (tail result))
      (while lst
        (let ((new-tail (list (pop lst))))
          (rplacd! tail new-tail)
          (setq tail new-tail)))
      result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (copy-list '(1 2 3 4 5)) returns (1 2 3 4 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intercalate (intercalated lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Intercalate INTERCALATED between items in LST."
  (unless (list? lst) (error "LST must be a list"))
  (when lst
    (let* ( (result (list (car lst)))
            (tail   result))
      (setq lst (cdr lst))
      (while lst
        (let* ( (head     (pop lst))
                (new-tail (list intercalated head)))
          (rplacd! tail new-tail)
          (setq tail (cdr new-tail))))
      result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (intercalate 'x '(1 2 3 4 5)) returns (1 x 2 x 3 x 4 x 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-list (pred? lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Destructivly split LST into two sublists:
1. The longest initial sublist of elements satisfying PRED?
2. The rest of the elements."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (when lst
    (let ( prev
           (current lst))
      (while (and current (funcall pred? (car current)))
        (setq
          prev    current
          current (cdr current)))
      (if prev
        (progn
          (rplacd! prev nil)
          (list lst current))
        (list nil lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (split-list 'even? '(2 4 6 7 8 10)) returns ((2 4 6) (7 8 10)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all (pred? lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when all elems in LST? are PRED?"
  (unless (fun? pred?) (error "PRED? must be a function"))
  (while (and lst (funcall pred? (car lst)))
    (pop lst))
  (nil? lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (all (lambda (x) (even? x)) '(2 4 6 8 10)) returns t)
(confirm that (all (lambda (x) (even? x)) '(2 4 6 7 8 10)) returns nil)
(confirm that (all 'even? '(2 4 6 8 10)) returns t)
(confirm that (all 'even? '(2 4 6 7 8 10)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun any (pred? lst)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "t when any elem in LST? is PRED?."
  (unless (fun? pred?) (error "PRED? must be a function"))
  (let (result)
    (while (and lst (not result))
      (setq result (funcall pred? (pop lst))))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (any 'even? '(1 3 5 7 9 10)) returns t)
(confirm that (any 'even? '(1 3 5 7 9 11)) returns nil)
(confirm that (any (lambda (x) (even? x)) '(1 3 5 7 9 10)) returns t)
(confirm that (any (lambda (x) (even? x)) '(1 3 5 7 9 11)) returns nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun heads (lsts)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a list of the heads of the lists in LSTS."
  (unless (list? lsts)     (error "LSTS must be a list of lists"))
  (unless (all 'list? lsts) (error "LSTS must be a list of lists"))
  (let* ( (result (list (car (pop lsts))))
          (tail   result))
    (while lsts
      (let ((new-tail (list (car (pop lsts)))))
        (setq tail (rplacd! tail new-tail))))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (heads '((1 2 3) (4 5 6) (7 8 9))) returns (1 4 7))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tails (lsts)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a list of the tails of the lists in LSTS."
  (unless (list? lsts)     (error "LSTS must be a list of lists"))
  (unless (all 'list? lsts) (error "LSTS must be a list of lists"))
  (let* ((result (list (cdr (pop lsts))))
          (tail   result))
    (while lsts
      (let ((new-tail (list (cdr (pop lsts)))))
        (setq tail (rplacd! tail new-tail))))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (tails '((1 2 3) (4 5 6) (7 8 9))) returns ((2 3) (5 6) (8 9)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (retrieving by position):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun first    (lst)                    (car lst))
(defun second   (lst)                   (cadr lst))
(defun third    (lst)                  (caddr lst))
(defun fourth   (lst)                 (cadddr lst))
(defun fifth    (lst)            (car (cddddr lst)))
(defun sixth    (lst)           (cadr (cddddr lst)))
(defun seventh  (lst)          (caddr (cddddr lst)))
(defun eighth   (lst)         (cadddr (cddddr lst)))
(defun ninth    (lst)    (car (cddddr (cddddr lst))))
(defun tenth    (lst)   (cadr (cddddr (cddddr lst))))
(defun eleventh (lst)  (caddr (cddddr (cddddr lst))))
(defun twelfth  (lst) (cadddr (cddddr (cddddr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (first '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 1)
(confirm that (second '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 2)
(confirm that (third '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 3)
(confirm that (fourth '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 4)
(confirm that (fifth '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 5)
(confirm that (sixth '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 6)
(confirm that (seventh '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 7)
(confirm that (eighth '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 8)
(confirm that (ninth '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 9)
(confirm that (tenth '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 10)
(confirm that (eleventh '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 11)
(confirm that (twelfth '(1 2 3 4 5 6 7 8 9 10 11 12)) returns 12)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lst '(1 2 3 4 5 6 7 8 9 10 11 12))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (car Car)))
(defun cadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (car Cdr)))
(defun cdar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdr Car)))
(defun cddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdr Cdr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caar Car)))
(defun caadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caar Cdr)))
(defun cadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cadr Car)))
(defun caddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cadr Cdr)))
(defun cdaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdar Car)))
(defun cdadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdar Cdr)))
(defun cddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cddr Car)))
(defun cdddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cddr Cdr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caaaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caaar Car)))
(defun caaadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caaar Cdr)))
(defun caadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caadr Car)))
(defun caaddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caadr Cdr)))
(defun cadaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cadar Car)))
(defun cadadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cadar Cdr)))
(defun caddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (caddr Car)))
(defun cadddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (caddr Cdr)))
(defun cdaaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdaar Car)))
(defun cdaadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdaar Cdr)))
(defun cdadar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdadr Car)))
(defun cdaddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdadr Cdr)))
(defun cddaar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cddar Car)))
(defun cddadr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cddar Cdr)))
(defun cdddar-safe (lst)
  (when-let ((Car (car-safe lst)))
    (cdddr Car)))
(defun cddddr-safe (lst)
  (when-let ((Cdr (cdr-safe lst)))
    (cdddr Cdr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caaaaar (lst) (car (caaaar lst)))
(defun caaaadr (lst) (car (caaadr lst)))
(defun caaadar (lst) (car (caadar lst)))
(defun caaaddr (lst) (car (caaddr lst)))
(defun caadaar (lst) (car (cadaar lst)))
(defun caadadr (lst) (car (cadadr lst)))
(defun caaddar (lst) (car (caddar lst)))
(defun caadddr (lst) (car (cadddr lst)))
(defun cadaaar (lst) (car (cdaaar lst)))
(defun cadaadr (lst) (car (cdaadr lst)))
(defun cadadar (lst) (car (cdadar lst)))
(defun cadaddr (lst) (car (cdaddr lst)))
(defun caddaar (lst) (car (cddaar lst)))
(defun caddadr (lst) (car (cddadr lst)))
(defun cadddar (lst) (car (cddddr lst)))
(defun caddddr (lst) (car (cadddr lst)))
(defun cdaaaar (lst) (cdr (caaaar lst)))
(defun cdaaadr (lst) (cdr (caaadr lst)))
(defun cdaadar (lst) (cdr (caadar lst)))
(defun cdaaddr (lst) (cdr (caaddr lst)))
(defun cdadaar (lst) (cdr (cadaar lst)))
(defun cdadadr (lst) (cdr (cadadr lst)))
(defun cdaddar (lst) (cdr (caddar lst)))
(defun cdadddr (lst) (cdr (cadddr lst)))
(defun cddaaar (lst) (cdr (cdaaar lst)))
(defun cddaadr (lst) (cdr (cdaadr lst)))
(defun cddadar (lst) (cdr (cdadar lst)))
(defun cddaddr (lst) (cdr (cdaddr lst)))
(defun cdddaar (lst) (cdr (cddaar lst)))
(defun cdddadr (lst) (cdr (cddadr lst)))
(defun cddddar (lst) (cdr (cdddar lst)))
(defun cdddddr (lst) (cdr (cadddr lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun nthcdr (index lst)
;;   "Get the INDEXth cdr of LST."
;;   (unless (and (integer? index) (positive? index)) (error "N must be a positive integer"))
;;   (unless (list? lst)                              (error "LST must be a list"))
;;   (unless (>= index 0)                              (error "INDEX must be non-negative"))
;;   (until (zero? index)
;;     (setq lst (cdr lst))
;;     (decr index))
;;   lst)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (confirm that (nthcdr 2 '(1 2 3 4 5)) returns (3 4 5))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapr (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over LST."
  (mapcar fn lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (mapr (list 1 2 3) #'1+) returns (2 3 4))
(confirm that (mapr '(1 2 3) #'1+) returns (2 3 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun maprc (lst fn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Map FN over LST, discarding the result and returning LST."
  (mapc fn lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(confirm that (maprc (list 1 2 3) (lambda (x) (* 2 x))) returns (1 2 3))
(confirm that (maprc '(1 2 3) (lambda (x) (* 2 x))) returns (1 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old functions using dash. These are a bit stale, some of them should probably be
;; renamed, updated or discarded:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -mapr (l fun)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (-map fun l))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -maprc (l &rest funs)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (-map (eval (cons #'-compose funs)) l))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -maprcr (l &rest funs)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Reverse the order of funs, compose them and then map them over 𝒍."
  ((eval (cons #'-compose (nreverse funs))) l))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
