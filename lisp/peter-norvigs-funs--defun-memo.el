;; -*- fill-column: 80; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Peter Norvig's `defun-memo', translated from Common Lisp to Emacs Lisp by
;; ariane-emory.
;;
;; Based largely on materiel found at these URLs:
;; https://finnvolkel.com/memoization
;; https://people.eecs.berkeley.edu/~fateman/lisp/memo-simp.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs--with-messages)
(require 'aris-funs--with-gensyms)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-memo (fun-name args &rest body)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari changed the return value to align with `defun''s.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Define a memoized function."
  `(progn
     (bind-to-memo-fun (defun ,fun-name ,args . ,body))
     ',fun-name))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defun bind-to-memo-fun (fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari removed the TEST argument.
  ;; - ari removed the KEY argument.
  ;; - ari renamed this from `memoize' to `bind-to-memo-fun'.
  ;; - ari changed the default key from #'first to #'identity.
  ;; - ari changed the default test from from #'eql to #'equal.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Replace fun-name's global definition with a memoized version."
  (when (get fun-name 'memo-table)
    (clear-memos fun-name))
  (setf (symbol-function fun-name) (make-memo-fun fun-name))
  fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-memo-fun (fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari removed the FUN argument.
  ;; - ari removed the TEST argument.
  ;; - ari removed the KEY argument.
  ;; - ari replaced a "(`setf' (`get'" with a "(`put'".
  ;; - ari renamed 'memo to 'memo-table.
  ;; - ari renamed this fun from `memo' to `make-memo-fun'.
  ;; - ari simplified the arguments to this function compared to the original.
  ;; - ari adjusted this to use backwote so as to align with elisps scoping.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a memo-function of FUN."
  (let ( (fun   (symbol-function fun-name))
         (table (make-hash-table :test #'equal)))
    (setf (get fun-name 'memo-table) table)
    (with-gensyms (memo-not-found)
      `(lambda (&rest args)
         (let ((val (gethash args ,table ',memo-not-found)))
           (if (eq val ',memo-not-found)
             (setf (gethash args ,table) (apply ,fun args))
             val))))))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clear-memos (fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari renamed this from `clear-memoize' to `clear-memos'.
  ;; - ari renamed 'memo to 'memo-table.
  ;; - ari made this fun more elisp-y by using `when-let'.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Clear the hash table of a memo function."
  (if-let ((table (get fun-name 'memo-table)))
    (clrhash table)
    (error "No memo table found for '%S." fun-name)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-memo-table (fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari renamed 'memo to 'memo-table.
  ;; - ari adjusted the format string for elisp.
  ;; - ari wrote this herself, it is not from Norvig.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Format the memo table of FUN-NAME for debugging purposes."
  (let (result)
    (maphash
      (lambda (k v) (push (format "%S ⇒ %S" k v) result))
      (get fun-name 'memo-table))
    (nreverse result)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-memo-table (fun-name)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - ari wrote this herself, it is not from Norvig.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Format the memo table of FUN-NAME for debugging purposes."
  (prndiv)
  (prn "Memo table for '%S:" fun-name)
  (prndiv)
  (mapc #'prn (format-memo-table fun-name))
  (prndiv))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun make-memo-fun (fun name key test)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;; - ari replaced a "(`setf' (`get'" with a "(`put'".
;;   ;; - ari renamed 'memo to 'memo-table.
;;   ;; - ari renamed this fun from `memo' to `make-memo-fun'.
;;   ;; - ari simplified the arguments to this function compared to the original.
;;   ;; - ari adjusted this to use backwote so as to align with elisps scoping.
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Return a memo-function of FUN."
;;   (let ((table (make-hash-table :test test)))
;;     (setf (get name 'memo-table) table)
;;     (with-gensyms (memo-not-found)
;;       `(lambda (&rest args)
;;          (let* ( (k   (funcall #',key args))
;;                  (val (gethash k ,table ',memo-not-found)))
;;            (if (eq val ',memo-not-found)
;;              (setf (gethash k ,table) (apply ,fun args))
;;              val))))))
;;              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (cl-defun bind-to-memo-fun (fun-name &key (key #'identity) (test #'equal))
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;; - ari renamed this from `memoize' to `bind-to-memo-fun'.
;;   ;; - ari changed the default key from #'first to #'identity.
;;   ;; - ari changed the default test from from #'eql to #'equal.
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   "Replace fun-name's global definition with a memoized version."
;;   (clear-memos fun-name)
;;   (setf (symbol-function fun-name)
;;     (make-memo-fun (symbol-function fun-name) fun-name key test)))
;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'peter-norvigs-funs--defun-memo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; peter-norvigs-funs--defun-memo.el ends here
