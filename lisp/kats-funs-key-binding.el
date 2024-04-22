;; -*- lisp-indent-offset: 2; -*-
(require 'kats-funs-unsorted)

(defun kats-bind-to-initials-with-prefix (fun-name &optional prefix)
  "Quickly bind a function to a user keybind based on it's initials"
  (let* ((prefix (or prefix "C-c"))
          (words (split-string (symbol-name fun-name) "-"))
          (letters (mapcar (lambda (x) (substring x 0 1)) words))
          (key-bind-string (concat prefix " " (mapconcat 'identity letters " ")))
          (key-bind (kbd key-bind-string)))
    (global-set-key key-bind fun-name)))

(defun kats-make-pairs (left-list right-list)
  "Make a list containing the Cartesian product of left-list and right-list as pairs"
  (seq-reduce #'append
    (mapcar
      (lambda (left-item)
        (mapcar
          (lambda (right-item)
            (cons left-item right-item))
          right-list))
      left-list)
    nil))

(defun kats-bind-pair-with-prefix (&optional prefix-key)
  "Accept a pair like '(verb noun) and bind 'verb-noun to a key like \"v b\" with a prefix."
  (let ((prefix-key (or prefix-key "C-c")))
    (lambda (pair)
      (let* ((verb-symbol-name (symbol-name (car pair)))
              (verb-initial (substring verb-symbol-name 0 1))
              (noun-symbol-name (symbol-name (cdr pair)))
              (noun-initial (substring noun-symbol-name 0 1))
              (prefix-part (concat prefix-key " " verb-initial))
              (key-string (concat prefix-part " " noun-initial))
              (kbd-repr (kbd key-string))
              (fun-symbol (intern (concat verb-symbol-name "-" noun-symbol-name))))
        (global-unset-key kbd-repr)
        (global-set-key kbd-repr fun-symbol)))))

(defun kats-bind-pairs-with-prefix (verbs nouns &optional prefix-key)
  "Call kats-bind-pair-with-prefix on a pair of lists"
  (let ((prefix-key (or prefix-key "C-c"))
         (verbs (if (listp verbs) verbs (list verbs)))
         (nouns (if (listp nouns) nouns (list nouns))))
    (mapcar (kats-bind-pair-with-prefix prefix-key)
      (kats-make-pairs verbs nouns))))

(cl-defun kats-auto-bind (key fun &key map (when t))
  "Bind KEY to FUN in MAP when WHEN is true.
If MAP is nil, bind in the global key map."
  (when (kats-truthify when)
    (let ((key-seq (if (stringp key) (kbd key) key)))
      (if map
        (define-key (eval map) key-seq fun)
        (global-set-key key-seq fun)))))

(provide 'kats-funs-key-binding)
