;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
    '( ("lambda" . ?λ)
       ("member" . ?∈)
       ("mapcar" . ?∀)
       ("mapc" . ?∀)
       ("-compose" . ?∘)
       ("-revf" . ?⟲)
       ("and" . ?∧)
       ("or" . ?∨)
       ("-maprcr" . ?⇓)
       ("-partial" . ?$)
       ("." . ?·)
       ("1s+" . ?∆)
       ("*" . ?×)
       ("/" . ?÷)
       ("t" . ?⊤)
       ("fun" . ?𝒇)
       ("i" . ?𝒊)
       ("l" . ?𝒍)
       ("n" . ?𝒏)
       ("obj" . ?𝒐)
       ("x" . ?𝒙)
       ("s". ?𝒔)
       ("y" . ?𝒚)
       ("z" . ?𝒛)
       ("e" . ?𝒆)
       ("nil" . ?⊥)
       ("not" . ?¬)
       ("->" . ?→)
       ("<-" . ?←)
       ("=>" . ?⇒)
       ("goto-char" . ?⇒)
       ("<=" . ?≤)
       (">=" . ?≥)))
  (prettify-symbols-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-prettify-symbols-lisp ()
  (interactive)
  (setq-local prettify-symbols-alist
    '(
       ("|>" . ?▶)
       ("lambda" . ?λ)
       ("member" . ?∈)
       ;;("mapcar" . ?∀)
       ("*" . ?×)
       ("/" . ?÷)
       ("t" . ?⊤)
       ;;("nil" . ?⊥)
       ("not" . ?¬)
       (">>" . ?»)
       ("<<" . ?«)
       ("->" . ?→)
       ("<-" . ?←)
       ("=>" . ?⇒)
       ("<=" . ?≤)
       (">=" . ?≥)))
  (prettify-symbols-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-prettify-symbols-c++ ()
  (interactive)
  (setq-local prettify-symbols-alist
    '(
       ("&&" . ?∧)
       ("||" . ?∨)
       ("." . ?·)
       ("/" . ?÷)
       ("true" . ?⊤)
       ("false" . ?⊥)
       ("not" . ?¬)
       ("->" . ?→)
       (">>" . ?»)
       ("<<" . ?«)
       ("<=" . ?≤)
       (">=" . ?≥)))
  (prettify-symbols-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-prettify-symbols-rust ()
  (interactive)
  (setq-local prettify-symbols-alist
    '(
       ("fn" . ?λ)
       ("&&" . ?∧)
       ("||" . ?∨)
       ;;      ("." . ?·)
       ("/" . ?÷)
       ("!" . ?¬)
       ("->" . ?⇒)
       ("<=" . ?≤)
       (">=" . ?≥)))
  (prettify-symbols-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--prettify-symbols)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
