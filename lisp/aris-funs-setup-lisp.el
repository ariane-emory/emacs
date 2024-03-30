;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package aris-funs-with-messages)
(use-package aris-funs-prettify-symbols)
(use-package aggressive-indent :ensure t)
(use-package display-fill-column-indicator :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-setup-lisp ()
  (interactive)
  (indented-message  "Setting up Lisp in buffer '%s.'" (buffer-name))
  (setq-local display-fill-column-indicator-mode nil)
  (setq-local display-fill-column-indicator nil)
  (display-fill-column-indicator-mode -1)
  (aris-prettify-symbols-lisp)
  (aggressive-indent-mode 1)
  (eldoc-mode 1)
  (variable-pitch-mode 1)
  (face-remap-add-relative 'font-lock-function-name-face
    '(:box (:line-width (1 . 1) :color "#ec0" :style pressed-button)))
  (face-remap-add-relative 'font-lock-doc-face '( :family "XITS")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs-setup-lisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
