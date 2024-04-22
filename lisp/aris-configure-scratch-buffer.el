;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename '*scratch*' → 'scratch' and setup other details of the scratch buffer:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs-remove-buffers)

(remove-scratch-buffer)

(with-current-buffer (switch-to-buffer "scratch")
  (fundamental-mode) ;; (emacs-lisp-mode)
  (variable-pitch-mode -1)
  (rainbow-mode 1)
  (setq-local fill-column 60)
  (when (featurep 'trav) (trav-mode 1))
  (end-of-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-configure-scratch-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
