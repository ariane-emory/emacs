;; -*- lisp-indent-offset: 2; -*-
"Define my auto-revert-log mode:"

(define-derived-mode auto-revert-log-mode nil "Log" nil
  (progn
    (auto-revert-tail-mode 1)
    (view-mode 1)
    (centered-cursor-mode 0)
    (end-of-buffer)))

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-log-mode))

(provide 'aris-mode-auto-revert-log)