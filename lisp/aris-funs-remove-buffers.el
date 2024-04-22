;; -*- lisp-indent-offset: 2; -*-

(defun remove-scratch-buffer ()
  "Get rid of the *scratch* buffer"
  (interactive)
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))

(defun aris-remove-messages-buffer ()
  "Get rid of the *Messages* buffer"
  (interactive)
  (when (get-buffer "*Messages*")
    (kill-buffer "*Messages*")))

(provide 'aris-funs-remove-buffers)
