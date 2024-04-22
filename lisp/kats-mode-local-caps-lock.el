(defvar kats-local-caps-lock-commands
  '(self-insert-command isearch-printing-char)
  "List of commands that are subject to `kats-local-caps-lock-mode'.")

(defun kats-local-caps-lock--pch ()
  (when (and (bound-and-true-p kats-local-caps-lock-mode)
             (characterp last-command-event)
             (or (memq this-command kats-local-caps-lock-commands)
                 (eq this-command (key-binding [remap self-insert-command]))))
    (setq last-command-event
          (condition-case nil
              (let ((up (upcase last-command-event)))
                (if (eq up last-command-event)
                    (downcase last-command-event)
                  up))
            (error last-command-event)))))

;;;###autoload
(define-minor-mode kats-local-caps-lock-mode
  "Make self-inserting keys invert the capitalization."
  :lighter " Caps"
  :init-value nil
  (if kats-local-caps-lock-mode
      (add-hook 'pre-command-hook #'kats-local-caps-lock--pch nil t)
    (remove-hook 'pre-command-hook #'kats-local-caps-lock--pch t)))

(provide 'kats-mode-local-caps-lock)
;;; kats-local-caps-lock.el ends here
