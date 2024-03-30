;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define some random functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aris-funs-prettify-symbols)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-log (message &optional newline-first)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Log a MESSAGE to a custom buffer, optionally with a NEWLINE_FIRST."
  (let ((log-buffer-name "ari")) ; Changed name for clarity
    (unless (get-buffer log-buffer-name)
      (with-current-buffer (get-buffer-create log-buffer-name)
        (let ((aris-log-keymap (make-sparse-keymap)))
          (define-key aris-log-keymap (kbd "SPC") 'end-of-buffer)
	  (define-key aris-log-keymap (kbd "DEL") 'ccm-scroll-down)
          (use-local-map aris-log-keymap))
        (face-remap-add-relative 'default '(:foreground "#da0"))
        (centered-cursor-mode 1)
        (insert (format "[%s] %s\n" (current-time-string) "Log started."))
        (setq buffer-read-only t)))
    (with-current-buffer (get-buffer log-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (when newline-first (insert "\n"))
        (insert (format "[%s] %s\n" (current-time-string) message))
        (setq buffer-read-only t)))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-dump-key-macro ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Insert a lambda function equivalent to the last keyboard macro."
  (interactive)
  (let* ((sym (gensym))
          (buffer-string
            (with-temp-buffer
              (name-last-kbd-macro sym)
              (insert-kbd-macro sym)
              (buffer-string)))
          (defalias-expr (read buffer-string))
          (kmacro-string (cadr (caddr defalias-expr)))
          (fun `(lambda () (interactive) (execute-kbd-macro (kbd ,kmacro-string))))
          (fun-string (prin1-to-string fun)))
    (insert fun-string)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-ns-copy-including-secondary-keep-selection ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Copy the region to the clipboard without deactivating the mark."
  (interactive)
  (let ((deactivate-mark nil)) ; Prevent `deactivate-mark` from being set to t
    (ns-copy-including-secondary)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun beginning-of-line-text-pos ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the position of the beginning of the text on the line."
  (interactive)
  (save-excursion ; Preserve the original point position
    (beginning-of-line-text)
    (point))) ; Compare the positions
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun end-of-line-pos ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the position of the end of the line."
  (interactive)
  (save-excursion ; Preserve the original point position
    (end-of-line)
    (point))) ; Compare the positions
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun beginning-of-line-pos ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Get the position of the beginning of the line."
  (interactive)
  (save-excursion ; Preserve the original point position
    (beginning-of-line)
    (point))) ; Compare the positions
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun beginning-of-line-text-p ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Check if the point is at the beginning of text on the current line."
  (interactive)
  (= (beginning-of-line-text-pos) (point)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun after-beginning-of-line-text-p ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Check if the point is after the beginning of text on the current line."
  (interactive)
  (> (point) (beginning-of-line-text-pos)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun before-beginning-of-line-text-p ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Check if the point is before the beginning of text on the current line."
  (interactive)
  (< (point) (beginning-of-line-text-pos)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *aris-cycle-position-initial-position* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-cycle-position-back ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Cycle cursor position backwards between beginning of line text, beginning
of line, end of line and the initial position."
  (interactive)
  (when (not (or
             (eq last-command 'aris-cycle-position-back)
             (eq last-command 'aris-cycle-position-forward)))
    (setq *aris-cycle-position-initial-position* (point)))
  (cond
    ((bolp)
      (end-of-line))
    ((or (beginning-of-line-text-p) (before-beginning-of-line-text-p))
      (beginning-of-line))
    ((and (eolp)
       (or
         (< *aris-cycle-position-initial-position* (beginning-of-line-text-pos))
         (= *aris-cycle-position-initial-position* (end-of-line-pos))))
      (beginning-of-line-text))
    ((eolp)
      (goto-char *aris-cycle-position-initial-position*))
    (t (beginning-of-line-text))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-cycle-position-forward ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Cycle cursor position forwards between beginning of line text, beginning of
 line, end of line and the initial position."
  (interactive)
  (when (not (or
             (eq last-command 'aris-cycle-position-back)
             (eq last-command 'aris-cycle-position-forward)))
    (setq *aris-cycle-position-initial-position* (point)))
  (cond
    ((eolp)
      (beginning-of-line))
    ((and (beginning-of-line-text-p)
       (or
         (< *aris-cycle-position-initial-position* (beginning-of-line-text-pos))
         (= *aris-cycle-position-initial-position* (beginning-of-line-text-pos))))
      (end-of-line))
    ((beginning-of-line-text-p)
      (goto-char *aris-cycle-position-initial-position*))
    ((before-beginning-of-line-text-p)
      (beginning-of-line-text))
    (t (end-of-line))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-swap-buffers-in-windows ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Swap buffers between two windows, preserving window positions."
  (interactive)
  (unless (= (count-windows) 2)
    (error "You need exactly 2 windows to do this"))
  (let* ((win1 (car (window-list)))
          (win2 (cadr (window-list)))
          (buf1 (window-buffer win1))
          (buf2 (window-buffer win2))
          (pos1 (window-point win1))
          (pos2 (window-point win2)))
    (set-window-buffer win1 buf2)
    (set-window-buffer win2 buf1)
    (set-window-point win1 pos2)
    (set-window-point win2 pos1)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-move-line-up ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Drag the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-move-line-down ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Drag the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-delete-previous-word ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Delete the previous word without affecting the kill ring."
  (interactive)
  (let ( (start (point))
         (end (progn (backward-word) (point))))
    (delete-region end start)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-delete-line ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Delete the curent line without affecting the kill ring."
  (interactive)
  (delete-line))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-kill-region-or-line ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Kill the current region if active, otherwise kill the current line."
  (interactive)
  (if (use-region-p)
    (kill-region (region-beginning) (region-end))
    (kill-line)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-kill-whole-line ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Kill the whole current line."
  (interactive)
  (beginning-of-line)
  (kill-line))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-yank-rectangle ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Yank a rectangle with save-excursion"
  (interactive)
  (save-excursion
    (yank-rectangle)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-join-next-line ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Join the next line with this one."
  (interactive)
  (save-excursion
    (move-end-of-line nil)     
    (delete-char 1)
    (fixup-whitespace)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-always (&rest _) t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-never (&rest _) nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-truthify(thing)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "If THING is a boolean return t, if it's a function, call it and return the
result, if it's a bound symbol, return its value, if it's a function symbol,
call it and return the result, otherwise return nil."
  (cond
    ((booleanp thing) thing)
    ((functionp thing) (funcall thing))
    ((boundp thing) (symbol-value thing))
    ((functionp (symbol-function thing)) (funcall (symbol-function thing)))
    (t nil)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-start-dired-here ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Start dired in the current directory."
  (interactive)
  (dired "."))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-eval-sexp-and-insert-as-comment ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Evaluate the first sexp on this line and insert the result after it as a
 comment."
  (interactive)
  (execute-kbd-macro
    (kbd
      (concat
        "C-M-a C-M-f C-SPC C-e C-w SPC ; ; SPC "
        "C-u C-x C-e C-r ; ; <right> <right> <right> = > SPC C-e"))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-make-setq ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn a Custom setting beginning on this line into a setq expression."
  (interactive)
  (execute-kbd-macro
    (kbd
      (concat
        "C-M-a C-e <right> <backspace> "
        "<right> s e t q SPC C-M-e <right>"))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-close-all-parentheses ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Close all open parentheses."
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
	             (progn
		             (backward-up-list)
		             (let ((syntax (syntax-after (point))))
		               (cl-case (car syntax)
		                 ((4) (setq closing
			                      (cons (cdr syntax) closing)))
		                 ((7 8) (setq closing
			                        (cons (char-after (point)) closing)))))
		             t)
	             ((scan-error) nil))))
    (apply #'insert (nreverse closing))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-delete-backwards-char ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (interactive)
  (backward-char)
  (delete-char 1))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-backwards-comment-sexp ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Comment the sexp behind point."
  (interactive)
  (save-excursion
    (backward-sexp)
    (aris-forwards-comment-sexp)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-forwards-comment-sexp ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Comment the sexp at point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (comment-region (point) (mark))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-find-function-at-point ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Quickly find-function."
  (interactive)
  (find-function (function-called-at-point)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-rename-scratch-buffer ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Rename the *scratch* buffer to `scratch'."
  (interactive)
  (when (get-buffer bufname)
    (let ((bufname "*scratch*"))
      (with-current-buffer (get-buffer bufname)
        (rename-buffer "scratch")))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-revert-buffer-no-confirm ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun re-load ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Re-Load the .emacs file."
  (interactive)
  (load-file (expand-file-name "init.el" aris-config-dir)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-remove-dos-eol ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-force-kill-buffer ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Kill current buffer unconditionally."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flash-background ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Flash the entire screen red a few times annoyingly"
  (interactive)
  (let ((original-color (cdr (assoc 'background-color (frame-parameters)))))
    (when original-color
      (set-background-color "red")
      (sleep-for 0.05)
      (set-background-color original-color)
      (sleep-for 0.1)
      (set-background-color "red")
      (sleep-for 0.05)
      (set-background-color original-color)
      (sleep-for 0.1)
      (set-background-color "red")
      (sleep-for 0.05)
      (set-background-color original-color)
      (sleep-for 0.1))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-switch-to-last-buffer ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (interactive)
  (switch-to-buffer nil))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-switch-to-previous-buffer ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Switch to previously open buffer. Repeated invocations toggle between the
two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-buffer ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic functions for reading/writing colour strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rgb-list-to-hex-color-string (l)
  "Turn a list like \='(69 103 137) into a string like \"#456789\"."
  (let* ( (fun (-partial #'format "%02x")))
    (apply #'concat `("#" ,@(mapcar fun l)))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -revf (fun)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Return a lambda that calls 𝒇 with the order of arguments reversed."
  (let ((fun fun))
    (lambda (&rest args) (eval (cons fun (nreverse args))))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hex-color-string-to-rgb-list (hex-color)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Turn a string like \"#456789\" into a list like \='(69 103 137)."
  (let* ((l (mapcar (-compose #'1+ (-partial #'* 2) ) (number-sequence 0 2)))
	        (hex-string-to-number (-partial (-revf #'string-to-number) 16))
	        (grab-two-chars (lambda (n) (substring hex-color n (+ n 2))))
	        (fun (-compose hex-string-to-number grab-two-chars)))
    (mapcar fun l)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for adjusting the luminance of color strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -mapr (l fun)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (-map fun l))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -maprc (l &rest funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (-map (eval (cons #'-compose funs)) l))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fmuls (x y z)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Multiply y by z and then shift it right by x."
  (ash (* y z) (* -1 x)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'clamp8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (-compose (-partial #'min 255) (-partial #'min 255))
  "Limit values to a 0-255 range.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'fmuls8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (-partial #'fmuls 8)
  "Multiply x by y and then shift it right by 8.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fmuls8-hex-color-string (hcs x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Apply fmuls8 to each value in a color string like \"#aabbcc\" and return a
new color string."
  (rgb-list-to-hex-color-string
    (mapcar (-compose #'clamp8 (-partial #'fmuls8 x))
      (hex-color-string-to-rgb-list hcs))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun -maprcr (l &rest funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Reverse the order of funs, compose them and then map them over 𝒍."
  ((eval (cons #'-compose (nreverse funs))) l))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shades-of (hex-color-string &rest pl)
;;;;;;;;;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((step (or (plist-get pl :step) 16))
         (counter (or (plist-get pl :initial-step) 0))
	       (l nil))
    (while counter 
      (let* ((rgb-list (mapcar
                         (-partial #'fmuls8 (* step counter))
		                     (hex-color-string-to-rgb-list hex-color-string)))
	            (hex-color-string (rgb-list-to-hex-color-string rgb-list))
	            (rgb-list-max (apply #'max rgb-list)))
	      (if (>= rgb-list-max 256)
	        (setq counter nil)
          (progn
	          (setq l (cons hex-color-string l))
	          (setq counter (1+ counter))))))
    l))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *shades-table* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shades-clear ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq *shades-table* nil))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shades-put (face property value)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when (not (boundp '*shades-table*))
    (setq *shades-table* nil))
  (setq this-member (cadr (plist-member *shades-table* face)))
  (setq this-member (plist-put this-member property value))
  (setq *shades-table* (plist-put *shades-table* face this-member)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shades-get (face property)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when (not (boundp '*shades-table*))
    (setq *shades-table* nil))
  (plist-get (cadr (plist-member *shades-table* face)) property))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun stop-auto-shades (face-name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((timer (shades-get face-name 'timer)))
    (when timer
      (cancel-timer timer)))
  (internal-set-lisp-face-attribute face-name :foreground
    (car (shades-get face-name 'shades))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun start-auto-shades (face-name hex-color-string step interval)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (stop-auto-shades face-name)
  (let* ( (shades (shades-of hex-color-string step))
	        (shades (append shades (reverse shades)))
	        (length (length shades))
	        (counter 0))
    (shades-put face-name 'color-string hex-color-string)
    (shades-put face-name 'step step)
    (shades-put face-name 'shades shades)
    (shades-put face-name 'timer
      (run-with-timer 0 interval
	      (lambda ()
	        (setq counter (mod (+ counter 1) length))
	        (internal-set-lisp-face-attribute face-name :foreground
	          (nth counter shades)))))))
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun odd? (x) (= (mod x 2) 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun even? (x) (= (mod x 2) 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 2* (x) (* x 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun match6 (p s)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    ((null p) (null s)) ;case with both p and s null ;trom here on we can assume p is not null.
    ((atom (car p)) ;case when car p is an atom (and s
      (and s ;s must not be null.
        (equal (car p) (car s))
        (match6 (cdr p) (cdr s)))) ;from here on car of p is non atomic.
    ((and ;case when p starts with 'some form.
       s ;s must not be null.
       (eq (caar p) 'some))
      (cond
        ((match6 (cdr p) (cdr s)) ; If rest match6 too
          (set (cadar p) (car s))) ; Then set something
        (t nil))) ; Otherwise, return nil
    ((eq (caar p) 'any) ;case when p starts with any form.
      (cond
        ((and s (match6 (cdr p) (cdr s))) ;subcase 1
          (set (cadar p) (list (car s))) t)
        ((match6 (cdr p) s) ;subcase 2
          (set (cadar p) nil) t)
        ((and s (match6 p (cdr s))) ;subcase 3
          (set (cadar p) (cons (car s) (eval (cadar p)))) t)
        (t nil)))
    ((and s ;case when p starts with predicate form. ;s must not be null.
       (apply (caar p) (list (car s )))
       (match6 (cdr p) (cdr s)))
      (set (cadar p) (car s)))
    (t nil)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (match6 '((some x) (any ys) (some z) (even? za)) '(3333 31 32 34 36 3444))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x ;; => 3333
;; ys ;; => (31 32 34)
;; z ;; => 36
;; za ;; => 3444
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-merge-duplicate-alist-keys (alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Merge the values of duplicate keys in ALIST."
  (message "Merging duplicated alist keys.")
  (let ((result nil))
    (dolist (pair alist)
      (let* ((key (car pair))
              (value (cdr pair))
              (existing (assoc key result)))
        (if existing
          (rplacd existing (append (cdr existing) (list value)))
          (push (cons key (list value)) result))))
    (mapcar (lambda (pair)
              (if (length> (cdr pair) 1)
                pair
                (cons (car pair) (car (cdr pair)))))
      result)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'dash)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defvar *match--init-fun* nil
    "The function to call to initialize a new match.")
  
  (defvar *match--merge-duplicate-alist-keys* t
    "Whether to merge the values of duplicate keys in the result alist.")
  
  (defvar *match--kleene-tag* '* "The symbol used by match to represent a Kleene star, matching 0 or more times.")
  (defvar *match--anything-tag* '? "The symbol used by match to represent a wildcard, matching any single item in the
`TARGET'.")
  (defvar *match--get-capture-symbol-fun* #'cdr "The function used by match to extract the symbol from a capture.")
  (defvar *match--get-capture-tag-fun* #'car "The function used by match to extract the 'tag' from a capture element.")
  (defvar *match--capture-can-be-predicate* t "Whether a capture's 'tag' is allowed to be a predicate function.")
  (defvar *match--capture-element?* #'-cons-pair? "The function used by match to determine if a `PATTERN' element
represents a capture. By default, true pairs are considered captures.")
  (defvar *match--verbatim-element?* nil 
    "The function used by match to determine if a `PATTERN' element is a verbatim (non-capturing) element. By default
any element that isn't a capture element is a verbatim element.")
  (defvar *match--invalid-element?* nil
    "The function used by match to determine if a `PATTERN' element is an illegal / invalid element. By default, any
element that is neither a capture element or a verbatim element is an invalid element.")
  (defvar *match--target-elements-must-be-verbatim* t "Whether the elements of the target list must be verbatim.") 
  (defvar *match--signal-error-if-target-elements-is-not-verbatim* t
    "Whether to signal an error (or merely fail to match) if a non-verbatim `TARGET' element is encountered.
This setting only applies when `*match--target-elements-must-be-verbatim*'.")
  (defvar *match--use-dotted-pairs-in-result* t "Whether to use dotted pairs in the result alist.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ) ;; end progn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aris-match-pattern (pattern target)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Match a `PATTERN' list against a `TARGET' list."
  (message "MATCHING PATTERN %S AGAINST TARGET %s!" pattern target)
  (when *match--init-fun* (funcall *match--init-fun*))
  (cl-labels
    ((match-inner (pattern target &optional (depth 0) (acc nil))
       (let ( (pattern-head (car pattern))
              (pattern-tail (cdr pattern))
              (target-head (car target))
              (target-tail (cdr target)))
         (cl-flet* ( (match (pattern target &optional
                              (cons-onto-acc :CONS-ONTO-ACC-NOT-SUPPLIED))
                       (match-inner pattern target depth
                         (if (eq cons-onto-acc :CONS-ONTO-ACC-NOT-SUPPLIED)
                           acc
                           (cons cons-onto-acc acc))))
                     (string-head (string) ;; pure.
                       (substring string 0 1))
                     (string-tail (string) ;; pure.
                       (substring string 1))
                     (transform-string-head (string fun) ;; pure.
                       (concat (funcall fun (string-head string)) (string-tail string)))
                     (capitalize (string) ;; pure.
                       (transform-string-head string #'upcase))
                     (uncapitalize (string) ;; pure.
                       (transform-string-head string #'downcase))
                     (plural? (string) ;; pure.
                       (equal "s" (substring string -1)))
                     (indent-string ()
                       (make-string depth ?\ ))
                     (message (&rest args)
                       (message "%s%s" (indent-string)
                         (apply #'format (car args) (cdr args))))
                     (continue-with-tails ()
                       (match pattern-tail target-tail))
                     (lookahead (target string)
                       (message "Looking ahead to see if %s match%s..."
                         string
                         (if (plural? string) "" "es"))
                       (setq depth (1+ depth))
                       (let ((tails-match (car (continue-with-tails)))
                              (string (capitalize string)))
                         (setq depth (1- depth))                        
                         (message
                           (if tails-match "%s matched!" "%s didn't match!")
                           string)
                         tails-match))
                     (tails-match? ()
                       (lookahead target-tail "tails"))
                     (pattern-tail-matches-target? ()
                       (lookahead target "PATTERN-TAIL"))
                     (pattern-head-is-atom? ()
                       (atom pattern-head))
                     (pattern-head-is-capture? ()
                       (message "capture?")
                       (cond
                         (*match--capture-element?*
                           (funcall *match--capture-element?* pattern-head))
                         ;; If `*match--capture-element?*' isn't set, but
                         ;; `*match--verbatim-element?*' is, we'll assume anything that's
                         ;; not a verbatim element must be a capture.
                         (*match--verbatim-element?*
                           (not (funcall *match--verbatim-element?* pattern-head)))
                         (t nil)))
                     (elem-is-verbatim? (elem) ;; semi-pure.
                       (message "verbatim?")
                       (cond
                         (*match--verbatim-element?*
                           (funcall *match--verbatim-element?* elem))
                         ;; If `*match--verbatim-element?*' isn't set, but
                         ;; `*match--capture-element?*' is, we'll assume anything that's
                         ;; not a capture element must be a verbatim.
                         (*match--capture-element?*
                           (not (funcall *match--capture-element?* elem)))
                         (t nil)))
                     (pattern-head-is-verbatim? ()
                       (elem-is-verbatim? pattern-head))
                     (pattern-head-is-invalid? ()
                       (if *match--invalid-element?*
                         (funcall *match--invalid-element?* pattern-head)
                         (not (or (pattern-head-is-verbatim?) (pattern-head-is-capture?)))))
                     (assert-pattern-head-is-capture! ()
                       (unless (pattern-head-is-capture?)
                         (error
                           "Logic error: PATTERN-HEAD '%s' is not a capture."
                           pattern-head)))
                     (pattern-head-capture-field (fun)
                       (assert-pattern-head-is-capture!)
                       (funcall fun pattern-head))
                     (pattern-head-capture-symbol ()
                       (pattern-head-capture-field *match--get-capture-symbol-fun*))
                     (pattern-head-capture-tag ()
                       (pattern-head-capture-field *match--get-capture-tag-fun*))
                     (pattern-head-capture-has-tag? (tag)
                       ;;(message "Does it have '%s'?" tag)
                       (when tag
                         (assert-pattern-head-is-capture!)
                         (eq tag (pattern-head-capture-tag))))
                     (accumulate-and-continue-with (value pattern)
                       (let ((kvp (cons (pattern-head-capture-symbol)
                                    (if *match--use-dotted-pairs-in-result* value (list value)))))
                         (message "Accumulating %s." kvp)
                         (match pattern target-tail kvp)))
                     (heads-are-equal? ()
                       (message "Compare %s with %s..." pattern-head target-head)
                       (equal pattern-head target-head))
                     (fail-to-match ()
                       (cons nil acc))
                     (match-successfully ()
                       (cons t acc)))
           (message "Matching %s against %s with acc %s..." pattern target acc)
           (if (elem-is-verbatim? target-head)
             (message "TARGET-HEAD %s is a verbatim element." target-head)
             (message "TARGET-HEAD %s is not a verbatim element." target-head))
           (setq depth (1+ depth))
           (cond
             ;; If `pattern' is null, match successfully when `target' is null too:
             ((null pattern)              
               (message "PATTERN is null and %s match!"
                 (if target
                   "TARGET is not, sono"
                   "so is TARGET,"))
               (if (null target)
                 (match-successfully)
                 (fail-to-match)))
             ;; Fail to match if `target' is null and `pattern' isn't:
             ((null target)
               (message "TARGET is null and PATTERN isn't, no match!")
               (fail-to-match))
             ;; If `pattern-head' is a verbatim element, match if it's equal to (car `target'):
             ((pattern-head-is-verbatim?)
               (message "PATTERN-HEAD %s is a verbatim element." pattern-head)
               (if (heads-are-equal?)
                 (continue-with-tails)
                 (fail-to-match)))
             ;; If `*match--target-elements-must-be-verbatim*' is set, then signal 
             ;; an error if `target-head' isn't a verbatim element:
             ((and
                *match--target-elements-must-be-verbatim*
                (not (elem-is-verbatim? target-head)))
               (let ((complaint
                       (format "TARGET-HEAD '%s' is not a verbatim element."
                         target-head)))
                 (if *match--signal-error-if-target-elements-is-not-verbatim*
                   (error complaint)
                   (message complaint)
                   (fail-to-match))) )
             ;; If `pattern-head' isn't either a verbatim element or a capture,
             ;; something has gone wrong:
             ((pattern-head-is-invalid?)
               (error
                 "PATTERN-HEAD '%s' is an invalid element."
                 pattern-head))
             ;; From here on, we know that `pattern-head' must be a capture.
             ;; Case when `pattern-head' is tagged with the "anything" tag:
             ((pattern-head-capture-has-tag? *match--anything-tag*)
               (message "Head of PATTERN has 'anything' tag.")
               (accumulate-and-continue-with target-head pattern-tail))
             ;; Case when `pattern-head' is tagged with the Kleene tag:
             ((pattern-head-capture-has-tag? *match--kleene-tag*)
               (message "Head of PATTERN has Kleene tag.")
               (cond
                 ((tails-match?)
                   (message (concat
                              "Kleene case 1: The rest of PATTERN matches the "
                              "rest of TARGET, so we'll take LIST_HEAD as a Kleene item."))
                   (accumulate-and-continue-with target-head pattern-tail))
                 ((pattern-tail-matches-target?)
                   (message (concat
                              "Kleene case 2: The rest of PATTERN matches the "
                              "entire TARGET, so the Kleene item is nil."))
                   (accumulate-and-continue-with nil))
                 (t
                   (message "Kleene case 3: Take LIST head as a Kleene item.")
                   (accumulate-and-continue-with target-head pattern))))
             ;; Case when `pattern-head' starts with predicate form:
             ((and
                *match--capture-can-be-predicate*
                (apply (pattern-head-capture-tag) (list target-head)))
               (accumulate-and-continue-with target-head pattern-tail))
             ;; Some unimplemented case happened, signal an error:
             (t (error "Unhandled case! Double-check your configuration.")))))))
    (let ((match-result (match-inner pattern target)))
      (when (car match-result)
        ;; (message "Match result is %s." match-result)
        (if (cdr match-result)
          (if *match--merge-duplicate-alist-keys*
            (nreverse (aris-merge-duplicate-alist-keys (cdr match-result)))
            (cdr match-result))
          ;; If the match succeeded but there were no captures, just return t:
          t)))))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'match 'aris-match-pattern)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(match '((? . v) (* . w) 4 5 (? . x) (even? . y)) '(77 1 2 3 4 5 66 22))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs-unsorted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
