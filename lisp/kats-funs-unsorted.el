;; -*- lisp-indent-offset: 2; lexical-binding: t; -*-

"Define some random functions:"

(defun kats-dump-key-macro ()
  "Dump the last macro into the current buffer as a lambda and position point in order to write a docstring for it."
  (interactive)
  (let ((sym (gensym)))
    (name-last-kbd-macro sym)
    (insert-kbd-macro sym)
    (backward-sexp)
    (forward-char)
    (kill-sexp)
    (kill-sexp)
    (insert "defun temporary-name ()\n(interactive)\n(execute-kbd-macro ")
    (delete-char 1)
    (cycle-spacing)
    (forward-char)
    (kill-sexp)
    (insert "vconcat (kbd")
    (forward-sexp)
    (insert "))")
    (backward-up-list)
    (backward-up-list)
    (forward-char)
    (forward-sexp)
    (forward-char)
    (kill-sexp)
    (insert "kats-")))

(defun kats-ns-copy-including-secondary-keep-selection ()
  "Copy the region to the clipboard without deactivating the mark."
  (interactive)
  (let ((deactivate-mark nil)) ; Prevent `deactivate-mark` from being set to t
    (ns-copy-including-secondary)))

(defun beginning-of-line-text-pos ()
  "Get the position of the beginning of the text on the line."
  (interactive)
  (save-excursion ; Preserve the original point position
    (beginning-of-line-text)
    (point))) ; Compare the positions

(defun end-of-line-pos ()
  "Get the position of the end of the line."
  (interactive)
  (save-excursion ; Preserve the original point position
    (end-of-line)
    (point))) ; Compare the positions

(defun beginning-of-line-pos ()
  "Get the position of the beginning of the line."
  (interactive)
  (save-excursion ; Preserve the original point position
    (beginning-of-line)
    (point))) ; Compare the positions

(defun beginning-of-line-text-p ()
  "Check if the point is at the beginning of text on the current line."
  (interactive)
  (eql (beginning-of-line-text-pos) (point)))

(defun after-beginning-of-line-text-p ()
  "Check if the point is after the beginning of text on the current line."
  (interactive)
  (> (point) (beginning-of-line-text-pos)))

(defun before-beginning-of-line-text-p ()
  "Check if the point is before the beginning of text on the current line."
  (interactive)
  (< (point) (beginning-of-line-text-pos)))

(defvar kats-cycle-position-initial-position nil)

(defun kats-cycle-position-back ()
  "Cycle cursor position backwards between beginning of line text, beginning of line, end of line and the initial position."
  (interactive)
  (when (not (or (eq last-command 'kats-cycle-position-back) (eq last-command 'kats-cycle-position-forward)))
    (setq kats-cycle-position-initial-position (point)))
  (cond
    ((bolp)
      (end-of-line)
      ;; (message
      ;;   "1. go to end of line: %d"
      ;;   kats-cycle-position-initial-position)
      )
    ((or (beginning-of-line-text-p) (before-beginning-of-line-text-p))
      (beginning-of-line)
      ;; (message
      ;;   "2. go to beginning of line from at or before text: %d"
      ;;   kats-cycle-position-initial-position)
      )
    ((and (eolp)
       (or (< kats-cycle-position-initial-position (beginning-of-line-text-pos))
         (= kats-cycle-position-initial-position (end-of-line-pos))))
      (beginning-of-line-text) 
      ;; (message
      ;;   "3. go to beginning of line text from eol because initial pos was either at eol or before text: %d"
      ;;   kats-cycle-position-initial-position)
      )
    ((eolp)
      (goto-char kats-cycle-position-initial-position)
      ;; (message
      ;;   "4. go to initial position from eol: %d"
      ;;   kats-cycle-position-initial-position)
      )
    (t
      (beginning-of-line-text)
      ;; (message
      ;;   "5. go to beginning of line text: %d"
      ;;   kats-cycle-position-initial-position)
      )))

(defun kats-cycle-position-forward ()
  "Cycle cursor position forwards between beginning of line text, beginning of line, end of line and the initial position."
  (interactive)
  (when (not (or (eq last-command 'kats-cycle-position-back) (eq last-command 'kats-cycle-position-forward)))
    (setq kats-cycle-position-initial-position (point)))
  (cond
    ((eolp)
      (beginning-of-line)
      ;; (message
      ;;   "1. go to beginning of line: %d"
      ;;   kats-cycle-position-initial-position)
      )
    ((and (beginning-of-line-text-p)
       (or (< kats-cycle-position-initial-position (beginning-of-line-text-pos))
         (= kats-cycle-position-initial-position (beginning-of-line-text-pos))))
      (end-of-line) 
      ;; (message
      ;;   "2. go to end of line from beginning of line text because initial pos was either at eol or before text: %d"
      ;;   kats-cycle-position-initial-position)
      )
    ((beginning-of-line-text-p)
      (goto-char kats-cycle-position-initial-position)
      ;; (message
      ;;   "3. go to initial position from beginning of line text: %d"
      ;;   kats-cycle-position-initial-position)
      )
    ((before-beginning-of-line-text-p)
      (beginning-of-line-text)
      ;; (message
      ;;   "4. go to beginning of line text from before text: %d"
      ;;   kats-cycle-position-initial-position)
      )
    (t
      (end-of-line)
      ;; (message
      ;;   "5. go to end of line text: %d"
      ;;   kats-cycle-position-initial-position)
      )))

(defun kats-swap-buffers-in-windows ()
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
    (set-window-point win2 pos1))
  ;; (other-window 1)
  )

(defun kats-delete-line ()
  (interactive)
  (delete-line))

(defun kats-move-line-up ()
  "Drag the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun kats-move-line-down ()
  "Drag the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun kats-delete-previous-word ()
  "Delete the previous word without affecting the kill ring."
  (interactive)
  (let ((start (point))
         (end (progn (backward-word) (point))))
    (delete-region end start)))

(defun kats-delete-line ()
  "Delete the curent line without affecting the kill ring."
  (interactive)
  (delete-line))

(defun kats-kill-region-or-line ()
  "Kill the current region if active, otherwise kill the current line."
  (interactive)
  (if (use-region-p)
    (kill-region (region-beginning) (region-end))
    (kill-line)))

(defun kats-kill-whole-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

;; (defun kats-do-nothing ()
;;   "Do nothing."
;;   (interactive))

(defun kats-yank-rectangle ()
  "Yank a rectangle with save-excursion"
  (interactive)
  (save-excursion
    (yank-rectangle)))

(defun kats-join-next-line ()
  (interactive)
  (save-excursion
    (move-end-of-line nil) 
    (delete-forward-char 1)))

(defun kats-always (&rest ignored) t)

(defun kats-never (&rest ignored) nil)

(defun kats-truthify(thing)
  (cond
    ((booleanp thing) thing)
    ((functionp thing) (funcall thing))
    ((boundp thing) (symbol-value thing))
    ((functionp (symbol-function thing)) (funcall (symbol-function thing)))
    (t nil)))

(defun kats-start-dired-here ()
  (interactive)
  (dired "."))

(defalias 'kats-eval-sexp-and-insert-as-comment
  (kmacro "C-M-f C-SPC C-e C-w SPC C-u C-x C-e C-r SPC <right> ; ; SPC = > SPC C-e <right>"))

(defalias 'kats-make-setq
   (kmacro "C-M-a C-e <right> <backspace> <right> s e t q SPC C-M-e <right>"))

(defun kats-close-all-parentheses ()
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

(defun kats-delete-backwards-char ()
  (interactive)
  (backward-char)
  (delete-char 1))

(defun kats-backwards-comment-sexp ()
  "Comment the sexp behind point."
  (interactive)
  (save-excursion
    (backward-sexp)
    (kats-forwards-comment-sexp)
    ))

(defun kats-forwards-comment-sexp ()
  "Comment the sexp at point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (comment-region (point) (mark))
    ))

(defun kats-find-function-at-point ()
  "Quickly find-function:"
  (interactive)
  (find-function (function-called-at-point)))

(defun kats-rename-scratch-buffer ()
  "Get rid of the *scratch* buffer"
  (interactive)
  (let ((bufname "*scratch*"))
    (when (get-buffer bufname)
      (with-current-buffer (get-buffer buffname)
        (rename-buffer "scratch")))))

(defun kats-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun re-load ()
  "Re-Load the .emacs file."
  (interactive)
  (load-file (expand-file-name "init.el" kats-config-dir)))

(defun kats-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun kats-force-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (not-modified)
  (kill-buffer (current-buffer)))

(defun flash-background ()
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

(defun kats-switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun kats-switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic functions for reading/writing colour strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rgb-list-to-hex-color-string (l)
  "Turn a list like '(69 103 137) into a string like \"#456789\"."
  (let* ( (fun (-partial #'format "%02x")))
    (apply #'concat `("#" ,@(mapcar fun l)))))

(defun -revf (fun)
  "Return a lambda that calls 𝒇 with the order of arguments reversed."
  (let ((fun fun))
    (lambda (&rest args) (eval (cons fun (nreverse args))))))

(defun hex-color-string-to-rgb-list (hex-color)
  "Turn a string like \"#456789\" into a list like '(69 103 137)."
  (let* ((l (mapcar (-compose #'1+ (-partial #'* 2) ) (number-sequence 0 2)))
	        (hex-string-to-number (-partial (-revf #'string-to-number) 16))
	        (grab-two-chars (lambda (n) (substring hex-color n (+ n 2))))
	        (fun (-compose hex-string-to-number grab-two-chars)))
    (mapcar fun l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for adjusting the luminance of color strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -mapr (l fun)
  (-map fun l))

;; (defun -maprc (l &rest funs)
;;   (-map (eval (cons #'-compose funs)) l))

(defun fmuls (x y z)
  "Multiply y by z and then shift it right by x."
  (ash (* y z) (* -1 x)))

(defalias 'clamp8
  (-compose (-partial #'min 255) (-partial #'min 255))
  "Limit values to a 0-255 range.")

(defalias 'fmuls8
  (-partial #'fmuls 8)
  "Multiply x by y and then shift it right by 8.")

(defun fmuls8-hex-color-string (hcs x)
  "Apply fmuls8 to each value in a color string like \"#aabbcc\" and return a new color string."
  (rgb-list-to-hex-color-string
    (mapcar (-compose #'clamp8 (-partial #'fmuls8 x))
      (hex-color-string-to-rgb-list hcs))))

(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun -maprcr (l &rest funs)
  "Reverse the order of funs, compose them and then map them over 𝒍."
  ( (eval (cons #'-compose (nreverse funs))) l))

(defun shades-of (hex-color-string &rest pl)
  (let ((step (or (plist-get pl :step) 16))
         (counter (plist-get :initial-step) 0)
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

(defun shades-clear ()
  (setq shades-table nil))

(defun shades-put (face property value)
  (when (not (boundp 'shades-table))
    (setq shades-table nil))
  (setq this-member (cadr (plist-member shades-table face)))
  (setq this-member (plist-put this-member property value))
  (setq shades-table (plist-put shades-table face this-member)))

(defun shades-get (face property)
  (when (not (boundp 'shades-table))
    (setq shades-table nil))
  (plist-get (cadr (plist-member shades-table face)) property))

(defun stop-auto-shades (face-name)
  (let ((timer (shades-get face-name 'timer)))
    (when timer
      (cancel-timer timer)))
  (internal-set-lisp-face-attribute face-name :foreground
    (car (shades-get face-name 'shades))))

(defun start-auto-shades (face-name hex-color-string step interval)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'kats-funs-unsorted)
