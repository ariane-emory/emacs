;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package aris-funs-with-messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey-patch a couple of functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-messages  "applying monkey patches"
  (defun rainbow-colorize-match (color &optional match)
    "Tamper with this function from rainbow-mode so that it plays nice with
 hl-line mode: Return a matched string propertized with a face whose
 background is COLOR. The foreground is computed using
 `rainbow-color-luminance', and is either white or black."
    (let ((match (or match 0)))
      (if (< 0.25 (rainbow-x-color-luminance color))
        ;; bright colors
        (put-text-property
          (match-beginning match)
          (match-end match)
          'face `((:foreground ,color)
                   ;; (:background "#white ")
                   ))
        ;; dim colors
        (put-text-property
          (match-beginning match)
          (match-end match)
          'face `((:foreground "gray70")
                   (:background ,color))))))

  (when nil
    (rainbow-x-color-luminance "Red")
    (rainbow-x-color-luminance "#f40")
    (rainbow-x-color-luminance "#74cfa8")
    (rainbow-x-color-luminance "#104050")

    (mapcar (lambda (color)  (> 0.5 (rainbow-x-color-luminance color)))
      '( "gray10" "gray20" "gray30" "gray40"
         "gray50" "gray60" "gray70" "gray80")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun desktop-save (dirname &optional release only-if-changed version)
    "Monkey patch tthis to be less cautious about overwwriting desktop files."
    (interactive (list
                   ;; Or should we just use (car desktop-path)?
                   (let ((default (if (member "." desktop-path)
                                    default-directory
                                    user-emacs-directory)))
                     (read-directory-name "Directory to save desktop file in: "
                       default default t))
                   nil
                   nil
                   current-prefix-arg))
    (setq desktop-dirname
      (file-name-as-directory (expand-file-name dirname)))
    (save-excursion
      (let ((eager desktop-restore-eager)
	           (new-modtime (file-attribute-modification-time
			                      (file-attributes (desktop-full-file-name)))))
        (when t
	        ;; (or (not new-modtime)		; nothing to overwrite
	        ;;   (time-equal-p desktop-file-modtime new-modtime)
	        ;;   (unless release (error "Desktop file conflict")))

	        ;; If we're done with it, release the lock.
	        ;; Otherwise, claim it if it's unclaimed or if we created it.
	        (if release
	          (desktop-release-lock)
	          (unless (and new-modtime (desktop-owner)) (desktop-claim-lock)))

          ;; What format are we going to write the file in?
          (setq desktop-io-file-version
            (cond
              ((equal version '(4))
                (if (or (eq desktop-io-file-version 208)
                      (yes-or-no-p "Save desktop file in format 208 \
\(Readable by Emacs 25.1 and later only)? "))
                  208
                  (or desktop-io-file-version desktop-native-file-version)))
              ((equal version '(16))
                (if (or (eq desktop-io-file-version 206)
                      (yes-or-no-p "Save desktop file in format 206 \
\(Readable by all Emacs versions since 22.1)? "))
                  206
                  (or desktop-io-file-version desktop-native-file-version)))
              ((memq version '(206 208))
                version)
              ((null desktop-io-file-version) ; As yet, no desktop file exists.
                desktop-native-file-version)
              (t
                desktop-io-file-version)))

	        (with-temp-buffer
	          (insert
	            ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
	            desktop-header
	            ";; Created " (current-time-string) "\n"
	            ";; Desktop file format version " (format "%d" desktop-io-file-version) "\n"
	            ";; Emacs version " emacs-version "\n")
	          (save-excursion (run-hooks 'desktop-save-hook))
	          (goto-char (point-max))
	          (insert "\n;; Global section:\n")
	          ;; Called here because we save the window/frame state as a global
	          ;; variable for compatibility with previous Emacsen.
	          (desktop-save-frameset)
	          (unless (memq 'desktop-saved-frameset desktop-globals-to-save)
	            (desktop-outvar 'desktop-saved-frameset))
	          (mapc #'desktop-outvar desktop-globals-to-save)
	          (setq desktop-saved-frameset nil) ; after saving desktop-globals-to-save
	          (when (memq 'kill-ring desktop-globals-to-save)
	            (insert
	              "(setq kill-ring-yank-pointer (nthcdr "
	              (int-to-string (- (length kill-ring) (length kill-ring-yank-pointer)))
	              " kill-ring))\n"))

	          (insert "\n;; Buffer section -- buffers listed in same order as in buffer list:\n")
	          (dolist (l (mapcar #'desktop-buffer-info (buffer-list)))
	            (let ((base (pop l)))
	              (when (apply #'desktop-save-buffer-p l)
		              (insert "("
		                (if (or (not (integerp eager))
			                    (if (zerop eager)
			                      nil
			                      (setq eager (1- eager))))
		                  "desktop-create-buffer"
		                  "desktop-append-buffer-args")
		                " "
		                (format "%d" desktop-io-file-version))
		              ;; If there's a non-empty base name, we save it instead of the buffer name
		              (when (and base (not (string= base "")))
		                (setcar (nthcdr 1 l) base))
		              (dolist (e l)
		                (insert "\n  " (desktop-value-to-string e)))
		              (insert ")\n\n"))))

	          (setq default-directory desktop-dirname)
	          ;; When auto-saving, avoid writing if nothing has changed since the last write.
	          (let* ((beg (and only-if-changed
			                    (save-excursion
			                      (goto-char (point-min))
			                      ;; Don't check the header with changing timestamp
			                      (and (search-forward "Global section" nil t)
			                        ;; Also skip the timestamp in desktop-saved-frameset
			                        ;; if it's saved in the first non-header line
			                        (search-forward "desktop-saved-frameset"
			                          (line-beginning-position 3) t)
			                        ;; This is saved after the timestamp
			                        (search-forward (format "%S" desktop--app-id) nil t))
			                      (point))))
		                (checksum (and beg (md5 (current-buffer) beg (point-max) 'utf-8-emacs))))
	            (unless (and checksum (equal checksum desktop-file-checksum))
	              (let ((coding-system-for-write 'utf-8-emacs))
		              (write-region (point-min) (point-max) (desktop-full-file-name) nil 'nomessage))
	              (setq desktop-file-checksum checksum)
	              ;; We remember when it was modified (which is presumably just now).
	              (desktop--get-file-modtime))))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun buffer-menu--display-help () nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun persistent-scratch-save (&optional file)
    "Monkey patch this to only save if modified."
    (interactive)
    (let ((my-scratch-buf
            (get-buffer
              (if (boundp 'aris-scratch-buffer-name) aris-scratch-buffer-name "*scratch*"))))
      (when (buffer-modified-p my-scratch-buf)
        (let* ((actual-file (or file persistent-scratch-save-file))
                (tmp-file (concat actual-file ".new"))
                (saved-state (persistent-scratch--save-buffers-state)))
          (let ((old-umask (default-file-modes)))
            (set-default-file-modes #o600)
            (unwind-protect
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region (cdr saved-state) nil tmp-file nil 0))
              (set-default-file-modes old-umask)))
          (run-hook-with-args 'persistent-scratch-before-save-commit-functions tmp-file)
          (rename-file tmp-file actual-file t)
          (dolist (buffer (car saved-state))
            (with-current-buffer buffer
              (set-buffer-modified-p nil))))
        (unless file
          (persistent-scratch--update-backup)
          (persistent-scratch--cleanup-backups)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (provide 'aris-funs-monkey-patched))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
