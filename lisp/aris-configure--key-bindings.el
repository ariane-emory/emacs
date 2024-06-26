;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure my key bindings:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package aris-funs--unsorted)
(use-package aris-funs--key-binding)
(use-package bind-key)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-bind a bunch of stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(aris-bind-pairs-with-prefix 'describe
  '(bindings char function key mode symbol variable))

(aris-bind-pairs-with-prefix 'customize
  '(face group mode theme variable))

(aris-bind-pairs-with-prefix 'rename
  '(buffer uniquely))

(aris-bind-pairs-with-prefix 'package
  '(install list-packages refresh-contents))

(mapc 'aris-bind-to-initials-with-prefix
  '( aggressive-indent-mode
     artist-mode
     auto-revert-mode
     copilot-mode
     follow-mode
     glasses-mode
     global-centered-cursor-mode
     global-hl-line-mode
     idle-highlight-global-mode
     indent-buffer
     make-frame
     nov-mode
     raise-sexp
     re-load
     reverse-region
     variable-pitch-mode
     view-lossage
     whitespace-mode
     zoom-mode))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs thinks Mac's insert key is [help], re-bind it to DWIM:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key* [help] 'overwrite-mode (eq system-type 'darwin))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unbind this so it can be a prefix:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-x C-c"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind-keys:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys*
  ("M-s-g" . goto-char)
  ("M-s-t" . trace-function)

  ;; Ignore these:
  ("C-x C-n" . ignore)
  ("C-x C-<left>" . ignore)
  ("C-x C-<right>" . ignore)
  ("C-x C-<up>" . ignore)
  ("C-x C-<down>" . ignore)

  ;; Ignore these with a message:
  ("C-c C-<escape>" . (lambda () (interactive) (message "Cancelled.")))
  ("C-x C-<escape>" . (lambda () (interactive) (message "Cancelled.")))

  ;; Delete previous word:
  ("C-<backspace>" . aris-delete-previous-word)

  ;; Insert macro expansion:
  ("C-s-z" .
    (lambda ()
      (interactive)
      (pp-macroexpand-last-sexp t) ;; (insert "\n")
      (message "Inserted macro.")))
  
  ;; ("C-x C-e" . eval-last-sexp) ;; Default, don't actually bind.
  ("C-c C-<backspace>" .
    (lambda ()
      (interactive)
      (let ((old-lisp-indent-offset lisp-indent-offset))
        (setq lisp-indent-offset 1)
        (unwind-protect
          ;; (eval-last-sexp t)
          (pp-eval-last-sexp t)
          ;; (insert "\n")
          (message "Inserted sexp.")
          (setq lisp-indent-offset old-lisp-indent-offset)))))

  ;; Join lines:
  ("C-j".  (lambda () (interactive) (join-line) (beginning-of-line-text)))
  ;; ("C-j" . join-line)
  ("M-j" . (lambda () (interactive) (aris-join-next-line) (end-of-line)))
  
  ;; Find function at point in current window:
  ("C-c C-d" .
    (lambda ()
      "Find directly the function at point in the current window."
      (interactive)
      (let ((symb (function-called-at-point)))
        (when symb
          (tab-bar-new-tab)
          (find-function symb)))))

  ;; Greedily kill forward sexp and devour whitespace:
  ;; ("s-K" .
  ;;   (lambda ()
  ;;     (interactive)
  ;;     "Greedily kill forward sexp and devour whitespace."
  ;;     (execute-kbd-macro
  ;;       (kbd (concat
  ;;              "C-M-f C-M-b C-M-k C-M-b C-M-f "
  ;;              "C-SPC C-M-f C-M-b <backspace> SPC")))))

  ;; Greedily kill forward sexp and devour whitespace:
  ;; ("M-s-<backspace>" .
  ;;   (lambda ()
  ;;     (interactive)
  ;;     "Greedily kill forward sexp and devour whitespace."
  ;;     (execute-kbd-macro
  ;;       (kbd (concat
  ;;              "C-M-f C-M-b C-M-k C-M-b C-M-f "
  ;;              "C-SPC C-M-f C-M-b <backspace> SPC")))))

  ;; Devour whitespace between enclosing s-exps:
  ;; ("M-s-SPC" .
  ;;   (lambda ()
  ;;     (interactive)
  ;;     "Devour whitespace between enclosing s-exps."
  ;;     (execute-kbd-macro
  ;;       (kbd "C-M-b C-M-f C-SPC C-M-f C-M-b <backspace> SPC"))))

  ;; Forward transform sexp into a dotted list:
  (" C-x C-a" .
    (lambda ()
      (interactive)
      "Forward transform sexp into a dotted list."
      (execute-kbd-macro
        (kbd "C-s ( <right> <left> C-M-f <right> . SPC C-e"))))
  
  ;; Transpose sexp forwards:
  ("C-M-S-t" .
    (lambda ()
      "Transpose sexp ahead of point forwards."
      (interactive)
      (condition-case nil
        (progn
          (forward-sexp 2)
          (backward-sexp 1)
          (transpose-sexps 1)
          (backward-sexp 1))
        (scan-error (message "Can't transpose")))))

  ;; Transpose sexp backwards:
  ("C-M-t" . 
    (lambda ()
      "Transpose sexp ahead of point backwards."
      (interactive)
      (condition-case nil
        (progn
          (transpose-sexps 1)
          (backward-sexp 2))
        (scan-error (message "Can't transpose")))))
  ;; ;; Transpose sexp forwards:
  ;; ("C-M-S-t" .
  ;;   (lambda ()
  ;;     "Transpose sexp ahead of point forwards."
  ;;     (interactive)
  ;;     (forward-sexp 2)
  ;;     (backward-sexp 1)
  ;;     (transpose-sexps 1)
  ;;     (backward-sexp 1)))
  
  ;; ;; Transpose sexp backwards:
  ;; ("C-M-t" . 
  ;;   (lambda ()
  ;;     "Transpose sexp ahead of point backwards."
  ;;     (interactive)
  ;;     (transpose-sexps 1)
  ;;     (backward-sexp 2)))
  
  ;; Dump last key macro:
  ("C-x C-d" . aris-dump-key-macro)

  ;; isearch-forward-thing-at-point:
  ("M-s M-s" . isearch-forward-thing-at-point)
  
  ;; eval-expression with either hand:
  ("s-:" . eval-expression)

  ;; Make kill-emacs harder to hit accidentally:
  ("C-x C-c C-c C-c" . kill-emacs)
  
  ;; Raise sexp;
  ("C-c C-r" . raise-sexp)

  ;; Revert and kill buffers without confirmations:
  ("C-x C-k" . aris-force-kill-buffer)

  ;; Revert buffer without confirmation:
  ("C-x C-r" . aris-revert-buffer-no-confirm)

  ;; Shell:
  ("C-x C-t" . shell)

  ;; Switch to last buffer:
  ("C-x C-p" . aris-switch-to-last-buffer)

  ;; Comment/uncomment region:
  ("C-/" . comment-or-uncomment-region)

  ;; Describe these:
  ;; ("s-<backspace>" . macrostep-mode)
  ;; ("s-m" . macrostep-mode)
  ;; ("M-m" . macrostep-mode)
  ("C-c C-e" . ignore)
  ("M-\\" . ignore)
  ("S-<delete>" . ignore)
  ;; ("M-<down-mouse-1>" . ignore)

  ;; describe things:
  ("C-c C-f" . describe-function)
  ("M-s-f" . describe-function)
  ("C-c C-v" . describe-mode)
  ("M-s-v" . describe-variable)
  ("C-c C-k" . describe-key)
  ("M-s-k" . describe-key)
  ("C-c RET" . describe-mode)
  ("M-s-m" . describe-mode)
  ;;("C-c C-c" . describe-char)
  ("M-s-c" . describe-char)

  ;; Cycle position:
  ("C-a" . aris-cycle-position-back)
  ("C-e" . aris-cycle-position-forward)

  ;; Beginning/end of line
  ("C-M-a" . beginning-of-line)
  ("C-M-e" . end-of-line)

  ;; Move lines up and down:
  ("<M-s-up>" . aris-move-line-up) 
  ("<M-s-down>" . aris-move-line-down) 

  ;; eval buffer:
  ;; ("C-x C-z" . aris-eval-buffer)
  ;; ("C-x C-n" . aris-eval-buffer)
  ("C-x RET" . aris-eval-buffer)
  ("C-x m" . aris-eval-buffer)

  ;; Copy without unselecting:
  ("s-c" . aris-ns-copy-including-secondary-keep-selection)

  ;; Close window/frame on s- so that they're hard to hit accidentally:
  ("s-w" . (lambda () (interactive)
             (if (one-window-p)
               (message "Cannot delete the sole window.")
               (delete-window))))

  ;; Toggle truncate lines:
  ("C-x C-l" . toggle-truncate-lines)

  ;; Move point out of  current sexp:
  ("C-x C-u" . backward-up-list)

  ;; Cycle spacing:
  ("M-SPC" . cycle-spacing)

  ;; Tab switching/opening/closing:
  ;; These are usually already bound but we are bind-key*ing them to make sure
  ;; that they are bound in all modes.
  ("C-<tab>" . tab-next)
  ("C-S-<tab>" . tab-previous) 
  ("s-T" . tab-bar-new-tab)
  ("s-W" . tab-bar-close-tab)

  ;; Local caps lock:
  ("M-s-c" . aris-local-caps-lock-mode)

  ;; Swap windows... undecrided whether to keep this or the next one:
  ;;("C-c w s s" . window-swap-states)

  ;; Swap split windows:
  ;; ("C-x C-<tab>" . aris-swap-buffers-in-windows)

  ;; Repeat last command:
  ("C-z" . repeat)

  ;; Zap up to char:
  ("M-z" . zap-up-to-char)

  ;; My delete-line:
  ("M-k" . aris-delete-line)

  ;; C-h → backspace to match Cocoa text system defaults
  ;; (clobbering some default describe- bindings as a result):
  ("C-h" . aris-delete-backwards-char)

  ;; Some NS-style bindings:
  ("s-a" . mark-whole-buffer)
  ("s-k" . ignore)
  ("s-p" . ignore)
  ("s-q" . ignore)
  ("s-t" . ignore)
  ("s-n" . ignore)
  ("s-o" . find-file)
  ("s-r" . query-replace)

  ;; Start dired here:
  ("C-x C-j" . aris-start-dired-here)

  ;; Rectangles:
  ("M-s-o" . open-rectangle)
  ("M-s-p" . kill-rectangle)
  ("M-s-y" . aris-yank-rectangle)

  ;; Describe these:
  ("M-s-u" . rename-uniquely)
  ("M-s-i" . ielm)
  ("M-s-n" . narrow-to-region)
  ("M-s-m" . widen)
  ("M-s-SPC" . aris-goto-nonwhite)

  ;; Mess with sexps:
  ("C-c i c" . aris-eval-sexp-and-insert-as-comment)
  ("C-c c s" . aris-forwards-comment-sexp)
  ("C-c b c s" . aris-backwards-comment-sexp)
  ("C-c C-s" . aris-forwards-comment-sexp)

  ;; Find function at point:
  ("C-c f f" . aris-find-function-at-point)

  ;; Find other file:
  ("C-c f o f" . ff-find-other-file)

  ;; Close parens:
  ("C-c a p" . aris-close-all-parentheses)

  ;; Sort lines:
  ("C-c o l" . sort-lines)

  ;; Browse URL:
  ("C-c x b" . xwidget-webkit-browse-url)

  ;; Redox thumb kill keys:
  ("C-k" . aris-kill-region-or-line)
  ("C-M-s-k" . aris-kill-whole-line)

  ;; Forward/back paragraphs:
  ("C-M-<right>" . forward-paragraph)
  ("C-M-<left>" . backward-paragraph))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-configure--key-bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

