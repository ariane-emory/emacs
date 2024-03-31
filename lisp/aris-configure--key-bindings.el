;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure my key bindings:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package aris-funs--unsorted)
(use-package aris-funs--key-binding)
(use-package bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-bind a bunch of stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
     whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs thinks Mac's insert key is [help], re-bind it to DWIM:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key* [help] 'overwrite-mode (eq system-type 'darwin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unbind this so it can be a prefix:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-x C-c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind-keys:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys*
  ("s-:" . eval-expression)
  ("C-c f c" . (lambda () (interactive) (byte-recompile-directory aris-lisp-dir 0)))
  ("C-c C-e" . ignore)
  ("M-\\" . ignore)
  ("M-s M-s" . isearch-forward-thing-at-point)
  
  ;; Make kill-emacs harder to hit accidentally:
  ("C-x C-c C-c C-c" . kill-emacs)
  
  ;; Recent/temporary additions:
  ("C-x C-d" . aris-dump-key-macro)
  ("C-x C-q" . aris-make-setq)
  ("C-c C-SPC" . eval-last-sexp)

  ("C-c C-S-d" .
    (lambda ()
      "Find directly the function at point in a new tab."
      (interactive)
      (let ((symb (function-called-at-point)))
        (when symb
          (tab-bar-new-tab)
          (find-function symb)))))

  ("C-c C-d" .
    (lambda ()
      "Find directly the function at point in the current window."
      (interactive)
      (let ((symb (function-called-at-point)))
        (when symb
          (tab-bar-new-tab)
          (find-function symb)))))

  ("C-x C-SPC" .
    (lambda () (interactive) (eval-print-last-sexp) (message "Inserted sexp.")))

  ("C-x C-<backspace>" .
    (lambda () (interactive) (eval-print-last-sexp) (message "Inserted sexp.")))

  ("C-M-t" . 
    (lambda ()
      "Transpose sexp ahead of point backwards."
      (interactive)
      (transpose-sexps 1)
      (backward-sexp 2)))

  ("C-M-S-t" .
    (lambda ()
      "Transpose sexp ahead of point forwards."
      (interactive)
      (forward-sexp 2)
      (backward-sexp 1)
      (transpose-sexps 1)
      (backward-sexp 1)))
  
  (" C-x C-a" .
    (lambda ()
      (interactive)
      "Forward transform sexp into a dotted list."
      (execute-kbd-macro
        (kbd "C-s ( <right> <left> C-M-f <right> . SPC C-e"))))
  
  ("s-K" .
    (lambda ()
      (interactive)
      "Greedily kill forward sexp and devour whitespace."
      (execute-kbd-macro
        (kbd (concat
               "C-M-f C-M-b C-M-k C-M-b C-M-f "
               "C-SPC C-M-f C-M-b <backspace> SPC")))))
  
  ("M-s-<backspace>" .
    (lambda ()
      (interactive)
      "Greedily kill forward sexp and devour whitespace."
      (execute-kbd-macro
        (kbd (concat
               "C-M-f C-M-b C-M-k C-M-b C-M-f "
               "C-SPC C-M-f C-M-b <backspace> SPC")))))

  ("M-s-SPC" .
    (lambda ()
      (interactive)
      "Devour whitespace between enclosing s-exps."
      (execute-kbd-macro
        (kbd "C-M-b C-M-f C-SPC C-M-f C-M-b <backspace> SPC"))))
  
  ;; Raise sexp;
  ("C-c C-r" . raise-sexp)

  ;; Kill and revert buffers without confirmations:
  ("C-x C-k" . aris-force-kill-buffer)

  ;; Describe these:
  ("C-x C-r" . aris-revert-buffer-no-confirm)
  ("C-x C-t" . shell)
  ("C-x C-p" . aris-switch-to-last-buffer)
  ("C-/" . comment-or-uncomment-region)
  ("C-<backspace>" . aris-delete-previous-word)
  ("S-<delete>" . ignore)
  ("M-<down-mouse-1>" . ignore)

  ;; Describe:
  ("C-c C-f" . describe-function)
  ("M-s-f" . describe-function)
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
  ("C-x C-z" . eval-buffer)
  ("C-c C-e" . eval-buffer)

  ;; Copy without unselecting:
  ("s-c" . aris-ns-copy-including-secondary-keep-selection)

  ;; Close window/frame on s- so that they're hard to hit accidentally::
  ("s-w" . delete-window)

  ;; Toggle truncate lines:
  ("C-x C-l" . toggle-truncate-lines)

  ;; Move point out of  current sexp:
  ("C-x C-u" . backward-up-list)

  ;; Cycle spacing:
  ("M-SPC" . cycle-spacing)

  ;; Tab switching/opening/closing:
  ;; These are usually already bound but we are bind-key*ing them to make sure.
  ("C-<tab>" . tab-next)
  ("C-S-<tab>" . tab-previous) 
  ("s-T" . tab-bar-new-tab)
  ("s-W" . tab-bar-close-tab)

  ;; Local caps lock:
  ("M-s-c" . aris-local-caps-lock-mode)

  ;; Swap windows... undecrided whether to keep this or the next one:
  ("C-c w s s" . window-swap-states)

  ;; Swap split windows:
  ("C-x C-<tab>" . aris-swap-buffers-in-windows)

  ;; Repeat last command:
  ("C-z" . repeat)

  ;; Zap up to char:
  ("M-z" . zap-up-to-char)

  ;; Join lines:
  ("C-j" . join-line)
  ("M-j" . aris-join-next-line)

  ;; My delete-line:
  ("M-k" . aris-delete-line)
  
  ;; C-h → backspace to match Cocoa text system defaults
  ;; (clobbering somedefault describe- bindings as a result):
  ("C-h" . aris-delete-backwards-char)

  ;; Some NS-style bindings:
  ("s-a" . mark-whole-buffer)
  ("s-k" . ignore)
  ("s-p" . ignore)
  ;;("s-q" . ignore)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (bind-keys :map dired-mode-map
;;   ;; Edit filenames:
;;   ("s-q" . dired-toggle-read-only)
;;   ;; Unbind i:
;;   ("i" . ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comint:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (bind-keys :map comint-mode-map
;;   ("M-<down>" . comint-next-input)
;;   ("M-<up>" . comint-next-input)
;;   ("s-<up>" . comint-previous-input)
;;   ("s-<down>" . comint-next-input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-configure--key-bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

