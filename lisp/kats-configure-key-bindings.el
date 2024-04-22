;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure my key bindings:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'kats-funs-unsorted)
(require 'kats-funs-key-binding)

;; ================================================================================
;; Manually rebind some stuff:
;; ================================================================================
;; "I forget what this does?"
;; (let ((frame (framep (selected-frame))))
;;   (or (eq  t frame)
;;     (eq 'pc frame)
;;     (define-key input-decode-map 
;;       (kbd "C-[") 
;;       [control-bracketleft])))

;; ================================================================================
;; Auto-bind a bunch of stuff:
;; ================================================================================

(kats-bind-pairs-with-prefix 'describe
  '(bindings char function key mode symbol variable))

(kats-bind-pairs-with-prefix 'customize
  '(face group mode theme variable))

(kats-bind-pairs-with-prefix 'rename
  '(buffer uniquely))

(kats-bind-pairs-with-prefix 'package
  '(install list-packages refresh-contents))

(mapcar 'kats-bind-to-initials-with-prefix
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

(setq kats-auto-bindings
  (cl-remove-if #'stringp 
    '(
       ("C-x C-q" kats-make-setq)

       "Make kill-emacs much harder to hit accidentally:"
       ("C-x C-c" nil) ;; must be nil, not ignore, since we're turning the sequence into a prefix!
       ("C-x C-c C-c C-c" kill-emacs)

       "Raise sexp:"
       ("C-c C-r" raise-sexp)
       
       "Kill and revert buffers without confirmations:"
       ("C-x C-k" kats-force-kill-buffer)

       "Describe these:"
       ("C-x C-r" kats-revert-buffer-no-confirm)
       ("C-x C-t" shell)
       ("C-x C-p" kats-switch-to-last-buffer)
       ("C-/" comment-or-uncomment-region)
       ("C-<backspace>" kats-delete-previous-word)
       ("S-<delete>" ignore)
       ("M-<down-mouse-1>" ignore)
       
       "Describe:"
       ("C-c C-f" describe-function)
       ("C-c C-k" describe-key)
       ("M-s-f" describe-function)
       ("M-s-k" describe-key)
       ("C-c RET" describe-mode)
       ("M-s-m" describe-mode)
       ("C-c C-c" describe-char)
       ("M-s-c" describe-char)
       
       "Cycle position:"
       ("C-a" kats-cycle-position-back)
       ("C-e" kats-cycle-position-forward)

       "Beginning/end of line"
       ("C-M-a" beginning-of-line)
       ("C-M-e" end-of-line)

       "Move lines up and down:"
       ("<M-s-up>" kats-move-line-up) 
       ("<M-s-down>" kats-move-line-down) 

       "Eval buffer:"
       ("C-x C-z" eval-buffer)
       
       "Copy without unselecting:"
       ("s-c" kats-ns-copy-including-secondary-keep-selection)

       "Close window/frame on s- so that they're hard to hit accidentally.:"
       ("s-w" delete-window)

       "Toggle truncate lines:"
       ("C-x C-l" toggle-truncate-lines)

       "Move point out of  current sexp:"
       ("C-x C-u" backward-up-list)

       "Cycle spacing:"
       ("M-SPC" cycle-spacing)
       
       "Tab switching/opening/closing:"
       ("C-<tab>" tab-next)
       ("C-S-<tab>" tab-previous) 
       ("s-T" tab-bar-new-tab)
       ("s-W" tab-bar-close-tab)

       "Local caps lock:"
       ("M-s-c" kats-local-caps-lock-mode)
       
       "Swap windows... undecrided whether to keep this or the next one:"
       ("C-c w s s" window-swap-states)

       "Swap split windows:"
       ("C-x C-<tab>" kats-swap-buffers-in-windows)

       "Repeat last command:"
       ("C-z" repeat)

       "Zap up to char:"
       ("M-z" zap-up-to-char)

       "Join lines:"
       ("C-j" join-line)
       ("M-j" kats-join-next-line)

       "My delete-line:"
       ("M-k" kats-delete-line)

       "C-h → backspace to match Cocoa text system defaults (some clobbering default describe- bindings as a result):"
       ("C-h" kats-delete-backwards-char)

       "Some NS-style bindings:"
       ("s-a" mark-whole-buffer)
       ("s-k" ignore)
       ("s-p" ignore)
       ("s-q" ignore)
       ("s-t" ignore)
       ("s-n" ignore)
       ("s-o" find-file)
       ("s-r" query-replace)

       "emacs thinks Mac's insert key is [help], re-bind it to DWIM:"
       ([help] overwrite-mode :when (lambda () (eq system-type 'darwin)))

       "Start dired here:"
       ("C-x C-j" kats-start-dired-here)

       "Edit filenames in dired:"
       ("s-q" dired-toggle-read-only :map dired-mode-map)

       "Unbind i in dired:"
       ("i" nil :map dired-mode-map)

       "No q in Info:"
       ("q" ignore :map Info-mode-map)

       "comint:"
       ("M-<down>" comint-next-input :map comint-mode-map)
       ("M-<up>" comint-next-input :map comint-mode-map)
       ("s-<up>" comint-previous-input :map comint-mode-map)
       ("s-<down>" comint-next-input :map comint-mode-map)

       "Copilot:"
       ("S-<return>" copilot-next-completion :map copilot-completion-map :when (lambda () (featurep 'copilot)))
       ("SPC" copilot-next-completion :map copilot-completion-map :when (lambda () (featurep 'copilot)))
       ("TAB" ignore :map copilot-completion-map :when (lambda () (featurep 'copilot)))
       ("C-<return>" copilot-accept-completion :map copilot-completion-map :when (lambda () (featurep 'copilot)))
       ("M-<return>" copilot-accept-completion-by-word :map copilot-completion-map :when (lambda () (featurep 'copilot)))

       "Rectangles:"
       ("M-s-o" open-rectangle)
       ("M-s-p" kill-rectangle)
       ("M-s-y" kats-yank-rectangle)

       ("M-s-u" rename-uniquely)
       ("M-s-i" ielm)
       ("M-s-n" narrow-to-region)
       ("M-s-m" widen)

       "Mess with sexps:"
       ("C-c i c" kats-eval-sexp-and-insert-as-comment)
       ("C-c c s" kats-forwards-comment-sexp)
       ("C-c b c s" kats-backwards-comment-sexp)
       ("C-c C-s" kats-forwards-comment-sexp)

       "Find function at point:"
       ("C-c f f" kats-find-function-at-point)

       "Find other file:"
       ("C-c f o f" ff-find-other-file)

       "Close parens:"
       ("C-c a p" kats-close-all-parentheses)
       
       "Sort lines:"
       ("C-c o l" sort-lines)

       "Browse URL:"
       ("C-c x b" xwidget-webkit-browse-url)
       
       "Redox thumb kill keys:"
       ("C-k" kats-kill-region-or-line)
       ("C-M-s-k" kats-kill-whole-line)

       "Forward/back paragraphs:"
       ("C-M-<right>" forward-paragraph)
       ("C-M-<left>" backward-paragraph)
       )))

(mapcar (lambda (x) (apply #'kats-auto-bind x)) kats-auto-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'kats-configure-key-bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

