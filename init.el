;; -*- fill-column: 80; lexical-binding: nil; lisp-indent-offset: 2; -*-
(message "[ARI] Loading init.el...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
(cl-tagbody   
  (defun bisect ()
    "Bisect for debugging .emacs file errors. Keep this in init.el so it's always available:"
    (message "Bisecting...")
    (go config-end))

  (setq debug-on-error t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Setup config dir paths:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Set the right config directory if we're on Windows:"
  (setq aris-config-dir
    (expand-file-name
      (if (eq system-type 'windows-nt)
        (expand-file-name "AppData\\Roaming\\.emacs.d\\"
          (getenv "USERPROFILE"))
        "~/.emacs.d/")))

  "Set my lisp directory and add it to the load path:"
  (setq aris-lisp-dir (expand-file-name "lisp/" aris-config-dir))
  (add-to-list 'load-path aris-lisp-dir)

  "If trav.el's directory exists, add it to the load-path:"
  (setq aris-trav-dir (expand-file-name "trav-el" aris-lisp-dir))
  (add-to-list 'load-path aris-trav-dir)

  (add-to-list 'exec-path "/opt/homebrew/bin/dotnet")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (setenv "PATH" (concat "~/.cargo/bin:" (getenv "PATH")))
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
  (setenv "DOTNET_ROOT" "/usr/local/share/dotnet")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load theme:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Load my toxikat theme from aris-config dir:"
  (add-to-list 'custom-theme-load-path aris-config-dir)
  (load-theme 'toxikat t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Switch to fullscreen and turn off scrollbar:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (sit-for 0.1)
  (or (cdr (assoc 'fullscreen (frame-parameters)))
    (toggle-frame-fullscreen))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; We'r going to use this all over, so use it early:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package aris-funs--with-messages)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (with-messages "loading my settings"
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Set up the environment:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "preparing global variables" 
      (setq ansi-color-names-vector
        ["black" "red3" "green3" "yellow3" "Cyan" "magenta3" "cyan3" "gray90"])
      (setq auto-revert-interval 1)
      (setq auto-save-default nil)
      (setq completions-format 'vertical)
      (setq create-lockfiles nil)
      (setq cursor-in-non-selected-windows nil)
      (setq custom-file (expand-file-name "custom.el" aris-config-dir))
      (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
      (setq eval-expression-print-length nil)
      (setq eval-expression-print-level nil)
      (setq explicit-shell-file-name "/bin/bash")
      (setq find-file-existing-other-name nil)
      (setq find-function-C-source-directory
        (expand-file-name "~/Code/3p/emacs/src"))
      (setq follow-mode-line-text " ")
      (setq frame-resize-pixelwise t)
      (setq gc-cons-threshold 3200000)
      (setq ielm-prompt #("🐈> " 0 3 (field output)))
      (setq image-auto-resize 'fit-window)
      (setq image-dired-cmd-create-thumbnail-program "/opt/homebrew/bin/convert")
      (setq inhibit-startup-buffer-menu t)
      (setq inhibit-startup-echo-area-message (substitute-in-file-name "$USER"))
      (setq inhibit-startup-screen t)
      (setq initial-scratch-message "")
      (setq isearch-lax-whitespace nil)
      (setq isearch-wrap-pause 'no)
      (setq js-indent-level 2)      
      (setq large-file-warning-threshold 100000000)
      (setq lisp-indent-offset 2)
      (setq make-backup-files nil)
      (setq max-mini-window-height 0.2)
      (setq messages-buffer-max-lines t)
      (setq ns-pop-up-frames nil)
      (setq next-screen-context-lines 0)
      (setq ns-pop-up-frames nil)
      (setq print-length nil) 
      (setq print-level nil)
      (setq read-process-output-max 16384)
      (setq ring-bell-function 'ignore)
      (setq scroll-preserve-screen-position t)
      (setq source-directory (expand-file-name "~/Code/3p/emacs/src"))
      (setq text-scale-mode-step 1.025)
      (setq tooltip-use-echo-area nil)
      (setq undo-outer-limit 200000000)
      (setq use-dialog-box nil)
      (setq visible-bell t)
      (setq display-buffer-alist
	      '(("\\*Buffer List\\*" display-buffer-same-window
	          (nil))
	         ("\\*Backtrace\\*" display-buffer-same-window
	           (nil))
	         ("\\*Compile-Log\\*" display-buffer-same-window
	           (nil))
	         (".*\\.el" display-buffer-same-window
	           (nil))
	         (".*\\.c" display-buffer-same-window
	           (nil))
	         (".*\\.h" display-buffer-same-window
	           (nil))
	         ("*Help*" display-buffer-same-window
	           (nil)))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Customize other properties/functions/lists:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "putting some properties"
      "Confirmation prompts accept y/n instead of yes/no:"
      (fset 'yes-or-no-p 'y-or-n-p)
      "I forget why I do this:"
      (put 'lisp-indent-function 'safe-local-variable 'integerp)
      "Enable the erase-buffer function:"
      (put 'erase-buffer 'disabled nil)
      "Enable the narrow-to-region function:"
      (put 'narrow-to-region 'disabled nil))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Define a face
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defface aris-alt-face '((t (:foreground "#c09000"))) "Orangeish")
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Set up package manager and install/require packages:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (use-package-with-messages aris-configure--packages)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; use aris-funs-setup-lisp early because so many other
    ;;   packages will try to call into it:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (use-package-with-messages aris-funs--setup-lisp :demand)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; use-packages (built-in):
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "using built-in packages" 
      (use-package-with-message abbrev :diminish abbrev-mode
        :config
        (setq only-global-abbrevs t))

      (use-package-with-message comint
        :bind
        (:map comint-mode-map
          ("M-<down>" . comint-next-input)
          ("M-<up>" . comint-next-input)
          ("s-<up>" . comint-previous-input)
          ("s-<down>" . comint-next-input))
        :hook
        (comint-exec .
          (lambda ()
            "Let the shell be killed without warning about running processes."
            (set-process-query-on-exit-flag
              (get-buffer-process (current-buffer)) nil))))

      (use-package-with-message csharp-mode) ;; built-in

      (use-package-with-message delsel
        :init
        (delete-selection-mode 1))
      
      (use-package-with-message desktop
        :init
        (setq desktop-auto-save-timeout 5)
        (setq desktop-load-locked-desktop t)
        (setq desktop-dirname aris-config-dir)
        (setq desktop-save t)
        (setq desktop-minor-mode-table
          '((defining-kbd-macro nil)
             (isearch-mode nil)
             (vc-mode nil)
             (vc-dir-mode nil)
             (erc-track-minor-mode nil)
             (savehist-mode nil)
             (company-posframe-mode nil)))
        :config
        (desktop-save-mode 1)
        (add-hook 'kill-emacs-hook
          (lambda () (desktop-save-in-desktop-dir))))
      
      (use-package-with-message dired
        :init
        (setq dired-listing-switches "-alhG")
        (setq dired-auto-revert-buffer t)
        (setq dired-use-ls-dired nil)
        (setq ls-lisp-dirs-first t)
        (setq ls-lisp-ignore-case t)
        (setq ls-lisp-use-insert-directory-program nil)
        (setq ls-lisp-use-localized-time-format t)
        (setq ls-lisp-use-string-collate nil)
        (setq ls-lisp-verbosity '(links))
        :hook
        (dired-mode .
          (lambda ()
            (auto-revert-mode 1)
            (face-remap-add-relative 'default '(:foreground "#c90"))))
        :bind
        (:map dired-mode-map
          ("i" . ignore)
          ("o" .
            (lambda ()
              (interactive)
              (let ((file (dired-get-file-for-visit)))
                (start-process "default-app" nil "open" file))))
          ([mouse-2] .
            (lambda (event)
              "In Dired, visit the file or directory name you click on in the same window."
              (interactive "e")
              (let (window pos file)
                (save-excursion
                  (setq window (posn-window (event-end event))
                    pos (posn-point (event-end event)))
                  (if (not (windowp window))
                    (error "No file chosen"))
                  (set-buffer (window-buffer window))
                  (goto-char pos)
                  (setq file (dired-get-file-for-visit)))
                (if (file-directory-p file)
                  (or (and (cdr dired-subdir-alist)
                        (dired-goto-subdir file))
                    (progn
                      (select-window window)
                      (dired file)))
                  (select-window window)
                  (find-file (file-name-sans-versions file t))))))))

      (use-package-with-message display-line-numbers
        :init
        (setq display-line-numbers-widen t)
        :config
        (global-display-line-numbers-mode 1))

      (use-package-with-message display-fill-column-indicator
        :hook
        (prog-mode . display-fill-column-indicator-mode))

      (use-package-with-message eldoc
        :diminish (eldoc-mode . "ed")
        :init
        (setq eldoc-echo-area-prefer-doc-buffer t)
        (setq eldoc-idle-delay 0.1))

      (use-package-with-message frame
        :init
        (setq blink-cursor-blinks -1)
        (setq blink-cursor-delay 0.0)
        (setq blink-cursor-interval 0.1))
      
      (use-package-with-message elisp-mode
        :bind
        ("C-c ." . (lambda () (interactive) (pp-macroexpand-last-sexp t)))
        :hook
        (emacs-lisp-mode .
          (lambda ()
            ;;(setq-local lexical-binding t)
	          (eldoc-mode 1)
            (aris-setup-lisp))))

      (use-package-with-message face-remap
        :diminish (buffer-face-mode text-scale-mode))

      (use-package-with-message lisp-mode
        :hook  j
        (lisp-mode .
          (lambda ()
            (font-lock-add-keywords nil
	            '( ;; AELisp's special forms are keywords.
                 ("(\\(apply\\_>\\)" . 1)
                 ("(\\(case\\_>\\)"  . 1)
                 ("(\\(decr\\_>\\)"  . 1)
                 ("(\\(incr\\_>\\)"  . 1)
                 ("(\\(letrec\\_>\\)" . 1)
                 ("(\\(pop\\_>\\)"  . 1)
                 ("(\\(push\\_>\\)"  . 1)
                 ("(\\(quote\\_>\\)"  . 1)
                 ("(\\(repeat\\_>\\)"  . 1)
                 ("(\\(setq\\_>\\)"  . 1)
                 ("(\\(until\\_>\\)" . 1))
	            'prepend))))

      (use-package-with-message paren
        :init
        (setq show-paren-mode t)
        (setq source-directory (expand-file-name "~/Code/3p/emacs/src")))

      (use-package-with-message pp :demand)

      (use-package-with-message sh-script
        :diminish shell-script-mode)

      (use-package-with-message shell
        :diminish shell-mode
        :bind
        (:map shell-mode-map
          ("C-c C-f" . describe-function))
        :hook
        (shell-mode .
          (lambda ()
            (setq-local tab-width 8)
            (display-line-numbers-mode -1)
            (face-remap-add-relative 'default 'aris-alt-face))))

      (use-package-with-message simple
        :config
        (column-number-mode 1))
      
      (use-package-with-message tab-bar
        :init
        (setq tab-bar-close-button-show 'selected)
        (setq tab-bar-new-button-show nil)
        (setq tab-bar-new-tab-choice 'list-buffers)
        (setq tab-bar-show t)
        :config
        (tab-bar-mode 1))

      (use-package-with-message time
        :config
        (setq display-time-default-load-average nil)
        (display-time)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; use-packages (ELPA/MELPA/GNU):
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "using 3rd party packages"
      (use-package-with-message aggressive-indent :ensure t :demand
        :diminish aggressive-indent-mode
        :init
        (setq aggressive-indent-excluded-modes
          '( elm-mode haskell-mode inf-ruby-mode makefile-mode
             makefile-gmake-mode python-mode sql-interactive-mode text-mode
             yaml-mode Shell-script-mode Shell-script shell-mode
             customize-mode)))

      (mapc (lambda (pkg) (eval `(use-package-with-message ,pkg :ensure t)))
        '( use-package ac-inf-ruby adjust-parens clhs coffee-mode company-box 
           dash devdocs devdocs-browser diminish editorconfig f
           flycheck-inline flycheck-rust macrostep markdown-mode 
           paredit platformio-mode rust-mode swift-mode which-key))

      (use-package-with-message beacon :ensure t
        :init
        (setq beacon-blink-delay 0.2)
        (setq beacon-blink-duration 1)
        (setq beacon-blink-when-point-moves-horizontally 1)
        (setq beacon-blink-when-point-moves-vertically 1)
        (setq beacon-color "#f00")
        (setq beacon-lighter "")
        (setq beacon-size 15)
        :config
        (beacon-mode 1))

      (use-package-with-message centered-cursor-mode
        :ensure t
        :diminish centered-cursor-mode
        :config (global-centered-cursor-mode t))

      (use-package-with-message company
        :ensure t
        :diminish company-mode
        :init
        (setq company-lighter-base "cmp")
        (setq company-minimum-prefix-length 2)
        (setq company-tooltip-minimum-width 40)
        (setq company-tooltip-maximum-width 80)
        (setq company-tooltip-width-grow-only t)
        :config
        (global-company-mode t)
        :bind 
        (("C-\\" . company-complete)
          :map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-d" . company-show-doc-buffer)
          ("M-." . company-show-location)
          ("<prior>" . company-previous-page)
          ("<next>" . company-next-page))
        ;; :hook
        ;; (company-completion-started .
        ;;   (lambda (&rest _)
        ;;     "Stash the original states of some modes and then disable them during company."
        ;;     (setq-local aris-stashed-beacon-mode
        ;;       (bound-and-true-p beacon-mode))
        ;;     (setq-local aris-stashed-flycheck-mode
        ;;       (bound-and-true-p flycheck-mode))
        ;;     (setq-local aris-stashed-flycheck-inline-mode
        ;;       (bound-and-true-p flycheck-inline-mode))
        ;;     (when (bound-and-true-p beacon-mode) (beacon-mode -1))
        ;;     (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
        ;;     (when (bound-and-true-p flycheck-inline-mode) (flycheck-inline-mode -1))
        ;;     (setq-local hl-line-mode nil)))
        ;; (company-after-completion-hook .
        ;;   (lambda (&rest _)
        ;;     "Restore the state of modes that might have been on during company."
        ;;     (when (boundp 'aris-stashed-flycheck-mode)
	      ;;       (when aris-stashed-flycheck-mode (flycheck-mode 1))
	      ;;       (kill-local-variable 'aris-stashed-flycheck-mode))
        ;;     (when (boundp 'aris-stashed-flycheck-inline-mode)
	      ;;       (when aris-stashed-flycheck-inline-mode (flycheck-inline-mode 1))
	      ;;       (kill-local-variable 'aris-stashed-flycheck-inline-mode))
        ;;     (when (boundp 'aris-stashed-beacon-mode)
	      ;;       (when aris-stashed-beacon-mode (beacon-mode 1))
	      ;;       (kill-local-variable 'aris-stashed-beacon-mode))))
        )
      
      ;; (use-package-with-message company-quickhelp :ensure t
      ;;   :config
      ;;   (setq company-quickhelp-delay 0)
      ;;   (setq company-quickhelp-mode t)
      ;;   :bind
      ;;   (:map company-active-map
      ;;     ("C-c h" . company-quickhelp-manual-begin)))

      (use-package-with-message company-posframe :ensure t
        :init
        (setq company-posframe-lighter "")
        (setq company-posframe-quickhelp-delay 0.1)
        (setq company-posframe-quickhelp-show-header nil)
        :config
        (company-posframe-mode 1))

      (use-package-with-message company-sourcekit :ensure t
        :init 
        (add-to-list 'company-backends 'company-sourcekit)
        (setq sourcekit-available-ports '(8081))  
        (setq sourcekit-sourcekittendaemon-executable
          "/usr/local/bin/sourcekittendaemon")
        (setq sourcekit-verbose t)
        (setq company-sourcekit-verbose t))

      (use-package-with-message copilot :diminish copilot-mode
        :init
        (add-to-list 'load-path (expand-file-name "copilot.el" aris-lisp-dir))
        (setq copilot-idle-delay 1)
        (setq copilot-indent-offset-warning-disable t)
        (setq copilot-max-char -1)
        (setq copilot-node-executable "/opt/homebrew/bin/node")
        (setq global-copilot-mode nil)
        :bind
        (:map copilot-completion-map
          ("S-<return>" . copilot-next-completion)
          ("SPC" . copilot-next-completion)
          ("TAB" . ignore)
          ("C-<return>" . copilot-accept-completion)
          ("M-<return>" . copilot-accept-completion-by-word))
        :hook
        (prog-mode . copilot-mode))

      (use-package-with-message default-text-scale :ensure t
        :bind
        ( ("s-=" . default-text-scale-increase)
          ("s--" . default-text-scale-decrease)))

      (use-package-with-message dpaste :ensure t
        :bind
        ("C-c d p" . dpaste-region-or-buffer)
        :init
        (setq dpaste-poster "Ariane Emory")
        :config
        (add-to-list 'dpaste-supported-modes-alist '(rust-mode . "rust"))
        (add-to-list 'dpaste-supported-modes-alist '(rust-ts-mode . "rust")))

      (use-package-with-message flycheck :ensure t
        :diminish (flycheck-mode . "fc")
        :init
        (setq flycheck-display-errors-delay 1.0)
        (setq flycheck-display-errors-function
          #'flycheck-display-error-messages-unless-error-list)
        (setq flycheck-idle-change-delay 1.0))

      (use-package-with-message highlight-parentheses :ensure t
        :diminish highlight-parentheses-mode
        :init
        (setq highlight-parentheses-colors
          '("#ff0" "#fa0" "#f00" "#f0f" "#a0f" "#0af" "#0f0"))
        :config
        (global-highlight-parentheses-mode))

      (use-package-with-message hl-line :ensure t
        :config (global-hl-line-mode t))

      (use-package-with-message idle-highlight-mode :ensure t
        :init
        (setq idle-highlight-exceptions
          '("defun" "defmacro" "cond" "when" "unless" "let" "if" "progn"
             "lambda" "defclsas" "provide" "require" "error" "defmethod"))
        (setq idle-highlight-exceptions-face nil)
        (setq idle-highlight-idle-time 0.1)
        (setq idle-highlight-visible-buffers t)
        :config
        (global-idle-highlight-mode t))

      (use-package-with-message inf-ruby :ensure t
        :init
        (setq inf-ruby-default-implementation "brewruby")
        (setq inf-ruby-implementations
          '(("ruby" . inf-ruby--irb-command)
             ("brewruby" . "/opt/homebrew/opt/ruby/bin/irb --inf-ruby-mode")
             ("jruby" .
               "jruby -S irb --prompt default --noreadline -r irb/completion")
             ("rubinius" . "rbx -r irb/completion")
             ("yarv" . "irb1.9 -r irb/completion")
             ("macruby" . "macirb -r irb/completion")
             ("pry" . "pry")))
        (setq inf-ruby-prompt-read-only nil))
      
      (use-package-with-message lsp-mode :ensure t
        :bind
        ( ("C-x C-<next>" . lsp-inlay-hints-mode)
          ("C-x RET" . lsp-inlay-hints-mode)
          ("C-x C-h" . lsp-inlay-hints-mode))
        :init
        (use-package-with-message lsp-rust)
        (setq lsp-diagnostics-provider :flycheck)
        (setq lsp-auto-guess-root t)
        (setq lsp-inlay-hint-enable t)
        (setq lsp-log-io t)
        (setq lsp-signature-auto-activate nil)
        (setq lsp-enable-snippet nil)
        (setq lsp-eldoc-enable-hover nil)
        (setq lsp-enable-symbol-highlighting nil)
        (setq lsp-headerline-breadcrumb-enable nil)
        (setq lsp-lens-place-position 'above-line)
        (setq lsp-semantic-tokens-enable nil)
        (setq lsp-eldoc-enable-hover nil)
        (setq lsp-signature-render-documentation nil))

      (use-package-with-message lsp-ui :ensure t
        :init
        (setq lsp-ui-doc-delay 0.1)
        (setq lsp-ui-doc-include-signature t)
        (setq lsp-ui-doc-max-height 16)
        (setq lsp-ui-doc-max-width 100)
        (setq lsp-ui-doc-position 'top)
        (setq lsp-ui-doc-show-with-cursor t)
        (setq lsp-ui-doc-show-with-mouse nil)
        (setq lsp-ui-doc-text-scale-level -1)
        (setq lsp-ui-doc-use-childframe t)
        (setq lsp-ui-imenu-auto-refresh t)
        (setq lsp-ui-sideline-delay 1.0)
        (setq lsp-ui-sideline-diagnostic-max-line-length 70)
        (setq lsp-ui-sideline-diagnostic-max-lines 4)
        (setq lsp-ui-sideline-show-code-actions nil)
        (setq lsp-ui-sideline-show-diagnostics t)
        (setq lsp-ui-sideline-show-hover nil)
        (setq lsp-ui-sideline-wait-for-all-symbols t))

      (use-package-with-message multiple-cursors :ensure t
        :bind
        ( ("C-c C-c" . mc/edit-lines)
          ("M-<mouse-1>". mc/add-cursor-on-click)
          ("M-S-<up>" . mc/mark-previous-lines)
          ("M-S-<down>" . mc/mark-next-lines)))

      (use-package-with-message modern-cpp-font-lock :ensure t
        :hook
        (c++-mode . modern-c++-font-lock-mode)
        (c++-ts-mode . modern-c++-font-lock-mode))

      (use-package-with-message nov :ensure t
        :hook
        (nov-mode . 
          (lambda ()
            (setq-local centered-cursor-mode nil)
            (setq-local beacon-mode nil)
            (setq-local cursor-type nil)
            (follow-mode 1)
            (display-line-numbers-mode -1)
            (text-scale-adjust 16)
            (face-remap-add-relative 'highlight '(:highlight nil))
            (face-remap-add-relative 'hl-line '(:background "#000"))))
        :bind
        (:map nov-mode-map
          ("w" . nov-scroll-down)
          ("e" . previous-line)
          ("r" . nov-scroll-up)
          ("s" . ignore)
          ("d" . next-line)
          ("f" . ignore)
          ("q" . ignore)
          ("t" . ignore)
          ("a" . ignore)
          ("g" . ignore)
          ("z" . ignore)
          ("x" . ignore)
          ("c" . ignore)
          ("v" . ignore)
          ("b" . ignore)
          ("SPC" . ignore)
          ("TAB" . ignore)     
          ("u". nov-scroll-down)
          ("i" . previous-line)
          ("o". nov-scroll-up)
          ("j" . ignore)
          ("k" . next-line)
          ("l" . ignore)
          ("y" . ignore)
          ("p" . ignore)
          ("h" . ignore)
          ("'" . ignore)
          ("n" . ignore)
          ("m" . ignore)
          ("," . ignore)
          ("." . ignore)
          ("/" . ignore)
          ("M-<left>" . nov-previous-document)
          ("M-<right>" . nov-next-document)
          ("." . ignore)
          ("/" . ignore)
          ("DEL" . ignore)
          ("_" . ignore) 
          ("<left>" . ignore)
          ("<right>" . ignore)
          ("s-[" . ignore)
          ("s-]" . ignore)
          ("q" . ignore)
          ("p" . ignore)
          ("t" . ignore)
          ("g" . ignore))
        :init
        (setq nov-header-line-format nil)
        (setq nov-text-width 54))

      (toggle-debug-on-error)
      (use-package-with-message persistent-scratch :ensure t
        :init
        ;; (use-package aris-funs-setup-lisp)
        ;; (setq aris-scratch-buffer-name "scratch")
        ;; (when  (boundp 'aris-scratch-buffer-name)
        ;;   (with-current-buffer (get-buffer "*scratch*")
        ;;     (rename-buffer "scratch")))
        ;; (with-current-buffer
        ;;   (get-buffer
        ;;     (if (boundp  'aris-scratch-buffer-name)  aris-scratch-buffer-name "*scratch*"))
        ;;   (emacs-lisp-mode)
        ;;   (aris-setup-lisp)
        ;;   ;; (aris-prettify-symbols-lisp)
        ;;   ;; (prettify-symbols-mode 1)
        ;;   (variable-pitch-mode -1)
        ;;   (display-fill-column-indicator-mode 1)
        ;;   (when (featurep 'trav) (trav-mode 1))
        ;;   (goto-char (point-max)))
        ;; (use-package-with-message aris-configure-scratch-buffer :demand)
        (setq persistent-scratch-autosave-interval 5)
        ;; (setq persistent-scratch-scratch-buffer-p-function
        ;;   (lambda ()
        ;;     (cond
        ;;       ((not (string=
        ;;             (buffer-name)
        ;;             (if (boundp 'aris-scratch-buffer-name)
        ;;               aris-scratch-buffer-name
        ;;               "*scratch*")))
        ;;         nil)
        ;;       ((not (buffer-modified-p)) nil)
        ;;       (t
        ;;         (set-buffer-modified-p nil)
        ;;         t))))
        :config
        (persistent-scratch-autosave-mode))

      (use-package-with-message powerline :ensure t
        :init
        (setq powerline-text-scale-factor 0.8))

      (use-package-with-message rainbow-delimiters :ensure t :demand
        :hook
        (prog-mode . rainbow-delimiters-mode)
        (Info-mode . rainbow-delimiters-mode))

      (use-package-with-message rainbow-mode :ensure t :demand
        ;; the monkey patch doesn't work unless demanded!
        :diminish rainbow-mode
        :hook
        (prog-mode . rainbow-mode)
        (text-mode . rainbow-mode)
        (shell-mode . rainbow-mode)
        (Info-mode . rainbow-mode))

      (use-package-with-message rust-mode :ensure t
        :init
        (setq lsp-rust-analyzer-closing-brace-hints t)
        (setq lsp-rust-analyzer-closing-brace-hints-min-lines 1)
        (setq lsp-rust-analyzer-lens-enable t)
        (setq lsp-rust-analyzer-lens-implementations-enable nil)
        (setq lsp-rust-analyzer-lens-references-adt-enable nil)
        (setq lsp-rust-analyzer-lens-references-enum-variant-enable nil)
        (setq lsp-rust-analyzer-lens-references-method-enable t)
        (setq lsp-rust-analyzer-lens-references-trait-enable t)
        (setq rust-cargo-bin (expand-file-name "~/.cargo/bin/cargo"))
        (setq rust-format-goto-problem nil)
        (setq rust-format-on-save t)
        (setq rust-format-show-buffer nil)
        (setq rust-indent-offset 2)
        (setq rust-rustfmt-bin (expand-file-name "~/.cargo/bin/rustfmt"))
        :hook
        (rust-mode . aggressive-indent-mode)
        (rust-mode . aris-prettify-symbols-rust))

      (use-package-with-message rust-ts-mode :ensure t
        :init
        (use-package-with-message rust-mode)
        (setq rust-ts-mode-indent-offset 2)
        (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
        :hook
        (before-save .
          (lambda () (when (eq major-mode 'rust-ts-mode) (rust-format-buffer))))
        (rust-ts-mode .
          (lambda ()
            (setq-local company-tooltip-minimum-width 60)
            (setq-local company-tooltip-maximum-width 80)
            (setq-local lsp-semantic-tokens-enable 1)
	          (aggressive-indent-mode 1)
            (aris-prettify-symbols-rust)
            (flycheck-rust-setup)
            (lsp)
	          )))

      (use-package-with-message slime-company :ensure t)

      (use-package-with-message slime :ensure t
        :hook
        (slime-repl-mode . (lambda () (variable-pitch-mode 1)))
        :init
        (setq slime-kill-without-query-p t)
        (setq slime-lisp-implementations
          '((sbcl
	            ("/opt/homebrew/bin/sbcl"))
             (clisp
	             ("/opt/homebrew/bin/clisp"))
             (ecl
	             ("/opt/homebrew/bin/ecl"))))
        (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
        (setq slime-compilation-finished-hook 'slime-maybe-show-compilation-log)
        (setq slime-load-failed-fasl 'never)
        :config
        (let ((quicklisp (expand-file-name "~/.quicklisp/slime-helper.el")))
          (when (file-exists-p quicklisp)
            (load quicklisp)))
        (slime-setup '(slime-fancy slime-company)))

      (use-package-with-message spaceline :ensure t
        :init
        (setq spaceline-minor-modes-separator " ")
        (setq spaceline-version-control-p nil)
        :config
        (use-package-with-message powerline)
        (use-package-with-message spaceline)
        (use-package-with-message spaceline-config)
        (spaceline--theme '((buffer-id) :priority 98) nil nil)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Add various hooks:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "adding hooks"
      (add-hook 'Buffer-menu-mode-hook
        (lambda () (face-remap-add-relative 'default 'aris-alt-face)))

      (dolist (hook
                '(c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook))
        (add-hook hook
          (lambda ()
            (c-set-offset 'arglist-close 0)
	          (setq-local lsp-inlay-hint-enable nil)
    	      (lsp)
            ;; (lsp-inlay-hints-mode -1)
            ;; (lsp-lens-mode -1)
            ;; (lsp-semantic-tokens-mode -1)
            )))
      
      (add-hook 'compilation-mode-hook
        (lambda () (face-remap-add-relative 'default 'aris-alt-face)))

      (dolist
        (hook '(csharp-mode-hook csharp-ts-mode-hook))
        (add-hook hook
          (lambda ()
            (setq-local c-basic-offset 2)
            (setq-local fill-column 120)
            (setq-local lsp-semantic-tokens-enable t)
            (setq-local lsp-signature-auto-activate t)
            (setq-local lsp-signature-render-documentation t)
            (aggressive-indent-mode 1)
            (eldoc-mode 1)
            (lsp))))

      (add-hook 'Custom-mode-hook
        (lambda () (face-remap-add-relative 'default
                '(:family "XITS" :height 1.25))))

      (add-hook 'find-file-hooks
        (lambda ()
          "Hides the DOS end-of-line (EOL) characters in the current buffer."
          (unless buffer-display-table
            (setq buffer-display-table (make-display-table)))
          (aset buffer-display-table ?\^M [])))

      ;;     (add-hook 'comint-exec-hook
      ;;       (lambda ()
      ;;"Let the shell be killed without warning about running processes."
      ;; (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

      (add-hook 'help-mode-hook
        (lambda () (face-remap-add-relative 'default
                '(:family "Gill Sans" :height 1.15))))

      (add-hook 'Info-mode-hook
        (lambda ()
          (variable-pitch-mode 1)
          (face-remap-add-relative 'default '(:height 1.1))))

      (add-hook 'tetris-mode-hook
        (lambda ()
          (setq-local global-hl-line-mode nil)
          (display-line-numbers-mode -1)))
      
      (add-hook 'window-buffer-change-functions
        (lambda (_)
          (when (string= (buffer-name) "*Messages*")
            (idle-highlight-mode 1)
            (face-remap-add-relative 'default 'aris-alt-face))))

      (add-hook 'xwidget-webkit-mode-hook
        (lambda ()
          (setq-local global-hl-line-mode nil)
          (setq-local beacon-mode nil)
          (setq-local display-line-numbers-mode nil)
          (setq-local cursor-type nil)))

      (add-hook 'Buffer-menu-mode-hook (lambda () (setq-local truncate-lines t)))
      (add-hook 'inferior-emacs-lisp-mode-hook 'aris-setup-lisp)
      (add-hook 'org-mode-hook (lambda () (variable-pitch-mode)))
      (add-hook 'special-mode-hook (lambda () (setq-local truncate-lines nil))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Use my own custom packages:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "using my custom packages"
      (mapc
        (lambda (pkg) (eval `(use-package-with-messages ,pkg :demand t)))
        '( aris-funs--unsorted
           aris-funs--match-pattern
           aris-funs--match-pattern2
           aris-funs--alist-funs
           aris-funs--pattern-dispatch
           ;;aris-funs--pipe
           aris-funs--prettify-symbols
           aris-funs--setup-lisp
           aris-funs--error-when-and-error-unless
           aris-configure--key-bindings
           aris-configure--tetris-keymap
           aris-configure--global-abbrevs-table 
           aris-configure--rainbow-cursor
           aris-configure--xwidget-browse-menu
           aris-mode--local-caps-lock
           xah-lees-configure--emoji-fix))

      (when (file-directory-p aris-trav-dir) (use-package-with-messages trav)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; auto-mode-alists:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "setting up auto-mode-alist"
      "Treating T4 templates is pretty close:"
      (add-to-list 'auto-mode-alist '("\\.tt\\'" . csharp-mode))

      "lex/yacc grammars:"
      (add-to-list 'auto-mode-alist '("\\.lex\\'" . flex-mode))
      (add-to-list 'auto-mode-alist '("\\.yacc\\'" . bison-mode))

      "Treat .ino, .inl, .inc, .cppm files as C++ files:"
      (add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.cppm\\'" . foo-mode))
      (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.inc\\'" . c-mode))

      "Treat various files as Lisp files:"
      (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
      (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
      (add-to-list 'auto-mode-alist '("\\.scm\\'" . lisp-mode))

      "Open .epub files in nov.el:"
      (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

      "Open .pl files in prolog-mode:"
      (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Major mode remappings
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "remapping major modes"
      (add-to-list 'major-mode-remap-alist '(c-ts-mode . c-mode))
      (add-to-list 'major-mode-remap-alist '(c++-ts-mode . c++-mode))
      (add-to-list 'major-mode-remap-alist '(c-or-c++-ts-mode . c-or-c++-mode)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Load Custom file:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "loading Custom file" (load custom-file))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Define the after-init hook and hook it up:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-messages "setting after init hook"
      (add-hook 'after-init-hook
        (lambda ()
          (with-messages "running after-init-hook"
            (with-messages "opening some directories"
              (dolist (dir `("~/" ,aris-config-dir ,aris-lisp-dir))
                (eval `(with-message (format "opening '%s" ,dir)
                         (dired (expand-file-name dir))))))

            (with-messages "opening some files"
              (dolist (file '("~/.profile" ))
                (let ((file (expand-file-name file)))
                  (when (file-regular-p file)
                    (eval `(with-message (format "opening '%s'" ,file) (find-file ,file))))))
              (dolist (file '("init.el" "custom.el" "toxikat-theme.el"))
                (let ((file (expand-file-name file aris-config-dir)))
                  (when (file-regular-p file)
                    (eval `(with-message (format "opening '%s'" ,file) (find-file ,file))))))
              (dolist (file (directory-files aris-lisp-dir t "\\`[^.].*\\.el\\'"))
                (when (and (file-regular-p file) (not (string-suffix-p ".elc" file)))
                  (eval `(with-messages (format "opening '%s'" ,file) (find-file ,file))))))
            
            (with-messages "starting some shells"
              (let ((default-directory aris-config-dir))
                (shell "emacs"))
              "Shells in home:"
              (let ((default-directory "~"))
                (shell)
                (shell "git")
                (shell "dcp")))

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	          ;; Load desktop file:
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	          (with-messages "loading desktop" (desktop-read))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (provide 'aris-emacs-configuration))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  config-end
  (message "[ARI] Leaving init.el."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

