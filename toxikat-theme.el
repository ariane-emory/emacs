(deftheme toxikat
  "Created 2024-03-18.")

(custom-theme-set-faces
  'toxikat

  '(cursor ((t (:foreground "yellow1" :background "#8db"))))
  '(default ((t (:inherit nil :extend nil :stipple nil :background "#000" :foreground "#60b890" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 180 :width normal :foundry "nil" :family "Iosevka")))) 
  '(error ((t (:foreground "Red" :weight bold))))
  '(fill-column-indicator ((t (:inherit highlight :stipple nil :background "#061b21" :foreground "#050" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal)))) 
  '(fixed-pitch ((t (:family "Monospace"))))
  '(fixed-pitch-serif ((t (:family "Monospace Serif"))))
  '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :foreground "Red")))) ;
  '(region ((t (:extend t :background "#050")))) 
  '(show-paren-match ((t (:background "#bb0"))))
  '(tab-bar ((t (:background "#081018" :height 0.85 :family "XITS")))) 
  '(tab-bar-tab ((t (:inherit tab-bar :background "#282828" :foreground "#9b0" :box (:line-width (1 . 1) :color "#333" :style flat-button))))) 
  '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "black" :foreground "#888")))) 
  '(tab-line ((t (:inherit tab-bar))))
  '(tab-line-tab ((t (:inherit tab-line))))
  '(tab-line-tab-current ((t (:inherit tab-line-tab))))
  '(tab-line-tab-inactive ((t (:inherit tab-bar-inactive))))
  '(term-underline ((t (:inherit ansi-color-underline :foreground "#60b890"))))
  '(variable-pitch ((t (:weight semi-light :height 1.05 :family "XITS")))) 
  '(vertical-border ((t (:foreground "#104050"))))
  '(whitespace-line ((t nil))) 
  '(widget-field ((t (:extend t :background "gray15" :inverse-video nil :box (:line-width 1 :color "grey10" :style pressed-button)))))

  '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
  '(font-lock-builtin-face ((t (:foreground "#0aa"))))
  '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
  '(font-lock-comment-face ((t (:foreground "#888")))) 
  '(font-lock-constant-face ((t (:foreground "#ed7")))) 
  '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
  '(font-lock-doc-face ((t (:inherit font-lock-string-face)))) 
  '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
  '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
  '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face)))))
  '(font-lock-function-name-face ((t (:foreground "#ec0" :weight bold))))
  '(font-lock-keyword-face ((t (:foreground "#8d8" :slant italic :weight bold))))
  '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
  '(font-lock-negation-char-face ((t nil)))
  '(font-lock-number-face ((t nil)))
  '(font-lock-operator-face ((t nil)))
  '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
  '(font-lock-property-name-face ((t (:inherit (font-lock-variable-name-face)))))
  '(font-lock-property-use-face ((t (:inherit (font-lock-property-name-face)))))
  '(font-lock-punctuation-face ((t nil)))
  '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
  '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
  '(font-lock-string-face ((t (:foreground "#ce0")))) 
  '(font-lock-type-face ((t (:foreground "orchid2" :slant italic))))
  '(font-lock-type-face ((t nil)))
  '(font-lock-variable-name-face ((t (:foreground "#0b0"))))
  '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
  '(font-lock-warning-face ((t (:foreground "#f20" :weight bold))))
  '(line-number ((t (:inherit (shadow default) :foreground "#0aa")))) 
  '(line-number-current-line ((t (:inherit line-number :foreground "#0cc")))) 
  '(fringe ((t (:background "#000"))))
  '(hl-line ((t (:background "#082838"))))
  '(highlight ((t (:background "#505"))))
  '(lazy-highlight ((t (:background "#615" )))) 
  '(Info-quoted ((t (:inherit fixed-pitch-serif :foreground "#cb0"))))
  '(ansi-color-blue ((t (:background "DeepSkyBlue1" :foreground "DeepSkyBlue1"))))
  '(compilation-info ((t (:foreground "olive drab"))))
  '(custom-button ((t (:background "gray15" :foreground "gray60" :box (:line-width 1 :color "grey10" :style released-button)))))
  '(custom-group-tag ((t (:inherit variable-pitch :foreground "light blue" :weight bold :height 1.0))))
  '(flycheck-error ((t (:box (:line-width (2 . 2) :color "#000" :style released-button) :underline (:color "Red1" :style wave :position nil)))))
  '(idle-highlight ((t (:inherit region))))
  '(info-header-node ((t (:inherit info-node :foreground "#ce0" :underline t :slant normal :family "XITS"))))
  '(info-header-xref ((t (:inherit info-xref :foreground "#9b0" :family "XITS"))))
  '(info-menu-header ((t (:inherit variable-pitch :foreground "#0aa" :underline t :weight bold :height 0.9 :family "XITS"))))
  '(info-menu-star ((t (:foreground "#3A6854"))))
  '(info-title-1 ((t (:inherit info-title-2 :underline t :family "XITS"))))
  '(info-title-2 ((t (:inherit info-title-3 :foreground "#ce0" :underline t :height 0.95 :family "XITS"))))
  '(info-title-3 ((t (:inherit info-title-4 :foreground "#ce0" :underline t :height 0.95 :family "XITS"))))
  '(info-xref ((t (:inherit link :height 1.0 :family "XITS"))))
  '(info-xref-visited ((t (:inherit (link-visited info-xref)))))
  '(isearch ((t (:background "#918" )))) ;
  '(lsp-face-highlight-read ((t (:inherit highlight)))) 
  '(lsp-face-highlight-textual ((t nil)))
  '(lsp-face-semhl-function ((t nil))) ;
  '(lsp-face-semhl-keyword ((t (:inherit font-lock-keyword-face)))) 
  '(lsp-face-semhl-method ((t nil))) 
  '(lsp-face-semhl-number ((t nil))) 
  '(lsp-face-semhl-operator ((t nil))) 
  '(lsp-face-semhl-variable ((t nil))) ;
  '(lsp-rust-analyzer-constant-modifier-face ((t (:inherit font-lock-function-name-face)))) 
  '(lsp-rust-analyzer-declaration-modifier-face ((t (:inherit font-lock-variable-name-face)))) 
  '(lsp-rust-analyzer-mutable-modifier-face ((t nil))) 
  '(lsp-ui-doc-background ((t (:background "#222222"))))
  '(lsp-ui-doc-header ((t (:background "deep sky blue" :foreground "black" :height 0.8))))
  '(lsp-ui-doc-highlight-hover ((t (:inherit region :height 0.8))))
  '(lsp-ui-sideline-code-action ((t (:foreground "yellow" :height 0.8))))
  '(lsp-ui-sideline-global ((t (:background "#333" :box (:line-width (2 . 2) :color "#444" :style released-button) :height 1.0))))
  '(lsp-ui-sideline-symbol ((t (:foreground "grey" :box (:line-width (1 . -1) :color "grey") :height 0.9))))
  '(lsp-ui-sideline-symbol-info ((t (:slant italic :height 0.9))))
  '(minibuffer-prompt ((t (:foreground "#14bccc")))) 
  '(mode-line ((t (:background "grey10" :foreground "#ce0" :height 0.85)))) 
  '(mode-line-buffer-id ((t (:weight bold))))
  '(mode-line-emphasis ((t (:weight bold))))
  '(mode-line-highlight ((((class color) (min-colors 88))) (t (:inherit (highlight)))))
  '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "#570" :weight light))))
  '(org-hide ((t (:foreground "#082028"))))
  '(org-table ((t (:inherit fixed-pitch :foreground "#9b0"))))
  '(outline-1 ((t (:foreground "#ce0" :underline t :weight bold :height 1.12 :family "XITS"))))
  '(outline-2 ((t (:foreground "#ce0" :weight bold :height 1.09 :family "XITS"))))
  '(outline-3 ((t (:foreground "#9b0" :weight bold :height 1.06 :family "XITS"))))
  '(outline-4 ((t (:foreground "#9b0" :weight bold :height 1.03 :family "XITS"))))
  '(outline-5 ((t (:foreground "#9b0" :weight bold :family "XITS"))))
  '(query-replace ((t (:inherit (isearch)))))
  )

(provide-theme 'toxikat)
