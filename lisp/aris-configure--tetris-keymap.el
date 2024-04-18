;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tetris mode keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar tetris-mode-map (make-sparse-keymap 'tetris-mode-map))
(define-key tetris-mode-map "n" 'tetris-start-game)
(define-key tetris-mode-map "q" 'tetris-end-game)
(define-key tetris-mode-map "p" 'tetris-pause-game)
(define-key tetris-mode-map " " 'tetris-move-bottom)
(define-key tetris-mode-map "DEL" 'tetris-move-bottom)
(define-key tetris-mode-map "a" 'tetris-move-left)
(define-key tetris-mode-map "d" 'tetris-move-right)
(define-key tetris-mode-map "w" 'tetris-rotate-prev)
(define-key tetris-mode-map "s" 'tetris-move-down)
(define-key tetris-mode-map "j" 'tetris-move-left)
(define-key tetris-mode-map "l" 'tetris-move-right)
(define-key tetris-mode-map "i" 'tetris-rotate-prev)
(define-key tetris-mode-map "k" 'tetris-move-down)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-configure--tetris-keymap)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
