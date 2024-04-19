;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(makunbound 'blink-cursor-colors)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar blink-cursor-colors
  (list "#f0f" "#f08" "#f00" "#f00" "#f80" "#ff0" "#ff0" "#8f0"
    "#0f0" "#0f8" "#0ff" "#8ff" "#8db" "#08f" "#00f" "#80f"
    "#84f" "#88f" "#fff")
  "On each blink the cursor will cycle to the next color in this list.")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq blink-cursor-count 0)
(setq blink-cursor-random t)
(setq last-blink-cursor-color "#f00")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun blink-cursor-timer-function ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  "Zarza wrote the original version of this cyberpunk variant of timer
`blink-cursor-timer'.

Ari has modified it so that it will choose colours randomly when
blink-cursor-random is true and so that it updates the colour of beacon-mode's
beacon to match the cursor's color at the time the beacon starts.

Warning: overwrites original version in `frame.el'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (setq last-blink-cursor-color
      (if 'blink-cursor-random
        (seq-random-elt blink-cursor-colors)
        (nth blink-cursor-count blink-cursor-colors)))
    (when (boundp 'beacon-color) (setq beacon-color last-blink-cursor-color))
    (set-cursor-color last-blink-cursor-color)
    (setq blink-cursor-count (+ 1 blink-cursor-count)))
  (internal-show-cursor nil (not (internal-show-cursor-p))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--rainbow-cursor)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
