;; -*- lexical-binding: nil; fill-column: 120; eval: (display-fill-column-indicator-mode 1);  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; naming examples for an imaginary frobnosticate-widget package:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ( (aris-naming-conventions--examples 
         '( ('fw--frobnosticate-widget .
              "The name of a public-facing function in the frobnosticate-widget package.")
            ('--fw-frobnostication-helper .
              "The name of an internal function in the frobnosticate-widget package.")
            ('--do-fw-stuff .
              "The name of an internal function in the frobnosticate-widget package.")
            ('*fw--frobnostication-level* .
              "The name of a public-facing (usually customizable) variable in the frobnosticate-widget package.")
            ('*--fw-frobnostication-count* .
              "The name of an internal variable in the frobnosticate-widget package not meant for customization")
            ('frobnosticate-widget .
              "The name of either a public-facing function in the frobnosticate-widget package or a convenient
alias for fw--frobnosticate-widget."))))
  aris-sample-names)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-naming-conventions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
