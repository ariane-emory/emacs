;; -*- fill-column: 90;  eval: (display-fill-column-indicator-mode 1); eval: (variable-pitch-mode -1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define some random functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'atom?           'atom)
(defalias 'bound?          'boundp)
(defalias 'bound-and-true? 'bound-and-true-p)
(defalias 'cons?           'consp)
(defalias 'equal?          'equal)
(defalias 'fun?            'functionp)
(defalias 'function?       'functionp)
(defalias 'integer?        'integerp)
(defalias 'list?           'listp)
(defalias 'nil?            'null)
(defalias 'number?         'numberp)
(defalias 'rplaca!         'rplaca)
(defalias 'rplacd!         'rplacd)
(defalias 'string?         'stringp)
(defalias 'zero?           'zerop)
(defalias 'decr            'cl-decf)
(defalias 'incr            'cl-incf)
(setq else t) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
