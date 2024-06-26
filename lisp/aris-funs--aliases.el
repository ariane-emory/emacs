;; -*- fill-column: 90; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define some random functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'atom?           'atom)
(defalias 'bound?          'boundp)
(defalias 'bound-and-true? 'bound-and-true-p)
(defalias 'cons?           'consp)
;;(defalias 'copy-list       'cl-copy-list)
(defalias 'equal?          'equal)
(defalias 'eq?             'eq)
(defalias 'eql?            'eql)
(defalias 'equal?          'equal)
(defalias 'fun?            'functionp)
(defalias 'function?       'functionp)
(defalias 'integer?        'integerp)
(defalias 'keyword?        'keywordp)
(defalias 'kw?             'keywordp)
(defalias 'list?           'listp)
(defalias 'map             'mapcar)
(defalias 'nil?            'null)
(defalias 'number?         'numberp)
(defalias 'proper?         'proper-list-p)
(defalias 'proper-list?    'proper-list-p)
(defalias 'rplaca!         'rplaca)
(defalias 'rplacd!         'rplacd)
(defalias 'setcar!         'rplaca)
(defalias 'setcdr!         'rplacd)
(defalias 'string?         'stringp)
(defalias 'sym?            'symbolp)
(defalias 'symbol?         'symbolp)
(defalias 'zero?           'zerop)
(defalias 'decr            '1-)
(defalias 'incr            '1+)
(defalias 'decf            'cl-decf)
(defalias 'incf            'cl-incf)
;; (defalias 'match           'pcase)
;; (defalias 'head            'car)
;; (defalias 'tail            'cdr)
(defalias 'first           'car)
(defalias 'rest            'cdr)
(setq else t) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--aliases)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
