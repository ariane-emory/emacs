;; -*- fill-column: 100; eval: (display-fill-column-indicator-mode 1); -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars and customs used by `dm:match', my destructuring pattern matching function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup destructuring-match nil
  "Ari's destructuring pattern matcher.")
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:verbose* t
  "Whether or not functions in the 'destructuring-match' group should print verbose messages."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:forbid-ambiguous-lambda-pred-arg-names* t
  "Whether or not `lambda' forms used as pattern element predicates are allowed to 
use argument names in their ARGS whose names overlap (and would, if this option
is disabled/nil, shadow) bound variables in `dm:match1's ALIST / REFERENCE-ALIST.

Turn this off at your own risk."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:debug* nil 
  "Whether or not the debug breakpoints in 'destructuring-match' are enabled."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:warn-on-consecutive-flexible-elements* nil
  "Whether or not 'destructuring-match' should warn if it encounters consecutive flexible pattern elements."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:test-match* t
  "Whether or not dm:match's unit tests are enabled."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:test-fill* t
  "Whether or not dm:fill's unit tests are enabled."
  :group 'destructuring-match
  :type 'boolean)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:label-width* 32
  "Label width used by functions in the 'destructuring-match' group."
  :group 'destructuring-match
  :type 'integer)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:div-width* 90
  "Div width used by functions in the 'destructuring-match' group."
  :group 'destructuring-match
  :type 'integer)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:default-dont-care* '_
  "dm:match's default DONT-CARE indicator."
  :group 'destructuring-match
  :type 'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:default-ellipsis* '...
  "dm:match's default ELLIPSIS indicator."
  :group 'destructuring-match
  :type 'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:default-unsplice* '\,@
  "dm:match's default UNSPLICE indicator."
  :group 'destructuring-match
  :type 'symbol)
;;---------------------------------------------------------------------------------------------------
(defcustom *dm:fill-unsplice-atoms* t
  "Whether or not fill should allow 'unsplicing' an atom (or improper list) as if it were a list of 1 item"
  :group 'destructuring-match
  :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aris-funs--destructuring-match-vars)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
