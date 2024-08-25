(uiop:define-package #:ck-clle
  (:use #:cl)
  (:use-reexport #:ck-clle/symbols
                 #:ck-clle/collections
                 #:ck-clle/types
                 #:ck-clle/macros
                 #:iterate))

(in-package #:iterate)

;; Remove ITERATE:COUNT from the ITERATE clauses as it often clashes with CL:COUNT.  We can use the
;; ITERATE:COUNTING clause instead.
(remprop 'count 'synonym)

(in-package #:ck-clle)

;; Add synonyms that adhere to the convention of having predicates be posfixed with 'p'.
;; This should really be the default in the standard, but was not adopted due to historical reasons.
(setf (fdefinition 'atomp) (fdefinition 'atom))
(setf (fdefinition 'nullp) (fdefinition 'null))

#+sbcl
(progn
  (sb-ext:lock-package 'ck-clle)
  (sb-ext:lock-package 'ck-clle/symbols)
  (sb-ext:lock-package 'ck-clle/collections)
  (sb-ext:lock-package 'ck-clle/types)
  (sb-ext:lock-package 'ck-clle/macros))
