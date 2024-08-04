(uiop:define-package #:ck-clle
  (:use #:cl)
  (:use-reexport #:ck-clle/symbols
                 #:ck-clle/collections
                 #:ck-clle/types
                 #:iterate)
  (:lock t))

(in-package #:iterate)

(remprop 'count 'synonym)

(in-package #:ck-clle)

(setf (fdefinition 'atomp) (fdefinition 'atom))
(setf (fdefinition 'nullp) (fdefinition 'null))
