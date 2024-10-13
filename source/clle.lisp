(defmacro define-clle (imports-and-exports)
  `(uiop:define-package #:ck-clle
       (:use #:cl)
     (:use-reexport #:ck-clle/list)
     ,@(mapcar (lambda (i&e)
                 `(:import-from ,(car i&e)
                                ,@(cdr i&e)))
               imports-and-exports)
     (:export #:atomp
              #:nullp
      ,@(alexandria:flatten (mapcar #'cdr imports-and-exports)))))

(define-clle
    ((#:alexandria #:with-gensyms
                   #:when-let
                   #:if-let)))

(in-package #:ck-clle)

;; Add synonyms that adhere to the convention of having predicates be posfixed with 'p'.
;; This should really be the default in the standard, but was not adopted due to historical reasons.
(setf (fdefinition 'atomp) (fdefinition 'atom))
(setf (fdefinition 'nullp) (fdefinition 'null))

#+sbcl
(progn
  (sb-ext:lock-package 'ck-clle)
  (sb-ext:lock-package 'ck-clle/list))
