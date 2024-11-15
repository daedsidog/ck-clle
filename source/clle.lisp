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
              #:eqp
              #:eqlp
      ,@(alexandria:flatten (mapcar #'cdr imports-and-exports)))))

(define-clle
    ((#:alexandria #:with-gensyms
                   #:when-let
                   #:if-let
                   #:iota)))

(in-package #:ck-clle)

;; Add synonyms that adhere to the convention of having predicates be posfixed with 'p'.
;; This should really be the default in the standard, but was not adopted due to historical reasons.
(setf (fdefinition 'atomp) (fdefinition 'cl:atom)
      (fdefinition 'nullp) (fdefinition 'cl:null)
      ;; We consider a predicate function to be a boolean (or generalized boolean) function which
      ;; never throws an error.  By this definition, the following functions should also have
      ;; suffixes.  Note that we already have EQUALP, so we do not need to alias EQUAL.
      (fdefinition 'eqp) (fdefinition 'cl:eq)
      (fdefinition 'eqlp) (fdefinition 'cl:eql))

#+sbcl
(progn
  (sb-ext:lock-package 'ck-clle)
  (sb-ext:lock-package 'ck-clle/list))
