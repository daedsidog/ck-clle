(defmacro define-clle (imports-and-exports)
  `(uiop:define-package #:ck-clle
       (:use #:cl)
     (:use-reexport #:ck-clle)
     ,@(mapcar (lambda (i&e)
                 `(:import-from ,(car i&e)
                                ,@(cdr i&e)))
               imports-and-exports)
     (:export ,@(alexandria:flatten (mapcar #'cdr imports-and-exports)))))

;; Import symbols which I believe should be available in the base package.
(define-clle
    ((#:alexandria #:with-gensyms
                   #:when-let
                   #:flatten
                   #:if-let
                   #:iota)
     (#:ck-clle/string #:string-empty-p)))

(in-package #:ck-clle)

(defmacro alias (alias-sym pred-sym)
  `(progn
     (setf (fdefinition ,alias-sym) (fdefinition ,pred-sym))
     (export ,alias-sym)))

;; Add synonyms that adhere to the convention of having predicates be posfixed with 'P'.
;; This should really be the default in the standard, but was not adopted due to historical reasons.
;;
;; Functions such as OR, AND, & NOT, are statements rather than predicates.
;; Functions such as =, <=, STRING= are exempt from this convention because they consist of only
;; non-alphanumeric characters.

(alias 'atomp 'cl:atom)
(alias 'nullp 'cl:null)

(alias 'eqp 'cl:eq)
(alias 'eqlp 'cl:eql)

(alias 'memberp 'cl:member)
