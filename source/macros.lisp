(defpackage #:ck-clle/macros
  (:use #:cl)
  (:export #:with-gensyms))

(in-package #:ck-clle/macros)

(defmacro with-gensyms ((&rest symbols) &body body)
  `(let ,(mapcar (lambda (s) `(,s (gensym))) symbols)
     ,@body))
