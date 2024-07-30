(defpackage #:ck-clle/types
  (:use #:cl)
  (:export #:unit-interval
           #:standard-interval))

(in-package #:ck-clle/types)

(deftype unit-interval ()
  '(real 0 1))

(deftype standard-interval ()
  '(real -1 1))
