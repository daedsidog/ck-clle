(defpackage #:ck-clle/symbols
  (:use #:cl)
  (:export #:keywordicate))

(in-package #:ck-clle/symbols)

(defun keywordicate (symbol)
  "Intern SYMBOL into the keyword package and return it."
  (intern (symbol-name symbol) :keyword))
