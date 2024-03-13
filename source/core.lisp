(defpackage #:ck.clle
  (:use #:cl #:ck.clle.exportation))

(in-package #:ck.clle)

(loop for symbol being the external-symbols of :ck.clle.exportation
      do (export symbol :ck.clle))
