(defsystem #:ck.clle
  :components ((:module "source" :components ((:file "exportation")
                                              (:file "core" :depends-on ("exportation"))))))
