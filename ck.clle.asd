(defsystem #:ck.clle
  :components ((:module "source" :components ((:file "exportation")
                                              (:file "clle" :depends-on ("exportation"))))))
