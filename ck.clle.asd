(defsystem #:ck.clle
  :components ((:module "source" :components ((:file "exportation")
                                              (:file "collections")
                                              (:file "clle" :depends-on ("exportation"
                                                                         "collections")))))
  :depends-on (#:iterate))
