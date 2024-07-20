(defsystem #:ck.clle
  :components ((:module "source" :components ((:file "symbols")
                                              (:file "collections")
                                              (:file "clle" :depends-on ("symbols"
                                                                         "collections")))))
  :depends-on (#:iterate))
