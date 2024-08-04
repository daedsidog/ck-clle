(defsystem #:ck-clle
  :components ((:module "source" :components ((:file "symbols")
                                              (:file "collections")
                                              (:file "types")
                                              (:file "clle" :depends-on ("symbols"
                                                                         "collections"
                                                                         "types")))))
  :depends-on (#:iterate))