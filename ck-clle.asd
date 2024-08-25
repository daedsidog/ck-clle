(defsystem #:ck-clle
  :components ((:module "source" :components ((:file "symbols")
                                              (:file "collections")
                                              (:file "types")
                                              (:file "macros")
                                              (:file "clle" :depends-on ("symbols"
                                                                         "collections"
                                                                         "types"
                                                                         "macros")))))
  :depends-on ("iterate"))
