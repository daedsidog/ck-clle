(defsystem #:ck-clle
  :components ((:module "source"
                :components ((:file "list")
                             (:file "clle" :depends-on ("list")))))
  :depends-on (#:alexandria))
