(defsystem #:ck-clle
  :components ((:module "source"
                :components ((:file "list")
                             (:file "string")
                             (:file "clle" :depends-on ("list" "string")))))
  :depends-on (#:alexandria))
