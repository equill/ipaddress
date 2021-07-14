(asdf:defsystem #:ipaddress
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "Dual-stack IP address manipulation libary"
  :depends-on (#:cl-cidr-notation)
  :components ((:file "package")
               (:file "generics")
               (:file "ipv4")
               (:file "ipv6")
               (:file "utilities")))
