(asdf:defsystem #:ipaddress-test
                :serial t
                :license "MIT license"
                :author "James Fleming <james@electronic-quill.net>"
                :description "Test suite for the ipaddress library"
                :depends-on (#:syscat
                             #:fiveam)
                :components ((:file "package")
                             (:file "ipaddress-test")))
