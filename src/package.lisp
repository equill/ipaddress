(defpackage ipaddress
  (:use
    #:cl)
  (:export
    ip-address
    ip-version
    ipv4-address
    ipv6-address
    ip-interface
    ipv4-interface
    ipv6-interface
    address
    prefixlength
    as-string
    as-integer
    subnetp))
