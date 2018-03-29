(defpackage ipaddress
  (:use
    #:cl)
  (:export
    ipv4-address
    ipv6-address
    ip-version
    as-string
    as-integer
    ipv4-interface
    ipv6-interface
    ipv4-subnet
    ipv6-subnet
    address
    prefixlength
    subnetp))
