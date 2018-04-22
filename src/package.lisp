(defpackage ipaddress
  (:use
    #:cl)
  (:export
    ip-address
    ipv4-address
    ipv6-address
    ip-version
    as-string
    as-integer
    as-cidr
    ip-interface
    ipv4-interface
    ipv6-interface
    prefix-length
    ip-subnet
    ipv4-subnet
    ipv6-subnet
    address
    prefix-length
    subnetp
    ipv4-subnet-p
    make-ipv4-address
    make-ipv4-interface
    make-ipv4-subnet
    make-ipv6-address
    make-ipv6-interface
    make-ipv6-subnet))
