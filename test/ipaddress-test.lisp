(in-package :ipaddress-test)

(fiveam:def-suite main)
(fiveam:in-suite main)


;;; Addresses

(fiveam:test
  create-ipv4-address
  "Creation of IPv4 address objects."
  ;; Create with address
  (fiveam:is (make-instance 'ipaddress:ipv4-address
                            :address "127.0.0.1"))
  ;; Create with integer value
  (fiveam:is (make-instance 'ipaddress:ipv4-address
                            :integer 2130706433))
  (fiveam:is (equal
               4
               (ipaddress:ip-version (make-instance
                                       'ipaddress:ipv4-address
                                       :address "127.0.0.1")))))

(fiveam:test
  render-ipv4-address
  "Rendering of an address in various forms."
  (fiveam:is
    (equal 2130706433
           (ipaddress:as-integer
             (make-instance
               'ipaddress:ipv4-address
               :address "127.0.0.1"))))
  (fiveam:is
    (equal "127.0.0.1"
           (ipaddress:as-string
             (make-instance
               'ipaddress:ipv4-address
               :address "127.0.0.1"))))
  (fiveam:is
    (equal "127.0.0.1/32"
           (ipaddress:as-cidr
             (make-instance
               'ipaddress:ipv4-address
               :address "127.0.0.1")))))

(fiveam:test
  miscreate-ipv4-address
  "Error conditions around IPv4 address creation"
  ;; No values given
  (fiveam:signals (simple-error
                    "Either a string or integer representation of the address must be supplied.")
    (make-instance 'ipaddress:ipv4-address))
  ;; Integer value too large
  (fiveam:signals (simple-error "Integer value is out of range")
    (make-instance 'ipaddress:ipv4-address
                   :integer 4294967296)))

(fiveam:test
  create-ipv6-address
  "Creation of IPv6 address objects."
  (fiveam:is (make-instance 'ipaddress:ipv6-address
                            :address "cafe:beef::1"))
  (fiveam:is (equal
               6
               (ipaddress:ip-version (make-instance
                                       'ipaddress:ipv6-address
                                       :address "cafe:beef::1")))))

(fiveam:test
  render-ipv6-address
  "Rendering of an address in various forms."
  (fiveam:is
    (equal 269826771143976387270773007078999982081
           (ipaddress:as-integer
             (make-instance
               'ipaddress:ipv6-address
               :address "cafe:beef::1"))))
  (fiveam:is
    (equal "cafe:beef:0000:0000:0000:0000:0000:0001"
           (ipaddress:as-string
             (make-instance
               'ipaddress:ipv6-address
               :address "cafe:beef::1"))))
  (fiveam:is
    (equal "cafe:beef:0000:0000:0000:0000:0000:0001/128"
           (ipaddress:as-cidr
             (make-instance
               'ipaddress:ipv6-address
               :address "cafe:beef::1")))))

(fiveam:test
  miscreate-ipv6-address
  "Error conditions around IPv6 address creation"
  ;; No values given
  (fiveam:signals (simple-error
                    "Either a string or integer representation of the address must be supplied.")
    (make-instance 'ipaddress:ipv6-address))
  ;; Integer value out of range
  (fiveam:signals (simple-error "Integer value is out of range")
    (make-instance 'ipaddress:ipv4-address
                   :integer 340282366920938463463374607431768211456)))


;;; Interfaces

(fiveam:test
  create-ipv4-interface
  "Creation of IPv4 interface objects."
  (fiveam:is (make-instance 'ipaddress:ipv4-interface
                            :address "127.0.0.1"
                            :prefix-length 32)))

(fiveam:test
  create-ipv6-interface
  "Creation of IPv6 interface objects."
  (fiveam:is (make-instance 'ipaddress:ipv6-interface
                            :address "cafe:beef::1"
                            :prefix-length 64)))


;;; Subnets

(fiveam:test
  create-ipv4-subnet
  "Creation of IPv4 subnets"
  (fiveam:is (make-instance 'ipaddress:ipv4-subnet
                            :address "10.80.0.0"
                            :prefix-length 24)))

(fiveam:test
  miscreate-ipv4-subnet
  "Error conditions around IPv4 subnet creation"
  ;; Host address instead of network address
  (fiveam:signals (simple-error ":address value is not a valid network address")
    (make-instance 'ipaddress:ipv4-subnet
                   :address "10.80.0.2"
                   :prefix-length 24))
  ;; Octet value out of range
  (fiveam:signals CL-CIDR-NOTATION:CIDR-PARSE-ERROR
    (make-instance 'ipaddress:ipv4-subnet
                   :address "10.80.256.0"
                   :prefix-length 24))
  ;; Prefix-length too long
  (fiveam:signals (simple-error
                    "prefix-length must be an integer between 0 and 32")
    (make-instance 'ipaddress:ipv4-subnet
                   :address "10.80.0.0"
                   :prefix-length 33)))

(fiveam:test
  render-ipv4-subnet
  "Rendering of an IPv4 subnet in various forms"
  (fiveam:is
    (equal "192.0.2.0"
           (ipaddress:as-string
             (make-instance 'ipaddress:ipv4-subnet
                            :address "192.0.2.0"
                            :prefix-length 24))))
  (fiveam:is
    (equal "192.0.2.0/24"
           (ipaddress:as-cidr
             (make-instance 'ipaddress:ipv4-subnet
                            :address "192.0.2.0"
                            :prefix-length 24)))))

(fiveam:test
  create-ipv6-subnet
  "Creation of IPv6 subnets"
  (fiveam:is (make-instance 'ipaddress:ipv6-subnet
                            :address "cafe:beef::"
                            :prefix-length 64)))

(fiveam:test
  render-ipv6-subnet
  "Rendering of an IPv6 subnet in various forms."
  (fiveam:is
    (equal "cafe:beef:0000:0000:0000:0000:0000:0000"
           (ipaddress:as-string
             (make-instance
               'ipaddress:ipv6-subnet
               :address "cafe:beef::"
               :prefix-length 64))))
  (fiveam:is
    (equal 64
           (ipaddress:prefix-length
             (make-instance
               'ipaddress:ipv6-subnet
               :address "cafe:beef::"
               :prefix-length 64))))
  (fiveam:is
    (equal "cafe:beef:0000:0000:0000:0000:0000:0000/64"
           (ipaddress:as-cidr
             (make-instance
               'ipaddress:ipv6-subnet
               :address "cafe:beef::"
               :prefix-length 64)))))


;;; Utilities

(fiveam:test
  subnetp-ipv4
  "IPv4 subnet comparisons"
  (fiveam:is (ipaddress:subnetp
               (ipaddress:make-ipv4-address "127.0.0.1")
               (ipaddress:make-ipv4-subnet "127.0.0.0/16")))
  (fiveam:is (ipaddress:subnetp
               (ipaddress:make-ipv4-address "127.0.0.1")
               (ipaddress:make-ipv4-interface "127.0.0.1/24")))
  (fiveam:is (ipaddress:subnetp
               (ipaddress:make-ipv4-subnet "127.0.0.0/24")
               (ipaddress:make-ipv4-subnet "127.0.0.0/16")))
  (fiveam:is (not (ipaddress:subnetp
                    (ipaddress:make-ipv4-subnet "127.0.0.0/16")
                    (ipaddress:make-ipv4-subnet "127.0.0.0/24")))))

(fiveam:test
  subnetp-ipv6
  "IPv6 subnet comparisons"
  (fiveam:is (ipaddress:subnetp
               (ipaddress:make-ipv6-address "cafe:beef::1")
               (ipaddress:make-ipv6-subnet "cafe:beef::/64")))
  (fiveam:is (ipaddress:subnetp
               (ipaddress:make-ipv6-address "cafe:beef::1")
               (ipaddress:make-ipv6-interface "cafe:beef::1/64")))
  (fiveam:is (ipaddress:subnetp
               (ipaddress:make-ipv6-subnet "cafe:beef::/64")
               (ipaddress:make-ipv6-subnet "cafe:beef::/48")))
  (fiveam:is (not (ipaddress:subnetp
                    (ipaddress:make-ipv6-subnet "cafe:beef::/48")
                    (ipaddress:make-ipv6-subnet "cafe:beef::/64")))))

(fiveam:test
  ipv4-conveniences
  "Convenience IPv4 functions"
  ;; Address
  (fiveam:is (equalp (ipaddress:as-string (make-instance
                                            'ipaddress:ipv4-address
                                            :address "127.0.0.1"))
                     (ipaddress:as-string (ipaddress:make-ipv4-address "127.0.0.1"))))
  ;; Interface
  (let ((foo (make-instance
               'ipaddress:ipv4-interface
               :address "192.0.2.2"
               :prefix-length 24))
        (bar (ipaddress:make-ipv4-interface "192.0.2.2/24")))
    (fiveam:is (equal "192.0.2.2/24" (ipaddress:as-cidr bar)))
    (fiveam:is (equal (ipaddress:as-string foo) (ipaddress:as-string bar)))
    (fiveam:is (equal (ipaddress:prefix-length foo) (ipaddress:prefix-length bar))))
  ;; Subnet
  (let ((foo (make-instance
               'ipaddress:ipv4-subnet
               :address "192.0.2.0"
               :prefix-length 24))
        (bar (ipaddress:make-ipv4-interface "192.0.2.0/24")))
    (fiveam:is (equal "192.0.2.0/24" (ipaddress:as-cidr bar)))
    (fiveam:is (equal (ipaddress:as-string foo) (ipaddress:as-string bar)))
    (fiveam:is (equal (ipaddress:prefix-length foo) (ipaddress:prefix-length bar)))))

(fiveam:test
  ipv6-conveniences
  "Convenience IPv6 functions"
  ;; Address
  (fiveam:is (equalp (ipaddress:as-string (make-instance
                                            'ipaddress:ipv6-address
                                            :address "cafe:beef::1"))
                     (ipaddress:as-string (ipaddress:make-ipv6-address "cafe:beef::1"))))
  ;; Interface
  (let ((foo (make-instance
               'ipaddress:ipv6-interface
               :address "cafe:beef::22"
               :prefix-length 64))
        (bar (ipaddress:make-ipv6-interface "cafe:beef::22/64")))
    (fiveam:is (equal "cafe:beef:0000:0000:0000:0000:0000:0022/64" (ipaddress:as-cidr bar)))
    (fiveam:is (equal (ipaddress:as-string foo) (ipaddress:as-string bar)))
    (fiveam:is (equal (ipaddress:prefix-length foo) (ipaddress:prefix-length bar))))
  ;; Subnet
  (let ((foo (make-instance
               'ipaddress:ipv6-subnet
               :address "cafe:beef::22"
               :prefix-length 64))
        (bar (ipaddress:make-ipv6-interface "cafe:beef::22/64")))
    (fiveam:is (equal "cafe:beef:0000:0000:0000:0000:0000:0022/64" (ipaddress:as-cidr bar)))
    (fiveam:is (equal (ipaddress:as-string foo) (ipaddress:as-string bar)))
    (fiveam:is (equal (ipaddress:prefix-length foo) (ipaddress:prefix-length bar)))))
