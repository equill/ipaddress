(in-package :ipaddress-test)

(fiveam:def-suite main)
(fiveam:in-suite main)

(fiveam:test
  create-ipv4-address
  "Test creation of IPv4 address objects."
  (fiveam:is (make-instance 'ipaddress:ipv4-address :address "127.0.0.1")))