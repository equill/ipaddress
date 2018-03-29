(in-package :ipaddress)

(defclass ip-address ()
  ((str
     :initarg :address
     :initform nil
     :documentation "Human-friendly string representation of the address")
   (int
     :reader as-integer
     :initarg :integer
     :initform nil
     :documentation "Integer representation of the address, for easier implementation of operations such as comparison."))
  (:documentation "Represents an IP address. If both :address and :integer initargs are supplied, the value of :integer is discarded."))

(defgeneric check-address-values (ipaddress)
  (:documentation "Common checks for the string and address representations of an IP address at creation."))

(defgeneric check-prefix-length (ipaddress)
  (:documentation "Common checks of the prefix-length slot of an interface or subnet object."))

(defgeneric ip-version (ipaddress)
  (:documentation "Return a single digit, either 4 or 6, indicating whether this is an IPv4 entity or an IPv6 one."))

(defgeneric as-string (ipaddress)
  (:documentation "Return the string representation of this address."))

(defgeneric as-integer (ipaddress)
  (:documentation "Return the integer representation of this address."))

(defgeneric subnetp (ip-entity supernet)
  (:documentation "Return a boolean indicating whether the supplied entity is a subnet of the supernet. Treats IPv4 addresses as /32 subnets, and IPv6 addresses as /128 subnets."))
