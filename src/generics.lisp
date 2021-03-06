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

(defclass ip-interface (ip-address)
  ((prefix-length
     :reader prefix-length
     :initarg :prefix-length
     :initform (error ":prefix-length argument must be specified.")))
  (:documentation "These represent the addresses configured on an interface, and thus have a prefix-length in addition to the address, so the OS can infer the subnet to which the address belongs."))

(defclass ip-subnet (ip-interface)
  ()
  (:documentation "These represent actual networks, so have a prefix-length and a network address."))

(defgeneric check-address-values (ipaddress)
  (:documentation "Common checks for the string and address representations of an IP address at creation."))

(defmethod initialize-instance :after ((addr ip-address) &key)
  ;; If a string representation was specified, check it
  (check-address-values addr))

(defmethod initialize-instance :after ((iface ip-interface) &key )
  ;; Check the rest of the requirements for an address
  (check-address-values iface)
  ;; Ensure the prefix-length is within the permitted bounds
  (check-prefix-length iface))

(defgeneric check-prefix-length (ipaddress)
  (:documentation "Common checks of the prefix-length slot of an interface or subnet object."))

(defgeneric ip-version (ipaddress)
  (:documentation "Return a single digit, either 4 or 6, indicating whether this is an IPv4 entity or an IPv6 one."))

(defgeneric as-string (ipaddress)
  (:documentation "Return the string representation of this address."))

(defgeneric as-integer (ipaddress)
  (:documentation "Return the integer representation of this address."))

(defgeneric as-cidr (ipaddress)
  (:documentation "Return the CIDR representation of this object. Addresses are rendered with a /32 or /128 prefix-length."))

(defmethod as-cidr (addr)
  (with-output-to-string (str)
               (princ (as-string addr) str)
               (princ #\/ str)
               (princ (prefix-length addr) str)))

(defgeneric subnetp (ip-entity supernet)
  (:documentation "Return a boolean indicating whether the first supplied entity is a subnet of the second."))
