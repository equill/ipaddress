(in-package :ipaddress)


;;; Addresses

(defclass ipv6-address (ip-address)
  ())

(defmethod ip-version ((addr ipv6-address))
  6)

;; Sanity checks
(defmethod check-address-values ((addr ipv6-address))
  (cond
    ;; No value for either representation of an address
    ((and (not (slot-value addr 'str))
          (not (slot-value addr 'int)))
     (error "Either a string or integer representation of the address must be supplied."))
    ;; Integer value is provided, but not a string representation
    ((and (not (slot-value addr 'str))
          (slot-value addr 'int))
     (if (or (< (slot-value addr 'int) 0)
             (> (slot-value addr 'int) 340282366920938463463374607431768211455))
         (error "Integer value is out of range")))
    ;; String representation is provided - we don't care whether an integer one was.
    (t
     (setf (slot-value addr 'int) (cl-cidr-notation:parse-ipv6 (slot-value addr 'str)))))
  ;; Now canonicalise the string representation from the integer version
  (setf (slot-value addr 'str)
        (cl-cidr-notation:ipv6-string (slot-value addr 'int))))

;; From a certain POV, an address is simply a /32 subnet.
;; It certainly makes some operations simpler.
(defmethod prefix-length ((addr ipv6-address))
  128)

;; Memoised calculation
(defmethod as-string ((addr ipv6-address))
  ;; If we don't already have it, now we need to calculate it
  (unless (slot-value addr 'str)
    (setf (slot-value addr 'str)
          (cl-cidr-notation:ipv6-string (slot-value addr 'int))))
  (slot-value addr 'str))


;;; Interfaces

(defclass ipv6-interface (ipv6-address)
  ((prefix-length
     :reader prefix-length
     :initarg :prefix-length
     :initform (error ":prefix-length argument must be specified.")))
  (:documentation "These represent the addresses configured on an interface, and thus have a prefix-length in addition to the address, so the OS can infer the subnet to which the address belongs."))

;; Sanity checks
(defmethod check-prefix-length ((addr ipv6-address))
  (when (or (not (integerp (slot-value addr 'prefix-length)))
            (< (slot-value addr 'prefix-length) 0)
            (> (slot-value addr 'prefix-length) 128))
    (error "prefix-length must be an integer between 0 and 128")))

(defmethod initialize-instance :after ((iface ipv6-interface) &key )
  ;; Check the rest of the requirements for an address
  (check-address-values iface)
  ;; Ensure the prefix-length is within the permitted bounds
  (check-prefix-length iface))


;;; Subnets

(defclass ipv6-subnet (ipv6-interface)
  ()
  (:documentation "These represent actual networks, so have a prefix-length and a network address."))

;; Sanity checks
;; FIXME: add a check to test that it's an actual network address
(defmethod initialize-instance :after ((subnet ipv6-subnet) &key )
  ;; Check the rest of the requirements for an address
  (check-address-values subnet)
  ;; Ensure the prefix-length is within the permitted bounds
  (check-prefix-length subnet))
