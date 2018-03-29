(in-package :ipaddress)


;; Address objects

(defclass ipv4-address (ip-address)
  ())

(defmethod ip-version ((addr ipv4-address))
  4)

;; Sanity checks
(defmethod check-address-values ((addr ipv4-address))
  (cond
    ;; No value for either representation of an address
    ((and (not (slot-value addr 'str))
          (not (slot-value addr 'int)))
     (error "Either a string or integer representation of the address must be supplied."))
    ;; Integer value is provided, but not a string representation
    ((and (not (slot-value addr 'str))
          (slot-value addr 'int))
     (if (or (< (slot-value addr 'int) 0)
             (> (slot-value addr 'int) 4294967295))
         (error "Integer value is out of range")))
    ;; String representation is provided - we don't care whether an integer one was.
    (t
     (setf (slot-value addr 'int) (cl-cidr-notation:parse-ip (slot-value addr 'str)))))
  ;; Now canonicalise the string representation from the integer version
  (setf (slot-value addr 'str)
        (cl-cidr-notation:ip-string (slot-value addr 'int))))

(defmethod initialize-instance :after ((addr ipv4-address) &key)
  ;; If a string representation was specified, check it
  (check-address-values addr))

;; Memoised calculation
(defmethod as-string ((addr ipv4-address))
  ;; If we don't already have it, now we need to calculate it
  (unless (slot-value addr 'str)
    (setf (slot-value addr 'str)
          (cl-cidr-notation:ip-string (slot-value addr 'int))))
  (slot-value addr 'str))


;;; Interface objects

;;; These represent the addresses configured on an interface,
;;; and thus have a prefix-length in addition to the address,
;;; so the OS can infer the subnet to which the address belongs

(defclass ipv4-interface (ipv4-address)
  ((prefix-length
     :initarg :prefix-length
     :initform (error ":prefix-length argument must be specified."))))

;; Sanity checks
(defmethod check-prefix-length ((addr ipv6-address))
  (when (or (not (integerp (slot-value addr 'prefix-length)))
            (< (slot-value addr 'prefix-length) 0)
            (> (slot-value addr 'prefix-length) 32))
    (error "prefix-length must be an integer between 0 and 32")))

(defmethod initialize-instance :after ((iface ipv4-interface) &key )
  ;; Now check the rest of the requirements for an address
  (check-address-values iface)
  ;; Ensure the prefix-length is within the permitted bounds
  (check-prefix-length iface))
