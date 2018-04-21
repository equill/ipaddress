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

(defclass ipv6-interface (ip-interface ipv6-address)
  ())

;; Sanity checks
(defmethod check-prefix-length ((addr ipv6-address))
  (when (or (not (integerp (slot-value addr 'prefix-length)))
            (< (slot-value addr 'prefix-length) 0)
            (> (slot-value addr 'prefix-length) 128))
    (error "prefix-length must be an integer between 0 and 128")))


;;; Subnets

(defclass ipv6-subnet (ip-subnet ipv6-interface)
  ())

;; Sanity checks
(defmethod initialize-instance :after ((subnet ipv6-subnet) &key )
  ;; FIXME: add a check to test that it's an actual network address
  (check-prefix-length subnet))
