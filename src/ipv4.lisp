(in-package :ipaddress)

(defclass ipv4-address (ip-address)
  ())

(defmethod ip-version ((addr ipv4-address))
  4)

;; Sanity checks
(defmethod initialize-instance :after ((addr ipv4-address) &key)
  ;; If a string representation was specified, check it
  (if (slot-value addr 'str)
      (let ((int-value (cl-cidr-notation:parse-ip (slot-value addr 'str))))
        ;; If it passed the sanity check, don't waste that work.
        ;; Set the integer value to the generated integer value.
        (setf (slot-value addr 'int) int-value))
      ;; If it wasn't, but an integer representation was, check that
      (if (slot-value addr 'int)
          (if (or (< (slot-value addr 'int) 0)
                  (> (slot-value addr 'int) 4294967296))
              (error "Integer value is out of range"))))
  ;; If neither was supplied, this object isn't much use
  (if (and (not (slot-value addr 'str)) (not (slot-value addr 'int)))
      (error "Either a string or integer representation of the address must be supplied.")))

;; Memoised calculation
(defmethod as-string ((addr ipv4-address))
  ;; If we don't already have it, now we need to calculate it
  (unless (slot-value addr 'str)
    (setf (slot-value addr 'str)
          (cl-cidr-notation:ip-string (slot-value addr 'int))))
  (slot-value addr 'str))

;; No need to calculate this; one way or another, it was set at initialisation.
(defmethod as-integer ((addr ipv4-address))
  (slot-value addr 'int))
