(in-package :ipaddress)

(defun ipv4-address-p (address)
  "Determine whether the supplied string is plausibly an IPv4 address in dotted-quad notation."
  (cl-ppcre:all-matches
    "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
    address))

(defun ipv4-subnet-p (subnet)
  "Determine whether the supplied string is a CIDR-formatted IPv4 subnet"
  (cl-ppcre:all-matches
    "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\/[0-9]{1,2}"
    subnet))

(defun make-ipv4-address (addr)
  "Take a string representation of an address and return an object."
  (make-instance 'ipv4-address :address addr))

(defun make-ipv4-interface (cidrstr)
  "Convert a CIDR representation of an IPv4 interface, e.g. 127.0.0.1/8
  into an ipv4-interface object.
  If just given an address, e.g. 127.0.0.1, it will auto-assign
  a prefix-length of 32."
  (let ((parts (cl-ppcre:split "/" cidrstr)))
    (make-instance
      'ipv4-interface
      :integer (cl-cidr-notation:parse-ip (first parts))
      :prefix-length
      (if (second parts)
          (parse-integer (second parts) :junk-allowed nil)
          32))))

(defun make-ipv4-subnet (cidrstr)
  "Convert a CIDR representation of an IPv4 subnet, e.g. 192.0.0.0/24
  into an ipv4-subnet object."
  (let ((parts (cl-ppcre:split "/" cidrstr)))
    (make-instance
      'ipv4-subnet
      :integer (cl-cidr-notation:parse-ip (first parts))
      :prefix-length (parse-integer (second parts) :junk-allowed nil))))

(defun make-ipv6-address (addr)
  "Take a string representation of an address and return an object."
  (make-instance 'ipv6-address :address addr))

(defun make-ipv6-interface (cidrstr)
  "Convert a CIDR representation of an IPv6 interface, e.g. cafe:beef::1/64
   into an ipv6-interface object.
   If just given an address, e.g. cafe:beef::1it will auto-assign
   a prefix-length of 128."
  (let* ((slash (position #\/ cidrstr))
         (prefixlength (subseq cidrstr (+ slash 1)))
         (addr (subseq cidrstr 0 slash)))
    (make-instance
      'ipv6-interface
      :integer (cl-cidr-notation:parse-ipv6 addr)
      :prefix-length
      (if prefixlength
          (parse-integer prefixlength :junk-allowed nil)
          128))))

(defun make-ipv6-subnet (cidrstr)
  "Convert a CIDR representation of an IPv6 subnet, e.g. cafe:beef::/64
   into an ipv6-subnet object."
  (let* ((slash (position #\/ cidrstr))
         (prefixlength (subseq cidrstr (+ slash 1)))
         (addr (subseq cidrstr 0 slash)))
    (make-instance
      'ipv6-subnet
      :integer (cl-cidr-notation:parse-ipv6 addr)
      :prefix-length (parse-integer prefixlength :junk-allowed nil))))

(defun compare-addresses (addr1 addr2 pos finish)
  "Compare two addresses bitwise. Mostly intended for subnet checking.
   Both addresses are expected in integer form."
  (if (> pos finish)
      ;; If we've checked each of the bits in question, we're good.
      t
      ;; Check the current bit
      (if (= (ldb (byte 1 pos) addr1)
             (ldb (byte 1 pos) addr2))
          ;; If this bit matches, check the next one
          (compare-addresses addr1 addr2 (+ pos 1) finish)
          ;; If it doesn't match, we have our result.
          nil)))

(defmethod subnetp ((ip-entity ip-address) (supernet ip-subnet))
  (if
     ;; Sanity check: is the subnet's prefix-length shorter than the supernet's?
     (<= (prefix-length ip-entity) (prefix-length supernet))
     ;; If so, we already have our answer
     nil
     ;; Passed that test. Now actually compare them.
     ;; Now compare each bit from least- to most-significant, over the length of the
     ;; candidate parent's prefix.
     (compare-addresses (as-integer ip-entity)
                        (as-integer supernet)
                        (- (if (equal (ip-version ip-entity) 4)
                               32
                               128)
                           (prefix-length supernet))
                        (if (equal (ip-version ip-entity) 4)
                            31
                            127))))
