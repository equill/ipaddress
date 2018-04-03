(in-package :ipaddress)

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
