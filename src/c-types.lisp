;;; These are currently x86oid based.
(in-package :nass.ctypes)

(deftype bool ()
  "Valid values are 0 and 1.

Technically a boolean value can be any value, we are restricting the value
of bool to 0 and 1 so we can take advantage of cheap boolean
expressions. instad of branching."
  '(mod 2))

(deftype char ()
  "C chars are always 1 byte."
  '(octet 1))

(deftype short-int ()
  "C short int is 2 bytes/octets long on all versions."
  '(octet 2))

(deftype int ()
  "C ints are always 4 bytes/octets long on all versions."
  '(octet 4))

(deftype float ()
  "C floats are always 4 bytes/octets long on all versions."
  '(octet 4))

(deftype double ()
  "C doubles are always 8 bytes/octets long on all versions."
  '(octet 8))


;;; END
