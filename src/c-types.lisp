;;; These are currently x86oid based.
(in-package :nass.ctypes)

(deftype char ()
  "C chars are always 1 byte."
  '(octet 1))

(deftype short-int ()
  "C short int is 2 bytes long on all versions."
  '(octet 2))

