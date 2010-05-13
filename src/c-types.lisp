(in-package :nass.ctypes)

(deftype char ()
  "C chars are always 1 byte."
  '(octet 1))

