(in-package :nass.types)

(deftype architectures ()
  "Valid machine architecture names as keywords."
  '(member :x86 :i8086))

(deftype nibble (&optional (size 1))
  "4 bits, hexidecimal digit."
  `(unsigned-byte ,(* 4 size)))

(deftype signed-nibble (&optional (size 1))
  "4 bits, -8 to 7."
  `(signed-byte ,(* 4 size)))

(deftype word (&optional (size 1))
  "16 bits, 2 `octet's or 2 bytes."
  `(octet ,(* 2 size)))

(deftype signed-word (&optional (size 1))
  "16 bits, 2 `signed-octet's, -32768 to 32767."
  `(signed-octet ,(* 2 size)))

(deftype double-word (&optional (size 1))
  "Pair of words, 32 bits, 4 `octet's."
  `(word ,(* 2 size)))

(deftype signed-double-word (&optional (size 1))
  "Pair of `signed-word's. 32 bits, -2147483648 to 2147483647."
  `(signed-word ,(* 2 size)))

(deftype mips-word (&optional (size 1))
  "32 bits, 4 `octet's, 2 x88 `word's."
  `(double-word ,(* 2 size)))

(deftype signed-mips-word (&optional (size 1))
  "32 bits, same as `signed-double-word'."
  `(signed-double-word ,size))

(deftype unicode-point ()
  "Any value from 0 to #x10FFFF.

Reference: http://unicode.org/glossary/#C under 'Code Point'."
  '(integer 0 #x10FFFF))

(deftype octal-digit (&optional (digits 1))
  "0 to 7, octal numbers."
  `(mod ,(expt 8 digits)))

(deftype hexadecimal-digit (&optional (digits 1))
  "#x0 to #xF. Range of numbers from 0 to 15."
  `(mod ,(expt 16 digits)))

(deftype endian ()
  "Computers encode bytes two different ways."
  '(member :big-endian :little-endian))

(in-package :nass.types.mips)

(deftype halfword (&optional size)
  "16 bits, also referred to as: H"
  `(octet ,(* 2 size)))

(deftype word (&optional size)
  "32 bits, also referred to as: W"
  `(halfword ,(* 2 size)))

(deftype doubleword (&optional size)
  "64 bits, also referred to as: D"
  `(word ,(* 2 size)))