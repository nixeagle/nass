
(defpackage #:nass.types
  (:use :cl :eos :convert :nutils)
  (:nicknames :nass-type)
  (:export #:nibble
           #:octet
           #:word
           #:unicode-point
           #:octal-digit
           #:double-word
           #:mips-word
           #:hexadecimal-digit
           #:signed-nibble
           #:signed-octet
           #:signed-word
           #:signed-double-word
           #:signed-mips-word
           #:endian))

(defpackage #:nass.types.mips
  (:use :cl)
  (:nicknames :mips-type)
  (:import-from :nass.types
                :octet)
  (:export :octet
           #:halfword
           #:word
           #:doubleword))


(defpackage #:nass.convert
  (:use :cl :nutils :convert))

(defpackage #:nass.util
  (:use :cl :eos :nutils)
  (:export #:write-binary-file
           #:with-hex))

(defpackage #:nass.arch.amd64
  (:use :cl :nass.util :eos))

(defpackage #:nass.arch.x86
  (:use :cl :nass.util :eos))

(defpackage #:nass.arch.4004
  (:use :cl :nass.util :nutils :convert :eos)
  (:documentation "Really old processor, this is mostly for goofing off
  and learning a bit."))

(defpackage #:nass.elf
  (:use :cl :nass.util :eos))

(defpackage #:nass.goof
  (:use :cl :eos :nutils :binary-data)
  (:import-from :convert #:conv))

(defpackage #:nass.general
  (:use :cl :nutils :binary-data))

(defpackage #:nass.abstract
  (:use :cl :nutils :binary-data :convert))

;;; END
