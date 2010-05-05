
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
  (:use :cl :nutils))

(defpackage #:nass.util
  (:use :cl :eos)
  (:export #:write-binary-file
           #:with-hex))

(defpackage #:nass.arch.amd64
  (:use :cl :nass.util :eos))

(defpackage #:nass.arch.x86
  (:use :cl :nass.util :eos))

(defpackage #:nass.arch.4004
  (:use :cl :nass.util :convert :eos)
  (:documentation "Really old processor, this is mostly for goofing off
  and learning a bit."))

(defpackage #:nass.elf
  (:use :cl :nass.util :eos))

(defpackage #:nixeagle.helpers.binary-streams
  (:use :cl :flexi-streams)
  (:nicknames :nh-binary-streams)
  (:documentation "Various helper functions for dealing with binary things.")
  (:export #:memory-input-stream
           #:memory-output-stream
           #:with-output-to-memory
           #:with-output-to-octet-array))

(defpackage #:nass.goof
  (:use :cl :eos
        ;; Going to allow nh-binary-streams to be used here as this is
        ;; more or less my testing package and those functions are pretty
        ;; useful here
        :nh-binary-streams)
  (:import-from :convert #:conv))