
(in-package :nass.types)

(deftype nibble (&optional (size 1))
  "4 bits, hexidecimal digit."
  `(unsigned-byte ,(* 4 size)))

(deftype signed-nibble (&optional (size 1))
  "4 bits, -8 to 7."
  `(signed-byte ,(* 4 size)))

(deftype octet (&optional (size 1))
  "8 bits, 2 `nibble's, a `byte'."
  `(nibble ,(* 2 size)))

(deftype signed-octet (&optional (size 1))
  "8 bits, 2 `signed-nibble's. -128 to 127."
  `(signed-nibble ,(* 2 size)))

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

(in-package :nass.convert)
(defun integer->bit-base-list
    (integer bits &key size (endian :little-endian)
     &aux (size-given-p (not (not size)))
     (size (if size
               (1- size)
               (floor (log (abs (if (zerop integer)
                                    1   ;Prevent divide by 0
                                    integer))
                           (expt 2 bits))))))
  "Convert INTEGER to base that is 2^BITS.

The result list will be SIZE long."
  (declare (integer integer)
           (positive-fixnum bits)
           ((or null non-negative-fixnum) size)
           (nass-type:endian endian))

  ; negative numbers take an extra element in the list for the sign part
  (let ((size (if (and (not size-given-p) (< integer 0))
                  (1+ size)
                  size)))
    (if (eq :little-endian endian)
        (loop for i from 0 below (* bits (1+ size)) by bits
           collect (ldb (byte bits i) integer))
        (loop for i from (* bits size) downto 0 by bits
           collect (ldb (byte bits i) integer)))))



(define-convert (integer list bit &key size)
  (integer->bit-base-list integer 1 :endian :big-endian :size size))

(define-convert (integer simple-bit-vector &key size)
  (let ((res (conv integer list bit :endian :big-endian :size size)))
    (make-array (length res) :element-type 'bit
                :initial-contents res)))

(define-convert (integer list nass-type:octal-digit &key
                         (endian :little-endian)
                         size)
  (integer->bit-base-list integer 3 :endian endian :size size))

(define-convert (integer list nass-type:hexadecimal-digit &key
                         (endian :little-endian)
                         size)
  (integer->bit-base-list integer 4 :endian endian :size size))

(define-convert (integer list nass-type:signed-octet
                         &key (endian :little-endian)
                         size)
  (integer->bit-base-list integer 8 :endian endian :size size))

(define-convert (integer list nass-type:signed-word &key
                         (endian :little-endian)
                         size)
  (integer->bit-base-list integer 16 :endian endian :size size))



(in-package :nass.util)

(defun call-with-hex (thunk)
  "Bind `*printbase*' and `*print-radix*'.

Base is obviously 16 here."
  (let ((*print-base* 16)
        (*read-base* 16)
        (*print-radix* t))
    (funcall thunk)))

(defmacro with-hex (&body body)
  "Bind printer settings to sensible values for working with hex."
  `(call-with-hex (lambda () ,@body)))

(defmacro write-binary-file ((stream filespec) &body body)
  "Output to a binary file stream that clobbers pre-existing items."
  `(with-open-file (,stream ,filespec
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type 'nass-type:octet)
     ,@body))

(defmacro tbd (&body define-form)
  "State a DEFINE-FORM as needing to be done.

Place a string describing what needs to be done before the define form if
desired."
  (let ((stuff-to-do (if (stringp (car define-form))
                         (format nil "~%~%--------------------~%~A" (pop define-form))
                         "")))
    (with-gensyms (funcallable-name)
      `(let ((,funcallable-name ,(appendf (car define-form)
                                          (list '(error "Not yet implemented!")))))
         (setf (documentation ,funcallable-name 'function)
               (format nil "TBD: ~A~A" (or (documentation ,funcallable-name 'function) "")
                       ,stuff-to-do))))))

(in-package :nass.general)
(defclass abstract-instruction ()
  ()
  (:documentation "Top level abstract instruction class."))

(defclass instruction-metaclass (standard-class)
  ())

(in-package :nass.arch.amd64)


(in-package :nass.arch.4004)

(deftype word (&optional (size 1))
  "Words in 4004 instruction set is 8 bits long."
  `(nass-type:octet ,size))

(defparameter +op-codes+
  (plist-hash-table
   '(:NOP #x00
     :JCN #x01
     :FIM #x02
     :FIN #x03
     :JIN #x03
     :JUN #x04
     :JMS #x05
     :INC #x06
     :ISZ #x07
     :ADD #x08
     :SUB #x09
     :LD  #x0A
     :XCH #x0B
     :BBL #x0C
     :LDM #x0D
     :CLB #xF0
     :CLC #xF1
     :IAC #xF2
     :CMC #xF3
     :CMA #xF4
     :RAL #xF5
     :RAR #xF6
     :TCC #xF7
     :DAC #xF8
     :TCS #xF9
     :STC #xFA
     :DAA #xFB
     :KBP #xFC
     :DCL #xFD
     :SRC #x02
     :WRM #xE0
     :WMP #xE1
     :WRR #xE2
     :WPM #xE3
     :WR0 #xE4
     :WR1 #xE5
     :WR2 #xE6
     :WR3 #xE7
     :SBM #xE8
     :RDM #xE9
     :RDR #xEA
     :ADM #xEB
     :RD0 #xEC
     :RD1 #xED
     :RD2 #xEE
     :RD3 #xEF))
  "Mnemonic to opcode mapping for the 4004 CPU.")



(in-package :nass.elf)


;(flexi-streams:string-to-octets "ELF" :external-format :utf8)



(in-package :nixeagle.helpers.binary-streams)

(defun memory-input-stream (simple-vector)
  "Make a `flexi-streams:in-memory-stream' using SIMPLE-VECTOR.

There are no options for transformers or using part of the simple vector
for simplicity."
  (declare (simple-vector simple-vector))
  (make-in-memory-input-stream simple-vector))

(defun memory-output-stream ()
  "Shorter name for making an output stream with no transformers."
  (make-in-memory-output-stream))

(defmacro with-output-to-memory ((symbol &key (element-type 'nass-type:octet))
                                 &body body)
  "Output to a `flexi-streams:in-memory-stream'."
  `(with-open-stream (,symbol (flexi-streams:make-in-memory-output-stream
                               :element-type ',element-type))
     ,@body))

(defun call-with-output-to-octet-array (thunk in-memory-stream)
  "Pass call THUNK with IN-MEMORY-STREAM returning stream's octed array."
  (funcall thunk in-memory-stream)
  (flexi-streams:get-output-stream-sequence in-memory-stream))

(defmacro with-output-to-octet-array (symbol &body body)
  "Output to memory stream identified by SYMBOL, returning octet array."
  `(with-output-to-memory (,symbol)
     (call-with-output-to-octet-array (lambda (,symbol)
                                        ,@body)
                                      ,symbol)))


(in-package :nass.goof)


;;; http://paste.lisp.org/display/98095
;;; Permission to use requested/received over irc
;;; Thanks to Zach Beane of Wigflip
(defun bits (integer &optional (size 8))
  (format t "~&~v,'0B~%" size integer))

;;; Thanks to Zach Beane of Wigflip
(defun hex (integer &optional (size 4))
  (format t "~&~v,'0X~%" size integer))

;;; END
