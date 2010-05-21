(in-package :nass.arch.x86oids)

(in-suite* :nisp)
(in-suite* :nass :in :nisp)
(in-suite* root :in :nass)
;;; FIXME: Are there better longer named forms of these?
;;; These are taken directly from the manuals and specifications.
(deftype r8 ()
  "8 bit address locations on 16 bit."
  '(member :al :cl :dl :bl :ah :ch :dh :bh))

(deftype r16 ()
  "16 bit registers."
  '(member :ax :cx :dx :bx :sp :bp :si :di))

(deftype r32 ()
  "32 bit registers."
  '(member :eax :ecx :edx :ebx :esp :ebp :esi :edi))

(deftype r64 ()
  "64 bit registers."
  '(member :rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi
    :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))

(deftype mm ()
  '(member :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7))

(deftype xmm ()
  '(member :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7))

(deftype segment-register ()
  "sreg: Segment register names."
  '(member :es :cs :ss :ds :fs :gs))

(deftype eee ()
  '(member :cr0 :cr2 :cr3 :cr4 :dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7))

(deftype x87-stack-register ()
  "x87 operates with a stack, not directly addressable registers."
  '(member :st0 :st1 :st2 :st3 :st4 :st5 :st6 :st7))

(deftype mod-rem-r/m-register ()
  "All valid x86oid register names."
  '(or r8 r16 r32 r64 mm xmm segment-register eee x87-stack-register
    ;; FIXME: These below should become their own subtypes
    (member :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b
     :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w
     :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d
     :XMM8 :XMM9 :XMM10 :XMM11 :XMM12 :XMM13 :XMM14 :XMM15
     :cr8)))

(deftype segment-override-prefix-codes ()
  "Octets that override to a specific segment:

Segment names are cs ss ds es fs gs."
  '(member #x2e #x36 #x3e #x26 #x64 #x65))

(deftype opcode-prefix-codes ()
  "Octets that can appear before the primary opcode."
  '(or segment-override-prefix-codes (member #x66 #x67)))

(defclass segment ()
  ((segment :initform :ds
            :type segment-register
            :accessor segment
            :initarg :segment)))

(defclass register-indirect (segment)
  ((indirect-register :initform :bx
                      :accessor register-indirect
                      :initarg :indirect
                      :type (member :bx :bp :si :di))))

(defun indirect (register &optional (segment :ds))
  (make-instance 'register-indirect
                 :indirect register
                 :segment segment))

(defun indirect-displacement (register displacement &optional (segment :ds))
  (make-instance 'indirect-displacement
                 :indirect register
                 :displacement displacement
                 :segment segment))

(defclass indirect-displacement (register-indirect displacement)
  ())

(defclass indirect-base (register-indirect)
  ((base :initform :bx
         :accessor indirect-base
         :initarg :base
         :type (member :bx :bp))))

(defun indirect-base (register base &optional (segment :ds))
  (make-instance 'indirect-base-displacement
                 :indirect register
                 :segment segment
                 :base base))

(defclass indirect-base-displacement (indirect-base displacement) ())

(defun indirect-base-displacement (register base displacement
                                   &optional (segment :ds))
  (make-instance 'indirect-base-displacement
                 :indirect register
                 :displacement displacement
                 :segment segment
                 :base base))

(defun direct (address &key (segment :ds))
  (make-instance 'displacement
                 :segment segment
                 :displacement address))

(deftype lock-prefix-code ()
  ;; DOCME!
  '(member #xF0))

(deftype string-instruction-prefix-codes ()
  "Prefix octets that are allowed only for string instructions."
  '(member #xF2 #xF3))

(deftype instruction-prefix-codes ()
  "All prefixes that appear before an instruction."
  '(or lock-prefix-code string-instruction-prefix-codes
    opcode-prefix-codes))

(defclass mod-reg-r/m ()
  ((mod :initarg :mod
        :documentation "bits 7-6")
   (reg :initarg :reg
        :documentation "bits 5-3")
   (r/m :initarg :r/m
        :documentation "bits 2-0")))

(deftype valid-x86-machine-size ()
  "Valid machine sizes are 16,32 and 64 bit.

There are no standards for 128 bit x86 machines known so far."
  '(member 16 32 64))

;;; Needs to be more specific...
(deftype immediate ()
  "Anything that can be treated as an immediate value."
  t)

(defclass displacement (segment)
  ((displacement :initform 0
                 :accessor displacement
                 :initarg :displacement))
  (:documentation "Describe how far to displace."))

(defgeneric encode-reg-r/m (destination source size)
  (:documentation "For x86 only, encode the mod-reg-r/m stuff.

Size needs to be 16, 32, or 64 only.")
  (:method :before ((destination t) (source t) (size t))
           "Check that SIZE is a `valid-x86-machine-size'."
           (check-type size valid-x86-machine-size)))

(defmethod encode-reg-r/m ((destination symbol)
                           (source symbol)
                           (size (eql 16)))
  (declare ((or r8 r16 r32 x87-stack-register
                mm xmm) destination)
           ((or r8 r16 r32 mm xmm eee segment-register) source))
  (logior #b11000000 (reg-reg destination source)))

(defun encode-displacement (displacement size)
  "Compute x86 DISPLACEMENT of SIZE.

Doing this means reversing the order of the octets.
   #xFF01 => #x01FF."
  (declare ((nass.types:octet 4) displacement)
           ((member 8 16 32) size))
  (let ((size (1- (ash size -3))))
    (loop for i from size downto 0
       for opp from 0 to size
       summing (ash (ldb (byte 8 (* i 8)) displacement) (* opp 8))
       do (print (list i opp (* i 8) (ldb (byte 4 (* i 8)) displacement)
                       (ash (ldb (byte 8 (* i 8)) displacement) (* opp 8))))
       )))

(defmethod encode-reg-r/m ((destination symbol)
                           (source displacement)
                           (size (eql 16)))
  (declare ((or r8 r16 r32 mm xmm eee segment-register) destination))
  (let ((result 0))
    (setf (ldb (byte 3 19) result) (encode-reg-bits destination))
    (setf (ldb (byte 3 16) result) #b110)
    (setf (ldb (byte 8 16) result)
          (encode-displacement (displacement source) 16))
    result))

(defmethod encode-reg-r/m ((destination symbol)
                           (source register-indirect)
                           (size (eql 16)))
  (declare ((or r8 r16 r32 mm xmm eee segment-register) destination))
  (assert (member (register-indirect source) '(:si :di :bx)))
  (logior #x00
          (ash (encode-reg-bits destination) 3)
          (+ 4 (position (register-indirect source) #(:si :di :bp :bx)))))

(defmethod encode-reg-r/m ((destination symbol)
                           (source indirect-displacement)
                           (size (eql 16)))
  "For example: ADD [bx+1], al

Does not append displacement value."
  (logior
   (if (> (displacement source) #xFF)
       #b10000000
       #b01000000)
   (ash (encode-reg-bits destination) 3)
   (+ 4 (position (register-indirect source) #(:si :di :bp :bx)))))

(defmethod encode-reg-r/m ((destination symbol)
                           (source indirect-base)
                           (size (eql 16)))
  (declare ((or r8 r16 r32 mm xmm eee segment-register) destination))
  (logior
   (ash (encode-reg-bits destination) 3)
   (ash (position (the (member :si :di) (register-indirect source))
                  '(:si :di))
        (position (the (member :bx :bp) (indirect-base source))
                  '(:bx :bp)))))

(defmethod encode-reg-r/m ((destination symbol)
                           (source indirect-base-displacement)
                           (size (eql 16)))
  (logior
   (if (> (displacement source) #xFF)
       #b10000000
       #b01000000)
   (ash (encode-reg-bits destination) 3)
   (ash (position (the (member :si :di) (register-indirect source))
                  '(:si :di))
        (position (the (member :bx :bp) (indirect-base source))
                  '(:bx :bp)))))

(defmethod encode-object ((displacement displacement))
  (let* ((disp (displacement displacement))
        (disp-octet-size (floor (log disp 256))))
    (values
     (loop for i from disp-octet-size downto 0
        for opp from 0 to disp-octet-size
        summing (ash (ldb (byte 8 (* i 8)) disp) (* opp 8)))
     (1+ disp-octet-size))))

(defclass sib ()
  ((scale :initarg :scale
          :documentation "bits 7-6")
   (index :initarg :index
          :documentation "bits 5-3")
   (base :initarg :base
         :documentation "bits 2-0")))

(defclass instruction (mod-reg-r/m sib)
  ((mnemonic :type keyword
             :initarg :mnemonic
             :accessor mnemonic)
   (source :initarg :source
           :accessor source)
   (destination :initarg :destination
                :accessor destination)
   (instruction-prefix
    :accessor instruction-instruction-prefix
    :initarg :instruction-prefix)
   (address-size-prefix
    :accessor instruction-address-size-prefix
    :initarg :address-size)
   (operand-size-prefix
    :accessor instruction-operand-size-prefix
    :initarg :operand-size)
   (segment-override-prefix
    :accessor instruction-segment-override
    :initarg :segment-override)
   (opcode
    :accessor instruction-opcode
    :initarg :opcode)
   (sib
    :accessor instruction-sib
    :initarg :sib)
   (displacement
    :accessor instruction-displacement
    :initarg :displacement)
   (immediate
    :accessor instruction-immediate
    :initarg :immediate))
  (:documentation "X86 assembly instruction.

Everything we can possibly need to make an x86 assembly instruction, if
there is something missing for extended instruction types, add them to
this class or inherit it."))

;;; OPTIMIZE: If anyone cares or thinks this is too slow: Put these in the
;;; order of expected use. The items closer to the front of this array
;;; will be looked up faster then the items at the end. So put the more
;;; common instances at the front, however! do not change the order of the
;;; rows themselves.
(defparameter +register-list+
  #(:al :cl :dl :bl :ah :ch :dh :bh     ;16 and 32 bit regs
    :ax :cx :dx :bx :sp :bp :si :di
    :eax :ecx :edx :ebx :esp :ebp :esi :edi
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
    :es :cs :ss :ds :fs :gs :reserved :reserved
    :cr0 :invalid :cr2 :cr3 :cr4 :invalid :invalid :invalid
    :dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7
    ;; x87 FPU
    :st0 :st1 :st2 :st3 :st4 :st5 :st6 :st7
    ;; These below apply only for amd64 arch.
    :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b
    :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w
    :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d
    :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :XMM8 :XMM9 :XMM10 :XMM11 :XMM12 :XMM13 :XMM14 :XMM15
    :cr8 :invalid :invalid :invalid :invalid :invalid :invalid :invalid)
  "table of all valid x86oid registers.

These are in order such that:
  (mod (position keyword `+register-list+') 8)
will return the correct bit sequence as an integer.")

(defun encode-reg-bits (reg-name)
  "Compute 3 bit number corresponding to REG-NAME."
  (declare (mod-rem-r/m-register reg-name)
           (optimize (speed 3) (space 0)))
  (mod (position reg-name (the simple-vector +register-list+)) 8))

;;; Inspired by movitz's assembler which is written by:
;;; Frode Vatvedt Fjeld <frodef@acm.org>
;;; See /COPYING-MOVITZ
(defparameter *opcode-disassemblers*
  (make-array '(3 256) :initial-element nil :element-type '(or symbol null list function))
  "Arrays of disassemblers for 16/32/64 bit machine code.

These disassemblers are indexed by opcode.

The 0th row is 16 bit, 1st row is 32 bit, 3rd row is 64 bit. Checks are
made from the highest requested down to the lowest requested. If an opcode
means the same thing in 16 bit as it does in 64 bit only the 16 bit
translation needs to be defined.")

(defparameter *asm-mnemonic* (make-hash-table :test #'eq)
  "Lookup table for x86 mnemonics")

(defstruct (instruction-entry (:copier nil))
  (architecture nil :type (or null nass.types:architectures))
  (size 16 :type (member 4 8 16 32 64 128 256 512 1024))
  (types (error "missing arg") :type t)
  (priority 0 :type fixnum)
  (test-function nil :type (or null function))
  (function (error "missing arg")))

(declaim (inline (setf opcode-disassembler)))

(defun opcode-disassembler (opcode &optional (size 16))
  "Get disassembler(s) from `*opcode-disassemblers*'."
  ;; FIXME: SIZE should default to a dynamic variable
  (declare ((member 16 32 64) size)
           ((mod 256) opcode)
           (optimize (speed 3)))
  (let ((result (aref (the (simple-array (or null symbol function list))
                        *opcode-disassemblers*) (ash size -5) opcode)))
    (if result
        result
        (unless (= 16 size)
          (opcode-disassembler opcode (ash size -1))))))

(defun (setf opcode-disassembler) (value opcode &optional (size 16))
  (declare ((or null function list symbol) value)
           ((member 16 32 64) size)
           ((mod 256) opcode)
           (optimize (speed 3) (safety 0)))
  (setf (aref (the (simple-array (or null function symbol list)) *opcode-disassemblers*)
              (ash size -5) opcode)
        value))

(defmacro define-single-opcode-instruction/+r (name opcode)
  (check-type name (or simple-string symbol))
  (check-type opcode nass.types:octet)
  `(defun ,(format-symbol t "ASSEMBLE-~A/+R" name) (opcode-register-field)
     (declare ((or r64 r32 r16 ) opcode-register-field))
     (logior ,opcode (encode-reg-bits opcode-register-field))))

(define-single-opcode-instruction/+r push #x50)
(define-single-opcode-instruction/+r pop #x58)
(define-single-opcode-instruction/+r inc #x40)
(define-single-opcode-instruction/+r dec #x48)
;;; needs to exclude some registers as #x90 is NOP
;;(define-single-opcode-instruction/+r xchg #x90)

(defmacro define-single-opcode-instruction (name opcode)
  (check-type name (or string symbol))
  (check-type opcode (nass.types:octet))
  `(progn
     (defun ,(format-symbol t "ASSEMBLE-~A" name) ()
       ,opcode)
     (defun ,(format-symbol t "DISASSEMBLE-~A" name) (octet)
       (declare (nass.types:octet octet))
       (assert (= ,opcode octet))
       ,(make-keyword (if (stringp name)
                          (string-upcase name)
                          name)))
     (setf (opcode-disassembler ,opcode)
           ',(format-symbol t "DISASSEMBLE-~A" name))))


(define-single-opcode-instruction nop #x90)


;;; FIXME: Do something more sensible that gives an indication of how big
;;; or how small if possible.
(defun operand-size (primary-opcode)
  "Operand size for PRIMARY-OPCODE.

For now we return :bigger and :smaller."
  (declare (nass.types:octet primary-opcode))
  (if (logbitp 0 primary-opcode)
      :bigger
      :smaller))

(defun reg-reg (destination source)
  (declare (mod-rem-r/m-register destination source))
  (logior #xC0
          (ash (encode-reg-bits source) 3)
          (encode-reg-bits destination)))



;;; Ugly first shot at MOV. This "works" assuming we are doing reg-reg.
(defun assemble-mov (destination source &key (size *machine-size*))
  (declare (mod-rem-r/m-register destination source)
           ((member 16 32 64) size))
  (logior (etypecase destination
            (r8 (check-type source r8)
                #x8800)
            (r16 (check-type source r16)
                 (assert (= 16 size))
                 #x8900)
            (r32 (check-type source r32)
                 (assert (or (= 32 size) (= 64 size)))
                 #x8900))
          (reg-reg destination source)))


(test (encode-displacement :suite root)
  (is (= #xFF01 (encode-displacement #x01FF 16)))
  (is (= #x01FF (encode-displacement #xFF01 16)))
  (is (= #xFF00FF00 (encode-displacement #x00FF00FF 32)))
  (is (= #xA10F (encode-displacement #xFA1 16))))

;;; END
