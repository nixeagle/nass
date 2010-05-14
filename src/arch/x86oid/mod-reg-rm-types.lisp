(in-package :nass.arch.x86oids)

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

(deftype mm ()
  '(member :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7))

(deftype xmm ()
  '(member :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7))

(deftype 16-bit-segment-register ()
  "sreg: Segment register names."
  '(member :es :cs :ss :ds :fs :gs))

(deftype eee ()
  '(member :cr0 :cr2 :cr3 :cr4 :dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7))

(deftype mod-rem-r/m-register ()
  "All valid x86oid register names."
  '(or r8 r16 r32 mm xmm 16-bit-segment-register eee
    ;; FIXME: These below should become their own subtypes
    (member :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b
     :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w
     :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d
     :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
     :XMM8 :XMM9 :XMM10 :XMM11 :XMM12 :XMM13 :XMM14 :XMM15
     :cr8)))

(deftype valid-segment-override-prefixes ()
  "Octets that override to a specific segment:

Segment names are cs ss ds es fs gs."
  '(member #x2e #x36 #x3e #x26 #x64 #x65))

(deftype valid-opcode-prefixes ()
  "Octets that can appear before the primary opcode."
  '(or valid-opcode-prefixes (member #x66 #x67)))


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
  (make-array '(3 256) :initial-element nil :element-type '(or null list function))
  "Arrays of disassemblers for 16/32/64 bit machine code.

These disassemblers are indexed by opcode.

The 0th row is 16 bit, 1st row is 32 bit, 3rd row is 64 bit. Checks are
made from the highest requested down to the lowest requested. If an opcode
means the same thing in 16 bit as it does in 64 bit only the 16 bit
translation needs to be defined.")

(declaim (inline (setf opcode-disassembler)))

(defun opcode-disassembler (opcode &optional (size 16))
  "Get disassembler(s) from `*opcode-disassemblers*'."
  ;; FIXME: SIZE should default to a dynamic variable
  (declare ((member 16 32 64) size)
           ((mod 256) opcode)
           (optimize (speed 3)))
  (let ((result (aref (the (simple-array (or null function list))
                        *opcode-disassemblers*) (ash size -5) opcode)))
    (if result
        result
        (unless (= 16 size)
          (opcode-disassembler opcode (ash size -1))))))

(defun (setf opcode-disassembler) (value opcode &optional (size 16))
  (declare ((or null function list) value)
           ((member 16 32 64) size)
           ((mod 256) opcode)
           (optimize (speed 3) (safety 0)))
  (setf (aref (the (simple-array (or null function list)) *opcode-disassemblers*)
              (ash size -5) opcode)
        value))
;;; END
