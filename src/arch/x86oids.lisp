(in-package :nass.arch.x86oids)

(define-binary-class x86oid (little-endian) ())

(macrolet ((define-type (name full-name width &rest members)
             "Define NAME and FULL-NAME as types that make sense in the reg bits.

MEMBERS is a list of acceptable values for each machine register."
             (declare (symbol name full-name)
                      ((unsigned-byte 1) width))
             (let ((documentation
                    (format nil "Mnemonic ~A stands for ~(~A~).~
                                ~%~%WIDTH must be ~D." name
                                (substitute #\Space #\- (symbol-name full-name))
                                width)))
               `(progn
                  (deftype ,full-name (&key (width ,width))
                    ,documentation
                    (when (= width ,width)
                      '(member ,@members)))
                  (deftype ,name (&key (width ,width))
                    ,documentation
                    (when (= width ,width)
                      ',full-name))))))
  (define-type ax accumulator-register 1 0)
  (define-type bx base-address-register 1 3)
  (define-type cx count-register 1 1)
  (define-type dx data-register 1 2)
  (define-type sp stack-pointer 1 4)
  (define-type bp base-pointer 1 5)
  (define-type si string-index 1 6)
  (define-type di data-index 1 7)
  ;; low and high bits
  (define-type al low-accumulator-register 0 0)
  (define-type bl low-base-address-register 0 3)
  (define-type cl low-count-register 0 1)
  (define-type dl low-data-register 0 2)
  (define-type ah high-accumulator-register 0 4)
  (define-type bh high-base-address-register 0 7)
  (define-type ch high-count-register 0 5)
  (define-type dl high-data-register 0 6))

(define-binary-class mod-reg-r/m (x86oid)
  ((mod :bits 2 :initarg :mod :type (mod 4))
   (r/m :bits 3 :initarg :r/m :type (mod 8))
   (reg/opcode :bits 3 :initarg :reg/opcode :type (mod 8))))


;;; See http://www.arl.wustl.edu/~lockwood/class/cs306/books/artofasm/Chapter_3/CH03-3.html#HEADING3-102
;;;
;;; These do _NOT_ seem to match up with what Intel64 mentions or what
;;; ndisasm shows for 16, 32 or 64 bits.
(deftype valid-direct-address-instruction-opcodes ()
  "Valid keywords matching up to opcode classes.

These keywords will translate to the correct binary opcodes."
  '(member :special :or :and :cmp :sub :add
    :mov-reg-mem :mov-reg-reg :mov-reg-const
    :mov-mem-reg))

(deftype valid-direct-address-instruction-rr ()
  "Keywords matching with the ax, bx, cx, dx registers."
  '(member :ax :bx :cx :dx
    :accumulator-register :base-address-register
    :count-register :data-register))

(deftype valid-direct-address-instruction-mmm ()
  "Keywords matching rr registers and 4 other addressing modes."
  '(or valid-direct-address-instruction-rr
    (member :immediate :indirect :indexed :direct)))

(define-binary-class direct-address-opcode-byte
    (x86oid)
  ((instruction-class :bits 3
                      :type valid-direct-address-instruction-opcodes
                      :initarg :instruction-class)
   (register :bits 2
             :type valid-direct-address-instruction-rr
             :documentation "Registers only."
             :initarg :register)
   (memory :bits 3
           :type valid-direct-address-instruction-mmm
           :documentation "register, immediate, indirect, indexed, direct."
           :initarg :memory)))

(macrolet ((define-binary-slot-value (keyword value-integer &optional docstring)
             (declare (type (or null string) docstring)
                      (keyword keyword)
                      (non-negative-integer value-integer))
             `(defmethod binary-slot-value
                  ((value (eql ,keyword)) (slot t)
                   (name t) (object direct-address-opcode-byte))
                ,@(list docstring
                 value-integer))))
  (define-binary-slot-value :ax 0)
  (define-binary-slot-value :bx 1)
  (define-binary-slot-value :cx 2)
  (define-binary-slot-value :dx 3)
  (define-binary-slot-value :indirect 4
    "Indirect addressing mode, the value in the [bx] register.")
  (define-binary-slot-value :indexed 4
    "Indexed addressing mode, constant and the value in [bx] register.")
  (define-binary-slot-value :direct 6
    "Direct addressing mode, fetch location in memory, example: [1000].")
  (define-binary-slot-value :immediate 7
    "Immediate addressing mode, put constant in destination register.")
  (define-binary-slot-value :special 0)
  (define-binary-slot-value :or 1)
  (define-binary-slot-value :and 2)
  (define-binary-slot-value :cmp 3)
  ;; Long form of cmp (compare)... sdkmvx tell me your thoughts if this
  ;; and subtract below are useful or if we should just stick to the short
  ;; assembly names. They irk me though.
  (define-binary-slot-value :compare 3)
  (define-binary-slot-value :sub 4)
  (define-binary-slot-value :subtract 4)
  (define-binary-slot-value :add 5)
  (define-binary-slot-value :mov-reg-mem 6)
  (define-binary-slot-value :mov-reg-reg 6)
  (define-binary-slot-value :mov-reg-const 6)
  (define-binary-slot-value :mov-mem-reg 7))

(define-binary-class zero-operand-opcode (x86oid)
  ((opcode :type non-negative-fixnum))
  (:documentation "No arguments or mod-rem-r/m bytes.

These opcodes should be able to be executed on their own.

For example: HLT, NOP

Do not use this for PUSH or POP or anything that includes registers in the
opcode. These can be encoded in a simpler manner."))

#+ ()
(defmacro define-simple-zero-operand-opcode
    (name superclasses (opcode &optional (octets 1))
     &optional docstring)
  `(define-binary-class ,name (,@superclasses zero-operand-opcode)
     ((opcode :initform ,opcode :octets ,octets))
     (:documentation ,docstring)))

(define-binary-class nop (zero-operand-opcode)
  ((opcode :initform #x90 :octets 1))
  (:documentation "Does no instruction.

There are multibyte NOPs as well, these are TBD."))

(define-binary-class halt (zero-operand-opcode)
  ((opcode :initform #xf4 :octets 1))
  (:documentation "hlt: Halts the CPU, Requires privilege level 0."))

(define-binary-class xlatb (zero-operand-opcode)
  ((opcode :initform #xD7 :octets 1))
  (:documentation "Table lookup translation.

Grab the contents of a table using the value in the
`low-accumulator-register' (AL) as the index, and destructively replaces
the value of AL with the contents indexed by AL.

  (setq low-accumulator-register
        (aref <some table>
              (+ DS:rBX (zero-extend low-accumulator-register))))

There are two names for this, xlat and xlatb, xlatb stands for the
assembly instruction with no arguments (explicit). xlat allows one operand
for the purpose of documentation and is not yet supplied in this
assembler."))

(define-binary-class wait (zero-operand-opcode)
  ((opcode :initform #x9B :octets 1))
  (:documentation
   "wait: Check for pending unmasked floating point exceptions.

Also known as fwait."))

(define-binary-class clear-carry (zero-operand-opcode)
  ((opcode :initform #xF8 :octets 1))
  (:documentation "clc: Clear carry bit."))

(define-binary-class complement-carry (zero-operand-opcode)
  ((opcode :initform #xF5 :octets 1))
  (:documentation "cmc: Complement carry bit."))

(define-binary-class set-carry (zero-operand-opcode)
  ((opcode :initform #xF9 :octets 1))
  (:documentation "stc: Set carry bit."))

(deftype valid-opcode-register-fields ()
  "8 valid fields for the 3 bits in an opcode (when used)."
  '(member :ax :bx :cx :dx :sp :dp :si :di))

(define-binary-class opcode-register-field (x86oid)
  ((register :bits 3
             :type valid-opcode-register-fields
             :documentation "Access general purpose registers in opcode."
             :initarg :register))
  (:documentation "mixin for opcodes that use this instructin layout.

These are typically no operand instructions such as POP and PUSH."))

(macrolet ((define-binary-slot-value (keyword value-integer &optional docstring)
             (declare (type (or null string) docstring)
                      (keyword keyword)
                      (non-negative-integer value-integer))
             `(defmethod binary-slot-value
                  ((value (eql ,keyword)) (slot bit-field-slot-definition)
                   (name (eql 'register)) (object opcode-register-field))
                ,@(list docstring
                        value-integer))))
  (define-binary-slot-value :ax 0)
  (define-binary-slot-value :cx 1)
  (define-binary-slot-value :dx 2)
  (define-binary-slot-value :bx 3)
  (define-binary-slot-value :sp 4)
  (define-binary-slot-value :bp 5)
  (define-binary-slot-value :si 6)
  (define-binary-slot-value :di 7))

(define-binary-class push (opcode-register-field)
  ((opcode :bits 5 :initform #b01010)
   register))

(define-binary-class pop (opcode-register-field)
  ((opcode :bits 5 :initform #b01011)
   register))

(define-binary-class dec (opcode-register-field)
  ((opcode :bits 5 :initform #b01001)
   register))

;;; END
