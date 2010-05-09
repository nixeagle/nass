;;;
;;; Primary defined abstractions general to all instruction sets

(in-package :nass.abstract)

(define-binary-class opcode-mixin () ()
  (:documentation "Mixin for all opcode related classes.

Anything to be used as a _primary_ opcode for a machine instruction should
make sure this class is in the superclass list."))

;;; This being an integer is an arbitrary specification Lets try to adhere
;;; to it unless overriding reasons for the _primary_ opcode cannot be
;;; expressed as a single integer.
(defgeneric opcode (assembly-object &key &allow-other-keys)
  (:documentation "Gets ASSEMBLY-OBJECT's opcode as an integer."))