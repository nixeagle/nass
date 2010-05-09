;;;
;;; Primary defined abstractions general to all instruction sets

(in-package :nass.abstract)

(define-binary-class opcode-mixin () ()
  (:documentation "Mixin for all opcode related classes.

Anything to be used as a _primary_ opcode for a machine instruction should
make sure this class is in the superclass list."))