(in-package :nass.global-state)

(defvar *assembly-unit* 8
  "Smallest assembly unit. Most architectures will be 8 bits.

Some arch's require a different size from 8 bits, for example MIX if we
ever assemble for that.")

(defvar *machine-size* 16
  "Size of machine in bits.

This defaults to 16 bits for now as its the simplest x86 machine size.")

(defvar *architecture* :x86
  "Type of machine.

  - :x86 is all the various i8086 derived things.
  - others might be: :arm :mips :ppc")

(defvar *instruction-set* '(:general)
  "List of additional instruction sets that should be supported. If the
set is not in the list the assembler will not use the instruction set
and will signal an error.")

;;; END

