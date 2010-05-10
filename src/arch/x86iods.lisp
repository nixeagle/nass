(in-package :nass.arch.x86oids)

(define-binary-class x86oid (little-endian) ())

(define-binary-class mod-reg-r/m (x86oid)
  ((mod :bits 2 :initarg :mod :type (mod 4))
   (r/m :bits 3 :initarg :r/m :type (mod 8))
   (reg/opcode :bits 3 :initarg :reg/opcode :type (mod 8))))

;;; END