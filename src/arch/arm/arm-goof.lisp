(in-package :nass.arch.arm)

(deftype bitstring (&optional (length '*))
  "Finite length bit-vector. Smallest length is 1."
  (check-type length (or positive-fixnum (member *)))
  `(bit-vector ,length))

(declaim (inline make-bitstring))
(defun make-bitstring (length)
  "Makes a vector LENGTH bits and initializes them to 0."
  (declare ((integer 1 #.(- most-positive-fixnum 63)) length)
           (optimize (speed 3) (safety 1) (debug 1)))
  (make-array length :element-type 'bit :initial-element 0))