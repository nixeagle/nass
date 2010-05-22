(in-package :nass.arch.arm)

(deftype bitstring (&optional (length '*))
  "Finite length bit-vector. Smallest length is 1."
  (check-type length (or positive-fixnum (member *)))
  (if (eq '* length)
      'simple-bit-vector
      `(simple-bit-vector (,length))))

(declaim (inline make-bitstring))
(defun make-bitstring (length)
  "Makes a vector LENGTH bits and initializes them to 0."
  (declare ((integer 1 #.(- most-positive-fixnum 63)) length)
           (optimize (speed 3) (safety 1) (debug 1)))
  (make-array length :element-type 'bit :initial-element 0))

(defun top-bit (bitstring)
  "Grab most significent bit."
  (declare (bitstring bitstring))
  (aref bitstring 0))

(defun concatenate-bitstrings (bitstring1 bitstring2)
  "Concat BITSTRING1 and BITSTRING2.

You can do (concatenate 'bitstring ....) as well."
  (declare (bitstring bitstring1 bitstring2))
  (concatenate 'bitstring bitstring1 bitstring2))

(defun replicate-bitstring
    (bitstring times &aux (length (length bitstring)))
  "Repeat BITSTRING by TIMES."
  (declare (bitstring bitstring)
           ((integer 0 #.(isqrt most-positive-fixnum)) times length)
           (optimize (speed 3) (safety 1)))
  (let ((result (make-bitstring (* length times))))
    (dotimes (i times result)
      (setf (subseq result (* i length) (+ (* i length) length))
            bitstring))))


;;; END
