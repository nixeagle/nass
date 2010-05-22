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

(declaim (inline bitstring)
         (ftype (function (&rest bit) bitstring) bitstring))
(defun bitstring (&rest bits)
  "Make a `simple-bit-array' of BITS."
  (declare (dynamic-extent bits)
           (optimize (speed 3) (debug 1) (safety 0)))
  ;; With sbcl we can rely on make-array to catch invalid inputs, we
  ;; really can't verify valid inputs with safety 1 anyway as any length
  ;; is a valid length, and there is no way to do a cons type check with
  ;; dynamic-extent in effect.
  (make-array (the (integer 0 #.(- most-positive-fixnum 63))
                (length bits)) :element-type 'bit :initial-contents bits))

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


(declaim (inline zeros ones))
(defun zeros (n)
  "Make a `bitstring' of 0s."
  (declare (positive-fixnum n)
           (optimize (speed 3) (safety 1) (debug 1)))
  (make-bitstring n))

(defun ones (n)
  "Make a `bitstring' of 1s."
  (declare ((integer 1 #.(- most-positive-fixnum 63)) n)
           (optimize (speed 3) (safety 1) (debug 1)))
  (make-array n :element-type 'bit :initial-element 1))


(defun extract-bitstring (x &rest integers))

(defgeneric %extract-bitstring (x integers)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (:documentation "Extract bits from X.

concat results if more then one INTEGERS are supplied."))

(defmethod %extract-bitstring ((bitstring simple-bit-vector) (position integer))
  (assert (< position (length bitstring)))
  (bitstring (sbit bitstring (- (length bitstring) position 1))))

(defmethod %extract-bitstring ((integer integer) (position integer))
  (bitstring (ldb (byte 1 position) integer)))

;;; END
