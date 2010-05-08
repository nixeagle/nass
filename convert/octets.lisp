(in-package :nass.convert)
(defun integer->bit-base-list
    (integer bits &key size (endian :little-endian)
     &aux (size-given-p (not (not size)))
     (size (if size
               (1- size)
               (floor (log (abs (if (zerop integer)
                                    1   ;Prevent divide by 0
                                    integer))
                           (expt 2 bits))))))
  "Convert INTEGER to base that is 2^BITS.

The result list will be SIZE long."
  (declare (integer integer)
           (positive-fixnum bits)
           ((or null non-negative-fixnum) size)
           (nass-type:endian endian))

  ; negative numbers take an extra element in the list for the sign part
  (let ((size (if (and (not size-given-p) (< integer 0))
                  (1+ size)
                  size)))
    (if (eq :little-endian endian)
        (loop for i from 0 below (* bits (1+ size)) by bits
           collect (ldb (byte bits i) integer))
        (loop for i from (* bits size) downto 0 by bits
           collect (ldb (byte bits i) integer)))))



(define-convert (integer list bit &key size)
  (integer->bit-base-list integer 1 :endian :big-endian :size size))

(define-convert (integer simple-bit-vector &key size)
  (let ((res (conv integer list bit :endian :big-endian :size size)))
    (make-array (length res) :element-type 'bit
                :initial-contents res)))

(define-convert (integer list nass-type:octal-digit &key
                         (endian :little-endian)
                         size)
  (integer->bit-base-list integer 3 :endian endian :size size))

(define-convert (integer list nass-type:hexadecimal-digit &key
                         (endian :little-endian)
                         size)
  (integer->bit-base-list integer 4 :endian endian :size size))

(define-convert (integer list nass-type:signed-octet
                         &key (endian :little-endian)
                         size)
  (integer->bit-base-list integer 8 :endian endian :size size))

(define-convert (integer list nass-type:signed-word &key
                         (endian :little-endian)
                         size)
  (integer->bit-base-list integer 16 :endian endian :size size))


