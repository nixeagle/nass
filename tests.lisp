(in-package :convert)

(test (integer->bit-base-list/binary :suite :nass)
  (is (equal '(1) (integer->bit-base-list 1 1)))
  (is (equal '(0) (integer->bit-base-list 0 1)))
  (is (equal '(0 1) (integer->bit-base-list 2 1)))
  (is (equal '(1 1) (integer->bit-base-list 3 1)))
  ;; Negative numbers
  (is (equal '(1 1) (integer->bit-base-list -1 1))))


(test (integer->bit-base-list/quad :suite :nass)
  (is (equal '(1) (integer->bit-base-list 1 2)))
  (is (equal '(0) (integer->bit-base-list 0 2)))
  (is (equal '(3) (integer->bit-base-list 3 2)))
  (is (equal '(0 1) (integer->bit-base-list 4 2)))
  (is (equal '(1 1) (integer->bit-base-list 5 2)))
  ;; negative numbers
  (is (equal '(1 1) (integer->bit-base-list -1 2))))


(test (integer->bit-base-list/octal :suite :nass)
  (is (equal '(0) (integer->bit-base-list 0 3)))
  (is (equal '(7) (integer->bit-base-list 7 3)))
  (is (equal '(0 1) (integer->bit-base-list 8 3)))
  (is (equal '(1 1) (integer->bit-base-list 9 3)))
  ;; negative number 1 (-1)
  (is (equal '(1 1) (integer->bit-base-list -1 3))))


(test (integer->bit-base-list/hexadecimal :suite :nass)
  (is (equal '(0) (integer->bit-base-list 0 4)))
  (is (equal '(15) (integer->bit-base-list 15 4)))
  (is (equal '(0 1) (integer->bit-base-list 16 4)))
  (is (equal '(1 1) (integer->bit-base-list 17 4)))
  ;; negative number 1 (-1)
  (is (equal '(1 1) (integer->bit-base-list -1 4))))

(test (integer->bit-base-list/octet :suite :nass)
  (is (equal '(#x0A #x0B #x0C #x0D)
             (integer->bit-base-list #x0A0B0C0D 8 :endian :big-endian)))
  (is (equal '(#x0D #x0C #x0B #x0A)
             (integer->bit-base-list #x0A0B0C0D 8 :endian :little-endian))))

(test (integer->bit-base-list/word :suite :nass)
  (is (equal '(#x0A0B #x0C0D)
             (integer->bit-base-list #x0A0B0C0D 16 :endian :big-endian)))
  (is (equal '(#x0C0D #x0A0B)
             (integer->bit-base-list #x0A0B0C0D 16 :endian :little-endian))))

(test (integer->bit-base-list/positive-endians :suite :nass)
  "Make sure endians are treated right.

Big endian means the way you read the hex number, such as #xf0 is the way
it looks in the resulting list: (15 0). When it is little endian, this
order is reversed such that #xf0 is represented as (0 15)."
  (is (equal '(0 1)
             (convert::INTEGER->BIT-BASE-LIST #x10 4 :endian :little-endian)))
  (is (equal '(1 0)
             (convert::INTEGER->BIT-BASE-LIST #x10 4 :endian :big-endian))))
;;; END
