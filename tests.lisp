(in-package :convert)

(test (integer->bit-base-list/binary :suite :nass)
  (is (equal '(1) (integer->bit-base-list 1 1)))
  (is (equal '(0) (integer->bit-base-list 0 1)))
  (is (equal '(1 0) (integer->bit-base-list 2 1)))
  (is (equal '(1 1) (integer->bit-base-list 3 1)))
  ;; Negative numbers
  (is (equal '(1 1) (integer->bit-base-list -1 1))))


(test (integer->bit-base-list/quad :suite :nass)
  (is (equal '(1) (integer->bit-base-list 1 2)))
  (is (equal '(0) (integer->bit-base-list 0 2)))
  (is (equal '(3) (integer->bit-base-list 3 2)))
  (is (equal '(1 0) (integer->bit-base-list 4 2)))
  (is (equal '(1 1) (integer->bit-base-list 5 2)))
  ;; negative numbers
  (is (equal '(1 1) (integer->bit-base-list -1 2))))


(test (integer->bit-base-list/octal :suite :nass)
  (is (equal '(0) (integer->bit-base-list 0 3)))
  (is (equal '(7) (integer->bit-base-list 7 3)))
  (is (equal '(1 0) (integer->bit-base-list 8 3)))
  (is (equal '(1 1) (integer->bit-base-list 9 3)))
  ;; negative number 1 (-1)
  (is (equal '(1 1) (integer->bit-base-list -1 3))))


(test (integer->bit-base-list/hexadecimal :suite :nass)
  (is (equal '(0) (integer->bit-base-list 0 4)))
  (is (equal '(15) (integer->bit-base-list 15 4)))
  (is (equal '(1 0) (integer->bit-base-list 16 4)))
  (is (equal '(1 1) (integer->bit-base-list 17 4)))
  ;; negative number 1 (-1)
  (is (equal '(1 1) (integer->bit-base-list -1 4))))