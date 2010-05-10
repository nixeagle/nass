(in-package :nass.arch.x86oids)

(define-binary-class x86oid (little-endian) ())

(macrolet ((define-type (name full-name &rest members)
             (declare (symbol name full-name))
             (let ((documentation
                    (format nil "Mnemonic ~A stands for ~(~A~).~
                                ~%WIDTH must be 1." name
                                (substitute #\Space #\- (symbol-name full-name)))))
               `(progn
                  (deftype ,full-name (&key (width 1))
                    ,documentation
                    (when (= width 1)
                      '(member ,@members)))
                  (deftype ,name (&key (width 1))
                    ,documentation
                    (when (= width 1)
                      ',full-name))))))
  (define-type ax accumulator-register 0)
  (define-type bx base-address-register 3)
  (define-type cx count-register 1)
  (define-type dx data-register 2)
  (define-type sp stack-pointer 4)
  (define-type bp base-pointer 5)
  (define-type si string-index 6)
  (define-type di data-index 7))

(define-binary-class mod-reg-r/m (x86oid)
  ((mod :bits 2 :initarg :mod :type (mod 4))
   (r/m :bits 3 :initarg :r/m :type (mod 8))
   (reg/opcode :bits 3 :initarg :reg/opcode :type (mod 8))))

;;; END