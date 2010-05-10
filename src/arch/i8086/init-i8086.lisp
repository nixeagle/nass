(in-package :nass.arch.i8086)

(macrolet ((define-type (name full-name &rest members)
             (let ((documentation
                    (format nil "Mnemonic ~A stands for ~(~A~)." name
                            (substitute #\Space #\- (symbol-name full-name)))))
               `(progn
                  (deftype ,full-name ()
                    ,documentation
                    '(member ,@members))
                  (deftype ,name ()
                    ,documentation
                    ',full-name)))))
  (define-type es extra-segment 0)
  (define-type cs code-segment 1)
  (define-type ds data-segment 2)
  (define-type ss stack-segment 3))

;;; END



