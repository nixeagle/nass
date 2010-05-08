(in-package :nass.util)

(defun call-with-hex (thunk)
  "Bind `*printbase*' and `*print-radix*'.

Base is obviously 16 here."
  (let ((*print-base* 16)
        (*read-base* 16)
        (*print-radix* t))
    (funcall thunk)))

(defmacro with-hex (&body body)
  "Bind printer settings to sensible values for working with hex."
  `(call-with-hex (lambda () ,@body)))

(defmacro write-binary-file ((stream filespec) &body body)
  "Output to a binary file stream that clobbers pre-existing items."
  `(with-open-file (,stream ,filespec
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type 'nass-type:octet)
     ,@body))

(defmacro tbd (&body define-form)
  "State a DEFINE-FORM as needing to be done.

Place a string describing what needs to be done before the define form if
desired."
  (let ((stuff-to-do (if (stringp (car define-form))
                         (format nil "~%~%--------------------~%~A" (pop define-form))
                         "")))
    (with-gensyms (funcallable-name)
      `(let ((,funcallable-name ,(appendf (car define-form)
                                          (list '(error "Not yet implemented!")))))
         (setf (documentation ,funcallable-name 'function)
               (format nil "TBD: ~A~A" (or (documentation ,funcallable-name 'function) "")
                       ,stuff-to-do))))))