(in-package :nass.general)
(defclass abstract-instruction ()
  ()
  (:documentation "Top level abstract instruction class."))

(defclass instruction-metaclass (standard-class)
  ())

(in-package :nass.arch.amd64)



(in-package :nass.elf)


;(flexi-streams:string-to-octets "ELF" :external-format :utf8)



(in-package :nixeagle.helpers.binary-streams)

(defun memory-input-stream (simple-vector)
  "Make a `flexi-streams:in-memory-stream' using SIMPLE-VECTOR.

There are no options for transformers or using part of the simple vector
for simplicity."
  (declare (simple-vector simple-vector))
  (make-in-memory-input-stream simple-vector))

(defun memory-output-stream ()
  "Shorter name for making an output stream with no transformers."
  (make-in-memory-output-stream))

(defmacro with-output-to-memory ((symbol &key (element-type 'nass-type:octet))
                                 &body body)
  "Output to a `flexi-streams:in-memory-stream'."
  `(with-open-stream (,symbol (flexi-streams:make-in-memory-output-stream
                               :element-type ',element-type))
     ,@body))

(defun call-with-output-to-octet-array (thunk in-memory-stream)
  "Pass call THUNK with IN-MEMORY-STREAM returning stream's octed array."
  (funcall thunk in-memory-stream)
  (flexi-streams:get-output-stream-sequence in-memory-stream))

(defmacro with-output-to-octet-array (symbol &body body)
  "Output to memory stream identified by SYMBOL, returning octet array."
  `(with-output-to-memory (,symbol)
     (call-with-output-to-octet-array (lambda (,symbol)
                                        ,@body)
                                      ,symbol)))


(in-package :nass.goof)


;;; http://paste.lisp.org/display/98095
;;; Permission to use requested/received over irc
;;; Thanks to Zach Beane of Wigflip
(defun bits (integer &optional (size 8))
  (format t "~&~v,'0B~%" size integer))

;;; Thanks to Zach Beane of Wigflip
(defun hex (integer &optional (size 4))
  (format t "~&~v,'0X~%" size integer))

;;; END
