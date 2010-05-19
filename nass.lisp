(in-package :nass.general)
(defclass abstract-instruction ()
  ()
  (:documentation "Top level abstract instruction class."))

(defclass instruction-metaclass (standard-class)
  ())

(in-package :nass.arch.amd64)
(in-package :nass.elf)
(in-package :nass.goof)


(defun bits (value)
  (if (zerop value)
      "0000"
      (apply #'concatenate 'string
             (loop for i from (* 8 (floor (log value 256))) downto 0 by 8
                collect (format nil "~8,'0,' ,8:B " (ldb (byte 8 i) value))))))

;;; http://paste.lisp.org/display/98095
;;; Permission to use requested/received over irc
;;; Thanks to Zach Beane of Wigflip
(defun hex (integer &optional (size 4))
  (format nil "~&~v,'0X~%" size integer))

(defvar *ndisasm-test-file-path*
  "/tmp/nass-ndisam-test")

(defvar *nasm-test-file-path*
  "/tmp/nass-nasm-test.S")

(defvar *gas-test-file-path*
  "/tmp/nass-gas-test.S")

(defmacro ndisasm-binary ((s &optional (path *ndisasm-test-file-path*))
                          &body body)
  `(progn
     (nass.util:write-binary-file (,s ,path)
       ,@body)
     ,(if (find-package :trivial-shell)
           `(funcall #',(find-symbol "SHELL-COMMAND" :trivial-shell)
                     ,(format nil "ndisasm ~A" path))
           `(error "Requires trivial-shell!"))))

(defun nasm-string (asm-instructions &key (file *nasm-test-file-path*))
  (declare (string asm-instructions))
  (with-open-file (s file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (princ asm-instructions s))
  (trivial-shell:shell-command (format nil "nasm -fbin -l ~A.listing ~A -o ~A.out && cat ~A.listing" file file file file)))

(defun gas-string (asm-instructions &key (file *gas-test-file-path*))
  (declare (string asm-instructions))
  (with-open-file (s file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (princ asm-instructions s))
  (trivial-shell:shell-command (format nil "as -acln ~A -o ~A.out" file file)))
;;; END
