(in-package :nass.general)
(defclass abstract-instruction ()
  ()
  (:documentation "Top level abstract instruction class."))

(defclass instruction-metaclass (standard-class)
  ())

(in-package :nass.arch.amd64)
(in-package :nass.elf)
(in-package :nass.goof)


;;; http://paste.lisp.org/display/98095
;;; Permission to use requested/received over irc
;;; Thanks to Zach Beane of Wigflip
(defun bits (integer &optional (size 8))
  (format nil "~&~v,'0B~%" size integer))

;;; Thanks to Zach Beane of Wigflip
(defun hex (integer &optional (size 4))
  (format nil "~&~v,'0X~%" size integer))

(defvar *ndisasm-test-file-path*
  "/tmp/nass-ndisam-test")

(defvar *nasm-test-file-path*
  "/tmp/nass-nasm-test.S")

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
;;; END
