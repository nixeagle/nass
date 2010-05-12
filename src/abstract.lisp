;;;
;;; Primary defined abstractions general to all instruction sets

(in-package :nass.abstract)

(define-binary-class opcode-mixin () ()
  (:documentation "Mixin for all opcode related classes.

Anything to be used as a _primary_ opcode for a machine instruction should
make sure this class is in the superclass list."))

;;; This being an integer is an arbitrary specification Lets try to adhere
;;; to it unless overriding reasons for the _primary_ opcode cannot be
;;; expressed as a single integer.
(defgeneric opcode (assembly-object &key &allow-other-keys)
  (:documentation "Gets ASSEMBLY-OBJECT's opcode as an integer.")
  (:method ((object opcode-mixin) &key)
    (error "Generic function OPCODE needs to be defined for ~A."
           object)))

(define-binary-class operand-mixin () ()
  (:documentation "Mixin for assembly operands/arguments.

Operand in assembly speak means the arguments to an assembly
instruction. For example:
   mov     ax, 25

Operands there are ax and 25."))

(defgeneric operands (assembly-object)
  (:documentation "Gets ASSEMBLY-OBJECT's applicable operands.

These should be returned in an untranslated form, best if exactly how the
operands were created.")
  (:method ((object operand-mixin))
    (error "Generic function OPERANDS needs to be defined for ~A."
           object)))

(define-binary-class addressing-mixin () ()
  (:documentation "Mixin for assembly addressing classes.

Generally anything that acts as an addressing byte, bit, portion, whatever
shoul superclass this."))

(defmacro define-assembly-class
    ((long-name &rest aliases) superclasses slots &rest options)
  `(progn
     (define-binary-class ,long-name ,superclasses ,slots ,@options)
     ,@(mapcar (lambda (aliased-class)
                 `(define-binary-class ,aliased-class  (,long-name) ()
                                       ,@options))
               aliases)))
;;; END