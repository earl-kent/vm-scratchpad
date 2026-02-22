

(defpackage cross
  (:use :common-lisp :fiveam)
  (:nicknames clisp)
  (:shadow macroexpand-1 macroexpand compile-file *date-format*)
  (:export compile-file))
