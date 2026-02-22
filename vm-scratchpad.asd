





(asdf:defsystem vm-scratchpad
  :depends-on (uiop fiveam)
  :components
  ((:file package)
   (:file bytecode)
   (:file lisp-bibl)
   (:file eval)
   (:file test)
   (:file compiler-94-package)
   (:file compiler-94)
   ;; (:file loadit)
   ;; (:file clever)
   ;; (:file pseudo)
   ))
