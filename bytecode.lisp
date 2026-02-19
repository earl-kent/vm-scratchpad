
(in-package vm-scratchpad)

;; (defun generate-constants (bytecodes)
;;   (loop for bytecode in *bytecodes* for i from 0
;; 	collect
;; 	`(defconstant ,bytecode ,i)))

(defmacro define-constants (bytecodes)
  `(progn
     ,@(loop for bytecode in bytecodes for i from 0
	collect
	`(defconstant ,bytecode ,i))))

(define-constants
    (;;  (1) constants
     cod-nil
     cod-push-nil
     cod-t
     cod-const
     ;;  (2) static variables
     cod-load
     cod-loadi
     cod-loadc
     cod-loadv
     cod-loadic
     cod-store
     cod-storei
     cod-storec
     cod-storev
     cod-storeic
     ;;  (3) dynamic variables
     cod-getvalue
     cod-setvalue
     cod-bind
     cod-unbind1
     cod-unbind
     cod-progv
     ;;  (4) stack operations
     cod-push
     cod-pop
     cod-skip
     cod-skipi
     cod-skipsp
     ;;  (5) execution flow und jumps
     cod-skip-ret
     cod-skip-retgf
     cod-jmp
     cod-jmpif
     cod-jmpifnot
     cod-jmpif1
     cod-jmpifnot1
     cod-jmpifatom
     cod-jmpifconsp
     cod-jmpifeq
     cod-jmpifnoteq
     cod-jmpifeqto
     cod-jmpifnoteqto
     cod-jmphash
     cod-jmphashv
     cod-jsr
     cod-jmptail
     ;;  (6) environments and closures
     cod-venv
     cod-make-vector1-push
     cod-copy-closure
     ;;  (7) function calls
     cod-call
     cod-call0
     cod-call1
     cod-call2
     cod-calls1
     cod-calls2
     cod-callsr
     cod-callc
     cod-callckey
     cod-funcall
     cod-apply
     ;;  (8) optional and keyword arguments
     cod-push-unbound
     cod-unlist
     cod-unliststar
     cod-jmpifboundp
     cod-boundp
     cod-unbound-nil
     ;;  (9) multiple values handling
     cod-values0
     cod-values1
     cod-stack-to-mv
     cod-mv-to-stack
     cod-nv-to-stack
     cod-mv-to-list
     cod-list-to-mv
     cod-mvcallp
     cod-mvcall
     ;;  (10) BLOCK
     cod-block-open
     cod-block-close
     cod-return-from
     cod-return-from-i
     ;;  (11) TAGBODY
     cod-tagbody-open
     cod-tagbody-close-nil
     cod-tagbody-close
     cod-go
     cod-go-i
     ;;  (12) CATCH und THROW
     cod-catch-open
     cod-catch-close
     cod-throw
     ;;  (13) UNWIND-PROTECT
     cod-uwp-open
     cod-uwp-normal-exit
     cod-uwp-close
     cod-uwp-cleanup
     ;;  (14) HANDLER-BIND
     cod-handler-open
     cod-handler-begin-push
     ;;  (15) some individual functions
     cod-not
     cod-eq
     cod-car
     cod-cdr
     cod-cons
     cod-symbol-function
     cod-svref
     cod-svset
     cod-list
     cod-liststar
     ;;  (16) combined operations
     cod-nil-push
     cod-t-push
     cod-const-push
     cod-load-push
     cod-loadi-push
     cod-loadc-push
     cod-loadv-push
     cod-pop-store
     cod-getvalue-push
     cod-jsr-push
     cod-copy-closure-push
     cod-call-push
     cod-call1-push
     cod-call2-push
     cod-calls1-push
     cod-calls2-push
     cod-callsr-push
     cod-callc-push
     cod-callckey-push
     cod-funcall-push
     cod-apply-push
     cod-car-push
     cod-cdr-push
     cod-cons-push
     cod-list-push
     cod-liststar-push
     cod-nil-store
     cod-t-store
     cod-load-storec
     cod-calls1-store
     cod-calls2-store
     cod-callsr-store
     cod-load-cdr-store
     cod-load-cons-store
     cod-load-inc-store
     cod-load-dec-store
     cod-load-car-store
     cod-call1-jmpif
     cod-call1-jmpifnot
     cod-call2-jmpif
     cod-call2-jmpifnot
     cod-calls1-jmpif
     cod-calls1-jmpifnot
     cod-calls2-jmpif
     cod-calls2-jmpifnot
     cod-callsr-jmpif
     cod-callsr-jmpifnot
     cod-load-jmpif
     cod-load-jmpifnot
     cod-load-car-push
     cod-load-cdr-push
     cod-load-inc-push
     cod-load-dec-push
     cod-const-symbol-function
     cod-const-symbol-function-push
     cod-const-symbol-function-store
     cod-apply-skip-ret
     cod-funcall-skip-retgf
     ;;  (17) shortcut codes
     cod-load0
     cod-load1
     cod-load2
     cod-load3
     cod-load4
     cod-load5
     cod-load6
     cod-load7
     cod-load8
     cod-load9
     cod-load10
     cod-load11
     cod-load12
     cod-load13
     cod-load14
     ;; extra loads
     cod-load15
     cod-load16
     cod-load17
     cod-load18
     cod-load19
     cod-load20
     cod-load21
     cod-load-push0
     cod-load-push1
     cod-load-push2
     cod-load-push3
     cod-load-push4
     cod-load-push5
     cod-load-push6
     cod-load-push7
     cod-load-push8
     cod-load-push9
     cod-load-push10
     cod-load-push11
     cod-load-push12
     cod-load-push13
     cod-load-push14
     cod-load-push15
     cod-load-push16
     cod-load-push17
     cod-load-push18
     cod-load-push19
     cod-load-push20
     cod-load-push21
     cod-load-push22
     cod-load-push23
     cod-load-push24
     cod-const0
     cod-const1
     cod-const2
     cod-const3
     cod-const4
     cod-const5
     cod-const6
     cod-const7
     cod-const8
     cod-const9
     cod-const10
     cod-const11
     cod-const12
     cod-const13
     cod-const14
     cod-const15
     cod-const16
     cod-const17
     cod-const18
     cod-const19
     cod-const20
     ;; extra consts
     cod-const21
     cod-const22
     cod-const23
     cod-const24
     cod-const-push0
     cod-const-push1
     cod-const-push2
     cod-const-push3
     cod-const-push4
     cod-const-push5
     cod-const-push6
     cod-const-push7
     cod-const-push8
     cod-const-push9
     cod-const-push10
     cod-const-push11
     cod-const-push12
     cod-const-push13
     cod-const-push14
     cod-const-push15
     cod-const-push16
     cod-const-push17
     cod-const-push18
     cod-const-push19
     cod-const-push20
     cod-const-push21
     cod-const-push22
     cod-const-push23
     cod-const-push24
     cod-const-push25
     cod-const-push26
     cod-const-push27
     cod-const-push28
     cod-const-push29
     ;; extra push
     cod-const-push30
     cod-const-push31
     cod-const-push32
     cod-store0
     cod-store1
     cod-store2
     cod-store3
     cod-store4
     cod-store5
     cod-store6
     cod-store7
     ;; extra store
     cod-store8
     cod-store9
     cod-store10
     cod-store11
     cod-store12
     cod-store13
     cod-store14
     cod-store15
     cod-store16
     cod-store17
     cod-store18
     cod-store19
     cod-store20
     cod-store21))
