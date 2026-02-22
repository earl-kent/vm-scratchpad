

(in-package vm-scratchpad)


(defvar *stack*) ;; lisp objects
(defvar *sp*)    ;; program pointer

(defconstant +default-mem-size+ 1024)
(defconstant +stack-start+ 128)
(defconstant +stack-size+ 256)

(defun initialize-vm ()
  (setf *memory*
	(make-array +default-mem-size+ :element-type '(unsigned-byte 8)))
  (setf *stack-pointer* 0))

(deftype gcv-object-t () '(unsigned-byte 32))
(deftype uintw () '(unsigned-byte 32))
(deftype uintb () '(unsigned-byte 8))

;; Code vector

(defstruct codevec
  (gc-self 0 :type gcv-object-t)
  (ccv-spdepth-1 0 :type uintw) ; maximal SP-depth, 1-part
  (ccv-spdepth-jmpbufsize 0 :type uintw) ; maximal SP-depth, jmpbufsize-part
  (ccv-numreq 0 :type uintw) ; number of required parameters
  (ccv-numopt 0 :type uintw) ; number of optional parameters
  (ccv-flags 0 :type uintb) ; Flags: Bit 0: &REST - parameter given?
                          ; Bit 1: full lambda list at the end of const vec
                          ; Bit 2: docstring at the end of const vec
                          ; Bit 3: generic function with call-inhibition?
                          ; Bit 4: generic function?
                          ; Bit 5: JITC code at the end of const vec
                          ; Bit 6: &ALLOW-OTHER-KEYS-Flag
                          ; Bit 7: keyword-parameter given?
  (ccv-signature 0 :type uintb) ; abbreviated argument type, for faster FUNCALL
  ;; If keyword-parameters are given
  (ccv-numkey 0 :type uintw) ; Number of keyword-parameters
  (ccv-keyconsts 0 :type uintw)) ; Offset in FUNC of the keywords


;; Stack primitives

;; For now we assume a stack that grows up.


(defun stack_ (index)
  "#define STACK_(n)  (STACK[(sintP)(n)])"
  (aref *memory* (+ +stack-start+ *stack-pointer* index)))

(defun (setf stack_) (new-value index)
  (setf (aref *memory* (+ +stack-start+ *stack-pointer* index))
	new-value))

;; We could cast STACK-VAR, but for now we just return it.
(defun stack-pointable (stack-var)
  stack-var)

(defun skip-stack-op (n)
  "#define skipSTACKop  +="
  (incf *stack-pointer* n))

(defun stack-op (n)
  "#define STACKop      +"
    (incf *stack-pointer* n))

(defun cmp-stack-op (arg)
  "#define cmpSTACKop   <"
  arg)

(defun stack-block_ (type n)
  "#define STACKblock_(type,n)  (((type*)STACK)[(sintP)(n)])"
  (list type n))

(defun push-stack (object)
  "#define pushSTACK(obj)  (STACK_(-1) = (obj), STACK skipSTACKop -1)"
  (prog1
      (setf (stack_ -1) object)
    (skip-stack-op -1)))

(defun pop-stack ()
  "#define popSTACK()  (STACK skipSTACKop 1, STACK_(-1))"
  (prog1
      (stack_ -1)
    (decf *stack-pointer*)))

(defun skip-stack (n)
  "#define skipSTACK(n)  (STACK skipSTACKop (sintP)(n))"
  (skip-stack-op n))

(defun check-stack ()
  "check_STACK -- tests the LISP-Stack for overflow"
  )

(defun get-space-on-stack (n)
  "get_space_on_STACK(n); tests, whether there are still D0.L Bytes free
on the LISP-Stack"
  (declare (ignore n))
  (error "get-space-on-stack not implemented"))


;; #define VALUES1(A)                               \
;;   do { value1 = (A); mv_count = 1; } while (0)

;; Various things to consider when interpreting byte code
;; - Triggering garbage collection.
;; - checking stack
;; - debugging bytecode (

;; Note that clisp coordinates the CL stack with the C stack using
;; setjmp and longjmp:
;;   - finish_entry_frame (setjmp)
;;   - unwind, unwind_upto, reset (longjmp)
;;
;; We avoid using stack control to make our VM logic compatable for
;; languanges that don't have programatic access to the call stack,
;; e.g. WASM(?), and to make the initial code more explicit.


;;; The instruction set
;;Instructions for constants
;; (NIL)        Load NIL into values                           value1 := NIL, mv-count := 1
;; (PUSH-NIL n) Push n NILs into the STACK                     n times do: *--STACK := NIL, invalidate values
;; (t)          Load t into values                             value1 := t, mv-count := 1
;; (CONST n)    Load the function's nth constant into values   value1 := consts[n], mv-count := 1

;; Instructions for lexical variables
;; (LOAD n)             Load a directly accessible local variable into values.                                           value1 := *(STACK+n), mv-count := 1
;; (LOADI k1 k2 n)      Load an indirectly accessible local variable into values.                                        k := k1 + jmpbufsize * k2, value1 := *(*(SP+k)+ n), mv-count := 1
;; (LOADC n m)          Load a closed-up variable, defined in the same function and directly accessible, into values.    value1 := SVREF(*(STACK+n),1+m), mv-count := 1
;; (LOADV k m)          Load a closed-up variable, defined in an outer function, into values.                            v := venv-const, m times do: v := SVREF(v,0), value1 := SVREF(v,m), mv-count := 1
;; (LOADIC k1 k2 n m)   Load a closed-up variable, defined in the same function and indirectly accessible, into values.  k := k1 + jmpbufsize * k2, value1 := SVREF(*(*(SP+k)+n),1+m), mv-count := 1
;; (STORE n)            Store values into a directly accessible local variable.                                          *(STACK+n) := value1, mv-count := 1
;; (STOREI k1 k2 n)     Store values into an indirectly accessible local variable.                                       k := k1 + jmpbufsize * k2, *(*(SP+k)+ n) := value1, mv-count := 1
;; (STOREC n m)         Store values into a closed-up variable, defined in the same function and directly accessible.    SVREF(*(STACK+n),1+m) := value1, mv-count := 1
;; (STOREV k m)         Store values into a closed-up variable, defined in an outer function.                            v := venv-const, m times do: v := SVREF(v,0), SVREF(v,m) := value1, mv-count := 1
;; (STOREIC k1 k2 n m)  Store values into a closed-up variable, defined in the same function and indirectly accessible.  k := k1 + jmpbufsize * k2, SVREF(*(*(SP+k)+n),1+m) := value1, mv-count := 1

;; Instructions for dynamic variables
;; (GETVALUE n)  Load a symbol's value into values.                     value1 := symbol-value(consts[n]), mv-count := 1
;; (SETVALUE n)  Store values into a symbol's value.                    symbol-value(consts[n]) := value1, mv-count := 1
;; (BIND n)      Bind a symbol dynamically.                             Bind the value of the symbol consts[n] to value1, implicitly STACK -= 3, invalidate values
;; (UNBIND1)     Dissolve one binding frame.                            Unbind the binding frame STACK is pointing to, implicitly STACK += 3
;; (UNBIND n)    Dissolve n binding frames.                             n times do: Unbind the binding frame STACK is pointing to, thereby incrementing STACK Thus, STACK += 1+2*n
;; (PROGV)       Bind a set of symbols dynamically to a set of values.  symbols := *STACK++, *--SP := STACK, build a single binding frame binding the symbols in symbols to the values in value1, invalidate values

;; Instructions for stack operations
;; (PUSH)
;; Push one object onto the STACK.
;; *--STACK := value1,
;;              invalidate values
;; (POP)
;; Pop one object from the STACK, into values.
;; value1 := *STACK++, mv-count := 1
;; (SKIP n)
;; Restore a previous STACK pointer.
;;              Remove n objects from the STACK.
;; STACK := STACK + n
;; (SKIPI k1 k2 n)
;; Restore a previous STACK pointer. Remove an unknown
;;              number of objects from the STACK.
;; k := k1 + jmpbufsize * k2,
;;              STACK := *(SP+k),
;;              SP := SP+k+1,
;;              STACK := STACK + n
;; (SKIPSP k1 k2)
;; Restore a previous SP pointer.
;; k := k1 + jmpbufsize * k2,
;;              SP := SP+k

;; Instructions for control flow, jumps
;; (SKIP&amp;RET n)
;; Clean up the STACK, and return from the function.
;; STACK := STACK+n,
;;             return from the function, returning values.
;; (SKIP&amp;RETGF n)
;; Clean up the STACK, and return from the generic
;;              function.
;; If bit 3 is set in the function's &flags;,
;;                 then STACK := STACK+n, mv-count := 1,
;;                      and return from the function.
;;              Otherwise: if the current function has no &rest-amp; argument,
;;                 then STACK := STACK+n-&numreq;,
;;                      apply value1 to the &numreq; arguments
;;                            still on the STACK, and
;;                            return from the function.
;;                 Else STACK := STACK+n-&numreq;-1,
;;                      apply value1 to the &numreq; arguments and the
;;                            &rest-amp; argument, all still on the STACK, and
;;                            return from the function.
;; (JMP &lab-r;)
;; Jump to &lab-r;.
;; PC := &lab-r;.
;; (JMPIF &lab-r;)
;; Jump to &lab-r;, if value1 is true.
;; If value1 is not NIL, PC := &lab-r;.
;; (JMPIFNOT &lab-r;)
;; Jump to &lab-r;, if value1 is false.
;; If value1 is NIL, PC := &lab-r;.
;; (JMPIF1 &lab-r;)
;; Jump to &lab-r; and forget secondary values,
;;              if value1 is true.
;; If value1 is not NIL,
;;                 mv-count := 1, PC := &lab-r;.
;; (JMPIFNOT1 &lab-r;)
;; Jump to &lab-r; and forget secondary values,
;;              if value1 is false.
;; If value1 is NIL,
;;                 mv-count := 1, PC := &lab-r;.
;; (JMPIFATOM &lab-r;)
;; Jump to &lab-r;, if value1 is not a cons.
;; If value1 is not a cons, PC := &lab-r;.
;;              invalidate values
;; (JMPIFCONSP &lab-r;)
;; Jump to &lab-r;, if value1 is a cons.
;; If value1 is a cons, PC := &lab-r;.
;;                 invalidate values
;; (JMPIFEQ &lab-r;)
;; Jump to &lab-r;, if value1 is &eq; to the top-of-stack.
;; If eq(value1,*STACK++), PC := &lab-r;.
;;              invalidate values
;; (JMPIFNOTEQ &lab-r;)
;; Jump to &lab-r;, if value1 is not &eq;
;;              to the top-of-stack.
;; If not eq(value1,*STACK++), PC := &lab-r;.
;;              invalidate values
;; (JMPIFEQTO n &lab-r;)
;; Jump to &lab-r;,
;;              if the top-of-stack is &eq; to a constant.
;; If eq(*STACK++,consts[n]), PC := &lab-r;.
;;              invalidate values
;; (JMPIFNOTEQTO n &lab-r;)
;; Jump to &lab-r;, if the top-of-stack is not &eq;
;;              to a constant.
;; If not eq(*STACK++,consts[n]), PC := &lab-r;.
;;              invalidate values
;; (JMPHASH n &lab-r;)
;; Table-driven jump, depending on value1.
;; Lookup value1 in the hash table consts[n].
;;              (The hash table's test is either &eq; or &eql;.)
;;              If found, the hash table value is a signed &fixnum-t;,
;;              jump to it: PC := PC + value.  Else jump to &lab-r;.
;;              invalidate values
;; (JMPHASHV n &lab-r;)
;; Table-driven jump, depending on value1,
;;              inside a generic function.
;; Lookup value1 in the hash table SVREF(consts[0],n).
;;              (The hash table's test is either &eq; or &eql;.)
;;              If found, the hash table value is a signed &fixnum-t;,
;;              jump to it: PC := PC + value.  Else jump to &lab-r;.
;;              invalidate values
;; (JSR &lab-r;)
;; Subroutine call.
;; *--STACK := function. Then start interpreting the
;;              bytecode at &lab-r;, with values undefined.
;;              When a (RET) is encountered,
;;              program execution is resumed at the instruction after
;;              (JSR &lab-r;).
;; (JMPTAIL m n
;;              &lab-r;)
;; Tail subroutine call.
;; n &geq; m.
;;              The STACK frame of size n is reduced to size m:
;;              {*(STACK+n-m), ..., *(STACK+n-1)} :=
;;                {*STACK, ..., *(STACK+m-1)}.
;;              STACK += n-m.
;;              *--STACK := function.
;;              Then jump to &lab-r;, with values undefined.

;; Instructions for &lex-env;,
;;    creation of closures
;; (VENV)
;; Load the venv-const into values.
;; value1 := consts[0], mv-count := 1.

;; (MAKE-VECTOR1&amp;PUSH n)
;; Create a &simple-vector-t; used for closed-up variables.
;; v := new &simple-vector-t; of size n+1.
;;              SVREF(v,0) := value1.
;;              *--STACK := v. invalidate values
;; (COPY-CLOSURE m n)
;; Create a closure by copying the prototype and filling in
;;              the &lex-env;.
;; &f-r; := copy-function(consts[m]).
;;              For &i-r;=0,..,n-1:
;;                  &f-r;_consts[i] := *(STACK+n-1-&i-r;).
;;              STACK += n.
;;              value1 := &f-r;, mv-count := 1

;; Instructions for function calls
;; (CALL k n)
;; Calls a constant function with k arguments.
;; The function consts[n] is called
;;              with the arguments *(STACK+k-1), ..., *(STACK+0).
;;              STACK += k. &ret-mvalues;
;; (CALL0 n)
;; Calls a constant function with 0 arguments.
;; The function consts[n] is called with 0 arguments.
;;              &ret-mvalues;
;; (CALL1 n)
;; Calls a constant function with 1 argument.
;; The function consts[n] is called with one argument *STACK.
;;              STACK += 1. &ret-mvalues;
;; (CALL2 n)
;; Calls a constant function with 2 arguments.
;; The function consts[n] is called
;;              with two arguments *(STACK+1) and *(STACK+0).
;;              STACK += 2. &ret-mvalues;
;; (CALLS1 &b-r;)
;; Calls a system function with no &rest-amp;.
;; Calls the system function &FUNTAB;[&b-r;].
;;              The right number of arguments is already on the STACK
;;              (including &unbound;s in place of absent &optional-amp; or
;;               &key-amp; parameters).
;;              &clear-stack; &ret-mvalues;
;; (CALLS2 &b-r;)
;; Calls a system function with no &rest-amp;.
;; Calls the system function &FUNTAB;[256+&b-r;].
;;              The right number of arguments is already on the STACK
;;              (including &unbound;s in place of absent &optional-amp; or
;;               &key-amp; parameters).
;;              &clear-stack; &ret-mvalues;
;; (CALLSR m &b-r;)
;; Calls a system function with &rest-amp;.
;; Calls the system function &FUNTAB;R[&b-r;].
;;              The minimum number of arguments is already on the STACK,
;;              and m additional arguments as well.
;;              &clear-stack; &ret-mvalues;
;; (CALLC)
;; Calls a computed compiled function with no &key-amp;s.
;; Calls the compiled function value1.
;;              The right number of arguments is already on the STACK
;;              (including &unbound;s in place of absent &optional-amp;
;;               parameters).
;;              &clear-stack; &ret-mvalues;
;; (CALLCKEY)
;; Calls a computed compiled function with &key-amp;s.
;; Calls the compiled function value1.
;;              The right number of arguments is already on the STACK
;;              (including &unbound;s in place of absent &optional-amp;
;;               or &key-amp; parameters).
;;              &clear-stack; &ret-mvalues;
;; (FUNCALL n)
;; Calls a computed function.
;; Calls the function *(STACK+n)
;;              with the arguments *(STACK+n-1), ..., *(STACK+0).
;;              STACK += n+1. &ret-mvalues;
;; (APPLY n)
;;   Calls a computed function with an unknown number of arguments.
;;   Calls the function *(STACK+n)
;;          with the arguments *(STACK+n-1), ..., *(STACK+0)
;;          and a list of additional arguments value1.
;;          STACK += n+1. &ret-mvalues;

;; Instructions for optional
;;   and keyword parameters
;; <informaltable id="instr-optkey-tab" frame="all">
;; (PUSH-UNBOUND n)
;; Push n &unbound;s into the STACK.
;; n times do: *--STACK := &unbound;.
;;              invalidate values
;; (UNLIST n m)
;; Destructure a proper &list-t;.
;; 0 &le; m &le; n.
;;              n times do: *--STACK := &car;(value1),
;;              value1 := &cdr;(value1).
;;              During the last m iterations, the list value1
;;              may already have reached its end;
;;              in this case, *--STACK := &unbound;.
;;              At the end, value1 must be NIL.
;;              invalidate values
;; (UNLIST* n m)
;; Destructure a proper or dotted &list-t;.
;; 0 &le; m &le; n, n &gt; 0.
;;              n times do: *--STACK := &car;(value1),
;;              value1 := &cdr;(value1).
;;              During the last m iterations, the list value1
;;              may already have reached its end;
;;              in this case, *--STACK := &unbound;.
;;              At the end, after n &cdr;s, *--STACK := value1.
;;              invalidate values
;; (JMPIFBOUNDP n &lab-r;)
;; Jump to &lab-r;, if a local variable is not unbound.
;; If *(STACK+n) is not &unbound;,
;;                 value1 := *(STACK+n), mv-count := 1, PC := &lab-r;.
;;              Else: invalidate values.
;; (BOUNDP n)
;; Load t or NIL into values, depending on whether a local
;;              variable is bound.
;; If *(STACK+n) is not &unbound;,
;;                 value1 := t, mv-count := 1.
;;              Else: value1 := NIL, mv-count := 1.
;; (UNBOUND->NIL n)
;; If a local variable is unbound, assign a default value
;;              NIL to it.
;; If *(STACK+n) is &unbound;,
;;              *(STACK+n) := NIL.

;; Instructions for &mul-val;
;; (VALUES0)
;; Load no values into values.
;; value1 := NIL, mv-count := 0

;; (VALUES1)
;; Forget secondary values.
;; mv-count := 1

;; (STACK-TO-MV n)
;; Pop the first n objects from STACK into values.
;; Load values(*(STACK+n-1),...,*(STACK+0)) into
;;              values. STACK += n.
;; (MV-TO-STACK)
;; Save values on STACK.
;; Push the mv-count values onto the STACK
;;              (in order: value1 comes first).
;;              STACK -= mv-count. invalidate values
;; (NV-TO-STACK n)
;; Save n values on STACK.
;; Push the first n values onto the STACK
;;              (in order: value1 comes first).
;;              STACK -= n. invalidate values
;; (MV-TO-LIST)
;; Convert &mul-val; into a list.
;; value1 := list of values, mv-count := 1
;; (LIST-TO-MV)
;; Convert a &list-t; into &mul-val;.
;; Call the function &values-list; with value1 as argument.
;;              &ret-mvalues;
;; (MVCALLP)
;; Start a &multiple-value-call; invocation.
;; *--SP := STACK. *--STACK := value1.
;; (MVCALL)
;; Finish a &multiple-value-call; invocation.
;; newSTACK := *SP++.
;;              Call the function *(newSTACK-1), passing it
;;              *(newSTACK-2), ..., *(STACK+0) as arguments.
;;              STACK := newSTACK. &ret-mvalues;

;; Instructions for
;;    &block; and &return-from;
;; (BLOCK-OPEN n &lab-r;)
;;   Create a &block; frame.
;;   Create a &block; frame, STACK -= 3, SP -= 2+jmpbufsize.
;;    The topmost (third) object in the block frame is
;;    &cons;(consts[n],frame-pointer) (its &bc-r;).
;;    Upon a &return-from; to this frame, execution will continue at &lab-r;.
;;    invalidate values.
;; (BLOCK-CLOSE)
;; Dissolve a &block; frame.
;; Dissolve the &block; frame at STACK, STACK += 3,
;;              SP += 2+jmpbufsize. Mark the &bc-r; as invalid.
;; (RETURN-FROM n)
;; Leave a &block; whose &bc-r; is given.
;; &bc-r; := consts[n].
;;              If &cdr;(&bc-r;) = &disabled;, an &err-sig;.
;;              Else &cdr;(&bc-r;) is a frame-pointer.
;;              Unwind the stack up to this frame, pass it values.
;; (RETURN-FROM-I k1 k2 n)
;; Leave a &block; whose &bc-r; is indirectly accessible.
;; k := k1 + jmpbufsize * k2,
;;              &bc-r; := *(*(SP+k)+n).
;;              If &cdr;(&bc-r;) = &disabled;, an &err-sig;.
;;              Else &cdr;(&bc-r;) is a frame-pointer.
;;              Unwind the stack up to this frame, pass it values.

;; Instructions for &tagbody; and &go;
;; (TAGBODY-OPEN m
;;              <replaceable>label&sub-1;</replaceable> ...
;;              <replaceable>label&sub-n;</replaceable>)
;; Create a &tagbody; frame.
;; Fetch consts[m], this is a &simple-vector-t; with
;;              n elements, then decode n label operands.
;;              Create a &tagbody; frame, STACK -= 3+n, SP -= 1+jmpbufsize.
;;              The third object in the &tagbody; frame is
;;              &cons;(consts[m],frame-pointer) (the &tbc-r;)
;;              Upon a &go; to tag &lab-r; of this frame, execution
;;              will continue at <replaceable>label&sub-l;</replaceable>.
;;              invalidate values
;; (TAGBODY-CLOSE-NIL)
;; Dissolve a &tagbody; frame, and load NIL into values.
;; Dissolve the &tagbody; frame at STACK,
;;              STACK += 3+m, SP += 1+jmpbufsize.
;;              Mark the &tbc-r; as invalid.
;;              value1 := NIL, mv-count := 1.
;; (TAGBODY-CLOSE)
;; Dissolve a &tagbody; frame.
;; Dissolve the &tagbody; frame at STACK,
;;              STACK += 3+m, SP += 1+jmpbufsize.
;;              Mark the &tbc-r; as invalid.
;; (GO n &lab-r;)
;; Jump into a &tagbody; whose &tbc-r; is given.
;; &tbc-r; := consts[n].
;;              If &cdr;(&tbc-r;) = &disabled;, an &err-sig;.
;;              Else &cdr;(&tbc-r;) is a frame-pointer. Unwind the stack up
;;              to this frame, pass it the number &lab-r;.
;; (GO-I k1 k2 n &lab-r;)
;; Jump into a &tagbody; whose &tbc-r; is indirectly
;;              accessible.
;; k := k1 + jmpbufsize * k2,
;;              &tbc-r; := *(*(SP+k)+n).
;;              If &cdr;(&tbc-r;) = &disabled;, an &err-sig;.
;;              Else &cdr;(&tbc-r;) is a frame-pointer. Unwind the stack up
;;              to this frame, pass it the number &lab-r;.

;; Instructions for &catch; and &throw;
;; (CATCH-OPEN &lab-r;)
;; Create a &catch; frame.
;; Create a &catch; frame, with value1 as tag.
;;              STACK -= 3, SP -= 2+jmpbufsize.
;;              Upon a &throw; to this tag execution continues at
;;              &lab-r;.
;; (CATCH-CLOSE)
;; Dissolve a &catch; frame.
;; Dissolve the &catch; frame at STACK.
;;              STACK += 3, SP += 2+jmpbufsize.
;; (THROW)
;; Non-local exit to a &catch; frame.
;; &tag-r; := *STACK++.
;;              Search the innermost &catch; frame with tag
;;              &tag-r; on the STACK, unwind the
;;              stack up to it, pass it values.

;; Instructions for &unwind-protect;
;; (UNWIND-PROTECT-OPEN
;;              &lab-r;)
;; Create an &unwind-protect; frame.
;; Create an &unwind-protect; frame.
;;              STACK -= 2, SP -= 2+jmpbufsize.
;;              When the stack will be unwound by a non-local exit,
;;              values will be saved on STACK, and execution will be
;;              transferred to &lab-r;.
;; (UNWIND-PROTECT-NORMAL-EXIT)
;; Dissolve an &unwind-protect; frame, and start the cleanup
;;              code.
;; Dissolve the &unwind-protect; frame at STACK.
;;              STACK += 2, SP += 2+jmpbufsize.
;;              *--SP := 0, *--SP := 0, *--SP := STACK.
;;              Save the values on the STACK,
;;              STACK -= mv-count.
;; (UNWIND-PROTECT-CLOSE)
;; Terminate the cleanup code.
;; newSTACK := *SP++. Load
;;              values(*(newSTACK-1), ..., *(STACK+0)) into values.
;;              STACK := newSTACK. SPword1 := *SP++, SPword2 := *SP++.
;;              Continue depending on SPword1 and SPword2.
;;              If both are 0, simply continue execution.
;;              If SPword2 is 0 but SPword1 is nonzero, interpret it as a
;;              label and jump to it.
;; (UNWIND-PROTECT-CLEANUP)
;; Dissolve an &unwind-protect; frame, and execute the cleanup
;;              code like a subroutine call.
;; Dissolve the &unwind-protect; frame at STACK,
;;              get &lab-r; out of the frame.
;;              STACK += 2, SP += 2+jmpbufsize.
;;              *--SP := 0, *--SP := PC, *--SP := STACK.
;;              Save the values on the STACK, STACK -= mv-count.
;;              PC := &lab-r;.

;; Instructions for &handler-bind;
;; (HANDLER-OPEN n)
;; Create a handler frame.
;; Create a handler frame, using consts[n] which
;;              contains the &condition-t; types, the corresponding labels and
;;              the current SP depth (= function entry SP - current SP).

;; (HANDLER-BEGIN&amp;PUSH)
;; Start a handler.
;; Restore the same SP state as after the HANDLER-OPEN.
;;              value1 := the &condition-t; that was passed to the handler,
;;              mv-count := 1.
;;              *--STACK := value1.

;; Instructions for some inlined
;;   functions
;; (NOT)
;; Inlined call to &not-f;.
;; value1 := not(value1), mv-count := 1.
;; (EQ)
;; Inlined call to &eq;.
;; value1 := eq(*STACK++,value1),
;;              mv-count := 1.
;; (CAR)
;; Inlined call to &car;.
;; value1 := &car;(value1), mv-count := 1.
;; (CDR)
;; Inlined call to &cdr;.
;; value1 := &cdr;(value1), mv-count := 1.
;; (CONS)
;; Inlined call to &cons;.
;; value1 := cons(*STACK++,value1),
;;              mv-count := 1.
;; (SYMBOL-FUNCTION)
;; Inlined call to &symbol-function;.
;; value1 := &symbol-function;(value1),
;;              mv-count := 1.
;; (SVREF)
;; Inlined call to SVREF.
;; value1 := SVREF(*STACK++,value1),
;;              mv-count := 1.
;; (SVSET)
;; Inlined call to <function>(&setf; SVREF</function>.
;; <replaceable>arg1</replaceable> := *(STACK+1),
;;              <replaceable>arg2</replaceable> := *(STACK+0), STACK += 2.
;;              SVREF(<replaceable>arg2</replaceable>,value1) :=
;;                   <replaceable>arg1</replaceable>.
;;              value1 := <replaceable>arg1</replaceable>,
;;              mv-count := 1.
;; (LIST n)
;; Inlined call to &list;.
;; value1 := &list;(*(STACK+n-1),...,*(STACK+0)),
;;              mv-count := 1, STACK += n.
;; (LIST* n)
;; Inlined call to &list-star;.
;; value1 := &list-star;(*(STACK+n-1),...,
;;                                      *(STACK+0),value1),
;;              mv-count := 1, STACK += n.

;; Combined instructions

;; <para>The most frequent short sequences of instructions have an
;; equivalent combined instruction.  They are only present for space and
;; speed optimization. The only exception is
;; FUNCALL&amp;SKIP&amp;RETGF, which is needed for
;; generic functions.</para>

;;  <tgroup cols="2" colsep="1" rowsep="1" align="center">
;;  <thead><row>mnemonicequivalent</thead>
;; (NIL&amp;PUSH)
;; (NIL) (PUSH)

;; (T&amp;PUSH)
;; (T) (PUSH)

;; (CONST&amp;PUSH n)
;; (CONST n) (PUSH)

;; (LOAD&amp;PUSH n)
;; (LOAD n) (PUSH)

;; (LOADI&amp;PUSH k1 k2 n)
;; (LOADI k1 k2 n) (PUSH)

;; (LOADC&amp;PUSH n m)
;; (LOADC n m) (PUSH)

;; (LOADV&amp;PUSH k m)
;; (LOADV k m) (PUSH)

;; (POP&amp;STORE n)
;; (POP) (STORE n)

;; (GETVALUE&amp;PUSH n)
;; (GETVALUE n) (PUSH)

;; (JSR&amp;PUSH &lab-r;)
;; (JSR &lab-r;) (PUSH)

;; (COPY-CLOSURE&amp;PUSH m n)
;; (COPY-CLOSURE m n) (PUSH)

;; (CALL&amp;PUSH k n)
;; (CALL k n) (PUSH)

;; (CALL1&amp;PUSH n)
;; (CALL1 n) (PUSH)

;; (CALL2&amp;PUSH n)
;; (CALL2 n) (PUSH)

;; (CALLS1&amp;PUSH &b-r;)
;; (CALLS1 &b-r;) (PUSH)

;; (CALLS2&amp;PUSH &b-r;)
;; (CALLS2 &b-r;) (PUSH)

;; (CALLSR&amp;PUSH m n)
;; (CALLSR m n) (PUSH)

;; (CALLC&amp;PUSH)
;; (CALLC) (PUSH)

;; (CALLCKEY&amp;PUSH)
;; (CALLCKEY) (PUSH)

;; (FUNCALL&amp;PUSH n)
;; (FUNCALL n) (PUSH)

;; (APPLY&amp;PUSH n)
;; (APPLY n) (PUSH)

;; (CAR&amp;PUSH)
;; (CAR) (PUSH)

;; (CDR&amp;PUSH)
;; (CDR) (PUSH)

;; (CONS&amp;PUSH)
;; (CONS) (PUSH)

;; (LIST&amp;PUSH n)
;; (LIST n) (PUSH)

;; (LIST*&amp;PUSH n)
;; (LIST* n) (PUSH)

;; (NIL&amp;STORE n)
;; (NIL) (STORE n)

;; (T&amp;STORE n)
;; (T) (STORE n)

;; (LOAD&amp;STOREC k n m)
;; (LOAD k) (STOREC n m)

;; (CALLS1&amp;STORE &b-r; k)
;; (CALLS1 &b-r;) (STORE k)

;; (CALLS2&amp;STORE &b-r; k)
;; (CALLS2 &b-r;) (STORE k)

;; (CALLSR&amp;STORE m n k)
;; (CALLSR m n) (STORE k)

;; (LOAD&amp;CDR&amp;STORE n)
;; (LOAD n) (CDR) (STORE n)

;; (LOAD&amp;CONS&amp;STORE n)
;; (LOAD n+1) (CONS) (STORE n)

;; (LOAD&amp;INC&amp;STORE n)
;; (LOAD n) (CALL1 #'1+) (STORE n)

;; (LOAD&amp;DEC&amp;STORE n)
;; (LOAD n) (CALL1 #'1-) (STORE n)

;; (LOAD&amp;CAR&amp;STORE m n)
;; (LOAD m) (CAR) (STORE n)

;; (CALL1&amp;JMPIF n &lab-r;)
;; (CALL1 n) (JMPIF &lab-r;)

;; (CALL1&amp;JMPIFNOT n &lab-r;)
;; (CALL1 n) (JMPIFNOT &lab-r;)

;; (CALL2&amp;JMPIF n &lab-r;)
;; (CALL2 n) (JMPIF &lab-r;)

;; (CALL2&amp;JMPIFNOT n &lab-r;)
;; (CALL2 n) (JMPIFNOT &lab-r;)

;; (CALLS1&amp;JMPIF &b-r; &lab-r;)
;; (CALLS1 &b-r;) (JMPIF &lab-r;)

;; (CALLS1&amp;JMPIFNOT &b-r; &lab-r;)
;; (CALLS1 &b-r;) (JMPIFNOT &lab-r;)

;; (CALLS2&amp;JMPIF &b-r; &lab-r;)
;; (CALLS2 &b-r;) (JMPIF &lab-r;)

;; (CALLS2&amp;JMPIFNOT &b-r; &lab-r;)
;; (CALLS2 &b-r;) (JMPIFNOT &lab-r;)

;; (CALLSR&amp;JMPIF m n &lab-r;)
;; (CALLSR m n) (JMPIF &lab-r;)

;; (CALLSR&amp;JMPIFNOT m n &lab-r;)
;; (CALLSR m n) (JMPIFNOT &lab-r;)

;; (LOAD&amp;JMPIF n &lab-r;)
;; (LOAD n) (JMPIF &lab-r;)

;; (LOAD&amp;JMPIFNOT n &lab-r;)
;; (LOAD n) (JMPIFNOT &lab-r;)

;; (LOAD&amp;CAR&amp;PUSH n)
;; (LOAD n) (CAR) (PUSH)

;; (LOAD&amp;CDR&amp;PUSH n)
;; (LOAD n) (CDR) (PUSH)

;; (LOAD&amp;INC&amp;PUSH n)
;; (LOAD n) (CALL1 #'1+) (PUSH)

;; (LOAD&amp;DEC&amp;PUSH n)
;; (LOAD n) (CALL1 #'1-) (PUSH)

;; (CONST&amp;SYMBOL-FUNCTION n)
;; (CONST n) (SYMBOL-FUNCTION)

;; (CONST&amp;SYMBOL-FUNCTION&amp;PUSH n)
;; (CONST n) (SYMBOL-FUNCTION) (PUSH)

;; (CONST&amp;SYMBOL-FUNCTION&amp;STORE n k)
;; (CONST n) (SYMBOL-FUNCTION) (STORE k)

;; (APPLY&amp;SKIP&amp;RET n k)
;; (APPLY n) (SKIP&amp;RET k)

;; (FUNCALL&amp;SKIP&amp;RETGF n k)
;; (FUNCALL n) (SKIP&amp;RETGF k)
;; Shortcut instructions
;; <para>There are special one-byte instructions (without explicit
;; operands) for the following frequent instructions:</para>
;;  <tgroup cols="2" colsep="1" rowsep="1" align="center">
;;  <thead><row>mnemonicoperand range</thead>
;; (LOAD n)
;; 0 &le; n &lt; 15
;; (LOAD&amp;PUSH n)
;; 0 &le; n &lt; 25
;; (CONST n)
;; 0 &le; n &lt; 21
;; (CONST&amp;PUSH n)
;; 0 &le; n &lt; 30
;; (STORE n)
;; 0 &le; n &lt; 8

(defun test-interpret-bytecode ()
  (let ((test1 '((0 NIL) (1 SYSTEM::SKIP&RET 1))) ;; (lambda () (declare (compile)) nil)
	(codevec #(0 0 0 0 0 0 0 0 32 1 0 25 1)))
    (interpret-bytecode nil codevec)))


(defun interpret-bytecode (closureptr code)
  "local /*maygc*/ Values interpret_bytecode_ (object closure_in, Sbvector codeptr,
                                               const uintB* byteptr_in)"
  ;; Situate closure in STACK, below the arguments. But why note used closureptr
  (push-stack closure)
  ;; If there is no fast sp-access, one has to introduce an extra
  ;; pointer:
  (let* ((codevec (codevec-ize codeptr))
	 (private-sp-length
	   (+ (ccv-spdepth-1 codevec)
	      (* jmpbufsize (ccv-spdepth-jmpbufsize codevec))))
	 ;; points to one more than the current current number of
	 ;; items in the stack. Stack grows up.
	 (private-sp 0)
	 (private-sp-space (make-array private-sp-length
				       :element-type 'integer)))
    (labels ((sp_ (n)
	       (aref private-sp-space n))
	     ;; (_sp_ (n)
	     ;;   (address-of private-sp n))
	     (skipsp (n)
	       (incf private-sp n))
	     (pushsp (item)
	       (prog1
		   (setf (aref private-sp-space private-sp) item)
		 (incf private-sp)))
	     (popsp ()
	       (aref private-sp-space (decf private-sp)))
	     ;; Note, stack grows upward
	     (jmpbuf-on-sp (name)
	       (sp-jmp-buf name)
	       (let* ((sp (sp))
		      (name (+ spoffset 1))
		      (sp jmpbufsize))
		 (setsp sp)))
	     (free-jmpbuf-on-sp ()
	       (skipsp jmpbufsize))
	     (finish-entry-frame-1 (frametype returner reentry-statement)
	       (finish-entry-frame frametype returner nil reentry_statement))
	     ;; Operand-Fetch:
	     ;; next Byte:
	     ;;   Bit 7 = 0 --> Bits 6..0 are the Operand (7 Bits).
	     ;;   Bit 7 = 1 --> Bits 6..0 and next Byte form the
	     ;;                 Operand (15 Bits).
	     ;;                 For jump-distances: Should this be =0, the next
	     ;;                 4 Bytes form the Operand
	     ;;                 (32 Bits).
	     ;; Macro B_operand(where);
	     ;; moves the next Operand (a Byte as Unsigned Integer)
	     ;; to (uintL)where and advances  bytecodepointer. */
	     (b-operand ()
               (prog1 (aref codevec byteptr)
		 (incf byteptr)))
	     ;; Macro S_operand(where);
	     ;; moves the next Operand (a Signed Integer)
	     ;; to (uintL)where and advances the bytecodepointer.
	     (s_operand (where)
               (let ((where (prog1 (aref codevec byteptr)
			      (incf byteptr))))
		 (when (ldb (byte 3 0) where))))
	     ;; Macro S_operand_ignore();
	     ;; skips the next Operand (a Signed Integer)
	     ;; and advances the bytecodepointer. */
	     (s_operand_ignore ()
               (let ((where (prog1 (aref codevec byteptr)
			      (incf byteptr))))
		 (when (ldb (byte 3 0) where))))
	     ;; store context-information:
	     ;; If sth. is called, that can trigger a GC, this must be framed within
	     ;; with_saved_context( ... ) . */
	     (with-saved-context (statement)
	       (let ((index (- byteptr codeptr)))
		 ;; really?
		 statement
		 ;; fetch Closure from Stack
		 (setf closure  (fetch-from-stack closureptr))
		 (setf codeptr (the-sbvector (clos-codevec (the-c-closure closure))))
		 (setf byteptr (+ codeptr  index)))))
      ;; Also l-operand
      (tagbody
	 ;; targets which must be handled: next-byte
	 ;;  - block_return (special target used in cod_block_open)
	 ;;  - catch_return (special target used in cod_catch_open)
	 ;;  - code_nil (special 'switch/case' target)
	 ;;  - code_t (special 'switch/case' target)
	 ;;  - error_STACK_putt
	 ;;  - error_byteptr
	 ;;  - error_toomany_values
	 ;;  - finished
	 ;;  - jmp (special 'switch/case' target)
	 ;;  - jmp0 (target that flows through to CASE cod_jmp: jmp)
	 ;;  - next_byte
	 ;;  - notjmp (target that flows through to CASE cod_jmpifnot:)
	 ;;  - nv_to_stack_end (special target used in cod_nv_to_stack)
	 ;;  - nv_to_stack_fill (special target used in cod_nv_to_stack)
	 ;;  - store (special 'switch/case' target)
	 ;;  - svref_not_a_svector (special target used in cod_svref & cod_svset)
	 ;;  - svref_not_an_index (special target used in cod_svref & cod_svset)
	 ;;  - tagbody_go (special target used in cod_tagbody_open)
	 ;;  - throw_save (special target used in cod_uwp_open)
	 ;;  - unlist_unbound (special target used in cod_unlist)
	 ;;  - unliststar_unbound (special target used in cod_unliststar)
       next-byte
	 (etypecase (byte-code-read-next)
	   ;; ------------------- (1) Constants -----------------------
	   ((cod-nil code-nil) ;(NIL)
	    (values1 vm-nil)
	    (go next-byte))
	   (cod_nil_push ; (NIL & PUSH)
	    (push-stack vm-nil)
	    (go next-byte))
	   (cod-push-nil ; (PUSH-NIL n)
	    (dotimesc (u-operandn) (u-operandn))
	    (pushstack vm-nil)
	    (go next-byte))
	   ((cod-t code-t) ; (t)
	    (values1 vm-t)
	    (go next-byte))
	   (cod-t-push ; (t&push)
	    (push-stack vm-t)
	    (go next-byte))
	   (cod-const ;(const n)
	    (values1 (aref (clos-consts (the-cclosure closure)) (u-operand)))
	    (go next-byte))
	   (cod-const-push ; (const&push n)
	    (pushstack (aref (clos-consts (the-cclosure closure)) (u-operand)))
	    (go next-byte)))
       finished))))


;; var JMPBUF_on_SP(name);  allocates a sp_jmp_buf in SP.
;; FREE_JMPBUF_on_SP();  deallocates it.
;; finish_entry_frame_1(frametype,returner,reentry_statement);  is like
;; finish_entry_frame(frametype,returner,,reentry_statement);  but
;; also private_SP is saved. */
