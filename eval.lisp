

(in-package vm-scratchpad)


(defvar *memory*)
(defvar *stack-pointer*) ;; Point one past the last value.

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
