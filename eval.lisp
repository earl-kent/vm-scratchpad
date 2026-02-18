

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
  (incf *stack-point* n))

(defun stack-op (n)
  "#define STACKop      +"
    (incf *stack-pointer* n))

(defun cmp-stack-op (arg)
  "#define cmpSTACKop   <"
  arg)

(defun stack-block_ (type n)
  "#define STACKblock_(type,n)  (((type*)STACK)[(sintP)(n)])"
  arg)

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

(defun get_space_on_stack (n)
  "get_space_on_STACK(n); tests, whether there are still D0.L Bytes free
on the LISP-Stack"
  (error "get_space_on_stack not implemented"))



(defun interpret-bytecode (closure-in codeptr byteptr-in)
  "local /*maygc*/ Values interpret_bytecode_ (object closure_in, Sbvector codeptr,
                                               const uintB* byteptr_in)"
