
(in-package cross)

;; (sys::assemble-LAP (mapcar #'rest *))
;; (sys::disassemble-LAP byte-list const-list)


;; Tests -- Note, these only work in clisp.


(defvar *current-function-lambda-expression*)
;; Note the difference between codevec and lap-assembly. The codevec
;; is actually the encoding of the full closure object.
(defvar *current-codevec*)
(defvar *current-lap-assembly*)
(defvar *current-disassembly*)



(defun str (&rest rest)
  (apply #'concatenate 'string rest))

;; make output of sys::disassemble-LAP compatable with assemble-LAP
(defun translate-system-lap (system-lap)
  (mapcar (lambda (instruction)
	    (cons (intern (symbol-name (car instruction))) (cdr instruction)))
	  (mapcar #'rest system-lap)))


;; (intern (symbol-name symbol))

(defun generate-artifacts (expression)
  ;; closure-p is null for expressions defined in the top level
  ;; environment.
  (multiple-value-bind (lambda-expression closure-p name)
      (function-lambda-expression expression)
    (setf *current-function-lambda-expression*
	  (list lambda-expression closure-p name))
    (setf *current-codevec* (sys::closure-codevec expression))
    (multiple-value-bind (req-num opt-num rest-p key-p keyword-list
			  allow-other-keys-p byte-list const-list)
	(sys::signature expression)
      (declare (ignore req-num opt-num rest-p key-p keyword-list
		       allow-other-keys-p))
      (setf *current-disassembly* (translate-system-lap
				   (sys::disassemble-LAP byte-list const-list)))
      (setf *current-lap-assembly*
	    (assemble-LAP *current-disassembly*))
      (format t (str "lambda-expression=~s name=~s~% *current-codevec*=~s~%"
		     "*current-disassembly*=~s~% *current-lap-assembly*=~s~%")
	      lambda-expression name *current-codevec* *current-disassembly*
	      *current-lap-assembly*))))

(defun debug-assemble-lap ()
  ;; after running: (generate-artifacts #'my-plus-2)
  (let ((lap-disassembly (mapcar #'rest *current-disassembly*)))
    lap-disassembly))


;; works only on closure objects
;; (sys::closure-name #'my-plus-1)

;; (sys::closure-codevec #'my-plus-1)


;; (let ((x 123) (y 456))
;;   (defun my-plus-2 (z) (declare (compile)) (+ x y z)))

;; (sys::closure-consts #'my-plus-2)


(multiple-value-bind (req-num opt-num rest-p key-p keyword-list
                      allow-other-keys-p byte-list const-list)
    (sys::signature #'my-plus-1)
  (sys::disassemble-LAP byte-list const-list))

;; (sys::assemble-LAP (mapcar #'rest *))


;; (sys::assemble-LAP
;;  (mapcar #'rest
;; 	 '((0 LOAD&PUSH 2) (1 LOAD&PUSH 2) (2 CALLSR 2 53) (5 SKIP&RET 3))))






;; make trampoline provides some insight into how to 'manfucture'
;; arbitrary byte code by hand.

;; Creates a trampoline bytecode vector for jumping to a given function.
;; Used by set-funcallable-instance-function.
(defun make-trampoline (function)
  (multiple-value-bind (name req-num opt-num rest-p key-p)
      (function-signature function)
    (declare (ignore name))
    ; Don't use optional parameters, since the RETGF instruction doesn't
    ; support them.
    (when (> opt-num 0)
      (setq opt-num 0 rest-p t))
    (let ((fnode
            (make-fnode :name 'trampoline
                        :req-num req-num
                        :opt-num opt-num
                        :rest-flag (or rest-p key-p)
                        ;; no lalist, docstring or jitc:
                        :denv '((optimize (space 3) (speed 0)))
                        :code (let ((*form* nil) (*stackz* nil))
                                (make-anode :seclass *seclass-dirty*))
                        :Blocks-Offset 0
                        :Tagbodys-Offset 0
                        :Keyword-Offset 0
                        :Consts-Offset 0)))
      (create-fun-obj fnode
                      (assemble-LAP
                       `((VENV) (SKIP&RETGF ,(+ 1 req-num
                                                (if (or rest-p key-p) 1 0)))))
                      '(0 . 0))
      (let ((trampoline (fnode-code fnode)))
        (sys::closure-codevec trampoline)))))
