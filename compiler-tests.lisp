


(setf (ext:package-lock "SYS") nil)


(with-output-to-string (c-error-output)
       (let ((*c-error-output* c-error-output))
	 (sys::c-lambdabody 'CL-USER::my-add (lambda (a b) (+ a b)))))


;; try
(defparameter *my-fnode*
  (make-fnode :enclosing nil :venvc nil
              :name 'my-add))

(defparameter *my-fnode* nil)

(defun run-in-dynamic-scope (closure)
  (let ((definition nil)) ;; parameter of compile
    (flet ((closure-slot (obj num)
             (if (sys::closurep obj)
		 (sys::%record-ref obj num)
		 nil)))
      (let ((*compiling* t)
	    (*error-count* 0)
	    (*warning-count* 0)
	    (*style-warning-count* 0)
	    (*compiling-from-file* nil)
	    (*c-listing-output* nil)
	    (*fasoutput-stream* nil) ; compiled code may be called right away
	    (*c-error-output* *error-output*)
	    (*known-special-vars* '())
	    (*constant-special-vars* '())
	    (*func* nil)
	    (*fenv* (closure-slot definition 5))
	    (*benv* (closure-slot definition 6))
	    (*genv* (closure-slot definition 7))
	    (*venv* (closure-slot definition 4))
	    (*venvc* nil)
	    (*denv* (or (closure-slot definition 8)
			*toplevel-denv*))
	    (*no-code* nil)
	    ;; and a stray
	    (*fnode-list* '()))
	(funcall closure)))))



;; (lambda (a b) (+ a b)) --> lambdabody: '((a b) (+ a b))

(defun test-c-lambdabody ()
  (run-in-dynamic-scope
   (lambda ()
     (my-c-lambdabody 'CL-USER::my-add '((a b) (+ a b))))))

(defun test-make-fnode ()
  (run-in-dynamic-scope
   (lambda ()
     (setf *my-fnode*
	   (make-fnode :enclosing nil :venvc nil
		       :name 'my-add)))))

(defun test-with-output-to-string ()
  (with-output-to-string (c-error-output)
    (format c-error-output "A nice string")))



(let ((decls '()))
  (block spelalist-to-ordinary
    (tagbody
     start
       (when (null lalist)
	 (return-from spelalist-to-ordinary decls))
       (when (null speclist)
	 (return-from spelalist-to-ordinary decls))
       (let ((arg (car lalist)) (spec (car speclist)))
         (if (not (eq spec 'T))
             (setq decls (cons (list spec arg) decls))))
       (setq lalist (cdr lalist) speclist (cdr speclist))
       (go start))))

(lambda (spelalist caller)
    (multiple-value-bind (lalist speclist)
	(copy-list spelalist)
      (values
        lalist ; MAPCAN needs a LAMBDA which leads to infinite recursion
        (let ((decls '()))
          (block spelalist-to-ordinary
            (tagbody start
              (when (null lalist) (return-from spelalist-to-ordinary decls))
              (when (null speclist) (return-from spelalist-to-ordinary decls))
              (let ((arg (car lalist)) (spec (car speclist)))
                (if (not (eq spec 'T))
                  (setq decls (cons (list spec arg) decls))))
              (setq lalist (cdr lalist) speclist (cdr speclist))
              (go start)))))))



(defun c-PLUS ()
  (test-list *form* 1)
  (multiple-value-bind (const-sum other-parts)
      (c-collect-numeric-constants (cdr *form*))
    (unless (try-eval (setq const-sum (reduce #'+ const-sum)))
      (return-from c-PLUS (c-GLOBAL-FUNCTION-CALL-form *form*)))
    (cond ((null other-parts)   ; constant addends only
           (c-form const-sum))  ; ==> constant result
          ((eql const-sum 0)    ; const-sum == 0 ==> skip it
           ;; this is a bad optimization: THE is slower than #'+
           ;;(if (cdr other-parts) (c-form `(the number ,@other-parts)) ...)
	   (format t "(c-GLOBAL-FUNCTION-CALL-form `(+ ,@other-parts))=~s~%"
		   (typeof (c-GLOBAL-FUNCTION-CALL-form `(+ ,@other-parts))))
           (c-GLOBAL-FUNCTION-CALL-form `(+ ,@other-parts)))
          ((null (cdr other-parts)) ; just one non-constant summand
           (case const-sum
             (+1 (c-form `(1+ ,@other-parts)))
             (-1 (c-form `(1- ,@other-parts)))
             (t (c-GLOBAL-FUNCTION-CALL-form `(+ ,const-sum ,@other-parts)))))
          (t (c-GLOBAL-FUNCTION-CALL-form `(+ ,const-sum ,@other-parts))))))




;; compiles a form.
;; No code is generated, if no values are needed and the form
;; does not produce side effects.
;;
;; EEK -- not that only some only some 'inner' calls to my-c-form will
;; be processed here.
(defun my-c-form (*form* &optional (*for-value* *for-value*))
 (let
  ((anode
    (catch 'c-error
      (if (atom *form*)
        (if (symbolp *form*)
          (multiple-value-bind (macrop expansion)
              (venv-search-macro *form* *venv*)
            (if macrop ; Symbol-Macro ?
              (my-c-form expansion) ; -> expand
              (c-VAR *form*)))
          (c-CONST))
        (let ((fun (first *form*)))
          (if (function-name-p fun)
            (multiple-value-bind (a m f1 f2 f3 f4) (fenv-search fun)
              (declare (ignore f2 f4))
              (if (null a)
                ;; no local definition
                (multiple-value-bind (expansion expanded-p)
                    (expand-compiler-macro *form*)
		  (format t "my-c-form -- expansion=~s expanded-p=~s~%" expansion expanded-p)
                  (if expanded-p
                    (my-c-form expansion) ; -> expand
                    (let ((handler
                            (and (symbolp fun) (gethash fun c-form-table))))
                      (if handler ; found handler function?
                        ;; ==> (symbolp fun) = T
                        (if (or (and (special-operator-p fun)
                                     (not (macro-function fun)))
                                (not (declared-notinline fun)))
                          (funcall handler) ; yes -> call
                          (if (macro-function fun)
                            (my-c-form (mac-exp (macro-function fun) *form*))
                            ;; normal global function call
                            (c-GLOBAL-FUNCTION-CALL fun)))
                        ;; no -> not a special-form anyway
                        ;; (all those are in the c-form-table)
                        (if (and (symbolp fun) (macro-function fun))
                          ;; global macro
                          (my-c-form (mac-exp (macro-function fun) *form*))
                          ;; global function
                          (if (and (in-defun-p fun)
                                   (not (declared-notinline fun)))
                            ;; recursive call of the current global function
                            (c-LOCAL-FUNCTION-CALL fun (cons *func* nil)
                                                   (cdr *form*))
                            ;; normal call of the global function
                            (c-GLOBAL-FUNCTION-CALL fun)))))))
                (if (and m (not (and f1 (declared-notinline fun))))
                  (my-c-form (mac-exp m *form*))
                  (case f1
                    (GLOBAL ; found in the interpreter environment %fenv%
                     ; (my-c-form `(FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
                     (c-FUNCALL-NOTINLINE `(FUNCTION ,fun) (cdr *form*)))
                    (LOCAL  ; local function (found in *fenv*)
                     ; (my-c-form `(FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
                     (c-LOCAL-FUNCTION-CALL fun f3 (cdr *form*)))
                    (t (compiler-error 'c-form f1))))))
            (if (lambda-form-p fun)
              (my-c-form `(FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
              #| not: (c-LAMBDA-FUNCTION-CALL fun (cdr *form*)) |#
              (c-error fun (TEXT "Not the name of a function: ~S") fun))))))))
  #+CLISP-DEBUG (setf (anode-source anode) *form*)
  ;; If no values are needed and no side effects are produced,
  ;; the appendant code can be discarded completely:
  (when (and (null *for-value*) (anode-side-effect-free-p anode))
    (setf (anode-code anode) '())
    (setf (anode-seclass anode) *seclass-pure*))
   anode))

(defvar *my-func* nil)

;; compile (name lambdalist {declaration|docstring}* {form}*), return the FNODE
(defun my-c-LAMBDABODY (name lambdabody &optional fenv-cons gf-p reqoptimflags)
  (test-list lambdabody 1)
  (let* ((*func* (make-fnode :enclosing *func* :venvc *venvc*
                             :name (if (integerp name)
                                       (symbol-suffix (fnode-name *func*) name)
                                       name)))
         (*stackz* *func*) ; empty stack
         (*venvc* (cons *func* *venvc*))
         (*func-start-label* (make-label 'NIL))
         (*anonymous-count* 0)
         (lalist (car lambdabody))
         (type-decls '())
         (anode (catch 'c-error
		  ;; here it starts to become complicated
		  (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
					keyflag keyword keyvar keyinit keysvar
					allow-other-keys auxvar auxinit)
		      (if fenv-cons
			  ;; c-analyze-lambdalist was already called at c-LABELS
			  (values-list (cddar fenv-cons))
			  (progn
			    (when *defun-accept-specialized-lambda-list*
			      (multiple-value-setq (lalist type-decls)
				(sys::specialized-lambda-list-to-ordinary
				 lalist 'compile)))
			    (c-analyze-lambdalist lalist)))
		    (when (and optvar keyvar)
		      (c-style-warn (TEXT "Mixing ~S and ~S in lambda list ~S is bad design")
				    '&OPTIONAL '&KEY lalist))
		    (setf (fnode-req-num *func*) (length reqvar)
			  (fnode-opt-num *func*) (length optvar)
			  (fnode-rest-flag *func*) (not (eql restvar 0))
			  (fnode-keyword-flag *func*) keyflag
			  (fnode-keywords *func*) keyword
			  (fnode-lambda-list *func*) lalist
			  (fnode-allow-other-keys-flag *func*) allow-other-keys)
		    (format t "*func*=~s~%" *func*)
		    (when fenv-cons (setf (caar fenv-cons) *func*)) ; Fixup for c-LABELS
		    (multiple-value-bind (body-rest declarations docstring)
			(parse-body (cdr lambdabody) t)
		      (format t "parse-body: body-rest=~s declarations=~s docstring=~s~%"
			      body-rest declarations docstring)
		      (setf (fnode-documentation *func*) docstring)
		      (setq declarations (nreconc type-decls declarations))
		      (let ((oldstackz *stackz*)
			    (*stackz* *stackz*)
			    (*denv* *denv*)
			    (*venv* *venv*)
			    (*venvc* *venvc*)
			    *specials* *impdependents* *ignores* *ignorables* *readonlys* other-decls
			    req-vars req-dummys req-stackzs
			    opt-vars opt-dummys opt-anodes opts-vars opts-anodes opt-stackzs
			    rest-vars rest-dummys rest-stackzs
			    key-vars key-dummys key-anodes keys-vars keys-anodes key-stackzs
			    aux-vars aux-anodes
			    closuredummy-stackz closuredummy-venvc)
			(multiple-value-setq
			    (*specials* *impdependents* *ignores* *ignorables* *readonlys* other-decls)
			  (process-declarations declarations))
			;; visibility of Closure-Dummyvar:
			(push nil *venvc*)
			(setq closuredummy-venvc *venvc*)
			;; build Stack-Dummy-Variable for reqvar, optvar, restvar, keyvar:
			(multiple-value-setq (req-vars req-dummys)
			  (process-fixed-var-list reqvar reqoptimflags))
			(format t "process-fixed-var-list: req-vars=~s~%req-dummys=~s~%"
				req-vars req-dummys)
			(multiple-value-setq (opt-vars opt-dummys)
			  (process-fixed-var-list optvar))
			(multiple-value-setq (rest-vars rest-dummys)
			  (if (eql restvar 0)
			      (values '() '())
			      (process-fixed-var-list (list restvar))))
			(multiple-value-setq (key-vars key-dummys)
			  (process-fixed-var-list keyvar))
			;; room for the function itself (below the arguments):
			(push 1 *stackz*)
			;; room for Closure-Dummyvar:
			(push 0 *stackz*)
			(setq closuredummy-stackz *stackz*)
			;; activate the bindings of the required-parameters:
			(setq req-stackzs (bind-req-vars req-vars))
			;; req-stackzs (my-add (a b)) --> (0 1 1 1 fnode)
			(format t "req-stackzs=~s~%" req-stackzs)
			;; activate the bindings of the optional-parameters/svar:
			(multiple-value-setq (opt-anodes opt-stackzs opts-vars opts-anodes)
			  (bind-opt-vars opt-vars opt-dummys optinit optsvar))
			;; activate the bindings of the rest-parameters:
			(unless (eql restvar 0)
			  (setq rest-stackzs (bind-rest-vars rest-vars)))
			;; activate the bindings of the keyword-parameters/svar:
			(multiple-value-setq (key-anodes key-stackzs keys-vars keys-anodes)
			  (bind-opt-vars key-vars key-dummys keyinit keysvar))
			;; activate the bindings of the Aux-Variables:
			(multiple-value-setq (aux-vars aux-anodes)
			  (bind-aux-vars auxvar auxinit))
			(push-specials-impdependents)
			(push-*denv* other-decls)
			(format t "*denv*=~s~%" *denv*)
			(setf (fnode-denv *func*) *denv*)
			(let* ((body-anode (my-c-form `(PROGN ,@body-rest) (if gf-p 'ONE 'ALL)))
			       (throw-away (progn
					     (format t "gf-p=~s~%" gf-p)
					     (format t "body-rest=~s~%" body-rest)
					     ;; (format t "c-form=~s~%" (my-c-form '(PROGN (+ A B)) 'all))
					     (format t "c-form (list)in=~s~%" `(PROGN ,@body-rest))))
			       ;; check the variables:
			       (closurevars
				 (append
				  (checking-fixed-var-list req-vars reqoptimflags)
				  (checking-fixed-var-list opt-vars)
				  (checking-movable-var-list opts-vars opts-anodes)
				  (checking-fixed-var-list rest-vars)
				  (checking-fixed-var-list key-vars)
				  (checking-movable-var-list keys-vars keys-anodes)
				  (checking-movable-var-list aux-vars aux-anodes)))
			       (codelist
				 `(,*func-start-label*
				   ,@(c-make-closure closurevars closuredummy-venvc
						     closuredummy-stackz)
				   ,@(mapcap #'c-bind-fixed-var req-vars req-dummys
					     req-stackzs)
				   ,@(c-bind-with-svars opt-vars opt-dummys opts-vars
							opt-anodes opts-anodes opt-stackzs)
				   ,@(mapcap #'c-bind-fixed-var rest-vars rest-dummys
					     rest-stackzs)
				   ,@(c-bind-with-svars key-vars key-dummys keys-vars
							key-anodes keys-anodes key-stackzs)
				   ,@(mapcap #'c-bind-movable-var-anode aux-vars aux-anodes)
				   ,body-anode
				   (UNWIND ,*stackz* ,oldstackz t)
				   ,(if gf-p '(RETGF) '(RET))))
			       (anode-list `(,@opt-anodes ,@(remove nil opts-anodes)
							  ,@key-anodes ,@(remove nil keys-anodes)
							  ,@aux-anodes ,body-anode))
			       (anode
				 (make-anode
				  :type 'LAMBDABODY
				  :source lambdabody
				  :sub-anodes anode-list
				  :seclass *seclass-foldable*
				  :stackz oldstackz
				  :code codelist)))
			  (closuredummy-add-stack-slot
			   closurevars closuredummy-stackz closuredummy-venvc)
			  (optimize-var-list (append req-vars opt-vars opts-vars rest-vars
						     key-vars keys-vars aux-vars))
			  (setf (anode-seclass anode) (anodelist-seclass-or anode-list))
			  anode))))
		  ;; this was the production of the Anode
		  )))
    ;; anonymous functions are ignorable
    (unless (integerp name) (push *func* *fnode-list*))
    (setf (fnode-code *func*) anode)
    (when reqoptimflags
      (decf (fnode-req-num *func*) (count 'GONE reqoptimflags)))
    (when (eq (anode-type anode) 'ERROR)
      ;; turn it into a correct function, that does nothing
      (setf (fnode-req-num *func*) 0
            (fnode-opt-num *func*) 0
            (fnode-rest-flag *func*) t
            (fnode-keyword-flag *func*) nil
            (fnode-keywords *func*) '()
            (fnode-allow-other-keys-flag *func*) nil
            (anode-code (fnode-code *func*)) `((NIL) (SKIP 2) (RET))))
    (setf (fnode-gf-p *func*) gf-p)
    (setf (fnode-venvconst *func*)
          (not (and (null (fnode-far-used-vars *func*))
                    (null (fnode-far-assigned-vars *func*)))))
    (setf (fnode-Consts-Offset *func*)
	  (+ (setf (fnode-Keyword-Offset *func*)
		   (+ (setf (fnode-Tagbodys-Offset *func*)
			    (+ (setf (fnode-Blocks-Offset *func*)
				     (if (fnode-venvconst *func*) 1 0))
			       (length (fnode-Blocks *func*))))
		      (length (fnode-Tagbodys *func*))))
             (length (fnode-Keywords *func*))))
    (when gf-p
      ;; the dispatch of generic functions cannot refer to external blocks and
      ;; tagbodies. the keywords are indeed displaced perforce.
      (when (or (fnode-Blocks *func*) (fnode-Tagbodys *func*))
        (compiler-error 'c-LAMBDABODY "GF"))
      ;; Now (fnode-Keyword-Offset *func*) = (fnode-Tagbodys-Offset *func*) =
      ;;    = (fnode-Blocks-Offset *func*) = (if (fnode-venvconst *func*) 1 0)
      )
    ;; Set list of outer blocks that are needed by *func*.
    (setf (fnode-far-used-blocks *func*)
          (remove-if #'(lambda (block) (eq (block-fnode block) *func*))
                     (fnode-Blocks *func*)))
    ;; Set list of outer tagbodys and tags that are needed by *func*.
    (setf (fnode-far-used-tagbodys *func*)
          (remove-if #'(lambda (tagbody+tag)
                         (eq (tagbody-fnode (car tagbody+tag)) *func*))
                     (fnode-Tags *func*)))
    *func*))

(progn
  (test-c-lambdabody)
  nil)
