


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



(defun my-c-GLOBAL-FUNCTION-CALL (fun) ; fun is a Symbol or (SETF symbol)
  (test-list *form* 1)
  (note-function-used fun (cdr *form*) nil)
  ;; eek
  ;; (when *compiling-from-file* ; called by COMPILE-FILE?
  ;;   ;; take note of PROCLAIM-declarations:
  ;;   (when (and (eq fun 'PROCLAIM) (= (length *form*) 2))
  ;;     (let ((h (second *form*)))
  ;;       (when (c-constantp h)
  ;;         (c-form
  ;;          `(eval-when-compile (c-PROCLAIM ',(c-constant-value h)))))))
  ;;   ;; take note of module requirements:
  ;;   (when (and (memq fun '(PROVIDE REQUIRE))
  ;;              (every #'c-constantp (rest *form*)))
  ;;     (c-form
  ;;       `(eval-when-compile
  ;;          (,(case fun
  ;;              (PROVIDE 'c-PROVIDE)  ; c-PROVIDE instead of PROVIDE
  ;;              (REQUIRE 'c-REQUIRE)) ; c-REQUIRE instead of REQUIRE
  ;;           ,@(mapcar           ; quote arguments
  ;;              #'(lambda (x) (list 'QUOTE (c-constant-value x)))
  ;;              (rest *form*))))))
  ;;   ;; take note of package-requirements:
  ;;   (when (and *package-tasks-treat-specially*
  ;;              (memq fun '(MAKE-PACKAGE SYSTEM::%IN-PACKAGE
  ;;                          SHADOW SHADOWING-IMPORT EXPORT UNEXPORT
  ;;                          USE-PACKAGE UNUSE-PACKAGE IMPORT))
  ;;              (every #'c-constantp (rest *form*)))
  ;;     (push
  ;;       `(,fun
  ;;         ,@(mapcar
  ;;             ;; Quote the arguments, but only when necessary, because
  ;;             ;; ANSI-CL IN-PACKAGE wants unquoted arguments.
  ;;             #'(lambda (x)
  ;;                 (let ((v (c-constant-value x)))
  ;;                   (if (or (numberp v) (characterp v) (arrayp v) (keywordp v))
  ;;                     v
  ;;                     (list 'QUOTE v))))
  ;;             (rest *form*)))
  ;;       *package-tasks*)))
  (let* ((args (cdr *form*))    ; arguments
         (n (length args)))     ; number of arguments
    (if (or (not (fboundp fun)) (declared-notinline fun))
      ;; the function arguments will not be checked
      (c-NORMAL-FUNCTION-CALL fun)
      (multiple-value-bind (name req opt rest-p key-p keylist allow-p check)
          (function-signature fun t)
        (setq check (and name (test-argument-syntax args nil fun req opt rest-p
                                                    key-p keylist allow-p)))
        (if (and name (equal fun name)) ; function is valid
          (case fun
            ((CAR CDR FIRST REST NOT NULL CONS SVREF VALUES
              CAAR CADR CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR
              CDDAR CDDDR SECOND THIRD FOURTH CAAAAR CAAADR CAADAR CAADDR
              CADAAR CADADR CADDAR CADDDR CDAAAR CDAADR CDADAR CDADDR
              CDDAAR CDDADR CDDDAR CDDDDR ATOM CONSP SYS::%UNBOUND
              VALUES-LIST SYS::%SVSTORE EQ SYMBOL-FUNCTION LIST LIST*)
             ;; these here have keylist=NIL, allow-p=NIL and
             ;; (which is not used) opt=0.
             (if check ; (and (<= req n) (or rest-p (<= n (+ req opt))))
               ;; we make the call INLINE.
               (let ((sideeffects ; side-effect-class of the function-execution
                       (if (>= (declared-optimize 'SAFETY) 3)
                         *seclass-dirty* ; see comment in F-SIDE-EFFECT
                         (function-side-effect fun)))) ; no need for a check
                 (cond ((and (null *for-value*)
                             (null (seclass-modifies sideeffects)))
                        ;; don't have to call the function,
                        ;; only evaluate the arguments
                        (c-form `(PROGN ,@args)))
                       ((and (seclass-foldable-p sideeffects)
                             (every #'c-constantp args))
                        ;; call the function at compile time
                        (let ((*stackz* *stackz*))
                          (multiple-value-bind (seclass codelist)
                              (collect-args sideeffects args)
                            (return-if-foldable seclass fun codelist
                                                c-GLOBAL-FUNCTION-CALL)
                            (compiler-error 'c-GLOBAL-FUNCTION-CALL
                                            (list fun args seclass codelist)))))
                       ((and (eq fun 'VALUES) (eq *for-value* 'ONE))
                        (if (= n 0) (c-NIL) (c-form `(PROG1 ,@args))))
                       (t (let ((*stackz* *stackz*))
			    (multiple-value-bind (seclass codelist)
			        (collect-args sideeffects args)
                              ;; evaluate the arguments and push all to the
                              ;; stack except for the last (because the last
                              ;; one is expected in A0):
			      (pop codelist) (pop *stackz*)
                              (setq codelist
                                    (nreconc codelist
                                             (function-code-list fun n)))
                              (make-anode
                               :type `(PRIMOP ,fun)
                               :sub-anodes (remove-if-not #'anode-p codelist)
                               :seclass seclass
                               :code codelist))))))
               ;; check failed (wrong argument count) => not INLINE:
               (c-NORMAL-FUNCTION-CALL fun)))
            (t ; is the SUBR fun contained in the FUNTAB?
             (let ((index (gethash fun function-codes)))
               (case fun
                 ;; accept &KEY and an even number of &OPTIONAL
                 ((PARSE-NAMESTRING MERGE-PATHNAMES READ-FROM-STRING)
                  ;; (#:ARG0 &OPTIONAL #:ARG1 #:ARG2 &KEY ...)
                  ;; it just so happens that a keyword is a bad
                  ;; (or, in case of READ-FROM-STRING, an unusual)
                  ;; second (1st optional) argument for these functions,
                  ;; thus this keywordp test is good enough
                  (when (keywordp (second args))
                    (c-style-warn (TEXT "Apparently passing &KEY arguments without &OPTIONAL arguments in ~S") *form*))))
               (if index
                 (case check
                   ((NO-KEYS STATIC-KEYS)
                    ;; correct syntax, stack layout is known
                    ;; at compile time ==> INLINE
                    (c-DIRECT-FUNCTION-CALL
                      args nil fun req opt rest-p keylist keylist
                      t ; it is a SUBR
                      (let ((call-code ; call with help from the FUNTAB:
                              (cons
                                (if (not rest-p)
                                  (CALLS-code index)
                                  `(CALLSR ,(max 0 (- n req opt)) ; When n<req+opt another (PUSH-UNBOUND ...) still has to come
                                           ,(- index funtabR-index)))
                                (case fun
                                  (;; functions, that do not return:
                                   (;; control.d:
                                    SYS::DRIVER SYS::UNWIND-TO-DRIVER
                                    ;; debug.d:
                                    ;; SYS::REDO-EVAL-FRAME
                                    ;; SYS::RETURN-FROM-EVAL-FRAME
                                    ;; error.d:
                                    ERROR SYSTEM::ERROR-OF-TYPE
                                    INVOKE-DEBUGGER)
                                   '((BARRIER)))
                                  (t '())))))
                        #'(lambda () call-code))))
                   (t (c-NORMAL-FUNCTION-CALL fun)))
                 ;; not a SUBR
                 (let ((inline-lambdabody (inline-lambdabody fun)))
                   (if (inline-callable-lambdabody-p inline-lambdabody n)
                     ;; inline call of the global function is possible
                     (c-FUNCALL-INLINE fun args nil inline-lambdabody nil)
                     (c-NORMAL-FUNCTION-CALL fun)))))))
          (c-NORMAL-FUNCTION-CALL fun))))))

(defun my-c-PLUS ()
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


;; (sys::assemble-LAP (mapcar #'rest *))
;; (sys::disassemble-LAP byte-list const-list)


;; Tests -- Note, these only work in clisp.


(defvar *current-lambda-expression*)
(defvar *current-lambda-name*)
;; Note the difference between codevec and lap-assembly. The codevec
;; is actually the encoding of the full closure object.
(defvar *current-codevec*)
(defvar *current-lap-assembly*)
(defvar *current-disassembly*)

(defun my-plus-1 (x y)
  (declare (compile))
  (+ x y))

(defun my-empty-function ()
  (declare (compile)))


(function-lambda-expression #'my-plus-1)

(defun str (&rest rest)
  (apply #'concatenate 'string rest))

(defun generate-artifacts (expression)
  (multiple-value-bind (lambda-expression lambdap name)
      (function-lambda-expression expression)
    (declare (ignore lambdap))
    (setf *current-lambda-expression* lambda-expression)
    (setf *current-lambda-name* name)
    (setf *current-codevec* (sys::closure-codevec expression))
    (multiple-value-bind (req-num opt-num rest-p key-p keyword-list
			  allow-other-keys-p byte-list const-list)
	(sys::signature expression)
      (declare (ignore req-num opt-num rest-p key-p keyword-list
			  allow-other-keys-p))
      (setf *current-disassembly* (sys::disassemble-LAP byte-list const-list))
      (setf *current-lap-assembly*
	    (sys::assemble-LAP
	     (mapcar #'rest *current-disassembly*)))
      (format t (str "lambda-expression=~s name=~s~% *current-codevec*=~s~%"
		     "*current-disassembly*=~s~% *current-lap-assembly*=~s~%")
	      lambda-expression name *current-codevec* *current-disassembly*
	      *current-lap-assembly*))))




;; works only on closure objects
(sys::closure-name #'my-plus-1)

(sys::closure-codevec #'my-plus-1)


(let ((x 123) (y 456))
  (defun my-plus-2 (z) (declare (compile)) (+ x y z)))

(sys::closure-consts #'my-plus-2)


(multiple-value-bind (req-num opt-num rest-p key-p keyword-list
                      allow-other-keys-p byte-list const-list)
    (sys::signature #'my-plus-1)
  (sys::disassemble-LAP byte-list const-list))

;; (sys::assemble-LAP (mapcar #'rest *))


(sys::assemble-LAP
 (mapcar #'rest
	 '((0 LOAD&PUSH 2) (1 LOAD&PUSH 2) (2 CALLSR 2 53) (5 SKIP&RET 3))))
