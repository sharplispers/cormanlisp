;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		misc-features.lisp
;;;;	Contents:	Corman Lisp miscellaneous features.
;;;;	History:	2/27/97  RGC  Created.
;;;;                11/29/02 RGC  Redefined cl::package-hash-index to handle
;;;;                              strings with fill pointers correctly.
;;;;                03/08/06 RGC  Added code to refresh the symbol cache when necessary
;;;;                              (used by DISASSEMBLE).
;;;;

(in-package :common-lisp)

;;; some forward references
(defun cl::generic-function-discriminating-function (gf) (declare (ignore gf)) nil) ;; define later
(defun cl::standard-generic-function-p (x) (declare (ignore x)) nil)   ;; define later

(defun %features-member (feature-list)
	(if (symbolp feature-list)
		(return-from %features-member (member feature-list *features*)))
	(if (consp feature-list)
		(ecase (car feature-list)
			(:and (every #'%features-member (cdr feature-list)))
			(:or  (some #'%features-member (cdr feature-list)))	
			(:not (notany #'%features-member (cdr feature-list))))
		(error "~A is not a valid feature." feature-list)))

(set-dispatch-macro-character #\# #\+
	#'(lambda (stream char int)
		(declare (ignore char int))
		(let (feature)
			(let* ((*package* (find-package :keyword)))
			   (setf feature (read stream)))
			(if (and (not *read-suppress*) (%features-member feature))       
				(read stream)
    			(let ((*read-suppress* t))
    				(read stream)
                    (values))))))
 
(set-dispatch-macro-character #\# #\-
	#'(lambda (stream char int)
		(declare (ignore char int))
		(let (feature)
			(let* ((*package* (find-package :keyword)))
				(setf feature (read stream)))
			(if (and (not *read-suppress*) (not (%features-member feature)))
				(read stream)
                (let ((*read-suppress* t))
    				(read stream)
                    (values))))))

;;;
;;;		Common Lisp DESCRIBE function.
;;;
(defun describe (object &optional stream)
	;; need to implement this
	(write object :stream stream))

;;;
;;;		Common Lisp *QUERY-IO* global symbol
;;;
(defvar *query-io* *terminal-io*)

;;;
;;;	Common Lisp Y-OR-N-P function
;;;
(defun y-or-n-p (&optional format-string &rest arguments)
	(let ((stream *query-io*))
		(if format-string
			(progn
				(fresh-line stream)
				(apply #'format stream format-string arguments)))
		(format stream "(Y/N)~%")
		(do ((response-char))
			(nil nil)			
			(setq response-char (char-upcase (read-char stream)))
			(cond
				((not (graphic-char-p response-char))) 
				((eq response-char #\Y) (return-from y-or-n-p t))
				((eq response-char #\N) (return-from y-or-n-p nil))
				(t (format stream "(Y/N)~%"))))))

(defun yes-or-no-p (&optional format-string &rest arguments)
	(let ((stream *query-io*))
		(if format-string
			(progn
				(fresh-line stream)
				(apply #'format stream format-string arguments)))
		(format stream "(Yes/No)~%")
		(do ((response))
			(nil nil)			
			(setq response (read stream))
			(cond
				((string-equal response "YES")(return-from yes-or-no-p t))
				((string-equal response "NO") (return-from yes-or-no-p nil))
				(t (format stream "(Yes/No)~%"))))))

;;;
;;;	Corman Lisp WEAK-POINTER-OBJ function.
;;;
(defun weak-pointer-obj (weak-ptr) 
	(unless (weak-pointer-p weak-ptr)
		(error "Not a weak-pointer: ~A" weak-ptr))
	(uref weak-ptr weak-ptr-offset))

(defun function-info-list (func)
	(if (and (symbolp func)(fboundp func))
		(setq func (symbol-function func)))
	(check-type func function)
	(if (standard-generic-function-p func)
		(setf func (generic-function-discriminating-function func)))
	(if (pl::kernel-function-p func)
		nil 
		(let* ((compiled-code (uref func function-code-buffer-offset))
			   (properties (uref compiled-code compiled-code-info-offset)))
			properties)))

;;;
;;;	FUNCTION-ENVIRONMENT function.
;;;
(defun function-environment (func)
	(if (and (symbolp func)(fboundp func))
		(setq func (symbol-function func)))
	(check-type func function)
	(if (standard-generic-function-p func)
		(setf func (generic-function-discriminating-function func)))
	(uref func function-environment-offset))

;;;
;;;	Corman Lisp FUNCTION-REFERENCES function.
;;;
(defun function-references (func) 
	(if (and (symbolp func)(fboundp func))
		(setq func (symbol-function func)))
	(check-type func function)
	(if (standard-generic-function-p func)
		(setf func (generic-function-discriminating-function func)))
	(if (pl::kernel-function-p func)
		nil 
		(uref (uref func function-code-buffer-offset) 
			compiled-code-references-offset)))

;;;
;;;	Common Lisp FUNCTION-LAMBDA-EXPRESSION function.
;;;
(defun function-lambda-expression (func)
	(let ((info (function-info-list func)))
		(values (getf info 'lambda)
				(function-environment func)
				(getf info 'function-name))))

;;; extension--returns only the function name
(defun ccl::function-name (func) 
	(let ((info (function-info-list func)))
		(getf info 'function-name)))

#|
(defun cl::address-find-function-callback (obj)
	(if (functionp obj)
		(let* ((execaddr (execution-address obj))
			   (diff (- cl::FIND_FUNCTION_CURR_ADDR execaddr)))
			(unless (minusp diff)
				(when (< diff cl::FIND_FUNCTION_CURR_OFFSET)
					(setq cl::FIND_FUNCTION_CURR_OFFSET
						(- cl::FIND_FUNCTION_CURR_ADDR execaddr))
					(setq cl::FIND_FUNCTION_CURR_FUNC obj))))))
|#

(defvar *count-heap* 0)
(defun count-heap-objects (predicate)
	(setq *count-heap* 0)
	(cl::process-each-heap-block 
		#'(lambda (obj) 
			(if (funcall predicate obj) 
				(incf *count-heap*))))
	*count-heap*)

(defun ensure-var-table-entry (symbol)
	(if (zerop (uref symbol cl::symbol-var-table-offset))
		(cl::%create-var-table-entry symbol))
	(uref symbol cl::symbol-var-table-offset))

;;;
;;;	Common Lisp PROGV Special Form
;;;	Implemented here as a macro.
;;;
(defmacro progv (symbols values &rest forms)
	;; establish the bindings
	(let ((syms-sym (gensym))
		  (vals-sym (gensym)))
		`(let* ((,syms-sym ,symbols)
			    (,vals-sym ,values))
			(do* ((s ,syms-sym (cdr s))
		       	  (v ,vals-sym (cdr v)))
				((null s))
				(check-type (car s) symbol)
				(ensure-var-table-entry (car s))
				(if (null v)
					(progn
						(cl::%push-special-bindings (car s) nil)
						(makunbound (car s)))
					(cl::%push-special-bindings (car s) (car v))))
			(unwind-protect 
				(progn ,@forms) 
				(cl::%pop-special-bindings ,syms-sym)))))

;;;
;;;	Common Lisp ASSERT macro
;;;
(defmacro assert (test-form &rest rest)
	(let ((places (first rest))
		   (msg    (second rest))
		   (args   (cddr rest)))
		(declare (ignore places))
		(if (null msg)
			`(unless ,test-form (error "Assertion failed: ~A" ',test-form))
			`(unless ,test-form (error ,msg ,@args)))))

(defvar *gensym-counter* 0)
(defconstant *gensym-string* "G")

(defun gensym (&optional x)
    (prog1
        (if x
            (if (integerp x)
                (make-symbol (format nil "~A~A" *gensym-string* x))
                (if (stringp x)
                    (make-symbol (format nil "~A~A" x *gensym-counter*))
                    (error "Invalid argument to GENSYM: ~A" x)))
            (make-symbol (format nil "~A~A" *gensym-string* *gensym-counter*)))
        (unless (integerp x) (incf *gensym-counter*))))

;; REMOVE-PROPERTY
;; Returns two values: 
;;		the (possibly) modified list
;;		t if the values was found, nil otherwise
;;
(defun remove-property (plist property)
	(cond ((null plist) (values plist nil))
		  ((eq (car plist) property) (values (cddr plist) t))
		  (t (do* ((p (cddr plist) (cddr p))
				   (q plist (cddr q)))
				  ((null p) (values plist nil))
				(when (eq (car p) property)
					(setf (cdr (cdr q)) (cddr p)) 
					(return (values plist t)))))))

;;; Common Lisp REMF function.
(defmacro REMF (place indicator)
	(let ((list-sym (gensym))
		  (flag-sym (gensym)))			
		(if (not (consp place))
			`(multiple-value-bind (,list-sym ,flag-sym)
				(remove-property ,place ,indicator)
				(when ,flag-sym
					(setf ,place ,list-sym)
					,flag-sym))
			(let* ((ret (cl::%once-only-forms place))
				   (let-vars (caar ret))
				   (set-form (cdr ret)))
				(if let-vars
					`(let (,let-vars)
						(multiple-value-bind (,list-sym ,flag-sym)
							(remove-property ,set-form ,indicator)
							(when ,flag-sym
								(setf ,set-form ,list-sym)
								,flag-sym)))
					`(multiple-value-bind (,list-sym ,flag-sym)
						(remove-property ,place ,indicator)
						(when ,flag-sym
							(setf ,place ,list-sym)
							,flag-sym)))))))

;;; Common Lisp REMPROP function.
(defun REMPROP (symbol indicator)
	(remf (symbol-plist symbol) indicator))	

;;;
;;; This hash-table is used internally to map the QV offset (as found in
;;; the slots 8 and 9 of the symbols) back to the symbol that contains
;;; the index. It is only needed for disassembly purposes (annotating
;;; the output.
;;;
(defvar *symbol-mapping* nil 
	"Map all jump-table-offsets and var-table-offsets to the symbols")

(defun lookup-qv-offset (index)
	(when (or (null *symbol-mapping*) 
            (null (second (multiple-value-list (gethash index *symbol-mapping*)))))  ;; need to refresh?
		(setf *symbol-mapping* (make-hash-table))
		(dolist (package (list-all-packages))
			(do-symbols (x package)
				(let ((jump-index (uref x symbol-jump-table-offset))
					(var-index (uref x symbol-var-table-offset)))
					(if (/= jump-index 0)
						(setf (gethash jump-index *symbol-mapping*) x))
					(if (/= var-index 0)
						(setf (gethash var-index  *symbol-mapping*) x))))))
	(gethash index *symbol-mapping*))

(defun calc-dword (bytes start end)
	(do ((x (- end 1) (- x 1))
		 (val 0))
		((< x start) val)
		(setf val (+ (* val 256) (aref bytes x)))))

(defun find-instruction-reference (bytes)	;; byte vector of instruction bytes
	(cond
		((and (= (length bytes) 6)
			  (= (aref bytes 0) #xff)
			  (= (aref bytes 1) #x96))		;; near dword ptr [esi+dword_offset]
		 (let* ((indirect-address (calc-dword bytes 2 6))
			    (referenced-address (/ (- indirect-address 4) 4))
			    (func-name (lookup-qv-offset referenced-address)))
			(format nil "Call ~S" func-name)))
		((and (= (length bytes) 6)
			  (= (aref bytes 0) #x8b)
			  (= (aref bytes 1) #x86))		;; mov eax,[esi+dword_offset]
		 (let* ((indirect-address (calc-dword bytes 2 6))
			    (referenced-address (/ indirect-address 4))
			    (var-name (lookup-qv-offset referenced-address)))
			(format nil "Get ~S special binding" var-name)))
		((and (= (length bytes) 6)
			  (= (aref bytes 0) #x8b)
			  (= (aref bytes 1) #xbe))		;; edi,[esi+dword_offset]
		 (let* ((indirect-address (calc-dword bytes 2 6))
			    (referenced-address (/ indirect-address 4))
			    (func-name (lookup-qv-offset referenced-address)))
			(format nil "Load ~S environment" func-name)))
		((and (= (length bytes) 2)
			  (= (aref bytes 0) #x3b)
			  (= (aref bytes 1) #x06))		;; cmp eax,[esi]
		 "EAX = NIL?")
		((and (= (length bytes) 3)
			  (= (aref bytes 0) #x3b)
			  (= (aref bytes 1) #x46)
			  (= (aref bytes 2) #x04))		;; cmp eax,[esi+4]
		 "EAX = T?")
		((and (= (length bytes) 2)
			  (= (aref bytes 0) #x8b)
			  (= (aref bytes 1) #x06))		;; mov eax,[esi]
		 "mov eax, NIL")
		((and (= (length bytes) 3)
			  (= (aref bytes 0) #x8b)
			  (= (aref bytes 1) #x46)
			  (= (aref bytes 2) #x04))		;; mov eax,[esi+4]
		 "mov eax, T")
		((and (= (length bytes) 2)
			  (= (aref bytes 0) #x8b)
			  (= (aref bytes 1) #x3e))		;; mov edi,[esi]
		 "Load null environment")
		((and (= (length bytes) 1)
			  (= (aref bytes 0) #xfd))		;; std
		 "BEGIN-ATOMIC")
		((and (= (length bytes) 1)
			  (= (aref bytes 0) #xfc))		;; cld
		 "END-ATOMIC")))

;;;
;;;	Common Lisp DISASSEMBLE function
;;;	
(defun ccl::function-code-references (x) (declare (ignore x)) nil) ;; redefine below

(defun disassemble (func &optional (max-instructions 1000))
	(let* ((addr nil)
		   (src func) 
		   (references nil)
		   (num-references nil)
		   (curr-ref 0))
		(if (symbolp src)
			(setq src (symbol-function src)))
		(if (and (consp src)(eq (car src) 'lambda))
			(setf src (eval `(function ,src))))
		(if (integerp src)
			(setq addr src)
			(if (functionp src)
				(let ((function (if (standard-generic-function-p src)
							(generic-function-discriminating-function src)
							src)))
					(setq addr (execution-address function)
						  references (ccl::function-code-references function)
						  num-references (/ (length references) 2)))
				(error "Cannot disassemble ~A" func)))
		(format t ";Disassembling from address #x~x:~%" addr)
		(do* ((offset 0)
			  instruction
			  (instruction-bytes -1)
			  (count 0 (+ count 1))
			  (bytes nil))
			((or (= instruction-bytes 0)(= count max-instructions)))
			(multiple-value-setq (instruction instruction-bytes)
				(disassembly-statement addr offset))
			;; if there is a code reference for this statement, append it here
			(if (and
					(< curr-ref num-references)
					(let ((pos (aref references (+ 1 (* curr-ref 2)))))
						(and (>= pos offset)(< pos (+ offset instruction-bytes)))))
				(setf instruction 
					(format nil "~A   ~60T; '~S" instruction 
						(aref references (* curr-ref 2)))
					curr-ref (+ 1 curr-ref)))
			
			;; See if any function calls or other operations are being made
			;; which implicitly reference other things.
			(setf bytes (make-array instruction-bytes :element-type 'byte))
			(dotimes (i instruction-bytes)
				(setf (aref bytes i)(ccl:peek-byte (+ addr offset i))))
			(let ((instruction-ref (find-instruction-reference bytes)))
				(if instruction-ref
					(setf instruction
						(if (stringp instruction-ref)
							(format nil "~A   ~60T; ~A" instruction instruction-ref)
							(format nil "~A   ~60T; ~S" instruction instruction-ref)))))
			
			(format t ";#x~5,'0x:~4t~A~%" offset instruction)
			(incf offset instruction-bytes))))

;;;
;;;	Common Lisp WITH-OPEN-FILE function.
;;;
(defmacro with-open-file ((stream filespec &rest options) 
				&rest decls-and-forms)
	`(let ((,stream (open ,filespec ,@options)))
		(unwind-protect
			(let* ()
				,@decls-and-forms)
			(and (streamp ,stream) (close ,stream)))))

;;;
;;;	Common Lisp WITH-OPEN-STREAM function.
;;;
(defmacro with-open-stream ((var stream) &rest decls-and-forms)
	`(let ((,var ,stream))
		(unwind-protect
			(let* ()
				,@decls-and-forms)
			(and (streamp ,var) (close ,var)))))

(defun equalp-arrays (x y)
	(unless (equalp (array-dimensions x)(array-dimensions y))
		(return-from equalp-arrays nil))
	(dotimes (i (array-total-size x))
		(unless (equalp (row-major-aref x i) (row-major-aref y i))
			(return-from equalp-arrays nil)))
	t)

;;; Assume two structures as input, return true iff both contain the same
;;; slot contents (under equalp).	
(defun equalp-structures (x y)
    (let ((num-slots-x (uvector-num-slots x))
          (num-slots-y (uvector-num-slots y)))
        (if (/= num-slots-x num-slots-y)
            nil
            (dotimes (i num-slots-x t)
                (unless (equalp (uref x (+ i 1)) (uref y (+ i 1)))
                    (return-from equalp-structures nil))))))
    
(defun equalp-hash-tables (x y)
  (and (eql (hash-table-test x) (hash-table-test y))
       (eql (hash-table-count x) (hash-table-count y))
       (with-hash-table-iterator (next x)
         (loop
            (multiple-value-bind (more x-key x-value)
                (next)
              (if more
                  (multiple-value-bind (y-value foundp)
                      (gethash x-key y)
                    (unless (and foundp (equalp x-value y-value))
                      (return nil)))
                  (return t)))))))

(defun equalp (x y)
	(cond ((equal x y)	t)
		  ((and (characterp x) (characterp y)) 	(char-equal x y))
		  ((and (numberp x)(numberp y)) 		(= x y))
		  ((and (consp x)(consp y))				(and (equalp (car x)(car y))(equalp (cdr x)(cdr y))))
		  ((and (stringp x)(stringp y))			(string-equal x y))
		  ((and (arrayp x)(arrayp y))			(equalp-arrays x y))
		  ((and (hash-table-p x)(hash-table-p y)) (equalp-hash-tables x y))
		  ((and (structurep x)(structurep y))	(equalp-structures x y))
   		  (t nil)))

;;;
;;; COPY-SYMBOL implementation by Frank Adrian
;;;
(defun copy-symbol (sym &optional copy-props)
	(let ((new-sym (make-symbol (symbol-name sym))))
		(when copy-props
			(when (boundp sym)
				(setf (symbol-value new-sym) (symbol-value sym)))
			(when (fboundp sym)
				(setf (symbol-function new-sym) (symbol-function sym)))				
			(let ((plist (symbol-plist sym)))
				(when plist (setf (symbol-plist new-sym) plist))))
		new-sym))

;;; redefine this internal function here, it is called by SORT
#|
(defun nquicksort-vector (v predicate key)
	(macrolet ((<less> (a b) `(funcall predicate ,a ,b))
			   (<elt> (v i) `(if key (funcall key (aref ,v ,i)) (aref ,v ,i))))
		(labels 
			((qs (l u)
				(if (>= l u) (return))
				(let ((temp (<elt> v l))
					  (i l)
					  (j (+ u 1)))
					(loop
						(loop do (incf i) while (and (<= i u) (<less> (<elt> v i) temp)))
						(loop do (decf j) while (<less> temp (<elt> v j)))
						(if (> i j) (return))
						(rotatef (aref v i) (aref v j)))
					(rotatef (aref v l) (aref v j))
					(qs l (- j 1))
					(qs (+ j 1) u))))
			(qs 0 (1- (length v)))))
	v)
|#
;;;
;;;	Corman Lisp  HYPERSPEC function
;;;
(defvar ccl::*hyperspec-loaded* nil)
(defvar ccl:*cormanlisp-directory* (symbol-value 'ccl:*cormanlisp-directory*))
(defvar ccl:*cormanlisp-server-directory* (symbol-value 'ccl:*cormanlisp-server-directory*))

(defun ccl:hyperspec (sym)
	(unless ccl::*hyperspec-loaded*
		(load (concatenate 'string ccl:*cormanlisp-directory* "/sys/hyperspec.lisp")))
	(funcall #'ccl:hyperspec sym))

(in-package :ccl)
;;;
;;;	Corman Lisp CORMANLISP-CLIENT-TYPE function.
;;;
(defun ccl::cormanlisp-client-type ()
	"Returns :win-app-client, :console-client or :ide-client"
	(case (cl::cormanlisp-client-type)
		(0 :win-app-client)
		(1 :console-client)
		(2 :ide-client)
		(otherwise :unknown)))

(export 'ccl::cormanlisp-client-type)


;;;
;;; functions for accessing function properties
;;;
(defun function-code-buffer (func)
	(check-type func function)
	(if (ccl::kernel-function-p func)
		nil
		(uref func cl::function-code-buffer-offset)))

(defun kernel-function-lambda-list (func)
	(declare (ignore func))
	nil)		;; not implemented yet

(defun function-info-list (func)
	(let ((cb (function-code-buffer func)))
		(if cb
			(uref cb cl::compiled-code-info-offset))))

(defun (setf function-info-list) (val func)
	(let ((cb (function-code-buffer func)))
		(if cb
			(setf (uref cb cl::compiled-code-info-offset) val))))
	
(defun function-lambda-list (func) (getf (function-info-list func) 'cl::lambda-list))
(defun function-lambda (func) (getf (function-info-list func) 'cl::lambda))
(defun function-source-file (func) (getf (function-info-list func) 'ccl:*source-file*))
(defun function-source-line (func) (getf (function-info-list func) 'ccl:*source-line*))
(defun macro-lambda-list (func) (getf (function-info-list func) 'cl::macro-lambda-list))
(defun (setf macro-lambda-list) (val func) 
	(setf (getf (function-info-list func) 'cl::macro-lambda-list)
		val))

;;;
;;; For an optimization, these are normally encoded in a packed array
;;; of 16-bit unsigned integers directly following the compiled code bytes.
;;; If disassembling, or otherwise debugging, we build the vector of
;;; references on the fly as requested.
;;;
(defun peek-unsigned-short (address)	
	(+ (* (peek-byte (+ address 1)) 256) (peek-byte address)))

(defun code-buffer-code-address (cb)
	(+ (ccl:%uvector-address cb) (* cl::compiled-code-code-offset 4)))

(defun build-compiled-code-references (cb)
	(let* ((refs (uref cb cl::compiled-code-references-offset))
		   (code-start-address (code-buffer-code-address cb))
		   (refs-address (+ refs code-start-address))
		   (num-refs (peek-unsigned-short refs-address))
		   (refs-vec (make-array (* num-refs 2)))
		   (curr-offset code-start-address))
		(dotimes (i num-refs)
			(let* ((pos (peek-unsigned-short (+ 2 refs-address (* i 2))))
				   (obj (peek-lisp-object (incf curr-offset pos))))
				(setf (aref refs-vec (* i 2)) obj 
					  (aref refs-vec (+ 1 (* i 2))) (- curr-offset code-start-address))))
		refs-vec))

(defun function-code-references (func)
	(let* ((cb (function-code-buffer func)))
		(if (null cb)
			nil
			(let ((refs (uref cb cl::compiled-code-references-offset)))
				(if (integerp refs)
					(if (= refs 0) nil (build-compiled-code-references cb))
					refs)))))

(export '(ccl::function-code-buffer
		ccl::function-info-list
		ccl::function-lambda-list
		ccl::function-lambda
		ccl::function-source-file
		ccl::function-source-line
		ccl::function-code-references
		ccl::function-name))

(in-package :cl)

;;; Redefine this hash function here to correctly handle strings with fill pointers
(defun package-hash-index (package str)
    (let ((h 0)
          (len (length str)))
        (dotimes (i len)
            (setq h (logxor h (char-int (elt str i))))
            (setq h (logand (ash h 5) #xffffffff)))
        (if (/= (logand h #x80000000) 0)
            (setf h (- (- h #x100000000))))
        (mod h (package-capacity package))))

;;;
;;; Redefine some functions here to use (SETF ELT) instead of the old kernel SETELT function
;;;
(defun set-package-entry-string (str package index)
	(let ((package-vector (package-table package)))
		(setf (elt package-vector (* index 3)) str)))

(defun set-package-entry-symbol (sym package index)
	(let ((package-vector (package-table package)))
		(setf (elt package-vector (+ (* index 3) 1)) sym)))

(defun set-package-entry-state (state package index)
	(let ((package-vector (package-table package)))
		(if (eq state 'external)
			(setf (elt package-vector (+ (* index 3) 2)) 1)
			(setf (elt package-vector (+ (* index 3) 2)) 0))))

;;
;; Suspend other Lisp threads execution. It is here
;; mostly for debugging purposes.
;;
(defmacro %with-other-threads-suspended (&rest body)
  `(unwind-protect
		(progn
		  (cl::suspend-other-threads)
		  (progn
			,@body))
	 (cl::resume-other-threads)))

(export 'cl::%with-other-threads-suspended)

