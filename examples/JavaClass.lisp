;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		JavaClass.lisp
;;;;	Contents:	Code for dumping a java class file.
;;;;	History:	2/21/01  RGC  Created.
;;;;
;;;;	As documented at:
;;;;	    http://java.sun.com/docs/books/vmspec/2nd-edition/html/ClassFile.doc.html
;;;;
;;;;	To create the console application, load this file and use
;;;;    this command:
;;;;        (save-application "javaclass" #'main :console t :static t)
;;;;
(defvar *display-java-method-info* nil)
(defvar *display-java-field-info* nil)
(defvar *display-usage-info* nil)
(defvar *descend-subdirectories* nil)

(defvar *class-bytes* nil)
(defvar *class-index* nil)

(defstruct java-class 
		magic 
		minor-version 
		major-version
		constant-pool
		access-flags
		this-class
		super-class
		interfaces
		fields
		methods
		attributes)

(defstruct constant-class-info 
		name-index)

(defstruct constant-field-ref-info 
		class-index 
		name-and-type-index)

(defstruct constant-method-ref-info 
		class-index 
		name-and-type-index)

(defstruct constant-interface-method-ref-info 
		class-index 
		name-and-type-index)

(defstruct constant-string-info 
		string-index)

(defstruct constant-integer-info 
		bytes)

(defstruct constant-float-info 
		bytes)

(defstruct constant-long-info 
		high-bytes 
		low-bytes)

(defstruct constant-double-info 
		high-bytes 
		low-bytes)

(defstruct constant-name-and-type-info 
		name-index 
		descriptor-index)

(defstruct constant-utf8-info 
		bytes 
		chars)

(defstruct constant-unused)

(defstruct field-info 
		access-flags 
		name 
		descriptor 
		attributes)

(defstruct attribute-info 
		name 
		info)

(defstruct method-info 
		access-flags 
		name 
		descriptor 
		attributes)

(defstruct (code-attribute (:print-function print-code-attribute))
		max-stack 
		max-locals 
		code 
		exception-table 
		attributes)

(defstruct exception-table-entry 
		start-pc 
		end-pc 
		handler-pc 
		catch-type)

(defstruct source-file-attribute 
		source-file)

(defstruct constant-value-attribute 
		constant-value)

(defstruct exceptions-attribute 
		exceptions)

(defstruct inner-class-table-entry 
		inner-class-info 
		outer-class-info 
		name 
		access-flags)

(defstruct inner-classes-attribute 
		classes)

(defstruct synthetic-attribute)

(defstruct line-number-table-entry 
		start-pc 
		line-number)

(defstruct line-number-table-attribute 
		line-number-table)

(defstruct local-variable-table-attribute 
		local-variable-table)

(defstruct local-variable-table-entry 
		start-pc 
		length 
		name 
		descriptor 
		index)

(defstruct deprecated-attribute)

;; access flags
(defconstant ACC_PUBLIC  		#x0001)	;;may be accessed from outside its package  
(defconstant ACC_PRIVATE  		#x0002) ;;usable only within the defining class
(defconstant ACC_PROTECTED  	#x0004) ;;may be accessed within subclasses 
(defconstant ACC_STATIC  		#x0008) ;;Declared static
(defconstant ACC_FINAL  		#x0010) ;;no further assignment after initialization 
(defconstant ACC_SYNCHRONIZED  	#x0020) ;;invocation is wrapped in a monitor lock  
(defconstant ACC_VOLATILE  		#x0040) ;;cannot be cached  
(defconstant ACC_TRANSIENT  	#x0080) ;;not written or read by a persistent object manager 
(defconstant ACC_NATIVE  		#x0100) ;;implemented in a language other than Java  
(defconstant ACC_ABSTRACT  		#x0400) ;;no implementation is provided  
(defconstant ACC_STRICT  		#x0800) ;;floating-point mode is FP-strict  


(defun print-code-attribute (obj stream level)
	(declare (ignore level))
	(print-unreadable-object (obj stream)
		(format stream 
			"Code object: Maxstack=~A, Maxlocals=~A, code=~A bytes, ~
			 exception-table=~A, attributes=~A"
			(code-attribute-max-stack obj)
			(code-attribute-max-locals obj)
			(length (code-attribute-code obj))
			(code-attribute-exception-table obj)
			(code-attribute-attributes obj))))
	
(defun read-java-class (path)
	"Returns a byte array containing the bytes of the passed path."
	(with-open-file (input-stream path :direction :input :element-type 'unsigned-byte)		
		(let* ((length (file-length input-stream))
			   (bytes (make-array length :element-type 'byte)))
			(dotimes (i length)
				(setf (aref bytes i) (read-byte input-stream)))
			bytes)))

(defun u1 () 
	(prog1 (aref *class-bytes* *class-index*) (incf *class-index*)))

(defun u2 () 
	(prog1 
		(+ (* #x100 (aref *class-bytes* *class-index*)) 
			(aref *class-bytes* (+ *class-index* 1)))
		(incf *class-index* 2)))

(defun u4 () 
	(prog1 
		(+ (* #x1000000 (aref *class-bytes* *class-index*))
		   (* #x10000 	(aref *class-bytes* (+ *class-index* 1)))
		   (* #x100 	(aref *class-bytes* (+ *class-index* 2)))
		   				(aref *class-bytes* (+ *class-index* 3)))
		(incf *class-index* 4)))

(defun ubytes (n)
	(prog1
		(subseq *class-bytes* *class-index* (+ *class-index* n))
		(incf *class-index* n)))

(defun bytes-to-string (bytes)
	(let ((a (make-array (length bytes) :element-type 'character)))
		(dotimes (i (length bytes) a)
			(setf (elt a i) (int-char (aref bytes i))))))
		
(defun get-constant-pool (num)
	(let ((constants '()))
		(dotimes (i num (apply 'vector (nreverse constants)))
			(let ((tag (u1)))
				(push
					(case tag
						(7 (make-constant-class-info :name-index (u2)))
						(9 (make-constant-field-ref-info 
									:class-index (u2) 
									:name-and-type-index (u2)))
						(10 (make-constant-method-ref-info 
									:class-index (u2) 
									:name-and-type-index (u2)))
						(11 (make-constant-interface-method-ref-info 
									:class-index (u2) 
									:name-and-type-index (u2)))
						(8 (make-constant-string-info :string-index (u2))) 
						(3 (make-constant-integer-info :bytes (u4)))
						(4 (make-constant-float-info :bytes (u4)))
						(5 (push (make-constant-long-info 
									:high-bytes (u4)
									:low-bytes (u4)) constants) 
								(incf i)
								(make-constant-unused))			;; don't use next entry
						(6 (push (make-constant-double-info 
									:high-bytes (u4)
									:low-bytes (u4)) constants) 
								(incf i)
								(make-constant-unused))			;; don't use next entry
						(12 (make-constant-name-and-type-info
									:name-index (u2)
									:descriptor-index (u2)))
						(1 (make-constant-utf8-info :chars (bytes-to-string (ubytes (u2))))))
					constants)))))

(defun constant-pool-item (pool index)
	"Get the constant associated with the index (or NIL if index = 0)"
	(and (> index 0) (svref pool (- index 1))))

(defun constant-pool-class-name (pool index)
	"Get the name associated with the class constant index (or NIL if index = 0)"
	(let* ((class-info (constant-pool-item pool index)))
		(if class-info
			(let ((class-name-info 
						(constant-pool-item pool 
							(constant-class-info-name-index class-info))))
				(and class-name-info (constant-utf8-info-chars class-name-info))))))

(defun constant-pool-name (pool index)
	"Get the name associated with the constant index (or NIL if index = 0)"
	(let* ((name-info (constant-pool-item pool index)))
		(and name-info (constant-utf8-info-chars name-info))))

(defun parse-exception-table (num)
	(let ((exception-table '()))
		(dotimes (i num (nreverse exception-table))
			(push (make-exception-table-entry 
						:start-pc 	(u2)
						:end-pc 	(u2)
				        :handler-pc (u2)
						:catch-type (u2))
				exception-table))))
	
(defun parse-code-attribute (pool)
	(make-code-attribute 
		:max-stack 		 (u2) 
		:max-locals 	 (u2) 
		:code 			 (ubytes (u4))
		:exception-table (parse-exception-table (u2))
		:attributes 	 (get-attributes (u2) pool)))

(defun parse-source-file-attribute (pool)
	(make-source-file-attribute 
		:source-file (constant-pool-name pool (u2))))

(defun parse-constant-value-attribute (pool)
	(make-constant-value-attribute 
		:constant-value (constant-pool-item pool (u2))))

(defun parse-exceptions (num pool)
	(let ((exceptions '()))
		(dotimes (i num (nreverse exceptions))
			(push (constant-pool-item pool (u2)) exceptions))))
	
(defun parse-exceptions-attribute (pool)
	(make-exceptions-attribute :exceptions (parse-exceptions (u2) pool)))

(defun parse-inner-classes (num pool)
	(let ((classes '()))
		(dotimes (i num (nreverse classes))
			(push (make-inner-class-table-entry
						:inner-class-info (constant-pool-item pool (u2))
						:outer-class-info (constant-pool-item pool (u2))
						:name (constant-pool-name pool (u2))
						:access-flags (u2))
					classes))))
		
(defun parse-inner-classes-attribute (pool)
	(make-inner-classes-attribute :classes (parse-inner-classes (u2) pool)))	

(defun parse-line-number-table (num)
	(let ((entries '()))
		(dotimes (i num (nreverse entries))
			(push (make-line-number-table-entry
						:start-pc (u2)
						:line-number (u2))
				entries))))
	
(defun parse-line-number-table-attribute (pool)
	(declare (ignore pool))
	(make-line-number-table-attribute 
		:line-number-table (parse-line-number-table (u2))))

(defun parse-local-variable-table (num pool)
	(let ((entries '()))
		(dotimes (i num (nreverse entries))
			(push (make-local-variable-table-entry
					:start-pc (u2)
					:length (u2)
					:name (constant-pool-name pool (u2))
					:descriptor (constant-pool-name pool (u2))
					:index (u2))
					entries))))
	
(defun parse-local-variable-table-attribute (pool)
	(make-local-variable-table-attribute 
		:local-variable-table (parse-local-variable-table (u2) pool)))

(defun parse-synthetic-attribute (pool)
	(declare (ignore pool))
	(make-synthetic-attribute))

(defun parse-deprecated-attribute (pool)
	(declare (ignore pool))
	(make-deprecated-attribute))

(defconstant *attribute-parse-funcs* 
	'("Code" 				parse-code-attribute 
	  "SourceFile" 			parse-source-file-attribute
	  "ConstantValue"		parse-constant-value-attribute
	  "Exceptions"			parse-exceptions-attribute
	  "InnerClasses"		parse-inner-classes-attribute
	  "Synthetic"			parse-synthetic-attribute
	  "LineNumberTable" 	parse-line-number-table-attribute
	  "LocalVariableTable"  parse-local-variable-table-attribute
	  "Deprecated"			parse-deprecated-attribute))

(defun get-attributes (num pool)
	(let ((attrs '()))
		(dotimes (i num (nreverse attrs))
			(let ((name (constant-pool-name pool (u2)))
				  length)
				(setf length (u4))
				(let ((func (second (member name *attribute-parse-funcs* :test #'equal))))
					(push
						(if func
							(funcall func pool)
							(make-attribute-info 
										:name name 
										:info (ubytes length)))
						attrs))))))

(defun get-fields (num pool)
	(let ((fields '()))
		(dotimes (i num (nreverse fields))
			(push 
				(make-field-info 
					:access-flags (u2) 
					:name (constant-pool-name pool (u2))
					:descriptor (constant-pool-name pool (u2))
					:attributes (get-attributes (u2) pool))
				fields))))

(defun get-methods (num pool)
	(let ((methods '()))
		(dotimes (i num (nreverse methods))
			(push 
				(make-method-info 
					:access-flags (u2) 
					:name (constant-pool-name pool (u2))
					:descriptor (constant-pool-name pool (u2))
					:attributes (get-attributes (u2) pool))
				methods))))

(defun get-interfaces (num)
	(let ((interfaces '()))
		(dotimes (i num (nreverse interfaces)) 
			(push (u2) interfaces))))

(defun parse-java-class (bytes)
	(if (< (length bytes) 24)
		(return-from parse-java-class nil))
	(let* ((*class-bytes* bytes)
		   (*class-index* 0))
		(let ((*class-index* 0))
			(if (/= (u4) #xCAFEBABE)
				(return-from parse-java-class nil)))
		(let ((pool nil))
			(make-java-class
				:magic (u4)
				:minor-version (u2)
				:major-version (u2)
				:constant-pool (setf pool (get-constant-pool (- (u2) 1)))
				:access-flags (u2)
				:this-class (constant-pool-class-name pool (u2))
				:super-class (constant-pool-class-name pool (u2))
				:interfaces (get-interfaces (u2))
				:fields (get-fields (u2) pool)
				:methods (get-methods (u2) pool)
				:attributes (get-attributes (u2) pool)))))

(defconstant *access-flags*
	(list 
		ACC_PUBLIC 		"public"		ACC_PRIVATE 	"private" 		
		ACC_PROTECTED 	"protected" 	ACC_STATIC 		"static" 	
		ACC_FINAL   	"final"   		ACC_VOLATILE  	"volatile"
		ACC_TRANSIENT 	"transient" 	ACC_SYNCHRONIZED "synchronized"
		ACC_NATIVE		"native"		ACC_ABSTRACT	"abstract"
		ACC_STRICT		"strict"))

(defun format-access-flags (access-flags)
	"Used by printing of both methods and fields, returns a formatted string"
	(do* ((flags '())
		  (x *access-flags* (cddr x))
		  (bit (car x) (car x))
		  (name (cadr x) (cadr x)))
		((null x)(format nil "~:A" (nreverse flags)))
		(if (plusp (logand access-flags bit))
			(push name flags))))

;field-info access-flags name descriptor attributes
(defun format-field-info (f)
	(let ((access-flags (field-info-access-flags f))
		  (name (field-info-name f))
		  (descriptor (field-info-descriptor f)))
		(declare (ignore descriptor))
		(format nil "~16t~A ~40t~A" name (format-access-flags access-flags))))
 
(defun field-report (java-class output-stream)
	(let ((fields (java-class-fields java-class)))
		(format output-stream "~%Fields:~16tName~40tAttributes~%")
		(dolist (f fields)
			(format output-stream "~A~%" (format-field-info f)))))

(defun format-method-info (m)
	(let* ((access-flags (method-info-access-flags m))
		   (name (method-info-name m))
		   (descriptor (method-info-descriptor m))
		   (attrs (method-info-attributes m))
		   (code (find-if #'code-attribute-p attrs)))
		(declare (ignore descriptor))
		(format nil "~16t~A ~40t~A ~60t~A" 
			name 
			(format-access-flags access-flags)
			(if code (length (code-attribute-code code)) 0))))

(defun method-report (java-class output-stream)
	(let ((methods (java-class-methods java-class)))
		(format output-stream "~%Methods:~16tName~40tAttributes~60tSize(bytes)~%")
		(dolist (m methods)
			(format output-stream "~A~%" (format-method-info m)))))

(defun largest-method (java-class)
	(let ((methods (java-class-methods java-class))
		  (max 0))
		(dolist (m methods max)
			(let* ((attrs (method-info-attributes m))
				   (code (find-if #'code-attribute-p attrs))
				   (code-length (and code (length (code-attribute-code code)))))
				(if (and code-length (> code-length max))
					(setf max code-length))))))
	
(defun java-class-report (path)
	(let* ((bytes (read-java-class path))
		   (jc (parse-java-class bytes)))
		(format t "File: ~A~%" (namestring (truename path)))
		(when (null jc)
			(format t "Not a valid java class file.~%~%")
			(return-from java-class-report nil))
		(format t "Class: ~20t~A~%" (java-class-this-class jc))
		(format t "Base class: ~20t~A~%" (java-class-super-class jc))
		(format t "Version: ~20t~A.~A~%" (java-class-major-version jc)
			(java-class-minor-version jc))
		(format t "Class size: ~20t~A bytes~%" (length bytes))
		(format t "Largest method: ~20t~A bytes~%" (largest-method jc))
		(when *display-java-field-info*
			(field-report jc *standard-output*))
		(when *display-java-method-info*
			(method-report jc *standard-output*))
		(format t "~%")))


(defun display-usage-info () 
	(format t "Usage: JavaClass [-methods] [-fields] [-all] [-recurse] [-?] classfile1 ...~%")
	(format t "~10t-methods~20tDisplay a report of methods~%")
	(format t "~10t-fields~20tDisplay a report of fields~%")
	(format t "~10t-all~20tShow everything~%")
	(format t "~10t-recurse~20tDescend subdirectories when expanding wildcards~%")
	(format t "~10t-?~20tDisplay this usage information~%"))

(defun process-command-line-args (args)
	"Filter out and process switches"
	(let ((new-args '()))
		(dolist (arg args (nreverse new-args))
			(let ((ch (char arg 0)))
				(if (or (char= ch #\/) (char= ch #\-))
					(let ((switch (subseq arg 1)))
						(cond ((equalp switch "methods")(setf *display-java-method-info* t))
							  ((equalp switch "fields") (setf *display-java-field-info* t))
							  ((equalp switch "all")    (setf *display-java-method-info* t
															  *display-java-field-info* t))
							  ((equalp switch "?")		(setf *display-usage-info* t))
							  ((equalp switch "recurse")(setf *descend-subdirectories* t))
							  (t 						(format t "Unknown switch: ~A" switch))))
					(push arg new-args))))))

(defun flatten (list)
	(let ((new-list '()))
		(dolist (x list (nreverse new-list))
			(if (atom x)
				(push x new-list)
				(dolist (y (flatten x))
					(push y new-list))))))

(defun expand-wildcards (files)
	(let ((new-files '()))
		(dolist (file files (nreverse new-files))
			(if (or (find #\? file) (find #\* file))
				(let ((expanded (flatten (directory file :recurse *descend-subdirectories*))))
					(dolist (x expanded)
						(push x new-files)))
				(push file new-files)))))

(defun check-files (files)
	(let ((new-files '()))
		(dolist (file files (nreverse new-files))
			(if (equalp (pathname-type (pathname file))	"class")
				(push file new-files)))))
		
(defun main ()
	(format t "Java Class Reporter by Roger Corman~%")
	(let ((args (ccl:get-command-line-args)))
		(setf args 
			(check-files 
				(expand-wildcards 
					(process-command-line-args args))))
		(if (or (null args) *display-usage-info*)
			(display-usage-info)
			(dolist (x args)
				(java-class-report x)))
		(force-output)
		(win:exitprocess 0)))

		