;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		misc.lisp
;;;;	Contents:	Corman Lisp misc. library functions
;;;;				system.
;;;;	History:	8/28/96  RGC  Created.
;;;;				12/16/98 RGC  Moved ASSOC, EQL functions to expand.lisp
;;;;							  as they are needed earlier during system building.
;;;;				2/1/01   RGC  Added mods by Frank Adrian for SETF GET.
;;;;				4/6/01   RGC  Moved SIMPLE-STRING-P to this file.
;;;;				5/16/01  RGC  Fixes for string handling functions.
;;;;				7/31/01  RGC  Added explicit SETF registry functions.
;;;;                12/10/01 RGC  Modified %once-only-forms to handle THE operators in place.
;;;;                9/19/03  RGC  Incorporated JP Massar's EVAL enhancement to handling of PROGN.
;;;;

;;;
;;; At the start, the property list of this symbol is used as the registry.
;;; During booting, a hashtable is assigned to this variable, and it
;;; becomes the registry.
;;;
(defvar *setf-registry* nil)

(defun |(SETF GET)| (val sym attr &optional (default nil))
	;; Not allowed to declare when this is loaded.
	;(declare (ignore default))
	(do* ((plist (symbol-plist sym))
		  (p plist (cddr p)))
		 ((null p) 
		  (progn 
			(set-symbol-plist (cons attr (cons val plist)) sym)
			val))
		 (if (eq (car p) attr)
			(progn
				(rplaca (cdr p) val)
				(return val)))))

(defun register-setf-function (name setf-func-name &optional (value-last nil))
	(|(SETF GET)| (list setf-func-name value-last) '*setf-registry* name))

(defun get-setf-function (name)
	(car (get '*setf-registry* name)))

(defun setf-function-value-last-p (name)
	(cadr (get '*setf-registry* name)))

(defun lookup-setf-function (name)
	(let ((result (get-setf-function name)))
		(if (null result)
			(error "Could not find function (SETF ~A)" name)
			(if (symbolp result)
				(symbol-function result)
				result))))

(register-setf-function 'get '|(SETF GET)|)

(defmacro setf (&rest args)
	(do* ((a args (cddr a))
		  (expr (car a) (car a))
		  (val (cadr a) (cadr a))
		  (exprs nil))
		((null a) `(progn ,@(nreverse exprs)))
		(if (symbolp expr)
			(setq exprs (cons `(setq ,expr ,val) exprs))
			(if (and (symbolp (car expr)) (get-setf-function (car expr)))
				(let* ((sym (car expr))
				   	   (func (get-setf-function sym)))
					(setq exprs (cons `(,func ,val ,@(cdr expr)) exprs)))
				(error "Invalid SETF expression")))))

(register-setf-function 'symbol-function 'set-symbol-function)
(register-setf-function 'elt 'setelt)
(register-setf-function 'uref 'uref-set)

(defun char= (&rest chars)
	(unless chars (return-from char= t))
	(let ((c (car chars)))
 		(unless (characterp c)
			(error "Not a character: ~A" c))
		(dolist (x (cdr chars))
 			(unless (characterp x)
				(error "Not a character: ~A" x))
			(unless (eq c x) (return-from char= nil)))
		t))

(defun char-equal (&rest chars)
	(unless chars (return-from char-equal t))
	(let ((c (car chars)))
 		(unless (characterp c)
			(error "Not a character: ~A" c))
		(setq c	(char-upcase c))
		(dolist (x (cdr chars))
 			(unless (characterp x)
				(error "Not a character: ~A" x))
			(unless (eq (char-upcase x) c) (return-from char-equal nil)))
		t))

(defun string= (x y &key (start1 0) end1 (start2 0) end2)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(let ((numchars (- end1 start1)))
		(unless (= numchars (- end2 start2))
			(return-from string= nil))
		(dotimes (i numchars)
			(unless (char= (elt x i) (elt y i))
				(return-from string= nil)))
		t))

(defun string-equal (x y &key (start1 0) end1 (start2 0) end2)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(let ((numchars (- end1 start1)))
		(unless (= numchars (- end2 start2))
			(return-from string-equal nil))
		(dotimes (i numchars)
			(unless (char-equal (elt x i) (elt y i))
				(return-from string-equal nil)))
		t))

(defun number-types-eq (x y) 
	(cond
		((integerp x) (integerp y))
		((double-float-p x)   (double-float-p y))
		((single-float-p x)   (single-float-p y))
		((short-float-p x)	  (short-float-p y))
		((ratiop x)   (ratiop y))
		((complexp x) (complexp y))))

(defun bit-vector-p (x) (and (vectorp x) (eq (array-type x) 'bit)))
(defun bit-vector= (x y)
	(let ((numbits (length x)))
		(unless (= numbits (length y))
			(return-from bit-vector= nil))
		(dotimes (i numbits)
			(unless (eq (elt x i) (elt y i))
				(return-from bit-vector= nil)))
		t))

(defun pathnamep (x) nil)		;; pathnames not implemented yet
(defun pathname= (x y) nil)		;; pathnames not implemented yet

(defun equal (x y)
	(or (eql x y)
		(and (consp x) (consp y) (equal (car x) (car y)) (equal (cdr x) (cdr y)))
		(and (stringp x) (stringp y) (string= x y))
		(and (bit-vector-p x) (bit-vector-p y) (bit-vector= x y))
		(and (pathnamep x) (pathnamep y) (pathname= x y))))

;;; need to fix this later!!!
(defun equalp (x y)
	(equal x y))

(defun sublis (alist tree &key key (test #'eql) test-not)
	(let ((match (assoc tree alist 
					:key key 
					:test test 
					:test-not test-not)))
		(if match (cdr match)
			(if (not (consp tree)) tree
				(cons (sublis alist (car tree) 
						:key key :test test :test-not test-not)
					  (sublis alist (cdr tree) 
						:key key :test test :test-not test-not))))))

(defun min-sequence-length (sequences)
	(let ((min (length (car sequences))))
		(dolist (x (cdr sequences))
			(let ((length (length x)))
				(if (< length min)
					(setq min length))))
		min))

(defun some (predicate &rest sequences)
	(unless sequences (return-from some nil))
	(let ((len (min-sequence-length sequences)))
		(dotimes (i len)
			(let ((args nil)
				  ret)
				(dolist (s sequences)
					(setq args (cons (elt s i) args)))
				(setq ret (apply predicate (nreverse args)))
				(if ret (return ret))))))

(defun %once-only-forms (form)
	(let* 
		((args (rest form)) ; raw form arguments
		 (letlist 
			(let ((newlist nil))
				(dolist (x form)
					(when (and (consp x) (not (eq (car form) 'the)))
						(push `(,(gensym) ,x) newlist)))
				(nreverse newlist)))
		 (revlist 
			(let ((newlist nil))
				(dolist (x letlist)
					(push (cons (second x) (first x)) newlist))
				(nreverse newlist)))
		 (newform (cons (first form) (sublis revlist args))))
		(cons letlist newform)))

(defmacro incf (form &optional (delta 1))
	(if (and (consp form) (some #'consp form))
		(let ((retval (%once-only-forms form)))
			`(let ,(car retval) 
				(setf ,(cdr retval) (+ ,(cdr retval) ,delta))))
		`(setf ,form (+ ,form ,delta))))

(defmacro decf (form &optional (delta 1))
	(if (and (consp form) (some #'consp form))
		(let ((retval (%once-only-forms form)))
			`(let ,(car retval) 
				(setf ,(cdr retval) (- ,(cdr retval) ,delta))))
		`(setf ,form (- ,form ,delta))))

(defmacro prog1 (first-x &rest rest-x)
	(let ((sym (gensym)))
		`(let* ((,sym ,first-x)) 
			,@rest-x
			,sym)))

(defmacro prog2 (first-x second-x &rest rest-x) 
	(let ((sym (gensym)))
		`(let* ((,sym (progn ,first-x ,second-x))) 
			,@rest-x
			,sym)))

(defmacro push (val form)
	(if (and (consp form) (some #'consp form))
		(let ((retval (%once-only-forms form)))
			`(let ,(car retval) 
				(setf ,(cdr retval) (cons ,val ,(cdr retval)))))
		`(setf ,form (cons ,val ,form))))

(defmacro pop (form)
	(if (and (consp form) (some #'consp form))
		(let ((retval (%once-only-forms form)))
			`(let ,(car retval) 
				(prog1 (first ,(cdr retval))
					(setf ,(cdr retval) (rest ,(cdr retval))))))
		`(prog1 (first ,form) (setf ,form (rest ,form)))))

(defun member (item list &key key (test #'eql) test-not)
	(if test-not
		(let ((save-test test))
			(setq test #'(lambda (x y) (not (funcall save-test x y))))))
	(do ((i list (cdr i)))
		((null i))
		(if (funcall test item (if key (funcall key (car i)) (car i)))
			(return i))))

(defun member-if (predicate list &key key)
	(do ((i list (cdr i)))
		((null i))
		(if (funcall predicate (if key (funcall key (car i)) (car i)))
			(return i))))

(defun member-if-not (predicate list &key key)
	(do ((i list (cdr i)))
		((null i))
		(if (not (funcall predicate (if key (funcall key (car i)) (car i))))
			(return i))))

(defun adjoin (item list &key key (test #'eql) test-not)
	(if (member (if key (funcall key item) item) list 
			:key key :test test :test-not test-not) 
		list 
		(cons item list)))

(defmacro defun (name lambda-list &rest forms)
	(let ((doc-form nil) 
		  (lambda-form nil) 
		  (declarations nil))

		;; look for declarations and doc string
		(do* ((f forms (cdr f)))
			((null f) (setq forms f))
			(if (and (stringp (car f)) (null doc-form) (cdr f))
				(setq doc-form 
					`((setf (documentation ',name 'function) ,(car f))))
				(if (and (consp (car f)) (eq (caar f) 'declare))
					(push (car f) declarations)
					(progn (setq forms f) (return)))))

		(setq lambda-form 
			`(lambda ,lambda-list ,@(nreverse declarations)
				(block ,name ,@forms))) 		
		`(progn
			,@doc-form
			(setf (symbol-function ',name) (function ,lambda-form))
			',name))) 

(defun set-property (plist property val)
	(do* ((p plist (cddr p)))
		 ((null p) (cons property (cons val plist)))
		 (if (eq (car p) property)
			(progn (rplaca (cdr p) val) (return plist)))))

(defmacro |(SETF GETF)| (val plist property &optional default)
	(declare (ignore default))
	(let ((sym (gensym)))
		`(let ((,sym ,val))
			(setf ,plist (set-property ,plist ,property ,sym))
			,sym)))
	
(register-setf-function 'getf '|(SETF GETF)|)

;;;
;;;	Common Lisp (SETF SYMBOL-PLIST) macro.
;;;
(defun |(SETF SYMBOL-PLIST)| (val symbol)
	(unless (symbolp symbol)
		(error "Not a symbol: ~A" symbol))
	(setf (uref symbol symbol-plist-offset) val))

;;;
;;; At the start, the property list of this symbol is used as the 
;;; documentation registry.
;;; During booting, a hash-table is assigned to this variable, and it
;;; becomes the registry.
;;;
(defvar *documentation-registry* nil)

(defun documentation (symbol doc-type)
	(getf (get '*documentation-registry* symbol) doc-type))

(defun |(SETF DOCUMENTATION)| (doc-string symbol doc-type)
	(when doc-string (setf (getf (get '*documentation-registry* symbol) doc-type) doc-string))
	doc-string)

(register-setf-function 'symbol-plist '|(SETF SYMBOL-PLIST)|)
(register-setf-function 'documentation '|(SETF DOCUMENTATION)|)
(register-setf-function 'macro-function 'set-symbol-macro)

(defun |(SETF CAR)| (val list)
	(rplaca list val) val)
(register-setf-function 'car '|(SETF CAR)|)
(defun |(SETF CDR)| (val list)
	(rplacd list val) val)
(register-setf-function 'cdr '|(SETF CDR)|)

(defun |(SETF CADR)| (val list)
	(setf (car (cdr list)) val))
(register-setf-function 'cadr '|(SETF CADR)|)
	 
;
;	Common Lisp 'multiple-value-setq' macro
;
(defmacro multiple-value-setq (varlist form)
	(let ((setq-forms nil) 
		  (value-list-sym (gensym)) 
		  (return-form-sym (gensym)))
		(do ((v varlist (cdr v)) (count 0 (1+ count)))
			((null v))
			(push 
				`(setq ,(car v) (nth ,count ,value-list-sym)) 
				setq-forms))
		`(let* ((,value-list-sym (multiple-value-list ,form))
				(,return-form-sym (car ,value-list-sym)))
			,@(reverse setq-forms)
			,return-form-sym)))

;
;	Common Lisp 'multiple-value-bind' macro
;
(defmacro multiple-value-bind (vars value-form &rest forms)
	(let ((sym (gensym))
		  (declarations nil))

		;; look for declarations
		(do* ((f forms (cdr f)))
			((null f) (setq forms f))
			(if (and (consp (car f)) (eq (caar f) 'declare))
				(push (car f) declarations)
				(progn (setq forms f) (return))))

		`(let ,vars
			,@(nreverse declarations) 
			(multiple-value-setq ,vars ,value-form)
			,@forms)))

;
;	Common Lisp 'in-package' macro
;
(defun make-package (name) nil)		;; dummy function, defined later
(defmacro in-package (name)
	`(eval-when (:load-toplevel :compile-toplevel :execute)
		(let ((package (find-package ',name)))
			(if package
				(setq *package* package)
				(setq *package* (make-package ',name))))))

;
;	Common Lisp 'provide' function.
;
(defun provide (module-name)
	(if (symbolp module-name)
		(setq module-name (symbol-name module-name)))
	(push module-name *modules*)
	module-name)

(defun parse-integer (string 
		&key (start 0) 
			 (end (length string))
			 (radix 10)
			 (junk-allowed nil)
		&aux (result 0)
			 (state :initial)
			 (sign 1)
			 c)

	;; check for leading sign
	(setf c (char string start))
	(if (char= c #\-)
		(progn (setf sign -1) (incf start))
		(if (char= c #\+)
			(incf start)))

	(do* ((i start (+ i 1))
		  (n 0))
		((>= i end))
		(setq c (char string i))
		(setq n (digit-char-p c radix))
		(cond
			(n (progn
				(cond
					((eq state :finished) 
					 (if (not junk-allowed)
						(error "Invalid integer parsed: ~A" string)
						(progn (setq end i) (return)))))
				(setq result (+ (* result radix) n))
				(setq state :collecting)))
			
			((member c (list (int-char 13) (int-char 32) (int-char 9)))	;; '(#\Newline #\Space #\Tab)
				(cond
					((eq state :collecting) (setq state :finished))
					((eq state :initial) nil)	; don't do anything
					((eq state :finished) nil)))
			(t 
				(if (not junk-allowed)
					(error "Invalid integer parsed: ~A" string)  ;; string  
					(progn (setq end i) (return))))))

	(if (eq state :initial)
		(setq result nil)
		(setq result (* result sign)))
	(values result end))

(defun char-code (ch) (char-int ch))

(defconstant adjustable-array-vector-offset			1)
(defconstant adjustable-array-fill-pointer-offset	2)
(defconstant adjustable-array-displaced-offset		3)
(defconstant adjustable-array-dimensions-offset		4)
(defconstant adjustable-array-dim1-offset			5)
(defconstant adjustable-array-header-max-size		12)
(defconstant adjustable-array-header-min-size		5)


(defun adjustable-array-p (a) 
	(and (uvectorp a)(= (uvector-type-bits a) uvector-array-tag)))

(defun array-has-fill-pointer-p	(a)
	(unless (adjustable-array-p a)
		(if (arrayp a)
			(return-from array-has-fill-pointer-p nil)
			(error "Not an array: ~A" a)))
	(let ((fp (uref a adjustable-array-fill-pointer-offset)))
		(if (>= fp 0) t nil)))

(defun fill-pointer (a) 
	(unless (adjustable-array-p a)
		(if (arrayp a)
			(error "Array ~A does not have a fill pointer" a)
			(error "Not an array: ~A" a)))
	(let ((fp (uref a adjustable-array-fill-pointer-offset)))
		(if (< fp 0)
			(error "Vector ~A does not have a fill pointer" a))
		fp))

(defun |(SETF FILL-POINTER)| (v a)
	(unless (adjustable-array-p a)
		(if (arrayp a)
			(error "Array ~A does not have a fill pointer" a)
			(error "Not an array: ~A" a)))
	(if (< (uref a adjustable-array-fill-pointer-offset) 0)
		(error "Vector ~A does not have a fill pointer" a))
	(unless (and (integerp v) (>= v 0) (<= v (array-dimension a 0)))
		(error "Invalid fill pointer specified: ~A" v))
	(setf (uref a adjustable-array-fill-pointer-offset) v)
	v)

(register-setf-function 'fill-pointer '|(SETF FILL-POINTER)|)

;
;	Common Lisp ARRAY-ELEMENT-TYPE function
;
(defun array-element-type (a) 
	(unless (arrayp a)
		(error "Not an array: ~A" a))
	(array-type a))

(defun bytes-cells (numbytes)
	"Returns the number of cells required to store the requested
	 number of bytes"
	 (truncate (+ numbytes 3) 4))

;;; Unicode mod
(defun char-cells (numbytes)
	"Returns the number of cells required to store the requested
	 number of bytes"
	 (truncate (+ numbytes 1) 2))

;;; Returns the number of cells required to store the requested
;;; number of bits.
(defun bits-cells (numbits)
	 (truncate (+ numbits 31) 32))

(defun allocate-char-vector (size)
	(let ((vec (alloc-uvector (+ (char-cells size) 1) uvector-simple-char-vector-tag)))
		(setf (uref vec 1) size)
		(register-untagged-values vec 2)
		vec))

(defun allocate-byte-vector (size)
	(let ((vec (alloc-uvector (+ (bytes-cells size) 1) uvector-simple-byte-vector-tag)))
		(setf (uref vec 1) size)
		(register-untagged-values vec 2)
		vec))

(defun allocate-bit-vector (size)
	(let ((vec (alloc-uvector (+ (bits-cells size) 1) uvector-simple-bit-vector-tag)))
		(setf (uref vec 1) size)
		(register-untagged-values vec 2)
		vec))

(defun allocate-generic-vector (size)
	(let ((vec (alloc-uvector (+ size 1) uvector-simple-vector-tag)))
		(setf (uref vec 1) size)
		vec))

(defun allocate-single-float-vector (size)
	(let ((vec (alloc-uvector (+ size 1) uvector-simple-single-float-vector-tag)))
		(setf (uref vec 1) size)
		(register-untagged-values vec 2)
		vec))

(defun allocate-double-float-vector (size)
	(let ((vec (alloc-uvector (+ (+ size size) 1) uvector-simple-double-float-vector-tag)))
		(setf (uref vec 1) size)
		(register-untagged-values vec 2)
		vec))

(defun allocate-short-vector (size)
	(let ((vec (alloc-uvector (+ (char-cells size) 1) uvector-simple-short-vector-tag)))
		(setf (uref vec 1) size)
		(register-untagged-values vec 2)
		vec))
	 
;;;
;;;	Common Lisp MAKE-ARRAY function
;;;
(defun make-array (dimensions 
	&key (element-type t) 
		 (initial-element nil supplied-initial-element)
		 (initial-contents nil supplied-initial-contents)
		 adjustable 
		 fill-pointer 
		 (displaced-to nil) 
		 (displaced-index-offset 0))
	(if (integerp dimensions)
		(setq dimensions (list dimensions)))

	(unless (member element-type '(bit byte character))
		(setq element-type t))
	(let ((array-type 'simple)
		  (num-dimensions (length dimensions))
		  (header-size 0))
		(when (or adjustable fill-pointer (> num-dimensions 1))
			(setq array-type 'adjustable))
		(setq header-size (+ adjustable-array-header-min-size num-dimensions -1))

		(let* ((num-cells (apply #'* dimensions))
		 	  a
		  	 vec
		  	 (d dimensions))
			(cond
				((eq element-type 'character)
				 (setq vec (allocate-char-vector num-cells)))
				((eq element-type 'bit)
				 (setq vec (allocate-bit-vector num-cells)))
				((eq element-type 'byte)
				 (setq vec (allocate-byte-vector num-cells)))
				((eq element-type 't)
				 (setq vec (allocate-generic-vector num-cells))))
		
			(if (eq array-type 'adjustable)
				(let ()
					(setq a (alloc-uvector header-size uvector-array-tag))
					(setf (uref a adjustable-array-vector-offset) vec)
					(setf (uref a adjustable-array-fill-pointer-offset) -1)
					(setf (uref a adjustable-array-displaced-offset) displaced-index-offset)
					(setf (uref a adjustable-array-dimensions-offset) num-dimensions)
					(dotimes (i num-dimensions)
						(setf (uref a (+ adjustable-array-dim1-offset i)) (car d))
					(setq d (cdr d))))
				(setq a vec))
					 		
			(if supplied-initial-element
				(array-initialize-element a initial-element)
				(if supplied-initial-contents
					(array-initialize-contents a initial-contents)))

			(if fill-pointer
				(progn
					(if (/= num-dimensions 1)
						(error "Cannot declare a fill pointer in a ~A dimensional array"))
					(if (eq fill-pointer t) (setq fill-pointer num-cells))
					(if (or (< fill-pointer 0) (> fill-pointer num-cells))
						(error "Fill pointer out of range"))
					(setf (uref a adjustable-array-fill-pointer-offset) fill-pointer)))
			a)))

;;;
;;;	Common Lisp VECTOR-PUSH function.
;;;
(defun vector-push (new-element vector)
	(unless (array-has-fill-pointer-p vector) 
		(error "Vector does not have a fill pointer: ~A" vector))
	(let ((pos (fill-pointer vector)))
		(if (>= pos (array-dimension vector 0))
			(return-from vector-push nil))
		(setf (fill-pointer vector) (+ pos 1))
		(setf (elt vector pos) new-element)
		pos))

;;;
;;;	Common Lisp VECTOR-PUSH-EXTEND function.
;;; This gets reimplemented later.
;;;
(defun vector-push-extend (new-element vector &optional extension)
	(vector-push new-element vector))

;;;
;;;	Common Lisp ALPHANUMERICP function.
;;;
(defun alphanumericp (x) 
	(or (alpha-char-p x) (not (null (digit-char-p x)))))

;;;
;;;	Common Lisp STRING function.
;;; This gets redefined later to include single character coercion.
;;;
(defun string (x)
	(unless (stringp x)
		(if (symbolp x)
			(setq x (symbol-name x))
			(error "Not a string designator: ~A" x)))
	x)

;;;;
;;;;	Common Lisp STRING-UPCASE function.
;;;;
(defun string-upcase (s &key (start 0) end)
	(if (symbolp s)
		(setf s (string s)))
	(unless (stringp s)
		(error "Not a string: ~A" s))
	(unless end (setq end (length s)))
	(let ((copy (make-array (length s) :element-type 'character)))
		(dotimes (i (length s))
			(setf (elt copy i) 
				(if (and (>= i start) (< i end))
					(char-upcase (elt s i))
					(elt s i))))
		copy))

;;;;
;;;;	Common Lisp NSTRING-UPCASE function.
;;;;
(defun nstring-upcase (s &key (start 0) end)
	(unless (stringp s)
		(error "Not a string: ~A" s))
	(unless end (setq end (length s)))
	(do ((i start (+ i 1)))
		((= i end) s)
		(setf (elt s i) (char-upcase (elt s i)))))

;;;;
;;;;	Common Lisp STRING-DOWNCASE function.
;;;;
(defun string-downcase (s &key (start 0) end)
	(if (symbolp s)
		(setf s (string s)))
	(unless (stringp s)
		(error "Not a string: ~A" s))
	(unless end (setq end (length s)))
	(let ((copy (make-array (length s) :element-type 'character)))
		(dotimes (i (length s))
			(setf (elt copy i) 
				(if (and (>= i start) (< i end))
					(char-downcase (elt s i))
					(elt s i))))
		copy))

;;;;
;;;;	Common Lisp NSTRING-DOWNCASE function.
;;;;
(defun nstring-downcase (s &key (start 0) end)
	(unless (stringp s)
		(error "Not a string: ~A" s))
	(unless end (setq end (length s)))
	(do ((i start (+ i 1)))
		((= i end) s)
		(setf (elt s i) (char-downcase (elt s i)))))

;;;;
;;;;	Common Lisp STRING-CAPITALIZE function.
;;;;
(defun string-capitalize (s &key (start 0) end)
	(if (symbolp s)
		(setf s (string s)))
	(unless (stringp s)
		(error "Not a string: ~A" s))
	(unless end (setq end (length s)))
	(let ((copy (make-array (length s) :element-type 'character))
		  (new-word t)
		  c
		  term)
		(dotimes (i (length s))
			(setq c (elt s i))
			(setq term (not (alphanumericp c)))
			(setf (elt copy i)
				(if (and (>= i start) (< i end))
					(if new-word 
						(char-upcase c)
						(char-downcase c))
					c))
			(if term		;; is word terminator
				(setq new-word t)
				(setq new-word nil)))
		copy))

;;;;
;;;;	Common Lisp NSTRING-CAPITALIZE function.
;;;;
(defun nstring-capitalize (s &key (start 0) end)
	(unless (stringp s)
		(error "Not a string: ~A" s))
	(unless end (setq end (length s)))
	(let ((new-word t)
		  c
		  term)
		(dotimes (i (length s))
			(setq c (elt s i))
			(setq term (not (alphanumericp c)))
			(if (and (>= i start) (< i end))
				(setf (elt s i)
					(if new-word 
						(char-upcase c)
						(char-downcase c))))
			(if term		;; is word terminator
				(setq new-word t)
				(setq new-word nil)))
		s))

;
;	Common Lisp LENGTH function.
;
(defun length (x) 
	(if (vectorp x)
		(if (array-has-fill-pointer-p x) 
			(fill-pointer x) 
			(array-dimension x 0))
		(let ((length 0)) 
			(tagbody loop 
				(if (null x) (return-from length length)) 
				(setq x (cdr x))
				(setq length (+ 1 length))
				(go loop)))))

;
;	Common Lisp REMOVE function.
;
(defun remove (item sequence 
		&key from-end (test #'eql) test-not (start 0) end count key)
	(unless (sequencep sequence) 
		(error "Not a sequence: ~A" sequence))
	(unless (integerp end) 
		(setq end (length sequence)))
	(if test-not (setq test #'(lambda (x y) (not (funcall test-not x y)))))

	(let* ((p nil)
		   (length (length sequence))
		   element)
		(if (null count)
			(setq count length))

		(if from-end
			;; loop backward
			(do ((i (1- length) (- i 1))
				 test-element
				 (remove-it nil nil))
				((< i 0) nil)
				(setq element (elt sequence i))
				(if (and (>= i start) (< i end))
					(let ((test-element element))
						(if key (setq test-element (funcall key test-element)))
						(if (and (funcall test item test-element) (> count 0))
							(progn 
								(decf count)
								(setq remove-it t)))))
				(unless remove-it (push element p)))

			;; else loop forward
			(do ((i 0 (+ i 1))
				 test-element
				 (remove-it nil nil))
				((>= i length) nil)
				(setq element (elt sequence i))
				(if (and (>= i start) (< i end))
					(let ((test-element element))
						(if key (setq test-element (funcall key test-element)))
						(if (and (funcall test item test-element) (> count 0))
							(progn 
								(decf count)
								(setq remove-it t)))))
				(unless remove-it (push element p))))
		(setq p (nreverse p))
		(if (vectorp sequence)
			(make-array (length p) 
				:element-type (array-element-type sequence)
				:initial-contents p)
			p)))

;;;
;;;	Common Lisp MAKE-LIST function.
;;;
(defun make-list (size &key initial-element)
	(let ((list nil))
		(dotimes (i size)
			(push initial-element list))
		(nreverse list)))

#|  Roger's implementation, replaced by Vassili's below
;;;
;;;	Common Lisp MAPCAR function.
;;;
(defun mapcar (function list &rest more-lists)
	(let ((result nil))
		(if (null more-lists)	;; special case optimized for one list
			(dolist (x list)
				(push (funcall function x) result))
			(let ((lists (cons list more-lists)))
				(block outer-loop
					(do ((args nil nil))
						(nil nil)
						(do* ((a lists (cdr a)))
							((null a))
							(if (null (car a))(return-from outer-loop nil))
							(push (caar a) args)
							(setf (car a) (cdar a)))
						(push (apply function (nreverse args)) result)))))
		(nreverse result)))
|#

;;;; List mapping function family for Corman Lisp.
;;;;
;;;; Author: Vassili Bykov <vassili@objectpeople.com>
;;;; Date:   December 23, 1998

;; -- MAPCAR is in misc.lisp, the above two functions should go there too. --

;;; The two workhorse functions for the MAP* family.  One is for maps
;;; on many lists, the other is for maps on a single list.  Whether
;;; they map on elements or sublists, and how the results are combined
;;; is determined by LISTWISEP (NIL = map on elements) and COMBINE
;;; (NIL = do not combine, return nil; CONS = collect results into a
;;; list; NCONC = nconc application results).
;;;
;;; NB: %MAP-N assumes that a called function's &REST parameter cannot
;;; share structure with the argument to APPLY.  This is true with the
;;; current calling mechanism.

(defun %map-N (function lists listwisep combine)
  (do* ((result (cons nil nil))
	(last result)
	(args (make-list (length lists)))
	(x nil))
       (nil)
    (do ((tail lists (cdr tail))
	 (argcell args (cdr argcell)))
	((null tail))
      (unless (car tail)
	(return-from %map-N (if (null combine) nil (cdr result))))
      (rplaca argcell (if listwisep (car tail) (caar tail)))
      (rplaca tail (cdar tail)))
    (setq x (apply function args))
    (cond ((eq combine 'cons)
	   (rplacd last (cons x nil))
	   (setq last (cdr last)))
	  ((eq combine 'nconc)
	   (rplacd last x)
	   ;; The following is essentially (setq last (last last)) but
	   ;; LAST is too intelligent.  The loop is 20 times faster.
	   (setq last (do ((ls last (cdr ls)))
			  ((null (cdr ls)) ls)))))))

(defun %map-1 (function list listwisep combine)
  (let* ((result (cons nil nil))
         (last result)
         (x nil))
    (do ((tail list (cdr tail)))
	((null tail)
	 (if (null combine) nil (cdr result)))
      (setq x (funcall function (if listwisep tail (car tail))))
      (cond ((eq combine 'cons)
	     (rplacd last (cons x nil))
	     (setq last (cdr last)))
	    ((eq combine 'nconc)
	     (rplacd last x)
	     (setq last (do ((ls last (cdr ls)))
			    ((null (cdr ls)) ls))))))))

(defun mapcar (function list &rest more-lists)
  (if more-lists
      (%map-N function (cons list more-lists) nil 'cons)
      (%map-1 function list nil 'cons)))

;;; -- the rest goes into lists.lisp --

(defun mapcan (function list &rest more-lists)
  (if more-lists
      (%map-N function (cons list more-lists) nil 'nconc)
      (%map-1 function list nil 'nconc)))

(defun mapc (function list &rest more-lists)
  (if more-lists
      (%map-N function (cons list more-lists) nil nil)
      (%map-1 function list nil nil))
  list)

(defun maplist (function list &rest more-lists)
  (if more-lists
      (%map-N function (cons list more-lists) t 'cons)
      (%map-1 function list t 'cons)))

(defun mapcon (function list &rest more-lists)
  (if more-lists
      (%map-N function (cons list more-lists) t 'nconc)
      (%map-1 function list t 'nconc)))

(defun mapl (function list &rest more-lists)
  (if more-lists
      (%map-N function (cons list more-lists) t nil)
      (%map-1 function list t nil))
  list)
		 
;;;
;;;	Common Lisp FUNCTION-LAMBDA-EXPRESSION function.
;;;
(defun function-lambda-expression (func)
	(let ((info (function-info-list func)))
		(values (getf info 'lambda)
				(function-environment func)
				(getf info 'function-name))))

;;;;
;;;;	Common Lisp SPECIAL-OPERATOR-P function.
;;;;
(defun special-operator-p (symbol)
	(unless (symbolp symbol)
		(error "Not a symbol: ~A" symbol))
	(eq (uref symbol symbol-function-type-offset) 'special-operator))

;;;;
;;;;	Common Lisp MACRO-FUNCTION function.
;;;;
(defun macro-function (symbol)
	(unless (symbolp symbol)
		(error "Not a symbol: ~A" symbol))
	(let ((func (car (uref symbol symbol-function-offset))))
		(if (and (not (uninitialized-object-p func))
				 (eq  (uref symbol symbol-function-type-offset) 'macro))
			func)))

;;;;
;;;;	Common Lisp FBOUNDP function.
;;;;
(defun fboundp (function-specifier)
	(if (consp function-specifier)
		(setq function-specifier (get-setf-function (cadr function-specifier))))
	(unless (symbolp function-specifier)
		(error "Not a symbol: ~A" function-specifier))
	(or (not (uninitialized-object-p (car (uref function-specifier symbol-function-offset))))
		(eq (uref function-specifier symbol-function-type-offset) 'special-operator)))

;;;;
;;;;	Common Lisp VALUES-LIST function.
;;;;
(defun values-list (list) (apply #'values list))

;;;
;;;	Common Lisp COPY-TREE function.
;;;
(defun copy-tree (tree)
	(if (consp tree) 
		(cons (copy-tree (car tree)) (copy-tree (cdr tree))) 
		tree))

;;;
;;;	Like COPY-TREE, but does not copy quoted list structure.
;;;
(defun copy-tree-unquoted (tree)
	(if (consp tree)
		(if (eq (car tree) 'quote)
			(cons 'quote (cdr tree))
			(cons (copy-tree (car tree)) (copy-tree (cdr tree)))) 
		tree))

;;;;
;;;;	Common Lisp EVAL function.
;;;;
(defun eval (form)
    (if *evalhook*
        (let* ((hookfn *evalhook*)
               (*evalhook* nil))
            (funcall hookfn form nil))
        (cond
            ((and (listp form) (eq 'progn (first form)))
            ;; Make sure multiple values are returned correctly
             (do ((subforms (cdr form) (cdr subforms))) 
                 (())
                 (if (null (cdr subforms)) 
                        (return-from eval (eval (first subforms)))
                        (eval (first subforms)))))
            (t (let ((compiled-form (compile-form (copy-tree-unquoted form))))
                    (funcall compiled-form))))))

;;;;
;;;;	Common Lisp EVALHOOK function.
;;;;
(defun evalhook (form evalhookfn applyhookfn &optional env)
	(setq form (macroexpand-all form env))
	(if (and (consp form) (fboundp (car form)) (not (special-operator-p (car form))))
		(let ((arg-list nil))
			(let ((*evalhook* evalhookfn)
				  (*applyhook* applyhookfn))
				(setq arg-list (mapcar #'eval (cdr form))))
			(apply (car form) arg-list))
		(eval form)))

;;;;
;;;;	Common Lisp LAST function.
;;;;
(defun last (list &optional (n 1))
	(let* ((length (list-length list))
		   (skip (- length n)))
		(if (> skip 0)
			(dotimes (i skip)
				(setq list (cdr list))))
		list))

;;;;
;;;;	Common Lisp MINUSP function.
;;;;
(defun minusp (x) (< x 0))

;;;;
;;;;	Common Lisp BUTLAST function.
;;;;
(defun butlast (list &optional (n 1))
	(if (minusp n)
		(error "Integer must not be negative: ~A" n))	
	(let* ((conses (length list))
		   (result-conses (- conses n))
		   (result nil))
		(if (>= result-conses 0)
			(dotimes (i result-conses)
				(push (car list) result)
				(setq list (cdr list))))
		(nreverse result)))

;;;;
;;;;	Common Lisp NBUTLAST function.
;;;;
(defun nbutlast (list &optional (n 1))
	(if (minusp n)
		(error "Integer must not be negative: ~A" n))	
	(let* ((conses (length list))
		   (result-conses (- conses n))
		   (result list))
		(if (> result-conses 0)
			(progn
				(dotimes (i (- result-conses 1))
					(setq list (cdr list)))
				(setf (cdr list) nil)
				result)
			nil)))

;;;;
;;;;	Common Lisp (SETF NTH) function.
;;;;
(defun |(SETF NTH)| (value index list)
	(rplaca (nthcdr index list) value)
	value)
(register-setf-function 'nth '|(SETF NTH)|)

;;;
;;; Common Lisp MIN function.
;;;
(defun min (number &rest more-numbers)
	(dolist (x more-numbers)
		(if (< x number)
			(setq number x)))
	number)

;;;
;;; Common Lisp MAX function.
;;;
(defun max (number &rest more-numbers)
	(dolist (x more-numbers)
		(if (> x number)
			(setq number x)))
	number)

;;;
;;;	Common Lisp COMPLEMENT function.
;;;
(defun complement (function)
	#'(lambda (&rest arguments)
		(not (apply function arguments))))

;;;
;;;	Common Lisp CONSTANTLY function.
;;;
(defun constantly (value)
	#'(lambda (&rest arguments) 
		(declare (ignore arguments))
		value))

;;;
;;;	Common Lisp SIMPLE-STRING-P function.
;;;
(defun simple-string-p (string) (and (stringp string) (not (adjustable-array-p string))))

(defun |(SETF READTABLE-CASE)| (val rt)
	(unless (readtablep rt)
		(error "Place is not a readtable: ~A" rt))
	(unless (member val '(:upcase :downcase :preserve :invert))
		(error "Invalid case sensitivity mode: ~A" val))
	(uref-set val rt readtable-case-offset))
(register-setf-function 'readtable-case '|(SETF READTABLE-CASE)|)

;;;
;;;	Common Lisp NCONC function.
;;;
(defun nconc (&rest lists)
	(do* ((x lists (cdr x))
		  (list (car x)(car x))
		  (ret nil))
		((null x) ret)
		(unless (listp x) (error "Not a list: ~A" x))
		(if ret (rplacd (last ret) list) (setq ret list))
		ret))
 
    
