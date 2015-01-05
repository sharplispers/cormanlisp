;;;;
;;;;	File:       ansi-chapter-4.lisp
;;;;   Contents:   Examples from the Hyperspec
;;;;

;;; TYPES AND CLASSES

(dotests COERCE
	 (coerce '(a b c) 'vector) =>  #(A B C)
	 (coerce 'a 'character) =>  #\A
	 (coerce 4.56 'complex) =>  #C(4.56 0.0)
	 (coerce 4.5s0 'complex) =>  #C(4.5s0 0.0s0)
	 (coerce 7/2 'complex) =>  7/2
	 (coerce 0 'short-float) =>  0.0s0
	 (coerce 3.5L0 'float) =>  3.5L0
	 (coerce 7/2 'float) =>  3.5
	 (coerce (cons 1 2) t) =>  (1 . 2)
	
	;;All the following forms should signal an error: 
	
	 (ignore-errors (coerce '(a b c) '(vector * 4))) => (values NIL true)
	 (ignore-errors (coerce #(a b c) '(vector * 4))) => (values NIL true)
	 (ignore-errors (coerce '(a b c) '(vector * 2))) => (values NIL true)
	 (ignore-errors (coerce #(a b c) '(vector * 2))) => (values NIL true)
	 (ignore-errors (coerce "foo" '(string 2))) => (values NIL true)
	 (ignore-errors (coerce #(#\a #\b #\c) '(string 2))) => (values NIL true)
	 (ignore-errors (coerce '(0 1) '(simple-bit-vector 3))) => (values NIL true)
)

(dotests DEFTYPE
	 (defun equidimensional (a)
	   (or (< (array-rank a) 2)
	       (apply #'= (array-dimensions a)))) =>  EQUIDIMENSIONAL
	 (deftype square-matrix (&optional type size)
	   `(and (array ,type (,size ,size))
	         (satisfies equidimensional))) =>  SQUARE-MATRIX
)

(dotests SUBTYPEP
	 (subtypep 'compiled-function 'function) =>  (values true true)
	 (subtypep 'null 'list) =>  (values true true)
	 (subtypep 'null 'symbol) =>  (values true true)
	 (subtypep 'integer 'string) =>  (values false true)
	 (subtypep '(satisfies dummy) nil) =>  false
	 (subtypep '(integer 1 3) '(integer 1 4)) =>  (values true true)
	 (subtypep '(integer (0) (0)) 'nil) =>  (values true true)
	 (subtypep 'nil '(integer (0) (0))) =>  (values true true)
	 (subtypep '(integer (0) (0)) '(member)) =>  implementation-dependent
	 (subtypep '(member) 'nil) =>  implementation-dependent
	 (subtypep 'nil '(member)) =>  implementation-dependent
)

(dotests TYPE-OF
	 (type-of 'a) =>  SYMBOL          
	 (type-of '(1 . 2)) =>  CONS ;; OR=>  (CONS FIXNUM FIXNUM)
	 (type-of #c(0 1))  =>  COMPLEX ;; OR=>  (COMPLEX INTEGER)
	 (defstruct temp-struct x y z) =>  TEMP-STRUCT
	 (type-of (make-temp-struct)) =>  TEMP-STRUCT
	 (type-of "abc") =>  STRING ;;OR=>  (STRING 3)
	 (subtypep (type-of "abc") 'string) =>  (values true true)
	 (type-of (expt 2 40)) =>  BIGNUM ;OR=>  INTEGER 
	 								  ;OR=>  (INTEGER 1099511627776 1099511627776) 
									  ;OR=>  SYSTEM::TWO-WORD-BIGNUM
									  ;OR=>  FIXNUM
	 (subtypep (type-of 112312) 'integer) =>  (values true true)
	 (defvar *foo* (make-array 5 :element-type t)) =>  *FOO*
	 (class-name (class-of *foo*)) =>  VECTOR
	 (type-of *foo*) =>  VECTOR		;;OR=>  (VECTOR T 5)
)

(dotests TYPEP
	 (typep 12 'integer) =>  true
	 (typep (1+ most-positive-fixnum) 'fixnum) =>  false
	 (typep nil t) =>  true
	 (typep nil nil) =>  false
	 (typep 1 '(mod 2)) =>  true
	 (typep #c(1 1) '(complex (eql 1))) =>  true
	;; To understand this next example, you might need to refer to
	;; Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals).
	 (typep #c(0 0) '(complex (eql 0))) =>  false
)

(dotests TYPE-ERROR-DATUM/TYPE-ERROR-EXPECTED-TYPE
	 (defun fix-digits (condition)
	   (check-type condition type-error)
	   (let* ((digits '(zero one two three four
	                   five six seven eight nine))
	         (val (position (type-error-datum condition) digits)))
	     (if (and val (subtypep 'fixnum (type-error-expected-type condition)))
	         (store-value 7)))) => FIX-DIGITS
	 
	 (defun foo (x)
	   (handler-bind ((type-error #'fix-digits))
	     (check-type x number)
	     (+ x 3))) => FOO
	 
	 (foo 'seven)
	=>  10
)
	