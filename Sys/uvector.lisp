;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		uvector.lisp
;;;;	Contents:	Functions to set/get uvector data.
;;;;	History:	8/1/96   RGC  Created.
;;;;				2/1/01   RGC  Added Frank Adrian's mod for GET.
;;;;				10/12/01 RGC  Corrected the behavior of GET-OUTPUT-STREAM-STRING.
;;;;

(defconstant uvector-function-tag		0)
(defconstant uvector-kfunction-tag		1)
(defconstant uvector-structure-tag		2)
(defconstant uvector-array-tag			3)
(defconstant uvector-symbol-tag			4)
(defconstant uvector-stream-tag			5)
(defconstant uvector-double-float-tag	6)
(defconstant uvector-package-tag		7)
(defconstant uvector-hashtable-tag		8)
(defconstant uvector-foreign-tag		9)
(defconstant uvector-compiled-code-tag	10)
(defconstant uvector-readtable-tag		11)
(defconstant uvector-complex-tag		12)
(defconstant uvector-ratio-tag			13)
(defconstant uvector-bignum-tag			14)
(defconstant uvector-foreign-heap-tag	15)
(defconstant uvector-weak-ptr-tag		16)
(defconstant uvector-simple-vector-tag	17)
(defconstant uvector-simple-char-vector-tag			18)
(defconstant uvector-simple-byte-vector-tag			19)
(defconstant uvector-simple-short-vector-tag		20)
(defconstant uvector-simple-double-float-vector-tag 21)
(defconstant uvector-simple-bit-vector-tag			22)
(defconstant uvector-simple-single-float-vector-tag	23)
(defconstant uvector-single-float-tag	24)
(defconstant uvector-clos-instance-tag	25)
(defconstant uvector-foreign-stack-tag	26)
(defconstant uvector-foreign-stack-end-tag	27)

(defconstant symbol-name-offset					1)
(defconstant symbol-value-offset				2)
(defconstant symbol-package-offset				3)
(defconstant symbol-plist-offset				4)
(defconstant symbol-constant-offset				5)
(defconstant symbol-function-type-offset		6)
(defconstant symbol-function-offset				7)
(defconstant symbol-jump-table-offset			8)
(defconstant symbol-var-table-offset			9)
(defconstant symbol-size            			9)
(defconstant symbol-constant-flag				1)
(defconstant symbol-special-flag				2)

(defconstant function-environment-offset		1)
(defconstant function-code-buffer-offset		2)
(defconstant function-size						3)

(defconstant compiled-code-references-offset	2)
(defconstant compiled-code-info-offset			3)
(defconstant compiled-code-code-offset 			4)

(defconstant foreign-heap-ptr-offset 			1)
(defconstant foreign-heap-length-offset 		2)
(defconstant weak-ptr-offset 					1)
(defconstant double-float-offset 				2)
(defconstant double-float-size	 				3)
(defconstant single-float-offset 				1)
(defconstant single-float-size 					1)
(defconstant bignum-num-cells-offset 			1)
(defconstant bignum-first-cell-offset 			2)

(defconstant clos-instance-class-offset			1)
(defconstant clos-instance-slots-offset			2)
(defconstant clos-instance-size					3)	;; includes 1 unused cell to make odd number

(defconstant foreign-stack-ptr-offset			1)
(defconstant foreign-stack-ptr-size				2)

(defun symbol-name (sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(uref sym symbol-name-offset)) 
(defun set-symbol-name (val sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(uref-set val sym symbol-name-offset)) 
(defun set-symbol-value (val sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(rplaca (uref sym symbol-value-offset) val) val) 
(defun symbol-package (sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(uref sym symbol-package-offset)) 
(defun set-symbol-package (val sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(uref-set val sym symbol-package-offset)) 
(defun symbol-plist (sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(uref sym symbol-plist-offset)) 
(defun set-symbol-plist (val sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(uref-set val sym symbol-plist-offset)) 
(defun symbol-constant-p (sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(/= 0 (logand (uref sym symbol-constant-offset) symbol-constant-flag)))
(defun symbol-special-p (sym) 
	(unless (symbolp sym) (signal-type-error sym 'symbol))
	(/= 0 (logand (uref sym symbol-constant-offset) symbol-special-flag)))

(defun get (sym attr &optional (default nil))
	(do* ((plist (symbol-plist sym) (cddr plist)))
		 ((null plist) default)
		 (if (eq (car plist) attr)
			(return (cadr plist)))))
		
(defconstant package-name-offset				1)
(defconstant package-nicknames-offset			2)
(defconstant package-use-list-offset			3)
(defconstant package-used-by-list-offset		4)
(defconstant package-shadowing-symbols-offset	5)
(defconstant package-capacity-offset			6)
(defconstant package-count-offset				7)
(defconstant package-table-offset				8)
(defconstant package-sync-offset				9)

(defun package-name (p) (uref p package-name-offset)) 
(defun package-nicknames (p) (uref p package-nicknames-offset)) 
(defun package-use-list (p) (uref p package-use-list-offset)) 
(defun package-used-by-list (p) (uref p package-used-by-list-offset)) 
(defun package-shadowing-symbols (p) (uref p package-shadowing-symbols-offset)) 
(defun package-capacity (p) (uref p package-capacity-offset)) 
(defun package-count (p) (uref p package-count-offset)) 
(defun package-table (p) (uref p package-table-offset)) 
(defun set-package-table (table p) (uref-set table p package-table-offset)) 

(defun set-package-count (count p) (uref-set count p package-count-offset)) 
(defun set-package-capacity (capacity p) (uref-set capacity p package-capacity-offset)) 
(defun package-sync (p) (uref p package-sync-offset)) 
(defun set-package-sync (sync-obj p) (uref-set sync-obj p package-sync-offset)) 

(defconstant stream-name-offset 				1)
(defconstant stream-underflow-func-offset 		2)
(defconstant stream-position-offset 			3)
(defconstant stream-col-position-offset 		4)
(defconstant stream-input-buffer-offset 		5)
(defconstant stream-input-buffer-length-offset 	6)
(defconstant stream-input-buffer-pos-offset 	7)
(defconstant stream-input-buffer-num-offset 	8)
(defconstant stream-handle-offset 				9)
(defconstant stream-subclass-offset 			10)
(defconstant stream-binary-offset 				11)
(defconstant stream-open-offset 				12)
(defconstant stream-direction-offset 			13)
(defconstant stream-interactive-offset 			14)
(defconstant stream-element-type-offset 		15)
(defconstant stream-associated-streams-offset	16)
(defconstant stream-overflow-func-offset		17)
(defconstant stream-output-buffer-offset 		18)
(defconstant stream-output-buffer-length-offset 19)
(defconstant stream-output-buffer-pos-offset 	20)
(defconstant stream-line-number-offset 			21)
(defconstant stream-size						21)

(defun make-string-output-stream ()
	(let* ((buf (alloc-char-vector 2048))
		   (s (alloc-uvector stream-size uvector-stream-tag)))
		(uref-set "output-string-stream" s stream-name-offset)
		(uref-set nil s stream-underflow-func-offset)
		(uref-set 'string-overflow-function s stream-overflow-func-offset)
		(uref-set 0 s stream-position-offset)
		(uref-set 0 s stream-col-position-offset)
		(uref-set 0 s stream-line-number-offset)
		(uref-set nil s stream-input-buffer-offset)
		(uref-set 0 s stream-input-buffer-length-offset)
		(uref-set 0 s stream-input-buffer-pos-offset)
		(uref-set 0 s stream-input-buffer-num-offset)
		(uref-set buf s stream-output-buffer-offset)
		(uref-set 2048 s stream-output-buffer-length-offset)
		(uref-set 0 s stream-output-buffer-pos-offset)
		(uref-set "" s stream-handle-offset)
		(uref-set 'string-stream s stream-subclass-offset)
		(uref-set nil s stream-binary-offset)
		(uref-set t s stream-open-offset)
		(uref-set (find-keyword "OUTPUT") s stream-direction-offset)
		(uref-set nil s stream-interactive-offset)
		(uref-set 'character s stream-element-type-offset)
		(uref-set nil s stream-associated-streams-offset)
	    s))

;;;
;;; Common Lisp GET-OUTPUT-STREAM-STRING function
;;;
(defun get-output-stream-string (s)
	(force-output s)
	(let ((ret (uref s stream-handle-offset)))
		(uref-set "" s stream-handle-offset)
		ret))

(defun stream-subclass (stream)
	(unless (streamp stream) (signal-type-error stream 'stream))
	(uref stream stream-subclass-offset))

(defun stream-column (stream)
	(unless (streamp stream) (signal-type-error stream 'stream))
	(uref stream stream-col-position-offset))
	 
;;;; returns the number of cells in an array	
(defun array-num-cells (array)
	(let ((dims (array-rank array))
		  (num 1))
		(dotimes (i dims)
			(setq num (* num (array-dimension array i))))
		num))

(defun char (string index) (elt string index))
(defun char= (&rest chars)
	(if (null chars)
		t
		(let ((c (car chars)))
			(dolist (ch (cdr chars)) 
				(if (not (eq c ch))
					(return-from char= nil)))
			t)))

;;;;
;;;;	Common Lisp RATIONALP function.
;;;;
(defun rationalp (x) (or (integerp x) (ratiop x)))

;;;;
;;;;	Common Lisp REALP function.
;;;;
(defun realp (x) (or (integerp x) (floatp x) (ratiop x)))

;;;;
;;;;	Common Lisp NUMBERP function.
;;;;
(defun numberp (x) (or (integerp x) (floatp x) (ratiop x) (complexp x)))

(defconstant ratio-numerator-offset 			1)
(defconstant ratio-denominator-offset 			2)

;;;;
;;;;	Common Lisp NUMERATOR function.
;;;;
(defun numerator (x) 
	(unless (rationalp x) (signal-type-error x 'rational))
	(if (ratiop x) (uref x ratio-numerator-offset) x))

;;;;
;;;;	Common Lisp DENOMINATOR function.
;;;;
(defun denominator (x) 
	(unless (rationalp x) (signal-type-error x 'rational))
	(if (ratiop x) (uref x ratio-denominator-offset) 1))


(defconstant complex-real-offset 				1)
(defconstant complex-imaginary-offset 			2)

;;;;
;;;;	Common Lisp REALPART function.
;;;;
(defun realpart (x)
	(unless (numberp x) (signal-type-error x 'number)) 
	(if (complexp x) (uref x complex-real-offset) x))

;;;;
;;;;	Common Lisp IMAGPART function.
;;;;
(defun imagpart (x) 
	(unless (numberp x) (signal-type-error x 'number))
	(if (complexp x) (uref x complex-imaginary-offset) (* 0 x)))

;;;
;;;	Corman Lisp FUNCTION-REFERENCES function.
;;;
(defun function-references (func) 
	(uref (uref func function-code-buffer-offset) 
		compiled-code-references-offset))

(defun simple-vector-p (x) 
	(and (uvectorp x)(eq (uvector-type-bits x) uvector-simple-vector-tag)))

(defun simple-char-vector-p (x) 
	(and (uvectorp x)(eq (uvector-type-bits x) uvector-simple-char-vector-tag)))

(defun simple-byte-vector-p (x) 
	(and (uvectorp x)(eq (uvector-type-bits x) uvector-simple-byte-vector-tag)))

(defun simple-short-vector-p (x) 
	(and (uvectorp x)(eq (uvector-type-bits x) uvector-simple-short-vector-tag)))

(defun simple-double-float-vector-p (x) 
	(and (uvectorp x)(eq (uvector-type-bits x) uvector-simple-double-float-vector-tag)))

(defun simple-bit-vector-p (x) 
	(and (uvectorp x)(eq (uvector-type-bits x) uvector-simple-bit-vector-tag)))

(defun simple-single-float-vector-p (x) 
	(and (uvectorp x)(eq (uvector-type-bits x) uvector-simple-single-float-vector-tag)))

(defun alloc-double-float () (alloc-uvector double-float-size uvector-double-float-tag))
(defun alloc-single-float () (alloc-uvector single-float-size uvector-single-float-tag))
(defun alloc-bignum (size) 	 
	(let ((bn (alloc-uvector (+ size 1) uvector-bignum-tag)))
		(uref-set (* size 2) bn 1)
		bn))

;; put minimal support for CLOS instances here, the rest will be added in clos.lisp
(defun alloc-clos-instance () (alloc-uvector clos-instance-size uvector-clos-instance-tag))
(defun clos-instance-class (i) (uref i clos-instance-class-offset))
(defun clos-instance-slots (i) (uref i clos-instance-slots-offset))
(defun clos-instance-p (i)
	(and (uvectorp i)(eq (uvector-type-bits i) uvector-clos-instance-tag)))

(defun foreign-stack-p (x)
	(and (uvectorp x)(eq (uvector-type-bits x) uvector-foreign-stack-tag)))

;;;
;;;	Common Lisp CONSTANTP function.
;;;
(defun constantp (x &optional environment)
	;(declare (ignore environment))
	(cond ((symbolp x) (symbol-constant-p x))
		  ((consp x) (eq (car x) 'quote))
		  (t t)))

