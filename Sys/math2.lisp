;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		math2.lisp
;;;;	Contents:	Corman Lisp math functions.
;;;;	History:	4/1/98  RGC  Created.
;;;;				4/25/01 RGC  Added LOGBITP, LOGCOUNT, LDB, (SETF LDB), EXPT, LDB-TEST
;;;;				8/15/01 RGC  Fixed MOD function.
;;;;				5/29/02 RGC	 Added Mayer Goldberg's complex trig function implementation.
;;;;							 Reimplemented trig functions to incorporate these.
;;;;                12/19/02 RGC Incorporated JP Massar's fix to %chars-to-float.
;;;;                11/03/03 RGC Fixed a problem with %chars-to-float implementation.
;;;;

(in-package "C-TYPES")

;;(defctype double :double-float)
;;(defctype float  :single-float)

(in-package "COMMON-LISP")
 
;;;
;;;	Common Lisp LOG function
;;;
;(defconstant e (exp 1))

(ct:defun-dll c-log ((d :double-float))
   :return-type :double-float
   :library-name "msvcrt.dll"
   :entry-name "log"
   :linkage-type :c)

(ct:defun-dll c-sin ((d :double-float))
   :return-type :double-float
   :library-name "msvcrt.dll"
   :entry-name "sin"
   :linkage-type :c)

(ct:defun-dll c-cos ((d :double-float))
   :return-type :double-float
   :library-name "msvcrt.dll"
   :entry-name "cos"
   :linkage-type :c)

(ct:defun-dll c-atan2 ((d1 :double-float)(d2 :double-float))
   :return-type :double-float
   :library-name "msvcrt.dll"
   :entry-name "atan2"
   :linkage-type :c)

(ct:defun-dll c-atan ((d1 :double-float))
   :return-type :double-float
   :library-name "msvcrt.dll"
   :entry-name "atan"
   :linkage-type :c)

(ct:defun-dll c-pow ((x :double-float)(y :double-float))
   :return-type :double-float
   :library-name "msvcrt.dll"
   :entry-name "pow"
   :linkage-type :c)

(ct:defun-dll c-exp ((x :double-float))
   :return-type :double-float
   :library-name "msvcrt.dll"
   :entry-name "exp"
   :linkage-type :c)

(ct:defun-dll c-atof ((d (:char *)))
   :return-type :double-float
   :library-name "msvcrt.dll"
   :entry-name "atof"
   :linkage-type :c)

;;;
;;;	Common Lisp LOG function.
;;;
(defun log (number &optional base)
	(if base 
		(if (zerop base)
			0
			(/ (log number) (log base)))
		(if (or (complexp number)(minusp number))
			(complex (c-log (float (abs number) 0d0)) (phase number))
			(c-log (float number 0d0)))))

;;;
;;;	Common Lisp ATAN function.
;;; arctan x = -i log  ((1+ix) sqrt(1/(1+x^2)) )
;;; 
(defun atan (x &optional y)
	(if y
		(c-atan2 (float x 0d0)(float y 0d0))
		(if (complexp x)
			(let* ((i #C(0 1))
				   (ix (* i x)))
				(* (- i)
					(log (* (+ 1 ix) (sqrt (/ 1 (+ 1 (* x x))))))))
			(c-atan (float x 0d0)))))

;;;;
;;;; Common Lisp LOGBITP function
;;;;
(defun logbitp (index integer)
	(unless (integerp integer)
		(error 'type-error :datum integer :expected-type 'integer))
	(unless (and (fixnump index) (>= index 0))
		(error 'type-error :datum index :expected-type '(integer 0 *)))
	(> (logand integer (expt 2 index)) 0))

;;;;
;;;; Common Lisp LOGCOUNT function
;;;;
(defun logcount (integer)
	(unless (integerp integer)
		(error 'type-error :datum integer :expected-type 'integer))
	;; if negative, use two's complement to flip
	(do ((x (if (< integer 0) (- (+ integer 1)) integer)(ash x -1))
		 (count 0 (+ count (logand x 1))))
		((= x 0) count)))

;;;;
;;;; Common Lisp LDB function
;;;;
(defun ldb (bytespec integer)
	(let ((mask (- (ash 1 (byte-size bytespec)) 1)))
		(logand (ash integer (- (byte-position bytespec))) mask)))

;;;;
;;;; Common Lisp (SETF LDB) function
;;;;
(defmacro |(SETF LDB)| (new-byte bytespec place)
	(if (and (consp place) (some #'consp place))
		(let ((retval (%once-only-forms place))
			  (sym (gensym)))
			`(let ,(car retval)
				(let ((,sym ,new-byte)) 
					(setf ,(cdr retval) (dpb ,sym ,bytespec ,(cdr retval)))
					,sym)))
		(let ((sym (gensym)))
			`(let ((,sym ,new-byte))
				(setf ,place (dpb ,sym ,bytespec ,place))
				,sym))))

(register-setf-function 'ldb '|(SETF LDB)|)


;;;;
;;;; Common Lisp LDB-TEST function
;;;;
(defun ldb-test (bytespec integer)
	(/= (ldb bytespec integer) 0))

(defun complex-expt (base power)
	(let* ((rbase (float (realpart base) 0d0))
		   (ibase (float (imagpart base) 0d0))
		   (rpower (float (realpart power) 0d0))
		   (ipower (float (imagpart power) 0d0))
		   abs)
		(setf abs
			(cond ((= rbase 0) (- (abs ibase)))
				  ((= ibase 0) (abs rbase))
				  ((> rbase ibase)
				   (* rbase (sqrt (+ 1 (c-pow (/ ibase rbase) 2d0)))))
				  (t 
				   (* ibase (sqrt (+ 1 (c-pow (/ rbase ibase) 2d0)))))))
		(let* ((rlog (c-log abs))
			   (ilog (c-atan2 ibase rbase))
			   (rphase (c-exp (- (* rlog rpower) (* ilog ipower))))
			   (iphase (+ (* rlog ipower) (* ilog rpower))))
			(complex (* rphase (cos iphase)) 
				     (* rphase (sin iphase))))))

;;;;
;;;; Common Lisp EXPT function
;;;;
(defun expt (base power)
	(unless (numberp base)
		(error 'type-error :datum base :expected-type 'number))
	(unless (numberp power)
		(error 'type-error :datum power :expected-type 'number))
	(if (and (integerp power)
			(or (rationalp base)
				(and (complexp base) 
					(rationalp (realpart base))
					(rationalp (imagpart base)))))
		;; calculate exact result
		(if (< power 0)
			(do ((result 1)
				 (p (- power)))
				((= p 0) (/ 1 result))
				(if (oddp p)
					(setf result (* result base)))
				(setf p (ash p -1))
				(setf base (* base base)))
			(do ((result 1))
				((= power 0) result)
				(if (oddp power)
					(setf result (* result base)))
				(setf power (ash power -1))
				(setf base (* base base))))
		
		;; approximate result
		(if (or (complexp base) 
				(complexp power)
				(and (< base 0) (not (integerp power))))
			(complex-expt base power)
			(c-pow (float base 0d0) (float power 0d0)))))

;;;
;;;	Both arguments should be fixnums.
;;;
(x86:defasm pos-fixnum-logbitp (index n)
	{
		push	ebp
		mov		ebp, esp
		mov		ecx, [ebp + (+ ARGS_OFFSET 4)]	;; ecx = index
		cmp		ecx, 232						;; 29, tagged
		jg		short :false
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = num
		mov		eax, 8
		shr		ecx, 3
		shl		eax, cl							;; eax has mask
		and		eax, edx
		jne		short :true
	:false
		mov		eax, [esi]
		jmp		short :t1
	:true
		mov		eax, [esi + t-offset]
	:t1
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	First argument should be fixnum, the second a bignum.
;;;
(x86:defasm pos-bignum-logbitp (index n)
	{
		push	ebp
		mov		ebp, esp
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = index
		mov		ecx, edx
		shr		edx, 8							;; edx = word offset (untagged)
		shl		edx, 4							;; edx = word offset (tagged) * 2
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = num
   		mov		eax, [eax + (uvector-offset cl::bignum-num-cells-offset)]
		cmp		edx, eax
		jge		short :false 
		shr		edx, 2							;; edx = word offset (untagged) * 4
		and		ecx, #xff
		shr		ecx, 3							;; ecx = index (untagged)
		mov		ebx, 1
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = num
		begin-atomic
		mov		eax, [eax + edx + (uvector-offset cl::bignum-first-cell-offset)]
		shl		ebx, cl							;; ebx = bit mask
		and		eax, ebx		
		jne		short :true
	:false
		mov		eax, [esi]
		jmp		short :t1
	:true
		mov		eax, [esi + t-offset]
	:t1
		end-atomic
		mov		ecx, 1
		pop		ebp
		ret
    
	})

(defun logbitp (index integer)
	(unless (integerp integer)
		(error 'type-error :datum integer :expected-type 'integer))
	(unless (and (fixnump index) (>= index 0))
		(error 'type-error :datum index :expected-type '(integer 0 *)))
	(if (fixnump integer)
		(if (< integer 0)
			(not (pos-fixnum-logbitp index (+ 1 (lognot integer))))	;; use twos complement
			(pos-fixnum-logbitp index integer))
		(if (< integer 0)
			(not (pos-bignum-logbitp index (+ 1 (lognot integer))))	;; use twos complement
			(pos-bignum-logbitp index integer))))

;;;
;;; Fixnum MOD
;;;
(x86:defasm mod-fixnums (fixnum divisor)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]	;; eax = fixnum
		xor		edx, edx
		begin-atomic
		div		[ebp + ARGS_OFFSET]
		mov		eax, edx
		end-atomic
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	Common Lisp MOD function.
;;;
(defun mod (number divisor)
	(if (and (integerp number) (integerp divisor))
		(if (or (bignump number)(bignump divisor))
			(if (>= number 0)
				(if (>= divisor 0)
					(mod-bignums number divisor)
					(- (mod-bignums number (- divisor)) (- divisor)))
				(if (>= divisor 0)
					(let ((result  (mod-bignums (- number) divisor)))      
						(if (eq result 0) 0 (- divisor result)))
					(- (mod-bignums (- number) (- divisor)))))
			(if (>= number 0)
				(if (>= divisor 0)
					(mod-fixnums number divisor)
					(- (mod-fixnums number (- divisor)) (- divisor)))
				(if (>= divisor 0)
					(let ((result  (mod-fixnums (- number) divisor)))      
						(if (eq result 0) 0 (- divisor result)))
					(- (mod-fixnums (- number) (- divisor))))))
		(multiple-value-bind (value remainder)
			(floor number divisor)
			(declare (ignore value))
			remainder)))

;; this overrides the kernel routine
(defun %chars-to-float (chars)
	(let ((default-format *read-default-float-format*)
		  (c chars)
		  (digits 0)
		  (precision)
		  (exp-digits 0)
		  (decimal 0)
          (exponent-index nil)
          (index 0))
		(unless (member default-format '(short-float double-float single-float long-float))
			(setf default-format 'single-float))
        (setf precision default-format)
		(unless (listp chars) (error "Expected a list of characters"))
				
		;; skip +/- if present
		(when (or (char= (car c) #\+) (char= (car c) #\-))
			(setf c (cdr c))
            (incf index))
		
		;; check mantissa
		(do ((char (car c)(car c)))
			((null c))
			(if (digit-char-p char)
				(incf digits)
				(if (char= char #\.)
					(if (> (incf decimal) 1)
						(return-from %chars-to-float nil))	;; more than one decimal point!			
					(return)))
			(setf c (cdr c))
            (incf index))
		
		(if (= digits 0)
			(return-from %chars-to-float nil))
		
		;; get exponent
		(if c
			(let ((char (char-upcase (car c))))
				(if (member char '(#\E #\F #\L #\S #\D))
					(progn
						(setf precision
							(cond
								((char= char #\E) default-format)
								((char= char #\D) 'double-float)
								((char= char #\S) 'short-float)
								((char= char #\F) 'single-float)
								((char= char #\L) 'double-float)))
		                (when (null (cdr c)) (return-from %chars-to-float nil))
   						(setf exponent-index index)
						(setf c (cdr c))
                        (incf index)
						
						;; allow +/- on exponent
						(when (or (char= (car c) #\+) (char= (car c) #\-))
							(setf c (cdr c))
                            (incf index))
					
						;; check exponent digits
						(do ((char (car c)(car c)))
							((or (null c)(not (digit-char-p char))))
							(if (digit-char-p char)
								(incf exp-digits))
							(setf c (cdr c))
                            (incf index))
						(if (= exp-digits 0)
							(return-from %chars-to-float nil))))))
		
		(unless (null c)
			(return-from %chars-to-float nil))	;; extra chars at end
		
        (let ((str (concatenate 'string chars)))
            (if exponent-index
                (setf (elt str exponent-index) #\E))
    		(let ((d (c-atof (ct:lisp-string-to-c-string str))))
    			(case precision
    				(double-float d)
    				(single-float (float d 0f0))
    				(short-float (float d 0s0))
                    (long-float d))))))

;;; complex-trig.lisp
;;; Programmer: Mayer Goldberg, 2002

(defun complex-sin (x)
  (let* ((eix (exp (* #c(0.0 1.0) x)))
	 (e-ix (/ 1.0 eix)))
    (/ (- eix e-ix) (* #c(0.0 1.0) 2.0))))

(defun complex-cos (x)
  (let* ((eix (exp (* #c(0.0 1.0) x)))
	 (e-ix (/ 1.0 eix)))
    (/ (+ eix e-ix) 2.0)))

(defun complex-tan (x)
  (let* ((eix (exp (* #c(0.0 1.0) x)))
	 (e-ix (/ 1.0 eix)))
    (/ (- eix e-ix) (+ eix e-ix) #c(0.0 1.0))))

#| not used
(defun complex-asin (x)
  (- (* #c(0.0 1.0)
	(log (+ (* #c(0.0 1.0) x)
		(sqrt (- 1.0 (* x x))))))))

(defun complex-acos (x) (- (/ pi 2.0) (complex-asin x)))

(defun complex-atan (y)
	(/ (- (log (+ 1.0 (* #c(0 1) y)))
	      (log (- 1.0 (* #c(0 1) y))))
	   (* 2 #c(0 1))))
|#

;;;
;;; Common Lisp SIN function
;;;
(defun sin (x)
	(if (complexp x)
		(complex-sin x)
		(c-sin (float x 0d0))))

;;;
;;; Common Lisp COS function
;;;
(defun cos (x)
	(if (complexp x)
		(complex-cos x)
		(c-cos (float x 0d0))))

;;;
;;;	Common Lisp TAN function.
;;;
(defun tan (x)
	(if (complexp x)
		(complex-tan x)
		(/ (sin x) (cos x))))

(in-package :sys)
(export '(D- DD- D+ DD+ D* DD* D/ DD/ 
        DSIN DDSIN DCOS DDCOS DSQRT DDSQRT 
        DV- DV+ DV* DV/ DVSIN DVCOS DVSQRT
        DV= DV/= DV< DV<= DV> DV>= DV-COPY))

;;;
;;; This function is optimized to assume two double-floats were passed.
;;; It does no type-checking.
;;;
(x86:defasm sys:d- (x y)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     1
        fpu-subtract-double 0
        fpu-retrieve-double
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

;; Like dd-, but expects the third argument to be a simple vector of type
;; double-float, with at least one element. The first element is overwritten
;; with the resulting value.
;; This method is fast because no memory allocation or garbage collection is
;; involved.
;;
(x86:defasm sys:dd- (x y result)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     2
        fpu-subtract-double 1
        fpu-store-double    0
        mov     eax, [esi]
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:d+ (x y)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     1
        fpu-add-double 0
        fpu-retrieve-double
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:dd+ (x y result)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     2
        fpu-add-double      1
        fpu-store-double    0
        mov     eax, [esi]
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:d* (x y)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     1
        fpu-multiply-double 0
        fpu-retrieve-double
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:dd* (x y result)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     2
        fpu-multiply-double 1
        fpu-store-double    0
        mov     eax, [esi]
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:d/ (x y)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     1
        fpu-divide-double   0
        fpu-retrieve-double
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:dd/ (x y result)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     2
        fpu-divide-double   1
        fpu-store-double    0
        mov     eax, [esi]
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:dsin (x)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     0
        fsin
        fpu-retrieve-double
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:ddsin (x result)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     1
        fsin
        fpu-store-double    0
        mov     eax, [esi]
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:dcos (x)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     0
        fcos
        fpu-retrieve-double
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:ddcos (x result)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     1
        fcos
        fpu-store-double    0
        mov     eax, [esi]
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:dsqrt (x)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     0
        fsqrt
        fpu-retrieve-double
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys:ddsqrt (x result)
	{
		push	ebp
		mov		ebp, esp
        fpu-load-double     1
        fsqrt
        fpu-store-double    0
        mov     eax, [esi]
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm dv- (x x-index y y-index result result-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 5 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 4 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = y-index
        fsub    [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = result
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = result-index 
        fstp    [eax + ecx + (uvector-offset 2)]
        mov     ecx, 1
        mov     eax, [esi]          ;; return NIL, since we don't return a useful value           
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm dv+ (x x-index y y-index result result-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 5 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 4 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = y-index
        fadd    [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = result
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = result-index 
        fstp    [eax + ecx + (uvector-offset 2)]
        mov     ecx, 1
        mov     eax, [esi]          ;; return NIL, since we don't return a useful value           
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm dv* (x x-index y y-index result result-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 5 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 4 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = y-index
        fmul    [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = result
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = result-index 
        fstp    [eax + ecx + (uvector-offset 2)]
        mov     ecx, 1
        mov     eax, [esi]          ;; return NIL, since we don't return a useful value           
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm dv/ (x x-index y y-index result result-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 5 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 4 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = y-index
        fdiv    [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = result
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = result-index 
        fstp    [eax + ecx + (uvector-offset 2)]
        mov     ecx, 1
        mov     eax, [esi]          ;; return NIL, since we don't return a useful value           
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm dvsin (x x-index result result-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]
        fsin
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = result
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = result-index 
        fstp    [eax + ecx + (uvector-offset 2)]
        mov     ecx, 1
        mov     eax, [esi]          ;; return NIL, since we don't return a useful value           
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm dvcos (x x-index result result-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]
        fcos
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = result
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = result-index 
        fstp    [eax + ecx + (uvector-offset 2)]
        mov     ecx, 1
        mov     eax, [esi]          ;; return NIL, since we don't return a useful value           
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm dvsqrt (x x-index result result-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]
        fsqrt
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = result
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = result-index 
        fstp    [eax + ecx + (uvector-offset 2)]
        mov     ecx, 1
        mov     eax, [esi]          ;; return NIL, since we don't return a useful value           
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys::dv= (x x-index y y-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]      ;; push x
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = y-index
        fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
        mov     eax, [esi]                          ;; eax = NIL
        jmp     short :done
    :equal
        mov     eax, [esi + 4]                      ;; eax = T 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })   

(x86:defasm sys::dv/= (x x-index y y-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]      ;; push x
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = y-index
        fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
        mov     eax, [esi + 4]                        ;; eax = T
        jmp     short :done
    :equal
        mov     eax, [esi]                            ;; eax = NIL 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys::dv< (x x-index y y-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]      ;; push x
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = y-index
        fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
    :not-equal
        test    eax, #x0100
        jne     short :less
    :greater
        mov     eax, [esi]                            ;; eax = NIL 
        jmp     short :done
    :less
        mov     eax, [esi + 4]                        ;; eax = T
        jmp     short :done
    :equal
        mov     eax, [esi]                            ;; eax = NIL 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys::dv> (x x-index y y-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]      ;; push x
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = y-index
        fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
    :not-equal
        test    eax, #x0100
        jne     short :less
    :greater
        mov     eax, [esi + 4]                        ;; eax = T 
        jmp     short :done
    :less
        mov     eax, [esi]                            ;; eax = NIL
        jmp     short :done
    :equal
        mov     eax, [esi]                            ;; eax = NIL 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys::dv<= (x x-index y y-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]      ;; push x
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = y-index
        fcomp    [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
    :not-equal
        test    eax, #x0100
        jne     short :less
    :greater
        mov     eax, [esi]                            ;; eax = NIL 
        jmp     short :done
    :less
        mov     eax, [esi + 4]                        ;; eax = T
        jmp     short :done
    :equal
        mov     eax, [esi + 4]                        ;; eax = T 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys::dv>= (x x-index y y-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]      ;; push x
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = y
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = y-index
        fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
    :not-equal
        test    eax, #x0100
        jne     short :less
    :greater
        mov     eax, [esi + 4]                        ;; eax = T 
        jmp     short :done
    :less
        mov     eax, [esi]                            ;; eax = NIL
        jmp     short :done
    :equal
        mov     eax, [esi + 4]                        ;; eax = T 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86:defasm sys::dv-copy (x x-index result result-index)
	{
		push	ebp
		mov		ebp, esp
        mov     eax, [ebp + (+ (* 3 4) ARGS_OFFSET)]  ;; eax = x
        mov     ecx, [ebp + (+ (* 2 4) ARGS_OFFSET)]  ;; ecx = x-index
        fld     [eax + ecx + (uvector-offset 2)]
        mov     eax, [ebp + (+ (* 1 4) ARGS_OFFSET)]  ;; eax = result
        mov     ecx, [ebp + (+ (* 0 4) ARGS_OFFSET)]  ;; ecx = result-index 
        fstp    [eax + ecx + (uvector-offset 2)]
        mov     ecx, 1
        mov     eax, [esi]          ;; return NIL, since we don't return a useful value           
        mov     esp, ebp
        pop     ebp
        ret
    })

(x86::defcodegen sys:dv- (form dest) ;; (x x-index y y-index result result-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fsub    [eax + ecx + (uvector-offset 2)] ; subtract y
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (seventh form) :dest-stack t)
    (cl::compile-sub-form (sixth form) :dest-eax-operand t)  
    (x86::parse-assembler 
        { 
            pop     ecx
            fstp    [eax + ecx + (uvector-offset 2)]  ; pop result  
            mov     eax, [esi]
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv+ (form dest) ;; (x x-index y y-index result result-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fadd    [eax + ecx + (uvector-offset 2)] ; add y
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (seventh form) :dest-stack t)
    (cl::compile-sub-form (sixth form) :dest-eax-operand t)  
    (x86::parse-assembler 
        { 
            pop     ecx
            fstp    [eax + ecx + (uvector-offset 2)]  ; pop result  
            mov     eax, [esi]
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv* (form dest) ;; (x x-index y y-index result result-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fmul    [eax + ecx + (uvector-offset 2)] ; mul y
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (seventh form) :dest-stack t)
    (cl::compile-sub-form (sixth form) :dest-eax-operand t)  
    (x86::parse-assembler 
        { 
            pop     ecx
            fstp    [eax + ecx + (uvector-offset 2)]  ; pop result  
            mov     eax, [esi]
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv/ (form dest) ;; (x x-index y y-index result result-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fdiv    [eax + ecx + (uvector-offset 2)] ; div y
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (seventh form) :dest-stack t)
    (cl::compile-sub-form (sixth form) :dest-eax-operand t)  
    (x86::parse-assembler 
        { 
            pop     ecx
            fstp    [eax + ecx + (uvector-offset 2)]  ; pop result  
            mov     eax, [esi]
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dvsin (form dest) ;; (x x-index result result-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (x86::parse-assembler 
        { 
            fsin
        })
    (cl::compile-sub-form (seventh form) :dest-stack t)
    (cl::compile-sub-form (sixth form) :dest-eax-operand t)  
    (x86::parse-assembler 
        { 
            pop     ecx
            fstp    [eax + ecx + (uvector-offset 2)]  ; pop result  
            mov     eax, [esi]
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dvcos (form dest) ;; (x x-index result result-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (x86::parse-assembler 
        { 
            fcos
        })
    (cl::compile-sub-form (seventh form) :dest-stack t)
    (cl::compile-sub-form (sixth form) :dest-eax-operand t)  
    (x86::parse-assembler 
        { 
            pop     ecx
            fstp    [eax + ecx + (uvector-offset 2)]  ; pop result  
            mov     eax, [esi]
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dvsqrt (form dest) ;; (x x-index result result-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (x86::parse-assembler 
        { 
            fsqrt
        })
    (cl::compile-sub-form (seventh form) :dest-stack t)
    (cl::compile-sub-form (sixth form) :dest-eax-operand t)  
    (x86::parse-assembler 
        { 
            pop     ecx
            fstp    [eax + ecx + (uvector-offset 2)]  ; pop result  
            mov     eax, [esi]
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv< (form dest) ;; (x x-index y y-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
            xor     eax, eax
            fstsw_AX
            test     eax, #x4000
            jne		short :equal
        :not-equal
            test    eax, #x0100
            jne     short :less
        :greater
            mov     eax, [esi]                            ;; eax = NIL 
            jmp     short :done
        :less
            mov     eax, [esi + 4]                        ;; eax = T
            jmp     short :done
        :equal
            mov     eax, [esi]                            ;; eax = NIL 
        :done
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv> (form dest) ;; (x x-index y y-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
            xor     eax, eax
            fstsw_AX
            test     eax, #x4000
            jne		short :equal
        :not-equal
            test    eax, #x0100
            jne     short :less
        :greater
            mov     eax, [esi + 4]                        ;; eax = T 
            jmp     short :done
        :less
            mov     eax, [esi]                            ;; eax = NIL
            jmp     short :done
        :equal
            mov     eax, [esi]                            ;; eax = NIL 
        :done
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv<= (form dest) ;; (x x-index y y-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
            xor     eax, eax
            fstsw_AX
            test     eax, #x4000
            jne		short :equal
        :not-equal
            test    eax, #x0100
            jne     short :less
        :greater
            mov     eax, [esi]                            ;; eax = NIL 
            jmp     short :done
        :less
            mov     eax, [esi + 4]                        ;; eax = T
            jmp     short :done
        :equal
            mov     eax, [esi + 4]                        ;; eax = T 
        :done
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv>= (form dest) ;; (x x-index y y-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
            xor     eax, eax
            fstsw_AX
            test     eax, #x4000
            jne		short :equal
        :not-equal
            test    eax, #x0100
            jne     short :less
        :greater
            mov     eax, [esi + 4]                        ;; eax = T 
            jmp     short :done
        :less
            mov     eax, [esi]                            ;; eax = NIL
            jmp     short :done
        :equal
            mov     eax, [esi + 4]                        ;; eax = T 
        :done
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv= (form dest) ;; (x x-index y y-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fcomp   [eax + ecx + (uvector-offset 2)]      ;; comp(x,y)
            xor     eax, eax
            fstsw_AX
            test     eax, #x4000
            jne		short :equal
            mov     eax, [esi]                          ;; eax = NIL
            jmp     short :done
        :equal
            mov     eax, [esi + 4]                      ;; eax = T 
        :done        
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv/= (form dest) ;; (x x-index y y-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fcomp   [eax + ecx + (uvector-offset 2)]    ;; comp(x,y)
            xor     eax, eax
            fstsw_AX
            test     eax, #x4000
            jne		short :equal
            mov     eax, [esi + 4]                      ;; eax = T
            jmp     short :done
        :equal
            mov     eax, [esi]                          ;; eax = NIL 
        :done        
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

(x86::defcodegen sys:dv-copy (form dest) ;; (x x-index result result-index)
    (cl::compile-sub-form (third form) :dest-stack t)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
            pop     ecx
            fld     [eax + ecx + (uvector-offset 2)] ; load x  
        })
    (x86::offset-stack 4)
    (cl::compile-sub-form (fifth form) :dest-stack t)
    (cl::compile-sub-form (fourth form) :dest-eax-operand t)  
    (x86::parse-assembler 
        { 
            pop     ecx
            fstp    [eax + ecx + (uvector-offset 2)]  ; pop result  
            mov     eax, [esi]
        })
    (x86::offset-stack 4)
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)

