;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		print-float.lisp
;;;;	Contents:	Common Lisp floating point formatting implementation
;;;;	Author:		Bill Maddox. Additonal code written by Roger Corman.
;;;;	History:	1/27/00	RGC  Created. Adapted Bill Maddox's code for
;;;;							 use with Corman Lisp.
;;;;

;;;; Float printing.

;;;
;;;  Written by Bill Maddox
;;;
;;;
;;;
;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does most of 
;;; the work for all printing of floating point numbers in the printer and in
;;; FORMAT.  It converts a floating point number to a string in a free or 
;;; fixed format with no exponent.  The interpretation of the arguments is as 
;;; follows:
;;;
;;;     X        - The floating point number to convert, which must not be
;;;                negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;                of fraction digits to produce if the FDIGITS parameter
;;;                is unspecified or NIL.  If the non-fraction digits and the
;;;                decimal point alone exceed this width, no fraction digits
;;;                will be produced unless a non-NIL value of FDIGITS has been
;;;                specified.  Field overflow is not considerd an error at this
;;;                level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;                trailing zeroes may be introduced as needed.  May be
;;;                unspecified or NIL, in which case as many digits as possible
;;;                are generated, subject to the constraint that there are no
;;;                trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;                printed is (* x (expt 10 scale)).  This scaling is exact,
;;;                and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;                number of fraction digits which will be produced, regardless
;;;                of the value of WIDTH or FDIGITS.  This feature is used by
;;;                the ~E format directive to prevent complete loss of
;;;                significance in the printed value due to a bogus choice of
;;;                scale factor.
;;;
;;; Most of the optional arguments are for the benefit for FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                       point.  Zero indicates point before first digit.
;;;
;;; WARNING: For efficiency, there is a single string object *digit-string*
;;; which is modified destructively and returned as the value of
;;; FLONUM-TO-STRING.  Thus the returned value is not valid across multiple 
;;; calls.
;;;
;;; NOTE:  FLONUM-TO-STRING goes to a lot of trouble to guarantee accuracy.
;;; Specifically, the decimal number printed is the closest possible 
;;; approximation to the true value of the binary number to be printed from 
;;; among all decimal representations  with the same number of digits.  In
;;; free-format output, i.e. with the number of digits unconstrained, it is 
;;; guaranteed that all the information is preserved, so that a properly-
;;; rounding reader can reconstruct the original binary number, bit-for-bit, 
;;; from its printed decimal representation. Furthermore, only as many digits
;;; as necessary to satisfy this condition will be printed.
;;;
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.  The
;;; algorithm is essentially that of algorithm Dragon4 in "How to Print 
;;; Floating-Point Numbers Accurately" by Steele and White.  The current 
;;; (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
;;; DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
;;; THE PAPER!

(in-package "CORMANLISP")

(defvar *digits* "0123456789")

(defvar *digit-string*
  (make-array 50 :element-type 'character :fill-pointer 0 :adjustable t
	      :initial-element #\?)) ; ### Hack around make-array bug.

(defun flonum-to-string (x &optional width fdigits scale fmin)
  (cond ((zerop x)
	 ;;zero is a special case which float-string cannot handle
	 (if fdigits
	     (let ((s (make-string (1+ fdigits) :initial-element #\0)))
	       (setf (schar s 0) #\.)
	       (values s (length s) t (zerop fdigits) 0))
	     (values "." 1 t t 0)))
	(t
	 (setf (fill-pointer *digit-string*) 0)
	 (multiple-value-bind (sig exp)
			      (integer-decode-float x)
	   (let* ((precision (float-precision x))
		  (digits (float-digits x))
		  (fudge (- digits precision))
		  (width (if width (max width 1) nil)))
	   (float-string (ash sig (- fudge)) (+ exp fudge) precision width
			 fdigits scale fmin))))))


(defun float-string (fraction exponent precision width fdigits scale fmin)
  (let ((r fraction) (s 1) (m- 1) (m+ 1) (k 0)
	(digits 0) (decpnt 0) (cutoff nil) (roundup nil) u low high)
    ;;Represent fraction as r/s, error bounds as m+/s and m-/s.
    ;;Rational arithmetic avoids loss of precision in subsequent calculations.
    (cond ((> exponent 0)
	   (setq r (ash fraction exponent))
	   (setq m- (ash 1 exponent))	   
	   (setq m+ m-))                   
	  ((< exponent 0)
	   (setq s (ash 1 (- exponent)))))
    ;;adjust the error bounds m+ and m- for unequal gaps
    (when (= fraction (ash 1 precision))
      (setq m+ (ash m+ 1))
      (setq r (ash r 1))
      (setq s (ash s 1)))
    ;;scale value by requested amount, and update error bounds
    (when scale
      (if (minusp scale)
	  (let ((scale-factor (expt 10 (- scale))))
	    (setq s (* s scale-factor)))
	  (let ((scale-factor (expt 10 scale)))
	    (setq r (* r scale-factor))
	    (setq m+ (* m+ scale-factor))
	    (setq m- (* m- scale-factor)))))
    ;;scale r and s and compute initial k, the base 10 logarithm of r
    (do ()
        ((>= r (ceiling s 10)))
      (decf k)
      (setq r (* r 10))
      (setq m- (* m- 10))
      (setq m+ (* m+ 10)))
    (do ()(nil)
      (do ()
	  ((< (+ (ash r 1) m+) (ash s 1)))
	(setq s (* s 10))
	(incf k))
      ;;determine number of fraction digits to generate
      (cond (fdigits
	     ;;use specified number of fraction digits
	     (setq cutoff (- fdigits))
	     ;;don't allow less than fmin fraction digits
	     (if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin))))
	    (width
	     ;;use as many fraction digits as width will permit
             ;;but force at least fmin digits even if width will be exceeded
	     (if (< k 0)
		 (setq cutoff (- 1 width))
		 (setq cutoff (1+ (- k width))))
	     (if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin)))))
      ;;If we decided to cut off digit generation before precision has
      ;;been exhausted, rounding the last digit may cause a carry propagation.
      ;;We can prevent this, preserving left-to-right digit generation, with
      ;;a few magical adjustments to m- and m+.  Of course, correct rounding
      ;;is also preserved.
      (when (or fdigits width)
	(let ((a (- cutoff k))
	      (y s))
	  (if (>= a 0)
	      (dotimes (i a) (setq y (* y 10)))
	      (dotimes (i (- a)) (setq y (ceiling y 10))))
	  (setq m- (max y m-))
	  (setq m+ (max y m+))
	  (when (= m+ y) (setq roundup t))))
      (when (< (+ (ash r 1) m+) (ash s 1)) (return)))
    ;;zero-fill before fraction if no integer part
    (when (< k 0)
      (setq decpnt digits)
      (vector-push-extend #\. *digit-string*)
      (dotimes (i (- k))
	(incf digits) (vector-push-extend #\0 *digit-string*)))
    ;;generate the significant digits
    (do ()(nil)
      (decf k)
      (when (= k -1)
	(vector-push-extend #\. *digit-string*)
	(setq decpnt digits))
      (multiple-value-setq (u r) (truncate (* r 10) s))
      (setq m- (* m- 10))
      (setq m+ (* m+ 10))
      (setq low (< (ash r 1) m-))
      (if roundup
	  (setq high (>= (ash r 1) (- (ash s 1) m+)))
	  (setq high (> (ash r 1) (- (ash s 1) m+))))
      ;;stop when either precision is exhausted or we have printed as many
      ;;fraction digits as permitted
      (when (or low high (and cutoff (<= k cutoff))) (return))
      (vector-push-extend (char *digits* u) *digit-string*)
      (incf digits))
    ;;if cutoff occured before first digit, then no digits generated at all
    (when (or (not cutoff) (>= k cutoff))
      ;;last digit may need rounding
      (vector-push-extend (char *digits*
				(cond ((and low (not high)) u)
				      ((and high (not low)) (1+ u))
				      (t (if (<= (ash r 1) s) u (1+ u)))))
			  *digit-string*)
      (incf digits))
    ;;zero-fill after integer part if no fraction
    (when (>= k 0)
      (dotimes (i k) (incf digits) (vector-push-extend #\0 *digit-string*))
      (vector-push-extend #\. *digit-string*)
      (setq decpnt digits))
    ;;add trailing zeroes to pad fraction if fdigits specified
    (when fdigits
      (dotimes (i (- fdigits (- digits decpnt)))
	(incf digits)
	(vector-push-extend #\0 *digit-string*)))
    ;;all done
    (values *digit-string* (1+ digits) (= decpnt 0) (= decpnt digits) decpnt)))

;;; SCALE-EXPONENT  --  Internal
;;;
;;;    Given a non-negative floating point number, SCALE-EXPONENT returns a new
;;; floating point number Z in the range (0.1, 1.0] and and exponent E such
;;; that Z * 10^E is (approximately) equal to the original number.  There may
;;; be some loss of precision due the floating point representation.  The
;;; scaling is always done with long float arithmetic, which helps printing of
;;; lesser precisions as well as avoiding generic arithmetic.
;;;
;;;    When computing our initial scale factor using EXPT, we pull out part of
;;; the computation to avoid over/under flow.  When denormalized, we must pull
;;; out a large factor, since there is more negative exponent range than
;;; positive range.
;;;
(defun scale-exponent (original-x)
  (let* ((x (coerce original-x 'long-float)))
    (multiple-value-bind (sig exponent)
			 (decode-float x)
      (declare (ignore sig))
      (if (= x 0.0l0)
	  (values (float 0.0l0 original-x) 1)
	  (let* ((ex (round (* exponent (log 2l0 10))))
		 (x (if (minusp ex)
			(if (float-denormalized-p x)
			    (* x 1.0l16 (expt 10.0l0 (- (- ex) 16)))
			    (* x 10.0l0 (expt 10.0l0 (- (- ex) 1))))
			(/ x 10.0l0 (expt 10.0l0 (1- ex))))))
	    (do ((d 10.0l0 (* d 10.0l0))
		 (y x (/ x d))
		 (ex ex (1+ ex)))
		((< y 1.0l0)
		 (do ((m 10.0l0 (* m 10.0l0))
		      (z y (* y m))
		      (ex ex (1- ex)))
		     ((>= z 0.1l0)
		      (values (float z original-x) ex))))))))))

;;;; Entry point for the float printer.

;;; Entry point for the float printer as called by PRINT, PRIN1, PRINC,
;;; etc.  The argument is printed free-format, in either exponential or 
;;; non-exponential notation, depending on its magnitude.
;;;
;;; NOTE: When a number is to be printed in exponential format, it is scaled in
;;; floating point.  Since precision may be lost in this process, the
;;; guaranteed accuracy properties of FLONUM-TO-STRING are lost.  The
;;; difficulty is that FLONUM-TO-STRING performs extensive computations with
;;; integers of similar magnitude to that of the number being printed.  For
;;; large exponents, the bignums really get out of hand.  If bignum arithmetic
;;; becomes reasonably fast and the exponent range is not too large, then it
;;; might become attractive to handle exponential notation with the same
;;; accuracy as non-exponential notation, using the method described in the
;;; Steele and White paper.


;;; PRINT-FLOAT-EXPONENT  --  Internal
;;;
;;;    Print the appropriate exponent marker for X and the specified exponent.
;;;
(defun print-float-exponent (x exp stream)
    (declare (float x) (integer exp) (stream stream))
    (let
        ((*print-radix* nil)
            (plusp (plusp exp)))
        (if
            (typep x *read-default-float-format*)
            (unless (eql exp 0) (format stream "e~:[~;+~]~D" plusp exp))
            (format
                stream
                "~C~:[~;+~]~D"
                (etypecase
                    x
                    (single-float #\f)
                    (double-float #\d)
                    (short-float #\s)
                    (long-float #\L))
                plusp
                exp))))


;;; FLOAT-FORMAT-NAME  --  Internal
;;;
;;;    Return the string name of X's float format.
;;;
(defun float-format-name (x)
  (declare (float x))
  (etypecase x
    (single-float "SINGLE-FLOAT")
    (double-float "DOUBLE-FLOAT")
    (short-float "SHORT-FLOAT")
    (long-float "LONG-FLOAT")))


;;; OUTPUT-FLOAT-INFINITY  --  Internal
;;;
;;;    Write out an infinity using #. notation, or flame out if
;;; *print-readably* is true and *read-eval* is false.
;;;
(defun output-float-infinity (x stream)
  (declare (float x) (stream stream))
  (cond (*read-eval*
	 (write-string "#." stream))
	(*print-readably*
	 (error 'print-not-readable :object x))
	(t
	 (write-string "#<" stream)))
  (write-string "EXT:" stream)
  (write-string (float-format-name x) stream)
  (write-string (if (plusp x) "-POSITIVE-" "-NEGATIVE-")
		stream)
  (write-string "INFINITY" stream)
  (unless *read-eval*
    (write-string ">" stream)))


;;; OUTPUT-FLOAT-NAN  --  Internal
;;;
;;;    Output a #< NaN or die trying.
;;;
(defun output-float-nan (x stream)
  (print-unreadable-object (x stream)
    (write-string (float-format-name x) stream)
    (write-string (if (float-trapping-nan-p x) " Trapping" " Quiet") stream)
    (write-string " NaN" stream)))


;;; OUTPUT-FLOAT  --  Internal
;;;
;;;    Functioned called by OUTPUT-OBJECT to handle floats.
;;;
(defun output-float (x stream)
  (cond
   ((float-infinity-p x)
    (output-float-infinity x stream))
   ((float-nan-p x)
    (output-float-nan x stream))
   (t
    (let ((x (cond ((minusp (float-sign x))
		    (write-char #\- stream)
		    (- x))
		   (t
		    x))))
      (cond
       ((zerop x)
	(write-string "0.0" stream)
	(print-float-exponent x 0 stream))
       (t
	(output-float-aux x stream (float 1/1000 x) (float 10000000 x))))))))
;;;  
(defun output-float-aux (x stream e-min e-max)
    (if
        (and (>= x e-min) (< x e-max))
        (multiple-value-bind
            (str len lpoint tpoint)
            (flonum-to-string x)
            (declare (ignore len))
            (when lpoint (write-char #\0 stream))
            (write-string str stream)
            (when tpoint (write-char #\0 stream))
            (print-float-exponent x 0 stream))
        (multiple-value-bind
            (f ex)
            (scale-exponent x)
            (multiple-value-bind
                (str len lpoint tpoint)
                (flonum-to-string f nil nil 1)
                (declare (ignore len))
                (when lpoint (write-char #\0 stream))
                (write-string str stream)
                (when tpoint (write-char #\0 stream))
                (print-float-exponent x (1- ex) stream)))))

(defun float-infinity-p (f)
	(multiple-value-bind (m e s)
		(integer-decode-float f)
		(declare (ignore s))
		(or (and (cl::double-float-p f)(= e 972)(= m #x10000000000000))
			(and (cl::single-float-p f)(= e 105)(= m #x800000))
			(and (cl::short-float-p f)(= e 107)(= m #x200000)))))

(defun float-nan-p (f) 
	(multiple-value-bind (m e s)
		(integer-decode-float f)
		(declare (ignore s))
		(or (and (cl::double-float-p f)(= e 972)(/= m #x10000000000000))
			(and (cl::single-float-p f)(= e 105)(/= m #x800000))
			(and (cl::short-float-p f)(= e 107)(/= m #x200000)))))

;;;
;;;	RGC Need to implement these:
;;;
(defun float-denormalized-p (f) (declare (ignore f)) nil)
(defun float-trapping-nan-p (f) (declare (ignore f)) nil)

#|
(%set-format-dispatch-func #\F 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional width digits (scale 0) overflow-char padchar)
		(declare (ignore control colon-modifier))
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~F format directive"))
		(if (and overflow-char (integerp overflow-char))
			(setf overflow-char (int-char overflow-char)))
		(setq padchar (if padchar (if (integerp padchar) (int-char padchar) padchar) #\Space))
		(let* ((f (abs (car args)))
			   (neg (minusp (car args)))
			   (print-sign (or neg (and atsign-modifier (plusp f))))
			   (sign-width (if print-sign 1 0)))
			(multiple-value-bind (float-str digit-length leading-point trailing-point point-pos)
				(ccl::flonum-to-string f (if width (- width sign-width)) digits scale)
				(declare (ignore digit-length point-pos))
				(if width
					;; do any necessary padding
					(dotimes (i (- width 
								(+ (length float-str) sign-width 
									(if leading-point 1 0)
									(if trailing-point 1 0))))
						(write-char padchar stream)))
				(if print-sign
					(write-char (if neg #\- #\+) stream))
				(if leading-point (write-char #\0) stream)
				(write-string float-str stream)
				(if trailing-point (write-char #\0) stream)))
		(1+ index)))
|#

(defun decimal-string (n)
	(write-to-string n :base 10 :radix nil :escape nil))

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  	(unless padleft
    	(write-string string stream))
  	(dotimes (i minpad)
	    (write-char padchar stream))
  	(do ((chars (+ (length string) minpad) (+ chars colinc)))
      	((>= chars mincol))
    (dotimes (i colinc)
      	(write-char padchar stream)))
  	(when padleft
    	(write-string string stream)))

;;; We return true if we overflowed, so that ~G can output the overflow char
;;; instead of spaces.
;;;
(defun format-fixed-aux (stream number w d k ovf pad atsign)
	(cond
		((and (not k) (not (or w d)))
			(prin1 number stream)
			nil)
		(t (let ((spaceleft w))
				(when (and w (or atsign (minusp number))) (decf spaceleft))
      			(multiple-value-bind 
	  				(str len lpoint tpoint)
	  				(ccl::flonum-to-string (abs number) spaceleft d k)
					;;if caller specifically requested no fraction digits, suppress the
					;;optional trailing zero
					(when (and d (zerop d)) (setq tpoint nil))
					(when w 
						(decf spaceleft len)
	  					;;optional leading zero
	  					(when lpoint
							(if (or (> spaceleft 0) tpoint) ;force at least one digit
								(decf spaceleft)
								(setq lpoint nil)))
	  					;;optional trailing zero
	  					(when tpoint
							(if (> spaceleft 0)
								(decf spaceleft)
								(setq tpoint nil))))
					(cond ((and w (< spaceleft 0) ovf)
	       					;;field width overflow
							(dotimes (i w) (write-char ovf stream))
							t)
						  (t
							(when w (dotimes (i spaceleft) (write-char pad stream)))
							(if (minusp number)
		   						(write-char #\- stream)
		   						(if atsign (write-char #\+ stream)))
	       					(when lpoint (write-char #\0 stream))
	       					(write-string str stream)
	       					(when tpoint (write-char #\0 stream))
	       					nil)))))))

(defun format-fixed (stream number w d k ovf pad atsign)
  (if (floatp number)
      (format-fixed-aux stream number w d k ovf pad atsign)
      (if (rationalp number)
	  (format-fixed-aux stream
			    (coerce number 'single-float)
			    w d k ovf pad atsign)
	  (format-write-field stream
			      (decimal-string number)
			      w 1 0 #\space t))))

(defun format-general-aux (stream number w d e k ovf pad marker atsign)
	(multiple-value-bind (ignore n)
		(ccl::scale-exponent (abs number))
		(declare (ignore ignore))
	    ;;Default d if omitted.  The procedure is taken directly
	    ;;from the definition given in the manual, and is not
	    ;;very efficient, since we generate the digits twice.
	    ;;Future maintainers are encouraged to improve on this.
	    (unless d
	      	(multiple-value-bind (str len)
				(ccl::flonum-to-string (abs number))
				(declare (ignore str))
				(let ((q (if (= len 1) 1 (1- len))))
					(setq d (max q (min n 7))))))
		(let* ((ee (if e (+ e 2) 4))
			   (ww (if w (- w ee) nil))
	   		   (dd (- d n)))
			(cond ((<= 0 dd d)
				   (let ((char (if (format-fixed-aux stream number ww dd nil
					       ovf pad atsign)
									ovf #\space)))
						(dotimes (i ee) (write-char char stream))))
				 (t (format-exp-aux stream number w d e (or k 1) ovf pad marker atsign))))))

(defun format-general (stream number w d e k ovf pad marker atsign)
  ;;The Excelsior edition does not say what to do if
  ;;the argument is not a float.  Here, we adopt the
  ;;conventions used by ~F and ~E.
	(if (floatp number)
		(format-general-aux stream number w d e k ovf pad marker atsign)
		(if (rationalp number)
	  		(format-general-aux stream
				(coerce number 'single-float)
			    w d e k ovf pad marker atsign)
	  		(format-write-field stream
				(decimal-string number)
			    w 1 0 #\space t))))


(defun format-exponential (stream number w d e k ovf pad marker atsign)
  (if (floatp number)
      (format-exp-aux stream number w d e k ovf pad marker atsign)
      (if (rationalp number)
	  (format-exp-aux stream
			  (coerce number 'single-float)
			  w d e k ovf pad marker atsign)
	  (format-write-field stream
			      (decimal-string number)
			      w 1 0 #\space t))))

(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\e
      (typecase number
	(single-float #\f)
	(double-float #\d)
	(short-float #\s)
	(long-float #\l))))

;;;Here we prevent the scale factor from shifting all significance out of
;;;a number to the right.  We allow insignificant zeroes to be shifted in
;;;to the left right, athough it is an error to specify k and d such that this
;;;occurs.  Perhaps we should detect both these condtions and flag them as
;;;errors.  As for now, we let the user get away with it, and merely guarantee
;;;that at least one significant digit will appear.

(defun format-exp-aux (stream number w d e k ovf pad marker atsign)
  (if (not (or w d))
      (prin1 number stream)
      (multiple-value-bind (num expt)
			   (ccl::scale-exponent (abs number))
	(let* ((expt (- expt k))
	       (estr (decimal-string (abs expt)))
	       (elen (if e (max (length estr) e) (length estr)))
	       (fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
	       (fmin (if (minusp k) (- 1 k) nil))
	       (spaceleft (if w
			      (- w 2 elen
				 (if (or atsign (minusp number))
				     1 0))
			      nil)))
	  (if (and w ovf e (> elen e)) ;exponent overflow
	      (dotimes (i w) (write-char ovf stream))
	      (multiple-value-bind
		  (fstr flen lpoint)
		  (ccl::flonum-to-string num spaceleft fdig k fmin)
		(when w 
		  (decf spaceleft flen)
		  (when lpoint
		    (if (> spaceleft 0)
			(decf spaceleft)
			(setq lpoint nil))))
		(cond ((and w (< spaceleft 0) ovf)
		       ;;significand overflow
		       (dotimes (i w) (write-char ovf stream)))
		      (t (when w
			   (dotimes (i spaceleft) (write-char pad stream)))
			 (if (minusp number)
			     (write-char #\- stream)
			     (if atsign (write-char #\+ stream)))
			 (when lpoint (write-char #\0 stream))
			 (write-string fstr stream)
			 (write-char (if marker
					 marker
					 (format-exponent-marker number))
				     stream)
			 (write-char (if (minusp expt) #\- #\+) stream)
			 (when e 
			   ;;zero-fill before exponent if necessary
			   (dotimes (i (- e (length estr)))
			     (write-char #\0 stream)))
			 (write-string estr stream)))))))))

(cl::%set-format-dispatch-func #\E 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional width digits exp-digits 
					scale overflow-char 
					padchar
					exponent-char)
		(declare (ignore control))
		(when colon-modifier
		    (error "Cannot specify the colon modifier ~~E format directive."))
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~E format directive"))

		;; initialize defaults
		(unless padchar (setf padchar #\Space))
		(unless scale (setf scale 1))
		(if (integerp overflow-char) 
			(setf overflow-char (int-char overflow-char)))
		(if (integerp padchar) 
			(setf padchar (int-char padchar)))
		(if (integerp exponent-char) 
			(setf exponent-char (int-char exponent-char)))

		(format-exponential stream (car args) width digits exp-digits scale
			overflow-char padchar exponent-char atsign-modifier)
		(1+ index)))

(cl::%set-format-dispatch-func #\F 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional width digits
					scale overflow-char 
					padchar)
		(declare (ignore control))
		(when colon-modifier
		    (error "Cannot specify the colon modifier with ~~F format directive."))
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~F format directive"))

		(unless padchar (setf padchar #\Space))
		(if (integerp overflow-char) 
			(setf overflow-char (int-char overflow-char)))
		(if (integerp padchar) 
			(setf padchar (int-char padchar)))

		(format-fixed stream (car args) width digits scale
			overflow-char padchar atsign-modifier)
		(1+ index)))

(cl::%set-format-dispatch-func #\G 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional width digits exp-digits scale overflow-char padchar
					exponent-char)
		(declare (ignore control))
		(when colon-modifier
		    (error "Cannot specify the colon modifier with ~~G format directive."))
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~G format directive"))

		;; initialize defaults
		(unless padchar (setf padchar #\Space))
		(if (integerp overflow-char) 
			(setf overflow-char (int-char overflow-char)))
		(if (integerp padchar) 
			(setf padchar (int-char padchar)))
		(if (integerp exponent-char) 
			(setf exponent-char (int-char exponent-char)))

		(format-general stream (car args) width digits exp-digits scale
			overflow-char padchar exponent-char atsign-modifier)
		(1+ index)))

(cl::%set-format-dispatch-func #\$ 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional digits n width padchar)
		(declare (ignore control atsign-modifier colon-modifier n)) ;; need to implement these
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~$ format directive"))

		(unless padchar (setf padchar #\Space))
		(if (integerp padchar) 
			(setf padchar (int-char padchar)))
		(format-fixed stream (car args) width (if digits digits 2) nil
			nil padchar nil)
		(1+ index)))

;;; Redefine this to handle infinity and NAN.
;;; Used by WRITE
(defun cl::write-float (float)
  (let ((os *standard-output*)
        (magnitude (if (minusp float) (- float) float))
        (*print-base* 10)
        (*print-radix* nil))
    (cond ((float-infinity-p float) (output-float-infinity float os))
          ((float-nan-p float) (output-float-nan float os))
          (t
           (when (= float 0.0)
             (cl::%output-chars "0.0" os 0 3)
             (let ((exp-signifier (cl::choose-exp float)))
               (unless (eql exp-signifier #\e)
                 (cl::%output-char exp-signifier os)
                 (cl::%output-char #\0 os))
               (return-from cl::write-float)))
           (if (and (>= magnitude 0.01) (< magnitude 1.0e8))
               (cl::write-decimal-float float os t)
               (cl::write-exp-float float os))))))
