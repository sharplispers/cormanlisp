;;;
;;; BOOLE function
;;; 27 October, 2000 - Frank A. Adrian
;;;

(defconstant boole-clr		0)
(defconstant boole-and		1)
(defconstant boole-andc1	2)
(defconstant boole-2		3)
(defconstant boole-andc2	4)
(defconstant boole-1		5)
(defconstant boole-xor		6)
(defconstant boole-ior		7)
(defconstant boole-nor		8)
(defconstant boole-eqv		9)
(defconstant boole-c1		10)
(defconstant boole-orc1		11)
(defconstant boole-c2		12)
(defconstant boole-orc2		13)
(defconstant boole-nand		14)
(defconstant boole-set		15)


(defun boole (op i1 i2)
	(case op
		(#.boole-clr
			(check-type i1 integer) (check-type i2 integer) 0)
		(#.boole-and
			(logand i1 i2))
		(#.boole-andc1
			(logandc1 i1 i2))
		(#.boole-2
			(check-type i1 integer) (check-type i2 integer) i2)
		(#.boole-andc2
			(logandc2 i1 i2))
		(#.boole-1
			(check-type i1 integer) (check-type i2 integer) i1)
		(#.boole-xor
			(logxor i1 i2))
		(#.boole-ior
			(logior i1 i2))
		(#.boole-nor
			(lognor i1 i2))
		(#.boole-eqv
			(logeqv i1 i2))
		(#.boole-c1
			(check-type i2 integer) (lognot i1))
		(#.boole-orc1
			(logorc1 i1 i2))
		(#.boole-c2
			(check-type i1 integer) (lognot i2))
		(#.boole-orc2
			(logorc2 i1 i2))
		(#.boole-nand
			(lognand i1 i2))
		(#.boole-set
			(check-type i1 integer) (check-type i2 integer) -1)
		(otherwise
			(error (make-condition 'type-error
				:datum op :expected-type "a valid boole- operation")))))
