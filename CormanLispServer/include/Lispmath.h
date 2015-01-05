//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		lispmath.h
//		Contents:	Corman Lisp kernel math functions.
//		History:	10/2/96  RGC  Created.
//

#ifndef LISPMATH_H
#define LISPMATH_H

#include "lisp.h"

LispObj _Add(LispObj, LispObj);
LispObj _Subtract(LispObj, LispObj);
LispObj _Multiply(LispObj, LispObj);
LispObj _Divide(LispObj, LispObj);
LispObj lispNumericEqual(LispObj, LispObj);
LispObj lispGreater(LispObj, LispObj);
LispObj lispGreaterEqual(LispObj, LispObj);
LispObj lispLess(LispObj, LispObj);
LispObj lispLessEqual(LispObj, LispObj);
LispObj lispNotNumericEqual(LispObj, LispObj);
LispObj lispNegate(LispObj);
LispObj lispAbs(LispObj);
LispObj bignumIntegerLength(LispObj b);
LispObj bignumShift(LispObj n, LispObj shift, LispObj signedShift);
LispObj fixnumToBignum(LispObj fn);

LispDeclare(Plus);
LispDeclare(Minus);
LispDeclare(Multiply);
LispDeclare(Divide);
LispDeclare(Truncate);
LispDeclare(Float);
LispDeclare(Floor);
LispDeclare(Ceiling);
LispDeclare(Round);
LispDeclare(NumericEqual);
LispDeclare(Greater);
LispDeclare(GreaterEqual);
LispDeclare(Less);
LispDeclare(LessEqual);
LispDeclare(NotEqual);
LispDeclare(Mod);
LispDeclare(Sqrt);
LispDeclare(Isqrt);
LispDeclare(Exp);
LispDeclare(Expt);
LispDeclare(Log);
LispDeclare(Sin);
LispDeclare(Cos);
LispDeclare(Logxor);
LispDeclare(Logior);
LispDeclare(Logand);
LispDeclare(Lognot);
LispDeclare(Logandc1);
LispDeclare(Logandc2);
LispDeclare(Logeqv);
LispDeclare(Lognand);
LispDeclare(Lognor);
LispDeclare(Logorc1);
LispDeclare(Logorc2);
LispDeclare(Complex);

#endif	// LISPMATH_H