//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		LispMath.cpp
//		Contents:	Corman Lisp kernel math functions.
//		History:	10/2/96  RGC  Created.
//					12/15/99 RGC  Made COMPLEX ansi compliant.
//					5/20/01  RGC  GCD now works correctly when one or more
//								  arguments are zero.
//

#include "stdafx.h"
#include <math.h>
#include <limits.h>

#include "lispmath.h"

#pragma warning (disable:4244)				// conversion from 'int' to 'unsigned short', possible loss of data
#pragma warning (disable:4127)				// conditional expression is constant
#pragma warning (disable:4505)				// unreferenced local function has been removed

#define both_fixnums(a,b)		((((a) | (b)) & 7) == 0)
#define both_shortFloats(a,b)   ((((a) & (b)) & 3) == 3)

static LispObj bignumAdd(LispObj, LispObj);
static LispObj addRatios(LispObj, LispObj);
static LispObj bignumSubtract(LispObj, LispObj);
static LispObj subtractRatios(LispObj, LispObj);
static LispObj bignumMultiply(LispObj, LispObj);
static LispObj multiplyRatios(LispObj, LispObj);
static LispObj multiplyComplexNumbers(LispObj, LispObj);
static LispObj bignumDivide(LispObj, LispObj);
static LispObj divideRatios(LispObj, LispObj);
static LispObj divideComplexNumbers(LispObj, LispObj);
static void addBignumWords(LispObj b1, long b1Len,
			LispObj b2, long b2Len, LispObj result);
static void subBignumWords(LispObj b1, long b1Len,
			LispObj b2, long b2Len, LispObj result);
static void mulBignumWords(LispObj b1, long b1Len,
			LispObj b2, long b2Len, LispObj result);
static void shiftBignumWords(LispObj b, long srcLen, LispObj res,
			long resultLen, long bits);
static void xorBignumWords(LispObj b1, LispObj b1Len,
			LispObj b2, LispObj b2Len, LispObj result);
static void orBignumWords(LispObj b1, LispObj b1Len,
			LispObj b2, LispObj b2Len, LispObj result);
static void andBignumWords(LispObj b1, LispObj b1Len,
			LispObj b2, LispObj b2Len, LispObj result);
static void notBignumWords(LispObj b, LispObj bLen, LispObj result);
static LispObj convertToDoubleFloat(LispObj);
static LispObj convertToSingleFloat(LispObj);
static LispObj convertToShortFloat(LispObj);
static long bignumAbsCompare(LispObj b1, LispObj b2);
static LispObj simplifyRatio(LispObj n1, LispObj n2);
static void bignumSetBit(LispObj b, long bit);
static long bignumHighBit(LispObj b);
static LispObj normalizeBignum(LispObj b);
static LispObj _Floor(LispObj);
static LispObj _Ceiling(LispObj);
static LispObj _Truncate(LispObj n);
static LispObj _Round(LispObj n);
static LispObj _Mod(LispObj n1, LispObj n2);
static LispObj divideIntegerNumbers(LispObj n1, LispObj n2);
static long numCompare(LispObj x, LispObj y);
       long compareNumbers(LispObj n1, LispObj n2);
static LispObj convertDoubleFloatToInteger(LispObj n);
static LispObj convertSingleFloatToInteger(LispObj n);
static LispObj convertShortFloatToInteger(LispObj n);
static LispObj convertToBignum(LispObj n);
static long compareRatios(LispObj n1, LispObj n2);
static long bignumCompare(LispObj b1, LispObj b2);
static double getFloat(LispObj n);
static double bignumToDouble(LispObj n);
static long isZero(LispObj n);
static long equalComplexNumbers(LispObj n1, LispObj n2);
static LispObj FloatNumber(LispObj n);
static LispObj _Negate(LispObj n);
static LispObj _Abs(LispObj n);
static LispObj gcdNumbers(LispObj n1, LispObj n2);
static LispObj absComplex(LispObj n);
static long gcdIntegers(long n1, long n2);
static LispObj complexLog(LispObj);
static LispObj _Log(LispObj);
static LispObj bignumXor(LispObj n1, LispObj n2);
static LispObj bignumIor(LispObj n1, LispObj n2);
static LispObj bignumAnd(LispObj n1, LispObj n2);
static LispObj bignumNot(LispObj n);
static LispObj _Logxor(LispObj n1, LispObj n2);
static LispObj _Logior(LispObj n1, LispObj n2);
static LispObj _Logand(LispObj n1, LispObj n2);
static LispObj _Lognot(LispObj n);
static void Bignum_2CtoSM(LispObj bn);
static LispObj createComplex(LispObj real, LispObj imag);
static LispObj createComplexNode(LispObj real, LispObj imag);
static LispObj divideBignums(LispObj num, LispObj denom, long RemDesired);
static LispObj singleToDoubleFloat(LispObj single);
static LispObj shortToDoubleFloat(LispObj single);
LispObj addShortFloats(LispObj, LispObj);

// Trim any leading zeros (at the end of bignum)
static void bignumReduce(LispObj b)
{
	int numcells = bignumNumCells(b);
	int neg = bignumNegative(b);
	while (numcells > 0 && bignumStart(b)[numcells-1] == 0)
		numcells--;
	if (numcells == 0)
		neg = 0;
	UVECTOR(b)[BIGNUM_LENGTH] = (numcells << 4) | neg;
}

//
//	convertToDoubleFloat()
//	This is designed to be a fast number to float promotion for
//	use by the math library routines. Returns a Common Lisp float number.
//
static LispObj convertToDoubleFloat(LispObj n)
{
	LispObj ret = 0;
	if (isDoubleFloat(n))	return n;
	if (isSingleFloat(n))	return singleToDoubleFloat(n);
	if (isShortFloat(n))	return shortToDoubleFloat(n);
	if (isFixnum(n))
	{
		ret = doubleFloatNode(0);
		doubleFloat(ret) = (double)integer(n);
		return ret;
	}
	if (isBignum(n))		
	{
		ret = doubleFloatNode(0);
		doubleFloat(ret) = bignumToDouble(n);
		return ret;
	}
	if (isRatio(n))			
	{
		ret = doubleFloatNode(0);
		doubleFloat(ret) = getFloat(ratioNumerator(n)) / getFloat(ratioDenominator(n));
		return ret;
	}
	Error("Cannot convert to float: ~A", n);
	return 0;	// never gets here
}

#define MostPositiveSingleFloat  3.4028235e38	// largest that can fit in a single float
#define MostNegativeSingleFloat -3.4028235e38	// largest that can fit in a single float
#define MostPositiveShortFloat   3.4028230e38	// largest that can fit in a short float
#define MostNegativeShortFloat  -3.4028230e38	// largest that can fit in a short float
//
//	convertToSingleFloat()
//
static LispObj convertToSingleFloat(LispObj n)
{
	LispObj temp = 0;
	double d = 0.0;
	if (isSingleFloat(n))	return n;
	if (isDoubleFloat(n))	
	{
		temp = singleFloatNode(0);
		singleFloat(temp) = doubleFloat(n);
		return temp;
	}
	if (isShortFloat(n))	
	{
		temp = singleFloatNode(0);
		singleFloat(temp) = shortFloat(n);
		return temp;
	}
	if (isFixnum(n))		
	{
		temp = singleFloatNode(0);
		singleFloat(temp) = (double)integer(n);
		return temp;
	}
	if (isBignum(n))
	{
		d = bignumToDouble(n);
		if (d < MostNegativeSingleFloat || d > MostPositiveSingleFloat)
			Error("The number ~A is too large in magnitude to be converted to a SINGLE-FLOAT", n);
		temp = singleFloatNode(0);
		singleFloat(temp) = d;
		return temp;
	}
	if (isRatio(n))
	{
		d = getFloat(ratioNumerator(n)) / getFloat(ratioDenominator(n));
		if (d < MostNegativeSingleFloat || d > MostPositiveSingleFloat)
			Error("The number ~A is too large in magnitude to be converted to a SINGLE-FLOAT", n);
		temp = singleFloatNode(0);
		singleFloat(temp) = d;
		return temp;
	}
	Error("Cannot convert to float: ~A", n);
	return 0;	// never gets here
}

//
//	convertToShortFloat()
//
static LispObj convertToShortFloat(LispObj n)
{
	double d = 0.0;

	if (isShortFloat(n))	return n;
	if (isSingleFloat(n))	return createShortFloat(singleFloat(n));
	if (isDoubleFloat(n))	return createShortFloat(doubleFloat(n));
	if (isFixnum(n))		return createShortFloat((double)integer(n));
	if (isBignum(n))
	{
		d = bignumToDouble(n);
		if (d < MostNegativeShortFloat || d > MostPositiveShortFloat)
			Error("The number ~A is too large in magnitude to be converted to a SHORT-FLOAT", n);
		return createShortFloat(d);
	}
	if (isRatio(n))
	{
		d = getFloat(ratioNumerator(n)) / getFloat(ratioDenominator(n));
		if (d < MostNegativeShortFloat || d > MostPositiveShortFloat)
			Error("The number ~A is too large in magnitude to be converted to a SHORT-FLOAT", n);
		return createShortFloat(d);
	}
	Error("Cannot convert to float: ~A", n);
	return 0;	// never gets her
}

LispObj fixnumToBignum(LispObj fn)
{
	LispObj bn = 0;
	long sign = 0;
	long n = (long)fn;
	bn = bignumNode(wrapInteger(1));
	if (n == 0)
		UVECTOR(bn)[BIGNUM_FIRST_CELL] = 0;
	else
    if (fn == 0x80000000)
	{
		UVECTOR(bn)[BIGNUM_FIRST_CELL] = 0x10000000;
		bignumSetSign(bn, 1);
	}
	else
	{
		if (n < 0)
		{
			sign++;
			n = -n;
		}
		UVECTOR(bn)[BIGNUM_FIRST_CELL] = integer(n);
		if (sign) bignumSetSign(bn, sign);
	}
	return bn;
}

LispObj addFixnumShort (LispObj a, LispObj b){return createShortFloat((double)integer(a)+ shortFloat(b));	 }
LispObj addFixnumSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = (double)integer(a)+ singleFloat(b);
	return ret;
}
LispObj addFixnumDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = (double)integer(a)+ doubleFloat(b);
	return ret;
}
LispObj addShortFixnum (LispObj a, LispObj b){return createShortFloat(shortFloat(a)    + (double)integer(b));}
LispObj addSingleFixnum(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) + (double)integer(b);
	return ret;
}
LispObj addDoubleFixnum(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) + (double)integer(b);
	return ret;
}
LispObj addShortShort  (LispObj a, LispObj b){return createShortFloat(shortFloat(a)    + shortFloat(b));	 }
LispObj addShortSingle (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = shortFloat(a) + singleFloat(b);
	return ret;
}
LispObj addShortDouble (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = shortFloat(a) + doubleFloat(b);
	return ret;
}
LispObj addSingleShort (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a)   + shortFloat(b);
	return ret;
}
LispObj addSingleSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) + singleFloat(b);
	return ret;
}
LispObj addSingleDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = singleFloat(a) + doubleFloat(b);
	return ret;
}
LispObj addDoubleShort (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) + shortFloat(b);
	return ret;
}
LispObj addDoubleSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) + singleFloat(b);
	return ret;
}
LispObj addDoubleDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) + doubleFloat(b);
	return ret;
}

LispObj subFixnumShort (LispObj a, LispObj b){return createShortFloat((double)integer(a)- shortFloat(b));	 }
LispObj subFixnumSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = (double)integer(a)- singleFloat(b);
	return ret;
}
LispObj subFixnumDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = (double)integer(a)- doubleFloat(b);
	return ret;
}
LispObj subShortFixnum (LispObj a, LispObj b){return createShortFloat(shortFloat(a)    - (double)integer(b));}
LispObj subSingleFixnum(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) - (double)integer(b);
	return ret;
}
LispObj subDoubleFixnum(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) - (double)integer(b);
	return ret;
}
LispObj subShortShort  (LispObj a, LispObj b){return createShortFloat(shortFloat(a)    - shortFloat(b));	 }
LispObj subShortSingle (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = shortFloat(a) - singleFloat(b);
	return ret;
}
LispObj subShortDouble (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = shortFloat(a) - doubleFloat(b);
	return ret;
}
LispObj subSingleShort (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) - shortFloat(b);
	return ret;
}
LispObj subSingleSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) - singleFloat(b);
	return ret;
}
LispObj subSingleDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = singleFloat(a) - doubleFloat(b);
	return ret;
}
LispObj subDoubleShort (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) - shortFloat(b);
	return ret;
}
LispObj subDoubleSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) - singleFloat(b);
	return ret;
}
LispObj subDoubleDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) - doubleFloat(b);
	return ret;
}

LispObj mulFixnumShort (LispObj a, LispObj b){return createShortFloat((double)integer(a)* shortFloat(b));	 }
LispObj mulFixnumSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = (double)integer(a)* singleFloat(b);
	return ret;
}
LispObj mulFixnumDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = (double)integer(a)* doubleFloat(b);
	return ret;
}
LispObj mulShortFixnum (LispObj a, LispObj b){return createShortFloat(shortFloat(a)    * (double)integer(b));}
LispObj mulSingleFixnum(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) * (double)integer(b);
	return ret;
}
LispObj mulDoubleFixnum(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) * (double)integer(b);
	return ret;
}
LispObj mulShortShort  (LispObj a, LispObj b){return createShortFloat(shortFloat(a)    * shortFloat(b));	 }
LispObj mulShortSingle (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = shortFloat(a)  * singleFloat(b);
	return ret;
}
LispObj mulShortDouble (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = shortFloat(a) * doubleFloat(b);
	return ret;
}
LispObj mulSingleShort (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) * shortFloat(b);
	return ret;
}
LispObj mulSingleSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) * singleFloat(b);
	return ret;
}
LispObj mulSingleDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = singleFloat(a) * doubleFloat(b);
	return ret;
}
LispObj mulDoubleShort (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) * shortFloat(b);
	return ret;
}
LispObj mulDoubleSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a)   * singleFloat(b);
	return ret;
}
LispObj mulDoubleDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) * doubleFloat(b);
	return ret;
}

LispObj divFixnumShort (LispObj a, LispObj b){return createShortFloat((double)integer(a)/ shortFloat(b));	 }
LispObj divFixnumSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = (double)integer(a) / singleFloat(b);
	return ret;
}
LispObj divFixnumDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = (double)integer(a)/ doubleFloat(b);
	return ret;
}
LispObj divShortFixnum (LispObj a, LispObj b){return createShortFloat(shortFloat(a)    / (double)integer(b));}
LispObj divSingleFixnum(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) / (double)integer(b);
	return ret;
}
LispObj divDoubleFixnum(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) / (double)integer(b);
	return ret;
}
LispObj divShortShort  (LispObj a, LispObj b){return createShortFloat(shortFloat(a)    / shortFloat(b));	 }
LispObj divShortSingle (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = shortFloat(a)  / singleFloat(b);
	return ret;
}
LispObj divShortDouble (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = shortFloat(a) / doubleFloat(b);
	return ret;
}
LispObj divSingleShort (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) / shortFloat(b);
	return ret;
}
LispObj divSingleSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = singleFloatNode(0);
	singleFloat(ret) = singleFloat(a) / singleFloat(b);
	return ret;
}
LispObj divSingleDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = singleFloat(a)   / doubleFloat(b);
	return ret;
}
LispObj divDoubleShort (LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) / shortFloat(b);
	return ret;
}
LispObj divDoubleSingle(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) / singleFloat(b);
	return ret;
}
LispObj divDoubleDouble(LispObj a, LispObj b)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = doubleFloat(a) / doubleFloat(b);
	return ret;
}

LispObj _Add(LispObj n1, LispObj n2)
{
	LispObj result = n1;

	// quick check here to optimize for adding 2 fixnums
	if (both_fixnums(n1, n2))
	{
		// attempt fixnum add
		__asm mov eax, dword ptr n2
		__asm add dword ptr result, eax
		__asm jo do_bignum
		return result;
	}

	if (both_shortFloats(n1, n2))
		return addShortFloats(n1, n2);

	if (isFixnum(n1))
	{
		if		(isDoubleFloat(n2))		return addFixnumDouble(n1, n2);
		else if (isSingleFloat(n2))		return addFixnumSingle(n1, n2);
		else if (isShortFloat(n2))		return addFixnumShort(n1, n2);
		else if (isRatio(n2))			return addRatios(n1, n2);
		else if (isBignum(n2))			return bignumAdd(fixnumToBignum(n1), n2);
		else if (isComplex(n2))			return createComplex(_Add(n1, complexReal(n2)), complexImaginary(n2));
		else goto error;
	}
	else
	if (isRatio(n1))
	{
		if		(isLispInteger(n2))		return addRatios(n1, n2);
		else if (isRatio(n2))			return addRatios(n1, n2);
		else if (isDoubleFloat(n2))		return addDoubleDouble(convertToDoubleFloat(n1), n2);
		else if (isSingleFloat(n2))		return addSingleSingle(convertToSingleFloat(n1), n2);
		else if (isShortFloat(n2))		return addShortShort(convertToShortFloat(n1), n2);
		else if (isComplex(n2))			return createComplex(_Add(n1, complexReal(n2)), complexImaginary(n2));
		else goto error;
	}
	else
	if (isDoubleFloat(n1))
	{
		if (isComplex(n2))				return createComplex(_Add(n1, complexReal(n2)),
													_Add(doubleFloatNode(0.0), complexImaginary(n2)));
		else return addDoubleDouble(n1, convertToDoubleFloat(n2));
	}
	else
	if (isSingleFloat(n1))
	{
		if (isComplex(n2))				return createComplex(_Add(n1, complexReal(n2)),
													_Add(singleFloatNode(0.0), complexImaginary(n2)));
		else if (isDoubleFloat(n2))		return addSingleDouble(n1, n2);
		else if (isSingleFloat(n2))		return addSingleSingle(n1, n2);
		else if (isShortFloat(n2))		return addSingleShort(n1, n2);
		else							return addSingleSingle(n1, convertToSingleFloat(n2));
	}
	else
	if (isShortFloat(n1))
	{
		if (isComplex(n2))				return createComplex(_Add(n1, complexReal(n2)),
													_Add(createShortFloat(0.0), complexImaginary(n2)));
		else if (isDoubleFloat(n2))		return addShortDouble(n1, n2);
		else if (isSingleFloat(n2))		return addShortSingle(n1, n2);
		else if (isShortFloat(n2))		return addShortShort(n1, n2);
		else							return addShortShort(n1, convertToShortFloat(n2));
	}
	else
	if (isBignum(n1))
	{
		if		(isFixnum(n2))			return bignumAdd(n1, fixnumToBignum(n2));
		else if (isRatio(n2))			return addRatios(n1, n2);
		else if (isDoubleFloat(n2))		return addDoubleDouble(convertToDoubleFloat(n1), n2);
		else if (isSingleFloat(n2))		return addSingleSingle(convertToSingleFloat(n1), n2);
		else if (isShortFloat(n2))		return addShortShort(convertToShortFloat(n1), n2);
		else if (isBignum(n2))			return bignumAdd(n1, n2);
		else if (isComplex(n2))			return createComplex(_Add(n1, complexReal(n2)), complexImaginary(n2));
		else goto error;
	}
	else
	if (isComplex(n1))
	{
		if (isComplex(n2))				return createComplex(_Add(complexReal(n1), complexReal(n2)),
												_Add(complexImaginary(n1), complexImaginary(n2)));
		else							return createComplex(_Add(complexReal(n1), n2), complexImaginary(n1));
	}
error:
	Error("Cannot call function '+' with these operands: ~A and ~A", n1, n2);

do_bignum:
	return bignumAdd(fixnumToBignum(n1), fixnumToBignum(n2));
}

#define FixnumID		0
#define BignumID		1
#define RatioID			2
#define ShortFloatID	3
#define SingleFloatID	4
#define DoubleFloatID	5
#define ComplexID		6
#define NotNumberID		7
#define NumMaxIDs		8

typedef LispObj (*SubractMethod)(LispObj n1, LispObj n2);
extern SubractMethod subtractMethods[];	// forward declaration
static LispObj bignumSubtract(LispObj b1, LispObj b2);

int numTypeTable[] =
{
	NotNumberID,	// FunctionType					0
	NotNumberID,	// KFunctionType				1
	NotNumberID,	// StructureType				2
	NotNumberID,	// ArrayType					3
	NotNumberID,	// SymbolType					4
	NotNumberID,	// StreamType					5
	DoubleFloatID,	// DoubleFloatType				6
	NotNumberID,	// PackageType					7
	NotNumberID,	// 	HashtableType				8
	NotNumberID,	// 	ForeignType					9
	NotNumberID,	// 	CompiledCodeType			10
	NotNumberID,	// 	ReadTableType				11
	ComplexID,		// 	ComplexType					12
	RatioID,		// 	RatioType					13
	BignumID,		// 	BignumType					14
	NotNumberID,	// 	ForeignHeapType				15
	NotNumberID,	// 	WeakPointerType				16
	NotNumberID,	// 	SimpleVectorType			17
	NotNumberID,	// 	SimpleCharVectorType		18
	NotNumberID,	// 	SimpleByteVectorType		19
	NotNumberID,	// 	SimpleShortVectorType		20
	NotNumberID,	// 	SimpleDoubleFloatVectorType 21
	NotNumberID,	// 	SimpleBitVectorType			22
	NotNumberID,	// 	SimpleSingleFloatVectorType 23
	SingleFloatID,	// 	SingleFloatType				24
	NotNumberID,	// 	CLOSInstanceType			25
	NotNumberID,	// 	ForeignStackType			26
	NotNumberID,	// 	ForeignStackEndType			27
	NotNumberID,	// 	Unused						28
	NotNumberID,	// 	Unused						29
	NotNumberID,	// 	Unused						30
	NotNumberID,	// 	Unused						31
};
#define numType2(n)											\
	(isFixnum(n) ? FixnumID :								\
		(isUvector(n) ? numTypeTable[uvectorType(n)] :		\
			(isShortFloat(n) ? ShortFloatID : NotNumberID)))

int numType(LispObj n)
{
	return
		(isFixnum(n) ? FixnumID :
			(isUvector(n) ? numTypeTable[uvectorType(n)] :
				(isShortFloat(n) ? ShortFloatID : NotNumberID)));
}

LispObj fixnumToRatio(LispObj n) { return ratioNode(n, wrapInteger(1)); }
LispObj fixnumToShortFloat(LispObj n) { return createShortFloat((double)integer(n)); }
LispObj fixnumToSingleFloat(LispObj n) { return singleFloatNode((double)integer(n)); }
LispObj fixnumToDoubleFloat(LispObj n) { return doubleFloatNode((double)integer(n)); }
LispObj fixnumToComplex(LispObj n) { return complexNode(n, wrapInteger(0)); }
LispObj bignumToRatio(LispObj n) { return ratioNode(n, wrapInteger(1)); }

LispObj bignumToShortFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	d_temp1 = bignumToDouble(n);
	return createShortFloat(d_temp1); 
}
LispObj bignumToSingleFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	d_temp1 = bignumToDouble(n);
	return singleFloatNode(d_temp1); 
}
LispObj bignumToDoubleFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	d_temp1 = bignumToDouble(n);
	return doubleFloatNode(d_temp1); 
}
LispObj bignumToComplex(LispObj n) { return complexNode(n, wrapInteger(0)); }
LispObj ratioToShortFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	double d_temp2 = 0.0;
	d_temp1 = getFloat(ratioNumerator(n));
	d_temp2 = getFloat(ratioDenominator(n));
	d_temp1 = d_temp1 / d_temp2;
	return createShortFloat(d_temp1);
}
LispObj ratioToSingleFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	double d_temp2 = 0.0;
	d_temp1 = getFloat(ratioNumerator(n));
	d_temp2 = getFloat(ratioDenominator(n));
	d_temp1 = d_temp1 / d_temp2;
	return singleFloatNode(d_temp1); 
}
LispObj ratioToDoubleFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	double d_temp2 = 0.0;
	d_temp1 = getFloat(ratioNumerator(n));
	d_temp2 = getFloat(ratioDenominator(n));
	d_temp1 = d_temp1 / d_temp2;
	return doubleFloatNode(d_temp1); 
}
LispObj ratioToComplex(LispObj n) { return complexNode(n, wrapInteger(0)); }
LispObj shortToSingleFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	d_temp1 = shortFloat(n);
	return singleFloatNode(d_temp1); 
}
LispObj shortFloatToDoubleFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	d_temp1 = shortFloat(n);
	return doubleFloatNode(d_temp1); 
}
LispObj shortToComplex(LispObj n) { return complexNode(n, wrapInteger(0)); }
LispObj singleFloatToDoubleFloat(LispObj n) 
{ 
	double d_temp1 = 0.0;
	d_temp1 = singleFloat(n);
	return doubleFloatNode(d_temp1); 
}
LispObj singleToComplex(LispObj n) { return complexNode(n, wrapInteger(0)); }
LispObj doubleToComplex(LispObj n) { return complexNode(n, wrapInteger(0)); }
LispObj anyToNotNumber(LispObj n) { return n; }

typedef LispObj (*NumberPromotionMethod)(LispObj);
NumberPromotionMethod promotionTable[NumMaxIDs][NumMaxIDs] =
{
	{	0,	fixnumToBignum,	fixnumToRatio, fixnumToShortFloat, fixnumToSingleFloat, fixnumToDoubleFloat, fixnumToComplex, anyToNotNumber },
	{	0,				 0,	bignumToRatio, bignumToShortFloat, bignumToSingleFloat, bignumToDoubleFloat, bignumToComplex, anyToNotNumber },
	{	0,				 0,			    0,  ratioToShortFloat,  ratioToSingleFloat,  ratioToDoubleFloat, ratioToComplex, anyToNotNumber },
	{	0,				 0,			    0,					0,  shortToSingleFloat,  shortToDoubleFloat, shortToComplex, anyToNotNumber },
	{	0,				 0,			    0,					0,					 0,  singleFloatToDoubleFloat, singleToComplex, anyToNotNumber },
	{	0,				 0,			    0,					0,					 0,					   0, doubleToComplex, anyToNotNumber },
	{	anyToNotNumber,	 anyToNotNumber, anyToNotNumber, anyToNotNumber, anyToNotNumber, anyToNotNumber, anyToNotNumber, anyToNotNumber }
};

LispObj promoteNumber(LispObj n, int type1, int type2)
{
	return promotionTable[type1][type2](n);
}

LispObj subtractFixnums(LispObj n1, LispObj n2)
{
	// attempt fixnum subtract
	LispObj result = n1;
	__asm mov eax, dword ptr n2
	__asm sub dword ptr result, eax
	__asm jo do_bignum
	return result;
do_bignum:
	n1 = promoteNumber(n1, FixnumID, BignumID);
	n2 = promoteNumber(n2, FixnumID, BignumID);
	return subtractMethods[BignumID](n1, n2);
}

LispObj subtractBignums(LispObj n1, LispObj n2)
{
	return bignumSubtract(n1, n2);
}

LispObj subtractShortFloats(LispObj n1, LispObj n2)
{
	double t1 = 0.0;
	double t2 = 0.0;
	t1 = shortFloat(n1);
	t2 = shortFloat(n2);
	t1 = t1 - t2;
	return createShortFloat(t1);
}

LispObj subtractSingleFloats(LispObj n1, LispObj n2)
{
	return subSingleSingle(n1, n2);
}

LispObj subtractDoubleFloats(LispObj n1, LispObj n2)
{
	return subDoubleDouble(n1, n2);
}

LispObj subtractComplex(LispObj n1, LispObj n2)
{
	LispObj t1 = 0;
	LispObj t2 = 0;
	t1 = _Subtract(complexReal(n1), complexReal(n2));
	t2 = _Subtract(complexImaginary(n1), complexImaginary(n2));
	return createComplex(t1, t2);
}

LispObj subtractNonNumeric(LispObj n1, LispObj n2)
{
	Error("Cannot call function '-' with these operands: ~A and ~A", n1, n2);
	return 0;		// doesn't get here
}

SubractMethod subtractMethods[NumMaxIDs] =
{
	subtractFixnums,
	subtractBignums,
	subtractRatios,
	subtractShortFloats,
	subtractSingleFloats,
	subtractDoubleFloats,
	subtractComplex,
	subtractNonNumeric
};

LispObj _Subtract(LispObj n1, LispObj n2)
{
	LispObj result = n1;
	int type1 = 0;
	int type2 = 0;

	// optimize for fixnums
	if (both_fixnums(n1, n2))
	{
		// attempt fixnum subtract
		__asm mov eax, dword ptr n2
		__asm sub dword ptr result, eax
		__asm jo do_bignum
		return result;
	do_bignum:
		return bignumSubtract(fixnumToBignum(n1), fixnumToBignum(n2));
	}

	// check types of both args
	type1 = numType(n1);
	type2 = numType(n2);

	if (type1 == type2)
		return subtractMethods[type1](n1, n2);

	// need to promote one of the numbers
	if (type1 < type2)
	{
		n1 = promoteNumber(n1, type1, type2);
		return subtractMethods[type2](n1, n2);
	}
	else
	{
		n2 = promoteNumber(n2, type2, type1);
		return subtractMethods[type1](n1, n2);
	}
}

#if 0
LispObj _Subtract(LispObj n1, LispObj n2)
{
	LispObj result = n1;
	double t1 = 0.0;
	double t2 = 0.0;

	if (both_fixnums(n1, n2))
	{
		// attempt fixnum subtract
		__asm mov eax, dword ptr n2
		__asm sub dword ptr result, eax
		__asm jo do_bignum
		return result;
	}

	if (both_shortFloats(n1, n2))
	{
		t1 = shortFloat(n1);
		t2 = shortFloat(n2);
		t1 = t1 - t2;
		return createShortFloat(t1);
	}

	if (isFixnum(n1))
	{
		if		(isDoubleFloat(n2))	return subFixnumDouble(n1, n2);
		else if (isSingleFloat(n2)) return subFixnumSingle(n1, n2);
		else if (isShortFloat(n2))  return subFixnumShort(n1, n2);
		else if (isRatio(n2))		return subtractRatios(n1, n2);
		else if (isBignum(n2))		return bignumSubtract(fixnumToBignum(n1), n2);
		else if (isComplex(n2))		return createComplex(_Subtract(n1, complexReal(n2)),
										_Subtract(0, complexImaginary(n2)));
		else goto error;
	}
	else
	if (isRatio(n1))
	{
		if		(isLispInteger(n2))	return subtractRatios(n1, n2);
		else if (isRatio(n2))		return subtractRatios(n1, n2);
		else if (isDoubleFloat(n2))	return subDoubleDouble(convertToDoubleFloat(n1), n2);
		else if (isSingleFloat(n2))	return subSingleSingle(convertToSingleFloat(n1), n2);
		else if (isShortFloat(n2))	return subShortShort(convertToShortFloat(n1), n2);
		else if (isComplex(n2))		return createComplex(_Subtract(n1, complexReal(n2)), complexImaginary(n2));
		else goto error;
	}
	else
	if (isDoubleFloat(n1))
	{
		if		(isComplex(n2))		return createComplex(_Subtract(n1, complexReal(n2)),
											_Subtract(doubleFloatNode(0.0), complexImaginary(n2)));
		else						return subDoubleDouble(n1, convertToDoubleFloat(n2));
	}
	else
	if (isSingleFloat(n1))
	{
		if		(isComplex(n2))		return createComplex(_Subtract(n1, complexReal(n2)),
											_Subtract(singleFloatNode(0.0), complexImaginary(n2)));
		else if	(isDoubleFloat(n2))	return subSingleDouble(n1, n2);
		else if (isSingleFloat(n2))	return subSingleSingle(n1, n2);
		else if (isShortFloat(n2))	return subSingleShort(n1, n2);
		else						return subSingleSingle(n1, convertToSingleFloat(n2));
	}
	else
	if (isShortFloat(n1))
	{
		if		(isComplex(n2))		return createComplex(_Subtract(n1, complexReal(n2)),
											_Subtract(createShortFloat(0.0), complexImaginary(n2)));
		else if (isDoubleFloat(n2)) return subShortDouble(n1, n2);
		else if (isSingleFloat(n2)) return subShortSingle(n1, n2);
		else if (isShortFloat(n2))	return subShortShort(n1, n2);
		else						return subShortShort(n1, convertToShortFloat(n2));
	}
	else
	if (isBignum(n1))
	{
		if		(isFixnum(n2))		return bignumSubtract(n1, fixnumToBignum(n2));
		else if (isRatio(n2))		return subtractRatios(n1, n2);
		else if (isDoubleFloat(n2))	return subDoubleDouble(convertToDoubleFloat(n1), n2);
		else if (isSingleFloat(n2))	return subSingleSingle(convertToSingleFloat(n1), n2);
		else if (isShortFloat(n2))	return subShortShort(convertToShortFloat(n1), n2);
		else if (isBignum(n2))		return bignumSubtract(n1, n2);
		else if (isComplex(n2))		return createComplex(_Subtract(n1, complexReal(n2)), complexImaginary(n2));
		else goto error;
	}
	else
	if (isComplex(n1))
	{
		if		(isComplex(n2))		return createComplex(_Subtract(complexReal(n1), complexReal(n2)),
												_Subtract(complexImaginary(n1), complexImaginary(n2)));
		else						return createComplex(_Subtract(complexReal(n1), n2), complexImaginary(n1));
	}

error:
	Error("Cannot call function '-' with these operands: ~A and ~A", n1, n2);

do_bignum:
	return bignumSubtract(fixnumToBignum(n1), fixnumToBignum(n2));
}
#endif

LispObj _Multiply(LispObj n1, LispObj n2)
{
	LispObj result = 0;

	if (both_fixnums(n1, n2))
	{
		// attempt fixnum multiply
		__asm mov eax, dword ptr n1
		__asm shr eax, 3
		__asm imul dword ptr n2
		__asm jo do_bignum
		__asm mov [result], eax

		return result;
	}

	if (both_shortFloats(n1, n2))
		return mulShortShort(n1, n2);

	if (isFixnum(n1))
	{
		if		(isDoubleFloat(n2)) return mulFixnumDouble(n1, n2);
		else if (isSingleFloat(n2)) return mulFixnumSingle(n1, n2);
		else if (isShortFloat(n2))  return mulFixnumShort (n1, n2);
		else if (isRatio(n2))		return multiplyRatios(n1, n2);
		else if (isBignum(n2))		return bignumMultiply(fixnumToBignum(n1), n2);
		else if (isComplex(n2))		return multiplyComplexNumbers(n1, n2);
		else goto error;
	}
	else
	if (isRatio(n1))
	{
		if		(isLispInteger(n2))	return multiplyRatios(n1, n2);
		else if (isRatio(n2))		return multiplyRatios(n1, n2);
		else if (isDoubleFloat(n2)) return mulDoubleDouble(convertToDoubleFloat(n1), n2);
		else if (isSingleFloat(n2))	return mulSingleSingle(convertToSingleFloat(n1), n2);
		else if (isShortFloat(n2))	return mulShortShort(convertToShortFloat(n1), n2);
		else if (isComplex(n2))		return multiplyComplexNumbers(n1, n2);
		else goto error;
	}
	else
	if (isDoubleFloat(n1))
	{
		if (isComplex(n2))			return multiplyComplexNumbers(n1, n2);
		else						return mulDoubleDouble(n1, convertToDoubleFloat(n2));
	}
	else
	if (isSingleFloat(n1))
	{
		if		(isComplex(n2))			return multiplyComplexNumbers(n1, n2);
		else if (isDoubleFloat(n2))		return mulSingleDouble(n1, n2);
		else if (isSingleFloat(n2))		return mulSingleSingle(n1, n2);
		else if (isShortFloat(n2))		return mulSingleShort(n1, n2);
		else							return mulSingleSingle(n1, convertToSingleFloat(n2));
	}
	else
	if (isShortFloat(n1))
	{
		if		(isComplex(n2))			return multiplyComplexNumbers(n1, n2);
		else if (isDoubleFloat(n2))		return mulShortDouble(n1, n2);
		else if (isSingleFloat(n2))		return mulShortSingle(n1, n2);
		else if (isShortFloat(n2))		return mulShortShort(n1, n2);
		else							return mulShortShort(n1, convertToShortFloat(n2));
	}
	else
	if (isBignum(n1))
	{
		if		(isFixnum(n2))			return bignumMultiply(n1, fixnumToBignum(n2));
		else if (isRatio(n2))			return multiplyRatios(n1, n2);
		else if (isDoubleFloat(n2))		return mulDoubleDouble(convertToDoubleFloat(n1), n2);
		else if (isSingleFloat(n2))		return mulSingleSingle(convertToSingleFloat(n1), n2);
		else if (isShortFloat(n2))		return mulShortShort(convertToShortFloat(n1), n2);
		else if (isBignum(n2))			return bignumMultiply(n1, n2);
		else if (isComplex(n2))			return multiplyComplexNumbers(n1, n2);
		else goto error;
	}
	else
	if (isComplex(n1))
		return multiplyComplexNumbers(n1, n2);
error:
	Error("Cannot call function '*' with these operands: ~A and ~A", n1, n2);

do_bignum:
	return bignumMultiply(fixnumToBignum(n1), fixnumToBignum(n2));
}

long isDoubleZero(LispObj d) {return (doubleFloat(d) == 0.0) ? 1 : 0;}
long isSingleZero(LispObj d) {return (singleFloat(d) == 0.0) ? 1 : 0;}
long isShortZero(LispObj d)  {return (shortFloat(d)  == 0.0) ? 1 : 0;}

LispObj _Divide(LispObj n1, LispObj n2)
{
	if (isFixnum(n2))
	{
		if (n2 == 0)
			Error("Divide by zero error: ~A / ~A", n1, n2);

		if		(isFixnum(n1))		return simplifyRatio(n1, n2);
		else if (isDoubleFloat(n1)) return divDoubleFixnum(n1, n2);
		else if (isSingleFloat(n1)) return divSingleFixnum(n1, n2);
		else if (isShortFloat(n1))	return divShortFixnum(n1, n2);
		else if (isRatio(n1))		return divideRatios(n1, n2);
		else if (isBignum(n1))		return simplifyRatio(n1, n2);
		else if (isComplex(n1))		return divideComplexNumbers(n1, n2);
		else goto error;
	}
	else
	if (isRatio(n2))
	{
		if		(isLispInteger(n1))	return divideRatios(n1, n2);
		else if (isRatio(n1))		return divideRatios(n1, n2);
		else if (isDoubleFloat(n1))	return divDoubleDouble(n1, convertToDoubleFloat(n2));
		else if (isSingleFloat(n1))	return divSingleSingle(n1, convertToSingleFloat(n2));
		else if (isShortFloat(n1))	return divShortShort(n1, convertToShortFloat(n2));
		else if (isComplex(n1))		return divideComplexNumbers(n1, n2);
		else goto error;
	}
	else
	if (isDoubleFloat(n2))
	{
		if (isDoubleZero(n2))
			Error("Divide by zero error: ~A / ~A", n1, n2);
		if (isComplex(n1))
			return divideComplexNumbers(n1, n2);
		else
			return divDoubleDouble(convertToDoubleFloat(n1), n2);
	}
	else
	if (isSingleFloat(n2))
	{
		if (isSingleZero(n2))
			Error("Divide by zero error: ~A / ~A", n1, n2);

		if		(isComplex(n1))		return divideComplexNumbers(n1, n2);
		else if (isDoubleFloat(n1))	return divDoubleSingle(n1, n2);
		else if (isSingleFloat(n1))	return divSingleSingle(n1, n2);
		else if (isShortFloat(n1))	return divShortSingle(n1, n2);
		else						return divSingleSingle(convertToSingleFloat(n1), n2);
	}
	else
	if (isShortFloat(n2))
	{
		if (isShortZero(n2))
			Error("Divide by zero error: ~A / ~A", n1, n2);

		if		(isComplex(n1))		return divideComplexNumbers(n1, n2);
		else if (isDoubleFloat(n1))	return divDoubleShort(n1, n2);
		else if (isSingleFloat(n1))	return divSingleShort(n1, n2);
		else if (isShortFloat(n1))	return divShortShort(n1, n2);
		else return divShortShort(convertToShortFloat(n1), n2);
	}
	else
	if (isBignum(n2))
	{
		if		(isFixnum(n1))		return simplifyRatio(n1, n2);
		else if (isRatio(n1))		return divideRatios(n1, n2);
		else if (isDoubleFloat(n1))	return divDoubleDouble(n1, convertToDoubleFloat(n2));
		else if (isSingleFloat(n1))	return divSingleSingle(n1, convertToSingleFloat(n2));
		else if (isShortFloat(n1))	return divShortShort(n1, convertToShortFloat(n2));
		else if (isBignum(n1))		return simplifyRatio(n1, n2);
		else if (isComplex(n1))		return divideComplexNumbers(n1, n2);
		else goto error;
	}
	else
	if (isComplex(n2))
		return divideComplexNumbers(n1, n2);
error:
	Error("Cannot call function '/' with these operands: ~A and ~A", n1, n2);

	return 0;	// never gets here
}

static LispObj
_Mod(LispObj n1, LispObj n2)
{
	long t1 = 0;
	long t2 = 0;
	long abst2 = 0;
	long res = 0;
	LispObj ret = 0;
	LispObj result = 0;
	LispObj rem = 0;

	if (isZero(n2))
		Error("Division by zero error: ~A / ~A", n1, n2);

	if (both_fixnums(n1, n2))
	{
		t1 = integer(n1);
		t2 = integer(n2);
		abst2 = abs(t2);
		res = abs(t1) % abst2;
		if (res && ((t1 < 0 ? 1 : 0) ^ (t2 < 0 ? 1 : 0)))
			res = abst2 - res;
		if (t2 < 0)
			res = -res;
		ret = wrapInteger(res);
	}
	else
	{
		result = _Floor(_Divide(n1, n2));
		rem = _Subtract(n1, _Multiply(result, n2));
		ret = rem;
	}

	return ret;
}

LispObj lispNumericEqual(LispObj n1, LispObj n2)
{
	long ret = 0;
	if (isFixnum(n1))
	{
			 if (isFixnum(n2))		ret = (n1 == n2);
		else if (isRatio(n2))		ret = compareRatios(n1, n2) == 0;
		else if (isDoubleFloat(n2))	ret = (((double)integer(n1)) == doubleFloat(n2));
		else if (isSingleFloat(n2))	ret = (((double)integer(n1)) == singleFloat(n2));
		else if (isShortFloat(n2))	ret = (((double)integer(n1)) == shortFloat(n2));
		else if (isBignum(n2))		ret = (bignumCompare(fixnumToBignum(n1), n2) == 0);
		else if (isComplex(n2))		ret = equalComplexNumbers(n1, n2);
		else						Error("Invalid number: ~A", n2);
	}
	else
	if (isRatio(n1))
	{
			 if (isLispInteger(n2) || isRatio(n2))
									ret = compareRatios(n1, n2) == 0;
		else if (isDoubleFloat(n2))	ret = (getFloat(n1) == doubleFloat(n2));
		else if (isSingleFloat(n2))	ret = (getFloat(n1) == singleFloat(n2));
		else if (isShortFloat(n2))	ret = (getFloat(n1) == shortFloat(n2));
		else if (isComplex(n2))		ret = equalComplexNumbers(n1, n2);
		else						Error("Invalid number: ~A", n2);
	}
	else
	if (isDoubleFloat(n1))
	{
		if (isComplex(n2))			ret = equalComplexNumbers(n1, n2);
		else						ret = doubleFloat(n1) == getFloat(n2);
	}
	else
	if (isSingleFloat(n1))
	{
		if (isComplex(n2))			ret = equalComplexNumbers(n1, n2);
		else						ret = singleFloat(n1) == getFloat(n2);
	}
	else
	if (isShortFloat(n1))
	{
		if (isComplex(n2))			ret = equalComplexNumbers(n1, n2);
		else						ret = shortFloat(n1) == getFloat(n2);
	}
	else
	if (isBignum(n1))
	{
			 if (isFixnum(n2))		ret = (bignumCompare(n1, fixnumToBignum(n2)) == 0);
		else if (isRatio(n2))		ret = compareRatios(n1, n2) == 0;
		else if (isDoubleFloat(n2)) ret = (bignumToDouble(n1) == doubleFloat(n2));
		else if (isSingleFloat(n2))	ret = (bignumToDouble(n1) == singleFloat(n2));
		else if (isShortFloat(n2))	ret = (bignumToDouble(n1) == shortFloat(n2));
		else if (isBignum(n2))		ret = (bignumCompare(n1, n2) == 0);
	}
	else
	if (isComplex(n1))
	{
		ret = equalComplexNumbers(n1, n2);
	}
	else
		Error("Invalid number: ~A", n1);

	return ret ? T : NIL;
}

LispObj lispGreater(LispObj n1, LispObj n2)
{
	LispObj result = NIL;

	if (both_fixnums(n1, n2))
	{
		// both fixnums
		if (((long)n1) > ((long)n2))
			result = T;
		else
			result = NIL;
	}
	else
		result = (numCompare(n1, n2) == 1 ? T : NIL);
	return result;
}

LispObj lispGreaterEqual(LispObj n1, LispObj n2)
{
	LispObj result = NIL;

	if (both_fixnums(n1, n2))
	{
		// both fixnums
		if (((long)n1) >= ((long)n2))
			result = T;
		else
			result = NIL;
	}
	else
		result = (numCompare(n1, n2) != -1 ? T : NIL);

	return result;
}

LispObj lispLess(LispObj n1, LispObj n2)
{
	LispObj result = NIL;

	if (both_fixnums(n1, n2))
	{
		// both fixnums
		if (((long)n1) < ((long)n2))
			result = T;
		else
			result = NIL;
	}
	else
		result = (numCompare(n1, n2) == -1 ? T : NIL);

	return result;
}

LispObj lispLessEqual(LispObj n1, LispObj n2)
{
	LispObj result = NIL;

	if (both_fixnums(n1, n2))
	{
		// both fixnums
		if (((long)n1) <= ((long)n2))
			result = T;
		else
			result = NIL;
	}
	else
		result = (numCompare(n1, n2) != 1 ? T : NIL);

	return result;
}

static LispObj _Negate(LispObj n)
{
	return _Subtract(0, n);
}

LispFunction(Plus)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj x = 0;
	long i = 0;
	if (ARG_COUNT > 0)
	{
		x = LISP_ARG(0);
		for (i = 1; i < ARG_COUNT; i++)
			x = _Add(x, LISP_ARG(i));
	}
	LISP_FUNC_RETURN(x);
}

LispFunction(Minus)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj x = LISP_ARG(0);
	long i = 0;

	if (ARG_COUNT == 1)
		x = _Negate(x);
	else
	{
		for (i = 1; i < ARG_COUNT; i++)
			x = _Subtract(x, LISP_ARG(i));
	}
	LISP_FUNC_RETURN(x);
}

LispFunction(Multiply)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj x = wrapInteger(1);
	long i = 0;

	if (ARG_COUNT > 0)
	{
		x = LISP_ARG(0);
		for (i = 1; i < ARG_COUNT; i++)
			x = _Multiply(x, LISP_ARG(i));
	}
	LISP_FUNC_RETURN(x);
}

LispFunction(Divide)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj x = LISP_ARG(0);
	long i = 0;

	if (ARG_COUNT == 1)
		x = _Divide(wrapInteger(1), x);
	else
	{
		for (i = 1; i < ARG_COUNT; i++)
			x = _Divide(x, LISP_ARG(i));
	}
	LISP_FUNC_RETURN(x);
}

//
//	Common Lisp TRUNCATE function.
//
LispFunction(Truncate)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj number = LISP_ARG(0);
	LispObj second = 0;
	LispObj res = 0;
	LispObj rem = 0;

	if (ARG_COUNT == 2)
	{
		second = LISP_ARG(1);
		checkReal(second);
		if (isZero(second))
			Error("Divide by zero error: ~A / ~A", number, second);
		res = _Truncate(_Divide(number, second));
		rem = _Subtract(number, _Multiply(res, second));
	}
	else
	{
		res = _Truncate(number);
		rem = _Subtract(number, res);
	}
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = cons(res, cons(rem, NIL));
	ret = res;

	ReturnCount(2);
	return ret;
}

//
//	Common Lisp CEILING function.
//
LispFunction(Ceiling)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj number = LISP_ARG(0);
	LispObj second = 0;
	LispObj res = 0;
	LispObj rem = 0;

	if (ARG_COUNT == 2)
	{
		second = LISP_ARG(1);
		checkReal(second);
		if (isZero(second))
			Error("Divide by zero error: ~A / ~A", number, second);
		res = _Ceiling(_Divide(number, second));
		rem = _Subtract(number, _Multiply(res, second));
	}
	else
	{
		res = _Ceiling(number);
		rem = _Subtract(number, res);
	}
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = cons(res, cons(rem, NIL));
	ret = res;

	ReturnCount(2);
	return ret;
}

//
//	Common Lisp ROUND function.
//
LispFunction(Round)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj number = LISP_ARG(0);
	LispObj second = 0;
	LispObj res = 0;
	LispObj rem = 0;

	if (ARG_COUNT == 2)
	{
		second = LISP_ARG(1);
		checkReal(second);
		if (isZero(second))
			Error("Divide by zero error: ~A / ~A", number, second);
		res = _Round(_Divide(number, second));
		rem = _Subtract(number, _Multiply(res, second));
	}
	else
	{
		res = _Round(number);
		rem = _Subtract(number, res);
	}
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = cons(res, cons(rem, NIL));
	ret = res;

	ReturnCount(2);
	return ret;
}

LispFunction(NumericEqual)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj x = 0;
	LispObj y = 0;
	LispObj result = T;
	long i = 0;

	if (ARG_COUNT >= 2)
	{
		x = LISP_ARG(0);
		checkNumber(x);
		for (i = 1; i < ARG_COUNT; i++)
		{
			y = LISP_ARG(i);
			checkNumber(y);
			if (lispNumericEqual(x, y) == NIL)
			{
				result = NIL;
				break;
			}
		}
	}
	LISP_FUNC_RETURN(result);
}

LispFunction(Greater)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj x = 0;
	LispObj result = T;
	long i = 0;

	if (ARG_COUNT >= 2)
	{
		x = LISP_ARG(0);
		for (i = 1; i < ARG_COUNT; i++)
		{
			if (lispGreater(x, LISP_ARG(i)) == NIL)
			{
				result = NIL;
				break;
			}
			x = LISP_ARG(i);
		}
	}
	LISP_FUNC_RETURN(result);
}

LispFunction(GreaterEqual)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj x = 0;
	LispObj result = T;
	long i = 0;

	if (ARG_COUNT >= 2)
	{
		x = LISP_ARG(0);
		for (i = 1; i < ARG_COUNT; i++)
		{
			if (lispGreaterEqual(x, LISP_ARG(i)) == NIL)
			{
				result = NIL;
				break;
			}
			x = LISP_ARG(i);
		}
	}
	LISP_FUNC_RETURN(result);
}

LispFunction(Less)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj x = 0;
	LispObj result = T;
	long i = 0;

	if (ARG_COUNT >= 2)
	{
		x = LISP_ARG(0);
		for (i = 1; i < ARG_COUNT; i++)
		{
			if (lispLess(x, LISP_ARG(i)) == NIL)
			{
				result = NIL;
				break;
			}
			x = LISP_ARG(i);
		}
	}
	LISP_FUNC_RETURN(result);
}

LispFunction(LessEqual)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj x = 0;
	LispObj result = T;
	long i = 0;

	if (ARG_COUNT >= 2)
	{
		x = LISP_ARG(0);
		for (i = 1; i < ARG_COUNT; i++)
		{
			if (lispLessEqual(x, LISP_ARG(i)) == NIL)
			{
				result = NIL;
				break;
			}
			x = LISP_ARG(i);
		}
	}
	LISP_FUNC_RETURN(result);
}

LispFunction(NotEqual)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj x = 0;
	LispObj result = T;
	long i = 0;

	if (ARG_COUNT >= 2)
	{
		x = LISP_ARG(0);
		for (i = 1; i < ARG_COUNT; i++)
		{
			if (lispNumericEqual(x, LISP_ARG(i)) != NIL)
			{
				result = NIL;
				break;
			}
			x = LISP_ARG(i);
		}
	}
	LISP_FUNC_RETURN(result);
}

//
//	Common Lisp MOD function.
//
LispFunction(Mod)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);
	checkReal(n1);
	checkReal(n2);
	ret = _Mod(n1, n2);

	LISP_FUNC_RETURN(ret);
}

//
//	Common Lisp FLOOR function.
//
LispFunction(Floor)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj number = LISP_ARG(0);
	LispObj second = NIL;
	LispObj res = 0;
	LispObj rem = 0;

	if (ARG_COUNT == 2)
		second = LISP_ARG(1);

	checkReal(number);

	if (second != NIL)
	{
		checkReal(second);
		if (isZero(second))
			Error("Divide by zero error: ~A / ~A", number, second);
		res = _Floor(_Divide(number, second));
		rem = _Subtract(number, _Multiply(res, second));
	}
	else
	{
		res = _Floor(number);
		rem = _Subtract(number, res);
	}
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = cons(res, cons(rem, NIL));
	ret = res;

	ReturnCount(2);
	return ret;
}

LispFunction(Gcd)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	long i = 0;
	if (ARG_COUNT == 0)
		ret = 0;
	else
	if (ARG_COUNT == 1)
		ret = _Abs(LISP_ARG(0));
	else
	{
		ret = LISP_ARG(0);
		for (i = 1; i < ARG_COUNT; i++)
			ret = gcdNumbers(ret, LISP_ARG(i));
	}
	LISP_FUNC_RETURN(ret);
}

// operations on bignums

//  bignumLength()
//	Returns the number of 32-bit significant words.
//
typedef unsigned long BignumWord;
static long
bignumLength(LispObj bn)
{
	BignumWord* buf = 0;
	BignumWord* p = 0;
	long len = bignumNumCells(bn);
	if (len == 0)
		return 0;
	buf = bignumStart(bn);
	for (p = buf + len - 1; p > buf; p--)
		if (*p) break;
	return (p - buf) + 1;
}

//	bignumExpand()
//	Returns a bignum which has been expanded to fill the requested number
//	of 32-bit words by adding zeros to the beginning. Assumes the passed
//	word is smaller or the same size as the requested length.
//
LispObj
bignumExpand(LispObj bn, LispObj length)
{
	LispObj ret = 0;
	BignumWord* b1 = 0;
	BignumWord* b2 = 0;
	long n = bignumLength(bn);
	long i = 0;

	ret = bignumNode(length);
	b1 = bignumStart(bn);
	b2 = bignumStart(ret);
	for (i = 0; i < n; i++)
		b2[i] = b1[i];
	if (bignumNegative(bn))
		bignumNegate(ret);
	return ret;
}

static LispObj
bignumAdd(LispObj b1, LispObj b2)
{
	long resSign = 0;
	long b1Length = bignumLength(b1);
	long b2Length = bignumLength(b2);
	long destSize = 0;
	LispObj result = 0;

	if (bignumNegative(b1))
	{
		if (bignumNegative(b2))
			resSign = 1;
		else
		{
			bignumNegate(b1);
			result = bignumSubtract(b2, b1);
			bignumNegate(b1);
			return result;
		}
	}
	else
	if (bignumNegative(b2))
	{
		bignumNegate(b2);
		result = bignumSubtract(b1, b2);
		bignumNegate(b2);
		return result;
	}

	destSize = (b1Length < b2Length ? b2Length : b1Length) + 1;
	result = bignumNode(wrapInteger(destSize));
	if (b1Length < b2Length)
		addBignumWords(b2, b2Length, b1, b1Length, result);
	else
		addBignumWords(b1, b1Length, b2, b2Length, result);
	if (resSign)
		bignumSetSign(result, resSign);
	return normalizeBignum(result);
}

static LispObj
bignumSubtract(LispObj b1, LispObj b2)
{
	long resSign = 0;
	long b1Length = bignumLength(b1);
	long b2Length = bignumLength(b2);
	long destSize = 0;
	LispObj result = 0;
	long b1Sign = bignumNegative(b1);
	long b2Sign = bignumNegative(b2);

	if ((b1Sign && !b2Sign) || (!b1Sign && b2Sign))
	{
		bignumNegate(b2);
		result = bignumAdd(b1, b2);
		bignumNegate(b2);
		return result;
	}
	destSize = b1Length < b2Length ? b2Length : b1Length;
	result = bignumNode(wrapInteger(destSize));

	if (bignumAbsCompare(b1, b2) >= 0)
	{
		if (!b1Sign)
			resSign = 0;
		else
			resSign = 1;
		subBignumWords(b1, b1Length, b2, b2Length, result);
	}
	else
	{
		if (!b1Sign)
			resSign = 1;
		else
			resSign = 0;
		subBignumWords(b2, b2Length, b1, b1Length, result);
	}

	if (resSign)
		bignumSetSign(result, resSign);

	return normalizeBignum(result);
}

static LispObj
bignumMultiply(LispObj b1, LispObj b2)
{
	long resSign = 0;
	long b1Length = bignumLength(b1);
	long b2Length = bignumLength(b2);
	long destSize = 0;
	LispObj result = 0;
	long b1Sign = bignumNegative(b1);
	long b2Sign = bignumNegative(b2);

	if ((!b1Sign && !b2Sign) || (b1Sign && b2Sign))
		resSign = 0;
	else
		resSign = 1;

	destSize = b1Length + b2Length;
	result = bignumNode(wrapInteger(destSize));

	if (b1Length < b2Length)
		mulBignumWords(b2, b2Length, b1, b1Length, result);
	else
		mulBignumWords(b1, b1Length, b2, b2Length, result);
	if (resSign)
		bignumSetSign(result, resSign);

	return normalizeBignum(result);
}

static LispObj
bignumDivide(LispObj b1, LispObj b2)
{
	return divideBignums(b1, b2, 0);
}

static LispObj
bignumMod(LispObj b1, LispObj b2)
{
	return CDR(divideBignums(b1, b2, 1));
}

//
//	addBignumWords()
//	Assumes b2Len <= b1Len.
//
static void
addBignumWords(LispObj b1, long b1Len, LispObj b2, long b2Len, LispObj result)
{
	BignumWord* b1ptr = bignumStart(b1);
	BignumWord* b2ptr = bignumStart(b2);
	BignumWord* rptr = bignumStart(result);

	__asm
	{
		push ecx
		push edi
		push esi
		push ebx

		mov esi, dword ptr b1ptr
		mov ebx, dword ptr b2ptr
		mov edi, dword ptr rptr
		mov ecx, -1
		jmp short t1

	loop1:
		mov eax, dword ptr [esi + ecx*4]
		add eax, dword ptr [ebx + ecx*4]
		adc dword ptr [edi + ecx*4 + 4], 0
		add dword ptr [edi + ecx*4], eax
		adc dword ptr [edi + ecx*4 + 4], 0
	t1:
		inc ecx
		cmp ecx, dword ptr b2Len
		jl	short loop1

		dec ecx
		jmp short t2
	loop2:
		mov eax, dword ptr [esi + ecx*4]
		add dword ptr [edi + ecx*4], eax
		adc dword ptr [edi + ecx*4 + 4], 0
	t2:
		inc ecx
		cmp ecx, dword ptr b1Len
		jl	short loop2

		pop ebx
		pop esi
		pop edi
		pop ecx
	}
}

//
//	subBignumWords()
//	Assumes b2Len <= b1Len.
//
static void
subBignumWords(LispObj b1, long b1Len, LispObj b2, long b2Len, LispObj result)
{
	BignumWord* b1ptr = bignumStart(b1);
	BignumWord* b2ptr = bignumStart(b2);
	BignumWord* rptr = bignumStart(result);

	__asm
	{
		push ecx
		push edi
		push esi
		push ebx

		mov esi, dword ptr b1ptr
		mov ebx, dword ptr b2ptr
		mov edi, dword ptr rptr
		mov ecx, -1
		xor edx, edx		;;borrow flag
		jmp short t1

	loop1:
		mov eax, dword ptr [esi + ecx*4]
		sub	eax, edx
		mov	edx, 0
		adc edx, 0
		sub eax, dword ptr [ebx + ecx*4]
		adc edx, 0
		mov dword ptr [edi + ecx*4], eax
	t1:
		inc ecx
		cmp ecx, dword ptr b2Len
		jl	short loop1

		dec ecx
		jmp short t2
	loop2:
		mov eax, dword ptr [esi + ecx*4]
		sub	eax, edx
		mov	edx, 0
		adc edx, 0
		mov dword ptr [edi + ecx*4], eax
	t2:
		inc ecx
		cmp ecx, dword ptr b1Len
		jl	short loop2

		pop ebx
		pop esi
		pop edi
		pop ecx
	}
}

//
//	mulBignumWords()
//	Assumes b2Len <= b1Len.
//
static void
mulBignumWords(LispObj b1, long b1Len, LispObj b2, long b2Len, LispObj result)
{
	long i = 0;
	b1Len <<= 2;
	b2Len <<= 2;
	for (i = 0; i < b2Len; i += 4)
	{
		__asm
		{
			push	edi
			push	ebx
			push	ecx
			push	edx

			push	0								; [esp + 8] = carry
			push	0								; [esp + 4] = overflow
			mov		edi, i
			mov		edx, dword ptr b2
			push	dword ptr [edx + edi + 3]		; [esp] = b2[i]
			mov		ecx, 0							; j = 0
			mov		ebx, dword ptr result

			jmp		end_test
		loop1:
			mov		eax, b1
			mov		eax, [eax + ecx + 3]		; eax = b1[j]
			mul		dword ptr [esp]
			add		eax, [esp + 4]
			adc		edx, 0						; edx:eax = b1[j]*b2[i]+overflow
			mov		[esp + 4], edx
			add		eax, [esp + 8]				; add previous carry
			mov		[esp + 8], 0
			jnc		t3
			inc		[esp + 8]
		t3: mov		edi, i
			add		edi, ecx
			add		[ebx + edi + 3], eax
			jnc		t2
			inc		[esp + 8]
		t2:
			add		ecx, 4
		end_test:
			cmp		ecx, b1Len
			jl		loop1

			mov		edi, i
			add		edi, ecx
			mov		eax, [ebx + edi + 3]
			add		eax, [esp + 4]
			add		eax, [esp + 8]
			mov		[ebx + edi + 3], eax

			add		esp, 12
			pop		edx
			pop		ecx
			pop		ebx
			pop		edi
		}
	}
}

#if 0
//
//	divBignumWords()
//	Assumes b2Len <= b1Len.
//
static void
divBignumWords(LispObj b1, long b1Len, LispObj b2, long b2Len,
			   LispObj result, LispObj remainder,
			   LispObj t1, long t1Len,
			   LispObj t2, long t2Len)
{
	long i = 0;
	long highBit1 = 0;
	long highBit2 = 0;
	long shift = 0;

	BignumWord* b1ptr = bignumStart(b1);
	BignumWord* b2ptr = bignumStart(b2);
	BignumWord* rptr = bignumStart(result);
	BignumWord* remptr = bignumStart(remainder);
	BignumWord* shifted = bignumStart(t1);
	BignumWord* temp = bignumStart(t2);

	// make a copy of the second array for shifting purposes--
	// it must be the size of the first array.

	//remainder = b1
	for (i = 0; i < b1Len; i++)
		remptr[i] = b1ptr[i];

	while (bignumAbsCompare(remainder, b2) >= 0)
	{
		highBit1 = bignumHighBit(remainder);
		highBit2 = bignumHighBit(b2);
		shift = highBit1 - highBit2;
		shiftBignumWords(b2, b2Len, t1, b1Len, shift);

		// if we shifted too far, then back up 1 bit
		if (bignumAbsCompare(remainder, t1) < 0)
		{
			shift--;
			shiftBignumWords(b2, b2Len, t1, b1Len, shift);
		}
		for (i = 0; i < t2Len; i++)
			temp[i] = 0;
		subBignumWords(remainder, b1Len, t1, b1Len, t2);
		bignumSetBit(result, shift);
		// remainder = temp
		for (i = 0; i < b1Len; i++)
			remptr[i] = temp[i];
	}
}
#endif

static void
shiftBignumWords(LispObj b, long srcLen, LispObj res, long resultLen, long bits)
{
	long shiftDir = 0;
	long wordShift = 0;
	long bitShift = 0;
	enum { Left, Right };
	long i = 0;
	unsigned long n = 0;

	unsigned short* src = (unsigned short*)bignumStart(b);
	unsigned short* dest = (unsigned short*)bignumStart(res);
	srcLen *= 2;
	resultLen *= 2;

	// make sure the destination is clear
	for (i = 0; i < resultLen; i++)
		dest[i] = 0;

	if (bits < 0)
	{
		shiftDir = Right;
		bits = -bits;
	}
	else
		shiftDir = Left;

	wordShift = bits / 16;
	bitShift = bits % 16;

	if (shiftDir == Left)
	{
		for (i = 0; i < srcLen; i++)
		{
			n = src[i];
			n <<= bitShift;
			dest[i + wordShift + 1] += (unsigned short)(n >> 16);
			dest[i + wordShift] += (unsigned short)(n & 0xffff);
		}
	}
	else
	{
		// do first word
		if (!wordShift)
		{
			n = src[0];
			n >>= bitShift;
			dest[0] += (unsigned short)(n & 0xffff);
		}
		// do the rest
		for (i = 1; i < srcLen; i++)
		{
			n = src[i];
			n <<= 16;
			n >>= bitShift;
			if ((i - wordShift) >= 0)
				dest[i - wordShift] += (unsigned short)(n >> 16);
			if ((i - wordShift - 1) >= 0)
				dest[i - wordShift - 1] += (unsigned short)(n & 0xffff);
		}
	}
}

//
//	xorBignumWords()
//	Assumes b2Len <= b1Len.
//	Assumes both bignums are in signed magnitude format.
//
#define TWOS_COMP_WORD(x, tcword, carry) ((tcword) = ~(x) + (carry),	\
	(carry) = ((carry) && !(tcword)),									\
	tcword)

static void
xorBignumWords(LispObj b1, LispObj b1Length, LispObj b2, LispObj b2Length, LispObj result)
{
	BignumWord* b1ptr = 0;
	BignumWord* b2ptr = 0;
	BignumWord* rptr = 0;
	long i = 0;
	long b1Sign = 0;
	long b2Sign = 0;
	LispObj temp = 0;
	unsigned long carry1 = 1;
	unsigned long tcword1 = 0;
	unsigned long carry2 = 1;
	unsigned long tcword2 = 0;
	long b1Len = 0;
	long b2Len = 0;

	if (b1Length < b2Length)
	{
		temp = b1;
		b1 = b2;
		b2 = temp;
		temp = b1Length;
		b1Length = b2Length;
		b2Length = temp;
	}
	rptr = bignumStart(result);
	b1Sign = bignumNegative(b1);
	b2Sign = bignumNegative(b2);

	b1ptr = bignumStart(b1);
	b2ptr = bignumStart(b2);
	b1Len = integer(b1Length);
	b2Len = integer(b2Length);

	if (b1Sign == 0)
	{
		if (b2Sign == 0)		// b1 >=0, b2 >= 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = b1ptr[i] ^ b2ptr[i];
			for (; i < b1Len; i++)
				rptr[i] = b1ptr[i];
		}
		else					// b1 >= 0, b2 < 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = b1ptr[i] ^ TWOS_COMP_WORD(b2ptr[i], tcword2, carry2);
			for (; i < b1Len; i++)
				rptr[i] = ~b1ptr[i];
			Bignum_2CtoSM(result);
		}
	}
	else
	{
		if (b2Sign == 0)		// b1 < 0, b2 >= 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1) ^ b2ptr[i];
			for (; i < b1Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1);
			Bignum_2CtoSM(result);
		}
		else					// b1 < 0, b2 < 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1)
						^ TWOS_COMP_WORD(b2ptr[i], tcword2, carry2);
			for (; i < b1Len; i++)
				rptr[i] = ~TWOS_COMP_WORD(b1ptr[i], tcword1, carry1);
			Bignum_2CtoSM(result);
		}
	}
}

//
//	orBignumWords()
//	Assumes b2Len <= b1Len.
//
static void
orBignumWords(LispObj b1, LispObj b1Length, LispObj b2, LispObj b2Length, LispObj result)
{
	BignumWord* b1ptr = 0;
	BignumWord* b2ptr = 0;
	BignumWord* rptr = 0;
	long i = 0;
	long b1Sign = 0;
	long b2Sign = 0;
	LispObj temp = 0;
	unsigned long carry1 = 1;
	unsigned long tcword1 = 0;
	unsigned long carry2 = 1;
	unsigned long tcword2 = 0;
	long b1Len = 0;
	long b2Len = 0;

	if (b1Length < b2Length)
	{
		temp = b1;
		b1 = b2;
		b2 = temp;
		temp = b1Length;
		b1Length = b2Length;
		b2Length = temp;
	}
	rptr = bignumStart(result);
	b1Sign = bignumNegative(b1);
	b2Sign = bignumNegative(b2);

	b1ptr = bignumStart(b1);
	b2ptr = bignumStart(b2);
	b1Len = integer(b1Length);
	b2Len = integer(b2Length);

	if (b1Sign == 0)
	{
		if (b2Sign == 0)		// b1 >=0, b2 >= 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = b1ptr[i] | b2ptr[i];
			for (; i < b1Len; i++)
				rptr[i] = b1ptr[i];
		}
		else					// b1 >= 0, b2 < 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = b1ptr[i] | TWOS_COMP_WORD(b2ptr[i], tcword2, carry2);
			for (; i < b1Len; i++)
				rptr[i] = (LispObj)-1;
			Bignum_2CtoSM(result);
		}
	}
	else
	{
		if (b2Sign == 0)		// b1 < 0, b2 >= 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1) | b2ptr[i];
			for (; i < b1Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1);
			Bignum_2CtoSM(result);
		}
		else					// b1 < 0, b2 < 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1)
						| TWOS_COMP_WORD(b2ptr[i], tcword2, carry2);
			for (; i < b1Len; i++)
				rptr[i] = (LispObj)-1;
			Bignum_2CtoSM(result);
		}
	}
}

//
//	andBignumWords()
//	Assumes b2Len <= b1Len.
//
static void
andBignumWords(LispObj b1, LispObj b1Length, LispObj b2, LispObj b2Length, LispObj result)
{
	BignumWord* b1ptr = 0;
	BignumWord* b2ptr = 0;
	BignumWord* rptr = 0;
	long i = 0;
	long b1Sign = 0;
	long b2Sign = 0;
	LispObj temp = 0;
	unsigned long carry1 = 1;
	unsigned long tcword1 = 0;
	unsigned long carry2 = 1;
	unsigned long tcword2 = 0;
	long b1Len = 0;
	long b2Len = 0;

	if (b1Length < b2Length)
	{
		temp = b1;
		b1 = b2;
		b2 = temp;
		temp = b1Length;
		b1Length = b2Length;
		b2Length = temp;
	}
	rptr = bignumStart(result);
	b1Sign = bignumNegative(b1);
	b2Sign = bignumNegative(b2);

	b1ptr = bignumStart(b1);
	b2ptr = bignumStart(b2);
	b1Len = integer(b1Length);
	b2Len = integer(b2Length);

	if (b1Sign == 0)
	{
		if (b2Sign == 0)		// b1 >=0, b2 >= 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = b1ptr[i] & b2ptr[i];
			for (; i < b1Len; i++)
				rptr[i] = 0;
		}
		else					// b1 >= 0, b2 < 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = b1ptr[i] & TWOS_COMP_WORD(b2ptr[i], tcword2, carry2);
			for (; i < b1Len; i++)
				rptr[i] = b1ptr[i];
			//Bignum_2CtoSM(result); // result should always be positive
		}
	}
	else
	{
		if (b2Sign == 0)		// b1 < 0, b2 >= 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1) & b2ptr[i];
			for (; i < b1Len; i++)
				rptr[i] = 0;
			//Bignum_2CtoSM(result); // result should always be positive
		}
		else					// b1 < 0, b2 < 0
		{
			for (i = 0; i < b2Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1)
						& TWOS_COMP_WORD(b2ptr[i], tcword2, carry2);
			for (; i < b1Len; i++)
				rptr[i] = TWOS_COMP_WORD(b1ptr[i], tcword1, carry1);
			Bignum_2CtoSM(result);
		}
	}
}

//
//	notBignumWords()
//
static void
notBignumWords(LispObj b, LispObj bLength, LispObj result)
{
	BignumWord* bptr = 0;
	BignumWord* rptr = 0;
	long i = 0;
	long bSign = 0;
	unsigned long carry1 = 1;
	unsigned long tcword1 = 0;
	long bLen = 0;

	rptr = bignumStart(result);
	bSign = bignumNegative(b);

	bptr = bignumStart(b);
	bLen = integer(bLength);

	if (bSign == 0)				// b >= 0
	{
		for (i = 0; i < bLen; i++)
			rptr[i] = ~bptr[i];
	}
	else						// b < 0
	{
		for (i = 0; i < bLen; i++)
			rptr[i] = ~TWOS_COMP_WORD(bptr[i], tcword1, carry1);
	}
	Bignum_2CtoSM(result);
}
//
//	Bignum_SMto2C() converts (in place) a signed magnitude bignum to
//	a twos complement representation. Note that the sign bit is not
//	modified. It is assumed that the top-most bit of the signed magnitude
//	is free to hold the sign bit in the twos complement representation.
//
void
Bignum_SMto2C(LispObj bn)
{
	long sign = bignumNegative(bn);
	BignumWord* b = 0;
	long len = 0;
	long i = 0;

	if (sign)
	{
		b = bignumStart(bn);
		len = bignumNumCells(bn);
		// flip all the bits
		for (i = 0; i < len; i++)
			b[i] = ~b[i];
		// add one to the result
		i = 0;
		b[i]++;
		// propogate carry as necessary
		while (!b[i])
		{
			i++;
			b[i]++;
		}
	}
}

//
//	Bignum_2CtoSM() converts (in place) a twos complement bignum
//	to a signed magnitude representation.
//
void
Bignum_2CtoSM(LispObj bn)
{
	long len = bignumNumCells(bn);
	BignumWord* b = bignumStart(bn);
	long negative = (b[len - 1] & 0x80000000) ? 1 : 0;
	long i = 0;

	// if negative, convert it
	if (negative)
	{
		// flip all the bits
		for (i = 0; i < len; i++)
			b[i] = ~b[i];
		// add one to the result
		i = 0;
		b[i]++;
		// propogate carry as necessary
		while (!b[i])
		{
			i++;
			b[i]++;
		}
	}
	bignumSetSign(bn, negative);
}


//
//	This is currently only correct for unsigned integers
//
static LispObj
bignumXor(LispObj b1, LispObj b2)
{
	long b1Length = bignumLength(b1);
	long b2Length = bignumLength(b2);
	long destSize = (b1Length < b2Length ? b2Length : b1Length);
	LispObj result = 0;

	result = bignumNode(wrapInteger(destSize));
	xorBignumWords(b1, wrapInteger(b1Length), b2, wrapInteger(b2Length), result);
	return normalizeBignum(result);
}

//
//	This is currently only correct for unsigned integers
//
static LispObj
bignumIor(LispObj b1, LispObj b2)
{
	long b1Length = bignumLength(b1);
	long b2Length = bignumLength(b2);
	long destSize = (b1Length < b2Length ? b2Length : b1Length);
	LispObj result = 0;

	result = bignumNode(wrapInteger(destSize));
	orBignumWords(b1, wrapInteger(b1Length), b2, wrapInteger(b2Length), result);
	return normalizeBignum(result);
}

static LispObj
bignumAnd(LispObj b1, LispObj b2)
{
	long b1Length = bignumLength(b1);
	long b2Length = bignumLength(b2);
	long destSize = (b1Length < b2Length ? b2Length : b1Length);
	LispObj result = 0;

	result = bignumNode(wrapInteger(destSize));
	andBignumWords(b1, wrapInteger(b1Length), b2, wrapInteger(b2Length), result);
	return normalizeBignum(result);
}

static LispObj
bignumNot(LispObj b)
{
	long bLength = bignumLength(b);
	long destSize = bLength;
	LispObj result = 0;

	destSize += 1;		// need to make sure there is an extra high-order bit
	b = bignumExpand(b, wrapInteger(destSize));
 	result = bignumNode(wrapInteger(destSize));
	notBignumWords(b, wrapInteger(destSize), result);
	return normalizeBignum(result);
}

static LispObj
addRatios(LispObj n1, LispObj n2)
{
	LispObj num1 = ratioNumerator(n1);
	LispObj den1 = ratioDenominator(n1);
	LispObj num2 = ratioNumerator(n2);
	LispObj den2 = ratioDenominator(n2);
	LispObj newNum = 0;
	LispObj newDenom = 0;
	newNum = _Add(_Multiply(den2, num1), _Multiply(den1, num2));
	newDenom = _Multiply(den1, den2);
	return simplifyRatio(newNum, newDenom);
}

static LispObj
subtractRatios(LispObj n1, LispObj n2)
{
	LispObj num1 = ratioNumerator(n1);
	LispObj den1 = ratioDenominator(n1);
	LispObj num2 = ratioNumerator(n2);
	LispObj den2 = ratioDenominator(n2);
	LispObj newNum = 0;
	LispObj newDenom = 0;
	newNum = _Subtract(_Multiply(den2, num1), _Multiply(den1, num2));
	newDenom = _Multiply(den1, den2);
	return simplifyRatio(newNum, newDenom);
}

static LispObj
multiplyRatios(LispObj n1, LispObj n2)
{
	LispObj num1 = ratioNumerator(n1);
	LispObj den1 = ratioDenominator(n1);
	LispObj num2 = ratioNumerator(n2);
	LispObj den2 = ratioDenominator(n2);
	LispObj newNum = 0;
	LispObj newDenom = 0;
	newNum = _Multiply(num1, num2);
	newDenom = _Multiply(den1, den2);
	return simplifyRatio(newNum, newDenom);
}

static LispObj
divideRatios(LispObj n1, LispObj n2)
{
	LispObj num1 = ratioNumerator(n1);
	LispObj den1 = ratioDenominator(n1);
	LispObj num2 = ratioNumerator(n2);
	LispObj den2 = ratioDenominator(n2);
	LispObj newNum = 0;
	LispObj newDenom = 0;
	newNum = _Multiply(num1, den2);
	newDenom = _Multiply(den1, num2);
	return simplifyRatio(newNum, newDenom);
}


static LispObj
multiplyComplexNumbers(LispObj n1, LispObj n2)
{
	LispObj a = 0;
	LispObj b = 0;
	LispObj c = 0;
	LispObj d = 0;
	LispObj ac = 0;
	LispObj bd = 0;
	LispObj ad = 0;
	LispObj bc = 0;

	if (!isComplex(n1))
		return createComplex(_Multiply(n1, complexReal(n2)),
								 _Multiply(n1, complexImaginary(n2)));
	else
	if (!isComplex(n2))
		return createComplex(_Multiply(n2, complexReal(n1)),
								 _Multiply(n2, complexImaginary(n1)));
	else
	{
		// (a + bi) * (c + di) == ac - bd + (bc + ad)i
		a = complexReal(n1);
		b = complexImaginary(n1);
		c = complexReal(n2);
		d = complexImaginary(n2);

		ac = _Multiply(a, c);
		bd = _Multiply(b, d);
		ad = _Multiply(a, d);
		bc = _Multiply(b, c);

		return createComplex(_Subtract(_Multiply(a, c), _Multiply(b, d)),
						_Add(_Multiply(a, d), _Multiply(b, c)));
	}
}

static LispObj
divideComplexNumbers(LispObj n1, LispObj n2)
{
	// (a + bi) / (c + di) == (ac + bd)/(cc + dd) + ((bc - ad)/(cc + dd))i
	LispObj a = 0;
	LispObj b = 0;
	LispObj c = 0;
	LispObj d = 0;
	LispObj denom = 0;
	LispObj realResult = 0;
	LispObj imagResult = 0;

	if (!isComplex(n2))
		return createComplex(_Divide(complexReal(n1), n2),
								 _Divide(complexImaginary(n1), n2));

	c = complexReal(n2);
	d = complexImaginary(n2);

	if (!isComplex(n1))
	{
		a = n1;
		b = 0;
	}
	else
	{
		a = complexReal(n1);
		b = complexImaginary(n1);
	}

	denom = _Add(_Multiply(c, c), _Multiply(d, d));

	if (isZero(denom))
		Error("Divide by zero error: ~A / ~A", n1, n2);

	realResult = _Divide(_Add(_Multiply(a, c), _Multiply(b, d)), denom);
	imagResult = _Divide(_Subtract(_Multiply(b, c), _Multiply(a, d)), denom);
	return createComplex(realResult, imagResult);
}

static void
bignumSetBit(LispObj bn, long bit)
{
	long word = bit / 32;
	long b = bit % 32;
	bignumStart(bn)[word] |= (1 << b);
}

static long
bignumHighBit(LispObj b)
{
	BignumWord* p = bignumStart(b) + bignumLength(b) - 1;
	long high = 0;
	BignumWord w = *p;

	if (w & 0xffff0000)
		high += 16, w >>= 16;
	if (w & 0xff00)
		high += 8, w >>= 8;
	if (w & 0xf0)
		high += 4, w >>= 4;
	if (w & 0x0c)
		high += 2, w >>= 2;
	if (w & 2)
		high++;

	return high + ((p - bignumStart(b)) * 32);
}

static long
bignumAbsCompare(LispObj b1, LispObj b2)
{
	long len1 = bignumLength(b1);
	long len2 = bignumLength(b2);
	long i = 0;

	if (len1 > len2)
		return 1;
	else
	if (len1 < len2)
		return -1;

	for (i = len1 - 1; i >= 0; i--)
	{
		if (bignumStart(b1)[i] > bignumStart(b2)[i])
			return 1;
		else
		if (bignumStart(b1)[i] < bignumStart(b2)[i])
			return -1;
	}
	return 0;
}

static LispObj
simplifyRatio(LispObj num, LispObj denom)
{
	LispObj temp = 0;
	if (num == 0)
		return 0;
	if (denom == 0)
		Error("Divide by zero error: ~A / ~A", num, denom);
	if (numCompare(denom, 0) < 0)
	{
		denom = _Negate(denom);
		num = _Negate(num);
	}
	temp = gcdNumbers(num, denom);
	if (temp != wrapInteger(1))
	{
		num = divideIntegerNumbers(num, temp);
		denom = divideIntegerNumbers(denom, temp);
	}
	return (denom == wrapInteger(1) ? num : ratioNode(num, denom));
}

static LispObj
normalizeBignum(LispObj b)
{
	unsigned long word = 0;
	long n = 0;
	long len = bignumLength(b);
	if (len == 0)
		return 0;
	if (len == 1)
	{
		word = (unsigned long)*bignumStart(b);
		if (bignumNegative(b))
		{
			if (word <= 0x10000000)
				return wrapInteger(-((long)word));
		}
		else
		{
			if (word < 0x10000000)
				return wrapInteger((long)word);
		}
	}
	bignumReduce(b);
	return b;
}

// does not handle complex
static LispObj
_Floor(LispObj n)
{
	LispObj res = 0;

	if (isLispInteger(n))
		return n;

	if (isRatio(n))
	{
		res = divideIntegerNumbers(ratioNumerator(n), ratioDenominator(n));
		if (numCompare(n, 0) < 0)
			return _Subtract(res, wrapInteger(1));
		return res;
	}
	if (isDoubleFloat(n))
	{
		res = doubleFloatNode(0);
		doubleFloat(res) = floor(doubleFloat(n));
		return convertDoubleFloatToInteger(res);
	}
	if (isSingleFloat(n))
	{
		res = singleFloatNode(0);
		singleFloat(res) = floor(singleFloat(n));
		return convertSingleFloatToInteger(res);
	}
	if (isShortFloat(n))
		return convertShortFloatToInteger(createShortFloat(floor(shortFloat(n))));
	Error("Cannot calculate FLOOR of ~S", n);
	return NIL;
}

// does not handle complex
static LispObj
_Ceiling(LispObj n)
{
	LispObj res = 0;

	if (isLispInteger(n))
		return n;

	if (isRatio(n))
	{
		res = divideIntegerNumbers(ratioNumerator(n), ratioDenominator(n));
		if (numCompare(n, 0) > 0)
			return _Add(res, wrapInteger(1));
		return res;
	}
	if (isDoubleFloat(n))
	{
		res = doubleFloatNode(0);
		doubleFloat(res) = ceil(doubleFloat(n));
		return convertDoubleFloatToInteger(res);
	}
	if (isSingleFloat(n))
	{
		res = singleFloatNode(0);
		singleFloat(res) = ceil(singleFloat(n));
		return convertSingleFloatToInteger(res);
	}
	if (isShortFloat(n))
		return convertShortFloatToInteger(createShortFloat(ceil(shortFloat(n))));
	Error("Cannot calculate CEILING of ~A", n);
	return NIL;
}

// does not handle complex
static LispObj
_Truncate(LispObj n)
{
	LispObj res = 0;
	if (isLispInteger(n))
		return n;

	if (isRatio(n))
	{
		res = divideIntegerNumbers(ratioNumerator(n), ratioDenominator(n));
		return res;
	}
	if (isDoubleFloat(n))
	{
		if (doubleFloat(n) >= 0.0)
		{
			res = doubleFloatNode(0);
			doubleFloat(res) = floor(doubleFloat(n));
			return convertDoubleFloatToInteger(res);
		}
		else
		{
			res = doubleFloatNode(0);
			doubleFloat(res) = ceil(doubleFloat(n));
			return convertDoubleFloatToInteger(res);
		}
	}
	if (isSingleFloat(n))
	{
		if (singleFloat(n) >= 0.0)
		{
			res = singleFloatNode(0);
			singleFloat(res) = floor(singleFloat(n));
			return convertSingleFloatToInteger(res);
		}
		else
		{
			res = singleFloatNode(0);
			singleFloat(res) = ceil(singleFloat(n));
			return convertSingleFloatToInteger(res);
		}
	}
	if (isShortFloat(n))
	{
		if (shortFloat(n) >= 0.0)
			return convertShortFloatToInteger(createShortFloat(floor(shortFloat(n))));
		else
			return convertShortFloatToInteger(createShortFloat(ceil(shortFloat(n))));
	}
	return NIL;
}

// does not handle complex
static LispObj
_Round(LispObj n)
{
	LispObj res = 0;
	long odd = 0;
	if (isLispInteger(n))
		return n;

	// add 1/2 to number
	n = _Add(n, ratioNode(wrapInteger(1), wrapInteger(2)));
	res = _Floor(n);

	// if exactly halfway and odd, subtract one
	if (numCompare(n, res) == 0)
	{
		odd = isLispInteger(res) && !isZero(_Mod(res, wrapInteger(2)));
		if (odd)
			res = _Subtract(res, wrapInteger(1));
	}
	return res;
}

//	divide and truncate
static LispObj
divideIntegerNumbers(LispObj n1, LispObj n2)
{
	if (both_fixnums(n1, n2))
		return wrapInteger((long)(((long)integer(n1)) / ((long)integer(n2))));

	return bignumDivide(convertToBignum(n1), convertToBignum(n2));
}

//
//		numCompare()
//		Returns -1 if first number < second number, 0 if they are the equal,
//		and 1 if the second number is greater than the first.
//
static long
numCompare(LispObj x, LispObj y)
{
	checkNumber(x);
	checkNumber(y);
	if (isComplex(x) || isComplex(y))
		Error("Cannot compare complex numbers: ~A ~A", x, y);
	return compareNumbers(x, y);
}

//
//	compareNumbers()
//	Does not handle complex numbers.
//	Assumes both arguments are real numbers.
//
long
compareNumbers(LispObj n1, LispObj n2)
{
	double d = 0.0;

	if (isFixnum(n1))
	{
		if (isFixnum(n2))
		{
			if (((long)n1) < ((long)n2)) return -1;
			if (((long)n1) == ((long)n2)) return 0;
			if (((long)n1) > ((long)n2)) return 1;
		}
		if (isRatio(n2))
			return compareRatios(n1, n2);
		if (isDoubleFloat(n2))
		{
			d = (double)(integer(n1)) - doubleFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
		if (isSingleFloat(n2))
		{
			d = (double)(integer(n1)) - singleFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
		if (isShortFloat(n2))
		{
			d = (double)(integer(n1)) - shortFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
		if (isBignum(n2))
			return bignumCompare(convertToBignum(n1), n2);
	}
	if (isRatio(n1))
	{
		if (isInteger(n2) || isRatio(n2) || isBignum(n2))
			return compareRatios(n1, n2);
		if (isDoubleFloat(n2))
		{
			d = getFloat(n1) - doubleFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
		if (isSingleFloat(n2))
		{
			d = getFloat(n1) - singleFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
		if (isShortFloat(n2))
		{
			d = getFloat(n1) - shortFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
	}
	if (isDoubleFloat(n1))
	{
		d = doubleFloat(n1) - getFloat(n2);
		if (d < 0) return -1;
		if (d == 0) return 0;
		if (d > 0) return 1;
	}
	if (isSingleFloat(n1))
	{
		d = singleFloat(n1) - getFloat(n2);
		if (d < 0) return -1;
		if (d == 0) return 0;
		if (d > 0) return 1;
	}
	if (isShortFloat(n1))
	{
		d = shortFloat(n1) - getFloat(n2);
		if (d < 0) return -1;
		if (d == 0) return 0;
		if (d > 0) return 1;
	}
	if (isBignum(n1))
	{
		if (isFixnum(n2))
			return bignumCompare(n1, convertToBignum(n2));
		if (isRatio(n2))
			return compareRatios(n1, n2);
		if (isDoubleFloat(n2))
		{
			d = getFloat(n1) - doubleFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
		if (isSingleFloat(n2))
		{
			d = getFloat(n1) - singleFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
		if (isShortFloat(n2))
		{
			d = getFloat(n1) - shortFloat(n2);
			if (d < 0) return -1;
			if (d == 0) return 0;
			if (d > 0) return 1;
		}
		if (isBignum(n2))
			return bignumCompare(n1, n2);
	}
	return 0;
}

static LispObj
convertDoubleFloatToInteger(LispObj n)
{
	double d = doubleFloat(n);
	if (d >= ((double)FixnumMin) && d <= ((double)FixnumMax))
		return wrapInteger((long)d);
	return convertToBignum(n);
}

static LispObj
convertSingleFloatToInteger(LispObj n)
{
	double d = singleFloat(n);
	if (d >= ((double)FixnumMin) && d <= ((double)FixnumMax))
		return wrapInteger((long)d);
	return convertToBignum(n);
}

static LispObj
convertShortFloatToInteger(LispObj n)
{
	double d = shortFloat(n);
	if (d >= ((double)FixnumMin) && d <= ((double)FixnumMax))
		return wrapInteger((long)d);
	return convertToBignum(n);
}

static LispObj
convertToBignum(LispObj n)
{
	LispObj x = wrapInteger(1);
	double d = 0.0;
	unsigned long* p = (unsigned long*)&d;

	if (isFixnum(n))
		return fixnumToBignum(n);
	if (isBignum(n))
		return n;
	if (isFloat(n))
	{
		if (isSingleFloat(n))
			d = singleFloat(n);
		else
		if (isShortFloat(n))
			d = shortFloat(n);
		else
			d = doubleFloat(n);

		x = bignumNode(wrapInteger(2));
		UVECTOR(x)[BIGNUM_FIRST_CELL] = p[0];
		UVECTOR(x)[BIGNUM_FIRST_CELL + 1] = (p[1] & 0xfffff) + 0x100000;
		x = bignumShift(x, wrapInteger(((p[1] >> 20) & 0x7ff) - 0x3ff - 52), NIL);
		if (p[1] & 0x80000000)	// if (d < 0)
			UVECTOR(x)[BIGNUM_LENGTH] |= wrapInteger(1);

#if 0
		while (d > 65536.0)
		{
			x = _Multiply(wrapInteger(65536), x);
			d /= 65536.0;
		}
		x = _Multiply(x, wrapInteger((long)d));
		if (isFixnum(x))
			x = fixnumToBignum(x);
#endif
		return x;
	}
	Error("Cannot convert to BIGNUM: ~A", n);
	return 0;
}

static long
compareRatios(LispObj n1, LispObj n2)
{
	LispObj t1 = 0;
	LispObj t2 = 0;

	t1 = _Multiply(ratioNumerator(n1), ratioDenominator(n2));
	t2 = _Multiply(ratioDenominator(n1), ratioNumerator(n2));
	return compareNumbers(t1, t2);
}

//
//	bignumCompare()
//	Compares two bignums. Returns -1 if the first is less,
//	0 if equal, or 1 if the first is greater.
//
static long
bignumCompare(LispObj b1, LispObj b2)
{
	if (!bignumNegative(b1))
	{
		if (!bignumNegative(b2))
			return bignumAbsCompare(b1, b2);
		else
			return 1;
	}
	else	// b1 is negative
	{
		if (bignumNegative(b2))
			return bignumAbsCompare(b2, b1);
		else
			return -1;
	}
}

//
//	getFloat()
//	This is designed to be a fast number to float promotion for
//	use by the math library routines. Returns a C++ double.
//
static double
getFloat(LispObj n)
{
	checkNumber(n);
	if (isDoubleFloat(n))
		return doubleFloat(n);
	if (isSingleFloat(n))
		return singleFloat(n);
	if (isShortFloat(n))
		return shortFloat(n);
	if (isFixnum(n))
		return (double)integer(n);
	if (isRatio(n))
		return getFloat(ratioNumerator(n)) / getFloat(ratioDenominator(n));
	if (isBignum(n))
		return bignumToDouble(n);
	Error("Cannot promote to float: ~A", n, 0);
	return 0.0;
}

static const double power_48 = 65536.0 * 65536.0 * 65536.0;
static const double power_32 = 65536.0 * 65536.0;
static const double power_16 = 65536.0;

static double
bignumToDouble(LispObj n)
{
	// return the upper 64 bits as a floating point number
	long max = bignumLength(n) - 1;
	double d1 = 0.0;
	double d2 = 0.0;
	double d = 0.0;

	d1 = (double)(bignumStart(n)[max]);
	if (max > 0)
	{
		d2 = (double)(bignumStart(n)[max - 1]);
		d = d1 * power_32 + d2;
	}
	else
		d = d1;
	if (bignumNegative(n))
		d = -d;
	if (max > 1)
		d *= pow(2.0, (double)((max - 1) * 32));
	return d;
}

LispObj singleToDoubleFloat(LispObj single)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = singleFloat(single);
	return ret;
}

LispObj shortToDoubleFloat(LispObj d)
{
	LispObj ret = 0;
	ret = doubleFloatNode(0);
	doubleFloat(ret) = shortFloat(d);
	return ret;
}

//
//	does not handle complex numbers
//
static long
isZero(LispObj n)
{
	if (n == 0)
		return 1;
	else
	if (isDoubleFloat(n))
		return doubleFloat(n) == 0.0;
	else
	if (isSingleFloat(n))
		return singleFloat(n) == 0.0;
	else
	if (isShortFloat(n))
		return shortFloat(n) == 0.0;
	else
		return 0;
}

static long
equalComplexNumbers(LispObj n1, LispObj n2)
{
	if (!isComplex(n1))
	{
		if (isZero(complexImaginary(n2)) && (lispNumericEqual(n1, complexReal(n2)) != NIL))
			return 1;
		else
			return 0;
	}
	else
	if (!isComplex(n2))
	{
		if (isZero(complexImaginary(n1)) && (lispNumericEqual(n2, complexReal(n1)) != NIL))
			return 1;
		else
			return 0;
	}
	else
	{
		if ((lispNumericEqual(complexReal(n1), complexReal(n2)) != NIL)
			&& (lispNumericEqual(complexImaginary(n1), complexImaginary(n2)) != NIL))
			return 1;
		else
			return 0;
	}
}

//
//	_Abs()
//
static LispObj
_Abs(LispObj n)
{
	if (isComplex(n))
		return absComplex(n);
	if (numCompare(n, 0) < 0)
		n = _Subtract(0, n);
	return n;
}

//
//		gcdNumbers()
//		Returns 0 if either number is zero.
//
static LispObj
gcdNumbers(LispObj n1, LispObj n2)
{
	LispObj temp = 0;
	n1 = _Abs(n1);
	n2 = _Abs(n2);
	if (n1 == 0)
		return n2;
	else
	if (n2 == 0)
		return n1;
	while (1)
	{
		if (isFixnum(n1) && isFixnum(n2))
			return wrapInteger(gcdIntegers(integer(n1), integer(n2)));
		if (isFixnum(n1))
			temp = bignumMod(fixnumToBignum(n1), n2);
		else
		if (isFixnum(n2))
			temp = bignumMod(n1, fixnumToBignum(n2));
		else
			temp = bignumMod(n1, n2);
		if (temp == 0)
			return n2;
		n1 = n2;
		n2 = temp;
	}
}

static long
gcdIntegers(long n1, long n2)
{
	if (!n1)
		return n2 < 0 ? -n2 : n2;
	if (!n2)
		return n1 < 0 ? -n1 : n1;
	long r = 0;
	while (1)
	{
		r = n1 % n2;
		if (!r)
			return n2;
		n1 = n2;
		n2 = r;
	}
}

#if 0
//
//		lcmNumbers()
//
static LispObj lcmNumbers(LispObj n1, LispObj n2)
{
	if (isZero(n1) || isZero(n2))
		return 0;
	return _Divide(_Abs(_Multiply(n1, n2)), gcdNumbers(n1, n2));
}
#endif

//
//	absComplex()
//
static LispObj
absComplex(LispObj n)
{
	LispObj temp = 0;
	double x = 0.0;
	double y = 0.0;
	double ret = 0.0;

	if (!isComplex(n))
		return _Abs(n);

	x = getFloat(complexReal(n));
	y = getFloat(complexImaginary(n));

  	if (x == 0)
    	ret = y;
  	else
	if (y == 0)
    	ret = x;
  	else
	{
		if (x > y)
			ret = x * sqrt(1 + pow(y/x, 2));
      	else
			ret = y * sqrt(1 + pow(x/y, 2));
    }
	temp = doubleFloatNode(0);
	doubleFloat(temp) = ret;
	return temp;
}

//
//	complexSqrt()
//	Assumes the passed number is complex.
//	Always returns a floating point complex number.
//
static LispObj
complexSqrt(LispObj n)
{
	LispObj realResult = 0;
	LispObj imagResult = 0;
	double re = getFloat(complexReal(n));
	double im = getFloat(complexImaginary(n));
	if (re == 0.0 && im == 0.0)
		return n;
	double x = fabs(re);
	double y = fabs(im);
	double r = 0.0;
	double w = 0.0;
	double imagpart = 0.0;

	if (x >= y)
	{
		r = y / x;
		w = sqrt(x) * sqrt(0.5 * (1.0 + sqrt(1.0 + r * r)));
	}
	else
	{
		r = x / y;
		w = sqrt(y) * sqrt(0.5 * (r + sqrt(1.0 + r * r)));
	}
	if (re >= 0.0)
	{
		realResult = doubleFloatNode(0);
		imagResult = doubleFloatNode(0);
		doubleFloat(realResult) = w;
		doubleFloat(imagResult) = im / (2.0 * w);
	}
	else
	{
		realResult = doubleFloatNode(0);
		imagResult = doubleFloatNode(0);
		imagpart = (im >= 0) ? w : -w;
		doubleFloat(imagResult) = imagpart;
		doubleFloat(realResult) = im / (2.0 * imagpart);
	}

	return createComplex(realResult, imagResult);
}

//
//	complexExpt()
//	Assumes the passed number is complex.
//	Always returns a floating point complex number.
//	TDE: This currently always returns floating point results, even if
//	an integer complex number is passed to it.
//
static LispObj
complexExpt(LispObj base, LispObj power)
{
	LispObj temp1 = 0;
	LispObj temp2 = 0;

	if (isZero(power))
		return wrapInteger(1);

	double realbase = 0.0;
	double imagbase = 0.0;
	double realpower = 0.0;
	double imagpower = 0.0;
    double logfReal = 0.0;
	double logfImag = 0.0;
	double phaseReal = 0.0;
	double phaseImag = 0.0;
	double absVal = 0.0;

	if (isComplex(base))
	{
		realbase = getFloat(complexReal(base));
		imagbase = getFloat(complexImaginary(base));
	}
	else
	{
		realbase = getFloat(base);
		imagbase = 0.0;
	}

	if (isComplex(power))
	{
		realpower = getFloat(complexReal(power));
		imagpower = getFloat(complexImaginary(power));
	}
	else
	{
		realpower = getFloat(power);
		imagpower = 0.0;
	}

	// calculate complex absolute value of base
  	if (realbase == 0)
    	absVal = -fabs(imagbase);
  	else
	if (imagbase == 0)
    	absVal = fabs(realbase);
  	else
	{
		if (realbase > imagbase)
			absVal = realbase * sqrt(1 + pow(imagbase / realbase, 2));
      	else
			absVal = imagbase * sqrt(1 + pow(realbase / imagbase, 2));
    }

    logfReal = log(absVal);
    logfImag = atan2(imagbase, realbase);
    phaseReal = exp(logfReal * realpower - logfImag * imagpower);
    phaseImag = logfReal * imagpower + logfImag * realpower;
	temp1 = doubleFloatNode(0);
	temp2 = doubleFloatNode(0);
	doubleFloat(temp1) = phaseReal * cos(phaseImag);
	doubleFloat(temp2) = phaseReal * sin(phaseImag);
	return createComplex(temp1, temp2);
}

//
//	complexLog()
//	Computes the natural log (log bas e) of the passed, complex argument.
//
static LispObj
complexLog(LispObj n)
{
	LispObj abs = 0;
	LispObj temp1 = 0;
	LispObj temp2 = 0;
	double re = 0.0;
	double im = 0.0;
	double ar = 0.0;
	double d = 0.0;

	abs = absComplex(n);
	if (isComplex(n))
	{
		re = getFloat(complexReal(n));
		im = getFloat(complexImaginary(n));
	}
	else
	{
		re = getFloat(n);
		im = 0.0;
	}
	ar = atan2(im, re);
	d = getFloat(abs);
	temp1 = doubleFloatNode(0);
	temp2 = doubleFloatNode(0);
	doubleFloat(temp1) = log(d);
	doubleFloat(temp2) = ar;
	return createComplex(temp1, temp2);
}

//
//	_Log()
//	Computes the natural log (log bas e) of the passed argument.
//
static LispObj
_Log(LispObj n)
{
	LispObj temp = 0;
	double d = 0.0;

	if (isComplex(n))
		return complexLog(n);

	d = getFloat(n);
	if (d < 0.0)
		return complexLog(n);
	temp = doubleFloatNode(0);
	doubleFloat(temp) = log(d);
	return temp;
}

//
//	Common Lisp SQRT function.
//
LispFunction(Sqrt)
{
	LISP_FUNC_BEGIN(1);
	LispObj n = LISP_ARG(0);
	double d = 0.0;
	LispObj temp1 = 0;

	if (isComplex(n))
		ret = complexSqrt(n);
	else
	// as a special case, handle (sqrt -1)
	if (n == wrapInteger(-1))
		ret = createComplex(0, wrapInteger(1));
	else
	{
		d = getFloat(n);
		if (d < 0.0)
		{
			temp1 = doubleFloatNode(0);
			doubleFloat(temp1) = d;
			ret = complexSqrt(complexNode(temp1, doubleFloatNode(0.0)));
		}
		else
		{
			temp1 = doubleFloatNode(0);
			doubleFloat(temp1) = sqrt(d);
			ret = temp1;
		}
	}

	LISP_FUNC_RETURN(ret);
}

//
//	Common Lisp ISQRT function.
//
LispFunction(Isqrt)
{
	LISP_FUNC_BEGIN(1);
	LispObj n = LISP_ARG(0);
    LispObj len = 0;
	unsigned long x = 0;
	unsigned long r = 0;
	unsigned long m = 0;
	unsigned long nr = 0;

	checkLispInteger(n);

	if (isFixnum(n))
	{
		if (integer(n) < 0)
			Error("Negative integer passed to ISQRT: ~A", n);
		x = (unsigned long)(integer(n));
 		r = 0;
		m = 0x40000000;
		do
		{
			nr = r + m;
			if (nr <= x)
			{
				x -= nr;
				r = nr + m;
			}
			r >>= 1;
			m >>= 2;
		} while (m != 0);
		ret = wrapInteger((long)r);
	}
	else
	{
		// else it is a big integer
		if (bignumNegative(n))
			Error("Negative integer passed to ISQRT: ~A", n);
		x = n;
 		r = 0;
		m = fixnumToBignum(wrapInteger(1));
        len = bignumIntegerLength(n);
        // if the integer length is odd, round up to next even length
		m = bignumShift(m, (len & wrapInteger(1)) ? (len + wrapInteger(1)) : len, T);
		do
		{
			nr = _Add(r, m);
			if (compareNumbers(nr, x) <= 0)
			{
				x = _Subtract(x, nr);
				r = _Add(nr, m);
			}
			if (isBignum(r))
				r = bignumShift(r, wrapInteger(-1), T);
			else
				r = wrapInteger(integer(r) >> 1);
			if (isBignum(m))
				m = bignumShift(m, wrapInteger(-2), T);
			else
				m = wrapInteger(integer(m) >> 2);
		} while (m != 0);
		ret = r;
	}
	LISP_FUNC_RETURN(ret);
}

LispObj
bignumShift(LispObj b, LispObj shift, LispObj signedShift)
{
	long wordLength = bignumLength(b);
	long bitLength = wordLength * 32;
	long newLength = bitLength + integer(shift);
	long newWordLength = (newLength + 31) / 32;
	LispObj result = 0;
	long negative = bignumNegative(b);

	result = bignumNode(wrapInteger(newWordLength));
//	if (negative)
//		Bignum_SMto2C(b);
	shiftBignumWords(b, wordLength, result, newWordLength, integer(shift));
	if (negative && signedShift != NIL)
	{
//		Bignum_2CtoSM(b);
//		Bignum_2CtoSM(result);
		result = bignumAdd(result, fixnumToBignum(wrapInteger(1)));
		if (isBignum(result))
		{
			bignumNegate(result);
			return normalizeBignum(result);
		}
		else
		{
			result = (LispObj)-(long)result;
			return result;
		}
	}

	return normalizeBignum(result);
}

LispObj
bignumIntegerLength(LispObj b)
{
	long highIndex = bignumLength(b) - 1;
	BignumWord highword = bignumStart(b)[highIndex];
	long res = (highIndex * 32);
	if (highword & 0xffff0000)	{ res += 16;	highword >>= 16;	}
	if (highword & 0x0000ff00)	{ res += 8;		highword >>= 8;	}
	if (highword & 0x000000f0) 	{ res += 4;		highword >>= 4;	}
	if (highword & 0x0000000c) 	{ res += 2;		highword >>= 2;	}
	if (highword & 0x00000002) 	{ res += 1;		highword >>= 1;	}
	res++;
	return wrapInteger(res);
}

//
//	Common Lisp EXP function
//
LispFunction(Exp)
{
	LISP_FUNC_BEGIN(1);
	LispObj n = LISP_ARG(0);
	LispObj temp1 = 0;
	LispObj temp2 = 0;
	double re = 0.0;
	double im = 0.0;
	double x = 0.0;

	checkNumber(n);

	if (isComplex(n))
	{
		re = getFloat(complexReal(n));
		im = getFloat(complexImaginary(n));
		x = exp(re);
		temp1 = doubleFloatNode(0);
		temp2 = doubleFloatNode(0);
		doubleFloat(temp1) = x * cos(im);
		doubleFloat(temp2) = x * sin(im);
		ret = createComplex(temp1, temp2);
	}
	else
	{
		temp1 = doubleFloatNode(0);
		doubleFloat(temp1) = exp(getFloat(n));
		ret = temp1;
	}
	LISP_FUNC_RETURN(ret);
}

//
//	Common Lisp EXPT function
//
LispFunction(Expt)
{
	LISP_FUNC_BEGIN(2);
	LispObj base = LISP_ARG(0);
	LispObj power = LISP_ARG(1);
	LispObj temp = 0;
	long p = 0;
	LispObj res = 0;
	long i = 0;
	double b = 0;
	double pd = 0;

	checkNumber(base);
	checkNumber(power);

	if ((isRational(base) ||
		(isComplex(base) && isRational(complexReal(base))
			&& isRational(complexImaginary(base)))) && isFixnum(power))
	{
		// calculate exact result
		p = integer(power);
		res = wrapInteger(1);
		if (p < 0)	// handle negative exponent
		{
			for (i = 0; i > p; i--)
				res = _Multiply(res, base);
			ret = _Divide(wrapInteger(1), res);
		}
		else
		{
			for (i = 0; i < p; i++)
				res = _Multiply(res, base);
			ret = res;
		}
	}
	else
	if (isComplex(base) || isComplex(power))
		ret = complexExpt(base, power);
	else
	{
		b = getFloat(base);
		pd = getFloat(power);
		if (b < 0.0 && !isFixnum(power))		// result will be complex
		{
			ret = complexExpt(base, power);
		}
		else
		{
			errno = 0;
			temp = doubleFloatNode(0);
			doubleFloat(temp) = pow(b, pd);
			ret = temp;
			if (errno != 0)
				Error("Invalid arguments to function 'expt': ~A ~A", base, power);
		}
	}
	LISP_FUNC_RETURN(ret);
}

//
//	Common Lisp LOG function
//
LispFunction(Log)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj n = LISP_ARG(0);
	LispObj base = 0;
	LispObj factor = 0;

	checkNumber(n);
	if (ARG_COUNT == 2)
	{
		base = LISP_ARG(1);
		checkNumber(base);
		factor = _Log(base);
	}

	ret = _Log(n);
	if (ARG_COUNT == 2)
		ret = _Divide(ret, factor);

	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp ABS function
//
LispFunction(Abs)
{
	LISP_FUNC_BEGIN(1);
	LispObj n = LISP_ARG(0);
	checkNumber(n);
	ret = _Abs(n);
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp SIN function
//
LispFunction(Sin)
{
	LISP_FUNC_BEGIN(1);
	LispObj n = LISP_ARG(0);
	checkNumber(n);

	if (isComplex(n))
	{
		Error("SIN of complex numbers not implemented: ~A", n);
	}
	ret = doubleFloatNode(0);
	doubleFloat(ret) = sin(getFloat(n));
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp 'cos' function
//
LispFunction(Cos)
{
	LISP_FUNC_BEGIN(1);
	LispObj n = LISP_ARG(0);
	checkNumber(n);

	if (isComplex(n))
	{
		Error("COS of complex numbers not implemented: ~A", n);
	}
	ret = doubleFloatNode(0);
	doubleFloat(ret) = cos(getFloat(n));
	LISP_FUNC_RETURN(ret);
}

static LispObj _Logxor(LispObj n1, LispObj n2)
{
	LispObj result = n1;

	// quick check here to optimize for adding 2 fixnums
	if (both_fixnums(n1, n2))
	{
		__asm mov eax, dword ptr n2
		__asm xor dword ptr result, eax
	}
	else
		result = bignumXor(isInteger(n1) ? fixnumToBignum(n1) : n1,
						   isInteger(n2) ? fixnumToBignum(n2) : n2);
	return result;
}

static LispObj _Logior(LispObj n1, LispObj n2)
{
	LispObj result = n1;

	// quick check here to optimize for adding 2 fixnums
	if (both_fixnums(n1, n2))
	{
		__asm mov eax, dword ptr n2
		__asm or dword ptr result, eax
	}
	else
		result = bignumIor(isInteger(n1) ? fixnumToBignum(n1) : n1,
						   isInteger(n2) ? fixnumToBignum(n2) : n2);
	return result;
}

static LispObj _Logand(LispObj n1, LispObj n2)
{
	LispObj result = n1;

	// quick check here to optimize for adding 2 fixnums
	if (both_fixnums(n1, n2))
	{
		__asm mov eax, dword ptr n2
		__asm and dword ptr result, eax
	}
	else
		result = bignumAnd(isInteger(n1) ? fixnumToBignum(n1) : n1,
						   isInteger(n2) ? fixnumToBignum(n2) : n2);
	return result;
}

static LispObj _Lognot(LispObj n)
{
	LispObj result = n;

	if (isFixnum(n))
	{
		__asm mov eax, dword ptr n
		__asm not eax
		__asm and eax, -8
		__asm mov dword ptr result, eax
	}
	else
		result = bignumNot(n);
	return result;
}

//
//		Common Lisp LOGXOR function
//
LispFunction(Logxor)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj x = 0;
	long i = 0;
	if (ARG_COUNT > 0)
	{
		x = LISP_ARG(0);
		checkLispInteger(x);
		for (i = 1; i < ARG_COUNT; i++)
		{
			checkLispInteger(LISP_ARG(i));
			x = _Logxor(x, LISP_ARG(i));
		}
	}
	LISP_FUNC_RETURN(x);
}

//
//		Common Lisp LOGIOR function
//
LispFunction(Logior)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj x = 0;
	long i = 0;
	if (ARG_COUNT > 0)
	{
		x = LISP_ARG(0);
		checkLispInteger(x);
		for (i = 1; i < ARG_COUNT; i++)
		{
			checkLispInteger(LISP_ARG(i));
			x = _Logior(x, LISP_ARG(i));
		}
	}
	LISP_FUNC_RETURN(x);
}

//
//		Common Lisp LOGAND function
//
LispFunction(Logand)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj x = wrapInteger(-1);
	long i = 0;
	if (ARG_COUNT > 0)
	{
		x = LISP_ARG(0);
		checkLispInteger(x);
		for (i = 1; i < ARG_COUNT; i++)
		{
			checkLispInteger(LISP_ARG(i));
			x = _Logand(x, LISP_ARG(i));
		}
	}
	LISP_FUNC_RETURN(x);
}

//
//		Common Lisp LOGNOT function
//
LispFunction(Lognot)
{
	LISP_FUNC_BEGIN(1);
	checkLispInteger(LISP_ARG(0));
	ret = _Lognot(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp LOGANDC1 function
//
LispFunction(Logandc1)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);
	checkLispInteger(n1);
	checkLispInteger(n2);
	ret = _Logand(_Lognot(n1), n2);
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp LOGANDC2 function
//
LispFunction(Logandc2)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);
	checkLispInteger(n1);
	checkLispInteger(n2);
	ret = _Logand(n1, _Lognot(n2));
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp LOGEQV function
//
LispFunction(Logeqv)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj x = wrapInteger(-1);
	long i = 0;
	if (ARG_COUNT > 0)
	{
		x = LISP_ARG(0);
		checkLispInteger(x);
		for (i = 1; i < ARG_COUNT; i++)
		{
			checkLispInteger(LISP_ARG(i));
			x = _Lognot(_Logxor(x, LISP_ARG(i)));
		}
	}
	LISP_FUNC_RETURN(x);
}

//
//		Common Lisp LOGNAND function
//
LispFunction(Lognand)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);
	checkLispInteger(n1);
	checkLispInteger(n2);
	ret = _Lognot(_Logand(n1, n2));
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp LOGNOR function
//
LispFunction(Lognor)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);
	checkLispInteger(n1);
	checkLispInteger(n2);
	ret = _Lognot(_Logior(n1, n2));
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp LOGORC1 function
//
LispFunction(Logorc1)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);
	checkLispInteger(n1);
	checkLispInteger(n2);
	ret = _Logior(_Lognot(n1), n2);
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp LOGORC2 function
//
LispFunction(Logorc2)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);
	checkLispInteger(n1);
	checkLispInteger(n2);
	ret = _Logior(n1, _Lognot(n2));
	LISP_FUNC_RETURN(ret);
}

//
//		Common Lisp FLOAT function
//
LispFunction(Float)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj number = LISP_ARG(0);
	LispObj prototype = (ARG_COUNT > 1) ? LISP_ARG(1) : NIL;
	if (prototype == NIL || isSingleFloat(prototype))
		ret = convertToSingleFloat(number);
	else
	if (isShortFloat(prototype))
		ret = convertToShortFloat(number);
	else
		ret = convertToDoubleFloat(number);
	LISP_FUNC_RETURN(ret);
}

LispObj
createLispInteger(long num)
{
	long tag = 0;
	if (num >= FixnumMin && num <= FixnumMax)
		return wrapInteger(num);
	tag = gettag(num);
	num = stripTag(num);
	return _Add(_Multiply(num, wrapInteger(8)), wrapInteger(tag));
}

LispObj
createUnsignedLispInteger(unsigned long num)
{
	return _Add(_Multiply(createLispInteger((long)(num >> 1)), wrapInteger(2)), wrapInteger(num & 1));
}

//
//	Assumes both args are of type real, and only creates complex
//	number if imaginary part is non-zero.
//
static LispObj createComplex(LispObj real, LispObj imag)
{
	if (!compareNumbers(imag, 0))
	{
		// the imaginary part is zero
		return real;
	}
	return createComplexNode(real, imag);
}

static LispObj createComplexNode(LispObj real, LispObj imag)
{
	LispObj ret = 0;

	if (isDoubleFloat(real))
	{
		ret = complexNode(real, convertToDoubleFloat(imag));
	}
	else
	if (isSingleFloat(real))
	{
		if (isDoubleFloat(imag))
			ret = complexNode(convertToDoubleFloat(real), imag);
		else
			ret = complexNode(real, convertToSingleFloat(imag));
	}
	else
	if (isShortFloat(real))
	{
		if (isDoubleFloat(imag))
			ret = complexNode(convertToDoubleFloat(real), imag);
		else
		if (isSingleFloat(imag))
			ret = complexNode(convertToSingleFloat(real), imag);
		else
			ret = complexNode(real, convertToShortFloat(imag));
	}
	else // the real part is an integer
	{
		if (isDoubleFloat(imag))
			ret = complexNode(convertToDoubleFloat(real), imag);
		else
		if (isSingleFloat(imag))
			ret = complexNode(convertToSingleFloat(real), imag);
		else
		if (isShortFloat(imag))
			ret = complexNode(convertToShortFloat(real), imag);
		else
			ret = complexNode(real, imag);
	}
	return ret;
}

LispFunction(Complex)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj real = LISP_ARG(0);
	LispObj imag = 0;
	checkReal(real);
	if (ARG_COUNT == 2)
	{
		imag = LISP_ARG(1);
		checkReal(imag);
	}
	else
	{	// make sure imaginary part is the same type as the real part
		if (isSingleFloat(real))
			imag = convertToSingleFloat(imag);
		else
		if (isShortFloat(real))
			imag = convertToShortFloat(imag);
		else
		if (isDoubleFloat(real))
			imag = convertToDoubleFloat(imag);
	}
	if (isRational(real) && imag == 0)
		ret = real;
	else
		ret = createComplexNode(real, imag);
	LISP_FUNC_RETURN(ret);
}

//
//	Returns the result of the division only, if RemDesired
//	is false, otherwise returns a cons of the
//	result and the remainder.
//

typedef unsigned int uint;
const uint uintmax = UINT_MAX;
const int wLen = sizeof(uint) * 8;			// wLen  = 32
const int hLen = wLen/2;					// hLen  = 16
const uint rMask = (1 << hLen) - 1;			// rMask = 0x0000ffff
const uint lMask = uintmax - rMask;			// lMask = 0xffff0000
const uint lBit = uintmax - (uintmax >> 1);	// lBit  = 0x80000000

// b2 must be at least big enough to hold b1
void copyBignum(LispObj b1, LispObj b2)
{
	int size = 0;
	int i = 0;
	size = bignumNumCells(b1);
	bignumSetSign(b2, bignumNegative(b1) ? 1 : 0);
	for (i = size - 1; i >= 0; i--)
		UVECTOR(b2)[BIGNUM_FIRST_CELL + i] = UVECTOR(b1)[BIGNUM_FIRST_CELL + i];
}

// Double-digit product (Hi, Lo) = A * B:
static void DDproduct(uint A, uint B, uint &Hi, uint &Lo)
{
	uint hiA = A >> hLen;
	uint loA = A & rMask;
    uint hiB = B >> hLen;
	uint loB = B & rMask;
	uint mid1;
	uint mid2;
	uint old;

	Lo = loA * loB; Hi = hiA * hiB;
	mid1 = loA * hiB; mid2 = hiA * loB;
	old = Lo;
	Lo += mid1 << hLen;
	Hi += (Lo < old) + (mid1 >> hLen);
	old = Lo;
	Lo += mid2 << hLen;
	Hi += (Lo < old) + (mid2 >> hLen);
}

// Double-digit value (A, B) is divided by d:
static uint DDquotient(uint A, uint B, uint d)
{
	uint left;
	uint middle;
	uint right;
	uint qHi;
	uint qLo;
	uint x;
	uint dLo1;
    uint dHi = d >> hLen;
	uint dLo = d & rMask;
	qHi = A / (dHi + 1);

	// This initial guess of qHi may be too small.
	middle = qHi * dLo;
	left = qHi * dHi;
	x = B - (middle << hLen);
	A -= (middle >> hLen) + left + (x > B); B = x;
	dLo1 = dLo << hLen;

	// Increase qHi if necessary:
	while (A > dHi || (A == dHi && B >= dLo1))
	{
		x = B - dLo1;
		A -= dHi + (x > B);
		B = x;
		qHi++;
	}

	qLo = ((A << hLen) | (B >> hLen))/(dHi + 1);

	// This initial guess of qLo may be too small.
	right = qLo * dLo; middle = qLo * dHi;
	x = B - right;
	A -= (x > B);
	B = x;
	x = B - (middle << hLen);
	A -= (middle >> hLen) + (x > B);
	B = x;

	// Increase qLo if necessary:
	while (A || B >= d)
	{
		x = B - d;
		A -= (x > B);
		B = x;
		qLo++;
	}
	return (qHi << hLen) + qLo;
}

// Subtract multiple q * b from a, where a and b
// are values of n digits. The remainder a - q * b
// will be less than b and must not be negative.
// The latter condition may require q to be
// decreased by 1:

static void subtractmul(uint *a, uint *b, int n, uint &q)
{
	uint Hi;
	uint Lo;
	uint d;
	uint carry = 0;
	int i;
	for (i = 0; i < n; i++)
	{
		DDproduct(b[i], q, Hi, Lo);
		d = a[i];
		a[i] -= Lo;
		if (a[i] > d)
			carry++;
		d = a[i + 1];
		a[i + 1] -= Hi + carry;
		carry = (a[i + 1] > d);
	}
	if (carry) // q was too large
	{
		q--;
		carry = 0;
		for (i = 0; i < n; i++)
		{
			d = a[i] + carry;
			carry = d < carry;
			a[i] = d + b[i];
			if (a[i] < d)
				carry = 1;
		}
		a[n] = 0;
	}
}

// force 8-byte packing by using a struct
static struct { unsigned long n[4]; } uintMax = { 0, 0x10, 0xffffffff, 0 };
static LispObj uintMaxBignum = (LispObj)(((char*)(&uintMax)) + UvectorTag);

static LispObj divideBignums(LispObj num, LispObj denom, long RemDesired)
{
	LispObj quotient = 0;
	LispObj remainder = 0;
	LispObj t = 0;
	int numLen = 0;
	int denomLen = 0;
	long QuotNeg = 0;
	long RemNeg = 0;
	long DenomNeg = 0;
	int i = 0;
	int r = 0;
	int x = 0;
	int k = 0;
	int n = 0;
	int SecondDone = 0;
	unsigned int q = 0;
	unsigned long d =0;
	unsigned long divisor = 0;
	unsigned long dHi = 0;
	unsigned long q1 = 0;
	unsigned long ru = 0;
	unsigned long q2 = 0;
	unsigned long dividend = 0;
	uint y = 0;
	int Lq = 0;

	bignumReduce(num);
	bignumReduce(denom);
	RemNeg = bignumNegative(num) ? 1 : 0;
	DenomNeg = bignumNegative(denom) ? 1 : 0;
	QuotNeg = RemNeg ^ DenomNeg;
	numLen = bignumNumCells(num);
	denomLen = bignumNumCells(denom);

	if (denomLen == 0)
		Error("Division by zero");
	if (bignumAbsCompare(num, denom) == -1)
	{
		if (RemDesired)
			return cons(0, normalizeBignum(num));
		else
			return 0;
	}
	if (numLen == 1 && denomLen == 1)
	{
		quotient = bignumNode(wrapInteger(1));
		remainder = bignumNode(wrapInteger(1));
		bignumStart(quotient)[0] = bignumStart(num)[0] / bignumStart(denom)[0];
		bignumStart(remainder)[0] = bignumStart(num)[0] % bignumStart(denom)[0];
		bignumSetSign(quotient, QuotNeg);
		if (RemDesired)
		{
			bignumSetSign(remainder, RemNeg);
			return cons(normalizeBignum(quotient),
						normalizeBignum(remainder));
		}
		else
			return normalizeBignum(quotient);
	}

	if (denomLen == 1 && (bignumStart(denom)[0] & lMask) == 0)
	{
		// Denominator fits into a half word
		divisor = bignumStart(denom)[0];
		dHi = 0;
		q1 = 0;
		ru = 0;
		q2 = 0;
		dividend = 0;
		quotient = bignumNode(wrapInteger(numLen));
		remainder = bignumNode(wrapInteger(1));
		for (i = numLen - 1; i >= 0; i--)
		{
			dividend = (dHi << hLen) | (bignumStart(num)[i] >> hLen);
			q1 = dividend / divisor;
			r = dividend % divisor;
			dividend = (r << hLen) | (bignumStart(num)[i] & rMask);
			q2 = dividend / divisor;
			dHi = dividend % divisor;
			bignumStart(quotient)[i] = (q1 << hLen) | q2;
		}
		bignumReduce(quotient);
		bignumSetSign(quotient, QuotNeg);
		bignumStart(remainder)[0] = dHi;
		bignumSetSign(remainder, RemNeg);

		if (RemDesired)
			return cons(normalizeBignum(quotient),
						normalizeBignum(remainder));
		else
			return normalizeBignum(quotient);
   }

	r = denomLen - 1;
	y = bignumStart(denom)[r];
	x = 0;
	while ((y & lBit) == 0)
	{
		y <<= 1;
		x++;
	}
	denom = bignumShift(denom, wrapInteger(x), NIL);
	num   = bignumShift(num,   wrapInteger(x), NIL);

	// Possibly second action according to C. J. Mifsud
	if (r > 0 && bignumStart(denom)[r] < bignumStart(denom)[r - 1])
	{
		numLen = bignumNumCells(num);
		t = bignumNode(wrapInteger(numLen + 1));
		mulBignumWords(num, numLen, uintMaxBignum, 1, t);
		num = normalizeBignum(t);
		denomLen = bignumNumCells(denom);
		t = bignumNode(wrapInteger(denomLen + 1));
		mulBignumWords(denom, denomLen, uintMaxBignum, 1, t);
		denom = normalizeBignum(t);
		SecondDone = 1;
	}
	else
		SecondDone = 0;

	r = bignumNumCells(denom) - 1;
	n = bignumNumCells(num) - 1;
	Lq = n - r;
	if (bignumStart(num)[n] >= bignumStart(denom)[r])
	{
		remainder = bignumNode(wrapInteger(n + 2));
		n++;
		quotient = bignumNode(wrapInteger(Lq + 1));
	}
	else
	{
		remainder = bignumNode(wrapInteger(n + 1));
		quotient = bignumNode(wrapInteger(Lq));
	}
	copyBignum(num, remainder);

	d = bignumStart(denom)[r];
	for (k = n; k > r; k--)
	{
		q = DDquotient(bignumStart(remainder)[k],
					   bignumStart(remainder)[k - 1],
					   d);
		subtractmul((uint*)&bignumStart(remainder)[k - r - 1],
					(uint*)&bignumStart(denom)[0],
					r + 1,
					q);
		bignumStart(quotient)[k - r - 1] = q;
	}

	bignumReduce(quotient);
	bignumSetSign(quotient, QuotNeg);
	if (RemDesired)
	{
		if (SecondDone)
			remainder = divideBignums(remainder, uintMaxBignum, 0);
		if (x > 0)
		{
			if (isBignum(remainder))
				remainder = bignumShift(remainder, wrapInteger(-x), NIL);
		}
		else
		if (isBignum(remainder))
			bignumReduce(remainder);
		if (isBignum(remainder))
		{
			bignumSetSign(remainder, RemNeg);
			remainder = normalizeBignum(remainder);
		}

		return cons(normalizeBignum(quotient), remainder);
	}
	return normalizeBignum(quotient);
}

//
//	Common Lisp DIVIDE-BIGNUMS function.
//
LispFunction(Divide_Bignums)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);

	if (isFixnum(n1))
		n1 = fixnumToBignum(n1);
	if (isFixnum(n2))
		n2 = fixnumToBignum(n2);
	if (!isBignum(n1))
		Error("Not a bignum: ~A", n1);
	if (!isBignum(n2))
		Error("Not a bignum: ~A", n2);

	ret = divideBignums(n1, n2, 0);
	LISP_FUNC_RETURN(ret);
}

//
//	Common Lisp MOD-BIGNUMS function.
//
LispFunction(Mod_Bignums)
{
	LISP_FUNC_BEGIN(2);
	LispObj n1 = LISP_ARG(0);
	LispObj n2 = LISP_ARG(1);

	if (isFixnum(n1))
		n1 = fixnumToBignum(n1);
	if (isFixnum(n2))
		n2 = fixnumToBignum(n2);
	if (!isBignum(n1))
		Error("Not a bignum: ~A", n1);
	if (!isBignum(n2))
		Error("Not a bignum: ~A", n2);

	ret = divideBignums(n1, n2, 1);
	ret = CDR(ret);
	LISP_FUNC_RETURN(ret);
}

LispObj __declspec(naked) addShortFloats(LispObj /*n1*/, LispObj /*n2*/)
{
	__asm
	{
		push		ebp
		mov			ebp, esp
		std									;; begin-atomic
		and			byte ptr  [ebp + 8], 0FCh
		and         byte ptr  [ebp + 12], 0FCh
		fld         dword ptr [ebp + 8]
		fadd        dword ptr [ebp + 12]

		fstp        dword ptr [ebp + 8]		;; float x = (float)d
		mov         edx,dword ptr [ebp + 8]
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx,7FFFFFh				;; eax = 23-bit mantissa
		cmp         edx,7FFFFEh				;; avoid overflow when rounding
		jae         short t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short t2
		cmp         cl, 5
		jle         short t1
	t2:
		add         eax, 2
	t1:
		or          al, 3
		cld						;; end-atomic
		mov			esp, ebp
		pop			ebp
		ret
	}
}
