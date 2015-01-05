//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		write.cpp
//		Contents:	Routines for outputing lisp expressions.
//		History:	6/5/96  RGC  Created.
//

#include "stdafx.h"
#include <ctype.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <io.h>
#include <stdio.h>

#include "generic.h"
#include "lisp.h"
#include "CormanLispServer.h"

// UpperAlpha, LowerAlpha: only convert alphabetic characters
inline char 
UpperAlpha(char c) 
{ 
	return (char)(islower(c) ? toupper(c) : c); 
}

inline long 
LowerAlpha(long c) 
{ 
	return isupper(c) ? tolower(c) : c; 
}

static void outputInteger(LispObj s, LispObj output, int base);
static void outputFloat(LispObj s, LispObj output);
static void outputCharacter(LispObj s, LispObj output);
static void outputText(LispObj s, LispObj output);
static void outputFunction(LispObj s, LispObj output);
static void outputStream(LispObj s, LispObj output);
static void outputList(LispObj s, LispObj output);
static void outputPrettyList(LispObj s, LispObj output, xbool needToIndent = TRUE);
static void outputColumnarList(LispObj s, LispObj output);
static void outputSymbol(LispObj s, LispObj output);
static void outputUvector(LispObj n, LispObj os);
static void outputStructure(LispObj n, LispObj os);
static void outputForwardObj(LispObj n, LispObj os);
static void outputImmediate(LispObj n, LispObj os);
static void outputUvectorLength(LispObj n, LispObj os);
static void outputArray(LispObj n, LispObj os);
static void outputPackage(LispObj n, LispObj os);
static void outputHashtable(LispObj n, LispObj os);
static void outputReadtable(LispObj n, LispObj os);
static void outputUnreadableObject(LispObj obj, LispObj name, LispObj os);
static void outputSimpleVector(LispObj n, LispObj os);
static void outputSimpleCharVector(LispObj n, LispObj os);
static void outputSimpleByteVector(LispObj n, LispObj os);
static void outputSimpleShortVector(LispObj n, LispObj os);
static void outputSimpleBitVector(LispObj n, LispObj os);
static void outputSimpleDoubleFloatVector(LispObj n, LispObj os);
static void outputSimpleSingleFloatVector(LispObj n, LispObj os);

static LispObj _Write_(LispObj s, LispObj output);

static xbool usePrettyStyle = FALSE;

//
//	Write()
//	Writes an s-expression to the stream or string.
//	Returns the expression written.
//
LispObj
_Write(LispObj s, LispObj output)
{
	xbool savePrettyStyle = usePrettyStyle;
	usePrettyStyle = TRUE;
	return _Write_(s, output);
	usePrettyStyle = savePrettyStyle;
}

//
//	_Write_()
//	Writes an s-expression to the stream or string.
//	Returns the expression written.
//
static LispObj
_Write_(LispObj s, LispObj output)
{
	checkStream(output);

	switch (gettag(s))
	{
		case FixnumTag:				outputInteger(s, output, 10);	break;
		case ConsTag:				outputList(s, output);			break;
		case UvectorTag:			outputUvector(s, output);		break;
		case ForwardTag:			outputForwardObj(s, output);	break;
		case ImmediateTag:			outputImmediate(s, output);		break;
		case UvectorLengthTag:		outputUvectorLength(s, output);	break;
 	}
	if (s == UNINITIALIZED)
		s = NIL;
	return s;
}

static void
outputUvector(LispObj n, LispObj os)
{
	switch (uvectorType(n))
	{
		case FunctionType:					outputFunction(n, os);	break;
		case KFunctionType:					outputFunction(n, os);	break;
		case StructureType:					outputStructure(n, os);	break;
		case ArrayType:						outputArray(n, os);		break;
		case SymbolType:					outputSymbol(n, os);	break;
		case StreamType:					outputStream(n, os);	break;
		case DoubleFloatType:				outputFloat(n, os);		break;
		case SingleFloatType:				outputFloat(n, os);		break;
		case PackageType:					outputPackage(n, os);	break;
		case HashtableType:					outputHashtable(n, os);	break;
		case ReadTableType:					outputReadtable(n, os);	break;
		case SimpleVectorType:				outputSimpleVector(n, os);	break;
		case SimpleCharVectorType:			outputSimpleCharVector(n, os);	break;
		case SimpleByteVectorType:			outputSimpleByteVector(n, os);	break;
		case SimpleShortVectorType:			outputSimpleShortVector(n, os);	break;
		case SimpleDoubleFloatVectorType:	outputSimpleDoubleFloatVector(n, os);	break;
		case SimpleBitVectorType:			outputSimpleBitVector(n, os);	break;
		case SimpleSingleFloatVectorType:	outputSimpleSingleFloatVector(n, os);	break;
		default:	Error("Unknown uvector type", 0);
	}
}

static const long intbufsize = 20;
static char intbuf[intbufsize];
static long intbufpos = 0;

static void
outputInteger(LispObj num, LispObj os, int base)
{
	intbufpos = intbufsize - 1;
	long n = integer(num);	
	xbool negative = FALSE;
	int digit = 0;

	if (n < 0)
	{
		negative = TRUE;
		n = -n;
	}
	if (n == 0)
		intbuf[intbufpos--] = '0';
	while (n > 0)
	{
		digit = (int)(n % base);
		n /= base;
		if (digit < 10)
			intbuf[intbufpos--] = (char)('0' + (char)digit);
		else
			intbuf[intbufpos--] = (char)('A' + (char)(digit - 10));
	}
	if (negative)
		intbuf[intbufpos--] = '-';
	intbufpos++;
	outputChars(stringNode((const char*)intbuf + intbufpos), intbufsize - intbufpos, os);
}

static void 
outputFloat(LispObj num, LispObj os)
{
	double d = 0.0;
	
	if (isDoubleFloat(num))
		d = doubleFloat(num);
	else
	if (isSingleFloat(num))
		d = singleFloat(num);
	else
	if (isShortFloat(num))
		d = shortFloat(num);
	static char str[80];
	sprintf_s(str, sizeof(str), "%f", d);
	outputChars(stringNode(str), strlen(str), os);
}

static void
outputCharacter(LispObj n, LispObj os)
{
	outputChar(n, os);
}

void
outputSymbol(LispObj n, LispObj os)
{
	LispObj name = 0;
	name = symbolName(n);
	outputChars(name, integer(vectorLength(name)), os);
}

static void
outputText(LispObj n, LispObj os)
{
	outputChars(n, integer(vectorLength(n)), os);
}

static void
outputFunction(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("FUNCTION"), os); 
}

static void
outputStream(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("STREAM"), os); 
}

static void
outputList(LispObj s, LispObj os)
{
	if (s == UNINITIALIZED)
	{
		char* s = "|--Uninitialized--|";
		outputChars(stringNode(s), strlen(s), os);
		return;
	}

	// check for (quote x) forms and output as 'x
	if (CAR(s) == QUOTE && isCons(CDR(s)))
	{
		outputChar(wrapCharacter('\''), os);
		_Write_(CAR(CDR(s)), os);
		return;
	}		
				
	outputChar(wrapCharacter('('), os);

	int count = 0;

	while (isCons(s))
	{
		if (count > 0)
			outputChar(wrapCharacter(' '), os);
		_Write_(CAR(s), os);
		s = CDR(s);
		count++;
	}

	if (s != NIL)
	{
		outputChars(stringNode(" . "), 3, os);
		_Write_(s, os);
	}

	outputChar(wrapCharacter(')'), os);
}		

static void 
outputForwardObj(LispObj n, LispObj os)
{
	static char str[80];
	sprintf_s(str, sizeof(str), "#< Forward pointer: 0x%x >", stripTag(n));
	outputChars(stringNode(str), strlen(str), os);
}

static void 
outputImmediate(LispObj n, LispObj os)
{
	if (isCharacter(n))
	{
		outputCharacter(n, os);
		return;
	}
	static char str[80];
	sprintf_s(str, sizeof(str), "#< Immediate object: 0x%x >", (long)immediate(n));
	outputChars(stringNode(str), strlen(str), os);
}

static void 
outputUvectorLength(LispObj n, LispObj os)
{
	static char str[80];
	long type = (long)((n >> 3) & 0x1f);
	long size = ((long)(n >> 8)) * sizeof(Node);
	sprintf_s(str, sizeof(str), "#< Uvector header: Type = %d, Size = %d bytes >", type, size);
	outputChars(stringNode(str), strlen(str), os);
}

static void 
outputArray(LispObj n, LispObj os)
{
	if (isString(n))
		outputText(n, os);
	else
	{
		outputUnreadableObject(n, stringNode("ARRAY"), os); 
	}
}

static void 
outputSimpleVector(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("SIMPLE-VECTOR"), os); 
}

static void 
outputSimpleCharVector(LispObj n, LispObj os)
{
	outputText(n, os);
}

static void 
outputSimpleByteVector(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("SIMPLE-BYTE0VECTOR"), os); 
}

static void 
outputSimpleShortVector(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("SIMPLE-SHORT-VECTOR"), os); 
}

static void 
outputSimpleBitVector(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("SIMPLE-BIT-VECTOR"), os); 
}

static void 
outputSimpleDoubleFloatVector(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("SIMPLE-DOUBLE-FLOAT-VECTOR"), os); 
}

static void 
outputSimpleSingleFloatVector(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("SIMPLE-SINGLE-FLOAT-VECTOR"), os); 
}

static void 
outputStructure(LispObj n, LispObj os)
{
	long i = 0;
	char* s = "#< STRUCTURE: ";
	long len = 0;
	outputChars(stringNode(s), strlen(s), os);
	len = (uvectorSize(n) - 1) * 2;
	for (i = 0; i < len; i++)
	{
		 _Write_(UVECTOR(n)[i + 2], os);
		 if (i != (len - 1))
			outputChar(wrapCharacter(' '), os);
	}
	s = ">";
	outputChars(stringNode(s), strlen(s), os);
}

static void 
outputPackage(LispObj n, LispObj os)
{
	outputChars(stringNode("#< "), strlen("#< "), os); 
	outputChars(stringNode("PACKAGE"), strlen("PACKAGE"), os); 
	outputChars(stringNode(": "), strlen(": "), os);
	_Write_(UVECTOR(n)[PACKAGE_NAME], os);
	outputChars(stringNode(" >"), strlen(" >"), os);
}

static void 
outputHashtable(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("HASHTABLE"), os); 
}

static void 
outputReadtable(LispObj n, LispObj os)
{
	outputUnreadableObject(n, stringNode("READTABLE"), os); 
}

static void
outputUnreadableObject(LispObj obj, LispObj name, LispObj os)
{
	outputChars(stringNode("#< "), strlen("#< "), os); 
	outputChars(name, integer(vectorLength(name)), os); 
	outputChars(stringNode(": #x"), strlen(": #x"), os);
	outputInteger(wrapInteger(stripTag(obj)), os, 16);
	outputChars(stringNode(" >"), strlen(" >"), os);
}

// outputs a vector of characters to a character stream
void 
outputChars(LispObj chars, long start, long length, LispObj os)
{
	long i = 0;
	if ((streamOutputBufferPos(os) + wrapInteger(length)) < streamOutputBufferLength(os))
	{
		memcpy(charArrayStart(streamOutputBuffer(os)) + integer(streamOutputBufferPos(os)), 
			charArrayStart(chars) + start, length * sizeof(LISP_CHAR));
		streamOutputBufferPos(os) = streamOutputBufferPos(os) + wrapInteger(length);
		for (i = 0; i < length; i++)
		{
			if (charArrayStart(chars)[i + start] == '\n')
				streamColPosition(os) = 0;
			else
				streamColPosition(os) += wrapInteger(1);
		}
	}
	else
	{
		for (i = 0; i < length; i++)
			outputChar(wrapCharacter(charArrayStart(chars)[i + start]), os);
	}
}

// outputs a vector of characters to a character stream
void 
outputChars(LispObj chars, long length, LispObj os)
{
	outputChars(chars, 0, length, os);
}

void 
outputChar(LispObj ch, LispObj os)
{
	if (streamOutputBufferPos(os) == streamOutputBufferLength(os))
		LispCall3(Funcall, FUNCALL, streamOverflowFunc(os), os);
	charArrayStart(streamOutputBuffer(os))[integer(streamOutputBufferPos(os))]
		= (LISP_CHAR)character(ch);
	streamOutputBufferPos(os) = streamOutputBufferPos(os) + wrapInteger(1);
	if (character(ch) == '\n')
		streamColPosition(os) = 0;
	else
		streamColPosition(os) += wrapInteger(1);
}

//
//	Returns number of chars in buffer.
//
long expandLineFeedsIntoBuffer(LISP_CHAR* src, long numChars, LISP_CHAR* buffer)
{
	LISP_CHAR* p = 0;
	LISP_CHAR* q = 0;
	long i = 0;
	if (numChars > 4096)
		Error("Cannot overflow more than 4096 characters at a time");
	for (i = 0; i < numChars; i++)
		buffer[i + numChars] = src[i];	
	p = buffer;
	q = p + numChars;
	for (i = 0; i < numChars; i++)
	{
		if (*q == ASCII_NEWLINE)
			*p++ = ASCII_CR, *p++ = *q++;
		else
			*p++ = *q++;
	}
	return p - buffer;
}

LispObj
consoleOverflow(LispObj s)
{
	long numBytes = 0;
	LispObj bytesToWrite = streamOutputBufferPos(s);
	long i = 0;
	LispObj buf = 0;
	LISP_CHAR* CharOverflowBuffer = 0;
	byte* ByteOutputBuffer = 0;

	if (streamBinary(s) != NIL)		// if binary stream
	{
		ByteOutputBuffer = new byte[(4096 + 1) * 2];
		buf = streamOutputBuffer(s);
		for (i = 0; i < integer(bytesToWrite); i++)
			ByteOutputBuffer[i] = byteArrayStart(buf)[i];
		numBytes = integer(bytesToWrite);
		if (CormanLispServer)
			CormanLispServer->OutputText((char*)ByteOutputBuffer, numBytes);
		delete [] ByteOutputBuffer;
	}
	else
	{
		CharOverflowBuffer = new LISP_CHAR[(4096 + 1) * 2];
		numBytes = expandLineFeedsIntoBuffer(charArrayStart(streamOutputBuffer(s)),
			integer(bytesToWrite), CharOverflowBuffer);
		if (CormanLispServer)
		{
			// convert from 16-bit to 8-bit chars
			ByteOutputBuffer = (byte*)CharOverflowBuffer;
			for (int i = 0; i < numBytes; i++)
				ByteOutputBuffer[i] = ByteOutputBuffer[i * 2];	// convert from 16-bit to 8-bit chars
			CormanLispServer->OutputText((char*)ByteOutputBuffer, numBytes);
		}
		else
		if (TextOutputFuncPtr)
			TextOutputFuncPtr((wchar_t*)CharOverflowBuffer, numBytes);
		delete [] CharOverflowBuffer;
	}
	streamOutputBufferPos(s) = 0;
	return bytesToWrite;
}

LispObj
fileOverflow(LispObj s)
{
	unsigned long charsWritten = 0;
	BOOL ret = 0;
	long numBytes = 0;
	LispObj buf = 0;
	long i = 0;
	LispObj bytesToWrite = 0;
	LISP_CHAR* CharOverflowBuffer = 0;
	byte* ByteOverflowBuffer = 0;

	bytesToWrite = streamOutputBufferPos(s);
	if (streamBinary(s) != NIL)		// if binary stream
	{
		ByteOverflowBuffer = new byte[4096 + 1];
		buf = streamOutputBuffer(s);
		for (i = 0; i < integer(bytesToWrite); i++)
			ByteOverflowBuffer[i] = byteArrayStart(buf)[i];
		numBytes = integer(bytesToWrite);
		ret = WriteFile(
			(void*)lispIntegerToLong(streamHandle(s)),	// handle of file to write 
			ByteOverflowBuffer,							// address of buffer that contains data
			numBytes,									// number of bytes to write
			&charsWritten,								// address of number of bytes write
			NULL);										// overlapped structure
		delete [] ByteOverflowBuffer;
	}
	else
	{
		CharOverflowBuffer = new LISP_CHAR[(4096 + 1) * 2];
		numBytes = expandLineFeedsIntoBuffer(charArrayStart(streamOutputBuffer(s)),
			integer(bytesToWrite), CharOverflowBuffer);
		ByteOverflowBuffer = (byte*)CharOverflowBuffer;
		for (int i = 0; i < numBytes; i++)
			ByteOverflowBuffer[i] = ByteOverflowBuffer[i * 2];	// convert from 16-bit to 8-bit chars
		ret = WriteFile(
			(void*)lispIntegerToLong(streamHandle(s)),	// handle of file to write 
			ByteOverflowBuffer,							// address of buffer that contains data
			numBytes,									// number of bytes to write
			&charsWritten,								// address of number of bytes write
			NULL);										// overlapped structure
		delete [] CharOverflowBuffer;
	}

	if (!ret)
		Error("Could not write to file ~A, error code = ~A", 
			s, createLispInteger(GetLastError()));

	streamOutputBufferPos(s) = 0;
	return bytesToWrite;
}

LispObj
stringOverflow(LispObj s)
{
	long handle = 0;
	LispObj newstr = 0;
	long t1 = 0;
	long t2 = 0;
 	LispObj bytesToWrite = streamOutputBufferPos(s);

	handle = streamHandle(s);				// handle is a string
	t1 = integer(vectorLength(handle));		// current string length
	t2 = integer(bytesToWrite);				// chars in buffer to add to string

	newstr = charVector(wrapInteger(t1 + t2));
	memcpy(charArrayStart(newstr), charArrayStart(handle), t1 * sizeof(LISP_CHAR));
	memcpy(charArrayStart(newstr) + t1, charArrayStart(streamOutputBuffer(s)), t2 * sizeof(LISP_CHAR));
	streamHandle(s) = newstr;
	streamOutputBufferPos(s) = 0;
	return bytesToWrite;
}

void 
flushStream(LispObj os)
{
	if (streamOverflowFunc(os) != NIL)
		LispCall3(Funcall, FUNCALL, streamOverflowFunc(os), os);
}
