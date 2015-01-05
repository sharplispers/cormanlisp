//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		read.cpp
//		Contents:	Corman Lisp kernel reader
//		History:	6/5/96  RGC  Created.
//
#include "stdafx.h"
#include <ctype.h>
#include <stdlib.h>
#include <io.h>
#include <fcntl.h>

#pragma warning (disable:4127)				// conditional expression is constant

#include "lisp.h"
#include "CormanLispServer.h"

// static functions defined in this file
static LispObj validInteger(const char* token);
static LispObj validFloat(const char* token);
static LispObj backquoteProcess(LispObj);
void terminatingMacro(LispObj readtable, long c, LispObj func);
void addDispatchingNonTerminatingMacro(LispObj readtable, long c);
void setDispatchFunction(LispObj readtable, long c, long dispatchChar, LispObj func);
LispObj dispatchFunction(LispObj readtable, long c, long dispatchChar);
LispObj createNumber(const char* str);
LispObj createSymbol(const char* str);

#define READTABLE_START(rt) arrayStart(UVECTOR(rt)[READTABLE_TABLE])


// UpperAlpha only converts alphabetic characters
inline LispObj UpperAlpha(LispObj c) 
{
	return islower(character(c)) ? wrapCharacter(toupper(character(c))) : c;
}

LispObj readtableNode() 
{
	// create table
	LispObj table = 0;
	int i = 0;
	LispObj readtable = 0;

	table = vectorNode(wrapInteger(512));

	for (i = 0; i < 512; i += 2)
	{	
		arrayStart(table)[i] = CONSTITUENT_CHAR_TYPE;
		arrayStart(table)[i + 1] = NIL;
	}

	// initialize whitespace characters
	arrayStart(table)['\t' * 2] = WHITESPACE_CHAR_TYPE;
	arrayStart(table)[' ' * 2] = WHITESPACE_CHAR_TYPE;
	arrayStart(table)[12 * 2] = WHITESPACE_CHAR_TYPE;			// new page character
	arrayStart(table)[ASCII_CR * 2] = WHITESPACE_CHAR_TYPE;
	arrayStart(table)[ASCII_NEWLINE * 2] = WHITESPACE_CHAR_TYPE;
	arrayStart(table)[0 * 2] = WHITESPACE_CHAR_TYPE;

	// initialize escape characters
	arrayStart(table)['\\' * 2] = SINGLE_ESCAPE_CHAR_TYPE;
	arrayStart(table)['|' * 2] = MULTIPLE_ESCAPE_CHAR_TYPE;

	readtable = AllocVector(READTABLE_SIZE);
	setUvectorType(readtable, ReadTableType);

	UVECTOR(readtable)[READTABLE_BACKQUOTE_PROCESSING] = NIL;
	UVECTOR(readtable)[READTABLE_READ_LEVEL] = 0;
	UVECTOR(readtable)[READTABLE_TABLE] = table;
	UVECTOR(readtable)[READTABLE_CASE] = NIL;

	// initialize macros
	terminatingMacro(readtable, '"', symbolFunction(findSymbol("%DOUBLEQUOTEMACRO")));
	terminatingMacro(readtable, '\'', symbolFunction(findSymbol("%QUOTEMACRO")));
	terminatingMacro(readtable, '(', symbolFunction(findSymbol("%LEFTPARENMACRO")));
	terminatingMacro(readtable, ')', symbolFunction(findSymbol("%RIGHTPARENMACRO")));
	terminatingMacro(readtable, ',', symbolFunction(findSymbol("%COMMAMACRO")));
	terminatingMacro(readtable, ';', symbolFunction(findSymbol("%SEMICOLONMACRO")));
	terminatingMacro(readtable, '`', symbolFunction(findSymbol("%BACKQUOTEMACRO")));
	addDispatchingNonTerminatingMacro(readtable, '#');

	setDispatchFunction(readtable, '#', '\'', symbolFunction(findSymbol("%POUNDQUOTEMACRO")));
	setDispatchFunction(readtable, '#', '(', symbolFunction(findSymbol("%POUNDLEFTPARENMACRO")));
	setDispatchFunction(readtable, '#', '\\', symbolFunction(findSymbol("%POUNDBACKSLASHMACRO")));
	setDispatchFunction(readtable, '#', '|', symbolFunction(findSymbol("%BRACKETEDCOMMENTMACRO")));

	return readtable;
}

//
//		readExpression()
//		The following code implements the lisp reader state machine as 
//		described Steele's Common Lisp the Language, pp. 335-338
//		Returns UNINITIALIZED if end of file.
//		Otherwise returns NIL, if no values were returned, or a list of one item.
//
LispObj readExpression(LispObj stream)
{
	int state = 1;
	LispObj x = 0;
	LispObj y = 0;
	LispObj z = 0;
	int index = 0;
	LispObj func = NIL;
	LispObj thirdDispatchArg = UNINITIALIZED;
	LispObj ret = 0;
	long dispatchInt = 0;
	LispObj callret = 0;
	LispObj charType = 0;
	LispObj readtable = 0;

	static char token[128];
	token[0] = 0;
	readtable = symbolValue(READTABLE);

	while (1)
	{
		switch (state)
		{
		case 1:
			x = getCharacter(stream);
			if (x == Eof)
				return UNINITIALIZED;

			charType = READTABLE_START(readtable)[character(x) * 2];
			if (charType == ILLEGAL_CHAR_TYPE)
			{
				Error("Illegal character found in input", 0);
			}
			else
			if (charType == WHITESPACE_CHAR_TYPE)
			{
				// no change in state -- state = 1;
			}
			else
			if (charType == TERMINATING_MACRO_CHAR_TYPE
				|| charType == NON_TERMINATING_MACRO_CHAR_TYPE)
			{
				callret = LispCall3(Funcall, READTABLE_START(readtable)[character(x) * 2 + 1],
									stream, x);
				if (NumReturnValues == 0)
					return NIL;
				else
					return cons(callret, NIL);
			}
			else
			if (charType == DISPATCHING_TERMINATING_MACRO_CHAR_TYPE
				|| charType == DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE)
			{
				y = getCharacter(stream);
				if (y == Eof)
					Error("Unexpected end of file", 0);
				y = UpperAlpha(y);
				if (isdigit(character(y)))
				{
					dispatchInt = 0;	
					while (isdigit(character(y)))
					{
						dispatchInt *= 10;
						dispatchInt += (character(y) - '0');
						y = getCharacter(stream);
						if (y == Eof)
							Error("Unexpected end of file", 0);
						y = UpperAlpha(y);
					}
					thirdDispatchArg = wrapInteger(dispatchInt);
				}
				else
					thirdDispatchArg = NIL;
					
				func = dispatchFunction(readtable, character(x), character(y));
				if (func == UNINITIALIZED || func == NIL)
					Error("Invalid input form", 0);
				callret = LispCall4(Funcall, func, stream, y, thirdDispatchArg);
				if (NumReturnValues == 0)
					return NIL;
				else
					return cons(callret, NIL);
			}
			else
			if (charType == SINGLE_ESCAPE_CHAR_TYPE)
			{
				y = getCharacter(stream);
				if (y == Eof)
					Error("Unexpected end of file", 0);
				else
					token[index++] = (char)character(y);
				state = 8;
			}
			else
			if (charType == MULTIPLE_ESCAPE_CHAR_TYPE)
			{
				state = 9;
			}
			else
			if (charType == CONSTITUENT_CHAR_TYPE)
			{
				x = UpperAlpha(x);
				token[index++] = (char)character(x);
				state = 8;
			}
			else
				Error("Invalid character type");
			break;

		case 8:
			y = getCharacter(stream);
			if (y == Eof)
				state = 10;
			else
			{
				charType = READTABLE_START(readtable)[character(y) * 2];
				if (charType == CONSTITUENT_CHAR_TYPE
					|| charType == NON_TERMINATING_MACRO_CHAR_TYPE
					|| charType == DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE)
				{
					y = UpperAlpha(y);
					token[index++] = (char)character(y);
					// state stays the same
				}
				else
				if (charType == SINGLE_ESCAPE_CHAR_TYPE)
				{
					z = getCharacter(stream);
					if (z == Eof)
						Error("Unexpected end of file", 0);
					token[index++] = (char)character(z);
					// state stays the same
				}
				else
				if (charType == MULTIPLE_ESCAPE_CHAR_TYPE)
				{
					state = 9;
				}
				else
				if (charType == ILLEGAL_CHAR_TYPE)
				{
					Error("Illegal character found in input", 0);
				}
				else
				if (charType == TERMINATING_MACRO_CHAR_TYPE
					|| charType == DISPATCHING_TERMINATING_MACRO_CHAR_TYPE)
				{
					putbackCharacter(stream, y);
					state = 10;
				}
				else
 				if (charType == WHITESPACE_CHAR_TYPE)
				{
					// if read-preserving-whitespace, putback(y) here
					state = 10;
				}
			}
			break;

		case 9:
			y = getCharacter(stream);
			if (y == Eof)
				Error("Unexpected end of file", 0);

			charType = READTABLE_START(readtable)[character(y) * 2];
 			if (charType == CONSTITUENT_CHAR_TYPE
				|| charType == TERMINATING_MACRO_CHAR_TYPE
				|| charType == NON_TERMINATING_MACRO_CHAR_TYPE
				|| charType == DISPATCHING_TERMINATING_MACRO_CHAR_TYPE
				|| charType == DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE
				|| charType == WHITESPACE_CHAR_TYPE)
			{
				token[index++] = (char)character(y);
				// state stays the same
			}
			else
			if (charType == SINGLE_ESCAPE_CHAR_TYPE)
			{
				z = getCharacter(stream);
				if (z == Eof)
					Error("Unexpected end of file", 0);
				token[index++] = (char)character(z);
				// state stays the same
			}
 			else
			if (charType == MULTIPLE_ESCAPE_CHAR_TYPE)
			{
				state = 8;
			}
			else
			if (charType == ILLEGAL_CHAR_TYPE)
			{
				Error("Illegal character found in input", 0);
			}
			break;
			
		case 10:
			token[index] = 0;
			ret = createNumber(token);
			if (ret != UNINITIALIZED)
				return list(ret, END_LIST);
			return list(createSymbol(token), END_LIST);
		}
	}
	return NIL;
}

LispObj createNumber(const char* str)
{
	LispObj n = 0;
	n = validInteger(str);
	if (n == UNINITIALIZED)
		n = validFloat(str);
	return n;
}

LispObj createSymbol(const char* str)
{
	return findSymbol(str);
}

//
//	validDigit()
//	Returns -1 if not a digit in the current read-base, else returns
//	the number represented by the digit.
//
static int validDigit(long c, int rbase)
{
	int ext_rbase = 0;
	if (rbase > 10)
		ext_rbase = rbase - 10, rbase = 10; 
	
	if (islower(c))
		c = toupper(c);
	if (c >= '0' && c < ('0' + rbase))
		return c - '0';
	if (ext_rbase && c >= 'A' && c < ('A' + ext_rbase))
		return (c - 'A' + 10);
	return -1;
}

static LispObj validInteger(const char* token)
{
	int digits = 0;
	const char* str = token;
	xbool negative = FALSE;
	long value = 0;
	long rbase = 10;
		
	if (*str == '+')
		str++;
	else
	if (*str == '-')
	{
		negative = TRUE;
		str++;
	}

	while (*str)
	{
		int d = validDigit(*str, (int)rbase);
		if (d < 0)
			break;
		value *= rbase;
		value += d;
		digits++;
		str++;
	}

	if (*str == '.')				// allow for trailing period
		str++;
		
	if (*str != (long) 0)			// if any other trailing characters, invalid
		return UNINITIALIZED;

	if (digits > 0)
	{
		if (negative)
			value = -value;
		return wrapInteger(value);
	}
	return UNINITIALIZED;
}

static LispObj validFloat(const char* token)
{
	LispObj temp = 0;
	int decimal = 0;
	int digits = 0;
	int expDigits = 0;
	const char* str = token;
	
	if (*str == '+' || *str == '-')
		str++;
		
	// check mantissa
	while (*str)
	{
		if (isdigit(*str))
			digits++;
		else
		if (*str == '.')
		{
			decimal++;
			if (decimal > 1)
				return 0;		// more than one decimal point!
		}
		else
			break;
		str++;
	}
	
	if (digits == 0)
		return UNINITIALIZED;
		
	if (*str == 'E' || *str == 'e')			// get exponent
	{
		str++;
		if (*str == '+' || *str == '-')		// allow for sign on exponent
			str++;

		while (isdigit(*str))
		{
			str++;
			expDigits++;
		}
		if (expDigits == 0)
			return 0;
	}
	else
		if (*str != (long) 0)
			return UNINITIALIZED;

	temp = doubleFloatNode(0);
	doubleFloat(temp) = atof(token); 
	return temp;
}

void nonTerminatingMacro(LispObj readtable, long c, LispObj func)
{
	READTABLE_START(readtable)[c * 2] = NON_TERMINATING_MACRO_CHAR_TYPE;	
	READTABLE_START(readtable)[c * 2 + 1] = func;
}

LispObj getMacro(LispObj readtable, long c)
{
	if (READTABLE_START(readtable)[c * 2] == NON_TERMINATING_MACRO_CHAR_TYPE ||
			READTABLE_START(readtable)[c * 2] == TERMINATING_MACRO_CHAR_TYPE)
		return READTABLE_START(readtable)[c * 2 + 1];
	else
		return NIL;
}

void terminatingMacro(LispObj readtable, long c, LispObj func)
{
	READTABLE_START(readtable)[c * 2] = TERMINATING_MACRO_CHAR_TYPE;	
	READTABLE_START(readtable)[c * 2 + 1] = func;
}

xbool isTerminatingMacro(LispObj readtable, long c)
{
	if (READTABLE_START(readtable)[c * 2] == TERMINATING_MACRO_CHAR_TYPE
		|| READTABLE_START(readtable)[c * 2] == DISPATCHING_TERMINATING_MACRO_CHAR_TYPE)
			return TRUE;
	else
		return FALSE;
}

xbool isWhiteSpace(LispObj readtable, long c)
{
	if (READTABLE_START(readtable)[c * 2] == WHITESPACE_CHAR_TYPE)
		return TRUE;
	else
		return FALSE;
}

void addDispatchingNonTerminatingMacro(LispObj readtable, long c)
{
	LispObj t1 = 0;
	t1 = vectorNode(wrapInteger(256));
	READTABLE_START(readtable)[c * 2] = DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE;	
	READTABLE_START(readtable)[c * 2 + 1] = t1;
}

void addDispatchingTerminatingMacro(LispObj readtable, long c)
{
	LispObj t1 = 0;
	t1 = vectorNode(wrapInteger(256));
	READTABLE_START(readtable)[c * 2] = DISPATCHING_TERMINATING_MACRO_CHAR_TYPE;
	READTABLE_START(readtable)[c * 2 + 1] = t1;
}

void setDispatchFunction(LispObj readtable, long c, long dispatchChar, LispObj func)
{
	if (READTABLE_START(readtable)[c * 2] != DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE
			&& READTABLE_START(readtable)[c * 2] != DISPATCHING_TERMINATING_MACRO_CHAR_TYPE)
		return;

	arrayStart(READTABLE_START(readtable)[c * 2 + 1])[dispatchChar] = func;
}

// 
//	getDispatchFunction()
//	Returns the dispatch function associated with a character pair.
//
LispObj dispatchFunction(LispObj readtable, long c, long dispatchChar)
{
	if (READTABLE_START(readtable)[c * 2] != DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE
			&& READTABLE_START(readtable)[c * 2] != DISPATCHING_TERMINATING_MACRO_CHAR_TYPE)
		return NIL;

	return arrayStart(READTABLE_START(readtable)[c * 2 + 1])[dispatchChar];
}

//
//	isDispatchingMacro()
//	Returns TRUE if the passed char is a dispatching macro, either terminating
//	or non-terminating.
//	Returns FALSE otherwise.
//
xbool isDispatchingMacro(LispObj readtable, long c)
{
	if (READTABLE_START(readtable)[c * 2] == DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE
		|| READTABLE_START(readtable)[c * 2] == DISPATCHING_TERMINATING_MACRO_CHAR_TYPE)
			return TRUE;
	else
		return FALSE;
}

LispObj readChar(LispObj is)
{
	LispObj c = 0;
	c = getCharacter(is);
	if (c == Eof)
		Error("Unexpected end of file", 0);
	return c;
}
	
LispObj peekChar(LispObj is)
{
	LispObj c = 0;
	c = getCharacter(is);
	if (c == Eof)
		Error("Unexpected end of file", 0);
	putbackCharacter(is, c);
	return c;
}
	
LispObj peekCharSkippingWS(LispObj readtable, LispObj is)
{
	LispObj c = 0;
	while (TRUE)
	{
		c = getCharacter(is);
		if (c == Eof)
			Error("Unexpected end of file", 0);
		if (READTABLE_START(readtable)[character(c) * 2] != WHITESPACE_CHAR_TYPE)
			break;
	}
	putbackCharacter(is, c);
	return c;
}

LispFunction(read_delimited_list)
{
	LISP_FUNC_BEGIN(2);

	ret = Cread_delimited_list(LISP_ARG(0), LISP_ARG(1));

	LISP_FUNC_RETURN(ret);
}	

//
//		Common Lisp 'read-delimited-list' function.
//
LispObj 
Cread_delimited_list(LispObj ch, LispObj stream)
{
	checkStream(stream);
	checkChar(ch);
	LispObj searchChar = ch;
	LispObj rt = symbolValue(READTABLE);

	LispObj lis = NIL;
	LispObj p = lis;
	LispObj n = NULL;
	LispObj c = 0;
	int foundDot = 0;
	int count = 0;
	if (symbolValue(SOURCE_LINE) == NIL)
		setSymbolValue(SOURCE_LINE, UVECTOR(stream)[STREAM_LINE_NUMBER]);
	while (TRUE)
	{
		c = peekCharSkippingWS(rt, stream);
		if (c == searchChar)		// if found terminator
		{
			c = readChar(stream);		// consume it
			if (foundDot == 1)
				Error("List ends with dot", 0);
			goto exit;
		}

		if (c == wrapCharacter('.'))
		{
			n = LispCall5(Funcall, READ, stream, T, NIL, T);
			if (NumReturnValues > 0 && n == DOT)
			{
				if (foundDot)
					Error("Multiple dots in list", 0);
				if (!count)
					Error("Invalid position for dot", 0);
				foundDot = 1;
				continue;
			}
		}
		else	
			n = LispCall5(Funcall, READ, stream, T, NIL, T);
			
		if (n == UNINITIALIZED)
			Error("Unexpected end of file", 0); 

		if (NumReturnValues > 0)
		{
			if (foundDot == 1)
			{
				if (p == NIL)
					p = lis = n;
				else
				{
					CDR(p) = n;				
					p = CDR(p);
				}
				foundDot++;
			}
			else
			{
				if (foundDot >= 2)
					Error("Error in dotted expression", 0);
				if (p == NIL)
					p = lis = cons(n, NIL);
				else
				{
					CDR(p) = cons(n, NIL);				
					p = CDR(p);
				}
				count++;
			}
		}
	}
exit:
		
	return lis;
}	
	

//
//		Macro functions used by the standard Common Lisp read table
//

//
//	doublequoteMacro()
//	This returns a string, and looks for a terminating character which matches
//	the character it was called with.
//
LispFunction(doublequoteMacro)
{
	LISP_FUNC_BEGIN(2);
	LispObj c = 0;
	int index = 0;
	LispObj stream = LISP_ARG(0);
	LispObj ch = LISP_ARG(1);
	LispObj doublequoteChar = ch;

	static char literal[1024];
	literal[0] = 0;

	while (1)
	{
		c = getCharacter(stream);
		if (c == Eof)
			break;
		if (c == doublequoteChar)
			break;
		if (c == wrapCharacter('\\'))
		{
			c = getCharacter(stream);
			if (c == Eof)
				Error("Unexpected end of file");
//			if (c == wrapCharacter('n'))
//				c = ASCII_CR;
		}
		literal[index++] = (char)character(c);
	}
	if (c == Eof)
		Error("Unexpected end of file", 0);
	literal[index] = 0;
	ret = stringNode(literal);

	LISP_FUNC_RETURN(ret);
}

LispFunction(quoteMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj stream = LISP_ARG(0);

	ret = list(QUOTE, LispCall2(Funcall, symbolFunction(READ), stream), END_LIST);

	LISP_FUNC_RETURN(ret);
}

LispFunction(leftparenMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj stream = LISP_ARG(0);

	ret = Cread_delimited_list(charNode(')'), stream);

	LISP_FUNC_RETURN(ret);
}

LispFunction(rightparenMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LISP_FUNC_RETURN(RIGHT_PAREN);
}

//
//	standard comment to end of line macro
//	Skips characters to the end of the current line.
//
LispFunction(semicolonMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj stream = LISP_ARG(0);
	LispObj c = 0;

	while (1)
	{
		c = getCharacter(stream);
		if (c == wrapCharacter(ASCII_CR) || c == wrapCharacter(ASCII_NEWLINE))
		{
			putbackCharacter(stream, c);
			break;
		}
			
		if (c == Eof)				// read characters to end of line
			break;					// no special handling of EOF required here
	}

	LISP_FUNC_RETURN_NO_VALUES();
}

//
//	standard comment to end of line macro
//	Skips characters to the end of the current line.
//
LispFunction(bracketedCommentMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 3);
	LispObj stream = LISP_ARG(0);
	LispObj c = 0;

	while (1)
	{
		c = getCharacter(stream);
		if (c == wrapCharacter('|'))
		{
			c = peekChar(stream);
			if (c == Eof)
				break;
			if (c == wrapCharacter('#'))
			{
				c = getCharacter(stream);
				break;
			}
		}
			
		if (c == Eof)				// read characters to end of line
			break;					// no special handling of EOF required here
	}

	LISP_FUNC_RETURN_NO_VALUES();
}

// 
//	Common Lisp standard backquote macro mechanism.
//
LispFunction(backquoteMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 3);
	LispObj rt = 0;
	LispObj saveBackquoteProcessing = 0;
	LispObj stream = 0;
	LispObj s = 0;

	rt = symbolValue(READTABLE);
	saveBackquoteProcessing = UVECTOR(rt)[READTABLE_BACKQUOTE_PROCESSING];
	UVECTOR(rt)[READTABLE_BACKQUOTE_PROCESSING] = T;
	stream = LISP_ARG(0);
	s = LispCall2(Funcall, symbolFunction(READ), stream);
	UVECTOR(rt)[READTABLE_BACKQUOTE_PROCESSING] = saveBackquoteProcessing;

	// if not a list, just quote it
	if (!isCons(s))
		return list(QUOTE, s, END_LIST);

	ret = backquoteProcess(s);

	LISP_FUNC_RETURN(ret);
}

static LispObj backquoteProcess(LispObj s)
{
	LispObj newlis = 0;
	LispObj np = 0;
	LispObj n = 0;

	// if not a list, just quote it
	if (!isCons(s))
		return s;

	// else do backquote processing
	newlis = cons(APPEND, NIL);
	np = newlis;
	while (isCons(s))
	{
		n = CAR(s);
		if (!isCons(n))
		{
			// the following three cases should only arise if
			// a comma expression followed a dot
			if (n == COMMA_TOKEN && isCons(CDR(s)))
			{
				if (!isCons(CAR(CDR(s))))
					CDR(np) = cons(CAR(CDR(s)), NIL);
				else
					CDR(np) = cons(eval(backquoteProcess(CAR(CDR(s))), NIL), NIL);
				return newlis;
			}
			else
			if (n == COMMA_DOT_TOKEN && isCons(CDR(s)))
			{
				if (!isCons(CAR(CDR(s))))
					CDR(np) = CAR(CDR(s));
				else
					CDR(np) = eval(backquoteProcess(CAR(CDR(s))), NIL);
				return newlis;
			}
			else 
			if (n == COMMA_ATSIGN_TOKEN && isCons(CDR(s)))
			{
				if (!isCons(CAR(CDR(s))))
					CDR(np) = CAR(CDR(s));
				else
					CDR(np) = eval(backquoteProcess(CAR(CDR(s))), NIL);
				return newlis;
			}

			CDR(np) = list(cons(LIST, list(list(QUOTE, n, END_LIST), END_LIST)), END_LIST);
			np = CDR(np);
		}
		else
		if (CAR(n) == COMMA_TOKEN && isCons(CDR(n)))
		{
			if (!isCons(CAR(CDR(n))))
				CDR(np) = list(list(LIST, CAR(CDR(n)), END_LIST), END_LIST);
			else
				CDR(np) = list(list(LIST, eval(backquoteProcess(CAR(CDR(n))), NIL), END_LIST), END_LIST);
			np = CDR(np);
		}
		else
		if (CAR(n) == COMMA_DOT_TOKEN && isCons(CDR(n)))
		{
			if (!isCons(CAR(CDR(n))))
				CDR(np) = cons(CAR(CDR(n)), NIL);
			else
				CDR(np) = list(eval(backquoteProcess(CAR(CDR(n))), NIL), END_LIST);
			np = CDR(np);
		}
		else
		if (CAR(n) == COMMA_ATSIGN_TOKEN && isCons(CDR(n)))
		{
			if (!isCons(CAR(CDR(n))))
				CDR(np) = cons(CAR(CDR(n)), NIL);
			else
				CDR(np) = list(eval(backquoteProcess(CAR(CDR(n))), NIL), END_LIST);
			np = CDR(np);
		}
		else
		{
			CDR(np) = list(list(LIST, backquoteProcess(n), END_LIST), END_LIST);
			np = CDR(np);
		}
		s = CDR(s);
	}

	if (s != NIL)
		CDR(np) = list(list(QUOTE, s, END_LIST), END_LIST);	// handle CDR of dot expression	
	return newlis;
}

//
//	Lisp::poundLeftParen_macro()
//	Normal vector reading character macro.
//
LispFunction(poundLeftParenMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(2, 3);
	LispObj n = 0;
	LispObj stream = LISP_ARG(0);
	LispObj ch = LISP_ARG(1);

	putbackCharacter(stream, ch);
	n = LispCall2(Funcall, symbolFunction(READ), stream);
	if (!isList(n))
		Error("Expected a list while reading");
	if (ARG_COUNT == 3 && isInteger(LISP_ARG(2)))
		ret = vectorNode(LISP_ARG(2), n);
	else
		ret = vectorNode(wrapInteger(listLength(n)), n);

	LISP_FUNC_RETURN(ret);
}

LispFunction(poundQuoteMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 3);
	LispObj stream = LISP_ARG(0);
	LispObj n = 0;

	n = LispCall2(Funcall, symbolFunction(READ), stream);
	ret = list(FUNCTION, n, END_LIST);

	LISP_FUNC_RETURN(ret);
}

LispFunction(poundBackslashMacro)
{
	LISP_FUNC_BEGIN(3);
	LispObj stream = LISP_ARG(0);
//	LispObj sub_char = LISP_ARG(1);
//	LispObj arg = LISP_ARG(2);

	ret = getCharacter(stream);
	if (ret == Eof)
		Error("End of file encountered");

	LISP_FUNC_RETURN(ret);
}

LispFunction(commaMacro)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 3);
	LispObj rt = 0;
	LispObj stream = 0;
	LispObj c = 0;
	LispObj x = 0;

	rt = symbolValue(READTABLE);
	stream = LISP_ARG(0);
	if (UVECTOR(rt)[READTABLE_BACKQUOTE_PROCESSING] == NIL)
		Error("Invalid form: comma found outside backquote form");
	c = getCharacter(stream);
	if (c == Eof)
		Error("Unexpected end of file");
	if (c == wrapCharacter('@'))
	{
		x = LispCall2(Funcall, symbolFunction(READ), stream);
		ret = list(COMMA_ATSIGN_TOKEN, x, END_LIST); 
	}
	else
	if (c == wrapCharacter('.'))
	{
		x = LispCall2(Funcall, symbolFunction(READ), stream);
		ret = list(COMMA_DOT_TOKEN, x, END_LIST); 
	}
	else
	{
		putbackCharacter(stream, c);
		x = LispCall2(Funcall, symbolFunction(READ), stream);
		ret = list(COMMA_TOKEN, x, END_LIST);
	}

	LISP_FUNC_RETURN(ret);
}

static unsigned char InputUnderflowBuffer[8092];	// based on 4096 char stream buffers

//
//	Returns number of chars in buffer.
//
static unsigned long compressLineFeedsIntoBuffer(unsigned long numChars)
{
	unsigned char* p = 0;
	unsigned char* q = 0;
	unsigned long i = 0;
	if (numChars > 4096)
		Error("Cannot underflow more than 4096 characters at a time");
	p = q = InputUnderflowBuffer;
	for (i = 0; i < numChars; i++)
	{
		if (*q != ASCII_CR)
			*p++ = *q++;
		else
			q++;
	}
	return p - InputUnderflowBuffer;
}

LispObj consoleUnderflow(LispObj s)
{
	long num = 0;
	unsigned long charsRead = 0;
	unsigned long i = 0;
	LispObj buf = 0;

	// make sure output is flushed
	flushStream(symbolValue(CONSOLE_OUTPUT_STREAM));

	while (TerminalInputBuf.numchars() == 0) // loop until some input
	{
		PLSingleLock myLock(TerminalInputBuf.charsAvailable(), TRUE);
	}
	num = TerminalInputBuf.getCharsInBuffer(InputUnderflowBuffer,
				 integer(streamInputBufferLength(s)));

	if (streamBinary(s) == NIL)		// if character stream, remove CR characters
		charsRead = compressLineFeedsIntoBuffer((unsigned long)num);
	else
		charsRead = (unsigned long)num;
	buf = streamInputBuffer(s);
	for (i = 0; i < charsRead; i++)
		charArrayStart(buf)[i] = InputUnderflowBuffer[i];
	streamInputBufferPos(s) = 0;
	streamInputBufferNum(s) = wrapInteger((long)charsRead);
	return wrapInteger(charsRead);
}

// Returns T if there is more data to be read from the console, NIL otherwise.
LispFunction(Console_Chars_Available)
{
	LISP_FUNC_BEGIN(0);
	PLEvent* event = 0;
	BOOL result = 0;
	ret = NIL;
	event = TerminalInputBuf.charsAvailable();
	result = event->Lock(10);		// timeout of 10 ms
	if (result == TRUE)
	{
		if (TerminalInputBuf.numchars() > 0)
			ret = T;
		event->Unlock();
	}
	else
	{
		if (TerminalInputBuf.numchars() > 0)
			ret = T;
	}
	LISP_FUNC_RETURN(ret);
}

LispObj fileUnderflow(LispObj s)
{
	unsigned long charsRead = 0;
	BOOL ret = 0;
	unsigned long i = 0;
	LispObj buf = 0;

	// make sure the buffer is not write-protected
	charArrayStart(streamInputBuffer(s))[0] = 0;
	charArrayStart(streamInputBuffer(s))[2047] = 0;

	ret = ReadFile(
		(void*)lispIntegerToLong(streamHandle(s)),	// handle of file to read 
		InputUnderflowBuffer,						// address of buffer that receives data
		integer(streamInputBufferLength(s)),		// number of bytes to read
		&charsRead,								// address of number of bytes read
		NULL);									// overlapped structure

	if (!ret)
		Error("Could not read from file ~A, error code = ~A", 
			s, createLispInteger(GetLastError()));

	if (streamBinary(s) == NIL)		// if character stream, remove CR characters
	{
		charsRead = compressLineFeedsIntoBuffer(charsRead);
	}
	buf = streamInputBuffer(s);
	for (i = 0; i < charsRead; i++)
		charArrayStart(buf)[i] = InputUnderflowBuffer[i];
	streamInputBufferPos(s) = 0;
	streamInputBufferNum(s) = wrapInteger((long)charsRead);
	return wrapInteger(charsRead);
}

// This is implemented in Lisp.
LispObj stringUnderflow(LispObj /*s*/)
{
	Error("String input not implemented");
	return 0;
}

LispObj getCharacter(LispObj s)
{
	LispObj c = 0;
	if (streamInputBufferPos(s) == streamInputBufferNum(s))
		LispCall3(Funcall, FUNCALL, streamUnderflowFunc(s), s);
	if (streamInputBufferPos(s) == streamInputBufferNum(s))
		return Eof;
	c = wrapCharacter(charArrayStart(streamInputBuffer(s))[integer(streamInputBufferPos(s))]);
	streamInputBufferPos(s) = streamInputBufferPos(s) + wrapInteger(1);
	if (c == wrapCharacter(ASCII_NEWLINE))
		streamLineNumber(s) = streamLineNumber(s) + wrapInteger(1);
	return c;
}

void putbackCharacter(LispObj stream, LispObj c)
{
	if (streamInputBufferPos(stream) == 0)
		Error("Cannot put back character");
	if (c == wrapCharacter(ASCII_NEWLINE))
		streamLineNumber(stream) = 
			streamLineNumber(stream) - wrapInteger(1);
	streamInputBufferPos(stream) = streamInputBufferPos(stream) - wrapInteger(1);
}

