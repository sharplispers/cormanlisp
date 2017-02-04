//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		lispfunc.cpp
//		Contents:	Kernel functions callable from lisp
//		History:	6/19/96 RGC Created.
//					1/12/00 RGC Added EXECUTE_FINALIZERS to support post-GC finalization.
//
#include "stdafx.h"
#include <io.h>
#include <windows.h>
#include <sys/timeb.h>
#include <stdio.h>
#include <direct.h>
#include <malloc.h>

#include "lisp.h"
#include "lispmath.h"
#include "CormanLispServer.h"
#include "../../zlib/zlib.h"

#pragma warning (disable:4127)				// conditional expression is constant

static void updateKernelFunctions();

LispDeclare(Stack_Trace);
LispDeclare(Floor);
LispDeclare(Gcd);
LispFunction(Hash_eq_function);
LispFunction(Hash_eql_function);
LispFunction(Hash_equal_function);
LispFunction(Mod_Bignums);
LispFunction(Divide_Bignums);
LispFunction(Console_Chars_Available);
LispDeclare(Memory_Report);
LispDeclare(Lisp_Shutdown);

// redefined in lisp
LispFunction(SymbolValue)
{
	LISP_FUNC_BEGIN(1);
	LispObj val = 0;

	checkSymbol(LISP_ARG(0));
	val = symbolValue(LISP_ARG(0));
	if (val == UNINITIALIZED)
		Error("Unbound symbol: ~A", LISP_ARG(0));

	LISP_FUNC_RETURN(val);
}

// redefined in lisp
LispFunction(SymbolFunction)
{
	LISP_FUNC_BEGIN(1);
	LispObj sf = 0;

	checkSymbol(LISP_ARG(0));
	sf = symbolFunction(LISP_ARG(0));
	if (sf == UNINITIALIZED)
		Error("Symbol does not have a function binding: ~A", LISP_ARG(0));

	LISP_FUNC_RETURN(sf);
}

// redefined in lisp
LispFunction(Functionp)
{
	LISP_FUNC_BEGIN(1);
	
	ret = NIL;
	if (isFunction(LISP_ARG(0)))
		ret = T;

	LISP_FUNC_RETURN(ret);
}

// redefined in lisp
LispFunction(Macro_Function)
{
	LISP_FUNC_BEGIN(1);
	LispObj type = 0;
	LispObj sf = 0;

	checkSymbol(LISP_ARG(0));
	type = UVECTOR(LISP_ARG(0))[SYMBOL_FUNCTION_TYPE];
	sf = symbolFunction(LISP_ARG(0));
	if (sf == UNINITIALIZED || type != MACRO)
		sf = NIL;

	LISP_FUNC_RETURN(sf);
}

LispFunction(SetSymbolFunction)
{
	LISP_FUNC_BEGIN(2);

	checkSymbol(LISP_ARG(1));
	setSymbolFunction(LISP_ARG(1), LISP_ARG(0), FUNCTION);

	LISP_FUNC_RETURN(LISP_ARG(0));
}

LispFunction(SetSymbolMacro)
{
	LISP_FUNC_BEGIN(2);

	checkSymbol(LISP_ARG(1));
	setSymbolFunction(LISP_ARG(1), LISP_ARG(0), MACRO);

	LISP_FUNC_RETURN(LISP_ARG(0));
}

// redefined in lisp
LispFunction(listStar)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj n = 0;
	long i = 0;

	n = LISP_ARG(ARG_COUNT - 1);
	for (i = ARG_COUNT - 2; i >= 0; i--)
		n = cons(LISP_ARG(i), n);
	
	LISP_FUNC_RETURN(n);
}

// redefined in lisp
LispFunction(lispAppend)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj copy = NIL;
	LispObj p = copy;
	LispObj n = 0;
	long i = 0;
	
	for (i = 0; i < ARG_COUNT; i++)
	{
		n = LISP_ARG(i);
		checkList(n);
		if (i == ARG_COUNT - 1)		// if we are at the last element
		{
			if (copy == NIL)
			{
				LISP_FUNC_RETURN(n);
			}
			CDR(p) = n;
			{	
				LISP_FUNC_RETURN(copy);
			}
		}
		while (isCons(n))
		{
			if (copy == NIL)
				p = copy = cons(CAR(n), NIL);
			else
			{
				CDR(p) = cons(CAR(n), NIL);
				p = CDR(p);
			}
			n = CDR(n);
		}
	}
	LISP_FUNC_RETURN(copy);
}

// redefined in lisp
LispFunction(lispList)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj list = NIL;
	LispObj c = 0;
	long i = 0;

	for (i = ARG_COUNT - 1; i >= 0; i--)
	{
		c = AllocLocalCons();
		CAR(c) = LISP_ARG(i);
		CDR(c) = list;
		list = c;
	}
	LISP_FUNC_RETURN(list);
}

extern unsigned long NumReturnValues;

// redefined in lisp
__declspec(naked)
LispFunction(Funcall)
{
	__asm 
	{
		push	ebp
		mov		ebp, esp
		push	ebx
		push	edi
		push	0			;; one cell local storage at [ebp - 12]

		cmp		ecx, 1
		jge		short t1
		call 	WrongNumberOfArgs
	t1:
		mov		eax, [ebp + ecx*4 + 4]		;; eax = function
		mov		edx, eax
		and		edx, 7
		cmp		edx, UvectorTag				;; see if func arg is a uvector
		je		short t2
		push	eax
		call	checkFunction
	t2:
		mov		edx, [eax - UvectorTag]		;; edx = uvector header
		shr 	dl, 3
		cmp		dl, SymbolType				;; see if it is a symbol
		jne		short t3
		;; get the function that is bound to the symbol
		mov		eax, [eax + ((SYMBOL_FUNCTION * 4) - UvectorTag)]
		mov		eax, [eax - 4]
		mov		edx, eax
		and		edx, 7
		cmp		edx, UvectorTag
		je 		short t9
		push	dword ptr [ebp + ecx*4 + 4]
		call	checkFunction
	t9:
		mov		edx, [eax - UvectorTag]		;; edx = uvector header
		shr 	dl, 3
	t3: ;; we now know we have a function in eax, and dl is the type

		;; push all the arguments
		mov		[ebp - 12], esp
		mov		ebx, ecx
		dec		ecx
	t4:
		dec		ebx
		jle		short t5
		push 	dword ptr [ebp + ebx*4 + 4]
		jmp		short t4
	t5:
		cmp 	dl, FunctionType
		jne		short t6
		mov		edi, [eax + ((FUNCTION_ENVIRONMENT * 4) - UvectorTag)] 
		mov		eax, [eax + ((FUNCTION_ADDRESS * 4) - UvectorTag)]
		lea		eax, [eax + ((COMPILED_CODE_OFFSET * 4) - UvectorTag)]
		call	eax	
		jmp		short t8
	t6:
		cmp 	dl, KFunctionType
		jne		short t7
		mov		edi, [esi]		;; environment for kfunctions is always NIL
		call	[eax + ((FUNCTION_ADDRESS * 4) - UvectorTag)]
		jmp		short t8
	t7:
		push	eax
		call	checkFunction
	t8:
		mov		esp, [ebp - 12]
		pop		edi
		pop		edi
		pop		ebx
		mov		esp, ebp
		pop		ebp
		ret
	}
}

#define ARGS_OFFSET	8

// redefined in lisp
__declspec(naked)
LispFunction(Apply)
{
	__asm 
	{
		push	ebp
		mov		ebp, esp
		push	ebx
		push	edi
		push	0			;; one cell local storage at [ebp - 12]

		cmp		ecx, 2
		jge		short t1
		call 	WrongNumberOfArgs
	t1:
		mov		eax, [ebp + ecx*4 + 4]		;; eax = function
		mov		edx, eax
		and		edx, 7
		cmp		edx, UvectorTag				;; see if func arg is a uvector
		je		short t2
		push	eax
		call	checkFunction
	t2:
		mov		edx, [eax - UvectorTag]		;; edx = uvector header
		shr 	dl, 3
		cmp		dl, SymbolType				;; see if it is a symbol
		jne		short t4
		;; get the function that is bound to the symbol
		mov		eax, [eax + ((SYMBOL_FUNCTION * 4) - UvectorTag)]
		mov		eax, [eax - 4]
		mov		edx, eax
		and		edx, 7
		cmp		edx, UvectorTag
		je 		short t3
		push	dword ptr [ebp + ecx*4 + 4]
		call	checkFunction
	t3:
		mov		edx, [eax - UvectorTag]		;; edx = uvector header
		shr 	dl, 3
	t4: ;; we now know we have a function in eax, and dl is the type

		;; push all the arguments except the last
		mov		[ebp - 12], esp
		dec		ecx
		mov		ebx, ecx
		dec		ecx
	t5:
		dec		ebx
		jle		short t6
		push 	dword ptr [ebp + ebx*4 + 8]
		jmp		short t5
	t6:
		;; the last argument is a list of remaining arguments
		mov		edi, [ebp + ARGS_OFFSET]
	t7:
		mov		ebx, edi
		and		ebx, 7
		cmp		ebx, ConsTag				;; is a cons cell?
		jne		short t8					;; if not, exit
		push	[edi - 4]
		inc		ecx
		mov		edi, [edi]
		jmp		short t7
	t8:
		cmp 	dl, FunctionType
		jne		short t9
		mov		edi, [eax + ((FUNCTION_ENVIRONMENT * 4) - UvectorTag)] 
		mov		eax, [eax + ((FUNCTION_ADDRESS * 4) - UvectorTag)]
		lea		eax, [eax + ((COMPILED_CODE_OFFSET * 4) - UvectorTag)]
		call	eax	
		jmp		short t11
	t9:
		cmp 	dl, KFunctionType
		jne		short t10
		mov		edi, [esi]		;; environment for kfunctions is always NIL
		call	[eax + ((FUNCTION_ADDRESS * 4) - UvectorTag)]
		jmp		short t11
	t10:
		push	eax
		call	checkFunction
	t11:
		mov		esp, [ebp - 12]
		pop		edi				;; remove local storage
		pop		edi
		pop		ebx
		mov		esp, ebp
		pop		ebp
		ret
	}
}

//
//	Lisp function create-closure
//	Takes a function and an environment, and returns a new function.
//
LispFunction(create_closure)
{
	LISP_FUNC_BEGIN(2);
	LispObj func = LISP_ARG(0);
	LispObj environment = LISP_ARG(1);
	LispObj n = 0;

	n = AllocVector(FUNCTION_SIZE);		// need two slots
	setUvectorType(n, FunctionType);
	UVECTOR(n)[FUNCTION_ENVIRONMENT] = environment;
	UVECTOR(n)[FUNCTION_ADDRESS] = UVECTOR(func)[FUNCTION_ADDRESS];
	
	LISP_FUNC_RETURN(n);
}

LispFunction(gc)
{
	LISP_FUNC_BEGIN_VARIABLE(0, 1);
	LispObj level = 0;

	if (ARG_COUNT == 1)
		level = LISP_ARG(0);
	
	garbageCollect(integer(level));

	LISP_FUNC_RETURN(T);
}

//
//	Non-standard 
//  Usage: (SAVE-IMAGE filename)
//	Returns NIL.
//
LispFunction(SaveLispImage)
{
	LISP_FUNC_BEGIN(1);
	LispObj path = LISP_ARG(0);
	LispObj funcs = 0;
	checkString(path);
	funcs = symbolValue(SAVE_IMAGE_CLEANUP_FUNCS);
	while (isCons(funcs))
	{
		LispCall1(Funcall, CAR(funcs));
		funcs = CDR(funcs);
	}
	writeHeapToFile(path);

	LISP_FUNC_RETURN(NIL);
}

//
//	Non-standard 
//  Usage: (LOAD-IMAGE filename)
//	Returns NIL.
//
LispFunction(LoadLispImage)
{
	LISP_FUNC_BEGIN(1);
	LispObj path = LISP_ARG(0);
	LispObj func = 0;
	LispObj funcs = 0;
	static long docLoaded = 0;

	checkString(path);
	readHeapFromFile(path);
	updateKernelFunctions();	// reinitialize kernel function pointers
								// in case the CormanLispServer DLL is loaded at
								// a different address than when the image 
								// was saved

	funcs = symbolValue(LOAD_IMAGE_RESTORE_FUNCS);
	while (isCons(funcs))
	{
		LispCall1(Funcall, CAR(funcs));
		funcs = CDR(funcs);
	}

	func = symbolValue(TOP_LEVEL);
	path = 0;
	_args = 0;
	LISP_ARG(0) = 0;
	InitializationEvent.SetEvent();		// done initializing				
	if (isFunction(func))
	{
		LispCall1(Funcall, symbolValue(TOP_LEVEL));
	}
	LISP_FUNC_RETURN(NIL);
}

LispObj 
loadFile(LispObj inputStream)
{
	long count = 0;
	LispObj x = UNINITIALIZED;
	LispObj val = UNINITIALIZED;
	LispObj outputStream = 0;

	outputStream = symbolValue(STANDARD_OUTPUT);

	while (1)
	{
		try
		{
			setSymbolValue(SOURCE_LINE, NIL);
			x = LispCall5(Funcall, symbolFunction(READ), inputStream, NIL, Eof, NIL);
			if (x == Eof)
				break;
			val = eval(x, NIL);
		}
		catch (LispObj)
		{
			LispCall1(Close, inputStream);
			LispCall2(Write, stringNode("An error occurred while loading.\n"), outputStream);
			throw;
		}
		count++;		
	}
	return wrapInteger(count);
}

LispFunction(Load)
{
	LISP_FUNC_BEGIN(1);
	LispObj path = LISP_ARG(0);
	LispObj inputStream = 0;

	checkString(path);
	inputStream = inputFileStreamNode(path);
	ret = loadFile(inputStream);
	LispCall1(Close, inputStream);

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Car)
{
	LISP_FUNC_BEGIN(1);

	if (isCons(LISP_ARG(0)))
		ret = CAR(LISP_ARG(0));
	else
	if (LISP_ARG(0) == NIL)
		ret = NIL;
	else
		Error("Not a list: ~A", LISP_ARG(0));

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Cdr)
{
	LISP_FUNC_BEGIN(1);

	if (isCons(LISP_ARG(0)))
		ret = CDR(LISP_ARG(0));
	else
	if (LISP_ARG(0) == NIL)
		ret = NIL;
	else
		Error("Not a list: ~A", LISP_ARG(0));

	LISP_FUNC_RETURN(ret);
}

LispFunction(Eval)
{
	LISP_FUNC_BEGIN(1);
	
	ret = eval(LISP_ARG(0), NIL);

	ReturnCount(NumReturnValues);
	return ret;
}

LispFunction(Compile_Form)
{
	LISP_FUNC_BEGIN(1);
	
	ret = compileExpression(LISP_ARG(0), NIL, NIL);

	LISP_FUNC_RETURN(ret);
}

LispFunction(Compile_Lambda)
{
	LISP_FUNC_BEGIN(5);
	LispObj lambda = 0;
	LispObj env = 0;
	LispObj lexMacros = 0;
	LispObj lexSymbolMacros = 0;
	LispObj name = 0;

	lambda		= LISP_ARG(0);
	env			= LISP_ARG(1);
	lexMacros	= LISP_ARG(2);
	lexSymbolMacros	= LISP_ARG(3);
	name		= LISP_ARG(4);

	ret = compileLambdaExpression(lambda, name, env, lexMacros, lexSymbolMacros);

	LISP_FUNC_RETURN(ret);
}

LispFunction(WrongNumberOfArgs)
{
	Error("Wrong number of arguments");
	return NIL;
}

void UnboundVariable(LispObj sym)
{
	Error("Unbound variable: ~A", sym);
}

void InvalidFixnum(LispObj num)
{
	Error("Expected a number of type FIXNUM, got ~A", num);
}

// redefined later
LispFunction(Null)
{
	LISP_FUNC_BEGIN(1);

	ret = LISP_ARG(0) == NIL ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Eq)
{
	LISP_FUNC_BEGIN(2);

	ret = LISP_ARG(0) == LISP_ARG(1) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Cons)
{
	LISP_FUNC_BEGIN(2);
	LispObj c = 0;

	c = AllocLocalCons();
	CAR(c) = LISP_ARG(0);
	CDR(c) = LISP_ARG(1);

	LISP_FUNC_RETURN(c);
}

// redefined later
LispFunction(Consp)
{
	LISP_FUNC_BEGIN(1);

	ret = isCons(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Symbolp)
{
	LISP_FUNC_BEGIN(1);

	ret = isSymbol(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Stringp)
{
	LISP_FUNC_BEGIN(1);

	ret = isString(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Streamp)
{
	LISP_FUNC_BEGIN(1);

	ret = isStream(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Hash_table_p)
{
	LISP_FUNC_BEGIN(1);

	ret = isHashtable(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Packagep)
{
	LISP_FUNC_BEGIN(1);

	ret = isPackage(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Readtablep)
{
	LISP_FUNC_BEGIN(1);

	ret = isReadtable(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Arrayp)
{
	LISP_FUNC_BEGIN(1);

	ret = isArray(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Sequencep)
{
	LISP_FUNC_BEGIN(1);

	ret = isSequence(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Structurep)
{
	LISP_FUNC_BEGIN(1);

	ret = isStruct(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Integerp)
{
	LISP_FUNC_BEGIN(1);

	ret = isLispInteger(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Fixnump)
{
	LISP_FUNC_BEGIN(1);

	ret = isFixnum(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Bignump)
{
	LISP_FUNC_BEGIN(1);
	ret = isBignum(LISP_ARG(0)) ? T : NIL;
	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Floatp)
{
	LISP_FUNC_BEGIN(1);
	ret = isFloat(LISP_ARG(0)) ? T : NIL;
	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Short_Float_P)
{
	LISP_FUNC_BEGIN(1);
	ret = isShortFloat(LISP_ARG(0)) ? T : NIL;
	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Single_Float_P)
{
	LISP_FUNC_BEGIN(1);
	ret = isSingleFloat(LISP_ARG(0)) ? T : NIL;
	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Double_Float_P)
{
	LISP_FUNC_BEGIN(1);
	ret = isDoubleFloat(LISP_ARG(0)) ? T : NIL;
	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Ratiop)
{
	LISP_FUNC_BEGIN(1);
	ret = isRatio(LISP_ARG(0)) ? T : NIL;
	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Complexp)
{
	LISP_FUNC_BEGIN(1);

	ret = isComplex(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Listp)
{
	LISP_FUNC_BEGIN(1);

	ret = isList(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Characterp)
{
	LISP_FUNC_BEGIN(1);

	ret = isCharacter(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Uvectorp)
{
	LISP_FUNC_BEGIN(1);

	ret = isUvector(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

LispFunction(UndefinedFunction)
{
	Error("No function definition");
	return NIL;
}

LispFunction(Gensym)
{
	LISP_FUNC_BEGIN(0);

	static char buf[16];
	static long gensymInt = 0;
	long n = 0;
	long digit = 0;
	long intbufpos = 15;
	
	gensymInt++;
	n = gensymInt;
	buf[intbufpos--] = 0;
	while (n > 0)
	{
		digit = (int)(n % 10);
		n /= 10;
		buf[intbufpos--] = (char)('0' + (char)digit);
	}
	buf[intbufpos] = 'T';
	ret = symbolNode(stringNode(buf + intbufpos));

	LISP_FUNC_RETURN(ret);
}

#ifdef X86
static LARGE_INTEGER largeTime;

LispFunction(Get_Internal_Run_Time)
{
	LISP_FUNC_BEGIN(0);
	BOOL rv = 0;
	ret = bignumNode(wrapInteger(2));
	rv = QueryPerformanceCounter(&largeTime);
	UVECTOR(ret)[BIGNUM_FIRST_CELL]		 = (LispObj)largeTime.LowPart;
	UVECTOR(ret)[BIGNUM_FIRST_CELL + 1]  = (LispObj)largeTime.HighPart;

	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_Time_Units_Per_Second)
{
	LISP_FUNC_BEGIN(0);
	BOOL rv = 0;
	rv = QueryPerformanceFrequency(&largeTime);
	ret = createLispInteger(largeTime.LowPart);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_Millisecond_Count)
{
	LISP_FUNC_BEGIN(0);

	ret = wrapInteger(GetTickCount());

	LISP_FUNC_RETURN(ret);
}

#define rdtsc _emit 0x0f __asm _emit 0x31

LispFunction(Get_Instruction_Count)
{
	LISP_FUNC_BEGIN(0);
	LispObj bn = 0;
	unsigned long lowtime = 0;
	unsigned long hightime = 0;

	bn = bignumNode(wrapInteger(2));
	
//	__asm db 0fH, 31H

	__asm rdtsc
	__asm mov dword ptr lowtime, eax
	__asm mov dword ptr hightime, edx

	UVECTOR(bn)[BIGNUM_FIRST_CELL] = lowtime;
	UVECTOR(bn)[BIGNUM_FIRST_CELL + 1] = hightime;

	LISP_FUNC_RETURN(bn);
}

#else
#error Not implemented
#endif

extern long GarbageCollectionTicks;

LispFunction(Get_Garbage_Collection_Time)
{
	LISP_FUNC_BEGIN(0);

	ret = wrapInteger(GarbageCollectionTicks);

	LISP_FUNC_RETURN(ret);
}

LispFunction(Terpri)
{
	LISP_FUNC_BEGIN_VARIABLE(0, 1);
	LispObj os = UNINITIALIZED;

	if (ARG_COUNT == 1 && isStream(LISP_ARG(0)))
		os = LISP_ARG(0);
	else
		os = symbolValue(STANDARD_OUTPUT);
	LispCall2(Write, charNode(ASCII_NEWLINE), os);

	LISP_FUNC_RETURN_NO_VALUES();
}

LispFunction(Force_Output)
{
	LISP_FUNC_BEGIN_VARIABLE(0, 1);
	LispObj os = UNINITIALIZED;

	if (ARG_COUNT == 1 && isStream(LISP_ARG(0)))
		os = LISP_ARG(0);
	else
		os = symbolValue(STANDARD_OUTPUT);
	flushStream(os);

	LISP_FUNC_RETURN(NIL);
}


// redefined later
LispFunction(Symbol_Get_Flags)
{
	LISP_FUNC_BEGIN(1);

	checkSymbol(LISP_ARG(0));
	ret = UVECTOR(LISP_ARG(0))[SYMBOL_FLAGS];

	LISP_FUNC_RETURN(ret);
}

// redefined later
// (%symbol-set-flags flags symbol)
LispFunction(Symbol_Set_Flags)
{
	LISP_FUNC_BEGIN(2);

	checkSymbol(LISP_ARG(1));
	UVECTOR(LISP_ARG(1))[SYMBOL_FLAGS] = LISP_ARG(0);
	
	LISP_FUNC_RETURN(LISP_ARG(0));
}

LispFunction(Bit_Or)
{
	LISP_FUNC_BEGIN(2);

	checkInteger(LISP_ARG(0));
	checkInteger(LISP_ARG(1));
	ret = (LispObj)(LISP_ARG(0) | LISP_ARG(1));

	LISP_FUNC_RETURN(ret);
}

LispFunction(Write)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj obj = LISP_ARG(0);
	LispObj destination = 0;

	if (ARG_COUNT == 2)
		destination = LISP_ARG(1);
	else
		destination = symbolValue(STANDARD_OUTPUT);
	checkOutputStream(destination);
	_Write(obj, destination);

	LISP_FUNC_RETURN(obj);
}

LispFunction(Read)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 4);
	LispObj eof_error_p = T;
	LispObj eof_value = Eof;
	LispObj recursive_p = NIL;
	LispObj s = LISP_ARG(0);

	if (ARG_COUNT > 1)
	{
		eof_error_p = LISP_ARG(1);
		if (ARG_COUNT > 2)
		{
			eof_value = LISP_ARG(2);
			if (ARG_COUNT > 3)
			{
				recursive_p = LISP_ARG(3);
			}
		}
	}

	checkInputStream(s);		
	
	// this loop allows skipping over read expressions which return
	// no values i.e. commented expressions
	while (TRUE)
	{
		ret = readExpression(s);
		if (ret == UNINITIALIZED)		// if end of file
		{
			if (eof_error_p != NIL)
				Error("End of file encountered in stream ~A", s);
			LISP_FUNC_RETURN(eof_value);
		}
		if (isCons(ret))
		{
			LISP_FUNC_RETURN(CAR(ret));
		}
	}
}

// redefined later
#ifdef X86
LispFunction(Values)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	LispObj arglist = NIL;
	long i = 0;

	for (i = ARG_COUNT - 1; i >= 0; i--)
		arglist = cons(LISP_ARG(i), arglist);
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = arglist;
	if (ARG_COUNT == 0)
		ret = NIL;
	else
		ret = LISP_ARG(0);

	ReturnCount(ARG_COUNT);
	return ret;
}
#else
#error Not implemented
#endif

// redefined later
LispFunction(Make_Array)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);

	checkInteger(LISP_ARG(0));
	ret = vectorNode(LISP_ARG(0));

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Room)
{
	LISP_FUNC_BEGIN(0);
	LispObj os = 0;
	long symbolTableEntries = 0;
	long symbolTableSize = 0;

	flushEphemeralHeaps();
	os = symbolValue(STANDARD_OUTPUT);
	LispCall2(Write, stringNode("Total heap size: "), os);
	LispCall2(Write, Heap1Capacity(), os);
	LispCall2(Write, stringNode(" bytes. Heap available: "), os);
	LispCall2(Write, Heap1Capacity() - Heap1CurrentlyUsed(), os);
	LispCall2(Write, stringNode(" bytes."), os);
	LispCall1(Terpri, os);
	symbolTableEntries = (integer(SYMBOL_TABLE_COUNT) - FirstJumpTableEntry)
							/ JumpTableCellsPerEntry;
	symbolTableSize = NumJumpTableEntries;
	LispCall2(Write, stringNode("Jump table size: "), os);
	LispCall2(Write, wrapInteger(symbolTableSize), os);
	LispCall2(Write, stringNode(" entries. Entries available: "), os);
	LispCall2(Write, wrapInteger(symbolTableSize - symbolTableEntries), os);
	LispCall2(Write, stringNode("."), os);
	LispCall1(Terpri, os);

	LISP_FUNC_RETURN_NO_VALUES();
}

LispFunction(DumpHeap)
{
	LISP_FUNC_BEGIN(1);
	LispObj path = LISP_ARG(0);

	checkString(path);
	flushEphemeralHeaps();
	dumpHeapToFile(path);

	LISP_FUNC_RETURN(NIL);
}

// redefined later
LispFunction(Rplaca)
{
	LISP_FUNC_BEGIN(2);

	checkCons(LISP_ARG(0));
	CAR(LISP_ARG(0)) = LISP_ARG(1);

	LISP_FUNC_RETURN(LISP_ARG(0));
}

// redefined later
LispFunction(Rplacd)
{
	LISP_FUNC_BEGIN(2);

	checkCons(LISP_ARG(0));
	CDR(LISP_ARG(0)) = LISP_ARG(1);

	LISP_FUNC_RETURN(LISP_ARG(0));
}

// redefined later
LispFunction(Function_Environment)
{
	LISP_FUNC_BEGIN(1);

	checkFunction(LISP_ARG(0));
	ret = UVECTOR(LISP_ARG(0))[FUNCTION_ENVIRONMENT];

	LISP_FUNC_RETURN(ret);
}

LispFunction(Function_Info_List)
{
	LISP_FUNC_BEGIN(1);

	checkFunction(LISP_ARG(0));
	if (uvectorType(LISP_ARG(0)) == KFunctionType)
		ret = NIL;
	else
		ret = UVECTOR(UVECTOR(LISP_ARG(0))[FUNCTION_ADDRESS])[COMPILED_CODE_PROPERTIES];

	LISP_FUNC_RETURN(ret);
}

LispFunction(Close)
{
	LISP_FUNC_BEGIN(1);
	LispObj stream = LISP_ARG(0);
	BOOL retval = 0;

	checkStream(LISP_ARG(0));
	if (streamSubclass(stream) == FILE_STREAM)
	{
		if (streamDirection(stream) == OUTPUT_KEY
				|| streamDirection(stream) == BIDIRECTIONAL_KEY)
			flushStream(stream);

		retval = CloseHandle((void*)lispIntegerToLong(streamHandle(stream)));
		if (!retval)
			Error("Could not close file ~A, error code = ~A", 
				stream, createLispInteger(GetLastError()));
		streamOpen(stream) = NIL;
	}

	LISP_FUNC_RETURN(NIL);
}

// low level function for setting uvector field
// example: (uref-set sym 1 "name")	causes the 
// symbol-name to be set to "name"
// This function is intended to be very fast and low-level.
// Returns the new value.
//
// redefined later
LispFunction(Uref_Set)
{
	LISP_FUNC_BEGIN(3);

	ret = LISP_ARG(0);
	UVECTOR(LISP_ARG(1))[integer(LISP_ARG(2))] = LISP_ARG(0);

	LISP_FUNC_RETURN(ret);
}

//
//	Usage:  (uref uvec index)
//
// redefined later
LispFunction(Uref)
{
	LISP_FUNC_BEGIN(2);
	
	ret = UVECTOR(LISP_ARG(0))[integer(LISP_ARG(1))];

	LISP_FUNC_RETURN(ret);
}

LispFunction(Read_Char)
{
	LISP_FUNC_BEGIN_VARIABLE(0, 4);
	LispObj stream = NIL;
	LispObj eof_error = T;
	LispObj eof_value = NIL;
	LispObj recursive_p = NIL;

	if (ARG_COUNT > 0)
		stream = LISP_ARG(0);
	if (ARG_COUNT > 1)
		eof_error = LISP_ARG(1);	
	if (ARG_COUNT > 2)
		eof_value = LISP_ARG(2);	
	if (ARG_COUNT > 3)
		recursive_p = LISP_ARG(3);
	if (stream == NIL || stream == T)
		stream = symbolValue(STANDARD_INPUT);
	checkStream(stream);
	ret = getCharacter(stream);
	if (ret == Eof)
	{
		if (eof_error != NIL)
			Error("End of file encountered in stream ~A", stream);
		ret = eof_value;
	}

	LISP_FUNC_RETURN(ret);
}

// %READ-CHAR--fast version of READ-CHAR
// Returns the next character, or NIL if end of file
LispFunction(_Read_Char)
{
	LISP_FUNC_BEGIN(1);
	LispObj stream = LISP_ARG(0);

	ret = getCharacter(stream);
	if (ret == Eof)
		ret = NIL;

	LISP_FUNC_RETURN(ret);
}

LispFunction(_Read_Char_With_Error)
{
	LISP_FUNC_BEGIN(1);
	LispObj stream = LISP_ARG(0);

	ret = getCharacter(stream);
	if (ret == Eof)
		Error("Unexpected end of file in stream ~A", stream);

	LISP_FUNC_RETURN(ret);
}


LispFunction(Unread_Char)
{
	LISP_FUNC_BEGIN_VARIABLE(1, 2);
	LispObj ch = LISP_ARG(0);
	LispObj stream = NIL;

	if (ARG_COUNT == 2)
		stream = LISP_ARG(1);
	if (stream == NIL || stream == T)
		stream = symbolValue(STANDARD_INPUT);
	checkStream(stream);
	putbackCharacter(stream, ch);

	LISP_FUNC_RETURN(NIL);
}

// usage: (%output-char char stream)
LispFunction(_Output_Char)
{
	LISP_FUNC_BEGIN(2);
	LispObj c = LISP_ARG(0);
	LispObj stream = LISP_ARG(1);
	checkCharacter(c);
	checkOutputStream(stream);
	outputChar(c, stream);
	ret = c;
	LISP_FUNC_RETURN(ret);
}

// usage: (%output-string string stream start end)
LispFunction(_Output_Chars)
{
	LISP_FUNC_BEGIN(4);
	LispObj str = LISP_ARG(0);
	LispObj stream = LISP_ARG(1);
	LispObj start = LISP_ARG(2);
	LispObj end = LISP_ARG(3);
	checkString(str);
	checkOutputStream(stream);
	checkInteger(start);
	checkInteger(end);
	if (integer(start) < 0 || end > vectorLength(str)
		|| end < start)
		Error("Index out of range: start = ~A, end = ~A", start, end);
	outputChars(str, integer(start), integer(end - start), stream);
	ret = str;
	LISP_FUNC_RETURN(ret);
}

const long errbufSize = 256;
char errbuf[errbufSize];
char* errorStart = ";; Error: ";

LispFunction(LispError)
{
	LISP_FUNC_BEGIN_VARIABLE(1, MaxLispArgs);
	LispObj msg = LISP_ARG(0);

	strcpy_s(errbuf, sizeof(errbuf), errorStart);
	strncat_s(errbuf, sizeof(errbuf), (char*)byteArrayStart(nullTerminate(msg)), 
			errbufSize - strlen(errorStart) - 1);
	throw stringNode(errbuf);

	LISP_FUNC_RETURN(NIL);		// never gets here
}

// redefined later
LispFunction(Char_Int)
{
	LISP_FUNC_BEGIN(1);
	LispObj ch = LISP_ARG(0);

	checkCharacter(ch);
	ret = wrapInteger((long)character(ch));

	LISP_FUNC_RETURN(ret);
}

// redefined later
LispFunction(Int_Char)
{
	LISP_FUNC_BEGIN(1);
	LispObj n = LISP_ARG(0);

	checkInteger(n);
	ret = wrapCharacter((char)integer(n));

	LISP_FUNC_RETURN(ret);
}

LispFunction(Char_Upcase)
{
	LISP_FUNC_BEGIN(1);
	LispObj ch = LISP_ARG(0);

	checkCharacter(ch);
	ret = islower(character(ch)) ? wrapCharacter(toupper(character(ch))) : ch;

	LISP_FUNC_RETURN(ret);
}

LispFunction(Char_Downcase)
{
	LISP_FUNC_BEGIN(1);
	LispObj ch = LISP_ARG(0);

	checkCharacter(ch);
	ret = isupper(character(ch)) ? wrapCharacter(tolower(character(ch))) : ch;

	LISP_FUNC_RETURN(ret);
}

LispFunction(Elt)
{
	LISP_FUNC_BEGIN(2);
	long n = 0;
	LispObj sequence = LISP_ARG(0);
	LispObj index = LISP_ARG(1);
	long i = 0;
	checkSequence(sequence);

	if (isVector(sequence))
	{
		n = integer(index);
		if (n < 0
				|| (arrayHasFillPointer(sequence) && index >= arrayFillPointer(sequence))
				|| n >= arrayDimension(sequence, 0))
				Error("Index out of range: ~A", index);
		if (isGenericArray(sequence))
			ret = arrayStart(sequence)[n];
		else
		if (isString(sequence))
			ret = wrapCharacter(charArrayStart(sequence)[n]);
		else
		if (isBitVector(sequence))
			ret = wrapInteger(byteArrayStart(sequence)[n]);
		else
		if (isByteVector(sequence))
			ret = wrapInteger(byteArrayStart(sequence)[n]);
	}
	else
	{
		// do list stuff
		n = integer(index);
		if (n < 0 || !isCons(sequence))
			Error("Index out of range: ~A", index);
		for (i = 0; i < n; i++)
		{
			sequence = CDR(sequence);
			if (!isCons(sequence))
				Error("Index out of range: ~A", index);
		}
		ret = CAR(sequence);
	}

	LISP_FUNC_RETURN(ret);
}

LispFunction(SetElt)
{
	LISP_FUNC_BEGIN(3);
	long n = 0;
	LispObj val = 0;
	LispObj sequence = 0;
	LispObj index = 0;
	long i = 0;

	val = LISP_ARG(0);
	sequence = LISP_ARG(1);
	index = LISP_ARG(2);
	if (isVector(sequence))
	{
		n = integer(index);
		if (n < 0 
			|| (arrayHasFillPointer(sequence) && index >= arrayFillPointer(sequence))
			|| n >= arrayDimension(sequence, 0))
			Error("Index out of range: ~A", index);

		if (isGenericArray(sequence))
			arrayStart(sequence)[n] = val;
		else
		if (isString(sequence))
		{
			checkCharacter(val);
			charArrayStart(sequence)[n] = (byte)character(val);
		}
		else
		if (isBitVector(sequence))
		{
			checkInteger(val);
			if (val != 0 && val != wrapInteger(1))
				Error("Invalid value stored in bit array--out of range: ~A", val);
			byteArrayStart(sequence)[n] = (byte)(unsigned long)integer(val);
		}
		else
		if (isByteVector(sequence))
		{
			checkInteger(val);
			if (integer(val) < 0 || integer(val) > 255)
				Error("Invalid value stored in byte array--out of range: ~A", val);
			byteArrayStart(sequence)[n] = (byte)(unsigned long)integer(val);
		}
	}
	else
	{
		// do list stuff
		n = integer(index);
		if (n < 0 || !isCons(sequence))
			Error("Index out of range: ~A", index);
		for (i = 0; i < n; i++)
		{
			sequence = CDR(sequence);
			if (!isCons(sequence))
				Error("Index out of range: ~A", index);
		}
		CAR(sequence) = val;
	}

	LISP_FUNC_RETURN(val);
}

//
//	Usage: (package-hash-index package string)
//
LispFunction(Package_Hash_Index)
{
	LISP_FUNC_BEGIN(2);
	LispObj p = LISP_ARG(0);
	LispObj str = LISP_ARG(1);
	long h = 0;
	LISP_CHAR* s = 0;
	long len = 0;
	long i = 0;
	checkString(str);
	checkPackage(p);
	s = charArrayStart(str);
	len = integer(vectorLength(str));

	for (i = 0; i < len; i++)
	{
		h ^= *s;
		h <<= 5;
		s++;
	}
	if (h < 0)
		h = -h;
	h %= GET_PACKAGE_CAPACITY(p);
	ret = wrapInteger(h);

	LISP_FUNC_RETURN(ret);
}

LispFunction(Vector_Slot_Initialized)
{
	LISP_FUNC_BEGIN(2);
	LispObj vec = 0;
	LispObj index = 0;
	long n = 0;

	vec = LISP_ARG(0);
	index = LISP_ARG(1);
//	if (isString(vec))
//		return T;		// characters are always initialized
	if (!isVector(vec) || !isGenericArray(vec))
		Error("Invalid vector type passed to VECTOR-SLOT-INITIALIZED: ~A", vec);
	n = integer(index);
	if (n < 0 || n >= arrayDimension(vec, 0))
		Error("Index out of range: ~A", index);
	ret = arrayStart(vec)[n] == UNINITIALIZED ? NIL : T;

	LISP_FUNC_RETURN(ret);
}

LispFunction(String_Equal)
{
	LISP_FUNC_BEGIN(2);
	LispObj s1 = LISP_ARG(0);
	LispObj s2 = LISP_ARG(1);

	checkString(s1);
	checkString(s2);
	if (lispStringsEqual(s1, s2))
		ret = T;
	else
		ret = NIL;

	LISP_FUNC_RETURN(ret);
}

LispFunction(Make_Symbol)
{
	LISP_FUNC_BEGIN(1);
	LispObj str = 0;
	str = LISP_ARG(0);

	checkString(str);
	ret = symbolNode(str);

	LISP_FUNC_RETURN(ret);
}

//
//	Limited implementation of COERCE for startup
//
LispFunction(Coerce)
{
	// support list of chars to string
	LISP_FUNC_BEGIN(2);
	LispObj seq = 0;
	LispObj type = 0;
	LispObj c = 0;
	long index = 0;
	long len = 0;

	seq = LISP_ARG(0);
	type = LISP_ARG(1);

	if (isList(seq) && type == STRING)
	{
		// coerce list of chars to string
		len = listLength(seq);
		ret = charVector(wrapInteger(len));
		index = 0;
		while (isCons(seq))
		{
			c = CAR(seq);
			if (!isCharacter(c))
				Error("Cannot coerce to string: ~A", seq);
			charArrayStart(ret)[index] = (LISP_CHAR)(unsigned long)character(c);
			index++;
			seq = CDR(seq);
		}
	}
	else
	{
		Error("Cannot coerce: ~A", seq);
	}

	LISP_FUNC_RETURN(ret);
}

LispFunction(Open_Input_File)
{
	LISP_FUNC_BEGIN(1);
	LispObj path = LISP_ARG(0);
	checkString(path);

	ret = inputFileStreamNode(path);

	LISP_FUNC_RETURN(ret);
}

LispFunction(Probe_File)
{
	LISP_FUNC_BEGIN(1);
	LispObj path = LISP_ARG(0);
	HANDLE handle = 0;
	checkString(path);
	
	handle = CreateFile(
		(char*)byteArrayStart(nullTerminate(path)), // pointer to name of the file
		GENERIC_READ,						 // access (read-write) mode
		FILE_SHARE_READ,					 // share mode
		NULL,								 // pointer to security descriptor
		OPEN_EXISTING,						 // how to create
		FILE_ATTRIBUTE_NORMAL,				 //	file attributes
		NULL);								 // handle to file with attributes to copy

	if (handle == INVALID_HANDLE_VALUE)
		ret = NIL;
	else
	{
		CloseHandle(handle);
		ret = T;
	}
	LISP_FUNC_RETURN(ret);
}

//
//	Usage:  (alloc-uvector size tag)
//
LispFunction(Alloc_Uvector)
{
	LISP_FUNC_BEGIN(2);
	LispObj size = LISP_ARG(0);
	LispObj tag = LISP_ARG(1);
	checkInteger(size);
	ret = AllocVector(integer(size));
	UVECTOR(ret)[0]	|= tag;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Alloc_Byte_Vector)
{
	LISP_FUNC_BEGIN(1);
	LispObj size = LISP_ARG(0);
	checkInteger(size);
	ret = byteVector(size);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Alloc_Char_Vector)
{
	LISP_FUNC_BEGIN(1);
	LispObj size = LISP_ARG(0);
	checkInteger(size);
	ret = charVector(size);
	LISP_FUNC_RETURN(ret);
}

//
//	Usage:	(string-to-float "123.4")
//	Returns NIL if cannot convert to float, otherwise
//	the float is returned.
//
static char float_buf[128];
enum FloatPrecision { ShortFloat, SingleFloat, DoubleFloat };

LispFunction(Chars_To_Float)
{
	LISP_FUNC_BEGIN(1);
	LispObj chars = LISP_ARG(0);
	LispObj saveChars = chars;
	LispObj defaultFormat = 0;
	long decimal = 0;
	long digits = 0;
	long expDigits = 0;
	ret = NIL;
	long i = 0;
 	char* str = float_buf;
	long precision = SingleFloat;

	defaultFormat = symbolValue(READ_DEFAULT_FLOAT_FORMAT);
	if (defaultFormat == SHORT_FLOAT)
		precision = ShortFloat;
	else
	if (defaultFormat == DOUBLE_FLOAT || defaultFormat == LONG_FLOAT)
		precision = DoubleFloat;

	checkList(chars);

	for (i = 0; i < 128; i++)
	{
		if (!isCons(saveChars))
			break;
		float_buf[i] = (char)character(CAR(saveChars));
		saveChars = CDR(saveChars);
	}
	float_buf[i] = 0;

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
				goto exit;		// more than one decimal point!
		}
		else
			break;
		str++;
	}
	
	if (digits == 0)
		goto exit;

	// get exponent
	if (*str == 'E' || *str == 'e'		// default float
		|| *str == 'F' || *str == 'f'	// single-float
		|| *str == 'L' || *str == 'l'	// long-float
		|| *str == 'S' || *str == 's'	// short-float
		|| *str == 'D' || *str == 'd')	// double-float
	{
		if (*str == 'S' || *str == 's')
			precision = ShortFloat;
		else
		if (*str == 'F' || *str == 'f')
			precision = SingleFloat;
		else
		if (*str == 'L' || *str == 'l' || *str == 'D'  || *str == 'd')
			precision = DoubleFloat;

		*str = 'E';
		str++;
		if (*str == '+' || *str == '-')		// allow for sign on exponent
			str++;

		while (isdigit(*str))
		{
			str++;
			expDigits++;
		}
		if (expDigits == 0)
			goto exit;
	}
	else
		if (*str != (long) 0)
			goto exit;
	
	switch (precision)
	{
		case SingleFloat:
			ret = singleFloatNode(0);
			singleFloat(ret) = (float)atof(float_buf);
			break;
		case ShortFloat:
			ret = createShortFloat(atof(float_buf));
			break;
		case DoubleFloat:
			ret = doubleFloatNode(0);
			doubleFloat(ret) = atof(float_buf);
			break;
	}
exit:
	LISP_FUNC_RETURN(ret);
}

LispFunction(Pop_Special_Bindings)
{
	LISP_FUNC_BEGIN(1);
	LispObj bindings = LISP_ARG(0);

	while (isCons(bindings))
	{
		popDynamicBinding(CAR(bindings));
		bindings = CDR(bindings);
	}
	LISP_FUNC_RETURN(ret);	// returns NIL
}

// Usage: (%push-special-bindings sym1 val1 sym2 val2 ...)
//
LispFunction(Push_Special_Bindings)
{
	LISP_FUNC_BEGIN_VARIABLE(0, MaxLispArgs);
	long i = 0;
	for (i = 0; i < ARG_COUNT; i += 2)
	{
		pushDynamicBinding(LISP_ARG(i), LISP_ARG(i + 1));
	}
	LISP_FUNC_RETURN(ret);	// returns NIL
}

LispFunction(Array_Dimension)
{
	LISP_FUNC_BEGIN(2);
	LispObj array = LISP_ARG(0);
	LispObj dim = LISP_ARG(1);
	long d = 0;
	checkArray(array);
	checkInteger(dim);
	d = integer(dim);
	if (d < 0 || d >= (long)arrayDimensions(array))
		Error("Invalid array dimension: ~A", dim);
	ret = wrapInteger(arrayDimension(array, d));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Array_Type)
{
	LISP_FUNC_BEGIN(1);
	LispObj array = LISP_ARG(0);
	LispObj vec = array;
	long type = 0;
	checkArray(array);
	if (isAdjustableArray(vec))
		vec = adjustableArrayVector(vec);
	type = uvectorType(vec);

	if (type == SimpleVectorType)
		ret = T;
	else
	if (type == SimpleCharVectorType)
		ret = CHARACTER;
	else
	if (type == SimpleByteVectorType)
		ret = LBYTE;
	else
	if (type == SimpleBitVectorType)
		ret = BIT;
	else
	if (type == SimpleShortVectorType)
		ret = SHORT_SYM;
	else
	if (type == SimpleDoubleFloatVectorType)
		ret = DOUBLE_FLOAT;
	else
	if (type == SimpleSingleFloatVectorType)
		ret = SINGLE_FLOAT;

	LISP_FUNC_RETURN(ret);
}

LispFunction(Array_Rank)
{
	LISP_FUNC_BEGIN(1);
	LispObj array = LISP_ARG(0);
	checkArray(array);
	ret = wrapInteger(arrayDimensions(array));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Vectorp)
{
	LISP_FUNC_BEGIN(1);

	ret = isVector(LISP_ARG(0)) ? T : NIL;

	LISP_FUNC_RETURN(ret);
}

static char floatbuf[80];

LispFunction(Float_To_String)
{
	LISP_FUNC_BEGIN(1);
	double d = 0.0;
	long chars = 0;
	char* p = 0;
	long precision = 0;
	long readFormat = 0;
	LispObj defaultFormat = 0;

	defaultFormat = symbolValue(READ_DEFAULT_FLOAT_FORMAT);
	if (defaultFormat == SINGLE_FLOAT)
		readFormat = SingleFloat;
	else
	if (defaultFormat == SHORT_FLOAT)
		readFormat = ShortFloat;
	else
		readFormat = DoubleFloat;

	if (isSingleFloat(LISP_ARG(0)))
	{
		precision = SingleFloat;
		d = singleFloat(LISP_ARG(0));
	}
	else
	if (isShortFloat(LISP_ARG(0)))
	{
		precision = ShortFloat;
		d = shortFloat(LISP_ARG(0));
	}
	else
	if (isDoubleFloat(LISP_ARG(0)))
	{
		precision = DoubleFloat;
		d = doubleFloat(LISP_ARG(0));
	}
	else
		Error("Not a floating point number: ~S", LISP_ARG(0));

	chars = sprintf_s(floatbuf, sizeof(floatbuf), "%g", d);
	if (!strchr(floatbuf, '.') && !strchr(floatbuf, 'e'))
		strcat_s(floatbuf, sizeof(floatbuf), ".0");

	if (precision != readFormat)
	{
		if (precision == SingleFloat)
		{
			p = strchr(floatbuf, 'e');
			if (p)
				*p = 'f';
			else
				strcat_s(floatbuf, sizeof(floatbuf), "f0");
		}
		else
		if (precision == ShortFloat)
		{
			p = strchr(floatbuf, 'e');
			if (p)
				*p = 's';
			else
				strcat_s(floatbuf, sizeof(floatbuf), "s0");
		}
		else	// (precision == DoubleFloat)
		{
			p = strchr(floatbuf, 'e');
			if (p)
				*p = 'd';
			else
				strcat_s(floatbuf, sizeof(floatbuf), "d0");
		}
	}
	ret = stringNode(floatbuf);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Uvector_Address)
{
	LISP_FUNC_BEGIN(1);
	checkUvector(LISP_ARG(0));
	ret = wrapInteger((long)(UVECTOR(LISP_ARG(0))));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Row_Major_Aref)
{
	LISP_FUNC_BEGIN(2);
	LispObj array = LISP_ARG(0);
	LispObj index = LISP_ARG(1);
	LispObj vec = array;
	checkArray(array);
	checkInteger(index);
	if (isAdjustableArray(array))
		vec = adjustableArrayVector(array);
	if (isGenericArray(vec))
	{
		ret = arrayStart(vec)[integer(index)];
		if (ret == UNINITIALIZED)
			ret = NIL;
	}
	else
	if (isString(vec))
 		ret = wrapCharacter(charArrayStart(vec)[integer(index)]);
	else
	if (isBitVector(vec) || isByteVector(vec))
		ret = wrapInteger((long)(byteArrayStart(vec)[integer(index)]));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Setf_Row_Major_Aref)
{
	LISP_FUNC_BEGIN(3);
	LispObj value = LISP_ARG(0);
	LispObj array = LISP_ARG(1);
	LispObj index = LISP_ARG(2);
	LispObj vec = array;
	checkArray(array);
	checkInteger(index);
	if (isAdjustableArray(array))
		vec = adjustableArrayVector(array);
	if (isGenericArray(vec))
		arrayStart(vec)[integer(index)] = value;
	else
	if (isString(vec))
	{
		checkCharacter(value);
		charArrayStart(vec)[integer(index)] = (LISP_CHAR)character(value);
	}
	else
	if (isBitVector(vec))
	{
		checkBit(value);
		byteArrayStart(vec)[integer(index)] = (byte)integer(value);
	}
	else
	if (isByteVector(vec))
	{
		if (integer(value) < 0 || integer(value) > 255)
			Error("Invalid value for byte array: ~A", value);
		byteArrayStart(vec)[integer(index)] = (byte)integer(value);
	}
	ret = value;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Array_Cell_Size)
{
	LISP_FUNC_BEGIN(1);
	LispObj array = LISP_ARG(0);
	checkArray(array);
	if (isGenericArray(array))
		ret = wrapInteger(4);
	else
		ret = wrapInteger(1);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Array_Initialize_Element)
{
	LISP_FUNC_BEGIN(2);
	LispObj array = LISP_ARG(0);
	LispObj element = LISP_ARG(1);
	LispObj vec = array;
	long numCells = 0;
	long i = 0;
	long dimensions = 0;
	long ch = 0;
	LispObj* p = 0;

	checkArray(array);
	if (isAdjustableArray(array))
		vec = adjustableArrayVector(array);
	dimensions = arrayDimensions(array);
	numCells = 1;
	for (i = 0; i < dimensions; i++)
		numCells *= arrayDimension(array, i);

	if (isSimpleGenericVector(vec))
	{
		p = arrayStart(vec);
		for (i = 0; i < numCells; i++)
			p[i] = element;
	}
	else
	if (isSimpleCharVector(vec))
	{
		checkCharacter(element);
		ch = character(element);
		for (i = 0; i < numCells; i++)
			charArrayStart(vec)[i] = (LISP_CHAR)ch;
	}
	else
	if (isSimpleByteVector(vec))
	{
		if (integer(element) < 0 || integer(element) > 255)
			Error("Invalid value for byte array: ~A", element);
		memset(byteArrayStart(vec), integer(element), numCells);
	}
	else
	if (isSimpleBitVector(vec))
	{
		checkBit(element);
		memset(byteArrayStart(vec), integer(element), numCells);
	}
	ret = array;
	LISP_FUNC_RETURN(ret);
}

//
//	Lisp internal routine to initialize members of an array
//
void
initArray(LispObj sequence, long depth, LispObj** a, LispObj* end, LispObj array)
{
	long seqlength = 0;
	long numdims = 0;
	long dimLength = 0;
	long i = 0;

	checkSequence(sequence);
	seqlength = sequenceLength(sequence);
	numdims = arrayDimensions(array);
	dimLength = arrayDimension(array, (numdims - 1 - depth));
	if (seqlength != dimLength)
		Error("Invalid initializer form, incorrect number of items in dimension: ~A", sequence);

	if (depth == 0)
	{
		for (i = 0; i < seqlength; i++)
		{
			if ((*a) >= end)
				Error("Error in array initializer form -- array size exceeded");
			*(*a) = LispCall2(Elt, sequence, wrapInteger(i));
			(*a)++;
		}
	}
	else
	{
		for (i = 0; i < seqlength; i++)
			initArray(LispCall2(Elt, sequence, wrapInteger(i)), depth - 1, a, end, array);
	}
}

//
//	Lisp internal routine to initialize members of a character array
//
void
initCharArray(LispObj sequence, long depth, LISP_CHAR** a, LISP_CHAR* end, LispObj array)
{
	long seqlength = 0;
	long numdims = 0;
	long dimLength = 0;
	long i = 0;
	LispObj c = 0;

	checkSequence(sequence);
	seqlength = sequenceLength(sequence);
	numdims = arrayDimensions(array);
	dimLength = arrayDimension(array, (numdims - 1 - depth));
	if (seqlength != dimLength)
		Error("Invalid initializer form, incorrect number of items in dimension: ~A", sequence);

	if (depth == 0)
	{
		for (i = 0; i < seqlength; i++)
		{
			if ((*a) >= end)
				Error("Error in array initializer form -- array size exceeded");
			c = LispCall2(Elt, sequence, wrapInteger(i));
			if (!isCharacter(c))
				Error("Only characters allowed when initializing a character array");
			*(*a) = (char)character(c);
			(*a)++;
		}
	}
	else
	{
		for (i = 0; i < seqlength; i++)
			initCharArray(LispCall2(Elt, sequence, wrapInteger(i)), depth - 1, a, end, array);
	}
}

//
//	Lisp internal routine to initialize members of a bit array
//
void
initBitArray(LispObj sequence, long depth, byte** a, byte* end, LispObj array)
{
	long seqlength = 0;
	long numdims = 0;
	long dimLength = 0;
	long i = 0;
	LispObj c = 0;

	checkSequence(sequence);
	seqlength = sequenceLength(sequence);
	numdims = arrayDimensions(array);
	dimLength = arrayDimension(array, (numdims - 1 - depth));
	if (seqlength != dimLength)
		Error("Invalid initializer form, incorrect number of items in dimension: ~A", sequence);

	if (depth == 0)
	{
		for (i = 0; i < seqlength; i++)
		{
			if ((*a) >= end)
				Error("Error in array initializer form -- array size exceeded");
			c = LispCall2(Elt, sequence, wrapInteger(i));
			if (c != 0 && c != wrapInteger(1))
				Error("Invalid bit found when initializing a bit array");
			*(*a) = (byte)(unsigned long)integer(c);
			(*a)++;
		}
	}
	else
	{
		for (i = 0; i < seqlength; i++)
			initBitArray(LispCall2(Elt, sequence, wrapInteger(i)), depth - 1, a, end, array);
	}
}

//
//	Lisp internal routine to initialize members of a byte array
//
void
initByteArray(LispObj sequence, long depth, byte** a, byte* end, LispObj array)
{
	long seqlength = 0;
	long numdims = 0;
	long dimLength = 0;
	long i = 0;
	LispObj c = 0;

	checkSequence(sequence);
	seqlength = sequenceLength(sequence);
	numdims = arrayDimensions(array);
	dimLength = arrayDimension(array, (numdims - 1 - depth));
	if (seqlength != dimLength)
		Error("Invalid initializer form, incorrect number of items in dimension: ~A", sequence);

	if (depth == 0)
	{
		for (i = 0; i < seqlength; i++)
		{
			if ((*a) >= end)
				Error("Error in array initializer form -- array size exceeded");
			c = LispCall2(Elt, sequence, wrapInteger(i));
			if (integer(c) < 0 || integer(c) > 255)
				Error("Invalid byte found when initializing a bit array: ~A", c);
			*(*a) = (byte)(unsigned long)integer(c);
			(*a)++;
		}
	}
	else
	{
		for (i = 0; i < seqlength; i++)
			initByteArray(LispCall2(Elt, sequence, wrapInteger(i)), depth - 1, a, end, array);
	}
}

LispFunction(Array_Initialize_Contents)
{
	LISP_FUNC_BEGIN(2);
	LispObj array = LISP_ARG(0);
	LispObj contents = LISP_ARG(1);
	byte* b = 0;
	LISP_CHAR* cs = 0;
	LispObj* start = 0;
	long dimensions = 0;
	long numCells = 0;
	long i = 0;

	checkArray(array);
	dimensions = arrayDimensions(array);
	numCells = 1;
	for (i = 0; i < dimensions; i++)
		numCells *= arrayDimension(array, i);
	if (isBitArray(array))
	{
		b = byteArrayStart(array);
		initBitArray(contents, dimensions - 1, &b, b + numCells, array);
	}
	else
	if (isCharArray(array))
	{
		cs = charArrayStart(array);
		initCharArray(contents, dimensions - 1, &cs, cs + numCells, array);
	}
	else
	if (isByteArray(array))
	{
		b = byteArrayStart(array);
		initByteArray(contents, dimensions - 1, &b, b + numCells, array);
	}
	else		// T type
	if (isGenericArray(array))
	{
		start = arrayStart(array);
		initArray(contents, dimensions - 1, &start, start + numCells, array);
	}

	ret = array;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Uninitialized_Object_P)
{
	LISP_FUNC_BEGIN(1);
	if (LISP_ARG(0) == UNINITIALIZED)
		ret = T;
	else
		ret = NIL;
	LISP_FUNC_RETURN(ret);
}

extern long unassemble(unsigned long, unsigned long);
extern char gDisassemblyOutputBuf[];

LispFunction(Disassembly_Statement)
{
	LISP_FUNC_BEGIN(2);
	LispObj address = LISP_ARG(0);
	LispObj offset = LISP_ARG(1);
	LispObj numbytes = 0;
	LispObj s = 0;
	checkLispInteger(address);
	checkInteger(offset);
	numbytes = wrapInteger(unassemble(lispIntegerToUnsignedLong(address),
				(unsigned long)integer(offset)));
 	s = stringNode(gDisassemblyOutputBuf); 
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = list(s, numbytes, END_LIST);
	ret = s;
	ReturnCount(2);
	return ret;
}

LispFunction(Execution_Address)
{
	LISP_FUNC_BEGIN(1);
	LispObj addr = 0;
	LispObj func = LISP_ARG(0);
	LispFunc execaddr = 0;
	checkFunction(func);
	execaddr = functionAddress(func);
	if (*(byte*)execaddr == 0xe9)
		execaddr = (LispFunc)(((unsigned long)execaddr) + *(long*)(((byte*)execaddr) + 1) + 5);
	addr = createLispInteger((long)(unsigned long)(execaddr));
	LISP_FUNC_RETURN(addr);
}

LispFunction(Address_Find_Function)
{
	LISP_FUNC_BEGIN(1);

	LispObj addr = LISP_ARG(0);
	ret = addressFindFunction(addr);

	LISP_FUNC_RETURN(ret);
}

//
//	Non-standard Lisp function 'print-float'.
//	Usage:	(print-float float-num stream format width digits scale 
//				padchar showpos)
//			Valid formats are :fixed :exponential and :general
//
LispFunction(Print_Float)
{
	LISP_FUNC_BEGIN(8);
	LispObj fnum =		LISP_ARG(0);
	LispObj stream =	LISP_ARG(1);
	LispObj format =	LISP_ARG(2);
	LispObj fwidth =	LISP_ARG(3);
	LispObj fdigits =	LISP_ARG(4);
	LispObj fscale =	LISP_ARG(5);
	LispObj fpadchar =	LISP_ARG(6);
	LispObj fshowpos =	LISP_ARG(7);
	double d = 0.0;
	int fixed_format = 0;
	int scientific_format = 0;
	long width = 0;
	long digits = 0;
	long scale = 0;
	long padchar = 0;
	int showPos = 0;

	// fnum = LispCall(Lisp::lispFloat, fnum, 0);	 // convert to a float if necessary
	if (isDoubleFloat(fnum))
		d = doubleFloat(fnum);
	else
	if (isSingleFloat(fnum))
		d = singleFloat(fnum);
	else
	if (isShortFloat(fnum))
		d = shortFloat(fnum);

	checkStream(stream);
	checkSymbol(format);
	checkInteger(fwidth);
	checkInteger(fdigits);
	checkInteger(fscale);
	checkCharacter(fpadchar);

	if (format == findKeyword("FIXED"))
		fixed_format++;
	else
	if (format == findKeyword("EXPONENTIAL"))
		scientific_format++;

	width = integer(fwidth);
	digits = integer(fdigits);
	scale = integer(fscale);
	padchar = character(fpadchar);
	showPos = (fshowpos != NIL) ? 1 : 0;

	while (scale > 0)
	{
		d *= 10.0;
		scale--;
	}
	while (scale < 0)
	{
		d /= 10.0;
		scale++;
	}

//	long saveFill = os->fill();
//	long saveFlags = os->flags();
//	int savePrecision = os->precision();
//	int saveWidth = os->width();

#if 0
	os->fill(padchar);
	if (digits >= 0)
		os->precision((int)digits);
	os->flags((showPos ? ios::showpos : 0) 
				| (fixed_format ? ios::fixed : 0)
				| (scientific_format ? ios::scientific : 0));
	if (width > 0)
		os->width((int)width);
	*os << d;
#endif
	LispCall3(Funcall, WRITE, fnum, stream);
	
//	os->fill(saveFill);
//	os->flags(saveFlags);
//	os->precision(savePrecision);
//	os->width(saveWidth);

	LISP_FUNC_RETURN(fnum);
}

LispFunction(Bignum_Byte)
{
	LISP_FUNC_BEGIN(2);

	LispObj bn = LISP_ARG(0);
	LispObj index = LISP_ARG(1);

	ret = wrapInteger((long)(unsigned long)((byte*)bignumStart(bn))[integer(index)]);

	LISP_FUNC_RETURN(ret);
}

long compareNumbers(LispObj n1, LispObj n2);

LispFunction(Uvector_Type_Bits)
{
	LISP_FUNC_BEGIN(1);

	LispObj obj = LISP_ARG(0);
	if (isUvector(obj))
		ret = wrapInteger(uvectorType(obj));
	else
		Error("Not a uvector: ~A", obj);

	LISP_FUNC_RETURN(ret);
}

LispFunction(Tag_Bits)
{
	LISP_FUNC_BEGIN(1);

	LispObj obj = LISP_ARG(0);
	ret = wrapInteger(gettag(obj));

	LISP_FUNC_RETURN(ret);
}

LispFunction(Test_Function)
{
	LISP_FUNC_BEGIN(2);

	LispObj obj1 = LISP_ARG(0);
	LispObj obj2 = LISP_ARG(1);
	long i = 0;

	i = integer(obj1) / integer(obj2);
	ret = wrapInteger(i);

	LISP_FUNC_RETURN(ret);
}

extern long gStackOverflowAddress;

LispFunction(Protect_Stack)
{
	LISP_FUNC_BEGIN(0);
	unsigned long oldProtect = 0;
	long result = 0;
	if (gStackOverflowAddress != 0)
	{
		result = VirtualProtect((void*)gStackOverflowAddress, 1, 
				PAGE_GUARD | PAGE_READWRITE, &oldProtect);
		ret = T;
	}
	else
		ret = NIL;

	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_OS_Version)
{
	LISP_FUNC_BEGIN(0);

	ret = findKeyword("WINDOWS-NT");

	LISP_FUNC_RETURN(ret);
}

LispFunction(Heap_Capacity)
{
	LISP_FUNC_BEGIN(0);
	ret = Heap1Capacity();
	LISP_FUNC_RETURN(ret);
}

LispFunction(Heap_Currently_Used)
{
	LISP_FUNC_BEGIN(0);
	ret = Heap1CurrentlyUsed();
	LISP_FUNC_RETURN(ret);
}

LispFunction(Jump_Table_Capacity)
{
	LISP_FUNC_BEGIN(0);
	ret = wrapInteger(NumJumpTableEntries);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Jump_Table_Used)
{
	LISP_FUNC_BEGIN(0);
	ret = wrapInteger((integer(SYMBOL_TABLE_COUNT) - FirstJumpTableEntry)
							/ JumpTableCellsPerEntry);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Symbol_Table_Capacity)
{
	LISP_FUNC_BEGIN(0);
	ret = wrapInteger(NumSpecialSymbolEntries);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Symbol_Table_Used)
{
	LISP_FUNC_BEGIN(0);
	ret = wrapInteger(integer(SYMBOL_TABLE_VAR_COUNT) - FirstSpecialSymbolEntry);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Fmakunbound)
{
	LISP_FUNC_BEGIN(1);
	LispObj fspec = LISP_ARG(0);
	setSymbolFunction(fspec, UNINITIALIZED, NIL);
	ret = fspec;
	LISP_FUNC_RETURN(ret);
}

//
//	REGISTER-FINALIZATION
//	Used to register a obj,func pair for finalization when garbage collection
//	happens.
//
LispFunction(Register_Finalization)
{
	LISP_FUNC_BEGIN(2);
	LispObj obj = LISP_ARG(0);
	LispObj func = LISP_ARG(1);
	addFinalizationObject(obj, func);
	ret = obj;
	LISP_FUNC_RETURN(ret);
}

//
//	UVECTOR-NUM-SLOTS function
//	Returns the number of slots in a uvector.
//
LispFunction(Uvector_Num_Slots)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);
	long slots = (UVECTOR(obj)[0] >> 8) * 2 - 1;
	ret = wrapInteger(slots);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Lisp_object_id)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);
	LispObj id = obj << 3;
	unsigned long lowbits = 0;

	if (obj & 0xf0000000)
	{
		lowbits = obj & 0xf;
		id = wrapInteger(obj >> 4);
		id = _Add(_Multiply(id, wrapInteger(16)), wrapInteger(lowbits));
	}
	ret = id;
	LISP_FUNC_RETURN(ret);
}

#define hashUlong(n) ((n)^((n)<<5)^((n)<<10)^((n)<<15)^((n)<<20)^((n) <<25))

static LispObj hashFloat(LispObj obj)
{
	if (isDoubleFloat(obj))
		return stripTag(hashUlong(UVECTOR(obj)[DOUBLE_FLOAT_OFFSET])
					  ^ hashUlong(UVECTOR(obj)[DOUBLE_FLOAT_OFFSET + 1]));
	else
	if (isSingleFloat(obj))
		return stripTag(hashUlong(UVECTOR(obj)[SINGLE_FLOAT_OFFSET]));
	else
		return hashUlong(obj);
}

static LispObj hashBignum(LispObj obj)
{
	unsigned long n = 0;
	unsigned long bnLength = bignumNumCells(obj);
	unsigned long i = 0;
	for (; i < bnLength; i++)
		n ^= hashUlong(UVECTOR(obj)[BIGNUM_FIRST_CELL + i]);
	return stripTag(n);
}

static LispObj hashRatio(LispObj obj)
{
	LispObj hashnum = 0;
	LispObj hashdenom = 0;
	hashnum = LispCall1(Hash_eql_function, UVECTOR(obj)[RATIO_NUMERATOR]);
	hashdenom = LispCall1(Hash_eql_function, UVECTOR(obj)[RATIO_DENOMINATOR]);

	return stripTag(hashUlong(hashnum) ^ hashUlong(hashdenom));
}

static LispObj hashComplex(LispObj obj)
{
	LispObj hashreal = 0;
	LispObj hashimag = 0;
	hashreal = LispCall1(Hash_eql_function, UVECTOR(obj)[COMPLEX_REAL]);
	hashimag = LispCall1(Hash_eql_function, UVECTOR(obj)[COMPLEX_IMAGINARY]);

	return stripTag(hashUlong(hashreal) ^ hashUlong(hashimag));
}

static LispObj hashList(LispObj obj)
{
	LispObj n = 0;
	if (!isCons(obj))
		n = LispCall1(Hash_equal_function, obj);
	else
	{
		n ^= hashList(CAR(obj));
		n ^= hashList(CDR(obj));
	}

	return stripTag(n);
}

static LispObj hashArray(LispObj obj)
{
	unsigned long n = 0;
	unsigned long len = uvectorSize(obj) << 1;
	unsigned long i = 0;
	for (; i < len; i++)
		n ^= hashUlong(UVECTOR(obj)[i]);
	return stripTag(n);
}

static LispObj hashPathname(LispObj obj)
{
	LispObj n = hashUlong(obj);		// need to implement!
	return stripTag(n);
}

LispFunction(Hash_eq_function)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);

	unsigned long hashval = hashUlong(obj);
	ret = stripTag(hashval);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Hash_eql_function)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);

	if (isDoubleFloat(obj) || isSingleFloat(obj))
		ret = hashFloat(obj);
	else
	if (isBignum(obj))
		ret = hashBignum(obj);
	else
	if (isRatio(obj))
		ret = hashRatio(obj);
	else
	if (isComplex(obj))
		ret = hashComplex(obj);
	else
		ret = LispCall1(Hash_eq_function, obj);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Hash_equal_function)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);

	if (isCons(obj))
		ret = hashList(obj);
	else
	if (isString(obj))
		ret = hashArray(obj);
	else
	if (isBitVector(obj))
		ret = hashArray(obj);
	else
	if (isPathname(obj))
		ret = hashPathname(obj);
	else
		ret = LispCall1(Hash_eql_function, obj);

	LISP_FUNC_RETURN(ret);
}

LispFunction(Hash_equalp_function)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);

	ret = LispCall1(Hash_equal_function, obj);	// need to implement!

	LISP_FUNC_RETURN(ret);
}

// this will get redefined in lisp code
LispFunction(Funcall_ignoring_errors)
{
	LISP_FUNC_BEGIN(1);
	LispObj func = LISP_ARG(0);
	ret = LispCall1(Funcall, func);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_gc_exec_registry)
{
	LISP_FUNC_BEGIN(0);
	ret = getGCExecRegistry();
	LISP_FUNC_RETURN(ret);
}

LispFunction(Add_gc_exec_registry)
{
	LISP_FUNC_BEGIN(1);
	LispObj func = LISP_ARG(0);
	addGCExecRegistry(func);
	ret = T;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Remove_gc_exec_registry)
{
	LISP_FUNC_BEGIN(1);
	LispObj func = LISP_ARG(0);
	removeGCExecRegistry(func);
	ret = T;
 	LISP_FUNC_RETURN(ret);
}

LispFunction(CompileFunctionCallForm)
{
	// this gets defined later in Lisp
	LISP_FUNC_BEGIN(2);
	ret = NIL;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Constantp)
{
	LISP_FUNC_BEGIN(1);
 	LispObj obj = LISP_ARG(0);
	ret = NIL;
	if (isSymbol(obj))
	{
		if (isConstantSymbol(obj))
			ret = T;
	}
	else
	if (!isCons(obj))
		ret = T;
	else
	if (CAR(obj) == QUOTE)
		ret = T;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Create_Compiled_Function)
{
	LISP_FUNC_BEGIN(7);
	ret = compiledFunctionNode(LISP_ARG(0), LISP_ARG(1), LISP_ARG(2), 
				LISP_ARG(3), LISP_ARG(4), LISP_ARG(5), LISP_ARG(6));
	LISP_FUNC_RETURN(ret);
}

//
//	Non-standard (LOAD-DLL dll-path)
//	Returns integer DLL handle.
//
LispFunction(Load_DLL)
{
	LISP_FUNC_BEGIN(1);
	LispObj path = LISP_ARG(0);
	HINSTANCE handle = 0;
	checkString(path);
	handle = LoadLibrary((char*)byteArrayStart(nullTerminate(path)));
	if (!handle)	
		Error("Could not load requested DLL: ~A, error code = ~A", path,
			createLispInteger(GetLastError()));
	ret = createLispInteger((long)handle);
	LISP_FUNC_RETURN(ret);
}

//
//	Non-standard (UNLOAD-DLL dll-handle)
//	Returns integer DLL handle.
//
LispFunction(Unload_DLL)
{
	LISP_FUNC_BEGIN(1);
	LispObj dll_handle = LISP_ARG(0);
	HMODULE module = 0;
	BOOL r = 0;
	checkLispInteger(dll_handle);
	if (isBignum(dll_handle))
	{
		module = (HMODULE)UVECTOR(dll_handle)[BIGNUM_FIRST_CELL];
		if (bignumNegative(dll_handle))
			module = (HMODULE)-((long)module);
	}
	else
		module = (HMODULE)integer(dll_handle);
	r = FreeLibrary(module);
	if (!r)	
		Error("Could not unload requested DLL handle: ~A, error code = ~A", dll_handle,
			createLispInteger(GetLastError()));
	LISP_FUNC_RETURN(NIL);
}

//
//	Non-standard (GET-DLL-PROC-ADDRESS procname dll-handle)
//	Returns foreign pointer.
//
LispFunction(Get_DLL_Proc_Address)
{
	LISP_FUNC_BEGIN(2);
	LispObj procname = LISP_ARG(0);
	LispObj dll_handle = LISP_ARG(1);
	HMODULE module = 0;
	FARPROC proc = 0;
	LispObj fp = 0;
	fp = foreignNode();
	checkString(procname);
	checkLispInteger(dll_handle);
	if (isBignum(dll_handle))
	{
		module = (HMODULE)UVECTOR(dll_handle)[BIGNUM_FIRST_CELL];
		if (bignumNegative(dll_handle))
			module = (HMODULE)-((long)module);
	}
	else
		module = (HMODULE)integer(dll_handle);

	proc = GetProcAddress(module, (char*)byteArrayStart(nullTerminate(procname))); 
	if (!proc)
		Error("Could not find the procedure ~A, error code = ~A", procname,
			createLispInteger(GetLastError()));
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)proc;
	ret = fp;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Foreignp)
{
	LISP_FUNC_BEGIN(1);
	ret = isForeign(LISP_ARG(0)) ? T : NIL;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Foreign_Heap_P)
{
	LISP_FUNC_BEGIN(1);
	ret = isForeignHeapPtr(LISP_ARG(0)) ? T : NIL;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Int_To_Foreign_Ptr)
{
	LISP_FUNC_BEGIN(1);
	LispObj fp = 0;
	LispObj num = 0;
	long addr = 0;	   // untagged integer
	num = LISP_ARG(0);
	checkLispInteger(num);
	fp = foreignNode();
	if (isBignum(num))
	{
		addr = (long)UVECTOR(num)[BIGNUM_FIRST_CELL];
		if (bignumNegative(num))
			addr = -addr;
	}
	else
		addr = (long)integer(num);

	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)addr;
	ret = fp;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Foreign_Ptr_To_Int)
{
	LISP_FUNC_BEGIN(1);
	LispObj fp = LISP_ARG(0);
	if (!isForeign(fp) && !isForeignHeapPtr(fp) && !isForeignStackPtr(fp))
		Error("Expected foreign pointer, got: ~A", fp);
	ret = createUnsignedLispInteger(UVECTOR(fp)[FOREIGN_PTR]);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Editor_Set_Message)
{
	LISP_FUNC_BEGIN(1);
	LispObj message = LISP_ARG(0);
	checkString(message);
	if (CormanLispServer)
		CormanLispServer->SetMessage((char*)byteArrayStart(nullTerminate(message)));
	ret = NIL;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Editor_Set_Default_Message)
{
	LISP_FUNC_BEGIN(0);
	if (CormanLispServer)
		CormanLispServer->SetDefaultMessage();
	ret = NIL;
	LISP_FUNC_RETURN(ret);
}

static char messageBuf[256];
LispFunction(Editor_Get_Message)
{
	LISP_FUNC_BEGIN(0);
	if (CormanLispServer)
		CormanLispServer->GetMessage(messageBuf, 256);
	ret = stringNode(messageBuf);
	LISP_FUNC_RETURN(ret);
}

//
//	Usage: (create-thread func)
//	Returns: thread handle (foreign pointer)
//
extern LispObj RunSecondaryThread(LispObj func);
LispFunction(Create_Thread)
{
	LISP_FUNC_BEGIN(1);
	LispObj func = LISP_ARG(0);
	checkFunction(func);
	ret = RunSecondaryThread(func);
	LISP_FUNC_RETURN(ret);
}

// used by the compiler
// (CL::COMPILE-SUB-FORM form dest result-type)
//
LispFunction(Compile_Sub_Form)
{
	LISP_FUNC_BEGIN(3);
	ret = compileForm(LISP_ARG(0), LISP_ARG(1), LISP_ARG(2));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Register_Untagged_Values)
{
	LISP_FUNC_BEGIN(2);
	registerUntaggedValues(LISP_ARG(0), LISP_ARG(1));
	LISP_FUNC_RETURN(NIL);
}

LispObj allocateCHeap(LispObj size)
{
	LispObj ret = 0;

	ret = AllocVector(FOREIGN_HEAP_PTR_SIZE);
	setUvectorType(ret, ForeignHeapType);

	foreignPtr(ret) = (LispObj)CAlloc(integer(size));
	foreignHeapSize(ret) = size;
	foreignHeapType(ret) = 0;
	addFinalizationObject(ret, symbolFunction(DEALLOCATE_C_HEAP));
	return ret;
}

LispFunction(Allocate_C_Heap)
{
	LISP_FUNC_BEGIN(1);
	LispObj size = LISP_ARG(0);
	checkInteger(size);
	ret = allocateCHeap(size);
	LISP_FUNC_RETURN(ret);
}

void deallocateCHeap(LispObj obj)
{
	if (foreignPtr(obj) != 0)
		CFree((void*)foreignPtr(obj));
	foreignPtr(obj) = 0;
	foreignHeapSize(obj) = 0;
	foreignHeapType(obj) = 0;
}

LispFunction(Deallocate_C_Heap)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);
	if (!isForeignHeapPtr(obj))
		Error("Not a C Ptr: ~A", obj);
	deallocateCHeap(obj);
	LISP_FUNC_RETURN(NIL);
}

// figure 16 leap years between 1900 and 1970
const int days_from_1900_to_1970 
	= 24 * ((365 * 70) + 16);
LispFunction(Get_Universal_Time)
{
	LISP_FUNC_BEGIN(0);
	_timeb time;
	_ftime_s(&time);
	ret = _Add(_Multiply(wrapInteger(days_from_1900_to_1970), wrapInteger(60 * 60)),
			createLispInteger((long)time.time));
	LISP_FUNC_RETURN(ret);
}

//
//	Corman Lisp LOCAL-TIME-ZONE function.
//	Number of hours differential between local time and UCT (increases
//	as you go west) between -24 and 24.
//	The second value returned is T if daylight savings time is in place,
//	NIL otherwise.
//
LispFunction(Local_Time_Zone)
{
	LISP_FUNC_BEGIN(0);
	TIME_ZONE_INFORMATION tzi;
	DWORD result = GetTimeZoneInformation(&tzi);
	LispObj daylight_time = NIL;
	if (result == 0xffffffff)
		ret = 0;			// we can't determine it
	else
	{
		ret = _Divide(wrapInteger(tzi.Bias), wrapInteger(60));
		if (result == TIME_ZONE_ID_DAYLIGHT)
			daylight_time = T;
	}
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = list(ret, daylight_time, END_LIST);
	ReturnCount(2);
	return ret;
}

LispFunction(Get_System_Time)
{
	LISP_FUNC_BEGIN(0);
	SYSTEMTIME time;
	GetSystemTime(&time);
	ret = list(wrapInteger(time.wYear),
			   wrapInteger(time.wMonth),
			   wrapInteger(time.wDayOfWeek),
			   wrapInteger(time.wDay),
			   wrapInteger(time.wHour),
			   wrapInteger(time.wMinute),
			   wrapInteger(time.wSecond),
			   wrapInteger(time.wMilliseconds),
			   END_LIST);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_Local_Time)
{
	LISP_FUNC_BEGIN(0);
	SYSTEMTIME time;
	GetLocalTime(&time);
	ret = list(wrapInteger(time.wYear),
			   wrapInteger(time.wMonth),
			   wrapInteger(time.wDayOfWeek),
			   wrapInteger(time.wDay),
			   wrapInteger(time.wHour),
			   wrapInteger(time.wMinute),
			   wrapInteger(time.wSecond),
			   wrapInteger(time.wMilliseconds),
			   END_LIST);
	LISP_FUNC_RETURN(ret);
}

LispFunction(System_Time_To_File_Time)
{
	LISP_FUNC_BEGIN(8);
	SYSTEMTIME	time	= {0};
	FILETIME	ftime	= {0};
	LispObj bn = 0;
	BOOL result = 0;

	checkInteger(LISP_ARG(0));
	checkInteger(LISP_ARG(1));
	checkInteger(LISP_ARG(2));
	checkInteger(LISP_ARG(3));
	checkInteger(LISP_ARG(4));
	checkInteger(LISP_ARG(5));
	checkInteger(LISP_ARG(6));
	checkInteger(LISP_ARG(7));

	time.wYear			= (short)integer(LISP_ARG(0));
	time.wMonth			= (short)integer(LISP_ARG(1));
	time.wDayOfWeek		= (short)integer(LISP_ARG(2));
	time.wDay			= (short)integer(LISP_ARG(3));
	time.wHour			= (short)integer(LISP_ARG(4));
	time.wMinute		= (short)integer(LISP_ARG(5));
	time.wSecond		= (short)integer(LISP_ARG(6));
	time.wMilliseconds	= (short)integer(LISP_ARG(7));
	result = SystemTimeToFileTime(&time, &ftime);
	if (!result)
		Error("SystemTimeToFileTime() failed, error code: ", 
			createLispInteger(GetLastError()));

	if (ftime.dwHighDateTime == 0)
		ret = createLispInteger(ftime.dwLowDateTime);
	else
	{
		bn = bignumNode(wrapInteger(2));	
		UVECTOR(bn)[BIGNUM_FIRST_CELL] = ftime.dwLowDateTime;
		UVECTOR(bn)[BIGNUM_FIRST_CELL + 1] = ftime.dwHighDateTime;
		ret = bn;
	}
	LISP_FUNC_RETURN(ret);
}

LispFunction(File_Time_To_System_Time)
{
	LISP_FUNC_BEGIN(1);
	SYSTEMTIME	time	= {0};
	FILETIME	ftime	= {0};
	LispObj n = LISP_ARG(0);
	BOOL result = 0;
	if (isInteger(n))
		ftime.dwLowDateTime = integer(n);
	else
	if (isBignum(n))
	{
		ftime.dwLowDateTime = UVECTOR(n)[BIGNUM_FIRST_CELL];
		ftime.dwHighDateTime = UVECTOR(n)[BIGNUM_FIRST_CELL + 1];
	}
	else
		Error("Invalid file time: ~A", n);
	result = FileTimeToSystemTime(&ftime, &time);
	if (!result)
		Error("SystemTimeToFileTime() failed, error code: ",
			createLispInteger(GetLastError()));
	ret = list(wrapInteger(time.wYear),
			   wrapInteger(time.wMonth),
			   wrapInteger(time.wDayOfWeek),
			   wrapInteger(time.wDay),
			   wrapInteger(time.wHour),
			   wrapInteger(time.wMinute),
			   wrapInteger(time.wSecond),
			   wrapInteger(time.wMilliseconds),
			   END_LIST);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Bignum_Integer_Length)
{
	LISP_FUNC_BEGIN(1);
	ret = bignumIntegerLength(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Bignum_Shift)
{
	LISP_FUNC_BEGIN(2);
	ret = bignumShift(LISP_ARG(0), LISP_ARG(1), T);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Create_Func_Table_Entry)
{
	LISP_FUNC_BEGIN(1);
	checkSymbol(LISP_ARG(0));
	createFuncTableEntry(LISP_ARG(0));
	ret = NIL;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Create_Var_Table_Entry)
{
	LISP_FUNC_BEGIN(1);
	checkSymbol(LISP_ARG(0));
	createSymbolTableEntry(LISP_ARG(0));
	ret = NIL;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Find_Keyword)
{
	LISP_FUNC_BEGIN(1);
	checkString(LISP_ARG(0));
	ret = findKeywordSym(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Console_Overflow_Function)
{
	LISP_FUNC_BEGIN(1);
	ret = consoleOverflow(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Console_Underflow_Function)
{
	LISP_FUNC_BEGIN(1);
	ret = consoleUnderflow(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

LispFunction(File_Overflow_Function)
{
	LISP_FUNC_BEGIN(1);
	ret = fileOverflow(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

LispFunction(File_Underflow_Function)
{
	LISP_FUNC_BEGIN(1);
	ret = fileUnderflow(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

LispFunction(String_Overflow_Function)
{
	LISP_FUNC_BEGIN(1);
	ret = stringOverflow(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

LispFunction(String_Underflow_Function)
{
	LISP_FUNC_BEGIN(1);
	ret = stringUnderflow(LISP_ARG(0));
	LISP_FUNC_RETURN(ret);
}

//
//	Corman Lisp MAKE-WEAK-POINTER function.
//	Used to create a weak pointer initialized to the passed
//	heap object.
//
LispFunction(Make_Weak_Pointer)
{
	LISP_FUNC_BEGIN(1);
	LispObj w = 0;
	w = weakPointer();
	weakPtr(w) = LISP_ARG(0);
	WEAK_PTR_REGISTRY = cons(w, WEAK_PTR_REGISTRY);
	LISP_FUNC_RETURN(w);
}

LispFunction(Address_Find_Function_Callback)
{
	LISP_FUNC_BEGIN(1);
	unsigned long execaddr = 0;
	unsigned long address = 0;
	LispObj currOffset = 0;
	LispObj p = LISP_ARG(0);

	address = lispIntegerToUnsignedLong(symbolValue(FIND_FUNCTION_CURR_ADDR));
	currOffset = symbolValue(FIND_FUNCTION_CURR_OFFSET);
	if (isFunction(p))
	{
		execaddr = (unsigned long)functionAddress(p);
		if (execaddr && *(byte*)execaddr == 0xe9)
			execaddr = (((unsigned long)execaddr) + *(long*)(((byte*)execaddr) + 1) + 5);

		// execaddr = LispCall(Funcall, EXECUTION_ADDRESS, p);
		// assume function smaller than 64k in size
		if (execaddr <= address && (address - execaddr) < 0x10000)	
		{
			if ((address - execaddr) < lispIntegerToUnsignedLong(currOffset))
			{
				setSymbolValue(FIND_FUNCTION_CURR_OFFSET, createLispInteger(address - execaddr));
				setSymbolValue(FIND_FUNCTION_CURR_FUNC, p);
			}
		}
	}
	LISP_FUNC_RETURN(NIL);
}

LispFunction(Process_Each_Heap_Block)
{
	LISP_FUNC_BEGIN(1);
	processEachHeapBlock(LISP_ARG(0), T);
	LISP_FUNC_RETURN(NIL);
}

LispFunction(Console_Input_Chars_Available)
{
	LISP_FUNC_BEGIN(0);
	ret = wrapInteger(TerminalInputBuf.numchars());
	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_Application_Instance)
{
	LISP_FUNC_BEGIN(0);
	HINSTANCE instance = 0;
	if (CormanLispServer)
		CormanLispServer->GetAppInstance(&instance);
	ret = AllocVector(FOREIGN_SIZE);
	setUvectorType(ret, ForeignType);
	foreignPtr(ret) = (LispObj)instance;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_Application_Main_Window)
{
	LISP_FUNC_BEGIN(0);
	HWND wnd = 0;
	if (CormanLispServer)
		CormanLispServer->GetAppMainWindow(&wnd);
	ret = AllocVector(FOREIGN_SIZE);
	setUvectorType(ret, ForeignType);
	foreignPtr(ret) = (LispObj)wnd;
	LISP_FUNC_RETURN(ret);
}

//
//	Common Lisp ED function (open edit window)
//
LispFunction(Ed)
{
	LISP_FUNC_BEGIN(1);
	LispObj filename = LISP_ARG(0);
	LispObj fp = 0;
	checkString(filename);
	HRESULT rv = E_FAIL;
	HWND wnd = 0;
	if (CormanLispServer)
		rv = CormanLispServer->OpenEditWindow((char*)byteArrayStart(nullTerminate(filename)), &wnd);
	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)wnd;
	ret = fp;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Display_URL)
{
	LISP_FUNC_BEGIN(1);
	LispObj filename = LISP_ARG(0);
	LispObj fp = 0;
	checkString(filename);
	HRESULT rv = E_FAIL;
	HWND wnd = 0;
	if (CormanLispServer)
		rv = CormanLispServer->OpenURL((char*)byteArrayStart(nullTerminate(filename)), &wnd);
	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)wnd;
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = list(fp, createLispInteger(rv), END_LIST);
	ret = fp;
	ReturnCount(2);
	return ret;
}

//
//	Common Lisp non-standard ADD-MENU-ITEM function
//
LispFunction(Add_Menu_Item)
{
	LISP_FUNC_BEGIN(2);
	LispObj menu = LISP_ARG(0);
	LispObj item = LISP_ARG(1);
	HRESULT rv = E_FAIL;

	checkString(menu);
	if (item == NIL)
	{
		if (CormanLispServer)
			rv = CormanLispServer->AddMenu((char*)byteArrayStart(nullTerminate(menu)));
	}
	else
	{
		checkString(item);
		if (CormanLispServer)
			rv = CormanLispServer->AddMenuItem(
						(char*)byteArrayStart(nullTerminate(menu)),
						(char*)byteArrayStart(nullTerminate(item)));
	}
	if (rv == S_OK)
		ret = T;
	else
		ret = NIL;
	LISP_FUNC_RETURN(ret);
}

// implemented later
LispFunction(Typeexpand_All)
{
	LISP_FUNC_BEGIN(1);
	ret = LISP_ARG(0);
	LISP_FUNC_RETURN(ret);
}

// implemented later
LispFunction(Lookup_Ftype)
{
	LISP_FUNC_BEGIN(1);
	LISP_FUNC_RETURN(NIL);
}

void __declspec(naked) Plus_EAX_EDX()
{
	__asm	push	ebp
	__asm	mov		ebp, esp
    __asm   push    edi
	__asm	push	edx
	__asm	push	eax
	__asm	mov		edi, dword ptr [esi]
	__asm	mov		ecx, 2
	__asm	call	Plus
	__asm	add		esp, 8
    __asm   pop     edi
	__asm	pop		ebp
	__asm	ret
}

void __declspec(naked) Minus_EAX_EDX()
{
	__asm	push	ebp
	__asm	mov		ebp, esp
    __asm   push    edi
	__asm	push	eax
	__asm	push	edx
	__asm	mov		edi, dword ptr [esi]
	__asm	mov		ecx, 2
	__asm	call	Minus
	__asm	add		esp, 8
    __asm   pop     edi
	__asm	pop		ebp
	__asm	ret
}

void __declspec(naked) Load_QV_Reg()
{
	__asm	push	ebp
	__asm	mov		ebp, esp
	TlsGetValue(QV_Index);
	__asm	mov		esi, eax
	__asm	pop		ebp
	__asm	ret
}

void __declspec(naked) genericThunkFunc	()
{
	__asm	mov		eax, dword ptr [0x1000000]		;; use global QV
	__asm	jmp		dword ptr [eax+0x12345678] ;; replace this with actual offset
}
const int sizeGenericThunk = 11;	// have to measure this and keep in sync

LispFunction(Create_Callback_Thunk)
{
	LISP_FUNC_BEGIN(1);
	LispObj sym = LISP_ARG(0);
	checkSymbol(sym);
	createFuncTableEntry(sym);
	LispObj index = UVECTOR(sym)[SYMBOL_JUMP_TABLE] * 4;
	LispObj thunkObj = LispCall1(Allocate_C_Heap, wrapInteger(sizeGenericThunk));
	foreignHeapType(thunkObj) = wrapInteger(FOREIGN_HEAP_TYPE_CALLBACK_THUNK);	// tag as callback thunk
	memcpy((void*)foreignPtr(thunkObj), (void*)genericThunkFunc, sizeGenericThunk);
	*(unsigned char*)foreignPtr(thunkObj) = 0xA1;
	*(unsigned long*)(((char*)foreignPtr(thunkObj)) + 1) = (unsigned long)SysGlobalsAddr;
	*(long*)(((char*)foreignPtr(thunkObj)) + sizeGenericThunk - 4) = integer(index) + 4;
	FlushInstructionCache(GetCurrentProcess(), NULL, 0);
	LISP_FUNC_RETURN(thunkObj);
}

//
//	Returns T if being called from the IDE, false otherwise.
//
extern int CormanLispClientType;
LispFunction(CormanLisp_Client_Type)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(wrapInteger(CormanLispClientType));
}

// This may currently only be called by one thread at a time.
LispFunction(Allocate_Foreign_Jump_Table_Entry)
{
	LISP_FUNC_BEGIN(1);
	LispObj execaddr = LISP_ARG(0);
	LispObj n = 0;
	unsigned char* jmp_entry = 0;
	unsigned long entryIndex = 0;

	if (!isForeign(execaddr))
		Error("Invalid type passed to %ALLOCATE-FOREIGN-JUMP-TABLE-ENTRY");
	if (*GlobalForeignJumpTableNumEntries >= *GlobalForeignJumpTableCapacity)
		Error("Failed to allocate a foreign jump table entry--table capacity exceeded");

	n = foreignNode();
	entryIndex = *GlobalForeignJumpTableNumEntries;
	(*GlobalForeignJumpTableNumEntries)++;			// thread synchronization issue here!
	jmp_entry = *GlobalForeignJumpTable + (entryIndex * SizeOfForeignJumpTableEntry);

	jmp_entry[0] = 0x8F;					// pop [edi]   ([edi] = return address)
	jmp_entry[1] = 0x07;
	jmp_entry[2] = 0xE8;					// call addr
	*(LispObj*)(jmp_entry + 3) = UVECTOR(execaddr)[FOREIGN_PTR] - (unsigned long)(jmp_entry + 7); // compute offset
	jmp_entry[7] = 0xFF;					// jmp [edi]
	jmp_entry[8] = 0x27;

	UVECTOR(n)[FOREIGN_PTR] = (LispObj)jmp_entry;

	LISP_FUNC_RETURN(n);
}

LispFunction(Clear_Foreign_Jump_Table)
{
	LISP_FUNC_BEGIN(0);
	*GlobalForeignJumpTableNumEntries = 0;
	LISP_FUNC_RETURN(T);
}

LispFunction(Sys_Globals_Address)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(createLispInteger((unsigned long)SysGlobalsAddr));
}

LispFunction(Get_Current_Thread_IDs)
{
	LISP_FUNC_BEGIN(0);
	int x = 0;
	DWORD* p = 0;
	int i = 0;

	x = GetNumLispThreads();
	x++;		// make buffer one larger just in case somehow another thread starts
	p = (DWORD*)_alloca(x * sizeof(DWORD));
	x = ThreadList.GetLispThreadIDs(p, x);
	ret = NIL;
	for (x-- ;x >= 0; x--)
	{
		ret = cons(wrapInteger(p[x]), ret);
	}
	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_Num_Lisp_Threads)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(wrapInteger(NumLispThreads));
}

LispFunction(Thread_Handle)
{
	LISP_FUNC_BEGIN(1);
	checkLispInteger(LISP_ARG(0));

	HANDLE h = ThreadList.GetLispThreadHandle(lispIntegerToUnsignedLong(LISP_ARG(0)));
	if (h == 0)
		ret = NIL;
	else
	{
		ret = foreignNode();
		UVECTOR(ret)[FOREIGN_PTR] = (LispObj)h;
	}
	LISP_FUNC_RETURN(ret);
}
//
//	Corman Lisp TERMINATE-THREAD function. Takes a thread identifier
//  and an exit code, and cause the thread to terminate.
//
extern void TerminateLispThread(LispObj threadID, LispObj ret);

LispFunction(Terminate_Thread)
{
	LISP_FUNC_BEGIN(2);
	checkLispInteger(LISP_ARG(0));
	TerminateLispThread(LISP_ARG(0), LISP_ARG(1));
	LISP_FUNC_RETURN(LISP_ARG(1));
}

LispFunction(Enter_Critical_Section)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);
	if (!isForeignHeapPtr(obj))
		Error("Not a valid CRITICAL-SECTION object: ~A", LISP_ARG(0));
	if (foreignHeapType(obj) != wrapInteger(FOREIGN_HEAP_TYPE_CRITICAL_SECTION))
		Error("Not a valid CRITICAL-SECTION object: ~A", LISP_ARG(0));
	EnterCriticalSection((CRITICAL_SECTION*)foreignPtr(obj));
	LISP_FUNC_RETURN(NIL);
}

LispFunction(Leave_Critical_Section)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);
	if (!isForeignHeapPtr(obj))
		Error("Not a valid CRITICAL-SECTION object: ~A", LISP_ARG(0));
	if (foreignHeapType(obj) != wrapInteger(FOREIGN_HEAP_TYPE_CRITICAL_SECTION))
		Error("Not a valid CRITICAL-SECTION object: ~A", LISP_ARG(0));
	LeaveCriticalSection((CRITICAL_SECTION*)foreignPtr(obj));
	LISP_FUNC_RETURN(NIL);
}

LispFunction(Allocate_Critical_Section)
{
	LISP_FUNC_BEGIN(0);
	ret = AllocVector(FOREIGN_HEAP_PTR_SIZE);
	setUvectorType(ret, ForeignHeapType);

	// allocate 4 byte multiples
	foreignPtr(ret) = (LispObj)CAlloc(sizeof(CRITICAL_SECTION));
	foreignHeapSize(ret) = wrapInteger(sizeof(CRITICAL_SECTION));
	foreignHeapType(ret) = wrapInteger(FOREIGN_HEAP_TYPE_CRITICAL_SECTION);
	addFinalizationObject(ret, symbolFunction(DEALLOCATE_CRITICAL_SECTION));
	InitializeCriticalSection((CRITICAL_SECTION*)foreignPtr(ret));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Deallocate_Critical_Section)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);
	if (!isForeignHeapPtr(obj))
		Error("Not a CRITICAL-SECTION object: ~A", obj);
	if (foreignPtr(obj) != 0)
	{
		DeleteCriticalSection((CRITICAL_SECTION*)foreignPtr(obj));
		CFree((void*)foreignPtr(obj));
		foreignPtr(obj) = 0;
	}
	foreignHeapSize(obj) = 0;
	foreignHeapType(obj) = 0;
	LISP_FUNC_RETURN(NIL);
}

/*
extern "C" int compress(byte* dest,   unsigned long* destLen,
                        byte* source, unsigned long  sourceLen);
extern "C" int uncompress(byte* dest,   unsigned long* destLen,
                        byte* source, unsigned long  sourceLen);
*/
extern byte* allocateCompressionBuffer(unsigned long size);
extern void freeCompressionBuffer(byte* buf);

//
//	Corman Lisp COMPRESS_BYTES functions (src-buffer)
//	The passed buffer should be a byte vector, and a byte
//	vector of the compressed bytes is returned.
//	The output format is:
//	bytes 0 - 3:  unsigned length (untagged) of compressed data
//	bytes 4 - 7   unsigned length (untegged) of uncompressed data
//  bytes 8 - ... Compressed bytes
//
LispFunction(Compress_Bytes)
{
	LISP_FUNC_BEGIN(1);
	LispObj src = LISP_ARG(0);
	long srclen = 0;
	unsigned long destlen = 0;
	LispObj length = 0;
	int compret = 0;
	length = vectorLength(src);
	if (arrayHasFillPointer(src))
		length = arrayFillPointer(src);
	srclen = integer(length);
	// compression can cause the data to grow to 1.01% + 12 bytes (in worst case!)
	destlen = ((long)(integer(length) * 1.02)) + 12;
	byte* srcbuf = allocateCompressionBuffer(srclen);
	byte* destbuf = allocateCompressionBuffer(destlen);
	
	// compress the buffer
	memcpy(srcbuf, byteArrayStart(src), srclen);
	compret = compress(destbuf, &destlen, srcbuf, srclen);
	if (compret != 0)
	{
		freeCompressionBuffer(srcbuf);
		freeCompressionBuffer(destbuf);
		Error("A compression error occurred");
	}
	ret = byteVector(wrapInteger(destlen + 8));
	memcpy(byteArrayStart(ret), &destlen, 4);		// size of compressed result
	memcpy(byteArrayStart(ret) + 4, &srclen, 4);	// size of uncompressed result
	memcpy(byteArrayStart(ret) + 8, destbuf, destlen);
	freeCompressionBuffer(srcbuf);
	freeCompressionBuffer(destbuf);
	LISP_FUNC_RETURN(ret);
}

//
//	Corman Lisp UNCOMPRESS_BYTES functions (src-buffer)
//	The passed buffer should be a byte vector, and a byte
//	vector of the uncompressed bytes is returned. If the data 
//	could not be uncompressed, NIL is returned. 
//	It is assumed that the data is in the format output by the
//	COMPRESS-BYTES function, above.
//
LispFunction(Uncompress_Bytes)
{
	LISP_FUNC_BEGIN(1);
	LispObj src = LISP_ARG(0);
	long srclen = 0;
	unsigned long destlen = 0;
	LispObj length = 0;
	int compret = 0;
	length = vectorLength(src);
	memcpy(&srclen, byteArrayStart(src), 4);		// size of compressed result
	memcpy(&destlen, byteArrayStart(src) + 4, 4);	// size of uncompressed result
	if (srclen != integer(length) - 8)
		Error("A decompression error occurred");

	byte* srcbuf = allocateCompressionBuffer(srclen);
	byte* destbuf = allocateCompressionBuffer(destlen);
	
	// uncompress the buffer
	memcpy(srcbuf, byteArrayStart(src) + 8, srclen);
	compret = uncompress(destbuf, &destlen, srcbuf, srclen);
	if (compret != 0)
	{
		freeCompressionBuffer(srcbuf);
		freeCompressionBuffer(destbuf);
		Error("A decompression error occurred");
	}
	ret = byteVector(wrapInteger(destlen));
	memcpy(byteArrayStart(ret), destbuf, destlen);
	freeCompressionBuffer(srcbuf);
	freeCompressionBuffer(destbuf);
	LISP_FUNC_RETURN(ret);
}

//
//	Corman Lisp function UNCOMPRESS-FOREIGN-BYTES.
//	Like UNCOMPRESS-BYTES, except using foreign byte buffers.
//	Returns a buffer allocated with CT:MALLOC.
//
LispFunction(Uncompress_Foreign_Bytes)
{
	LISP_FUNC_BEGIN(1);
	LispObj src = LISP_ARG(0);
	long srclen = 0;
	unsigned long destlen = 0;
	LispObj length = 0;
	int compret = 0;

	if (!isForeign(src) && !isForeignHeapPtr(src))
		Error("Not a foreign pointer: ~A", src);

	memcpy(&srclen, (byte*)foreignPtr(src), 4);		// size of compressed result
	memcpy(&destlen, (byte*)foreignPtr(src) + 4, 4);// size of uncompressed result

	ret = allocateCHeap(wrapInteger(destlen));
	
	// uncompress the buffer
	compret = uncompress((byte*)foreignPtr(ret), &destlen, 
					(byte*)foreignPtr(src) + 8, srclen);
	if (compret != 0)
	{
		deallocateCHeap(ret);
		Error("A decompression error occurred");
	}
	LISP_FUNC_RETURN(ret);
}

LispFunction(ThrowSystemException)
{
	LISP_FUNC_BEGIN(1);
	LispObj ex = LISP_ARG(0);
	if (symbolValue(TRACE_EXCEPTIONS) != NIL)
		setSymbolValue(ERROR_TRACE, LispCall0(Stack_Trace));
	LispCall4(Funcall, THROW_EXCEPTION, SYSTEM_EXCEPTION, ex, wrapInteger(1));
	LISP_FUNC_RETURN(ret);
}

//  Redefined in kernel.
//	Returns T if the passed function NAME (symbol) is
//	intended to be inlined. The code generator needs this to
//	decide whether to keep the lambda expression.
//
LispFunction(Inline_Proclaim_P)
{
	LISP_FUNC_BEGIN(1);
	LispObj name = LISP_ARG(0);
	LISP_FUNC_RETURN(NIL);
}

//
//	Note: if any finalization function causes an error, the following
//	finalization functions will not be called.
//	This will be redefined and made thread-safe in lisp code.
//
LispFunction(Execute_Finalizers)
{
	LISP_FUNC_BEGIN(0);
	LispObj x = 0;
	x = symbolValue(FINALIZATION_PENDING);
	setSymbolValue(FINALIZATION_PENDING, 0);
	while (isCons(x))
	{
		LispCall3(Funcall, FUNCALL, CDR(CAR(x)), CAR(CAR(x)));
		x = CDR(x);
	}
	LISP_FUNC_RETURN(NIL);	
}

LispFunction(Compiler_Check_Args_Num)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(T);	
}

LispFunction(Compiler_Check_Types)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(T);	
}

LispFunction(Compiler_Fold_Constants)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(T);	
}

LispFunction(Compiler_Inline_Functions)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(T);	
}

LispFunction(Compiler_Optimize_Tail_Recursion)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(NIL);	
}

LispFunction(Compiler_Check_Key_Args)
{
	LISP_FUNC_BEGIN(0);
	LISP_FUNC_RETURN(T);	
}

LispFunction(Lookup_Setf_Function)
{
	LISP_FUNC_BEGIN(1);
	Error("SETF function not defined");		// defined in lisp code
	LISP_FUNC_RETURN(NIL);	
}

LispFunction(InvalidKeyArgError)
{
	LISP_FUNC_BEGIN(1);
	LispObj key = LISP_ARG(0);
	Error("The passed key ~S is not defined for this function", key);
	LISP_FUNC_RETURN(NIL);	// never returns
}

#define BUFLEN      16384
static char gzip_buf[BUFLEN];

LispFunction(Compress_File)
{
	LISP_FUNC_BEGIN(2);
	LispObj inpath = LISP_ARG(0);
	LispObj outpath = LISP_ARG(1);
	FILE* in = 0;
    gzFile out = 0;
	int len = 0;
	int err = 0;
	
	checkString(inpath);
	checkString(outpath);

    err = fopen_s(&in, (char*)byteArrayStart(nullTerminate(inpath)), "rb");
    if (err != 0) 
        Error("Could not open file ~S for reading", inpath);

    out = gzopen((char*)byteArrayStart(nullTerminate(outpath)), "wb");
    if (out == NULL)        
		Error("Could not open file ~S for writing", outpath);

    for (;;) 
	{
        len = fread(gzip_buf, 1, sizeof(gzip_buf), in);
        if (ferror(in)) 
			Error("An error occurred while reading file ~S", inpath);
        if (len == 0) break;
        if (gzwrite(out, gzip_buf, (unsigned)len) != len) 
			Error("An error occurred while writing file ~S", outpath);
    }
    fclose(in);
    if (gzclose(out) != Z_OK) 
		Error("An error occurred while closing file ~S", outpath);

	LISP_FUNC_RETURN(outpath);
}

LispFunction(Uncompress_File)
{
	LISP_FUNC_BEGIN(2);
	LispObj inpath = LISP_ARG(0);
	LispObj outpath = LISP_ARG(1);
	gzFile in = 0;
    FILE* out = 0;
    int len = 0;
    int err = 0;

	checkString(inpath);
	checkString(outpath);

    in = gzopen((char*)byteArrayStart(nullTerminate(inpath)), "rb");
    if (in == NULL) 
        Error("Could not open file ~S for reading", inpath);

    err = fopen_s(&out, (char*)byteArrayStart(nullTerminate(outpath)), "wb");
    if (err != 0)        
		Error("Could not open file ~S for writing", outpath);

    for (;;) 
	{
        len = gzread(in, gzip_buf, sizeof(gzip_buf));
        if (len < 0) 
			Error("An error occurred while reading file ~S", inpath);
        if (len == 0) break;

        if ((int)fwrite(gzip_buf, 1, (unsigned)len, out) != len)
			Error("An error occurred while writing file ~S", outpath);
    }
    if (fclose(out)) 
		Error("An error occurred while closing file ~S", outpath);

    if (gzclose(in) != Z_OK)
		Error("An error occurred while closing file ~S", inpath);

	LISP_FUNC_RETURN(outpath);
}

LispFunction(Editor_Replace_Selection)
{
	LISP_FUNC_BEGIN(1);
	LispObj text = LISP_ARG(0);
	checkString(text);
	char* str = (char*)byteArrayStart(nullTerminate(text));
	if (CormanLispServer)
		CormanLispServer->ReplaceSelection(str, strlen(str));
	LISP_FUNC_RETURN(ret);
}

LispFunction(Get_Callback)
{
	LISP_FUNC_BEGIN(1);
	LISP_FUNC_RETURN(0);
}

LispFunction(Heap_Used)
{
	LISP_FUNC_BEGIN(1);
	LispObj percent = 0;
	long allocated = 0;
	long current = 0;
	percent = createLispInteger(getHeapStatistics(integer(LISP_ARG(0)), &allocated, &current));
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = 
		list(percent, createLispInteger(allocated), createLispInteger(current), END_LIST);
	ret = percent;
	ReturnCount(3);
	return ret;
}

LispFunction(Containing_Heap)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = 0;
	obj = LISP_ARG(0);
	if (isCons(obj) || isUvector(obj))
	{
		if (inEphemeralHeap1AddressRange(obj))
			ret = wrapInteger(0);
		else
		if (inEphemeralHeap2AddressRange(obj))
			ret = wrapInteger(1);
		else
		if (inLispHeap1AddressRange(obj))
			ret = wrapInteger(2);
		else
		if (inLispHeap2AddressRange(obj))
			ret = wrapInteger(3);
		else
			ret = NIL;
	}
	else
		ret = NIL;
	LISP_FUNC_RETURN(ret);
}

LispFunction(Registration_Info)
{	LISP_FUNC_BEGIN(0);
	HMODULE module = 0;
	LispObj registered = T;
	LispObj version = NIL;
	LispObj name = NIL;
	LispObj organization = NIL;
	LispObj daysRemaining = wrapInteger(0);

	// we should fill this values for better compatiblity with older versions
	if (CurrentUserInfo != NULL)
	{
		version = wrapInteger(CurrentUserInfo->GetVersion());
		name = stringNode(CurrentUserInfo->GetName());
	}
	ThreadQV()[MULTIPLE_RETURN_VALUES_Index] = 
		list(registered, version, name, organization, daysRemaining, END_LIST);
	ret = registered;
	ReturnCount(3);
	return ret;
}

LispFunction(Get_GC_ID)
{
	LISP_FUNC_BEGIN(0);
	LispObj id = 0;
	id = createUnsignedLispInteger(garbageCollectionID);
	LISP_FUNC_RETURN(id);
}

LispFunction(Reset_Hash_ID)
{
	LISP_FUNC_BEGIN(1);
	LispObj p = LISP_ARG(0);
	if (isHashtable(p))
	{
		UVECTOR(p)[2] = wrapInteger(2);
		UVECTOR(p)[12] = 0;	// zero out the hash ID
	}
	LISP_FUNC_RETURN(NIL);
}

LispFunction(Suspend_Other_Threads)
{
	LISP_FUNC_BEGIN(0);

	// suspend all threads but the current one
	ThreadList.suspendAllOtherThreads();

	LISP_FUNC_RETURN(NIL);
}

LispFunction(Resume_Other_Threads)
{
	LISP_FUNC_BEGIN(0);

	// resume all suspended threads
	ThreadList.resumeAllOtherThreads();

	LISP_FUNC_RETURN(NIL);
}

LispFunction(UpdateJumpTable)
{
	LISP_FUNC_BEGIN(3);
	LispObj sym = 0;
	LispObj func = 0;
	LispObj env = 0;
	sym = LISP_ARG(0);
	func = LISP_ARG(1);
	env = LISP_ARG(2);
	updateJumpTable(sym, func, env);
	LISP_FUNC_RETURN(NIL);
}

//
// Enable or disable hardware-assisted gc.
// If called with an argument, it sets the value.
// If called with no arguments, then it just returns the current setting.
//
LispFunction(Hardware_GC)
{
	LISP_FUNC_BEGIN_VARIABLE(0, 1);
	LispObj sym = 0;
	if (ARG_COUNT == 1)
	{
		sym = LISP_ARG(0);
		if (sym == NIL && HardwareAssist == 1)
		{
			// turn hardware-assist off
			garbageCollect(2);		// do full collection
			HardwareAssist = 0;		// switch off
			garbageCollect(2);		// another full collection
		}
		else if (sym != NIL && HardwareAssist == 0)
		{
			// turn hardware-assist on
			garbageCollect(2);		// do full collection
			HardwareAssist = 1;		// switch on
			garbageCollect(2);		// another full collection
		}
	}
	else
		sym = (HardwareAssist ? T : NIL);
	LISP_FUNC_RETURN(sym);
}

FunctEntry functTable[] =
{
	{	"SYMBOL-VALUE",				SymbolValue				},	// redefined in lisp
	{	"SYMBOL-FUNCTION",			SymbolFunction			},	// redefined in lisp
	{	"MACRO-FUNCTION",			Macro_Function			},	// redefined in lisp
	{	"LIST*",					listStar				},	// redefined in lisp
	{	"LIST",						lispList				},	// redefined in lisp
	{	"APPEND",					lispAppend				},	// redefined in lisp
	{	"FUNCALL",					Funcall					},	// redefined in lisp
	{	"%CREATE-CLOSURE",			create_closure			},	// redefined in lisp
	{	"SET-SYMBOL-FUNCTION",		SetSymbolFunction		},	// redefined in lisp
	{	"SET-SYMBOL-MACRO",			SetSymbolMacro			},	// redefined in lisp
	{	"GC",						gc						},	// leave in kernel
	{	"SAVE-IMAGE",				SaveLispImage			},	// leave in kernel
	{	"LOAD-IMAGE",				LoadLispImage			},	// leave in kernel
	{	"%STRINGNODE",				(LispFunc)stringNode	},	// redefined in lisp
	{	"%FOREIGNNODE",				(LispFunc)foreignNode	},	// redefined in lisp
	{	"%CREATE-BIG-NUM",			(LispFunc)createBignum	},	// not used anywhere 03/13/00
	{	"%DOUBLE-FLOAT-NODE",		(LispFunc)doubleFloatNode}, // redefined in lisp
	{	"%SINGLE-FLOAT-NODE",		(LispFunc)singleFloatNode}, // redefined in lisp
	{	"LOAD",						Load					},  // redefined in lisp
	{	"CAR",						Car						},	// redefined in lisp
	{	"CDR",						Cdr						},	// redefined in lisp
	{	"EVAL",						Eval					},	// redefined in lisp
	{	"COMPILE-FORM",				Compile_Form			},  // leave in kernel
	{	"COMPILE-LAMBDA",			Compile_Lambda			},  // leave in kernel
	{	"%WRONG-NUMBER-OF-ARGS",	WrongNumberOfArgs		},	// redefined in lisp
	{	"%UNBOUND-VARIABLE",		(LispFunc)UnboundVariable}, // redefined in lisp
	{	"%INVALID-FIXNUM",			(LispFunc)InvalidFixnum	},	// redefined in lisp
	{	"%CHECK-LIST",				(LispFunc)checkList		},	// redefined in lisp
	{	"+",						Plus					},	// refedined in lisp
	{	"-",						Minus					},	// refedined in lisp
	{	"*",						Multiply				},	// refedined in lisp
	{	"/",						Divide					},	// refedined in lisp
	{	"MOD",						Mod						},  // redefined in lisp
	{	"NULL",						Null					},	// redefined in lisp
	{	"EQ",						Eq						},	// redefined in lisp
	{	"%UNDEFINED-FUNCTION",		UndefinedFunction		},  // redefined in lisp
	{	"READ",						Read					},	// redefined in lisp
	{	"WRITE",					Write					},	// redefined in lisp
	{	"CLOSE",					Close					},	// redefined in lisp
	{	"OPEN-INPUT-FILE",			Open_Input_File			},  // redefined in lisp
	{	"=",						NumericEqual			},	// refedined in lisp
	{	"<",						Less					},	// refedined in lisp
	{	"<=",						LessEqual				},	// refedined in lisp
	{	">",						Greater					},	// refedined in lisp
	{	">=",						GreaterEqual			},	// refedined in lisp
	{	"/=",						NotEqual				},	// refedined in lisp
	{	"CONS",						Cons					},	// redefined in lisp
	{	"CONSP",					Consp					},	// redefined in lisp
	{	"SYMBOLP",					Symbolp					},	// redefined in lisp
	{	"STRINGP",					Stringp					},	// redefined in lisp
	{	"STREAMP",					Streamp					},	// redefined in lisp
	{	"HASH-TABLE-P",				Hash_table_p			},	// redefined in lisp
	{	"PACKAGEP",					Packagep				},	// redefined in lisp
	{	"READTABLEP",				Readtablep				},	// redefined in lisp
	{	"ARRAYP",					Arrayp					},	// redefined in lisp
	{	"SEQUENCEP",				Sequencep				},	// redefined in lisp
	{	"STRUCTUREP",				Structurep				},	// redefined in lisp
	{	"INTEGERP",					Integerp				},	// redefined in lisp
	{	"UVECTORP",					Uvectorp				},	// redefined in lisp
	{	"BIGNUMP",					Bignump					},	// redefined in lisp
	{	"FIXNUMP",					Fixnump					},	// redefined in lisp
	{	"FLOATP",					Floatp					},	// redefined in lisp
	{	"SHORT-FLOAT-P",			Short_Float_P			},	// redefined in lisp
	{	"SINGLE-FLOAT-P",			Single_Float_P			},	// redefined in lisp
	{	"DOUBLE-FLOAT-P",			Double_Float_P			},	// redefined in lisp
	{	"RATIOP",					Ratiop					},	// redefined in lisp
	{	"COMPLEXP",					Complexp				},	// redefined in lisp
	{	"LISTP",					Listp					},	// redefined in lisp
	{	"CHARACTERP",				Characterp				},	// redefined in lisp
	{	"ERROR",					LispError				},	// redefined in lisp
	{	"GENSYM",					Gensym					},	// redefined in lisp
	{	"GET-INTERNAL-RUN-TIME",	Get_Internal_Run_Time	},  // redefined in lisp
	{	"GET-INTERNAL-TIME-UNITS-PER-SECOND",Get_Time_Units_Per_Second}, // redefined in lisp
	{	"GET-MILLISECOND-COUNT",	Get_Millisecond_Count	},  // not used anywhere 07/20/06
	{	"GET-INSTRUCTION-COUNT",	Get_Instruction_Count	},  // not used anywhere 07/20/06
	{	"GET-GC-TIME",				Get_Garbage_Collection_Time	}, // not used anywhere 07/20/06 (time function overridden)
	{	"TERPRI",					Terpri					},  // redefined in lisp
	{	"FORCE-OUTPUT",				Force_Output			},	// redefined in lisp
	{	"%SYMBOL-GET-FLAGS",		Symbol_Get_Flags		},	// redefined in lisp
	{	"%SYMBOL-SET-FLAGS",		Symbol_Set_Flags		},	// redefined in lisp
	{	"BIT-OR",					Bit_Or					},	// redefined in lisp
	{	"%ALLOC-CONS",				(LispFunc)AllocLocalCons},	// redefined in lisp
	{	"%ALLOC-VECTOR",			(LispFunc)LispAllocVector}, // already naked asm in C++, leave for now
	{	"%PUSH_CATCHER",			(LispFunc)pushCatcher	},  // redefined in lisp
	{	"%POP_CATCHER",				(LispFunc)popCatcher	},  // redefined in lisp
	{	"%POP_SPECIALS",			(LispFunc)popSpecials	},  // redefined in lisp
	{	"%THROW_EXCEPTION",			Throw_Exception			},  // keep in kernel for now 03/13/00
	{	"VALUES",					Values					},	// redefined in lisp
	{	"ROOM",						Room					},  // redefined in lisp
	{	"DUMP-HEAP",				DumpHeap				},  // leave in kernel
	{	"MAKE-ARRAY",				Make_Array				},	// redefined in lisp
	{	"APPLY",					Apply					},	// redefined in lisp
	{	"RPLACA",					Rplaca					},	// redefined in lisp
	{	"RPLACD",					Rplacd					},	// redefined in lisp
	{	"FUNCTION-ENVIRONMENT",		Function_Environment	},	// redefined in lisp
	{	"FUNCTION-INFO-LIST",		Function_Info_List		},	// redefined in lisp
	{	"READ-CHAR",				Read_Char				},	// redefined in lisp
	{	"%READ-CHAR",				_Read_Char				},	// redefined in lisp
	{	"%READ-CHAR-WITH-ERROR",	_Read_Char_With_Error	},	// redefined in lisp
	{	"UNREAD-CHAR",				Unread_Char				},	// redefined in lisp
	{	"INT-CHAR",					Int_Char				},	// redefined in lisp
	{	"CHAR-INT",					Char_Int				},	// redefined in lisp
	{	"ELT",						Elt						},	// redefined in lisp
	{	"CHAR-UPCASE",				Char_Upcase				},	// redefined in lisp
	{	"CHAR-DOWNCASE",			Char_Downcase			},	// redefined in lisp
	{	"FUNCTIONP",				Functionp				},	// redefined in lisp
	{	"PACKAGE-HASH-INDEX",		Package_Hash_Index		},	// redefined in lisp
	{	"VECTOR-SLOT-INITIALIZED",	Vector_Slot_Initialized	},	// not used anywhere -04/25/06
	{	"STRING=",					String_Equal			},	// redefined in lisp
	{	"SETELT",					SetElt					},	// redefined in lisp
	{	"MAKE-SYMBOL",				Make_Symbol				},  // redefined in lisp
	{	"COERCE",					Coerce					},	// redefined in lisp
	{	"ALLOC-UVECTOR",			Alloc_Uvector			},	// redefined in lisp
	{	"ALLOC-BYTE-VECTOR",		Alloc_Byte_Vector		},	// not used 03/13/00
	{	"%CHARS-TO-FLOAT",			Chars_To_Float			},	// redefined in lisp	
	{	"%POP-SPECIAL-BINDINGS",	Pop_Special_Bindings	},	// redefined in lisp	
	{	"%PUSH-SPECIAL-BINDINGS",	Push_Special_Bindings	},	// redefined in lisp	
	{	"%ESTABLISH-SPECIAL-BINDINGS",	(LispFunc)establishSpecialBindings	}, // redefined in lisp0
	{	"%OUTPUT-CHAR",				_Output_Char			},	// redefined in lisp
	{	"%OUTPUT-CHARS",			_Output_Chars			},	// redefined in lisp
	{	"VECTORP",					Vectorp					},	// redefined in lisp
	{	"ARRAY-RANK",				Array_Rank				},	// redefined in lisp
	{	"ARRAY-DIMENSION",			Array_Dimension			},	// redefined in lisp
	{	"%UVECTOR-ADDRESS",			Uvector_Address			},	// redefined in lisp
	{	"%FLOAT-TO-STRING",			Float_To_String			},	// not used anywhere 03/13/00
	{	"ROW-MAJOR-AREF",			Row_Major_Aref			},	// redefined in lisp
	{	"(SETF ROW-MAJOR-AREF)",	Setf_Row_Major_Aref		},	// redefined in lisp
	{	"ARRAY-CELL-SIZE",			Array_Cell_Size			},	// not used 03/13/00
	{	"ARRAY-INITIALIZE-ELEMENT",	Array_Initialize_Element},	// redefined in lisp
	{	"ARRAY-INITIALIZE-CONTENTS",Array_Initialize_Contents},	// redefined in lisp
	{	"UNINITIALIZED-OBJECT-P",	Uninitialized_Object_P  },  // redefined in lisp
	{	"DISASSEMBLY-STATEMENT",	Disassembly_Statement	},  // redefined in lisp
	{	"EXECUTION-ADDRESS",		Execution_Address		},	// redefined in lisp
	{	"PRINT-FLOAT",				Print_Float				},  // redefined in lisp
	{	"STACK-TRACE",				Stack_Trace				},  // keep in kernel
	{	"ADDRESS-FIND-FUNCTION",	Address_Find_Function	},	// keep in kernel for now 03/13/00
	{	"%BIGNUM-BYTE",				Bignum_Byte				},	// not used anywhere 03/13/00
	{	"FLOAT",					Float					},	// redefined in lisp
	{	"FLOOR",					Floor					},	// redefined in lisp
	{	"CEILING",					Ceiling					},	// redefined in lisp
	{	"TRUNCATE",					Truncate				},	// redefined in lisp
	{	"ROUND",					Round					},	// redefined in lisp
	{	"COMPLEX",					Complex					},	// redefined in lisp
	{	"SQRT",						Sqrt					},	// redefined in lisp
	{	"ISQRT",					Isqrt					},	// redefined in lisp
	{	"EXP",						Exp						},	// redefined in lisp
	{	"EXPT",						Expt					},	// redefined in lisp
	{	"LOG",						Log						},	// redefined in lisp
	{	"SIN",						Sin						},	// redefined in lisp
	{	"COS",						Cos						},	// redefined in lisp
	{	"GCD",						Gcd						},	// redefined in lisp
	{	"UVECTOR-TYPE-BITS",		Uvector_Type_Bits		},	// redefined in lisp
	{	"TAG-BITS",					Tag_Bits				},	// redefined in lisp
	{	"TEST-FUNCTION",			Test_Function			},  // not used anywhere
	{	"PROTECT-STACK",			Protect_Stack			},
	{	"GET-OS-VERSION",			Get_OS_Version			},
	{	"HEAP-CAPACITY",			Heap_Capacity			},
	{	"HEAP-CURRENTLY-USED",		Heap_Currently_Used		},
	{	"JUMP-TABLE-CAPACITY",		Jump_Table_Capacity		},
	{	"JUMP-TABLE-USED",			Jump_Table_Used			},
	{	"SYMBOL-TABLE-CAPACITY",	Symbol_Table_Capacity	},
	{	"SYMBOL-TABLE-USED",		Symbol_Table_Used		},
	{	"%FMAKUNBOUND",				Fmakunbound				},	// redefined in lisp
	{	"REGISTER-FINALIZATION",	Register_Finalization	},	// redefined in lisp
	{	"UVECTOR-NUM-SLOTS",		Uvector_Num_Slots		},  // redefined in lisp
	{	"LOGXOR",					Logxor					},	// redefined in lisp
	{	"LOGIOR",					Logior					},	// redefined in lisp
	{	"LOGAND",					Logand					},	// redefined in lisp
	{	"LOGNOT",					Lognot					},	// redefined in lisp
	{	"LOGANDC1",					Logandc1				},	// redefined in lisp
	{	"LOGANDC2",					Logandc2				},	// redefined in lisp
	{	"LOGEQV",					Logeqv					},	// redefined in lisp
	{	"LOGNAND",					Lognand					},	// redefined in lisp
	{	"LOGNOR",					Lognor					},	// redefined in lisp
	{	"LOGORC1",					Logorc1					},	// redefined in lisp
	{	"LOGORC2",					Logorc2					},	// redefined in lisp
	{	"LISP-OBJECT-ID",			Lisp_object_id			},  // redefined in lisp
	{	"HASH-EQ-FUNCTION",			Hash_eq_function		},  // redefined in lisp
	{	"HASH-EQL-FUNCTION",		Hash_eql_function		},	// redefined in lisp
	{	"HASH-EQUAL-FUNCTION",		Hash_equal_function		},	// redefined in lisp
	{	"HASH-EQUALP-FUNCTION",		Hash_equalp_function	},	// redefined in lisp
	{	"FUNCALL-IGNORING-ERRORS",	Funcall_ignoring_errors	},	// redefined in lisp
	{	"GET-GC-EXEC-REGISTRY",		Get_gc_exec_registry	},  // not currently used anywhere
	{	"ADD-GC-EXEC-REGISTRY",		Add_gc_exec_registry	},	// not currently used anywhere
	{	"REMOVE-GC-EXEC-REGISTRY",	Remove_gc_exec_registry	},  // not currently used anywhere
	{	"COMPILE-FUNCTION-CALL-FORM", CompileFunctionCallForm },// redefined in lisp
	{	"UREF-SET",					Uref_Set				},	// redefined in lisp
	{	"UREF",						Uref					},	// redefined in lisp
	{	"CONSTANTP",				Constantp				},	// redefined in lisp
	{	"CREATE-COMPILED-FUNCTION",	Create_Compiled_Function},
	{	"LOAD-DLL",					Load_DLL				},  // redefined in lisp
	{	"UNLOAD-DLL",				Unload_DLL				},
	{	"GET-DLL-PROC-ADDRESS",		Get_DLL_Proc_Address	},
	{	"FOREIGNP",					Foreignp				},  // redefined in lisp
	{	"FOREIGN-HEAP-P",			Foreign_Heap_P			},
	{	"FOREIGN-PTR-TO-INT",		Foreign_Ptr_To_Int		},  // redefined in lisp
	{	"INT-TO-FOREIGN-PTR",		Int_To_Foreign_Ptr		},  // redefined in lisp
	{	"EDITOR-SET-MESSAGE",		Editor_Set_Message		},
	{	"EDITOR-SET-DEFAULT-MESSAGE", Editor_Set_Default_Message},
	{	"EDITOR-GET-MESSAGE",		Editor_Get_Message		},
//	{	"DISPLAY-VALUE",			Display_Value			},
	{	"CREATE-THREAD",			Create_Thread			},
	{	"COMPILE-SUB-FORM",			Compile_Sub_Form		},
	{	"REGISTER-UNTAGGED-VALUES",	Register_Untagged_Values},	// redefined in lisp
	{	"ALLOCATE-C-HEAP",			Allocate_C_Heap			},	// redefined in lisp
	{	"DEALLOCATE-C-HEAP",		Deallocate_C_Heap		},	// redefined in lisp
	{	"GET-UNIVERSAL-TIME",		Get_Universal_Time		},	// redefined in lisp
	{	"LOCAL-TIME-ZONE",			Local_Time_Zone			},	// redefined in lisp
	{	"GET-LOCAL-TIME",			Get_Local_Time			},
	{	"GET-SYSTEM-TIME",			Get_System_Time			},
	{	"SYSTEM-TIME-TO-FILE-TIME",	System_Time_To_File_Time},
	{	"FILE-TIME-TO-SYSTEM-TIME",	File_Time_To_System_Time},
	{	"BIGNUM-INTEGER-LENGTH",	Bignum_Integer_Length	},	// redefined in lisp
	{	"BIGNUM-SHIFT",				Bignum_Shift			},	// redefined in lisp
	{	"%FIXNUM-TO-BIGNUM",		(LispFunc)fixnumToBignum},	// not used anywhere 04/25/06
	{	"%CREATE-FUNC-TABLE-ENTRY", Create_Func_Table_Entry	},	// keep in kernel  03/13/00
	{	"%CREATE-VAR-TABLE-ENTRY",  Create_Var_Table_Entry	},	// keep in kernel  03/13/00
	{	"%CREATE-LISP-INTEGER",		(LispFunc)createLispInteger}, // TDE: replace with lisp code 03/13/00
	{	"%CREATE-UNSIGNED-LISP-INTEGER",(LispFunc)createUnsignedLispInteger}, // TDE: replace with lisp code 03/13/00
	{	"FIND-KEYWORD",				Find_Keyword			},
	{	"CONSOLE-OVERFLOW-FUNCTION",Console_Overflow_Function},
	{	"CONSOLE-UNDERFLOW-FUNCTION",Console_Underflow_Function},
	{	"FILE-OVERFLOW-FUNCTION",	File_Overflow_Function	},	// redefined in lisp
	{	"FILE-UNDERFLOW-FUNCTION",	File_Underflow_Function	},	// redefined in lisp
	{	"STRING-OVERFLOW-FUNCTION",	String_Overflow_Function},	// To Do!!
	{	"STRING-UNDERFLOW-FUNCTION",String_Underflow_Function},	// redefined in lisp
	{	"MAKE-WEAK-POINTER",		Make_Weak_Pointer		},
	{	"ADDRESS-FIND-FUNCTION-CALLBACK", Address_Find_Function_Callback }, // keep in kernel for now 03/13/00
	{	"PROCESS-EACH-HEAP-BLOCK",	Process_Each_Heap_Block	},
	{	"CONSOLE-INPUT-CHARS-AVAILABLE", Console_Input_Chars_Available	},
	{	"TYPEEXPAND-ALL",			Typeexpand_All			},	// redefined in lisp
	{	"LOOKUP-FTYPE",				Lookup_Ftype			},	// redefined in lisp
	{	"GET-APPLICATION-INSTANCE",	Get_Application_Instance},
	{	"GET-APPLICATION-MAIN-WINDOW",	Get_Application_Main_Window},
	{	"%LOAD-QV-REG",				(LispFunc)Load_QV_Reg	},	// TDE: replace with lisp code 03/13/00
	{	"CREATE-CALLBACK-THUNK",	Create_Callback_Thunk	},
	{	"ED",						Ed						},
	{	"DISPLAY-URL",				Display_URL				},
	{	"ADD-MENU-ITEM",			Add_Menu_Item			},	// not currently used anywhere 03/13/00
	{	"CORMANLISP-CLIENT-TYPE",	CormanLisp_Client_Type	},
	{	"ARRAY-TYPE",				Array_Type				},	// TDE: replace with lisp code 03/13/00
	{	"MOD-BIGNUMS",				Mod_Bignums				},		// debug function
	{	"DIVIDE-BIGNUMS",			Divide_Bignums			},		// debug function

	// read macros
	{	"%DOUBLEQUOTEMACRO",		doublequoteMacro		},  // only used during booting 03/13/00
	{	"%QUOTEMACRO",				quoteMacro				},	// only used during booting 03/13/00
	{	"%LEFTPARENMACRO",			leftparenMacro			},	// only used during booting 03/13/00
	{	"%RIGHTPARENMACRO",			rightparenMacro			},	// only used during booting 03/13/00
	{	"%COMMAMACRO",				commaMacro				},	// only used during booting 03/13/00
	{	"%SEMICOLONMACRO",			semicolonMacro			},	// only used during booting 08/18/01
	{	"%BACKQUOTEMACRO",			backquoteMacro			},	// only used during booting 03/13/00
	{	"%POUNDQUOTEMACRO",			poundQuoteMacro			},	// only used during booting 08/18/01
	{	"%POUNDLEFTPARENMACRO",		poundLeftParenMacro		},	// only used during booting 03/13/00
	{	"%POUNDBACKSLASHMACRO",		poundBackslashMacro		},	// only used during booting 03/13/00
	{	"%BRACKETEDCOMMENTMACRO",	bracketedCommentMacro	},	// redefined in lisp
	{	"%KERNEL-FUNCALL",			Funcall					},	// not used anywhere 03/13/00
	{	"%KERNEL-APPLY",			Apply					},	// not used anywhere 03/13/00
	{	"PROBE-FILE",				Probe_File				},	// redefined in lisp

	{	"%PLUS_EAX_EDX",			(LispFunc)Plus_EAX_EDX	},	// redefined in lisp
	{	"%MINUS_EAX_EDX",			(LispFunc)Minus_EAX_EDX	},	// redefined in lisp
	{	"%ADD-2",					(LispFunc)_Add			},	// not used anywhere 03/13/00
	{	"%SUBTRACT-2",				(LispFunc)_Subtract		},  // not used anywhere 03/13/00
	{	"%MULTIPLY-2",				(LispFunc)_Multiply		},	// not used anywhere 04/24/06
	{	"%DIVIDE-2",				(LispFunc)_Divide		},	// not used anywhere 03/13/00

	{   "ALLOCATE-FOREIGN-JUMP-TABLE-ENTRY", Allocate_Foreign_Jump_Table_Entry  }, //  TDE: replace with lisp code 03/13/00
	{   "CLEAR-FOREIGN-JUMP-TABLE",	Clear_Foreign_Jump_Table }, 
	{	"SYS-GLOBALS-ADDRESS",		Sys_Globals_Address		},
	{	"GET-CURRENT-THREAD-IDS",	Get_Current_Thread_IDs	},
	{	"GET-NUM-LISP-THREADS",		Get_Num_Lisp_Threads	},
	{	"THREAD-HANDLE",			Thread_Handle			},
	{	"TERMINATE-THREAD",			Terminate_Thread		},

	{	"ENTER-CRITICAL-SECTION",	Enter_Critical_Section	},
	{	"LEAVE-CRITICAL-SECTION",	Leave_Critical_Section	},
	{	"ALLOCATE-CRITICAL-SECTION",Allocate_Critical_Section }, // TDE: replace with lisp code 03/13/00
	{	"DEALLOCATE-CRITICAL-SECTION",Deallocate_Critical_Section },

	{	"COMPRESS-BYTES",			Compress_Bytes			},  // redefined in lisp
	{	"UNCOMPRESS-BYTES",			Uncompress_Bytes		},  // redefined in lisp
	{	"UNCOMPRESS-FOREIGN-BYTES",	Uncompress_Foreign_Bytes},  // redefined in lisp
	{	"%THROW-SYSTEM-EXCEPTION",	ThrowSystemException	},
	{	"INLINE-PROCLAIM-P",		Inline_Proclaim_P		},
	{	"%EXECUTE-FINALIZERS",		Execute_Finalizers		},	// called by garbage collector 03/13/00
	{	"ALLOC-CHAR-VECTOR",		Alloc_Char_Vector		},	// TDE: replace with lisp code 03/13/00
	{	"COMPILER-CHECK-ARGS-NUM",	Compiler_Check_Args_Num	},
	{	"COMPILER-CHECK-TYPES",		Compiler_Check_Types	},
	{	"COMPILER-FOLD-CONSTANTS",	Compiler_Fold_Constants	},
	{	"COMPILER-INLINE-FUNCTIONS",Compiler_Inline_Functions},
	{	"COMPILER-OPTIMIZE-TAIL-RECURSION",	Compiler_Optimize_Tail_Recursion	},
	{	"%LOAD-LOCAL-HEAP",			(LispFunc)LoadLocalHeap	},
	{	"LOOKUP-SETF-FUNCTION",		Lookup_Setf_Function	},
	{	"COMPILER-CHECK-KEY-ARGS",	Compiler_Check_Key_Args	},
	{	"INVALID-KEY-ARG",			InvalidKeyArgError		},
	{	"COMPRESS-FILE",			Compress_File			},
	{	"UNCOMPRESS-FILE",			Uncompress_File			},
	{	"EDITOR-REPLACE-SELECTION",	Editor_Replace_Selection},
	{	"GET-CALLBACK",				Get_Callback			},	// redefined in lisp (in ffi.lisp)
	{   "%HEAP_FAULT_HANDLER",		(LispFunc)Heap_Fault_Handler},	// not to be called directly from lisp
	{	"HEAP-USED",				Heap_Used				},
	{	"CONTAINING-HEAP",			Containing_Heap			},
	{	"REGISTRATION-INFO",		Registration_Info		},
    {   "CONSOLE-CHARS-AVAILABLE",  Console_Chars_Available },
	{   "GET-GC-ID",				Get_GC_ID				},	// get the garbage collection id
	{   "RESET-HASH-ID",			Reset_Hash_ID			},
	{	"SUSPEND-OTHER-THREADS",	Suspend_Other_Threads	},
	{	"RESUME-OTHER-THREADS",		Resume_Other_Threads	},
	{	"MEMORY-REPORT",			Memory_Report			},
	{	"%LISP-SHUTDOWN",			Lisp_Shutdown			},
	{	"%COMPRESS-FOREIGN-BYTES",  (LispFunc)compress	    },  // defined in Lisp via FFI
	{	"%UNCOMPRESS-FOREIGN-BYTES",(LispFunc)uncompress    },  // defined in Lisp via FFI
	{	"%COMPRESS-BOUND",          (LispFunc)compressBound },  // defined in Lisp via FFI
	{	"%ALLOC-VECTOR-TAGGED",		(LispFunc)LispAllocVectorTagged}, // already naked asm in C++, leave for now
	{	"UPDATE-JUMP-TABLE",		UpdateJumpTable			},
    {   "%UNASSEMBLE",              (LispFunc)unassemble    },  // defined in Lisp via FFI
    {   "%LOADLIBRARY",             (LispFunc)LoadLibraryA  },  // defined in Lisp via FFI
	{	"HARDWARE-GC",				Hardware_GC				},  // turn on or off hardware-assisted gc

    // these are just here for Lisp reporting purposes (so the names in stack dumps are known)
    { "LispCall0",                  (LispFunc)LispCall0     },
    { "LispCall1",                  (LispFunc)LispCall1     },
    { "LispCall2",                  (LispFunc)LispCall2     },
    { "LispCall3",                  (LispFunc)LispCall3     },
    { "LispCall4",                  (LispFunc)LispCall4     },
    { "LispCall5",                  (LispFunc)LispCall5     },
    { "LispCall6",                  (LispFunc)LispCall6     },
    { "LispCall7",                  (LispFunc)LispCall7     },
    { "LispCall8",                  (LispFunc)LispCall8     },
    { "LispLoop",                   (LispFunc)LispLoop      },
    { "lispmain",                   (LispFunc)lispmain      },
    { "consoleUnderflow",           (LispFunc)consoleUnderflow },
    { "garbageCollect",             (LispFunc)garbageCollect },

	// We need this built-in function to support callback in FFI on 64-bit versions of Windows
	// It seems one can not throw Access Violation Exceptions through the Windows API functions on 64 bit OSes.
	{ "%SAFECALL",                  (LispFunc)Safecall      },

	{ "%USER-HOMEDIR-NAMESTRING",   (LispFunc)User_HomeDir_Namestring }, // internal function to supply data for USER-HOMEDIR-PATHNAME

	// One needs the following functions to bootstrap Corman Lisp image
	{ "%CORMANLISP-DIRECTORY-NAMESTRING", (LispFunc)CormanLisp_Directory_Namestring }, // get Corman Lisp directory namestring
	{ "%CHANGE-DIRECTORY",          (LispFunc)Change_Directory },        // change working directory - it is needed during image building process.
	{ "%IMAGE-LOADS-COUNT",         (LispFunc)Image_Loads_Count }        // returns number of LOAD-IMAGE calls.
    // ----------------------------------------------------------
};
long sizeFunctTable = sizeof(functTable)/sizeof(FunctEntry);

char* specialOperatorTable[] =
{
	"BLOCK",
	"CATCH",
	"EVAL-WHEN",
	"FLET",
	"FUNCTION",
	"GO",
	"IF",
	"LABELS",
	"LET",
	"LET*",
	"LOAD-TIME-VALUE",
	"LOCALLY",
	"MACROLET",
	"MULTIPLE-VALUE-CALL",
	"MULTIPLE-VALUE-PROG1",
	"PROGN",
	"PROGV",
	"QUOTE",
	"RETURN-FROM",
	"SETQ",
	"SYMBOL-MACROLET",
	"TAGBODY",
	"THE",
	"THROW",
	"UNWIND-PROTECT",
	"GET-CURRENT-ENVIRONMENT",		// non-standard
	"CAPTURE-COMPILER-ENVIRONMENT"	// non-standard
};

static long sizeSpecialOperatorTable = sizeof(specialOperatorTable)/sizeof(char*);

static void addFunct(const char* name, LispFunc func)
{
	LispObj s = 0;
	s = findSymbol(name);
	setSymbolFunction(s, kernelFunctionNode(func), FUNCTION);
}	

static void addSpecialOperator(const char* name)
{
	LispObj s = 0;
	s = findSymbol(name);
	setSpecialOperator(s);
}	

void initKernelFunctions()
{
	long i = 0;
	for (i = 0; i < sizeFunctTable; i++)
		addFunct(functTable[i].functName, functTable[i].functAddr);
	for (i = 0; i < sizeSpecialOperatorTable; i++)
		addSpecialOperator(specialOperatorTable[i]);
}

extern char CormanLispDirectoryBuffer[];
extern CriticalSection TQCriticalSection;

static void updateKernelFunctions()
{
	long i = 0;
	LispObj s = 0;
	LispObj f = 0;
	HANDLE appHandle = 0;
	DWORD numChars = 0;

	for (i = 0; i < sizeFunctTable; i++)
	{
		s = findSymbol(functTable[i].functName);
		f = symbolFunction(s);
		if (isKFunction(f))
		{
			updateSymbolFunctionAddress(s, functTable[i].functAddr);
		}
	}

	// update global QV pointer
	f = symbolValue(SYSTEM_INTERNALS);
	UVECTOR(f)[FOREIGN_PTR] = (LispObj)QV;

	// initialize *cormanlisp-directory* variable
	if (CormanLispServer)
		CormanLispServer->GetAppInstance((HINSTANCE*)&appHandle);
	numChars = GetModuleFileName((HINSTANCE)appHandle, CormanLispDirectoryBuffer, _MAX_PATH);
	for (i = numChars - 1; i >= 0; i--)
	{
		if (CormanLispDirectoryBuffer[i] == '/' || CormanLispDirectoryBuffer[i] == '\\')
			break;
	}
	if (i >= 0)
		CormanLispDirectoryBuffer[i + 1] = 0;		// we only want the directory
	
//	_getcwd(CormanLispDirectoryBuffer, _MAX_PATH);
	setSymbolValue(CORMANLISP_DIRECTORY, stringNode(CormanLispDirectoryBuffer));
	setSymbolValue(CORMANLISP_SERVER_DIRECTORY, stringNode(CormanLispServerDirectory));

    // initialize these here because they may differ from the addresses when the
    // lisp heap was saved
	f = foreignNode();
	UVECTOR(f)[FOREIGN_PTR] = (LispObj)&GCCriticalSection.m_sect;
	setSymbolValue(GC_CRITICAL_SECTION, f);
	f = foreignNode();
	UVECTOR(f)[FOREIGN_PTR] = (LispObj)&TQCriticalSection.m_sect;
	setSymbolValue(TQ_CRITICAL_SECTION, f);
	f = foreignNode();
	UVECTOR(f)[FOREIGN_PTR] = (LispObj)PageTable;
	setSymbolValue(PAGE_TABLE, f);
	f = foreignNode();
	UVECTOR(f)[FOREIGN_PTR] = (LispObj)gDisassemblyOutputBuf;
	setSymbolValue(DISASSEMBLY_OUTPUT_BUF, f);
}

LispObj allRequiredArgs(LispObj lambda)
{
	LispObj x = 0;
	while (isCons(lambda))
	{
		x = CAR(lambda);
		if (x == LAMBDA_OPTIONAL || x == LAMBDA_REST || x == LAMBDA_KEY)
			return NIL;
		lambda = CDR(lambda);
	}
	return T;
}

LispObj getRequiredArgs(LispObj lambda)
{
	LispObj result = NIL;
	LispObj x = 0;
	while (isCons(lambda))
	{
		x = CAR(lambda);
		if (x == LAMBDA_OPTIONAL || x == LAMBDA_REST || x == LAMBDA_AUX
				|| x == LAMBDA_ALLOW_OTHER_KEYS)
			return result;
		result = cons(x, result);
		lambda = CDR(lambda);
	}
	return result;
}

LispFunction(Stack_Trace)
{
	LISP_FUNC_BEGIN(0);
	LispObj framesList = NIL;
	LispObj* basePointer = 0;
	LispObj retAddress = 0;
	LispObj func = 0;
	LispObj funcInfo = NIL;
	LispObj funcName = 0;
	LispObj funcLambda = 0;
	LispObj p = 0;
	LispObj argList = 0;
	LispObj numArgs = 0;
	LispObj requiredArgs = 0;
	LispObj sym = 0;
	LispObj arg = 0;
	long lexCount = 0;

	enum { required, optional, rest, key, aux } state = required;

	long i = 0;
	FunctEntry* funcEntry = 0;
	long count = 30;  // maximum of 30 frames
	LispObj* stackStart = ((ThreadRecord*)TlsGetValue(Thread_Index))->stackStart;

	__asm mov basePointer, ebp

	while (basePointer < stackStart)
	{
		retAddress = createLispInteger(basePointer[1]);
		func = addressFindFunction(retAddress);
		funcInfo = NIL;
		if (isFunction(func))
		{
			if (isKFunction(func))
			{
				funcEntry = functTable;
				for (i = 0; i < sizeFunctTable; i++)
				{
					if (((LispObj)(funcEntry->functAddr)) == UVECTOR(func)[FUNCTION_ADDRESS])
					{
						funcInfo = list(FUNCTION_NAME, findSymbol(funcEntry->functName), END_LIST);
						break;
					}
					funcEntry++;
				}
			}
			else
				funcInfo = LispCall1(Function_Info_List, func);
		}
		funcName = 0;
		funcLambda = NIL;
		argList = NIL;
		p = funcInfo;
		requiredArgs = NIL;
		lexCount = 0;
		while (isCons(p))
		{
			if (CAR(p) == FUNCTION_NAME)
				funcName = CAR(CDR(p));
			else if (CAR(p) == LAMBDA_LIST)
				funcLambda = CAR(CDR(p));
			p = CDR(CDR(p));
		}

		if (allRequiredArgs(funcLambda) != NIL)
		{
			requiredArgs = getRequiredArgs(funcLambda);
			numArgs = wrapInteger(listLength(requiredArgs));
			for (i = 0; i < integer(numArgs); i++)
			{
				arg = ((unsigned long*)*basePointer)[2 + i];
				sym = CAR(requiredArgs);
				argList = cons(arg, argList);
				requiredArgs = CDR(requiredArgs);
			}
		}
		else
		{	// handle functions with &optional, &rest or &key params
			// these all push ecx on the stack, and we need that for the
			// arg count
			numArgs = wrapInteger(((unsigned long*)*basePointer)[-2]);
			i = 0;
			state = required;
			while (/* isCons(funcLambda) && */ numArgs > 0)
			{
				sym = CAR(funcLambda);
				if (sym == LAMBDA_OPTIONAL)
					state = optional;
				else
				if (sym == LAMBDA_REST)
					state = rest;
				else
				if (sym == LAMBDA_KEY)
					state = key;
				else
				if (sym == LAMBDA_AUX)
					state = aux;
				else
				{
					switch (state)
					{
						case required:
							arg = ((LispObj*)*basePointer)[2 + integer(numArgs) - 1];
							numArgs -= wrapInteger(1);
							argList = cons(arg, argList);
							break;
						case optional:
						case rest:
						case key:
							arg = ((LispObj*)*basePointer)[2 + integer(numArgs) - 1];
							numArgs -= wrapInteger(1);
							argList = cons(arg, argList);
//							arg = ((LispObj*)*basePointer)[-2 - lexCount - 1];
//							lexCount++;
//							argList = cons(sym, cons(arg, argList));
							break;
						case aux:
							break;
					}
				}
				if (isCons(funcLambda))
					funcLambda = CDR(funcLambda);
			}
			argList = Cnreverse(argList);
		}

		if (funcName)
			framesList = cons(cons(funcName, argList), framesList);
		else
		{
			if (func && func != NIL)
				framesList = cons(cons(func, argList), framesList);
		}
		basePointer = (unsigned long*)*basePointer;
		count--;
		if (count <= 0)
			break;
        if (isFunction(func) && (symbolValue(TOP_LEVEL) == func ||
            (funcName && symbolValue(TOP_LEVEL) == symbolFunction(funcName))))
            break; 
	}
	ret = Cnreverse(framesList);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Memory_Report)
{
	LISP_FUNC_BEGIN(0);
	WriteMemoryReportTask((void*)(-2), 0);
	ret = NIL;
	LISP_FUNC_RETURN(ret);	
}

extern long expandLineFeedsIntoBuffer(LISP_CHAR* src, long numChars, LISP_CHAR* buffer);

LispFunction(Lisp_Shutdown)
{
	LISP_FUNC_BEGIN(1);
	long numBytes = 0;
	LISP_CHAR* CharBuffer = 0;
	byte* ByteOutputBuffer = 0;

	LispObj message = LISP_ARG(0);
	checkString(message);
	LispObj len = vectorLength(message);
	
	CharBuffer = new LISP_CHAR[(integer(len) + 1) * 2];
	numBytes = expandLineFeedsIntoBuffer(charArrayStart(message),
		integer(len), CharBuffer);

	if (CormanLispServer)
	{
		// convert from 16-bit to 8-bit chars
		ByteOutputBuffer = (byte*)CharBuffer;
		for (int i = 0; i < numBytes; i++)
			ByteOutputBuffer[i] = ByteOutputBuffer[i * 2];	// convert from 16-bit to 8-bit chars
		CormanLispServer->LispShutdown((char*)ByteOutputBuffer, numBytes);
	}
	delete [] CharBuffer;
	ret = NIL;
	LISP_FUNC_RETURN(ret);	
}


// We need this to implement %SAFECALL primitive to support callbacks on 64 bit versions of Windows.
volatile static LispObj doSafecall(LispObj func)
{
	LispObj res = NIL;
	__try
	{
		res = LispCall1(Funcall, func);
	}
	__except (handleStructuredException(GetExceptionCode(), GetExceptionInformation()))
	{

	}
	return res;
}

LispFunction(Safecall)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);
	checkFunction(obj);

	ret = doSafecall(obj);

	LISP_FUNC_RETURN(ret);
}

LispFunction(User_HomeDir_Namestring)
{
	LISP_FUNC_BEGIN(0);
	ret = stringNode(CurrentUserInfo->GetProfileDirectory());
	LISP_FUNC_RETURN(ret);
}

LispFunction(CormanLisp_Directory_Namestring)
{
	LISP_FUNC_BEGIN(0);
	ret = stringNode(CormanLispDirectoryBuffer);
	LISP_FUNC_RETURN(ret);
}

LispFunction(Change_Directory)
{
	LISP_FUNC_BEGIN(1);
	LispObj obj = LISP_ARG(0);
	checkString(obj);

	ret = NIL;
	if (SetCurrentDirectoryA((char*)byteArrayStart(nullTerminate(obj))) == TRUE)
	{
		ret = obj;
	}
	LISP_FUNC_RETURN(ret);
}

LispFunction(Image_Loads_Count)
{
	LISP_FUNC_BEGIN(0);

	ret = createLispInteger(getImageLoadsCount());

	LISP_FUNC_RETURN(ret);
}
