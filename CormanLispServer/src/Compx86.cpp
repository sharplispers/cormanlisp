//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		compx86.cpp
//		Contents:	80x86 bootstrap compiler for Corman Lisp
//		History:	6/19/96  RGC  Created.
//					11/16/99 RGC  Lexical functions now are given priority
//								  over inlined ones.
//					1/18/00  RGC  Compiler undefined function warnings are now
//								  appended to *undefined-functions* in some
//								  circumstances.
//					2/27/01  RGC  Modified code generation of UNWIND-PROTECT
//								  to correctly handle return from the cleanup
//								  clauses via RETURN or GO.
//					6/13/01  RGC  compileLambdaExpression() skips declarations when
//								  searching for the first block (for the name).
//					7/29/01  RGC  Fixed a problem with LET* and special variable
//								  bindings that would show up if the initializer of
//								  a variable caused a non-local exit.
//								  Fixed a problem with THROW and the number of returned values.
//					10/10/01 RGC  Fixed a problem with OPTIMIZE declarations.
//								  Added support for LOCALLY, LOAD-TIME-VALUE.
//                  02/27/03 RGC  Fixed incorrect implementation of LOAD-TIME-VALUE.
//					09/21/03 RGC  Fixed a bug in the compiler when multiple blocks with the same 
//                                name are used in a function.
//                  
//
//
#include "stdafx.h"
#include <setjmp.h>
#include <assert.h>

#include "generic.h"
#include "lisp.h"
#include "compx86.h"
#include "lispmath.h"

typedef LispObj LispObj;

static int ExpandMacrosInline = 1;

#define CURRENT_IP		(CBlength())
#define CURRENT_IP_RAW	(integer(CBlength()))


// static functions
static LispObj compileList(LispObj, LispObj, LispObj resultType);
static LispObj compileSymbol(LispObj, LispObj, LispObj resultType);
static LispObj compileSpecialOperator(LispObj, LispObj, LispObj resultType);
static LispObj compileFunctionCallForm(LispObj, LispObj);
static LispObj compileFunctionExpressionForm(LispObj, LispObj, LispObj resultType);
static LispObj compileLiteralForm(LispObj, LispObj, LispObj resultType);
static LispObj compileQuoteForm(LispObj, LispObj);
static LispObj compileSetqForm(LispObj, LispObj);
static LispObj compileFunctionSpecialOperator(LispObj, LispObj);
static LispObj compileBlockForm(LispObj, LispObj);
static LispObj compilePrognForm(LispObj, LispObj);
static LispObj compileNil(LispObj);
static LispObj compileIfForm(LispObj, LispObj);
static LispObj compileLetForm(LispObj, LispObj);
static LispObj compileFletForm(LispObj, LispObj);
static LispObj compileLabelsForm(LispObj, LispObj);
static LispObj compileLetstarForm(LispObj, LispObj);
static LispObj compileLoadTimeValueForm(LispObj, LispObj);
static LispObj compileLocallyForm(LispObj, LispObj);
static LispObj compileTagbodyForm(LispObj, LispObj);
static LispObj compileGoForm(LispObj, LispObj);
static LispObj compileReturnFromForm(LispObj, LispObj);
static LispObj compilePlusFunctionCall(LispObj, LispObj, LispObj resultType);
static LispObj compileMinusFunctionCall(LispObj, LispObj, LispObj resultType);
static LispObj compileNumericEqualFunctionCall(LispObj, LispObj);
static LispObj compileNumericCompareFunctionCall(LispObj, LispObj);
static LispObj compileConsFunctionCall(LispObj, LispObj);
static LispObj compileEqFunctionCall(LispObj, LispObj);
static LispObj compileCarFunctionCall(LispObj, LispObj);
static LispObj compileCdrFunctionCall(LispObj, LispObj);
static LispObj compileNullFunctionCall(LispObj, LispObj);
static LispObj compileArefFunctionCall(LispObj, LispObj);
static LispObj compileSetfArefFunctionCall(LispObj, LispObj);
static LispObj compileUrefFunctionCall(LispObj, LispObj);
static LispObj compileUrefSetFunctionCall(LispObj, LispObj);
static LispObj compileCatchForm(LispObj, LispObj);
static LispObj compileThrowForm(LispObj, LispObj);
static LispObj compileUnwindProtectForm(LispObj, LispObj);
static LispObj compileMultipleValueCallForm(LispObj, LispObj);
static LispObj compileEvalWhenForm(LispObj, LispObj);
static LispObj compileMultipleValueProg1Form(LispObj, LispObj);
static LispObj compileTheForm(LispObj, LispObj);
static LispObj compileCaptureCompilerEnvironmentForm(LispObj, LispObj);

static LispObj findLambdas(LispObj x);
static LispObj referencedByEmbeddedLambdas(LispObj sym);
static void compileVariableHeapBindings(LispObj newVars);
static void compileFunctionHeapBindings(LispObj newVars);

static void prolog(long envSize);
static void epilog(long envSize);
static void compileLambdaProlog(long envSize, LispObj needEBX, LispObj saveECX);
static void compileLambdaEpilog(LispObj needEBX);
static void compileArgsCheck(long num);
static void compileArgsRangeCheck(long low, long high);

static LispObj findRequiredArgs(LispObj lambdaList);
static LispObj findOptionalArgs(LispObj lambdaList);
static LispObj findRestArgs(LispObj lambdaList);
static LispObj findKeyArgs(LispObj lambdaList);
static LispObj findAuxArgs(LispObj lambdaList);
static LispObj findAllowOtherKeys(LispObj lambdaList);
static LispObj compileLambdaRequiredArgs(LispObj args, LispObj newVars, LispObj useBPOffset);
static LispObj compileLambdaOptionalArgs(LispObj args, LispObj newVars, LispObj argIndex);
static LispObj compileLambdaRestArgs(LispObj args, LispObj newVars, LispObj argIndex);
static LispObj compileLambdaKeyArgs(LispObj args, LispObj newVars, LispObj argIndex, LispObj allowOtherKeys);
static LispObj compileLambdaAuxArgs(LispObj args, LispObj newVars);
static int isConstantObject(LispObj x);
static LispObj findLexFunction(LispObj sym);
static void restoreOptimizeDeclarationBindings(LispObj bindings);
static LispObj processOptimizeDeclarations(LispObj declareForms);
static LispObj processSpecialDeclarations(LispObj declareForms);
static LispObj processTypeDeclarations(LispObj declareForms);
static LispObj processDynamicExtentDeclarations(LispObj declareForms);
static void pushTypeDeclaration(LispObj variable, LispObj type);
static void popTypeDeclarations(LispObj numVars);
static void pushDEDeclaration(LispObj variable);
static void popDEDeclarations(LispObj numVars);
static LispObj isSymbolTypeSpecifier(LispObj symbol);
static LispObj findVariableType(LispObj variable);
static LispObj isDynamicExtent(LispObj var);
static LispObj findFunctionType(LispObj func);
static LispObj findEmbeddedReturnForms(LispObj sym, LispObj form, LispObj labels);
static LispObj findEmbeddedGoForms(LispObj sym, LispObj form);
static LispObj findBlockIncludingOuterLambdas(LispObj label);
static LispObj findTagbodyTag(LispObj tag);
static LispObj findTagIncludingOuterLambdas(LispObj tag);
static LispObj findBlockLabels(LispObj outer, LispObj target, LispObj labels);

static LispObj compileLexicalEntity(LispObj sym, LispObj dest, LispObj type);
static LispObj compileLexicalSlot(LispObj sym, LispObj dest, LispObj type);
static LispObj compileCurrentEnvironment(LispObj x, LispObj dest);
static void addCompilerFrameInfo(LispObj name, LispObj base, LispObj offset, LispObj indirect);
static LispObj processIgnoreDeclarations(LispObj declareForms);
static LispObj findVariableRecord(LispObj sym);
static LispObj compilerCheckTypes();
static LispObj compilerCheckArgsNum();
static LispObj compilerFoldConstants();
static LispObj compilerInlineFunctions();
static LispObj compilerOptimizeTailRecursion();
static LispObj compilerCheckKeyArgs();

#define CODE_BUFFER_INFO					1
#define CODE_BUFFER_CBUF					2
#define CODE_BUFFER_REFS					3
#define CODE_BUFFER_CODE_INDEX				4
#define CODE_BUFFER_REF_INDEX				5
#define CODE_BUFFER_DYNAMIC_ENV_SIZE		6
#define CODE_BUFFER_HEAP_ENV_SIZE			7
#define CODE_BUFFER_STACK_INDEX				8
#define CODE_BUFFER_MAX_CODE_BYTES			9
#define CODE_BUFFER_MAX_REFS				10
#define CODE_BUFFER_SIZE					11

#pragma warning (disable:4127)				// conditional expression is constant
#pragma warning (disable:4244)				// '=' : conversion from 'int' to 'unsigned char', possible loss of data

void CBaddLong(unsigned long b)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	long index = integer(UVECTOR(buf)[CODE_BUFFER_CODE_INDEX]);
	byteArrayStart(cbuf)[index++] = (byte)(b & 0xff); 
	byteArrayStart(cbuf)[index++] = (byte)((b >> 8) & 0xff); 
	byteArrayStart(cbuf)[index++] = (byte)((b >> 16) & 0xff); 
	byteArrayStart(cbuf)[index++] = (byte)((b >> 24) & 0xff); 
	UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] += wrapInteger(4);
	if (UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] >= UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES])
		CBgrowCode();
}

void CBaddByte(LispObj a)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	long index = integer(UVECTOR(buf)[CODE_BUFFER_CODE_INDEX]);
	byteArrayStart(cbuf)[index] = (byte)(char)(integer(a)); 
	UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] += wrapInteger(1);
	if (UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] >= UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES])
		CBgrowCode();
}

void CBaddBytes(LispObj a, LispObj b)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	long index = integer(UVECTOR(buf)[CODE_BUFFER_CODE_INDEX]);
	byteArrayStart(cbuf)[index++] = (byte)(char)(integer(a)); 
	byteArrayStart(cbuf)[index++] = (byte)(char)(integer(b)); 
	UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] += wrapInteger(2);
	if (UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] >= UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES])
		CBgrowCode();
}

void CBaddBytes(LispObj a, LispObj b, LispObj c)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	long index = integer(UVECTOR(buf)[CODE_BUFFER_CODE_INDEX]);
	byteArrayStart(cbuf)[index++] = (byte)(char)(integer(a)); 
	byteArrayStart(cbuf)[index++] = (byte)(char)(integer(b)); 
	byteArrayStart(cbuf)[index++] = (byte)(char)(integer(c)); 
	UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] += wrapInteger(3);
	if (UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] >= UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES])
		CBgrowCode();
}

void CBaddLongVal(LispObj a)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	long index = integer(UVECTOR(buf)[CODE_BUFFER_CODE_INDEX]);
	long b = integer(a);
	byteArrayStart(cbuf)[index++] = (byte)(b & 0xff);
	byteArrayStart(cbuf)[index++] = (byte)((b >> 8) & 0xff);	
	byteArrayStart(cbuf)[index++] = (byte)((b >> 16) & 0xff); 
	byteArrayStart(cbuf)[index++] = (byte)((b >> 24) & 0xff); 
	UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] += wrapInteger(4);
	if (UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] >= UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES])
		CBgrowCode();
}

void CBaddObject(LispObj b)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	long index = integer(UVECTOR(buf)[CODE_BUFFER_CODE_INDEX]);
	byteArrayStart(cbuf)[index++] = (byte)(b & 0xff);
	byteArrayStart(cbuf)[index++] = (byte)((b >> 8) & 0xff);	
	byteArrayStart(cbuf)[index++] = (byte)((b >> 16) & 0xff); 
	byteArrayStart(cbuf)[index++] = (byte)((b >> 24) & 0xff); 
	UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] += wrapInteger(4);
	if (UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] >= UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES])
		CBgrowCode();
}

void CBsetByte(LispObj index, LispObj b)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	byteArrayStart(cbuf)[integer(index)] = (byte)(integer(b) & 0xff); 
}

void CBsetObject(LispObj index, LispObj x)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	long i = integer(index);
	byteArrayStart(cbuf)[i] = (byte)(x & 0xff); 
	byteArrayStart(cbuf)[i + 1] = (byte)((x >> 8) & 0xff); 
	byteArrayStart(cbuf)[i + 2] = (byte)((x >> 16) & 0xff); 
	byteArrayStart(cbuf)[i + 3] = (byte)((x >> 24) & 0xff); 
}

void CBsetLong(LispObj index, LispObj n)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	long i = integer(index);
	long x = integer(n);
	byteArrayStart(cbuf)[i] = (byte)(x & 0xff); 
	byteArrayStart(cbuf)[i + 1] = (byte)((x >> 8) & 0xff); 
	byteArrayStart(cbuf)[i + 2] = (byte)((x >> 16) & 0xff); 
	byteArrayStart(cbuf)[i + 3] = (byte)((x >> 24) & 0xff); 
}

void CBref(LispObj obj, LispObj pos)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
 	LispObj refs = UVECTOR(buf)[CODE_BUFFER_REFS];
	long refIndex = integer(UVECTOR(buf)[CODE_BUFFER_REF_INDEX]);
	arrayStart(refs)[refIndex++] = obj;
	arrayStart(refs)[refIndex++] = pos;
	UVECTOR(buf)[CODE_BUFFER_REF_INDEX] += wrapInteger(2);
	if (UVECTOR(buf)[CODE_BUFFER_REF_INDEX] >= UVECTOR(buf)[CODE_BUFFER_MAX_REFS])
		CBgrowRefs();
}

// Return the position (in the code) of the last stored literal reference, as a wrapped
// integer.
LispObj CBLastRefPos()
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
 	LispObj refs = UVECTOR(buf)[CODE_BUFFER_REFS];
	long refIndex = integer(UVECTOR(buf)[CODE_BUFFER_REF_INDEX]);
    return arrayStart(refs)[refIndex - 1];
}

void CBaddJumpTableRef(LispObj sym)
{
	if (symbolValue(COMPILER_COLLECT_JUMP_TABLE_REFS) != NIL)
		setSymbolValue(CODE_JUMP_TABLE_REFS, 
			cons(wrapInteger(CURRENT_IP_RAW - 4),
				cons(sym,
					symbolValue(CODE_JUMP_TABLE_REFS))));
}

void CBaddEnvTableRef(LispObj sym)
{
	if (symbolValue(COMPILER_COLLECT_JUMP_TABLE_REFS) != NIL)
		setSymbolValue(CODE_ENV_TABLE_REFS, 
			cons(wrapInteger(CURRENT_IP_RAW - 4),
				cons(sym,
					symbolValue(CODE_ENV_TABLE_REFS))));
}

void CBaddVarTableRef(LispObj sym)
{
	if (symbolValue(COMPILER_COLLECT_VAR_TABLE_REFS) != NIL)
		setSymbolValue(CODE_VAR_TABLE_REFS, 
			cons(wrapInteger(CURRENT_IP_RAW - 4),
				cons(sym,
					symbolValue(CODE_VAR_TABLE_REFS))));
}

//
//	Returns offset (from EBP) of allocated block
//
LispObj CBincDynamicEnvSize(LispObj size)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	UVECTOR(buf)[CODE_BUFFER_DYNAMIC_ENV_SIZE] += size;
	return wrapInteger(-4) - UVECTOR(buf)[CODE_BUFFER_DYNAMIC_ENV_SIZE];
}

// add lexical variable binding
void CBaddStackVar(LispObj sym, LispObj place)
{
	CBpushCleanup(list(LET, sym, place, 0, END_LIST));
}

// add lexical function binding
void CBaddStackFunction(LispObj sym, LispObj place)
{
	CBpushCleanup(list(FLET, sym, place, 0, END_LIST));
}

void CBaddHeapVar(LispObj sym, LispObj place)
{
	CBpushCleanup(list(LET, sym, place, 0, END_LIST));
}

void CBaddLabels(LispObj labels)
{
	CBpushCleanup(list(TAGBODY, labels, CBstackIndex(), END_LIST));
}

void CBaddBlock(LispObj block)
{
	CBpushCleanup(list(BLOCK, block, CBstackIndex(), END_LIST));
}

// add a lambda marker
void CBaddLambda()
{
	CBpushCleanup(list(LAMBDA, END_LIST));
}

void CBpushCleanup(LispObj rec)
{
	setSymbolValue(COMPILER_CLEANUPS, cons(rec, CBcleanups()));
}

// pops and returns most recently added cleanup form
LispObj CBpopCleanup()
{
	LispObj form = 0;
	LispObj temp = CBcleanups();
	LispObj line = 0;
	assert(isCons(temp));
	form = CAR(temp);
	setSymbolValue(COMPILER_CLEANUPS, CDR(temp));
	assert(isCons(form));
	if (CAR(form) == LET && CAR(CDR(CDR(CDR(form)))) == 0)
	{
		if (symbolValue(COMPILER_WARN_ON_UNUSED_VAR) != NIL)
		{
			// warn of unused variable
			if (symbolValue(SOURCE_FILE) == NIL)
			{
				if (symbolValue(COMPILER_FUNCTION_NAME) == NIL)
					LispCall3(Funcall, WARN,
						stringNode("Unused variable ~A in anonymous function"),
						CAR(CDR(form)));
				else
					LispCall4(Funcall, WARN,
						stringNode("Unused variable ~A in function ~S"),
						CAR(CDR(form)), symbolValue(COMPILER_FUNCTION_NAME));
			}
			else
			{
				// warn of unused variable
				line = symbolValue(SOURCE_LINE);
				if (isFixnum(line))
					line += wrapInteger(1);	// 1-based offset for message
				LispCall6(Funcall, WARN,
					stringNode("Unused variable ~A in function ~S, File ~A, line ~A"),
					CAR(CDR(form)), symbolValue(COMPILER_FUNCTION_NAME),
					symbolValue(SOURCE_FILE),
					line);
			}
		}
	}
	if (CAR(form) == FLET && CAR(CDR(CDR(CDR(form)))) == 0)
	{
		// warn of unused local function
	}
	return form;
}

// returns whole list
LispObj CBcleanups()
{
	return symbolValue(COMPILER_CLEANUPS);
}

// this must not trigger a garbage collection!
void CBinsertCode(LispObj position, void* b, LispObj numBytes)
{
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
 	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
 	LispObj refs = UVECTOR(buf)[CODE_BUFFER_REFS];
	LispObj jumpTableRefs = symbolValue(CODE_JUMP_TABLE_REFS);
	LispObj varTableRefs = symbolValue(CODE_VAR_TABLE_REFS);
	LispObj envTableRefs = symbolValue(CODE_ENV_TABLE_REFS);
	byte* code = byteArrayStart(cbuf);
	LispObj* r = arrayStart(refs);
	LispObj t1 = 0;
	long i = 0;
	long refIndex = integer(UVECTOR(buf)[CODE_BUFFER_REF_INDEX]);
	long pos = integer(position);
	long bytes = integer(numBytes);

	while (UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] + wrapInteger(numBytes) 
			>= UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES])
	{
		r = 0;
		code = 0;
		CBgrowCode();
 		cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
 		refs = UVECTOR(buf)[CODE_BUFFER_REFS];
		code = byteArrayStart(cbuf);
		r = arrayStart(refs);
	}

	memmove(code + pos + bytes, code + pos, integer(CBlength()) - pos);
	memmove(code + pos, b, bytes);
	UVECTOR(buf)[CODE_BUFFER_CODE_INDEX] += numBytes;

	// now update references
	for (i = 0; i < refIndex; i+=2)
	{
		if (integer(r[i + 1]) >= pos)
			r[i + 1] += numBytes;
	}

    // if any LOAD-TIME-VALUE forms were compiled, add those here
    t1 = symbolValue(LOAD_TIME_VALUES);
    while (isCons(t1))
	{
		if (integer(CAR(CDR(t1))) >= pos)
			CAR(CDR(t1)) += numBytes;
		t1 = CDR(CDR(t1));
	}

	// update jump table references
	while (isCons(jumpTableRefs))
	{
		if (integer(CAR(jumpTableRefs)) >= pos)
			CAR(jumpTableRefs) += numBytes;
		jumpTableRefs = CDR(CDR(jumpTableRefs));
	}

	// update var table references
	while (isCons(varTableRefs))
	{
		if (integer(CAR(varTableRefs)) >= pos)
			CAR(varTableRefs) += numBytes;
		varTableRefs = CDR(CDR(varTableRefs));
	}

	// update env table references
	while (isCons(envTableRefs))
	{
		if (integer(CAR(envTableRefs)) >= pos)
			CAR(envTableRefs) += numBytes;
		envTableRefs = CDR(CDR(envTableRefs));
	}

	code = 0;
	r = 0;
}

void CBincStackIndex(LispObj num)
{ 
	UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_STACK_INDEX] += num; 
}

void CBdecStackIndex(LispObj num)
{ 
	UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_STACK_INDEX] -= num; 
}

void CBsetStackIndex(LispObj num)
{ 
	UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_STACK_INDEX] = num; 
}

LispObj CBstackIndex()
{ 
	return UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_STACK_INDEX]; 
}

void CBgrowCode()
{
	LispObj bv = 0;
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
 	LispObj cbuf = UVECTOR(buf)[CODE_BUFFER_CBUF];
	UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES] *= 2;
	bv = byteVector(UVECTOR(buf)[CODE_BUFFER_MAX_CODE_BYTES] + wrapInteger(16));
	memcpy(byteArrayStart(bv), byteArrayStart(cbuf), integer(UVECTOR(buf)[CODE_BUFFER_CODE_INDEX]));
	UVECTOR(buf)[CODE_BUFFER_CBUF] = bv;
}

void CBgrowRefs()
{
	LispObj v = 0;
	LispObj buf = symbolValue(COMPILER_CODE_BUFFER);
	LispObj refs = UVECTOR(buf)[CODE_BUFFER_REFS];
	UVECTOR(buf)[CODE_BUFFER_MAX_REFS] *= 2;
	v = vectorNode(UVECTOR(buf)[CODE_BUFFER_MAX_REFS] + wrapInteger(8));
	memcpy(arrayStart(v), arrayStart(refs), integer(UVECTOR(buf)[CODE_BUFFER_REF_INDEX]) * sizeof(LispObj));
	UVECTOR(buf)[CODE_BUFFER_REFS] = v;
}

LispObj CBcode() { return UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_CBUF]; }
LispObj CBreferences() { return UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_REFS]; }
LispObj CBlength() { return UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_CODE_INDEX]; } 
LispObj CBnumReferences() { return UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_REF_INDEX] / 2; }
LispObj CBdynamicEnvSize()	{ return UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_DYNAMIC_ENV_SIZE]; }
LispObj CBheapEnvSize()	{ return UVECTOR(symbolValue(COMPILER_CODE_BUFFER))[CODE_BUFFER_HEAP_ENV_SIZE]; }

#define MAX_CODE_BYTES	0x1000
#define MAX_REFS		0x400

LispObj makeCodeBuffer()
{
	LispObj s = 0;
	LispObj bv = 0;
	LispObj rv = 0;

	bv = byteVector(wrapInteger(MAX_CODE_BYTES + 16));
	rv = vectorNode(wrapInteger((MAX_REFS * 2) + 8));

	s = AllocVector(CODE_BUFFER_SIZE);
	UVECTOR(s)[0]							|= (StructureType << 3);
	UVECTOR(s)[CODE_BUFFER_INFO]			= COMPILER_CODE_BUFFER;
	UVECTOR(s)[CODE_BUFFER_CBUF]			= bv;
	UVECTOR(s)[CODE_BUFFER_REFS]			= rv;
	UVECTOR(s)[CODE_BUFFER_CODE_INDEX]		= 0;
	UVECTOR(s)[CODE_BUFFER_REF_INDEX]		= 0;
	UVECTOR(s)[CODE_BUFFER_DYNAMIC_ENV_SIZE]= 0;
	UVECTOR(s)[CODE_BUFFER_HEAP_ENV_SIZE]	= 0;
	UVECTOR(s)[CODE_BUFFER_STACK_INDEX]		= 0;
	UVECTOR(s)[CODE_BUFFER_MAX_CODE_BYTES]	= wrapInteger(MAX_CODE_BYTES);
	UVECTOR(s)[CODE_BUFFER_MAX_REFS]		= wrapInteger(MAX_REFS * 2);

	return s;	
}

//
//	If it is NIL, return NIL.
//	Otherwise, reverse the list, and store it into a simple vector.
//	Returns the vector.
LispObj convertRefsToVector(LispObj list)
{
	LispObj t0 = 0;
	LispObj t1 = 0;
	if (list == NIL)
		return list;
	t0 = Cnreverse(list);
	t1 = wrapInteger(listLength(t0));
	t0 = vectorNode(t1, t0);
	return t0;
}

//
//	If it is NIL, return NIL.
//	Otherwise, reverse the list, and store it into a simple vector.
//	The first 4 cells are reserved for EBP, EBX, EDI and ESI.
//	Returns the vector.
LispObj convertFrameInfoToVector(LispObj list)
{
	LispObj t0 = 0;
	LispObj t1 = 0;
	if (list == NIL)
		return list;
	t0 = Cnreverse(list);
	t0 = cons(0, cons(0, cons(0, cons(0, t0))));
	t1 = wrapInteger(listLength(t0));
	t0 = vectorNode(t1, t0);
	return t0;
}

//
//	Like copyTree, but does not copy quoted list structure.
//
static LispObj copyTreeUnquoted(LispObj tree)
{
	if (isCons(tree))
	{
		if (CAR(tree) == QUOTE)
			return cons(QUOTE, CDR(tree));
		else
			return cons(copyTreeUnquoted(CAR(tree)), copyTreeUnquoted(CDR(tree)));
	}
	else
		return tree;
}

//
//	compileExpression()
//	Takes an arbitrary lisp expression as its argument, 
//	and compiles it to produce a Lisp functions, which
//	when executed, will imitate the behavior of interpreting
//	the expression.
//	Returns the compiled expression.
//
LispObj compileExpression(LispObj x, LispObj lexMacros, LispObj lexSymbolMacros)
{
	LispObj f = 0;
	long stackNeeded = 0;
	LispObj info = NIL;
	LispObj t1 = 0;

	pushDynamicBinding(COLLECT_LEXICAL_MACROS, NIL);
	pushDynamicBinding(LEXICAL_MACROS, lexMacros);	// from previous (outer) lambda
	pushDynamicBinding(COLLECT_LEXICAL_SYMBOL_MACROS, NIL);
	pushDynamicBinding(LEXICAL_SYMBOL_MACROS, lexSymbolMacros);	// from previous (outer) lambda

	if (ExpandMacrosInline)
		if (symbolValue(MACROEXPAND_INLINE) == NIL)
			ExpandMacrosInline = 0;

	if (!ExpandMacrosInline)
	{
		x = LispCall3(Funcall, symbolFunction(MACROEXPAND_ALL), x, NIL);
		x = LispCall2(Funcall, symbolFunction(COLLECT_LITERALS), x);
	}

	// add the source file to the info list
	t1 = symbolValue(SOURCE_FILE);
	if (t1 != NIL)
	{
		info = cons(t1, info);
		info = cons(SOURCE_FILE, info);
	}
	// add the source line to the info list
	t1 = symbolValue(SOURCE_LINE);
	if (t1 != NIL)
	{
		info = cons(t1, info);
		info = cons(SOURCE_LINE, info);
	}

	t1 = symbolValue(SAVE_DEBUG_INFO);
	if (t1 != NIL)
	{
		// add the source line to the info list
		t1 = symbolValue(SOURCE_LINES);
		if (t1 != NIL)
		{
			info = cons(t1, info);
			info = cons(SOURCE_LINES, info);
		}
	}

	pushDynamicBinding(COMPILER_CLEANUPS, NIL);
	pushDynamicBinding(EMBEDDED_LAMBDAS, NIL);
	pushDynamicBinding(ENV_COUNTER, 0);
	pushDynamicBinding(COMPILER_CODE_BUFFER, NIL);
	pushDynamicBinding(COMPILER_VARIABLE_TYPES, NIL);
	pushDynamicBinding(COMPILER_DYNAMIC_EXTENT_VARS, NIL);
	pushDynamicBinding(CODE_JUMP_TABLE_REFS, NIL);
	pushDynamicBinding(CODE_ENV_TABLE_REFS, NIL);
	pushDynamicBinding(CODE_VAR_TABLE_REFS, NIL);
	pushDynamicBinding(COMPILED_SPECIALS, NIL);
	pushDynamicBinding(COMPILER_FRAME_INFO, NIL);
	pushDynamicBinding(COMPILER_LAMBDA_LIST, NIL);
	pushDynamicBinding(COMPILING_LAMBDA, NIL);
	pushDynamicBinding(LOAD_TIME_VALUES, NIL);
	
	t1 = list(COLLECT_LEXICAL_MACROS, 
		      LEXICAL_MACROS,
              COLLECT_LEXICAL_SYMBOL_MACROS,
              LEXICAL_SYMBOL_MACROS,
			  COMPILER_CLEANUPS, 
			  EMBEDDED_LAMBDAS,
			  ENV_COUNTER, 
			  COMPILER_CODE_BUFFER,
			  COMPILER_VARIABLE_TYPES,
			  COMPILER_DYNAMIC_EXTENT_VARS,
			  CODE_JUMP_TABLE_REFS,
			  CODE_ENV_TABLE_REFS,
			  CODE_VAR_TABLE_REFS,
			  COMPILED_SPECIALS,
			  COMPILER_FRAME_INFO,
			  COMPILER_LAMBDA_LIST,
			  COMPILING_LAMBDA,
              LOAD_TIME_VALUES,
			  END_LIST);
	establishSpecialBindings(t1);

	setSymbolValue(EMBEDDED_LAMBDAS, findLambdas(x));
	setSymbolValue(COMPILER_CODE_BUFFER, makeCodeBuffer());

	CBsetStackIndex(0);
	compileForm(x, Dest_EAX, T);
	if (CBstackIndex() != 0)
		Error("Stack index invalid after expression compilation: ~A", CBstackIndex());

	stackNeeded = integer(CBdynamicEnvSize());
	epilog(stackNeeded);
	prolog(stackNeeded);

	// if needed, add the jump table references to the info list
	t1 = symbolValue(COMPILER_COLLECT_JUMP_TABLE_REFS);
	if (t1 != NIL)
	{
		t1 = cons(convertRefsToVector(symbolValue(CODE_JUMP_TABLE_REFS)), info);
		info = cons(CODE_JUMP_TABLE_REFS, t1);
		t1 = cons(convertRefsToVector(symbolValue(CODE_ENV_TABLE_REFS)), info);
		info = cons(CODE_ENV_TABLE_REFS, t1);
	}

	// if needed, add the variable table references to the info list
	t1 = symbolValue(COMPILER_COLLECT_VAR_TABLE_REFS);
	if (t1 != NIL)
	{
		t1 = cons(convertRefsToVector(symbolValue(CODE_VAR_TABLE_REFS)), info);
		info = cons(CODE_VAR_TABLE_REFS, t1);
	}
	
	// save stack frame info if necessary
	t1 = symbolValue(COMPILER_SAVE_STACK_FRAME_INFO);
	if (t1 != NIL)
	{
		t1 = symbolValue(COMPILED_SPECIALS);
		while (isCons(t1))
		{
			addCompilerFrameInfo(CAR(t1), wrapInteger(3) /* esi */, 
				UVECTOR(CAR(t1))[SYMBOL_VAR_TABLE] * 4, wrapInteger(1) /* indirect */);
			t1 = CDR(t1);
		}

		if (COMPILER_FRAME_INFO != NIL)
		{
			t1 = cons(convertFrameInfoToVector(symbolValue(COMPILER_FRAME_INFO)), info);
			info = cons(STACK_FRAME, t1);
		}
	}

    // if any LOAD-TIME-VALUE forms were compiled, add those here
    t1 = symbolValue(LOAD_TIME_VALUES);
    if (t1 != NIL)
    {
        info = cons(LOAD_TIME_VALUES, cons(t1, info));
    }

	f = compiledFunctionNode(CBcode(), CBlength(), 
			CBreferences(), CBnumReferences(), NIL, info, 
			symbolValue(APPEND_REFS_TO_CODE));

	popDynamicBinding(ENV_COUNTER);
	popDynamicBinding(EMBEDDED_LAMBDAS);
	popDynamicBinding(COMPILER_CLEANUPS);
	popDynamicBinding(COMPILER_CODE_BUFFER);
	popDynamicBinding(COMPILER_VARIABLE_TYPES);
	popDynamicBinding(COMPILER_DYNAMIC_EXTENT_VARS);
	popDynamicBinding(CODE_JUMP_TABLE_REFS);
	popDynamicBinding(CODE_ENV_TABLE_REFS);
	popDynamicBinding(CODE_VAR_TABLE_REFS);
	popDynamicBinding(COMPILED_SPECIALS);
	popDynamicBinding(COMPILER_FRAME_INFO);
	popDynamicBinding(COMPILER_LAMBDA_LIST);
	popDynamicBinding(LEXICAL_MACROS);
	popDynamicBinding(LEXICAL_SYMBOL_MACROS);
	popDynamicBinding(COLLECT_LEXICAL_MACROS);
	popDynamicBinding(COLLECT_LEXICAL_SYMBOL_MACROS);
	popDynamicBinding(COMPILING_LAMBDA);
	popDynamicBinding(LOAD_TIME_VALUES);
	popSpecials();

	return f;
}
static int counter = 0;

LispObj compileLambdaExpression(LispObj lambda, LispObj name, LispObj env, LispObj lexMacros, LispObj lexSymbolMacros)
{
	LispObj lambdaList = 0;
	LispObj forms = 0;
	LispObj newVars = NIL;
	long stackNeeded = 0;
	LispObj f = 0;
	LispObj requiredArgs = 0;
	LispObj optionalArgs = 0;
	LispObj restArgs = 0;
	LispObj keyArgs = 0;
	LispObj auxArgs = 0;
	long minArgs = 0;
	long maxArgs = 0;
	LispObj allowOtherKeys = NIL;
	long numRequiredArgs = 0;
	long numOptionalArgs = 0;
	long numAuxArgs = 0;
	LispObj declarations = 0;
	LispObj decForm = 0;
	LispObj info = NIL;
	LispObj funcname = name;
	LispObj specials = 0;
	LispObj compilerBindings = NIL;
	LispObj numTypeDeclarations = 0;
	LispObj numDynamicExtentDeclarations = 0;
	LispObj t1 = 0;
	LispObj origLambda = lambda;
	LispObj lexicalMacros = 0;
	LispObj lexicalSymbolMacros = 0;
	LispObj specialDecs = NIL;
	long i = 0;
	long numLocals = 0;
	long emptyKeywordList = 0;

	counter++;

	// add the source file to the info list
	t1 = symbolValue(SOURCE_FILE);
	if (t1 != NIL)
	{
		info = cons(t1, info);
		info = cons(SOURCE_FILE, info);
	}
	// add the source line to the info list
	t1 = symbolValue(SOURCE_LINE);
	if (t1 != NIL)
	{
		info = cons(t1, info);
		info = cons(SOURCE_LINE, info);
	}

	t1 = symbolValue(SAVE_DEBUG_INFO);
	if (t1 != NIL)
	{
		// add the source line to the info list
		t1 = symbolValue(SOURCE_LINES);
		if (t1 != NIL)
		{
			info = cons(t1, info);
			info = cons(SOURCE_LINES, info);
		}
	}

	if (ExpandMacrosInline)
	{
		t1 = symbolValue(MACROEXPAND_INLINE);
		if (t1 == NIL)
			ExpandMacrosInline = 0;
	}

	pushDynamicBinding(COLLECT_LEXICAL_MACROS, NIL);
	pushDynamicBinding(LEXICAL_MACROS, lexMacros);	// from previous (outer) lambda
	pushDynamicBinding(COLLECT_LEXICAL_SYMBOL_MACROS, NIL);
	pushDynamicBinding(LEXICAL_SYMBOL_MACROS, lexSymbolMacros);	// from previous (outer) lambda
	if (!ExpandMacrosInline)
	{
		t1 = symbolFunction(MACROEXPAND_ALL_EXCEPT_TOP);
		lambda = LispCall3(Funcall, t1, lambda, NIL);
		lexicalMacros = symbolValue(COLLECT_LEXICAL_MACROS);
		lexicalSymbolMacros = symbolValue(COLLECT_LEXICAL_SYMBOL_MACROS);
	}

	pushDynamicBinding(COMPILER_CLEANUPS, env);
	pushDynamicBinding(EMBEDDED_LAMBDAS, NIL);
	pushDynamicBinding(ENV_COUNTER, 0);
	pushDynamicBinding(LAMBDA_SPECIAL_VARS, NIL);
	pushDynamicBinding(LAMBDA_DECLARATIONS, NIL);
	pushDynamicBinding(LAMBDA_SPECIAL_DECS, symbolValue(LAMBDA_SPECIAL_DECS));
	pushDynamicBinding(COMPILER_CODE_BUFFER, NIL);
	pushDynamicBinding(COMPILER_VARIABLE_TYPES, symbolValue(COMPILER_VARIABLE_TYPES));
	pushDynamicBinding(COMPILER_DYNAMIC_EXTENT_VARS, symbolValue(COMPILER_DYNAMIC_EXTENT_VARS));
	pushDynamicBinding(COMPILER_FUNCTION_NAME, NIL);
	pushDynamicBinding(COMPILER_USES_ENV, NIL);
	pushDynamicBinding(CODE_JUMP_TABLE_REFS, NIL);
	pushDynamicBinding(CODE_ENV_TABLE_REFS, NIL);
	pushDynamicBinding(CODE_VAR_TABLE_REFS, NIL);
	pushDynamicBinding(COMPILED_SPECIALS, NIL);
	pushDynamicBinding(COMPILER_FRAME_INFO, NIL);
	pushDynamicBinding(COMPILER_LAMBDA_LIST, NIL);
	pushDynamicBinding(COMPILING_LAMBDA, T);
    pushDynamicBinding(LOAD_TIME_VALUES, NIL);

	t1 = list(COLLECT_LEXICAL_MACROS, LEXICAL_MACROS, 
        COLLECT_LEXICAL_SYMBOL_MACROS, LEXICAL_SYMBOL_MACROS,
		COMPILER_CLEANUPS, EMBEDDED_LAMBDAS,
		ENV_COUNTER, LAMBDA_SPECIAL_VARS, LAMBDA_DECLARATIONS,
		LAMBDA_SPECIAL_DECS, COMPILER_CODE_BUFFER, COMPILER_VARIABLE_TYPES,
		COMPILER_DYNAMIC_EXTENT_VARS,
		COMPILER_FUNCTION_NAME, COMPILER_USES_ENV, 
		CODE_JUMP_TABLE_REFS, CODE_ENV_TABLE_REFS,
		CODE_VAR_TABLE_REFS, COMPILED_SPECIALS, COMPILER_FRAME_INFO, 
		COMPILER_LAMBDA_LIST, COMPILING_LAMBDA, LOAD_TIME_VALUES, END_LIST);
	establishSpecialBindings(t1);

	t1 = CDR(lambda);
	t1 = findLambdas(t1);
	setSymbolValue(EMBEDDED_LAMBDAS, t1);
	t1 = makeCodeBuffer();
	setSymbolValue(COMPILER_CODE_BUFFER, t1);

	CBaddLambda();

	if (!isCons(CDR(lambda)))
		Error("Invalid lambda expression: ~A", lambda);
	lambdaList = CAR(CDR(lambda));
	if (!isList(lambdaList))
		Error("Invalid lambda expression: ~A", lambda);
	setSymbolValue(COMPILER_LAMBDA_LIST, lambdaList);
	forms = CDR(CDR(lambda));

	if (funcname == NIL)
	{
		// skip declarations
		f = forms;
		while (isCons(f))
		{
			if (!isCons(CAR(f)) || (CAR(CAR(f)) != DECLARE))
				break;
			f = CDR(f);
		}
 		if (isCons(f) && isCons(CAR(f))
			&& CAR(CAR(f)) == BLOCK && isCons(CDR(CAR(f))))
			funcname = CAR(CDR(CAR(f)));
	}

	// add the lambda expression to the info list
	t1 = symbolValue(COMPILER_SAVE_LAMBDAS);
	if (t1 != NIL || LispCall2(Funcall, INLINE_PROCLAIM_P, funcname) != NIL)
	{
		t1 = cons(copyTreeUnquoted(origLambda), info);
		info = cons(LAMBDA, t1);
	}	

	// add the lambda list to the info list
	if (symbolValue(COMPILER_SAVE_LAMBDA_LISTS) != NIL)
		info = cons(LAMBDA_LIST, cons(lambdaList, info));
 
	// add the function name to the info list
	info = cons(FUNCTION_NAME, cons(funcname, info));
	setSymbolValue(COMPILER_FUNCTION_NAME, funcname);

	if (compilerOptimizeTailRecursion() != NIL)
	{
		lambda = LispCall3(Funcall, REMOVE_TAIL_RECURSION, lambda, funcname);
		lambdaList = CAR(CDR(lambda));
		setSymbolValue(COMPILER_LAMBDA_LIST, lambdaList);
		forms = CDR(CDR(lambda));
	}

	requiredArgs = findRequiredArgs(lambdaList);
	numRequiredArgs = listLength(requiredArgs);
	minArgs = numRequiredArgs;
	maxArgs = numRequiredArgs;
	optionalArgs = findOptionalArgs(lambdaList);
	numOptionalArgs = listLength(optionalArgs); 
	maxArgs += numOptionalArgs;
	restArgs = findRestArgs(lambdaList);
	if (restArgs != NIL)
		maxArgs = -1;	// no limit to number of args
	keyArgs = findKeyArgs(lambdaList);
	// if list contains arguments, the first value is &key
	if (keyArgs != NIL)
	{
		keyArgs = CDR(keyArgs);
		maxArgs = -1;	// no limit to number of args
		if (keyArgs == NIL)
			emptyKeywordList = 1;	// this special case has caused problems
	}
	allowOtherKeys = findAllowOtherKeys(lambdaList);
	if (allowOtherKeys != NIL)
		maxArgs = -1;	// no limit to number of args
	auxArgs = findAuxArgs(lambdaList);
	numAuxArgs = listLength(auxArgs);
	numLocals = numRequiredArgs + numOptionalArgs + listLength(restArgs) 
		+ listLength(keyArgs);

	// look for declarations
	f = forms;
	while (isCons(f))
	{
		if (isCons(CAR(f)) && (CAR(CAR(f)) == DECLARE))
			setSymbolValue(LAMBDA_DECLARATIONS, 
				cons(CAR(f), symbolValue(LAMBDA_DECLARATIONS)));
		else
			break;
		f = CDR(f);
	}
	forms = f;			// skip past declarations

	// check the declarations for SPECIAL declarations
	specialDecs = processSpecialDeclarations(symbolValue(LAMBDA_DECLARATIONS));
	if (isCons(specialDecs))
		setSymbolValue(LAMBDA_SPECIAL_DECS,
			LispCall2(lispAppend, specialDecs, symbolValue(LAMBDA_SPECIAL_DECS)));

	// check the declarations for compiler optimization settings
	compilerBindings = processOptimizeDeclarations(symbolValue(LAMBDA_DECLARATIONS));

	// check the declarations for type declarations
	numTypeDeclarations = processTypeDeclarations(symbolValue(LAMBDA_DECLARATIONS));

	// check the declarations for dynamic-extent declarations
	numDynamicExtentDeclarations = processDynamicExtentDeclarations(symbolValue(LAMBDA_DECLARATIONS));

	// compile the required args
	CBsetStackIndex(0);

	// skip check for number of arguments if speed >= safety + 2
	if (compilerCheckArgsNum() != NIL)
	{
		if (minArgs == maxArgs)
			compileArgsCheck(minArgs);
		else
			compileArgsRangeCheck(minArgs, maxArgs);
	}

	if (maxArgs == minArgs)
		newVars = compileLambdaRequiredArgs(requiredArgs, newVars, T);
	else
	{
		CBincDynamicEnvSize(wrapInteger(4));  // need to push ECX
		newVars = compileLambdaRequiredArgs(requiredArgs, newVars, NIL);
	}
	newVars = compileLambdaOptionalArgs(optionalArgs, newVars, 
		wrapInteger(numRequiredArgs));
	newVars = compileLambdaRestArgs(restArgs, newVars, 
		wrapInteger(numRequiredArgs + numOptionalArgs));

	if (keyArgs != NIL || emptyKeywordList == 1)
	{
		newVars = compileLambdaKeyArgs(keyArgs, newVars, 
			wrapInteger(numRequiredArgs + numOptionalArgs),
			allowOtherKeys);
	}

	if (auxArgs != NIL)
		newVars = compileLambdaAuxArgs(auxArgs, newVars);

	// check for variables which must be heap-allocated
	compileVariableHeapBindings(newVars);

	if (symbolValue(LAMBDA_SPECIAL_VARS) != NIL)
	{ 
		specials = symbolValue(LAMBDA_SPECIAL_VARS);
		CBpushCleanup(list(SPECIAL, specials, END_LIST));
		compileLiteralForm(specials, Dest_Stack, LIST);
		CALL_INDIRECT(ESTABLISH_SPECIAL_BINDINGS);
		ADD_ESP_NUM(4);
	}

	// handle ignore declarations
	processIgnoreDeclarations(symbolValue(LAMBDA_DECLARATIONS));

#if 0
	t1 = cons(name, forms);
	t1 = cons(BLOCK, t1);
	compileBlockForm(t1, Dest_EAX);
#else
	t1 = cons(PROGN, forms);
	compilePrognForm(forms, Dest_EAX);
#endif

	if (specials)
	{
		PUSH_EAX();
		PUSH_ECX();
		compileLiteralForm(specials, Dest_Stack, LIST);
		MOV_EDI_NIL();		// null environment
		SET_ARG_COUNT(1);
		CALL_INDIRECT(POP_SPECIAL_BINDINGS);
		ADD_ESP_NUM(4);
		CALL_INDIRECT(POP_SPECIALS);
		POP_ECX();
		POP_EAX();
		CBpopCleanup();
	}

	if (CBstackIndex() != 0)
		Error("Stack index invalid after expression compilation: ~A", CBstackIndex());

	if (maxArgs == minArgs)
		compileLambdaEpilog(NIL);
	else
 		compileLambdaEpilog(T);

	stackNeeded = integer(CBdynamicEnvSize());
	if (maxArgs == minArgs)
		compileLambdaProlog(stackNeeded, NIL, NIL);
	else
		compileLambdaProlog(stackNeeded, T, T);

	// if needed, add the jump table references to the info list
	t1 = symbolValue(COMPILER_COLLECT_JUMP_TABLE_REFS);
	if (t1 != NIL)
	{
		t1 = cons(convertRefsToVector(symbolValue(CODE_JUMP_TABLE_REFS)), info);
		info = cons(CODE_JUMP_TABLE_REFS, t1);
		t1 = cons(convertRefsToVector(symbolValue(CODE_ENV_TABLE_REFS)), info);
		info = cons(CODE_ENV_TABLE_REFS, t1);
	}

	// if needed, add the variable table references to the info list
	t1 = symbolValue(COMPILER_COLLECT_VAR_TABLE_REFS);
	if (t1 != NIL)
	{
		t1 = cons(convertRefsToVector(symbolValue(CODE_VAR_TABLE_REFS)), info);
		info = cons(CODE_VAR_TABLE_REFS, t1);
	}

	// save stack frame info if necessary
	t1 = symbolValue(COMPILER_SAVE_STACK_FRAME_INFO);
	if (t1 != NIL)
	{
		t1 = symbolValue(COMPILED_SPECIALS);
		while (isCons(t1))
		{
			addCompilerFrameInfo(CAR(t1), wrapInteger(3) /* esi */, 
				UVECTOR(CAR(t1))[SYMBOL_VAR_TABLE] * 4, wrapInteger(1) /* indirect */);
			t1 = CDR(t1);
		}
		if (COMPILER_FRAME_INFO != NIL)
		{
			t1 = cons(convertFrameInfoToVector(symbolValue(COMPILER_FRAME_INFO)), info);
			info = cons(STACK_FRAME, t1);
		}
	}

    // if any LOAD-TIME-VALUE forms were compiled, add those here
    t1 = symbolValue(LOAD_TIME_VALUES);
    if (t1 != NIL)
        info = cons(LOAD_TIME_VALUES, cons(t1, info));

	f = compiledFunctionNode(CBcode(), CBlength(), 
			CBreferences(), CBnumReferences(), NIL, info, 
			symbolValue(APPEND_REFS_TO_CODE));

	if (compilerBindings != NIL)
		restoreOptimizeDeclarationBindings(compilerBindings);

	popTypeDeclarations(numTypeDeclarations);
	popDEDeclarations(numDynamicExtentDeclarations);
	
	// pop all variables
	numLocals = listLength(newVars); // RGC 2/9/00
	for (i = 0; i < numLocals; i++)
		CBpopCleanup();

	CBpopCleanup();		// get rid of lambda marker

	popDynamicBinding(LAMBDA_SPECIAL_DECS);
	popDynamicBinding(LAMBDA_DECLARATIONS);
	popDynamicBinding(LAMBDA_SPECIAL_VARS);
	popDynamicBinding(ENV_COUNTER);
	popDynamicBinding(EMBEDDED_LAMBDAS);
	popDynamicBinding(COMPILER_CLEANUPS);
	popDynamicBinding(COMPILER_CODE_BUFFER);
	popDynamicBinding(COMPILER_VARIABLE_TYPES);
	popDynamicBinding(COMPILER_DYNAMIC_EXTENT_VARS);
	popDynamicBinding(COMPILER_FUNCTION_NAME);
	popDynamicBinding(COMPILER_USES_ENV);
	popDynamicBinding(CODE_JUMP_TABLE_REFS);
	popDynamicBinding(CODE_ENV_TABLE_REFS);
	popDynamicBinding(CODE_VAR_TABLE_REFS);
	popDynamicBinding(COMPILED_SPECIALS);
	popDynamicBinding(COMPILER_FRAME_INFO);
	popDynamicBinding(COMPILER_LAMBDA_LIST);
	popDynamicBinding(LEXICAL_MACROS);
	popDynamicBinding(LEXICAL_SYMBOL_MACROS);
	popDynamicBinding(COLLECT_LEXICAL_MACROS);
	popDynamicBinding(COLLECT_LEXICAL_SYMBOL_MACROS);
	popDynamicBinding(COMPILING_LAMBDA);
	popDynamicBinding(LOAD_TIME_VALUES);
	popSpecials();

	return f;
}

static LispObj compileLambdaRequiredArgs(LispObj args, LispObj newVars, LispObj useBPOffsets)
{
	long count = 0;
	LispObj t = 0;
	LispObj sym = 0;
	long index = 0;
	long numVars = listLength(args);

	while (isCons(args))
	{
		sym = CAR(args);
		if (!isSymbol(sym))
			Error("Invalid lambda list required argument: ~A", sym);
		if (isSpecialSymbol(sym) || (Cmember(sym, symbolValue(LAMBDA_SPECIAL_DECS)) != NIL))
		{
			setSymbolValue(LAMBDA_SPECIAL_VARS, cons(sym, symbolValue(LAMBDA_SPECIAL_VARS)));
 			if (!symbolVarTableIndex(sym))
				createSymbolTableEntry(sym);

			// add dynamic binding to symbol
			PUSH_ECX();						// save arg count
			CALL_INDIRECT(ALLOC_CONS);		
			POP_ECX();						// restore arg count
			MOV_EDI_EAX();					// edi = new cons cell
			MOV_EAX_SYMBOL_BINDING(sym);
			MOV_EDI_PTR_WITH_OFFSET_EAX(0);	// CDR points to old value contents

			if (useBPOffsets != NIL)
			{
				MOV_EAX_EBP_PTR_WITH_OFFSET(8 + ((numVars - index - 1) * 4));
			}
			else
			{
				MOV_EAX_EBX_PTR_WITH_OFFSET(count); // initialize new value
			}
			MOV_EDI_PTR_WITH_OFFSET_EAX(-4);// CAR points to new value
			MOV_EAX_EDI();					// debug!!
			MOV_SYMBOL_BINDING_EAX(sym);
		}
		else
		{
			if (useBPOffsets != NIL)
				t = list(sym, EBP, wrapInteger(8 + ((numVars - index - 1) * 4)), END_LIST);
			else
				t = list(sym, EBX, wrapInteger(count), END_LIST);
			CBaddStackVar(sym, CDR(t));
			newVars = cons(t, newVars);
		}
		count -= 4;
		index++;
		args = CDR(args);
	}
	return newVars;
}

static LispObj compileLambdaOptionalArgs(LispObj args, LispObj newVars, 
		LispObj argIndex)
{
	LispObj t = 0;
	LispObj s = 0;
	LispObj sym = 0;
	LispObj form = 0;
	LispObj init = 0;
	LispObj supplied_p = 0;
	LispObj varOffset = 0;
	LispObj supplied_p_Offset = 0;
	LispObj firstBranchAddress = 0;
	LispObj secondBranchAddress = 0;
	long dynamicBind = 0;

	while (isCons(args))
	{
		form = CAR(args);
		if (isSymbol(form))
		{
			sym = form;
			init = NIL;
			supplied_p = NIL;
		}
		else
		if (isCons(form))
		{
			sym = CAR(form);
			if (isCons(CDR(form)))
			{
				init = CAR(CDR(form));
				if (isCons(CDR(CDR(form))))
					supplied_p = CAR(CDR(CDR(form)));
				else
					supplied_p = NIL;
			}
			else
			{
				init = NIL;
				supplied_p = NIL;
			}
		}
		if (!isSymbol(sym))
			Error("Lambda &optional argument is not a symbol: ~A", sym);
		if (!isSymbol(supplied_p))
			Error("Lambda &optional supplied_p argument is not a symbol: ~A", supplied_p);

		if (isSpecialSymbol(sym) || (Cmember(sym, symbolValue(LAMBDA_SPECIAL_DECS)) != NIL))
		{
			dynamicBind = 1;
			setSymbolValue(LAMBDA_SPECIAL_VARS, cons(sym, symbolValue(LAMBDA_SPECIAL_VARS)));
		}
		else
			dynamicBind = 0;

		// allocate space in the frame for the variable
		if (!dynamicBind)
		{
			varOffset = CBincDynamicEnvSize(wrapInteger(4));
			t = list(sym, EBP, varOffset, END_LIST);
			// CBaddStackVar(sym, CDR(t));      // add this after initialization
		}

		// if there is a supplied-p variable, allocate space for it
		if (supplied_p != NIL)	// always lexical, for now!
		{
			supplied_p_Offset = CBincDynamicEnvSize(wrapInteger(4));
			s = list(supplied_p, EBP, supplied_p_Offset, END_LIST);
			CBaddStackVar(supplied_p, CDR(s));
		}
		else
			s = 0;

		CMP_ECX_LONG(integer(argIndex));

		// jle next1
		JLE_SHORT_RELATIVE(0);
		firstBranchAddress = CURRENT_IP;

		if (supplied_p != NIL)
		{
			MOV_EAX_ESI_PTR_WITH_OFFSET(4);					// mov eax, [esi + 4]; supplied_p = T
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(supplied_p_Offset));	// mov [ebp + supplied_p_Offset], eax 
		}

		if (!dynamicBind)
		{
			MOV_EAX_EBX_PTR_WITH_OFFSET(-(integer(argIndex) * 4)); //	mov eax, [ebx - (argIndex * 4)]
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(varOffset));				// mov [ebp + varOffset], eax
		}
		else
		{
 			if (!symbolVarTableIndex(sym))
				createSymbolTableEntry(sym);

			// add dynamic binding to symbol
			PUSH_ECX();						// save arg count
			CALL_INDIRECT(ALLOC_CONS);		
			POP_ECX();						// restore arg count
			MOV_EDI_EAX();					// edi = new cons cell
			MOV_EAX_SYMBOL_BINDING(sym);
			MOV_EDI_PTR_WITH_OFFSET_EAX(0);	// CDR points to old value contents
			MOV_EAX_EBX_PTR_WITH_OFFSET(-(integer(argIndex) * 4)); // initialize new value
			MOV_EDI_PTR_WITH_OFFSET_EAX(-4);// CAR points to new value
			MOV_EAX_EDI();					// debug!!
			MOV_SYMBOL_BINDING_EAX(sym);
		}

		//	jmp short next2
		JMP_SHORT_RELATIVE(0);
 		secondBranchAddress = CURRENT_IP;

		// now resolve addresses
		CBsetByte(firstBranchAddress - wrapInteger(1), CURRENT_IP - firstBranchAddress);

		if (supplied_p != NIL)
		{
			MOV_EAX_NIL();					// mov eax, [esi]; supplied_p = NIL
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(supplied_p_Offset));
		}

		if (!dynamicBind)
		{
			if (init != NIL)
			{
				PUSH_ECX();			// push ecx
				compileForm(init, Dest_EAX_Operand, T);
 				POP_ECX();			// pop ecx
				MOV_EBP_PTR_WITH_OFFSET_EAX(integer(varOffset));			// mov [ebp + varOffset], eax
			}
		}
		else
		{
 			if (!symbolVarTableIndex(sym))
				createSymbolTableEntry(sym);

			// add dynamic binding to symbol
			PUSH_ECX();						// save arg count
			CALL_INDIRECT(ALLOC_CONS);		
			MOV_EDI_EAX();					// edi = new cons cell
			MOV_EAX_SYMBOL_BINDING(sym);
			MOV_EDI_PTR_WITH_OFFSET_EAX(0);	// CDR points to old value contents
            PUSH_EDI();
			compileForm(init, Dest_EAX_Operand, T);
            POP_EDI();
			POP_ECX();
			MOV_EDI_PTR_WITH_OFFSET_EAX(-4);// CAR points to new value
			MOV_EAX_EDI();					// debug!!
			MOV_SYMBOL_BINDING_EAX(sym);
		}

		//	next2:
		CBsetByte(secondBranchAddress - wrapInteger(1), CURRENT_IP - secondBranchAddress);

		if (!dynamicBind)
		{
			CBaddStackVar(sym, CDR(t));
		}

		if (isCons(s))
			newVars = cons(s, newVars);
		if (!dynamicBind)
			newVars = cons(t, newVars);
		argIndex += wrapInteger(1);
		args = CDR(args);
	}
	return newVars;
}

static LispObj compileLambdaRestArgs(LispObj args, LispObj newVars, LispObj argIndex)
{
	if (!isCons(args))
		return newVars;

	LispObj sym = 0;
	LispObj varOffset = 0;
	LispObj t = 0;
	LispObj address0 = 0;
	LispObj address1 = 0;
	LispObj address2 = 0;
	LispObj createDynamicList = T;

	// if all &rest arguments are dynamic-extent, create a list on 
	// the stack
	t = args;
	while (isCons(t))
	{
		if (isDynamicExtent(CAR(t)) == NIL)
		{
			createDynamicList = NIL;
			break;
		}
		t = CDR(t);
	}

	if (createDynamicList != NIL)
	{
		// create list of remaining vars in eax, use stack instead of heap
		PUSH_ECX();					// push ecx
		PUSH_EDX();					// push edx
		PUSH_EDI();					// push edi
		DEC_STACK(12);				// no net change

		// allocate space in the frame for the variable
		varOffset = CBincDynamicEnvSize(wrapInteger(4));
		MOV_EBP_PTR_WITH_OFFSET_ESP(integer(varOffset));
		
		MOV_EDI_ESP();				// edi = saved stack pointer
		TEST_ESP_NUM(4);				// see if ESP is at an 8-byte boundary
		JZ_SHORT_RELATIVE(0);
		address0 = CURRENT_IP;
		PUSH_NUM(0);
		DEC_STACK(4);				// no net change
		CBsetByte(address0 - wrapInteger(1), CURRENT_IP - address0);
		MOV_EAX_ECX();				// mov eax, ecx
		DEC_EAX();					// dec eax
		SHL_EAX_NUM(2);				// shl eax, 2
		NEG_EAX();					// neg eax
		MOV_EDI_EAX();				// mov edi, eax	  ; edi = offset into arg list of last arg
		SUB_ECX_NUM(integer(argIndex));	 // sub ecx, argIndex
		
		MOV_EAX_NIL();				// mov eax, dword ptr [esi]
		MOV_EDX_EAX();				// mov edx, eax
									// loop_continue:
		address1 = CURRENT_IP;
		TST_ECX_ECX();				// tst ecx, ecx
		JLE_SHORT_RELATIVE(0);		// jle exit_loop
		address2 = CURRENT_IP;

		// get a cons cell on the stack
		PUSH_NUM(0);
		DEC_STACK(4);				// no net change
		MOV_EAX_ESP();					// eax = cons cell (tag = 4)
		PUSH_NUM(0);
		DEC_STACK(4);				// no net change

		MOV_EAX_PTR_WITH_OFFSET_EDX(0);	// mov dword ptr [eax], edx
		PUSH_EDI_EBX_WITH_OFFSET(0);	// push dword ptr [edi + ebx]
		POP_EAX_PTR_WITH_OFFSET(-4);	// pop dword ptr [eax - 4]
		MOV_EDX_EAX();					// mov edx, eax
		ADD_EDI_NUM(4);					// add edi, 4
		DEC_ECX();						// dec ecx
		JMP_SHORT_RELATIVE(0);			// jmp short loop_continue
		CBsetByte(CURRENT_IP - wrapInteger(1), address1 - CURRENT_IP);
		CBsetByte(address2 - wrapInteger(1), CURRENT_IP - address2);
										// exit_loop:
		MOV_EDI_EBP_PTR_WITH_OFFSET(integer(varOffset));
		MOV_ECX_EDI_PTR_WITH_OFFSET(8);
		MOV_EDX_EDI_PTR_WITH_OFFSET(4);
		MOV_EDI_EDI_PTR_WITH_OFFSET(0);
	}
	else
	{
		// create list of remaining vars in eax

		PUSH_ECX();					// push ecx
		PUSH_EDX();					// push edx
		MOV_EAX_ECX();				// mov eax, ecx
		DEC_EAX();					// dec eax
		SHL_EAX_NUM(2);				// shl eax, 2
		NEG_EAX();					// neg eax
		MOV_EDI_EAX();				// mov edi, eax	  ; edi = offset into arg list of last arg
		SUB_ECX_NUM(integer(argIndex));	 // sub ecx, argIndex
		
		MOV_EAX_NIL();				// mov eax, dword ptr [esi]
		MOV_EDX_EAX();				// mov edx, eax
									// loop_continue:
		address1 = CURRENT_IP;
		TST_ECX_ECX();				// tst ecx, ecx
		JLE_SHORT_RELATIVE(0);		// jle exit_loop
		address2 = CURRENT_IP;

		
		PUSH_ECX();
		PUSH_EDX();
		CALL_INDIRECT(ALLOC_CONS);
		POP_EDX();
		POP_ECX();
		MOV_EAX_PTR_WITH_OFFSET_EDX(0);	// mov dword ptr [eax], edx
		PUSH_EDI_EBX_WITH_OFFSET(0);	// push dword ptr [edi + ebx]
		POP_EAX_PTR_WITH_OFFSET(-4);	// pop dword ptr [eax - 4]
		MOV_EDX_EAX();					// mov edx, eax
		ADD_EDI_NUM(4);					// add edi, 4
		DEC_ECX();						// dec ecx
		JMP_SHORT_RELATIVE(0);			// jmp short loop_continue
		CBsetByte(CURRENT_IP - wrapInteger(1), address1 - CURRENT_IP);
		CBsetByte(address2 - wrapInteger(1), CURRENT_IP - address2);
										// exit_loop:
		POP_EDX();						// pop edx
		POP_ECX();						// pop ecx
	}

	while (isCons(args))
	{
		sym = CAR(args);
		if (!isSymbol(sym))
			Error("Invalid &rest variable in lambda list: ~A", sym);

		// allocate space in the frame for the variable
		varOffset = CBincDynamicEnvSize(wrapInteger(4));

		t = list(sym, EBP, varOffset, END_LIST);
		CBaddStackVar(sym, CDR(t));

		MOV_EBP_PTR_WITH_OFFSET_EAX(integer(varOffset));

		newVars = cons(t, newVars);
		argIndex += wrapInteger(1);
		args = CDR(args);
	}
	return newVars;
}

static LispObj compileLambdaKeyArgs(LispObj args, LispObj newVars, 
					 LispObj argIndex, LispObj allowOtherKeys)
{
	LispObj vars = args;
	LispObj loopAddr = 0;
	LispObj foundAddr = 0;
	LispObj keySymbol = 0;
	LispObj varOffset = 0;
	LispObj t = 0;
	LispObj s = 0;
	LispObj form = 0;
	LispObj sym = 0;
	LispObj init = 0;
	LispObj supplied_p = 0;
	LispObj supplied_p_Offset = 0;
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	LispObj addr3 = 0;
	int numKeys = 0;
	int i = 0;
	int checkKeys = 0;

	checkKeys = (allowOtherKeys == NIL) && (compilerCheckKeyArgs() != NIL);

	while (isCons(vars))
	{
		keySymbol = 0;

		form = CAR(vars);
		if (isSymbol(form))
		{
			sym = form;
			init = NIL;
			supplied_p = NIL;
		}
		else
		if (isCons(form))
		{
			sym = CAR(form);
			if (isCons(CDR(form)))
			{
				init = CAR(CDR(form));
				if (isCons(CDR(CDR(form))))
					supplied_p = CAR(CDR(CDR(form)));
				else
					supplied_p = NIL;
			}
			else
			{
				init = NIL;
				supplied_p = NIL;
			}
		}
		if (isCons(sym) && isCons(CDR(sym)))
		{
			keySymbol = CAR(sym);
			sym = CAR(CDR(sym));
		}

		if (!isSymbol(sym))
			Error("Lambda &key variable is not a symbol: ~A", sym);

		if (!isSymbol(supplied_p))
			Error("Lambda &key supplied_p argument is not a symbol: ~A", supplied_p);

		// allocate space in the frame for the variable
		varOffset = CBincDynamicEnvSize(wrapInteger(4));

		t = list(sym, EBP, varOffset, END_LIST);
		CBaddStackVar(sym, CDR(t));

		// if there is a supplied-p variable, allocate space for it
		if (supplied_p != NIL)
		{
			supplied_p_Offset = CBincDynamicEnvSize(wrapInteger(4));
			s = list(supplied_p, EBP, supplied_p_Offset, END_LIST);
			CBaddStackVar(supplied_p, CDR(s));
		}
		else
			s = 0;
		
		if (!keySymbol)
			keySymbol = findKeywordSym(symbolName(sym));
		else
		if (!isSymbol(keySymbol))
			Error("Lambda &key keyword-name is not a symbol: ~A", keySymbol);

		PUSH_ECX();						// save arg count
		compileLiteralForm(keySymbol, Dest_EAX_Operand, SYMBOL);	// mov eax, keySymbol
		POP_ECX();						// restore arg count
		if (checkKeys)
		{
			PUSH_EAX();						// save key on stack
			numKeys++;
		}
		MOV_EDX_EAX();					// edx = key
		MOV_EDI_EBX();					// mov edi, ebx
		MOV_EAX_NUM(integer(argIndex) * 4);	// mov eax, -(argIndex * 4)
		SUB_EDI_EAX();					// edi = address of current argument
		PUSH_ECX();						// save ecx
		MOV_EAX_ECX();
		SHL_EAX_NUM(2);
		MOV_ECX_EBX();
		SUB_ECX_EAX();					// ecx = address of last argument - 4

		// loop:
		loopAddr = CURRENT_IP;

		CMP_EDI_ECX();					// checked all arguments?
		JLE_SHORT_RELATIVE(0);			// jle notFoundAddr	; yes, do default
		addr1 = CURRENT_IP;

		MOV_EAX_EDI_PTR_WITH_OFFSET(0);	// mov eax, [edi]	; eax = arg
		SUB_EDI_NUM(8);					// dec arg pointer
		CMP_EAX_EDX();					// found key symbol?									
		JNE_SHORT_RELATIVE(0);			// jne loopAddr		; no, loop
		CBsetByte(CURRENT_IP - wrapInteger(1), loopAddr - CURRENT_IP);

		// found thekey
		ADD_EDI_NUM(4);
		CMP_EDI_ECX();					// make sure there is another argument
		JNE_SHORT_RELATIVE(0);			// jne continueAddr
		addr2 = CURRENT_IP;
		CALL_INDIRECT(WRONG_NUMBER_OF_ARGS);	// call wrongNumberOfArgs()

										// continueAddr:
		CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);
		MOV_EAX_EDI_PTR_WITH_OFFSET(0);	// eax = key argument
		MOV_EBP_PTR_WITH_OFFSET_EAX(integer(varOffset));	// set key var
		if (supplied_p != NIL)
		{
			MOV_EAX_T();				// mov eax, [esi + 4]; supplied_p = T
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(supplied_p_Offset));	// mov [ebp + supplied_p_Offset], eax 
		}
		JMP_LONG_RELATIVE(0);			// jmp keydone
		addr3 = CURRENT_IP;
										// notFoundAddr:
		CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);

		PUSH_ECX();						// save arg count
		compileForm(init, Dest_EAX_Operand, T);
		POP_ECX();
		MOV_EBP_PTR_WITH_OFFSET_EAX(integer(varOffset));	// mov [ebp + varOffset], eax
		if (supplied_p != NIL)
		{
			MOV_EAX_NIL();				// mov eax, [esi + 4]; supplied_p = T
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(supplied_p_Offset));	// mov [ebp + supplied_p_Offset], eax 
		}										// keydone:
		CBsetLong(addr3 - wrapInteger(4), CURRENT_IP - addr3);
		POP_ECX();						// restore ecx

		if (isCons(s))
			newVars = cons(s, newVars);
		newVars = cons(t, newVars);
//		argIndex += wrapInteger(1);
		vars = CDR(vars);
	}

	if (checkKeys /*&& numKeys > 0*/)
	{
		// check for invalid key arguments

		// set up EDI to point to the first key argument, ECX points to last argument
		MOV_EDI_EBX();					// mov edi, ebx
		MOV_EAX_NUM(integer(argIndex) * 4);	// mov eax, -(argIndex * 4)
		SUB_EDI_EAX();					// edi = address of current argument
		PUSH_ECX();						// save ecx
		MOV_EAX_ECX();
		SHL_EAX_NUM(2);
		MOV_ECX_EBX();
		SUB_ECX_EAX();					// ecx = address of last argument - 4
			
		// loop:
		loopAddr = CURRENT_IP;

		CMP_EDI_ECX();					// checked all arguments?
		JLE_RELATIVE(0);				// jle doneChecking
		addr1 = CURRENT_IP;

		MOV_EAX_EDI_PTR_WITH_OFFSET(0);		// eax = key argument
		SUB_EDI_NUM(8);
		
		// checkKeys:
		for (i = 0; i < numKeys; i++)
		{
			CMP_EAX_ESP_PTR_WITH_OFFSET((i + 1) * 4);	// compare against key
			JE_RELATIVE(0);								// jump foundKey
			addr2 = CURRENT_IP;				
			CBsetLong(addr2 - wrapInteger(4), loopAddr - CURRENT_IP);
		}

		// did not find key
		POP_ECX();
		ADD_ESP_NUM(numKeys * 4);	
		PUSH_EAX();						// did not find it
		SET_ARG_COUNT(1);
		CALL_INDIRECT(INVALID_KEY_ARG);	// call invalidKeyArg()
		INC_STACK(numKeys * 4);
		
		// doneChecking:
		CBsetLong(addr1 - wrapInteger(4), CURRENT_IP - addr1);

		POP_ECX();
		ADD_ESP_NUM(numKeys * 4);
	}

	return newVars;
}

static LispObj compileLambdaAuxArgs(LispObj args, LispObj newVars)
{
	LispObj t = 0;
	LispObj sym = 0;
	LispObj form = 0;
	LispObj init = 0;
	LispObj varOffset = 0;
	LispObj firstBranchAddress = 0;
	LispObj secondBranchAddress = 0;
	long dynamicBind = 0;

	while (isCons(args))
	{
		form = CAR(args);
		if (isSymbol(form))
		{
			sym = form;
			init = NIL;
		}
		else
		if (isCons(form))
		{
			sym = CAR(form);
			if (isCons(CDR(form)))
				init = CAR(CDR(form));
			else
				init = NIL;
		}
		if (!isSymbol(sym))
			Error("Lambda &optional argument is not a symbol: ~A", sym);

		if (isSpecialSymbol(sym) || (Cmember(sym, symbolValue(LAMBDA_SPECIAL_DECS)) != NIL))
		{
			dynamicBind = 1;
			setSymbolValue(LAMBDA_SPECIAL_VARS, cons(sym, symbolValue(LAMBDA_SPECIAL_VARS)));
		}
		else
			dynamicBind = 0;

		// allocate space in the frame for the variable
		if (!dynamicBind)
		{
			varOffset = CBincDynamicEnvSize(wrapInteger(4));
			t = list(sym, EBP, varOffset, END_LIST);
		}

		if (!dynamicBind)
		{
			if (init != NIL)
			{
				PUSH_ECX();			// push ecx
				compileForm(init, Dest_EAX_Operand, T);
 				POP_ECX();			// pop ecx
				MOV_EBP_PTR_WITH_OFFSET_EAX(integer(varOffset));			// mov [ebp + varOffset], eax
			}
		}
		else
		{
 			if (!symbolVarTableIndex(sym))
				createSymbolTableEntry(sym);

			// add dynamic binding to symbol
			PUSH_ECX();						// save arg count
			CALL_INDIRECT(ALLOC_CONS);		
			MOV_EDI_EAX();					// edi = new cons cell
			MOV_EAX_SYMBOL_BINDING(sym);
			MOV_EDI_PTR_WITH_OFFSET_EAX(0);	// CDR points to old value contents
            PUSH_EDI();
			compileForm(init, Dest_EAX_Operand, T);
 			POP_EDI();
            POP_ECX();
			MOV_EDI_PTR_WITH_OFFSET_EAX(-4);// CAR points to new value
			MOV_EAX_EDI();					// debug!!
			MOV_SYMBOL_BINDING_EAX(sym);
		}

		if (!dynamicBind)
        {
			newVars = cons(t, newVars);
			CBaddStackVar(sym, CDR(t));
        }
		args = CDR(args);
	}
	return newVars;
}

static LispObj compileList(LispObj x, LispObj dest, LispObj resultType)
{
	LispObj first = CAR(x);
	LispObj funcallForm = 0;
	LispObj retval = T;

	if (ExpandMacrosInline)
	{
		while (isSymbol(first) && hasMacroBinding(first))
		{
			x = LispCall3(Funcall, symbolFunction(first), x, NIL);
			if (!isCons(x))
			{
				return compileForm(x, dest, resultType);
			}
			first = CAR(x);
		}
	}
	if (isSymbol(first))
	{
		if (isSpecialOperator(first))
			retval = compileSpecialOperator(x, dest, resultType);
		else
		if (hasFunctionBinding(first) || (findLexFunction(first) != NIL))
			retval = compileFunctionExpressionForm(x, dest, resultType);
		else
			retval = compileFunctionExpressionForm(x, dest, resultType);
	}
	else
	if (isLambdaForm(first))
	{
		funcallForm = cons(list(FUNCTION, first, END_LIST), CDR(x));
		funcallForm = cons(FUNCALL, funcallForm);
		retval = compileFunctionExpressionForm(funcallForm, dest, resultType); 
	}
	else
		Error("Cannot compile form: ~A", x);

	return retval;
}

static LispObj compileSymbol(LispObj sym, LispObj dest, LispObj /*resultType*/)
{
	LispObj addr1 = 0;
	LispObj retval = 0;

	if (sym == NIL)
	{
		return compileNil(dest);
	}

	if (isConstantSymbol(sym) && compilerFoldConstants() != NIL)
	{
		return compileLiteralForm(symbolValue(sym), dest, T);
	}

	// see if the symbol is in the stack environment
	retval = compileLexicalEntity(sym, dest, LET);

	if (retval == NIL)
	{
		// lexical variable not found, assume global/special
		if (!isSpecialSymbol(sym) && (Cmember(sym, symbolValue(LAMBDA_SPECIAL_DECS)) == NIL))
		{
			// warn of symbol assumed special if compiling a lambda
			if (symbolValue(COMPILER_WARN_ON_ASSUMED_SPECIAL) != NIL
				&& symbolValue(COMPILING_LAMBDA) != NIL)
			{
				LispCall3(Funcall, WARN, stringNode("Symbol ~S assumed special"), sym);
				/*
				LispCall(Write, stringNode(";;; Warning: Symbol "));
				LispCall(Write, sym);
				LispCall(Write, stringNode(" assumed special"));
				LispCall(Terpri);
				*/
			}
		}

		// if no symbol table entry, create one now
		if (!symbolVarTableIndex(sym))
			createSymbolTableEntry(sym);
	//	setSymbolValue(COMPILED_SPECIALS, cons(sym, symbolValue(COMPILED_SPECIALS)));

		MOV_EAX_SYMBOL_VALUE(sym);
		CMP_EAX_LONG(UNINITIALIZED);		// cmp eax, UNINITIALIZED
		JNE_RELATIVE(0);					// jne next
		addr1 = CURRENT_IP;
		compileLiteralForm(sym, Dest_Stack, T);
		SET_ARG_COUNT(1);
		CALL_INDIRECT(UNBOUND_VARIABLE);	// call UnboundVariable()
		ADD_ESP_NUM(4);
		// next:
		CBsetLong(addr1 - wrapInteger(4), CURRENT_IP - addr1);
		TARGET(1);
	}
	retval = findVariableType(sym);
	return retval;
}

static /*__declspec(thread)*/ byte pbuf[128];
static /*__declspec(thread)*/ long pc = 0;

static void prolog(long envSize)
{
	long i = 0;
	pc = 0;

	pbuf[pc++]= 0x55; 								// push	ebp
	pbuf[pc++]= 0x8b; pbuf[pc++]= 0xec;				// mov	ebp, esp
	pbuf[pc++]= 0x57; 								// push	edi	 (environment)

	if (envSize > 0)
	{
		pbuf[pc++]=0x8b; pbuf[pc++]=0x06;			// mov eax, [esi]
		if (envSize < 64)							// inline up to 16 push operations
		{
			for (i = 0; i < envSize; i += 4)
				pbuf[pc++]= 0x50; 					// push	eax		;; push nil
		}
		else
		{
 			pbuf[pc++]= 0xba; 							// mov edx, envSize	/ 4
			pbuf[pc++]= (unsigned char)((unsigned long)envSize >> 2) & 0xff;	
			pbuf[pc++]= (unsigned char)(((unsigned long)envSize >> 2) >> 8) & 0xff;	
			pbuf[pc++]= (unsigned char)(((unsigned long)envSize >> 2) >> 16) & 0xff;	
			pbuf[pc++]= (unsigned char)(((unsigned long)envSize >> 2) >> 24) & 0xff;
														// loop:
 			pbuf[pc++]= 0x50; 							// push	eax		;; push nil
 			pbuf[pc++]= 0x4a; 							// dec edx
			pbuf[pc++]= 0x7f; pbuf[pc++]= 0xfc;			// jg :loop
		}
	}

	// now insert into code buffer
	CBinsertCode(0, pbuf, wrapInteger(pc));
}

static void epilog(long /*envSize*/)
{
	MOV_ESP_EBP();
	POP_EBP();
	RET();
}

static void compileLambdaProlog(long envSize, LispObj needEBX, LispObj saveECX)
{
	long i = 0;
	pc = 0;
	pbuf[pc++]= 0x55; 								// push	ebp
	pbuf[pc++]= 0x8b; pbuf[pc++]= 0xec;				// mov	ebp, esp
	pbuf[pc++]= 0x57; 								// push	edi	 (environment)
	if (saveECX != NIL)
		pbuf[pc++]= 0x51; 							// push ecx	 (arg count)
	if (envSize > 0)
	{
		pbuf[pc++]=0x8b; pbuf[pc++]=0x06;			// mov eax, [esi]
		if (envSize < 64)							// inline up to 16 push operations
		{
			for (i = 0; i < envSize; i += 4)
				pbuf[pc++]= 0x50; 					// push	eax		;; push nil
		}
		else
		{
 			pbuf[pc++]= 0xba; 							// mov edx, envSize	/ 4
			pbuf[pc++]= (unsigned char)((unsigned long)envSize >> 2) & 0xff;	
			pbuf[pc++]= (unsigned char)(((unsigned long)envSize >> 2) >> 8) & 0xff;	
			pbuf[pc++]= (unsigned char)(((unsigned long)envSize >> 2) >> 16) & 0xff;	
			pbuf[pc++]= (unsigned char)(((unsigned long)envSize >> 2) >> 24) & 0xff;
														// loop:
 			pbuf[pc++]= 0x50; 							// push	eax		;; push nil
 			pbuf[pc++]= 0x4a; 							// dec edx
			pbuf[pc++]= 0x7f; pbuf[pc++]= 0xfc;			// jg :loop
		}
	}

	if (needEBX != NIL)
	{
		pbuf[pc++]=0x53;								// push ebx
		pbuf[pc++]=0x8d; pbuf[pc++]=0x5c; pbuf[pc++]=0x8d; pbuf[pc++]=0x04;	// lea ebx,[ebp+ecx*4+4]
		// ebx now contains the 1st arg address
	}

	// now insert into code buffer
	CBinsertCode(0, pbuf, wrapInteger(pc));
}

static void compileLambdaEpilog(LispObj needEBX)
{
	if (needEBX != NIL)
	{
		POP_EBX();
	}
	MOV_ESP_EBP();
	POP_EBP();
	RET();
}

static void compileArgsCheck(long num)
{
	LispObj addr1 = 0;
	if (num == 0)
	{
		TEST_ECX_ECX();
	}
	else
	{
		CMP_ECX_LONG(num);
	}
	JE_SHORT_RELATIVE(0);
	addr1 = CURRENT_IP;
	CALL_INDIRECT(WRONG_NUMBER_OF_ARGS);
	CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);
}

// We assume we can't get passed a negative number of
// args, so if the lower limit is 0 skip that check.
// If there is no upper limit, -1 will be passed in high parameter,
// so skip that check in this case.
static void compileArgsRangeCheck(long low, long high)
{
	LispObj codeAddr1 = 0;
	LispObj codeAddr2 = 0;
	LispObj codeAddr3 = 0;

	if (low > 0 || high >= 0)  // if high < 0, no upper limit
	{
		if (low > 0)
		{
			CMP_ECX_LONG(low);
			JL_SHORT_RELATIVE(0);				// jl err
			codeAddr1 = CURRENT_IP;
		}
		if (high >= 0)
		{
			CMP_ECX_LONG(high);
			JG_SHORT_RELATIVE(0);			// jg err
			codeAddr2 = CURRENT_IP;
		}
		JMP_SHORT_RELATIVE(0);		// jmp short next
		codeAddr3 = CURRENT_IP;
		if (low > 0)
			CBsetByte(codeAddr1 - wrapInteger(1), codeAddr3 - codeAddr1);
		if (high >= 0)
			CBsetByte(codeAddr2 - wrapInteger(1), codeAddr3 - codeAddr2);

		CALL_INDIRECT(WRONG_NUMBER_OF_ARGS);

		// next:
 		CBsetByte(codeAddr3 - wrapInteger(1), CURRENT_IP - codeAddr3);
	}
}

static LispObj compileFunctionExpressionForm(LispObj x, LispObj dest, LispObj resultType)
{
	LispObj sym = CAR(x);

	// if the function was defined with FLET, handle that and exit
	if (findLexFunction(sym) != NIL)
	{
		return compileFunctionCallForm(cons(FUNCALL, cons(list(FUNCTION, sym, END_LIST), CDR(x))), dest);
	}
#if 0	// this is slowing things down
	// first see if the lisp function can handle it
	LispObj a = LispCall(Funcall, COMPILE_FUNCTION_CALL_FORM, x, dest);
	if (a != NIL)
		return T;
#endif

	LispObj retval = T;
	if (sym == PLUS)
		retval = compilePlusFunctionCall(x, dest, resultType);
	else
	if (sym == MINUS)
		retval = compileMinusFunctionCall(x, dest, resultType);
	else
	if (sym == NUMERIC_EQUAL)
		retval = compileNumericEqualFunctionCall(x, dest);
	else
	if (sym == LESS_EQUAL || sym == LESS || sym == GREATER_EQUAL || sym == GREATER)
		retval = compileNumericCompareFunctionCall(x, dest);
	else
	if (sym == CONS)
		retval = compileConsFunctionCall(x, dest);
	else
	if (sym == EQ)
		retval = compileEqFunctionCall(x, dest);
	else
	if (sym == CAR_SYM)
		retval = compileCarFunctionCall(x, dest);
	else
	if (sym == CDR_SYM)
		retval = compileCdrFunctionCall(x, dest);
	else
	if (sym == NULL_SYM || sym == NOT)
		retval = compileNullFunctionCall(x, dest);
	else
	if (sym == ONEPLUS && (listLength(x) == 2))
		retval = compilePlusFunctionCall(cons(PLUS, cons(CAR(CDR(x)), cons(wrapInteger(1), NIL))), dest,
		resultType);
	else
	if (sym == ONEMINUS)
		retval = compileMinusFunctionCall(cons(MINUS, cons(CAR(CDR(x)), cons(wrapInteger(1), NIL))), dest, 
		resultType);
	else
	if (sym == UREF)
		retval = compileUrefFunctionCall(x, dest);
	else
	if (sym == UREF_SET)
		retval = compileUrefSetFunctionCall(x, dest);
	else
	if (sym == AREF)
		retval = compileArefFunctionCall(x, dest);
	else
	if (sym == SETF_AREF)
		retval = compileSetfArefFunctionCall(x, dest);
	else
		retval = compileFunctionCallForm(x, dest);
	return retval;
}

static LispObj compileFunctionCallForm(LispObj x, LispObj dest)
{
	LispObj sym = CAR(x);
	LispObj a = 0;
	LispObj undefinedFunc = 0;
	long nargs = 0;
	long i = 0;

	// first see if the lisp function can handle it
	a = LispCall3(Funcall, COMPILE_FUNCTION_CALL_FORM, x, dest);
	if (a == NIL)
	{
		if (!hasFunctionBinding(sym))
		{
			// assume it is a function which has not been defined yet
			undefinedFunc = LispCall2(Funcall, UNDEFINED_FUNCTION, sym);
			setSymbolFunction(sym, undefinedFunc, FUNCTION);
			if (symbolValue(COMPILER_WARN_ON_UNDEFINED_FUNCTION) != NIL
					&& LispCall2(Funcall, LOOKUP_FTYPE, sym) == NIL
					&& symbolValue(COMPILER_FUNCTION_NAME) != sym)	// recursive case
			{
				LispCall3(Funcall, WARN, stringNode("Function not defined: ~A"), sym);
				/*
				LispCall(Write, stringNode(";;; Warning: function not defined: "),
					symbolValue(STANDARD_OUTPUT));
				LispCall(Write, sym, symbolValue(STANDARD_OUTPUT));
				LispCall(Terpri, symbolValue(STANDARD_OUTPUT));
				*/
			}
			// If value of UNDEFINED-FUNCTIONS is not NIL, then push the symbol and the 
			// stub function onto it.
			// This is useful to LOAD and COMPILE-FILE.
			if (symbolValue(UNDEFINED_FUNCTIONS) != NIL)
				setSymbolValue(UNDEFINED_FUNCTIONS, 
					cons(sym, 
						cons(undefinedFunc, symbolValue(UNDEFINED_FUNCTIONS))));
		}

		a = CDR(x);
		nargs = listLength(a);	// number of arguments
		for (i = 0; i < nargs; i++)
		{
			compileForm(CAR(a), Dest_Stack, T);
			a = CDR(a);
		}

		SET_ARG_COUNT(nargs);					// ecx = number of args
		LOAD_ENVIRONMENT(sym);
		CALL_INDIRECT(sym);
		if (nargs > 0)
		{
			ADD_ESP_NUM(nargs * 4);
		}
		if (dest == Dest_Stack)
			PUSH_EAX();
	}
	return T;
}

static LispObj compileSpecialOperator(LispObj x, LispObj dest, LispObj /*resultType*/)
{
	LispObj sym = CAR(x);
	LispObj retval = T;

	if (sym == QUOTE)
		retval = compileQuoteForm(x, dest);
	else
	if (sym == SETQ)
		retval = compileSetqForm(x, dest);
	else
	if (sym == FUNCTION)
		retval = compileFunctionSpecialOperator(x, dest);
	else
	if (sym == BLOCK)
		retval = compileBlockForm(x, dest);
	else
	if (sym == PROGN)
		retval = compilePrognForm(CDR(x), dest);
	else
	if (sym == IF)
		retval = compileIfForm(x, dest);
	else
	if (sym == LET)
		retval = compileLetForm(x, dest);
	else
	if (sym == LETSTAR)
		retval = compileLetstarForm(x, dest);
	else
	if (sym == TAGBODY)
		retval = compileTagbodyForm(x, dest);
	else
	if (sym == GO)
		retval = compileGoForm(x, dest);
	else
	if (sym == RETURN_FROM)
 		retval = compileReturnFromForm(x, dest);
	else
	if (sym == CATCH)
		retval = compileCatchForm(x, dest);
	else
	if (sym == THROW)
		retval = compileThrowForm(x, dest);
	else
	if (sym == UNWIND_PROTECT)
		retval = compileUnwindProtectForm(x, dest);
	else
	if (sym == MULTIPLE_VALUE_CALL)
		retval = compileMultipleValueCallForm(x, dest);
	else
	if (sym == EVAL_WHEN)
		retval = compileEvalWhenForm(x, dest);
	else
	if (sym == MULTIPLE_VALUE_PROG1)
		retval = compileMultipleValueProg1Form(x, dest);
	else
	if (sym == THE)
		retval = compileTheForm(x, dest);
	else
	if (sym == FLET)
		retval = compileFletForm(x, dest);
	else
	if (sym == LABELS)
		retval = compileLabelsForm(x, dest);
	else
	if (sym == LOCALLY)
		retval = compileLocallyForm(x, dest);
	else
	if (sym == LOAD_TIME_VALUE)
		retval = compileLoadTimeValueForm(x, dest);
	else
	if (sym == GET_CURRENT_ENVIRONMENT)
		retval = compileCurrentEnvironment(x, dest);
	else
	if (sym == CAPTURE_COMPILER_ENVIRONMENT)
		retval = compileCaptureCompilerEnvironmentForm(x, dest);
	else
		Error("Unknown special operator: ~A", sym);
	return retval;
}

static LispObj compileIfForm(LispObj x, LispObj dest)
{
	if (!isCons(CDR(x)) || !isCons(CDR(CDR(x))))
		Error("Invalid IF form--wrong number of arguments: ~A", x);
	LispObj testForm = CAR(CDR(x));
	LispObj thenForm = CAR(CDR(CDR(x)));
	LispObj elseForm = CDR(CDR(CDR(x)));
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	
	if (isCons(elseForm) && isCons(CDR(elseForm)))
 		Error("Invalid IF form--form(s) following ELSE clause: ~A", x);

	// if the test is for numeric equal
	if (isCons(testForm) && CAR(testForm) == NUMERIC_EQUAL && listLength(testForm) == 3)
	{
		// make sure there is an else form if we take this path
		// to ensure NIL gets set as the value in case the condition
		// is false. Add NIL as the else clause in case there wasn't one.
		if (!isCons(elseForm))
			elseForm = cons(NIL, NIL);
		compileForm(testForm, Dest_Zero_Flag, T);
		JNE_RELATIVE(0);					// je else_label
	}
	else
	{
		if (elseForm != NIL /* || dest == Dest_EAX_Operand */)
			compileForm(testForm, Dest_EAX_Operand, T);	// don't need to set arg count
		else
			compileForm(testForm, Dest_EAX, T);
		CMP_EAX_NIL();
		JE_RELATIVE(0);					// je else_label
	}
	addr1 = CURRENT_IP;
//	if (dest == Dest_EAX_Operand)
//		compileForm(thenForm, Dest_EAX_Operand, T);
//	else
		compileForm(thenForm, Dest_EAX, T);
	if (isCons(elseForm))
		JMP_LONG_RELATIVE(0);			// jmp end_label
	addr2 = CURRENT_IP;
	if (isCons(elseForm))
	{
//		if (dest == Dest_EAX_Operand)
//			compileForm(CAR(elseForm), Dest_EAX_Operand, T);
//		else
			compileForm(CAR(elseForm), Dest_EAX, T);
	}
	// now resolve addresses
	CBsetLong(addr1 - wrapInteger(4), addr2 - addr1);
	if (isCons(elseForm))
		CBsetLong(addr2 - wrapInteger(4), CURRENT_IP - addr2);
	if (dest == Dest_Stack)
		PUSH_EAX();

	return T;
}

static LispObj compileQuoteForm(LispObj x, LispObj dest)
{
	LispObj retval = T;
	if (!isCons(CDR(x)))
		Error("Invalid QUOTE form: ~A", x);
	else
		retval = compileLiteralForm(CAR(CDR(x)), dest, T);
	return retval;
}

static LispObj findVariableRecord(LispObj sym)
{

	LispObj stackVars = 0;
	LispObj rec = 0;
	LispObj temp = 0;
	LispObj var = 0;
	LispObj src = 0;
	LispObj ref = 0;
	LispObj offset = 0;

	// see if the function is in the stack environment
	stackVars = CBcleanups();
	while (isCons(stackVars))
	{
		rec = CAR(stackVars);

		if (CAR(rec) == WITH_ONLY_LEXICALS)
		{
			temp = CAR(CDR(rec));
			while (isCons(temp))
			{
				if (CAR(temp) == sym)
					break;
				temp = CDR(temp);
			}
			if (temp == NIL)
				return NIL;	// don't inherit any lexicals passed this point
		}

		if (CAR(rec) == ENVIRONMENT)
			return NIL;

		if (CAR(rec) == FLET || CAR(rec) == LET)
		{
			var = CAR(CDR(rec));
			if (var == sym)
				return rec;
		}
		stackVars = CDR(stackVars);
	}
	return NIL;
}

//
//	Generate code to place the lexical function or variable contents
//	into the destination (it may be an offset from EBP, from EBX,
//	or it may be in the local heap environment).
//	The type argument specifies function (FLET), variable (LET) 
//	or either (T).
//	Returns NIL if the entity was not found, or the type of the entity
//	otherwise (currently this is always T).
//
static LispObj compileLexicalEntity(LispObj sym, LispObj dest, LispObj type)
{
	LispObj stackVars = 0;
	LispObj rec = 0;
	long isEnvironment = 0;
	long envCount = 0;
	LispObj var = 0;
	LispObj src = 0;
	LispObj reg = 0;
	LispObj offset = 0;
	long noffset = 0;
	long isHeap = 0;
	long len = 0;
	long i = 0;
	LispObj code = 0;
	long indirect = 0;
	long base = 0;
	long addr = 0;
	LispObj temp = 0;

	// see if the function is in the compiler environment
	stackVars = symbolValue(COMPILER_ENVIRONMENT);
	if (stackVars != NIL)
	{
		len = integer(UVECTOR(stackVars)[1]);	// get vector length
		for (i = 4 /* skip first 4 cells */; i < len; i += 2)
		{
			if (UVECTOR(stackVars)[i + 2] == sym)
			{
				code = UVECTOR(stackVars)[i + 3];
				indirect = (code >> 29) & 1;
				base = (code >> 27) & 3;
				code &= 0x7ffffff;
				if (code & 0x4000000)	// if code was signed
					code |= 0xf8000000;	// store 1's in upper five bits
				if (base == 0 || base == 1)
				{
					addr = integer(code) + foreignPtr(UVECTOR(stackVars)[2 + base]);
			//		MOV_EAX_NUM(integer(code) + foreignPtr(UVECTOR(stackVars)[2 + base]));  this overflows
					ASM_OP8(0xb8); 
					CBaddLong(addr); // like ASM_LONG(num); but with unwrapped integer
					MOV_EAX_EAX_PTR_WITH_OFFSET(0);
					if (indirect)
						MOV_EAX_EAX_PTR_WITH_OFFSET(-ConsTag);
					TARGET(1);
				}
				else
				if (base == 2) // EDI
				{
					compileLiteralForm(UVECTOR(stackVars)[2 + base], Dest_EAX, T);
					MOV_EDI_EAX();	// edi = environment
					MOV_EDI_EDI_PTR_WITH_OFFSET(8 - UvectorTag + integer(code)); // edi = binding
					MOV_EAX_EDI_PTR_WITH_OFFSET(-ConsTag);	// mov eax, [edi - 4]; get value in eax
					TARGET(1);
				}
				else
				if (base == 3)
				{
					MOV_EAX_ESI_PTR_WITH_OFFSET(integer(code));
					if (indirect)
						MOV_EAX_EAX_PTR_WITH_OFFSET(-ConsTag);
					TARGET(1);
				}
				return T;
			}
		}
	}

	// see if the function is in the stack environment
	stackVars = CBcleanups();
	while (isCons(stackVars))
	{
		rec = CAR(stackVars);

		if (CAR(rec) == WITH_ONLY_LEXICALS)
		{
			if (type != LET)
				return NIL;

			temp = CAR(CDR(rec));
			while (isCons(temp))
			{
				if (CAR(temp) == sym)
					break;
				temp = CDR(temp);
			}
			if (temp == NIL)
				return NIL;	// don't inherit any lexicals passed this point
		}

		if (!isEnvironment && CAR(rec) == ENVIRONMENT)
		{
			isEnvironment = 1;
			envCount = -1;
			stackVars = CDR(stackVars);
			continue;
		}
		
		if (CAR(rec) == SPECIAL && type != FLET)
		{
			for (LispObj p = CAR(CDR(rec)); isCons(p); p = CDR(p))
				if (CAR(p) == sym)
					return NIL;		// a special declaration is shadowing any variable declarations
		}

		if (CAR(rec) == FLET || CAR(rec) == LET)
		{
			var = CAR(CDR(rec));
			src = CAR(CDR(CDR(rec)));
			reg = CAR(src);
			offset = CAR(CDR(src));
			if (isCons(offset))
			{
				isHeap = 1;
				noffset = integer(CAR(offset));
			}
			else
			{
				isHeap = 0;
				noffset = integer(offset);
			}
			if (isEnvironment && isHeap)
				envCount++;

			if (var == sym && (dest == T || CAR(rec) == type))
			{
				if (isEnvironment)
				{
					if (isHeap)		// the function was found in the parent environment
					{
						setSymbolValue(COMPILER_USES_ENV, T);
						addCompilerFrameInfo(sym, wrapInteger(2) /* edi */, 
								wrapInteger(/*8 - UvectorTag + */envCount * 4), wrapInteger(1) /* indirect */);

						MOV_EDI_ENV();	// edi = environment
						MOV_EDI_EDI_PTR_WITH_OFFSET(8 - UvectorTag + (envCount * 4)); // edi = binding
						MOV_EAX_EDI_PTR_WITH_OFFSET(-ConsTag);	// mov eax, [edi - 4]; get value in eax
						TARGET(1);
						return (type == LET) ? findVariableType(sym) : findFunctionType(sym);
					}
					else
					{
						stackVars = CDR(stackVars);
						continue;
					}
				}
				else
				{
					if (dest == Dest_EAX || dest == Dest_EAX_Operand)
					{
						if (CAR(src) == EBX)	// mov eax, [ebx + offset]
						{	
							MOV_EAX_EBX_PTR_WITH_OFFSET(noffset);	
						}
						else
						if (CAR(src) == EBP)	// mov eax, [ebp + offset]
						{
							MOV_EAX_EBP_PTR_WITH_OFFSET(noffset);
						}
						else
							Error("Compiler: Invalid variable source: ~A", src);
				
						if (isHeap)
						{
							MOV_EDI_EAX();
							MOV_EAX_EDI_PTR_WITH_OFFSET(-4);
						}
						if (dest == Dest_EAX)
							SET_ARG_COUNT(1);
					}
					else
					if (dest == Dest_Stack)
					{
						if (CAR(src) == EBX)	// push [ebx + offset]
						{
							PUSH_EBX_PTR_WITH_OFFSET(noffset);
						}
						else
						if (CAR(src) == EBP)	// push [ebp + offset]
						{
							PUSH_EBP_PTR_WITH_OFFSET(noffset);
						}
						else
							Error("Compiler: Invalid variable source: ~A", src);
						if (isHeap)
						{
							POP_EDI();			// pop edi
							PUSH_EDI_PTR_WITH_OFFSET(-4);	// push dword ptr [edi - 4]
						}
					}
				}
				CAR(CDR(CDR(CDR(rec)))) += wrapInteger(1);	// inc number of references
				return (type == LET) ? findVariableType(sym) : findFunctionType(sym);
			}
		}
		stackVars = CDR(stackVars);
	}
	return NIL;
}

//
//	Like compileLexicalEntity(), above, but compiles the cons cell
//	which holds the variable binding.
// 
static LispObj compileLexicalSlot(LispObj sym, LispObj dest, LispObj type)
{
	LispObj stackVars = 0;
	LispObj rec = 0;
	long isEnvironment = 0;
	long envCount = 0;
	LispObj var = 0;
	LispObj src = 0;
	LispObj reg = 0;
	LispObj offset = 0;
	long noffset = 0;
	long isHeap = 0;
	LispObj temp = 0;

	// see if the function is in the stack environment
	stackVars = CBcleanups();
	while (isCons(stackVars))
	{
		rec = CAR(stackVars);
		if (CAR(rec) == WITH_ONLY_LEXICALS)
		{
			if (type != LET)
				return NIL;

			temp = CAR(CDR(rec));
			while (isCons(temp))
			{
				if (CAR(temp) == sym)
					break;
				temp = CDR(temp);
			}
			if (temp == NIL)
				return NIL;	// don't inherit any lexicals passed this point
		}

		if (!isEnvironment && CAR(rec) == ENVIRONMENT)
		{
			isEnvironment = 1;
			envCount = -1;
			stackVars = CDR(stackVars);
			continue;
		}

		if (CAR(rec) == FLET || CAR(rec) == LET)
		{
			var = CAR(CDR(rec));
			src = CAR(CDR(CDR(rec)));
			reg = CAR(src);
			offset = CAR(CDR(src));
			if (isCons(offset))
			{
				isHeap = 1;
				noffset = integer(CAR(offset));
			}
			else
			{
				isHeap = 0;
				noffset = integer(offset);
			}
			if (isEnvironment && isHeap)
				envCount++;

			if (var == sym && (dest == T || CAR(rec) == type))
			{
				if (isEnvironment)
				{
					if (isHeap)		// the function was found in the parent environment
					{
						setSymbolValue(COMPILER_USES_ENV, T);
						addCompilerFrameInfo(sym, wrapInteger(2) /* edi */, 
								wrapInteger(/*8 - UvectorTag + */envCount * 4), wrapInteger(1) /* indirect */);

						MOV_EDI_ENV();	// edi = environment
						MOV_EDI_EDI_PTR_WITH_OFFSET(8 - UvectorTag + (envCount * 4)); // edi = binding
						MOV_EAX_EDI();
						TARGET(1);
						return T;
					}
					else
					{
						stackVars = CDR(stackVars);
						continue;
					}
				}
				else
				{
					if (dest == Dest_EAX || dest == Dest_EAX_Operand)
					{
						if (CAR(src) == EBX)	// mov eax, [ebx + offset]
						{	
							MOV_EAX_EBX_PTR_WITH_OFFSET(noffset);	
						}
						else
						if (CAR(src) == EBP)	// mov eax, [ebp + offset]
						{
							MOV_EAX_EBP_PTR_WITH_OFFSET(noffset);
						}
						else
							Error("Compiler: Invalid variable source: ~A", src);
						/*
						if (!isHeap)
							LispCall(Funcall, WARN,
								stringNode("The symbol ~A does not contain a heap binding"),
								sym);
						*/
						if (dest == Dest_EAX)
							SET_ARG_COUNT(1);
					}
					else
					if (dest == Dest_Stack)
					{
						if (CAR(src) == EBX)	// push [ebx + offset]
						{
							PUSH_EBX_PTR_WITH_OFFSET(noffset);
						}
						else
						if (CAR(src) == EBP)	// push [ebp + offset]
						{
							PUSH_EBP_PTR_WITH_OFFSET(noffset);
						}
						else
							Error("Compiler: Invalid variable source: ~A", src);
						/*
						if (!isHeap)
							Error("The symbol ~A does not contain a heap binding", sym);
						*/
					}
				}
				CAR(CDR(CDR(CDR(rec)))) += wrapInteger(1);	// inc number of references
				return T;
			}
		}
		stackVars = CDR(stackVars);
	}
	return NIL;
}

//
//	Returns a list of currently referenced variabls, including
//  stack, environment, and special variables.
//	Each element of the list consists of 4 elements:
//		Name
//		Register (ENV, ARGS, FRAME or GLOBAL i.e. EDI, EBX, EBP or ESI)
//		Offset
//		Indirect-flag  (i.e reg+offset is a binding, a CONS with the value
//						in the CAR, as opposed to a direct value)
//
LispObj getEnvironmentVars()
{
	LispObj envars = NIL;
	LispObj specialVars = 0;
	LispObj sym = 0;
	LispObj reg = 0;
	LispObj offset = 0;
	LispObj indirect = 0;
	LispObj stackVars = 0;
	LispObj rec = 0;
	long isEnvironment = 0;
	long envCount = 0;
	LispObj src = 0;

	specialVars = symbolValue(COMPILED_SPECIALS);
	while (isCons(specialVars))
	{
		sym = CAR(specialVars);
		reg = GLOBAL;
		offset = UVECTOR(sym)[SYMBOL_VAR_TABLE] * 4;
		indirect = T;
		envars = cons(list(sym, reg, offset, indirect, END_LIST), envars);
		specialVars = CDR(specialVars);
	}

	stackVars = CBcleanups();
	while (isCons(stackVars))
	{
		rec = CAR(stackVars);
		if (!isEnvironment && CAR(rec) == ENVIRONMENT)
		{
			isEnvironment = 1;
			envCount = -1;
			stackVars = CDR(stackVars);
			continue;
		}

		if (CAR(rec) == LET)
		{
			sym = CAR(CDR(rec));
			src = CAR(CDR(CDR(rec)));
			if (CAR(src) == EBP)
				reg = FRAME;
			else
			if (CAR(src) == EBX)
				reg = ARGS;
			else
				reg = NIL;

			offset = CAR(CDR(src));
			if (isCons(offset))
			{
				indirect = T;
				offset = CAR(offset);
			}
			else
				indirect = NIL;

			if (isEnvironment && (indirect == T))
			{
				envCount++;
				reg = ENV;
				offset = wrapInteger(envCount * 4);
				envars = cons(list(sym, reg, offset, indirect, END_LIST), envars);
			}
			else
			if (!isEnvironment)
				envars = cons(list(sym, reg, offset, indirect, END_LIST), envars);
		}
		stackVars = CDR(stackVars);
	}
	return envars;
}

// Returns the number of slots, and slot information,
// including both lexical variable and
// lexical functions, that will be needed in the function environment
// structure.
// Returns a list of three items:
// 1) wrapped integer indicating the total number of slots
// 2) list of lexical variables and slot indices
// 3) list of lexical functions and slot indices
//
static LispObj lexEnvironmentSlots(LispObj env)
{
	LispObj p = 0;
 	LispObj q = 0;
	int lexSize = 0;
 	LispObj type = 0;
 	LispObj sym = 0;
 	LispObj offset = 0;
 	LispObj place = 0;
	LispObj vars = NIL;
	LispObj funcs = NIL;

	p = env;
	while (isCons(p))
	{
		q = CAR(p);
		type = CAR(q);
		if (type == LET || type == FLET)
		{
			sym = CAR(CDR(q));
			place = CAR(CDR(CDR(q)));
			offset = CAR(CDR(place));
			if (isCons(offset))
			{
				if (type == LET)
					vars = cons(sym, cons(wrapInteger(lexSize * 4), vars));
				else
					funcs = cons(sym, cons(wrapInteger(lexSize * 4), funcs));
				lexSize++;
			}
		}
		p = CDR(p);
	}
 	return list(wrapInteger(lexSize), vars, funcs, END_LIST);
}

// Given an environment info list (from cleanups) compiles an
// environment. If the passed callback is a function, it will 
// be called as each slot is created (at compile time) with the
// name of each slot passed as the single argument.
static LispObj compileLexEnvironment(LispObj env, LispObj callback)
{
	int lexSize = 0;
	LispObj slotInfo = 0;
	LispObj funcs = 0;
	LispObj vars = 0;
	long destoffset = 0;
	LispObj p = 0;

	slotInfo = lexEnvironmentSlots(env);
	lexSize = integer(CAR(slotInfo));
	vars = CAR(CDR(slotInfo));
	funcs = CAR(CDR(CDR(slotInfo)));

	if (lexSize > 0)
	{
		PUSH_NUM(lexSize + 1);			// push lexSize (+ 1 for structure header)
		CALL_INDIRECT(ALLOC_VECTOR);
		ADD_ESP_NUM(4);
		MOV_EDI_EAX();
		OR_EDI_PTR_WITH_OFFSET_NUM(-5, (StructureType << 3));
		MOV_EAX_ESI_PTR_WITH_OFFSET(ENVIRONMENT_Index * 4);
		MOV_EDI_PTR_WITH_OFFSET_EAX(4 - UvectorTag);
		PUSH_EDI();

		destoffset = -UvectorTag + 8;
		p = vars;
		while (isCons(p))
		{
			// if there is a callback, call it now
			if (isFunction(callback))
				LispCall2(Funcall, callback, CAR(p));

			// put variable contents in EAX
			compileLexicalSlot(CAR(p), Dest_EAX_Operand, LET);
			MOV_EDI_ESP_PTR_WITH_OFFSET(0);
 			MOV_EDI_PTR_WITH_OFFSET_EAX(destoffset + integer(CAR(CDR(p))));
			p = CDR(CDR(p));
		}
		p = funcs;
		while (isCons(p))
		{
			// if there is a callback, call it now
			if (isFunction(callback))
				LispCall2(Funcall, callback, CAR(p));

			// put variable contents in EAX
			compileLexicalSlot(CAR(p), Dest_EAX_Operand, FLET);
			MOV_EDI_ESP_PTR_WITH_OFFSET(0);
 			MOV_EDI_PTR_WITH_OFFSET_EAX(destoffset + integer(CAR(CDR(p))));
			p = CDR(CDR(p));
		}
	}
	else
		PUSH_NIL();		// push environment (NIL)
	return 0;
}

static LispObj compileFunctionSpecialLambda(
	LispObj lambda, LispObj env, LispObj lexicalMacros, LispObj lexicalSymbolMacros, LispObj callback, 
	LispObj compiledEnv, LispObj dest)
{
	LispObj t = 0;
	LispObj clambda = 0;
	LispObj lexMacros = NIL;
	LispObj lexSymbolMacros = NIL;

	// see if there were any lexical macros active for this lambda (as deduced
	// by the macroexpand-all call earlier
	if (isCons(lexicalMacros))
	{
		t = lexicalMacros;
		while (isCons(t) && CAR(t) != lambda)
			t = CDR(t);
		if (isCons(t))
			lexMacros = CAR(CDR(t));
	}

	// see if there were any lexical symbol macros active for this lambda (as deduced
	// by the macroexpand-all call earlier
	if (isCons(lexicalSymbolMacros))
	{
		t = lexicalSymbolMacros;
		while (isCons(t) && CAR(t) != lambda)
			t = CDR(t);
		if (isCons(t))
			lexSymbolMacros = CAR(CDR(t));
	}

	clambda = compileLambdaExpression(lambda, NIL, cons(cons(ENVIRONMENT, NIL), env), lexMacros, lexSymbolMacros);

	compileLiteralForm(clambda, Dest_Stack, T);	// push function
	if (compiledEnv)
		compileLiteralForm(compiledEnv, Dest_Stack, T);
	else
		compileLexEnvironment(env, callback);

	SET_ARG_COUNT(2);			// 2 args
	LOAD_ENVIRONMENT(CREATE_CLOSURE);
	CALL_INDIRECT(CREATE_CLOSURE);
	ADD_ESP_NUM(2 * 4);

	TARGET(1);
	return T;
}

#if 1
static LispObj compileFunctionSpecialOperator(LispObj x, LispObj dest)
{
	LispObj f = 0;
	LispObj g = 0;
	LispObj retval = T;
	LispObj lexicalMacros = NIL;
	LispObj lexicalSymbolMacros = NIL;
	LispObj callback = NIL;
	LispObj env = 0;

	if (!isCons(CDR(x)))
		Error("Invalid FUNCTION form: ~A", x);
	f = CAR(CDR(x));
	if (isLambdaForm(f))	// compile it and create a closure
	{
		if (isCons(CDR(CDR(x))))
			g = CAR(CDR(CDR(x)));
		if (isCons(g))
		{
			if (CAR(g) == CAPTURE_COMPILER_ENVIRONMENT)
			{
				// we have a previously captured compilation environment,
				// so use that. It should be in this form:
				// (CAPTURE_COMPILER_ENVIRONMENT cleanups lexical-macros callback compiled-env)
				//
				retval = compileFunctionSpecialLambda(f, 
					CAR(CDR(g)), CAR(CDR(CDR(g))), CAR(CDR(CDR(CDR(g)))), 
					CAR(CDR(CDR(CDR(CDR(CDR(g)))))), 
                    CAR(CDR(CDR(CDR(CDR(g))))), dest);
				return retval;
			}
			else
				Error("Invalid FUNCTION form: ~A", x);
		}
		lexicalMacros = symbolValue(COLLECT_LEXICAL_MACROS);
		lexicalSymbolMacros = symbolValue(COLLECT_LEXICAL_SYMBOL_MACROS);
		callback = symbolValue(CAPTURED_LEXICAL_CALLBACK);
		env = CBcleanups();
		retval = compileFunctionSpecialLambda(f, env, lexicalMacros, lexicalSymbolMacros, callback, 0, dest);
	}
	else
	if (isSymbol(f))
	{
		// see if the function is in the stack environment
		retval = compileLexicalEntity(f, dest, FLET);
		if (retval == NIL)
		{
			// not found--assume a global function definition
			retval = compileFunctionExpressionForm(
				list(SYMBOL_FUNCTION_SYM, list(QUOTE, f, END_LIST), END_LIST), dest, T);
		}
	}
	else
	if (isCons(f) && CAR(f) == SETF && isCons(CDR(f)) && isSymbol(CAR(CDR(f))))
		retval = compileFunctionExpressionForm(
				list(LOOKUP_SETF_FUNCTION, list(QUOTE, CAR(CDR(f)), END_LIST), END_LIST), dest, T);
	else
		Error("Invalid FUNCTION form: ~A", x);
	return retval;
}
#else
static LispObj
compileFunctionSpecialOperator(LispObj x, LispObj dest)
{
	LispObj clambda = 0;
	LispObj lex = 0;
	long lexSize = 0;
	LispObj p = 0;
 	LispObj q = 0;
 	LispObj type = 0;
 	LispObj sym = 0;
 	LispObj offset = 0;
 	LispObj place = 0;
	LispObj f = 0;
	LispObj retval = T;
	LispObj vars = NIL;
	LispObj funcs = NIL;
	LispObj lexicalMacros = NIL;
	LispObj t = 0;
	LispObj lexMacros = NIL;
	LispObj callback = NIL;

	long destoffset = 0;
	lexicalMacros = symbolValue(COLLECT_LEXICAL_MACROS);
	callback = symbolValue(CAPTURED_LEXICAL_CALLBACK);

	if (!isCons(CDR(x)))
		Error("Invalid FUNCTION form: ~A", x);
	f = CAR(CDR(x));
	if (isLambdaForm(f))
	{
		// compile it and create a closure
		lex = CBcleanups();

		// see if there were any lexical macros active for this lambda (as deduced
		// by the macroexpand-all call earlier
		if (isCons(lexicalMacros))
		{
			t = lexicalMacros;
			while (isCons(t) && CAR(t) != f)
				t = CDR(t);
			if (isCons(t))
				lexMacros = CAR(CDR(t));
		}
		clambda = compileLambdaExpression(f, NIL, cons(cons(ENVIRONMENT, NIL), lex), lexMacros);

		// create a run-time environment

		// go through list once to determine the size of the
		// environment
		p = lex;
		lexSize = 0;
		while (isCons(p))
		{
			q = CAR(p);
			type = CAR(q);
			if (type == LET || type == FLET)
			{
				sym = CAR(CDR(q));
				place = CAR(CDR(CDR(q)));
//				reg = CAR(place);
				offset = CAR(CDR(place));
				if (isCons(offset))
				{
					if (type == LET)
						vars = cons(sym, cons(wrapInteger(lexSize * 4), vars));
					else
						funcs = cons(sym, cons(wrapInteger(lexSize * 4), funcs));
					lexSize++;
				}
			}
			p = CDR(p);
		}
 		lexSize++;	// always need one extra slot for struct info

		compileLiteralForm(clambda, Dest_Stack, T);	// push function

		if (lexSize > 1)
		{
			PUSH_NUM(lexSize);			// push lexSize
			CALL_INDIRECT(ALLOC_VECTOR);
			ADD_ESP_NUM(4);
			MOV_EDI_EAX();
			OR_EDI_PTR_WITH_OFFSET_NUM(-5, (StructureType << 3));
			MOV_EAX_ESI_PTR_WITH_OFFSET(ENVIRONMENT_Index * 4);
			MOV_EDI_PTR_WITH_OFFSET_EAX(4 - UvectorTag);
			PUSH_EDI();

			destoffset = -UvectorTag + 8;
			p = vars;
			while (isCons(p))
			{
				// if there is a callback, call it now
				if (isFunction(callback))
					LispCall(Funcall, callback, CAR(p));

				// put variable contents in EAX
				compileLexicalSlot(CAR(p), Dest_EAX_Operand, LET);
				MOV_EDI_ESP_PTR_WITH_OFFSET(0);
 				MOV_EDI_PTR_WITH_OFFSET_EAX(destoffset + integer(CAR(CDR(p))));
				p = CDR(CDR(p));
			}
			p = funcs;
			while (isCons(p))
			{
				// if there is a callback, call it now
				if (isFunction(callback))
					LispCall(Funcall, callback, CAR(p));

				// put variable contents in EAX
				compileLexicalSlot(CAR(p), Dest_EAX_Operand, FLET);
				MOV_EDI_ESP_PTR_WITH_OFFSET(0);
 				MOV_EDI_PTR_WITH_OFFSET_EAX(destoffset + integer(CAR(CDR(p))));
				p = CDR(CDR(p));
			}
		}
		else
			PUSH_NIL();		// push environment (NIL)

		SET_ARG_COUNT(2);			// 2 args
		LOAD_ENVIRONMENT(CREATE_CLOSURE);
		CALL_INDIRECT(CREATE_CLOSURE);
		ADD_ESP_NUM(2 * 4);

		TARGET(1);
	}
	else
	if (isSymbol(f))
	{
		// see if the function is in the stack environment
		retval = compileLexicalEntity(f, dest, FLET);
		if (retval == NIL)
		{
			// not found--assume a global function definition
			retval = compileFunctionExpressionForm(
				list(SYMBOL_FUNCTION_SYM, list(QUOTE, f, END_LIST), END_LIST), dest, T);
		}
	}
	else
	if (isCons(f) && CAR(f) == SETF && isCons(CDR(f)) && isSymbol(CAR(CDR(f))))
	{
			retval = compileFunctionExpressionForm(
				list(LOOKUP_SETF_FUNCTION, list(QUOTE, CAR(CDR(f)), END_LIST), END_LIST), dest, T);
	}
	else
		Error("Invalid FUNCTION form: ~A", x);
	return retval;
}
#endif

static LispObj compileSetqForm(LispObj x, LispObj dest)
{
	LispObj pairs = CDR(x);
	LispObj src = 0;
	LispObj reg = 0;
	LispObj offset = 0;
	LispObj rec = 0;
	long noffset = 0;
	long isHeap = 0;
	long isEnvironment = 0;
	LispObj stackVars = 0;
	long envCount = 0;
	LispObj sym = 0;
	LispObj val = 0;
	LispObj valType = 0;
	LispObj var = 0;
	LispObj retval = T;
	long len = 0;
	long i = 0;
	LispObj code = 0;
	long indirect = 0;
	long base = 0;
	long addr = 0;
	LispObj temp = 0;

	// handle case of (setq)
	if (!isCons(pairs))
		return compileNil(dest);

	while (isCons(pairs) && isCons(CDR(pairs)))
	{
continue_loop:
		sym = CAR(pairs);
		val = CAR(CDR(pairs));
		pairs = CDR(CDR(pairs));
		isEnvironment = 0;
		isHeap = 0;

		checkSymbol(sym);
		valType = compileForm(val, Dest_EAX_Operand, T);
		retval = valType;

		// see if the function is in the compiler environment
		stackVars = symbolValue(COMPILER_ENVIRONMENT);
		if (stackVars != NIL)
		{
			len = integer(UVECTOR(stackVars)[1]);	// get vector length
			for (i = 4 /* skip first 4 cells */; i < len; i += 2)
			{
				if (UVECTOR(stackVars)[i + 2] == sym)
				{
					code = UVECTOR(stackVars)[i + 3];
					indirect = (code >> 29) & 1;
					base = (code >> 27) & 3;
					code &= 0x7ffffff;
					if (code & 0x4000000)	// if code was signed
						code |= 0xf8000000;	// store 1's in upper five bits
					if (base == 0 || base == 1)
					{
						addr = integer(code) + foreignPtr(UVECTOR(stackVars)[2 + base]);
						MOV_EDX_EAX();
				//		MOV_EAX_NUM(integer(code) + foreignPtr(UVECTOR(stackVars)[2 + base]));  this overflows
						ASM_OP8(0xb8); 
						CBaddLong(addr); // like ASM_LONG(num); but with unwrapped integer
						if (indirect)
						{
							MOV_EAX_EAX_PTR_WITH_OFFSET(0);
							MOV_EAX_PTR_WITH_OFFSET_EDX(-ConsTag);
						}
						else
							MOV_EAX_PTR_WITH_OFFSET_EDX(0);
						MOV_EAX_EDX();
						TARGET(1);
					}
					else
					if (base == 2) // EDI
					{
						MOV_EDX_EAX();
						compileLiteralForm(UVECTOR(stackVars)[2 + base], Dest_EAX, T);
						MOV_EDI_EAX();	// edi = environment
						MOV_EAX_EDX();
						MOV_EDI_EDI_PTR_WITH_OFFSET(8 - UvectorTag + integer(code)); // edi = binding
						MOV_EDI_PTR_WITH_OFFSET_EAX(-ConsTag);	// mov [edi - 4], edx
						TARGET(1);
					}
					else
					if (base == 3)
					{
						MOV_EDX_EAX();
						if (indirect)
						{
							MOV_EAX_ESI_PTR_WITH_OFFSET(integer(code));
							MOV_EAX_PTR_WITH_OFFSET_EDX(-ConsTag);
							MOV_EAX_EDX();
						}
						else
						{
							MOV_EAX_EDX();
							MOV_ESI_PTR_WITH_OFFSET_EAX(0);
						}
						TARGET(1);
					}
					if (isCons(pairs) && isCons(CDR(pairs)))
						goto continue_loop;
					else
						goto exit;
				}
			}
		}

		// see if the symbol is in the stack environment
		stackVars = CBcleanups();
		while (isCons(stackVars))
		{
			rec = CAR(stackVars);
			if (CAR(rec) == WITH_ONLY_LEXICALS)
			{
				temp = CAR(CDR(rec));
				while (isCons(temp))
				{
					if (CAR(temp) == sym)
						break;
					temp = CDR(temp);
				}
				if (temp == NIL)
					return NIL;	// don't inherit any lexicals passed this point
			}

			if (!isEnvironment && CAR(rec) == ENVIRONMENT)
			{
				isEnvironment = 1;
				envCount = -1;
				stackVars = CDR(stackVars);
				continue;
			}


			if (CAR(rec) == FLET || CAR(rec) == LET)
			{
				var = CAR(CDR(rec));
				src = CAR(CDR(CDR(rec)));
				reg = CAR(src);
				offset = CAR(CDR(src));
				if (isCons(offset))
				{
					noffset = integer(CAR(offset));
					isHeap = 1;
				}
				else
				{
					noffset = integer(offset);
					isHeap = 0;
				}

				if (isHeap && isEnvironment)
					envCount++;

				if (var == sym && (!isEnvironment || isHeap) && CAR(rec) != FLET)
				{	// found it
					if (isEnvironment)
					{
						setSymbolValue(COMPILER_USES_ENV, T);
						addCompilerFrameInfo(sym, wrapInteger(2) /* edi */, 
								wrapInteger(/*8 - UvectorTag + */envCount * 4), wrapInteger(1) /* indirect */);

						// the variable was found in the parent enironment
						MOV_EDI_ENV();		// edi = environment
						MOV_EDI_EDI_PTR_WITH_OFFSET(8 - UvectorTag + (envCount * 4)); // edi = binding
						MOV_EDI_PTR_WITH_OFFSET_EAX(-ConsTag);
					}
					else	// not environment
					{
						if (isHeap)
						{
							if (reg == EBX)		// mov edi, [ebx + offset]
							{
								MOV_EDI_EBX_PTR_WITH_OFFSET(noffset);
							}
							else
							if (reg == EBP)	// mov edi, [ebp + offset]
							{
								MOV_EDI_EBP_PTR_WITH_OFFSET(noffset);
							}
							else
								Error("Compiler: Invalid variable register: ~A", reg);

							MOV_EDI_PTR_WITH_OFFSET_EAX(-ConsTag);
						}
						else	// not a heap var
						{
							if (reg == EBX)	// mov [ebx + offset], eax
							{
								MOV_EBX_PTR_WITH_OFFSET_EAX(noffset);
							}
							else
							if (reg == EBP)	// mov [ebp + offset], eax
							{
								MOV_EBP_PTR_WITH_OFFSET_EAX(noffset);
							}
							else
								Error("Compiler: Invalid variable register: ~A", reg);
						}
					}
					if (isCons(pairs) && isCons(CDR(pairs)))
						goto continue_loop;
					else
						goto exit;
				}		// endif found it
			}	// endif let form
			stackVars = CDR(stackVars);
		}

		// lexical variable not found, assume global/special
		// if no symbol table entry, create one now
		if (isConstantSymbol(sym))
			Error("The symbol ~A has been declared constant, and may not be assigned to", sym);
		if (!symbolVarTableIndex(sym))
			createSymbolTableEntry(sym);

		if (!isSpecialSymbol(sym) && (Cmember(sym, symbolValue(LAMBDA_SPECIAL_DECS)) == NIL))
		{
			// warn of symbol assumed special if compiling a lambda
			if (symbolValue(COMPILER_WARN_ON_ASSUMED_SPECIAL) != NIL
				&& symbolValue(COMPILING_LAMBDA) != NIL)
			{
				LispCall3(Funcall, WARN, stringNode("Symbol ~S assumed special"), sym);
				/*
				LispCall(Write, stringNode(";;; Warning: Symbol "));
				LispCall(Write, sym);
				LispCall(Write, stringNode(" assumed special"));
				LispCall(Terpri);
				*/
			}
		}

		// setSymbolValue(COMPILED_SPECIALS, cons(sym, symbolValue(COMPILED_SPECIALS)));
		MOV_EDI_SYMBOL_BINDING(sym);
		MOV_EDI_PTR_WITH_OFFSET_EAX(-ConsTag);
	}	

	if (isCons(pairs))
		Error("Incorrect number of arguments to SETQ: ~A", x);
exit:
	TARGET(1);
	return retval;
}

static LispObj compileLiteralForm(LispObj x, LispObj dest, LispObj /*resultType*/)
{
	LispObj retval = T;
	if (isSymbol(x) || isFixnum(x) || isFunction(x) || isCharacter(x) || isShortFloat(x))
	{
		if (dest == Dest_EAX || dest == Dest_EAX_Operand)
		{
			MOV_EAX_OBJ(x);
			if (isSymbol(x) || isFunction(x))
				CBref(x, wrapInteger(CURRENT_IP_RAW - 4));	// add symbol reference
			if (dest == Dest_EAX)
				SET_ARG_COUNT(1);
		}
		else
		if (dest == Dest_Stack)
		{
			PUSH_OBJ(x);
			if (isSymbol(x) || isFunction(x))
				CBref(x, wrapInteger(CURRENT_IP_RAW - 4));	// add symbol reference
		}

		if (isSymbol(x))
			retval = SYMBOL;
		else
		if (isFixnum(x))
			retval = FIXNUM;
		else
		if (isFunction(x))
			retval = FUNCTION;
		else
		if (isCharacter(x))
			retval = CHARACTER;
	}
	else
	{
		if (dest == Dest_EAX || dest == Dest_EAX_Operand)
		{
			MOV_EAX_NUM(x);
			CBref(x, wrapInteger(CURRENT_IP_RAW - 4));	// add reference
			if (dest == Dest_EAX)
				SET_ARG_COUNT(1);
		}
		else
		if (dest == Dest_Stack)
		{
			PUSH_NUM(x);
			CBref(x, wrapInteger(CURRENT_IP_RAW - 4));	// add reference
		}
	}
	return retval;
}

LispObj compileForm(LispObj x, LispObj dest, LispObj resultType)
{
	LispObj retval = T;
	if (isCons(x))
		retval = compileList(x, dest, resultType);
	else
	if (isSymbol(x))
		retval = compileSymbol(x, dest, resultType);
	else
		retval = compileLiteralForm(x, dest, resultType);
	return retval;
}

static LispObj compileBlockForm(LispObj block, LispObj dest)
{
	long len = listLength(block);
	LispObj label = 0;
	LispObj blockForm = 0;
	LispObj p = 0;
	LispObj branchAddress = 0;
	LispObj srcAddress = 0;
	LispObj dstAddress = 0;
	LispObj embeddedReturns = 0;
	LispObj returns = 0;
	LispObj tempSym = 0;
	LispObj lambdas = 0;
	LispObj returnForm = 0;
	LispObj labels = 0;

	if (len < 2)
		Error("Invalid block form: ~A", block);
	
	label = CAR(CDR(block));
	if (!isSymbol(label))
		Error("Invalid BLOCK label: ~A", label);

	// in case an embedded lambda has a (return-from block-name) in it
	if (true /*label != NIL*/)
	{
		lambdas = findLambdas(block);
		embeddedReturns = NIL;
		for (; isCons(lambdas); lambdas = CDR(lambdas))
		{
			// find any inner scope instances of this same label
			labels = findBlockLabels(CDR(CDR(block)), CAR(lambdas), NIL);
			if (isCons(Cmember(label, labels)))
				continue;	// found an inner label, so we won't worry about embedded returns at this level

			returns = findEmbeddedReturnForms(label, CAR(lambdas), NIL);
			for (; isCons(returns); returns = CDR(returns))
			{	
				returnForm = CAR(returns);
				embeddedReturns = cons(returnForm, embeddedReturns);
			}
		}
		if (embeddedReturns != NIL)
		{
			tempSym = LispCall2(Funcall, FUNCALL, GENSYM);
			CBaddBlock(list(label, NIL, tempSym, END_LIST));
			while (isCons(embeddedReturns))
			{
				// transform (return-from label value) to
				//		(throw sym value)
				if (CAR(CAR(embeddedReturns)) == RETURN_FROM)
				{
					CAR(CAR(embeddedReturns)) = THROW;
					CAR(CDR(CAR(embeddedReturns))) = tempSym;
				}
				else if (CAR(CAR(embeddedReturns)) == RETURN)
				{
					CAR(CAR(embeddedReturns)) = THROW;
					CDR(CAR(embeddedReturns)) = cons(tempSym, CDR(CAR(embeddedReturns)));	// splice in NIL block name
				}
				embeddedReturns = CDR(embeddedReturns);
			}

			// generate (let ((sym (cons 0 0))) (catch sym ...))
			//
			compileLetForm(list(LET, 
				list(
					list(tempSym, 
						list(CONS, 0, 0, END_LIST), 
						END_LIST),
					END_LIST),
				list(CATCH, tempSym, cons(PROGN, CDR(CDR(block))), END_LIST),
				END_LIST), dest);
			if (symbolValue(COMPILER_WARN_ON_DYNAMIC_RETURN) != NIL)
				LispCall3(Funcall, WARN,
					stringNode("Had to compile a catch form for a block header: ~A"),
					label);
		}
		else
		{
			CBaddBlock(list(label, NIL, NIL, END_LIST));
			compilePrognForm(CDR(CDR(block)), Dest_EAX);
		}
	}
	else
	{
		CBaddBlock(list(label, NIL, NIL, END_LIST));
		compilePrognForm(CDR(CDR(block)), Dest_EAX);
	}

	blockForm = CBpopCleanup();

	// examine block returns to see if we need to update any code
	p = CAR(CDR(CAR(CDR(blockForm))));
	while (isCons(p))
	{
		branchAddress = CAR(p);
		if (!isInteger(branchAddress))
			Error("Internal compiler error--invalid return branch: ~A", branchAddress);

		// replace with a relative branch address
		srcAddress = branchAddress + wrapInteger(4);
		dstAddress = CURRENT_IP;
		CBsetLong(branchAddress, dstAddress - srcAddress);
		p = CDR(p);
	}
	if (dest == Dest_Stack)
	{
		PUSH_EAX();
	}
	return T;
}

static LispObj compilePrognForm(LispObj forms, LispObj dest)
{
	LispObj retval = T;
	if (!isCons(forms))
		retval = compileNil(dest);
	else
	{
		if (isCons(CDR(forms)))
		{
			while (isCons(forms))
			{
				// if there is another form following, and it isn't a GO special
				// form, then omit setting the arg count
//				if (isCons(CDR(forms)) && CAR(CDR(forms)) != GO)
//					retval = compileForm(CAR(forms), Dest_EAX_Operand, T);
//				else	
					retval = compileForm(CAR(forms), Dest_EAX, T);
				forms = CDR(forms);
			}
			if (dest == Dest_Stack)
				PUSH_EAX();
		}
		else
			retval = compileForm(CAR(forms), dest, T);
	}
	return retval;
}

static LispObj compileNil(LispObj dest)
{
	if (dest == Dest_EAX)
	{
		MOV_EAX_NIL();
		SET_ARG_COUNT(1);
	}
	else
	if (dest == Dest_EAX_Operand)
	{
		MOV_EAX_NIL();
	}
	else
	if (dest == Dest_Stack)
	{
		PUSH_NIL();
	}
	else
		Error("Invalid destination", 0);
	return SYMBOL;
}

static LispObj compileLetForm(LispObj x, LispObj dest)
{
	LispObj localVars = 0;
	LispObj letForms = 0;
	LispObj p = 0;
	LispObj localBindings = NIL;
	LispObj t = 0;
	long numLocals = 0;
	LispObj sym = 0;
	LispObj envOffset = 0;
	long i = 0;
	long dynamicBind = 0;
	long specials = 0;
	LispObj specialVars = NIL;
	LispObj f = 0;
	LispObj specialDecs = NIL;
	LispObj declarationStatements = NIL;
	LispObj declarations = 0;
	LispObj decForm = 0;
	LispObj compilerBindings = NIL;
	LispObj numTypeDeclarations = 0;
	LispObj retval = T;
	LispObj saveSpecialDecls = 0;

	if (!isCons(CDR(x)))
		Error("Invalid LET form: ~A", x);
	localVars = CAR(CDR(x));
	letForms = CDR(CDR(x));
	p = localVars;

	// look for declarations
	f = letForms;
	while (isCons(f))
	{
		if (isCons(CAR(f)) && (CAR(CAR(f)) == DECLARE))
			declarationStatements = cons(CAR(f), declarationStatements);
		else
			break;
		f = CDR(f);
	}
	letForms = f;			// skip past declarations

	// check the declarations for compiler optimization settings
	compilerBindings = processOptimizeDeclarations(declarationStatements);

	// check the declarations for type declarations
	numTypeDeclarations = processTypeDeclarations(declarationStatements);

	// Search declarations for special declarations.
	// specialDecs contains list of arguments declared special.
	specialDecs = processSpecialDeclarations(declarationStatements);

	while (isCons(p))
	{
		sym = CAR(p);
		if (!isCons(sym) && !isSymbol(sym))
			Error("Invalid LET variable: ~A", sym);
		envOffset = CBincDynamicEnvSize(wrapInteger(4));
		if (isSymbol(sym) || !isCons(CDR(sym)))
		{
			if (!isSymbol(sym))
				sym = CAR(sym);
			compileNil(Dest_EAX_Operand);
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(envOffset));
		}
		else
		{
			compileForm(CAR(CDR(sym)), Dest_EAX_Operand, T);
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(envOffset));
			sym = CAR(sym);
		}
		
		if (!isSymbol(sym))
			Error("Invalid symbol in LET form: ~A", sym);

		if (isSpecialSymbol(sym) || (Cmember(sym, specialDecs) != NIL))
			dynamicBind = 1;
		else
			dynamicBind = 0;

		if (!dynamicBind)
		{
			t = list(sym, EBP, envOffset, END_LIST);
			localBindings = cons(t, localBindings);
			numLocals++;
		}
		else
		{
			// special variable
			specials++;
 			if (!symbolVarTableIndex(sym))
				createSymbolTableEntry(sym);
			setSymbolValue(COMPILED_SPECIALS, cons(sym, symbolValue(COMPILED_SPECIALS)));

			compileLiteralForm(sym, Dest_Stack, T);	// push symbol
			PUSH_EBP_PTR_WITH_OFFSET(integer(envOffset));	// push value
			specialVars = cons(sym, specialVars);
		}

		p = CDR(p);
	}

	// add the bindings
	p = localBindings;
	while (isCons(p))
	{
		CBaddStackVar(CAR(CAR(p)), CDR(CAR(p)));
		p = CDR(p);
	}
	compileVariableHeapBindings(localBindings);

	// introduce new special declarations
	saveSpecialDecls = symbolValue(LAMBDA_SPECIAL_DECS);
	setSymbolValue(LAMBDA_SPECIAL_DECS,
		LispCall2(lispAppend, specialDecs, symbolValue(LAMBDA_SPECIAL_DECS)));

	// if any special variables are present, add those bindings now
	if (specials)
	{ 
		CBpushCleanup(list(SPECIAL, specialVars, END_LIST));

		MOV_EDI_NIL();	  // null environment
		SET_ARG_COUNT(specials * 2);
		CALL_INDIRECT(PUSH_SPECIAL_BINDINGS);
		ADD_ESP_NUM((specials * 2) * 4);

		compileLiteralForm(specialVars, Dest_Stack, T);
		CALL_INDIRECT(ESTABLISH_SPECIAL_BINDINGS);
		ADD_ESP_NUM(4);
	}
	// handle ignore declarations
	processIgnoreDeclarations(declarationStatements);
	retval = compilePrognForm(letForms, Dest_EAX);

	if (specials)
	{
		PUSH_EAX();
		PUSH_ECX();
		compileLiteralForm(specialVars, Dest_Stack, T);
		MOV_EDI_NIL();				// null environment
		SET_ARG_COUNT(1);
		CALL_INDIRECT(POP_SPECIAL_BINDINGS);
		ADD_ESP_NUM(4 * 1);
		CALL_INDIRECT(POP_SPECIALS);
		POP_ECX();
		POP_EAX();

		CBpopCleanup();
	}

	if (dest == Dest_Stack)
		PUSH_EAX();

	// remove the variables
	for (i = 0; i < numLocals; i++)
		CBpopCleanup();

	setSymbolValue(LAMBDA_SPECIAL_DECS, saveSpecialDecls);

	if (compilerBindings != NIL)
		restoreOptimizeDeclarationBindings(compilerBindings);
	popTypeDeclarations(numTypeDeclarations);
	return retval;
}

static LispObj compileLetstarForm(LispObj x, LispObj dest)
{
	LispObj localVars = 0;
	LispObj letForms = 0;
	LispObj p = 0;
	long numLocals = 0;
	LispObj sym = 0;
	LispObj envOffset = 0;
	LispObj t = 0;
	long i = 0;
	long dynamicBind = 0;
	long specials = 0;
	LispObj specialVars = NIL;
	LispObj f = 0;
	LispObj specialDecs = NIL;
	LispObj declarationStatements = NIL;
	LispObj declarations = 0;
	LispObj decForm = 0;
	LispObj compilerBindings = NIL;
	LispObj numTypeDeclarations = 0;
	LispObj retval = T;
	LispObj symList = 0;
	LispObj saveSpecialDecls = 0;

	if (!isCons(CDR(x)))
		Error("Invalid LET* form: ~A", x);
	localVars = CAR(CDR(x));
	letForms = CDR(CDR(x));
	p = localVars;

	// look for declarations
	f = letForms;
	while (isCons(f))
	{
		if (isCons(CAR(f)) && (CAR(CAR(f)) == DECLARE))
			declarationStatements = cons(CAR(f), declarationStatements);
		else
			break;
		f = CDR(f);
	}
	letForms = f;			// skip past declarations

	// check the declarations for compiler optimization settings
	compilerBindings = processOptimizeDeclarations(declarationStatements);

	// check the declarations for type declarations
	numTypeDeclarations = processTypeDeclarations(declarationStatements);

	// Search declarations for special declarations.
	// specialDecs contains list of arguments declared special.
	specialDecs = processSpecialDeclarations(declarationStatements);

	// introduce new special declarations
	saveSpecialDecls = symbolValue(LAMBDA_SPECIAL_DECS);
	setSymbolValue(LAMBDA_SPECIAL_DECS,
		LispCall2(lispAppend, specialDecs, symbolValue(LAMBDA_SPECIAL_DECS)));

	while (isCons(p))
	{
		sym = CAR(p);
		if (!isCons(sym) && !isSymbol(sym))
			Error("Invalid LET* variable: ~A", sym);
		envOffset = CBincDynamicEnvSize(wrapInteger(4));
		if (isSymbol(sym) || !isCons(CDR(sym)))
		{
			if (!isSymbol(sym))
				sym = CAR(sym);
			compileNil(Dest_EAX_Operand);
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(envOffset));
		}
		else
		{
			compileForm(CAR(CDR(sym)), Dest_EAX_Operand, T);
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(envOffset));
			sym = CAR(sym);
		}

		if (!isSymbol(sym))
			Error("Invalid symbol in LET* form: ~A", sym);

		t = list(sym, EBP, envOffset, END_LIST);

		if (isSpecialSymbol(sym) || (Cmember(sym, specialDecs) != NIL))
			dynamicBind = 1;
		else
			dynamicBind = 0;

		if (!dynamicBind)
		{
			CBaddStackVar(sym, CDR(t));
			compileVariableHeapBindings(cons(t, NIL));
			numLocals++;
		}
		else
		{
			// special variable
			specials++;
 			if (!symbolVarTableIndex(sym))
				createSymbolTableEntry(sym);
			setSymbolValue(COMPILED_SPECIALS, cons(sym, symbolValue(COMPILED_SPECIALS)));
			compileLiteralForm(sym, Dest_Stack, T);	// push symbol
			PUSH_EBP_PTR_WITH_OFFSET(integer(envOffset));	// push value
			MOV_EDI_NIL();	  // null environment
			SET_ARG_COUNT(2);
			CALL_INDIRECT(PUSH_SPECIAL_BINDINGS);
			ADD_ESP_NUM(2 * 4);
			specialVars = cons(sym, specialVars);
			symList = cons(sym, NIL);
			CBpushCleanup(list(SPECIAL, symList, END_LIST));
			compileLiteralForm(symList, Dest_Stack, T);
			SET_ARG_COUNT(1);
			CALL_INDIRECT(ESTABLISH_SPECIAL_BINDINGS);
			ADD_ESP_NUM(4);
		}
		p = CDR(p);
	}

	// handle ignore declarations
	processIgnoreDeclarations(declarationStatements);
	retval = compilePrognForm(letForms, Dest_EAX);

	if (specials)
	{
		PUSH_EAX();
		PUSH_ECX();
		compileLiteralForm(specialVars, Dest_Stack, T);
		MOV_EDI_NIL();				// null environment
		SET_ARG_COUNT(1);
		CALL_INDIRECT(POP_SPECIAL_BINDINGS);
		ADD_ESP_NUM(4 * 1);
		for (i = 0; i < specials; i++)
			CALL_INDIRECT(POP_SPECIALS);
		POP_ECX();
		POP_EAX();

		for (i = 0; i < specials; i++)
			CBpopCleanup();
	}

	if (dest == Dest_Stack)
		PUSH_EAX();

	setSymbolValue(LAMBDA_SPECIAL_DECS, saveSpecialDecls);

	// remove the variables
	for (i = 0; i < numLocals; i++)
		CBpopCleanup();

	if (compilerBindings != NIL)
		restoreOptimizeDeclarationBindings(compilerBindings);
	popTypeDeclarations(numTypeDeclarations);
	return retval;
}

//
//	Common Lisp LOCALLY special operator.
//	This is currently implemented similarly to LET but with no local
//  variables.
//
static LispObj compileLocallyForm(LispObj x, LispObj dest)
{
	LispObj forms = 0;
	LispObj f = 0;
	LispObj declarationStatements = NIL;
	LispObj compilerBindings = NIL;
	LispObj numTypeDeclarations = 0;
	LispObj declarations = 0;
	LispObj decForm = 0;
	LispObj specialDecs = NIL;
	LispObj retval = T;

	forms = CDR(x);

	// look for declarations
	f = forms;
	while (isCons(f))
	{
		if (isCons(CAR(f)) && (CAR(CAR(f)) == DECLARE))
			declarationStatements = cons(CAR(f), declarationStatements);
		else
			break;
		f = CDR(f);
	}
	forms = f;			// skip past declarations

	// check the declarations for compiler optimization settings
	compilerBindings = processOptimizeDeclarations(declarationStatements);

	// check the declarations for type declarations
	numTypeDeclarations = processTypeDeclarations(declarationStatements);

	// Search declarations for special declarations.
	// specialDecs contains list of arguments declared special.
	//
	f = declarationStatements;
	while (isCons(f))
	{
		declarations = CDR(CAR(f));
		while (isCons(declarations))
		{
			decForm = CAR(declarations);
			if (isCons(decForm) && (CAR(decForm) == SPECIAL))
				specialDecs = LispCall2(lispAppend, CDR(decForm), specialDecs);
			declarations = CDR(declarations);
		}
		f = CDR(f);
	}

	// handle ignore declarations
	processIgnoreDeclarations(declarationStatements);
	retval = compilePrognForm(forms, Dest_EAX);

	if (dest == Dest_Stack)
		PUSH_EAX();

	if (compilerBindings != NIL)
		restoreOptimizeDeclarationBindings(compilerBindings);
	popTypeDeclarations(numTypeDeclarations);
	return retval;
}

//
//	Common Lisp LOAD-TIME-VALUE special operator.
//  The optional second argument is ignored, since we don't do coalescing
//  anyway.
//
static LispObj compileLoadTimeValueForm(LispObj x, LispObj dest)
{
	LispObj form = 0;
	LispObj readOnlyP = 0;
	LispObj func = 0;
	LispObj result = 0;
	LispObj evaluatedForm = 0;

	if (!isCons(CDR(x)))
		Error("Invalid LOAD-TIME-VALUE form: missing form to be evaluated");
	form = CAR(CDR(x));
	if (isCons(CDR(CDR(x))))
		readOnlyP = CAR(CDR(CDR(x)));
    evaluatedForm = eval(form, NIL);        // evaluate it now

    result = compileLiteralForm(evaluatedForm, dest, T);

    // add a LOAD_TIME_VALUES entry to the information for the function
    setSymbolValue(LOAD_TIME_VALUES, cons(form, cons(CBLastRefPos(), symbolValue(LOAD_TIME_VALUES))));

	return result;
}

static LispObj compileFletForm(LispObj x, LispObj dest)
{
	LispObj localFuns = NIL;
	LispObj fletForms = NIL;
	LispObj declarations = NIL;
	LispObj localDecls = NIL;
	LispObj f = 0;
	LispObj g = 0;
	LispObj p = 0;
	LispObj funcName = 0;
	LispObj funcArgs = 0;
	LispObj funcForms = 0;
	LispObj t = 0;
	LispObj localBindings = NIL;
	LispObj retval = T;
	LispObj envOffset = 0;
	long numLocals = 0;
	long i = 0;

	if (listLength(x) < 2)
		Error("Invalid FLET form: ~A", x);

	localFuns = CAR(CDR(x));
	fletForms = CDR(CDR(x));
	p = localFuns;

	while (isCons(fletForms))
	{
		f = CAR(fletForms);
		if (isCons(f) && CAR(f) == DECLARE)
			declarations = cons(f, declarations);
		else
			break;
		fletForms = CDR(fletForms);
	}

	while (isCons(p))
	{
		f = CAR(p);
		if (listLength(f) < 2)
			Error("Invalid FLET function expression: ~A", f);
	
		funcName = CAR(f);
		funcArgs = CAR(CDR(f));
		funcForms = CDR(CDR(f));

		// gather declarations for the local function
		localDecls = NIL;
		while (isCons(funcForms))
		{
			g = CAR(funcForms);
			if (isCons(g) && CAR(g) == DECLARE)
				localDecls = cons(g, localDecls);
			else
				break;
			funcForms = CDR(funcForms);
		}

		if (!isSymbol(funcName) || !isList(funcArgs))
			Error("Invalid FLET function expression: ~A", f);

		envOffset = CBincDynamicEnvSize(wrapInteger(4));

		compileFunctionSpecialOperator(
			list(FUNCTION, 
				cons(LAMBDA, 
					cons(funcArgs, 
						LispCall2(lispAppend, 
							localDecls, 
							list(cons(BLOCK, cons(funcName, funcForms)), END_LIST)))), 
				END_LIST), 
			Dest_EAX_Operand);

		MOV_EBP_PTR_WITH_OFFSET_EAX(integer(envOffset));

		t = list(funcName, EBP, envOffset, END_LIST);
		localBindings = cons(t, localBindings);
		numLocals++;
		p = CDR(p);
	}

	// add the bindings
	p = localBindings;
	while (isCons(p))
	{
		CBaddStackFunction(CAR(CAR(p)), CDR(CAR(p)));
		p = CDR(p);
	}
	compileFunctionHeapBindings(localBindings);

	retval = compilePrognForm(fletForms, Dest_EAX);

	if (dest == Dest_Stack)
		PUSH_EAX();

	// remove the variables
	for (i = 0; i < numLocals; i++)
		CBpopCleanup();
	return retval;
}

static LispObj compileLabelsForm(LispObj x, LispObj dest)
{
	LispObj localFuns = NIL;
	LispObj fletForms = NIL;
	LispObj declarations = NIL;
	LispObj localDecls = NIL;
	LispObj f = 0;
	LispObj g = 0;
	LispObj p = 0;
	LispObj q = 0;
	LispObj funcName = 0;
	LispObj funcArgs = 0;
	LispObj funcForms = 0;
	LispObj t = 0;
	LispObj localBindings = NIL;
	LispObj retval = T;
	LispObj envOffset = 0;
	long numLocals = 0;
	long i = 0;
	LispObj pos = 0;

	if (listLength(x) < 2)
		Error("Invalid LABELS form: ~A", x);

	localFuns = CAR(CDR(x));
	fletForms = CDR(CDR(x));

	while (isCons(fletForms))
	{
		f = CAR(fletForms);
		if (isCons(f) && CAR(f) == DECLARE)
			declarations = cons(f, declarations);
		else
			break;
		fletForms = CDR(fletForms);
	}

	p = localFuns;
	while (isCons(p))
	{
		f = CAR(p);
		if (listLength(f) < 2)
			Error("Invalid LABELS function expression: ~A", f);
	
		funcName = CAR(f);
		if (!isSymbol(funcName))
			Error("Invalid LABELS function expression: ~A", f);

		envOffset = CBincDynamicEnvSize(wrapInteger(4));

		t = list(funcName, EBP, envOffset, END_LIST);
		localBindings = cons(t, localBindings);
		numLocals++;
		p = CDR(p);
	}
	
	// add the bindings
	localBindings = Cnreverse(localBindings);
	p = localBindings;
	while (isCons(p))
	{
		CBaddStackFunction(CAR(CAR(p)), CDR(CAR(p)));
		p = CDR(p);
	}
	compileFunctionHeapBindings(localBindings);

	p = localFuns;
	while (isCons(p))
	{
		f = CAR(p);	
		funcName = CAR(f);
		funcArgs = CAR(CDR(f));

		// gather declarations for the local function
		funcForms = CDR(CDR(f));
		localDecls = NIL;
		while (isCons(funcForms))
		{
			g = CAR(funcForms);
			if (isCons(g) && CAR(g) == DECLARE)
				localDecls = cons(g, localDecls);
			else
				break;
			funcForms = CDR(funcForms);
		}

		if (!isSymbol(funcName) || !isList(funcArgs))
			Error("Invalid LABELS function expression: ~A", f);

		// find the position indicator i.e. (EBP 12)
		q = localBindings;
		pos = 0;
		while (isCons(q))
		{
			if (CAR(CAR(q)) == funcName)
			{
				pos = CDR(CAR(q));
				break;
			}
			q = CDR(q);
		}
		if (!pos)
			Error("Compiler error in LABELS form--could not find function ~A", funcName);

		compileFunctionSpecialOperator(
			list(FUNCTION, 
				cons(LAMBDA, 
					cons(funcArgs, 
						LispCall2(lispAppend, 
							localDecls, 
							list(cons(BLOCK, cons(funcName, funcForms)), END_LIST)))), 
				END_LIST), 
			Dest_EAX_Operand);

		if (isInteger(CAR(CDR(pos))))
		{
			MOV_EBP_PTR_WITH_OFFSET_EAX(integer(CAR(CDR(pos))));
		}
		else
		{
			// store in heap binding
			checkInteger(CAR(CAR(CDR(pos))));
			MOV_EDX_EAX();
			MOV_EAX_EBP_PTR_WITH_OFFSET(integer(CAR(CAR(CDR(pos)))));
			MOV_EAX_PTR_WITH_OFFSET_EDX(-4);
		}
		p = CDR(p);
	}

	retval = compilePrognForm(fletForms, Dest_EAX);

	if (dest == Dest_Stack)
		PUSH_EAX();

	// remove the variables
	for (i = 0; i < numLocals; i++)
		CBpopCleanup();
	return retval;
}

//
//	For each found tag, create a list consisting of 3 items:
//	tag
//	address (once we get to it during compilation)
//	list of go statements whose instructions must be updated
//		to point to that address
//
LispObj compileTagbodyForm(LispObj x, LispObj dest)
{
	LispObj forms = CDR(x);
 	LispObj p = forms;
	LispObj tags = NIL;
	LispObj tagForms = NIL;
	LispObj t = 0;
	LispObj form = 0;
 	LispObj tagForm = 0;
	LispObj branches = 0;
	LispObj q = 0;
	LispObj branchAddress = 0;
	LispObj srcAddress = 0;
	LispObj dstAddress = 0;
	LispObj retval = T;
	long goForms = 0;
	LispObj embeddedGoForms = 0;
	LispObj lambdas = 0;
	LispObj foundTags = 0;
	LispObj newsym = 0;

	if (forms == NIL)
	{
		return compileNil(dest);
	}

	// go through list collecting tags
	while (isCons(p))
	{
		if (isLispInteger(CAR(p)) || isSymbol(CAR(p)))
		{
			tagForms = cons(list(CAR(p), NIL, NIL, END_LIST), tagForms);
			tags = cons(CAR(p), tags);
		}
		if (isCons(CAR(p)) && CAR(CAR(p)) == GO)
			goForms++;
		p = CDR(p);
	}
	tags = Cnreverse(tags);
	tagForms = Cnreverse(tagForms);

	// if no evaluating forms (forms which are not a tag or a go form), 
	// the tagbody returns NIL
	if (listLength(tagForms) == listLength(forms))
		return compileNil(dest);
	else
	if ((listLength(tagForms) + goForms) == listLength(forms))
		compileNil(Dest_EAX);

	CBaddLabels(tagForms);

	// in case an embedded lambda has a (go tag) to one of these tags in it
	if (tagForms != NIL)
	{
		embeddedGoForms = findEmbeddedGoForms(tags, symbolValue(EMBEDDED_LAMBDAS));
		if (embeddedGoForms != NIL)
		{
			// go through the tagbody forms, looking for guilty parties
			p = forms;
			while (isCons(p))
			{
				lambdas = findLambdas(CAR(p));
				if (lambdas != NIL)
				{
					// find any tags present in these lambdas
					foundTags = NIL;
					q = tags;
					while (isCons(q))
					{
						embeddedGoForms = findEmbeddedGoForms(CAR(q), lambdas); // look for specific tag
						if (embeddedGoForms != NIL)
						{
							// need to wrap this form with a CATCH block
							// <form>
							//	becomes
							//	(if (let ((sym (cons 0 0)))
							//			(eq (catch sym <form>) sym)) 
							//		(go tag))
							newsym = LispCall2(Funcall, FUNCALL, GENSYM);
							t = list(EQ, list(CATCH, newsym, CAR(p), END_LIST), newsym, END_LIST);
							t = list(LET, 
									list(
										list(newsym, 
											list(CONS, 0, 0, END_LIST), 
											END_LIST),
										END_LIST),
									t,
									END_LIST);
							t = list(IF,
									t,
									list(GO, CAR(q), END_LIST),
									END_LIST);

							CAR(p) = t;

							while (isCons(embeddedGoForms))
							{
								CAR(CAR(embeddedGoForms)) = THROW;
								CAR(CDR(CAR(embeddedGoForms))) = newsym;
								CDR(CDR(CAR(embeddedGoForms))) = cons(newsym, NIL);
								embeddedGoForms = CDR(embeddedGoForms);
							}
						}
						q = CDR(q);
					}
				}
				p = CDR(p);
			}
			if (symbolValue(COMPILER_WARN_ON_DYNAMIC_RETURN) != NIL)
				LispCall2(Funcall, WARN,
					stringNode("Had to compile a catch form for a tagbody tag"));
		}
	}

	p = forms;
	t = tagForms;
	while (isCons(p))
	{
		form = CAR(p);
		if (isCons(t) && form == CAR(CAR(t)))
		{
			tagForm = CAR(t);
			CAR(CDR(tagForm)) = CURRENT_IP;// get current code address
			t = CDR(t);
		}
		else
		{
			// try to omit the setting the return count
//			if (isCons(CDR(p)) && isCons(CAR(CDR(p))) && CAR(CAR(CDR(p))) != GO)
//				compileForm(form, Dest_EAX_Operand, T);
//			else
				compileForm(form, Dest_EAX, T);
		}
		p = CDR(p);
	}
	compileNil(Dest_EAX);		// tagbody returns NIL

	if (dest == Dest_Stack)
		PUSH_EAX();

	// examine tags to see if any were referenced--update
	// the code as necessary
	p = CBpopCleanup();
	p = CAR(CDR(p));
	while (isCons(p))
	{
		tagForm = CAR(p);
		branches = CAR(CDR(CDR(tagForm)));
		q = branches;
		while (isCons(q))
		{
			branchAddress = CAR(q);
			if (!isInteger(branchAddress))
				Error("Internal compiler error--invalid branch: ~A", branchAddress);

			// replace with a relative branch address
			srcAddress = branchAddress + wrapInteger(4);
			dstAddress = CAR(CDR(tagForm));
			CBsetLong(branchAddress, dstAddress - srcAddress);
			q = CDR(q);
		}
		p = CDR(p);
	}
	return retval;
}

static LispObj compileGoForm(LispObj x, LispObj dest)
{
	LispObj tag = 0;
	LispObj forms = 0;
	LispObj form = 0;
	LispObj type = 0;
	LispObj tagForms = 0;
	LispObj tagForm = 0;

	LispObj cleanup = 0;
	LispObj cleanupFunc = 0;
	LispObj numCodeBytes = 0;
	LispObj numReferences = 0;
	LispObj cbuf = 0;
	LispObj refs = 0;
	long numbytes = 0;
	long i = 0;
	LispObj saveAddress = 0;
	long numrefs = 0;
	long stackDepth = 0;
	long stackDifference = 0;
	LispObj tagbodyRecord = 0;

	if (!isCons(CDR(x)))
		Error("Invalid GO form--missing tag: ~A", x);
	tag = CAR(CDR(x));
	if (!isLispInteger(tag) && !isSymbol(tag))
		Error("Invalid GO tag: ~A", tag);

	// find the tag form
	tagbodyRecord = findTagbodyTag(tag);

	// if we couldn't find the block record, then throw the return value
	if (tagbodyRecord == NIL)
	{
		tagbodyRecord = findTagIncludingOuterLambdas(tag);
		if (tagbodyRecord != NIL)
			return compileThrowForm(
				list(THROW, 
					list(QUOTE, tag, END_LIST), 
					list(QUOTE, tag, END_LIST), 
					END_LIST), dest);
		else
			Error("No destination tag named ~A was found", tag);
	}

	// find the tag form
	forms = CBcleanups();
	while (isCons(forms))
	{
		form = CAR(forms);
		type = CAR(form);
		if (type == TAGBODY)
		{
			tagForms = CAR(CDR(form));
			stackDepth = integer(CAR(CDR(CDR(form))));
			while (isCons(tagForms))
			{
				tagForm = CAR(tagForms);
				if (CAR(tagForm) == tag ||
					// catch case of a bignum as a tag
					(isBignum(tag) && isBignum(CAR(tagForm)) && lispNumericEqual(tag, CAR(tagForm))))
				{
					// found the tag
					stackDifference = integer(CBstackIndex()) - stackDepth;
					if (stackDifference < 0)
						Error("Invalid stack index found compiling GO form: ~A", 
							wrapInteger(stackDifference));
					if (stackDifference > 0)
					{
						ADD_ESP_NUM(stackDifference);
						CBincStackIndex(wrapInteger(stackDifference)); // no net change
					}
					JMP_LONG_RELATIVE(0);		// jmp  tag
					CAR(CDR(CDR(tagForm))) = 
						cons(wrapInteger(CURRENT_IP_RAW - 4), CAR(CDR(CDR(tagForm))));
					return T;
				}
				tagForms = CDR(tagForms);
			}
		}
		else
		if (type == CATCH)
		{
			CALL_INDIRECT(POP_CATCHER);
		}
		else
		if (type == UNWIND_PROTECT)
		{
			// include cleanup code
			// we rebind the cleanups to effectively remove the cleanup
			// for the unwind-protect we are currently processing
			cleanup = CDR(form);
			pushDynamicBinding(COMPILER_CLEANUPS, CAR(CDR(cleanup)));
			establishSpecialBindings(list(COMPILER_CLEANUPS, END_LIST));
			PUSH_EAX();
			PUSH_ECX();
			PUSH_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
			CALL_INDIRECT(POP_CATCHER);
			compilePrognForm(CAR(cleanup), Dest_EAX);
			POP_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
			POP_ECX();
			POP_EAX();
			popDynamicBinding(COMPILER_CLEANUPS);
			popSpecials();
		}
		else
		if (type == SPECIAL)
		{
			PUSH_EAX();
			PUSH_ECX();
			compileLiteralForm(CAR(CDR(form)), Dest_Stack, T);
			MOV_EDI_NIL();				// null environment	
			SET_ARG_COUNT(1);
			CALL_INDIRECT(POP_SPECIAL_BINDINGS);
			ADD_ESP_NUM(4 * 1);
			CALL_INDIRECT(POP_SPECIALS);
			POP_ECX();
			POP_EAX();
		}

		forms = CDR(forms);
	}
	Error("Target of GO not found: ~A", x);
	return T;
}


static LispObj compileCurrentEnvironment(LispObj x, LispObj dest)
{
	long numvars = 0;
	LispObj currEnv = 0;
	LispObj var = 0;
	long index = 0;
	LispObj sym = 0;
	LispObj reg = 0;
	LispObj offset = 0;
	LispObj indirect = 0;

	currEnv = getEnvironmentVars();

	// determine the number of variables in the environment
	numvars = listLength(currEnv);

	// generate compiled code to allocate a vector, in EAX, and
	// load up the information into it, 5 slots for each variable:
	//    name, current-value, register, offset, indirect-flag
	PUSH_EDI();
	PUSH_NUM(numvars * 5 + 1);
	CALL_INDIRECT(ALLOC_VECTOR);
	ADD_ESP_NUM(4);
	MOV_EDI_EAX();
	OR_EDI_PTR_WITH_OFFSET_NUM(-5, (SimpleVectorType << 3));
	MOV_EAX_NUM(wrapInteger(numvars * 5));
	MOV_EDI_PTR_WITH_OFFSET_EAX(4 - UvectorTag);
	MOV_EDX_EDI();
	MOV_EDI_NUM(8 - UvectorTag);

	while (isCons(currEnv))
	{
		var = CAR(currEnv);
		sym = CAR(var);
		reg = CAR(CDR(var));
		offset = CAR(CDR(CDR(var)));
		indirect = CAR(CDR(CDR(CDR(var))));

		compileLiteralForm(sym, Dest_EAX_Operand, T);
		MOV_EDI_PLUS_EDX_PTR_WITH_OFFSET_EAX(0);
		ADD_EDI_NUM(4);
		if (reg == ENV)
			PUSH_EDI();
		compileForm(sym, Dest_EAX_Operand, T);	// evaluate it to get the value
		if (reg == ENV)
			POP_EDI();
		MOV_EDI_PLUS_EDX_PTR_WITH_OFFSET_EAX(0);
		ADD_EDI_NUM(4);
		compileLiteralForm(reg, Dest_EAX_Operand, T);
		MOV_EDI_PLUS_EDX_PTR_WITH_OFFSET_EAX(0);
		ADD_EDI_NUM(4);
		compileLiteralForm(offset, Dest_EAX_Operand, T);
		MOV_EDI_PLUS_EDX_PTR_WITH_OFFSET_EAX(0);
		ADD_EDI_NUM(4);
		compileLiteralForm(indirect, Dest_EAX_Operand, T);
		MOV_EDI_PLUS_EDX_PTR_WITH_OFFSET_EAX(0);
		ADD_EDI_NUM(4);

		currEnv = CDR(currEnv);	
	}
	POP_EDI();
	MOV_EAX_EDX();
	if (dest == Dest_Stack)
		PUSH_EAX();
	if (dest == Dest_EAX)
		SET_ARG_COUNT(1);

	return T;
}

#if 0
static LispObj findAnyBlock()
{
	LispObj forms = CBcleanups();
	LispObj form = 0;
	while (isCons(forms))
	{
		form = CAR(forms);
		if (CAR(form) == LAMBDA)
			return NIL;
		if (CAR(form) == BLOCK)
			return form;
		forms = CDR(forms);
	}
	return NIL;
}
#endif

static LispObj findBlock(LispObj label)
{
	LispObj forms = CBcleanups();
	LispObj form = 0;
	while (isCons(forms))
	{
		form = CAR(forms);
		if (CAR(form) == LAMBDA)
			return NIL;
		if (CAR(form) == BLOCK && CAR(CAR(CDR(form))) == label)
			return form;
		forms = CDR(forms);
	}
	return NIL;
}

static LispObj findBlockIncludingOuterLambdas(LispObj label)
{
	LispObj forms = CBcleanups();
	LispObj form = 0;
	while (isCons(forms))
	{
		form = CAR(forms);
		if (CAR(form) == BLOCK && CAR(CAR(CDR(form))) == label)
			return form;
		forms = CDR(forms);
	}
	return NIL;
}

static LispObj findTagbodyTag(LispObj tag)
{
	LispObj forms = CBcleanups();
	LispObj form = 0;
	LispObj p = 0;
	while (isCons(forms))
	{
		form = CAR(forms);
		if (CAR(form) == LAMBDA)
			return NIL;
		if (CAR(form) == TAGBODY)
		{
			p = CAR(CDR(form));		// p = list of tag forms
			while (isCons(p))
			{
				if (CAR(CAR(p)) == tag ||
					// catch case of a bignum as a tag
					(isBignum(tag) && isBignum(CAR(CAR(p))) && lispNumericEqual(tag, CAR(CAR(p)))))
					return CAR(p);	// return the tag form
				p = CDR(p);
			}
		}
		forms = CDR(forms);
	}
	return NIL;
}

static LispObj findTagIncludingOuterLambdas(LispObj tag)
{
	LispObj forms = CBcleanups();
	LispObj form = 0;
	LispObj p = 0;
	while (isCons(forms))
	{
		form = CAR(forms);
		if (CAR(form) == TAGBODY)
		{
			p = CAR(CDR(form));		// p = list of tag forms
			while (isCons(p))
			{
				if (CAR(CAR(p)) == tag)
					return CAR(p);	// return the tag form
				p = CDR(p);
			}
		}
		forms = CDR(forms);
	}
	return NIL;
}

static LispObj compileReturnFromForm(LispObj x, LispObj dest)
{
	LispObj label = 0;
	LispObj retx = NIL;
	LispObj forms = 0;
	LispObj form = 0;
	LispObj type = 0;
	LispObj cleanup = 0;
	LispObj cleanupFunc = 0;
	LispObj numCodeBytes = 0;
	LispObj numReferences = 0;
	LispObj cbuf = 0;
	LispObj refs = 0;
	LispObj retval = T;
	long numbytes = 0;
	long i = 0;
	LispObj saveAddress = 0;
	long numrefs = 0;
	LispObj blockForm = 0;
	long stackDepth = 0;
	long stackDifference = 0;
	LispObj blockRecord = 0;
	long saveCurrValues = 0;

	if (!isCons(CDR(x)))
		Error("Invalid RETURN-FROM form--missing block label: ~A", x);
	label = CAR(CDR(x));
	if (isCons(CDR(CDR(x))))
		retx = CAR(CDR(CDR(x)));
	if (!isSymbol(label))
		Error("Invalid BLOCK label: ~A", label);

	// find the block label
//	if (label == NIL)
//		blockRecord = findAnyBlock();
//	else
		blockRecord = findBlock(label);

	// if we couldn't find the block record, then throw the return value
	if (blockRecord == NIL)
	{
		blockRecord = findBlockIncludingOuterLambdas(label);
		if (blockRecord != NIL)
			return compileThrowForm(
				list(THROW, 
					CAR(CDR(CDR(CAR(CDR(blockRecord))))),
					retx, 
					END_LIST), dest);
		else
			Error("No enclosing block named ~A was found", label);
	}
	else
		label = CAR(CAR(CDR(blockRecord)));

	retval = compileForm(retx, dest, T);
	forms = CBcleanups();

	// determine if cleanup code is necessary
	while (isCons(forms))
	{
		form = CAR(forms);
		type = CAR(form);
		if ((type == BLOCK && form == blockRecord) || type == LAMBDA)
		{
			saveCurrValues = 0;
			break;
		}
		else
		if (type == CATCH || type == UNWIND_PROTECT || type == SPECIAL)
		{
			saveCurrValues = 1;
			break;
		}
		forms = CDR(forms);
	}
			
	if (saveCurrValues)
	{
		PUSH_EAX();
		PUSH_ECX();
		PUSH_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
	}
	forms = CBcleanups();
	while (isCons(forms))
	{
		form = CAR(forms);
		type = CAR(form);
		if (type == BLOCK)
		{
			blockForm = CAR(CDR(form));
			if (CAR(blockForm) == label)
			{
				// found the label
				stackDepth = integer(CAR(CDR(CDR(form))));
				if (saveCurrValues)
				{
					POP_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
					POP_ECX();
					POP_EAX();
				}
				stackDifference = integer(CBstackIndex()) - stackDepth;
				if (stackDifference < 0)
					Error("Invalid stack index found compiling RETURN-FROM form: ~A",
						wrapInteger(stackDifference));
				if (stackDifference > 0)
				{
					ADD_ESP_NUM(stackDifference);
					CBincStackIndex(wrapInteger(stackDifference)); // no net change
				}
				JMP_LONG_RELATIVE(0);		// jmp  tag
				CAR(CDR(blockForm)) = 
					cons(wrapInteger(CURRENT_IP_RAW - 4), CAR(CDR(blockForm)));
				return retval;
			}
		}
		else
		if (type == CATCH)
		{
			CALL_INDIRECT(POP_CATCHER);
		}
		else
		if (type == UNWIND_PROTECT)
		{
			// include cleanup code
			// we rebind the cleanups to effectively remove the cleanup
			// for the unwind-protect we are currently processing
			cleanup = CDR(form);
			pushDynamicBinding(COMPILER_CLEANUPS, CAR(CDR(cleanup)));
			establishSpecialBindings(list(COMPILER_CLEANUPS, END_LIST));
			PUSH_EAX();
			PUSH_ECX();
			PUSH_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
			CALL_INDIRECT(POP_CATCHER);
			compilePrognForm(CAR(cleanup), Dest_EAX);
			POP_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
			POP_ECX();
			POP_EAX();
			popDynamicBinding(COMPILER_CLEANUPS);
			popSpecials();
		}
		else
		if (type == SPECIAL)
		{
			PUSH_EAX();
			PUSH_ECX();
			compileLiteralForm(CAR(CDR(form)), Dest_Stack, T);
			MOV_EDI_NIL();				// null environment
			SET_ARG_COUNT(1);
			CALL_INDIRECT(POP_SPECIAL_BINDINGS);
			ADD_ESP_NUM(4 * 1);
			CALL_INDIRECT(POP_SPECIALS);
			POP_ECX();
			POP_EAX();
		}
		else
		if (type == LAMBDA)
			break;
		forms = CDR(forms);
	}
	Error("Target of RETURN-FROM not found: ~A", x);
	return retval;
}

static LispObj compilePlusFunctionCall(LispObj x, LispObj dest, LispObj resultType)
{
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	LispObj addr3 = 0;
	LispObj arg1 = 0;
	LispObj arg2 = 0;
	LispObj arg1type = 0;
	LispObj arg2type = 0;
	long swapArgs = 0;

	long numArgs = listLength(x) - 1;
	if (numArgs == 2)
	{
		x = CDR(x);
		arg1 = CAR(x);
		arg2 = CAR(CDR(x));

		if (isFixnum(arg2))
		{
			arg1type = compileForm(arg1, Dest_EAX_Operand, T);
			MOV_EDX_OBJ(arg2);
			arg2type = FIXNUM;
			swapArgs = 1;
		}
		else
		{
			arg1type = compileForm(arg1, Dest_Stack, T);
			arg2type = compileForm(arg2, Dest_EAX_Operand, T);
			POP_EDX();
		}
		if (!isFixnum(arg2) && (arg2type != FIXNUM || compilerCheckTypes() != NIL))
		{
			if (swapArgs)
			{
				TEST_DL_NUM(7);
			}
			else
			{
				TEST_AL_NUM(7);
			}
			JNE_SHORT_RELATIVE(0);	// jnz call_func
			addr1 = CURRENT_IP;
		}
		if (!isFixnum(arg1) && (arg1type != FIXNUM || compilerCheckTypes() != NIL))
		{
			if (swapArgs)
			{
				TEST_AL_NUM(7);
			}
			else
			{
				TEST_DL_NUM(7);
			}
			JNE_SHORT_RELATIVE(0);	// jnz call_func
			addr2 = CURRENT_IP;
		}
		ADD_EAX_EDX();

		if (resultType != FIXNUM || compilerCheckTypes() != NIL)
		{
			JNO_SHORT_RELATIVE(0);	// jno done
			addr3 = CURRENT_IP;
			SUB_EAX_EDX();	// whoops, back up

			// call_func:
			// resolve branch target
			if (addr1)
				CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);
			if (addr2)
				CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);

			if (addr1 || addr2 || addr3)
			{
				CALL_INDIRECT(PLUS_EAX_EDX);
			}
			// done:
			// resolve branch target
			if (addr3)
				CBsetByte(addr3 - wrapInteger(1), CURRENT_IP - addr3);
		}

		TARGET(1);
	}
	else 
		compileFunctionCallForm(x, dest);

	return NUMBER;
}

static LispObj compileMinusFunctionCall(LispObj x, LispObj dest, LispObj resultType)
{
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	LispObj addr3 = 0;
	LispObj arg1 = 0;
	LispObj arg2 = 0;
	LispObj arg1type = 0;
	LispObj arg2type = 0;

	LispObj retval = NUMBER;
	long numArgs = listLength(x) - 1;
	if (numArgs == 2)
	{
		x = CDR(x);
		arg1 = CAR(x);
		arg2 = CAR(CDR(x));
		arg1type = compileForm(arg1, Dest_Stack, T);
		arg2type = compileForm(arg2, Dest_EAX_Operand, T);
		MOV_EDX_EAX();		// edx = arg2
		POP_EAX();			// eax = arg1
		if (!isFixnum(arg1) && (arg1type != FIXNUM || compilerCheckTypes() != NIL))
		{
			TEST_AL_NUM(7);
			JNE_SHORT_RELATIVE(0);	// jnz call_func
			addr1 = CURRENT_IP;
		}
		if (!isFixnum(arg2) && (arg2type != FIXNUM || compilerCheckTypes() != NIL))
		{
			TEST_DL_NUM(7);
			JNE_SHORT_RELATIVE(0);	// jnz call_func
			addr2 = CURRENT_IP;
		}
		SUB_EAX_EDX();
		if (resultType != FIXNUM || compilerCheckTypes() != NIL)
		{
			JNO_SHORT_RELATIVE(0);	// jno done
			addr3 = CURRENT_IP;
			ADD_EAX_EDX();	// whoops, back up
			// call_func:
			// resolve branch target
			if (addr1)
				CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);
			if (addr2)
				CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);

			CALL_INDIRECT(MINUS_EAX_EDX);
			// done:
			// resolve branch target
			CBsetByte(addr3 - wrapInteger(1), CURRENT_IP - addr3);
		}
		TARGET(1);
	}
	else 
		retval = compileFunctionCallForm(x, dest);
	return retval;
}

static LispObj compileNumericEqualFunctionCall(LispObj x, LispObj dest)
{
	long numArgs = listLength(x) - 1;
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	LispObj addr3 = 0;
	LispObj addr4 = 0;
	LispObj addr5 = 0;
	LispObj retval = SYMBOL;
	LispObj arg1 = 0;
	LispObj arg2 = 0;
	LispObj arg1type = 0;
	LispObj arg2type = 0;

	if (numArgs == 2)
	{
		x = CDR(x);
		arg1 = CAR(x);
		arg2 = CAR(CDR(x));
		
		if ((isSymbol(arg1) || isConstantObject(arg1)) && 
			(isSymbol(arg2) || isConstantObject(arg2)))
		{
			arg1type = compileForm(arg1, Dest_EAX_Operand, T);
			MOV_EDX_EAX();
			arg2type = compileForm(arg2, Dest_EAX_Operand, T);
		}
		else
		{
			arg1type = compileForm(arg1, Dest_Stack, T);
			arg2type = compileForm(arg2, Dest_EAX_Operand, T);
			POP_EDX();
		}

		if (!isFixnum(arg2) && (arg2type != FIXNUM || compilerCheckTypes() != NIL))
		{
			TEST_AL_NUM(7);
			JNE_SHORT_RELATIVE(0);	// jnz call_func
			addr3 = CURRENT_IP;
		}
		if (!isFixnum(arg1) && (arg1type != FIXNUM || compilerCheckTypes() != NIL))
		{
			TEST_DL_NUM(7);
			JNE_SHORT_RELATIVE(0);	// jnz call_func
			addr4 = CURRENT_IP;
		}
		if (dest == Dest_Zero_Flag)
		{
			CMP_EAX_EDX();
			if (addr3 || addr4)
			{
				JMP_SHORT_RELATIVE(0);		// jmp done
				addr5 = CURRENT_IP;
			}
		}
		else
		{
			CMP_EAX_EDX();
			MOV_EAX_NIL();
			JNE_SHORT_RELATIVE(0);		// jne done
			addr1 = CURRENT_IP;
			MOV_EAX_T();
			JMP_SHORT_RELATIVE(0);		// jmp done
			addr2 = CURRENT_IP;
		}

		if (addr3 || addr4)
		{
			// call_func
			if (addr3)
				CBsetByte(addr3 - wrapInteger(1), CURRENT_IP - addr3);
			if (addr4)
				CBsetByte(addr4 - wrapInteger(1), CURRENT_IP - addr4);
			PUSH_EDX();			// arg1
			PUSH_EAX();			// arg2
			MOV_EDI_NIL();		// environment
			SET_ARG_COUNT(2);
			CALL_INDIRECT(NUMERIC_EQUAL);
			ADD_ESP_NUM(8);
			if (dest == Dest_Zero_Flag)
				CMP_EAX_T();
		}

		// done
		if (addr1)
			CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);
		if (addr2)
			CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);
		if (addr5)
			CBsetByte(addr5 - wrapInteger(1), CURRENT_IP - addr5);

		TARGET(1);
	}
	else 
		retval = compileFunctionCallForm(x, dest);
	return retval;
}

static LispObj compileNumericCompareFunctionCall(LispObj x, LispObj dest)
{
	long numArgs = listLength(x) - 1;
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	LispObj addr3 = 0;
	LispObj addr4 = 0;
	LispObj retval = SYMBOL;
	LispObj arg1 = 0;
	LispObj arg2 = 0;
	LispObj arg1type = 0;
	LispObj arg2type = 0;
	LispObj op = CAR(x);

	if (numArgs == 2)
	{
		x = CDR(x);
		arg1 = CAR(x);
		arg2 = CAR(CDR(x)); 
		arg1type = compileForm(arg1, Dest_Stack, T);
		arg2type = compileForm(arg2, Dest_EAX_Operand, T);

		POP_EDX();
		if (!isFixnum(arg2) && (arg2type != FIXNUM || compilerCheckTypes() != NIL))
		{
			TEST_AL_NUM(7);
			JNE_SHORT_RELATIVE(0);	// jnz call_func
			addr3 = CURRENT_IP;
		}
		if (!isFixnum(arg1) && (arg1type != FIXNUM || compilerCheckTypes() != NIL))
		{
			TEST_DL_NUM(7);
			JNE_SHORT_RELATIVE(0);	// jnz call_func
			addr4 = CURRENT_IP;
		}
		CMP_EAX_EDX();
		MOV_EAX_NIL();

		if (op == LESS_EQUAL)
		{
			JL_SHORT_RELATIVE(0);			// jl done
		}
		else
		if (op == LESS)
		{
			JLE_SHORT_RELATIVE(0);
		}
		else
		if (op == GREATER_EQUAL)
		{
			JG_SHORT_RELATIVE(0);
		}
		else
		if (op == GREATER)
		{
			JGE_SHORT_RELATIVE(0);
		}

		addr1 = CURRENT_IP;
		MOV_EAX_T();
		if (addr3 || addr4)
		{
			JMP_SHORT_RELATIVE(0);		// jmp done
			addr2 = CURRENT_IP;
		}

		// call_func
		if (addr3)
			CBsetByte(addr3 - wrapInteger(1), CURRENT_IP - addr3);
		if (addr4)
			CBsetByte(addr4 - wrapInteger(1), CURRENT_IP - addr4);

		if (addr3 || addr4)
		{
			PUSH_EDX();			// arg1
			PUSH_EAX();			// arg2
			MOV_EDI_NIL();		// environment
			SET_ARG_COUNT(2);

			if (op == LESS_EQUAL)
			{
				CALL_INDIRECT(LESS_EQUAL);
			}
			else
			if (op == LESS)
			{
				CALL_INDIRECT(LESS);
			}
			else
			if (op == GREATER_EQUAL)
			{
				CALL_INDIRECT(GREATER_EQUAL);
			}
			else
			if (op == GREATER)
			{
				CALL_INDIRECT(GREATER);
			}

			ADD_ESP_NUM(8);
		}
		// done
		CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);
		if (addr2)
			CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);

		TARGET(1);
	}
	else 
		retval = compileFunctionCallForm(x, dest);
	return retval;
}

static LispObj compileConsFunctionCall(LispObj x, LispObj dest)
{
	LispObj savex = x;
	LispObj tryLabel = 0;
	LispObj doneBranch = 0;

	x = CDR(x);
	long numArgs = listLength(x);
	if (numArgs == 2)
	{
		compileForm(CAR(x), Dest_Stack, T);			// compile CAR
		compileForm(CAR(CDR(x)), Dest_Stack, T);	// compile CDR

		tryLabel = CURRENT_IP;
		MOV_EAX_ESI_PTR_WITH_OFFSET(THREAD_HEAP_Index*4);
		ADD_EAX_NUM(ConsTag);	// tag it quick
		LEA_EDX_EAX_PTR_WITH_OFFSET(8 - ConsTag);

		// the next instruction will catch both the case of the
		// buffer being exhausted, and if GC got invoked (causing
		// the end pointer to be reset to 0).
		CMP_EDX_ESI_PTR_WITH_OFFSET(THREAD_HEAP_END_Index*4); 
		JLE_SHORT_RELATIVE(0);
		doneBranch = CURRENT_IP;
		CALL_INDIRECT(LOAD_LOCAL_HEAP);
		JMP_SHORT_RELATIVE(0);	// jmp try1
		CBsetByte(CURRENT_IP - wrapInteger(1), tryLabel - CURRENT_IP);
		CBsetByte(doneBranch - wrapInteger(1), CURRENT_IP - doneBranch);
		MOV_ESI_PTR_WITH_OFFSET_EDX(THREAD_HEAP_Index*4);
		POP_EAX_PTR_WITH_OFFSET(0);					// set CDR
		POP_EAX_PTR_WITH_OFFSET(-4);				// set CAR

		TARGET(1);
	}
	else 
		Error("Wrong number of arguments to function CONS: ~A", savex);
	return CONS;
}

static LispObj compileEqFunctionCall(LispObj x, LispObj dest)
{
	LispObj savex = x;
	LispObj addr1 = 0;
	x = CDR(x);
	long numArgs = listLength(x);
	if (numArgs == 2)
	{
		compileForm(CAR(x), Dest_Stack, T);
		compileForm(CAR(CDR(x)), Dest_EAX_Operand, T);
		POP_EDX();
		CMP_EAX_EDX();
		MOV_EAX_NIL();
		JNE_SHORT_RELATIVE(0);
		addr1 = CURRENT_IP;
		MOV_EAX_T();
		CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);
		TARGET(1);
	}
	else 
		Error("Wrong number of arguments to function EQ: ~A", savex);
	return SYMBOL;
}

static LispObj compileCarFunctionCall(LispObj x, LispObj dest)
{
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	LispObj addr3 = 0;
	LispObj savex = x;

	x = CDR(x);
	long numArgs = listLength(x);
	if (numArgs == 1)
	{
		compileForm(CAR(x), Dest_EAX_Operand, T);
		
		MOV_EDX_EAX();
		AND_EDX_LONG(7);				// check tag
		CMP_EDX_LONG(ConsTag);			// is it a cons cell?
		JNE_SHORT_RELATIVE(0);			// jne next1
		addr3 = CURRENT_IP;

		MOV_EAX_EAX_PTR_WITH_OFFSET(-4);// if so, get car field
		JMP_SHORT_RELATIVE(0);			// jmp short done
		addr1 = CURRENT_IP;
		// next1:
		CBsetByte(addr3 - wrapInteger(1), CURRENT_IP - addr3);

		CMP_EAX_NIL();					// is it NIL
		JE_SHORT_RELATIVE(0);			// je done
		addr2 = CURRENT_IP;
		PUSH_EAX();						// push eax
		CALL_INDIRECT(CHECK_LIST);
		ADD_ESP_NUM(4);
		// done:
		CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);
		CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);

		TARGET(1);
	}
	else 
		Error("Wrong number of arguments to function CAR: ~A", savex);
	return T;
}

static LispObj compileCdrFunctionCall(LispObj x, LispObj dest)
{
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	LispObj addr3 = 0;
 	LispObj savex = x;

	x = CDR(x);
	long numArgs = listLength(x);
	if (numArgs == 1)
	{
		compileForm(CAR(x), Dest_EAX_Operand, T);
		
		MOV_EDX_EAX();
		AND_EDX_LONG(7);				// check tag
		CMP_EDX_LONG(ConsTag);			// is it a cons cell?
		JNE_SHORT_RELATIVE(0);			// jne next1
		addr3 = CURRENT_IP;

		MOV_EAX_EAX_PTR_WITH_OFFSET(0);// if so, get cdr field
		JMP_SHORT_RELATIVE(0);			// jmp short done
		addr1 = CURRENT_IP;
		// next1:
		CBsetByte(addr3 - wrapInteger(1), CURRENT_IP - addr3);

		CMP_EAX_NIL();					// is it NIL
		JE_SHORT_RELATIVE(0);			// je done
		addr2 = CURRENT_IP;
		PUSH_EAX();						// push eax
		CALL_INDIRECT(CHECK_LIST);
		ADD_ESP_NUM(4);
		// done:
		CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);
		CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);

		TARGET(1);
	}
	else 
		Error("Wrong number of arguments to function CDR: ~A", savex);
	return T;
}

static LispObj compileNullFunctionCall(LispObj x, LispObj dest)
{
	LispObj addr1 = 0;
	LispObj savex = x;
	x = CDR(x);
	long numArgs = listLength(x);
	if (numArgs == 1)
	{
		compileForm(CAR(x), Dest_EAX_Operand, T);
		CMP_EAX_NIL();
		MOV_EAX_NIL();
		JNE_SHORT_RELATIVE(0);			// jne next
		addr1 = CURRENT_IP;
		MOV_EAX_T();
		// next:
		CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);

		TARGET(1);
	}
	else 
		Error("Wrong number of arguments to function NULL: ~A", savex);
	return SYMBOL;
}

static LispObj compileUrefFunctionCall(LispObj x, LispObj dest)
{
	LispObj uvec = 0;
	LispObj index = 0;
	if (listLength(x) != 3)
		Error("Wrong number of arguments passed to UREF: ~A", x);
	uvec = CAR(CDR(x));
	index =	CAR(CDR(CDR(x)));

	if (isConstantObject(uvec))
	{
		if (!isUvector(uvec))
			Error("Invalid type passed to UREF, expected uvector, got: ~A", uvec);
	}

	if (isConstantObject(index))
	{
		if (!isFixnum(index))
			Error("Invalid type passed to UREF, expected type FIXNUM, got: ~A", index);
		compileForm(uvec, Dest_EAX_Operand, T);
		MOV_EAX_EAX_PTR_WITH_OFFSET(-UvectorTag + (4 * integer(index)));
	}
	else
	{
		compileForm(uvec, Dest_Stack, T);
		compileForm(index, Dest_EAX_Operand, T);
		POP_EDX();
		BEGIN_ATOMIC();
		SHR_EAX_NUM(1);					// caution: untagged integer in EAX
		MOV_EAX_EAX_PLUS_EDX_PTR_WITH_OFFSET(-UvectorTag);
		END_ATOMIC();
	}
	TARGET(1);
	return T;
}

static LispObj compileUrefSetFunctionCall(LispObj x, LispObj dest)
{
	LispObj uvec = 0;
	LispObj index = 0;
	LispObj value = 0;

	if (listLength(x) != 4)
		Error("Wrong number of arguments passed to UREF-SET: ~A", x);
	value = CAR(CDR(x));
	uvec =	CAR(CDR(CDR(x)));
	index = CAR(CDR(CDR(CDR(x))));
	if (isConstantObject(uvec))
	{
		if (!isUvector(x))
			Error("Invalid type passed to UREF-SET, expected uvector, got: ~A", uvec);
	}

	if (isConstantObject(index))
	{
		if (!isFixnum(index))
			Error("Invalid type passed to UREF-SET, expected type FIXNUM, got: ~A", index);
		compileForm(uvec, Dest_Stack, T);
		compileForm(value, Dest_EAX_Operand, T);
		POP_EDI();
		MOV_EDI_PTR_WITH_OFFSET_EAX(-UvectorTag + (4 * integer(index)));
	}
	else
	{
		compileForm(uvec, Dest_Stack, T);
		compileForm(index, Dest_Stack, T);
		compileForm(value, Dest_EAX_Operand, T);

		POP_EDX();					// EDX = index
		BEGIN_ATOMIC();
		SHR_EDX_NUM(1);				// caution: untagged integer in EDX
		POP_EDI();					// EDI = uvector
		MOV_EDI_PLUS_EDX_PTR_WITH_OFFSET_EAX(-UvectorTag);
		END_ATOMIC();
	}
	TARGET(1);
	return T;
}

// we use this jump buffer (36 bytes)
//		0:		EBX
//		4:		ECX
//		8:		EDX
//		12:		ESI
//		16:		EDI
//		20:		ESP
//		24:		EIP
//		28:		EBP
//		32:		marker (0xf9f9f9f9)
//
static LispObj compileCatchForm(LispObj x, LispObj dest)
{
	LispObj addr1 = 0;
	LispObj retval = T;
	LispObj envOffset = 0;

	if (!isCons(CDR(x)))
		Error("Invalid CATCH form: ~A", x);
	LispObj tag = CAR(CDR(x));
	LispObj forms = CDR(CDR(x));
	CBpushCleanup(list(CATCH, tag, END_LIST));

	// allocate room on the stack for the jmpbuf
	envOffset = CBincDynamicEnvSize(wrapInteger(JumpBufferSize));

	// pushCatcher(tag, jmp_buf)
	LEA_EAX_EBP_PTR_WITH_OFFSET(integer(envOffset));
	PUSH_EAX();
	
	compileForm(tag, Dest_Stack, T);			// evaluate the tag

	CALL_INDIRECT(PUSH_CATCHER);
	ADD_ESP_NUM(8);

	// setjmp(esp)
	MOV_EAX_NUM(UNINITIALIZED);

	PUSH_NUM(JumpBufferMarker);
	POP_EBP_PTR_WITH_OFFSET(integer(envOffset) + 32);
	MOV_EBP_PTR_WITH_OFFSET_EBX(integer(envOffset) + 0);
	MOV_EBP_PTR_WITH_OFFSET_ECX(integer(envOffset) + 4);
	MOV_EBP_PTR_WITH_OFFSET_EDX(integer(envOffset) + 8);
	MOV_EBP_PTR_WITH_OFFSET_ESI(integer(envOffset) + 12);
	MOV_EBP_PTR_WITH_OFFSET_EDI(integer(envOffset) + 16);
	MOV_EBP_PTR_WITH_OFFSET_ESP(integer(envOffset) + 20);
	MOV_EBP_PTR_WITH_OFFSET_EBP(integer(envOffset) + 28);
	BEGIN_ATOMIC();
	PUSH_EIP();
	POP_EDX();					// edx contains IP
	ADD_EDX_NUM(15);
	MOV_EBP_PTR_WITH_LONG_OFFSET_EDX(integer(envOffset) + 24);	// store ip
	MOV_EDX_NUM(0);				// this must be 5 bytes-- make sure to clean up for gc
	END_ATOMIC();

	// if eax != UNINITIALIZED, we got here via an exception, and EAX contains
	// the return value
	CMP_EAX_NUM(UNINITIALIZED);
	JNE_RELATIVE(0);					// jne		next
	addr1 = CURRENT_IP;

	retval = compilePrognForm(forms, Dest_EAX);

	// resolve branch target
	CBsetLong(addr1 - wrapInteger(4), CURRENT_IP - addr1);

	CBpopCleanup();

	// popCatcher();
	PUSH_EAX();
	PUSH_ECX();
	CALL_INDIRECT(POP_CATCHER);
	POP_ECX();
	POP_EAX();

	if (dest == Dest_Stack)
		PUSH_EAX();
	return retval;
}

static LispObj compileThrowForm(LispObj x, LispObj /*dest*/)
{
	LispObj throwTag = 0;
	LispObj throwForm = NIL;
	LispObj retval = T;
	long len = listLength(x);
	if (len < 2 || len > 3)
		Error("Invalid THROW form: ~A", x);
	throwTag = CAR(CDR(x));
	if (len == 3)
		throwForm = CAR(CDR(CDR(x)));
	compileForm(throwTag, Dest_Stack, T);
	retval = compileForm(throwForm, Dest_EAX, T);
	PUSH_EAX();
	SHL_ECX_NUM(3);			// wrapInteger(count)
	PUSH_ECX();
	SET_ARG_COUNT(3);
	CALL_INDIRECT(THROW_EXCEPTION);	// throwException(tag, form, count);
	ADD_ESP_NUM(3 * 4);

	return retval;
}

static LispObj compileUnwindProtectForm(LispObj x, LispObj dest)
{
	LispObj protectedForm = 0;
	LispObj cleanupForms = 0;
	LispObj addr1 = 0;
	LispObj addr2 = 0;
	LispObj saveAddress = 0;
	LispObj numCodeBytes = 0;
	LispObj numReferences = 0;
	LispObj cleanupFunc = 0;
	LispObj cleanup = 0;
	LispObj cbuf = 0;
	LispObj refs = 0;
	long numbytes = 0;
	long i = 0;
	long numrefs = 0;
	LispObj envOffset = 0;

	if (!isCons(CDR(x)))
		Error("Invalid UNWIND-PROTECT form: ~A", x);
	protectedForm = CAR(CDR(x));
	cleanupForms = CDR(CDR(x));

		// allocate room on the stack for the jmpbuf
	envOffset = CBincDynamicEnvSize(wrapInteger(JumpBufferSize));

	// pushCatcher(tag, jmp_buf)
	LEA_EAX_EBP_PTR_WITH_OFFSET(integer(envOffset));
	PUSH_EAX();
	MOV_EAX_NUM(UNWIND_MARKER);
	PUSH_EAX();
	CALL_INDIRECT(PUSH_CATCHER);
	ADD_ESP_NUM(8);

	// setjmp(esp)
	MOV_EAX_NUM(UNINITIALIZED);

	PUSH_NUM(JumpBufferMarker);
	POP_EBP_PTR_WITH_OFFSET(integer(envOffset) + 32);
	MOV_EBP_PTR_WITH_OFFSET_EBX(integer(envOffset) + 0);
	MOV_EBP_PTR_WITH_OFFSET_ECX(integer(envOffset) + 4);
	MOV_EBP_PTR_WITH_OFFSET_EDX(integer(envOffset) + 8);
	MOV_EBP_PTR_WITH_OFFSET_ESI(integer(envOffset) + 12);
	MOV_EBP_PTR_WITH_OFFSET_EDI(integer(envOffset) + 16);
	MOV_EBP_PTR_WITH_OFFSET_ESP(integer(envOffset) + 20);
	MOV_EBP_PTR_WITH_OFFSET_EBP(integer(envOffset) + 28);
	BEGIN_ATOMIC();
	PUSH_EIP();
	POP_EDX();					// edx contains IP
	ADD_EDX_NUM(15);
	MOV_EBP_PTR_WITH_LONG_OFFSET_EDX(integer(envOffset) + 24);	// store ip
	MOV_EDX_NUM(0);				// this must be 5 bytes-- make sure to clean up for gc
	END_ATOMIC();

	// if eax != UNINITIALIZED, we got here via an exception, and EAX contains
	// the return value
	CMP_EAX_NUM(UNINITIALIZED);
	PUSH_EAX();						// push		eax		;; save result on stack
	JNE_RELATIVE(0);				// jne		next
	addr1 = CURRENT_IP;

	CBpushCleanup(list(UNWIND_PROTECT, cleanupForms, CBcleanups(), END_LIST));

	compileForm(protectedForm, Dest_EAX, T);

	CBpopCleanup();

	// resolve branch target	
	CBsetLong(addr1 - wrapInteger(4), CURRENT_IP - addr1);

	// inline cleanup code
	PUSH_EAX();
	PUSH_ECX();
	PUSH_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
	CALL_INDIRECT(POP_CATCHER);
	compilePrognForm(cleanupForms, Dest_EAX);
	POP_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
	POP_ECX();
	POP_EAX();

	// retrieve exception result
	MOV_EDX_EAX();
	POP_EAX();
	CMP_EAX_LONG(UNINITIALIZED);
	MOV_EAX_EDX();
	JE_SHORT_RELATIVE(0);						//je		next2
	addr2 = CURRENT_IP;

	// continue thrown exception
	PUSH_ESI_PTR_WITH_OFFSET(THROW_TAG_Index * 4);
	PUSH_EAX();						// push eax
	SHL_ECX_NUM(3);					// ecx = wrapInteger(ecx)
	PUSH_ECX();						// push ecx
	SET_ARG_COUNT(3);
	CALL_INDIRECT(THROW_EXCEPTION);	// throwException(tag, value, numValues);
	ADD_ESP_NUM(3 * 4);
	// resolve branch target
	CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);

	if (dest == Dest_Stack)
		PUSH_EAX();

	return T;
}

static LispObj compileMultipleValueCallForm(LispObj x, LispObj dest)
{
	long len = listLength(x);
	if (len < 2)
		Error("Invalid MULTIPLE-VALUE-CALL form: ~A", x);
	LispObj func = CAR(CDR(x));
	LispObj forms = CDR(CDR(x));
	long numforms = len - 2;
	long counter = 0;
	long addr1 = 0;
	long addr2 = 0;
	long addr3 = 0;
	long addr4 = 0;

	compileForm(func, Dest_Stack, T);	// push function
	
	for (; counter < numforms; counter++)
	{
		compileForm(CAR(forms), Dest_EAX, T);
		forms = CDR(forms);
		CMP_ECX_LONG(1);
		JZ_SHORT_RELATIVE(0);				// jz next1
		addr1 = CURRENT_IP;
		
		CMP_ECX_LONG(0);
		JNE_SHORT_RELATIVE(0);			// jne next3
		addr3 = CURRENT_IP;

		MOV_EAX_NIL();
		JMP_SHORT_RELATIVE(0);		// jmp short next4
 		addr4 = CURRENT_IP;

		// next3:
		CBsetByte(addr3 - wrapInteger(1), CURRENT_IP - addr3);

		MOV_EAX_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
		// next4:
		CBsetByte(addr4 - wrapInteger(1), CURRENT_IP - addr4);

		JMP_SHORT_RELATIVE(0);		// jmp short next2
		addr2 = CURRENT_IP;
		// next1: 
		// resolve branch target
		CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);

		PUSH_EAX();
		CALL_INDIRECT(ALLOC_CONS);
		MOV_EDI_EAX();
		POP_EDI_PTR_WITH_OFFSET(-4);	// set CAR
		MOV_EAX_NIL();
		MOV_EDI_PTR_WITH_OFFSET_EAX(0);	// set CDR = NIL
		MOV_EAX_EDI();

		// next2:
		CBsetByte(addr2 - wrapInteger(1), CURRENT_IP - addr2);
		PUSH_EAX();
	}

	// call append()
	SET_ARG_COUNT(numforms);
	LOAD_ENVIRONMENT(APPEND);
	CALL_INDIRECT(APPEND);
	ADD_ESP_NUM(numforms * 4);

	// call apply()
	POP_EDI();						// edi = function
	PUSH_EDI();						// push function
	PUSH_EAX();						// push arg list
	SET_ARG_COUNT(2);
	MOV_EDI_EDI_PTR_WITH_OFFSET(-UvectorTag + (FUNCTION_ENVIRONMENT * 4));
	CALL_INDIRECT(APPLY);
	ADD_ESP_NUM(2 * 4);

	if (dest == Dest_Stack)
		PUSH_EAX();

	return T;
}

static long validLambda(LispObj x)
{
	if (isCons(x) && isCons(CDR(x))
			&& CAR(x) == LAMBDA && isList(CAR(CDR(x))))
		return 1;
	return 0;
}

static long validBlock(LispObj x)
{
	if (isCons(x) && isCons(CDR(x))
			&& CAR(x) == BLOCK && isSymbol(CAR(CDR(x))))
		return 1;
	return 0;
}

static LispObj findLambdas(LispObj x)
{
	LispObj t = 0;
	if (!isCons(x))
		return NIL;
	if (validLambda(x))
		return cons(x, NIL);
	t = CAR(x);
	if ((t == FLET || t == LABELS)
		&& isCons(CDR(x)) && isList(CAR(CDR(x))))
		return LispCall2(lispAppend, CAR(CDR(x)), findLambdas(CDR(CDR(x))));
	if (t == QUOTE)
		return NIL;		// don't go into quoted expressions
	return LispCall2(lispAppend, findLambdas(CAR(x)), findLambdas(CDR(x)));
}

//
// If the target is found, a list is returned containing the symbol T,
// to indicate the item was found, followed by the list of block labels 
// that enclose the specified target form (there may not be any labels).
// If the target is not found, NIL is returned.
// The returned list of labels is inner to outer.
//
static LispObj findBlockLabels(LispObj outer, LispObj target, LispObj labels)
{
	LispObj t = 0;
	LispObj ret = 0;
	if (outer == target)
		return cons(T, labels);	// found the target--return the labels we have collected
	if (!isCons(outer))
		return NIL;		// could not find the target, return NIL to signify no labels found
	t = CAR(outer);
	if (t == QUOTE)
		return NIL;		// don't go into quoted expressions
	if (validBlock(outer))
		return findBlockLabels(CDR(CDR(outer)),
			target,
			cons(CAR(CDR(outer)), labels));  // we found a block--add its label to the list and recurse
	for (t = outer; isCons(t); t = CDR(t))
	{
		ret = findBlockLabels(CAR(t), target, labels);
		if (isCons(ret))
			return ret;
	}
	return NIL;
}

static LispObj searchLambdas(LispObj sym, LispObj lambdas)
{
	LispObj t = 0;

	if (lambdas == sym)
		return sym;
	//
	// Special case: if we bump into CAPTURE_COMPILER_ENVIRONMENT form,
	// always return true. This forces all lexical variables to be heap-allocated.
	//
	if (lambdas == CAPTURE_COMPILER_ENVIRONMENT)
		return sym;

	if (!isCons(lambdas))
		return NIL;

	t = searchLambdas(sym, CAR(lambdas));
	if (t != NIL)
		return t;
 	t = searchLambdas(sym, CDR(lambdas));
	return t;
}

//
//	This function searches for forms (RETURN-FROM sym ...)
//	It is used to determine if a non-local return-from is 
//	needed.
//	A list of the embedded return-from forms is returned.
//  The passed labels argument is a list of block labels that are active
//  for the form. It is initially empty, on the first call, but recursive
//  calls will populate this list as appropriate. We don't want to return an
//  embedded RETURN-FROM form which has a label in the enclosing syntax that
//  it could return to. We are only interested in the cases where it needs 
//  to return to a label in an enclosing LAMBDA.
//
static LispObj findEmbeddedReturnForms(LispObj sym, LispObj form, LispObj labels)
{
	LispObj retforms = NIL;
	LispObj ret = NIL;
	LispObj p = 0;
	if (isCons(form))
	{
		// don't perform any transformations on quoted lists
		if (CAR(form) == QUOTE)
			return NIL;
		if (validBlock(form))
			return findEmbeddedReturnForms(sym, CDR(CDR(form)), cons(CAR(CDR(form)), labels));
		p = form;
		while (isCons(p))
		{
			ret = findEmbeddedReturnForms(sym, CAR(p), labels);	// recurse on sublists
			while (isCons(ret))
			{
				retforms = cons(CAR(ret), retforms);
				ret = CDR(ret);
			}
			p = CDR(p);
		}		
		if (CAR(form) == RETURN_FROM
				&& isCons(CDR(form)) 
				&& CAR(CDR(form)) == sym
				&& !isCons(Cmember(sym, labels)))	// make sure there is not an enclosing block with this label
			retforms = cons(form, retforms);
		else if (CAR(form) == RETURN
				&& sym == NIL
				&& !isCons(Cmember(sym, labels)))	// make sure there is not an enclosing block with this label
			retforms = cons(form, retforms);

		return retforms;
	}
	return NIL;
}

//
//	This function searches for forms (GO sym)
//	It is used to determine if a non-local return-from is 
//	needed.
//	A list of the embedded GO forms is returned.
//
static LispObj findEmbeddedGoForms(LispObj sym, LispObj form)
{
	LispObj retforms = NIL;
	LispObj ret = NIL;
	LispObj p = 0;
	if (isCons(form))
	{
		// don't perform and transformations on quoted lists
		if (CAR(form) == QUOTE)
			return NIL;
		p = form;
		while (isCons(p))
		{
			ret = findEmbeddedGoForms(sym, CAR(p));	// recurse on sublists
			while (isCons(ret))
			{
				retforms = cons(CAR(ret), retforms);
				ret = CDR(ret);
			}
			p = CDR(p);
		}		
		if (CAR(form) == GO
				&& isCons(CDR(form)) 
				&& (CAR(CDR(form)) == sym || (Cmember(CAR(CDR(form)), sym) != NIL)))
			return cons(form, retforms);
	}
	return retforms;
}

static LispObj referencedByEmbeddedLambdas(LispObj sym)
{
	return searchLambdas(sym, symbolValue(EMBEDDED_LAMBDAS));
}

static void addCompilerFrameInfo(LispObj name, LispObj base, LispObj offset, LispObj indirect)
{
	setSymbolValue(COMPILER_FRAME_INFO, 
		cons((indirect << 26) | (base << 24) | (offset & 0x7fffff8), 
			cons(name, symbolValue(COMPILER_FRAME_INFO))));
}

static void compileVariableHeapBindings(LispObj newVars)
{
	// check for variables which must be heap-allocated
	LispObj p = newVars;
	LispObj q = 0;
	LispObj sym = 0;
	LispObj reg = 0;
	LispObj n = 0;
	long num = 0;
	LispObj indirect = 0;
	LispObj regbase = 0;	// 0 = EBP, 1 = EBX, 2 = EDI, 3 = ESI

	// update COMPILER_FRAME_INFO here, add records for all variables

	while (isCons(p))
	{
		q = CAR(p);
		sym = CAR(q);
		reg = CAR(CDR(q));
		n = CAR(CDR(CDR(q)));
		num = integer(n);
		indirect = 0;

		if (referencedByEmbeddedLambdas(sym) != NIL)	// check symbol
		{
			// for each heap variable, allocate a cons and replace
			// the car of the cons with the symbol value, then replace
			// the symbol value with the cons
			indirect = wrapInteger(1);

			if (reg == EBX)
			{	
				regbase = wrapInteger(1);
				PUSH_EBX_PTR_WITH_OFFSET(num);
				CALL_INDIRECT(ALLOC_CONS);
				MOV_EBX_PTR_WITH_OFFSET_EAX(integer(n));
			}
			else	// (reg == EBP)
			{
				regbase = wrapInteger(0);
				PUSH_EBP_PTR_WITH_OFFSET(num);
				CALL_INDIRECT(ALLOC_CONS);
 				MOV_EBP_PTR_WITH_OFFSET_EAX(integer(n));
			}

			MOV_EDI_EAX();
			POP_EDI_PTR_WITH_OFFSET(-4);	// set CAR
			MOV_EAX_NIL();
			MOV_EDI_PTR_WITH_OFFSET_EAX(0);	// CDR = NIL
			CAR(CDR(CDR(q))) = cons(n, NIL); // (sym EBX 12) -> (sym EBX (12)) 
		}
		else
		{
			if (reg == EBX)
				regbase = wrapInteger(1);
			else
				regbase = wrapInteger(0);
		}
		addCompilerFrameInfo(sym, regbase, n, indirect);
		p = CDR(p);
	}
}

static void compileFunctionHeapBindings(LispObj newFuncs)
{
	// check for lexical functions which must be heap-allocated
	LispObj p = newFuncs;
	LispObj q = 0;
	LispObj sym = 0;
	LispObj reg = 0;
	LispObj n = 0;
	long num = 0;

	while (isCons(p))
	{
		q = CAR(p);
		sym = CAR(q);
		reg = CAR(CDR(q));
		n = CAR(CDR(CDR(q)));
		num = integer(n);

		if (referencedByEmbeddedLambdas(sym) != NIL)	// check symbol
		{
			// for each heap variable, allocate a cons and replace
			// the car of the cons with the symbol value, then replace
			// the symbol value with the cons

			if (reg == EBX)
			{
				PUSH_EBX_PTR_WITH_OFFSET(num);
				CALL_INDIRECT(ALLOC_CONS);
				MOV_EBX_PTR_WITH_OFFSET_EAX(integer(n));
			}
			else	// (reg == EBP)
			{
				PUSH_EBP_PTR_WITH_OFFSET(num);
				CALL_INDIRECT(ALLOC_CONS);
 				MOV_EBP_PTR_WITH_OFFSET_EAX(integer(n));
			}

			MOV_EDI_EAX();
			POP_EDI_PTR_WITH_OFFSET(-4);	// set CAR
			MOV_EAX_NIL();
			MOV_EDI_PTR_WITH_OFFSET_EAX(0);	// CDR = NIL
			CAR(CDR(CDR(q))) = cons(n, NIL); // (sym EBX 12) -> (sym EBX (12)) 
		}
		p = CDR(p);
	}
}

static LispObj findRequiredArgs(LispObj lambdaList)
{
	LispObj vars = NIL;
	LispObj sym = 0;

	while (isCons(lambdaList))
	{
		sym = CAR(lambdaList);
		if (sym == LAMBDA_OPTIONAL
			|| sym == LAMBDA_REST
			|| sym == LAMBDA_KEY
			|| sym == LAMBDA_AUX
			|| sym == LAMBDA_ALLOW_OTHER_KEYS)
			break;
		vars = cons(sym, vars);
		lambdaList = CDR(lambdaList);
	}
	return Cnreverse(vars);	
}

static LispObj findOptionalArgs(LispObj lambdaList)
{
	LispObj vars = NIL;
	LispObj sym = 0;

	while (isCons(lambdaList) && CAR(lambdaList) != LAMBDA_OPTIONAL)
		lambdaList = CDR(lambdaList);
	if (isCons(lambdaList))
	{
		lambdaList = CDR(lambdaList);

		while (isCons(lambdaList))
		{
			sym = CAR(lambdaList);
			if (sym == LAMBDA_REST
				|| sym == LAMBDA_KEY
				|| sym == LAMBDA_AUX
				|| sym == LAMBDA_ALLOW_OTHER_KEYS)
				break;
			vars = cons(sym, vars);
			lambdaList = CDR(lambdaList);
		}
		return Cnreverse(vars);	
	}
	return NIL;	
}

static LispObj findRestArgs(LispObj lambdaList)
{
	LispObj vars = NIL;
	LispObj sym = 0;
	LispObj sll = lambdaList;

	while (isCons(lambdaList) && CAR(lambdaList) != LAMBDA_REST)
		lambdaList = CDR(lambdaList);
	if (isCons(lambdaList))
	{
		lambdaList = CDR(lambdaList);

		while (isCons(lambdaList))
		{
			sym = CAR(lambdaList);
			if (sym == LAMBDA_OPTIONAL)
				Error("&OPTIONAL following &REST in lambda list: ~A", sll);
			if (sym == LAMBDA_KEY
				|| sym == LAMBDA_AUX
				|| sym == LAMBDA_ALLOW_OTHER_KEYS)
				break;
			vars = cons(sym, vars);
			lambdaList = CDR(lambdaList);
		}
		return Cnreverse(vars);	
	}
	return NIL;	
}

static LispObj findKeyArgs(LispObj lambdaList)
{
	LispObj vars = NIL;
	LispObj sym = 0;
	LispObj sll = lambdaList;

	while (isCons(lambdaList) && CAR(lambdaList) != LAMBDA_KEY)
		lambdaList = CDR(lambdaList);
	if (isCons(lambdaList))
	{
		lambdaList = CDR(lambdaList);

		while (isCons(lambdaList))
		{
			sym = CAR(lambdaList);
			if (sym == LAMBDA_OPTIONAL)
				Error("&OPTIONAL following &KEY in lambda list: ~A", sll);
			if (sym == LAMBDA_REST)
				Error("&REST following &KEY in lambda list: ~A", sll);
			if (sym == LAMBDA_AUX
				|| sym == LAMBDA_ALLOW_OTHER_KEYS)
				break;
			vars = cons(sym, vars);
			lambdaList = CDR(lambdaList);
		}
		return cons(LAMBDA_KEY, Cnreverse(vars));	
	}
	return NIL;	
}

static LispObj findAuxArgs(LispObj lambdaList)
{
	LispObj vars = NIL;
	LispObj sym = 0;
	LispObj sll = lambdaList;

	while (isCons(lambdaList) && CAR(lambdaList) != LAMBDA_AUX)
		lambdaList = CDR(lambdaList);
	if (isCons(lambdaList))
	{
		lambdaList = CDR(lambdaList);

		while (isCons(lambdaList))
		{
			sym = CAR(lambdaList);
			if (sym == LAMBDA_OPTIONAL)
				Error("&OPTIONAL following &AUX in lambda list: ~A", sll);
			if (sym == LAMBDA_REST)
				Error("&REST following &AUX in lambda list: ~A", sll);
			if (sym == LAMBDA_KEY)
				Error("&KEY following &AUX in lambda list: ~A", sll);
			if (sym == LAMBDA_ALLOW_OTHER_KEYS)
				Error("&ALLOW-OTHER-KEYS following &AUX in lambda list: ~A", sll);
			vars = cons(sym, vars);
			lambdaList = CDR(lambdaList);
		}
		return Cnreverse(vars);	
	}
	return NIL;	
}

static LispObj findAllowOtherKeys(LispObj lambdaList)
{
	while (isCons(lambdaList) && CAR(lambdaList) != LAMBDA_ALLOW_OTHER_KEYS)
		lambdaList = CDR(lambdaList);
	return isCons(lambdaList) ? T : NIL;
}

static LispObj compileEvalWhenForm(LispObj x, LispObj dest)
{
	LispObj conditions = 0;
	LispObj forms = 0;
	LispObj retval = T;

	if (!isCons(x) || !isCons(CDR(x)) || !isList(CAR(CDR(x))))
		Error("EVAL-WHEN form missing condition list: ~A", x);
	conditions = CAR(CDR(x));
	forms = CDR(CDR(x));
	if ((Cmember(EXECUTE_KEY, conditions) != NIL)
			|| (Cmember(EVAL, conditions) != NIL))
	{
		retval = compilePrognForm(forms, dest);
	}
	else
		retval = compileNil(dest);
	return retval;
}

static int isConstantObject(LispObj x)
{
	return !(isSymbol(x) || isCons(x)); 
		//	|| (isSymbol(x) && isConstantSymbol(x));
}

static LispObj compileMultipleValueProg1Form(LispObj x, LispObj dest)
{
	long len = listLength(x);
	if (len < 2)
		Error("Invalid MULTIPLE_VALUE-PROG1 form: ~A", x);
	LispObj form = CAR(CDR(x));
	LispObj forms = CDR(CDR(x));
	LispObj retval = T;

	compileForm(form, Dest_EAX, T);
	if (len > 2)
	{
		PUSH_EAX();
		PUSH_ECX();
		PUSH_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);

		while (isCons(forms))
		{
			form = CAR(forms);
			compileForm(form, Dest_EAX, T);
			forms = CDR(forms);
		}

		POP_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
		POP_ECX();
		POP_EAX();
	}

	if (dest == Dest_Stack)
		PUSH_EAX();

	return retval;
}

static LispObj compileTheForm(LispObj x, LispObj dest)
{
	LispObj type = 0;
	LispObj obj = 0;
	LispObj retval = T;
	LispObj addr1 = 0;

	long len = listLength(x);
	if (len != 3)
		Error("Invalid THE form: ~A", x);
	type = CAR(CDR(x));
	obj = CAR(CDR(CDR(x)));

	type = LispCall2(Funcall, TYPEEXPAND_ALL, type);

	retval = type;

	if (isCons(type) && (CAR(type) == VALUES || CAR(type) == FUNCTION))
	{
		LispCall3(Funcall, WARN,
			stringNode("Type not currently checked by THE form: ~A"),
			type);
		return compileForm(obj, dest, type);
	}

	compileForm(obj, Dest_EAX, type);

	if (isConstantObject(obj))
	{
		// check object at compile time
		LispCall5(Funcall, CHECK_TYPE_BODY, obj, type, NIL, obj);
	}
	else
	if (compilerCheckTypes() != NIL)
	{
		if (type == FIXNUM)
		{
			TEST_AL_NUM(7);
			JE_SHORT_RELATIVE(0);
 			addr1 = CURRENT_IP;
			PUSH_EAX();
			CALL_INDIRECT(INVALID_FIXNUM);
			ADD_ESP_NUM(4);

			// resolve branch target
			CBsetByte(addr1 - wrapInteger(1), CURRENT_IP - addr1);			
		}
		else
		{
			PUSH_EAX();
			PUSH_ECX();
			PUSH_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);

			PUSH_EAX();
			compileLiteralForm(type, Dest_Stack, T);
			PUSH_NIL();
			compileLiteralForm(obj, Dest_Stack, T);
			SET_ARG_COUNT(4);
			LOAD_ENVIRONMENT(CHECK_TYPE_BODY);			// push environment
			CALL_INDIRECT(CHECK_TYPE_BODY);
			ADD_ESP_NUM(4 * 4);

			POP_ESI_PTR_WITH_OFFSET(MULTIPLE_RETURN_VALUES_Index * 4);
			POP_ECX();
			POP_EAX();
		}
	}
	if (dest == Dest_Stack)
		PUSH_EAX();
	return retval;
}

static LispObj compileArefFunctionCall(LispObj x, LispObj dest)
{
	long num = listLength(x);
	LispObj func = 0;

	if (num < 2)
		Error("Missing array in call to AREF: ~A", x);
	if (num > 9)
		Error("Too many subscripts in call to AREF, maximum of 7 allowed: ~A", x);
	func = arrayStart(symbolValue(AREF_FUNCS))[num - 2];
	return compileFunctionCallForm(cons(func, CDR(x)), dest);
}

static LispObj compileSetfArefFunctionCall(LispObj x, LispObj dest)
{
	long num = listLength(x);
	LispObj func = 0;

	if (num < 3)
		Error("Missing array in call to AREF: ~A", x);
	if (num > 10)
		Error("Too many subscripts in call to AREF, maximum of 7 allowed: ~A", x);
	func = arrayStart(symbolValue(AREF_SETTER_FUNCS))[num - 3];
	return compileFunctionCallForm(cons(func, CDR(x)), dest);
}

//
//	Returns T is the symbol as found to have a function bound to it in
//	the lexical environment.
//
static LispObj findLexFunction(LispObj sym)
{
	LispObj f = 0;
	LispObj lex = CBcleanups();
	while (isCons(lex))
	{
		f = CAR(lex);
		if (CAR(f) == WITH_ONLY_LEXICALS)
			return NIL;	// don't inherit any lexicals passed this point
		if (CAR(f) == FLET && CAR(CDR(f)) == sym)
			return T;
		lex = CDR(lex);
	}
	return NIL;
}

static void restoreOptimizeDeclarationBindings(LispObj bindings)
{
	if (isCons(bindings))
	{
		while (isCons(bindings))
		{
			popDynamicBinding(CAR(bindings));
			bindings = CDR(bindings);
		}
		popSpecials();
	}
}

static LispObj processOptimizeDeclarations(LispObj declareForms)
{
	LispObj forms = declareForms;
	LispObj declareForm = 0;
	LispObj f = 0;
	LispObj optimizations = 0;
	LispObj bindings = NIL;
	LispObj o = 0;
	LispObj value = 0;
	LispObj sym = 0;
	LispObj compilerSym = 0;

	while (isCons(forms))
	{
		declareForm = CDR(CAR(forms)); // skip DECLARE symbol
		while (isCons(declareForm))
		{
			f = CAR(declareForm);
			if (CAR(f) == OPTIMIZE)
			{
				optimizations = CDR(f);
				while (isCons(optimizations))
				{
					o = CAR(optimizations);
					// optimizations default to 3
					if (isSymbol(o))
					{
						sym = o;
						value = wrapInteger(3);
					}
					else
					{
						if (!isCons(o))
							Error("Invalid declaration syntax: ~A", declareForm);
						sym = CAR(o);
						if (isCons(CDR(o)))
							value = CAR(CDR(o));
						else
							value = wrapInteger(3);
					}
					if (!isFixnum(value) || (value != 0	&& value != wrapInteger(1)
								&& value != wrapInteger(2) && value != wrapInteger(3)))
 						Error("Invalid declaration syntax: ~A", declareForm);
					if (sym == SPEED)
						compilerSym = COMPILER_OPTIMIZE_SPEED;
					else if (sym == DEBUG)
						compilerSym = COMPILER_OPTIMIZE_DEBUG;
					else if (sym == SAFETY)
						compilerSym = COMPILER_OPTIMIZE_SAFETY;
					else if (sym == SPACE)
						compilerSym = COMPILER_OPTIMIZE_SPACE;
					else if (sym == COMPILATION_SPEED)
						compilerSym = COMPILER_OPTIMIZE_COMPILATION_SPEED;
					else
 						Error("Invalid declaration syntax: ~A", declareForm);
					pushDynamicBinding(compilerSym, value);
					bindings = cons(compilerSym, bindings);
					optimizations = CDR(optimizations);
				}
			}
			declareForm = CDR(declareForm);
		}
		forms = CDR(forms);
	}
	if (bindings != NIL)
	{
		establishSpecialBindings(bindings);
	}
	return bindings;
}

static LispObj processSpecialDeclarations(LispObj declareForms)
{
	// Search declarations for special declarations.
	// specialDecs contains list of arguments declared special.
	//
	LispObj forms = declareForms;
	LispObj declarations = 0;
	LispObj decForm = 0;
	LispObj specialDecs = NIL;
	while (isCons(forms))
	{
		declarations = CDR(CAR(forms));	 // skip DECLARE symbol
		while (isCons(declarations))
		{
			decForm = CAR(declarations);
			if (isCons(decForm) && (CAR(decForm) == SPECIAL))
				specialDecs = LispCall2(lispAppend, CDR(decForm), specialDecs);
			declarations = CDR(declarations);
		}
		forms = CDR(forms);
	}
	return specialDecs;
}

//
//	Returns the number of type declarations that were added
static LispObj processTypeDeclarations(LispObj declareForms)
{
	LispObj forms = declareForms;
	LispObj declareList = 0;
	LispObj declareForm = 0;
	LispObj sym = 0;
	LispObj variables = 0;
	LispObj type = 0;
	LispObj numTypeDeclarations = 0;
	while (isCons(forms))
	{
		declareList = CDR(CAR(forms)); // skip DECLARE symbol
		while (isCons(declareList))
		{
			declareForm = CAR(declareList);
			if (!isCons(declareForm))
				Error("Invalid declaration: ~A", CAR(forms)); 
			sym = CAR(declareForm);
			if (sym == TYPE || (isSymbolTypeSpecifier(sym) != NIL))
			{
				if (sym == TYPE)
				{
					if (listLength(declareForm) < 3)
						Error("Invalid TYPE declaration: ~A", declareForm);
					type = CAR(CDR(declareForm));
					variables = CDR(CDR(declareForm));
					while (isCons(variables))
					{
						pushTypeDeclaration(CAR(variables), type);
						numTypeDeclarations += wrapInteger(1);
						variables = CDR(variables);
					}
				}
				else
				{
					if (listLength(declareForm) < 2)
						Error("Invalid TYPE declaration: ~A", declareForm);
					type = sym;
					variables = CDR(declareForm);
					while (isCons(variables))
					{
						pushTypeDeclaration(CAR(variables), type);
						numTypeDeclarations += wrapInteger(1);
						variables = CDR(variables);
					}
				}
			}
			declareList = CDR(declareList);
		}
		forms = CDR(forms);
	}
	return numTypeDeclarations;
}

static LispObj processIgnoreDeclarations(LispObj declareForms)
{
	LispObj forms = declareForms;
	LispObj declareForm = 0;
	LispObj f = 0;
	LispObj ignores = 0;
	LispObj bindings = NIL;
	LispObj i = 0;
	LispObj value = 0;
	LispObj sym = 0;
	LispObj compilerSym = 0;
	LispObj rec = 0;

	while (isCons(forms))
	{
		declareForm = CDR(CAR(forms)); // skip DECLARE symbol
		while (isCons(declareForm))
		{
			f = CAR(declareForm);
			if (CAR(f) == IGNORE_DECL || CAR(f) == IGNORABLE)
			{
				ignores = CDR(f);
				while (isCons(ignores))
				{
					rec = findVariableRecord(CAR(ignores));
					if (isCons(rec))
						CAR(CDR(CDR(CDR(rec)))) += wrapInteger(1);	// inc number of references
					ignores = CDR(ignores);
				}
			}
			declareForm = CDR(declareForm);
		}
		forms = CDR(forms);
	}
	return NIL;
}

static void pushTypeDeclaration(LispObj variable, LispObj type)
{
	setSymbolValue(COMPILER_VARIABLE_TYPES, 
		cons(variable, cons(type, symbolValue(COMPILER_VARIABLE_TYPES))));
}

static void popTypeDeclarations(LispObj numVars)
{
	LispObj f = 0;
	LispObj i = 0;
	if (numVars > 0)
	{
		f = symbolValue(COMPILER_VARIABLE_TYPES);
		while (i < numVars)
		{
			f = CDR(CDR(f));
			i += wrapInteger(1);
		}
		setSymbolValue(COMPILER_VARIABLE_TYPES, f);
	}
}

static LispObj processDynamicExtentDeclarations(LispObj declareForms)
{
	LispObj forms = declareForms;
	LispObj declareList = 0;
	LispObj declareForm = 0;
	LispObj sym = 0;
	LispObj variables = 0;
	LispObj numDEDeclarations = 0;
	while (isCons(forms))
	{
		declareList = CDR(CAR(forms)); // skip DECLARE symbol
		while (isCons(declareList))
		{
			declareForm = CAR(declareList);
			if (!isCons(declareForm))
				Error("Invalid declaration: ~A", CAR(forms)); 
			sym = CAR(declareForm);
			if (sym == DYNAMIC_EXTENT)
			{
				if (!isCons(CDR(declareForm)))
					Error("Invalid DYNAMIC-EXTENT declaration: ~A", declareForm);
				variables = CDR(declareForm);
				while (isCons(variables))
				{
					pushDEDeclaration(CAR(variables));
					numDEDeclarations += wrapInteger(1);
					variables = CDR(variables);
				}
			}
			declareList = CDR(declareList);
		}
		forms = CDR(forms);
	}
	return numDEDeclarations;
}

static void pushDEDeclaration(LispObj variable)
{
	setSymbolValue(COMPILER_DYNAMIC_EXTENT_VARS, 
		cons(variable, symbolValue(COMPILER_DYNAMIC_EXTENT_VARS)));
}

static void popDEDeclarations(LispObj numVars)
{
	LispObj f = 0;
	LispObj i = 0;
	if (numVars > 0)
	{
		f = symbolValue(COMPILER_DYNAMIC_EXTENT_VARS);
		while (i < numVars)
		{
			f = CDR(f);
			i += wrapInteger(1);
		}
		setSymbolValue(COMPILER_DYNAMIC_EXTENT_VARS, f);
	}
}

static LispObj isSymbolTypeSpecifier(LispObj symbol)
{
	if (symbol == IGNORE_DECL)
		return NIL;
	if (symbol == SYMBOL || symbol == FIXNUM ||
			symbol == BIGNUM || symbol == FLOAT_SYM
			|| symbol == COMPLEX || symbol == RATIO
			|| symbol == NUMBER)
		return T;
	return LispCall3(Funcall, GET, symbol, TYPE_DISCRIMINATOR);
}

//
//	If type not found, returns T.
//
static LispObj findVariableType(LispObj variable)
{
	LispObj f = symbolValue(COMPILER_VARIABLE_TYPES);
	while (isCons(f))
	{
		if (CAR(f) == variable)
			return CAR(CDR(f));
		f = CDR(CDR(f));
	}
	return T;
}

//
//	If type not found, returns T.
//
static LispObj findFunctionType(LispObj /*func*/)
{
	return T;
}

static LispObj isDynamicExtent(LispObj var)
{
	LispObj f = symbolValue(COMPILER_DYNAMIC_EXTENT_VARS);
	while (isCons(f))
	{
		if (CAR(f) == var)
			return T;
		f = CDR(f);
	}
	return NIL;
}

static LispObj compilerCheckTypes()
{
	return LispCall1(Funcall, COMPILER_CHECK_TYPES);
}

static LispObj compilerCheckArgsNum()
{
	return LispCall1(Funcall, COMPILER_CHECK_ARGS_NUM);
}

static LispObj compilerFoldConstants()
{
	return LispCall1(Funcall, COMPILER_FOLD_CONSTANTS);
}

static LispObj compilerInlineFunctions()
{
	return LispCall1(Funcall, COMPILER_INLINE_FUNCTIONS);
}

static LispObj compilerOptimizeTailRecursion()
{
	return LispCall1(Funcall, COMPILER_OPTIMIZE_TAIL_RECURSION);
}

static LispObj compilerCheckKeyArgs()
{
	return LispCall1(Funcall, COMPILER_CHECK_KEY_ARGS);
}

//
// Returns a list of the form:
// (CAPTURE_COMPILER_ENVIRONMENT cleanups lexical-macros callback compiled-environment)
static LispObj compileCaptureCompilerEnvironmentForm(LispObj x, LispObj dest)
{
	LispObj env = 0;
	LispObj lexicalMacros = 0;
	LispObj lexicalSymbolMacros = 0;
	LispObj callback = 0;
	
	env = CBcleanups();
	lexicalMacros = symbolValue(COLLECT_LEXICAL_MACROS);
	lexicalSymbolMacros = symbolValue(COLLECT_LEXICAL_SYMBOL_MACROS);
	callback = symbolValue(CAPTURED_LEXICAL_CALLBACK);
	
	compileLiteralForm(CAPTURE_COMPILER_ENVIRONMENT, Dest_Stack, T);
	compileLiteralForm(env, Dest_Stack, T);
	compileLiteralForm(lexicalMacros, Dest_Stack, T);
	compileLiteralForm(callback, Dest_Stack, T);
	compileLexEnvironment(env, callback);
	compileLiteralForm(lexicalSymbolMacros, Dest_Stack, T);
	SET_ARG_COUNT(6);
	MOV_EDI_NIL();		// null environment
	CALL_INDIRECT(LIST);
	ADD_ESP_NUM(24);
	if (dest == Dest_Stack)
		PUSH_EAX();
	return T;
//	return compileLiteralForm(list(
//		CAPTURE_COMPILER_ENVIRONMENT, env, lexicalMacros, callback, END_LIST), dest, T);
}