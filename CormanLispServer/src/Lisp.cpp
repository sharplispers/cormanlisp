//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		lisp.cpp
//		Contents:	Code to start Corman Lisp
//		History:	6/5/96  RGC  Created.
//					1/12/00 RGC Added EXECUTE_FINALIZERS, FINALIZATIONS_PENDING
//								to support post-GC finalization.
//                  9/22/03 RGC Removed name on InitializationEvent--the name was
//                              causing it to be shared with other processes (which
//                              caused problems if multiple instances were running).
//                              stringNode() now handles chars > 127 correctly, which
//                              means ct:c-string-to-lisp-string handles them correctly.
//
#include "stdafx.h"
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>
#include <stdarg.h>
#include <malloc.h>
#include <io.h>
#include <fcntl.h>
#include <direct.h>
#include <stdio.h>

#include "lisp.h"
#include "threadclasses.h"
#include "ErrorMessage.h"
#include "CormanLispServer.h"
#include "Version.h"

#ifdef X86
#include <wtypes.h>
#endif

#pragma warning (disable:4127)				// conditional expression is constant

DWORD QV_Index = 0;
DWORD Thread_Index = 0;

LispObj QV__[QV_MAX] = {0};
LispObj* QV = QV__;
unsigned long NumReturnValues;
unsigned long NumLispThreads = 0;
// Kernel Version ID format:
// two first digits - major version
// two last digits - minor version
// Example:
// Corman Lisp 3.1 = 301 (0301)
unsigned long KernelVersionID = VERSION_MAJOR * 100 + VERSION_MINOR;

LispObj searchSymbol(LispObj package, LispObj str);
LispObj addSymbol(LispObj package, LispObj key, LispObj value, xbool externFlag);
void growPackage(LispObj package);
long handleStructuredException(long exception, LPEXCEPTION_POINTERS info);
static LispObj createConsoleStream();

#define INIT_SYMBOL(sym) sym = findSymbol(#sym)

LispThreadQueue ThreadList;
extern CriticalSection TQCriticalSection;
PLEvent InitializationEvent(FALSE, TRUE, 0 /*"CormanLispInitialization" */);

LispObj* 
createNewQV()
{
	int i = 0;
	LispObj* qvr = new LispObj[QV_MAX];
	for (i = 0; i < QV_MAX; i++)
		qvr[i] = 0;

	// initialize to global QV
	for (i = 0; i < FirstSpecialSymbolEntry; i++)
	{
		qvr[i] = QV[i];
	}

	for (i = FirstSpecialSymbolEntry; i < QV_MAX; i++)
	{
		// skip over any special bindings other than the 
		// last (shared) one
		LispObj c = QV[i];
		while (isCons(c) && isCons(CDR(c)))
			c = CDR(c);
		qvr[i] = c;
	}

	return qvr;
}

void
initSymbols()
{
	long i = 0;

	GlobalQVPointer = (LispObj**)SysGlobalsAddr; // set up sys globals page at known address
	*GlobalQVPointer = QV;
	GlobalForeignJumpTable = (unsigned char**)(SysGlobalsAddr + 4);
	*GlobalForeignJumpTable = (unsigned char*)(SysGlobalsAddr + 0x1000); // starts at page 2nd page of sys globals

	GlobalForeignJumpTableNumEntries = (long*)(SysGlobalsAddr + 8);
	*GlobalForeignJumpTableNumEntries = 0;
	GlobalForeignJumpTableCapacity = (long*)(SysGlobalsAddr + 12);
	*GlobalForeignJumpTableCapacity = 0x3000 / SizeOfForeignJumpTableEntry;

	// create NIL symbol -- everyone needs this!
	QV[Nil_Index] = AllocVector(SYMBOL_SIZE);
	UVECTOR(NIL)[0] |= (SymbolType << 3);
	UVECTOR(NIL)[SYMBOL_NAME] = stringNode("NIL");			//	print value
	UVECTOR(NIL)[SYMBOL_VALUE] = cons(NIL, NIL);			//	value
	UVECTOR(NIL)[SYMBOL_PACKAGE] = NIL;						//	package
	UVECTOR(NIL)[SYMBOL_PROPERTY_LIST] = NIL;				//	property list
	UVECTOR(NIL)[SYMBOL_CONSTANT] = 0;						//	constant (change later)
	UVECTOR(NIL)[SYMBOL_FUNCTION] = cons(UNINITIALIZED, NIL);//	function
	UVECTOR(NIL)[SYMBOL_FUNCTION_TYPE]	= NIL;
	UVECTOR(NIL)[SYMBOL_JUMP_TABLE]	= 0;
	UVECTOR(NIL)[SYMBOL_VAR_TABLE]	= 0;

	// create T symbol
	QV[T_Index] = AllocVector(SYMBOL_SIZE);
	UVECTOR(T)[0] |= (SymbolType << 3);
	UVECTOR(T)[SYMBOL_NAME] = stringNode("T");				//	print value
	UVECTOR(T)[SYMBOL_VALUE] = cons(T, NIL);				//	value
	UVECTOR(T)[SYMBOL_PACKAGE] = NIL;						//	package
	UVECTOR(T)[SYMBOL_PROPERTY_LIST] = NIL;					//	property list
	UVECTOR(T)[SYMBOL_CONSTANT] = SYMBOL_CONSTANT_FLAG;		//	constant
	UVECTOR(T)[SYMBOL_FUNCTION] = cons(UNINITIALIZED, NIL);	//	function
	UVECTOR(T)[SYMBOL_FUNCTION_TYPE]	= NIL;
	UVECTOR(T)[SYMBOL_JUMP_TABLE]	= 0;
	UVECTOR(T)[SYMBOL_VAR_TABLE]	= 0;

	UVECTOR(NIL)[SYMBOL_CONSTANT] = SYMBOL_CONSTANT_FLAG;	//	make NIL constant
	UVECTOR(T)[SYMBOL_CONSTANT] = SYMBOL_CONSTANT_FLAG;		//	make T constant

//	INIT_SYMBOL(CHARACTER);	
	// create CHARACTER symbol
	QV[CHARACTER_Index] = AllocVector(SYMBOL_SIZE);
	UVECTOR(CHARACTER)[0] |= (SymbolType << 3);
	UVECTOR(CHARACTER)[SYMBOL_NAME] = stringNode("CHARACTER");		//	print value
	UVECTOR(CHARACTER)[SYMBOL_VALUE] = cons(UNINITIALIZED, NIL);	//	value
	UVECTOR(CHARACTER)[SYMBOL_PACKAGE] = NIL;						//	package
	UVECTOR(CHARACTER)[SYMBOL_PROPERTY_LIST] = NIL;					//	property list
	UVECTOR(CHARACTER)[SYMBOL_CONSTANT] = 0;						//	constant
	UVECTOR(CHARACTER)[SYMBOL_FUNCTION] = cons(UNINITIALIZED, NIL);	//	function
	UVECTOR(CHARACTER)[SYMBOL_FUNCTION_TYPE]	= NIL;
	UVECTOR(CHARACTER)[SYMBOL_JUMP_TABLE]	= 0;
	UVECTOR(CHARACTER)[SYMBOL_VAR_TABLE]	= 0;

	addSymbol(CL_PACKAGE, UVECTOR(NIL)[SYMBOL_NAME], NIL, 1);
	addSymbol(CL_PACKAGE, UVECTOR(T)[SYMBOL_NAME], T, 1);
	addSymbol(CL_PACKAGE, UVECTOR(CHARACTER)[SYMBOL_NAME], CHARACTER, 1);

	FINALIZATION_REGISTRY = 0;
	WEAK_PTR_REGISTRY = 0;
	THREAD_HEAP = 0;
	THREAD_HEAP_END = 0;
	STACK_MARKER_INDEX = 0;

	for (i = 0; i < NUM_STACK_MARKERS; i++)
		QV[STACK_MARKERS_Index + i] = 0;

    for (i = 0; i < NUM_FOREIGN_CELLS; i++)
		QV[FOREIGN_CELLS_Index + i] = 0;

    for (i = 0; i < NUM_RETURN_VALUES; i++)
		QV[RETURN_VALUES_Index + i] = 0;

	INIT_SYMBOL(QUOTE);	
	INIT_SYMBOL(FUNCTION);	
	INIT_SYMBOL(LIST);	
	INIT_SYMBOL(APPEND);	
	INIT_SYMBOL(MACRO);	
	INIT_SYMBOL(LAMBDA);
	INIT_SYMBOL(SETQ);
	INIT_SYMBOL(FUNCALL);
	INIT_SYMBOL(BLOCK);
	INIT_SYMBOL(PROGN);
	INIT_SYMBOL(COMPILER_STACK_ENV);
	INIT_SYMBOL(COMPILER_HEAP_ENV);
	INIT_SYMBOL(EBP);
	INIT_SYMBOL(EBX);
	INIT_SYMBOL(ARG_ENV);
	INIT_SYMBOL(ARG_NUM);
	INIT_SYMBOL(IF);
	INIT_SYMBOL(LET);
	INIT_SYMBOL(TAGBODY);
	INIT_SYMBOL(GO);
	INIT_SYMBOL(RETURN);
	INIT_SYMBOL(COMPILER_LABELS);
	INIT_SYMBOL(COMPILER_BLOCKS);
	INIT_SYMBOL(CONS);
	INIT_SYMBOL(COMPILER_CLEANUPS);
	INIT_SYMBOL(COMPILER_RUNTIME);
	INIT_SYMBOL(CATCH);
	INIT_SYMBOL(THROW);
	INIT_SYMBOL(VALUES);
	INIT_SYMBOL(APPLY);
	INIT_SYMBOL(FLET);
	INIT_SYMBOL(LABELS);
	INIT_SYMBOL(MACROLET);
	INIT_SYMBOL(ENVIRONMENT);
	INIT_SYMBOL(INTERNAL);
	INIT_SYMBOL(EXTERNAL);
	INIT_SYMBOL(STRING);
	INIT_SYMBOL(READ);
	INIT_SYMBOL(INTERN);
	INIT_SYMBOL(DECLARE);
	INIT_SYMBOL(SPECIAL);
	INIT_SYMBOL(EVAL);
	INIT_SYMBOL(BIT);
	INIT_SYMBOL(THE);
	INIT_SYMBOL(TYPEP);
	INIT_SYMBOL(FORMAT);
	INIT_SYMBOL(AREF);
	INIT_SYMBOL(SYMBOL);
	INIT_SYMBOL(FIXNUM);
	INIT_SYMBOL(BIGNUM);
	INIT_SYMBOL(FLOAT_SYM);
	INIT_SYMBOL(COMPLEX);
	INIT_SYMBOL(RATIO);
	INIT_SYMBOL(NUMBER);

	LETSTAR = findSymbol("LET*");	
	RETURN_FROM = findSymbol("RETURN-FROM");	
	EVAL_WHEN = findSymbol("EVAL-WHEN");	
	SYMBOL_VALUE_SYM = findSymbol("SYMBOL-VALUE");	
	SYMBOL_FUNCTION_SYM = findSymbol("SYMBOL-FUNCTION");	
	LISTSTAR = findSymbol("LIST*");	
	SPECIAL_OPERATOR = findSymbol("SPECIAL-OPERATOR");	
	Eof = findSymbol("EOF");	
	COMMA_TOKEN = findSymbol("%__COMMA__");	
	COMMA_DOT_TOKEN = findSymbol("%__COMMA_DOT__");	
	COMMA_ATSIGN_TOKEN = findSymbol("%__COMMA_ATSIGN__");	
	CREATE_CLOSURE = findSymbol("%CREATE-CLOSURE");	
	DOT = findSymbol(".");	
	RIGHT_PAREN = findSymbol(")");	
	READTABLE = findSymbol("*READTABLE*");
	STRINGNODE = findSymbol("%STRINGNODE");
	STANDARD_INPUT = findSymbol("*STANDARD-INPUT*");
	STANDARD_OUTPUT = findSymbol("*STANDARD-OUTPUT*");
	WRONG_NUMBER_OF_ARGS = findSymbol("%WRONG-NUMBER-OF-ARGS");
	INVALID_FIXNUM = findSymbol("%INVALID-FIXNUM");
	UNBOUND_VARIABLE = findSymbol("%UNBOUND-VARIABLE");
	CHECK_LIST = findSymbol("%CHECK-LIST");
	UNDEFINED_FUNCTION = findSymbol("%UNDEFINED-FUNCTION");
	PLUS = findSymbol("+");
	MINUS = findSymbol("-");
	ONEPLUS = findSymbol("1+");
	ONEMINUS = findSymbol("1-");
	NUMERIC_EQUAL = findSymbol("=");
	LESS_EQUAL = findSymbol("<=");
	GREATER_EQUAL = findSymbol(">=");
	LESS = findSymbol("<");
	GREATER = findSymbol(">");
	ALLOC_CONS = findSymbol("%ALLOC-CONS");
	ALLOC_VECTOR = findSymbol("%ALLOC-VECTOR");
	UNWIND_PROTECT = findSymbol("UNWIND-PROTECT");
	PUSH_CATCHER = findSymbol("%PUSH_CATCHER");
	POP_CATCHER = findSymbol("%POP_CATCHER");
	POP_SPECIALS = findSymbol("%POP_SPECIALS");
	THROW_EXCEPTION = findSymbol("%THROW_EXCEPTION");
	THROW_TAG = findSymbol("%THROW_TAG");
	THROW_RETURN_VALUE = findSymbol("%THROW_RETURN_VALUE");
	THROW_NUM_RETURN_VALUES = findSymbol("%THROW_NUM_RETURN_VALUES");
	MULTIPLE_RETURN_VALUES = NIL;
	LBYTE = findSymbol("BYTE");
	MULTIPLE_VALUE_CALL = findSymbol("MULTIPLE-VALUE-CALL");
	MACROEXPAND_ALL = findSymbol("MACROEXPAND-ALL");
	MACROEXPAND_ALL_EXCEPT_TOP = findSymbol("MACROEXPAND-ALL-EXCEPT-TOP");
	MACROEXPAND_INLINE = findSymbol("MACROEXPAND-INLINE");
	EMBEDDED_LAMBDAS = findSymbol("*EMBEDDED-LAMBDAS*");
	ENV_COUNTER = findSymbol("*ENV-COUNTER*");
	CONSOLE_INPUT = findSymbol("CONSOLE-INPUT");
	CONSOLE_OUTPUT = findSymbol("CONSOLE-OUTPUT");
	FILE_INPUT = findSymbol("FILE-INPUT");
	FILE_OUTPUT = findSymbol("FILE-OUTPUT*");
	STRING_INPUT = findSymbol("STRING-INPUT");
	STRING_OUTPUT = findSymbol("STRING-OUTPUT");
	TOP_LEVEL = findSymbol("*TOP-LEVEL*");
	LISPERROR = findSymbol("ERROR");
	LAMBDA_OPTIONAL = findSymbol("&OPTIONAL");
	LAMBDA_REST = findSymbol("&REST");
	LAMBDA_KEY = findSymbol("&KEY");
	LAMBDA_AUX = findSymbol("&AUX");
	LAMBDA_ALLOW_OTHER_KEYS = findSymbol("&ALLOW-OTHER-KEYS");
	LAMBDA_WHOLE = findSymbol("&WHOLE");
	LAMBDA_BODY = findSymbol("&BODY");

	WHITESPACE_CHAR_TYPE = findSymbol("WHITESPACE-CHAR-TYPE");
	CONSTITUENT_CHAR_TYPE = findSymbol("CONSTITUENT-CHAR-TYPE");
	TERMINATING_MACRO_CHAR_TYPE = findSymbol("TERMINATING-MACRO-CHAR-TYPE");
	NON_TERMINATING_MACRO_CHAR_TYPE = findSymbol("NON-TERMINATING-MACRO-CHAR-TYPE");
	DISPATCHING_TERMINATING_MACRO_CHAR_TYPE = findSymbol("DISPATCHING-TERMINATING-MACRO-CHAR-TYPE");
	DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE = findSymbol("DISPATCHING-NON-TERMINATING-MACRO-CHAR-TYPE");
	SINGLE_ESCAPE_CHAR_TYPE = findSymbol("SINGLE-ESCAPE-CHAR-TYPE");
	MULTIPLE_ESCAPE_CHAR_TYPE = findSymbol("MULTIPLE-ESCAPE-CHAR-TYPE");
	ILLEGAL_CHAR_TYPE = findSymbol("ILLEGAL-CHAR-TYPE");
	PACKAGE = findSymbol("*PACKAGE*");
	PACKAGE_LIST = findSymbol("*PACKAGE-LIST*");
	EQ = findSymbol("EQ");
	CAR_SYM = findSymbol("CAR");
	CDR_SYM = findSymbol("CDR");
	NULL_SYM = findSymbol("NULL");
	NOT = findSymbol("NOT");
	LAMBDA_SPECIAL_VARS = findSymbol("*LAMBDA-SPECIAL-VARS*");
	LAMBDA_DECLARATIONS = findSymbol("*LAMBDA-DECLARATIONS*");
	LAMBDA_SPECIAL_DECS = findSymbol("*LAMBDA-SPECIAL-DECS*");
	POP_SPECIAL_BINDINGS = findSymbol("%POP-SPECIAL-BINDINGS");
	PUSH_SPECIAL_BINDINGS = findSymbol("%PUSH-SPECIAL-BINDINGS");
	ESTABLISH_SPECIAL_BINDINGS = findSymbol("%ESTABLISH-SPECIAL-BINDINGS");
	COMMON_LISP_READTABLE = findSymbol("*COMMON-LISP-READTABLE*");

	// compiler switches
	COMPILER_SAVE_LAMBDAS = findSymbol("*COMPILER-SAVE-LAMBDAS*");
	COMPILER_SAVE_LAMBDA_LISTS = findSymbol("*COMPILER-SAVE-LAMBDA-LISTS*");
	COMPILER_SAVE_STACK_FRAME_INFO = findSymbol("*COMPILER-SAVE-STACK-FRAME-INFO*");
	COMPILER_SAVE_ENVIRONMENT_INFO = findSymbol("*COMPILER-SAVE-ENVIRONMENT-INFO*");
	COMPILER_SAVE_DEBUG_INFO = findSymbol("*COMPILER-SAVE-DEBUG-INFO*");
	COMPILER_SAFETY_LEVEL = findSymbol("*COMPILER-SAFETY-LEVEL*");

	LAMBDA_LIST = findSymbol("LAMBDA-LIST");
	ENVIRONMENT_INFO = findSymbol("ENVIRONMENT-INFO");
	STACK_FRAME = findSymbol("STACK-FRAME");
	DEBUG_INFO = findSymbol("DEBUG-INFO");
	FUNCTION_NAME = findSymbol("FUNCTION-NAME");
	CREATE_BIG_NUM = findSymbol("%CREATE-BIG-NUM");
	MULTIPLE_VALUE_PROG1 = findSymbol("MULTIPLE-VALUE-PROG1");
	CHECK_TYPE_BODY = findSymbol("CHECK-TYPE-BODY");
	HASH_TABLE = findSymbol("HASH-TABLE");
	FUNCALL_IGNORING_ERRORS = findSymbol("FUNCALL-IGNORING-ERRORS");
	SETF_AREF = findSymbol("(SETF AREF)");
	AREF_FUNCS = findSymbol("AREF-FUNCS");
	AREF_SETTER_FUNCS = findSymbol("AREF-SETTER-FUNCS");
	COMPILE_FUNCTION_CALL_FORM = findSymbol("COMPILE-FUNCTION-CALL-FORM");

	// declaration symbols
	COMPILATION_SPEED = findSymbol("COMPILATION-SPEED");
	DEBUG = findSymbol("DEBUG");
	SAFETY = findSymbol("SAFETY");
	SPACE = findSymbol("SPACE");
	SPEED = findSymbol("SPEED");
	DECLARATION = findSymbol("DECLARATION");
	DYNAMIC_EXTENT = findSymbol("DYNAMIC-EXTENT");
	FTYPE = findSymbol("FTYPE");
	IGNORABLE = findSymbol("IGNORABLE");
	IGNORE_DECL = findSymbol("IGNORE");
	INLINE = findSymbol("INLINE");
	NOTINLINE = findSymbol("NOTINLINE");
	OPTIMIZE = findSymbol("OPTIMIZE");
	TYPE = findSymbol("TYPE");

	CONSOLE_INPUT_STREAM = findSymbol("CONSOLE-INPUT-STREAM");
	CONSOLE_OUTPUT_STREAM = findSymbol("CONSOLE-OUTPUT-STREAM");

	COMPILER_WARN_ON_UNDEFINED_FUNCTION = findSymbol("*COMPILER-WARN-ON-UNDEFINED-FUNCTION*");
	COMPILER_WARN_ON_DYNAMIC_RETURN = findSymbol("*COMPILER-WARN-ON-DYNAMIC-RETURN*");
	COMPILER_FUNCTION_NAME = findSymbol("*COMPILER-FUNCTION-NAME*");
	COMPILER_USES_ENV = findSymbol("*COMPILER-USES_ENV*");

	GET_INTERNAL_RUN_TIME = findSymbol("GET-INTERNAL-RUN-TIME");
	GC_TIME_COUNTER = findSymbol("*GC-TIME-COUNTER*");
	PATHNAME = findSymbol("PATHNAME");
	DEALLOCATE_C_HEAP = findSymbol("DEALLOCATE-C-HEAP");
	SYSTEM_INTERNALS = findSymbol("*SYSTEM-INTERNALS*");
	GC_CRITICAL_SECTION = findSymbol("*GC-CRITICAL-SECTION*");
	TQ_CRITICAL_SECTION = findSymbol("*TQ-CRITICAL-SECTION*");
	PAGE_TABLE = findSymbol("*PAGE-TABLE*");
	DISASSEMBLY_OUTPUT_BUF = findSymbol("*DISASSEMBLY-OUTPUT-BUF*");
	SAVE_IMAGE_CLEANUP_FUNCS = findSymbol("*SAVE-IMAGE-CLEANUP-FUNCS*");
	LOAD_IMAGE_RESTORE_FUNCS = findSymbol("*LOAD-IMAGE-RESTORE-FUNCS*");
	CONSOLE_STREAM = findSymbol("CONSOLE-STREAM");
	FILE_STREAM = findSymbol("FILE-STREAM");
	STRING_STREAM = findSymbol("STRING-STREAM");
	CONSOLE_OVERFLOW_FUNCTION = findSymbol("CONSOLE-OVERFLOW-FUNCTION");
	CONSOLE_UNDERFLOW_FUNCTION = findSymbol("CONSOLE-UNDERFLOW-FUNCTION");
	FILE_OVERFLOW_FUNCTION = findSymbol("FILE-OVERFLOW-FUNCTION");
	FILE_UNDERFLOW_FUNCTION = findSymbol("FILE-UNDERFLOW-FUNCTION");
	STRING_OVERFLOW_FUNCTION = findSymbol("STRING-OVERFLOW-FUNCTION");
	STRING_UNDERFLOW_FUNCTION = findSymbol("STRING-UNDERFLOW-FUNCTION");
	COLLECT_LITERALS = findSymbol("COLLECT-LITERALS");
	EXECUTION_ADDRESS = findSymbol("EXECUTION-ADDRESS");
	FIND_FUNCTION_CURR_OFFSET = findSymbol("FIND_FUNCTION_CURR_OFFSET");
	FIND_FUNCTION_CURR_FUNC = findSymbol("FIND_FUNCTION_CURR_FUNC");
	FIND_FUNCTION_CURR_ADDR = findSymbol("FIND_FUNCTION_CURR_ADDR");
	ADDRESS_FIND_FUNCTION_CALLBACK = findSymbol("ADDRESS-FIND-FUNCTION-CALLBACK");
	TERMINAL_IO = findSymbol("*TERMINAL-IO*");
	TYPEEXPAND_ALL = findSymbol("TYPEEXPAND-ALL");
	LOOKUP_FTYPE = findSymbol("LOOKUP-FTYPE");
	LOAD_QV_REG = findSymbol("%LOAD-QV-REG");
	GET = findSymbol("GET");
	TYPE_DISCRIMINATOR = findSymbol("TYPE-DISCRIMINATOR");
	HEAP_CHECKING = findSymbol("*HEAP-CHECKING*");
	INVALID_OBJECT_COUNT = findSymbol("*INVALID-OBJECT-COUNT*");
	SHORT_SYM = findSymbol("SHORT-INTEGER");
	CODE_JUMP_TABLE_REFS = findSymbol("*CODE-JUMP-TABLE-REFS*");
	CODE_VAR_TABLE_REFS = findSymbol("*CODE-VAR-TABLE-REFS*");
	CODE_ENV_TABLE_REFS = findSymbol("*CODE-ENV-TABLE-REFS*");
	PLUS_EAX_EDX = findSymbol("%PLUS_EAX_EDX");
	MINUS_EAX_EDX = findSymbol("%MINUS_EAX_EDX");
	ERROR_SIGNAL = findSymbol("%ERROR");
	INTERNAL_TIME_UNITS_PER_SECOND = findSymbol("INTERNAL-TIME-UNITS-PER-SECOND");
	GC_TIME_UNITS_PER_SECOND = findSymbol("GC-TIME-UNITS-PER-SECOND");
	APPEND_REFS_TO_CODE = findSymbol("*APPEND-REFS-TO-CODE*");
	SINGLE_FLOAT = findSymbol("SINGLE-FLOAT");
	DOUBLE_FLOAT = findSymbol("DOUBLE-FLOAT");
	SHORT_FLOAT = findSymbol("SHORT-FLOAT");
	LONG_FLOAT = findSymbol("LONG-FLOAT");
	READ_DEFAULT_FLOAT_FORMAT = findSymbol("*READ-DEFAULT-FLOAT-FORMAT*");
	SYS_GLOBALS_ADDRESS = findSymbol("SYS-GLOBALS-ADDRESS");
	EXIT_THREAD_TAG = findSymbol("%EXIT_THREAD_TAG");
	DEALLOCATE_CRITICAL_SECTION = findSymbol("DEALLOCATE-CRITICAL-SECTION");
	COMPRESS_IMG = findSymbol("*COMPRESS-IMG*");
	ENV = findSymbol("ENV");
	ARGS = findSymbol("ARGS");
	FRAME = findSymbol("FRAME");
	GLOBAL = findSymbol("GLOBAL");
	COMPILED_SPECIALS = findSymbol("*COMPILED-SPECIALS*");
	GET_CURRENT_ENVIRONMENT = findSymbol("GET-CURRENT-ENVIRONMENT");
	CAPTURE_COMPILER_ENVIRONMENT = findSymbol("CAPTURE-COMPILER-ENVIRONMENT");
	COMPILER_FRAME_INFO = findSymbol("*COMPILER-FRAME-INFO*");
	COMPILER_ENVIRONMENT = findSymbol("*COMPILER-ENVIRONMENT*");
	THROW_SYSTEM_EXCEPTION = findSymbol("%THROW-SYSTEM-EXCEPTION");
	WITH_ONLY_LEXICALS = findSymbol("WITH-ONLY-LEXICALS");
	COMPILER_WARN_ON_UNUSED_VAR = findSymbol("*COMPILER-WARN-ON-UNUSED-VARIABLE*");
	WRITE = findSymbol("WRITE");
	INLINE_PROCLAIM_P = findSymbol("INLINE-PROCLAIM-P");
	FINALIZATION_PENDING = findSymbol("*FINALIZATION-PENDING*");
	EXECUTE_FINALIZERS = findSymbol("%EXECUTE-FINALIZERS");
	UNDEFINED_FUNCTIONS = findSymbol("*UNDEFINED-FUNCTIONS*");
	INIT_SYMBOL(GENSYM);
	COMPILER_CHECK_ARGS_NUM	= findSymbol("COMPILER-CHECK-ARGS-NUM");
	COMPILER_CHECK_TYPES	= findSymbol("COMPILER-CHECK-TYPES");
	COMPILER_FOLD_CONSTANTS	= findSymbol("COMPILER-FOLD-CONSTANTS");
	COMPILER_INLINE_FUNCTIONS = findSymbol("COMPILER-INLINE-FUNCTIONS");
	COMPILER_OPTIMIZE_TAIL_RECURSION = findSymbol("COMPILER-OPTIMIZE-TAIL-RECURSION");
	LOAD_LOCAL_HEAP = findSymbol("%LOAD-LOCAL-HEAP");
	COLLECT_LEXICAL_MACROS = findSymbol("*COLLECT-LEXICAL-MACROS*");
	LEXICAL_MACROS = findSymbol("*LEXICAL-MACROS*");
	COLLECT_LEXICAL_SYMBOL_MACROS = findSymbol("*COLLECT-LEXICAL-SYMBOL-MACROS*");
	LEXICAL_SYMBOL_MACROS = findSymbol("*LEXICAL-SYMBOL-MACROS*");
	COMPILER_LAMBDA_LIST = findSymbol("*COMPILER-LAMBDA-LIST*");
	REMOVE_TAIL_RECURSION = findSymbol("REMOVE-TAIL-RECURSION");
	LOOKUP_SETF_FUNCTION = findSymbol("LOOKUP-SETF-FUNCTION");
	SETF = findSymbol("SETF");
	COMPILER_CHECK_KEY_ARGS = findSymbol("COMPILER-CHECK-KEY-ARGS");
	INVALID_KEY_ARG = findSymbol("INVALID-KEY-ARG");
	LOCALLY = findSymbol("LOCALLY");
	LOAD_TIME_VALUE = findSymbol("LOAD-TIME-VALUE");
	GET_CALLBACK = findSymbol("GET-CALLBACK");
	HEAP_FAULT_HANDLER = findSymbol("%HEAP_FAULT_HANDLER");
	ERROR_OUTPUT = findSymbol("*ERROR-OUTPUT*");
	COMPILER_WARN_ON_ASSUMED_SPECIAL = findSymbol("*COMPILER-WARN-ON-ASSUMED-SPECIAL*");
	COMPILING_LAMBDA = findSymbol("*COMPILING-LAMBDA*");
	CAPTURED_LEXICAL_CALLBACK = findSymbol("*CAPTURED-LEXICAL-CALLBACK*");
	WARN = findSymbol("WARN");
	RESET_HASH_ID = findSymbol("RESET-HASH-ID");
    LOAD_TIME_VALUES = findSymbol("*LOAD-TIME-VALUES*");

	HEAP_0_START = findSymbol("HEAP_0_START");
	HEAP_0_END = findSymbol("HEAP_0_END");
	HEAP_1_START = findSymbol("HEAP_1_START");
	HEAP_1_END = findSymbol("HEAP_1_END");
	HEAP_2_START = findSymbol("HEAP_2_START");
	HEAP_2_END = findSymbol("HEAP_2_END");

	HEAP_0_GC_ID = findSymbol("HEAP_0_GC_ID");
	HEAP_1_GC_ID = findSymbol("HEAP_1_GC_ID");
	HEAP_2_GC_ID = findSymbol("HEAP_2_GC_ID");
}

char CormanLispDirectoryBuffer[_MAX_PATH];

void
initLisp()
{
	long i = 0;
	long tableSize = (6007 * 3);
	LispObj packageTable = 0;
	LispObj fp = 0;
	HANDLE appHandle = 0;
	DWORD numChars = 0;

	for (i = 0; i < QV_MAX; i++)
		QV[i] = 0;

	// enable heap management
	initializeGarbageCollector();

	// set up common lisp package
	CL_PACKAGE = AllocVector(PACKAGE_SIZE);

	UVECTOR(CL_PACKAGE)[0]						|= (PackageType << 3);
	UVECTOR(CL_PACKAGE)[PACKAGE_NAME]			= 0;
	UVECTOR(CL_PACKAGE)[PACKAGE_NICKNAMES]		= 0;
	UVECTOR(CL_PACKAGE)[PACKAGE_USE_LIST]		= 0;
	UVECTOR(CL_PACKAGE)[PACKAGE_USED_BY_LIST] = 0;
	UVECTOR(CL_PACKAGE)[PACKAGE_SHADOWING_SYMBOLS] = 0;
	UVECTOR(CL_PACKAGE)[PACKAGE_CAPACITY]		= wrapInteger(4001);		// capacity
	UVECTOR(CL_PACKAGE)[PACKAGE_COUNT]			= wrapInteger(0);			// current count
	UVECTOR(CL_PACKAGE)[PACKAGE_SYNC]			= 0;
	// set up package table
	packageTable = AllocVector(tableSize + 1);
	UVECTOR(packageTable)[0] |= (SimpleVectorType << 3);
	UVECTOR(packageTable)[ARRAY_SIMPLE_VECTOR_LENGTH] = wrapInteger(tableSize);
	UVECTOR(CL_PACKAGE)[PACKAGE_TABLE] = packageTable;	// storage

	SYMBOL_TABLE_COUNT = wrapInteger(FirstJumpTableEntry);
	SYMBOL_TABLE_VAR_COUNT = wrapInteger(FirstSpecialSymbolEntry);

	initSymbols();

	setSymbolValue(COMPILER_CLEANUPS, NIL);
	setSymbolValue(COMPILER_RUNTIME, NIL);
	setSymbolValue(MACROEXPAND_INLINE, T);
	setSymbolValue(EMBEDDED_LAMBDAS, NIL);
	setSymbolValue(TOP_LEVEL, NIL);
	setSymbolValue(PACKAGE, CL_PACKAGE);

	UVECTOR(CL_PACKAGE)[PACKAGE_NAME] = stringNode("COMMON-LISP");
	UVECTOR(CL_PACKAGE)[PACKAGE_NICKNAMES]		= NIL;
	UVECTOR(CL_PACKAGE)[PACKAGE_USE_LIST]		= NIL;
	UVECTOR(CL_PACKAGE)[PACKAGE_USED_BY_LIST]	= NIL;
	UVECTOR(CL_PACKAGE)[PACKAGE_SHADOWING_SYMBOLS] = NIL;
	UVECTOR(CL_PACKAGE)[PACKAGE_SYNC]			= NIL;

	USER_PACKAGE = packageNode(stringNode("COMMON-LISP-USER"));
	CORMANLISP_PACKAGE = packageNode(stringNode("CORMANLISP"));
	KEYWORD_PACKAGE = packageNode(stringNode("KEYWORD"));

	UVECTOR(USER_PACKAGE)[PACKAGE_USE_LIST]		= list(CORMANLISP_PACKAGE, END_LIST);
	UVECTOR(CORMANLISP_PACKAGE)[PACKAGE_USE_LIST]= cons(CL_PACKAGE, NIL);
	UVECTOR(KEYWORD_PACKAGE)[PACKAGE_USE_LIST]	= NIL;
//	UVECTOR(CL_PACKAGE)[PACKAGE_USE_LIST]		= cons(CORMANLISP_PACKAGE, NIL);

	setSymbolValue(PACKAGE_LIST, list(CL_PACKAGE, USER_PACKAGE, 
				CORMANLISP_PACKAGE, KEYWORD_PACKAGE, END_LIST));

	initKernelFunctions();
	setSymbolValue(READTABLE, readtableNode());

	EXECUTE_KEY = findKeyword("EXECUTE");
	setSymbolValue(COMMON_LISP_READTABLE, symbolValue(READTABLE));
	setSymbolValue(COMPILER_SAVE_LAMBDAS, T);
	setSymbolValue(COMPILER_SAVE_LAMBDA_LISTS, T);
	setSymbolValue(COMPILER_SAVE_STACK_FRAME_INFO, T);
	setSymbolValue(COMPILER_SAVE_ENVIRONMENT_INFO, T);
	setSymbolValue(COMPILER_SAVE_DEBUG_INFO, T);
	setSymbolValue(COMPILER_SAFETY_LEVEL, 0);
	setSymbolValue(GC_TIME_COUNTER, 0);

	SYSTEM_EXCEPTION = findKeyword("SYSTEM-EXCEPTION");
	EX_ACCESS_VIOLATION = findKeyword("EXCEPTION-ACCESS-VIOLATION");
	EX_ARRAY_BOUNDS_EXCEEDED = findKeyword("EXCEPTION-ARRAY-BOUNDS-EXCEEDED");
	EX_BREAKPOINT = findKeyword("EXCEPTION-BREAKPOINT");
	EX_DATATYPE_MISALIGNMENT = findKeyword("EXCEPTION-DATATYPE-MISALIGNMENT");
	EX_FLT_DENORMAL_OPERAND = findKeyword("EXCEPTION-FLT-DENORMAL-OPERAND");
	EX_FLT_DIVIDE_BY_ZERO = findKeyword("EXCEPTION-FLT-DIVIDE-BY-ZERO");
	EX_FLT_INEXACT_RESULT = findKeyword("EXCEPTION-FLT-INEXACT-RESULT");
	EX_FLT_INVALID_OPERATION = findKeyword("EXCEPTION-FLT-INVALID-OPERATION");
	EX_FLT_OVERFLOW = findKeyword("EXCEPTION-FLT-OVERFLOW");
	EX_FLT_STACK_CHECK = findKeyword("EXCEPTION-FLT-STACK_CHECK");
	EX_FLT_UNDERFLOW = findKeyword("EXCEPTION-FLT-UNDERFLOW");
	EX_ILLEGAL_INSTRUCTION = findKeyword("EXCEPTION-ILLEGAL-INSTRUCTION");
	EX_IN_PAGE_ERROR = findKeyword("EXCEPTION-IN-PAGE-ERROR");
	EX_INT_DIVIDE_BY_ZERO = findKeyword("EXCEPTION-INT-DIVIDE-BY-ZERO");
	EX_INVALID_DISPOSITION = findKeyword("EXCEPTION-INVALID-DISPOSITION");
	EX_NONCONTINUABLE_EXCEPTION = findKeyword("EXCEPTION-NONCONTINUABLE-EXCEPTION");
	EX_PRIV_INSTRUCTION = findKeyword("EXCEPTION-PRIV-INSTRUCTION");
	EX_SINGLE_STEP = findKeyword("EXCEPTION-SINGLE-STEP");
	EX_STACK_OVERFLOW = findKeyword("EXCEPTION-STACK-OVERFLOW");
	EX_USER_ABORT = findKeyword("EXCEPTION-USER-ABORT");
	
	UREF = findSymbol("UREF");
	UREF_SET = findSymbol("UREF-SET");
	COMPILER_CODE_BUFFER = findPLSymbol("*COMPILER-CODE-BUFFER*");
	setSymbolValue(COMPILER_CODE_BUFFER, NIL);

	COMPILER_OPTIMIZE_SPEED = findPLSymbol("*COMPILER-OPTIMIZE-SPEED*");
	COMPILER_OPTIMIZE_SAFETY = findPLSymbol("*COMPILER-OPTIMIZE-SAFETY*");
	COMPILER_OPTIMIZE_SPACE = findPLSymbol("*COMPILER-OPTIMIZE-SPACE*");
	COMPILER_OPTIMIZE_DEBUG = findPLSymbol("*COMPILER-OPTIMIZE-DEBUG*");
	COMPILER_OPTIMIZE_COMPILATION_SPEED = findPLSymbol("*COMPILER-OPTIMIZE-COMPILATION-SPEED*");
	COMPILER_COLLECT_JUMP_TABLE_REFS = findPLSymbol("*COMPILER-COLLECT-JUMP-TABLE-REFS*");
	COMPILER_COLLECT_VAR_TABLE_REFS = findPLSymbol("*COMPILER-COLLECT-VAR-TABLE-REFS*");

	COMPILER_VARIABLE_TYPES = findPLSymbol("*COMPILER-VARIABLE-TYPES*");
	COMPILER_DYNAMIC_EXTENT_VARS = findSymbol("*COMPILER-DYNAMIC-EXTENT-VARS*");

	CORMANLISP_DIRECTORY = findPLSymbol("*CORMANLISP-DIRECTORY*");

	CURRENT_THREAD_ID = findPLSymbol("*CURRENT-THREAD-ID*");
	CURRENT_THREAD_HANDLE = findPLSymbol("*CURRENT-THREAD-HANDLE*");
	CURRENT_PROCESS_ID = findPLSymbol("*CURRENT-PROCESS-ID*");
	CURRENT_PROCESS_HANDLE = findPLSymbol("*CURRENT-PROCESS-HANDLE*");

	ERROR_TRACE = findSymbol("*ERROR-TRACE*");
	TRACE_EXCEPTIONS = findPLSymbol("*TRACE-EXCEPTIONS*");

	Dest_EAX = findKeyword("DEST-EAX");
	Dest_EAX_Operand = findKeyword("DEST-EAX-OPERAND");
	Dest_Stack = findKeyword("DEST-STACK");
	Dest_Zero_Flag = findKeyword("DEST-ZERO-FLAG");

	INPUT_KEY = findKeyword("INPUT");
	OUTPUT_KEY = findKeyword("OUTPUT");
	BIDIRECTIONAL_KEY = findKeyword("BIDIRECTIONAL");
	SOURCE_FILE	= findPLSymbol("*SOURCE-FILE*");
	SOURCE_LINE	= findPLSymbol("*SOURCE-LINE*");
	SAVE_DEBUG_INFO	= findPLSymbol("*SOURCE-DEBUG-INFO*");
	SOURCE_LINES	= findPLSymbol("*SOURCE-LINES*");
	CORMANLISP_SERVER_DIRECTORY = findPLSymbol("*CORMANLISP-SERVER-DIRECTORY*");

	createSymbolTableEntry(COMPILER_CLEANUPS);
	createSymbolTableEntry(EMBEDDED_LAMBDAS);
	createSymbolTableEntry(ENV_COUNTER);
	createSymbolTableEntry(LAMBDA_SPECIAL_VARS);
	createSymbolTableEntry(LAMBDA_DECLARATIONS);
	createSymbolTableEntry(LAMBDA_SPECIAL_DECS);
	createSymbolTableEntry(COMPILER_CODE_BUFFER);
	createSymbolTableEntry(COMPILER_FUNCTION_NAME);
	createSymbolTableEntry(COMPILER_USES_ENV);

	createSymbolTableEntry(COMPILER_OPTIMIZE_SPEED);
	createSymbolTableEntry(COMPILER_OPTIMIZE_SAFETY);
	createSymbolTableEntry(COMPILER_OPTIMIZE_SPACE);
	createSymbolTableEntry(COMPILER_OPTIMIZE_DEBUG);
	createSymbolTableEntry(COMPILER_OPTIMIZE_COMPILATION_SPEED);

	createSymbolTableEntry(COMPILER_RUNTIME);
	createSymbolTableEntry(COMPILER_VARIABLE_TYPES);
	createSymbolTableEntry(COMPILER_DYNAMIC_EXTENT_VARS);

	createSymbolTableEntry(SOURCE_FILE);
	createSymbolTableEntry(SOURCE_LINE);
	createSymbolTableEntry(SAVE_DEBUG_INFO);
	createSymbolTableEntry(SOURCE_LINES);
	createSymbolTableEntry(ERROR_OUTPUT);

	createSymbolTableEntry(COMPILER_COLLECT_JUMP_TABLE_REFS);
	createSymbolTableEntry(COMPILER_COLLECT_VAR_TABLE_REFS);
	createSymbolTableEntry(CODE_JUMP_TABLE_REFS);
	createSymbolTableEntry(CODE_ENV_TABLE_REFS);
	createSymbolTableEntry(CODE_VAR_TABLE_REFS);

	createSymbolTableEntry(CURRENT_THREAD_ID);
	createSymbolTableEntry(CURRENT_THREAD_HANDLE);
	createSymbolTableEntry(CURRENT_PROCESS_ID);
	createSymbolTableEntry(CURRENT_PROCESS_HANDLE);

	createSymbolTableEntry(CHARACTER);
	createSymbolTableEntry(COMPRESS_IMG);
	createSymbolTableEntry(COMPILED_SPECIALS);
	createSymbolTableEntry(COMPILER_FRAME_INFO);
	createSymbolTableEntry(COMPILER_SAVE_STACK_FRAME_INFO);
	createSymbolTableEntry(COMPILER_ENVIRONMENT);
	createSymbolTableEntry(COMPILER_WARN_ON_UNUSED_VAR);
	createSymbolTableEntry(COMPILER_WARN_ON_ASSUMED_SPECIAL);
	createSymbolTableEntry(COLLECT_LEXICAL_MACROS);
	createSymbolTableEntry(LEXICAL_MACROS);
	createSymbolTableEntry(COLLECT_LEXICAL_SYMBOL_MACROS);
	createSymbolTableEntry(LEXICAL_SYMBOL_MACROS);
	createSymbolTableEntry(COMPILER_LAMBDA_LIST);
	createSymbolTableEntry(COMPILING_LAMBDA);
	createSymbolTableEntry(FINALIZATION_PENDING);
	createSymbolTableEntry(CAPTURED_LEXICAL_CALLBACK);
    createSymbolTableEntry(LOAD_TIME_VALUES);
    createSymbolTableEntry(PAGE_TABLE);
    createSymbolTableEntry(DISASSEMBLY_OUTPUT_BUF);

	setSymbolValue(COMPILER_OPTIMIZE_SPEED, 0);
	setSymbolValue(COMPILER_OPTIMIZE_SAFETY, wrapInteger(3));
	setSymbolValue(COMPILER_OPTIMIZE_SPACE, 0);
	setSymbolValue(COMPILER_OPTIMIZE_DEBUG, wrapInteger(3));
	setSymbolValue(COMPILER_OPTIMIZE_COMPILATION_SPEED, 0);

	setSymbolValue(COMPILER_VARIABLE_TYPES, NIL);
	setSymbolValue(COMPILER_DYNAMIC_EXTENT_VARS, NIL);

	setSymbolValue(COMPILER_WARN_ON_UNDEFINED_FUNCTION, T);
	setSymbolValue(COMPILER_WARN_ON_DYNAMIC_RETURN, NIL);
	setSymbolValue(COMPILER_WARN_ON_ASSUMED_SPECIAL, T);

	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)QV;
	setSymbolValue(SYSTEM_INTERNALS, fp);
	setSymbolValue(SAVE_IMAGE_CLEANUP_FUNCS, NIL);
	setSymbolValue(LOAD_IMAGE_RESTORE_FUNCS, NIL);

	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)&GCCriticalSection.m_sect;
	setSymbolValue(GC_CRITICAL_SECTION, fp);
	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)&TQCriticalSection.m_sect;
	setSymbolValue(TQ_CRITICAL_SECTION, fp);
	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)PageTable;
	setSymbolValue(PAGE_TABLE, fp);
	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)gDisassemblyOutputBuf;
	setSymbolValue(DISASSEMBLY_OUTPUT_BUF, fp);

	setSymbolValue(CONSOLE_INPUT_STREAM, createConsoleStream());
	setSymbolValue(STANDARD_INPUT, symbolValue(CONSOLE_INPUT_STREAM));
	setSymbolValue(CONSOLE_OUTPUT_STREAM, symbolValue(CONSOLE_INPUT_STREAM));
	setSymbolValue(STANDARD_OUTPUT, symbolValue(CONSOLE_OUTPUT_STREAM));
	setSymbolValue(TERMINAL_IO, symbolValue(CONSOLE_OUTPUT_STREAM));
	setSymbolValue(ERROR_OUTPUT, symbolValue(CONSOLE_OUTPUT_STREAM));

	setSymbolValue(COMPILER_COLLECT_JUMP_TABLE_REFS, NIL);
	setSymbolValue(COMPILER_COLLECT_VAR_TABLE_REFS, NIL);
	setSymbolValue(CODE_JUMP_TABLE_REFS, NIL);
	setSymbolValue(CODE_VAR_TABLE_REFS, NIL);
	setSymbolValue(CODE_ENV_TABLE_REFS, NIL);

	setSymbolValue(HEAP_CHECKING, NIL);
	setSymbolValue(INVALID_OBJECT_COUNT, 0);

	setSymbolValue(ERROR_TRACE, NIL);
	setSymbolValue(TRACE_EXCEPTIONS, NIL);
	setSymbolValue(SOURCE_FILE, NIL);
	setSymbolValue(SOURCE_LINE, NIL);
	setSymbolValue(SAVE_DEBUG_INFO, NIL);
	setSymbolValue(SOURCE_LINES, NIL);
	setSymbolValue(LAMBDA_SPECIAL_DECS, NIL);

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

	setSymbolValue(CORMANLISP_DIRECTORY, stringNode(CormanLispDirectoryBuffer));
	setSymbolValue(CORMANLISP_SERVER_DIRECTORY, stringNode(CormanLispServerDirectory));

	// initialize internal-time-units-per-second constant
	setSymbolValue(INTERNAL_TIME_UNITS_PER_SECOND, LispCall0(Get_Time_Units_Per_Second));
	setSymbolValue(GC_TIME_UNITS_PER_SECOND, symbolValue(INTERNAL_TIME_UNITS_PER_SECOND));
	setSymbolValue(APPEND_REFS_TO_CODE, NIL);
	setSymbolValue(READ_DEFAULT_FLOAT_FORMAT, SINGLE_FLOAT);

	setSymbolValue(CURRENT_THREAD_ID, NIL);
	setSymbolValue(CURRENT_THREAD_HANDLE, NIL);
	setSymbolValue(CURRENT_PROCESS_ID, createUnsignedLispInteger(GetCurrentProcessId()));
	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)GetCurrentProcess();
	setSymbolValue(CURRENT_PROCESS_HANDLE, fp);
	setSymbolValue(COMPRESS_IMG, NIL);
	setSymbolValue(COMPILED_SPECIALS, NIL);
	setSymbolValue(COMPILER_FRAME_INFO, NIL);
	setSymbolValue(COMPILER_ENVIRONMENT, NIL);
	setSymbolValue(COMPILER_WARN_ON_UNUSED_VAR, NIL);
	setSymbolValue(FINALIZATION_PENDING, NIL);
	setSymbolValue(UNDEFINED_FUNCTIONS, NIL);
	setSymbolValue(COLLECT_LEXICAL_MACROS, NIL);
	setSymbolValue(LEXICAL_MACROS, NIL);
	setSymbolValue(COLLECT_LEXICAL_SYMBOL_MACROS, NIL);
	setSymbolValue(LEXICAL_SYMBOL_MACROS, NIL);
	setSymbolValue(COMPILER_LAMBDA_LIST, NIL);
	setSymbolValue(LAMBDA_SPECIAL_VARS, NIL);
	setSymbolValue(COMPILING_LAMBDA, NIL);
	setSymbolValue(CAPTURED_LEXICAL_CALLBACK, NIL);
    setSymbolValue(LOAD_TIME_VALUES, NIL);

	setSymbolValue(HEAP_0_START, foreignNode());
	setSymbolValue(HEAP_0_END, foreignNode());
	setSymbolValue(HEAP_1_START, foreignNode());
	setSymbolValue(HEAP_1_END, foreignNode());
	setSymbolValue(HEAP_2_START, foreignNode());
	setSymbolValue(HEAP_2_END, foreignNode());

	foreignPtr(symbolValue(HEAP_0_START))	= (LispObj)EphemeralHeap1.start;	
	foreignPtr(symbolValue(HEAP_0_END))		= (LispObj)EphemeralHeap1.end;	
	foreignPtr(symbolValue(HEAP_1_START))	= (LispObj)EphemeralHeap2.start;	
	foreignPtr(symbolValue(HEAP_1_END))		= (LispObj)EphemeralHeap2.end;	
	foreignPtr(symbolValue(HEAP_2_START))	= (LispObj)LispHeap1.start;	
	foreignPtr(symbolValue(HEAP_2_END))		= (LispObj)LispHeap1.end;

	createSymbolTableEntry(HEAP_0_START);
	createSymbolTableEntry(HEAP_0_END);
	createSymbolTableEntry(HEAP_1_START);
	createSymbolTableEntry(HEAP_1_END);
	createSymbolTableEntry(HEAP_2_START);
	createSymbolTableEntry(HEAP_2_END);

	createSymbolTableEntry(HEAP_0_GC_ID);
	createSymbolTableEntry(HEAP_1_GC_ID);
	createSymbolTableEntry(HEAP_2_GC_ID);
	setSymbolValue(HEAP_0_GC_ID, 0);
	setSymbolValue(HEAP_1_GC_ID, 0);
	setSymbolValue(HEAP_2_GC_ID, 0);
}

void
exitLisp()
{
}

//
//	Create bidirection console stream (*terminal-io*)
//
static LispObj
createConsoleStream()
{
	LispObj bufLen = 0;
	LispObj buf = 0;
	LispObj s = 0;

	s = AllocVector(STREAM_SIZE);
	setUvectorType(s, StreamType);
	bufLen = wrapInteger(0x0800);		// use 2k buffer
	buf = charVector(bufLen);

	streamName(s)				= symbolName(TERMINAL_IO);
	streamUnderflowFunc(s)		= CONSOLE_UNDERFLOW_FUNCTION;
	streamOverflowFunc(s)		= CONSOLE_OVERFLOW_FUNCTION;
	streamPosition(s)			= 0;
	streamColPosition(s)		= 0;
	streamInputBuffer(s)		= buf;
	streamInputBufferLength(s)	= bufLen;
	streamInputBufferPos(s)		= 0;
	streamInputBufferNum(s)		= 0;
	streamHandle(s)				= 0;
	streamSubclass(s)			= CONSOLE_STREAM;
	streamBinary(s)				= NIL;
	streamOpen(s)				= T;
	streamDirection(s)			= BIDIRECTIONAL_KEY;
	streamInteractive(s)		= T;
	streamElementType(s)		= CHARACTER;
	streamAssociatedStreams(s)	= NIL;
	streamOutputBuffer(s)		= charVector(bufLen);
	streamOutputBufferLength(s) = bufLen;
	streamOutputBufferPos(s)	= 0;
	streamLineNumber(s)			= 0;

	return s;
}

LispObj
inputFileStreamNode(LispObj path)
{
	LispObj bufLen = 0;
	LispObj buf = 0;
	LispObj s = 0;
	HANDLE handle = 0;
	
	handle = CreateFile(
	(char*)byteArrayStart(nullTerminate(path)), // pointer to name of the file
		GENERIC_READ,						 // access (read-write) mode
		FILE_SHARE_READ,					 // share mode
		NULL,								 // pointer to security descriptor
		OPEN_EXISTING,						 // how to create
		FILE_ATTRIBUTE_NORMAL,				 //	file attributes
		NULL);								 // handle to file with attributes to copy

	if (handle == INVALID_HANDLE_VALUE)
		Error("Could not open file ~A for reading, error code = ~A", 
			path, createLispInteger(GetLastError()));

	s = AllocVector(STREAM_SIZE);
	setUvectorType(s, StreamType);
	bufLen = wrapInteger(0x0800);		// use 2k buffer
	buf = charVector(bufLen);

	streamName(s)				= path;
	streamUnderflowFunc(s)		= FILE_UNDERFLOW_FUNCTION;
	streamOverflowFunc(s)		= NIL;
	streamPosition(s)			= 0;
	streamColPosition(s)		= 0;
	streamInputBuffer(s)		= buf;
	streamInputBufferLength(s)	= bufLen;
	streamInputBufferPos(s)		= 0;
	streamInputBufferNum(s)		= 0;
	streamHandle(s)				= createLispInteger((unsigned long)handle);
	streamSubclass(s)			= FILE_STREAM;
	streamBinary(s)				= NIL;
	streamOpen(s)				= T;
	streamDirection(s)			= INPUT_KEY;
	streamInteractive(s)		= T;
	streamElementType(s)		= CHARACTER;
	streamAssociatedStreams(s)	= NIL;
 	streamOutputBuffer(s)		= NIL;
	streamOutputBufferLength(s) = 0;
	streamOutputBufferPos(s)	= 0;
	streamLineNumber(s)			= 0;

	return s;
}

LispObj
outputFileStreamNode(LispObj path)
{
	LispObj bufLen = 0;
	LispObj buf = 0;
	LispObj s = 0;
	HANDLE handle = 0;
	
	handle = CreateFile(
		(char*)byteArrayStart(nullTerminate(path)), // pointer to name of the file
		GENERIC_WRITE,						 // access (read-write) mode
		FILE_SHARE_READ,					 // share mode
		NULL,								 // pointer to security descriptor
		CREATE_ALWAYS,						 // how to create
		FILE_ATTRIBUTE_NORMAL,				 //	file attributes
		NULL);								 // handle to file with attributes to copy

	if (handle == INVALID_HANDLE_VALUE)
		Error("Could not open file ~A for writing, error code = ~A", 
			path, createLispInteger(GetLastError()));
	
	s = AllocVector(STREAM_SIZE);
	setUvectorType(s, StreamType);
	bufLen = wrapInteger(0x0800);		// use 2k buffer
	buf = charVector(bufLen);

	streamName(s)				= path;
	streamUnderflowFunc(s)		= NIL;
	streamOverflowFunc(s)		= FILE_OVERFLOW_FUNCTION;
	streamPosition(s)			= 0;
	streamColPosition(s)		= 0;
	streamInputBuffer(s)		= NIL;
	streamInputBufferLength(s)	= 0;
	streamInputBufferPos(s)		= 0;
	streamInputBufferNum(s)		= 0;
	streamHandle(s)				= createLispInteger((unsigned long)handle);
	streamSubclass(s)			= FILE_STREAM;
	streamBinary(s)				= NIL;
	streamOpen(s)				= T;
	streamDirection(s)			= OUTPUT_KEY;
	streamInteractive(s)		= T;
	streamElementType(s)		= CHARACTER;
	streamAssociatedStreams(s)	= NIL;
	streamOutputBuffer(s)		= buf;
	streamOutputBufferLength(s) = bufLen;
	streamOutputBufferPos(s)	= 0;
	streamLineNumber(s)			= 0;

	return s;
}

int
lispmain()
{
	__try
	{
		LispLoop();
	}
	__except (handleStructuredException(GetExceptionCode(), GetExceptionInformation()))
	{
	}
	exitLisp();
	return 0;
}

int
lispSecondary(LispObj func)
{
	__try
	{
		LispCall1(Funcall, func);
	}
	__except (handleStructuredException(GetExceptionCode(), GetExceptionInformation()))
	{
	}
	return 0;
}

static void
throwOSException()
{
	LispObj ex = 0;
	long exceptionCode = 0;

	__asm mov exceptionCode, eax

	switch (exceptionCode)
	{
	case EXCEPTION_ACCESS_VIOLATION:			ex = EX_ACCESS_VIOLATION;			break;
	case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:		ex = EX_ARRAY_BOUNDS_EXCEEDED;		break;
	case EXCEPTION_BREAKPOINT:					ex = EX_BREAKPOINT;					break;
	case EXCEPTION_DATATYPE_MISALIGNMENT:		ex = EX_DATATYPE_MISALIGNMENT;		break;
	case EXCEPTION_FLT_DENORMAL_OPERAND:		ex = EX_FLT_DENORMAL_OPERAND;		break;
	case EXCEPTION_FLT_DIVIDE_BY_ZERO:			ex = EX_FLT_DIVIDE_BY_ZERO;			break;
	case EXCEPTION_FLT_INEXACT_RESULT:			ex = EX_FLT_INEXACT_RESULT;			break;
	case EXCEPTION_FLT_INVALID_OPERATION:		ex = EX_FLT_INVALID_OPERATION;		break;
	case EXCEPTION_FLT_OVERFLOW:				ex = EX_FLT_OVERFLOW;				break;
	case EXCEPTION_FLT_STACK_CHECK:				ex = EX_FLT_STACK_CHECK;			break;
	case EXCEPTION_FLT_UNDERFLOW:				ex = EX_FLT_UNDERFLOW;				break;
	case EXCEPTION_ILLEGAL_INSTRUCTION:			ex = EX_ILLEGAL_INSTRUCTION;		break;
	case EXCEPTION_IN_PAGE_ERROR: 				ex = EX_IN_PAGE_ERROR;				break;
	case EXCEPTION_INT_DIVIDE_BY_ZERO: 			ex = EX_INT_DIVIDE_BY_ZERO;			break;
	case EXCEPTION_INVALID_DISPOSITION:			ex = EX_INVALID_DISPOSITION;		break;
	case EXCEPTION_NONCONTINUABLE_EXCEPTION:	ex = EX_NONCONTINUABLE_EXCEPTION;	break;
	case EXCEPTION_PRIV_INSTRUCTION: 			ex = EX_PRIV_INSTRUCTION;			break;
	case EXCEPTION_SINGLE_STEP:					ex = EX_SINGLE_STEP;				break;
	case EXCEPTION_STACK_OVERFLOW: 				ex = EX_STACK_OVERFLOW;				break;
	}

	LispCall2(Funcall, THROW_SYSTEM_EXCEPTION, ex);
}

long gStackOverflowAddress = 0;

// This function stub is used by plungeBackIn() to force
// a call to the ThrowUserException() function.
void __declspec(naked) CallThrowOSExceptionStub()
{
	__asm push edx
	__asm jmp throwOSException
	// __asm call throwOSException;
}

// a handler to be called by the system, used in compiled lisp code
EXCEPTION_DISPOSITION __cdecl Heap_Fault_Handler(struct _EXCEPTION_RECORD* ExceptionRecord,
    void* EstablisherFrame, struct _CONTEXT* ContextRecord, void* DispatcherContext)
{
	BOOL ret = 0;
	byte* attemptedAddress = 0;
	if (ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
	{
		attemptedAddress = (byte*)(ExceptionRecord->ExceptionInformation[1]);
		if (handleMemoryAccessException(attemptedAddress))
			return ExceptionContinueExecution;	// handled page fault--try again

        // dump information about the problem
        WriteMemoryReportTask(attemptedAddress, ContextRecord);
	}
	return ExceptionContinueSearch;
}

long handleStructuredException(long exception, LPEXCEPTION_POINTERS info)
{
	byte* attemptedAddress = 0;
	BOOL ret = 0;
	unsigned long ip = 0;

	if (exception == EXCEPTION_ACCESS_VIOLATION)
	{
		attemptedAddress = (byte*)info->ExceptionRecord->ExceptionInformation[1];
		ret = handleMemoryAccessException(attemptedAddress);
		if (ret)
			return EXCEPTION_CONTINUE_EXECUTION;

        // dump information about the problem
        WriteMemoryReportTask(attemptedAddress, info->ContextRecord);

		ip = info->ContextRecord->Eip;
		// check for trying to execute code in the collected heap
		// debug purposes only
		if (inLispHeap2AddressRange(ip))
		{
			ip = tranlateToOtherPrimaryHeap(ip); 
			info->ContextRecord->Eip = (long)ip;
			//DebugBreak();
			OutputDebugString("Error: trying to execute code in the collected heap!!");

			return EXCEPTION_CONTINUE_EXECUTION;
		}
	}	
	if (exception == CONTROL_C_EXIT)
		return EXCEPTION_CONTINUE_EXECUTION;

	info->ContextRecord->Edx = info->ContextRecord->Eip;
	info->ContextRecord->Eip = (long)CallThrowOSExceptionStub;
	info->ContextRecord->Eax = exception;
	__asm mov dword ptr gStackOverflowAddress, esp

	return EXCEPTION_CONTINUE_EXECUTION;
}

extern char LispImageName[];
char missingImgMessage[MAX_PATH+64] = "Could not find the image file ";

void LispLoop()
{
	long firstTime = 1;
	long initializing = 0;
	LispObj x = 0;
	LispObj val = 0;
	LispObj vals = 0;

	while (1)
	{
		try
		{
			if (firstTime)
			{
				firstTime = 0;
				initializing = 1;

				if (*LispImageName)		// if an image name was provided
				{
					// see if the image file exists
					if (LispCall1(Probe_File, stringNode(LispImageName)) == NIL)
					{
						strcat_s(missingImgMessage, sizeof(missingImgMessage), LispImageName);
						InitializationEvent.SetEvent();		// done initializing				
						Error(missingImgMessage);
					}
					LispCall1(LoadLispImage, stringNode(LispImageName));
				}
				else
					throw stringNode("");	// ignorable message
				x = 0;
				val = 0;
				vals = 0;
				initializing = 0;
				return;
			//	if (isFunction(symbolValue(TOP_LEVEL)))
			//		LispCall(Apply, symbolValue(TOP_LEVEL), NIL);
			}
			setSymbolValue(SOURCE_LINE, NIL);
			x = LispCall2(Funcall, symbolFunction(READ), symbolValue(STANDARD_INPUT));
			val = eval(x, NIL);
			if (NumReturnValues == 1)
			{
				LispCall2(Write, val, symbolValue(STANDARD_OUTPUT));	// just echo for now
				LispCall1(Terpri, symbolValue(STANDARD_OUTPUT));
				LispCall1(Force_Output, symbolValue(STANDARD_OUTPUT));
			}
			else
			if (NumReturnValues > 1)
			{
				vals = MULTIPLE_RETURN_VALUES;
				while (isCons(vals))
				{
					LispCall2(Write, CAR(vals), symbolValue(STANDARD_OUTPUT));	// just echo for now
					LispCall1(Terpri, symbolValue(STANDARD_OUTPUT));
					LispCall1(Force_Output, symbolValue(STANDARD_OUTPUT));
					vals = CDR(vals);
				}
			}
			else
				LispCall1(Force_Output, symbolValue(STANDARD_OUTPUT));
		}
		catch (LispObj x)
		{
			// This is just to make sure that the initialization is flagged
			// as done in case LOAD-IMAGE threw an exception. We don't want other
			// threads to keep waiting in this case.
			if (initializing)
			{
				initializing = 0;
				InitializationEvent.SetEvent();		// done initializing
			}
				
			LispCall2(Write, x, symbolValue(STANDARD_OUTPUT));	// just echo for now
			LispCall1(Terpri, symbolValue(STANDARD_OUTPUT));
		}					
	}
}

//
//	LispCall()
//	Facilitates calling of a Lisp Function from C++.
//	This is stack-direction specific -- assumes stack grows down!!!
//	Portability issue here!!
//
#ifdef X86

#ifdef WINNT_ONLY
#define SETUP_LISP_CALL(numargs)		\
	__asm push esi						\
	__asm push ecx						\
	__asm mov eax, fs:0018h				\
	__asm mov edx, dword ptr QV_Index	\
	__asm mov esi, dword ptr [eax + edx*4 + 0e10h]	\
	__asm mov ecx, numargs

#define END_LISP_CALL()					\
	__asm mov dword ptr NumReturnValues, ecx   \
	__asm pop ecx						\
	__asm pop esi

#else
#define SETUP_LISP_CALL(numargs)		\
	__asm push ebp						\
	__asm mov  ebp, esp					\
	__asm push esi						\
	__asm push edi						\
	__asm push ecx						\
	__asm push ebx						\
	__asm call ThreadQV					\
	__asm mov esi, eax					\
	__asm mov ecx, numargs				\
	__asm mov edi, [esi]

#define END_LISP_CALL()					\
	__asm mov dword ptr NumReturnValues, ecx   \
	__asm pop ebx						\
	__asm pop ecx						\
	__asm pop edi						\
	__asm pop esi						\
	__asm mov esp, ebp					\
	__asm pop ebp						\
	__asm ret
#endif

__declspec(naked) LispObj 
LispCall0(LispFunc func)
{
	SETUP_LISP_CALL(0);

	__asm	call	dword ptr [func]

	END_LISP_CALL();
}

__declspec(naked) LispObj 
LispCall1(LispFunc func, LispObj a1)
{
	SETUP_LISP_CALL(1);

	__asm	mov		eax, dword ptr [a1]
	__asm	push	eax
	__asm	call	dword ptr [func]
	__asm	add		esp, 4

	END_LISP_CALL();
}

__declspec(naked) LispObj 
LispCall2(LispFunc func, LispObj a1, LispObj a2)
{
	SETUP_LISP_CALL(2);

	__asm	mov		eax, dword ptr [a1]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a2]
	__asm	push	eax
	__asm	call	dword ptr [func]
	__asm	add		esp, 8

	END_LISP_CALL();
}

__declspec(naked) LispObj 
LispCall3(LispFunc func, LispObj a1, LispObj a2, LispObj a3)
{
	SETUP_LISP_CALL(3);

	__asm	mov		eax, dword ptr [a1]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a2]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a3]
	__asm	push	eax
	__asm	call	dword ptr [func]
	__asm	add		esp, 12

	END_LISP_CALL();
}

__declspec(naked) LispObj 
LispCall4(LispFunc func, LispObj a1, LispObj a2, LispObj a3, LispObj a4)
{
	SETUP_LISP_CALL(4);

	__asm	mov		eax, dword ptr [a1]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a2]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a3]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a4]
	__asm	push	eax
	__asm	call	dword ptr [func]
	__asm	add		esp, 16

	END_LISP_CALL();
}

__declspec(naked) LispObj 
LispCall5(LispFunc func, LispObj a1, LispObj a2, LispObj a3, LispObj a4, LispObj a5)
{
	SETUP_LISP_CALL(5);

	__asm	mov		eax, dword ptr [a1]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a2]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a3]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a4]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a5]
	__asm	push	eax
	__asm	call	dword ptr [func]
	__asm	add		esp, 20

	END_LISP_CALL();
}

__declspec(naked) LispObj 
LispCall6(LispFunc func, LispObj a1, LispObj a2, LispObj a3, LispObj a4, LispObj a5, LispObj a6)
{
	SETUP_LISP_CALL(6);

	__asm	mov		eax, dword ptr [a1]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a2]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a3]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a4]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a5]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a6]
	__asm	push	eax
	__asm	call	dword ptr [func]
	__asm	add		esp, 24

	END_LISP_CALL();
}

__declspec(naked) LispObj 
LispCall7(LispFunc func, LispObj a1, LispObj a2, LispObj a3, LispObj a4, LispObj a5, LispObj a6, LispObj a7)
{
	SETUP_LISP_CALL(7);

	__asm	mov		eax, dword ptr [a1]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a2]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a3]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a4]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a5]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a6]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a7]
	__asm	push	eax
	__asm	call	dword ptr [func]
	__asm	add		esp, 28

	END_LISP_CALL();
}

__declspec(naked) LispObj 
LispCall8(LispFunc func, LispObj a1, LispObj a2, LispObj a3, LispObj a4, 
		 LispObj a5, LispObj a6, LispObj a7, LispObj a8)
{
	SETUP_LISP_CALL(8);

	__asm	mov		eax, dword ptr [a1]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a2]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a3]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a4]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a5]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a6]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a7]
	__asm	push	eax
	__asm	mov		eax, dword ptr [a8]
	__asm	push	eax
	__asm	call	dword ptr [func]
	__asm	add		esp, 32

	END_LISP_CALL();
}

#else
#error Not Implemented
#endif

//
//	Lookup the requested symbol in the common lisp package.
//	If not there add it. Then return the symbol.
//
LispObj 
findSymbol(const char* name)
{
	LispObj sym = 0;
	LispObj str = 0;

	// first try to find the requested symbol
	str = stringNode(name);
	sym = searchSymbol(CL_PACKAGE, str);
	if (sym != UNINITIALIZED)
		return sym;

	// need to add the symbol
	sym = symbolNode(str);
	return addSymbol(CL_PACKAGE, str, sym, FALSE);
}

// look up the requested symbol in the Corman Lisp package
LispObj 
findPLSymbol(const char* name)
{
	LispObj sym = 0;
	LispObj str = 0;

	// first try to find the requested symbol
	str = stringNode(name);
	sym = searchSymbol(CORMANLISP_PACKAGE, str);
	if (sym != UNINITIALIZED)
		return sym;

	// need to add the symbol
	sym = symbolNode(str);
	return addSymbol(CORMANLISP_PACKAGE, str, sym, FALSE);
}

LispObj 
findKeyword(const char* name)
{
	LispObj str = 0;

	str = stringNode(name);
	return findKeywordSym(str);
}

LispObj 
findKeywordSym(LispObj str)
{
	LispObj sym = 0;

	// first try to find the requested symbol
	sym = searchSymbol(KEYWORD_PACKAGE, str);
	if (sym != UNINITIALIZED)
		return sym;

	// need to add the symbol
	sym = symbolNode(str);
	addSymbol(KEYWORD_PACKAGE, str, sym, FALSE);
	setSymbolValue(sym, sym);
	setConstantSymbol(sym);
	setSpecialSymbol(sym);
	// need to set package also
	return sym;
}

//
//	searchSymbol()
//	Looks for the passed symbol in the CL package
//	Returns UNINITIALIZED if error, symbol otherwise.
//
LispObj
searchSymbol(LispObj package, LispObj str)
{
	long tableSize = 0;
	long h = 0;
	long offset = 0;

	// first try to find the requested symbol
	tableSize = GET_PACKAGE_CAPACITY(package);
	h = integer(LispCall2(Package_Hash_Index, package, str));
	if (!isString(PACKAGE_ENTRY(package, h)[0]))
		return UNINITIALIZED;
	if (!lispStringsEqual(str, PACKAGE_ENTRY(package, h)[0]))
	{		
		// if not found there, look from the end
		offset = (tableSize - 1) - h;
		if (offset == 0) offset++;
		while (TRUE)
		{
			h -= offset;
			if (h < 0)
				h += tableSize;
			if (!isString(PACKAGE_ENTRY(package, h)[0]))
				return UNINITIALIZED;
			if (lispStringsEqual(str, PACKAGE_ENTRY(package, h)[0]))
				return PACKAGE_ENTRY(package, h)[1];
		}
	}
	return PACKAGE_ENTRY(package, h)[1];
}

LispObj
addSymbol(LispObj package, LispObj key, LispObj value, xbool externFlag)
{
	long h = 0;
	long offset = 0;

	long tableSize = GET_PACKAGE_CAPACITY(package);
	long count = integer(UVECTOR(package)[PACKAGE_COUNT]);
	
	// grow the hash table if necessary
	if ((double)count > (PACKAGE_RESIZE_THRESHOLD * (double)tableSize))
		growPackage(package);

	h = integer(LispCall2(Package_Hash_Index, package, key));
	if (isString(PACKAGE_ENTRY(package, h)[0]) && !lispStringsEqual(PACKAGE_ENTRY(package, h)[0], key))
	{
		// if occupied but not equal to key, loop
		offset = (tableSize - 1) - h;
		if (offset == 0) offset++;
		while (TRUE)
		{
			h -= offset;
			if (h < 0)
				h += tableSize;
			if ((!isString(PACKAGE_ENTRY(package, h)[0])) || lispStringsEqual(PACKAGE_ENTRY(package, h)[0], key))
				break;
		}
	}

	if (!isString(PACKAGE_ENTRY(package, h)[0]))		// if not already occupied
		PACKAGE_ENTRY(package, h)[0] = key;
	PACKAGE_ENTRY(package, h)[1] = value;
	PACKAGE_ENTRY(package, h)[2] = externFlag ? wrapInteger(1) : wrapInteger(0);
	UVECTOR(package)[PACKAGE_COUNT] =
		wrapInteger(integer(UVECTOR(package)[PACKAGE_COUNT]) + 1);
	UVECTOR(value)[SYMBOL_PACKAGE] = package;

	return value;
}			

void
growPackage(LispObj package)
{
	Error("Package capacity exceeded: ~A", package);		// need to implement
}

LispObj 
symbolNode(LispObj name)
{
	LispObj sym = 0;
	LispObj t1 = 0;
	LispObj t2 = 0;

	sym = AllocVector(SYMBOL_SIZE);
	t1 = cons(UNINITIALIZED, NIL);
	t2 = cons(UNINITIALIZED, NIL);
	UVECTOR(sym)[0]						|= (SymbolType << 3);
	UVECTOR(sym)[SYMBOL_NAME]			= name;
	UVECTOR(sym)[SYMBOL_VALUE]			= t1;
	UVECTOR(sym)[SYMBOL_PACKAGE]		= NIL;
	UVECTOR(sym)[SYMBOL_PROPERTY_LIST]	= NIL;
	UVECTOR(sym)[SYMBOL_CONSTANT]		= 0;
	UVECTOR(sym)[SYMBOL_FUNCTION]		= t2;
	UVECTOR(sym)[SYMBOL_FUNCTION_TYPE]	= NIL;
	UVECTOR(sym)[SYMBOL_JUMP_TABLE]		= 0;
	UVECTOR(sym)[SYMBOL_VAR_TABLE]		= 0;

	return sym;
}

LispObj 
foreignNode()
{
	LispObj fp = 0;
	fp = AllocVector(FOREIGN_SIZE);
	UVECTOR(fp)[0]						|= (ForeignType << 3);
	UVECTOR(fp)[FOREIGN_PTR]			= 0;
	return fp;
}

LispObj 
weakPointer()
{
	LispObj wp = 0;
	wp = AllocVector(WEAK_PTR_SIZE);
	UVECTOR(wp)[0]						|= (WeakPointerType << 3);
	UVECTOR(wp)[WEAK_PTR]				= 0;
	return wp;
}

//
// This may be called from non-lisp contexts, so we need to set up ESI
//
__declspec(naked) LispObj cons(LispObj a, LispObj b)
{
	__asm
	{
		push	ebp
		mov		ebp, esp
		push	esi
		call	ThreadQV
		mov		esi, eax
		call	AllocLocalCons
		mov		ecx, dword ptr a
		mov		dword ptr [eax - 4], ecx	;; CAR(ret) = a
		mov		ecx, dword ptr b
		mov		dword ptr [eax], ecx		;; CDR(ret) = b
		pop		esi
		pop		ebp
		ret
	}
}

LispObj stringNode(const char* str)
{
	GCCriticalSection.Enter();
	long len = strlen(str);
	LispObj a = 0;
	long i = 0;

	a = AllocVector(((len + 1) >> 1) + 1);
	UVECTOR(a)[0]					|= (SimpleCharVectorType << 3);
	UVECTOR(a)[ARRAY_SIMPLE_VECTOR_LENGTH] = wrapInteger(len);
 
	registerUntaggedValues(a, wrapInteger(ARRAY_SIMPLE_VECTOR_START));
	for (i = 0; i < len; i++)
		charArrayStart(a)[i] = (LISP_CHAR)(unsigned char)str[i];
	GCCriticalSection.Leave();
	return a;
}

LispObj wstringNode(const wchar_t* str)
{
	GCCriticalSection.Enter();
	long len = wcslen(str);
	LispObj a = 0;
	long i = 0;

	a = AllocVector(((len + 1) >> 1) + 1);
	UVECTOR(a)[0]					|= (SimpleCharVectorType << 3);
	UVECTOR(a)[ARRAY_SIMPLE_VECTOR_LENGTH] = wrapInteger(len);
 
	registerUntaggedValues(a, wrapInteger(ARRAY_SIMPLE_VECTOR_START));
	for (i = 0; i < len; i++)
		charArrayStart(a)[i] = str[i];
	GCCriticalSection.Leave();
	return a;
}

LispObj byteVector(LispObj length)
{
	LispObj a = 0;
	GCCriticalSection.Enter();

	a = AllocVector(((integer(length) + 3) >> 2) + 1);
	UVECTOR(a)[0]					|= (SimpleByteVectorType << 3);
	UVECTOR(a)[ARRAY_SIMPLE_VECTOR_LENGTH] = length;

	registerUntaggedValues(a, wrapInteger(ARRAY_SIMPLE_VECTOR_START));

	GCCriticalSection.Leave();
	return a;
}

LispObj charVector(LispObj length)
{
	LispObj a = 0;
	GCCriticalSection.Enter();

	a = AllocVector(((integer(length) + 1) >> 1) + 1);
	UVECTOR(a)[0]					|= (SimpleCharVectorType << 3);
	UVECTOR(a)[ARRAY_SIMPLE_VECTOR_LENGTH] = length;
	registerUntaggedValues(a, wrapInteger(ARRAY_SIMPLE_VECTOR_START));
	GCCriticalSection.Leave();
	return a;
}

void Error(const char* msg)
{
	LispCall2(Funcall, LISPERROR, stringNode(msg));
}

void Error(const char* msg, LispObj a1)
{
	LispCall3(Funcall, LISPERROR, stringNode(msg), a1);
}

void Error(const char* msg, LispObj a1, LispObj a2)
{
	LispCall4(Funcall, LISPERROR, stringNode(msg), a1, a2);
}

void Error(const char* msg, LispObj a1, LispObj a2, LispObj a3)
{
	LispCall5(Funcall, LISPERROR, stringNode(msg), a1, a2, a3);
}

LispObj 
list(LispObj args, ...)
{
	va_list va__; 
	va_start(va__, args);
	LispObj list = NIL;
	LispObj arg = args;
	while (arg != END_LIST)
	{
		list = cons(arg, list);
		arg = va_arg(va__, LispObj);
	}
	return Cnreverse(list);
}

LispObj 
Cnreverse(LispObj list)
{
	if (!isCons(list))
		return list;
	if (!isCons(CDR(list)))
		return list;

	LispObj p = CDR(list);
	LispObj q = list;
	CDR(q) = NIL;
	while (isCons(p))
	{
		LispObj temp = CDR(p);
		CDR(p) = q;
		q = p;
		p = temp;
	}
	return q;
}

long 
listLength(LispObj n)
{
	long length = 0;
	while (isCons(n))
	{
		length++;
		n = CDR(n);
	}
	return length;
}

long 
sequenceLength(LispObj n)
{
	if (n == NIL)
		return 0;
	if (isList(n))
		return listLength(n);
	if (isVector(n))
		return integer(vectorLength(n));
	Error("Not a sequence: ~A", n);
	return 0;	// never gets here
}

LispObj 
charNode(long c)
{
	return wrap(c << 8, ImmediateTag);
}

LispObj 
integerNode(long n)
{
	return wrapInteger(n);
}

LispObj 
doubleFloatNode(double d)
{
	LispObj f = 0;
	f = AllocVector(DOUBLE_FLOAT_SIZE);
	setUvectorType(f, DoubleFloatType);
	registerUntaggedValues(f, wrapInteger(DOUBLE_FLOAT_OFFSET));
	*(double*)&UVECTOR(f)[DOUBLE_FLOAT_OFFSET] = d;
	return f;
}

LispObj 
singleFloatNode(double d)
{
	LispObj f = 0;
	f = AllocVector(SINGLE_FLOAT_SIZE);
	setUvectorType(f, SingleFloatType);
	*(float*)&UVECTOR(f)[SINGLE_FLOAT_OFFSET] = (float)d;
	return f;
}

LispObj 
ratioNode(LispObj num, LispObj denom)
{
	LispObj f = 0;
	f = AllocVector(RATIO_SIZE);
	setUvectorType(f, RatioType);
	UVECTOR(f)[RATIO_NUMERATOR] = num;
	UVECTOR(f)[RATIO_DENOMINATOR] = denom;
	return f;
}

LispObj 
complexNode(LispObj real, LispObj imag)
{
	LispObj f = 0;
	f = AllocVector(COMPLEX_SIZE);
	setUvectorType(f, ComplexType);
	UVECTOR(f)[COMPLEX_REAL] = real;
	UVECTOR(f)[COMPLEX_IMAGINARY] = imag;
	return f;
}

LispObj bignumNode(LispObj length)
{
	LispObj f = 0;
	long i = 0;
	long len = integer(length);
	f = AllocVector(BIGNUM_HEADER_SIZE + len);
	setUvectorType(f, BignumType);
	registerUntaggedValues(f, wrapInteger(BIGNUM_FIRST_CELL));
	UVECTOR(f)[BIGNUM_LENGTH] = (length * 2);
	for (i = 0; i < len; i++)
	{
		UVECTOR(f)[BIGNUM_FIRST_CELL + i] = 0;	   // init all cells to zero
	}
	return f;
}

LispObj createBignum(long* p)
{
	LispObj f = 0;
	long i = 0;
	long len = integer(*(unsigned long*)p) >> 1;
	f = AllocVector(BIGNUM_HEADER_SIZE + len);
	setUvectorType(f, BignumType);
	registerUntaggedValues(f, wrapInteger(BIGNUM_FIRST_CELL));
	UVECTOR(f)[BIGNUM_LENGTH] = *p;
	for (i = 0; i < len; i++)
	{
		UVECTOR(f)[BIGNUM_FIRST_CELL + i] = *(p + i + 1);
	}
	return f;
}

LispObj
vectorNode(LispObj size, LispObj initialValues)
{
	LispObj v = 0;
	LispObj* p = 0;
	long count = 0;

	v = AllocVector(integer(size) + 1);
	setUvectorType(v, SimpleVectorType);
	UVECTOR(v)[ARRAY_SIMPLE_VECTOR_LENGTH] = size;

	p = arrayStart(v);
	count = integer(size);
	while (isCons(initialValues))
	{
		*p = CAR(initialValues);
		initialValues = CDR(initialValues);
		p++;
		count--;
	}
	while (count > 0)
	{
		*p = NIL;
		p++;
		count--;
	}
	return v;
}

LispObj
vectorNode(LispObj size)
{
	LispObj v = 0;

	v = AllocVector(integer(size) + 1);
	setUvectorType(v, SimpleVectorType);
	UVECTOR(v)[ARRAY_SIMPLE_VECTOR_LENGTH]	= size;

	return v;
}

LispObj 
packageNode(LispObj name)
{
	LispObj p = 0;
	LispObj table = 0;

	p = AllocVector(PACKAGE_SIZE);
	UVECTOR(p)[0] |= (PackageType << 3);
	table = vectorNode(wrapInteger(1009 * 3));
	UVECTOR(p)[PACKAGE_NAME] = name;
	UVECTOR(p)[PACKAGE_NICKNAMES] = NIL;
	UVECTOR(p)[PACKAGE_USE_LIST] = NIL;
	UVECTOR(p)[PACKAGE_USED_BY_LIST] = NIL;
	UVECTOR(p)[PACKAGE_SHADOWING_SYMBOLS] = NIL;
	UVECTOR(p)[PACKAGE_CAPACITY] = wrapInteger(1009);
	UVECTOR(p)[PACKAGE_COUNT] = 0;
	UVECTOR(p)[PACKAGE_TABLE] = table;
	UVECTOR(p)[PACKAGE_SYNC] = NIL;
	
	return p;
}

void 
checkNumArgs(long numArgsPassed, long numRequired)
{
	if (numArgsPassed != numRequired)
		Error("Wrong number of arguments: number passed = ~A, number required = ~A", 
			wrapInteger(numArgsPassed), wrapInteger(numRequired));
}

void 
checkNumArgsRange(long numArgsPassed, long min, long max)
{
	if (numArgsPassed < min || numArgsPassed > max)
		Error("Wrong number of arguments: number passed = ~A, number required >= ~A and <= ~A", 
			wrapInteger(numArgsPassed), wrapInteger(min), wrapInteger(max));
}

void
checkChar(LispObj n)
{
	if (!isCharacter(n))
		Error("Not a character: ~A", n);
}

void
checkList(LispObj n)
{
	if (!isList(n))
		Error("Not a list: ~A", n);
}

void
checkSequence(LispObj n)
{
	if (!isSequence(n))
		Error("Not a sequence: ~A", n);
}

void
checkCons(LispObj n)
{
	if (!isCons(n))
		Error("Not a cons cell: ~A", n);
}

void
checkSymbol(LispObj n)
{
	if (!isSymbol(n))
		Error("Not a symbol: ~A", n);
}

void
checkFunction(LispObj n)
{
	if (!isFunction(n))
		Error("Not a function: ~A", n);
}

void
checkStream(LispObj n)
{
	if (!isStream(n))
		Error("Not a stream: ~A", n);
}

void
checkArray(LispObj n)
{
	if (!isArray(n))
		Error("Not an array: ~A", n);
}

void
checkSimpleVector(LispObj n)
{
	if (!(isUvector(n) && isSimpleGenericVector(n)))
		Error("Not a simple vector: ~A", n);
}

void
checkOutputStream(LispObj n)
{
	LispObj stype = 0;
	if (!isStream(n))
		Error("Not a stream: ~A", n);
	stype = streamDirection(n);
	if (stype != OUTPUT_KEY && stype != BIDIRECTIONAL_KEY)
		Error("Not an output stream: ~A", n);
}

void
checkInputStream(LispObj n)
{
	LispObj stype = 0;
	if (!isStream(n))
		Error("Not a stream: ~A", n);
	stype = streamDirection(n);
	if (stype != INPUT_KEY && stype != BIDIRECTIONAL_KEY)
		Error("Not an input stream: ~A", n);
}

void
checkPackage(LispObj n)
{
	if (!isPackage(n))
		Error("Not a package: ~A", n);
}

void
checkString(LispObj n)
{
	if (!isString(n))
		Error("Not a string: ~A", n);
}

void
checkUvector(LispObj n)
{
	if (!isUvector(n))
		Error("Not a uvector: ~A", n);
}

void
checkInteger(LispObj n)
{
	if (!isFixnum(n))
		Error("Not a fixnum: ~A", n);
}

void
checkBit(LispObj n)
{
	if (!isBit(n))
		Error("Not a bit: ~A", n);
}

void
checkLispInteger(LispObj n)
{
	if (!isLispInteger(n))
		Error("Not an integer: ~A", n);
}

void
checkFloat(LispObj n)
{
	if (!isFloat(n))
		Error("Not a floating point number: ~A", n);
}

void
checkReal(LispObj n)
{
	if (!isFixnum(n) && !isFloat(n) && !isRatio(n) && !isBignum(n))
		Error("Not a real number: ~A", n);
}

void
checkNumber(LispObj n)
{
	if (!isFixnum(n) && !isFloat(n) && !isRatio(n) && !isBignum(n) && !isComplex(n))
		Error("Not a number: ~A", n);
}

void
checkCharacter(LispObj n)
{
	if (!isCharacter(n))
		Error("Not a character: ~A", n);
}

LispObj eval(LispObj s, LispObj env)
{
	LispObj func = 0;
	LispObj val = 0;

	func = compileExpression(s, NIL, NIL);
	functionEnvironment(func) = env;
	val = LispCall1(Funcall, func);
	return val;
}

LispObj 
compiledFunctionNode(LispObj code, LispObj length, LispObj refs, 
					 LispObj numRefs, LispObj env, LispObj info, 
					 LispObj appendRefsToCode)
{
	long len = integer(length);
	long nRefs = integer(numRefs);
	LispObj rbuf = 0;
	LispObj cbuf = 0;
	LispObj func = 0;
	long i = 0;
	long pos = 0;
	long offset = 0;
	long diff = 0;
	long refOffset = 0;

	GCCriticalSection.Enter();

	if (appendRefsToCode == NIL || nRefs == 0)
	{
		// create references vector
		if (nRefs > 0)
			rbuf = vectorNode(wrapInteger(nRefs * 2));

		cbuf = AllocVector(((len + 3) / 4) + COMPILED_CODE_HEADER_SIZE);
		setUvectorType(cbuf, CompiledCodeType);
		registerUntaggedValues(cbuf, wrapInteger(COMPILED_CODE_OFFSET));
		UVECTOR(cbuf)[COMPILED_CODE_MAGIC] = COMPILED_CODE_MAGIC_ID;

		func = AllocVector(FUNCTION_SIZE);
		setUvectorType(func, FunctionType);

		memcpy((byte*)(UVECTOR(cbuf) + COMPILED_CODE_OFFSET), byteArrayStart(code), len);
		if (nRefs > 0)
			memcpy(arrayStart(rbuf), arrayStart(refs), nRefs * 8);

		UVECTOR(func)[FUNCTION_ENVIRONMENT] = env;
		UVECTOR(func)[FUNCTION_ADDRESS] = cbuf;
		UVECTOR(cbuf)[COMPILED_CODE_REFERENCES] = rbuf;
		UVECTOR(cbuf)[COMPILED_CODE_PROPERTIES] = info;
	}
	else
	{
		// append the references to the and of the code
		cbuf = AllocVector(((len + 3) / 4) + COMPILED_CODE_HEADER_SIZE + ((nRefs + 2) / 2));
		setUvectorType(cbuf, CompiledCodeType);
		registerUntaggedValues(cbuf, wrapInteger(COMPILED_CODE_OFFSET));
		UVECTOR(cbuf)[COMPILED_CODE_MAGIC] = COMPILED_CODE_MAGIC_ID;

		func = AllocVector(FUNCTION_SIZE);
		setUvectorType(func, FunctionType);

		memcpy((byte*)(UVECTOR(cbuf) + COMPILED_CODE_OFFSET), byteArrayStart(code), len);
		offset = 0;
		pos = ((len + 3) / 4) * 4;
		UVECTOR(cbuf)[COMPILED_CODE_REFERENCES] = wrapInteger(pos);
		*(unsigned short*)(((byte*)(UVECTOR(cbuf) + COMPILED_CODE_OFFSET)) + pos)
			= (unsigned short)nRefs;
		pos += 2;
		for (i = 0; i < nRefs; i++)
		{
			refOffset = integer(arrayStart(refs)[i * 2 + 1]);
			diff = refOffset - offset;
			if (diff > 0x10000)
			{
				GCCriticalSection.Leave();
				Error("Function is too large--references are more than 64k apart");
			}
			*(unsigned short*)(((byte*)(UVECTOR(cbuf) + COMPILED_CODE_OFFSET)) + pos)
				= (unsigned short)diff;
			offset = refOffset;
			pos += 2;
		}

		UVECTOR(func)[FUNCTION_ENVIRONMENT] = env;
		UVECTOR(func)[FUNCTION_ADDRESS] = cbuf;
		UVECTOR(cbuf)[COMPILED_CODE_PROPERTIES] = info;

	}

	// update the code references in case a gc occurred
	for (i = 0; i < nRefs; i++)
	{
		*(LispObj*)(((byte*)(UVECTOR(cbuf) + COMPILED_CODE_OFFSET))
				+ integer(arrayStart(refs)[i * 2 + 1]))
			= arrayStart(refs)[i * 2];
	}
	GCCriticalSection.Leave();
	return func;
}

LispObj 
kernelFunctionNode(LispFunc f)
{
	LispObj func = 0;

	func = AllocVector(KFUNCTION_SIZE);
	setUvectorType(func, KFunctionType);
	registerUntaggedValues(func, wrapInteger(FUNCTION_ADDRESS));
	UVECTOR(func)[FUNCTION_ENVIRONMENT] = NIL;
	UVECTOR(func)[FUNCTION_ADDRESS] = (LispObj)f;
	return func;
}

//
//	lastCons()
//	Given a list of length 1 or more, returns the last cons
//	cell of the list.
//
LispObj 
lastCons(LispObj x)
{
	while (isCons(CDR(x)))
		x = CDR(x);
	return x;
}

//
//	Creverse()
//	Reverses a passed list.
//
LispObj
Creverse(LispObj x)
{
	LispObj n = NIL;
	while (isCons(x))
	{
		n = cons(CAR(x), n);
		x = CDR(x);
	}
	return n;
}

// like MEMBER, but simpler and uses EQ test.
LispObj
Cmember(LispObj item, LispObj list)
{
	while (isCons(list))
	{
		if (item == CAR(list))
			return list;
		list = CDR(list);
	}
	return NIL;
}

//
//	updateJumpTable()
//	This is a critical section, and must not cause garbage collection.
//
void updateJumpTable(LispObj sym, LispObj func, LispObj env)
{
	ThreadRecord* tr = 0;
	HANDLE process = 0;
	LispObj tableIndex = 0;
	LispObj funcaddr = 0;

	GCCriticalSection.Enter();
	TQCriticalSection.Enter();
	tr = ThreadList.getList();

	createFuncTableEntry(sym);

	// set up jump table entry
	tableIndex = UVECTOR(sym)[SYMBOL_JUMP_TABLE];

	funcaddr = UVECTOR(func)[FUNCTION_ADDRESS]; 
	if (uvectorType(func) == FunctionType)
		funcaddr = (LispObj)(UVECTOR(funcaddr) + COMPILED_CODE_OFFSET);	// start after header cells
	// funcaddr is untagged address

	QV[integer(tableIndex)] = env;
	QV[integer(tableIndex) + 1] = funcaddr;

	while (tr)
	{
		tr->QV_rec[integer(tableIndex)] = env;
		tr->QV_rec[integer(tableIndex) + 1] = funcaddr;
		tr = tr->next;
	}

	process = GetCurrentProcess(); 
	FlushInstructionCache(process, NULL, 0);
	TQCriticalSection.Leave();
	GCCriticalSection.Leave();
}

void setSymbolFunction(LispObj sym, LispObj func, LispObj type)
{
	LispObj env = 0;

	checkFunction(func);
	CAR(UVECTOR(sym)[SYMBOL_FUNCTION]) = func;
	UVECTOR(sym)[SYMBOL_FUNCTION_TYPE] = type;
	env = UVECTOR(func)[FUNCTION_ENVIRONMENT];
	updateJumpTable(sym, func, env);
}

// called after a Load-image() to update the kernel function addresses
void updateSymbolFunctionAddress(LispObj s, LispFunc funcaddr)
{
	long jumpTableIndex = 0;
	HANDLE process = 0;
	TQCriticalSection.Enter();
	ThreadRecord* tr = ThreadList.getList();
	LispObj f = 0;

	f = symbolFunction(s);
	if (UVECTOR(f)[FUNCTION_ADDRESS] != (LispObj)funcaddr)
		UVECTOR(f)[FUNCTION_ADDRESS] = (LispObj)funcaddr;

	// set up jump table entry
	jumpTableIndex = integer(UVECTOR(s)[SYMBOL_JUMP_TABLE]);

	if (QV[jumpTableIndex + 1] != (LispObj)funcaddr)
	{
		QV[jumpTableIndex + 1] = (LispObj)funcaddr;

		while (tr)
		{
			tr->QV_rec[jumpTableIndex + 1] = (LispObj)funcaddr;
			tr = tr->next;
		}
	}

	process = GetCurrentProcess(); 
	FlushInstructionCache(process, NULL, 0);
	TQCriticalSection.Leave();
}

void
createFuncTableEntry(LispObj s)
{
	if (UVECTOR(s)[SYMBOL_JUMP_TABLE] == 0)
	{
		UVECTOR(s)[SYMBOL_JUMP_TABLE] = SYMBOL_TABLE_COUNT;
		SYMBOL_TABLE_COUNT += wrapInteger(JumpTableCellsPerEntry);
	}
}

void
createSymbolTableEntry(LispObj sym)
{
	LispObj varIndex = SYMBOL_TABLE_VAR_COUNT;
	LispObj symval = UVECTOR(sym)[SYMBOL_VALUE];
	TQCriticalSection.Enter();
	ThreadRecord* tr = ThreadList.getList();
	SYMBOL_TABLE_VAR_COUNT += wrapInteger(1);
	setSymbolVarTableIndex(sym, varIndex);
	
 	QV[integer(varIndex)] = symval;
	while (tr)
	{
		tr->QV_rec[integer(varIndex)] = symval;
		tr = tr->next;
	}
	TQCriticalSection.Leave();
}

void
pushCatcher(LispObj tag, unsigned long* regs)
{
	setSymbolValue(COMPILER_RUNTIME, 
		cons(list(CATCH_HEADER_CODE, tag, (LispObj)regs, END_LIST), symbolValue(COMPILER_RUNTIME)));
}

void 
popCatcher()
{
	setSymbolValue(COMPILER_RUNTIME, CDR(symbolValue(COMPILER_RUNTIME)));
}

void
establishSpecialBindings(LispObj bindings)
{
	setSymbolValue(COMPILER_RUNTIME, 
		cons(cons(SPECIAL, bindings), symbolValue(COMPILER_RUNTIME)));
}

void 
popSpecials()
{
	LispObj temp = 0;
	temp = symbolValue(COMPILER_RUNTIME);
	assert(isCons(temp));
	setSymbolValue(COMPILER_RUNTIME, CDR(temp));
}

#pragma warning (disable:4731) // frame pointer register 'ebp' 
							   // modified by inline assembly code (we know!)
LispFunction(Throw_Exception)
{
	LISP_FUNC_BEGIN(3);
	LispObj tag = LISP_ARG(0);
	LispObj form = LISP_ARG(1);
	LispObj numValues = LISP_ARG(2);

	ThreadQV()[THROW_TAG_Index] = tag;
	ThreadQV()[THROW_RETURN_VALUE_Index] = form;
	ThreadQV()[THROW_NUM_RETURN_VALUES_Index] = integer(numValues);
	LispObj runtimeInfo = symbolValue(COMPILER_RUNTIME);
	LispObj runtimeInfo2 = runtimeInfo;
	LispObj catcher = NIL;
	unsigned long* regs = 0;
	unsigned long* qv = 0;
	LispObj f = 0;
	LispObj bindings = 0;
	HWND wnd = 0;
	unsigned long index = 0;

	// look for a catcher
	while (isCons(runtimeInfo))
	{
		f = CAR(runtimeInfo);
		if (CAR(f) == CATCH_HEADER_CODE && CAR(CDR(f)) == tag)
		{
			catcher = f;
			break;
		}
		runtimeInfo = CDR(runtimeInfo);
	}
	if (catcher == NIL)
	{
		if (tag == ERROR_SIGNAL)
		{
			// we can't throw an error here--that's how we got here!
			if (CormanLispServer)
				CormanLispServer->GetAppMainWindow(&wnd);

			MessageBox(wnd, "Sorry, no global error handler was found--Corman Lisp is quitting.",
				"Fatal Error", 
				MB_OK|MB_SETFOREGROUND);
			if (CormanLispServer)
				CormanLispServer->SetMessage("Corman Lisp has stopped.");
			ExitThread((unsigned long)-1);
		}
		else
			Error("No catch block was found to match the throw tag: ~A", tag);
	}

	// now peel back the stack to that point, executing cleanups
	while (runtimeInfo2 != runtimeInfo)
	{
		f = CAR(runtimeInfo2);
		if (CAR(f) == CATCH_HEADER_CODE && CAR(CDR(f)) == UNWIND_MARKER)
		{
			catcher = f;		// if an unwind-protect form is found, transfer control to it
			break;
		}
		else
		if (CAR(f) == SPECIAL)
		{
			bindings = CDR(f);
			while (isCons(bindings))
			{
				popDynamicBinding(CAR(bindings));
				bindings = CDR(bindings);
			}
		}
		runtimeInfo2 = CDR(runtimeInfo2);
	}

	setSymbolValue(COMPILER_RUNTIME, runtimeInfo2);			// synch runtime stack
	regs = (unsigned long*)CAR(CDR(CDR(catcher)));

	// peel off any context change entries on the stack which are no longer
	// going to be valid once the stack pointer is adjusted
	qv = ThreadQV();
	index = qv[STACK_MARKER_INDEX_Index] >> 2;
	if (index > 0)
	{
//		if (index & 1)
//		{
//			// error--this shouldn't happen, because we always should be jumping
//			// into lisp code.
//			Error("Throw initial context was in a foreign stack context");
//		}
		while ((index > 0) &&
			qv[((index - 1) * 2) + STACK_MARKERS_Index] < regs[5])
		{
			qv[((index - 1) * 2) + STACK_MARKERS_Index] = 0;
			index--;
		}
		if (index & 1)
		{
			// error--this shouldn't happen, because we always should be jumping
			// into lisp code.
			Error("Throw target was in a foreign stack context");
		}
		qv[STACK_MARKER_INDEX_Index] = index << 2;
	}

	__asm	mov		ebx, dword ptr regs
//	__asm	mov		eax, dword ptr [ebx + 24]	; eax = ip * 8
//	__asm	shr		eax, 3					; eax = ip
//	__asm	mov		dword ptr [ebx + 24], eax				
	__asm	mov		eax, form
	__asm	mov		ecx, numValues
	__asm	shr		ecx, 3					; untag integer
//	__asm	mov		ecx, dword ptr [ebx + 4]
	__asm	mov		edx, dword ptr [ebx + 8]
	__asm	mov		esi, dword ptr [ebx + 12]
	__asm	mov		edi, dword ptr [ebx + 16]
	__asm	mov		esp, dword ptr [ebx + 20]
	__asm	mov		ebp, dword ptr [ebx + 28]
	__asm	push	dword ptr [ebx + 24]		;; push ip
	__asm	mov		ebx, dword ptr [ebx + 0]
	__asm	ret
	// never returns normally
	LISP_FUNC_RETURN(ret);
}

//
//	nullTerminate()
//	Expects a Lisp string, returns a new byte array with a 0 byte appended
//	on the end.
//
LispObj 
nullTerminate(LispObj str)
{
	long len = 0;
	long i = 0;
	LispObj newstr = 0;

	LispObj length = vectorLength(str);
	newstr = byteVector(length + wrapInteger(1));
	len = integer(length);
	for (i = 0; i < len; i++)
		byteArrayStart(newstr)[i] = (byte)charArrayStart(str)[i];
	byteArrayStart(newstr)[i] = 0;
	return newstr;
}											  

long 
lispStringsEqual(LispObj str1, LispObj str2)
{
	LispObj len1 = vectorLength(str1);
	LispObj len2 = vectorLength(str2);
	long i = 0;
	long len = integer(len1);

	if (len1 != len2)
		return 0;
	for (i = 0; i < len; i++)
		if (charArrayStart(str1)[i] != charArrayStart(str2)[i])
			return 0;
	return 1;
}

unsigned long 
lispIntegerToUnsignedLong(LispObj x)
{
	if (isBignum(x))
		return (unsigned long)UVECTOR(x)[BIGNUM_FIRST_CELL];
	else
		return (unsigned long)integer(x);
}

long 
lispIntegerToLong(LispObj x)
{
	if (isBignum(x))
		return (long)UVECTOR(x)[BIGNUM_FIRST_CELL];
	else
		return (long)integer(x);
}

LispObj symbolValue(LispObj sym)
{
	if (UVECTOR(sym)[SYMBOL_VAR_TABLE])
		return CAR(ThreadQV()[integer(UVECTOR(sym)[SYMBOL_VAR_TABLE])]);
	else
		return CAR(UVECTOR(sym)[SYMBOL_VALUE]);
}

void setSymbolValue(LispObj sym, LispObj value)	
{
	if (UVECTOR(sym)[SYMBOL_VAR_TABLE])
		CAR(ThreadQV()[integer(UVECTOR(sym)[SYMBOL_VAR_TABLE])]) = value;
	else
		CAR(UVECTOR(sym)[SYMBOL_VALUE]) = value;
}

LispFunc functionAddress(LispObj func)
{
	LispFunc f = 0;
	int badAddress = 0;
	if (UVECTOR(func)[FUNCTION_ADDRESS] == UNINITIALIZED)
	{
		badAddress = 1;
		return f;
	}
	if (uvectorType(func) == FunctionType)
		f = (LispFunc)(UVECTOR(UVECTOR(func)[FUNCTION_ADDRESS]) + COMPILED_CODE_OFFSET);
	else
		f = (LispFunc)(UVECTOR(func)[FUNCTION_ADDRESS]);
	return f;
}

LispObj setSymbolMacro(LispObj sym, LispObj func)
{
	CAR(UVECTOR(sym)[SYMBOL_FUNCTION]) = func;
	UVECTOR(sym)[SYMBOL_FUNCTION_TYPE] = MACRO;
	return 0;
}

LispObj setSpecialOperator(LispObj sym)
{
	CAR(UVECTOR(sym)[SYMBOL_FUNCTION]) = NIL;
	UVECTOR(sym)[SYMBOL_FUNCTION_TYPE] = SPECIAL_OPERATOR;
	return 0;
}

LispObj pushDynamicBinding(LispObj sym, LispObj val)
{
	ThreadQV()[integer(symbolVarTableIndex(sym))] = 
		cons(val, ThreadQV()[integer(symbolVarTableIndex(sym))]);
	return 0;
}

LispObj popDynamicBinding(LispObj sym)
{
	LispObj currBinding = 0;
	currBinding = ThreadQV()[integer(symbolVarTableIndex(sym))];
	if (!isCons(currBinding) || !isCons(CDR(currBinding)))
	{
		OutputDebugString("Error: trying to pop a dynamic binding that doesn't exist!!");
		//DebugBreak();
		return NIL;
	}
	else
	{
		ThreadQV()[integer(symbolVarTableIndex(sym))] = CDR(currBinding);
		return CAR(currBinding);
	}
}

LispObj createShortFloat_foo(double d)
{
	long low3 = 0;
	float x = (float)d;
	LispObj mantissa = ((*((LispObj*)&x)) << 3) & 0x3fffff8;	// get 23-bit mantissa as wrapped integer
	if (mantissa < 0x3fffff0)				// avoid overflow when rounding
	{
		// determine rounding direction (use "bankers rounding" i.e. like Common Lisp round function)
		low3 = (mantissa & 0x38) >> 3;		// get low three bits of wrapped mantissa
		if (low3 == 3 || low3 > 5)
			return (*((LispObj*)&x) + 2) | 3;	// round up to nearest 21 bit number
		else
			return *((LispObj*)&x) | 3;	// round down nearest 21 bit number
	}
	else
		return *((LispObj*)&x) | 3;	// round down in this case
}

LispObj __declspec(naked) createShortFloat(double /*d*/)
{
	__asm
	{
		push        ebp
		mov         ebp,esp
		std									;; begin-atomic
		sub			esp, 4
		fld         qword ptr [ebp + 8]
		fstp        dword ptr [ebp - 4]		;; float x = (float)d
		mov         edx,dword ptr [ebp - 4]
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
		cld									;; end-atomic
		mov         esp,ebp
		pop         ebp
		ret
	}
}


double shortFloat_foo(LispObj f)
{
	f &= ~3;
	return *((float*)&f);
}

double __declspec(naked) shortFloat(LispObj /*d*/)
{
	__asm
	{
		push        ebp
		mov         ebp,esp
		and         byte ptr [ebp + 8], 0FCh
		fld         dword ptr [ebp + 8]
		pop         ebp
		ret
	}
}

// Use these instead of malloc()/free()
void* CAlloc(unsigned long size)
{
	HANDLE heap = 0;
	heap = GetProcessHeap();
	return HeapAlloc(heap, 0, size);
}

void CFree(void* p)
{
	HANDLE heap = 0;
	heap = GetProcessHeap();
	HeapFree(heap, 0, p);
}

// Use instead of realloc()
void* CRealloc(void* p, unsigned long size)
{
	HANDLE heap = 0;
	heap = GetProcessHeap();
	return HeapReAlloc(heap, HEAP_ZERO_MEMORY, p, size);
}
