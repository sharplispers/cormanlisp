//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		lisp.h
//		Contents:	Core header file for Corman Lisp.
//		History:	6/5/96  RGC Created.
//					1/12/00 RGC Added EXECUTE_FINALIZERS, FINALIZATIONS_PENDING
//								to support post-GC finalization.
//					1/18/00 RGC Added UNDEFINED-FUNCTIONS.
//                  9/21/03 RGC Increased NumJumpTableEntries from 0x4000 to 0x8000.
//					10/05/16  Artem Boldarev
//							  %USER-HOMEDIR-NAMESTRING
//					11/22/16  Artem Boldarev
//							  getImageLoadsCount() function.
//

#ifndef LISP_H
#define LISP_H

#include "generic.h"

typedef unsigned long LispObj;

#include "LispThreadQueue.h"

#define Thread  __declspec( thread )

typedef unsigned short LISP_CHAR;

struct Node;
struct Node 
{ 
	LispObj car;	// if more than 2 LispObjs, this contains the length
	LispObj cdr;

	// could contain more LispObjs, even number...
};

const unsigned long MaxLispArgs = 10000;

extern unsigned long NumReturnValues;
extern LispObj* ThreadQV();

const char ASCII_CR			= (char)0x0d;
const char ASCII_NEWLINE	= (char)0x0a;

//
//	Tags:
//	Integers are 29-bit values with the low 3 bits cleared.
//	If the low bits are 011, it marks the start of a uvector heap block
//	where the upper 29 bits is the number of cells in the block
//

// tag types  (bits 0 - 2)
#define FixnumTag			0		// 29-bit signed integer
#define ImmediateTag		1		// characters, other small (<= 24 bit) values
#define	ForwardTag			2		// used during garbage collection only
#define	ShortFloatTag		3		// 30-bit short float
#define ConsTag				4		// 8 byte heap objects
#define	UvectorTag			5		// arbitrary sized heap object, first cell contains header
#define	UvectorLengthTag	6		// used to mark heap block headers
#define ShortFloatTag2		7		// 30-bit short float

//	Immediate extended tags	(bits 3 - 7) (data in upper 24 bits)
#define CharacterType		0

// uvector extended tags
// These types are stored in bits 3 - 7 of the first cell in a
// uvector i.e. a heap block of arbitrary size
#define FunctionType				0
#define KFunctionType				1
#define StructureType				2
#define ArrayType					3
#define SymbolType					4
#define StreamType					5
#define DoubleFloatType				6
#define PackageType					7
#define HashtableType				8
#define ForeignType					9
#define CompiledCodeType			10
#define ReadTableType				11
#define ComplexType					12
#define RatioType					13
#define BignumType					14
#define ForeignHeapType				15
#define WeakPointerType				16
#define SimpleVectorType			17
#define SimpleCharVectorType		18
#define SimpleByteVectorType		19
#define SimpleShortVectorType		20
#define SimpleDoubleFloatVectorType 21
#define SimpleBitVectorType			22
#define SimpleSingleFloatVectorType 23
#define SingleFloatType				24
#define CLOSInstanceType			25
#define ForeignStackType			26
#define ForeignStackEndType			27

#define UVECTOR(n) ((LispObj*)(n - UvectorTag))


//	Lisp Function (closure)	
//|-------------------------------------------------|
//|	0	uvector length = 2		| FunctionType		|
//|-------------------------------------------------|
//|	1	environment									|
//|-------------------------------------------------|
//|	2	pointer to code	vector						|
//|-------------------------------------------------|
//|	3	unused										|
//|-------------------------------------------------|

//	Lisp Kernel Function (closure)	
//|-------------------------------------------------|
//|	0	uvector length = 2		| KFunctionType		|
//|-------------------------------------------------|
//|	1	environment									|
//|-------------------------------------------------|
//|	2	pointer to C function						|
//|-------------------------------------------------|
//|	3	unused										|
//|-------------------------------------------------|

#define FUNCTION_ENVIRONMENT	1
#define FUNCTION_ADDRESS		2

#define FUNCTION_SIZE			3
#define KFUNCTION_SIZE			3

//	Structure	
//|-------------------------------------------------|
//|	0	uvector length			| StructureType		|
//|-------------------------------------------------|
//|	1	structure definition vector					|
//|-------------------------------------------------|
//|	2	slot 1										|
//|-------------------------------------------------|
//|	3	slot 2										|
//|-------------------------------------------------|
//|	4	...											|
//|-------------------------------------------------|

#define STRUCT_HEADER_SIZE		1
#define STRUCT_TEMPLATE			1

//	Array (adjustable)
//------------------------------------------|
//|	0	uvector length		|  ArrayType	|
//------------------------------------------|
//|	1	vector (simple, float, char, etc.)	|
//------------------------------------------|
//|	2	fill pointer						|
//------------------------------------------|
//|	3	offset (displaced arrays)			|
//------------------------------------------|
//|	4	number of dimensions				|
//------------------------------------------|
//|	5	dimension 1	length					|
//------------------------------------------|
//|	6	dimension 2	length					|
//------------------------------------------|
//|	7	dimension 3	length					|
//------------------------------------------|
//|	8	dimension 4	length					|
//------------------------------------------|
//|	9	dimension 5	length					|
//------------------------------------------|
//|	10	dimension 6	length					|
//------------------------------------------|
//|	11	dimension 7	length					|
//------------------------------------------|
//|	12	dimension 8	length					|
//------------------------------------------|
//|	unused									|
//------------------------------------------|
//
#define ARRAY_VECTOR				1
#define ARRAY_FILL_POINTER			2
#define ARRAY_OFFSET				3
#define ARRAY_DIMENSIONS			4
#define ARRAY_DIM1_LENGTH			5
#define ARRAY_HEADER_MAX_SIZE		13
#define ARRAY_HEADER_MIN_SIZE		5
#define ARRAY_SIMPLE_VECTOR_LENGTH	1
#define ARRAY_SIMPLE_VECTOR_START	2

// If the array is non-adjustable, the number of dimensions
// determines how many slots are actually allocated
//


//	Symbol
//------------------------------------------|
//|	0	uvector length = 5	|  SymbolType	|
//------------------------------------------|
//|	1	print value							|
//------------------------------------------|
//|	2	value								|
//------------------------------------------|
//|	3	package								|
//------------------------------------------|
//|	4	property list						|
//------------------------------------------|
//|	5	constant flag						|
//------------------------------------------|
//|	6	function type						|
//------------------------------------------|
//|	7	function							|
//------------------------------------------|
//|	8	jump table entry					|
//------------------------------------------|
//|	9	var table entry						|
//------------------------------------------|

#define SYMBOL_NAME						1
#define SYMBOL_VALUE					2
#define SYMBOL_PACKAGE					3
#define SYMBOL_PROPERTY_LIST			4
#define SYMBOL_CONSTANT					5
#define SYMBOL_FLAGS					5
#define SYMBOL_FUNCTION_TYPE			6	// NIL, FUNCTION, MACRO, SPECIAL-OPERATOR
#define SYMBOL_FUNCTION					7
#define SYMBOL_JUMP_TABLE				8
#define SYMBOL_VAR_TABLE				9
#define SYMBOL_UNBOUND					(LispObj)ConsTag					
#define UNINITIALIZED					(LispObj)ConsTag					
#define END_LIST						(LispObj)ConsTag					
#define SYMBOL_CONSTANT_FLAG			1 << 3
#define SYMBOL_SPECIAL_FLAG				1 << 4
#define UNWIND_MARKER					(LispObj)(ConsTag+8)

#define SYMBOL_SIZE						9
//
//	Stream types are: 
//		CONSOLE-INPUT
//		CONSOLE-OUTPUT
//		FILE-INPUT
//		FILE-OUTPUT
//		STRING-INPUT
//		STRING-OUTPUT
//
//
//	Stream
//------------------------------------------|
//|	0	uvector length = 11	|  StreamType	|
//------------------------------------------|
//|	1	stream name							|
//------------------------------------------|
//|	2	stream underflow function			|
//------------------------------------------|
//|	3	stream position						|
//------------------------------------------|
//|	4	column position						|
//------------------------------------------|
//|	5	input char buffer (char array)		|
//------------------------------------------|
//|	6	input char buffer length			|
//------------------------------------------|
//|	7	input char buffer position			|
//------------------------------------------|
//|	8	input char buffer number of chars	|
//------------------------------------------|
//|	9	file handle or string				|
//------------------------------------------|
//|	10	Common Lisp subclass				|
//------------------------------------------|
//|	11	binary (boolean)					|
//------------------------------------------|
//|	12	open (boolean)						|
//------------------------------------------|
//|	13	direction 							|
//|		(:input, :output, :bidirectional or |
//|		:nil)								|
//------------------------------------------|
//|	14	interactive (boolean)				|
//------------------------------------------|
//|	15	element type						|
//------------------------------------------|
//|	16	associated streams					|
//------------------------------------------|
//|	17	stream overflow function			|
//------------------------------------------|
//|	18	output char buffer (char array)		|
//------------------------------------------|
//|	19	output char buffer length			|
//------------------------------------------|
//|	20	output char buffer position			|
//------------------------------------------|
//|	21	stream line number					|
//------------------------------------------|


#define STREAM_NAME					1
#define STREAM_UNDERFLOW_FUNC		2
#define STREAM_POSITION				3
#define STREAM_COL_POSITION			4
#define STREAM_INPUT_BUFFER			5
#define STREAM_INPUT_BUFFER_LENGTH	6
#define STREAM_INPUT_BUFFER_POS		7
#define STREAM_INPUT_BUFFER_NUM		8
#define STREAM_HANDLE				9
#define STREAM_SUBCLASS				10
#define STREAM_BINARY				11
#define STREAM_OPEN					12
#define STREAM_DIRECTION			13
#define STREAM_INTERACTIVE			14
#define STREAM_ELEMENT_TYPE			15
#define STREAM_ASSOCIATED_STREAMS	16
#define STREAM_OVERFLOW_FUNC		17
#define STREAM_OUTPUT_BUFFER		18
#define STREAM_OUTPUT_BUFFER_LENGTH	19
#define STREAM_OUTPUT_BUFFER_POS	20
#define STREAM_LINE_NUMBER			21

#define STREAM_SIZE					21


//	Double Float number
//------------------------------------------|
//|	0	uvector length = 2| DoubleFloatType	|
//------------------------------------------|
//|	1	unused								|
//------------------------------------------|
//|	2	float (upper 32 bits, unwrapped)	|
//------------------------------------------|
//|	3	float (lower 32 bits, unwrapped)	|
//------------------------------------------|

#define DOUBLE_FLOAT_OFFSET			2
#define DOUBLE_FLOAT_SIZE			3

//	Package
//------------------------------------------|
//|	0	uvector length = 5	|  PackageType	|
//------------------------------------------|
//|	1	package name						|
//------------------------------------------|
//|	2	nicknames							|
//------------------------------------------|
//|	3	use-packages						|
//------------------------------------------|
//|	4	used-by packages					|
//------------------------------------------|
//|	5	shadowing symbols					|
//------------------------------------------|
//|	6	current capacity					|
//------------------------------------------|
//|	7	number of symbols currently stored	|
//------------------------------------------|
//|		vector containing					| 
//|	8	(symbol,object,extern flag)			|
//|		tuples (size = 3 * capacity)		|
//------------------------------------------|
//|	9	synchronization object				|
//------------------------------------------|
#define PACKAGE_NAME				1
#define PACKAGE_NICKNAMES			2
#define PACKAGE_USE_LIST			3
#define PACKAGE_USED_BY_LIST		4
#define PACKAGE_SHADOWING_SYMBOLS	5
#define PACKAGE_CAPACITY			6
#define PACKAGE_COUNT				7
#define PACKAGE_TABLE				8
#define PACKAGE_SYNC				9
#define PACKAGE_RESIZE_THRESHOLD	0.8
#define PACKAGE_ENTRY(pk, n)	(packageTableStart(pk) + (3 * (n))) 
#define GET_PACKAGE_CAPACITY(pk)(integer(UVECTOR(pk)[PACKAGE_CAPACITY])) 

#define PACKAGE_SIZE				9

//	Hashtable
//------------------------------------------|
//|	0	uvector length = 3	|  HashtableType|
//------------------------------------------|
//|	1	current capacity					|
//------------------------------------------|
//|	2	number of objects currently stored	|
//------------------------------------------|
//|	3	test function						|
//------------------------------------------|
//|		vector containing					| 
//|	4	(symbol,object)						|
//|		tuples (size = 2 * capacity)		|
//------------------------------------------|
//|	5	unused								|
//------------------------------------------|

//	Foreign pointer
//------------------------------------------|
//|	0	uvector length = 1	|  ForeignType	|
//------------------------------------------|
//|	1	foreign ptr (raw)					|
//------------------------------------------|
#define FOREIGN_PTR			1
#define FOREIGN_SIZE		1

//	Compiled Code block
//------------------------------------------|
//|	0	uvector length 	|  CompiledCodeType	|
//------------------------------------------|
//|	1	magic number identifier: 0xDEADBEEF	|
//------------------------------------------|
//|	2	references vector					|
//|-----------------------------------------|
//|	3	properties list						|
//|-----------------------------------------|
//|	4	code bytes 0 - 3					|
//------------------------------------------|
//|	5	code bytes 4 - 7					|
//------------------------------------------|
//	etc.
#define COMPILED_CODE_MAGIC				1
#define COMPILED_CODE_REFERENCES		2
#define COMPILED_CODE_PROPERTIES		3
#define COMPILED_CODE_OFFSET			4
#define COMPILED_CODE_MAGIC_ID			0xDEADBEEF

#define COMPILED_CODE_HEADER_SIZE		3

//	ReadTable
//------------------------------------------|
//|	0	uvector length = 3	|  ReadTableType|
//------------------------------------------|
//|	1	read level (integer)				|
//------------------------------------------|
//|	2	backquote processing (xbool)		|
//|-----------------------------------------|
//|	3	table (256 * 2 entries)				|
//------------------------------------------|
//|	4	case								|
//------------------------------------------|
//|	5	unused								|
//------------------------------------------|
//
#define READTABLE_READ_LEVEL				1
#define READTABLE_BACKQUOTE_PROCESSING		2
#define READTABLE_TABLE						3
#define READTABLE_CASE						4

#define READTABLE_SIZE						5

//	Complex number
//------------------------------------------|
//|	0	uvector length = 2	|  ComplexType	|
//------------------------------------------|
//|	1	real component						|
//------------------------------------------|
//|	2	imaginary component					|
//|-----------------------------------------|
//|	3	unused								|
//|-----------------------------------------|
#define COMPLEX_REAL						1
#define COMPLEX_IMAGINARY					2

#define COMPLEX_SIZE						3	

//	Ratio number
//------------------------------------------|
//|	0	uvector length = 2	|  RatioType	|
//------------------------------------------|
//|	1	numerator							|
//------------------------------------------|
//|	2	denominator							|
//|-----------------------------------------|
//|	3	unused								|
//|-----------------------------------------|
#define RATIO_NUMERATOR						1
#define RATIO_DENOMINATOR					2

#define RATIO_SIZE							3	

//	Bignum (arbitrary precision integer) number
//------------------------------------------|
//|	0	uvector length		|  BignumType	|
//------------------------------------------|
//|	1	number of 32-bit cells * 2 + sign	|
//------------------------------------------|
//|	2	cell 0								|
//|-----------------------------------------|
//|	3	cell 1								|
//|-----------------------------------------|
//|	4	etc.								|
//|-----------------------------------------|
#define BIGNUM_LENGTH						1
#define BIGNUM_FIRST_CELL					2

#define BIGNUM_HEADER_SIZE					1	

//	Foreign heap pointer (uses malloc()/free())
//------------------------------------------|
//|	0	uvector length = 2| ForeignHeapType	|
//------------------------------------------|
//|	1	foreign heap ptr (raw)				|
//------------------------------------------|
//|	2	tagged size of heap block (in bytes)|
//------------------------------------------|
//|	3	wrapped integer code				|
//|     0 = normal							|
//|		1 = callback thunk,					|
//|		2 = critical section				|
//------------------------------------------|
#define FOREIGN_HEAP_PTR					1
#define FOREIGN_HEAP_SIZE					2
#define FOREIGN_HEAP_TYPE					3
#define FOREIGN_HEAP_PTR_SIZE				3

#define FOREIGN_HEAP_TYPE_NORMAL			0
#define FOREIGN_HEAP_TYPE_CALLBACK_THUNK	1
#define FOREIGN_HEAP_TYPE_CRITICAL_SECTION	2

//	Weak pointer
//------------------------------------------|
//|	0	uvector length = 1| WeakPointerType	|
//------------------------------------------|
//|	1	weak ptr (lisp object)				|
//------------------------------------------|
#define WEAK_PTR			1
#define WEAK_PTR_SIZE		1

//	Simple Vector
//------------------------------------------|
//|	0	uvector length 	|  SimpleVectorType	|
//------------------------------------------|
//|	1	vector length						|
//------------------------------------------|
//|	2	cell 0								|
//|-----------------------------------------|
//|	3	cell 1								|
//|-----------------------------------------|
//|	4	etc.								|
//------------------------------------------|
//	etc.
#define SIMPLE_VECTOR_LENGTH			1
#define SIMPLE_VECTOR_START				2

//	Simple Character Vector
//------------------------------------------|
//|	0	uvector length|SimpleCharVectorType	|
//------------------------------------------|
//|	1	vector length						|
//------------------------------------------|
//|	2	chars 0-1							|
//|-----------------------------------------|
//|	3	chars 2-3							|
//|-----------------------------------------|
//|	4	etc.								|
//------------------------------------------|
//	etc.
#define SIMPLE_CHAR_VECTOR_LENGTH		1
#define SIMPLE_CHAR_VECTOR_START		2

//	Simple Byte Vector
//------------------------------------------|
//|	0	uvector length|SimpleByteVectorType	|
//------------------------------------------|
//|	1	vector length						|
//------------------------------------------|
//|	2	bytes 0-3							|
//|-----------------------------------------|
//|	3	bytes 4-7							|
//|-----------------------------------------|
//|	4	etc.								|
//------------------------------------------|
//	etc.
#define SIMPLE_BYTE_VECTOR_LENGTH		1
#define SIMPLE_BYTE_VECTOR_START		2

//	Simple Short Vector
//------------------------------------------|
//|	0	uvector length|SimpleShortVectorType|
//------------------------------------------|
//|	1	vector length						|
//------------------------------------------|
//|	2	shorts 0-1							|
//|-----------------------------------------|
//|	3	shorts 2-3							|
//|-----------------------------------------|
//|	4	etc.								|
//------------------------------------------|
//	etc.
#define SIMPLE_SHORT_VECTOR_LENGTH		1
#define SIMPLE_SHORT_VECTOR_START		2

//	Simple Double Float Vector
//------------------------------------------------|
//|	0	uvector length|SimpleDoubleFloatVectorType|
//------------------------------------------------|
//|	1	vector length							  |
//------------------------------------------------|
//|	2	float 0 (low 32-bits)				      |
//|-----------------------------------------------|
//|	3	float 0 (high 32-bits)					  |
//------------------------------------------------|
//|	4	float 1 (low 32-bits)				      |
//|-----------------------------------------------|
//|	5	float 1 (high 32-bits)					  |
//|-----------------------------------------------|
//|	6	etc.									  |
//------------------------------------------------|
//	etc.
#define SIMPLE_DOUBLE_FLOAT_VECTOR_LENGTH		1
#define SIMPLE_DOUBLE_FLOAT_VECTOR_START		2

//	Simple Bit Vector
//------------------------------------------|
//|	0	uvector length|SimpleBitVectorType	|
//------------------------------------------|
//|	1	vector length						|
//------------------------------------------|
//|	2	bits 0-3	(one per byte)			|
//|-----------------------------------------|
//|	3	bits 4-7	(one per byte)			|
//|-----------------------------------------|
//|	4	etc.								|
//------------------------------------------|
//	etc.
#define SIMPLE_BIT_VECTOR_LENGTH		1
#define SIMPLE_BIT_VECTOR_START			2

//	Simple Single Float Vector
//------------------------------------------------|
//|	0	uvector length|SimpleSingleFloatVectorType|
//------------------------------------------------|
//|	1	vector length							  |
//------------------------------------------------|
//|	2	float 0								      |
//|-----------------------------------------------|
//|	3	float 1									  |
//------------------------------------------------|
//|	4	float 2								      |
//|-----------------------------------------------|
//|	5	float 3									  |
//|-----------------------------------------------|
//|	6	etc.									  |
//------------------------------------------------|
//	etc.
#define SIMPLE_SINGLE_FLOAT_VECTOR_LENGTH		1
#define SIMPLE_SINGLE_FLOAT_VECTOR_START		2

//	Single Float number
//------------------------------------------|
//|	0	uvector length = 1| SingleFloatType	|
//------------------------------------------|
//|	1	float	(32 bits, unwrapped)		|
//------------------------------------------|
#define SINGLE_FLOAT_OFFSET			1
#define SINGLE_FLOAT_SIZE			1

//	CLOS Instance  (future usage)
//------------------------------------------|
//|	0	uvector length = 2| CLOSInstanceType|
//------------------------------------------|
//|	1	class								|
//------------------------------------------|
//|	2	slots								|
//------------------------------------------|
//|	3	unused								|
//------------------------------------------|
#define CLOS_INSTANCE_CLASS_OFFSET	1
#define CLOS_INSTANCE_SLOTS_OFFSET	2
#define CLOS_INSTANCE_SIZE			2

//	Foreign stack pointer
//------------------------------------------|
//|	0	uvector length = 2| ForeignStackType|
//------------------------------------------|
//|	1	foreign stack ptr (raw)				|
//------------------------------------------|
//|	2	tagged size of block (in bytes)	    |
//------------------------------------------|
//|	3	unused								|
//------------------------------------------|
#define FOREIGN_STACK_PTR					1
#define FOREIGN_STACK_SIZE					2
#define FOREIGN_STACK_PTR_SIZE				3
#define ForeignStackStartMarker			((ForeignStackType << 3)    | UvectorLengthTag)
#define ForeignStackEndMarker			((ForeignStackEndType << 3) | UvectorLengthTag)

typedef LispObj LispReturnType;
#define LispFuncArgs LispObj _args...
#define LispFunction(func) LispReturnType func(LispFuncArgs)
#define LispDeclare(func) LispReturnType func(LispFuncArgs) 
typedef LispReturnType (*LispFunc)(LispFuncArgs);

void Error(const char*);
void Error(const char*, LispObj);
void Error(const char*, LispObj, LispObj);
void Error(const char*, LispObj, LispObj, LispObj);

LispObj LispCall0(LispFunc);
LispObj LispCall1(LispFunc, LispObj);
LispObj LispCall2(LispFunc, LispObj, LispObj);
LispObj LispCall3(LispFunc, LispObj, LispObj, LispObj);
LispObj LispCall4(LispFunc, LispObj, LispObj, LispObj, LispObj);
LispObj LispCall5(LispFunc, LispObj, LispObj, LispObj, LispObj, LispObj);
LispObj LispCall6(LispFunc, LispObj, LispObj, LispObj, LispObj, LispObj, LispObj);
LispObj LispCall7(LispFunc, LispObj, LispObj, LispObj, LispObj, LispObj, LispObj, LispObj);
LispObj LispCall8(LispFunc, LispObj, LispObj, LispObj, LispObj, LispObj, LispObj, LispObj, LispObj);

//#define CFuncBegin()	va_list va__; va_start(va__, args)
//#define FirstCArg() (args)
//#define NextCArg() (va_arg(va__, LispObj))  

LispObj findSymbol(const char*);
LispObj findPLSymbol(const char*);
LispObj cons(LispObj, LispObj);
LispObj list(LispObj ...);
LispObj Cnreverse(LispObj list);
LispObj vectorNode(LispObj size, LispObj initialList);
LispObj vectorNode(LispObj size);
LispObj packageNode(LispObj name);
LispObj readableNode();
LispObj charNode(long c);
LispObj integerNode(long);
LispObj doubleFloatNode(double);
LispObj singleFloatNode(double);
LispObj bignumNode(LispObj length);
LispObj ratioNode(LispObj num, LispObj denom);
LispObj complexNode(LispObj real, LispObj imag);
LispObj* vectorStart(LispObj);
void checkNumArgs(long numArgsPassed, long numRequired);
void checkNumArgsRange(long numArgsPassed, long min, long max);
LispObj Cread_delimited_list(LispObj ch, LispObj stream);
void checkStream(LispObj);
void checkOutputStream(LispObj);
void checkInputStream(LispObj);
void checkArray(LispObj);
void checkSimpleVector(LispObj);
void checkString(LispObj);
void checkChar(LispObj);
void checkList(LispObj);
void checkSequence(LispObj);
void checkCons(LispObj);
void checkSymbol(LispObj);
void checkFunction(LispObj);
void checkInteger(LispObj);
void checkLispInteger(LispObj);
void checkFloat(LispObj);
void checkReal(LispObj);
void checkNumber(LispObj);
void checkCharacter(LispObj);
void checkPackage(LispObj);
void checkUvector(LispObj);
void checkBit(LispObj);
xbool isCons(LispObj);
LispObj stringNode(const char*);
LispObj wstringNode(const wchar_t*);
LispObj createBignum(long*);
LispObj symbolNode(LispObj);
LispObj foreignNode();
LispObj eval(LispObj s, LispObj env);
void initializeKernelSymbols();
LispObj findSymbol(const char*);
LispObj findKeyword(const char*);
LispObj findKeywordSym(LispObj str);
LispObj AllocVector(long num); 
LispObj LispAllocVector(long num); 
LispObj LispAllocVectorTagged(LispObj num); 
LispObj ReserveConsPage();
LispObj AllocCons();
LispObj AllocLocalCons();
long listLength(LispObj);
long sequenceLength(LispObj);
LispObj compiledFunctionNode(LispObj code, LispObj length, LispObj refs, 
							 LispObj numRefs, LispObj env, LispObj info,
							 LispObj appendRefsToCode);
LispObj kernelFunctionNode(LispFunc);
void initKernelFunctions();
LispObj lastCons(LispObj);
LispObj Creverse(LispObj);
void setSymbolFunction(LispObj sym, LispObj func, LispObj type);
void updateSymbolFunctionAddress(LispObj s, LispFunc funcaddr);
void garbageCollect(long level);
LispObj readtableNode();
void writeHeapToFile(LispObj);
void readHeapFromFile(LispObj);
void dumpHeapToFile(LispObj);
LispObj _Write(LispObj s, LispObj output);
LispObj readExpression(LispObj stream);
void pushCatcher(LispObj, unsigned long*);
void popCatcher();
void establishSpecialBindings(LispObj bindings);
void popSpecials();
LispObj byteVector(LispObj length);
LispObj charVector(LispObj length);
LispObj inputFileStreamNode(LispObj path);
LispObj outputFileStreamNode(LispObj path);
void flushStream(LispObj outputStream);
LispObj getCharacter(LispObj s);
void putbackCharacter(LispObj stream, LispObj c);
LispObj Cmember(LispObj item, LispObj list);
void outputChars(LispObj bytes, long start, long length, LispObj os);
void outputChars(LispObj chars, long length, LispObj os);
void outputChar(LispObj ch, LispObj os);
xbool pointsIntoHeap(unsigned long n);
LispObj addressFindFunction(LispObj address);
void flushEphemeralHeaps();
LispObj Heap1Capacity();
LispObj Heap1CurrentlyUsed();
LispObj _Add(LispObj n1, LispObj n2);
LispObj _Subtract(LispObj n1, LispObj n2);
LispObj _Multiply(LispObj n1, LispObj n2);
LispObj _Divide(LispObj n1, LispObj n2);
void createSymbolTableEntry(LispObj sym);
void createFuncTableEntry(LispObj sym);
LispObj nullTerminate(LispObj str);
long lispStringsEqual(LispObj str1, LispObj str2);
void addFinalizationObject(LispObj obj, LispObj func);
LispObj getGCExecRegistry();
void addGCExecRegistry(LispObj func);
void removeGCExecRegistry(LispObj func);
LispObj createLispInteger(long num);
LispObj createUnsignedLispInteger(unsigned long num);
void initializeGarbageCollector();
BOOL handleMemoryAccessException(byte*);
void registerUntaggedValues(LispObj uvec, LispObj index);	// all values starting with
															// index (which should be even!)
															// are to be considered untagged
LONG getImageLoadsCount(void);
LispObj* createNewQV();
void initLisp();
LispObj RunSecondaryThread(LispObj func);
unsigned long lispIntegerToUnsignedLong(LispObj);
long lispIntegerToLong(LispObj);
LispObj consoleOverflow(LispObj s);
LispObj fileOverflow(LispObj s);
LispObj stringOverflow(LispObj s);
LispObj consoleUnderflow(LispObj s);
LispObj fileUnderflow(LispObj s);
LispObj stringUnderflow(LispObj s);
LispObj weakPointer();
LispObj compileExpression(LispObj, LispObj lexMacros, LispObj lexSymbolMacros);
LispObj compileForm(LispObj, LispObj, LispObj resultType);
LispObj compileLambdaExpression(LispObj lambda, LispObj name, LispObj env, LispObj lexMacros, LispObj lexSymbolMacros);
LispObj processEachHeapBlock(LispObj func, LispObj processConses);
LispObj processEachHeapBlock(LispFunc func, LispObj processConses);

LispObj symbolValue(LispObj sym);
void setSymbolValue(LispObj sym, LispObj value);	
LispFunc functionAddress(LispObj func);
LispObj setSymbolMacro(LispObj sym, LispObj func);
LispObj setSpecialOperator(LispObj sym);
LispObj pushDynamicBinding(LispObj sym, LispObj val);
LispObj popDynamicBinding(LispObj sym);
LispObj LoadLocalHeap();

// lisp functions
LispDeclare(Read);
LispDeclare(Write);
LispDeclare(SymbolValue);
LispDeclare(SymbolFunction);
LispDeclare(listStar);
LispDeclare(lispAppend);
LispDeclare(lispList);
LispDeclare(Funcall);
LispDeclare(create_closure);
LispDeclare(LoadLispImage);
LispDeclare(SaveLispImage);
LispDeclare(Terpri);
LispDeclare(Force_Output);
LispDeclare(WrongNumberOfArgs);
LispDeclare(Close);
LispDeclare(Apply);
LispDeclare(LispError);
LispDeclare(Package_Hash_Index);
LispDeclare(Execution_Address);
LispDeclare(Values);
LispDeclare(Address_Find_Function_Callback);
LispDeclare(Stack_Trace);
LispDeclare(Throw_Exception);

// reader macros
LispDeclare(doublequoteMacro);
LispDeclare(quoteMacro);
LispDeclare(leftparenMacro);
LispDeclare(rightparenMacro);
LispDeclare(commaMacro);
LispDeclare(semicolonMacro);
LispDeclare(backquoteMacro);
LispDeclare(poundQuoteMacro);
LispDeclare(poundLeftParenMacro);
LispDeclare(poundBackslashMacro);
LispDeclare(bracketedCommentMacro);

LispDeclare(Get_Time_Units_Per_Second);
LispDeclare(Probe_File);
LispDeclare(Memory_Report);

LispDeclare(Safecall);

LispDeclare(User_HomeDir_Namestring);

LispDeclare(CormanLisp_Directory_Namestring);
LispDeclare(Change_Directory);
LispDeclare(Image_Loads_Count);

LispDeclare(Is_Bad_Mem_Ptr);
//
//	QV pointers
//
#define NUM_STACK_MARKERS			60
#define NUM_FOREIGN_CELLS           64      // foreign calls allow up to 64 params
#define NUM_RETURN_VALUES          128      // allow up to 128 return values

enum
{
	Nil_Index						= 0,
	T_Index							= 1,
	MULTIPLE_RETURN_VALUES_Index	= 2,
	FINALIZATION_REGISTRY_Index		= 3,
	WEAK_PTR_REGISTRY_Index			= 4,
	THREAD_HEAP_Index				= 5,
	THREAD_HEAP_END_Index			= 6,
	STACK_MARKER_INDEX_Index		= 7,
	STACK_MARKERS_Index				= 8,
	// reserve 60 cells for the stack markers (2 at each transition)
    FOREIGN_CELLS_Index             = STACK_MARKERS_Index + NUM_STACK_MARKERS,
    RETURN_VALUES_Index             = FOREIGN_CELLS_Index + NUM_FOREIGN_CELLS,
	CL_PACKAGE_Index				= RETURN_VALUES_Index + NUM_RETURN_VALUES,
	CHARACTER_Index,				// etc.
	QUOTE_Index,
	FUNCTION_Index,
	LIST_Index,
	APPEND_Index,
	COMMA_TOKEN_Index,
	COMMA_DOT_TOKEN_Index,
	COMMA_ATSIGN_TOKEN_Index,
	CEof_Index,
	MACRO_Index,
	SPECIAL_OPERATOR_Index,
	LAMBDA_Index,
	SYMBOL_VALUE_Index,
	SYMBOL_TABLE_COUNT_Index,
	SYMBOL_TABLE_VAR_COUNT_Index,
	LISTSTAR_Index,
	SETQ_Index,
	FUNCALL_Index,
	SYMBOL_FUNCTION_SYM_Index,
	CREATE_CLOSURE_Index,
	BLOCK_Index,
	PROGN_Index,
	COMPILER_STACK_ENV_Index,
	COMPILER_HEAP_ENV_Index,
	EBP_Index,
	EBX_Index,
	ARG_ENV_Index,
	ARG_NUM_Index,
	DOT_Index,
	RIGHT_PAREN_Index,
	READTABLE_Index,
	STRINGNODE_Index,
	STANDARD_INPUT_Index,
	STANDARD_OUTPUT_Index,
	WRONG_NUMBER_OF_ARGS_Index,
	INVALID_FIXNUM_Index,
	UNBOUND_VARIABLE_Index,
	CHECK_LIST_Index,
	IF_Index,
	UNDEFINED_FUNCTION_Index,
	LET_Index,
	LETSTAR_Index,
	TAGBODY_Index,
	GO_Index,
	RETURN_Index,
	RETURN_FROM_Index,
	COMPILER_LABELS_Index,
	COMPILER_BLOCKS_Index,
	PLUS_Index,
	MINUS_Index,
	ONEPLUS_Index,
	ONEMINUS_Index,
	NUMERIC_EQUAL_Index,
	LESS_EQUAL_Index,
	GREATER_EQUAL_Index,
	LESS_Index,
	GREATER_Index,
	ALLOC_CONS_Index,
	ALLOC_VECTOR_Index,
	CONS_Index,
	COMPILER_CLEANUPS_Index,
	COMPILER_RUNTIME_Index,
	CATCH_Index,
	THROW_Index,
	UNWIND_PROTECT_Index,
	PUSH_CATCHER_Index,
	POP_CATCHER_Index,
	THROW_EXCEPTION_Index,
	THROW_TAG_Index,
	THROW_RETURN_VALUE_Index,
	THROW_NUM_RETURN_VALUES_Index,
	VALUES_Index,
	LBYTE_Index,
	MULTIPLE_VALUE_CALL_Index,
	APPLY_Index,
	MACROEXPAND_ALL_Index,
	MACROEXPAND_ALL_EXCEPT_TOP_Index,
	MACROEXPAND_INLINE_Index,
	EMBEDDED_LAMBDAS_Index,
	FLET_Index,
	LABELS_Index,
	MACROLET_Index,
	EVAL_WHEN_Index,
	ENV_COUNTER_Index,
	ENVIRONMENT_Index,
	CONSOLE_INPUT_Index,
	CONSOLE_OUTPUT_Index,
	FILE_INPUT_Index,
	FILE_OUTPUT_Index,
	STRING_INPUT_Index,
	STRING_OUTPUT_Index,
	TOP_LEVEL_Index,
	LISPERROR_Index,
	LAMBDA_OPTIONAL_Index,
	LAMBDA_REST_Index,
	LAMBDA_KEY_Index,
	LAMBDA_AUX_Index,
	LAMBDA_ALLOW_OTHER_KEYS_Index,
	LAMBDA_WHOLE_Index,
	LAMBDA_BODY_Index,

	WHITESPACE_CHAR_TYPE_Index,
 	CONSTITUENT_CHAR_TYPE_Index,
	TERMINATING_MACRO_CHAR_TYPE_Index,
	NON_TERMINATING_MACRO_CHAR_TYPE_Index,
	DISPATCHING_TERMINATING_MACRO_CHAR_TYPE_Index,
	DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE_Index,
	SINGLE_ESCAPE_CHAR_TYPE_Index,
	MULTIPLE_ESCAPE_CHAR_TYPE_Index,
 	ILLEGAL_CHAR_TYPE_Index,

	INTERNAL_Index,
	EXTERNAL_Index,
	PACKAGE_Index,
	PACKAGE_LIST_Index,
	STRING_Index,
	READ_Index,
	USER_PACKAGE_Index,
	CORMANLISP_PACKAGE_Index,
	KEYWORD_PACKAGE_Index,
	EQ_Index,
	CAR_SYM_Index,
	CDR_SYM_Index,
	NULL_SYM_Index,
	NOT_Index,
	INTERN_Index,

	LAMBDA_SPECIAL_VARS_Index,
	LAMBDA_DECLARATIONS_Index,
	LAMBDA_SPECIAL_DECS_Index,
	DECLARE_Index,
	SPECIAL_Index,
	POP_SPECIAL_BINDINGS_Index,
	PUSH_SPECIAL_BINDINGS_Index,
	EXECUTE_KEY_Index,
	EVAL_Index,
	BIT_Index,
	COMMON_LISP_READTABLE_Index,

	// compiler switches
	COMPILER_SAVE_LAMBDAS_Index,
	COMPILER_SAVE_LAMBDA_LISTS_Index,
	COMPILER_SAVE_STACK_FRAME_INFO_Index,
	COMPILER_SAVE_ENVIRONMENT_INFO_Index,
	COMPILER_SAVE_DEBUG_INFO_Index,
	COMPILER_SAFETY_LEVEL_Index,

	LAMBDA_LIST_Index,
	ENVIRONMENT_INFO_Index,
	STACK_FRAME_Index,
	DEBUG_INFO_Index,
	FUNCTION_NAME_Index,
	CREATE_BIG_NUM_Index,

	// exception related stuff
	SYSTEM_EXCEPTION_Index,
	EX_ACCESS_VIOLATION_Index,
	EX_ARRAY_BOUNDS_EXCEEDED_Index,
	EX_BREAKPOINT_Index,
	EX_DATATYPE_MISALIGNMENT_Index,
	EX_FLT_DENORMAL_OPERAND_Index,
	EX_FLT_DIVIDE_BY_ZERO_Index,
	EX_FLT_INEXACT_RESULT_Index,
	EX_FLT_INVALID_OPERATION_Index,
	EX_FLT_OVERFLOW_Index,
	EX_FLT_STACK_CHECK_Index,
	EX_FLT_UNDERFLOW_Index,
	EX_ILLEGAL_INSTRUCTION_Index,
	EX_IN_PAGE_ERROR_Index,
	EX_INT_DIVIDE_BY_ZERO_Index,
	EX_INVALID_DISPOSITION_Index,
	EX_NONCONTINUABLE_EXCEPTION_Index,
	EX_PRIV_INSTRUCTION_Index,
	EX_SINGLE_STEP_Index,
	EX_STACK_OVERFLOW_Index,
	EX_USER_ABORT_Index,
	UREF_Index,
	UREF_SET_Index,
	MULTIPLE_VALUE_PROG1_Index,
	THE_Index,
	TYPEP_Index,
	FORMAT_Index,
	CHECK_TYPE_BODY_Index,
	HASH_TABLE_Index,
	FUNCALL_IGNORING_ERRORS_Index,
	AREF_Index,
	AREF_FUNCS_Index,
	AREF_SETTER_FUNCS_Index,
	SETF_AREF_Index,
	ESTABLISH_SPECIAL_BINDINGS_Index,
	POP_SPECIALS_Index,
	COMPILER_CODE_BUFFER_Index,
	COMPILE_FUNCTION_CALL_FORM_Index,

	// declaration symbols
	COMPILATION_SPEED_Index,
	DEBUG_Index,
	SAFETY_Index,
	SPACE_Index,
	SPEED_Index,
	DECLARATION_Index,
	DYNAMIC_EXTENT_Index,
	FTYPE_Index,
	IGNORABLE_Index,
	IGNORE_Index,
	INLINE_Index,
	NOTINLINE_Index,
	OPTIMIZE_Index,
	TYPE_Index,

	COMPILER_OPTIMIZE_SPEED_Index,
	COMPILER_OPTIMIZE_SAFETY_Index,
	COMPILER_OPTIMIZE_SPACE_Index,
	COMPILER_OPTIMIZE_DEBUG_Index,
	COMPILER_OPTIMIZE_COMPILATION_SPEED_Index,

	COMPILER_VARIABLE_TYPES_Index,
	COMPILER_DYNAMIC_EXTENT_VARS_Index,

	SYMBOL_Index,
	FIXNUM_Index,
	BIGNUM_Index,
	FLOAT_Index,
	COMPLEX_Index,
	RATIO_Index,
	NUMBER_Index,

	CONSOLE_INPUT_STREAM_Index,
	CONSOLE_OUTPUT_STREAM_Index,
	
	// compiler keywords
	Dest_EAX_Index,
	Dest_EAX_Operand_Index,
	Dest_Stack_Index,
	Dest_Zero_Flag_Index,

	COMPILER_WARN_ON_UNDEFINED_FUNCTION_Index,
	COMPILER_WARN_ON_DYNAMIC_RETURN_Index,
	COMPILER_FUNCTION_NAME_Index,
	COMPILER_USES_ENV_Index,
	GET_INTERNAL_RUN_TIME_Index,
	GC_TIME_COUNTER_Index,
	PATHNAME_Index,
	DEALLOCATE_C_HEAP_Index,
	SYSTEM_INTERNALS_Index,
	SAVE_IMAGE_CLEANUP_FUNCS_Index,
	LOAD_IMAGE_RESTORE_FUNCS_Index,
	CONSOLE_STREAM_Index,
	FILE_STREAM_Index,
	STRING_STREAM_Index,
	INPUT_KEY_Index,
	OUTPUT_KEY_Index,
	BIDIRECTIONAL_KEY_Index,
	CONSOLE_OVERFLOW_FUNCTION_Index,
	CONSOLE_UNDERFLOW_FUNCTION_Index,
	FILE_OVERFLOW_FUNCTION_Index,
	FILE_UNDERFLOW_FUNCTION_Index,
	STRING_OVERFLOW_FUNCTION_Index,
	STRING_UNDERFLOW_FUNCTION_Index,
	COLLECT_LITERALS_Index,
	EXECUTION_ADDRESS_Index,
	FIND_FUNCTION_CURR_OFFSET_Index,
	FIND_FUNCTION_CURR_FUNC_Index,
	FIND_FUNCTION_CURR_ADDR_Index,
	ADDRESS_FIND_FUNCTION_CALLBACK_Index,
	TERMINAL_IO_Index,
	TYPEEXPAND_ALL_Index,
	LOOKUP_FTYPE_Index,
	LOAD_QV_REG_Index,
	GET_Index,
	TYPE_DISCRIMINATOR_Index,
	CORMANLISP_DIRECTORY_Index,
	HEAP_CHECKING_Index,
	INVALID_OBJECT_COUNT_Index,
	ERROR_TRACE_Index,
	TRACE_EXCEPTIONS_Index,
	SHORT_SYM_Index,
	SOURCE_FILE_Index,
	SOURCE_LINE_Index,
	SAVE_DEBUG_INFO_Index,
	SOURCE_LINES_Index,
	CODE_JUMP_TABLE_REFS_Index,
	CODE_VAR_TABLE_REFS_Index,
	CODE_ENV_TABLE_REFS_Index,
	COMPILER_COLLECT_JUMP_TABLE_REFS_Index,
	COMPILER_COLLECT_VAR_TABLE_REFS_Index,

	PLUS_EAX_EDX_Index,
	MINUS_EAX_EDX_Index,
	ERROR_SIGNAL_Index,
	INTERNAL_TIME_UNITS_PER_SECOND_Index,
	GC_TIME_UNITS_PER_SECOND_Index,
	APPEND_REFS_TO_CODE_Index,
	SINGLE_FLOAT_Index,
	DOUBLE_FLOAT_Index,
	SHORT_FLOAT_Index,
	LONG_FLOAT_Index,
	READ_DEFAULT_FLOAT_FORMAT_Index,
	SYS_GLOBALS_ADDRESS_Index,
	CORMANLISP_SERVER_DIRECTORY_Index,
	CURRENT_THREAD_ID_Index,
	CURRENT_THREAD_HANDLE_Index,
	CURRENT_PROCESS_ID_Index,
	CURRENT_PROCESS_HANDLE_Index,
	EXIT_THREAD_TAG_Index,
	DEALLOCATE_CRITICAL_SECTION_Index,
	COMPRESS_IMG_Index,
	ENV_Index,
	ARGS_Index,
	FRAME_Index,
	GLOBAL_Index,
	COMPILED_SPECIALS_Index,
	GET_CURRENT_ENVIRONMENT_Index,
	COMPILER_FRAME_INFO_Index,
	COMPILER_ENVIRONMENT_Index,
	THROW_SYSTEM_EXCEPTION_Index,
	WITH_ONLY_LEXICALS_Index,
	COMPILER_WARN_ON_UNUSED_VAR_Index,
	WRITE_Index,
	INLINE_PROCLAIM_P_Index,
	FINALIZATION_PENDING_Index,
	EXECUTE_FINALIZERS_Index,
	UNDEFINED_FUNCTIONS_Index,
	GENSYM_Index,
	COMPILER_CHECK_ARGS_NUM_Index,
	COMPILER_CHECK_TYPES_Index,
	COMPILER_FOLD_CONSTANTS_Index,
	COMPILER_INLINE_FUNCTIONS_Index,
	COMPILER_OPTIMIZE_TAIL_RECURSION_Index,
	LOAD_LOCAL_HEAP_Index,
	COLLECT_LEXICAL_MACROS_Index,
	LEXICAL_MACROS_Index,
	COMPILER_LAMBDA_LIST_Index,
	REMOVE_TAIL_RECURSION_Index,
	LOOKUP_SETF_FUNCTION_Index,
	SETF_Index,
	COMPILER_CHECK_KEY_ARGS_Index,
	INVALID_KEY_ARG_Index,
	LOCALLY_Index,
	LOAD_TIME_VALUE_Index,
	GET_CALLBACK_Index,
	HEAP_FAULT_HANDLER_Index,
	ERROR_OUTPUT_Index,
	COMPILER_WARN_ON_ASSUMED_SPECIAL_Index,
	COMPILING_LAMBDA_Index,
	CAPTURED_LEXICAL_CALLBACK_Index,
	WARN_Index,
	RESET_HASH_ID_Index,
	CAPTURE_COMPILER_ENVIRONMENT_Index,
	LOAD_TIME_VALUES_Index,
	GC_CRITICAL_SECTION_Index,
	TQ_CRITICAL_SECTION_Index,
	PAGE_TABLE_Index,
    DISASSEMBLY_OUTPUT_BUF_Index,
    LEXICAL_SYMBOL_MACROS_Index,
    COLLECT_LEXICAL_SYMBOL_MACROS_Index,
	HEAP_0_START_Index,
	HEAP_0_END_Index,
	HEAP_1_START_Index,
	HEAP_1_END_Index,
	HEAP_2_START_Index,
	HEAP_2_END_Index,
	HEAP_0_GC_ID_Index,
	HEAP_1_GC_ID_Index,
	HEAP_2_GC_ID_Index,
};

enum { SYSTEM_OBJ_MAX = 1024 };
const long FirstJumpTableEntry = SYSTEM_OBJ_MAX;
const long NumJumpTableEntries = 0x8000;
const long JumpTableCellsPerEntry = 2;
const long FirstSpecialSymbolEntry = FirstJumpTableEntry + 
				(NumJumpTableEntries * JumpTableCellsPerEntry);
const long NumSpecialSymbolEntries = 8192;
const long QV_MAX = FirstSpecialSymbolEntry + NumSpecialSymbolEntries;

extern LispObj* QV;
extern DWORD QV_Index;
extern DWORD Thread_Index;

#define NIL							QV[Nil_Index]
#define T							QV[T_Index]
#define MULTIPLE_RETURN_VALUES		QV[MULTIPLE_RETURN_VALUES_Index]
#define FINALIZATION_REGISTRY		QV[FINALIZATION_REGISTRY_Index]
#define WEAK_PTR_REGISTRY			QV[WEAK_PTR_REGISTRY_Index]
#define THREAD_HEAP					QV[THREAD_HEAP_Index]
#define THREAD_HEAP_END				QV[THREAD_HEAP_END_Index]
#define STACK_MARKER_INDEX			QV[STACK_MARKER_INDEX_Index]
#define STACK_MARKERS				QV[STACK_MARKERS_Index]
#define FOREIGN_CELLS				QV[FOREIGN_CELLS_Index]
#define RETURN_VALUES				QV[RETURN_VALUES_Index]
#define CL_PACKAGE					QV[CL_PACKAGE_Index]
#define CHARACTER					QV[CHARACTER_Index]
#define QUOTE						QV[QUOTE_Index]
#define FUNCTION					QV[FUNCTION_Index]
#define LIST						QV[LIST_Index]
#define APPEND						QV[APPEND_Index]
#define COMMA_TOKEN					QV[COMMA_TOKEN_Index]
#define COMMA_DOT_TOKEN				QV[COMMA_DOT_TOKEN_Index]
#define COMMA_ATSIGN_TOKEN			QV[COMMA_ATSIGN_TOKEN_Index]
#define Eof							QV[CEof_Index]
#define MACRO						QV[MACRO_Index]
#define SPECIAL_OPERATOR			QV[SPECIAL_OPERATOR_Index]
#define LAMBDA						QV[LAMBDA_Index]
#define SYMBOL_VALUE_SYM			QV[SYMBOL_VALUE_Index]
#define SYMBOL_TABLE_COUNT			QV[SYMBOL_TABLE_COUNT_Index]
#define SYMBOL_TABLE_VAR_COUNT		QV[SYMBOL_TABLE_VAR_COUNT_Index]
#define LISTSTAR					QV[LISTSTAR_Index]
#define SETQ						QV[SETQ_Index]
#define FUNCALL						QV[FUNCALL_Index]
#define SYMBOL_FUNCTION_SYM			QV[SYMBOL_FUNCTION_SYM_Index]
#define CREATE_CLOSURE				QV[CREATE_CLOSURE_Index]
#define BLOCK						QV[BLOCK_Index]
#define PROGN						QV[PROGN_Index]
#define COMPILER_STACK_ENV			QV[COMPILER_STACK_ENV_Index]
#define COMPILER_HEAP_ENV			QV[COMPILER_HEAP_ENV_Index]
#define EBP							QV[EBP_Index]
#define EBX							QV[EBX_Index]
#define ARG_ENV						QV[ARG_ENV_Index]
#define ARG_NUM						QV[ARG_NUM_Index]
#define DOT							QV[DOT_Index]
#define RIGHT_PAREN					QV[RIGHT_PAREN_Index]
#define READTABLE					QV[READTABLE_Index]
#define STRINGNODE					QV[STRINGNODE_Index]
#define STANDARD_INPUT				QV[STANDARD_INPUT_Index]
#define STANDARD_OUTPUT				QV[STANDARD_OUTPUT_Index]
#define WRONG_NUMBER_OF_ARGS		QV[WRONG_NUMBER_OF_ARGS_Index]
#define INVALID_FIXNUM				QV[INVALID_FIXNUM_Index]
#define UNBOUND_VARIABLE			QV[UNBOUND_VARIABLE_Index]
#define CHECK_LIST					QV[CHECK_LIST_Index]
#define IF							QV[IF_Index]
#define UNDEFINED_FUNCTION			QV[UNDEFINED_FUNCTION_Index]
#define LET							QV[LET_Index]
#define LETSTAR						QV[LETSTAR_Index]
#define TAGBODY						QV[TAGBODY_Index]
#define GO							QV[GO_Index]
#define RETURN						QV[RETURN_Index]
#define RETURN_FROM					QV[RETURN_FROM_Index]
#define COMPILER_LABELS				QV[COMPILER_LABELS_Index]
#define COMPILER_BLOCKS				QV[COMPILER_BLOCKS_Index]
#define PLUS						QV[PLUS_Index]
#define MINUS						QV[MINUS_Index]
#define ONEPLUS						QV[ONEPLUS_Index]
#define ONEMINUS					QV[ONEMINUS_Index]
#define NUMERIC_EQUAL				QV[NUMERIC_EQUAL_Index]
#define LESS_EQUAL					QV[LESS_EQUAL_Index]
#define GREATER_EQUAL				QV[GREATER_EQUAL_Index]
#define LESS						QV[LESS_Index]
#define GREATER						QV[GREATER_Index]
#define ALLOC_CONS					QV[ALLOC_CONS_Index]
#define ALLOC_VECTOR				QV[ALLOC_VECTOR_Index]
#define CONS						QV[CONS_Index]
#define COMPILER_CLEANUPS			QV[COMPILER_CLEANUPS_Index]
#define COMPILER_RUNTIME			QV[COMPILER_RUNTIME_Index]
#define CATCH						QV[CATCH_Index]
#define THROW						QV[THROW_Index]
#define UNWIND_PROTECT				QV[UNWIND_PROTECT_Index]
#define PUSH_CATCHER				QV[PUSH_CATCHER_Index]
#define POP_CATCHER					QV[POP_CATCHER_Index]
#define THROW_EXCEPTION				QV[THROW_EXCEPTION_Index]
#define THROW_TAG					QV[THROW_TAG_Index]
#define THROW_RETURN_VALUE			QV[THROW_RETURN_VALUE_Index]
#define THROW_NUM_RETURN_VALUES		QV[THROW_NUM_RETURN_VALUES_Index]
#define MULTIPLE_RETURN_VALUES		QV[MULTIPLE_RETURN_VALUES_Index]
#define VALUES						QV[VALUES_Index]
#define LBYTE						QV[LBYTE_Index]
#define MULTIPLE_VALUE_CALL			QV[MULTIPLE_VALUE_CALL_Index]
#define APPLY						QV[APPLY_Index]
#define MACROEXPAND_ALL				QV[MACROEXPAND_ALL_Index]
#define MACROEXPAND_ALL_EXCEPT_TOP	QV[MACROEXPAND_ALL_EXCEPT_TOP_Index]
#define MACROEXPAND_INLINE			QV[MACROEXPAND_INLINE_Index]
#define EMBEDDED_LAMBDAS			QV[EMBEDDED_LAMBDAS_Index]
#define FLET						QV[FLET_Index]
#define LABELS						QV[LABELS_Index]
#define MACROLET					QV[MACROLET_Index]
#define EVAL_WHEN					QV[EVAL_WHEN_Index]
#define ENV_COUNTER					QV[ENV_COUNTER_Index]
#define ENVIRONMENT					QV[ENVIRONMENT_Index]
#define CONSOLE_INPUT				QV[CONSOLE_INPUT_Index]
#define CONSOLE_OUTPUT				QV[CONSOLE_OUTPUT_Index]
#define FILE_INPUT					QV[FILE_INPUT_Index]
#define FILE_OUTPUT					QV[FILE_OUTPUT_Index]
#define STRING_INPUT				QV[STRING_INPUT_Index]
#define STRING_OUTPUT				QV[STRING_OUTPUT_Index]
#define TOP_LEVEL					QV[TOP_LEVEL_Index]
#define LISPERROR					QV[LISPERROR_Index]
#define LAMBDA_OPTIONAL				QV[LAMBDA_OPTIONAL_Index]
#define LAMBDA_REST					QV[LAMBDA_REST_Index]
#define LAMBDA_KEY					QV[LAMBDA_KEY_Index]
#define LAMBDA_AUX					QV[LAMBDA_AUX_Index]
#define LAMBDA_ALLOW_OTHER_KEYS		QV[LAMBDA_ALLOW_OTHER_KEYS_Index]
#define LAMBDA_WHOLE				QV[LAMBDA_WHOLE_Index]
#define LAMBDA_BODY					QV[LAMBDA_BODY_Index]

#define WHITESPACE_CHAR_TYPE						QV[WHITESPACE_CHAR_TYPE_Index]
#define CONSTITUENT_CHAR_TYPE						QV[CONSTITUENT_CHAR_TYPE_Index]
#define TERMINATING_MACRO_CHAR_TYPE					QV[TERMINATING_MACRO_CHAR_TYPE_Index]
#define NON_TERMINATING_MACRO_CHAR_TYPE				QV[NON_TERMINATING_MACRO_CHAR_TYPE_Index]
#define DISPATCHING_TERMINATING_MACRO_CHAR_TYPE		QV[DISPATCHING_TERMINATING_MACRO_CHAR_TYPE_Index]
#define DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE	QV[DISPATCHING_NON_TERMINATING_MACRO_CHAR_TYPE_Index]
#define SINGLE_ESCAPE_CHAR_TYPE						QV[SINGLE_ESCAPE_CHAR_TYPE_Index]
#define MULTIPLE_ESCAPE_CHAR_TYPE					QV[MULTIPLE_ESCAPE_CHAR_TYPE_Index]
#define ILLEGAL_CHAR_TYPE							QV[ILLEGAL_CHAR_TYPE_Index]
#define READ										QV[READ_Index]
#define USER_PACKAGE								QV[USER_PACKAGE_Index]
#define CORMANLISP_PACKAGE							QV[CORMANLISP_PACKAGE_Index]
#define KEYWORD_PACKAGE								QV[KEYWORD_PACKAGE_Index]

#define INTERNAL					QV[LAMBDA_BODY_Index]
#define EXTERNAL					QV[LAMBDA_BODY_Index]
#define PACKAGE						QV[PACKAGE_Index]
#define PACKAGE_LIST				QV[PACKAGE_LIST_Index]
#define STRING						QV[STRING_Index]
#define EQ							QV[EQ_Index]
#define CAR_SYM						QV[CAR_SYM_Index]
#define CDR_SYM						QV[CDR_SYM_Index]
#define NULL_SYM					QV[NULL_SYM_Index]
#define NOT							QV[NOT_Index]
#define INTERN						QV[INTERN_Index]
#define LAMBDA_SPECIAL_VARS			QV[LAMBDA_SPECIAL_VARS_Index]
#define LAMBDA_DECLARATIONS			QV[LAMBDA_DECLARATIONS_Index]
#define LAMBDA_SPECIAL_DECS			QV[LAMBDA_SPECIAL_DECS_Index]
#define DECLARE						QV[DECLARE_Index]
#define SPECIAL						QV[SPECIAL_Index]
#define POP_SPECIAL_BINDINGS		QV[POP_SPECIAL_BINDINGS_Index]
#define PUSH_SPECIAL_BINDINGS		QV[PUSH_SPECIAL_BINDINGS_Index]
#define EXECUTE_KEY					QV[EXECUTE_KEY_Index]
#define EVAL						QV[EVAL_Index]
#define BIT							QV[BIT_Index]
#define COMMON_LISP_READTABLE		QV[COMMON_LISP_READTABLE_Index]
#define COMPILER_SAVE_LAMBDAS		QV[COMPILER_SAVE_LAMBDAS_Index]
#define COMPILER_SAVE_LAMBDA_LISTS	QV[COMPILER_SAVE_LAMBDA_LISTS_Index]
#define COMPILER_SAVE_STACK_FRAME_INFO	QV[COMPILER_SAVE_STACK_FRAME_INFO_Index]
#define COMPILER_SAVE_ENVIRONMENT_INFO	QV[COMPILER_SAVE_ENVIRONMENT_INFO_Index]
#define COMPILER_SAVE_DEBUG_INFO	QV[COMPILER_SAVE_DEBUG_INFO_Index]
#define COMPILER_SAFETY_LEVEL		QV[COMPILER_SAFETY_LEVEL_Index]

#define LAMBDA_LIST					QV[LAMBDA_LIST_Index]
#define ENVIRONMENT_INFO			QV[ENVIRONMENT_INFO_Index]
#define STACK_FRAME					QV[STACK_FRAME_Index]
#define DEBUG_INFO					QV[DEBUG_INFO_Index]
#define FUNCTION_NAME				QV[FUNCTION_NAME_Index]
#define CREATE_BIG_NUM				QV[CREATE_BIG_NUM_Index]

#define SYSTEM_EXCEPTION			QV[SYSTEM_EXCEPTION_Index]
#define EX_ACCESS_VIOLATION			QV[EX_ACCESS_VIOLATION_Index]
#define EX_ARRAY_BOUNDS_EXCEEDED	QV[EX_ARRAY_BOUNDS_EXCEEDED_Index]
#define EX_BREAKPOINT				QV[EX_BREAKPOINT_Index]
#define EX_DATATYPE_MISALIGNMENT	QV[EX_DATATYPE_MISALIGNMENT_Index]
#define EX_FLT_DENORMAL_OPERAND		QV[EX_FLT_DENORMAL_OPERAND_Index]
#define EX_FLT_DIVIDE_BY_ZERO		QV[EX_FLT_DIVIDE_BY_ZERO_Index]
#define EX_FLT_INEXACT_RESULT		QV[EX_FLT_INEXACT_RESULT_Index]
#define EX_FLT_INVALID_OPERATION	QV[EX_FLT_INVALID_OPERATION_Index]
#define EX_FLT_OVERFLOW				QV[EX_FLT_OVERFLOW_Index]
#define EX_FLT_STACK_CHECK			QV[EX_FLT_STACK_CHECK_Index]
#define EX_FLT_UNDERFLOW			QV[EX_FLT_UNDERFLOW_Index]
#define EX_ILLEGAL_INSTRUCTION		QV[EX_ILLEGAL_INSTRUCTION_Index]
#define EX_IN_PAGE_ERROR			QV[EX_IN_PAGE_ERROR_Index]
#define EX_INT_DIVIDE_BY_ZERO		QV[EX_INT_DIVIDE_BY_ZERO_Index]
#define EX_INVALID_DISPOSITION		QV[EX_INVALID_DISPOSITION_Index]
#define EX_NONCONTINUABLE_EXCEPTION	QV[EX_NONCONTINUABLE_EXCEPTION_Index]
#define EX_PRIV_INSTRUCTION			QV[EX_PRIV_INSTRUCTION_Index]
#define EX_SINGLE_STEP				QV[EX_SINGLE_STEP_Index]
#define EX_STACK_OVERFLOW			QV[EX_STACK_OVERFLOW_Index]
#define EX_USER_ABORT				QV[EX_USER_ABORT_Index]
#define UREF						QV[UREF_Index]
#define UREF_SET					QV[UREF_SET_Index]
#define MULTIPLE_VALUE_PROG1		QV[MULTIPLE_VALUE_PROG1_Index]
#define THE							QV[THE_Index]
#define TYPEP						QV[TYPEP_Index]
#define FORMAT						QV[FORMAT_Index]
#define CHECK_TYPE_BODY				QV[CHECK_TYPE_BODY_Index]
#define HASH_TABLE					QV[HASH_TABLE_Index]
#define FUNCALL_IGNORING_ERRORS		QV[FUNCALL_IGNORING_ERRORS_Index]
#define AREF						QV[AREF_Index]
#define SETF_AREF					QV[SETF_AREF_Index]
#define AREF_FUNCS					QV[AREF_FUNCS_Index]
#define AREF_SETTER_FUNCS			QV[AREF_SETTER_FUNCS_Index]
#define	ESTABLISH_SPECIAL_BINDINGS	QV[ESTABLISH_SPECIAL_BINDINGS_Index]
#define	POP_SPECIALS				QV[POP_SPECIALS_Index]
#define COMPILER_CODE_BUFFER		QV[COMPILER_CODE_BUFFER_Index]
#define COMPILE_FUNCTION_CALL_FORM	QV[COMPILE_FUNCTION_CALL_FORM_Index]

// declaration symbols
#define COMPILATION_SPEED			QV[COMPILATION_SPEED_Index]
#define DEBUG						QV[DEBUG_Index]
#define SAFETY						QV[SAFETY_Index]
#define SPACE						QV[SPACE_Index]
#define SPEED						QV[SPEED_Index]
#define DECLARATION					QV[DECLARATION_Index]
#define DYNAMIC_EXTENT				QV[DYNAMIC_EXTENT_Index]
#define FTYPE						QV[FTYPE_Index]
#define IGNORABLE					QV[IGNORABLE_Index]
#define IGNORE_DECL					QV[IGNORE_Index]
#define INLINE						QV[INLINE_Index]
#define NOTINLINE					QV[NOTINLINE_Index]
#define OPTIMIZE					QV[OPTIMIZE_Index]
#define TYPE						QV[TYPE_Index]

#define COMPILER_OPTIMIZE_SPEED		QV[COMPILER_OPTIMIZE_SPEED_Index]
#define COMPILER_OPTIMIZE_SAFETY	QV[COMPILER_OPTIMIZE_SAFETY_Index]
#define COMPILER_OPTIMIZE_SPACE		QV[COMPILER_OPTIMIZE_SPACE_Index]
#define COMPILER_OPTIMIZE_DEBUG		QV[COMPILER_OPTIMIZE_DEBUG_Index]
#define COMPILER_OPTIMIZE_COMPILATION_SPEED	QV[COMPILER_OPTIMIZE_COMPILATION_SPEED_Index]

#define COMPILER_VARIABLE_TYPES		QV[COMPILER_VARIABLE_TYPES_Index]
#define COMPILER_DYNAMIC_EXTENT_VARS QV[COMPILER_DYNAMIC_EXTENT_VARS_Index]
#define GET_INTERNAL_RUN_TIME		QV[GET_INTERNAL_RUN_TIME_Index]

#define SYMBOL						QV[SYMBOL_Index]
#define FIXNUM						QV[FIXNUM_Index]
#define BIGNUM						QV[BIGNUM_Index]
#define FLOAT_SYM					QV[FLOAT_Index]
#define COMPLEX						QV[COMPLEX_Index]
#define RATIO						QV[RATIO_Index]
#define NUMBER						QV[NUMBER_Index]

#define CONSOLE_INPUT_STREAM		QV[CONSOLE_INPUT_STREAM_Index]
#define CONSOLE_OUTPUT_STREAM		QV[CONSOLE_OUTPUT_STREAM_Index]

#define Dest_EAX					QV[Dest_EAX_Index]
#define Dest_EAX_Operand			QV[Dest_EAX_Operand_Index]
#define Dest_Stack					QV[Dest_Stack_Index]
#define	Dest_Zero_Flag				QV[Dest_Zero_Flag_Index]

#define COMPILER_WARN_ON_UNDEFINED_FUNCTION	QV[COMPILER_WARN_ON_UNDEFINED_FUNCTION_Index]
#define COMPILER_WARN_ON_DYNAMIC_RETURN QV[COMPILER_WARN_ON_DYNAMIC_RETURN_Index]
#define COMPILER_FUNCTION_NAME		QV[COMPILER_FUNCTION_NAME_Index]
#define COMPILER_USES_ENV		QV[COMPILER_USES_ENV_Index]
#define GC_TIME_COUNTER				QV[GC_TIME_COUNTER_Index]
#define PATHNAME					QV[PATHNAME_Index]
#define DEALLOCATE_C_HEAP			QV[DEALLOCATE_C_HEAP_Index]
#define	SYSTEM_INTERNALS			QV[SYSTEM_INTERNALS_Index]
#define	SAVE_IMAGE_CLEANUP_FUNCS	QV[SAVE_IMAGE_CLEANUP_FUNCS_Index]
#define	LOAD_IMAGE_RESTORE_FUNCS	QV[LOAD_IMAGE_RESTORE_FUNCS_Index]
#define CONSOLE_STREAM				QV[CONSOLE_STREAM_Index]
#define FILE_STREAM					QV[FILE_STREAM_Index]
#define STRING_STREAM				QV[STRING_STREAM_Index]
#define INPUT_KEY					QV[INPUT_KEY_Index]
#define OUTPUT_KEY					QV[OUTPUT_KEY_Index]
#define BIDIRECTIONAL_KEY			QV[BIDIRECTIONAL_KEY_Index]
#define CONSOLE_OVERFLOW_FUNCTION	QV[CONSOLE_OVERFLOW_FUNCTION_Index]
#define CONSOLE_UNDERFLOW_FUNCTION	QV[CONSOLE_UNDERFLOW_FUNCTION_Index]
#define FILE_OVERFLOW_FUNCTION		QV[FILE_OVERFLOW_FUNCTION_Index]
#define FILE_UNDERFLOW_FUNCTION		QV[FILE_UNDERFLOW_FUNCTION_Index]
#define STRING_OVERFLOW_FUNCTION	QV[STRING_OVERFLOW_FUNCTION_Index]
#define STRING_UNDERFLOW_FUNCTION	QV[STRING_UNDERFLOW_FUNCTION_Index]
#define COLLECT_LITERALS			QV[COLLECT_LITERALS_Index]
#define EXECUTION_ADDRESS			QV[EXECUTION_ADDRESS_Index]
#define FIND_FUNCTION_CURR_OFFSET	QV[FIND_FUNCTION_CURR_OFFSET_Index]
#define FIND_FUNCTION_CURR_FUNC		QV[FIND_FUNCTION_CURR_FUNC_Index]
#define FIND_FUNCTION_CURR_ADDR		QV[FIND_FUNCTION_CURR_ADDR_Index]
#define ADDRESS_FIND_FUNCTION_CALLBACK QV[ADDRESS_FIND_FUNCTION_CALLBACK_Index]
#define	TERMINAL_IO					QV[TERMINAL_IO_Index]
#define	TYPEEXPAND_ALL				QV[TYPEEXPAND_ALL_Index]
#define	LOOKUP_FTYPE				QV[LOOKUP_FTYPE_Index]
#define LOAD_QV_REG					QV[LOAD_QV_REG_Index]
#define GET							QV[GET_Index]
#define TYPE_DISCRIMINATOR			QV[TYPE_DISCRIMINATOR_Index]
#define CORMANLISP_DIRECTORY		QV[CORMANLISP_DIRECTORY_Index]
#define HEAP_CHECKING				QV[HEAP_CHECKING_Index]
#define INVALID_OBJECT_COUNT		QV[INVALID_OBJECT_COUNT_Index]
#define ERROR_TRACE					QV[ERROR_TRACE_Index]
#define TRACE_EXCEPTIONS			QV[TRACE_EXCEPTIONS_Index]
#define SHORT_SYM					QV[SHORT_SYM_Index]
#define SOURCE_FILE					QV[SOURCE_FILE_Index]
#define SOURCE_LINE					QV[SOURCE_LINE_Index]
#define SAVE_DEBUG_INFO				QV[SAVE_DEBUG_INFO_Index]
#define SOURCE_LINES				QV[SOURCE_LINES_Index]
#define CODE_JUMP_TABLE_REFS		QV[CODE_JUMP_TABLE_REFS_Index]
#define CODE_VAR_TABLE_REFS			QV[CODE_VAR_TABLE_REFS_Index]
#define CODE_ENV_TABLE_REFS			QV[CODE_ENV_TABLE_REFS_Index]
#define COMPILER_COLLECT_JUMP_TABLE_REFS  QV[COMPILER_COLLECT_JUMP_TABLE_REFS_Index]
#define COMPILER_COLLECT_VAR_TABLE_REFS	  QV[COMPILER_COLLECT_VAR_TABLE_REFS_Index]
#define PLUS_EAX_EDX				QV[PLUS_EAX_EDX_Index]
#define MINUS_EAX_EDX				QV[MINUS_EAX_EDX_Index]
#define ERROR_SIGNAL				QV[ERROR_SIGNAL_Index]
#define INTERNAL_TIME_UNITS_PER_SECOND	QV[INTERNAL_TIME_UNITS_PER_SECOND_Index]
#define GC_TIME_UNITS_PER_SECOND	QV[GC_TIME_UNITS_PER_SECOND_Index]
#define APPEND_REFS_TO_CODE		    QV[APPEND_REFS_TO_CODE_Index]
#define SINGLE_FLOAT				QV[SINGLE_FLOAT_Index]
#define DOUBLE_FLOAT				QV[DOUBLE_FLOAT_Index]
#define SHORT_FLOAT					QV[SHORT_FLOAT_Index]
#define LONG_FLOAT					QV[LONG_FLOAT_Index]
#define READ_DEFAULT_FLOAT_FORMAT	QV[READ_DEFAULT_FLOAT_FORMAT_Index]
#define SYS_GLOBALS_ADDRESS			QV[SYS_GLOBALS_ADDRESS_Index]
#define CORMANLISP_SERVER_DIRECTORY	QV[CORMANLISP_SERVER_DIRECTORY_Index]
#define CURRENT_THREAD_ID			QV[CURRENT_THREAD_ID_Index]
#define CURRENT_THREAD_HANDLE		QV[CURRENT_THREAD_HANDLE_Index]
#define CURRENT_PROCESS_ID			QV[CURRENT_PROCESS_ID_Index]
#define CURRENT_PROCESS_HANDLE		QV[CURRENT_PROCESS_HANDLE_Index]
#define EXIT_THREAD_TAG				QV[EXIT_THREAD_TAG_Index]
#define DEALLOCATE_CRITICAL_SECTION QV[DEALLOCATE_CRITICAL_SECTION_Index]
#define COMPRESS_IMG				QV[COMPRESS_IMG_Index]
#define ENV							QV[ENV_Index]
#define ARGS						QV[ARGS_Index]
#define FRAME						QV[FRAME_Index]
#define GLOBAL						QV[GLOBAL_Index]
#define COMPILED_SPECIALS			QV[COMPILED_SPECIALS_Index]
#define GET_CURRENT_ENVIRONMENT		QV[GET_CURRENT_ENVIRONMENT_Index]
#define COMPILER_FRAME_INFO			QV[COMPILER_FRAME_INFO_Index]
#define COMPILER_ENVIRONMENT		QV[COMPILER_ENVIRONMENT_Index]
#define THROW_SYSTEM_EXCEPTION		QV[THROW_SYSTEM_EXCEPTION_Index]
#define WITH_ONLY_LEXICALS			QV[WITH_ONLY_LEXICALS_Index]
#define COMPILER_WARN_ON_UNUSED_VAR QV[COMPILER_WARN_ON_UNUSED_VAR_Index]
#define WRITE						QV[WRITE_Index]
#define INLINE_PROCLAIM_P			QV[INLINE_PROCLAIM_P_Index]
#define	FINALIZATION_PENDING		QV[FINALIZATION_PENDING_Index]
#define EXECUTE_FINALIZERS			QV[EXECUTE_FINALIZERS_Index]
#define UNDEFINED_FUNCTIONS			QV[UNDEFINED_FUNCTIONS_Index]
#define GENSYM						QV[GENSYM_Index]
#define COMPILER_CHECK_ARGS_NUM		QV[COMPILER_CHECK_ARGS_NUM_Index]
#define COMPILER_CHECK_TYPES		QV[COMPILER_CHECK_TYPES_Index]
#define COMPILER_FOLD_CONSTANTS		QV[	COMPILER_FOLD_CONSTANTS_Index]
#define COMPILER_INLINE_FUNCTIONS	QV[	COMPILER_INLINE_FUNCTIONS_Index]
#define COMPILER_OPTIMIZE_TAIL_RECURSION	QV[	COMPILER_OPTIMIZE_TAIL_RECURSION_Index]
#define LOAD_LOCAL_HEAP				QV[LOAD_LOCAL_HEAP_Index]
#define COLLECT_LEXICAL_MACROS		QV[COLLECT_LEXICAL_MACROS_Index]
#define LEXICAL_MACROS				QV[LEXICAL_MACROS_Index]
#define COMPILER_LAMBDA_LIST		QV[COMPILER_LAMBDA_LIST_Index]
#define REMOVE_TAIL_RECURSION		QV[REMOVE_TAIL_RECURSION_Index]
#define LOOKUP_SETF_FUNCTION		QV[LOOKUP_SETF_FUNCTION_Index]
#define SETF						QV[SETF_Index]
#define COMPILER_CHECK_KEY_ARGS		QV[COMPILER_CHECK_KEY_ARGS_Index]
#define INVALID_KEY_ARG				QV[INVALID_KEY_ARG_Index]
#define LOCALLY						QV[LOCALLY_Index]
#define LOAD_TIME_VALUE				QV[LOAD_TIME_VALUE_Index]
#define	GET_CALLBACK				QV[GET_CALLBACK_Index]
#define HEAP_FAULT_HANDLER			QV[HEAP_FAULT_HANDLER_Index]
#define ERROR_OUTPUT				QV[ERROR_OUTPUT_Index]
#define COMPILER_WARN_ON_ASSUMED_SPECIAL QV[COMPILER_WARN_ON_ASSUMED_SPECIAL_Index]
#define COMPILING_LAMBDA			QV[COMPILING_LAMBDA_Index]
#define CAPTURED_LEXICAL_CALLBACK   QV[CAPTURED_LEXICAL_CALLBACK_Index]
#define WARN						QV[WARN_Index]
#define RESET_HASH_ID				QV[RESET_HASH_ID_Index]
#define CAPTURE_COMPILER_ENVIRONMENT QV[CAPTURE_COMPILER_ENVIRONMENT_Index]
#define LOAD_TIME_VALUES            QV[LOAD_TIME_VALUES_Index]
#define GC_CRITICAL_SECTION         QV[GC_CRITICAL_SECTION_Index]
#define TQ_CRITICAL_SECTION         QV[TQ_CRITICAL_SECTION_Index]
#define PAGE_TABLE					QV[PAGE_TABLE_Index]
#define DISASSEMBLY_OUTPUT_BUF		QV[DISASSEMBLY_OUTPUT_BUF_Index]
#define LEXICAL_SYMBOL_MACROS		QV[LEXICAL_SYMBOL_MACROS_Index]
#define COLLECT_LEXICAL_SYMBOL_MACROS QV[COLLECT_LEXICAL_SYMBOL_MACROS_Index]
#define HEAP_0_START				QV[HEAP_0_START_Index]
#define HEAP_0_END					QV[HEAP_0_END_Index]
#define HEAP_1_START				QV[HEAP_1_START_Index]
#define HEAP_1_END					QV[HEAP_1_END_Index]
#define HEAP_2_START				QV[HEAP_2_START_Index]
#define HEAP_2_END					QV[HEAP_2_END_Index]
#define HEAP_0_GC_ID				QV[HEAP_0_GC_ID_Index]
#define HEAP_1_GC_ID				QV[HEAP_1_GC_ID_Index]
#define HEAP_2_GC_ID				QV[HEAP_2_GC_ID_Index]

#define wrap(n, t)			((((LispObj)n) & ~7) | t) 
#define gettag(n)			(((LispObj)n) & 7) 
#define stripTag(n)			(((LispObj)n) & ~7)
#define immediateType(n)	((((LispObj)n) >> 3) & 0x1f)
#define wrapInteger(n)		(((LispObj)n) << 3)
#define wrapCharacter(n)	((((LispObj)n) << 8) | ImmediateTag)

LispObj createShortFloat(double);

#define isFixnum(n)			(!(n & 7))
#define isImmediate(n)		(gettag(n) == ImmediateTag)
#define isForwardingPtr(n)	(gettag(n) == ForwardTag)
#define isUvectorHeader(n)	(gettag(n) == UvectorLengthTag)
#define isCons(n)			(gettag(n) == ConsTag)
#define isUvector(n)		(gettag(n) == UvectorTag)
#define isShortFloat(n)		(((n) & 3) == 3)

#define isHeapBlock(n)	((gettag(n) == ConsTag) || (gettag(n) == UvectorTag))

// make sure high bit gets filled with sign bit during shift
#define integer(n)			(((long)(n)) >> 3)

double shortFloat(LispObj);

#define isNegativeFixnum(n)	(isFixnum(n) && (n & 0x80000000))
#define fixnumNegative(n)	(n & 0x80000000)
#define isInteger(n)		(isFixnum(n))
#define isBignum(n)			(isUvector(n) && uvectorType(n) == BignumType)
#define isLispInteger(n)	(isFixnum(n) || isBignum(n))
#define bignumNumCells(b)	(UVECTOR(b)[BIGNUM_LENGTH] >> 4)
#define bignumNegative(b)	(UVECTOR(b)[BIGNUM_LENGTH] & 8)
#define bignumSetSign(b,f)	(UVECTOR(b)[BIGNUM_LENGTH] = (UVECTOR(b)[BIGNUM_LENGTH] & ~8)|((f) << 3)) 
#define bignumNegate(b)		(UVECTOR(b)[BIGNUM_LENGTH] ^= 8) 
#define bignumStart(b)		(UVECTOR(b) + BIGNUM_FIRST_CELL) 

#define uvectorSize(n)		(UVECTOR((LispObj)n)[0] >> 8)
#define uvectorStart(n)		((UVECTOR((LispObj)n)) + 1)
#define setUvectorType(n, type) (*(UVECTOR(n)) = ((*(UVECTOR(n))) & ~0xf8) | (type << 3))	
#define uvectorType(n)		((*(UVECTOR(n)) >> 3) & 0x1f)
#define doubleFloat(n)		(*(double*)(UVECTOR(n) + DOUBLE_FLOAT_OFFSET))
#define singleFloat(n)		(*(float*) (UVECTOR(n) + SINGLE_FLOAT_OFFSET))

// array macros
#define isSimpleVector(a)	(uvectorType(a) >= SimpleVectorType && uvectorType(a) <= SimpleSingleFloatVectorType) 
#define isAdjustableArray(a)(uvectorType(a) == ArrayType) 
#define simpleVectorLength(a) (integer(UVECTOR(a)[ARRAY_SIMPLE_VECTOR_LENGTH]))
#define adjustableArrayDimensions(a) (integer(UVECTOR(a)[ARRAY_DIMENSIONS]))
#define adjustableArrayLength(a,dim) (integer(UVECTOR(a)[ARRAY_DIM1_LENGTH + dim]))
#define arrayDimensions(a)	(isAdjustableArray(a) ? adjustableArrayDimensions(a) : 1)
#define arrayDimension(a, dim)(isAdjustableArray(a) ? \
							(adjustableArrayLength(a, dim)) : 	\
							simpleVectorLength(a))
//#define arrayCellSize(a)	((UVECTOR(a)[ARRAY_DIMENSIONS] >> 3) & 0xff)
#define simpleVectorStart(a)(UVECTOR(a) + ARRAY_SIMPLE_VECTOR_START)
#define adjustableArrayVector(a) (UVECTOR(a)[ARRAY_VECTOR])
#define adjustableArrayStart(a) (simpleVectorStart(adjustableArrayVector(a)))
#define arrayStart(a)		(isAdjustableArray(a) ? adjustableArrayStart(a) : simpleVectorStart(a))
#define byteArrayStart(a)	((byte*)arrayStart(a))
#define charArrayStart(a)	((LISP_CHAR*)arrayStart(a))

//#define arrayType(a)		(UVECTOR(a)[ARRAY_TYPE])
#define vectorLength(a)		(isAdjustableArray(a) ? (UVECTOR(a)[ARRAY_DIM1_LENGTH]) : \
							(UVECTOR(a)[ARRAY_SIMPLE_VECTOR_LENGTH]))	// returns wrapped length
#define isArray(a)			(isUvector(a) && (isSimpleVector(a) || isAdjustableArray(a)))
#define isNonSimpleVector(a)(isUvector(a) && isAdjustableArray(a) && arrayDimensions(a) == 1)
#define isVector(a)			(isArray(a) && arrayDimensions(a) == 1)
#define isSimpleBitVector(a)(uvectorType(a) == SimpleBitVectorType)
#define isSimpleByteVector(a)(uvectorType(a) == SimpleByteVectorType)
#define isSimpleCharVector(a)(uvectorType(a) == SimpleCharVectorType)
#define isSimpleShortVector(a)(uvectorType(a) == SimpleShortVectorType)
#define isSimpleGenericVector(a)(uvectorType(a) == SimpleVectorType)
#define isSimpleDoubleFloatVector(a)(uvectorType(a) == SimpleDoubleFloatVectorType)
#define isSimpleSingleFloatVector(a)(uvectorType(a) == SimpleSingleFloatVectorType)
#define isGenericArray(a)	(isUvector(a) && (isSimpleGenericVector(a) || \
								(isAdjustableArray(a) && isSimpleGenericVector(adjustableArrayVector(a)))))
#define isString(a)			(isVector(a) && (isSimpleCharVector(a) || \
								(isAdjustableArray(a) && isSimpleCharVector(adjustableArrayVector(a)))))
#define isBitVector(a)		(isVector(a) && (isSimpleBitVector(a) || \
								(isAdjustableArray(a) && isSimpleBitVector(adjustableArrayVector(a)))))
#define isByteVector(a)		(isVector(a) && (isSimpleByteVector(a) || \
								(isAdjustableArray(a) && isSimpleByteVector(adjustableArrayVector(a)))))
#define isCharArray(a)		(isArray(a) && (isSimpleCharVector(a) || \
								(isAdjustableArray(a) && isSimpleCharVector(adjustableArrayVector(a)))))
#define isByteArray(a)		(isArray(a) && (isSimpleByteVector(a) || \
								(isAdjustableArray(a) && isSimpleByteVector(adjustableArrayVector(a)))))
#define isBitArray(a)		(isArray(a) && (isSimpleBitVector(a) || \
								(isAdjustableArray(a) && isSimpleBitVector(adjustableArrayVector(a)))))

#define isDoubleFloat(a)	(isUvector(a) && uvectorType(a) == DoubleFloatType)
#define isSingleFloat(a)	(isUvector(a) && uvectorType(a) == SingleFloatType)
#define isFloat(a)			(isShortFloat(a) || isSingleFloat(a) || isDoubleFloat(a))
#define isComplex(a)		(isUvector(a) && uvectorType(a) == ComplexType)
#define isRatio(a)			(isUvector(a) && uvectorType(a) == RatioType)
#define isRational(a)		(isLispInteger(a) || isRatio(a))
#define symbolName(a)		(UVECTOR(a)[SYMBOL_NAME])
#define immediate(a)		(a >> 8)
#define packageTableStart(pk) (arrayStart(UVECTOR(pk)[PACKAGE_TABLE]))
#define isFunction(func)	(isUvector(func) && uvectorType(func) <= KFunctionType)
#define isKFunction(func)	(isUvector(func) && uvectorType(func) == KFunctionType)
#define CAR(s)				(*(LispObj*)(s - ConsTag))
#define CDR(s)				(*(LispObj*)s)

#define symbolVarTableIndex(s)		(UVECTOR(s)[SYMBOL_VAR_TABLE])
#define setSymbolVarTableIndex(s,n)	(UVECTOR(s)[SYMBOL_VAR_TABLE] = (n))

#define ratioNumerator(s)	(isRatio(s) ? UVECTOR(s)[RATIO_NUMERATOR] : s)
#define ratioDenominator(s)	(isRatio(s) ? UVECTOR(s)[RATIO_DENOMINATOR] : wrapInteger(1))
#define complexReal(s)		(UVECTOR(s)[COMPLEX_REAL])
#define complexImaginary(s)	(UVECTOR(s)[COMPLEX_IMAGINARY])

#define streamName(s)				(UVECTOR(s)[STREAM_NAME])
#define streamUnderflowFunc(s)		(UVECTOR(s)[STREAM_UNDERFLOW_FUNC])
#define streamPosition(s)			(UVECTOR(s)[STREAM_POSITION])
#define streamColPosition(s)		(UVECTOR(s)[STREAM_COL_POSITION])
#define streamInputBuffer(s)		(UVECTOR(s)[STREAM_INPUT_BUFFER])
#define streamInputBufferLength(s)	(UVECTOR(s)[STREAM_INPUT_BUFFER_LENGTH])
#define streamInputBufferPos(s)		(UVECTOR(s)[STREAM_INPUT_BUFFER_POS])
#define streamInputBufferNum(s)		(UVECTOR(s)[STREAM_INPUT_BUFFER_NUM])
#define streamHandle(s)				(UVECTOR(s)[STREAM_HANDLE])
#define streamSubclass(s)			(UVECTOR(s)[STREAM_SUBCLASS])
#define streamBinary(s)				(UVECTOR(s)[STREAM_BINARY])
#define streamOpen(s)				(UVECTOR(s)[STREAM_OPEN])
#define streamDirection(s)			(UVECTOR(s)[STREAM_DIRECTION])
#define streamInteractive(s)		(UVECTOR(s)[STREAM_INTERACTIVE])
#define streamElementType(s)		(UVECTOR(s)[STREAM_ELEMENT_TYPE])
#define streamAssociatedStreams(s)	(UVECTOR(s)[STREAM_ASSOCIATED_STREAMS])
#define streamOverflowFunc(s)		(UVECTOR(s)[STREAM_OVERFLOW_FUNC])
#define streamOutputBuffer(s)		(UVECTOR(s)[STREAM_OUTPUT_BUFFER])
#define streamOutputBufferLength(s)	(UVECTOR(s)[STREAM_OUTPUT_BUFFER_LENGTH])
#define streamOutputBufferPos(s)	(UVECTOR(s)[STREAM_OUTPUT_BUFFER_POS])
#define streamLineNumber(s)			(UVECTOR(s)[STREAM_LINE_NUMBER])

#define functionEnvironment(func) (UVECTOR(func)[FUNCTION_ENVIRONMENT])
#define isList(n)			(isCons(n) || n == NIL)
#define character(n)		(n >> 8)
#define isCharacter(n)		(isImmediate(n) && (immediateType(n) == CharacterType))
#define isStream(n)			(isUvector(n) && (uvectorType(n) == StreamType))
#define isSymbol(n)			(isUvector(n) && (uvectorType(n) == SymbolType))
#define isPackage(n)		(isUvector(n) && (uvectorType(n) == PackageType))
#define isReadtable(n)		(isUvector(n) && (uvectorType(n) == ReadTableType))

#define isHashtable(n)		(isStruct(n) && isSimpleVector(structTemplate(n)) && (simpleVectorStart(structTemplate(n))[0] == HASH_TABLE))
#define isPathname(n)		(isStruct(n) && isCons(structTemplate(n)) && (CAR(structTemplate(n)) == PATHNAME))
#define isStruct(n)			(isUvector(n) && (uvectorType(n) == StructureType))
#define isCompiledCode(n)	(isUvector(n) && (uvectorType(n) == CompiledCodeType))
#define isSequence(n)		(isList(n) || isVector(n))
#define isBit(n)			((n) == 0 || (n) == wrapInteger(1))
#define isForeign(n)		(isUvector(n) && (uvectorType(n) == ForeignType))
#define isForeignHeapPtr(n)	(isUvector(n) && (uvectorType(n) == ForeignHeapType))
#define isForeignStackPtr(n)(isUvector(n) && (uvectorType(n) == ForeignStackType))
#define foreignPtr(n)		(UVECTOR(n)[FOREIGN_PTR])
#define foreignHeapSize(n)	(UVECTOR(n)[FOREIGN_HEAP_SIZE])
#define foreignHeapType(n)	(UVECTOR(n)[FOREIGN_HEAP_TYPE])
#define WeakPointerObj(n)	(UVECTOR(n)[WEAK_PTR])
#define isWeakPtr(n)		(isUvector(n) && (uvectorType(n) == WeakPointerType))
#define weakPtr(n)			(UVECTOR(n)[WEAK_PTR])

#define hasMacroBinding(sym)	(UVECTOR(sym)[SYMBOL_FUNCTION_TYPE] == MACRO)
#define hasFunctionBinding(sym) (UVECTOR(sym)[SYMBOL_FUNCTION_TYPE] == FUNCTION)

#define isSpecialOperator(sym)	(UVECTOR(sym)[SYMBOL_FUNCTION_TYPE] == SPECIAL_OPERATOR)
#define isLambdaForm(n)			(isCons(n) && CAR(n) == LAMBDA)
#define symbolFunction(s)		(CAR(UVECTOR(s)[SYMBOL_FUNCTION]))
#define isSpecialSymbol(s)		(UVECTOR(s)[SYMBOL_FLAGS] & SYMBOL_SPECIAL_FLAG)
#define isConstantSymbol(s)		(UVECTOR(s)[SYMBOL_FLAGS] & SYMBOL_CONSTANT_FLAG)
#define setSpecialSymbol(s)		(UVECTOR(s)[SYMBOL_FLAGS] |= SYMBOL_SPECIAL_FLAG)
#define setConstantSymbol(s)	(UVECTOR(s)[SYMBOL_FLAGS] |= SYMBOL_CONSTANT_FLAG)

#define arrayFillPointer(a)		(UVECTOR(a)[ARRAY_FILL_POINTER])
#define arrayHasFillPointer(a)	(isAdjustableArray(a) && (!(arrayFillPointer(a) & 0x80000000)))
#define structTemplate(s)		(UVECTOR(s)[STRUCT_TEMPLATE])

#define FixnumMax				((1 << 28) - 1)
#define FixnumMin				(-(FixnumMax) - 1)

#define CheckNumArgs(num)			\
__asm								\
{									\
	__asm cmp ecx, num				\
	__asm jz numargs_next			\
	__asm call WrongNumberOfArgs	\
	__asm numargs_next:				\
}

#define CheckNumArgsRange(low, high) \
__asm								\
{									\
	__asm cmp ecx, low				\
	__asm jge numargs_next1			\
	__asm call WrongNumberOfArgs	\
	__asm numargs_next1:			\
	__asm cmp ecx, high				\
	__asm jle numargs_next2			\
	__asm call WrongNumberOfArgs	\
	__asm numargs_next2:			\
}

#define GetArgCount(var)			\
	__asm mov var, ecx

#define ReturnCount(num)			\
	__asm mov ecx, num

//
//	This macro causes _arg_count to be set to the number of
//  passed arguments, and _arg_ptr to be loaded with the address of 
//  argument #1.

#define LISP_FUNC_BEGIN(numargs)				\
	LispObj ret = 0;							\
	long _arg_count;							\
	LispObj* _arg_ptr;							\
	__asm										\
	{											\
		__asm	lea   eax, _args				\
		__asm	mov   dword ptr _arg_ptr,eax	\
		__asm	mov   _arg_count, ecx			\
		__asm	lea   eax, [eax+ecx*4-4]		\
		__asm	mov   dword ptr [_arg_ptr],eax	\
	}											\
	CheckNumArgs(numargs);

#define LISP_FUNC_BEGIN_VARIABLE(minargs, maxargs)	\
	LispObj ret = 0;						\
	long _arg_count;						\
	LispObj* _arg_ptr = &_args;				\
	__asm									\
	{										\
		__asm mov dword ptr _arg_count, ecx	\
	}										\
	_arg_ptr += (_arg_count - 1);			\
	CheckNumArgsRange(minargs, maxargs);

#define LISP_FUNC_RETURN(val)				\
	ret = (val);							\
	__asm									\
	{										\
		__asm mov ecx, 1					\
	}										\
	return ret;

#define LISP_FUNC_RETURN_NO_VALUES()		\
	ret = NIL;								\
	__asm									\
	{										\
		__asm mov ecx, 0					\
	}										\
	return ret;

#define StackMarkersMax ((NUM_STACK_MARKERS * 4) / 2)

// push foreign stack context
#define LISP_TO_FOREIGN()														\
	__asm																		\
	{																			\
		__asm		std															\
        __asm		mov		eax, [esi + (STACK_MARKER_INDEX_Index * 4)]			\
		__asm		cmp		eax, StackMarkersMax								\
		__asm		jge		short err1											\
        __asm		test    eax, 4												\
        __asm		jz      short t11000										\
		__asm	err1:															\
        __asm		cld															\
		__asm		mov		ecx, 0												\
		__asm		call	Memory_Report										\
		__asm	t11000:															\
		__asm		push	eax													\
        __asm		mov		[esi + eax*2 + (STACK_MARKERS_Index * 4)], esp		\
        __asm		lea		edi, [esi + eax*2 + ((STACK_MARKERS_Index + 1) * 4)]\
		__asm		pop		eax													\
        __asm		push    0													\
        __asm		pop		dword ptr [esi + eax*2 + ((STACK_MARKERS_Index + 1) * 4)]	\
		__asm		add		eax, 4												\
        __asm		mov		[esi + (STACK_MARKER_INDEX_Index * 4)], eax			\
        __asm		cld															\
	}

// push lisp stack context
#define FOREIGN_TO_LISP()														\
	__asm																		\
	{																			\
        __asm		push	esi													\
        __asm		call	ThreadQV											\
        __asm		mov		esi, eax											\
		__asm		std															\
        __asm		mov		eax, [esi + (STACK_MARKER_INDEX_Index * 4)]			\
		__asm		cmp		eax, StackMarkersMax								\
		__asm		jge		short err2											\
        __asm		test    eax, 4												\
        __asm		jnz      short t11001										\
		__asm	err2:															\
        __asm		cld															\
		__asm		mov		ecx, 0												\
		__asm		call	Memory_Report										\
		__asm	t11001:															\
        __asm		mov		[esi + eax*2 + (STACK_MARKERS_Index * 4)], esp		\
        __asm		push    0													\
        __asm		pop		dword ptr [esi + eax*2 + ((STACK_MARKERS_Index + 1) * 4)]	\
		__asm		add		eax, 4												\
        __asm		mov		[esi + (STACK_MARKER_INDEX_Index * 4)], eax			\
        __asm		cld															\
        __asm		pop		esi													\
	}

// pop foreign stack context
#define FOREIGN_RETURN_TO_LISP()											\
	__asm																	\
	{																		\
        __asm		push	esi												\
        __asm		call	ThreadQV										\
        __asm		mov		esi, eax										\
        __asm		std														\
        __asm		mov		edx, [esi + (STACK_MARKER_INDEX_Index * 4)]		\
		__asm		cmp		edx, 0											\
		__asm		jle		short err3										\
        __asm		test    edx, 4											\
        __asm		jnz      short t11002									\
		__asm	err3:														\
        __asm		cld														\
		__asm		mov		ecx, 0											\
		__asm		call	Memory_Report									\
		__asm	t11002:														\
		__asm		sub		edx, 4											\
        __asm		mov		[esi + (STACK_MARKER_INDEX_Index * 4)], edx		\
        __asm		push    0												\
   		__asm		pop		dword ptr [esi + edx*2 + (STACK_MARKERS_Index * 4)] \
        __asm		push    0												\
        __asm		pop		dword ptr [esi + edx*2 + ((STACK_MARKERS_Index + 1) * 4)] \
        __asm		cld														\
        __asm		pop		esi												\
	}

// pop lisp stack context
#define LISP_RETURN_TO_FOREIGN()											\
	__asm																	\
	{																		\
        __asm		push	esi												\
        __asm		call	ThreadQV										\
        __asm		mov		esi, eax										\
        __asm		std														\
        __asm		mov		edx, [esi + (STACK_MARKER_INDEX_Index * 4)]		\
		__asm		cmp		edx, 0											\
		__asm		jle		short err4										\
        __asm		test    edx, 4											\
        __asm		jz      short t11003									\
		__asm	err4:														\
        __asm		cld														\
		__asm		mov		ecx, 0											\
		__asm		call	Memory_Report									\
		__asm	t11003:														\
		__asm		sub		edx, 4											\
        __asm		mov		[esi + (STACK_MARKER_INDEX_Index * 4)], edx		\
        __asm		push    0												\
   		__asm		pop		dword ptr [esi + edx*2 + (STACK_MARKERS_Index * 4)] \
        __asm		push    0												\
        __asm		pop		dword ptr [esi + edx*2 + ((STACK_MARKERS_Index + 1) * 4)] \
        __asm		cld														\
        __asm		pop		esi												\
	}


#define LISP_ARG(n) (_arg_ptr[-(n)])
#define ARG_COUNT	(_arg_count)

#define CATCH_HEADER_CODE	wrapInteger(1)

#define JumpBufferSize  36
#define JumpBufferMarker 0xf9f9f9f9

typedef unsigned int ULONG32;

extern LispThreadQueue ThreadList;
extern unsigned long NumLispThreads;
extern CriticalSection GCCriticalSection;
extern PLEvent InitializationEvent;

extern ULONG32 SysGlobalsAddr;
extern LispObj** GlobalQVPointer;
extern unsigned char** GlobalForeignJumpTable;
extern long* GlobalForeignJumpTableNumEntries;
extern long* GlobalForeignJumpTableCapacity;

extern BOOL inEphemeralHeap1AddressRange(unsigned long addr);
extern BOOL inEphemeralHeap2AddressRange(unsigned long addr);
extern BOOL inLispHeap1AddressRange(unsigned long addr);
extern BOOL inLispHeap2AddressRange(unsigned long addr);
extern unsigned long tranlateToOtherPrimaryHeap(unsigned long addr);
extern unsigned long looksLikeEBP(unsigned long addr);
extern const char* DumpFileName;

extern bool inAnyLispHeap(DWORD addr);

// Use these instead of malloc()/free()
extern void* CAlloc(unsigned long size);
extern void* CRealloc(void* p, unsigned long size);
extern void CFree(void*);

#define SizeOfForeignJumpTableEntry 16
extern char CormanLispServerDirectory[MAX_PATH+1];
extern unsigned long garbageCollectionID;
extern BOOL IsBadMemPtr(BOOL write, void *ptr, size_t size);
extern void WriteMemoryReportTask(void* address, CONTEXT* context);
extern void LispLoop();
extern int lispmain();
void updateJumpTable(LispObj sym, LispObj func, LispObj env);

struct PageTableEntry
{
    byte flags;
    byte unused;
    short taggedOffset;
};
extern PageTableEntry* PageTable;

struct FunctEntry
{
	char* functName;
	LispFunc functAddr;
	long isKernel;
};
extern FunctEntry functTable[];
extern long sizeFunctTable;

class LispHeap
{
public:
    LispHeap();
    ~LispHeap();
    void alloc(long unsigned size, unsigned long overflow,
        unsigned long generation, unsigned long reserveRequested);
    BOOL inHeap(byte* addr);
    void reset();

    void writeProtectAllPages();
    void unWriteProtectAllPages();
    int grow(long bytesToGrow);
    int commitAllPages();
    int decommitAllPages();
    int commitTrailingPages(int readonly);
    int decommitTrailingPages();
    int commitPage(byte* addr, int readonly);
    int pageCommitted(byte* addr);

    Node* start;
    Node* end;
    Node* current;
    Node* overflow;
    byte* mem;
    long generation;
    long sizeMem;				// number of bytes in the allocated memory segment
    ULONG32 firstPage;			// page_id of first page
    ULONG32 firstUncommittedPage;// page_id of first uncommitted page (or 0)
    ULONG32 numPages;			// number of 4k-byte pages
    ULONG32 reserve;			// total reserved space
    ULONG32 numReservedPages;	// number of 4k-byte reserved pages
    friend void swapLispHeaps();
};

// GC-related variables
extern LispHeap EphemeralHeap1;
extern LispHeap EphemeralHeap2;
extern LispHeap LispHeap1;
extern LispHeap LispHeap2;
extern LispHeap SysGlobals;

extern int EphemeralHeap1SizeMin; 
extern int EphemeralHeap1Size;    
extern int EphemeralHeap1SizeMax; 
extern int EphemeralHeap2SizeMin; 
extern int EphemeralHeap2Size;
extern int EphemeralHeap2SizeMax; 
extern int LispHeapSizeMin;       
extern int LispHeapSize;		     
extern int LispHeapSizeMax;       
extern int SysGlobalsSize;	     
extern int LispHeapReserveMin;
extern int LispHeapReserveDefault;
extern int LispHeapReserveMax;   

extern int HardwareAssist;   // 0 if hardware-assisted GC is off, 1 if it is on

extern char gDisassemblyOutputBuf[];

#define EVEN(n) (!((n) & 1))

#endif // LISP_H