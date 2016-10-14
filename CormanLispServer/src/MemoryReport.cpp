//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		MemoryReport.cpp
//		Contents:	Process (stack/memory) report for Corman Lisp.
//                  Used to diagnose memory violations and other issues.
//		History:	4/29/06  RGC  Created.
//
#include "stdafx.h"
#include <stdio.h>
#include <process.h>
#include <time.h>

#include "Lisp.h"
#include "CormanLispServer.h"

static LispObj* stackStart = 0;
static LispObj* stackEnd = 0;

static void writeSymbolName(FILE* file, LispObj sym)
{
    LispObj temp = 0;
    unsigned short* ch = 0;
    int numChars = 0;

    temp = symbolName(sym);
    if (isForwardingPtr(temp))
        temp = wrap(temp, UvectorTag);  // if called during GC, watch for this case
    ch = charArrayStart(temp);
    numChars = integer(vectorLength(temp));
    if (IsBadReadPtr(ch, numChars * 2))
        return;
    for (int i = 0; i < numChars; i++)
        fprintf(file, "%c", ch[i]);
}

static void writeSimpleCharVector(FILE* file, LispObj str)
{
    unsigned short* ch = 0;
    int numChars = 0;
    fprintf(file, "\"");
    ch = charArrayStart(str);
    numChars = integer(vectorLength(str));
    if (IsBadReadPtr(ch, numChars * 2))
        return;
    for (int i = 0; i < numChars; i++)
        fprintf(file, "%c", ch[i]);
    fprintf(file, "\"");
}

//
// Returns true if the passed simple char vector appears valid--false otherwise.
static bool checkSimpleCharVector(LispObj str)
{
    DWORD elements = uvectorStart(str)[0];
    // round up to multiple of 4
    elements = (elements + 3)/4;
    int expectedSize = 1 + (elements / 4);
    if (uvectorSize(str) != expectedSize)
        return false;
    unsigned short* w = charArrayStart(str);
    for (int i = 0; i < expectedSize; i++)
        if (w[i] & 0xff00)
            return false;
    return true;
}

static void writeHeapInfo(LispHeap* heap, FILE* file, char* name)
{
    fprintf(file, "Heap: %s  Generation: %x\n", name, heap->generation);
    fprintf(file, "  Start: %08.8x", heap->start);
    fprintf(file, "  End: %08.8x", heap->end);
    fprintf(file, "  Current: %08.8x", heap->current);
    fprintf(file, "  Overflow: %08.8x", heap->overflow);
    fprintf(file, "\n\n");
}

static bool pointsIntoAnyLispHeap(void* addr)
{
    return EphemeralHeap1.inHeap((byte*)addr) ||
           EphemeralHeap2.inHeap((byte*)addr) ||
           LispHeap1.inHeap((byte*)addr) ||
           LispHeap2.inHeap((byte*)addr);
}

static void outputUvector(FILE* file, LispObj uvector)
{
    int length = 0;
    if (!pointsIntoAnyLispHeap((void*)uvector))
    {
        fprintf(file, "{Warning: Unknown or untagged value: %08.8x}", uvector);
        return;
    }

    if (IsBadReadPtr((void*)stripTag(uvector), 8))
    {
        fprintf(file, "{Error: Apparent heap object at address %08.8x is not accessible}", stripTag(uvector));
        return;
    }
    length = uvectorSize(uvector) * 8;
    if (IsBadReadPtr((void*)stripTag(uvector), length))
    {
        fprintf(file, "{Error: Apparent heap object at address %08.8x is not accessible}", stripTag(uvector));
        return;
    }

    int type = uvectorType(uvector);
    switch (type)
    {
        case FunctionType:
            if (functionEnvironment(uvector) == NIL)
                fprintf(file, "{Function: Environment = NIL");
            else
                fprintf(file, "{Function: Environment = %08.8x", functionEnvironment(uvector));
            fprintf(file, ", Code Buffer = %08.8x}", UVECTOR(uvector) + FUNCTION_ADDRESS);
            break;
        case KFunctionType:	
            if (functionEnvironment(uvector) == NIL)
                fprintf(file, "{Kernel Function: Environment = NIL");
            else
                fprintf(file, "{Kernel Function: Environment = %08.8x", functionEnvironment(uvector));
            fprintf(file, ", C Function Pointer = %08.8x}", UVECTOR(uvector) + FUNCTION_ADDRESS);
            break;
        case StructureType:	
            fprintf(file, "{Structure}");
            break;
        case ArrayType:		
            fprintf(file, "{Array}");
            break;
        case SymbolType:
            fprintf(file, "{Symbol: ");
            writeSymbolName(file, uvector);
            fprintf(file, "}");
            break;
        case StreamType:		
            fprintf(file, "{Stream}");
            break;
        case DoubleFloatType:
            fprintf(file, "{Double Float}");
            break;
        case PackageType:	
            fprintf(file, "{Package}");
            break;
        case HashtableType:	
            fprintf(file, "{Hashtable}");
            break;
        case ForeignType:	
            fprintf(file, "{Foreign Object}");
            break;
        case CompiledCodeType:	
            fprintf(file, "{Compiled Code}");
            break;
        case ReadTableType:
            fprintf(file, "{Readtable}");
            break;
        case ComplexType:		
            fprintf(file, "{Complex}");
            break;
        case RatioType:			
            fprintf(file, "{Ratio}");
            break;
        case BignumType:			
            fprintf(file, "{Bignum}");
            break;
        case ForeignHeapType:	
            fprintf(file, "{Foreign Heap Object}");
            break;
        case WeakPointerType:	
            fprintf(file, "{Weak Pointer}");
            break;
        case SimpleVectorType:	
            fprintf(file, "{Simple Vector}");
            break;
        case SimpleCharVectorType:		
            fprintf(file, "{Simple Character Vector: ");
            if (checkSimpleCharVector(uvector))
                writeSimpleCharVector(file, uvector);
            else
                fprintf(file, "<invalid simple char vector>");
            fprintf(file, "}");
            break;
        case SimpleByteVectorType:		
            fprintf(file, "{Simple Byte Vector}");
            break;
        case SimpleShortVectorType:	
            fprintf(file, "{Simple Short Vector}");
            break;
        case SimpleDoubleFloatVectorType:
            fprintf(file, "{Simple Double Float Vector}");
            break;
        case SimpleBitVectorType:	
            fprintf(file, "{Simple Bit Vector}");
            break;
        case SimpleSingleFloatVectorType:
            fprintf(file, "{Simple Single Float Vector}");
            break;
        case SingleFloatType:
            fprintf(file, "{Single Float}");
            break;
        case CLOSInstanceType:
            fprintf(file, "{CLOS Instance}");
            break;
        case ForeignStackType:
            fprintf(file, "{Foreign Stack Marker}");
            break;
        case ForeignStackEndType:
            fprintf(file, "{Foreign Stack End Marker}");
            break;
    }
}

char* getHeapName(LispObj obj)
{
    if (EphemeralHeap1.inHeap((byte*)obj))
    {
        return "EphemeralHeap1";
    }
    else if (EphemeralHeap2.inHeap((byte*)obj))
    {
        return "EphemeralHeap2";
    }
    else if (LispHeap1.inHeap((byte*)obj))
    {
        return "LispHeap1";
    }
    else if (LispHeap2.inHeap((byte*)obj))
    {
        return "LispHeap2";
    }
    else
    {
        return "Unknown heap";
    }
}

void doIndent(FILE* file, int indent)
{
    for (int index = 0; index < indent; index++)
        fprintf(file, " ");
}

static void outputReferencedObject(FILE* file, LispObj obj, int indent)
{
    int tag = obj & 7;
    bool heapObj = isHeapBlock(obj);

    doIndent(file, indent);
    
    if (isFixnum(obj))
	{
		if (obj != 0 && (void*)obj <= stackStart && (void*)obj >= stackEnd)
			fprintf(file, "{Warning: Integer %x or possible stack reference: %08.8x}", integer(obj), obj);		// may indicate a reference into the stack
		else
			fprintf(file, "{Integer: %x}", integer(obj));
	}
    else if (isCons(obj))
    {
        if (obj < 0x10000)
        {
            fprintf(file, "{Untagged value: %08.8x}", obj);
            return;
        }
        if (!pointsIntoAnyLispHeap((void*)obj))
        {
			if (obj != 0 && (void*)obj <= stackStart && (void*)obj >= stackEnd)
				fprintf(file, "{Warning: Stack reference: %08.8x}", obj);		// indicates a reference into the stack
			else
				fprintf(file, "{Warning: Unknown or untagged value: %08.8x}", obj);
            return;
        }

        if (IsBadReadPtr((void*)stripTag(obj), 8))
            fprintf(file, "{Error: Apparent heap object at address %08.8x is not accessible}", stripTag(obj));
        else
        {
            fprintf(file, "{Cons (in %s): CAR = %08.8x, CDR = %08.8x}", 
                getHeapName(obj), CAR(obj), CDR(obj));
        }
    }
    else if (obj == JumpBufferMarker)
    {
        fprintf(file, "{JumpBufferMarker: %08.8x}", obj);
    }
    else if (isShortFloat(obj))
    {
        fprintf(file, "{Short Float: %g}", shortFloat(obj));
    }
    else if (isForwardingPtr(obj))
    {
        if (obj < 0x10000)
        {
            fprintf(file, "{Untagged value: %08.8x}", obj);
            return;
        }

        fprintf(file, "{Forwarding Pointer}");
    }
    else if (isUvectorHeader(obj))
    {
        if (obj < 0x10000)
        {
            fprintf(file, "{Untagged value: %08.8x}", obj);
            return;
        }
        fprintf(file, "{Uvector Header: Uvector length = %08.8x bytes}", (obj >> 8) * 8);
    }
    else if (isUvector(obj))
    {
        outputUvector(file, obj);
    }
    else
    {
        if (obj < 0x10000)
        {
            fprintf(file, "{Untagged value: %08.8x}", obj);
            return;
        }
        fprintf(file, "{UNKNOWN OBJECT: %08.8x}", obj);
    }
}

static char timeBuffer[32];
static struct tm newtime;
static __time32_t aclock;

static char* getTime()
{
    errno_t errNum;
    _time32(&aclock);   // Get time in seconds.
    _localtime32_s(&newtime, &aclock);   // Convert time to struct tm form.

    // Print local time as a string.

    errNum = asctime_s(timeBuffer, 32, &newtime);
    return timeBuffer;
}

void writeThreadRegistersReport(FILE* file, ThreadRecord* th)
{
    static CONTEXT lispContext;
    lispContext.ContextFlags = CONTEXT_CONTROL|CONTEXT_INTEGER;
    GetThreadContext(th->thread, &lispContext);

    fprintf(file, "Thread ID %08.8x Registers:\n", th->threadID);

    fprintf(file, "  EAX: %08.8x\n", lispContext.Eax); 
    outputReferencedObject(file, lispContext.Eax, 4);
    fprintf(file, "\n");

    fprintf(file, "  EBX: %08.8x\n", lispContext.Ebx); 
    outputReferencedObject(file, lispContext.Ebx, 4);
    fprintf(file, "\n");

    fprintf(file, "  ECX: %08.8x\n", lispContext.Ecx); 
    outputReferencedObject(file, lispContext.Ecx, 4);
    fprintf(file, "\n");

    fprintf(file, "  EDX: %08.8x\n", lispContext.Edx); 
    outputReferencedObject(file, lispContext.Edx, 4);
    fprintf(file, "\n");

    fprintf(file, "  ESI: %08.8x\n", lispContext.Esi); 
    outputReferencedObject(file, lispContext.Esi, 4);
    fprintf(file, "\n");

    fprintf(file, "  EDI: %08.8x\n", lispContext.Edi); 
    outputReferencedObject(file, lispContext.Edi, 4);
    fprintf(file, "\n");

    fprintf(file, "  ESP: %08.8x\n", lispContext.Esp); 
    fprintf(file, "  EBP: %08.8x\n", lispContext.Ebp); 
    fprintf(file, "  EIP: %08.8x\n", lispContext.Eip); 
    fprintf(file, "\n");
}

// on the flags, 0 = normal cell, 1 = ebp, 2 = return address
struct StackCell { DWORD value; DWORD returnAddress; DWORD ebp; DWORD flags; };

void processStack(StackCell* stack, DWORD stackLength, ThreadRecord* th)
{
    static CONTEXT lispContext;
    LispObj* start = th->stackStart;
    unsigned long* basePointer = 0;
    unsigned long* esp = 0;
    LispObj runtimeInfo = 0;
    LispObj f = 0;
    LispObj* qv = th->QV_rec;
    int index = 0;
    long inForeignCode = 0;
    DWORD retAddr = 0;
    DWORD ebp = 0;
    LispObj* end = 0;

    // we skip portions of the stack which are not tagged
    // to do this we look at every other segment
    // Every even segment should be a lisp (tagged) segment,
    index = qv[STACK_MARKER_INDEX_Index] >> 2;

    lispContext.ContextFlags = CONTEXT_CONTROL|CONTEXT_INTEGER;
    GetThreadContext(th->thread, &lispContext);
    basePointer = (unsigned long*)lispContext.Ebp;
    esp = (unsigned long*)lispContext.Esp;
    end = (LispObj*)lispContext.Esp;

    while (index > 0 &&
            ((unsigned long)esp) > qv[STACK_MARKERS_Index + ((index - 1) * 2)])
        index--;
    if (!EVEN(index))		// if not a lisp segment
    {
        inForeignCode = 1;

        // we are into a non-lisp segment of the stack.
        // Find	the next stack frame in a lisp segment
        index--;
        esp	= (unsigned long*)qv[STACK_MARKERS_Index + (index * 2)];
        if (esp == th->stackStart)
            basePointer = start;
        else
        {
            // search for the first index into the stack which points
            // into the next 1k region of the stack. Assume this is the
            // next base pointer.
            basePointer = esp;
            while (!(looksLikeEBP((unsigned long)basePointer)))
                basePointer++;
        }
    }

    while (basePointer < start)
    {
        retAddr = basePointer[1];
        ebp = basePointer[0];
        stack[basePointer - end].ebp = ebp;
        stack[basePointer - end + 1].returnAddress = retAddr;

        basePointer = (unsigned long*)basePointer[0];
        
        while (index > 0 &&
                ((unsigned long)basePointer) >= qv[STACK_MARKERS_Index + ((index - 1) * 2)])
            index--;
        if (!EVEN(index))		// if not a lisp segment
        {
            // we are into a non-lisp segment of the stack.
            // Find	the next stack frame in a lisp segment
            index--;
            esp	= (unsigned long*)qv[STACK_MARKERS_Index + (index * 2)];
            if (esp == th->stackStart)
                basePointer = start;
            else
            {
                // search for the first index into the stack which points
                // into the next 1k region of the stack. Assume this is the
                // next base pointer.
                basePointer = esp;
                while (!(looksLikeEBP((unsigned long)basePointer)))
                    basePointer++;
            }
        }

    }

    assert(basePointer == start);
}

static LispObj funcCandidate = 0;
static DWORD offsetCandidate = 0xfffffff;
static DWORD addrCandidate = 0;
static DWORD funcBaseAddrCandidate = 0;

typedef void (*HeapIterateFunc)(LispObj obj);
extern void* nextHeapObject(void* addr);

void doHeapBlock(LispHeap* heap, HeapIterateFunc func)
{
    long ulength = 0;
    LispObj uvec = 0;
    Node* p = 0;

    // check each heap
    p = heap->start;
    while (p < heap->current)
    {
        if (gettag(p->car) == UvectorLengthTag)	// we have a uvector
        {
            ulength = ((p->car) >> 8) * 2;
			if (ulength == 0)
			{
				// Something is messed up--a uvector should never have size 0.
				// For this function, we will just try to advance to the next valid
				// heap block on the next page--other parts of the system would abort
				// the entire lisp process if this case is detected. We won't do that because
				// this function is used to generate a Memory Report (dump) in cases where 
				// a problem is detected and it wouldn't be useful to abort that.
				//
				p = (Node*)nextHeapObject(p);
				continue;
			}
            uvec = wrap((LispObj)p, UvectorTag);
            func(uvec);
            p += (ulength / 2);
        }
        else		// we have a cons cell
        {
            uvec = wrap((LispObj)p, ConsTag);
            func(uvec);
            p++;
        }
    }
}

void doEachHeapBlock(HeapIterateFunc func)
{
    doHeapBlock(&EphemeralHeap1, func);
    doHeapBlock(&EphemeralHeap2, func);
    doHeapBlock(&LispHeap1, func);
}

void findFunctionCallback(LispObj p)
{
    unsigned long execaddr = 0;
    if (isFunction(p))
    {
        execaddr = (unsigned long)functionAddress(p);
        if (execaddr)
        {
            if (IsBadReadPtr((void*)execaddr, 4))
                return;
            if (*(byte*)execaddr == 0xe9)
                execaddr = (((unsigned long)execaddr) + *(long*)(((byte*)execaddr) + 1) + 5);
        }

        // assume function smaller than 64k in size
        if (execaddr <= addrCandidate && (addrCandidate - execaddr) < 0x10000)	
        {
            if ((addrCandidate - execaddr) < offsetCandidate)
            {
                offsetCandidate = addrCandidate - execaddr;
                funcCandidate = p;
                funcBaseAddrCandidate = execaddr;
            }
        }
    }
}

static char FunctionNameBuffer[256] = {0};

//
// Given a pointer into a function code block (usually a return address,
// returns the name of the function (if it can be determined, and
// stores the base address of the function in funcBase.
//
char* lookupFunctionName(DWORD addr, DWORD* funcBase, char* buffer, int bufLength)
{
    funcCandidate = 0;
    offsetCandidate = 0xfffffff;
    addrCandidate = 0;
    funcBaseAddrCandidate = 0;
    char* name = buffer;
    addrCandidate = addr;
    doEachHeapBlock(findFunctionCallback);
    *funcBase = funcBaseAddrCandidate;
    LispObj func = funcCandidate;
    LispObj funcName = 0;
    LispObj funcLambda = 0;
    LispObj funcInfo = 0;
    LispObj p = 0;
    FunctEntry* funcEntry = 0;
    name[0] = 0;

    if (isFunction(func))
    {
        if (isKFunction(func))
        {
            funcEntry = functTable;
            for (int i = 0; i < sizeFunctTable; i++)
            {
                if (((LispObj)(funcEntry->functAddr)) == *funcBase)
                {
                    strcat_s(name, bufLength, funcEntry->functName);
                    break;
                }
                funcEntry++;
            }
        }
        else
        {
            funcInfo = UVECTOR(UVECTOR(func)[FUNCTION_ADDRESS])[COMPILED_CODE_PROPERTIES];
            funcLambda = NIL;
            p = funcInfo;
            while (isCons(p))
            {
                if (CAR(p) == FUNCTION_NAME)
                    funcName = CAR(CDR(p));
                else if (CAR(p) == LAMBDA_LIST)
                    funcLambda = CAR(CDR(p));
                p = CDR(CDR(p));
            }
            if (funcName != NIL && funcName != 0    )
            {
                if (isSymbol(funcName))
                    funcName = symbolName(funcName);
                int length = integer(vectorLength(funcName));
                if (length > 255)
                    length = 255;
                for (int i = 0; i < length; i++)
                    name[i] = (char)(unsigned char)(charArrayStart(funcName)[i]);
                name[length] = 0;
            }
        }
    }
    return name;
}

void writeThreadSummary(FILE* file, ThreadRecord* th)
{
    StackCell* stack = 0;	// copy of the stack
    DWORD stackLength = 0;
    static CONTEXT lispContext;
    lispContext.ContextFlags = CONTEXT_CONTROL|CONTEXT_INTEGER;
    GetThreadContext(th->thread, &lispContext);
    char* msg;
    if (th->stackStart == 0)
        msg = "Not started";
    unsigned long* basePointer = (unsigned long*)lispContext.Ebp;
    LispObj* start = th->stackStart;
    LispObj* end = (LispObj*)lispContext.Esp;
    LispObj* qv = th->QV_rec;

    // make a copy of the stack
    stackLength = start - end + 1;		// stack start > end
    stack = (StackCell*)HeapAlloc(GetProcessHeap(), 0, stackLength * sizeof(StackCell));
    for (unsigned int i = 0; i < stackLength; i++)
    {
        stack[i].value = *(end + i);
        stack[i].returnAddress = 0;
        stack[i].ebp = 0;
        stack[i].flags = 0;
    }

    // now figure out where the code return addresses and BP links are
    processStack(stack, stackLength, th);

    // we skip portions of the stack which are not tagged
    // to do this we look at every other segment
    // Every even segment should be a lisp (tagged) segment,
    int index = qv[STACK_MARKER_INDEX_Index] >> 2;
    int i = 0;
    char* frameName = 0;
    char* executionContext = 0;
    LispObj* x = 0;

    for (x = start; x >= end; x--)
    {
        if (stack[x - end].ebp != 0)
        {
            continue;
        }
        else if (stack[x - end].returnAddress != 0)
        {
            DWORD addr = stack[x - end].value;
            DWORD funcBase = 0;
            char* name = 0;

            // try to find the name of the frame
            DWORD index = x - end - 1;
            while (index > 0)
            {
                if (stack[index].returnAddress != 0)
                {
                    frameName = lookupFunctionName(stack[index].value, &funcBase, FunctionNameBuffer, sizeof(FunctionNameBuffer));
                    if (!frameName || frameName[0] == 0)
                        frameName = "<Unknown>";
                    break;
                }
                index--;
            }
            continue;
        }

        if (EVEN(i))
        {
            if (inAnyLispHeap(*x) && *(unsigned long*)x == JumpBufferMarker)
            {
                x -= ((JumpBufferSize / 4) - 1);
            }
            else // if a foreign stack block, then skip it
            if (*(unsigned char*)x == ForeignStackEndMarker
                && ((*x >> 8) <= 0x2000)        // allow up to 64k foreign stack block
                && (x - (2 * (*x >> 8))) > end
                    && *(unsigned char*)(x - (2 * (*x >> 8))) == ForeignStackStartMarker)
            {
                // foreign stack block
                x -= ((2 * (*x >> 8)) - 1);
            }
        }
        if ((i < index) && ((unsigned long)x) <= qv[(i * 2) + STACK_MARKERS_Index])
            i++;
    }
    // only update registers if we are running in lisp (not foreign) code
    // We determine this based on the region of the top of the stack.
    if (EVEN(i))
        executionContext = "Lisp";
    else
        executionContext = "Foreign";
    if (stack)
        HeapFree(GetProcessHeap(), 0, stack);

    if (!frameName)
        frameName = "<Unknown>";
    fprintf(file, "Thread ID %08.8x  Executing function: %s  Execution context: %s", 
        th->threadID, frameName, executionContext);
}

void writeThreadStackReport(FILE* file, ThreadRecord* th)
{
    static CONTEXT lispContext;
    LispObj* start = th->stackStart;
    LispObj* x = 0;
    StackCell* stack = 0;	// copy of the stack
    DWORD stackLength = 0;

    if (start == 0)
    {
        fprintf(file, "Thread ID %08.8x has not been started--no stack trace will be output\n", th->threadID);
        return;			// watch for the case where a thread has not actually started
    }
    int i = 0;
    lispContext.ContextFlags = CONTEXT_CONTROL|CONTEXT_INTEGER;
    GetThreadContext(th->thread, &lispContext);
    LispObj* end = (LispObj*)lispContext.Esp;
    LispObj* qv = th->QV_rec;
    int index = 0;

    fprintf(file, "Thread ID %08.8x Stack:\n", th->threadID);
    fprintf(file, "  Stack start (base) address: %08.8x\n", start);
    fprintf(file, "  Stack end (top of stack) address: %08.8x\n", end);
    fprintf(file, "\n");

    // make a copy of the stack
    stackLength = start - end + 1;		// stack start > end
    stack = (StackCell*)HeapAlloc(GetProcessHeap(), 0, stackLength * sizeof(StackCell));
    for (unsigned int i = 0; i < stackLength; i++)
    {
        stack[i].value = *(end + i);
        stack[i].returnAddress = 0;
        stack[i].ebp = 0;
        stack[i].flags = 0;
    }

    // now figure out where the code return addresses and BP links are
    processStack(stack, stackLength, th);

    // we skip portions of the stack which are not tagged
    // to do this we look at every other segment
    // Every even segment should be a lisp (tagged) segment,
    index = qv[STACK_MARKER_INDEX_Index] >> 2;
    i = 0;
	stackStart = start;
	stackEnd = end;

    for (x = start; x >= end; x--)
    {
        if (stack[x - end].ebp != 0)
        {
            fprintf(file, "    %08.8x: %08.8x  ", x, *x);
            fprintf(file, " {EBP Link: %08.8x}\n", stack[x - end].value);
            continue;
        }
        else if (stack[x - end].returnAddress != 0)
        {
            DWORD addr = stack[x - end].value;
            DWORD funcBase = 0;
            char* frameName = 0;
            char* name = 0;

            // try to find the name of the frame
            DWORD index = x - end - 1;
            while (index > 0)
            {
                if (stack[index].returnAddress != 0)
                {
                    frameName = lookupFunctionName(stack[index].value, &funcBase, FunctionNameBuffer, sizeof(FunctionNameBuffer));
                    if (!frameName || frameName[0] == 0)
                        frameName = "<Unknown>";
                    break;
                }
                index--;
            }
            fprintf(file, "\n");
            fprintf(file, "  Function: %s\n", frameName);

            name = lookupFunctionName(addr, &funcBase, FunctionNameBuffer, sizeof(FunctionNameBuffer));
            if (!name || name[0] == 0)
                name = "<Unknown>";

            fprintf(file, "    %08.8x: %08.8x  ", x, *x);
            fprintf(file, " {Return address: %08.8x [%s + %x]}\n", stack[x - end].value, name, 
                addr - funcBase);
            continue;
        }

        if (EVEN(i))
        {
            fprintf(file, "    %08.8x: %08.8x  ", x, *x);
            if (inAnyLispHeap(*x) && *(unsigned long*)x == JumpBufferMarker)
            {
                outputReferencedObject(file, *(x - 4), 0);	// x -> EDI
                outputReferencedObject(file, *(x - 6), 0);	// x -> EDX
                outputReferencedObject(file, *(x - 7), 0);	// x -> ECX
                outputReferencedObject(file, *(x - 8), 0);	// x -> EBX
                x -= ((JumpBufferSize / 4) - 1);
            }
            else // if a foreign stack block, then skip it
            if (*(unsigned char*)x == ForeignStackEndMarker
                && ((*x >> 8) <= 0x2000)        // allow up to 64k foreign stack block
                && (x - (2 * (*x >> 8))) > end
                    && *(unsigned char*)(x - (2 * (*x >> 8))) == ForeignStackStartMarker)
            {
                // foreign stack block
                fprintf(file, "{Foreign Stack Block: Start = %08.8x, Length = %08.8x bytes}",
                    *x, (2 * (*x >> 8)) * 4);
                x -= ((2 * (*x >> 8)) - 1);
            }
            else
            {
                LispObj o = *x;
                if (isHeapBlock(o))
                {
                    if (inAnyLispHeap(o))
                        fprintf(file, " ");
                    else if (o < 0x10000)
                        fprintf(file, "*");
 					else if ((LispObj*)o <= stackStart && (LispObj*)o >= stackEnd)
						fprintf(file, "-");		// indicates a reference into the stack
                    else
                        fprintf(file, "#");
                }
                else if (isFixnum(o) || isShortFloat(o))
                    fprintf(file, " ");
                else if (o < 0x10000)
                    fprintf(file, "*");
				else if ((LispObj*)o <= stackStart && (LispObj*)o >= stackEnd)
					fprintf(file, "-");		// indicates a reference into the stack
                else
                    fprintf(file, "@");

                outputReferencedObject(file, *x, 0);
            }

            fprintf(file, "\n");
        }
        else
        {
            fprintf(file, "    %08.8x: %08.8x  Foreign stack\n", x, *x);
        }
        if ((i < index) && ((unsigned long)x) <= qv[(i * 2) + STACK_MARKERS_Index])
            i++;
    }

    // only update registers if we are running in lisp (not foreign) code
    // We determine this based on the region of the top of the stack.
    if (EVEN(i))
        fprintf(file, "  Execution context: Lisp\n");
    else
        fprintf(file, "  Execution context: Foreign\n");
    fprintf(file, "\n");
    if (stack)
        HeapFree(GetProcessHeap(), 0, stack);

	// output QV array for this thread (system objects)
	fprintf(file, "\nQV Array:\n");
	for (i = 0; i < SYSTEM_OBJ_MAX; i++)
	{
        fprintf(file, "    %08.8x: %08.8x  ", i, qv[i]);
        LispObj o = qv[i];
        if (isHeapBlock(o))
        {
            if (inAnyLispHeap(o))
                fprintf(file, " ");
            else if (o < 0x10000)
                fprintf(file, "*");
			else if ((LispObj*)o <= stackStart && (LispObj*)o >= stackEnd)
				fprintf(file, "-");		// indicates a reference into the stack
            else
                fprintf(file, "#");
        }
        else if (isFixnum(o) || isShortFloat(o))
            fprintf(file, " ");
        else if (o < 0x10000)
            fprintf(file, "*");
		else if ((LispObj*)o <= stackStart && (LispObj*)o >= stackEnd)
			fprintf(file, "-");		// indicates a reference into the stack
        else
            fprintf(file, "@");

        outputReferencedObject(file, qv[i], 0);
		fprintf(file, "\n");
	}
	fprintf(file, "\n\n");
	stackStart = 0;
	stackEnd = 0;
}

static FILE* dumpFile = 0;

static char* UvectorTypeNames[32] =
{
    "FUNCTION",							// FunctionType					0
    "KERNEL-FUNCTION",					// KFunctionType				1
    "STRUCTURE",						// StructureType				2
    "ARRAY",							// ArrayType					3
    "SYMBOL",							// SymbolType					4
    "STREAM",							// StreamType					5
    "DOUBLE-FLOAT",						// DoubleFloatType				6
    "PACKAGE",							// PackageType					7
    "HASHTABLE",						// HashtableType				8
    "FOREIGN-POINTER",					// ForeignType					9
    "COMPILED-CODE",					// CompiledCodeType				10
    "READTABLE",						// ReadTableType				11
    "COMPLEX",							// ComplexType					12
    "RATIO",							// RatioType					13
    "BIGNUM",							// BignumType					14
    "FOREIGN-HEAP-POINTER",				// ForeignHeapType				15
    "WEAK-POINTER",						// WeakPointerType				16
    "SIMPLE-VECTOR",					// SimpleVectorType				17
    "SIMPLE-CHAR-VECTOR",				// SimpleCharVectorType			18
    "SIMPLE-BYTE-VECTOR",				// SimpleByteVectorType			19
    "SIMPLE-SHORT-VECTOR",				// SimpleShortVectorType		20
    "SIMPLE-DOUBLE-FLOAT-VECTOR",		// SimpleDoubleFloatVectorType  21
    "SIMPLE-BIT-VECTOR",				// SimpleBitVectorType			22
    "SIMPLE-SINGLE-FLOAT-VECTOR",		// SimpleSingleFloatVectorType  23
    "SINGLE-FLOAT",						// SingleFloatType				24
    "CLOS-OBJECT",						// CLOSInstanceType				25
    "FOREIGN-STACK-POINTER",			// ForeignStackType				26
    "FOREIGN-STACK-END",				// ForeignStackEndType			27
    "UNKNOWN1",							// Unknown 1					28
    "UNKNOWN2",							// Unknown 2					29
    "UNKNOWN3",							// Unknown 3					30
    "UNKNOWN4",							// Unknown 4					31
};

static int ConsCount;
static int UvectorCounts[32];
static int UvectorCount;

extern long unassemble(unsigned long, unsigned long);
extern char gDisassemblyOutputBuf[];

void disassembleCode(FILE* file, LispObj u)
{
    DWORD start = stripTag(u);
    DWORD end = start + uvectorSize(u) * 8;
    start += 16;
    DWORD p = start;
    while (p < end)
    {
        int ret = unassemble(start, p - start);
        if (ret > 0)
        {
            p += ret;
            fprintf(file, "            %s\n", gDisassemblyOutputBuf);
        }
        else
            break;
    }
}

void dumpUvector(FILE* file, LispObj u)
{
    DWORD blockLength = uvectorSize(u) * 8;
    DWORD* block = (DWORD*)stripTag(u);
    int type = uvectorType(u);
    fprintf(dumpFile, "  %08.8x: UVECTOR %08.8x bytes     %s", 
        stripTag(u), blockLength, UvectorTypeNames[type]);
    if (type == SymbolType)
    {
        fprintf(dumpFile, ":             ");
        writeSymbolName(dumpFile, u);
    }
    else if (type == SimpleCharVectorType)
    {
        fprintf(dumpFile, ": ");
        writeSimpleCharVector(dumpFile, u);
    }

    //fprintf(dumpFile, "\n");
#if 0
    if (type == CompiledCodeType)
    {
        fprintf(file, "\n");
        disassembleCode(file, u);
    }
    else
#endif
    {
        // display the contents of the block    
        int cells = (blockLength / 4);
        if (!IsBadReadPtr(block, cells * 4))
        {
            if (cells > 1024)
                cells = 1024;       // print at most the first 4k bytes of a heap object
            for (int i = 0; i < cells; i++)
            {
                if ((i % 8) == 0)
                    fprintf(file, "\n                    ");
                fprintf(file, "%08.8x ", block[i]);
            }
        }
    }
    fprintf(file, "\n");
}

void dumpCons(FILE* file, LispObj c)
{
    fprintf(file, "  %08.8x: CONS    %08.8x ", stripTag(c), *(LispObj*)stripTag(c));
	outputReferencedObject(file, *(LispObj*)stripTag(c), 0);
	fprintf(file, "\n"); 
	fprintf(file, "                    %08.8x ", *(LispObj*)(stripTag(c) + 4));
	outputReferencedObject(file, *(LispObj*)(stripTag(c) + 4), 0);
	fprintf(file, "\n"); 
}

void dumpHeapBlock(LispObj block)
{
    if (isCons(block))
    {
        ConsCount++;
        dumpCons(dumpFile, block);
    }
    else
    if (isUvector(block))
    {
        UvectorCount++;
        int type = uvectorType(block);
        UvectorCounts[type]++;
        dumpUvector(dumpFile, block);
    }
}


void dumpLispHeap(FILE* file, LispHeap* heap, char* name)
{
    ConsCount = 0;
    UvectorCount = 0;
    for (int i = 0; i < 32; i++)
        UvectorCounts[i] = 0;
    dumpFile = file;
    fprintf(file, "%s:\n", name);
    doHeapBlock(heap, dumpHeapBlock);
    dumpFile = 0;
    fprintf(file, "\n%s Heap Summary:\n", name);
    fprintf(file, "  Total number of heap objects:                 %08.8x\n", ConsCount + UvectorCount);
    fprintf(file, "  Number of CONS objects:                       %08.8x\n", ConsCount); 
    fprintf(file, "  Number of UVECTOR objects:                    %08.8x\n", UvectorCount); 
    fprintf(file, "\n");
    fprintf(file, "  Object Totals By Type\n");
    fprintf(file, "  ---------------------\n");
    for (int i = 0; i < 28; i++)
    {
        char* name = UvectorTypeNames[i];
        int len = strlen(name);
        fprintf(file, "  %s", name); 
        for (int n = 0; n <  (28 - len); n++)
            fprintf(file, " ");
        fprintf(file, "%08.8x\n", UvectorCounts[i]); 
    }
}

//
// When an access violation occurs, write a report before quitting.
// If the address is (-2) then assume there was no access violation.
//
void writeMemoryReport(void* address, CONTEXT* context)
{
    FILE* file = 0;
    int i = 0;
    __try
    {
        __try
        {
            ThreadList.suspendAllOtherThreadsWithoutLocking();

            HWND wnd = 0;
			int err = 0;
			{
				char ApplicationDumpPath[MAX_PATH + 1] = { 0 };
				if (*CormanLispOutputDirectory) // directory was set
				{
					strncpy_s(ApplicationDumpPath, sizeof(ApplicationDumpPath),
						CormanLispOutputDirectory, CormanLispOutputDirectoryLen);
					if (CormanLispOutputDirectoryLen <= 2 || CormanLispOutputDirectory[CormanLispOutputDirectoryLen - 2] != '\\')
					{
						strcat_s(ApplicationDumpPath, sizeof(ApplicationDumpPath), "\\");
					}
				}

				strcat_s(ApplicationDumpPath, sizeof(ApplicationDumpPath), DumpFileName);
				err = fopen_s(&file, ApplicationDumpPath, "w");	// write
			}
            if (err != 0)
            {
                CormanLispServer->GetAppMainWindow(&wnd);
                ThreadList.resumeAllOtherThreads();
                MessageBox(wnd, "A system report could not be written because an error was encountered on\n"
                                "the attempt to open the file 'CormanLisp_Dump.log'.",
                                "System Error",
                                MB_OK);
                return;
            }

            fprintf(file, "Corman Lisp Memory Report\n");
            fprintf(file, "Time: %s\n\n", getTime());
            
            if (address != (void*)(-2))
            {
                fprintf(file, "The operating system has reported a memory access violation at the address %08.8x.\n\n", address);
                fprintf(file, "Registers:\n"); 

                fprintf(file, "  EAX: %08.8x\n", context->Eax); 
                outputReferencedObject(file, context->Eax, 4);
                fprintf(file, "\n");

                fprintf(file, "  EBX: %08.8x\n", context->Ebx); 
                outputReferencedObject(file, context->Ebx, 4);
                fprintf(file, "\n");

                fprintf(file, "  ECX: %08.8x\n", context->Ecx); 
                outputReferencedObject(file, context->Ecx, 4);
                fprintf(file, "\n");

                fprintf(file, "  EDX: %08.8x\n", context->Edx); 
                outputReferencedObject(file, context->Edx, 4);
                fprintf(file, "\n");

                fprintf(file, "  ESI: %08.8x\n", context->Esi); 
                outputReferencedObject(file, context->Esi, 4);
                fprintf(file, "\n");

                fprintf(file, "  EDI: %08.8x\n", context->Edi); 
                outputReferencedObject(file, context->Edi, 4);
                fprintf(file, "\n");

                fprintf(file, "  ESP: %08.8x\n", context->Esp); 
                fprintf(file, "  EBP: %08.8x\n", context->Ebp); 
                fprintf(file, "  EIP: %08.8x\n", context->Eip); 
                fprintf(file, "\n");
            }

            // output heap info
            writeHeapInfo(&EphemeralHeap1, file, "EphemeralHeap1");
            writeHeapInfo(&EphemeralHeap2, file, "EphemeralHeap2");
            writeHeapInfo(&LispHeap1, file, "LispHeap1");
            writeHeapInfo(&LispHeap2, file, "LispHeap2");
            writeHeapInfo(&SysGlobals, file, "SysGlobals");

            // output thread summary
            fprintf(file, "Lisp Threads:\n");
            ThreadRecord* tr = ThreadList.getList();
            while (tr)
            {
                if (tr->started)
                {
                    fprintf(file, "  ");
                    writeThreadSummary(file, tr);
                    fprintf(file, "\n");
                }
                tr = tr->next;
            }
            fprintf(file, "\n");

            // check stacks of all suspended threads
            tr = ThreadList.getList();
            while (tr)
            {
                if (tr->started)
                {
                    writeThreadRegistersReport(file, tr);
                    writeThreadStackReport(file, tr);
                }
                tr = tr->next;
            }

            // dump the three active lisp heaps
            dumpLispHeap(file, &EphemeralHeap1, "EphemeralHeap1");
            fprintf(file, "\n");
            dumpLispHeap(file, &EphemeralHeap2, "EphemeralHeap2");
            fprintf(file, "\n");
            dumpLispHeap(file, &LispHeap1, "LispHeap1");
            fprintf(file, "\n");

            MessageBox(wnd, "A system report has been saved to the file 'CormanLisp_Dump.log'\n",
                            "System Report Generated",
                            MB_OK);

        }
        __finally
        {
            // resume all suspended threads
            ThreadList.resumeAllOtherThreads();

            if (file)
            {
                fflush(file);
                fclose(file);
            }
        }
    }
    __except (i)
    {
        // if the memory report task fails with a serious error, we need to abort
        // the whole lisp process
        char* msg = "A problem has occurred while writing the memory report.\n"
                        "This is probably a serious problem, so the Lisp process will be closed down.\n";
        CormanLispServer->LispShutdown(msg, strlen(msg));
    }
}
static struct MemReportParams
{
    void* address;
    CONTEXT* context;
    BOOL completed;
} memReportParams;

void WriteMemoryReport(void* param)
{
    MemReportParams* memReportParams = (MemReportParams*)param;
    writeMemoryReport(memReportParams->address, memReportParams->context);
    memReportParams->completed = TRUE;
}

void WriteMemoryReportTask(void* address, CONTEXT* context)
{
    memReportParams.address = address;
    memReportParams.context = context;
    memReportParams.completed = FALSE;
    _beginthread(WriteMemoryReport, 0, &memReportParams);
    while (!memReportParams.completed)
    {
        Sleep(500);     // wait for .5 seconds before polling again
    }
}
