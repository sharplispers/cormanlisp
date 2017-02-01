//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		gc.cpp
//		Contents:	Garbage collector for Corman Lisp
//					This collector is specific to Intel processors,
//					80386 and above, and the Win32 platform.
//		History:	6/22/96  RGC  Created.
//					5/9/99	 RGC  Modified to dynamically allocate heaps
//								  in a position independent fashion.
//								  This should allow working inside other
//								  apps such as AutoCad.
//					5/9/99   RU   Some Acad14 fixes by Reini Urban, UR 4/30/99 - 5/3/99
//								  Acad uses more than the assumed 0x01000000 Virtual Memory.
//								  So when loading this DLL VirtualAlloc() fails. All addresses
//								  must be dynamically calculated.
//								  UR: within ACAD we must use the runtime non-debug mfcrt.dll
//								  (Note from RGC: Many of these changes were unnecessary given the
//								   previous modifications, so they were not included here.)
//                  7/19/03  RGC  Small change to the collector--global stack pointer gets assigned
//                                after garbage collector recursion detection.
//					4/24/06  RGC  Numerous mods to the garbage collector to try to better handle
//							      and report memory access violations that occur during collections.
//					10/31/16 Artem Boldarev
//							      Look for image in the installation directory (as last resort). Useful for loading "CormanLisp.img".
//
#include "stdafx.h"
#include <wtypes.h>
#include <memory.h>
#include <windows.h>
#include <assert.h>
#include <stdio.h>
#include <process.h>
#include <stddef.h>
#include <time.h>

#include "Lisp.h"
#include "threadclasses.h"
#include "CormanLispServer.h"

#pragma warning (disable:4505)				// unreferenced local function has been removed			

//
//		EphemeralHeap1
//		This heap never needs to be write-protected, and all allocation
//		happens in this heap. It is never scanned for roots.
//
//		EphemeralHeap2
//		This heap is only scanned for roots in the first level of the
//		collection (looking for roots for level 0).
//		It is initially write-protected. Each time one of its pages is
//		scanned, a bit in its page table is set or cleared to indicate
//		whether that page contains any roots back to EH1. If it does not
//		contain any roots for EH1 it will be write-protected.
//
//
//		Each page table entry contains a 32-bit unsigned long. The high
//		bit is 1 if the page is dirty i.e. has been written to since the
//		last garbage collection or was known to contain pointers to
//		heap objects in a newer heap space (at the last garbage collection).
//		The low 16-bits are used to store the offset in the page of the
//		first node containing tagged data. This will normally be 0, but
//		character or byte arrays that overlap page boundaries require that
//		these be maintained for heap scanning purposes.
//
#define address_to_page(addr)	(((unsigned long)addr) >> 12)
#define page_address(page)		((byte*)((page) << 12))
#define page_offset(addr)		((((unsigned long)addr) << 20) >> 23)

/* definition moved to Lisp.h
struct PageTableEntry
{
    byte flags;
    byte unused;
    short taggedOffset;
};
*/

#define EH1_Bit			0x01
#define EH1_Mask		0xfe
#define EH2_Bit			0x02
#define EH2_Mask		0xfd
#define Protected_Bit	0x80
#define Protected_Mask	0x7f
#define Committed_Bit	0x40
#define Committed_Mask	0xbf

//PageTableEntry PageTable[0x10000];		// allocate 64k entries
                                        // allows 256 megs of heap
#define PageOffset(page_id)				(PageTable[page_id].taggedOffset)
#define SetPageOffset(page_id, offset)	(PageTable[page_id].taggedOffset = offset)
#define PageFlags(page_id)				(PageTable[page_id].flags)
#define SetPageReferencesEH1(page_id)	(PageTable[page_id].flags |= EH1_Bit)
#define ClrPageReferencesEH1(page_id)	(PageTable[page_id].flags &= EH1_Mask)
#define SetPageReferencesEH2(page_id)	(PageTable[page_id].flags |= EH2_Bit)
#define ClrPageReferencesEH2(page_id)	(PageTable[page_id].flags &= EH2_Mask)
#define SetProtectFlag(page_id)			(PageTable[page_id].flags |= Protected_Bit)
#define ClrProtectFlag(page_id)			(PageTable[page_id].flags &= Protected_Mask)
//#define SetCommittedFlag(page_id)		(PageTable[page_id].flags |= Committed_Bit)
//#define ClrCommittedFlag(page_id)		(PageTable[page_id].flags &= Committed_Mask)
//#define PageCommitted(page_id)			(PageTable[page_id].flags & Committed_Bit)
#define ClrFlags(page_id)				(PageTable[page_id].flags = 0, PageTable[page_id].taggedOffset = 0)

static void UnWriteProtectPage(ULONG32 page_id);
static int growLispHeaps(long numCells);
static void offsetUvector(LispObj obj, long offset, long length);
static BYTE* MapFile(const char* path, DWORD* length);
static void UnmapFile(BYTE* mapping);
static void outputReferencedObject(FILE* file, LispObj obj, int indent);
void* nextHeapObject(void* addr);

const char* DumpFileName = "CormanLisp_Dump.log";

int HeapChecking = false;

const int PAGE_SIZE = 0x1000;						// 4k pages
const int CELLS_PER_PAGE = PAGE_SIZE / sizeof(Node);// 512 cells per page

const int MAX_CELLS_PER_ARRAY = 0x01000000;			// allow 16 meg cells per array

// All these must be multiples of the page size!!
// Just make sure the last 3 hex digits are 0's
int EphemeralHeap1SizeMin    = 0x00100000;          // 1 meg min
int EphemeralHeap1Size       = 0x00400000;			// 4 megs default
int EphemeralHeap1SizeMax    = 0x00800000;			// 8 megs max

int EphemeralHeap2SizeMin    = 0x00100000;			// 1 megs min
int EphemeralHeap2Size       = 0x00300000;			// 3 megs  (must be <= EphemeralHeap1Size)
int EphemeralHeap2SizeMax    = 0x00800000;			// 8 megs max

int LispHeapSizeMin          = 0x01000000;          // 16 megs
int LispHeapSize		     = 0x04000000;			// 64 megs
int LispHeapSizeMax          = 0x40000000;          // 1 gig
int SysGlobalsSize	         = 0x00004000;          // 16k
int LispHeapReserveMin       = 0x08000000;          // min 64 megs reserve
int LispHeapReserveDefault   = 0x20000000;          // default to 512 meg reserve
int LispHeapReserveMax       = 0x40000000;          // max 1 gig reserve

// if HardwareAssist is true (1) then use hardware virtual memory to keep track of which 
// areas of the heap need to be searched for roots. This page trapping mechanism occasionally 
// causes crashes in applications with a lot of Windows OS callbacks and multi-threading.
// It used to give performance gains but I am not sure the advantage is still significant.
// By default we will leave this switched off, in favor of better stability.
//
int HardwareAssist = 0;   // 0 if hardware-assisted GC is off, 1 if it is on. 

//int SysGlobalsAddr = 0x1000000;
LispObj** GlobalQVPointer;
unsigned char** GlobalForeignJumpTable;
long* GlobalForeignJumpTableNumEntries;
long* GlobalForeignJumpTableCapacity;

static PageTableEntry* allocPageTable();

LispHeap EphemeralHeap1;
LispHeap EphemeralHeap2;
LispHeap LispHeap1;
LispHeap LispHeap2;
LispHeap SysGlobals;
ULONG32 SysGlobalsAddr = 0;
PageTableEntry* PageTable = 0;

long ToSpaceStart = 0;
long ToSpaceCurrent = 0;
long ToSpaceEnd = 0;

//	static functions
static void checkStackRoots(LispHeap* fromSpace, LispHeap* toSpace);
static void checkGlobalRoots(LispHeap* fromSpace, LispHeap* toSpace);
static void checkHeapRoots(LispHeap* fromSpace, LispHeap* toSpace, LispHeap* checkHeap,
                            Node* start, Node* end);
static void promoteExecutingFunctions();
static void copyReferencedBlocks(LispHeap* fromSpace, LispHeap* toSpace, Node* start);
static void promoteBlock(LispObj* obj, LispHeap* toSpace);
static Node* checkObject(Node* p);
static void resurrectFinalizationObjects(LispHeap* fromSpace, LispHeap* toSpace);
static void addResurrectedObjectPair(LispObj pair);
static void finalizeResurrectedObjects();
static void executeGCRegistryFunctions();
static void releaseWeakPointers();
static void checkWeakPointers(LispHeap* fromSpace, LispHeap* toSpace);
static LispObj AllocLargeVector(long num);
static void verifyHeapBlocks();
unsigned long looksLikeEBP(unsigned long addr);
static void promoteJumpBufferBlocks(LispObj* x, LispHeap* toSpace);
static void offsetHeapAddresses(long offset);
static void offsetGlobals(long offset, unsigned long origAddress, unsigned long origAddressEnd);
static void offsetCode(LispObj obj, long offset);

long GarbageCollectionTicks = 0;

LispHeap* GCFromSpace = 0;
LispHeap* GCToSpace = 0;
LispHeap* GCScanSpace = 0;
xbool GCCheckCode = FALSE;

CriticalSection GCCriticalSection;
bool GCFailure = false;

// objects to be resurrected and finalized after GC is over
struct FinalizationNode
{
    FinalizationNode* next;
    LispObj obj;
};
static FinalizationNode* resurrectedObjects = 0;

//	each item in the FinalizationRegistry is a cons with
//	(object . finalization-function)
//  The FinalizationRegistry is now stored in FINALIZATION_REGISTRY.

//	each item in the GCExecRegistry is a function to be called
//	at garbage collection time (directly following collection)
//  This may be used to recompute pointer addresses i.e. for hash tables.
//
LispObj GCExecRegistry = 0;

int HeapInitializationError = 0;

static PageTableEntry* allocPageTable()
{
    // find the first and last pages being used
    unsigned long minPage = 0xffffffff;
    unsigned long maxPage = 0;
    unsigned char* mem = 0;
    minPage = min(minPage, EphemeralHeap1.firstPage);
    maxPage = max(maxPage, EphemeralHeap1.firstPage + EphemeralHeap1.numReservedPages);
    minPage = min(minPage, EphemeralHeap2.firstPage);
    maxPage = max(maxPage, EphemeralHeap2.firstPage + EphemeralHeap2.numReservedPages);
    minPage = min(minPage, LispHeap1.firstPage);
    maxPage = max(maxPage, LispHeap1.firstPage + LispHeap1.numReservedPages);
    minPage = min(minPage, LispHeap2.firstPage);
    maxPage = max(maxPage, LispHeap2.firstPage + LispHeap2.numReservedPages);
    minPage = min(minPage, SysGlobals.firstPage);
    maxPage = max(maxPage, SysGlobals.firstPage + SysGlobals.numReservedPages);

    mem = (byte*)VirtualAlloc(NULL, (maxPage + 1) * sizeof(PageTableEntry),
        MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    return (PageTableEntry*)mem;
}

void swapLispHeaps()
{
    Node* t = 0;
    byte* bptr = 0;
    unsigned long n = 0;

    // exchange pointers
    t = LispHeap1.start;
    LispHeap1.start = LispHeap2.start;
    LispHeap2.start = t;

    t = LispHeap1.end;
    LispHeap1.end = LispHeap2.end;
    LispHeap2.end = t;

    t = LispHeap1.overflow;
    LispHeap1.overflow = LispHeap2.overflow;
    LispHeap2.overflow = t;

    LispHeap1.current = LispHeap2.current;
    LispHeap2.current = LispHeap2.start;

    bptr = LispHeap1.mem;
    LispHeap1.mem = LispHeap2.mem;
    LispHeap2.mem = bptr;

    n = LispHeap1.firstPage;
    LispHeap1.firstPage = LispHeap2.firstPage;
    LispHeap2.firstPage = n;

    n = LispHeap1.firstUncommittedPage;
    LispHeap1.firstUncommittedPage = LispHeap2.firstUncommittedPage;
    LispHeap2.firstUncommittedPage = n;

    LispHeap2.reset();

	foreignPtr(symbolValue(HEAP_2_START))	= (LispObj)LispHeap1.start;	
	foreignPtr(symbolValue(HEAP_2_END))		= (LispObj)LispHeap1.end;		
}

#define isHeapPointer(n) (isHeapBlock(n) && \
            ((n) >= (LispObj)(GCFromSpace->start)) && \
            ((n) < (LispObj)(GCFromSpace->end)))

#define pointsIntoHeap(n) \
            (((n) >= (LispObj)(GCFromSpace->start)) && \
            ((n) < (LispObj)(GCFromSpace->end)))

static void UnWriteProtectPage(ULONG32 page_id)
{
    DWORD oldProtect = 0;
    BOOL ret = 0;
    DWORD err = 0;
    if (PageTable[page_id].flags & Protected_Bit)
    {
        ClrProtectFlag(page_id);
        ret = VirtualProtect(page_address(page_id), PAGE_SIZE,	PAGE_EXECUTE_READWRITE, &oldProtect);
        if (!ret)
        {
            err = GetLastError();
        }
    }
}

// Returns 0 if successful, -1 otherwise.
//
static int growLispHeaps(long numCells)
{
    int ret = 0;

    // allocate in multiples of a megabyte
    long bytesToGrow = (((numCells * 8) / 0x100000) + 1) * 0x100000;

    if (LispHeap1.grow(bytesToGrow) ||
        LispHeap2.grow(bytesToGrow))
    {
        return -1;
    }
    LispHeap2.decommitAllPages();

	foreignPtr(symbolValue(HEAP_2_START))	= (LispObj)LispHeap1.start;	
	foreignPtr(symbolValue(HEAP_2_END))		= (LispObj)LispHeap1.end;		

    return 0;
}

void
ClearPageTable()
{
    memset(PageTable, 0, sizeof(PageTable));
}

LispHeap::LispHeap()
    : start(0), end(0), current(0), overflow(0), mem(0), generation(0),
      sizeMem(0), firstPage(0), numPages(0), reserve(0)
{
}

void LispHeap::alloc(unsigned long size, unsigned long overflowVal,
                     unsigned long gen, unsigned long reserveRequested)
{
    assert((size % PAGE_SIZE) == 0);
    assert((overflowVal % PAGE_SIZE) == 0);
    assert((reserveRequested % PAGE_SIZE) == 0);

    unsigned long i = 0;
    void* ret = 0;
    sizeMem = size;
    size /= sizeof(Node);

    mem = (byte*)VirtualAlloc(NULL, reserveRequested, MEM_RESERVE, PAGE_EXECUTE_READWRITE);
    if (!mem)
    {
        // if we couldn't reserve that much, try to at least reserve half that amount.
        // Keep trying as necessary.
        while (!mem && reserveRequested > (size * 2))
        {
            reserveRequested = (reserveRequested / 2) & ~0xfff;
            mem = (byte*)VirtualAlloc(NULL, reserveRequested, MEM_RESERVE, PAGE_EXECUTE_READWRITE);
        }
        if (!mem)
            return;
    }
    reserve = reserveRequested;
    start = (Node*)mem;
    end = start + size;
    current = start;
    overflow = (Node*)(overflowVal + mem);
    numPages = sizeMem / PAGE_SIZE;
    firstPage = address_to_page((unsigned long)mem);
    firstUncommittedPage = firstPage;
    numReservedPages = reserve / PAGE_SIZE;
    generation = gen;
}

LispHeap::~LispHeap()
{
    if (mem)
        VirtualFree(mem, sizeMem, MEM_DECOMMIT);
}

void LispHeap::writeProtectAllPages()
{
	if (HardwareAssist)
	{
		unsigned long i = 0;
		DWORD oldProtect = 0;
		BOOL ret = 0;
		DWORD err = 0;
		ret = VirtualProtect(page_address(firstPage), PAGE_SIZE * numPages,
			PAGE_EXECUTE_READ, &oldProtect);
		if (!ret)
		{
			err = GetLastError();
		}
		else
		{
			for (i = 0; i < numPages; i++)
			{
				SetProtectFlag(i + firstPage);
			}
		}
	}
}

void LispHeap::unWriteProtectAllPages()
{
	unsigned long i = 0;
	DWORD oldProtect = 0;
	BOOL ret = 0;
	DWORD err = 0;
	ret = VirtualProtect(page_address(firstPage), PAGE_SIZE * numPages,
		PAGE_EXECUTE_READWRITE, &oldProtect);
	if (!ret)
	{
		err = GetLastError();
	}
	else
	{
		for (i = 0; i < numPages; i++)
		{
			ClrProtectFlag(i + firstPage);
		}
	}
}

// Returns 1 if successful, 0 otherwise.
int LispHeap::decommitAllPages()
{
    unsigned long i = 0;
    BOOL ret = 1;
    DWORD err = 0;
    if (firstUncommittedPage > firstPage)
        ret = VirtualFree((void*)page_address(firstPage),
            PAGE_SIZE * (firstUncommittedPage - firstPage),
            MEM_DECOMMIT);
    firstUncommittedPage = firstPage;
    if (!ret)
    {
        err = GetLastError();
        return 0;
    }
    return 1;
}

// Returns 1 if successful, 0 otherwise.
int LispHeap::commitAllPages()
{
    unsigned long i = 0;
    void* ret = 0;
    int result = 1;
    DWORD err = 0;
    if (firstUncommittedPage < (firstPage + numPages))
    {
        ret = (void*)VirtualAlloc((void*)page_address(firstUncommittedPage),
                PAGE_SIZE * (firstPage + numPages - firstUncommittedPage),
                MEM_COMMIT, PAGE_EXECUTE_READWRITE);
        result = ret ? 1 : 0;
        if (!ret)
        {
            err = GetLastError();
        }
    }
    firstUncommittedPage = firstPage + numPages;
    return result;
}

// Commit from the current page to the end.
// Returns 1 if successful, 0 otherwise.
int LispHeap::commitTrailingPages(int readonly)
{
    return commitAllPages();
}

// Commit the requested page.
// Returns 1 if successful, 0 otherwise.
int LispHeap::commitPage(byte* addr, int readonly)
{
    unsigned long i = 0;
    void* ret = 0;
    DWORD err = 0;
    unsigned long newpage = 0;
    newpage = address_to_page(addr);
    assert(newpage >= firstUncommittedPage);
    ret = (void*)VirtualAlloc((byte*)page_address(firstUncommittedPage),
                (newpage - firstUncommittedPage + 1) * PAGE_SIZE,
                MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    firstUncommittedPage = newpage + 1;
    if (!ret)
    {
        err = GetLastError();
        return 0;
    }
    else
        return 1;
}

// Commit the requested page.
// Returns 1 if the page is committed, 0 otherwise.
int LispHeap::pageCommitted(byte* addr)
{
    unsigned long page = 0;
    page = address_to_page(addr);
    return page < firstUncommittedPage;
}

// Decommit from the current page to the end.
// Returns 1 if successful, 0 otherwise.
int LispHeap::decommitTrailingPages()
{
/*  RGC DEBUG
    unsigned long i = 0;
    BOOL ret = 1;
    DWORD err = 0;
    unsigned long currpage = 0;
    currpage = address_to_page(current);
    if ((currpage + 1) < firstUncommittedPage)
        ret = VirtualFree((void*)page_address(currpage + 1),
            PAGE_SIZE * (firstUncommittedPage - currpage - 1),
            MEM_DECOMMIT);
    firstUncommittedPage = currpage + 1;
    if (!ret)
    {
        err = GetLastError();
        return 0;
    }
*/
    return 1;
}

BOOL LispHeap::inHeap(byte* addr)
{
    return (addr >= mem && addr < (mem + sizeMem));
}

void LispHeap::reset()
{
    unsigned long i = 0;
    unsigned long page_id;
    current = start;
    for (; i < numPages; i++)
    {
        page_id = i + firstPage;
        SetPageOffset(page_id, 0);
        ClrPageReferencesEH1(page_id);
        ClrPageReferencesEH2(page_id);
    }
}

// Returns 0 if successful, -1 otherwise.
// Growing the heap reserve is not supported at this time.
//
int LispHeap::grow(long bytesToGrow)
{
    assert((bytesToGrow % PAGE_SIZE) == 0);
    /*
        byte* mem = (byte*)VirtualAlloc((void*)page_address(firstUncommittedPage),
                PAGE_SIZE * (firstPage + numPages - firstUncommittedPage) + bytesToGrow,
                MEM_COMMIT, PAGE_EXECUTE_READWRITE);
        if (!mem)
        {
            Error("Virtual memory could not be allocated");
            return;
        }
    */
    if ((unsigned long)(sizeMem + bytesToGrow) > reserve)
    {
        // not enough reserve
        return -1;
    }

    sizeMem += bytesToGrow;
    end += (bytesToGrow / 8);
    overflow += (bytesToGrow / 8);	// TDE: this probably needs to be modified to
                                    // an appropriate percentage of the new heap size
    numPages += (bytesToGrow / PAGE_SIZE);
    return 0;
}

//
//	AllocCons()
//	Allocates a 2-cell block
//

__declspec(naked) void EnterGCCriticalSection()
{
    __asm
    {
        push	ebp
        mov		ebp, esp
        push    edi
	}
	LISP_TO_FOREIGN()
	
	__asm
	{
		push	4	;; push argument length (bytes)
		lea		eax, dword ptr GCCriticalSection.m_sect
		push	eax
		call	dword ptr EnterCriticalSection
		add		esp, 4	;; pop argument length
	}

	FOREIGN_RETURN_TO_LISP()
	__asm
	{
        pop     edi
        pop		ebp
        ret
    }
}

__declspec(naked) void LeaveGCCriticalSection()
{
    __asm
    {
        push	ebp
        mov		ebp, esp
        push    edi
	}
	LISP_TO_FOREIGN()

	__asm
	{
		push	4	;; push argument length (bytes)
		lea		eax, dword ptr GCCriticalSection.m_sect
		push	eax
		call	dword ptr LeaveCriticalSection
		add		esp, 4	;; pop argument length
	}

	FOREIGN_RETURN_TO_LISP()

	__asm
	{
        pop     edi
        pop		ebp
        ret
    }
}

#if 0
__declspec(naked) void EnterGCCriticalSection()
{
    __asm
    {
        push	ebp
        mov		ebp, esp
        push    edi

        ;; push foreign stack context
        std		;; begin-atomic
        mov		eax, [esi + (STACK_MARKER_INDEX_Index * 4)]

 		cmp		eax, StackMarkersMax
		jge		short err1
        test    eax, 4          ;; index should not be odd (already in foreign code)
        jz      short t11000
	err1:
        cld		;; end-atomic
		mov		ecx, 0
		call	Memory_Report
	t11000:
        
		push	eax		;; just to get esp - 4
        mov		[esi + eax*2 + (STACK_MARKERS_Index * 4)], esp
        lea		edi, [esi + eax*2 + ((STACK_MARKERS_Index + 1) * 4)]
        pop		eax		;; restore esp
 		push	0
        pop		dword ptr [esi + eax*2 + ((STACK_MARKERS_Index + 1) * 4)]
        add		eax, 4
        mov		[esi + (STACK_MARKER_INDEX_Index * 4)], eax
        cld		;; end-atomic
		
		push	4	;; push argument length (bytes)
        lea		eax, dword ptr GCCriticalSection.m_sect
        push	eax
        call	dword ptr EnterCriticalSection
		add		esp, 4	;; pop argument length

        ;; pop foreign stack context
        std		;; begin-atomic
        mov		edx, [esi + (STACK_MARKER_INDEX_Index * 4)]
        sub		edx, 4
        mov		[esi + (STACK_MARKER_INDEX_Index * 4)], edx
        push    0  
   		pop		dword ptr [esi + edx*2 + (STACK_MARKERS_Index * 4)]   ;; clear stack marker   			
		push	0
        pop		dword ptr [esi + edx*2 + ((STACK_MARKERS_Index + 1) * 4)]
        cld		;; end-atomic

        pop     edi
        pop		ebp
        ret
    }
}

__declspec(naked) void LeaveGCCriticalSection()
{
    __asm
    {
        push	ebp
        mov		ebp, esp
        push    edi

        ;; push foreign stack context
        std		;; begin-atomic
        mov		eax, [esi + (STACK_MARKER_INDEX_Index * 4)]
        push	eax		;; just to get esp - 4
        mov		[esi + eax*2 + (STACK_MARKERS_Index * 4)], esp
        lea		edi, [esi + eax*2 + ((STACK_MARKERS_Index + 1) * 4)]
        pop		eax		;; restore esp
 		push	0
        pop		dword ptr [esi + eax*2 + ((STACK_MARKERS_Index + 1) * 4)]
        add		eax, 4
        mov		[esi + (STACK_MARKER_INDEX_Index * 4)], eax
        cld		;; end-atomic

 		push	4	;; push argument length (bytes)
        lea		eax, dword ptr GCCriticalSection.m_sect
        push	eax
        call	dword ptr LeaveCriticalSection
		add		esp, 4	;; pop argument length

        ;; pop foreign stack context
        std		;; begin-atomic
        mov		edx, [esi + (STACK_MARKER_INDEX_Index * 4)]
        sub		edx, 4
        mov		[esi + (STACK_MARKER_INDEX_Index * 4)], edx
        push    0  
   		pop		dword ptr [esi + edx*2 + (STACK_MARKERS_Index * 4)]   ;; clear stack marker   			
		push	0
        pop		dword ptr [esi + edx*2 + ((STACK_MARKERS_Index + 1) * 4)]
        cld		;; end-atomic

        pop     edi
        pop		ebp
        ret
    }
}
#endif

// to be called from Lisp code only
__declspec( naked ) LispObj AllocLocalCons()
{
    __asm
    {
        push	ebp
        mov		ebp, esp
    try1:
        mov		eax, [esi + THREAD_HEAP_Index*4]
        add		eax, 4
        lea		edx, [eax + 4]
        cmp		edx, [esi + THREAD_HEAP_END_Index*4]
        jle		done
        call	LoadLocalHeap
        jmp		short try1
    done:
        mov		[esi + THREAD_HEAP_Index*4], edx
        pop		ebp
        ret
    }
}

//
//	AllocVector()
//	Allocates an n-cell vector, with the first cell (cell 0) always
//	storing the length in the upper 24 bits, and 6 in the lower 3 bits.
//
__declspec(naked) LispObj AllocVector(long num)
{
    __asm
    {
        push	ebp
        mov		ebp, esp
        push	edi
        push	esi
        push	ebx
        call	ThreadQV
        mov		esi, eax		;; set up esi
        mov		edx, dword ptr num
        cmp		edx, 8000h		;; num < 32k cells?
        jb		t1
        shl     edx, 3          ;; pass tagged size
        push	edx				;; num >= 32k cells, alloc from primary heap
        call	AllocLargeVector
        add		esp, 4
        jmp		end
    t1:
        add		edx, 2
        sar		edx, 1			;; cells = (num + 2) >> 1

        // multi-threaded version
        push	edx
        call	EnterGCCriticalSection
        pop		edx
        mov		eax, dword ptr EphemeralHeap1.current	;; eax = new block
        lea		ecx, [eax + edx*8]
        cmp		ecx, dword ptr EphemeralHeap1.end
        jl		t2
        push	edx
        push	0
        call	garbageCollect
        add		esp, 4
        pop		edx
        mov		eax, dword ptr EphemeralHeap1.current
        lea		ecx, [eax + edx*8]
    t2:
        mov		dword ptr EphemeralHeap1.current, ecx
        mov		ecx, edx				;; ecx = num 8-byte cells
        shl		edx, 8
        or		dl, UvectorLengthTag
        mov		dword ptr [eax], edx	;; set block header cell
        mov		edi, eax
        mov		eax, 0 ;;initialize to 0
        mov		[edi + 4], eax
        dec		ecx
        jle		skip_loop1
    loop1:
        mov		[edi + ecx*8], eax
        mov		[edi + ecx*8 + 4], eax
        dec		ecx
        jg		loop1
    skip_loop1:
        mov		eax, edi
        add		eax, UvectorTag
        push	eax
        call	LeaveGCCriticalSection
        pop		eax
    end:
        pop		ebx
        pop		esi
        pop		edi
        pop		ebp
        ret
    }
}

//
//	LispAllocVector()
//	Allocates an n-cell vector, with the first cell (cell 0) always
//	storing the length in the upper 24 bits, and 6 in the lower 3 bits.
//	Same as AllocVector(), but expects to be called from Lisp code.
//  This function expects the passed length to be untagged.
//
__declspec(naked) LispObj LispAllocVector(long num)
{
    __asm
    {
        push	ebp
        mov		ebp, esp
        push	edi
        push	esi
        push	ebx
        mov		edx, dword ptr num
        cmp		edx, 8000h		;; num < 32k cells?
        jb		t1
        push	edx				;; num >= 32k cells, alloc from primary heap
        shl     edx, 3          ;; pass tagged size
        call	AllocLargeVector
        add		esp, 4
        jmp		end
    t1:
        add		edx, 2
        sar		edx, 1			;; cells = (num + 2) >> 1

        // multi-threaded version
        push	edx
        call	EnterGCCriticalSection
        pop		edx
        mov		eax, dword ptr EphemeralHeap1.current	;; eax = new block
        lea		ecx, [eax + edx*8]
        cmp		ecx, dword ptr EphemeralHeap1.end
        jl		t2
        push	edx
        push	0
        call	garbageCollect
        add		esp, 4
        pop		edx
        mov		eax, dword ptr EphemeralHeap1.current
        lea		ecx, [eax + edx*8]
    t2:
        mov		dword ptr EphemeralHeap1.current, ecx
        mov		ecx, edx				;; ecx = num 8-byte cells
        shl		edx, 8
        or		dl, UvectorLengthTag
        mov		dword ptr [eax], edx	;; set block header cell
        mov		edi, eax
        mov		eax, 0 ;;initialize to 0
        mov		[edi + 4], eax
        dec		ecx
        jle		skip_loop1
    loop1:
        mov		[edi + ecx*8], eax
        mov		[edi + ecx*8 + 4], eax
        dec		ecx
        jg		loop1
    skip_loop1:
        mov		eax, edi
        add		eax, UvectorTag
        push	eax
        call	LeaveGCCriticalSection
        pop		eax
end:
        pop		ebx
        pop		esi
        pop		edi
        pop		ebp
        ret
    }
}

//
//	LispAllocVectorTagged()
//	Allocates an n-cell vector, with the first cell (cell 0) always
//	storing the length in the upper 24 bits, and 6 in the lower 3 bits.
//	Same as AllocVector(), but expects to be called from Lisp code.
//  This function expects the passed length to be tagged.
//
__declspec(naked) LispObj LispAllocVectorTagged(LispObj num)
{
    __asm
    {
        push	ebp
        mov		ebp, esp
        push	edi
        push	esi
        push	ebx
        mov		edx, dword ptr num
        cmp		edx, 40000h		;; num < 32k (tagged) cells?
        jb		t1
        push	edx				;; num >= 32k cells, alloc from primary heap
        call	AllocLargeVector
        add		esp, 4
        jmp		end
    t1:
        add		edx, 16
        and     edx, 0xfffffff0
        sar		edx, 1			;; cells = (num + 2) >> 1

        // multi-threaded version
        push	edx
        call	EnterGCCriticalSection
        pop		edx
        shr     edx, 3                                  ;; safe in critical section, untagged edx
        mov		eax, dword ptr EphemeralHeap1.current	;; eax = new block
        lea		ecx, [eax + edx*8]
        cmp		ecx, dword ptr EphemeralHeap1.end
        jl		t2
        push	edx
        push	0
        call	garbageCollect
        add		esp, 4
        pop		edx
        mov		eax, dword ptr EphemeralHeap1.current
        lea		ecx, [eax + edx*8]
    t2:
        mov		dword ptr EphemeralHeap1.current, ecx
        mov		ecx, edx				;; ecx = num 8-byte cells
        shl		edx, 8
        or		dl, UvectorLengthTag
        mov		dword ptr [eax], edx	;; set block header cell
        mov		edi, eax
        mov		eax, 0 ;; initialize to 0
        mov		[edi + 4], eax
        dec		ecx
        jle		skip_loop1
    loop1:
        mov		[edi + ecx*8], eax
        mov		[edi + ecx*8 + 4], eax
        dec		ecx
        jg		loop1
    skip_loop1:
        mov		eax, edi
        add		eax, UvectorTag
        push	eax
        mov     ecx, 1
        mov     edx, 1
        call	LeaveGCCriticalSection
        pop		eax
        mov     ecx, 1
end:
        pop		ebx
        pop		esi
        pop		edi
        pop		ebp
        ret
    }
}

//
//	LoadLocalHeap()
//	Reserves 2048 conses on the ephemeral heap (16k worth)
//
#define LocalHeapSize 0x4000

__declspec(naked) LispObj LoadLocalHeap()
{
    __asm
    {
        push	ebp
        mov		ebp, esp
        push	edi
        push	esi
        push	ebx

        // multi-threaded version
        call	EnterGCCriticalSection
        mov		eax, dword ptr EphemeralHeap1.current	;; eax = new block
        lea		ecx, [eax + LocalHeapSize]
        cmp		ecx, dword ptr EphemeralHeap1.end
        jl		t2
        push	0
        call	garbageCollect
        add		esp, 4
        mov		eax, dword ptr EphemeralHeap1.current
        lea		ecx, [eax + LocalHeapSize]
    t2:
        mov		dword ptr EphemeralHeap1.current, ecx
        mov		ecx, (LocalHeapSize/8)				;; ecx = num 8-byte cells
        mov		edi, eax
        mov		eax, 0 ;;initialize to 0
        dec		ecx
    loop1:
        mov		[edi + ecx*8], eax
        mov		[edi + ecx*8 + 4], eax
        dec		ecx
        jge		loop1

        mov		[esi + THREAD_HEAP_Index*4], edi
        add		edi, (LocalHeapSize - 16)	;; leave two conses as pad
        mov		[esi + THREAD_HEAP_END_Index*4], edi
        call	LeaveGCCriticalSection
        pop		ebx
        pop		esi
        pop		edi
        pop		ebp
        ret
    }
}

//
//	AllocLargeVector()
//	Allocates an n-cell vector, with the first cell always
//	storing the length in the upper 29 bits, and 7 in he lower 3 bits.
//	This function is used to allocate blocks which are too large to be
//	stored in the ephemeral heaps, so they are allocated directly in
//	the primary heap (LispHeap1).
//  The passed size should be a tagged fixnum.
//
static LispObj AllocLargeVector(long num)
{
    LispObj ret = 0;
    long cells = 0;

    EnterGCCriticalSection();
    cells = (integer(num) + 2) >> 1;     // untagged, but safe inside critical section

    Node* block = LispHeap1.current;
    LispHeap1.current += cells;
    if (LispHeap1.current > LispHeap1.overflow)
    {
        LispHeap1.current -= cells;
        block = 0;
        garbageCollect(2);			// collect LispHeap1

        // try to allocate the block directly from LispHeap1
        block = LispHeap1.current;
        LispHeap1.current += cells;
        if (LispHeap1.current > LispHeap1.overflow)
        {
            LispHeap1.current -= cells;
            block = 0;

            // attempt to grow LispHeap1 and LispHeap2
            if (growLispHeaps(cells))
            {
                cells = 0;
                LeaveGCCriticalSection();
                Error("Could not allocate block of ~A bytes", num);
            }
            block = LispHeap1.current;
            LispHeap1.current += cells;
            if (LispHeap1.current > LispHeap1.overflow)
            {
                LispHeap1.current -= cells;
                cells = 0;
                LeaveGCCriticalSection();
                Error("Could not allocate block of ~A bytes", num);
            }
        }
    }
    *(LispObj*)block = (cells << 8) | UvectorLengthTag;

//	__asm		push edi  // unnecessary if done in prolog
    __asm		mov	ecx, dword ptr cells
    __asm		mov eax, 0 ;;initialize to 0
    __asm		mov edi, dword ptr block
    __asm		mov [edi + 4], eax
    __asm		dec	ecx
    __asm		jle skip_loop
    __asm	loop1:
    __asm		mov [edi + ecx*8], eax
    __asm		mov [edi + ecx*8 + 4], eax
    __asm		dec ecx
    __asm		jg loop1
    __asm	skip_loop:
//	__asm		pop edi

    ret = ((LispObj)block) + UvectorTag;
    cells = 0;
    LeaveGCCriticalSection();
    return ret;
}

BOOL handleMemoryAccessException(byte* addr)
{
    // if we are writing into the first page following the end
    // of the SysGlobals heap, expand the SysGlobals heap by 1 page (4096 bytes).
    if (!SysGlobals.inHeap(addr) && SysGlobals.inHeap(addr - PAGE_SIZE))
    {
        SysGlobals.grow(PAGE_SIZE);
        SysGlobals.commitAllPages();
        return TRUE;
    }

	if (addr >= (byte*)EphemeralHeap1.start && addr < (byte*)EphemeralHeap1.end)
	{
		UnWriteProtectPage(address_to_page((ULONG32)addr));
	}
	else if (addr >= (byte*)EphemeralHeap2.start && addr < (byte*)EphemeralHeap2.end)
	{
		if (!EphemeralHeap2.pageCommitted(addr))
			EphemeralHeap2.commitPage(addr, 0);
		UnWriteProtectPage(address_to_page((ULONG32)addr));
	}
	else if (addr >= (byte*)LispHeap1.start && addr < (byte*)LispHeap1.end)
	{
		if (!LispHeap1.pageCommitted(addr))
			LispHeap1.commitPage(addr, 0);
		UnWriteProtectPage(address_to_page((ULONG32)addr));
	}
	else
		return FALSE;

    return TRUE;
}

BOOL inEphemeralHeap1AddressRange(unsigned long addr)
{
    return ((Node*)addr >= EphemeralHeap1.start && (Node*)addr < EphemeralHeap1.end);
}

BOOL inEphemeralHeap2AddressRange(unsigned long addr)
{
    return ((Node*)addr >= EphemeralHeap2.start && (Node*)addr < EphemeralHeap2.end);
}

BOOL inLispHeap1AddressRange(unsigned long addr)
{
    return ((Node*)addr >= LispHeap1.start && (Node*)addr < LispHeap1.end);
}

BOOL inLispHeap2AddressRange(unsigned long addr)
{
    return ((Node*)addr >= LispHeap2.start && (Node*)addr < LispHeap2.end);
}

unsigned long tranlateToOtherPrimaryHeap(unsigned long addr)
{
    return addr - ((unsigned long)LispHeap2.start) + ((unsigned long)LispHeap1.start);
}

// needs to be called at lisp startup.
// This should leave:
//		SysGlobals:		committed, read/write
//		EphemeralHeap1:	committed, read/write
//		EphemeralHeap2:	reserved, not committed, read-only
//		LispHeap1:		reserved, not committed, read-only
//		LispHeap2:		reserved, not-committed, read/write
//
void initializeGarbageCollector()
{
    SysGlobals.alloc(SysGlobalsSize, 0, 0, 0x80000);
    SysGlobals.commitAllPages();

    if (EphemeralHeap2Size > EphemeralHeap1Size)
        EphemeralHeap2Size = EphemeralHeap1Size;    // keep EphemeralHeap2 size <= EphemeralHeap1Size

    EphemeralHeap1.alloc(EphemeralHeap1Size, 0, 0, EphemeralHeap1Size);
    EphemeralHeap2.alloc(EphemeralHeap1Size + EphemeralHeap2Size, EphemeralHeap2Size, 1,
                EphemeralHeap1Size + EphemeralHeap2Size);
    LispHeap1.alloc(EphemeralHeap1Size + EphemeralHeap2Size + LispHeapSize, LispHeapSize, 2,
                LispHeapReserveDefault);	// try to reserve 256 meg area
    LispHeap2.alloc(EphemeralHeap1Size + EphemeralHeap2Size + LispHeapSize, LispHeapSize, 2,
                LispHeapReserveDefault);

    SysGlobalsAddr = (int)SysGlobals.start;
    PageTable = allocPageTable();

    ClearPageTable();
    EphemeralHeap1.commitAllPages();
    EphemeralHeap2.commitAllPages();
    EphemeralHeap2.writeProtectAllPages();
    EphemeralHeap2.decommitTrailingPages();
    LispHeap1.commitAllPages();
    LispHeap1.writeProtectAllPages();
    LispHeap1.decommitTrailingPages();
    LispHeap2.decommitAllPages();
}

// all values starting with index (which should be even!)
// are to be considered untagged.
// Uvectors of length 1 are never registered, even if the
// single cell is untagged, because it can never span another page.
//
//	Because all new heap blocks are allocated from EphemeralHeap1,
//	we assume the uvector is in that heap.

void registerUntaggedValues(LispObj uvec, LispObj index)
{
    unsigned long i = 0;

    // see if the uvector spans a page
    unsigned long byteOffset = (index >> 1);
    unsigned long page = address_to_page(uvec + byteOffset);
    unsigned long offset = page_offset(uvec + byteOffset); // range 0 - 511
    byteOffset = (uvectorSize(uvec) << 3);
    unsigned long endpage = address_to_page(uvec + byteOffset);
    unsigned long endoffset = page_offset(uvec + byteOffset);
    if (page != endpage || offset == 0)
    {
        i = page;
        if (offset != 0) i++;
        for (; i < endpage; i++)
            SetPageOffset(i, 512);
        SetPageOffset(i, (short)endoffset);
    }
}

// expects GCToSpace to represent the destination heap
void handleUntaggedValues(LispObj uvec, LispObj index)
{
    unsigned long i = 0;

    // see if the uvector spans a page
    unsigned long startPage = address_to_page(uvec);
    unsigned long byteOffset = (index >> 1);
    unsigned long page = address_to_page(uvec + byteOffset);
    unsigned long offset = page_offset(uvec + byteOffset); // range 0 - 511
    byteOffset = (uvectorSize(uvec) << 3);
    unsigned long endpage = address_to_page(uvec + byteOffset);
    unsigned long endoffset = page_offset(uvec + byteOffset);

    // as a special case, if the object is an array of untagged
    // values or a compiled code buffer, and it started on the
    // page previous to the untagged data, cause scanning to start
    // at the beginning of the block by storing a negative block offset.
    // This is a bit of a hack, but is the only immediate way I can see
    // to get the block scanning in sync with the data without storing
    // more information with each page.
    if (startPage != page && offset != 0 &&
            ((isSimpleVector(uvec) && !isSimpleGenericVector(uvec))
                || isCompiledCode(uvec)))
    {
        SetPageOffset(page, (short)(page_offset(uvec) - 512));	// this will be negative
    }

    if (page != endpage || offset == 0)
    {
        i = page;
        if (offset != 0) i++;
        for (; i < endpage; i++)
            SetPageOffset(i, 512);
        SetPageOffset(i, (short)endoffset);
    }
}

static int GarbageEntry = 0;
static int GarbageCollectionLevel = 0;

static __int64 GCTime1;
static __int64 GCTime2;
static __int64 GCTimeElapsed;
static __int64 GCTimeTotal;

bool inAnyLispHeap(DWORD addr)
{
    return EphemeralHeap1.inHeap((byte*)addr)
                    || EphemeralHeap2.inHeap((byte*)addr)
                    || LispHeap1.inHeap((byte*)addr);
}

int inSysGlobalsHeap(DWORD addr)
{
    return SysGlobals.inHeap((byte*)addr);
}

static unsigned long* gBasePointer = 0;
static LispObj* gStackEnd = 0;
unsigned long garbageCollectionID = 0;

static long gcHandleStructuredException(long exception, LPEXCEPTION_POINTERS info);
void garbageCollect(long level)
{
    HANDLE process = 0;
    BOOL ret = 0;

    Node* temp = 0;		// careful--unwrapped pointer!
    Node* mark1 = 0;
    Node* mark2 = 0;
    LispObj t1 = 0;
    LispObj t2 = 0;
    LispObj bn = 0;
	LispObj gc_id = 0;
    byte* mem = 0;
    HWND wnd = 0;
    BOOL criticalSectionEntered = 0;
    int threadsSuspended = 0;

    GCCriticalSection.Enter();
    
   __try
    {

        __try
        {

            garbageCollectionID++;		// increment level

            // store in static variable so called functions can access
            GarbageCollectionLevel = level;

            if (GCFailure)
                _endthreadex(1);

            if (symbolValue(HEAP_CHECKING) != NIL)
                verifyHeapBlocks();

            if (GarbageEntry > 0)
            {
                // dump information about the problem
        	    WriteMemoryReportTask((void*)(-2), 0);

                CormanLispServer->GetAppMainWindow(&wnd);

                char* msg = "A problem has occurred during garbage collection.\n"
                                "This may be because the heap has become corrupted, \n"
                                "or some other system failure has occurred. \n"
                                "All lisp threads will be exited and you will need to restart \n"
                                "Corman Lisp to continue executing Lisp code.\n"
                                "A system report has been written to the file 'CormanLisp_Dump.log'.";
                GCFailure = true;
                CormanLispServer->LispShutdown(msg, strlen(msg));
            }

 //           GCCriticalSection.Enter();
 //           criticalSectionEntered = TRUE;

            GarbageEntry++;

			// save current ebp, esp values
			__asm mov	gBasePointer, ebp
			//__asm mov	gStackEnd, esp

            // link back to the values before entry into this function
            gStackEnd = gBasePointer;
            gBasePointer = *(unsigned long**)gBasePointer;

            QueryPerformanceCounter((LARGE_INTEGER*)&GCTime1);

            // suspend all threads but the current one
            __try
            {
                ThreadList.suspendAllOtherThreads();
                threadsSuspended = 1;

                // Make sure none of the other threads are in lisp atomic units.
                ThreadList.ensureSafeStates();

                process = GetCurrentProcess();
                ret = FlushInstructionCache(process, NULL, 0);

                EphemeralHeap2.commitTrailingPages(0);
                EphemeralHeap2.unWriteProtectAllPages();

                resurrectedObjects = 0;

                // copy any live data from Ephemeral1 to Ephemeral2
                mark1 = EphemeralHeap2.current;	// save this position
                checkGlobalRoots(&EphemeralHeap1, &EphemeralHeap2);	// check all global roots
                checkStackRoots(&EphemeralHeap1, &EphemeralHeap2);	// check stack and registers
                checkHeapRoots(&EphemeralHeap1, &EphemeralHeap2, &LispHeap1, LispHeap1.start, LispHeap1.current);
                checkHeapRoots(&EphemeralHeap1, &EphemeralHeap2, &EphemeralHeap2, EphemeralHeap2.start, mark1);
                EphemeralHeap1.reset();

                // check all the blocks found so far
                copyReferencedBlocks(&EphemeralHeap1, &EphemeralHeap2, mark1);

                // now check the finalization registry
                mark1 = EphemeralHeap2.current;	// save this position
                resurrectFinalizationObjects(&EphemeralHeap1, &EphemeralHeap2);
                copyReferencedBlocks(&EphemeralHeap1, &EphemeralHeap2, mark1);

                // At this point, all live data from Ephemeral1 should have
                // been copied to Ephemeral2. If Ephemeral2 overflowed, we need
                // to do the same process to it.
                if (EphemeralHeap2.current > EphemeralHeap2.overflow || level > 0)
                {
                    if (level == 0)
                    {
                        level++;
                        GarbageCollectionLevel = level;
                    }
                    LispHeap1.commitTrailingPages(0);
                    LispHeap1.unWriteProtectAllPages();

                    // grow the primary heap if necessary
                    if (((EphemeralHeap2.current - EphemeralHeap2.start)
                        + LispHeap1.current) >= LispHeap1.end)
                    {
                        growLispHeaps(EphemeralHeap2.current - EphemeralHeap2.start); // need to grow now
                    }

                    mark2 = LispHeap1.current;
                    checkGlobalRoots(&EphemeralHeap2, &LispHeap1);
                    checkStackRoots(&EphemeralHeap2, &LispHeap1);
                    checkHeapRoots(&EphemeralHeap2, &LispHeap1, &LispHeap1, LispHeap1.start, mark2);

                    // check all the blocks found so far
                    copyReferencedBlocks(&EphemeralHeap2, &LispHeap1, mark2);	// check all the blocks found so far
                    EphemeralHeap2.reset();

                    // clear all the EphemeralHeap2 offsets
                    // now check the finalization registry
                    mark2 = LispHeap1.current;	// save this position
                    resurrectFinalizationObjects(&EphemeralHeap2, &LispHeap1);
                    copyReferencedBlocks(&EphemeralHeap2, &LispHeap1, mark2);

                    // Now all live data should be copied into the permanent heap.
                    // If the permanent heap has reached overflow capacity, do a collection
                    // on it.
                    if (LispHeap1.current > LispHeap1.overflow || level > 1)
                    {
                        if (level == 1)
                        {
                            level++;
                            GarbageCollectionLevel = level;
                        }

                        LispHeap2.commitAllPages();

                        checkGlobalRoots(&LispHeap1, &LispHeap2);
                        checkStackRoots(&LispHeap1, &LispHeap2);

                        // check all the blocks found so far
                        copyReferencedBlocks(&LispHeap1, &LispHeap2, LispHeap2.start);

                        // now check the finalization registry
                        mark2 = LispHeap2.current;	// save this position
                        resurrectFinalizationObjects(&LispHeap1, &LispHeap2);
                        copyReferencedBlocks(&LispHeap1, &LispHeap2, mark2);

                        // exchange pointers
                        swapLispHeaps();

                        ret = LispHeap2.decommitAllPages();
                        assert(ret);

                        // make sure the data is at the lowest real heap in physical memory
                        // if the level requested is level 3 or greater
                        if (LispHeap1.start > LispHeap2.start && level > 2)
                        {
                            if (level == 2)
                            {
                                level++;
                                GarbageCollectionLevel = level;
                            }

                            ret = LispHeap2.commitAllPages();
                            assert(ret);

                            checkGlobalRoots(&LispHeap1, &LispHeap2);
                            checkStackRoots(&LispHeap1, &LispHeap2);

                            // check all the blocks found so far
                            copyReferencedBlocks(&LispHeap1, &LispHeap2, LispHeap2.start);

                            // now check the finalization registry
                            mark2 = LispHeap2.current;	// save this position
                            resurrectFinalizationObjects(&LispHeap1, &LispHeap2);
                            copyReferencedBlocks(&LispHeap1, &LispHeap2, mark2);

                            // exchange pointers
                            swapLispHeaps();

                            ret = LispHeap2.decommitAllPages();
                            assert(ret);
                        }
                    }
                }

                EphemeralHeap2.writeProtectAllPages();
                EphemeralHeap2.decommitTrailingPages();
                if (level > 0)
                {
                    if (LispHeap1.current >= LispHeap1.overflow)
                    {
                        growLispHeaps(EphemeralHeap2.end - EphemeralHeap2.start); // need to grow the heap
                    }

                    LispHeap1.writeProtectAllPages();
                    LispHeap1.decommitTrailingPages();
                }

                releaseWeakPointers();
            }
            __finally
            {
				GarbageEntry--;
            }

            // These have been moved to outside the locked section--they can potentially
            // trigger another garbage collection, which is OK since they are moved here.
            executeGCRegistryFunctions();
            finalizeResurrectedObjects();

            process = 0;
            ret = 0;
            temp = 0;

            if (symbolValue(HEAP_CHECKING) != NIL)
                verifyHeapBlocks();
        }
        __except (gcHandleStructuredException(GetExceptionCode(), GetExceptionInformation()))
        {
            OutputDebugString("A structured exception occurred during garbage collection");
        }
        
        // add garbage collection time
        QueryPerformanceCounter((LARGE_INTEGER*)&GCTime2);
        GCTimeElapsed = GCTime2 - GCTime1;
		GCTimeTotal += GCTimeElapsed;
        bn = bignumNode(wrapInteger(2));
		*(__int64*)&(UVECTOR(bn)[BIGNUM_FIRST_CELL]) = GCTimeTotal;
        setSymbolValue(GC_TIME_COUNTER, bn);

	    gc_id = bignumNode(wrapInteger(1));
	    UVECTOR(gc_id)[BIGNUM_FIRST_CELL] = garbageCollectionID;
	    setSymbolValue(HEAP_0_GC_ID, gc_id);
	    if (level > 0)
		    setSymbolValue(HEAP_1_GC_ID, gc_id);
	    if (level > 1)
		    setSymbolValue(HEAP_2_GC_ID, gc_id);

        //	dumpHeapToFile(stringNode("heapdump.txt"));
        LispCall2(Funcall, FUNCALL, EXECUTE_FINALIZERS);		// if this triggers another GC, it's OK
    }
    __finally
    {
         LeaveGCCriticalSection();
         if (threadsSuspended)
         {
             threadsSuspended = false;
            // resume all suspended threads
            ThreadList.resumeAllOtherThreads();
         }
    }
}

//
// By design, this handler will not handle write-protected heap pages. It should not encounter
// any write-protected pages during correct operation. If it does we would rather know about it
// now than allow it to continue.
static long gcHandleStructuredException(long exception, LPEXCEPTION_POINTERS info)
{
    byte* attemptedAddress = 0;
    BOOL ret = 0;
    unsigned long ip = 0;
    HWND wnd = 0;
    if (exception == EXCEPTION_ACCESS_VIOLATION)
    {
        attemptedAddress = (byte*)info->ExceptionRecord->ExceptionInformation[1];

        // dump information about the problem
        WriteMemoryReportTask(attemptedAddress, info->ContextRecord);

        // resume all suspended threads
        ThreadList.resumeAllOtherThreads();
        GCCriticalSection.Leave();

        CormanLispServer->GetAppMainWindow(&wnd);

        char* msg = "A memory access violation has occurred during garbage collection.\n"
                        "This may be because the heap has become corrupted, \n"
                        "or some other system failure has occurred. \n"
                        "All lisp threads will be exited and you will need to restart \n"
                        "Corman Lisp to continue executing Lisp code.\n"
                        "A system report has been written to the file 'CormanLisp_Dump.log'.";
        GCFailure = true;
        CormanLispServer->LispShutdown(msg, strlen(msg));

        return EXCEPTION_EXECUTE_HANDLER;
    }	
    if (exception == CONTROL_C_EXIT)
        return EXCEPTION_CONTINUE_EXECUTION;

    return EXCEPTION_EXECUTE_HANDLER;
}

// This handler will properly handle write-protected heap pages--otherwise it is the 
// same as the above.
//
static long gcHandleStructuredExceptionDuringFinalization(long exception, LPEXCEPTION_POINTERS info)
{
    byte* attemptedAddress = 0;
    BOOL ret = 0;
    unsigned long ip = 0;
    HWND wnd = 0;

    if (exception == EXCEPTION_ACCESS_VIOLATION)
    {
        attemptedAddress = (byte*)info->ExceptionRecord->ExceptionInformation[1];

        ret = handleMemoryAccessException(attemptedAddress);
        if (ret)
            return EXCEPTION_CONTINUE_EXECUTION;

        // dump information about the problem
        WriteMemoryReportTask(attemptedAddress, info->ContextRecord);

        CormanLispServer->GetAppMainWindow(&wnd);
        char* msg = "A memory access violation has occurred during the finalization stage\n"
                        "of garbage collection.\n"
                        "This may be because the heap has become corrupted, \n"
                        "or some other system failure has occurred. \n"
                        "All lisp threads will be exited and you will need to restart \n"
                        "Corman Lisp to continue executing Lisp code.\n"
                        "A system report has been written to the file 'CormanLisp_Dump.log'.";
        GCFailure = true;
        CormanLispServer->LispShutdown(msg, strlen(msg));

        return EXCEPTION_EXECUTE_HANDLER;
    }	
    if (exception == CONTROL_C_EXIT)
        return EXCEPTION_CONTINUE_EXECUTION;

    return EXCEPTION_EXECUTE_HANDLER;
}

static void
checkQVRoots(LispObj* qv)
{
    long i = 0;
    unsigned long codeAddress = 0;
    LispObj blockptr = 0;

    // toss any temporary heap remaining
    qv[THREAD_HEAP_Index] = 0;
    qv[THREAD_HEAP_END_Index] = 0;

    if (isHeapPointer(qv[Nil_Index]))
        promoteBlock(&qv[Nil_Index], GCToSpace);
    if (isHeapPointer(qv[T_Index]))
        promoteBlock(&qv[T_Index], GCToSpace);
    if (isHeapPointer(qv[MULTIPLE_RETURN_VALUES_Index]))
        promoteBlock(&qv[MULTIPLE_RETURN_VALUES_Index], GCToSpace);

    // finalization registry and weak pointer registry are handled
    // separately later during GC
    for (i = RETURN_VALUES_Index; i < SYSTEM_OBJ_MAX; i++)
    {
        if (isHeapPointer(qv[i]))
            promoteBlock(&qv[i], GCToSpace);
    }

    for (i = 0; i < NumJumpTableEntries; i += JumpTableCellsPerEntry)
    {
        if (isHeapPointer(qv[i + FirstJumpTableEntry]))
            promoteBlock(&qv[i + FirstJumpTableEntry], GCToSpace);// promote environment

        // now promote code
        codeAddress = (unsigned long)qv[i + FirstJumpTableEntry + 1];
        if (pointsIntoHeap(codeAddress))
        {
            // obtain a pointer to the beginning of the block
            blockptr = (LispObj)(codeAddress - (COMPILED_CODE_OFFSET * 4) + UvectorTag);
            promoteBlock(&blockptr, GCToSpace);
            qv[i + FirstJumpTableEntry + 1] = (LispObj)(UVECTOR(blockptr) + COMPILED_CODE_OFFSET);
        }
    }
    for (i = 0; i < NumSpecialSymbolEntries; i++)
    {
        if (isHeapPointer(qv[i + FirstSpecialSymbolEntry]))
            promoteBlock(&qv[i + FirstSpecialSymbolEntry], GCToSpace);// promote binding
    }
    i = 0;
    codeAddress = 0;
    blockptr = 0;
}

static void
checkGlobalRoots(LispHeap* fromSpace, LispHeap* toSpace)
{
    GCFromSpace = fromSpace;
    GCToSpace = toSpace;
    FinalizationNode* n = 0;
    Node* p = 0;
    ThreadRecord* tr = ThreadList.getList();

    checkQVRoots(QV);
    while (tr)
    {
        if (tr->started)
            checkQVRoots(tr->QV_rec);
        tr = tr->next;
    }

    // check resurrected objects
    n =	resurrectedObjects;
    while (n)
    {
        p = (Node*)stripTag(n->obj);
        if (isHeapPointer(p->car))
            promoteBlock(&(p->car), toSpace);
        if (isHeapPointer(p->cdr))
            promoteBlock(&(p->cdr), toSpace);
        if (isHeapPointer(n->obj))
            promoteBlock(&(n->obj), toSpace);
        n = n->next;
    }

    // check gc exec registry
    if (isHeapPointer(GCExecRegistry))
        promoteBlock(&GCExecRegistry, toSpace);
}


static void updateReturnAddress(unsigned long* addr)
{
    unsigned long retAddress = 0;
    Node* cell = 0;
    Node* oldAddress = 0;
    Node* newAddress = 0;
    long difference = 0;
    LispObj oaddr = 0;
    LispObj naddr = 0;

    retAddress = *addr;
    if (pointsIntoHeap(retAddress))
    {
        // find the beginning of the code block
        cell = (Node*)stripTag(retAddress);
        while (cell->cdr != COMPILED_CODE_MAGIC_ID)
            cell--;

        // see if the block has already been copied
        if (gettag(cell->car) == ForwardTag)
        {
            oldAddress = cell;
            newAddress = (Node*)stripTag(cell->car);
            difference = (long)newAddress - (long)oldAddress;
            *addr += difference;	// update return address on stack
        }
        else	// need to copy the block
        {
            oaddr = wrap((LispObj)cell, UvectorTag);
            naddr = oaddr;
            promoteBlock(&naddr, GCToSpace);
            difference = (long)naddr - (long)oaddr;
            *addr += difference;	// update return address on stack
        }
    }
}

//
//	promoteThreadExecutingFunctions()
//	This function causes all the functions currently being executed
//	to get promoted to the new heap. Return addresses on the stack are
//	updated as necessary.
static void
promoteThreadExecutingFunctions(ThreadRecord* th)
{
    static CONTEXT lispContext;
    LispObj* start = th->stackStart;
    unsigned long* basePointer = 0;
    unsigned long* esp = 0;
    LispObj runtimeInfo = 0;
    LispObj f = 0;
    LispObj* regs = 0;
    LispObj* qv = th->QV_rec;
    int index = 0;
    long inForeignCode = 0;
    int argLength = 0;

    if (start == 0)
        return;			// watch for the case where a thread has not actually started
    // we skip portions of the stack which are not tagged
    // to do this we look at every other segment
    // Every even segment should be a lisp (tagged) segment,
    index = qv[STACK_MARKER_INDEX_Index] >> 2;

    lispContext.ContextFlags = CONTEXT_CONTROL;
    GetThreadContext(th->thread, &lispContext);
    basePointer = (unsigned long*)lispContext.Ebp;
    esp = (unsigned long*)lispContext.Esp;
    updateReturnAddress((unsigned long*)&(lispContext.Eip));
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
            argLength = esp[0];
            updateReturnAddress(esp - (argLength >> 2) - 1);
            updateReturnAddress(&qv[STACK_MARKERS_Index + ((index * 2) + 1)]);
            basePointer = esp;
            while (!(looksLikeEBP((unsigned long)basePointer)))
                basePointer++;
        }
    }

    while (basePointer < start)
    {
        updateReturnAddress(&(basePointer[1]));
        basePointer = (unsigned long*)*basePointer;

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
                argLength = esp[0];
                updateReturnAddress(esp - (argLength >> 2) - 1);
                updateReturnAddress(&qv[STACK_MARKERS_Index + ((index * 2) + 1)]);
                basePointer = esp;
                while (!(looksLikeEBP((unsigned long)basePointer)))
                    basePointer++;
            }
        }

    }

    assert(basePointer == start);

    // now need to promote catch block saved return addresses
    runtimeInfo = CAR(th->QV_rec[integer(UVECTOR(COMPILER_RUNTIME)[SYMBOL_VAR_TABLE])]);
    while (isCons(runtimeInfo))
    {
        f = CAR(runtimeInfo);
        assert(isCons(f));
        if (CAR(f) == CATCH_HEADER_CODE)
        {
            regs = (unsigned long*)CAR(CDR(CDR(f)));
            updateReturnAddress(&(regs[6]));
        }
        runtimeInfo = CDR(runtimeInfo);
    }
    lispContext.ContextFlags = CONTEXT_CONTROL;
    SetThreadContext(th->thread, &lispContext);
}

//
//	It looks like a valid EBP link if
//	1) if is a multiple of 4 (all pointers into the stack should be dword aligned)
//	2) it points to an address > than it
//	3) it points to an address < it + 4096 (arbitrary upper limit--may need to increase)
//	4) what it points to should also look like an EBP link
//
unsigned long looksLikeEBP(unsigned long addr)
{
    unsigned long ret =
        (!(*(LispObj*)addr & 0x3))		// must be multiple of 4
        && (*(LispObj*)addr > addr)
        && (*(LispObj*)addr < (addr + 4096));
//	assert(!ret || looksLikeEBP(*(LispObj*)addr)); // this would recurse indefinitely
    if (ret)
    {
        ret = ((!(*(LispObj*)(*(LispObj*)addr) & 0x3))
                    && (*(LispObj*)(*(LispObj*)addr) > (*(LispObj*)addr))
                    && (*(LispObj*)(*(LispObj*)addr) < ((*(LispObj*)addr) + 4096)));
    }

//	assert(!ret || ((!(*(LispObj*)(*(LispObj*)addr) & 0x3))
//					&& (*(LispObj*)(*(LispObj*)addr) > (*(LispObj*)addr))
//					&& (*(LispObj*)(*(LispObj*)addr) < ((*(LispObj*)addr) + 1024))));
    return ret;
}

static void
promoteExecutingFunctions()
{
    ULONG32 retAddress = 0;
    Node* cell = 0;
    Node* oldAddress = 0;
    Node* newAddress = 0;
    long difference = 0;
    LispObj oaddr = 0;
    LispObj naddr = 0;
    LispObj runtimeInfo = 0;
    LispObj f = 0;
    LispObj* regs = 0;
    ThreadRecord* tr = ThreadList.getList();
    ThreadRecord* currThread = (ThreadRecord*)TlsGetValue(Thread_Index);
    LispObj* stackStart = currThread->stackStart;
    unsigned long* qv = 0;
    int index = 0;
    unsigned long* basePointer = gBasePointer;
    unsigned long* esp = 0;
    int argLength = 0;

    // we skip portions of the stack which are not tagged
    // to do this we look at every other segment
    // Every even segment should be a lisp (tagged) segment,
    qv = ThreadQV();
    index = qv[STACK_MARKER_INDEX_Index] >> 2;

    while (index > 0 &&
            ((unsigned long)basePointer) >= qv[STACK_MARKERS_Index + ((index - 1) * 2)])
        index--;
    if (!EVEN(index))		// if not a lisp segment
    {
        // this should never happen!
        // We will always be in a lisp segment of the stack
        // because this thread called the garbage collector,
        // which will only be called from lisp code.
        OutputDebugString("Error: Garbage collector called from foreign code!!");
        //DebugBreak();
    }

    while (basePointer < stackStart)
    {
        updateReturnAddress(&(basePointer[1]));
        basePointer = (unsigned long*)*basePointer;

        while (index > 0 &&
                ((unsigned long)basePointer) >= qv[STACK_MARKERS_Index + ((index - 1) * 2)])
            index--;
        if (!EVEN(index))		// if not a lisp segment
        {
            // we are into a non-lisp segment of the stack.
            // Find	the next stack frame in a lisp segment
            index--;
            esp	= (unsigned long*)qv[STACK_MARKERS_Index + (index * 2)];
            if (esp == stackStart)
                basePointer = stackStart;
            else
            {
                // search for the first index into the stack which points
                // into the next 1k region of the stack. Assume this is the
                // next base pointer.
                argLength = esp[0];
                updateReturnAddress(esp - (argLength >> 2) - 1);
                updateReturnAddress(&qv[STACK_MARKERS_Index + ((index * 2) + 1)]);
                basePointer = esp;
                while (!(looksLikeEBP((unsigned long)basePointer)))
                    basePointer++;
            }
        }
    }
    assert(basePointer == stackStart);

    // now need to promote catch block saved return addresses
    runtimeInfo = symbolValue(COMPILER_RUNTIME);
    while (isCons(runtimeInfo))
    {
        f = CAR(runtimeInfo);
        assert(isCons(f));
        if (CAR(f) == CATCH_HEADER_CODE)
        {
            regs = (unsigned long*)CAR(CDR(CDR(f)));
            retAddress = regs[6];
            updateReturnAddress(&(regs[6]));
        }
        runtimeInfo = CDR(runtimeInfo);
    }

    // check stacks of all suspended threads
    while (tr)
    {
        if (tr != currThread && tr->started)
            promoteThreadExecutingFunctions(tr);
        tr = tr->next;
    }

    retAddress = 0;
    cell = 0;
    oldAddress = 0;
    newAddress = 0;
    difference = 0;
    oaddr = 0;
    naddr = 0;
    basePointer = 0;
    regs = 0;
}

static void
checkThreadStackRoots(ThreadRecord* th, LispHeap* /*fromSpace*/, LispHeap* toSpace)
{
    static CONTEXT lispContext;
    LispObj* start = th->stackStart;
    LispObj* x = 0;
    LispObj regs[6] = {0};
    int i = 0;

    lispContext.ContextFlags = CONTEXT_FULL;
    GetThreadContext(th->thread, &lispContext);
    LispObj* end = (LispObj*)lispContext.Esp;
    LispObj* qv = th->QV_rec;
    int index = 0;

    // we skip portions of the stack which are not tagged
    // to do this we look at every other segment
    // Every even segment should be a lisp (tagged) segment,
    index = qv[STACK_MARKER_INDEX_Index] >> 2;
    if (index > 0)
    {
        i = 0;
        for (x = start; x >= end; x--)
        {
			if (EVEN(i) && *(unsigned long*)x == JumpBufferMarker)
            {
                promoteJumpBufferBlocks(x, toSpace);
                x -= ((JumpBufferSize / 4) - 1);
            }
			else
            if (EVEN(i) && isHeapPointer(*x))
            {
                promoteBlock(x, toSpace);
            }
            if ((i < index) && ((unsigned long)x) <= qv[(i * 2) + STACK_MARKERS_Index])
                i++;
        }
    }
    else
    {
        for (x = start; x >= end; x--)
        {
            if (*(unsigned long*)x == JumpBufferMarker)
            {
                promoteJumpBufferBlocks(x, toSpace);
                x -= ((JumpBufferSize / 4) - 1);
            }
            else
            if (isHeapPointer(*x))
            {
                promoteBlock(x, toSpace);
            }
        }
    }
    // only update registers if we are running in lisp (not foreign) code
    // We determine this based on the region of the top of the stack.
    if (EVEN(i))
    {
        // check registers
        regs[0] = (LispObj)lispContext.Eax;
        regs[1] = (LispObj)lispContext.Ebx;
        regs[2] = (LispObj)lispContext.Ecx;
        regs[3] = (LispObj)lispContext.Edx;
        regs[4] = (LispObj)lispContext.Esi;
        regs[5] = (LispObj)lispContext.Edi;
        for (i = 0; i < 6; i++)
        {
            if (isHeapPointer(regs[i]))
                promoteBlock(&regs[i], toSpace);
        }
        lispContext.Eax = (DWORD)regs[0];
        lispContext.Ebx = (DWORD)regs[1];
        lispContext.Ecx = (DWORD)regs[2];
        lispContext.Edx = (DWORD)regs[3];
        lispContext.Esi = (DWORD)regs[4];
        lispContext.Edi = (DWORD)regs[5];

        lispContext.ContextFlags = CONTEXT_FULL;
        SetThreadContext(th->thread, &lispContext);
    }
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
// On entry, x points to the marker.
//
static void promoteJumpBufferBlocks(LispObj* x, LispHeap* toSpace)
{
    x -= 4;			// x -> EDI
    if (isHeapPointer(*x))
        promoteBlock(x, toSpace);
    x -= 2;			// x -> EDX
    if (isHeapPointer(*x))
        promoteBlock(x, toSpace);
    x--;			// x -> ECX
    if (isHeapPointer(*x))
        promoteBlock(x, toSpace);
    x--;			// x -> EBX
    if (isHeapPointer(*x))
        promoteBlock(x, toSpace);
#if 0	// don't need to check these
    if (isHeapPointer(*(x + 3)))
        promoteBlock((x + 3), toSpace);
    if (isHeapPointer(*(x + 5)))
        promoteBlock((x + 5), toSpace);
    if (isHeapPointer(*(x + 6)))
        promoteBlock((x + 6), toSpace);
    if (isHeapPointer(*(x + 7)))
        promoteBlock((x + 7), toSpace);
#endif
}

const int ForeignStackHeader = UvectorTag + (ForeignStackType << 3);

static void
checkStackRoots(LispHeap* fromSpace, LispHeap* toSpace)
{
    ThreadRecord* currThread = (ThreadRecord*)TlsGetValue(Thread_Index);
    LispObj* stackStart = currThread->stackStart;
    LispObj dummy = 0;
    LispObj* start = stackStart;
    LispObj* x = 0;
    LispObj regs[6] = {0};
    ThreadRecord* tr = ThreadList.getList();
    int i = 0;
    unsigned long* qv = 0;
    int index = 0;

    GCFromSpace = fromSpace;
    GCToSpace = toSpace;

//	if (isHeapPointer(NIL))
//		promoteBlock(&NIL, toSpace);	   // make sure NIL is the first block

    // first deal with the execution stack
    promoteExecutingFunctions();

    // we skip portions of the stack which are not tagged
    // to do this we look at every other segment
    // Every even segment should be a lisp (tagged) segment,
    qv = ThreadQV();
    index = qv[STACK_MARKER_INDEX_Index] >> 2;
    if (index > 0)
    {
        i = 0;
        for (x = start; x >= gStackEnd; x--)
        {
            if (EVEN(i))
            {
                if (*(unsigned long*)x == JumpBufferMarker)
                {
                    promoteJumpBufferBlocks(x, toSpace);
                    x -= ((JumpBufferSize / 4) - 1);
                }
                else
                if (isHeapPointer(*x))
                    promoteBlock(x, toSpace);
            }
            if ((i < index) && ((unsigned long)x) <= qv[(i * 2) + STACK_MARKERS_Index])
                i++;
        }
    }
    else
    {
        for (x = start; x >= gStackEnd; x--)
        {
            if (*(unsigned long*)x == JumpBufferMarker)
            {
                promoteJumpBufferBlocks(x, toSpace);
                x -= ((JumpBufferSize / 4) - 1);
            }
            else
            if (isHeapPointer(*x))
                promoteBlock(x, toSpace);
        }
    }

    // only update registers if we are running in lisp (not foreign) code
    // We determine this based on the region of the top of the stack.
    if (EVEN(i))
    {

        // check registers -- processor specific
        __asm mov regs[0], eax
        __asm mov regs[4], ebx
        __asm mov regs[8], ecx
        __asm mov regs[12], edx
        __asm mov regs[16], esi
        __asm mov regs[20], edi

        for (i = 0; i < 6; i++)
        {
            if (isHeapPointer(regs[i]))
                promoteBlock(&regs[i], toSpace);
        }

        __asm mov eax, regs[0]
        __asm mov ebx, regs[4]
        __asm mov ecx, regs[8]
        __asm mov edx, regs[12]
        __asm mov esi, regs[16]
        __asm mov edi, regs[20]
    }

    // check stacks of all suspended threads
    while (tr)
    {
        if (tr != currThread && tr->started)
            checkThreadStackRoots(tr, fromSpace, toSpace);
        tr = tr->next;
    }

    dummy = 0;
    start = 0;
    x = 0;
    regs[0] = regs[1] = regs[2] = regs[3] = regs[4] = regs[5] = 0;
}

static Node*
checkObject(Node* p)
{
    LispObj uvec = 0;
    LispObj* u = 0;
    long type = 0;
    long numcells = 0;
    long i = 0;
    LispObj table = 0;
    LispObj* pp = 0;
    byte* code = 0;
    long numEntries = 0;
    LispObj* referenced = 0;
    unsigned short* us = 0;
    long offset = 0;

    // params
//	GCFromSpace = fromSpace;
//	GCToSpace = toSpace;
//	GCCheckCode

    if (gettag(p->car) == UvectorLengthTag)
    {
        // we have a Uvector--only search tagged data
        uvec = wrap((LispObj)p, UvectorTag);
        numcells = uvectorSize(uvec) * 2;
        assert (numcells >= 1 && numcells <= MAX_CELLS_PER_ARRAY);

        u = UVECTOR(uvec);

        type = (u[0] >> 3) & 0x1f;
        switch (type)
        {
        case ArrayType:
            if (isHeapPointer(u[ARRAY_VECTOR]))
                promoteBlock(&u[ARRAY_VECTOR], GCToSpace);
            break;

        case DoubleFloatType:
        case SingleFloatType:
        case BignumType:
        case ForeignType:
        case ForeignHeapType:
        case WeakPointerType:
        case SimpleCharVectorType:
        case SimpleByteVectorType:
        case SimpleShortVectorType:
        case SimpleDoubleFloatVectorType:
        case SimpleSingleFloatVectorType:
        case SimpleBitVectorType:
            break;

        case CompiledCodeType:
            // when checking for roots, skip code scan because
            // compiled code can only contain pointers to older
            // objects i.e. no way to destructively change to point
            // to newer objects
            if (GCCheckCode)
            {
                if (isHeapPointer(u[COMPILED_CODE_REFERENCES]) || isFixnum(u[COMPILED_CODE_REFERENCES]))
                {
                    promoteBlock(&u[COMPILED_CODE_REFERENCES], GCToSpace);

                    // update the code references
                    if (isUvector(u[COMPILED_CODE_REFERENCES]))
                    {
                        table = u[COMPILED_CODE_REFERENCES];
                        pp = arrayStart(table);
                        numEntries = arrayDimension(table, 0) / 2;
                        code = (byte*)(u + COMPILED_CODE_OFFSET);
                        for (i = 0; i < numEntries; i++)
                        {
                            referenced = (LispObj*)(code + integer(pp[i * 2 + 1]));
                            if (isHeapPointer(*referenced))
                                promoteBlock(referenced, GCToSpace);
                        }
                    }
                    else
                    if (isFixnum(u[COMPILED_CODE_REFERENCES]) && u[COMPILED_CODE_REFERENCES] > 0)
                    {
                        // the references are stored following the code
                        // u[COMPILED_CODE_REFERENCES] = offset from start of code to reference header
                        // Reference tags are 16 bit non-tagged integers, each representing an offset
                        // from the previous reference.
                        // Byte                Purpose
                        // ----                -------
                        // 00-01				unsigned number of references (max 65535)
                        // 02-03			   1st reference (unsigned offset from code start)
                        // 04-05			   2nd reference (unsigned offset from 1st reference)
                        // etc.
                        code = (byte*)(u + COMPILED_CODE_OFFSET);
                        us = (unsigned short*)(code + integer(u[COMPILED_CODE_REFERENCES]));
                        numEntries = (long)(unsigned long)(*us);
                        us++;
                        offset = 0;
                        for (i = 0; i < numEntries; i++)
                        {
                            offset += us[i];
                            referenced = (LispObj*)(code + offset);
                            if (isHeapPointer(*referenced))
                                promoteBlock(referenced, GCToSpace);
                        }
                    }
                }
                if (isHeapPointer(u[COMPILED_CODE_PROPERTIES]))
                {
                    promoteBlock(&u[COMPILED_CODE_PROPERTIES], GCToSpace);
                }
            }
            break;

        case StructureType:
            if (isVector(structTemplate(uvec)) && arrayStart(structTemplate(uvec))[0] == HASH_TABLE)
            {
                // mark hash table
                handleMemoryAccessException((byte*)&(u[2])); // need to ensure page is committed and not write-protected
                u[2] = wrapInteger(GarbageCollectionLevel);
            }
        // purposely fall through here
        default:
            {
                for (i = 1; i < numcells; i++)
                {
                    if (isHeapPointer(u[i]))
                        promoteBlock(&u[i], GCToSpace);
                }
            }
            break;
            ;
        }
        return (p + uvectorSize(uvec));
    }
    else
    {
        // we have a cons cell
        if (isHeapPointer(p->car))
            promoteBlock(&p->car, GCToSpace);
        if (isHeapPointer(p->cdr))
            promoteBlock(&p->cdr, GCToSpace);
        return (p + 1);
    }
}

static Node*
scanObjectForRoots(Node* p, long* updated)
{
    LispObj uvec = 0;
    LispObj* u = 0;
    long type = 0;
    long numcells = 0;
    long i = 0;
    *updated = 0;

    // params
//	GCFromSpace = fromSpace;
//	GCToSpace = toSpace;
//	GCCheckCode

    if (gettag(p->car) == UvectorLengthTag)
    {
        // we have a Uvector--only search tagged data
        uvec = wrap((LispObj)p, UvectorTag);
        numcells = uvectorSize(uvec) * 2;
        assert(numcells >= 1 && numcells <= MAX_CELLS_PER_ARRAY);
        u = UVECTOR(uvec);

        type = (u[0] >> 3) & 0x1f;
        switch (type)
        {
        case ArrayType:
            if (isHeapPointer(u[ARRAY_VECTOR]))
            {
                promoteBlock(&u[ARRAY_VECTOR], GCToSpace);
                *updated = 1;
            }
            break;

        case DoubleFloatType:
        case SingleFloatType:
        case BignumType:
        case ForeignType:
        case CompiledCodeType:
        case SimpleCharVectorType:
        case SimpleByteVectorType:
        case SimpleShortVectorType:
        case SimpleDoubleFloatVectorType:
        case SimpleSingleFloatVectorType:
        case SimpleBitVectorType:
            break;

        case StructureType:
            if (isVector(structTemplate(uvec)) && arrayStart(structTemplate(uvec))[0] == HASH_TABLE)
            {
                // mark hash table
                handleMemoryAccessException((byte*)&(u[2])); // need to ensure page is committed and not write-protected
                u[2] = wrapInteger(GarbageCollectionLevel);
            }
        // purposely fall through here
        default:
            {
                for (i = 1; i < numcells; i++)
                {
                    if (isHeapPointer(u[i]))
                    {
                        promoteBlock(&u[i], GCToSpace);
                        *updated = 1;
                    }
                }
            }
            break;
            ;
        }
        return (p + uvectorSize(uvec));
    }
    else
    {
        // we have a cons cell
        if (isHeapPointer(p->car))
        {
            promoteBlock(&p->car, GCToSpace);
            *updated = 1;
        }
        if (isHeapPointer(p->cdr))
        {
            promoteBlock(&p->cdr, GCToSpace);
            *updated = 1;
        }
        return (p + 1);
    }
}


// returns TRUE if any references were found and updated,
// returns FALSE if no references were found
static BOOL scanPageForRoots(unsigned long page_id)
{
    Node* p = 0;
    Node* end = 0;
    long updated = 0;
    long offset = PageOffset(page_id);
    if (offset < CELLS_PER_PAGE)
    {
        p = (Node*)page_address(page_id);
        end = p + CELLS_PER_PAGE;
        p += offset;
        while (p < end)
            p = scanObjectForRoots(p, &updated);

        // if we were promoting EH1 references to EH2
        if (GCFromSpace->generation == 0)
        {
            SetPageReferencesEH2(page_id);
            ClrPageReferencesEH1(page_id);
        }
        else
        // if we were looking for EH2 references
        if (GCFromSpace->generation == 1)
        {
            if (updated)
                SetPageReferencesEH2(page_id);
            else
                ClrPageReferencesEH2(page_id);
        }
        assert(GCToSpace->generation != 0);
    }
    return updated;
}

// returns TRUE if any references were found and updated,
// returns FALSE if no references were found
static BOOL scanPartialPage(unsigned long page_id, Node* end)
{
    Node* p = 0;
    long updated = 0;
    long offset = PageOffset(page_id);
    if (offset < CELLS_PER_PAGE)
    {
        p = (Node*)page_address(page_id);
        p += offset;
        while (p < end)
            p = scanObjectForRoots(p, &updated);
    }
    SetPageReferencesEH2(page_id);
    SetPageReferencesEH1(page_id);

    return updated;
}

void
copyReferencedBlocks(LispHeap* fromSpace, LispHeap* toSpace, Node* start)
{
    GCFromSpace = fromSpace;
    GCToSpace = toSpace;
    GCScanSpace = toSpace;
    GCCheckCode = TRUE;

    Node* p = start;
    while (p < toSpace->current)
    {
        p = checkObject(p);
    }
}


static void
checkHeapRoots(LispHeap* fromSpace, LispHeap* toSpace, LispHeap* checkHeap, Node* start, Node* end)
{
    GCFromSpace = fromSpace;
    GCToSpace = toSpace;
    GCScanSpace = checkHeap;
    GCCheckCode = FALSE;
    ULONG32 start_page_id	= address_to_page(start);
    ULONG32 end_page_id	= address_to_page(end);
    ULONG32 i = 0;
    ULONG32 flag_bits;

    if (fromSpace->generation == 0)
    {
        for (i = start_page_id; i < end_page_id; i++)
        {
            flag_bits = PageFlags(i);
            if (!(flag_bits & Protected_Bit) || (flag_bits & EH1_Bit))
                scanPageForRoots(i);
        }
    }
    else
    if (fromSpace->generation == 1)
    {
        for (i = start_page_id; i < end_page_id; i++)
        {
            flag_bits = PageFlags(i);
            if (!(flag_bits & Protected_Bit) || (flag_bits & EH2_Bit))
                scanPageForRoots(i);
        }
    }
    else
        assert(0);

    // handle partial last page
    scanPartialPage(i, end);
}

// if the object crossed a page boundary, make sure
// the tagged offset is set correctly
static void
crossesPageBoundary(LispObj uvec)
{
    if (isSimpleVector(uvec) && !isSimpleGenericVector(uvec))
    {
        handleUntaggedValues(uvec, wrapInteger(2));
    }
    else
    if (isKFunction(uvec))
    {
        handleUntaggedValues(uvec, wrapInteger(2));
    }
    else
    if (isDoubleFloat(uvec))
    {
        handleUntaggedValues(uvec, wrapInteger(2));
    }
    else
    if (isCompiledCode(uvec))
    {
        handleUntaggedValues(uvec, wrapInteger(4));
    }
    else
    if (isBignum(uvec))
    {
        handleUntaggedValues(uvec, wrapInteger(2));
    }
}

// some extra consistency checking
// Returns 1 if valid, 0 otherwise.
int checkVerifyUvector(LispObj obj)
{
    if (!isUvectorHeader(UVECTOR(obj)[0]))
        return 0;
    unsigned long length = uvectorSize(obj);
    unsigned long type = uvectorType(obj);
    if (UVECTOR(obj)[1] == UNINITIALIZED)
        return 1;	// probably still being constructed
    switch (type)
    {
    case SymbolType:
        if (length != 5)
            return 0;
        break;
    case FunctionType:
        if (length != 2)
            return 0;
        break;
    case KFunctionType:
        if (length != 2)
            return 0;
        break;
    case StructureType:
    case ArrayType:
        break;
    case StreamType:
        if (length != 11)
            return 0;
        break;
    case DoubleFloatType:
        if (length != 2)
            return 0;
        break;
    case PackageType:
        if (length != 5)
            return 0;
        break;
    case HashtableType:
        if (length != 3)
            return 0;
        break;
    case ForeignType:
        if (length != 1)
            return 0;
        break;
    case CompiledCodeType:
        break;
    case ReadTableType:
        if (length != 3)
            return 0;
        break;
    case ComplexType:
        if (length != 2)
            return 0;
        break;
    case RatioType:
        if (length != 2)
            return 0;
        break;
    case BignumType:
        if (length < (((UVECTOR(obj)[1] >> 4) + 2 + 1) / 2))
            return 0;
        break;
    case ForeignHeapType:
        if (length != 2)
            return 0;
        break;
    case WeakPointerType:
        if (length != 1)
            return 0;
        break;
    case SimpleVectorType:
        if (length != (((UVECTOR(obj)[1] >> 3) + 2 + 1) / 2))
            return 0;
        break;
    case SimpleCharVectorType:
        if (length != (((((UVECTOR(obj)[1] >> 3) + 1) / 2) + 2 + 1) / 2))
            return 0;
        break;
    case SimpleByteVectorType:
        if (length != (((((UVECTOR(obj)[1] >> 3) + 3) / 4) + 2 + 1) / 2))
            return 0;
        break;
    case SimpleShortVectorType:
        if (length != (((((UVECTOR(obj)[1] >> 3) + 1) / 2) + 2 + 1) / 2))
            return 0;
        break;
    case SimpleDoubleFloatVectorType:
    case SimpleBitVectorType:
    case SimpleSingleFloatVectorType:
    case SingleFloatType:
        if (length != 1)
            return 0;
        break;
    case CLOSInstanceType:
        if (length != 2)
            return 0;
        break;
    case ForeignStackType:
        if (length != 2)
            return 0;
        break;
    case ForeignStackEndType:
        break;
    }
    return 1;
}

void
promoteBlock(LispObj* ptr, LispHeap* toSpace)
{
    LispObj obj = *ptr;							// object we are promoting
    byte* destAddr = (byte*)toSpace->current;	// where it is moving to
    LispObj* srcAddr = (LispObj*)stripTag(obj);	// current address of object

    if (!isHeapBlock(obj))
        return;

    // If the referenced block as already been promoted,
    // a forwarding pointer will be located at the start of
    // the block. If this is the case, simply update the passed
    // pointer to point to the new block and return.
    if (isForwardingPtr(*srcAddr))
    {
        *ptr = wrap(stripTag(*srcAddr), gettag(obj));
        return;
    }

    // move block obj to new heap
    if (isUvector(obj))
    {
        long numNodes = uvectorSize(obj);
        if (HeapChecking)
        {
            if (checkVerifyUvector(obj) == 0)
            {
                char* message = "Invalid heap object detected.";
                CormanLispServer->OutputText(message, strlen(message));
            }
        }

        LispObj* pp = UVECTOR(obj);
        // rgc
        if (!((gettag(*pp) == UvectorLengthTag) && numNodes > 0 && numNodes <= MAX_CELLS_PER_ARRAY/2))
        {
            OutputDebugString("Error: Invalid uvector detected by garbage collector!!\n");
            //DebugBreak();
        }
        //assert((gettag(*pp) == UvectorLengthTag)
        //		&& numNodes > 0 && numNodes <= MAX_CELLS_PER_ARRAY / 2);

        __asm
        {
                push eax
                push ecx
                push edi
                push esi
                mov esi, dword ptr pp
                mov edi, dword ptr destAddr
                mov	ecx, dword ptr numNodes
                dec ecx

            loop1:
                mov	eax, dword ptr [esi + ecx*8]
                mov	dword ptr [edi + ecx *8], eax
                mov	eax, dword ptr [esi + ecx*8 + 4]
                mov	dword ptr [edi + ecx*8 + 4], eax
                dec ecx
                jge loop1

                pop esi
                pop edi
                pop ecx
                pop eax
        }

        toSpace->current += numNodes;

        // if the object crossed a page boundary, make sure
        // the tagged offset is set correctly
        if (address_to_page((ULONG32)destAddr) != address_to_page((ULONG32)toSpace->current))
            crossesPageBoundary(((LispObj)destAddr) + UvectorTag);
    }
    else
    {
        assert(isCons(obj));	// Invalid address passed to promoteBlock() during garbage collection
        toSpace->current->car = CAR(obj);
        toSpace->current->cdr = CDR(obj);
        toSpace->current++;
    }

    *srcAddr = ((LispObj)destAddr) + ForwardTag;// leave forwarding pointer
    *ptr = ((LispObj)destAddr) + gettag(obj);	// update pointer
}

static int pointsIntoAlternateHeap(LispObj p)
{
    return (p >= (unsigned long)LispHeap2.start
        && p < (unsigned long)LispHeap2.end);
}

static int mayBeForwardingPointer(LispObj p)
{
    return isForwardingPtr(p) &&
        ((p >= (unsigned long)LispHeap2.start
        && p < (unsigned long)LispHeap2.end)
        || (p >= (unsigned long)LispHeap1.start
            && p < (unsigned long)LispHeap1.end));
}

//
//	flushEphemeralHeaps()
//	Make sure any ephemeral heaps are empty, and the live data
//	is all in permanent Heap 1.
//
void
flushEphemeralHeaps()
{
    garbageCollect(3);	// clears out all heaps
}

//
//	Returns the total number of bytes the heap is currently capable
//	of storing.
//
LispObj
Heap1Capacity()
{
    return createLispInteger((LispHeap1.overflow - LispHeap1.start) * 8);
}

//
//	Returns the total number of bytes currently stored in the
//	heap.
//
LispObj
Heap1CurrentlyUsed()
{
    return createLispInteger((LispHeap1.current - LispHeap1.start) * 8);
}

class CHeapBlocks
{
public:
    CHeapBlocks();
    ~CHeapBlocks();
    enum { DefaultAllocation = 0x8000 };

    void add(void* paddr, long size, long type);
    unsigned char* buf() const	{ return m_buf; }
    long bufLength() const		{ return m_bufLength; }
    long length() const			{ return m_length; }
    long count() const			{ return m_count; }
private:
    unsigned char* m_buf;
    long m_bufLength;
    long m_count;
    long m_length;
};

CHeapBlocks* cblocks = 0;

LispFunction(findCHeapBlocks)
{
    LISP_FUNC_BEGIN(1);
    LispObj p = LISP_ARG(0);
    long size = 0;
    void* paddr = 0;
    if (isForeignHeapPtr(p) && foreignPtr(p) != 0)
    {
        paddr = &foreignPtr(p);				// points to foreign ptr
        size = integer(foreignHeapSize(p));	// size in bytes of foreign heap block
        cblocks->add(paddr, size, integer(foreignHeapType(p)));
    }
    LISP_FUNC_RETURN(NIL);
}

CHeapBlocks::CHeapBlocks()
    : m_buf(0), m_bufLength(0), m_count(0), m_length(0)
{
    cblocks = this;
    processEachHeapBlock(findCHeapBlocks, NIL);
}

CHeapBlocks::~CHeapBlocks()
{
    CFree(m_buf);
}

void CHeapBlocks::add(void* paddr, long size, long type)
{
    long spaceNeeded = size + (3 * sizeof(long));
    if (!m_buf || (m_length + spaceNeeded) > m_bufLength)
    {
        if (!m_buf)
        {
            m_bufLength = max(DefaultAllocation, spaceNeeded);
            m_buf = (unsigned char*)CAlloc(m_bufLength);
        }
        while (m_length + spaceNeeded > m_bufLength)
        {
            // keep doubling in size
            m_bufLength *= 2;
            m_buf = (unsigned char*)CRealloc(m_buf, m_bufLength);
        }
    }
    *(long*)(m_buf + m_length) = type;
    *(void**)(m_buf + m_length + sizeof(long)) = paddr;
    *(long*)(m_buf + m_length + sizeof(long) + sizeof(void*)) = size;
    memcpy(m_buf + m_length + sizeof(long) + sizeof(void*) + sizeof(long),
            *(unsigned char**)paddr, size);
    m_length += spaceNeeded;
    m_count++;
}

//
//	The structure of the image file is this:
//
//	Bytes					Contents
//	-----					--------
//	0 - 3					Corman Lisp image identifier  (0xC0C0BABE)
//  4 - 7					Unused
//  8 - 11					Kernel Version ID
//	12 - 15					Size of header (bytes) (512)
//	16 - 19					Start of static section
//	20 - 23					Size of static section (bytes)
//	24 - 27					Start of Heap section
//	28 - 31					Size of Heap section
//	32 - 35					Start of page map
//	36 - 39					Size of page map (bytes)
//	40 - 43					Start of foreign blocks
//	44 - 47					Size of foreign blocks
//	48 - 127				Corman Lisp Copyright message
//  128 - 131				Compression type (0 = none, 1 = gzip)
//  132 - 135				Compressed size (in bytes)
//  136 - 139				Uncompressed size (in bytes)
//	140 - 511				Unused (future expansion)
//	512 - ?					Static section
//	...						Heap section
//	...						Page map
//	...						Foreign heap blocks
//

unsigned long CormanLispImageID = 0xC0C0BABE;
char* LispImageCopyright = "Corman Lisp Copyright (c) Corman Technologies. See LICENSE.txt for license information.";
struct LispImageHeader
{
    unsigned long imageID;
    unsigned long unused1;
    unsigned long kernelVersionID;
    unsigned long headerSize;
    unsigned long staticStart;
    unsigned long staticSize;
    unsigned long heapStart;
    unsigned long heapSize;
    unsigned long pageMapStart;
    unsigned long pageMapSize;
    unsigned long foreignBlocksStart;
    unsigned long foreignBlocksSize;
    char copyright[80];
    unsigned long compressionType;	// Compression type (0 = none, 1 = gzip)
    unsigned long compressedSize;
    unsigned long uncompressedSize;
    unsigned char unused2[372];		// should total 512 bytes
};
extern unsigned long KernelVersionID;

//
// This will return true if the file is a Portable Executable: either a .exe or .dll
// For now, all it does is look at the extension to determine this.
//
int fileIsEXE(LispObj path)
{
    FILE* ret = 0;
    LispObj np = 0;
    int len = 0;
    long c1 = 0;
    long c2 = 0;
    long c3 = 0;
    int err = 0;
    np = path;
    len = strlen((char*)byteArrayStart(np));
    if (len < 5)
        return 0;
    c1 = byteArrayStart(np)[len - 3];
    c2 = byteArrayStart(np)[len - 2];
    c3 = byteArrayStart(np)[len - 1];

    if (((c1 == 'E' || c1 == 'e')
            && (c2 == 'X' || c2 == 'x')
            && (c3 == 'E' || c3 == 'e'))
        || ((c1 == 'D' || c1 == 'd')
            && (c2 == 'L' || c2 == 'l')
            && (c3 == 'L' || c3 == 'l')))
    {
        err = fopen_s(&ret, (char*)byteArrayStart(np), "r");
        if (err != 0)
            return 0;
        else
        {
            fclose(ret);
            return 1;
        }
    }
    else
        return 0;
}

static char ExePath[MAX_PATH];
static int AppendPos = 0;

static FILE* openWriteEXE(LispObj path)
{
    long pos = 0;
    long extra = 0;
    FILE* f = 0;
    int pad = 0;
    int err = 0;
    strcpy_s(ExePath, sizeof(ExePath), (char*)byteArrayStart(nullTerminate(path)));
    err = fopen_s(&f, ExePath, "r+b");	// read/write, binary
    if (err != 0)
        return 0;
    fseek(f, 0, SEEK_END);
    pos = ftell(f);	// determine file length
    extra = pos % PAGE_SIZE;		// determine how much past a page we are
    if (extra > 0)
    {
        // pad file by PAGE_SIZE - extra bytes to get to page boundary
        unsigned char buf[PAGE_SIZE];
        pad = (PAGE_SIZE - extra);
        memset(buf, 0, PAGE_SIZE);	// clear memory
        if (fwrite(buf, 1, pad, f) != (size_t)pad)
        {
            fclose(f);
            return 0;
        }
    }
    AppendPos = pos + pad;
    return f;
}

#define SizeOfNtSignature 4
#define PaddedSize(n) ((n) % PAGE_SIZE ? (n) + (PAGE_SIZE - ((n) % PAGE_SIZE)) : (n))
IMAGE_SECTION_HEADER LispSectionHeader = { 0 };
IMAGE_SECTION_HEADER PrevSectionHeader;

static FILE* openReadEXE(LispObj path, char* sectionName)
{
    long pos = 0;
    long extra = 0;
    FILE* exe = 0;
    int pad = 0;
    BOOL ret = 0;
    int err = 0;

    strcpy_s(ExePath, sizeof(ExePath), (char*)byteArrayStart(nullTerminate(path)));
    err = fopen_s(&exe, ExePath, "rb");	// read, binary
    if (err != 0)
        return 0;

    // look for lisp section
    long dosHeaderPos = 0;
    long imageHeaderPos = 0;
    long optionalHeaderPos = 0;
    long startOfSectionHeaderPos = 0;
    long newSectionHeaderPos = 0;
    long temp = 0;
    long numSections = 0;
    long i = 0;
    fseek(exe, offsetof(IMAGE_DOS_HEADER, e_lfanew), SEEK_SET);
    ret = fread(&temp, 1, 4, exe);
    imageHeaderPos = dosHeaderPos + temp + SizeOfNtSignature;
    optionalHeaderPos = imageHeaderPos + sizeof(IMAGE_FILE_HEADER);
    startOfSectionHeaderPos = optionalHeaderPos + sizeof(IMAGE_OPTIONAL_HEADER);
    fseek(exe, imageHeaderPos + offsetof(IMAGE_FILE_HEADER, NumberOfSections), SEEK_SET);
    ret = fread(&numSections, 1, 2, exe);	//numSections is a short

    fseek(exe, startOfSectionHeaderPos, SEEK_SET);
    for (i = 0; i < numSections; i++)
    {
        ret = fread(&PrevSectionHeader, 1, sizeof(PrevSectionHeader), exe);
        if (!strcmp((const char*)PrevSectionHeader.Name, sectionName))
            break;
    }
    if (i == numSections)
    {
        // could not find lisp section
        fclose(exe);
        return 0;
    }
    // position file at lisp section
    fseek(exe, PrevSectionHeader.PointerToRawData, SEEK_SET);
    return exe;
}

static int finishEXE(FILE* exe, char* sectionName)
{
    long pos = 0;
    long extra = 0;
    long paddedSection = 0;
    long bytesAdded = 0;
    long pad = 0;
    BYTE* f = 0;
    int ret = 0;
    unsigned long length = 0;
    fseek(exe, 0, SEEK_END);
    pos = ftell(exe);	// determine file length
    bytesAdded = pos - AppendPos;
    paddedSection = bytesAdded;
    extra = pos % PAGE_SIZE;		// determine how much past a page we are
    if (extra > 0)
    {
        // pad file by PAGE_SIZE - extra bytes to get to page boundary
        unsigned char buf[PAGE_SIZE];
        memset(buf, 0, PAGE_SIZE);	// clear memory
        pad = (PAGE_SIZE - extra);
        if (fwrite(buf, 1, pad, exe) != (size_t)pad)
        {
            fclose(exe);
            return 0;
        }
        paddedSection += pad;
        pos += pad;
    }

    long dosHeaderPos = 0;
    long imageHeaderPos = 0;
    long optionalHeaderPos = 0;
    long startOfSectionHeaderPos = 0;
    long newSectionHeaderPos = 0;
    long temp = 0;
    long numSections = 0;
    fseek(exe, offsetof(IMAGE_DOS_HEADER, e_lfanew), SEEK_SET);
    ret = fread(&temp, 1, 4, exe);
    imageHeaderPos = dosHeaderPos + temp + SizeOfNtSignature;
    optionalHeaderPos = imageHeaderPos + sizeof(IMAGE_FILE_HEADER);
    startOfSectionHeaderPos = optionalHeaderPos + sizeof(IMAGE_OPTIONAL_HEADER);
    fseek(exe, imageHeaderPos + offsetof(IMAGE_FILE_HEADER, NumberOfSections), SEEK_SET);
    ret = fread(&numSections, 1, 2, exe);	//numSections is a short
    newSectionHeaderPos = startOfSectionHeaderPos + numSections * sizeof(IMAGE_SECTION_HEADER);

    // increment the number of sections
    fseek(exe, imageHeaderPos + offsetof(IMAGE_FILE_HEADER, NumberOfSections), SEEK_SET);
    numSections++;
    ret = fwrite(&numSections, 1, 2, exe);

    // get the next to last section
    fseek(exe, newSectionHeaderPos - sizeof(PrevSectionHeader), SEEK_SET);
    ret = fread(&PrevSectionHeader, 1, sizeof(PrevSectionHeader), exe);

    // write out the header info for the new section
    fseek(exe, newSectionHeaderPos, SEEK_SET);
    strcpy_s((char*)LispSectionHeader.Name, sizeof(LispSectionHeader.Name), sectionName);
    LispSectionHeader.Misc.VirtualSize = bytesAdded;
    LispSectionHeader.VirtualAddress = PrevSectionHeader.VirtualAddress +
        PaddedSize(PrevSectionHeader.Misc.VirtualSize);
    LispSectionHeader.SizeOfRawData = paddedSection;
    LispSectionHeader.PointerToRawData = AppendPos;
    LispSectionHeader.PointerToRelocations = 0;
    LispSectionHeader.PointerToLinenumbers = 0;
    LispSectionHeader.NumberOfRelocations = 0;
    LispSectionHeader.NumberOfLinenumbers = 0;
    LispSectionHeader.Characteristics = 0; //0x40 | 0x40000000;	// readable, initialized

    ret = fwrite(&LispSectionHeader, 1, sizeof(LispSectionHeader), exe);

    // update size of image
    fseek(exe, optionalHeaderPos + offsetof(IMAGE_OPTIONAL_HEADER, SizeOfImage), SEEK_SET);
    ret = fread(&temp, 1, 4, exe);		// get old image size
    temp += paddedSection;
    fseek(exe, optionalHeaderPos + offsetof(IMAGE_OPTIONAL_HEADER, SizeOfImage), SEEK_SET);
    ret = fwrite(&temp, 1, 4, exe);		// update image size

    fclose(exe);
    return 1;
}

//
//	If the file exists, and has a .EXE extension, add the heap
//	as a new section to the existing EXE file. The new section
//	will be called ".lisp". Otherwise, just create a file with
//	the passed name.
//

// compression utility (zlib)
extern "C" int compress(byte* dest,   unsigned long* destLen,
                        byte* source, unsigned long  sourceLen);
extern "C" int uncompress(byte* dest,   unsigned long* destLen,
                        byte* source, unsigned long  sourceLen);

byte* allocateCompressionBuffer(unsigned long size)
{
    return (byte*)VirtualAlloc(0, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
}

void freeCompressionBuffer(byte* buf)
{
    if (buf)
        VirtualFree(buf, 0, MEM_RELEASE);
}

void writeHeap(FILE* writeHeap_os)
{
    long staticSize = 0;
    long heapSize = 0;
    long pageTableSize = 0;
    long CHeapBlocksSize = 0;
    unsigned long heapAddress = 0;
    HANDLE process = 0;
    BOOL ret = 0;
    LispImageHeader header = {0};
    int compressHeap = 0;
    byte* compressionSrcBuffer = 0;
    byte* compressionDestBuffer = 0;
    unsigned long offset = 0;
    int compret = 0;
    unsigned long destLen = 0;

    if (symbolValue(COMPRESS_IMG) != NIL)
        compressHeap = 1;

    // suspend all threads but the current one
    ThreadList.suspendAllOtherThreads();

    flushEphemeralHeaps();
    GCCriticalSection.Enter();

    setSymbolValue(GC_TIME_COUNTER, 0);	// make sure this is not a heap object
                                        // as it would get allocated after collection

    // zero out all the hash ids to force rehash on next access
    processEachHeapBlock(RESET_HASH_ID, NIL);

    CHeapBlocks cHeapBlocks;

    staticSize = QV_MAX * sizeof(LispObj);
    heapSize = (LispHeap1.current - LispHeap1.start) * sizeof(Node);
    pageTableSize = LispHeap1.numPages * sizeof(PageTableEntry);
    CHeapBlocksSize = cHeapBlocks.length();

    header.imageID = CormanLispImageID;
    header.kernelVersionID = KernelVersionID;
    header.headerSize = sizeof(LispImageHeader);		// should be 512
    header.staticStart = header.headerSize;
    header.staticSize = staticSize;
    header.heapStart = header.staticStart + header.staticSize;
    header.heapSize = heapSize + 4;	// add 4-byte length
    header.pageMapStart = header.heapStart + header.heapSize;
    header.pageMapSize = pageTableSize;
    header.foreignBlocksStart = header.pageMapStart + header.pageMapSize;
    header.foreignBlocksSize = CHeapBlocksSize;
    strncpy_s(header.copyright, sizeof(header.copyright), LispImageCopyright, _TRUNCATE);
    header.compressionType = 0;
    header.compressedSize = 0;
    header.uncompressedSize = staticSize + sizeof(heapAddress)
        + heapSize + pageTableSize + CHeapBlocksSize;

    compressionSrcBuffer = allocateCompressionBuffer(header.uncompressedSize);
    if (!compressionSrcBuffer)
    {
        // resume all suspended threads
        ThreadList.resumeAllOtherThreads();
        GCCriticalSection.Leave();
        Error("Not enough memory to write lisp image to file");
    }

    if (compressHeap)
    {
        // compression can cause the data to grow to 1.01% + 12 bytes (in worst case!)
        destLen = ((int)(header.uncompressedSize * 1.02)) + 12;
        compressionDestBuffer = allocateCompressionBuffer(destLen);
        if (!compressionDestBuffer)
        {
            freeCompressionBuffer(compressionSrcBuffer);

            // resume all suspended threads
            ThreadList.resumeAllOtherThreads();
            GCCriticalSection.Leave();

            Error("Not enough memory to write lisp image to file");
        }
    }
    process = GetCurrentProcess();

    // copy the static section
    ret = FlushInstructionCache(process, NULL, 0);
    memcpy(compressionSrcBuffer + offset, QV, staticSize);
    offset += staticSize;

    // copy heap base address
    FlushInstructionCache(process, NULL, 0);
    heapAddress = (unsigned long)LispHeap1.start;
    memcpy(compressionSrcBuffer + offset, (char*)&heapAddress, sizeof(heapAddress));
    offset += sizeof(heapAddress);

    // copy heap contents
    memcpy(compressionSrcBuffer + offset, (char*)LispHeap1.start, heapSize);
    offset += heapSize;

    // copy page table
    memcpy(compressionSrcBuffer + offset, (char*)(PageTable + LispHeap1.firstPage), pageTableSize);
    offset += pageTableSize;

    // output C heap blocks
    memcpy(compressionSrcBuffer + offset, (char*)(cHeapBlocks.buf()), CHeapBlocksSize);
    offset += CHeapBlocksSize;

    assert(offset == header.uncompressedSize);

    if (compressHeap)
    {
        // compress the buffer
        compret = compress(compressionDestBuffer, &destLen,
            compressionSrcBuffer, header.uncompressedSize);
        if (compret != 0)
        {
            freeCompressionBuffer(compressionSrcBuffer);
            freeCompressionBuffer(compressionDestBuffer);

            // resume all suspended threads
            ThreadList.resumeAllOtherThreads();
            GCCriticalSection.Leave();

            Error("A compression error occurred writing lisp image to file");
        }
        header.compressionType = 1;
        header.compressedSize = destLen;
    }

    // output header
    fwrite(&header, 1, sizeof(LispImageHeader), writeHeap_os);

    if (compressHeap)
    {
        fwrite(compressionDestBuffer, 1, destLen, writeHeap_os);
        freeCompressionBuffer(compressionDestBuffer);
    }
    else
        fwrite(compressionSrcBuffer, 1, header.uncompressedSize, writeHeap_os);

    freeCompressionBuffer(compressionSrcBuffer);
    fflush(writeHeap_os);

    // resume all suspended threads
    ThreadList.resumeAllOtherThreads();

    GCCriticalSection.Leave();
}

void readHeap(FILE* is)
{
    void* paddr = 0;
    long size = 0;
    LispObj temp = 0;
    long offset = 0;
    HANDLE process = 0;
    unsigned char* c_blocks = 0;
    unsigned char* p = 0;
    long type = 0;
    byte* compressionSrcBuffer = 0;
    byte* compressionDestBuffer = 0;
    unsigned long index = 0;
    unsigned long heapAddress = 0;
    unsigned long destLen = 0;
    int compret = 0;
    int i = 0;
    size_t length = 0;
    LispObj* thqv = 0;
    LispObj* x = 0;
    LispObj dummy = 0;
    LispObj* end = &dummy;
    LispImageHeader header = {0};
    LispObj fp = 0;

    __try
    {
        __try
        {
            ThreadRecord* currThread = (ThreadRecord*)TlsGetValue(Thread_Index);
            LispObj* stackStart = currThread->stackStart;
            ThreadList.Lock();
            flushEphemeralHeaps();

            // suspend all threads but the current one
            ThreadList.suspendAllOtherThreads();

            // input the header
            fread(&header, 1, sizeof(LispImageHeader), is);

            // check header version
            if (header.imageID != CormanLispImageID)
            {
                fclose(is);
                Error("Invalid lisp image file");
            }
            if (header.kernelVersionID != KernelVersionID)
            {
                fclose(is);
                Error("The image file is not compatible with this version of the Corman Lisp kernel");
            }

            // See if we need to grow the heap to accomodate a large image.
            // Make sure we leave a meg free. Nothing magic about that number.
            // Just a heuristic.
            if (header.heapSize >= (unsigned long)(LispHeap1.sizeMem - 0x100000))
            {
                growLispHeaps((header.heapSize - LispHeap1.sizeMem + 0x100000) / 8);
            }

            LispHeap1.commitTrailingPages(0);

            destLen = header.uncompressedSize;
            compressionDestBuffer = allocateCompressionBuffer(destLen);

            if (header.compressionType == 1) // gz compression
            {
                compressionSrcBuffer = allocateCompressionBuffer(header.compressedSize);
                length = fread(compressionSrcBuffer, 1, header.compressedSize, is);
                if (length != header.compressedSize)
                    Error("Invalid lisp image file");
                compret = uncompress(compressionDestBuffer, &destLen,
                                compressionSrcBuffer, header.compressedSize);
                if (destLen != header.uncompressedSize)
                {
                    freeCompressionBuffer(compressionSrcBuffer);
                    freeCompressionBuffer(compressionDestBuffer);
                    Error("Invalid lisp image file");
                }
            }
            else
            {
                length = fread(compressionDestBuffer, 1, header.uncompressedSize, is);
                if (length != header.uncompressedSize)
                    Error("Invalid lisp image file");
            }
            process = GetCurrentProcess();
            FlushInstructionCache(process, NULL, 0);
            memcpy(QV, compressionDestBuffer + index, header.staticSize);
            index += header.staticSize;

            // input the heap
            LispHeap1.unWriteProtectAllPages();
            // read heap base address
            memcpy((char*)&heapAddress, compressionDestBuffer + index, sizeof(heapAddress));
            index += sizeof(heapAddress);

            // read heap contents
            memcpy((char*)LispHeap1.start, compressionDestBuffer + index, header.heapSize - 4);
            index += (header.heapSize - 4);
            LispHeap1.reset();
            LispHeap1.current = (Node*)((char*)LispHeap1.start + header.heapSize - 4);

            // see if the heap is being relocated
            if (heapAddress != (unsigned long)LispHeap1.start)
            {
                // add offsets to all heap addresses
                offset = (long)((unsigned long)LispHeap1.start - heapAddress);
                offsetHeapAddresses(offset);
                offsetGlobals(offset, heapAddress, heapAddress + header.heapSize - 4);
            }

            // input the page table
            memcpy((char*)(PageTable + LispHeap1.firstPage),
                compressionDestBuffer + index, header.pageMapSize);
            index += header.pageMapSize;

            // input the C heap blocks
            c_blocks = (unsigned char*)CAlloc(header.foreignBlocksSize);
            memcpy((char*)c_blocks, compressionDestBuffer + index, header.foreignBlocksSize);
            index += header.foreignBlocksSize;
            assert(index == header.uncompressedSize);
            if (compressionSrcBuffer)
                freeCompressionBuffer(compressionSrcBuffer);
            freeCompressionBuffer(compressionDestBuffer);
            compressionSrcBuffer = 0;
            compressionDestBuffer = 0;

            p = c_blocks;
            while (p < c_blocks + header.foreignBlocksSize)
            {
                type = *(long*)p;
                paddr = *(void**)(p + sizeof(long));
                size = *(long*)(p + sizeof(long) + sizeof(void*));
            //	assert(size > 0);
                if (heapAddress != (unsigned long)LispHeap1.start)
                    paddr = ((char*)paddr) + offset;
                *(void**)paddr = CAlloc(size);
                memcpy(*(void**)paddr, p + sizeof(long) + sizeof(void*) + sizeof(long), size);
                *(long*)(((char*)paddr) + 8) = wrapInteger(type);
                if (type == FOREIGN_HEAP_TYPE_CALLBACK_THUNK)
                {
                    // store SysGlobalsAddr in callback thunk
                    assert(size == 11);		// track this--tied to sizeGenericThunk
                    *(long*)(((char*)(*(void**)paddr)) + 1) = SysGlobalsAddr;	// whew! -RGC 5/8/99
                }
                else
                if (type == FOREIGN_HEAP_TYPE_CRITICAL_SECTION)
                {
                    InitializeCriticalSection((CRITICAL_SECTION*)*(void**)paddr);
                }
                p += (size + sizeof(long) + sizeof(void*) + sizeof(long));
            }

            assert(p == c_blocks + header.foreignBlocksSize);
            CFree(c_blocks);

            thqv = ThreadQV();
            for (i = 0; i < QV_MAX; i++)
            {
                thqv[i] = QV[i];
            }

            // make sure all bindings of this are NIL
        //	setSymbolValue(COMPILER_RUNTIME, NIL);
            // get the binding list of COMPILER_RUNTIME

            temp = ThreadQV()[integer(UVECTOR(COMPILER_RUNTIME)[SYMBOL_VAR_TABLE])];
            while (isCons(temp))
            {
                CAR(temp) = NIL;
                temp = CDR(temp);
            }

            LispHeap1.writeProtectAllPages();
            LispHeap1.decommitTrailingPages();

            // now replace all heap references on the stack with
            // NIL.
            __asm	mov	dword ptr [end], esp		// go to stack pointer

            for (x = stackStart; x >= end; x--)
            {
                if (isHeapPointer(*x))
                    *x = 0;
            }

            // bind ccl:*current-thread-id*, ccl:*current-thread-handle*
            pushDynamicBinding(CURRENT_THREAD_ID, createUnsignedLispInteger(currThread->threadID));
            fp = foreignNode();
            UVECTOR(fp)[FOREIGN_PTR] = (LispObj)currThread->thread;
            pushDynamicBinding(CURRENT_THREAD_HANDLE, fp);
            setSymbolValue(CURRENT_PROCESS_ID, createUnsignedLispInteger(GetCurrentProcessId()));
            fp = foreignNode();
            UVECTOR(fp)[FOREIGN_PTR] = (LispObj)GetCurrentProcess();
            setSymbolValue(CURRENT_PROCESS_HANDLE, fp);
            fp = 0;

			foreignPtr(symbolValue(HEAP_0_START))	= (LispObj)EphemeralHeap1.start;	
			foreignPtr(symbolValue(HEAP_0_END))		= (LispObj)EphemeralHeap1.end;	
			foreignPtr(symbolValue(HEAP_1_START))	= (LispObj)EphemeralHeap2.start;	
			foreignPtr(symbolValue(HEAP_1_END))		= (LispObj)EphemeralHeap2.end;	
			foreignPtr(symbolValue(HEAP_2_START))	= (LispObj)LispHeap1.start;	
			foreignPtr(symbolValue(HEAP_2_END))		= (LispObj)LispHeap1.end;

			setSymbolValue(HEAP_0_GC_ID, 0);
			setSymbolValue(HEAP_1_GC_ID, 0);
			setSymbolValue(HEAP_2_GC_ID, 0);
        }
        __except (handleStructuredException(GetExceptionCode(), GetExceptionInformation()))
        {
            OutputDebugString("A structured exception occurred during the readHeap() call");
        }
    }
    __finally
    {
        FINALIZATION_REGISTRY = 0;
        GCExecRegistry = 0;

        // resume all suspended threads
        ThreadList.resumeAllOtherThreads();
        ThreadList.Unlock();
    }
}

void writePESection(LispObj path, char* sectionName, void (*writeFn)(FILE*))
{
    FILE* file = 0;
    file = openWriteEXE(path);
    if (!file)
        Error("Could not open file ~A for writing", path);
    writeFn(file);
    finishEXE(file, sectionName);
}

void writeImgFile(LispObj path, void (*writeFn)(FILE*))
{
    FILE* file = 0;
    int err = 0;
    err = fopen_s(&file, (char*)byteArrayStart(nullTerminate(path)), "wb");
    if (err != 0)
        Error("Could not open file ~A for writing", path);
    writeFn(file);
    fclose(file);
}

void writeHeapToFile(LispObj path)
{
    LispObj np = 0;
    np = nullTerminate(path);
    if (fileIsEXE(np))
        writePESection(path, ".lisp", writeHeap);
    else
        writeImgFile(path, writeHeap);
}

void readPESection(LispObj path, char* sectionName, void (*readFn)(FILE*))
{
    FILE* file = 0;
    file = openReadEXE(path, sectionName);
    if (!file)
        Error("Could not open file ~A for writing", path);
    readFn(file);
    fclose(file);
}

void readImgFile(LispObj path, void (*readFn)(FILE*))
{
    FILE* file = 0;
    LispObj np = 0;
    int err = 0;
    np = nullTerminate(path);
	err = fopen_s(&file, (char*)byteArrayStart(np), "rb");
	// try to look in the installation directory
	if (err != 0)
	{
		char inst_path[MAX_PATH + 1] = { 0 };
		strcat_s(inst_path, sizeof(inst_path), CormanLispServerDirectory);
		strcat_s(inst_path, sizeof(inst_path), "\\");
		strcat_s(inst_path, sizeof(inst_path), (char*)byteArrayStart(np));

		err = fopen_s(&file, inst_path, "rb");
	}

    if (err != 0)
        Error("Could not open file ~A for reading", path);
    readFn(file);
    fclose(file);
}

void readHeapFromFile(LispObj path)
{
    LispObj np = 0;
    np = nullTerminate(path);
    if (fileIsEXE(np))
        readPESection(path, ".lisp", readHeap);
    else
        readImgFile(path, readHeap);
}

#define ADJUST(place) \
    if (isHeapBlock(place) && place != UNINITIALIZED) \
        (place) += offset

// add offsets to all heap addresses
static void offsetHeapAddresses(long offset)
{
    Node* p = 0;
    long ulength = 0;
    LispObj obj = 0;

    p = LispHeap1.start;
    while (p < LispHeap1.current)
    {
        if (gettag(p->car) == UvectorLengthTag)	// we have a uvector
        {
            ulength = ((p->car) >> 8) * 2;
            obj = wrap((LispObj)p, UvectorTag);

            offsetUvector(obj, offset, ulength);
            p += (ulength / 2);
        }
        else		// we have a cons cell
        {
            obj = wrap((LispObj)p, ConsTag);
            ADJUST(CAR(obj));
            ADJUST(CDR(obj));
            p++;
        }
    }
}

static void offsetUvector(LispObj obj, long offset, long length)
{
    int i = 0;
    int type = uvectorType(obj);
    switch (type)
    {
    case FunctionType:
        ADJUST(UVECTOR(obj)[FUNCTION_ENVIRONMENT]);
        ADJUST(UVECTOR(obj)[FUNCTION_ADDRESS]);
        break;

    case KFunctionType:
        ADJUST(UVECTOR(obj)[FUNCTION_ENVIRONMENT]);
        break;

    case StructureType:
        for (i = 1; i < length; i++)
            ADJUST(UVECTOR(obj)[i]);
        break;

    case ArrayType:
        for (i = 1; i < length; i++)
            ADJUST(UVECTOR(obj)[i]);
        break;

    case SymbolType:
        for (i = 1; i < length; i++)
            ADJUST(UVECTOR(obj)[i]);
        break;

    case StreamType:
        for (i = 1; i < length; i++)
            ADJUST(UVECTOR(obj)[i]);
        break;

    case DoubleFloatType:
        break;

    case PackageType:
        for (i = 1; i < length; i++)
            ADJUST(UVECTOR(obj)[i]);
        break;

    case HashtableType:
        for (i = 1; i < length; i++)
            ADJUST(UVECTOR(obj)[i]);
        break;

    case ForeignType:
        break;

    case CompiledCodeType:
        offsetCode(obj, offset);
        break;

    case ReadTableType:
        for (i = 1; i < length; i++)
            ADJUST(UVECTOR(obj)[i]);
        break;

    case ComplexType:
        ADJUST(UVECTOR(obj)[COMPLEX_REAL]);
        ADJUST(UVECTOR(obj)[COMPLEX_IMAGINARY]);
        break;

    case RatioType:
        ADJUST(UVECTOR(obj)[RATIO_NUMERATOR]);
        ADJUST(UVECTOR(obj)[RATIO_DENOMINATOR]);
        break;

    case BignumType:
    case ForeignHeapType:
        break;

    case WeakPointerType:
        ADJUST(UVECTOR(obj)[WEAK_PTR]);
        break;

    case SimpleVectorType:
        for (i = 1; i < length; i++)
            ADJUST(UVECTOR(obj)[i]);
        break;

    case SimpleCharVectorType:
    case SimpleByteVectorType:
    case SimpleShortVectorType:
    case SimpleDoubleFloatVectorType:
    case SimpleBitVectorType:
    case SimpleSingleFloatVectorType:
    case SingleFloatType:
        break;

    case CLOSInstanceType:
        ADJUST(UVECTOR(obj)[CLOS_INSTANCE_CLASS_OFFSET]);
        ADJUST(UVECTOR(obj)[CLOS_INSTANCE_SLOTS_OFFSET]);
        break;

    case ForeignStackType:
        break;
    }
}

// add offsets to all globals in the QV vector
static void offsetGlobals(long offset,
            unsigned long origAddress, unsigned long origAddressEnd)
{
    int i = 0;
    for (i = 0; i < SYSTEM_OBJ_MAX; i++)
        ADJUST(QV[i]);
    for (i = 0; i < NumJumpTableEntries; i++)
    {
        ADJUST(QV[FirstJumpTableEntry + (i * JumpTableCellsPerEntry)]);	// catch environments
        if (QV[FirstJumpTableEntry + (i * JumpTableCellsPerEntry) + 1] >= origAddress
            && QV[FirstJumpTableEntry + (i * JumpTableCellsPerEntry) + 1] < origAddressEnd)
            QV[FirstJumpTableEntry + (i * JumpTableCellsPerEntry) + 1] += offset;
    }
    for (i = 0; i < NumSpecialSymbolEntries; i++)
        ADJUST(QV[FirstSpecialSymbolEntry + i]);	// catch special variables
}

static void offsetCode(LispObj obj, long offset)
{
    LispObj table = 0;
    LispObj* pp = 0;
    byte* code = 0;
    long numEntries = 0;
    LispObj* referenced = 0;
    unsigned short* us = 0;
    long noffset = 0;
    int i = 0;

    ADJUST(UVECTOR(obj)[COMPILED_CODE_REFERENCES]);
    ADJUST(UVECTOR(obj)[COMPILED_CODE_PROPERTIES]);
    if (isHeapPointer(UVECTOR(obj)[COMPILED_CODE_REFERENCES])
        || isFixnum(UVECTOR(obj)[COMPILED_CODE_REFERENCES]))
    {
        // update the code references
        if (isUvector(UVECTOR(obj)[COMPILED_CODE_REFERENCES]))
        {
            table = UVECTOR(obj)[COMPILED_CODE_REFERENCES];
            pp = arrayStart(table);
            numEntries = arrayDimension(table, 0) / 2;
            code = (byte*)(UVECTOR(obj) + COMPILED_CODE_OFFSET);
            for (i = 0; i < numEntries; i++)
            {
                referenced = (LispObj*)(code + integer(pp[i * 2 + 1]));
                ADJUST(*referenced);
            }
        }
        else
        if (isFixnum(UVECTOR(obj)[COMPILED_CODE_REFERENCES])
            && UVECTOR(obj)[COMPILED_CODE_REFERENCES] > 0)
        {
            // the references are stored following the code
            // u[COMPILED_CODE_REFERENCES] = offset from start of code to reference header
            // Reference tags are 16 bit non-tagged integers, each representing an offset
            // from the previous reference.
            // Byte                Purpose
            // ----                -------
            // 00-01				unsigned number of references (max 65535)
            // 02-03			   1st reference (unsigned offset from code start)
            // 04-05			   2nd reference (unsigned offset from 1st reference)
            // etc.
            code = (byte*)(UVECTOR(obj) + COMPILED_CODE_OFFSET);
            us = (unsigned short*)(code + integer(UVECTOR(obj)[COMPILED_CODE_REFERENCES]));
            numEntries = (long)(unsigned long)(*us);
            us++;
            noffset = 0;
            for (i = 0; i < numEntries; i++)
            {
                noffset += us[i];
                referenced = (LispObj*)(code + noffset);
                ADJUST(*referenced);
            }
        }
    }
}
// static ofstream* dump_outfile = 0;

void
dumpHeapToFile(LispObj /*path*/)
{
#if 0
    Node* p = 0;
    long numUvectors = 0;
    long numConses = 0;
    long invalidObjects = 0;
    long forwardingPointers = 0;
    long type = 0;
    long ulength = 0;
    LispObj uvec = 0;
    LispObj* n = 0;
    long i = 0;

    dump_outfile = new ofstream(byteArrayStart(nullTerminate(path)),
                    ios::out | ios::trunc | ios::binary);
    if (!dump_outfile || !*dump_outfile || dump_outfile->fail())
    {
        delete dump_outfile;
        dump_outfile = 0;
        Error("Invalid output stream: ~A", path);
    }
//	garbageCollect();
//	if (LispHeap1.start > LispHeap2.start)
//		garbageCollect();

    // output global size in bytes
    *dump_outfile << "Heap bytes used: " << hex << (long)((LispHeap1.current - LispHeap1.start) * 8) << " bytes." << '\n';
    *dump_outfile << "Current heap range: " << hex << LispHeap1.start << " to " << LispHeap1.end << '\n';
    *dump_outfile << "Alternate heap range: " << hex << LispHeap2.start << " to " << LispHeap2.end << '\n';

    p = LispHeap1.start;

    while (p < LispHeap1.current)
    {
        if (gettag(p->car) == UvectorLengthTag)
        {
            // we have a uvector
            type = ((p->car) >> 3) & 0x1f;
            ulength = ((p->car) >> 8) * 2;
            uvec = wrap((LispObj)p, UvectorTag);

            *dump_outfile << (void*)p << " ";

            switch (type)
            {
            case FunctionType:	*dump_outfile << "Function";			break;
            case KFunctionType:	*dump_outfile << "Kernel Function";	break;
            case StructureType: *dump_outfile << "Structure";			break;
            case ArrayType:
                if (isString(uvec))
                    *dump_outfile << "String: " << charArrayStart(uvec);
                else
                    *dump_outfile << "Array";
                break;
            case SymbolType:	*dump_outfile << "Symbol";			break;
            case StreamType:	*dump_outfile << "Stream";			break;
            case DoubleFloatType:*dump_outfile << "Double-Float";	break;
            case SingleFloatType:*dump_outfile << "Single-Float";	break;
            case PackageType:	*dump_outfile << "Package";			break;
            case HashtableType:	*dump_outfile << "Hashtable";		break;
            case ForeignType:	*dump_outfile << "Foreign";			break;
            case CompiledCodeType:	*dump_outfile << "Compiled code";	break;
            case ReadTableType:	*dump_outfile << "ReadTable";			break;
            default:			*dump_outfile << "Unknown type: " << type;
            }
            *dump_outfile << "\n        ";

            // dump the uvector contents
            n = (LispObj*)p;
            for (i = 0; i < ulength; i++)
            {
                if (pointsIntoAlternateHeap(n[i]))
                {
                    *dump_outfile << "<" << (void*)(n[i]) << ">";
                    invalidObjects++;
                }
                else
                if (mayBeForwardingPointer(n[i]))
                {
                    *dump_outfile << "{" << (void*)(n[i]) << "}";
                    forwardingPointers++;
                }
                else
                    *dump_outfile << (void*)(n[i]);
                *dump_outfile << " ";
                if ((i % 4) == 3)
                    *dump_outfile << "\n        ";
            }
            *dump_outfile << "\n";
            if ((i % 4) != 0)
                *dump_outfile << '\n';

            p += (ulength / 2);
            numUvectors++;
        }
        else
        {
            // we have a cons cell
            *dump_outfile << hex << p << " ";
            *dump_outfile << "Cons:  CAR = ";
            if (pointsIntoAlternateHeap(p->car))
                *dump_outfile << "<";
            *dump_outfile << (void*)p->car;
            if (pointsIntoAlternateHeap(p->car))
            {
                *dump_outfile << ">";
                invalidObjects++;
            }
            *dump_outfile << " CDR = ";
            if (pointsIntoAlternateHeap(p->cdr))
                *dump_outfile << "<";
            *dump_outfile << (void*)p->cdr << '\n';
            if (pointsIntoAlternateHeap(p->cdr))
            {
                *dump_outfile << ">";
                invalidObjects++;
            }
            *dump_outfile << '\n';
            p++;
            numConses++;
        }
    }
    *dump_outfile << '\n' << "Total number of objects: " << dec << numUvectors << " Uvectors and ";
    *dump_outfile << numConses << " cons cells.\n";
    *dump_outfile << '\n' << "Total number of potentially invalid object pointers: "<< dec << invalidObjects << '\n';
    *dump_outfile << '\n' << "Total number of forwarding pointers: "<< dec << forwardingPointers << '\n';

    dump_outfile->flush();
    dump_outfile->close();
    delete dump_outfile;
#endif
}

#if 0
// Given a raw address (fixnum), return the address of the function most
// likely to contain the code at that address.
//
static LispObj CurrOffset = 0;
static LispObj CurrFunc = 0;
static LispObj Address = 0;
static LispObj findFunctionCallback(LispObj p)
{
    long type = 0;
    LispObj execaddr = 0;

    if (isFunction(p))
    {
        execaddr = LispCall(Execution_Address, p);
        if (execaddr <= Address)
        {
            if ((Address - execaddr) < CurrOffset)
            {
                CurrOffset = (Address - execaddr);
                CurrFunc = p;
            }
        }
    }
    return NIL;
}
#endif

LispObj
addressFindFunction(LispObj address)
{
    LispObj result = 0;
    unsigned long execaddr = 0;
    unsigned long size = 0;
    setSymbolValue(FIND_FUNCTION_CURR_ADDR, address);
    setSymbolValue(FIND_FUNCTION_CURR_OFFSET, wrapInteger(0xfffffff));
    setSymbolValue(FIND_FUNCTION_CURR_FUNC, 0);

    processEachHeapBlock(ADDRESS_FIND_FUNCTION_CALLBACK, NIL);
    result = symbolValue(FIND_FUNCTION_CURR_FUNC);
    if (isFunction(result))
    {
        execaddr = (unsigned long)functionAddress(result);
        if (!isKFunction(result))
        {
            // see if the address lies within the function
            size = (UVECTOR(UVECTOR(result)[FUNCTION_ADDRESS])[0] >> 8);
            if (lispIntegerToUnsignedLong(address) > (execaddr + (8 * size)))
                result = NIL;
        }
        else if (lispIntegerToUnsignedLong(address) - execaddr > 0x10000)
            result = NIL;
    }
    return result;
}

LispObj
processEachHeapBlock(LispObj func, LispObj processConses)
{
    Node* p = 0;
    long ulength = 0;
    LispObj uvec = 0;
    LispObj ret = 0;
    Node* saveCurrent = 0;

    GCCriticalSection.Enter();	// this blocks other threads from modifying the heap

    // check each heap
    p = EphemeralHeap1.start;
    saveCurrent = EphemeralHeap1.current;
    while (p < EphemeralHeap1.current)
    {
        if (saveCurrent != EphemeralHeap1.current)
            Error("The function ~A (passed to PROCESS-EACH-HEAP-BLOCK) allocated a heap object", func);
        if (gettag(p->car) == UvectorLengthTag)	// we have a uvector
        {
            ulength = ((p->car) >> 8) * 2;
            uvec = wrap((LispObj)p, UvectorTag);
            ret = LispCall2(Funcall, func, uvec);
            p += (ulength / 2);
        }
        else		// we have a cons cell
        {
            if (processConses != NIL)
            {
                uvec = wrap((LispObj)p, ConsTag);
                ret = LispCall2(Funcall, func, uvec);
            }
            p++;
        }
    }

    p = EphemeralHeap2.start;
    while (p < EphemeralHeap2.current)
    {
        if (gettag(p->car) == UvectorLengthTag)	// we have a uvector
        {
            ulength = ((p->car) >> 8) * 2;
            uvec = wrap((LispObj)p, UvectorTag);
            ret = LispCall2(Funcall, func, uvec);
            p += (ulength / 2);
        }
        else		// we have a cons cell
        {
            if (processConses != NIL)
            {
                uvec = wrap((LispObj)p, ConsTag);
                ret = LispCall2(Funcall, func, uvec);
            }
            p++;
        }
    }

    p = LispHeap1.start;
    while (p < LispHeap1.current)
    {
        if (gettag(p->car) == UvectorLengthTag)	// we have a uvector
        {
            ulength = ((p->car) >> 8) * 2;
            uvec = wrap((LispObj)p, UvectorTag);
            ret = LispCall2(Funcall, func, uvec);
            p += (ulength / 2);
        }
        else		// we have a cons cell
        {
            if (processConses != NIL)
            {
                uvec = wrap((LispObj)p, ConsTag);
                ret = LispCall2(Funcall, func, uvec);
            }
            p++;
        }
    }
    GCCriticalSection.Leave();

    return NIL;
}

//
//	Use this if you don't have a true lisp function
//	(i.e. a kernel function only)
//
LispObj
processEachHeapBlock(LispFunc func, LispObj processConses)
{
    Node* p = 0;
    long ulength = 0;
    LispObj uvec = 0;
    LispObj ret = 0;
    Node* saveCurrent = 0;

    // check each heap
    p = EphemeralHeap1.start;
    saveCurrent = EphemeralHeap1.current;
    while (p < EphemeralHeap1.current)
    {
        if (saveCurrent != EphemeralHeap1.current)
            Error("The kernel function (passed to PROCESS-EACH-HEAP-BLOCK) allocated a heap object");
        if (gettag(p->car) == UvectorLengthTag)	// we have a uvector
        {
            ulength = ((p->car) >> 8) * 2;
            uvec = wrap((LispObj)p, UvectorTag);
            ret = LispCall1(func, uvec);
            p += (ulength / 2);
        }
        else		// we have a cons cell
        {
            if (processConses != NIL)
            {
                uvec = wrap((LispObj)p, ConsTag);
                ret = LispCall1(func, uvec);
            }
            p++;
        }
    }

    p = EphemeralHeap2.start;
    while (p < EphemeralHeap2.current)
    {
        if (gettag(p->car) == UvectorLengthTag)	// we have a uvector
        {
            ulength = ((p->car) >> 8) * 2;
            uvec = wrap((LispObj)p, UvectorTag);
            ret = LispCall1(func, uvec);
            p += (ulength / 2);
        }
        else		// we have a cons cell
        {
            if (processConses != NIL)
            {
                uvec = wrap((LispObj)p, ConsTag);
                ret = LispCall1(func, uvec);
            }
            p++;
        }
    }

    p = LispHeap1.start;
    while (p < LispHeap1.current)
    {
        if (gettag(p->car) == UvectorLengthTag)	// we have a uvector
        {
            ulength = ((p->car) >> 8) * 2;
            uvec = wrap((LispObj)p, UvectorTag);
            ret = LispCall1(func, uvec);
            p += (ulength / 2);
        }
        else		// we have a cons cell
        {
            if (processConses != NIL)
            {
                uvec = wrap((LispObj)p, ConsTag);
                ret = LispCall1(func, uvec);
            }
            p++;
        }
    }

    return NIL;
}

static void
resurrectFinalizationObjects(LispHeap* fromSpace, LispHeap* toSpace)
{
    __try
    {

        // for each item in the finalization list, if it is still sitting
        // in fromSpace that means it died. If it died, resurrect it by
        // copying it to toSpace, and add it to the
        // resurrectedObjects list (along with the finalization function).
        // Also, promote the cons cell, but remove it from the finalization list.
        //
        // If the item is not dead (not in fromSpace), then promote the list
        // cell, thereby keeping it in the finalization list.
        GCFromSpace = fromSpace;
        GCToSpace = toSpace;
        LispObj x = FINALIZATION_REGISTRY;
        LispObj obj = 0;
        LispObj pair = 0;
        LispObj newFinalizationRegistry = 0;
        LispObj n = 0;
        Node* nptr = 0;
        long objectDied = 0;

        int count =  0;
        count = listLength(x);

        while (isCons(x))
        {
            pair = CAR(x);
            obj = CAR(pair);
            objectDied = isHeapPointer(obj) && !isForwardingPtr(*(LispObj*)stripTag(obj));

            // promote the two cons cells
            nptr = (Node*)stripTag(pair);
            if (isHeapPointer(nptr->car))
                promoteBlock(&nptr->car, GCToSpace);
            if (isHeapPointer(nptr->cdr))
                promoteBlock(&nptr->cdr, GCToSpace);
            nptr = (Node*)stripTag(x);
            if (isHeapPointer(nptr->car))
                promoteBlock(&nptr->car, GCToSpace);
            if (isHeapPointer(nptr->cdr))
                promoteBlock(&nptr->cdr, GCToSpace);
            if (isHeapPointer(x))
                promoteBlock(&x, GCToSpace);

            // did the object die?
            if (objectDied)
            {
                addResurrectedObjectPair(CAR(x));
                x = CDR(x);
            }
            else
            {	// the object did not die, so keep in the list
                n = x;
                x = CDR(x);
                CDR(n) = newFinalizationRegistry;
                newFinalizationRegistry = n;		// link into new list
            }
        }

        // update the registry with the new list, and make sure
        // all its cons cells get promoted
        FINALIZATION_REGISTRY = newFinalizationRegistry;
        if (isHeapPointer(FINALIZATION_REGISTRY))
            promoteBlock(&FINALIZATION_REGISTRY, GCToSpace);

        checkWeakPointers(fromSpace, toSpace);
    }
    __except (gcHandleStructuredExceptionDuringFinalization(GetExceptionCode(), GetExceptionInformation()))
    {
        OutputDebugString("A structured exception occurred during garbage collection");
    }
}

static void
checkWeakPointers(LispHeap* fromSpace, LispHeap* toSpace)
{
    // for each item in the weak pointer list, if it is still sitting
    // in fromSpace that means it died. If it died, set the weak pointer
    // to NIL and remove it from the weak pointer list.
    // If the item is not dead (not in fromSpace), then promote the list
    // cell, thereby keeping it in the weak pointer list.
    GCFromSpace = fromSpace;
    GCToSpace = toSpace;
    LispObj x = WEAK_PTR_REGISTRY;
    LispObj weak_ptr = 0;
    LispObj obj = 0;
    LispObj* p = 0;

    while (isCons(x))
    {
        if (isHeapPointer(x))
            promoteBlock(&x, GCToSpace);
        weak_ptr = CAR(x);
        if (isHeapPointer(weak_ptr))
            promoteBlock(&weak_ptr, GCToSpace);
        obj = WeakPointerObj(weak_ptr);
        if (isHeapPointer(obj))
        {
            // see if the object got promoted
            p = (LispObj*)stripTag(obj);
            if (!isForwardingPtr(*p))
            {
                // no forward pointer, object died.
                // so set the pointer to NIL
                WeakPointerObj(weak_ptr) = NIL;
            }
            else
                promoteBlock(&WeakPointerObj(weak_ptr), GCToSpace);
        }
        x = CDR(x);
    }
    if (isHeapPointer(WEAK_PTR_REGISTRY))
        promoteBlock(&WEAK_PTR_REGISTRY, GCToSpace);
}

// this is OK to call after garbage collection is finished
static void
releaseWeakPointers()
{
    __try
    {
        LispObj newList = NIL;
        LispObj p = WEAK_PTR_REGISTRY;
        WEAK_PTR_REGISTRY = 0;
        LispObj q = 0;
        while (isCons(p))
        {
            if (WeakPointerObj(CAR(p)) != NIL)
            {
                q = CDR(p);
                CDR(p) = newList;
                newList = p;
                p = q;
            }
            else
                p = CDR(p);
        }
        WEAK_PTR_REGISTRY = newList;
    }
    __except (gcHandleStructuredExceptionDuringFinalization(GetExceptionCode(), GetExceptionInformation()))
    {
        OutputDebugString("A structured exception occurred during garbage collection");
    }
}

static void
addResurrectedObjectPair(LispObj pair)
{
    FinalizationNode* node = new FinalizationNode;
    if (!node)
        Error("Cannot allocate finalization node");
    node->next = resurrectedObjects;
    resurrectedObjects = node;
    node->obj = pair;
}

//
// This routine can do heap allocations, because it will *not* be called
// during collections.
//
void
addFinalizationObject(LispObj obj, LispObj func)
{
    FINALIZATION_REGISTRY = cons(cons(obj, func),
        FINALIZATION_REGISTRY);
}

static void
finalizeResurrectedObjects()
{
    __try
    {

        LispObj pair = 0;
        FinalizationNode* node = resurrectedObjects;
        resurrectedObjects = 0;
        FinalizationNode* temp = 0;
        // add objects to the finalization list
        while (node)
        {
            pair = node->obj;
            setSymbolValue(FINALIZATION_PENDING, cons(pair, symbolValue(FINALIZATION_PENDING)));
            // LispCall(Funcall, FUNCALL_IGNORING_ERRORS, CDR(pair), CAR(pair));	// finalize the object
            temp = node;
            node = node->next;
            delete temp;
        }
    }
    __except (gcHandleStructuredExceptionDuringFinalization(GetExceptionCode(), GetExceptionInformation()))
    {
        OutputDebugString("A structured exception occurred during garbage collection");
    }
}

static void
executeGCRegistryFunctions()
{
    LispObj n = GCExecRegistry;
    while (isCons(n))
    {
        LispCall2(Funcall, FUNCALL_IGNORING_ERRORS, CAR(n));
        n = CDR(n);
    }
}

LispObj
getGCExecRegistry()
{
    return GCExecRegistry;
}

void
addGCExecRegistry(LispObj func)
{
    if (!isFunction(func))
        Error("Cannot add item ~A to the GC Exec registry, must be a function", func);
    GCExecRegistry = cons(func, GCExecRegistry);
}

void
removeGCExecRegistry(LispObj func)
{
    LispObj n = GCExecRegistry;
    if (CAR(n) == func)
        GCExecRegistry = CDR(n);
    else
    {
        while (isCons(CDR(n)))
        {
            if (CAR(CDR(n)) == func)
            {
                CDR(n) = CDR(CDR(n));
                return;
            }
            n = CDR(n);
        }
        Error("Could not find the item ~A in the GC registry", func);
    }
}

LispFunction(verifyHeapBlock)
{
    LISP_FUNC_BEGIN(1);
    LispObj p = LISP_ARG(0);
    long found = 0;
    long size = (*(UVECTOR(p))) >> 8;
    if (size > 0x10000)
    {
        setSymbolValue(INVALID_OBJECT_COUNT,
            symbolValue(INVALID_OBJECT_COUNT) + wrapInteger(1));
        found = 1;
    }
    LISP_FUNC_RETURN(NIL);
}

static void verifyHeapBlocks()
{
    processEachHeapBlock(verifyHeapBlock, NIL);
}

static HANDLE MapFileFile = 0;
static HANDLE MapFileMap = 0;

static BYTE* MapFile(const char* path, DWORD* length)
{
    HANDLE hfile = CreateFile(path,
                        GENERIC_READ | GENERIC_WRITE,
                        FILE_SHARE_READ,
                        NULL,
                        OPEN_EXISTING,
                        FILE_ATTRIBUTE_NORMAL,
                        NULL);
    if (hfile == INVALID_HANDLE_VALUE)
        return 0;
    *length = GetFileSize(hfile, 0);
    if (*length == 0xffffffff)
    {
        CloseHandle(hfile);
        return 0;
    }

    HANDLE hfilemap = CreateFileMapping(hfile,
                        0,
                        PAGE_READWRITE,
                        0, 0,
                        0);

    //CloseHandle(hfile);
    MapFileFile = hfile;  // close later
    if (!hfilemap)
        return 0;

    BYTE* pbFile = (BYTE*)MapViewOfFile(hfilemap, FILE_MAP_READ, 0, 0, 0);
    //CloseHandle(hfilemap);
    MapFileMap = hfilemap; // close later
    return pbFile;
}

static void UnmapFile(BYTE* mapping)
{
    FlushViewOfFile(mapping, 0);
    UnmapViewOfFile(mapping);
    if (MapFileFile)
        CloseHandle(MapFileFile);
    if (MapFileMap)
        CloseHandle(MapFileMap);
}

//
//	Returns a number from 0 to 100 representing the percentage full
//  the requested heap is. Valid generations are 0 - 2.
//  It the passed allocated and used variables are non-null, they will
//  return the actual allocated size and used amount of the heap.
long getHeapStatistics(long generation, long* allocated, long* used)
{
    long capacity = 0;
    long current = 0;
    long percent = 0;

    switch (generation)
    {
    case 0:
        capacity = (EphemeralHeap1.end - EphemeralHeap1.start) * sizeof(Node);
        current = (EphemeralHeap1.current - EphemeralHeap1.start) * sizeof(Node);
        break;
    case 1:
        capacity = (EphemeralHeap2.overflow - EphemeralHeap2.start) * sizeof(Node);
        current = (EphemeralHeap2.current - EphemeralHeap2.start) * sizeof(Node);
        break;
    case 2:
        capacity = (LispHeap1.overflow - LispHeap1.start) * sizeof(Node);
        current = (LispHeap1.current - LispHeap1.start) * sizeof(Node);
        break;
    }
    if (capacity > 0)
        percent = (long)((double)current * 100.0 / (double)capacity);
    if (percent > 100)
        percent = 100;
    if (allocated) *allocated = capacity;
    if (used) *used = current;
    return percent;
}

//
// Given an address (presumably points into a Lisp heap), return the next known
// address of a valid heap object. If the passed address is a valid cons or uvector,
// it will calculate the address of the following block. If it is an invalid
// object (such as a uvector with a size of zero, which should not happen except
// in serious error situations, it will give the address of the next tagged
// cell in the following page.
void* nextHeapObject(void* addr)
{
	Node* p = (Node*)addr;
	if (gettag(p->car) == UvectorLengthTag)
	{
        unsigned long ulength = ((p->car) >> 8) * 2;
		if (ulength == 0)
		{
			unsigned long page_id = address_to_page(addr);
			page_id++;	// advance to next page
			unsigned long offset = PageOffset(page_id);
			return (page_address(page_id) + offset);
		}
		else
			return (p + (ulength / 2));
	}
	else
		return (p + 1);
}


