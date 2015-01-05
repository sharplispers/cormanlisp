//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		compx86.h
//		Contents:	Definitions	and macros for Corman Lisp X86 
//					compiler.
//		History:	8/15/96  RGC  Created.
//
#ifndef COMPX86_H
#define COMPX86_H

void CBaddLong(unsigned long);
void CBaddByte(LispObj);
void CBaddBytes(LispObj, LispObj);
void CBaddBytes(LispObj, LispObj, LispObj);
void CBaddLongVal(LispObj);
void CBaddObject(LispObj);
void CBsetByte(LispObj index, LispObj);
void CBsetObject(LispObj index, LispObj);
void CBsetLong(LispObj index, LispObj);
void CBref(LispObj obj, LispObj pos);
LispObj CBincDynamicEnvSize(LispObj size);
void CBaddStackVar(LispObj sym, LispObj place);
void CBaddHeapVar(LispObj sym, LispObj place);
void CBaddLabels(LispObj labels);
void CBaddBlock(LispObj block);
void CBpushCleanup(LispObj rec);
LispObj CBpopCleanup();
LispObj CBcleanups();
void CBinsertCode(LispObj position, void* buf, LispObj numBytes);
void CBincStackIndex(LispObj num);
void CBdecStackIndex(LispObj num);
void CBsetStackIndex(LispObj num);
LispObj CBstackIndex();
void CBgrowCode();
void CBgrowRefs();
LispObj CBcode();
LispObj CBreferences();
LispObj CBlength(); 
LispObj CBnumReferences();
LispObj CBdynamicEnvSize();
LispObj CBheapEnvSize();
void CBaddJumpTableRef(LispObj sym);
void CBaddEnvTableRef(LispObj sym);
void CBaddVarTableRef(LispObj sym);

#define CHAR_LOW wrapInteger(-128)
#define CHAR_HIGH wrapInteger(127)

// these are defined in limits.h now
//#define CHAR_MIN	-128
//#define CHAR_MAX	127

#define ASM_OP8(x)		(CBaddByte(wrapInteger(x)))
#define ASM_OP16(x,y)	(CBaddBytes(wrapInteger(x), wrapInteger(y)))
#define ASM_OP24(x,y,z)	(CBaddBytes(wrapInteger(x), wrapInteger(y), wrapInteger(z)))
#define IS_BYTE(x)		((x) >= CHAR_MIN && (x) <= CHAR_MAX)
#define ASM_BYTE(x)		(CBaddByte(wrapInteger(x)))
#define ASM_2BYTE(x,y)	(CBaddBytes(wrapInteger(x), wrapInteger(y)))
#define ASM_LONG(x)		(CBaddLongVal(wrapInteger(x)))
#define ASM_OBJ(x)		(CBaddObject(x))
#define INC_STACK(num)	(CBincStackIndex(wrapInteger(num)))
#define DEC_STACK(num)	(CBdecStackIndex(wrapInteger(num)))

#define TEST_EAX_EAX()	{ ASM_OP16(0x85, 0xc0);	}	// test eax, eax
#define TEST_EBX_EBX()	{ ASM_OP16(0x85, 0xdb);	}	// test ebx, ebx
#define TEST_ECX_ECX()	{ ASM_OP16(0x85, 0xc9);	}	// test ecx, ecx
#define TEST_EDX_EDX()	{ ASM_OP16(0x85, 0xd2);	}	// test edx, edx

// cmp ecx, 0x1234
#define CMP_ECX_LONG(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xf9); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xf9); ASM_LONG(num);	}
	
// cmp eax, 0x1234
#define CMP_EAX_LONG(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xf8); ASM_BYTE(num);	} else		\
	{	ASM_OP8(0x3d);		  ASM_LONG(num);	}

// cmp edx, 0x1234
#define CMP_EDX_LONG(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xfa); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xfa); ASM_LONG(num);	}

// cmp eax, dword ptr [esi] ; cmp eax, NIL
#define CMP_EAX_NIL()	ASM_OP16(0x3b, 0x06)

#define CMP_EAX_ESI_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x3b, 0x06);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x3b, 0x46); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x3b, 0x86); ASM_LONG(offset); }

#define CMP_EAX_EBP_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x3b, 0x05);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x3b, 0x45); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x3b, 0x85); ASM_LONG(offset); }

#define CMP_EAX_ESP_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP24(0x3b, 0x04, 0x24);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP24(0x3b, 0x44, 0x24); ASM_BYTE(offset);	} else		\
	{	ASM_OP24(0x3b, 0x84, 0x24); ASM_LONG(offset); }

#define CMP_EDX_ESI_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x3b, 0x16);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x3b, 0x56); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x3b, 0x96); ASM_LONG(offset); }

// cmp eax, dword ptr [esi+4] ; cmp eax, T
#define CMP_EAX_T()	ASM_OP24(0x3b, 0x46, 0x04)

// and edx, 0x1234
#define AND_EDX_LONG(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xe2); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xe2); ASM_LONG(num);	}

// and esp, 0x1234
#define AND_ESP_NUM(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xe4); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xe4); ASM_LONG(num);	}

// test esp, 0x1234
#define TEST_ESP_NUM(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0xF7, 0xc4); ASM_LONG(num);	} else		\
	{	ASM_OP16(0xF7, 0xc4); ASM_LONG(num);	}

// mov ecx, 0x1234
// #define MOV_ECX_LONG(num) { ASM_OP8(0xb9); ASM_LONG((unsigned long)num); }
#define MOV_ECX_LONG(num)									\
	if (num == 0)											\
	{	XOR_ECX_ECX();							} else		\
	{	ASM_OP8(0xb9); ASM_LONG(num);			}

//	mov eax, [esi + offset]
#define MOV_EAX_ESI_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x8b, 0x06);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x8b, 0x46); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0x86); ASM_LONG(offset); }

//	mov edi, [esi + offset]
#define MOV_EDI_ESI_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x8b, 0x3e);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x8b, 0x7e); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0xbe); ASM_LONG(offset); }

//	mov edi, [esp + offset]
#define MOV_EDI_ESP_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP24(0x8b, 0x3c, 0x24);				}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP24(0x8b, 0x7c, 0x24); ASM_BYTE(offset);} else \
	{	ASM_OP24(0x8b, 0xbc, 0x24); ASM_LONG(offset); }

//	mov eax, [edi + offset]
#define MOV_EAX_EDI_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x8b, 0x07);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x8b, 0x47); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0x87); ASM_LONG(offset); }

//	mov eax, [ebp + offset]
#define MOV_EAX_EBP_PTR_WITH_OFFSET(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8b, 0x45); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0x85); ASM_LONG(offset);	}

//	mov eax, [eax + offset]
#define MOV_EAX_EAX_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x8b, 0x00);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x8b, 0x40); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0x80); ASM_LONG(offset); }

//	mov eax, [eax + edx + offset]
#define MOV_EAX_EAX_PLUS_EDX_PTR_WITH_OFFSET(offset)		\
	if ((offset) == 0)										\
	{	ASM_OP24(0x8b, 0x04, 0x10);						}	\
	else if (IS_BYTE(offset))								\
	{	ASM_OP24(0x8b, 0x44, 0x10); ASM_BYTE(offset);	} else	\
	{	ASM_OP24(0x8b, 0x84, 0x10); ASM_LONG(offset); }

//	lea eax, [ebp + offset]
#define LEA_EAX_EBP_PTR_WITH_OFFSET(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8d, 0x45); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8d, 0x85); ASM_LONG(offset);	}

//	lea edx, [eax + offset]
#define LEA_EDX_EAX_PTR_WITH_OFFSET(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8d, 0x50); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8d, 0x90); ASM_LONG(offset);	}

//	mov edi, [ebp + offset]
#define MOV_EDI_EBP_PTR_WITH_OFFSET(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8b, 0x7d); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0xbd); ASM_LONG(offset);	}

//	mov edi, [edi + offset]
#define MOV_EDI_EDI_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x8b, 0x3f);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x8b, 0x7f); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0xbf); ASM_LONG(offset); }

//	mov ecx, [edi + offset]
#define MOV_ECX_EDI_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x8b, 0x0f);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x8b, 0x4f); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0x8f); ASM_LONG(offset); }

//	mov edx, [edi + offset]
#define MOV_EDX_EDI_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x8b, 0x17);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x8b, 0x57); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0x97); ASM_LONG(offset); }

// mov [ebp + offset], eax
#define MOV_EBP_PTR_WITH_OFFSET_EAX(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x89, 0x45); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0x85); ASM_LONG(offset);	}

// mov [ebx + offset], eax
#define MOV_EBX_PTR_WITH_OFFSET_EAX(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x89, 0x03);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x89, 0x43); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0x83); ASM_LONG(offset); }

// mov [edi + edx + offset], eax
#define MOV_EDI_PLUS_EDX_PTR_WITH_OFFSET_EAX(offset)		\
	if ((offset) == 0)										\
	{	ASM_OP24(0x89, 0x04, 0x17);					}		\
	else if (IS_BYTE(offset))								\
	{	ASM_OP24(0x89, 0x44, 0x17); ASM_BYTE(offset);	} else \
	{	ASM_OP24(0x89, 0x84, 0x17); ASM_LONG(offset); }

// mov [ebp + offset], ebx
#define MOV_EBP_PTR_WITH_OFFSET_EBX(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x89, 0x5d); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0x9d); ASM_LONG(offset);	}

// mov [ebp + offset], ecx
#define MOV_EBP_PTR_WITH_OFFSET_ECX(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x89, 0x4d); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0x8d); ASM_LONG(offset);	}

// mov [ebp + offset], edx
#define MOV_EBP_PTR_WITH_OFFSET_EDX(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x89, 0x55); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0x95); ASM_LONG(offset);	}

// mov [ebp + offset], edx
#define MOV_EBP_PTR_WITH_LONG_OFFSET_EDX(offset)			\
	{	ASM_OP16(0x89, 0x95); ASM_LONG(offset);	}

// mov [ebp + offset], esi
#define MOV_EBP_PTR_WITH_OFFSET_ESI(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x89, 0x75); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0xb5); ASM_LONG(offset);	}

// mov [ebp + offset], edi
#define MOV_EBP_PTR_WITH_OFFSET_EDI(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x89, 0x7d); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0xbd); ASM_LONG(offset);	}

// mov [ebp + offset], esp
#define MOV_EBP_PTR_WITH_OFFSET_ESP(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x89, 0x65); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0xa5); ASM_LONG(offset);	}

// mov [ebp + offset], ebp
#define MOV_EBP_PTR_WITH_OFFSET_EBP(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x89, 0x6d); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0xad); ASM_LONG(offset);	}

// mov eax, [ebx + offset]
#define	MOV_EAX_EBX_PTR_WITH_OFFSET(offset)					\
	if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8b, 0x43); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0x83); ASM_LONG(offset);	}

// mov edi, [ebx + offset]
#define	MOV_EDI_EBX_PTR_WITH_OFFSET(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x8b, 0x3b);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x8b, 0x7b); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x8b, 0xbb); ASM_LONG(offset); }

//	mov [eax + offset], edx
#define MOV_EAX_PTR_WITH_OFFSET_EDX(offset)					\
	if ((offset) == 0)										\
	{	ASM_OP16(0x89, 0x10);					}			\
	else if (IS_BYTE(offset))								\
	{	ASM_OP16(0x89, 0x50); ASM_BYTE(offset);	} else		\
	{	ASM_OP16(0x89, 0x90); ASM_LONG(offset); }

// sub eax, num
#define	SUB_ECX_NUM(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xe9); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xe9); ASM_LONG(num);	}

// cmp eax, num
#define	CMP_EAX_NUM(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xf8); ASM_BYTE(num);	} else		\
	{	ASM_OP8(0x3d);		  ASM_LONG(num);	}

// sub esp, num
#define	SUB_ESP_NUM(num)									\
{	INC_STACK(num);											\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xec); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xec); ASM_LONG(num);	}}

// add edi, num
#define	ADD_EDI_NUM(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xc7); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xc7); ASM_LONG(num);	}

// add edx, num
#define	ADD_EDX_NUM(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xc2); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xc2); ASM_LONG(num);	}

// sub edx, num
#define	SUB_EDX_NUM(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xea); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xea); ASM_LONG(num);	}

// sub edi, num
#define	SUB_EDI_NUM(num)									\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xef); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xef); ASM_LONG(num);	}

// add esp, num
#define	ADD_ESP_NUM(num)									\
{	DEC_STACK(num);											\
	if (IS_BYTE(num))										\
	{	ASM_OP16(0x83, 0xc4); ASM_BYTE(num);	} else		\
	{	ASM_OP16(0x81, 0xc4); ASM_LONG(num);	}}

#define JLE_RELATIVE(offset) {ASM_OP16(0x0f, 0x8e); ASM_LONG(offset);}
#define JL_RELATIVE(offset)  {ASM_OP16(0x0f, 0x8c); ASM_LONG(offset);}
#define JG_RELATIVE(offset)  {ASM_OP16(0x0f, 0x8f); ASM_LONG(offset);}
#define JGE_RELATIVE(offset) {ASM_OP16(0x0f, 0x8d); ASM_LONG(offset);}
#define JZ_RELATIVE(offset)  {ASM_OP16(0x0f, 0x84); ASM_LONG(offset);}
#define JNE_RELATIVE(offset) {ASM_OP16(0x0f, 0x85); ASM_LONG(offset);}
#define JNO_RELATIVE(offset) {ASM_OP16(0x0f, 0x81); ASM_LONG(offset);}
#define JE_RELATIVE(offset)	 JZ_RELATIVE(offset)

#define JLE_SHORT_RELATIVE(offset) {ASM_OP8(0x7e); ASM_BYTE(offset);}
#define JL_SHORT_RELATIVE(offset)  {ASM_OP8(0x7c); ASM_BYTE(offset);}
#define JG_SHORT_RELATIVE(offset)  {ASM_OP8(0x7f); ASM_BYTE(offset);}
#define JGE_SHORT_RELATIVE(offset) {ASM_OP8(0x7d); ASM_BYTE(offset);}
#define JZ_SHORT_RELATIVE(offset)  {ASM_OP8(0x74); ASM_BYTE(offset);}
#define JNE_SHORT_RELATIVE(offset) {ASM_OP8(0x75); ASM_BYTE(offset);}
#define JNO_SHORT_RELATIVE(offset) {ASM_OP8(0x71); ASM_BYTE(offset);}
#define JE_SHORT_RELATIVE(offset)	 JZ_SHORT_RELATIVE(offset)

// jmp short relative_address
#define JMP_SHORT_RELATIVE(offset)  {ASM_OP8(0xeb); ASM_BYTE(offset);}

#define JMP_LONG_RELATIVE(offset)	{ASM_OP8(0xe9); ASM_LONG(offset);}

//	call [esi + index + 1]
#define CALL_INDIRECT(symbol)											\
	{ ASM_OP16(0xff, 0x96);												\
	  ASM_LONG((integer(UVECTOR(symbol)[SYMBOL_JUMP_TABLE]) + 1) * 4);  \
	  CBaddJumpTableRef(symbol);  }		

#define MOV_EAX_SYMBOL_VALUE(symbol)	\
	{									\
		MOV_EAX_ESI_PTR_WITH_OFFSET(integer(UVECTOR(symbol)[SYMBOL_VAR_TABLE]) * 4);  \
		CBaddVarTableRef(symbol);					 								  \
		MOV_EAX_EAX_PTR_WITH_OFFSET(-4);	\
	}

#define MOV_EAX_SYMBOL_BINDING(symbol)	\
	{									\
	MOV_EAX_ESI_PTR_WITH_OFFSET(integer(UVECTOR(symbol)[SYMBOL_VAR_TABLE]) * 4) \
	CBaddVarTableRef(symbol);			\
	}

#define MOV_EDI_SYMBOL_BINDING(symbol)	\
	{									\
	MOV_EDI_ESI_PTR_WITH_OFFSET(integer(UVECTOR(symbol)[SYMBOL_VAR_TABLE]) * 4) \
	CBaddVarTableRef(symbol);			\
	}

#define MOV_SYMBOL_BINDING_EAX(symbol)		\
	{										\
	MOV_ESI_PTR_WITH_OFFSET_EAX(integer(UVECTOR(symbol)[SYMBOL_VAR_TABLE]) * 4);   \
	CBaddVarTableRef(symbol);				\
	}

#define CALL_RELATIVE(offset) { ASM_OP8(0xe8); ASM_LONG(offset); }
#define PUSH_EIP() { CALL_RELATIVE(0); INC_STACK(4); }
#define PUSH_EIP_AND_JMP(offset) { CALL_RELATIVE(offset); INC_STACK(4); }

#define MOV_EAX_NIL() MOV_EAX_ESI_PTR_WITH_OFFSET(0)
#define MOV_EAX_T()	  MOV_EAX_ESI_PTR_WITH_OFFSET(4)
#define MOV_EDI_NIL() ASM_OP16(0x8b, 0x3e)		// mov edi, [esi]
#define MOV_EDI_ENV() MOV_EDI_EBP_PTR_WITH_OFFSET(-4);		// mov edi, [ebp - 4] (environment)

#define MOV_EDI_EAX() ASM_OP16(0x8b, 0xf8)		// mov edi, eax

#define PUSH_EAX() { ASM_OP8(0x50); INC_STACK(4); }		// push eax
#define POP_EAX()  { ASM_OP8(0x58); DEC_STACK(4); }		// pop eax
#define PUSH_EBX() { ASM_OP8(0x53); INC_STACK(4); }		// push ebx
#define POP_EBX()  { ASM_OP8(0x5b); DEC_STACK(4); }		// pop ebx
#define PUSH_ECX() { ASM_OP8(0x51); INC_STACK(4); }		// push ecx
#define POP_ECX()  { ASM_OP8(0x59); DEC_STACK(4); }		// pop ecx
#define PUSH_EDX() { ASM_OP8(0x52); INC_STACK(4); }		// push edx
#define POP_EDX()  { ASM_OP8(0x5a); DEC_STACK(4); }		// pop edx
#define PUSH_EDI() { ASM_OP8(0x57); INC_STACK(4); }		// push edi
#define POP_EDI()  { ASM_OP8(0x5f); DEC_STACK(4); }		// pop edi
#define PUSH_EBP() { ASM_OP8(0x55); INC_STACK(4); }		// push ebp
#define POP_EBP()  { ASM_OP8(0x5d); DEC_STACK(4); }		// pop ebp

#define MOV_EAX_ECX() ASM_OP16(0x8b, 0xc1)		// mov eax, ecx
#define MOV_EAX_EDX() ASM_OP16(0x8b, 0xc2)		// mov eax, edx
#define MOV_EAX_EDI() ASM_OP16(0x8b, 0xc7)		// mov eax, edi
#define MOV_EDX_EDI() ASM_OP16(0x8b, 0xd7)		// mov edx, edi
#define MOV_ECX_EAX() ASM_OP16(0x8b, 0xc8)		// mov ecx, eax
#define MOV_EDX_EAX() ASM_OP16(0x8b, 0xd0)		// mov edx, eax
#define MOV_EDI_EBX() ASM_OP16(0x8b, 0xfb)		// mov edi, ebx
#define MOV_ECX_EBX() ASM_OP16(0x8b, 0xcb)		// mov ecx, ebx
#define SUB_ECX_EAX() ASM_OP16(0x2b, 0xc8)		// sub ecx, eax
#define SUB_ECX_ECX() ASM_OP16(0x2b, 0xc9)		// sub ecx, ecx
#define MOV_EBP_ESP() ASM_OP16(0x8b, 0xec)		// mov ebp, esp
#define MOV_ESP_EBP() ASM_OP16(0x8b, 0xe5)		// mov esp, ebp
#define XOR_ECX_ECX() ASM_OP16(0x33, 0xc9)		// xor ecx, ecx
#define XOR_EAX_EAX() ASM_OP16(0x33, 0xc0)		// xor eax, eax
#define XOR_EDX_EDX() ASM_OP16(0x33, 0xd2)		// xor edx, edx

#define MOV_EAX_NUM(num)									\
	if (num == 0)											\
	{	XOR_EAX_EAX();							} else		\
	{ ASM_OP8(0xb8); ASM_LONG(num); } // mov eax, 0x1234

#define ADD_EAX_NUM(num)									\
	{ ASM_OP8(0x05); ASM_LONG(num); } // add eax, 0x12345678

#define MOV_EAX_OBJ(num)									\
	if (num == 0)											\
	{	XOR_EAX_EAX();							} else		\
	{ ASM_OP8(0xb8); ASM_OBJ(num);  } // mov eax, 0x1234

#define MOV_EDX_OBJ(num)									\
	if (num == 0)											\
	{	XOR_EDX_EDX();							} else		\
	{ ASM_OP8(0xba); ASM_OBJ(num);  } // mov edx, 0x1234

#define MOV_EDX_NUM(num) { ASM_OP8(0xba); ASM_LONG(num); } // mov edx, 0x1234
#define MOV_EDI_NUM(num) { ASM_OP8(0xbf); ASM_LONG(num); } // mov edi, 0x1234

#define MOV_EDI_ESP() { ASM_OP16(0x8b, 0xfc); } // mov edi, esp
#define MOV_EAX_ESP() { ASM_OP16(0x8b, 0xc4); } // mov eax, esp

#define DEC_EAX()  ASM_OP8(0x48)				// dec eax
#define DEC_ECX()  ASM_OP8(0x49)				// dec ecx
#define DEC_EDX()  ASM_OP8(0x4A)				// dec edx

#define SHL_EAX_NUM(num) { ASM_OP16(0xc1, 0xe0); ASM_BYTE(num); } // shl eax, 2
#define SHL_ECX_NUM(num) { ASM_OP16(0xc1, 0xe1); ASM_BYTE(num); } // shl ecx, 2
#define SHR_EAX_NUM(num) { ASM_OP16(0xc1, 0xe8); ASM_BYTE(num); } // shr eax, 2
#define SHR_EDX_NUM(num) { ASM_OP16(0xc1, 0xea); ASM_BYTE(num); } // shr edx, 2

#define NEG_EAX()	  ASM_OP16(0xf7, 0xd8)		// neg eax
#define TST_ECX_ECX() ASM_OP16(0x85, 0xc9)		// tst ecx, ecx			 
#define SUB_EDI_EAX() ASM_OP16(0x2b, 0xf8)		// sub edi, eax			 
#define ADD_EAX_EBX() ASM_OP16(0x03, 0xc3)		// add eax, ebx			 
#define ADD_EAX_EDX() ASM_OP16(0x03, 0xc2)		// add eax, edx			 
#define ADD_EDI_EDX() ASM_OP16(0x03, 0xfa)		// add edi, edx			 
#define SUB_EAX_EDX() ASM_OP16(0x2b, 0xc2)		// sub eax, edx			 
#define SUB_EDX_EAX() ASM_OP16(0x2b, 0xd0)		// sub edx, eax			 

#define CMP_EDI_ECX() ASM_OP16(0x3b, 0xf9)		// cmp edi, ecx			 
#define CMP_EAX_EDX() ASM_OP16(0x3b, 0xc2)		// cmp eax, edx			 

// push [edi + ebx + 0x12]
#define	PUSH_EDI_EBX_WITH_OFFSET(offset)						\
{	INC_STACK(4);												\
	if ((offset) == 0)											\
	{	ASM_OP24(0xff, 0x34, 0x1f);					}			\
	else if (IS_BYTE(offset))									\
	{	ASM_OP24(0xff, 0x74, 0x1f); ASM_BYTE(offset); } else	\
	{	ASM_OP24(0xff, 0xb4, 0x1f); ASM_LONG(offset); }}

// push [ebx + 0x12]
#define	PUSH_EBX_PTR_WITH_OFFSET(offset)						\
{	INC_STACK(4);												\
	if ((offset) == 0)											\
	{	ASM_OP16(0xff, 0x33);					}				\
	else if (IS_BYTE(offset))									\
	{	ASM_OP16(0xff, 0x73); ASM_BYTE(offset); } else			\
	{	ASM_OP16(0xff, 0xb3); ASM_LONG(offset); }}

// push [ebp + 0x12]
#define	PUSH_EBP_PTR_WITH_OFFSET(offset)						\
{	INC_STACK(4);												\
	if (IS_BYTE(offset))										\
	{	ASM_OP16(0xff, 0x75); ASM_BYTE(offset);	} else			\
	{	ASM_OP16(0xff, 0xb5); ASM_LONG(offset);	}}

// push [edi + 0x12]
#define	PUSH_EDI_PTR_WITH_OFFSET(offset)						\
{	INC_STACK(4);												\
	if ((offset) == 0)											\
	{	ASM_OP16(0xff, 0x37);					}				\
	else if (IS_BYTE(offset))									\
	{	ASM_OP16(0xff, 0x77); ASM_BYTE(offset); } else			\
	{	ASM_OP16(0xff, 0xb7); ASM_LONG(offset); }}

// push [esi + 0x1234]
#define	PUSH_ESI_PTR_WITH_OFFSET(offset)						\
{	INC_STACK(4);												\
	if ((offset) == 0)											\
	{	ASM_OP16(0xff, 0x36);					}				\
	else if (IS_BYTE(offset))									\
	{	ASM_OP16(0xff, 0x76); ASM_BYTE(offset); } else			\
	{	ASM_OP16(0xff, 0xb6); ASM_LONG(offset); }}

// pop [esi + 0x1234]
#define	POP_ESI_PTR_WITH_OFFSET(offset)							\
{	DEC_STACK(4);												\
	if ((offset) == 0)											\
	{	ASM_OP16(0x8f, 0x06);					}				\
	else if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8f, 0x46); ASM_BYTE(offset); } else			\
	{	ASM_OP16(0x8f, 0x86); ASM_LONG(offset); }}

// push 0x12
#define	PUSH_NUM(n)												\
{	INC_STACK(4);												\
	if (IS_BYTE(n))												\
	{	ASM_OP8(0x6a); ASM_BYTE(n);	} else						\
	{	ASM_OP8(0x68); ASM_LONG(n);	}}

#define	PUSH_OBJ(n)												\
{	INC_STACK(4);												\
	if (IS_BYTE(n))												\
	{	ASM_OP8(0x6a); ASM_BYTE(n);	} else						\
	{	ASM_OP8(0x68); ASM_OBJ(n);	}}

// pop dword ptr [eax + 0x12]
#define	POP_EAX_PTR_WITH_OFFSET(offset)							\
{	DEC_STACK(4);												\
	if ((offset) == 0)											\
	{	ASM_OP16(0x8f, 0x00);					}				\
	else if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8f, 0x40); ASM_BYTE(offset); } else			\
	{	ASM_OP16(0x8f, 0x80); ASM_LONG(offset); }}

// pop dword ptr [edi + 0x12]
#define	POP_EDI_PTR_WITH_OFFSET(offset)							\
{	DEC_STACK(4);												\
	if ((offset) == 0)											\
	{	ASM_OP16(0x8f, 0x07);					}				\
	else if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8f, 0x47); ASM_BYTE(offset); } else			\
	{	ASM_OP16(0x8f, 0x87); ASM_LONG(offset); }}

// pop dword ptr [ebp + 0x12]
#define	POP_EBP_PTR_WITH_OFFSET(offset)							\
{	DEC_STACK(4);												\
	if ((offset) == 0)											\
	{	ASM_OP16(0x8f, 0x45);					}				\
	else if (IS_BYTE(offset))									\
	{	ASM_OP16(0x8f, 0x45); ASM_BYTE(offset); } else			\
	{	ASM_OP16(0x8f, 0x85); ASM_LONG(offset); }}

// or [eax + offset], 0x12
#define OR_EAX_PTR_WITH_OFFSET_NUM(offset, num)					\
	if (IS_BYTE(num))											\
		ASM_OP8(0x83);											\
	else ASM_OP8(0x81);											\
	if (IS_BYTE(offset))										\
	{ ASM_OP8(0x48); ASM_BYTE(offset); } else					\
	{ ASM_OP8(0x88); ASM_LONG(offset); }						\
 	if (IS_BYTE(num))											\
		ASM_BYTE(num);											\
	else ASM_LONG(num);											\

  // or [edi + offset], 0x12
#define OR_EDI_PTR_WITH_OFFSET_NUM(offset, num)					\
	if (IS_BYTE(num))											\
		ASM_OP8(0x83);											\
	else ASM_OP8(0x81);											\
	if (IS_BYTE(offset))										\
	{ ASM_OP8(0x4f); ASM_BYTE(offset); } else					\
	{ ASM_OP8(0x8f); ASM_LONG(offset); }						\
 	if (IS_BYTE(num))											\
		ASM_BYTE(num);											\
	else ASM_LONG(num);											\

  // mov [eax + offset], 0x12
#define MOV_EAX_PTR_WITH_OFFSET_NUM(offset, num)							\
	if (IS_BYTE(offset))													\
	{ ASM_OP16(0xc7, 0x40); ASM_BYTE(offset); ASM_LONG(num); }	else		\
	{ ASM_OP16(0xc7, 0x80);	ASM_LONG(offset); ASM_LONG(num); }

  // mov [eax + offset], 0x12
#define MOV_EAX_PTR_WITH_OFFSET_OBJ(offset, num)							\
	if (IS_BYTE(offset))													\
	{ ASM_OP16(0xc7, 0x40); ASM_BYTE(offset); ASM_OBJ(num); }	else		\
	{ ASM_OP16(0xc7, 0x80);	ASM_LONG(offset); ASM_OBJ(num); }

#define TEST_EAX_NUM(num) { ASM_OP8(0xa9); ASM_LONG(num); }			// test eax, 0x12
#define TEST_AL_NUM(num)  { ASM_OP8(0xa8); ASM_BYTE(num); }			// test al, 0x12
#define TEST_EDX_NUM(num) { ASM_OP16(0xf7, 0xc2); ASM_LONG(num); }	// test edx, 0x12
#define TEST_DL_NUM(num)  { ASM_OP16(0xf6, 0xc2); ASM_BYTE(num); }	// test dl, 0x12

#define PUSH_NIL()	{ INC_STACK(4); ASM_OP16(0xff, 0x36); }			// push dword ptr [esi]

// mov [edi + 0x12], eax
#define	MOV_EDI_PTR_WITH_OFFSET_EAX(offset)						\
	if (IS_BYTE(offset))										\
	{	ASM_OP16(0x89, 0x47); ASM_BYTE(offset);	} else			\
	{	ASM_OP16(0x89, 0x87); ASM_LONG(offset);	}

// mov [esi + 0x12], eax
#define	MOV_ESI_PTR_WITH_OFFSET_EAX(offset)						\
	if (IS_BYTE(offset))										\
	{	ASM_OP16(0x89, 0x46); ASM_BYTE(offset);	} else			\
	{	ASM_OP16(0x89, 0x86); ASM_LONG(offset);	}

// mov [esi + 0x12], edx
#define	MOV_ESI_PTR_WITH_OFFSET_EDX(offset)						\
	if (IS_BYTE(offset))										\
	{	ASM_OP16(0x89, 0x56); ASM_BYTE(offset);	} else			\
	{	ASM_OP16(0x89, 0x96); ASM_LONG(offset);	}

// mov [eax + 0x12], edi
#define	MOV_EAX_PTR_WITH_OFFSET_EDI(offset)						\
	if (IS_BYTE(offset))										\
	{	ASM_OP16(0x89, 0x78); ASM_BYTE(offset);	} else			\
	{	ASM_OP16(0x89, 0xb8); ASM_LONG(offset);	}


#define LEAVE() ASM_OP8(0xc9);			// leave
#define RET()	ASM_OP8(0xc3);			// ret
#define INC_ECX() ASM_OP8(0x41);		// inc ecx
#define STD()	ASM_OP8(0xfd);			// std	--  set direction flag
#define CLD()	ASM_OP8(0xfc);			// cld  --  clear direction flag

#define BEGIN_ATOMIC()	STD()
#define END_ATOMIC()	CLD()

//	mov edi, [esi + index]
#define LOAD_ENVIRONMENT(symbol)	\
	{								\
	MOV_EDI_ESI_PTR_WITH_OFFSET(integer(UVECTOR(symbol)[SYMBOL_JUMP_TABLE]) * 4) \
	CBaddEnvTableRef(symbol);		\
	}		

#define SET_ARG_COUNT(num)									\
		if (num != 0)									\
			{ MOV_ECX_LONG(num);	}					\
		else { XOR_ECX_ECX(); }

#define TARGET(num)		{	if (dest == Dest_EAX)		\
							{							\
								SET_ARG_COUNT(num);		\
							}							\
							else if (dest == Dest_Stack)\
							{							\
								PUSH_EAX();				\
							}							\
						}

#endif	// COMPX86_H

