#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "../distorm/include/distorm.h"
#include "../distorm/include/mnemonics.h"

#define OUTPUT_BUF_SIZE (128)
#define MAX_INSTRUCTION_SIZE (15) // maximum size of the instruction on x86

char gDisassemblyOutputBuf[OUTPUT_BUF_SIZE];

long unassemble(unsigned long addr, unsigned long offset)
{
	_CodeInfo ci;
	_DInst inst;
	_DecodedInst format;
	unsigned int count = 0;
	char hex_buf[OUTPUT_BUF_SIZE] = { 0 };
	char mnemonic_buf[OUTPUT_BUF_SIZE] = { 0 };

	memset(&ci, 0, sizeof(ci));
	ci.code = (const unsigned char*)(addr + offset);
	ci.codeOffset = 0;
	ci.codeLen = MAX_INSTRUCTION_SIZE;
	ci.dt = Decode32Bits;
	ci.features = DF_STOP_ON_RET;

	distorm_decompose(&ci, &inst, 1, &count);
	if (count == 0)
	{
		return 0;
	}

	/* format the data */
	distorm_format(&ci, &inst, &format);
	/* hex data - to upper case */
	{
		size_t i = 0;
		const char *p = (char*)format.instructionHex.p;
		while (p[i])
		{
			hex_buf[i] = toupper(p[i]);
			i++;
		}
	}

	/* mnemonic instruction data - to lower case */
	{
		size_t i = 0;
		sprintf(mnemonic_buf, "%s%s%s", (char*)format.mnemonic.p, format.operands.length != 0 ? " " : "", (char*)format.operands.p);

		char *p = (char*)mnemonic_buf;
		while (p[i])
		{
			p[i] = tolower(p[i]);
			i++;
		}
	}

	sprintf(gDisassemblyOutputBuf, "%-24s %s", (const char *)hex_buf, (const char *)mnemonic_buf);

	/* stop on RET */
	if (inst.opcode == I_RET)
	{
		return 0;
	}

	return format.size;
}
