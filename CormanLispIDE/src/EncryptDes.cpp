//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		EncryptDES.cpp
//		Contents:	Class for DES encryption.
//		History:	10/21/97  RGC  Created.
//
//		This class is based on a public domain implementation of
//		the DES (Data Encryption Standard) algorithm by 
//		Richard Outerbridge, version D3DES (V5.09).
//

#include "stdafx.h"

#include <string.h>
#include <stdio.h>

#include <EncryptDES.h>

#define TEST 0		// set this to 1 to compile a test console app
					// set this to 0 to include in another component

static char* binaryToHex(char* dest, const unsigned char* src, int length);
static unsigned char* hexToBinary(unsigned char* dest, const char* src);
static void reverseBytes(const DESBlock* src, DESBlock* dest);

EncryptDES::EncryptDES() : buf(0), bufSize(0)
{
	memset(processedKey, 0, sizeof processedKey);
}

EncryptDES::~EncryptDES()
{
	clear();
}

void EncryptDES::clear()
{
	delete [] buf;
	buf = 0;
	bufSize = 0;
}

//
//	EncryptDES::encrypt()
//
//	You need to pass this member function:
//
//	key:		ascii, null-terminated	string of 1 - 8 characters
//	srcbuf:		pointer to buffer of bytes to encrypt
//	srclen:		number of bytes in buffer to be encrypted
//
//	Returns -1 on error, or the number of bytes resulting
//	from the encryption process if successful.
//
int EncryptDES::encrypt(const char* key, const unsigned char* srcbuf, unsigned int srclen)
{
	clear();		// clear out anything that was already there

	// validate the key
	int keylen = strlen(key);
	if (keylen < 1 || keylen > 8)
	{
		ASSERT(0);			// invalid key length
		return -1;
	}

	int numBlocks = srclen / sizeof DESBlock;		// number of 8-byte blocks
	int extra = srclen % sizeof DESBlock;			// number of bytes left after those blocks

	bufSize = (numBlocks + 1) * sizeof DESBlock;
	buf = new unsigned char[bufSize];

	DESBlock* src = (DESBlock*)srcbuf;
	DESBlock* dest = (DESBlock*)buf;

	processKey(key, false);
	
	DESBlock lastBlock = {0, 0};
	DESBlock block;

	for (int i = 0; i < numBlocks; i++)
	{
		block = src[i];
        if (cbcMode) 
		{
			// In Cipher Block Chaining mode, first XOR the input
            // with the output from the last block.
           block.data[0] ^= lastBlock.data[0];
           block.data[1] ^= lastBlock.data[1];
        }
            
		encryptBlock(&block);

        // Save the encrypted output of this block for next time.  
		// Needed in CBC mode only.
		lastBlock = block;
		dest[i] = block;
 	}

	// copy last bytes into the block
	block.data[0] = block.data[1] = 0;
	memcpy(&block, src + numBlocks, extra);

	// last byte is number of valid bytes in last block
	((unsigned char*)&block)[7] = (unsigned char)extra;		
	if (cbcMode) 
	{
       block.data[0] ^= lastBlock.data[0];
       block.data[1] ^= lastBlock.data[1];
	}
	encryptBlock(&block);
	*(dest + numBlocks) = block;
	return (numBlocks + 1) * sizeof DESBlock;
}

//
//	EncryptDES::decrypt()
//
//	You need to pass this member function:
//
//	key:		ascii, null-terminated	string of 1 - 8 characters
//	destbuf:	pointer to buffer to hold destination bytes (unencrypted)
//	destlen:	capacity (in bytes) of destination buffer, such that 
//					destlen >= getBinaryLength()
//
//	Returns -1 on error, or the number of bytes resulting
//	from the decryption process if successful.
//	The key must be an 8-character ascii string.
//	The unencrypted string will automatically have a 0 byte
//	added to the end (if there is room in the destination buffer).
//
//	Note: the function may or may not return an error if the key is wrong. 
//	However, in either case the data returned will be garbage.
//
int EncryptDES::decrypt(const char* key, unsigned char* destbuf, unsigned int destlen)
{
	// validate the key
	int keylen = strlen(key);
	if (keylen < 1 || keylen > 8)
	{
		ASSERT(0);			// invalid key length
		return -1;
	}

	if (!buf)
	{
		if (destlen > 0)
			destbuf[0] = 0;
		return 0;			// nothing to decrypt
	}

	int numBlocks = bufSize / 8;	// number of 8-byte blocks
	ASSERT((bufSize % sizeof DESBlock) == 0);

	if (destlen < bufSize)
	{
		ASSERT(0);		// error--destination buffer is too short
		return -1;
	}

	processKey(key, true);
	
	DESBlock* src = (DESBlock*)buf;
	DESBlock* dest = (DESBlock*)destbuf;


	DESBlock lastBlock = {0, 0};
	DESBlock block;
	DESBlock origBlock;

	for (int i = 0; i < numBlocks - 1; i++)
	{
		origBlock = block = src[i];
		encryptBlock(&block);
		if (cbcMode) 
		{
			block.data[0] ^= lastBlock.data[0];
			block.data[1] ^= lastBlock.data[1];
			lastBlock = origBlock;
        }
		dest[i] = block;
 	}

	// handle last block
	block = *(src + numBlocks - 1);
	encryptBlock(&block);
	if (cbcMode) 
	{
		block.data[0] ^= lastBlock.data[0];
		block.data[1] ^= lastBlock.data[1];
    }

	int extra = ((unsigned char*)&block)[7];
	if (extra > 7)
	{
//		ASSERT(0);	// DES decryption error
		return -1;
	}

	// copy last bytes to the destination
	memcpy(dest + numBlocks - 1, &block, extra);
	unsigned int numDestBytes = (numBlocks - 1) * sizeof DESBlock + extra;
	
	// null terminate the result if there is room in the buffer
	if (numDestBytes < destlen)
		destbuf[numDestBytes] = 0;	
	return numDestBytes;
}

CString EncryptDES::EncryptCString(const CString& key, const CString& source)
{
	EncryptDES des;
	des.encrypt(key, (const LPBYTE)(const char*)source, source.GetLength());
	return des.exportHex();
}

CString EncryptDES::DecryptCString(const CString& key, const CString& hex)
{
	EncryptDES des;
	CString s;
	des.importHex(hex);
	int decryptedLength = 
		des.decrypt(key, (LPBYTE)s.GetBuffer(des.getBinaryLength()),
		des.getBinaryLength());
	s.ReleaseBuffer(decryptedLength);
	return s;
}

int EncryptDES::getBinaryLength() { return bufSize; }

int EncryptDES::getHexStringLength() { return bufSize * 2; }

void EncryptDES::exportBinary(unsigned char* destbuf)
{
	memcpy(destbuf, buf, bufSize);
}

CString EncryptDES::exportHex()
{
	CString result;
	if (bufSize == 0)
		return CString();	// return empty string
	char* destbuf = result.GetBuffer(bufSize * 2);
	binaryToHex(destbuf, buf, bufSize);
	result.ReleaseBuffer(bufSize * 2);
	return result;
}

// The number of bytes to load must be divisible by 8. 
void EncryptDES::importBinary(unsigned char* srcbuf, unsigned int srclen)
{
	clear();
	ASSERT((srclen % sizeof DESBlock) == 0);
	buf = new unsigned char[srclen];
	bufSize = srclen;
	memcpy(buf, srcbuf, bufSize);
}	

//
//	Expects an ascii string containing the hex representation of a string
//	of binary bytes such that the length is divisible by 8
void EncryptDES::importHex(const char* srcbuf)
{
	clear();
	unsigned int srclen = strlen(srcbuf);
	ASSERT((srclen % (sizeof DESBlock * 2)) == 0);
	bufSize = srclen / 2;
	buf = new unsigned char[bufSize];
	hexToBinary(buf, srcbuf);
}

void
EncryptDES::processKey(const char* key, bool decipher)
{
	// Process an ASCII key.  As is common with DES keys,
	// set the top bit of each byte to reflect odd parity.
	// Karn's DES code ignores the bottom bit of each byte.
	//
	unsigned char localKey[8];
	int keylen = strlen(key);
	ASSERT(keylen > 0 && keylen <= 8);

	int j;
	for (j = 0; j < keylen; j++) 
	{
		int c = key[j];
		int parity, bit;
		for (parity = 0x80, bit = 0; bit < 7; bit++) 
		{
			c <<= 1;
			parity ^= (c & 0x80);
		}
		localKey[j] = (unsigned char)((key[j] & 0x7f) | parity);
	}

	// Zero-pad the key (with odd parity)
	for(; j < 8; j++) 
		localKey[j] = 0x80;

	deskey(localKey, decipher);
}

void EncryptDES::deskey(unsigned char* key, bool decipher)
{
	int i, j, l, m, n;
	unsigned char pc1m[56], pcr[56];
	unsigned long kn[32];

	for (j = 0; j < 56; j++) 
	{
		l = pc1[j];
		m = l & 07;
		pc1m[j] = (unsigned char)((key[l >> 3] & bytebit[m]) ? 1 : 0);
	}
	for (i = 0; i < 16; i++) 
	{
		if (decipher) 
			m = (15 - i) << 1;
		else 
			m = i << 1;
		n = m + 1;
		kn[m] = kn[n] = 0L;
		for (j = 0; j < 28; j++) 
		{
			l = j + totrot[i];
			if (l < 28) 
				pcr[j] = pc1m[l];
			else 
				pcr[j] = pc1m[l - 28];
		}
		for (j = 28; j < 56; j++) 
		{
			l = j + totrot[i];
			if (l < 56 )
				pcr[j] = pc1m[l];
			else pcr[j] = pc1m[l - 28];
		}
		for (j = 0; j < 24; j++) 
		{
			if (pcr[pc2[j]]) 
				kn[m] |= bigbyte[j];
			if (pcr[pc2[j+24]]) 
				kn[n] |= bigbyte[j];
		}
	}

	unsigned long* cook = processedKey;
	unsigned long* raw1 = kn;
	for (i = 0; i < 16; i++, raw1++)
	{
		unsigned long* raw0 = raw1++;
		*cook  = (*raw0 & 0x00fc0000L) << 6;
		*cook |= (*raw0 & 0x00000fc0L) << 10;
		*cook |= (*raw1 & 0x00fc0000L) >> 10;
		*cook |= (*raw1 & 0x00000fc0L) >> 6;
		cook++;
		*cook  = (*raw0 & 0x0003f000L) << 12;
		*cook |= (*raw0 & 0x0000003fL) << 16;
		*cook |= (*raw1 & 0x0003f000L) >> 4;
		*cook |= (*raw1 & 0x0000003fL);
		cook++;
	}
}

//
//	Encrypt one 8-byte block. 
//
void EncryptDES::encryptBlock(DESBlock* src)
{
	DESBlock block;
	unsigned long* keys = processedKey;

	reverseBytes(src, &block);

	unsigned long fval, work, right, left, temp;

	left  = block.data[0];
	right = block.data[1];

	// initial permutation
	work = ((left >> 4) ^ right) & 0x0f0f0f0fL;
	right ^= work;
	left ^= (work << 4);
	work = ((left >> 16) ^ right) & 0x0000ffffL;
	right ^= work;
	left ^= (work << 16);
	work = ((right >> 2) ^ left) & 0x33333333L;
	left ^= work;
	right ^= (work << 2);
	work = ((right >> 8) ^ left) & 0x00ff00ffL;
	left ^= work;
	right ^= (work << 8);
	right = ((right << 1) | ((right >> 31) & 1L)) & 0xffffffffL;
	work = (left ^ right) & 0xaaaaaaaaL;
	left ^= work;
	right ^= work;
	left = ((left << 1) | ((left >> 31) & 1L)) & 0xffffffffL;

	//
	//	16-round S-Box substitution and P-Box permutation
	//
	for (int round = 0; round < 16; round++) 
	{
		work  = (right << 28) | (right >> 4);
		work ^= *keys++;
		fval  = SP7[ work     & 0x3fL];
		fval |= SP5[(work >>  8) & 0x3fL];
		fval |= SP3[(work >> 16) & 0x3fL];
		fval |= SP1[(work >> 24) & 0x3fL];
		work  = right ^ *keys++;
		fval |= SP8[ work     & 0x3fL];
		fval |= SP6[(work >>  8) & 0x3fL];
		fval |= SP4[(work >> 16) & 0x3fL];
		fval |= SP2[(work >> 24) & 0x3fL];
		left ^= fval;

		temp = left, left = right, right = temp;	// swap left, right
	}

	// Final permutation (inverse of initial permutation)
	right = (right << 31) | (right >> 1);
	work = (left ^ right) & 0xaaaaaaaaL;
	left ^= work;
	right ^= work;
	left = (left << 31) | (left >> 1);
	work = ((left >> 8) ^ right) & 0x00ff00ffL;
	right ^= work;
	left ^= (work << 8);
	work = ((left >> 2) ^ right) & 0x33333333L;
	right ^= work;
	left ^= (work << 2);
	work = ((right >> 16) ^ left) & 0x0000ffffL;
	left ^= work;
	right ^= (work << 16);
	work = ((right >> 4) ^ left) & 0x0f0f0f0fL;
	left ^= work;
	right ^= (work << 4);
	block.data[0] = right;
	block.data[1] = left;
	
	reverseBytes(&block, src);
}

// reverse the byte orders within the 2 32-bit words of the block
static void reverseBytes(const DESBlock* src, DESBlock* dest)
{
#ifdef _M_IX86
	__asm	mov	eax, dword ptr src
	__asm	mov	edx, dword ptr dest
	__asm	mov	ecx, dword ptr [eax]
	__asm	bswap ecx
	__asm	mov dword ptr [edx], ecx
	__asm	mov	ecx, dword ptr [eax + 4]
	__asm	bswap ecx
	__asm	mov dword ptr [edx + 4], ecx
#else
	const unsigned char* s = (const unsigned char*)src;
	dest->data[0] = s[0] << 24 | s[1] << 16 | (s[2] << 8) | s[3];
	dest->data[1] = s[4] << 24 | s[5] << 16 | (s[6] << 8) | s[7];
#endif
}

inline char nibToHex(int nib)
{
	return (char)(nib > 9 ? 'A' + (nib - 10)  : '0' + nib);
}

inline char hexToNib(int hexdigit)
{
	hexdigit = toupper(hexdigit);
	return (char)(hexdigit >= 'A' ? (hexdigit - 'A') + 10 : hexdigit - '0');
}

static char* binaryToHex(char* dest, const unsigned char* src, int length)
{
	char* ret = dest;
	for (int i = 0; i < length; i++)
	{
		int highNib = *src >> 4;
		int lowNib  = *src &  0xf;
		*dest++ = nibToHex(highNib);
		*dest++ = nibToHex(lowNib);
		src++;
	}
	*dest = 0;
	return ret;
}

static unsigned char* hexToBinary(unsigned char* dest, const char* src)
{
	unsigned char* ret = dest;
	while (*src && *(src + 1))
	{
		int highNib = hexToNib(*src++);
		int lowNib = hexToNib(*src++);
		*dest++ = (unsigned char)((highNib << 4) + lowNib);
	}
	return ret;
}

//
//	Static tables are defined below. These are never modified, and
//	therefore do not cause a problem for reentrancy.
//
const unsigned char EncryptDES::pc1[56] = 
{
   56, 48, 40, 32, 24, 16,  8,    0, 57, 49, 41, 33, 25, 17,
    9,  1, 58, 50, 42, 34, 26,   18, 10,  2, 59, 51, 43, 35,
   62, 54, 46, 38, 30, 22, 14,    6, 61, 53, 45, 37, 29, 21,
   13,  5, 60, 52, 44, 36, 28,   20, 12,  4, 27, 19, 11,  3 
};

const unsigned char EncryptDES::pc2[48] = 
{
   13, 16, 10, 23,  0,  4,  2, 27, 14,  5, 20,  9,
   22, 18, 11,  3, 25,  7, 15,  6, 26, 19, 12,  1,
   40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47,
   43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 28, 31 
};

const unsigned short EncryptDES::bytebit[8] = 
{
   0200, 0100, 040, 020, 010, 04, 02, 01 
};

const unsigned char EncryptDES::totrot[16] = 
{
   1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28 
};

const unsigned long EncryptDES::bigbyte[24] = 
{
   0x800000L,	0x400000L,  0x200000L,  0x100000L,
   0x80000L,	0x40000L,   0x20000L,   0x10000L,
   0x8000L,		0x4000L,	0x2000L,	0x1000L,
   0x800L,		0x400L,     0x200L,     0x100L,
   0x80L,		0x40L,      0x20L,      0x10L,
   0x8L,		0x4L,		0x2L,		0x1L  
};

const unsigned long EncryptDES::SP1[64] = 
{
	0x01010400L, 0x00000000L, 0x00010000L, 0x01010404L,
	0x01010004L, 0x00010404L, 0x00000004L, 0x00010000L,
	0x00000400L, 0x01010400L, 0x01010404L, 0x00000400L,
	0x01000404L, 0x01010004L, 0x01000000L, 0x00000004L,
	0x00000404L, 0x01000400L, 0x01000400L, 0x00010400L,
	0x00010400L, 0x01010000L, 0x01010000L, 0x01000404L,
	0x00010004L, 0x01000004L, 0x01000004L, 0x00010004L,
	0x00000000L, 0x00000404L, 0x00010404L, 0x01000000L,
	0x00010000L, 0x01010404L, 0x00000004L, 0x01010000L,
	0x01010400L, 0x01000000L, 0x01000000L, 0x00000400L,
	0x01010004L, 0x00010000L, 0x00010400L, 0x01000004L,
	0x00000400L, 0x00000004L, 0x01000404L, 0x00010404L,
	0x01010404L, 0x00010004L, 0x01010000L, 0x01000404L,
	0x01000004L, 0x00000404L, 0x00010404L, 0x01010400L,
	0x00000404L, 0x01000400L, 0x01000400L, 0x00000000L,
	0x00010004L, 0x00010400L, 0x00000000L, 0x01010004L 
};

const unsigned long EncryptDES::SP2[64] = 
{
	0x80108020L, 0x80008000L, 0x00008000L, 0x00108020L,
	0x00100000L, 0x00000020L, 0x80100020L, 0x80008020L,
	0x80000020L, 0x80108020L, 0x80108000L, 0x80000000L,
	0x80008000L, 0x00100000L, 0x00000020L, 0x80100020L,
	0x00108000L, 0x00100020L, 0x80008020L, 0x00000000L,
	0x80000000L, 0x00008000L, 0x00108020L, 0x80100000L,
	0x00100020L, 0x80000020L, 0x00000000L, 0x00108000L,
	0x00008020L, 0x80108000L, 0x80100000L, 0x00008020L,
	0x00000000L, 0x00108020L, 0x80100020L, 0x00100000L,
	0x80008020L, 0x80100000L, 0x80108000L, 0x00008000L,
	0x80100000L, 0x80008000L, 0x00000020L, 0x80108020L,
	0x00108020L, 0x00000020L, 0x00008000L, 0x80000000L,
	0x00008020L, 0x80108000L, 0x00100000L, 0x80000020L,
	0x00100020L, 0x80008020L, 0x80000020L, 0x00100020L,
	0x00108000L, 0x00000000L, 0x80008000L, 0x00008020L,
	0x80000000L, 0x80100020L, 0x80108020L, 0x00108000L 
};

const unsigned long EncryptDES::SP3[64] = 
{
	0x00000208L, 0x08020200L, 0x00000000L, 0x08020008L,
	0x08000200L, 0x00000000L, 0x00020208L, 0x08000200L,
	0x00020008L, 0x08000008L, 0x08000008L, 0x00020000L,
	0x08020208L, 0x00020008L, 0x08020000L, 0x00000208L,
	0x08000000L, 0x00000008L, 0x08020200L, 0x00000200L,
	0x00020200L, 0x08020000L, 0x08020008L, 0x00020208L,
	0x08000208L, 0x00020200L, 0x00020000L, 0x08000208L,
	0x00000008L, 0x08020208L, 0x00000200L, 0x08000000L,
	0x08020200L, 0x08000000L, 0x00020008L, 0x00000208L,
	0x00020000L, 0x08020200L, 0x08000200L, 0x00000000L,
	0x00000200L, 0x00020008L, 0x08020208L, 0x08000200L,
	0x08000008L, 0x00000200L, 0x00000000L, 0x08020008L,
	0x08000208L, 0x00020000L, 0x08000000L, 0x08020208L,
	0x00000008L, 0x00020208L, 0x00020200L, 0x08000008L,
	0x08020000L, 0x08000208L, 0x00000208L, 0x08020000L,
	0x00020208L, 0x00000008L, 0x08020008L, 0x00020200L 
};

const unsigned long EncryptDES::SP4[64] = 
{
	0x00802001L, 0x00002081L, 0x00002081L, 0x00000080L,
	0x00802080L, 0x00800081L, 0x00800001L, 0x00002001L,
	0x00000000L, 0x00802000L, 0x00802000L, 0x00802081L,
	0x00000081L, 0x00000000L, 0x00800080L, 0x00800001L,
	0x00000001L, 0x00002000L, 0x00800000L, 0x00802001L,
	0x00000080L, 0x00800000L, 0x00002001L, 0x00002080L,
	0x00800081L, 0x00000001L, 0x00002080L, 0x00800080L,
	0x00002000L, 0x00802080L, 0x00802081L, 0x00000081L,
	0x00800080L, 0x00800001L, 0x00802000L, 0x00802081L,
	0x00000081L, 0x00000000L, 0x00000000L, 0x00802000L,
	0x00002080L, 0x00800080L, 0x00800081L, 0x00000001L,
	0x00802001L, 0x00002081L, 0x00002081L, 0x00000080L,
	0x00802081L, 0x00000081L, 0x00000001L, 0x00002000L,
	0x00800001L, 0x00002001L, 0x00802080L, 0x00800081L,
	0x00002001L, 0x00002080L, 0x00800000L, 0x00802001L,
	0x00000080L, 0x00800000L, 0x00002000L, 0x00802080L 
};

const unsigned long EncryptDES::SP5[64] = 
{
	0x00000100L, 0x02080100L, 0x02080000L, 0x42000100L,
	0x00080000L, 0x00000100L, 0x40000000L, 0x02080000L,
	0x40080100L, 0x00080000L, 0x02000100L, 0x40080100L,
	0x42000100L, 0x42080000L, 0x00080100L, 0x40000000L,
	0x02000000L, 0x40080000L, 0x40080000L, 0x00000000L,
	0x40000100L, 0x42080100L, 0x42080100L, 0x02000100L,
	0x42080000L, 0x40000100L, 0x00000000L, 0x42000000L,
	0x02080100L, 0x02000000L, 0x42000000L, 0x00080100L,
	0x00080000L, 0x42000100L, 0x00000100L, 0x02000000L,
	0x40000000L, 0x02080000L, 0x42000100L, 0x40080100L,
	0x02000100L, 0x40000000L, 0x42080000L, 0x02080100L,
	0x40080100L, 0x00000100L, 0x02000000L, 0x42080000L,
	0x42080100L, 0x00080100L, 0x42000000L, 0x42080100L,
	0x02080000L, 0x00000000L, 0x40080000L, 0x42000000L,
	0x00080100L, 0x02000100L, 0x40000100L, 0x00080000L,
	0x00000000L, 0x40080000L, 0x02080100L, 0x40000100L 
};

const unsigned long EncryptDES::SP6[64] = 
{
	0x20000010L, 0x20400000L, 0x00004000L, 0x20404010L,
	0x20400000L, 0x00000010L, 0x20404010L, 0x00400000L,
	0x20004000L, 0x00404010L, 0x00400000L, 0x20000010L,
	0x00400010L, 0x20004000L, 0x20000000L, 0x00004010L,
	0x00000000L, 0x00400010L, 0x20004010L, 0x00004000L,
	0x00404000L, 0x20004010L, 0x00000010L, 0x20400010L,
	0x20400010L, 0x00000000L, 0x00404010L, 0x20404000L,
	0x00004010L, 0x00404000L, 0x20404000L, 0x20000000L,
	0x20004000L, 0x00000010L, 0x20400010L, 0x00404000L,
	0x20404010L, 0x00400000L, 0x00004010L, 0x20000010L,
	0x00400000L, 0x20004000L, 0x20000000L, 0x00004010L,
	0x20000010L, 0x20404010L, 0x00404000L, 0x20400000L,
	0x00404010L, 0x20404000L, 0x00000000L, 0x20400010L,
	0x00000010L, 0x00004000L, 0x20400000L, 0x00404010L,
	0x00004000L, 0x00400010L, 0x20004010L, 0x00000000L,
	0x20404000L, 0x20000000L, 0x00400010L, 0x20004010L 
};

const unsigned long EncryptDES::SP7[64] = 
{
	0x00200000L, 0x04200002L, 0x04000802L, 0x00000000L,
	0x00000800L, 0x04000802L, 0x00200802L, 0x04200800L,
	0x04200802L, 0x00200000L, 0x00000000L, 0x04000002L,
	0x00000002L, 0x04000000L, 0x04200002L, 0x00000802L,
	0x04000800L, 0x00200802L, 0x00200002L, 0x04000800L,
	0x04000002L, 0x04200000L, 0x04200800L, 0x00200002L,
	0x04200000L, 0x00000800L, 0x00000802L, 0x04200802L,
	0x00200800L, 0x00000002L, 0x04000000L, 0x00200800L,
	0x04000000L, 0x00200800L, 0x00200000L, 0x04000802L,
	0x04000802L, 0x04200002L, 0x04200002L, 0x00000002L,
	0x00200002L, 0x04000000L, 0x04000800L, 0x00200000L,
	0x04200800L, 0x00000802L, 0x00200802L, 0x04200800L,
	0x00000802L, 0x04000002L, 0x04200802L, 0x04200000L,
	0x00200800L, 0x00000000L, 0x00000002L, 0x04200802L,
	0x00000000L, 0x00200802L, 0x04200000L, 0x00000800L,
	0x04000002L, 0x04000800L, 0x00000800L, 0x00200002L 
};

const unsigned long EncryptDES::SP8[64] = 
{
	0x10001040L, 0x00001000L, 0x00040000L, 0x10041040L,
	0x10000000L, 0x10001040L, 0x00000040L, 0x10000000L,
	0x00040040L, 0x10040000L, 0x10041040L, 0x00041000L,
	0x10041000L, 0x00041040L, 0x00001000L, 0x00000040L,
	0x10040000L, 0x10000040L, 0x10001000L, 0x00001040L,
	0x00041000L, 0x00040040L, 0x10040040L, 0x10041000L,
	0x00001040L, 0x00000000L, 0x00000000L, 0x10040040L,
	0x10000040L, 0x10001000L, 0x00041040L, 0x00040000L,
	0x00041040L, 0x00040000L, 0x10041000L, 0x00001000L,
	0x00000040L, 0x10040040L, 0x00001000L, 0x00041040L,
	0x10001000L, 0x00000040L, 0x10000040L, 0x10040000L,
	0x10040040L, 0x10000000L, 0x00040000L, 0x10001040L,
	0x00000000L, 0x10041040L, 0x00040040L, 0x10000040L,
	0x10040000L, 0x10001000L, 0x10001040L, 0x00000000L,
	0x10041040L, 0x00041000L, 0x00041000L, 0x00001040L,
	0x00001040L, 0x00040040L, 0x10000000L, 0x10041000L 
};

#if TEST
void usage()
{
	printf(
	"Usage: encryptdes key string\n"
	"Where key is an ascii key of 8 characters or less, \n"
	"and string is a text string to be encrypted.");
}

void
main(int argc, char* argv[])
{
	unsigned char buf2[1024];

	EncryptDES des1;
	EncryptDES des2;

	if (argc < 3 || argv[1][1] == '?')
	{
		usage();
		return;
	}

	char* key = argv[1];
	char* encryptString = argv[2];

	printf("String to be encrypted: %s, length = %d\n", encryptString, strlen(encryptString));
	printf("Encryption key: %s\n", key);
	
	int encryptedLength = des1.encrypt(key, (unsigned char*)encryptString, strlen(encryptString));
	CString outputStr = des1.exportHex();

	printf("Encrypted string: %s, length = %d\n", (const char*)outputStr, encryptedLength);

	des2.importHex(outputStr);
	int decryptedLength = des2.decrypt(key, buf2, sizeof buf2);
	if (decryptedLength == -1)
	{
		printf("An error occurred during decryption\n");
		return;
	}
 	printf("Decrypted string: %s, length = %d\n", (char*)buf2, decryptedLength);

	// example from header
	CString persist;

	{
		EncryptDES des;
		CString s("this is a test"); 
		printf("Original string: %s\n", (const char*)s);

		CString key("mykey");
		des.encrypt(key, (const LPBYTE)(const char*)s, s.GetLength());
		CString encryptedData = des.exportHex();

		// ... store the encrypted data somewhere
		persist = encryptedData;
	}
	printf("Persistent storage contains: %s\n", (const char*)persist);
	
	//	Later to unencrypt:
	// .. retrieve the encrypted data
	{
		CString encryptedData = persist;
		EncryptDES des;
		des.importHex(encryptedData);
		CString s;
		CString key("mykey");
		int decryptedLength = 
			des.decrypt(key, (LPBYTE)s.GetBuffer(des.getBinaryLength()),
						des.getBinaryLength());
		s.ReleaseBuffer(decryptedLength);
		// .. s contains unencrypted string
		printf("Result string: %s\n", (const char*)s);
	}

	CString s("Microsoft loves Java, Microsoft loves Java not,...");
	CString encrypted = EncryptDES::EncryptCString("pablo", s);
	CString decrypted = EncryptDES::DecryptCString("pablo", encrypted);
	printf("src = %s\n, encrypted = %s\n, decrypted = %s\n", 
			s, encrypted, decrypted);
}

#endif // TEST
