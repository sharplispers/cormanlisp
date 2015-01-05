//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		EncryptDES.h
//		Contents:	Class definition for DES encryption.
//		History:	10/21/97  RGC  Created.
//
//
//		-----------------------------------------------------------------
//		Usage Notes:
//		---------------------		
//		This class is designed to allow easy encryption/decryption
//		of a binary block of data using standard DES encryption.
//		The EncryptDES class can be thought of as a buffer
//		which holds encrypted data only. When you create one,
//		it is empty. 
//
//		Use the encrypt() member function to encrypt a block of 
//		data and load that encrypted data into its internal buffer.
//		You need to pass this function an ascii, null-terminated
//		string of 1-8 characters to use as a key. If the string is
//		longer than 8 characters the call will fail.
//		
//		The decrypt() member function is the mirror equivalent to
//		encrypt(), causing the encrypted data in the object's
//		internal buffer to be decrypted into a buffer.
//		As with the encrypt() function, you need to pass this function 
//		an ascii, null-terminated string of 1-8 characters to use as a 
//		key. Of course, this must be the same key that was used to encrypt the
//		data, or the decryption will fail. Note: the function may or may
//		not fail if the key is wrong. However, in either case the data
//		returned will be garbage.
//
//		Obviously, this capability is not too useful alone. The other
//		piece you need are member functions to make the encrypted
//		data persistent. This class provides member functions to
//		export and import its encrypted contents either as binary
//		data or as a hex-encoded ascii string.
//
//		Use exportBinary() and importBinary() to save and restore
//		the contents of a EncryptDES object.
//
//		Alternately, you can use exportHex() and importHex() if
//		ascii storage is more convenient.
//
//		-----------------------------------------------------------------
//		Example 1:
//		---------------------		
//
//		CString persist;
//
//		{
//			EncryptDES des;
//			CString s("this is a test"); 
//			printf("Original string: %s\n", (const char*)s);
//
//			CString key("mykey");
//			des.encrypt(key, (const LPBYTE)(const char*)s, s.GetLength());
//			CString encryptedData = des.exportHex();
//
//			// ... store the encrypted data somewhere
//			persist = encryptedData;
//		}
//		printf("Persistent storage contains: %s\n", (const char*)persist);
//	
//		//	Later to unencrypt:
//		// .. retrieve the encrypted data
//		{
//			CString encryptedData = persist;
//			EncryptDES des;
//			des.importHex(encryptedData);
//			CString s;
//			CString key("mykey");
//			int decryptedLength = 
//				des.decrypt(key, (LPBYTE)s.GetBuffer(des.getBinaryLength()),
//							des.getBinaryLength());
//			s.ReleaseBuffer(decryptedLength);
//			// .. s contains unencrypted string
//			printf("Result string: %s\n", (const char*)s);
//		}
//
//		---------------------		
//		Example 2:
//		---------------------		
//		If you are dealing with only text strings, you can just use the
//		static member functions below, for convenience.
//
//		CString s("Microsoft loves Java, Microsoft loves Java not,...");
//		CString encrypted = EncryptDES::EncryptCString("pablo", s);
//		CString decrypted = EncryptDES::DecryptCString("pablo", encrypted);
//		printf("src = %s\n, encrypted = %s\n, decrypted = %s\n", 
//				s, encrypted, decrypted);
//
//		-----------------------------------------------------------------
//		Implementation Notes:
//		---------------------		
//		The implementation of this class uses a public
//		domain version of the DES encryption standard algorithm,
//		implemented by Richard Outerbridge. This implementation currently
//		uses 56-bit encryption (8 byte key with 1 parity bit
//		in each key byte). The source code can be found in the
//		software cryptographer's bible, Applied Cryptography,
//		by Bruce Schneier.
//
//		I have modified the encryption code to allow reentrancy
//		(and therefore multiple threads may use it). I also rewrote
//		it somewhat from the original K&R C code to fit into a C++ 
//		class, and to optimize it a bit (and eliminate some non-optimizations 
//		that, in my opinion, muddled the code).
//

#ifndef ENCRYPTDES_H
#define ENCRYPTDES_H

//	cbcMode:	If true, Cipher Block Chaining mode
//				is used to improve the security of this algorithm
//				If false, normal DES mode is used.
//
const bool cbcMode = true;

//	The DES algorithm acts on 64-bit data blocks, which we encapsulate
//	with the DESBlock data type.
struct DESBlock
{
	unsigned long data[2];
};

class EncryptDES
{
public:
	EncryptDES();		// create an empty object
	~EncryptDES();

	// encryption/decryption of binary data
	int encrypt(const char* key, const unsigned char* srcbuf, unsigned int srclen);
	int decrypt(const char* key, unsigned char* destbuf, unsigned int destlen);

	int	getBinaryLength();		// length of binary encrypted data
	int	getHexStringLength();	// length of hex-ascii encoded encrypted data

	// import/export binary (for use by persistence mechanism)
	void importBinary(unsigned char* srcbuf, unsigned int srclen);
	void exportBinary(unsigned char* destbuf);

	// import/export hex-ascii encoded (for use by persistence mechanism)
	CString exportHex();
	void importHex(const char* srcbuf);

	// static public functions defined for convenience when dealing with text

	//	If using CStrings, these functions allow easy encryption/decryption.
	static CString EncryptCString(const CString& key, const CString& source);
	static CString DecryptCString(const CString& key, const CString& hex);

private:
	void processKey(const char*, bool decipher);
	void clear();
	void deskey(unsigned char* key, bool decipher);
	void encryptBlock(DESBlock* src);

	// Copy constructor, assignment operator are private to prevent use
	EncryptDES(const EncryptDES&);
	EncryptDES& operator =(const EncryptDES&);

	// data members
	unsigned char* buf;
	unsigned int bufSize;
	unsigned long processedKey[32];

	// static tables used by the algorithm
	static const unsigned char pc1[56];
	static const unsigned char pc2[48];
	static const unsigned short bytebit[8];
	static const unsigned char totrot[16];
	static const unsigned long bigbyte[24];
	static const unsigned long SP1[64];
	static const unsigned long SP2[64];
	static const unsigned long SP3[64];
	static const unsigned long SP4[64];
	static const unsigned long SP5[64];
	static const unsigned long SP6[64];
	static const unsigned long SP7[64];
	static const unsigned long SP8[64];
};


#endif	// ENCRYPTDES_H
