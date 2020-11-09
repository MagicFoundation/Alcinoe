// CodeGear C++Builder
// Copyright (c) 1995, 2018 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALCipher.pas' rev: 33.00 (Windows)

#ifndef AlcipherHPP
#define AlcipherHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <System.Hash.hpp>
#include <System.Classes.hpp>
#include <ALCommon.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alcipher
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EALCipherException;
struct TALBFContext;
struct TALRDLVector;
struct TALRDLContext;
struct _PUBLICKEYSTRUC;
struct _RSAPUBKEY;
struct _CRYPT_DECODE_PARA;
struct PRIVATEKEYBLOB;
struct BCRYPT_RSAKEY_BLOB;
struct BCRYPT_PKCS1_PADDING_INFO;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EALCipherException : public Alcommon::EALException
{
	typedef Alcommon::EALException inherited;
	
public:
	/* EALException.Create */ inline __fastcall EALCipherException(const System::AnsiString Msg) : Alcommon::EALException(Msg) { }
	/* EALException.CreateFmt */ inline __fastcall EALCipherException(const System::AnsiString Msg, const System::TVarRec *Args, const int Args_High) : Alcommon::EALException(Msg, Args, Args_High) { }
	
public:
	/* Exception.CreateRes */ inline __fastcall EALCipherException(NativeUInt Ident)/* overload */ : Alcommon::EALException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EALCipherException(System::PResStringRec ResStringRec)/* overload */ : Alcommon::EALException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EALCipherException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Alcommon::EALException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EALCipherException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Alcommon::EALException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EALCipherException(const System::UnicodeString Msg, int AHelpContext) : Alcommon::EALException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EALCipherException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Alcommon::EALException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALCipherException(NativeUInt Ident, int AHelpContext)/* overload */ : Alcommon::EALException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALCipherException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Alcommon::EALException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALCipherException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Alcommon::EALException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALCipherException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Alcommon::EALException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EALCipherException() { }
	
};

#pragma pack(pop)

typedef System::StaticArray<System::Byte, 16> TALCipherKey128;

typedef TALCipherKey128 *PALCipherKey128;

enum DECLSPEC_DENUM TALkeyDerivationFunction : unsigned char { MD5, SHA2 };

typedef System::StaticArray<int, 2> TALBFBlock;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TALBFContext
{
public:
	System::StaticArray<int, 18> PBox;
	System::StaticArray<System::StaticArray<int, 256>, 4> SBox;
};
#pragma pack(pop)


struct DECLSPEC_DRECORD TALRDLVector
{
	
public:
	union
	{
		struct 
		{
			System::StaticArray<System::Byte, 4> bt;
		};
		struct 
		{
			unsigned dw;
		};
		
	};
};


typedef System::StaticArray<System::Byte, 16> TALRDLBlock;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TALRDLContext
{
public:
	bool Encrypt;
	System::StaticArray<System::Byte, 3> Dummy;
	unsigned Rounds;
	
public:
	union
	{
		struct 
		{
			System::StaticArray<System::StaticArray<System::Byte, 16>, 15> Rk;
		};
		struct 
		{
			System::StaticArray<TALRDLVector, 61> W;
		};
		
	};
};
#pragma pack(pop)


typedef TALCipherKey128 TALMD5Digest;

typedef System::StaticArray<System::Byte, 20> TALSHA1Digest;

typedef unsigned __fastcall (*TALStringHashCrc32)(const System::AnsiString str);

typedef unsigned __fastcall (*TALHashCrc32)(char * buf, unsigned len);

typedef __int64 __fastcall (*TALStringHashCrc64)(const System::AnsiString str);

typedef __int64 __fastcall (*TALHashCrc64)(char * buf, unsigned len);

typedef unsigned ALG_ID;

typedef NativeUInt HCRYPTPROV;

typedef NativeUInt *PHCRYPTPROV;

typedef NativeUInt HCRYPTKEY;

typedef NativeUInt *PHCRYPTKEY;

typedef NativeUInt HCRYPTHASH;

typedef NativeUInt *PHCRYPTHASH;

struct DECLSPEC_DRECORD _PUBLICKEYSTRUC
{
public:
	System::Byte bType;
	System::Byte bVersion;
	System::Word reserved;
	unsigned aiKeyAlg;
};


typedef _PUBLICKEYSTRUC BLOBHEADER;

typedef _PUBLICKEYSTRUC PUBLICKEYSTRUC;

struct DECLSPEC_DRECORD _RSAPUBKEY
{
public:
	unsigned magic;
	unsigned bitlen;
	unsigned pubexp;
};


typedef _RSAPUBKEY RSAPUBKEY;

typedef void * __stdcall (*PFN_CRYPT_ALLOC)(NativeUInt cbSize);

typedef void __stdcall (*PFN_CRYPT_FREE)(void * pv);

typedef _CRYPT_DECODE_PARA *PCRYPT_DECODE_PARA;

struct DECLSPEC_DRECORD _CRYPT_DECODE_PARA
{
public:
	unsigned cbSize;
	PFN_CRYPT_ALLOC pfnAlloc;
	PFN_CRYPT_FREE pfnFree;
};


typedef _CRYPT_DECODE_PARA CRYPT_DECODE_PARA;

typedef int NTSTATUS;

typedef void * BCRYPT_HANDLE;

typedef void * BCRYPT_ALG_HANDLE;

typedef void * BCRYPT_KEY_HANDLE;

typedef void * BCRYPT_HASH_HANDLE;

typedef void * BCRYPT_SECRET_HANDLE;

struct DECLSPEC_DRECORD PRIVATEKEYBLOB
{
public:
	_PUBLICKEYSTRUC PublicKeyStruc;
	_RSAPUBKEY RSAPubKey;
	System::DynamicArray<System::Byte> Modulus;
	System::DynamicArray<System::Byte> Prime1;
	System::DynamicArray<System::Byte> Prime2;
	System::DynamicArray<System::Byte> Exponent1;
	System::DynamicArray<System::Byte> Exponent2;
	System::DynamicArray<System::Byte> Coefficient;
	System::DynamicArray<System::Byte> PrivateExponent;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD BCRYPT_RSAKEY_BLOB
{
public:
	unsigned Magic;
	unsigned BitLength;
	unsigned CbPublicExp;
	unsigned CbModulus;
	unsigned CbPrime1;
	unsigned CbPrime2;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD BCRYPT_PKCS1_PADDING_INFO
{
public:
	System::WideChar *pszAlgId;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 cALBFRounds = System::Int8(0x10);
static const System::Int8 cALMaxRDLRounds = System::Int8(0xe);
extern DELPHI_PACKAGE TALStringHashCrc32 ALStringHashCrc32;
extern DELPHI_PACKAGE TALHashCrc32 ALHashCrc32;
extern DELPHI_PACKAGE TALStringHashCrc64 ALStringHashCrc64;
extern DELPHI_PACKAGE TALHashCrc64 ALHashCrc64;
#define crypt32 L"crypt32.dll"
#define bcrypt L"bcrypt.dll"
static const unsigned CRYPT_VERIFYCONTEXT = unsigned(0xf0000000);
static const System::Int8 CRYPT_NEWKEYSET = System::Int8(0x8);
static const System::Int8 CRYPT_SILENT = System::Int8(0x40);
static const System::Int8 PROV_RSA_FULL = System::Int8(0x1);
static const System::Int8 PROV_RSA_AES = System::Int8(0x18);
#define MS_ENHANCED_PROV_A "Microsoft Enhanced Cryptographic Provider v1.0"
#define MS_ENHANCED_PROV_W L"Microsoft Enhanced Cryptographic Provider v1.0"
static const System::Int8 CRYPT_STRING_BASE64HEADER = System::Int8(0x0);
static const System::Int8 CRYPT_STRING_BASE64 = System::Int8(0x1);
static const System::Int8 PUBLICKEYBLOB = System::Int8(0x6);
static const System::Int8 CUR_BLOB_VERSION = System::Int8(0x2);
static const System::Word CALG_RSA_KEYX = System::Word(0xa400);
static const System::Word CALG_SHA_256 = System::Word(0x800c);
static const int RSA1 = int(0x31415352);
static const System::Int8 X509_ASN_ENCODING = System::Int8(0x1);
static const int PKCS_7_ASN_ENCODING = int(0x10000);
#define PKCS_RSA_PRIVATE_KEY (char *)(0x2b)
#define PKCS_PRIVATE_KEY_INFO (char *)(0x2c)
#define PKCS_ENCRYPTED_PRIVATE_KEY_INFO (char *)(0x2d)
#define BCRYPT_AES_ALGORITHM L"AES"
#define BCRYPT_RNG_ALGORITHM L"RNG"
#define BCRYPT_RSA_ALGORITHM L"RSA"
#define BCRYPT_SHA256_ALGORITHM L"SHA256"
static const System::Int8 BCRYPT_PAD_NONE = System::Int8(0x1);
static const System::Int8 BCRYPT_PAD_PKCS1 = System::Int8(0x2);
static const System::Int8 BCRYPT_PAD_OAEP = System::Int8(0x4);
static const System::Int8 BCRYPT_PAD_PSS = System::Int8(0x8);
#define BCRYPT_RSAPUBLIC_BLOB L"RSAPUBLICBLOB"
#define BCRYPT_RSAPRIVATE_BLOB L"RSAPRIVATEBLOB"
#define BCRYPT_RSAFULLPRIVATE_BLOB L"RSAFULLPRIVATEBLOB"
static const int BCRYPT_RSAPUBLIC_MAGIC = int(0x31415352);
static const int BCRYPT_RSAPRIVATE_MAGIC = int(0x32415352);
static const int BCRYPT_RSAFULLPRIVATE_MAGIC = int(0x33415352);
static const System::Int8 STATUS_SUCCESS = System::Int8(0x0);
extern "C" bool __stdcall CryptStringToBinaryA(char * pszString, unsigned cchString, unsigned dwFlags, System::PByte pbBinary, unsigned* pcbBinary, unsigned* pdwSkip, unsigned* pdwFlags);
extern "C" System::LongBool __stdcall CryptDecodeObjectEx(unsigned dwCertEncodingType, char * lpszStructType, const System::PByte pbEncoded, unsigned cbEncoded, unsigned dwFlags, PCRYPT_DECODE_PARA pDecodePara, void * pvStructInfo, unsigned* pcbStructInfo);
extern "C" System::LongBool __stdcall CryptAcquireContextA(PHCRYPTPROV phProv, char * szContainer, char * szProvider, unsigned dwProvType, unsigned dwFlags);
extern "C" System::LongBool __stdcall CryptReleaseContext(NativeUInt hProv, unsigned dwFlags);
extern "C" System::LongBool __stdcall CryptGenRandom(NativeUInt hProv, unsigned dwLen, System::PByte pbBuffer);
extern "C" System::LongBool __stdcall CryptImportKey(NativeUInt hProv, const System::PByte pbData, unsigned dwDataLen, NativeUInt hPubKey, unsigned dwFlags, PHCRYPTKEY phKey);
extern "C" System::LongBool __stdcall CryptDestroyKey(NativeUInt hKey);
extern "C" System::LongBool __stdcall CryptVerifySignatureA(NativeUInt hHash, const System::PByte pbSignature, unsigned dwSigLen, NativeUInt hPubKey, char * szDescription, unsigned dwFlags);
extern "C" System::LongBool __stdcall CryptCreateHash(NativeUInt hProv, unsigned Algid, NativeUInt hKey, unsigned dwFlags, PHCRYPTHASH phHash);
extern "C" System::LongBool __stdcall CryptDestroyHash(NativeUInt hHash);
extern "C" System::LongBool __stdcall CryptHashData(NativeUInt hHash, const System::PByte pbData, unsigned dwDataLen, unsigned dwFlags);
extern "C" System::LongBool __stdcall CryptSignHashA(NativeUInt hHash, unsigned dwKeySpec, char * szDescription, unsigned dwFlags, System::PByte pbSignature, unsigned* pdwSigLen);
extern "C" int __stdcall BCryptOpenAlgorithmProvider(/* out */ void * &phAlgorithm, System::WideChar * pszAlgId, System::WideChar * pszImplementation, unsigned dwFlags);
extern "C" int __stdcall BCryptCloseAlgorithmProvider(void * hAlgorithm, unsigned dwFlags);
extern "C" int __stdcall BCryptImportKeyPair(void * hAlgorithm, void * hImportKey, System::WideChar * pszBlobType, /* out */ void * &phKey, PUCHAR pbInput, unsigned cbInput, unsigned dwFlags);
extern "C" int __stdcall BCryptDestroyKey(void * hKey);
extern "C" int __stdcall BCryptSignHash(void * hKey, void * pPaddingInfo, PUCHAR pbInput, unsigned cbInput, PUCHAR pbOutput, unsigned cbOutput, unsigned &pcbResult, unsigned dwFlags);
extern DELPHI_PACKAGE void __fastcall ALStringHashMD5(TALCipherKey128 &Digest, const System::AnsiString Str)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALStringHashMD5(const System::AnsiString Str, const bool HexEncode = true)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashMD5U(TALCipherKey128 &Digest, const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALStringHashMD5U(const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashSHA1(TALSHA1Digest &Digest, const System::AnsiString Str)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALStringHashSHA1(const System::AnsiString Str, const bool HexEncode = true)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashSHA1U(TALSHA1Digest &Digest, const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALStringHashSHA1U(const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashSHA2(System::DynamicArray<System::Byte> &Digest, const System::AnsiString Str, const System::Hash::THashSHA2::TSHA2Version AHashVersion = (System::Hash::THashSHA2::TSHA2Version)(0x1))/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALStringHashSHA2(const System::AnsiString Str, const System::Hash::THashSHA2::TSHA2Version AHashVersion = (System::Hash::THashSHA2::TSHA2Version)(0x1), const bool HexEncode = true)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashSHA2U(System::DynamicArray<System::Byte> &Digest, const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding, const System::Hash::THashSHA2::TSHA2Version AHashVersion = (System::Hash::THashSHA2::TSHA2Version)(0x1))/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALStringHashSHA2U(const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding, const System::Hash::THashSHA2::TSHA2Version AHashVersion = (System::Hash::THashSHA2::TSHA2Version)(0x1))/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBCryptHashPassword(const System::AnsiString password, int cost, const bool MCFEncode = true);
extern DELPHI_PACKAGE bool __fastcall ALBCryptCheckPassword(const System::AnsiString password, const System::AnsiString expectedHashString, /* out */ bool &PasswordRehashNeeded);
extern DELPHI_PACKAGE bool __fastcall ALBCryptPasswordRehashNeeded(const System::AnsiString HashString);
extern DELPHI_PACKAGE bool __fastcall ALBCryptSelfTest(void);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALCalcHMACSHA1(const System::AnsiString Str, const System::AnsiString Key);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALCalcHMACMD5(const System::AnsiString Str, const System::AnsiString Key);
extern DELPHI_PACKAGE void __fastcall ALEncryptRDL(const TALRDLContext &Context, TALRDLBlock &Block);
extern DELPHI_PACKAGE void __fastcall ALEncryptRDLCBC(const TALRDLContext &Context, const TALRDLBlock &Prev, TALRDLBlock &Block);
extern DELPHI_PACKAGE void __fastcall ALInitEncryptRDL(const void *Key, const int KeySize, TALRDLContext &Context, const bool Encrypt);
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStream(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStreamCBC(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptString(const System::AnsiString InString, System::AnsiString &OutString, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringCBC(const System::AnsiString InString, System::AnsiString &OutString, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRDLEncryptString(const System::AnsiString InString, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRDLEncryptStringCBC(const System::AnsiString InString, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptString(const System::AnsiString InString, System::AnsiString &OutString, const System::AnsiString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringCBC(const System::AnsiString InString, System::AnsiString &OutString, const System::AnsiString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRDLEncryptString(const System::AnsiString InString, const System::AnsiString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRDLEncryptStringCBC(const System::AnsiString InString, const System::AnsiString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStream(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const System::AnsiString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStreamCBC(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const System::AnsiString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringU(const System::UnicodeString InString, System::UnicodeString &OutString, const void *Key, const int KeySize, const bool Encrypt, System::Sysutils::TEncoding* const encoding, const bool UseBase64 = false)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringCBCU(const System::UnicodeString InString, System::UnicodeString &OutString, const void *Key, const int KeySize, const bool Encrypt, System::Sysutils::TEncoding* const encoding, const bool UseBase64 = false)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRDLEncryptStringU(const System::UnicodeString InString, const void *Key, const int KeySize, const bool Encrypt, System::Sysutils::TEncoding* const encoding, const bool UseBase64 = false)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRDLEncryptStringCBCU(const System::UnicodeString InString, const void *Key, const int KeySize, const bool Encrypt, System::Sysutils::TEncoding* const encoding, const bool UseBase64 = false)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringU(const System::UnicodeString InString, System::UnicodeString &OutString, const System::UnicodeString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt, System::Sysutils::TEncoding* const encoding, const bool UseBase64 = false)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringCBCU(const System::UnicodeString InString, System::UnicodeString &OutString, const System::UnicodeString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt, System::Sysutils::TEncoding* const encoding, const bool UseBase64 = false)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRDLEncryptStringU(const System::UnicodeString InString, const System::UnicodeString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt, System::Sysutils::TEncoding* const encoding, const bool UseBase64 = false)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRDLEncryptStringCBCU(const System::UnicodeString InString, const System::UnicodeString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt, System::Sysutils::TEncoding* const encoding, const bool UseBase64 = false)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStreamU(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const System::UnicodeString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStreamCBCU(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const System::UnicodeString Key, const TALkeyDerivationFunction KeyDerivationFunction, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALInitEncryptBF(const TALCipherKey128 &Key, TALBFContext &Context);
extern DELPHI_PACKAGE void __fastcall ALEncryptBF(const TALBFContext &Context, TALBFBlock &Block, bool Encrypt);
extern DELPHI_PACKAGE void __fastcall ALEncryptBFCBC(const TALBFContext &Context, const TALBFBlock &Prev, TALBFBlock &Block, bool Encrypt);
extern DELPHI_PACKAGE void __fastcall ALBFEncryptString(const System::AnsiString InString, System::AnsiString &OutString, const TALCipherKey128 &Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBFEncryptStringCBC(const System::AnsiString InString, System::AnsiString &OutString, const TALCipherKey128 &Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBFEncryptString(const System::AnsiString InString, const TALCipherKey128 &Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBFEncryptStringCBC(const System::AnsiString InString, const TALCipherKey128 &Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBFEncryptStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream, const TALCipherKey128 &Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBFEncryptStreamCBC(System::Classes::TStream* InStream, System::Classes::TStream* OutStream, const TALCipherKey128 &Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBFEncryptString(const System::AnsiString InString, System::AnsiString &OutString, const System::AnsiString Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBFEncryptStringCBC(const System::AnsiString InString, System::AnsiString &OutString, const System::AnsiString Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBFEncryptString(const System::AnsiString InString, const System::AnsiString Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBFEncryptStringCBC(const System::AnsiString InString, const System::AnsiString Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBFEncryptStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream, const System::AnsiString Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALBFEncryptStreamCBC(System::Classes::TStream* InStream, System::Classes::TStream* OutStream, const System::AnsiString Key, bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRandomBytes(const void *Dest, const unsigned Len)/* overload */;
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALRandomBytes(const unsigned Len)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRandomByteStr(const unsigned Len);
extern DELPHI_PACKAGE unsigned __fastcall ALRandom32(const unsigned ARange);
extern DELPHI_PACKAGE unsigned __int64 __fastcall ALRandom64(const unsigned __int64 ARange);
extern DELPHI_PACKAGE void __fastcall ALTestCRC32Implementation(const System::AnsiString aSavedResultsFilename = System::AnsiString());
extern DELPHI_PACKAGE __int64 __fastcall ALFnv1aInt64(const System::AnsiString str);
extern DELPHI_PACKAGE __int64 __fastcall ALFnv1aInt64U(const System::UnicodeString str, System::Sysutils::TEncoding* const encoding);
extern DELPHI_PACKAGE int __fastcall ALFnv1aInt32(const System::AnsiString str);
extern DELPHI_PACKAGE int __fastcall ALFnv1aInt32U(const System::UnicodeString str, System::Sysutils::TEncoding* const encoding);
extern DELPHI_PACKAGE bool __fastcall ALVerifyRSA256Signature(const System::AnsiString aData, const System::AnsiString aSignature, const System::AnsiString aBase64PubKeyModulus, const System::AnsiString aBase64PubKeyExponent);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRSA256Sign(const System::AnsiString aData, const System::AnsiString aPemPrivateKey);
}	/* namespace Alcipher */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALCIPHER)
using namespace Alcipher;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlcipherHPP
