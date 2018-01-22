// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALCipher.pas' rev: 32.00 (Windows)

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
#include <ALString.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alcipher
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EALCipherException;
struct TALCipherBFContext;
struct TALCipherRDLVector;
struct TALCipherRDLContext;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EALCipherException : public Alstring::EALException
{
	typedef Alstring::EALException inherited;
	
public:
	/* EALException.Create */ inline __fastcall EALCipherException(const System::AnsiString Msg) : Alstring::EALException(Msg) { }
	/* EALException.CreateFmt */ inline __fastcall EALCipherException(const System::AnsiString Msg, const System::TVarRec *Args, const int Args_High) : Alstring::EALException(Msg, Args, Args_High) { }
	
public:
	/* Exception.CreateRes */ inline __fastcall EALCipherException(NativeUInt Ident)/* overload */ : Alstring::EALException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EALCipherException(System::PResStringRec ResStringRec)/* overload */ : Alstring::EALException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EALCipherException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Alstring::EALException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EALCipherException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Alstring::EALException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EALCipherException(const System::UnicodeString Msg, int AHelpContext) : Alstring::EALException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EALCipherException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Alstring::EALException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALCipherException(NativeUInt Ident, int AHelpContext)/* overload */ : Alstring::EALException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EALCipherException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Alstring::EALException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALCipherException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Alstring::EALException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EALCipherException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Alstring::EALException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EALCipherException(void) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<System::Byte, 16> TALCipherKey128;

typedef TALCipherKey128 *PALCipherKey128;

typedef System::StaticArray<int, 2> TALCipherBFBlock;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TALCipherBFContext
{
public:
	System::StaticArray<int, 18> PBox;
	System::StaticArray<System::StaticArray<int, 256>, 4> SBox;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TALCipherRDLVector
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
#pragma pack(pop)


typedef System::StaticArray<System::Byte, 16> TALCipherRDLBlock;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TALCipherRDLContext
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
			System::StaticArray<TALCipherRDLVector, 57> W;
		};
		
	};
};
#pragma pack(pop)


typedef TALCipherKey128 TALCipherMD5Digest;

typedef System::StaticArray<System::Byte, 20> TALCipherSHA1Digest;

typedef unsigned __fastcall (*TALStringHashCrc32)(const System::AnsiString str);

typedef NativeUInt HCRYPTPROV;

typedef NativeUInt *PHCRYPTPROV;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 cALCipherBFRounds = System::Int8(0x10);
static const System::Int8 cALCipherMaxRDLRounds = System::Int8(0xe);
extern DELPHI_PACKAGE TALStringHashCrc32 ALStringHashCrc32;
static const unsigned CRYPT_VERIFYCONTEXT = unsigned(0xf0000000);
static const System::Int8 CRYPT_NEWKEYSET = System::Int8(0x8);
static const System::Int8 PROV_RSA_FULL = System::Int8(0x1);
#define MS_ENHANCED_PROV_A "Microsoft Enhanced Cryptographic Provider v1.0"
#define MS_ENHANCED_PROV_W L"Microsoft Enhanced Cryptographic Provider v1.0"
extern DELPHI_PACKAGE void __fastcall ALStringHashMD5(TALCipherKey128 &Digest, const System::AnsiString Str)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALStringHashMD5(const System::AnsiString Str, const bool HexEncode = true)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashMD5U(TALCipherKey128 &Digest, const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALStringHashMD5U(const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashSHA1(TALCipherSHA1Digest &Digest, const System::AnsiString Str)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALStringHashSHA1(const System::AnsiString Str, const bool HexEncode = true)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashSHA1U(TALCipherSHA1Digest &Digest, const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALStringHashSHA1U(const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashSHA2(System::DynamicArray<System::Byte> &Digest, const System::AnsiString Str, const System::Hash::THashSHA2::TSHA2Version AHashVersion = (System::Hash::THashSHA2::TSHA2Version)(0x1))/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALStringHashSHA2(const System::AnsiString Str, const System::Hash::THashSHA2::TSHA2Version AHashVersion = (System::Hash::THashSHA2::TSHA2Version)(0x1), const bool HexEncode = true)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALStringHashSHA2U(System::DynamicArray<System::Byte> &Digest, const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding, const System::Hash::THashSHA2::TSHA2Version AHashVersion = (System::Hash::THashSHA2::TSHA2Version)(0x1))/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALStringHashSHA2U(const System::UnicodeString Str, System::Sysutils::TEncoding* const encoding, const System::Hash::THashSHA2::TSHA2Version AHashVersion = (System::Hash::THashSHA2::TSHA2Version)(0x1))/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALBCryptHashPassword(const System::AnsiString password, int cost);
extern DELPHI_PACKAGE bool __fastcall ALBCryptCheckPassword(const System::AnsiString password, const System::AnsiString expectedHashString, /* out */ bool &PasswordRehashNeeded);
extern DELPHI_PACKAGE bool __fastcall ALBCryptPasswordRehashNeeded(const System::AnsiString HashString);
extern DELPHI_PACKAGE bool __fastcall ALBCryptSelfTest(void);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALCalcHMACSHA1(const System::AnsiString Str, const System::AnsiString Key);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALCalcHMACMD5(const System::AnsiString Str, const System::AnsiString Key);
extern DELPHI_PACKAGE void __fastcall ALCipherEncryptRDL(const TALCipherRDLContext &Context, TALCipherRDLBlock &Block);
extern DELPHI_PACKAGE void __fastcall ALCipherEncryptRDLCBC(const TALCipherRDLContext &Context, const TALCipherRDLBlock &Prev, TALCipherRDLBlock &Block);
extern DELPHI_PACKAGE void __fastcall ALCipherInitEncryptRDL(const void *Key, const int KeySize, TALCipherRDLContext &Context, const bool Encrypt);
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStream(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStreamCBC(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptString(const System::AnsiString InString, System::AnsiString &OutString, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringCBC(const System::AnsiString InString, System::AnsiString &OutString, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRDLEncryptString(const System::AnsiString InString, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRDLEncryptStringCBC(const System::AnsiString InString, const void *Key, const int KeySize, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptString(const System::AnsiString InString, System::AnsiString &OutString, const System::AnsiString Key, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringCBC(const System::AnsiString InString, System::AnsiString &OutString, const System::AnsiString Key, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRDLEncryptString(const System::AnsiString InString, const System::AnsiString Key, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRDLEncryptStringCBC(const System::AnsiString InString, const System::AnsiString Key, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStream(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const System::AnsiString Key, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStreamCBC(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const System::AnsiString Key, const bool Encrypt)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringU(const System::UnicodeString InString, System::UnicodeString &OutString, const void *Key, const int KeySize, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringCBCU(const System::UnicodeString InString, System::UnicodeString &OutString, const void *Key, const int KeySize, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRDLEncryptStringU(const System::UnicodeString InString, const void *Key, const int KeySize, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRDLEncryptStringCBCU(const System::UnicodeString InString, const void *Key, const int KeySize, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringU(const System::UnicodeString InString, System::UnicodeString &OutString, const System::UnicodeString Key, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStringCBCU(const System::UnicodeString InString, System::UnicodeString &OutString, const System::UnicodeString Key, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRDLEncryptStringU(const System::UnicodeString InString, const System::UnicodeString Key, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALRDLEncryptStringCBCU(const System::UnicodeString InString, const System::UnicodeString Key, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStreamU(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const System::UnicodeString Key, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALRDLEncryptStreamCBCU(System::Classes::TStream* const InStream, System::Classes::TStream* const OutStream, const System::UnicodeString Key, const bool Encrypt, System::Sysutils::TEncoding* const encoding)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALCipherInitEncryptBF(const TALCipherKey128 &Key, TALCipherBFContext &Context);
extern DELPHI_PACKAGE void __fastcall ALCipherEncryptBF(const TALCipherBFContext &Context, TALCipherBFBlock &Block, bool Encrypt);
extern DELPHI_PACKAGE void __fastcall ALCipherEncryptBFCBC(const TALCipherBFContext &Context, const TALCipherBFBlock &Prev, TALCipherBFBlock &Block, bool Encrypt);
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
extern "C" System::LongBool __stdcall CryptAcquireContextA(PHCRYPTPROV phProv, char * pszContainer, char * pszProvider, unsigned dwProvType, unsigned dwFlags);
extern "C" System::LongBool __stdcall CryptAcquireContextW(PHCRYPTPROV phProv, System::WideChar * pszContainer, System::WideChar * pszProvider, unsigned dwProvType, unsigned dwFlags);
extern "C" System::LongBool __stdcall CryptReleaseContext(NativeUInt hProv, unsigned dwFlags);
extern "C" System::LongBool __stdcall CryptGenRandom(NativeUInt hProv, unsigned dwLen, System::PByte pbBuffer);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALRandomBytes(const unsigned Len);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALRandomByteStr(const unsigned Len);
extern DELPHI_PACKAGE unsigned __fastcall ALRandom32(const unsigned ARange);
extern DELPHI_PACKAGE unsigned __int64 __fastcall ALRandom64(const unsigned __int64 ARange);
extern DELPHI_PACKAGE __int64 __fastcall ALFnv1aInt64(const System::AnsiString str);
extern DELPHI_PACKAGE __int64 __fastcall ALFnv1aInt64U(const System::UnicodeString str, System::Sysutils::TEncoding* const encoding);
extern DELPHI_PACKAGE __int64 __fastcall ALFnv1aInt32(const System::AnsiString str);
extern DELPHI_PACKAGE __int64 __fastcall ALFnv1aInt32U(const System::UnicodeString str, System::Sysutils::TEncoding* const encoding);
}	/* namespace Alcipher */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALCIPHER)
using namespace Alcipher;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlcipherHPP
