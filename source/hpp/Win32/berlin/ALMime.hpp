// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALMime.pas' rev: 31.00 (Windows)

#ifndef AlmimeHPP
#define AlmimeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <ALStringList.hpp>

//-- user supplied -----------------------------------------------------------

namespace Almime
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::Byte> TALDynByteArray;

typedef int ALSizeInt;

typedef unsigned TALAddr32;

typedef __int64 TALAddr64;

typedef unsigned TALAddr;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 cALMIME_ENCODED_LINE_BREAK = System::Int8(0x4c);
static const System::Int8 cALMIME_DECODED_LINE_BREAK = System::Int8(0x39);
static const System::Word cALMIME_BUFFER_SIZE = System::Word(0xab0);
extern DELPHI_PACKAGE Alstringlist::TALStrings* vAlMimeContentTypeByExtList;
extern DELPHI_PACKAGE Alstringlist::TALStrings* vAlExtbyMimeContentTypeList;
extern DELPHI_PACKAGE void __fastcall ALMimeEncode(const TALDynByteArray InputBuffer, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeNoCRLF(const TALDynByteArray InputBuffer, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeFullLines(const TALDynByteArray InputBuffer, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecode(const TALDynByteArray InputBuffer, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecodePartial(const TALDynByteArray InputBuffer, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer, unsigned &ByteBuffer, unsigned &ByteBufferSpace)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecodePartialEnd(/* out */ TALDynByteArray &OutputBuffer, const unsigned ByteBuffer, const unsigned ByteBufferSpace)/* overload */;
extern DELPHI_PACKAGE System::AnsiString __fastcall ALMimeEncodeString(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALMimeEncodeStringNoCRLF(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALMimeDecodeString(const System::AnsiString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALMimeEncodeStringU(const System::UnicodeString S, System::Sysutils::TEncoding* const AEncoding = (System::Sysutils::TEncoding*)(0x0));
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALMimeEncodeStringNoCRLFU(const System::UnicodeString S, System::Sysutils::TEncoding* const AEncoding = (System::Sysutils::TEncoding*)(0x0));
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALMimeDecodeStringU(const System::UnicodeString S, System::Sysutils::TEncoding* const AEncoding = (System::Sysutils::TEncoding*)(0x0));
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALMimeEncodeBytesU(const System::DynamicArray<System::Byte> Bytes);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALMimeEncodeBytesNoCRLFU(const System::DynamicArray<System::Byte> Bytes);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall ALMimeDecodeBytesU(const System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall ALMimeEncodedSize(const int InputSize);
extern DELPHI_PACKAGE int __fastcall ALMimeEncodedSizeNoCRLF(const int InputSize);
extern DELPHI_PACKAGE int __fastcall ALMimeDecodedSize(const int InputSize);
extern DELPHI_PACKAGE void __fastcall ALMimeEncode(const TALDynByteArray InputBuffer, int InputOffset, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer, int OutputOffset = 0x0)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALMimeEncode(const void *InputBuffer, const int InputByteCount, /* out */ void *OutputBuffer)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeFullLines(const TALDynByteArray InputBuffer, int InputOffset, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer, int OutputOffset = 0x0)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeFullLines(const void *InputBuffer, const int InputByteCount, /* out */ void *OutputBuffer)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeNoCRLF(const TALDynByteArray InputBuffer, int InputOffset, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer, int OutputOffset = 0x0)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeNoCRLF(const void *InputBuffer, const int InputByteCount, /* out */ void *OutputBuffer)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecode(const TALDynByteArray InputBuffer, int InputOffset, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer, int OutputOffset = 0x0)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecode(const void *InputBuffer, const int InputByteCount, /* out */ void *OutputBuffer)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecodePartial(const TALDynByteArray InputBuffer, int InputOffset, const int InputByteCount, /* out */ TALDynByteArray &OutputBuffer, int OutputOffset, unsigned &ByteBuffer, unsigned &ByteBufferSpace)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecodePartial(const void *InputBuffer, const int InputByteCount, /* out */ void *OutputBuffer, unsigned &ByteBuffer, unsigned &ByteBufferSpace)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecodePartialEnd(/* out */ TALDynByteArray &OutputBuffer, int OutputOffset, const unsigned ByteBuffer, const unsigned ByteBufferSpace)/* overload */;
extern DELPHI_PACKAGE int __fastcall ALMimeDecodePartialEnd(/* out */ void *OutputBuffer, const unsigned ByteBuffer, const unsigned ByteBufferSpace)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeFile(const System::Sysutils::TFileName InputFileName, const System::Sysutils::TFileName OutputFileName);
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeFileNoCRLF(const System::Sysutils::TFileName InputFileName, const System::Sysutils::TFileName OutputFileName);
extern DELPHI_PACKAGE void __fastcall ALMimeDecodeFile(const System::Sysutils::TFileName InputFileName, const System::Sysutils::TFileName OutputFileName);
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeStream(System::Classes::TStream* const InputStream, System::Classes::TStream* const OutputStream);
extern DELPHI_PACKAGE void __fastcall ALMimeEncodeStreamNoCRLF(System::Classes::TStream* const InputStream, System::Classes::TStream* const OutputStream);
extern DELPHI_PACKAGE void __fastcall ALMimeDecodeStream(System::Classes::TStream* const InputStream, System::Classes::TStream* const OutputStream);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGetDefaultFileExtFromMimeContentType(System::AnsiString aContentType);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGetDefaultMIMEContentTypeFromExt(const System::AnsiString aExt);
}	/* namespace Almime */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALMIME)
using namespace Almime;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlmimeHPP
