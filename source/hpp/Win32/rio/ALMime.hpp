// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALMime.pas' rev: 33.00 (Windows)

#ifndef AlmimeHPP
#define AlmimeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <ALStringList.hpp>

//-- user supplied -----------------------------------------------------------

namespace Almime
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Alstringlist::TALStrings* AlMimeContentTypeByExtList;
extern DELPHI_PACKAGE Alstringlist::TALStrings* AlExtbyMimeContentTypeList;
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
