// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALCommon.pas' rev: 31.00 (Windows)

#ifndef AlcommonHPP
#define AlcommonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alcommon
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TalLogType : unsigned char { VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT };

typedef void __fastcall (__closure *TALCustomDelayedFreeObjectProc)(System::TObject* &aObject);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TALCustomDelayedFreeObjectProc ALCustomDelayedFreeObjectProc;
extern DELPHI_PACKAGE unsigned __int64 ALMAXUInt64;
extern DELPHI_PACKAGE __int64 ALMAXInt64;
extern DELPHI_PACKAGE unsigned ALMAXUInt;
extern DELPHI_PACKAGE int ALMAXInt;
#define ALNullDate  (-5.000000E-01)
extern DELPHI_PACKAGE void __fastcall ALLog(const System::UnicodeString Tag, const System::UnicodeString msg, const TalLogType _type = (TalLogType)(0x2));
extern DELPHI_PACKAGE int __fastcall AlBoolToInt(bool Value);
extern DELPHI_PACKAGE bool __fastcall AlIntToBool(int Value);
extern DELPHI_PACKAGE int __fastcall ALMediumPos(int LTotal, int LBorder, int LObject);
extern DELPHI_PACKAGE System::TDateTime __fastcall AlLocalDateTimeToUTCDateTime(const System::TDateTime aLocalDateTime);
extern DELPHI_PACKAGE System::TDateTime __fastcall AlUTCDateTimeToLocalDateTime(const System::TDateTime aUTCDateTime);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALUTCNow(void);
extern DELPHI_PACKAGE int __fastcall ALInc(int &x, int Count);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALUnixMsToDateTime(const __int64 aValue);
extern DELPHI_PACKAGE __int64 __fastcall ALDateTimeToUnixMs(const System::TDateTime aValue);
extern DELPHI_PACKAGE void __fastcall ALFreeAndNil(void *Obj, const bool adelayed = false)/* overload */;
extern DELPHI_PACKAGE void __fastcall ALFreeAndNil(void *Obj, const bool adelayed, const bool aRefCountWarn)/* overload */;
}	/* namespace Alcommon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALCOMMON)
using namespace Alcommon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlcommonHPP
