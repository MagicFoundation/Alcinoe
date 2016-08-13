// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxCommon.pas' rev: 31.00 (Windows)

#ifndef AlfmxcommonHPP
#define AlfmxcommonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxcommon
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::Uitypes::TFontName __fastcall (*TALCustomConvertFontFamilyProc)(const System::Uitypes::TFontName AFamily);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TALCustomConvertFontFamilyProc ALCustomConvertFontFamilyProc;
extern DELPHI_PACKAGE System::Uitypes::TFontName __fastcall ALConvertFontFamily(const System::Uitypes::TFontName AFamily);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ALTranslate(const System::UnicodeString AText);
extern DELPHI_PACKAGE void __fastcall ALFmxMakeBufBitmaps(Fmx::Controls::TControl* const aControl);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall ALPrepareColor(const System::Uitypes::TAlphaColor SrcColor, const float Opacity);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelRound(const System::Types::TRectF &Rect, const float Scale);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ALAlignDimensionToPixelCeil(const System::Types::TRectF &Rect, const float Scale);
}	/* namespace Alfmxcommon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXCOMMON)
using namespace Alfmxcommon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxcommonHPP
