// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALGraphics.pas' rev: 32.00 (Windows)

#ifndef AlgraphicsHPP
#define AlgraphicsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <FMX.Graphics.hpp>

//-- user supplied -----------------------------------------------------------

namespace Algraphics
{
//-- forward type declarations -----------------------------------------------
__interface TALResizeImageGetDestSizeFunct;
typedef System::DelphiInterface<TALResizeImageGetDestSizeFunct> _di_TALResizeImageGetDestSizeFunct;
__interface TALResizeAndBlurImageGetDestSizeFunct;
typedef System::DelphiInterface<TALResizeAndBlurImageGetDestSizeFunct> _di_TALResizeAndBlurImageGetDestSizeFunct;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TalExifOrientationInfo : unsigned char { FLIP_HORIZONTAL, FLIP_VERTICAL, NORMAL, ROTATE_180, ROTATE_270, ROTATE_90, TRANSPOSE, TRANSVERSE, UNDEFINED };

__interface TALResizeImageGetDestSizeFunct  : public System::IInterface 
{
	virtual System::Types::TPointF __fastcall Invoke(const System::Types::TPointF &aOriginalSize) = 0 ;
};

__interface TALResizeAndBlurImageGetDestSizeFunct  : public System::IInterface 
{
	virtual System::Types::TPointF __fastcall Invoke(const System::Types::TPointF &aOriginalSize, float &aRadius) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsRoundRectImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const float XRadius, const float YRadius, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsMaskImageV1(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsMaskImageV1(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsMaskImageV2(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsMaskImageV2(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsMaskImageV3(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsMaskImageV3(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsMaskImageV1(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsMaskImageV1(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsMaskImageV2(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsMaskImageV2(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsMaskImageV3(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsMaskImageV3(System::Classes::TCustomMemoryStream* const aStream, Fmx::Graphics::TBitmap* const aMask, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsMaskImageV1(const System::UnicodeString aResName, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsMaskImageV1(const System::UnicodeString aResName, Fmx::Graphics::TBitmap* const aMask)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsMaskImageV2(const System::UnicodeString aResName, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsMaskImageV2(const System::UnicodeString aResName, Fmx::Graphics::TBitmap* const aMask)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsMaskImageV3(const System::UnicodeString aResName, Fmx::Graphics::TBitmap* const aMask, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsMaskImageV3(const System::UnicodeString aResName, Fmx::Graphics::TBitmap* const aMask)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsRoundRectImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const float XRadius, const float YRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsRoundRectImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const float XRadius, const float YRadius, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsRoundRectImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const float XRadius, const float YRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsRoundRectImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const float XRadius, const float YRadius, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsRoundRectImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const float XRadius, const float YRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsCircleImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsCircleImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsCircleImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsCircleImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsCircleImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropAsCircleImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsCircleImageV1(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsCircleImageV1(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsCircleImageV2(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsCircleImageV2(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsCircleImageV3(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceAsCircleImageV3(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsCircleImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsCircleImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsCircleImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsCircleImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsCircleImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropAsCircleImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, float aBlurRadius, const float aBlurW, const float aBlurH)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeAndBlurImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter, float aRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, float aRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeAndBlurImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter, float aRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, float aRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeAndBlurImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, const System::Types::TPointF &aCropCenter, float aRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALBlurFitIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H, float aRadius)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceImageV1(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceImageV1(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceImageV2(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceImageV2(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceImageV3(const System::UnicodeString aResName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropResourceImageV3(const System::UnicodeString aResName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropFileImageV1(const System::UnicodeString aFileName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropFileImageV1(const System::UnicodeString aFileName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropFileImageV2(const System::UnicodeString aFileName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropFileImageV2(const System::UnicodeString aFileName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropFileImageV3(const System::UnicodeString aFileName, const float W, const float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoAndCropFileImageV3(const System::UnicodeString aFileName, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV1(System::Classes::TCustomMemoryStream* const aStream, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV2(System::Classes::TCustomMemoryStream* const aStream, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALPlaceIntoAndCropImageV3(System::Classes::TCustomMemoryStream* const aStream, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropResourceImageV1(const System::UnicodeString aResName, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropResourceImageV1(const System::UnicodeString aResName, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropResourceImageV2(const System::UnicodeString aResName, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropResourceImageV2(const System::UnicodeString aResName, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropResourceImageV3(const System::UnicodeString aResName, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropResourceImageV3(const System::UnicodeString aResName, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropFileImageV1(const System::UnicodeString aFileName, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropFileImageV1(const System::UnicodeString aFileName, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropFileImageV2(const System::UnicodeString aFileName, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropFileImageV2(const System::UnicodeString aFileName, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropFileImageV3(const System::UnicodeString aFileName, float W, float H, const System::Types::TPointF &aCropCenter)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadPlaceIntoAndCropFileImageV3(const System::UnicodeString aFileName, float W, float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoImageV1(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoImageV2(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoImageV3(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALFitIntoImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoResourceImageV1(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoResourceImageV2(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoResourceImageV3(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoFileImageV1(const System::UnicodeString aFileName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoFileImageV2(const System::UnicodeString aFileName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadFitIntoFileImageV3(const System::UnicodeString aFileName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALStretchImageV1(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALStretchImageV1(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALStretchImageV2(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALStretchImageV2(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALStretchImageV3(System::Classes::TCustomMemoryStream* const aStream, const _di_TALResizeImageGetDestSizeFunct aGetDestSizeFunct)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALStretchImageV3(System::Classes::TCustomMemoryStream* const aStream, const float W, const float H)/* overload */;
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadStretchResourceImageV1(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadStretchResourceImageV2(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadStretchResourceImageV3(const System::UnicodeString aResName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadStretchFileImageV1(const System::UnicodeString aFileName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadStretchFileImageV2(const System::UnicodeString aFileName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadStretchFileImageV3(const System::UnicodeString aFileName, const float W, const float H);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadNormalizeOrientationImageV1(System::Classes::TCustomMemoryStream* const aStream, const TalExifOrientationInfo aExifOrientationInfo);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadNormalizeOrientationImageV2(System::Classes::TCustomMemoryStream* const aStream, const TalExifOrientationInfo aExifOrientationInfo);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadNormalizeOrientationImageV3(System::Classes::TCustomMemoryStream* const aStream, const TalExifOrientationInfo aExifOrientationInfo);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadNormalizeOrientationFileImageV1(const System::UnicodeString aFileName);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadNormalizeOrientationFileImageV2(const System::UnicodeString aFileName);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALLoadNormalizeOrientationFileImageV3(const System::UnicodeString aFileName);
extern DELPHI_PACKAGE TalExifOrientationInfo __fastcall AlGetExifOrientationInfo(const System::UnicodeString aFilename);
extern DELPHI_PACKAGE void __fastcall ALNormalizeImageOrientationV1(Fmx::Graphics::TBitmap* const aBitmap, const TalExifOrientationInfo aExifOrientationInfo);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall ALNormalizeImageOrientationV2(Fmx::Graphics::TBitmap* const aBitmap, const TalExifOrientationInfo aExifOrientationInfo);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall AlGetImageSignature(System::Classes::TStream* const aStream)/* overload */;
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall AlGetImageSignature(const System::UnicodeString aFileName)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall AlDetectImageExtensionU(System::Classes::TStream* const aStream)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall AlDetectImageExtensionU(const System::UnicodeString aFileName)/* overload */;
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall ALPrepareColor(const System::Uitypes::TAlphaColor SrcColor, const float Opacity);
}	/* namespace Algraphics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALGRAPHICS)
using namespace Algraphics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlgraphicsHPP
