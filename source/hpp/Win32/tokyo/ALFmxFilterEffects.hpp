// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxFilterEffects.pas' rev: 32.00 (Windows)

#ifndef AlfmxfiltereffectsHPP
#define AlfmxfiltereffectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Types.hpp>
#include <FMX.Filter.hpp>
#include <FMX.Filter.Effects.hpp>
#include <FMX.Effects.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxfiltereffects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALFilterBaseFilterAccessPrivate;
class DELPHICLASS TALFilterBaseFilter;
class DELPHICLASS TFilterColorAdjust;
class DELPHICLASS TALColorAdjustEffect;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALFilterBaseFilterAccessPrivate : public Fmx::Types::TFmxObject
{
	typedef Fmx::Types::TFmxObject inherited;
	
protected:
	Fmx::Filter::TFilter* FFilter;
	Fmx::Filter::Effects::TFilterBaseFilter* FInputFilter;
public:
	/* TFmxObject.Create */ inline __fastcall virtual TALFilterBaseFilterAccessPrivate(System::Classes::TComponent* AOwner) : Fmx::Types::TFmxObject(AOwner) { }
	/* TFmxObject.Destroy */ inline __fastcall virtual ~TALFilterBaseFilterAccessPrivate(void) { }
	
};


class PASCALIMPLEMENTATION TALFilterBaseFilter : public Fmx::Filter::Effects::TFilterBaseFilter
{
	typedef Fmx::Filter::Effects::TFilterBaseFilter inherited;
	
protected:
	Fmx::Filter::TFilter* __fastcall filter(void);
public:
	/* TFilterBaseFilter.Create */ inline __fastcall virtual TALFilterBaseFilter(System::Classes::TComponent* AOwner) : Fmx::Filter::Effects::TFilterBaseFilter(AOwner) { }
	/* TFilterBaseFilter.Destroy */ inline __fastcall virtual ~TALFilterBaseFilter(void) { }
	
};


class PASCALIMPLEMENTATION TFilterColorAdjust : public TALFilterBaseFilter
{
	typedef TALFilterBaseFilter inherited;
	
private:
	float __fastcall GetContrast(void);
	void __fastcall SetContrast(float AValue);
	float __fastcall GetHighlights(void);
	void __fastcall SetHighlights(float AValue);
	float __fastcall GetShadows(void);
	void __fastcall SetShadows(float AValue);
	float __fastcall GetSaturation(void);
	void __fastcall SetSaturation(float AValue);
	float __fastcall GetVibrance(void);
	void __fastcall SetVibrance(float AValue);
	float __fastcall GetWhites(void);
	void __fastcall SetWhites(float AValue);
	float __fastcall GetBlacks(void);
	void __fastcall SetBlacks(float AValue);
	float __fastcall GetTemperature(void);
	void __fastcall SetTemperature(float AValue);
	float __fastcall GetTint(void);
	void __fastcall SetTint(float AValue);
	float __fastcall GetExposure(void);
	void __fastcall SetExposure(float AValue);
	float __fastcall GetGamma(void);
	void __fastcall SetGamma(float AValue);
	
__published:
	__property float Contrast = {read=GetContrast, write=SetContrast};
	__property float Highlights = {read=GetHighlights, write=SetHighlights};
	__property float Shadows = {read=GetShadows, write=SetShadows};
	__property float Saturation = {read=GetSaturation, write=SetSaturation};
	__property float Vibrance = {read=GetVibrance, write=SetVibrance};
	__property float Whites = {read=GetWhites, write=SetWhites};
	__property float Blacks = {read=GetBlacks, write=SetBlacks};
	__property float Temperature = {read=GetTemperature, write=SetTemperature};
	__property float Tint = {read=GetTint, write=SetTint};
	__property float Exposure = {read=GetExposure, write=SetExposure};
	__property float Gamma = {read=GetGamma, write=SetGamma};
public:
	/* TFilterBaseFilter.Create */ inline __fastcall virtual TFilterColorAdjust(System::Classes::TComponent* AOwner) : TALFilterBaseFilter(AOwner) { }
	/* TFilterBaseFilter.Destroy */ inline __fastcall virtual ~TFilterColorAdjust(void) { }
	
};


class PASCALIMPLEMENTATION TALColorAdjustEffect : public Fmx::Filter::Effects::TImageFXEffect
{
	typedef Fmx::Filter::Effects::TImageFXEffect inherited;
	
private:
	float __fastcall GetContrast(void);
	void __fastcall SetContrast(float AValue);
	float __fastcall GetHighlights(void);
	void __fastcall SetHighlights(float AValue);
	float __fastcall GetShadows(void);
	void __fastcall SetShadows(float AValue);
	float __fastcall GetSaturation(void);
	void __fastcall SetSaturation(float AValue);
	float __fastcall GetVibrance(void);
	void __fastcall SetVibrance(float AValue);
	float __fastcall GetWhites(void);
	void __fastcall SetWhites(float AValue);
	float __fastcall GetBlacks(void);
	void __fastcall SetBlacks(float AValue);
	float __fastcall GetTemperature(void);
	void __fastcall SetTemperature(float AValue);
	float __fastcall GetTint(void);
	void __fastcall SetTint(float AValue);
	float __fastcall GetExposure(void);
	void __fastcall SetExposure(float AValue);
	float __fastcall GetGamma(void);
	void __fastcall SetGamma(float AValue);
	
public:
	__fastcall virtual TALColorAdjustEffect(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALColorAdjustEffect(void);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property float Contrast = {read=GetContrast, write=SetContrast};
	__property float Highlights = {read=GetHighlights, write=SetHighlights};
	__property float Shadows = {read=GetShadows, write=SetShadows};
	__property float Saturation = {read=GetSaturation, write=SetSaturation};
	__property float Vibrance = {read=GetVibrance, write=SetVibrance};
	__property float Whites = {read=GetWhites, write=SetWhites};
	__property float Blacks = {read=GetBlacks, write=SetBlacks};
	__property float Temperature = {read=GetTemperature, write=SetTemperature};
	__property float Tint = {read=GetTint, write=SetTint};
	__property float Exposure = {read=GetExposure, write=SetExposure};
	__property float Gamma = {read=GetGamma, write=SetGamma};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxfiltereffects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXFILTEREFFECTS)
using namespace Alfmxfiltereffects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxfiltereffectsHPP
