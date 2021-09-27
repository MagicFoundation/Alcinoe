// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxConfetti.pas' rev: 34.00 (Windows)

#ifndef AlfmxconfettiHPP
#define AlfmxconfettiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Math.Vectors.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.UITypes.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Graphics.hpp>
#include <ALFmxAni.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxconfetti
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALConfetti;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALConfetti : public Fmx::Types::TFmxObject
{
	typedef Fmx::Types::TFmxObject inherited;
	
	
public:
	struct DECLSPEC_DRECORD TParticule
	{
		
	public:
		enum DECLSPEC_DENUM TShape : unsigned char { square, circle };
		
		
	public:
		float x;
		float y;
		float wobble;
		float wobbleSpeed;
		float velocity;
		float angle2D;
		float tiltAngle;
		System::Uitypes::TAlphaColor color;
		TShape shape;
		float tick;
		float totalTicks;
		float decay;
		float drift;
		float random;
		float tiltSin;
		float tiltCos;
		float wobbleX;
		float wobbleY;
		float gravity;
		float ovalScalar;
		float scalar;
	};
	
	
	
private:
	Fmx::Controls::TOnPaintEvent FOriginalOnPaint;
	System::Classes::TNotifyEvent FOnFinish;
	System::Generics::Collections::TList__1<TParticule>* FParticules;
	Fmx::Types::TTimer* FTimer;
	bool __fastcall updateParticule(Fmx::Graphics::TCanvas* const ACanvas, const System::Types::TRectF &ARect, TParticule &AParticule);
	TParticule __fastcall randomPhysics(const float AX, const float AY, const float AAngle, const float ASpread, const float AStartVelocity, const System::Uitypes::TAlphaColor AColor, const TParticule::TShape AShape, const float ATicks, const float ADecay, const float AGravity, const float ADrift, const float AScalar);
	void __fastcall onTimer(System::TObject* Sender);
	void __fastcall onPaint(System::TObject* Sender, Fmx::Graphics::TCanvas* Canvas, const System::Types::TRectF &ARect);
	int __fastcall getParticuleCount();
	
public:
	__fastcall virtual TALConfetti(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALConfetti();
	bool __fastcall isRunning();
	void __fastcall Fire(const float AOriginX = 5.000000E-01f, const float AOriginY = 5.000000E-01f, const int AParticleCount = 0x32, const float AAngle = 9.000000E+01f, const float ASpread = 4.500000E+01f, const float AStartVelocity = 4.500000E+01f, const System::DynamicArray<System::Uitypes::TAlphaColor> AColors = System::DynamicArray<System::Uitypes::TAlphaColor>(), const System::DynamicArray<TParticule::TShape> AShapes = System::DynamicArray<TParticule::TShape>(), const float ATicks = 2.000000E+02f, const float ADecay = 9.000000E-01f, const float AGravity = 1.000000E+00f, const float ADrift = 0.000000E+00f, const float AScalar = 1.000000E+00f);
	__property int ParticuleCount = {read=getParticuleCount, nodefault};
	
__published:
	__property System::Classes::TNotifyEvent OnFinish = {read=FOnFinish, write=FOnFinish};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxconfetti */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXCONFETTI)
using namespace Alfmxconfetti;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxconfettiHPP
