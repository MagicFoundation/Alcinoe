﻿// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxInertialMovement.pas' rev: 31.00 (Windows)

#ifndef AlfmxinertialmovementHPP
#define AlfmxinertialmovementHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Messaging.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxinertialmovement
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALScrollingAcquiredMessage;
struct TALPointD;
struct TALRectD;
class DELPHICLASS TALAniCalculations;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TALScrollingAcquiredMessage : public System::Messaging::TMessageBase
{
	typedef System::Messaging::TMessageBase inherited;
	
public:
	/* TObject.Create */ inline __fastcall TALScrollingAcquiredMessage(void) : System::Messaging::TMessageBase() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TALScrollingAcquiredMessage(void) { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TALPointD
{
public:
	double X;
	double Y;
	__fastcall TALPointD(const TALPointD &P)/* overload */;
	__fastcall TALPointD(const System::Types::TPointF &P)/* overload */;
	__fastcall TALPointD(const System::Types::TPoint &P)/* overload */;
	__fastcall TALPointD(const double X, const double Y)/* overload */;
	void __fastcall SetLocation(const TALPointD &P);
	static bool __fastcall _op_Equality(const TALPointD &Lhs, const TALPointD &Rhs);
	static bool __fastcall _op_Inequality(const TALPointD &Lhs, const TALPointD &Rhs);
	static TALPointD __fastcall _op_Addition(const TALPointD &Lhs, const TALPointD &Rhs);
	static TALPointD __fastcall _op_Subtraction(const TALPointD &Lhs, const TALPointD &Rhs);
	static TALPointD __fastcall _op_Implicit(const System::Types::TPointF &APointF);
	TALPointD& __fastcall operator=(const System::Types::TPointF &APointF) { *this = TALPointD::_op_Implicit(APointF); return *this; };
	double __fastcall Distance(const TALPointD &P2);
	double __fastcall Abs(void);
	void __fastcall Offset(const double DX, const double DY);
	TALPointD() {}
};


struct DECLSPEC_DRECORD TALRectD
{
public:
	double Left;
	double Top;
	double Right;
	double Bottom;
	
private:
	double __fastcall GetHeight(void);
	double __fastcall GetWidth(void);
	void __fastcall SetHeight(const double Value);
	void __fastcall SetWidth(const double Value);
	TALPointD __fastcall GetTopLeft(void);
	void __fastcall SetTopLeft(const TALPointD &P);
	TALPointD __fastcall GetBottomRight(void);
	void __fastcall SetBottomRight(const TALPointD &P);
	
public:
	__fastcall TALRectD(const TALPointD &Origin)/* overload */;
	__fastcall TALRectD(const double Left, const double Top, const double Right, const double Bottom)/* overload */;
	static bool __fastcall _op_Equality(const TALRectD &Lhs, const TALRectD &Rhs);
	static bool __fastcall _op_Inequality(const TALRectD &Lhs, const TALRectD &Rhs);
	static TALRectD __fastcall Empty();
	__property double Width = {read=GetWidth, write=SetWidth};
	__property double Height = {read=GetHeight, write=SetHeight};
	void __fastcall Inflate(const double DX, const double DY);
	void __fastcall Offset(const double DX, const double DY);
	__property TALPointD TopLeft = {read=GetTopLeft, write=SetTopLeft};
	__property TALPointD BottomRight = {read=GetBottomRight, write=SetBottomRight};
	TALRectD() {}
};


struct TTarget;
class PASCALIMPLEMENTATION TALAniCalculations : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
	
public:
	enum class DECLSPEC_DENUM TTargetType : unsigned char { Achieved, Max, Min, Other };
	
	struct DECLSPEC_DRECORD TTarget
	{
	public:
		TALAniCalculations::TTargetType TargetType;
		TALPointD Point;
	};
	
	
	
private:
	struct DECLSPEC_DRECORD TPointTime
	{
	public:
		TALPointD Point;
		System::TDateTime Time;
	};
	
	
	typedef System::DynamicArray<TTarget> _TALAniCalculations__1;
	
	
private:
	System::Types::TPointF FVelocityFactor;
	bool FEnabled;
	bool FInTimerProc;
	System::Uitypes::TTouchTracking FTouchTracking;
	NativeUInt FTimerHandle;
	System::Word FInterval;
	TALPointD FCurrentVelocity;
	TALPointD FUpVelocity;
	TALPointD FUpPosition;
	System::TDateTime FUpDownTime;
	System::TDateTime FLastTimeCalc;
	System::Classes::TPersistent* FOwner;
	Fmx::Types::_di_IFMXTimerService FPlatformTimer;
	System::Generics::Collections::TList__1<TPointTime>* FPointTime;
	_TALAniCalculations__1 FTargets;
	TTarget FMinTarget;
	TTarget FMaxTarget;
	TTarget FTarget;
	TTarget FLastTarget;
	bool FCancelTargetX;
	bool FCancelTargetY;
	System::Classes::TNotifyEvent FOnStart;
	System::Classes::TNotifyEvent FOnTimer;
	System::Classes::TNotifyEvent FOnStop;
	bool FDown;
	bool FAnimation;
	TALPointD FViewportPosition;
	bool FLowChanged;
	System::TDateTime FLastTimeChanged;
	TALPointD FDownPoint;
	TALPointD FDownPosition;
	System::Int8 FUpdateTimerCount;
	double FElasticity;
	double FDecelerationRate;
	double FStorageTime;
	bool FInDoStart;
	bool FInDoStop;
	bool FMoved;
	bool FStarted;
	bool FBoundsAnimation;
	bool FAutoShowing;
	float FOpacity;
	bool FShown;
	TTarget FMouseTarget;
	bool FAveraging;
	int FMinVelocity;
	int FMaxVelocity;
	int FDeadZone;
	int FUpdateCount;
	System::Types::TPoint FElasticityFactor;
	void __fastcall StartTimer(void);
	void __fastcall StopTimer(void);
	void __fastcall TimerProc(void);
	void __fastcall Clear(System::TDateTime T = 0.000000E+00);
	void __fastcall UpdateTimer(void);
	void __fastcall SetInterval(const System::Word Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetTouchTracking(const System::Uitypes::TTouchTracking Value);
	void __fastcall InternalCalc(double DeltaTime);
	void __fastcall SetAnimation(const bool Value);
	void __fastcall SetDown(const bool Value);
	bool __fastcall FindTarget(TTarget &Target);
	int __fastcall GetTargetCount(void);
	bool __fastcall DecelerationRateStored(void);
	bool __fastcall ElasticityStored(void);
	bool __fastcall StorageTimeStored(void);
	void __fastcall CalcVelocity(const System::TDateTime Time = 0.000000E+00);
	void __fastcall InternalStart(void);
	void __fastcall InternalTerminated(void);
	void __fastcall SetBoundsAnimation(const bool Value);
	void __fastcall UpdateViewportPositionByBounds(void);
	void __fastcall SetAutoShowing(const bool Value);
	void __fastcall SetShown(const bool Value);
	System::Types::TPointF __fastcall GetViewportPositionF(void);
	void __fastcall SetViewportPositionF(const System::Types::TPointF &Value);
	void __fastcall SetMouseTarget(const TTarget &Value);
	System::Uitypes::TTouchTracking __fastcall GetInternalTouchTracking(void);
	TALPointD __fastcall GetPositions(const int Index);
	int __fastcall GetPositionCount(void);
	System::TDateTime __fastcall GetPositionTimes(const int Index);
	TALPointD __fastcall PosToView(const TALPointD &APosition);
	void __fastcall SetViewportPosition(const TALPointD &Value);
	float __fastcall GetOpacity(void);
	bool __fastcall GetLowVelocity(void);
	TPointTime __fastcall AddPointTime(const double X, const double Y, const System::TDateTime Time = 0.000000E+00);
	void __fastcall InternalChanged(void);
	void __fastcall UpdateTarget(void);
	bool __fastcall DoStopScrolling(System::TDateTime CurrentTime = 0.000000E+00);
	
protected:
	bool __fastcall IsSmall(const TALPointD &P, const double Epsilon)/* overload */;
	bool __fastcall IsSmall(const TALPointD &P)/* overload */;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	virtual void __fastcall DoStart(void);
	virtual void __fastcall DoChanged(void);
	virtual void __fastcall DoStop(void);
	virtual void __fastcall DoCalc(const double DeltaTime, TALPointD &NewPoint, TALPointD &NewVelocity, bool &Done);
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property bool Shown = {read=FShown, write=SetShown, nodefault};
	__property TTarget MouseTarget = {read=FMouseTarget, write=SetMouseTarget};
	__property System::Uitypes::TTouchTracking InternalTouchTracking = {read=GetInternalTouchTracking, nodefault};
	__property TALPointD Positions[const int index] = {read=GetPositions};
	__property System::TDateTime PositionTimes[const int index] = {read=GetPositionTimes};
	__property int PositionCount = {read=GetPositionCount, nodefault};
	__property TALPointD UpVelocity = {read=FUpVelocity};
	__property TALPointD UpPosition = {read=FUpPosition};
	__property System::TDateTime UpDownTime = {read=FUpDownTime};
	__property TTarget MinTarget = {read=FMinTarget};
	__property TTarget MaxTarget = {read=FMaxTarget};
	__property TTarget Target = {read=FTarget};
	__property int MinVelocity = {read=FMinVelocity, write=FMinVelocity, default=10};
	__property int MaxVelocity = {read=FMaxVelocity, write=FMaxVelocity, default=5000};
	__property int DeadZone = {read=FDeadZone, write=FDeadZone, default=8};
	__property bool CancelTargetX = {read=FCancelTargetX, nodefault};
	__property bool CancelTargetY = {read=FCancelTargetY, nodefault};
	
public:
	__fastcall virtual TALAniCalculations(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TALAniCalculations(void);
	virtual void __fastcall AfterConstruction(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall MouseDown(double X, double Y);
	virtual void __fastcall MouseMove(double X, double Y);
	virtual void __fastcall MouseLeave(void);
	virtual void __fastcall MouseUp(double X, double Y);
	virtual void __fastcall MouseWheel(double X, double Y);
	__property bool Animation = {read=FAnimation, write=SetAnimation, default=0};
	__property bool AutoShowing = {read=FAutoShowing, write=SetAutoShowing, default=0};
	__property bool Averaging = {read=FAveraging, write=FAveraging, default=0};
	__property bool BoundsAnimation = {read=FBoundsAnimation, write=SetBoundsAnimation, default=1};
	__property System::Word Interval = {read=FInterval, write=SetInterval, default=10};
	__property System::Uitypes::TTouchTracking TouchTracking = {read=FTouchTracking, write=SetTouchTracking, default=3};
	__property int TargetCount = {read=GetTargetCount, nodefault};
	void __fastcall SetTargets(const TTarget *ATargets, const int ATargets_High);
	void __fastcall GetTargets(TTarget *ATargets, const int ATargets_High);
	void __fastcall UpdatePosImmediately(const bool Force = false);
	__property double DecelerationRate = {read=FDecelerationRate, write=FDecelerationRate, stored=DecelerationRateStored};
	__property double Elasticity = {read=FElasticity, write=FElasticity, stored=ElasticityStored};
	__property double StorageTime = {read=FStorageTime, write=FStorageTime, stored=StorageTimeStored};
	__property System::Types::TPointF VelocityFactor = {read=FVelocityFactor, write=FVelocityFactor};
	__property TALPointD CurrentVelocity = {read=FCurrentVelocity};
	__property TALPointD ViewportPosition = {read=FViewportPosition, write=SetViewportPosition};
	__property System::Types::TPointF ViewportPositionF = {read=GetViewportPositionF, write=SetViewportPositionF};
	__property System::TDateTime LastTimeCalc = {read=FLastTimeCalc};
	__property bool Down = {read=FDown, write=SetDown, nodefault};
	__property float Opacity = {read=GetOpacity};
	__property bool InTimerProc = {read=FInTimerProc, nodefault};
	__property bool Moved = {read=FMoved, nodefault};
	__property bool LowVelocity = {read=GetLowVelocity, nodefault};
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	__property System::Classes::TNotifyEvent OnStart = {read=FOnStart, write=FOnStart};
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnTimer, write=FOnTimer};
	__property System::Classes::TNotifyEvent OnStop = {read=FOnStop, write=FOnStop};
};


//-- var, const, procedure ---------------------------------------------------
#define ALDefaultStorageTime  (1.500000E-01)
static const System::Int8 ALDefaultIntervalOfAni = System::Int8(0xa);
#define ALDecelerationRateNormal  (1.950000E+00)
#define ALDecelerationRateFast  (9.500000E+00)
static const System::Int8 ALDefaultElasticity = System::Int8(0x64);
static const System::Int8 ALDefaultMinVelocity = System::Int8(0xa);
static const System::Word ALDefaultMaxVelocity = System::Word(0x1388);
static const System::Int8 ALDefaultDeadZone = System::Int8(0x8);
static const System::Int8 ALDefaultDeadZoneBeforeAcquireScrolling = System::Int8(0x10);
}	/* namespace Alfmxinertialmovement */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXINERTIALMOVEMENT)
using namespace Alfmxinertialmovement;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxinertialmovementHPP
