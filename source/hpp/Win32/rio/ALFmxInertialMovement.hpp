// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxInertialMovement.pas' rev: 33.00 (Windows)

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
#include <System.TypInfo.hpp>
#include <FMX.Types.hpp>
#include <ALCommon.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxinertialmovement
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALScrollingAcquiredMessage;
class DELPHICLASS TALAniCalculations;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TALScrollingAcquiredMessage : public System::Messaging::TMessageBase
{
	typedef System::Messaging::TMessageBase inherited;
	
private:
	bool FAcquired;
	
public:
	__fastcall TALScrollingAcquiredMessage(const bool AAcquired);
	__property bool Acquired = {read=FAcquired, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TALScrollingAcquiredMessage() { }
	
};

#pragma pack(pop)

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
		Alcommon::TALPointD Point;
	};
	
	
	
private:
	struct DECLSPEC_DRECORD TPointTime
	{
	public:
		Alcommon::TALPointD Point;
		System::TDateTime Time;
	};
	
	
	typedef System::DynamicArray<TTarget> _TALAniCalculations__1;
	
	
private:
	NativeUInt FTimerHandle;
	bool fMouseEventReceived;
	bool FTimerActive;
	double FVelocityFactor;
	bool FEnabled;
	bool FInTimerProc;
	System::Uitypes::TTouchTracking FTouchTracking;
	System::Word FInterval;
	Alcommon::TALPointD FCurrentVelocity;
	Alcommon::TALPointD FUpVelocity;
	Alcommon::TALPointD FUpPosition;
	System::TDateTime FUpDownTime;
	double FlastTickCalc;
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
	System::Classes::TNotifyEvent FOnChanged;
	System::Classes::TNotifyEvent FOnStop;
	bool FDown;
	bool FAnimation;
	Alcommon::TALPointD FViewportPosition;
	bool FLowChanged;
	System::TDateTime FLastTimeChanged;
	Alcommon::TALPointD FDownPoint;
	Alcommon::TALPointD FDownPosition;
	int FUpdateTimerCount;
	double FElasticity;
	double FDecelerationRate;
	double FStorageTime;
	bool FInDoStart;
	bool FInDoStop;
	bool FMoved;
	bool FStarted;
	bool FBoundsAnimationAtMinTarget;
	bool FBoundsAnimationAtMaxTarget;
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
	System::Classes::TNotifyEvent FOnCalcVelocity;
	void __fastcall StartTimer();
	void __fastcall StopTimer();
	void __fastcall Clear(System::TDateTime T = 0.000000E+00);
	void __fastcall UpdateTimer();
	void __fastcall SetInterval(const System::Word Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetTouchTracking(const System::Uitypes::TTouchTracking Value);
	void __fastcall InternalCalc(double DeltaTime);
	void __fastcall SetAnimation(const bool Value);
	void __fastcall SetDown(const bool Value);
	bool __fastcall FindTarget(TTarget &Target);
	int __fastcall GetTargetCount();
	bool __fastcall DecelerationRateStored();
	bool __fastcall ElasticityStored();
	bool __fastcall StorageTimeStored();
	bool __fastcall VelocityFactorStored();
	void __fastcall CalcVelocity(const System::TDateTime Time = 0.000000E+00);
	void __fastcall InternalStart();
	void __fastcall InternalTerminated();
	bool __fastcall GetBoundsAnimation();
	void __fastcall SetBoundsAnimation(const bool Value);
	void __fastcall SetBoundsAnimationAtMinTarget(const bool Value);
	void __fastcall SetBoundsAnimationAtMaxTarget(const bool Value);
	void __fastcall UpdateViewportPositionByBounds();
	void __fastcall SetAutoShowing(const bool Value);
	void __fastcall SetShown(const bool Value);
	System::Types::TPointF __fastcall GetViewportPositionF();
	void __fastcall SetViewportPositionF(const System::Types::TPointF &Value);
	void __fastcall SetMouseTarget(const TTarget &Value);
	System::Uitypes::TTouchTracking __fastcall GetInternalTouchTracking();
	Alcommon::TALPointD __fastcall GetPositions(const int Index);
	int __fastcall GetPositionCount();
	System::TDateTime __fastcall GetPositionTimes(const int Index);
	Alcommon::TALPointD __fastcall PosToView(const Alcommon::TALPointD &APosition);
	void __fastcall SetViewportPosition(const Alcommon::TALPointD &Value);
	float __fastcall GetOpacity();
	bool __fastcall GetLowVelocity();
	TPointTime __fastcall AddPointTime(const double X, const double Y, const System::TDateTime Time = 0.000000E+00);
	void __fastcall InternalChanged();
	void __fastcall UpdateTarget();
	bool __fastcall DoStopScrolling(System::TDateTime CurrentTime = 0.000000E+00);
	
protected:
	bool __fastcall IsSmall(const Alcommon::TALPointD &P, const double Epsilon)/* overload */;
	bool __fastcall IsSmall(const Alcommon::TALPointD &P)/* overload */;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall DoStart();
	virtual void __fastcall DoChanged();
	virtual void __fastcall DoStop();
	virtual void __fastcall DoCalc(const double DeltaTime, Alcommon::TALPointD &NewPoint, Alcommon::TALPointD &NewVelocity, bool &Done);
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property bool Shown = {read=FShown, write=SetShown, nodefault};
	__property TTarget MouseTarget = {read=FMouseTarget, write=SetMouseTarget};
	__property System::Uitypes::TTouchTracking InternalTouchTracking = {read=GetInternalTouchTracking, nodefault};
	__property Alcommon::TALPointD Positions[const int index] = {read=GetPositions};
	__property System::TDateTime PositionTimes[const int index] = {read=GetPositionTimes};
	__property int PositionCount = {read=GetPositionCount, nodefault};
	__property Alcommon::TALPointD UpVelocity = {read=FUpVelocity};
	__property Alcommon::TALPointD UpPosition = {read=FUpPosition};
	__property System::TDateTime UpDownTime = {read=FUpDownTime};
	__property TTarget Target = {read=FTarget};
	__property int MinVelocity = {read=FMinVelocity, write=FMinVelocity, default=10};
	__property int MaxVelocity = {read=FMaxVelocity, write=FMaxVelocity, default=5000};
	__property bool CancelTargetX = {read=FCancelTargetX, nodefault};
	__property bool CancelTargetY = {read=FCancelTargetY, nodefault};
	
public:
	__fastcall virtual TALAniCalculations(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TALAniCalculations();
	virtual void __fastcall AfterConstruction();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall TimerProc();
	virtual void __fastcall MouseDown(double X, double Y);
	virtual void __fastcall MouseMove(double X, double Y);
	virtual void __fastcall MouseLeave();
	virtual void __fastcall MouseUp(double X, double Y);
	virtual void __fastcall MouseWheel(double X, double Y);
	__property bool Animation = {read=FAnimation, write=SetAnimation, default=0};
	__property bool AutoShowing = {read=FAutoShowing, write=SetAutoShowing, default=0};
	__property bool Averaging = {read=FAveraging, write=FAveraging, default=0};
	__property bool BoundsAnimation = {read=GetBoundsAnimation, write=SetBoundsAnimation, default=1};
	__property bool BoundsAnimationAtMinTarget = {read=FBoundsAnimationAtMinTarget, write=SetBoundsAnimationAtMinTarget, default=1};
	__property bool BoundsAnimationAtMaxTarget = {read=FBoundsAnimationAtMaxTarget, write=SetBoundsAnimationAtMaxTarget, default=1};
	__property System::Uitypes::TTouchTracking TouchTracking = {read=FTouchTracking, write=SetTouchTracking, default=3};
	__property int TargetCount = {read=GetTargetCount, nodefault};
	void __fastcall SetTargets(const TTarget *ATargets, const int ATargets_High);
	void __fastcall GetTargets(TTarget *ATargets, const int ATargets_High);
	__property TTarget MinTarget = {read=FMinTarget};
	__property TTarget MaxTarget = {read=FMaxTarget};
	void __fastcall UpdatePosImmediately(const bool Force = false);
	__property Alcommon::TALPointD CurrentVelocity = {read=FCurrentVelocity, write=FCurrentVelocity};
	__property Alcommon::TALPointD ViewportPosition = {read=FViewportPosition, write=SetViewportPosition};
	__property System::Types::TPointF ViewportPositionF = {read=GetViewportPositionF, write=SetViewportPositionF};
	__property Alcommon::TALPointD DownPosition = {read=FDownPosition, write=FDownPosition};
	__property System::TDateTime LastTimeCalc = {read=FLastTimeCalc};
	__property bool Down = {read=FDown, write=SetDown, nodefault};
	__property float Opacity = {read=GetOpacity};
	__property bool InTimerProc = {read=FInTimerProc, nodefault};
	void __fastcall WakeUpTimer();
	void __fastcall Calculate();
	__property bool Moved = {read=FMoved, nodefault};
	__property bool LowVelocity = {read=GetLowVelocity, nodefault};
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	__property System::Classes::TNotifyEvent OnStart = {read=FOnStart, write=FOnStart};
	__property System::Classes::TNotifyEvent OnTimer = {read=FOnTimer, write=FOnTimer};
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	__property System::Classes::TNotifyEvent OnStop = {read=FOnStop, write=FOnStop};
	__property int DeadZone = {read=FDeadZone, write=FDeadZone, default=8};
	__property bool mouseEventReceived = {read=fMouseEventReceived, nodefault};
	__property bool TimerActive = {read=FTimerActive, nodefault};
	
__published:
	__property System::Word Interval = {read=FInterval, write=SetInterval, default=10};
	__property double DecelerationRate = {read=FDecelerationRate, write=FDecelerationRate, stored=DecelerationRateStored};
	__property double Elasticity = {read=FElasticity, write=FElasticity, stored=ElasticityStored};
	__property double StorageTime = {read=FStorageTime, write=FStorageTime, stored=StorageTimeStored};
	__property double VelocityFactor = {read=FVelocityFactor, write=FVelocityFactor, stored=VelocityFactorStored};
	__property System::Classes::TNotifyEvent OnCalcVelocity = {read=FOnCalcVelocity, write=FOnCalcVelocity};
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
static const System::Int8 ALDefaultVelocityFactor = System::Int8(0x1);
extern DELPHI_PACKAGE System::Generics::Collections::TList__1<TALAniCalculations*>* ALAniCalcTimerProcs;
}	/* namespace Alfmxinertialmovement */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXINERTIALMOVEMENT)
using namespace Alfmxinertialmovement;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxinertialmovementHPP
