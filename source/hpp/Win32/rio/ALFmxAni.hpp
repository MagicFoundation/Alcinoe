// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFmxAni.pas' rev: 33.00 (Windows)

#ifndef AlfmxaniHPP
#define AlfmxaniHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SyncObjs.hpp>
#include <System.Rtti.hpp>
#include <System.Generics.Collections.hpp>
#include <System.UITypes.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfmxani
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALAniThread;
class DELPHICLASS TALAnimation;
class DELPHICLASS TALFloatAnimation;
class DELPHICLASS TALColorAnimation;
class DELPHICLASS TALCustomPropertyAnimation;
class DELPHICLASS TALFloatPropertyAnimation;
class DELPHICLASS TALColorPropertyAnimation;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TALAniThread : public Fmx::Types::TTimer
{
	typedef Fmx::Types::TTimer inherited;
	
private:
	System::Generics::Collections::TList__1<TALAnimation*>* FAniList;
	double FTime;
	double FDeltaTime;
	Fmx::Types::_di_IFMXTimerService FTimerService;
	void __fastcall OneStep();
	void __fastcall DoSyncTimer(System::TObject* Sender);
	
public:
	__fastcall TALAniThread();
	__fastcall virtual ~TALAniThread();
	void __fastcall AddAnimation(TALAnimation* const Ani);
	void __fastcall RemoveAnimation(TALAnimation* const Ani);
	void __fastcall WakeUpTimer();
};


class PASCALIMPLEMENTATION TALAnimation : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	static const System::Int8 DefaultAniFrameRate = System::Int8(0x3c);
	
	static int AniFrameRate;
	
private:
	static TALAniThread* FAniThread;
	__int64 FTag;
	System::TObject* FTagObject;
	double FTagFloat;
	float fOvershoot;
	int FTickCount;
	float FDuration;
	float FDelay;
	float FDelayTime;
	float FTime;
	bool FInverse;
	bool FSavedInverse;
	bool FLoop;
	bool FPause;
	bool FRunning;
	System::Classes::TNotifyEvent FOnFirstFrame;
	System::Classes::TNotifyEvent FOnProcess;
	System::Classes::TNotifyEvent FOnFinish;
	Fmx::Types::TInterpolationType FInterpolation;
	Fmx::Types::TAnimationType FAnimationType;
	bool FEnabled;
	bool FAutoReverse;
	void __fastcall SetEnabled(const bool Value);
	__classmethod void __fastcall Uninitialize();
	
protected:
	float __fastcall GetNormalizedTime();
	virtual void __fastcall FirstFrame();
	virtual void __fastcall ProcessAnimation() = 0 ;
	virtual void __fastcall DoProcess();
	virtual void __fastcall DoFinish();
	
public:
	__classmethod void __fastcall WakeUpTimer();
	__fastcall virtual TALAnimation();
	__fastcall virtual ~TALAnimation();
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
	virtual void __fastcall StopAtCurrent();
	void __fastcall ProcessTick(float time, float deltaTime);
	__property bool Running = {read=FRunning, nodefault};
	__property bool Pause = {read=FPause, write=FPause, nodefault};
	__property Fmx::Types::TAnimationType AnimationType = {read=FAnimationType, write=FAnimationType, default=0};
	__property bool AutoReverse = {read=FAutoReverse, write=FAutoReverse, default=0};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
	__property float Delay = {read=FDelay, write=FDelay};
	__property float Duration = {read=FDuration, write=FDuration};
	__property Fmx::Types::TInterpolationType Interpolation = {read=FInterpolation, write=FInterpolation, default=0};
	__property bool Inverse = {read=FInverse, write=FInverse, default=0};
	__property float NormalizedTime = {read=GetNormalizedTime};
	__property bool Loop = {read=FLoop, write=FLoop, default=0};
	__property float CurrentTime = {read=FTime};
	__property System::Classes::TNotifyEvent OnFirstFrame = {read=FOnFirstFrame, write=FOnFirstFrame};
	__property System::Classes::TNotifyEvent OnProcess = {read=FOnProcess, write=FOnProcess};
	__property System::Classes::TNotifyEvent OnFinish = {read=FOnFinish, write=FOnFinish};
	__property float Overshoot = {read=fOvershoot, write=fOvershoot};
	/* static */ __property TALAniThread* AniThread = {read=FAniThread};
	__property __int64 Tag = {read=FTag, write=FTag, default=0};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	__property double TagFloat = {read=FTagFloat, write=FTagFloat};
};


class PASCALIMPLEMENTATION TALFloatAnimation : public TALAnimation
{
	typedef TALAnimation inherited;
	
private:
	double FStartFloat;
	double FStopFloat;
	double fcurrentFloat;
	
protected:
	virtual void __fastcall ProcessAnimation();
	
public:
	__fastcall virtual TALFloatAnimation();
	virtual void __fastcall Start();
	__property double StartValue = {read=FStartFloat, write=FStartFloat};
	__property double StopValue = {read=FStopFloat, write=FStopFloat};
	__property double CurrentValue = {read=fcurrentFloat};
public:
	/* TALAnimation.Destroy */ inline __fastcall virtual ~TALFloatAnimation() { }
	
};


class PASCALIMPLEMENTATION TALColorAnimation : public TALAnimation
{
	typedef TALAnimation inherited;
	
private:
	System::Uitypes::TAlphaColor FStartColor;
	System::Uitypes::TAlphaColor FStopColor;
	System::Uitypes::TAlphaColor fcurrentColor;
	
protected:
	virtual void __fastcall ProcessAnimation();
	
public:
	__fastcall virtual TALColorAnimation();
	virtual void __fastcall Start();
	__property System::Uitypes::TAlphaColor StartValue = {read=FStartColor, write=FStartColor, nodefault};
	__property System::Uitypes::TAlphaColor StopValue = {read=FStopColor, write=FStopColor, nodefault};
	__property System::Uitypes::TAlphaColor CurrentValue = {read=fcurrentColor, nodefault};
public:
	/* TALAnimation.Destroy */ inline __fastcall virtual ~TALColorAnimation() { }
	
};


class PASCALIMPLEMENTATION TALCustomPropertyAnimation : public Fmx::Types::TFmxObject
{
	typedef Fmx::Types::TFmxObject inherited;
	
protected:
	System::TObject* FInstance;
	System::Rtti::TRttiProperty* FRttiProperty;
	System::UnicodeString FPath;
	System::UnicodeString FPropertyName;
	void __fastcall SetPropertyName(const System::UnicodeString AValue);
	bool __fastcall FindProperty();
	virtual void __fastcall ParentChanged();
	
public:
	__property System::UnicodeString PropertyName = {read=FPropertyName, write=SetPropertyName};
	virtual void __fastcall Stop();
public:
	/* TFmxObject.Create */ inline __fastcall virtual TALCustomPropertyAnimation(System::Classes::TComponent* AOwner) : Fmx::Types::TFmxObject(AOwner) { }
	/* TFmxObject.Destroy */ inline __fastcall virtual ~TALCustomPropertyAnimation() { }
	
};


class PASCALIMPLEMENTATION TALFloatPropertyAnimation : public TALCustomPropertyAnimation
{
	typedef TALCustomPropertyAnimation inherited;
	
private:
	bool FStartFromCurrent;
	TALFloatAnimation* fFloatAnimation;
	System::Classes::TNotifyEvent FOnFirstFrame;
	System::Classes::TNotifyEvent FOnProcess;
	System::Classes::TNotifyEvent FOnFinish;
	Fmx::Types::TAnimationType __fastcall getAnimationType();
	bool __fastcall getAutoReverse();
	float __fastcall getDelay();
	float __fastcall getDuration();
	bool __fastcall getEnabled();
	Fmx::Types::TInterpolationType __fastcall getInterpolation();
	bool __fastcall getInverse();
	bool __fastcall getLoop();
	float __fastcall getOvershoot();
	bool __fastcall getPause();
	bool __fastcall getRunning();
	float __fastcall GetStartValue();
	float __fastcall GetStopValue();
	bool __fastcall OvershootStored();
	void __fastcall setAnimationType(const Fmx::Types::TAnimationType Value);
	void __fastcall setAutoReverse(const bool Value);
	void __fastcall setDelay(const float Value);
	void __fastcall setDuration(const float Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall setInterpolation(const Fmx::Types::TInterpolationType Value);
	void __fastcall setInverse(const bool Value);
	void __fastcall setLoop(const bool Value);
	void __fastcall setOvershoot(const float Value);
	void __fastcall setPause(const bool Value);
	void __fastcall SetStartValue(const float Value);
	void __fastcall setStopValue(const float Value);
	
protected:
	void __fastcall doFirstFrame(System::TObject* Sender);
	void __fastcall doProcess(System::TObject* Sender);
	void __fastcall doFinish(System::TObject* Sender);
	
public:
	__fastcall virtual TALFloatPropertyAnimation(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALFloatPropertyAnimation();
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
	virtual void __fastcall StopAtCurrent();
	__property bool Running = {read=getRunning, nodefault};
	__property bool Pause = {read=getPause, write=setPause, nodefault};
	
__published:
	__property Fmx::Types::TAnimationType AnimationType = {read=getAnimationType, write=setAnimationType, default=0};
	__property bool AutoReverse = {read=getAutoReverse, write=setAutoReverse, default=0};
	__property bool Enabled = {read=getEnabled, write=SetEnabled, default=0};
	__property float Delay = {read=getDelay, write=setDelay};
	__property float Duration = {read=getDuration, write=setDuration};
	__property Fmx::Types::TInterpolationType Interpolation = {read=getInterpolation, write=setInterpolation, default=0};
	__property bool Inverse = {read=getInverse, write=setInverse, default=0};
	__property bool Loop = {read=getLoop, write=setLoop, default=0};
	__property System::Classes::TNotifyEvent OnFirstFrame = {read=FOnFirstFrame, write=FOnFirstFrame};
	__property System::Classes::TNotifyEvent OnProcess = {read=FOnProcess, write=FOnProcess};
	__property System::Classes::TNotifyEvent OnFinish = {read=FOnFinish, write=FOnFinish};
	__property PropertyName = {default=0};
	__property float StartValue = {read=GetStartValue, write=SetStartValue, stored=true};
	__property bool StartFromCurrent = {read=FStartFromCurrent, write=FStartFromCurrent, default=0};
	__property float StopValue = {read=GetStopValue, write=setStopValue, stored=true};
	__property float Overshoot = {read=getOvershoot, write=setOvershoot, stored=OvershootStored};
};


class PASCALIMPLEMENTATION TALColorPropertyAnimation : public TALCustomPropertyAnimation
{
	typedef TALCustomPropertyAnimation inherited;
	
private:
	bool FStartFromCurrent;
	TALColorAnimation* fColorAnimation;
	System::Classes::TNotifyEvent FOnFirstFrame;
	System::Classes::TNotifyEvent FOnProcess;
	System::Classes::TNotifyEvent FOnFinish;
	Fmx::Types::TAnimationType __fastcall getAnimationType();
	bool __fastcall getAutoReverse();
	float __fastcall getDelay();
	float __fastcall getDuration();
	bool __fastcall getEnabled();
	Fmx::Types::TInterpolationType __fastcall getInterpolation();
	bool __fastcall getInverse();
	bool __fastcall getLoop();
	float __fastcall getOvershoot();
	bool __fastcall getPause();
	bool __fastcall getRunning();
	System::Uitypes::TAlphaColor __fastcall GetStartValue();
	System::Uitypes::TAlphaColor __fastcall GetStopValue();
	bool __fastcall OvershootStored();
	void __fastcall setAnimationType(const Fmx::Types::TAnimationType Value);
	void __fastcall setAutoReverse(const bool Value);
	void __fastcall setDelay(const float Value);
	void __fastcall setDuration(const float Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall setInterpolation(const Fmx::Types::TInterpolationType Value);
	void __fastcall setInverse(const bool Value);
	void __fastcall setLoop(const bool Value);
	void __fastcall setOvershoot(const float Value);
	void __fastcall setPause(const bool Value);
	void __fastcall SetStartValue(const System::Uitypes::TAlphaColor Value);
	void __fastcall setStopValue(const System::Uitypes::TAlphaColor Value);
	
protected:
	void __fastcall doFirstFrame(System::TObject* Sender);
	void __fastcall doProcess(System::TObject* Sender);
	void __fastcall doFinish(System::TObject* Sender);
	
public:
	__fastcall virtual TALColorPropertyAnimation(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALColorPropertyAnimation();
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
	virtual void __fastcall StopAtCurrent();
	__property bool Running = {read=getRunning, nodefault};
	__property bool Pause = {read=getPause, write=setPause, nodefault};
	
__published:
	__property Fmx::Types::TAnimationType AnimationType = {read=getAnimationType, write=setAnimationType, default=0};
	__property bool AutoReverse = {read=getAutoReverse, write=setAutoReverse, default=0};
	__property bool Enabled = {read=getEnabled, write=SetEnabled, default=0};
	__property float Delay = {read=getDelay, write=setDelay};
	__property float Duration = {read=getDuration, write=setDuration};
	__property Fmx::Types::TInterpolationType Interpolation = {read=getInterpolation, write=setInterpolation, default=0};
	__property bool Inverse = {read=getInverse, write=setInverse, default=0};
	__property bool Loop = {read=getLoop, write=setLoop, default=0};
	__property System::Classes::TNotifyEvent OnFirstFrame = {read=FOnFirstFrame, write=FOnFirstFrame};
	__property System::Classes::TNotifyEvent OnProcess = {read=FOnProcess, write=FOnProcess};
	__property System::Classes::TNotifyEvent OnFinish = {read=FOnFinish, write=FOnFinish};
	__property PropertyName = {default=0};
	__property System::Uitypes::TAlphaColor StartValue = {read=GetStartValue, write=SetStartValue, stored=true, nodefault};
	__property bool StartFromCurrent = {read=FStartFromCurrent, write=FStartFromCurrent, default=0};
	__property System::Uitypes::TAlphaColor StopValue = {read=GetStopValue, write=setStopValue, stored=true, nodefault};
	__property float Overshoot = {read=getOvershoot, write=setOvershoot, stored=OvershootStored};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alfmxani */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFMXANI)
using namespace Alfmxani;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfmxaniHPP
