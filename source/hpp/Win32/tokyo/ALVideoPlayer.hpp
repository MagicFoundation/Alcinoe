// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALVideoPlayer.pas' rev: 32.00 (Windows)

#ifndef AlvideoplayerHPP
#define AlvideoplayerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SyncObjs.hpp>
#include <FMX.Graphics.hpp>
#include <ALFmxCommon.hpp>
#include <ALFmxObjects.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alvideoplayer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TALWinVideoPlayer;
class DELPHICLASS TALVideoPlayer;
class DELPHICLASS TALVideoPlayerSurface;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TALVideoSizeChangedNotifyEvent)(System::TObject* const Sender, const int width, const int height);

class PASCALIMPLEMENTATION TALWinVideoPlayer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Fmx::Graphics::TBitmap* fbitmap;
	System::Classes::TNotifyEvent fOnFrameAvailableEvent;
	System::Classes::TNotifyEvent fOnCompletionEvent;
	System::Classes::TNotifyEvent fOnErrorEvent;
	System::Classes::TNotifyEvent FOnPreparedEvent;
	TALVideoSizeChangedNotifyEvent fonVideoSizeChangedEvent;
	
protected:
	int __fastcall getState(void);
	
public:
	__fastcall virtual TALWinVideoPlayer(void);
	__fastcall virtual ~TALWinVideoPlayer(void);
	__int64 __fastcall getCurrentPosition(void);
	__int64 __fastcall getDuration(void);
	int __fastcall getVideoHeight(void);
	int __fastcall getVideoWidth(void);
	bool __fastcall isPlaying(void);
	void __fastcall pause(void);
	void __fastcall prepare(const System::UnicodeString aDataSource);
	void __fastcall Start(void);
	void __fastcall Stop(void);
	void __fastcall seekTo(const __int64 msec);
	void __fastcall setLooping(const bool looping);
	void __fastcall setVolume(const float Value);
	void __fastcall setPlaybackSpeed(const float Value);
	__property Fmx::Graphics::TBitmap* bitmap = {read=fbitmap};
	__property System::Classes::TNotifyEvent OnError = {read=fOnErrorEvent, write=fOnErrorEvent};
	__property System::Classes::TNotifyEvent OnPrepared = {read=FOnPreparedEvent, write=FOnPreparedEvent};
	__property System::Classes::TNotifyEvent OnFrameAvailable = {read=fOnFrameAvailableEvent, write=fOnFrameAvailableEvent};
	__property System::Classes::TNotifyEvent OnCompletion = {read=fOnCompletionEvent, write=fOnCompletionEvent};
	__property TALVideoSizeChangedNotifyEvent onVideoSizeChanged = {read=fonVideoSizeChangedEvent, write=fonVideoSizeChangedEvent};
};


class PASCALIMPLEMENTATION TALVideoPlayer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TALWinVideoPlayer* fVideoPlayerControl;
	System::Classes::TNotifyEvent fOnErrorEvent;
	System::Classes::TNotifyEvent fOnPreparedEvent;
	System::Classes::TNotifyEvent fOnFrameAvailableEvent;
	System::Classes::TNotifyEvent fOnCompletionEvent;
	TALVideoSizeChangedNotifyEvent fonVideoSizeChangedEvent;
	bool FAutoStartWhenPrepared;
	__int64 FTag;
	System::TObject* FTagObject;
	double FTagFloat;
	System::UnicodeString FTagString;
	Fmx::Graphics::TBitmap* __fastcall GetBitmap(void);
	void __fastcall doOnCompletion(System::TObject* Sender);
	void __fastcall doOnError(System::TObject* Sender);
	void __fastcall doOnPrepared(System::TObject* Sender);
	void __fastcall doOnFrameAvailable(System::TObject* Sender);
	void __fastcall doOnVideoSizeChanged(System::TObject* const Sender, const int width, const int height);
	
public:
	__fastcall virtual TALVideoPlayer(void);
	__fastcall virtual ~TALVideoPlayer(void);
	__int64 __fastcall getCurrentPosition(void);
	__int64 __fastcall getDuration(void);
	int __fastcall getVideoHeight(void);
	int __fastcall getVideoWidth(void);
	int __fastcall getState(void);
	bool __fastcall isPlaying(void);
	void __fastcall pause(void);
	void __fastcall prepare(const System::UnicodeString aDataSource, const bool aAutoStartWhenPrepared = false);
	void __fastcall Start(void);
	void __fastcall Stop(void);
	void __fastcall seekTo(const __int64 msec);
	void __fastcall setLooping(const bool looping);
	void __fastcall setVolume(const float Value);
	void __fastcall setPlaybackSpeed(const float Value);
	__property Fmx::Graphics::TBitmap* Bitmap = {read=GetBitmap};
	__property System::Classes::TNotifyEvent OnError = {read=fOnErrorEvent, write=fOnErrorEvent};
	__property System::Classes::TNotifyEvent OnPrepared = {read=fOnPreparedEvent, write=fOnPreparedEvent};
	__property System::Classes::TNotifyEvent OnFrameAvailable = {read=fOnFrameAvailableEvent, write=fOnFrameAvailableEvent};
	__property System::Classes::TNotifyEvent OnCompletion = {read=fOnCompletionEvent, write=fOnCompletionEvent};
	__property TALVideoSizeChangedNotifyEvent onVideoSizeChanged = {read=fonVideoSizeChangedEvent, write=fonVideoSizeChangedEvent};
	__property int State = {read=getState, nodefault};
	__property bool AutoStartWhenPrepared = {read=FAutoStartWhenPrepared, write=FAutoStartWhenPrepared, nodefault};
	__property __int64 Tag = {read=FTag, write=FTag, default=0};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	__property double TagFloat = {read=FTagFloat, write=FTagFloat};
	__property System::UnicodeString TagString = {read=FTagString, write=FTagString};
};


class PASCALIMPLEMENTATION TALVideoPlayerSurface : public Alfmxobjects::TALRectangle
{
	typedef Alfmxobjects::TALRectangle inherited;
	
private:
	TALVideoPlayer* fVideoPlayer;
	void __fastcall OnFrameAvailable(System::TObject* Sender);
	TALVideoSizeChangedNotifyEvent __fastcall GetVideoSizeChangedEvent(void);
	void __fastcall SetVideoSizeChangedEvent(const TALVideoSizeChangedNotifyEvent Value);
	
protected:
	virtual void __fastcall Paint(void);
	
public:
	__fastcall virtual TALVideoPlayerSurface(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALVideoPlayerSurface(void);
	void __fastcall resetVideoPlayer(void);
	__property TALVideoPlayer* VideoPlayer = {read=fVideoPlayer};
	
__published:
	__property TALVideoSizeChangedNotifyEvent onVideoSizeChanged = {read=GetVideoSizeChangedEvent, write=SetVideoSizeChangedEvent};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 vpsIdle = System::Int8(0x0);
static const System::Int8 vpsPreparing = System::Int8(0x1);
static const System::Int8 vpsPrepared = System::Int8(0x2);
static const System::Int8 vpsStarted = System::Int8(0x3);
static const System::Int8 vpsPaused = System::Int8(0x4);
static const System::Int8 vpsStopped = System::Int8(0x5);
static const System::Int8 vpsPlaybackCompleted = System::Int8(0x6);
static const System::Int8 vpsError = System::Int8(0x7);
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Alvideoplayer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALVIDEOPLAYER)
using namespace Alvideoplayer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlvideoplayerHPP
