// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALVideoPlayer.pas' rev: 31.00 (Windows)

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
class DELPHICLASS TALVideoPlayerAsync;
class DELPHICLASS TALVideoPlayerSurface;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TALBufferingUpdateNotifyEvent)(System::TObject* const Sender, const int mp);

typedef void __fastcall (__closure *TALVideoSizeChangedNotifyEvent)(System::TObject* const Sender, const int width, const int height);

enum DECLSPEC_DENUM TVideoPlayerState : unsigned char { vpsInitialized, vpsPreparing, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted, vpsIdle, vpsError };

class PASCALIMPLEMENTATION TALWinVideoPlayer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Fmx::Graphics::TBitmap* fbitmap;
	System::Classes::TNotifyEvent fOnFrameAvailableEvent;
	TALBufferingUpdateNotifyEvent fOnBufferingUpdateEvent;
	System::Classes::TNotifyEvent fOnCompletionEvent;
	System::Classes::TNotifyEvent fOnErrorEvent;
	System::Classes::TNotifyEvent FOnPreparedEvent;
	TALVideoSizeChangedNotifyEvent fonVideoSizeChangedEvent;
	
public:
	__fastcall virtual TALWinVideoPlayer(void);
	__fastcall virtual ~TALWinVideoPlayer(void);
	int __fastcall getCurrentPosition(void);
	int __fastcall getDuration(void);
	int __fastcall getVideoHeight(void);
	int __fastcall getVideoWidth(void);
	bool __fastcall isPlaying(void);
	void __fastcall pause(void);
	void __fastcall prepare(void);
	void __fastcall prepareAsync(void);
	void __fastcall Start(void);
	void __fastcall Stop(void);
	void __fastcall seekTo(const int msec);
	void __fastcall setDataSource(const System::UnicodeString aDataSource);
	void __fastcall setLooping(const bool looping);
	void __fastcall setVolume(const float Value);
	__property Fmx::Graphics::TBitmap* bitmap = {read=fbitmap};
	__property System::Classes::TNotifyEvent OnError = {read=fOnErrorEvent, write=fOnErrorEvent};
	__property System::Classes::TNotifyEvent OnPrepared = {read=FOnPreparedEvent, write=FOnPreparedEvent};
	__property System::Classes::TNotifyEvent OnFrameAvailable = {read=fOnFrameAvailableEvent, write=fOnFrameAvailableEvent};
	__property TALBufferingUpdateNotifyEvent OnBufferingUpdate = {read=fOnBufferingUpdateEvent, write=fOnBufferingUpdateEvent};
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
	TALBufferingUpdateNotifyEvent fOnBufferingUpdateEvent;
	System::Classes::TNotifyEvent fOnCompletionEvent;
	TALVideoSizeChangedNotifyEvent fonVideoSizeChangedEvent;
	bool FAutoStartWhenPrepared;
	TVideoPlayerState fState;
	__int64 FTag;
	System::TObject* FTagObject;
	double FTagFloat;
	Fmx::Graphics::TBitmap* __fastcall GetBitmap(void);
	void __fastcall doOnCompletion(System::TObject* Sender);
	void __fastcall doOnError(System::TObject* Sender);
	void __fastcall doOnPrepared(System::TObject* Sender);
	void __fastcall doOnFrameAvailable(System::TObject* Sender);
	void __fastcall doOnBufferingUpdate(System::TObject* const Sender, const int mp);
	void __fastcall doOnVideoSizeChanged(System::TObject* const Sender, const int width, const int height);
	
public:
	__fastcall virtual TALVideoPlayer(void);
	__fastcall virtual ~TALVideoPlayer(void);
	int __fastcall getCurrentPosition(void);
	int __fastcall getDuration(void);
	int __fastcall getVideoHeight(void);
	int __fastcall getVideoWidth(void);
	bool __fastcall isPlaying(void);
	void __fastcall pause(void);
	void __fastcall prepare(const bool aAutoStartWhenPrepared = false);
	void __fastcall prepareAsync(const bool aAutoStartWhenPrepared = false);
	void __fastcall Start(void);
	void __fastcall Stop(void);
	void __fastcall seekTo(const int msec);
	void __fastcall setDataSource(const System::UnicodeString aDataSource);
	void __fastcall setLooping(const bool looping);
	void __fastcall setVolume(const float Value);
	__property Fmx::Graphics::TBitmap* Bitmap = {read=GetBitmap};
	__property System::Classes::TNotifyEvent OnError = {read=fOnErrorEvent, write=fOnErrorEvent};
	__property System::Classes::TNotifyEvent OnPrepared = {read=fOnPreparedEvent, write=fOnPreparedEvent};
	__property System::Classes::TNotifyEvent OnFrameAvailable = {read=fOnFrameAvailableEvent, write=fOnFrameAvailableEvent};
	__property TALBufferingUpdateNotifyEvent OnBufferingUpdate = {read=fOnBufferingUpdateEvent, write=fOnBufferingUpdateEvent};
	__property System::Classes::TNotifyEvent OnCompletion = {read=fOnCompletionEvent, write=fOnCompletionEvent};
	__property TALVideoSizeChangedNotifyEvent onVideoSizeChanged = {read=fonVideoSizeChangedEvent, write=fonVideoSizeChangedEvent};
	__property TVideoPlayerState State = {read=fState, nodefault};
	__property bool AutoStartWhenPrepared = {read=FAutoStartWhenPrepared, write=FAutoStartWhenPrepared, nodefault};
	__property __int64 Tag = {read=FTag, write=FTag, default=0};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	__property double TagFloat = {read=FTagFloat, write=FTagFloat};
};


class PASCALIMPLEMENTATION TALVideoPlayerAsync : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	System::Syncobjs::TEvent* fSignal;
	bool FReady;
	TALWinVideoPlayer* fVideoPlayerControl;
	System::Classes::TNotifyEvent fOnErrorEvent;
	System::Classes::TNotifyEvent fOnPreparedEvent;
	System::Classes::TNotifyEvent fOnFrameAvailableEvent;
	TALBufferingUpdateNotifyEvent fOnBufferingUpdateEvent;
	System::Classes::TNotifyEvent fOnCompletionEvent;
	TALVideoSizeChangedNotifyEvent fonVideoSizeChangedEvent;
	bool FAutoStartWhenPrepared;
	int fState;
	__int64 FTag;
	System::TObject* FTagObject;
	double FTagFloat;
	bool fDoSetDataSource;
	System::UnicodeString fDoSetDataSourceValue;
	bool fDoPrepare;
	bool fDoPause;
	bool fDoStart;
	bool fDoStop;
	bool fDoSetVolume;
	int fDoSetVolumeValue;
	bool fDoSetLooping;
	bool fDoSetLoopingValue;
	bool fDoSeekTo;
	int fDoSeekToValue;
	bool fDoGetDuration;
	System::Syncobjs::TEvent* fDoGetDurationSignal;
	int fDoGetDurationValue;
	Fmx::Graphics::TBitmap* __fastcall GetBitmap(void);
	TVideoPlayerState __fastcall getState(void);
	void __fastcall doOnCompletion(System::TObject* Sender);
	void __fastcall doOnError(System::TObject* Sender);
	void __fastcall doOnPrepared(System::TObject* Sender);
	void __fastcall doOnFrameAvailable(System::TObject* Sender);
	void __fastcall doOnBufferingUpdate(System::TObject* const Sender, const int mp);
	void __fastcall doOnVideoSizeChanged(System::TObject* const Sender, const int width, const int height);
	
public:
	__fastcall virtual TALVideoPlayerAsync(void);
	__fastcall virtual ~TALVideoPlayerAsync(void);
	virtual void __fastcall Execute(void);
	int __fastcall getDuration(void);
	void __fastcall pause(void);
	void __fastcall prepare(const bool aAutoStartWhenPrepared = false);
	HIDESBASE void __fastcall Start(void);
	void __fastcall Stop(void);
	void __fastcall seekTo(const int msec);
	void __fastcall setDataSource(const System::UnicodeString aDataSource);
	void __fastcall setLooping(const bool looping);
	void __fastcall setVolume(const float Value);
	__property Fmx::Graphics::TBitmap* Bitmap = {read=GetBitmap};
	__property System::Classes::TNotifyEvent OnError = {read=fOnErrorEvent, write=fOnErrorEvent};
	__property System::Classes::TNotifyEvent OnPrepared = {read=fOnPreparedEvent, write=fOnPreparedEvent};
	__property System::Classes::TNotifyEvent OnFrameAvailable = {read=fOnFrameAvailableEvent, write=fOnFrameAvailableEvent};
	__property TALBufferingUpdateNotifyEvent OnBufferingUpdate = {read=fOnBufferingUpdateEvent, write=fOnBufferingUpdateEvent};
	__property System::Classes::TNotifyEvent OnCompletion = {read=fOnCompletionEvent, write=fOnCompletionEvent};
	__property TALVideoSizeChangedNotifyEvent onVideoSizeChanged = {read=fonVideoSizeChangedEvent, write=fonVideoSizeChangedEvent};
	__property TVideoPlayerState State = {read=getState, nodefault};
	__property bool AutoStartWhenPrepared = {read=FAutoStartWhenPrepared, write=FAutoStartWhenPrepared, nodefault};
	__property __int64 Tag = {read=FTag, write=FTag, default=0};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	__property double TagFloat = {read=FTagFloat, write=FTagFloat};
};


class PASCALIMPLEMENTATION TALVideoPlayerSurface : public Alfmxobjects::TALRectangle
{
	typedef Alfmxobjects::TALRectangle inherited;
	
private:
	TALVideoPlayer* fVideoPlayer;
	void __fastcall OnFrameAvailable(System::TObject* Sender);
	
protected:
	virtual void __fastcall Paint(void);
	
public:
	__fastcall virtual TALVideoPlayerSurface(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALVideoPlayerSurface(void);
	void __fastcall resetVideoPlayer(void);
	__property TALVideoPlayer* VideoPlayer = {read=fVideoPlayer};
};


//-- var, const, procedure ---------------------------------------------------
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
