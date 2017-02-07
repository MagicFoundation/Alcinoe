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
#include <FMX.Graphics.hpp>
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
typedef void __fastcall (__closure *TALBufferingUpdateNotifyEvent)(System::TObject* const Sender, const int mp);

typedef void __fastcall (__closure *TALVideoSizeChangedNotifyEvent)(System::TObject* const Sender, const int width, const int height);

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
	virtual int __fastcall getCurrentPosition(void) = 0 ;
	virtual int __fastcall getDuration(void) = 0 ;
	virtual int __fastcall getVideoHeight(void) = 0 ;
	virtual int __fastcall getVideoWidth(void) = 0 ;
	virtual bool __fastcall isPlaying(void) = 0 ;
	virtual void __fastcall pause(void) = 0 ;
	virtual void __fastcall prepareAsync(const bool aStartWhenReady = false) = 0 ;
	virtual void __fastcall Start(void) = 0 ;
	virtual void __fastcall Stop(void) = 0 ;
	virtual void __fastcall seekTo(const int msec) = 0 ;
	virtual void __fastcall setDataSource(const System::UnicodeString aDataSource) = 0 ;
	virtual void __fastcall setLooping(const bool looping) = 0 ;
	__property Fmx::Graphics::TBitmap* bitmap = {read=fbitmap};
	__property System::Classes::TNotifyEvent OnError = {read=fOnErrorEvent, write=fOnErrorEvent};
	__property System::Classes::TNotifyEvent OnPrepared = {read=FOnPreparedEvent, write=FOnPreparedEvent};
	__property System::Classes::TNotifyEvent OnFrameAvailable = {read=fOnFrameAvailableEvent, write=fOnFrameAvailableEvent};
	__property TALBufferingUpdateNotifyEvent OnBufferingUpdate = {read=fOnBufferingUpdateEvent, write=fOnBufferingUpdateEvent};
	__property System::Classes::TNotifyEvent OnCompletion = {read=fOnCompletionEvent, write=fOnCompletionEvent};
	__property TALVideoSizeChangedNotifyEvent onVideoSizeChangedEvent = {read=fonVideoSizeChangedEvent, write=fonVideoSizeChangedEvent};
public:
	/* TObject.Create */ inline __fastcall TALWinVideoPlayer(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TALWinVideoPlayer(void) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TALVideoPlayer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TALWinVideoPlayer* fVideoPlayerControl;
	Fmx::Graphics::TBitmap* __fastcall GetBitmap(void);
	TALBufferingUpdateNotifyEvent __fastcall GetOnBufferingUpdateEvent(void);
	System::Classes::TNotifyEvent __fastcall GetOnCompletionEvent(void);
	System::Classes::TNotifyEvent __fastcall GetOnErrorEvent(void);
	System::Classes::TNotifyEvent __fastcall GetOnFrameAvailableEvent(void);
	System::Classes::TNotifyEvent __fastcall GetOnPreparedEvent(void);
	TALVideoSizeChangedNotifyEvent __fastcall GetonVideoSizeChangedEvent(void);
	void __fastcall SetOnBufferingUpdateEvent(const TALBufferingUpdateNotifyEvent Value);
	void __fastcall SetOnCompletionEvent(const System::Classes::TNotifyEvent Value);
	void __fastcall SetOnErrorEvent(const System::Classes::TNotifyEvent Value);
	void __fastcall SetOnFrameAvailableEvent(const System::Classes::TNotifyEvent Value);
	void __fastcall SetOnPreparedEvent(const System::Classes::TNotifyEvent Value);
	void __fastcall SetonVideoSizeChangedEvent(const TALVideoSizeChangedNotifyEvent Value);
	
public:
	__fastcall virtual TALVideoPlayer(void);
	__fastcall virtual ~TALVideoPlayer(void);
	int __fastcall getCurrentPosition(void);
	int __fastcall getDuration(void);
	int __fastcall getVideoHeight(void);
	int __fastcall getVideoWidth(void);
	bool __fastcall isPlaying(void);
	void __fastcall pause(void);
	void __fastcall prepareAsync(const bool aStartWhenReady = false);
	void __fastcall Start(void);
	void __fastcall Stop(void);
	void __fastcall seekTo(const int msec);
	void __fastcall setDataSource(const System::UnicodeString aDataSource);
	void __fastcall setLooping(const bool looping);
	__property Fmx::Graphics::TBitmap* Bitmap = {read=GetBitmap};
	__property System::Classes::TNotifyEvent OnError = {read=GetOnErrorEvent, write=SetOnErrorEvent};
	__property System::Classes::TNotifyEvent OnPrepared = {read=GetOnPreparedEvent, write=SetOnPreparedEvent};
	__property System::Classes::TNotifyEvent OnFrameAvailable = {read=GetOnFrameAvailableEvent, write=SetOnFrameAvailableEvent};
	__property TALBufferingUpdateNotifyEvent OnBufferingUpdate = {read=GetOnBufferingUpdateEvent, write=SetOnBufferingUpdateEvent};
	__property System::Classes::TNotifyEvent OnCompletion = {read=GetOnCompletionEvent, write=SetOnCompletionEvent};
	__property TALVideoSizeChangedNotifyEvent onVideoSizeChangedEvent = {read=GetonVideoSizeChangedEvent, write=SetonVideoSizeChangedEvent};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TALVideoPlayerSurface : public Alfmxobjects::TALRectangle
{
	typedef Alfmxobjects::TALRectangle inherited;
	
private:
	TALVideoPlayer* fVideoPlayer;
	
protected:
	virtual void __fastcall Paint(void);
	
public:
	__fastcall virtual TALVideoPlayerSurface(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TALVideoPlayerSurface(void);
	__property TALVideoPlayer* VideoPlayer = {read=fVideoPlayer, write=fVideoPlayer};
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
