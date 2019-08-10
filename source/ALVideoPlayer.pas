unit ALVideoPlayer;

interface

//
// ALVideoPlayer will render a video on a TEXTURE. this is really important because you can fully integrate the video in
// the delphi form and you can place any controls you want on the top of it as it's support Z-ORDER. Official delphi video player
// are just native video player window on the top of the form and thus not supporting Z-ORDER.
//
// Under android I use ExoPlayer. ExoPlayer is an open source project that is not part of the Android framework and is distributed
// separately from the Android SDK. ExoPlayer’s standard audio and video components are built on Android’s MediaCodec API, which
// was released in Android 4.1 (API level 16). Because ExoPlayer is a library, you can easily take advantage of new features as
// they become available by updating your app. ExoPlayer supports features like Dynamic adaptive streaming over HTTP (DASH), HLS,
// SmoothStreaming and Common Encryption, which are not supported by MediaPlayer. It's designed to be easy to customize and extend.
//
// Under Ios i use AVPlayer with support also HLS like exoplayer do.
//
// Notice: I stop to support delphi berlin with have 2 UI threads on android, it's too hard to manage and debug the both compiler
//
//
// INSTALLATION
// 1) on android you will need to patch the RTL because it's not support GL_TEXTURE_EXTERNAL_OES https://quality.embarcadero.com/browse/RSP-16830
//    see sample of the patched library in demos\alfmxcontrols
// 2) on android we use exoplayer so you must add the exoplayer libraries (lib\jar\exoplayer\exoplayer-core.jar and if you need HLS support lib\jar\exoplayer\exoplayer-hls.jar)
//    in project manager > target platforms > Android > libraries
// 3) after it's must be quite easy, just read the source code or look the sample located at demos\alfmxcontrols
//

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE _MACOS}
{$IFEND}

uses system.Classes,
     System.SyncObjs,
     {$IF defined(DEBUG)}
     system.diagnostics,
     {$endIF}
     {$IF defined(ANDROID)}
     Androidapi.JNI.Os,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.JNI.JavaTypes,
     ALAndroidExoPlayerApi,
     AlFmxTypes3D,
     {$endIF}
     {$IF defined(IOS)}
     Macapi.ObjectiveC,
     iOSapi.Foundation,
     iOSapi.AVFoundation,
     iOSapi.CoreVideo,
     Fmx.types,
     AlFmxTypes3D,
     {$endIF}
     Fmx.graphics,
     ALFmxCommon,
     AlFmxObjects;

type

  TALVideoSizeChangedNotifyEvent = procedure(const Sender: TObject; const width: Integer; const height: Integer) of object;

const

  //VideoPlayerState
  vpsIdle = 0; // << When a MediaPlayer object is just created
  vpsPreparing = 1;
  vpsPrepared = 2;
  vpsStarted = 3;
  vpsPaused = 4;
  vpsStopped = 5;
  vpsPlaybackCompleted = 6;
  vpsError = 7;

type

  {*****************}
  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  TALAndroidVideoPlayer = class(Tobject)
  private

    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALFrameAvailableListener = class(TJavaLocal, JSurfaceTexture_OnFrameAvailableListener)
      private
        {$IF defined(DEBUG)}
        fTotalFramesProcessed: integer;
        fFpsStopWatch: TstopWatch;
        {$ENDIF}
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
        fSdkInt: integer;
        procedure DoOnFrameAvailable(surfaceTexture: JSurfaceTexture);
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onFrameAvailable(surfaceTexture: JSurfaceTexture); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALEventListener = class(TJavaLocal, JPlayer_EventListener)
      private
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onTimelineChanged(timeline: JTimeline; manifest: JObject); cdecl;
        procedure onTracksChanged(trackGroups: JTrackGroupArray; trackSelections: JTrackSelectionArray); cdecl;
        procedure onLoadingChanged(isLoading: Boolean); cdecl;
        procedure onPlayerStateChanged(playWhenReady: Boolean; playbackState: Integer); cdecl;
        procedure onRepeatModeChanged(repeatMode: Integer); cdecl;
        procedure onShuffleModeEnabledChanged(shuffleModeEnabled: Boolean); cdecl;
        procedure onPlayerError(error: JExoPlaybackException); cdecl;
        procedure onPositionDiscontinuity(reason: Integer); cdecl;
        procedure onPlaybackParametersChanged(playbackParameters: JPlaybackParameters); cdecl;
        procedure onSeekProcessed; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALVideoListener = class(TJavaLocal, JSimpleExoPlayer_VideoListener)
      private
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onVideoSizeChanged(width: Integer; height: Integer; unappliedRotationDegrees: Integer; pixelWidthHeightRatio: Single); cdecl;
        procedure onRenderedFirstFrame; cdecl;
      end;

  private
    class var DefaultBandwidthMeter: JBandwidthMeter;
  private
    fHandler: JHandler;
    fVideoTrackSelectionFactory: JTrackSelection_Factory;
    fTrackSelector: JTrackSelector;
    fSimpleExoPlayer: JSimpleExoPlayer;
    fDataSourceFactory: JDataSource_Factory;
    fSurface: JSurface;
    fSurfaceTexture: JSurfaceTexture;
    fbitmap: TALTexture;
    fOnFrameAvailableEvent: TNotifyEvent;
    FOnFrameAvailableListener: TALFrameAvailableListener;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    fEventListener: TALEventListener;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    fVideoListener: TALVideoListener;
    fVideoWidth: integer;
    fVideoHeight: integer;
    fState: integer;
  protected
    function getState: integer;
    function setState(const aValue: integer): boolean; overload;
    function setState(const aValue: integer; const aCompareAnd: integer): boolean; overload;
    function setState(const aValue: integer; const aCompareAnd: array of integer): boolean; overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: int64;
    function getDuration: int64;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare(Const aDataSource: String);
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Int64);
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    procedure setPlaybackSpeed(const Value: single);
    property bitmap: TalTexture read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
  end;
  {$endIF}
  {$ENDREGION}

  {*************}
  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TALIOSVideoPlayer = class(Tobject)
  private

    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      IALKVODelegate = interface(IObjectiveC)
        ['{B0C6720A-79B7-4FD6-B9E3-77594E905B3C}']
        procedure observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALKVODelegate = class(TOCLocal, IALKVODelegate)
      private
        [Weak] FVideoPlayerControl: TALIOSVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALIOSVideoPlayer);
        procedure observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      IALNotificationsDelegate = interface(IObjectiveC)
        ['{EF778992-B249-474F-B1AD-D64903EF7947}']
        procedure ItemDidPlayToEndTime; cdecl;
        procedure ItemFailedToPlayToEndTime; cdecl;
        procedure ItemTimeJumped; cdecl;
        procedure ItemPlaybackStalled; cdecl;
        procedure ItemNewAccessLogEntry; cdecl;
        procedure ItemNewErrorLogEntry; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALNotificationsDelegate = class(TOCLocal, IALNotificationsDelegate)
      private
        [Weak] FVideoPlayerControl: TALIOSVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALIOSVideoPlayer);
        procedure ItemDidPlayToEndTime; cdecl;
        procedure ItemFailedToPlayToEndTime; cdecl;
        procedure ItemTimeJumped; cdecl;
        procedure ItemPlaybackStalled; cdecl;
        procedure ItemNewAccessLogEntry; cdecl;
        procedure ItemNewErrorLogEntry; cdecl;
      end;

  private
    class var appAudioSessionActivated: Boolean;
  private
    FPlayer: AVPlayer;
    FPlayerItem: AVPlayerItem;
    FPlayerItemVideoOutput: AVPlayerItemVideoOutput;
    FKVODelegate: TALKVODelegate;
    fNotificationsDelegate: TALNotificationsDelegate;
    FFrameRefreshTimer: TTimer;
    fhasNONewPixelBufferForItemTimeCounter: integer;
    {$IFDEF DEBUG}
    fFrameRefreshCounter: integer;
    {$ENDIF}
    fbitmap: TALTexture;
    fTextureRef: CVOpenGLESTextureRef;
    fvideoTextureCacheRef: CVOpenGLESTextureCacheRef;
    fState: Integer;
    fLooping: boolean;
    fVolume: single;
    fPrepareThread: TThread;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
  protected
    function getState: integer;
    function setState(const aValue: integer): boolean; overload;
    function setState(const aValue: integer; const aCompareAnd: integer): boolean; overload;
    function setState(const aValue: integer; const aCompareAnd: array of integer): boolean; overload;
    procedure doOnFrameRefresh(Sender: TObject);
    procedure doOnReady;
    procedure doOnItemDidPlayToEndTime;
    procedure doOnItemFailedToPlayToEndTime;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: int64;
    function getDuration: int64;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare(Const aDataSource: String);
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Int64);
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    procedure setPlaybackSpeed(const Value: single);
    property bitmap: TalTexture read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
  end;
  {$endIF}
  {$ENDREGION}

  {*******************}
  {$REGION 'MSWINDOWS'}
  {$IF defined(MSWINDOWS)}
  TALWinVideoPlayer = class(Tobject)
  private
    fbitmap: TBitmap;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
  protected
    function getState: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: int64;
    function getDuration: int64;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare(Const aDataSource: String);
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Int64);
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    procedure setPlaybackSpeed(const Value: single);
    property bitmap: TBitmap read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
  end;
  {$endIF}
  {$ENDREGION}

  {****************}
  {$REGION '_MACOS'}
  {$IF defined(_MACOS)}
  TALMacOSVideoPlayer = class(Tobject)
  private
    fbitmap: TBitmap;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
  protected
    function getState: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: int64;
    function getDuration: int64;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare(Const aDataSource: String);
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Int64);
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    property bitmap: TBitmap read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
  end;
  {$endIF}
  {$ENDREGION}

type

  {*****************************}
  TALVideoPlayer = class(TObject)
  private
    {$IF defined(android)}
    fVideoPlayerControl: TALAndroidVideoPlayer;
    {$ELSEIF defined(IOS)}
    fVideoPlayerControl: TALIOSVideoPlayer;
    {$ELSEIF defined(MSWINDOWS)}
    fVideoPlayerControl: TALWinVideoPlayer;
    {$ELSEIF defined(_MACOS)}
    fVideoPlayerControl: TALMacOSVideoPlayer;
    {$ENDIF}
    //-----
    fOnErrorEvent: TNotifyEvent;
    fOnPreparedEvent: TNotifyEvent;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    //-----
    FAutoStartWhenPrepared: Boolean;
    //-----
    FTag: int64;
    [Weak] FTagObject: TObject;
    FTagFloat: Double;
    FTagString: String;
    //-----
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function GetBitmap: TalTexture;
    {$ELSE}
    function GetBitmap: TBitmap;
    {$ENDIF}
    procedure doOnCompletion(Sender: TObject);
    procedure doOnError(Sender: TObject);
    procedure doOnPrepared(Sender: TObject);
    procedure doOnFrameAvailable(Sender: TObject);
    procedure doOnVideoSizeChanged(const Sender: TObject; const width: Integer; const height: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: int64;
    function getDuration: int64;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function getState: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare(Const aDataSource: String; const aAutoStartWhenPrepared: Boolean=False);
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Int64);
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    procedure setPlaybackSpeed(const Value: single);
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property Bitmap: TALTexture read Getbitmap;
    {$ELSE}
    property Bitmap: TBitmap read Getbitmap;
    {$ENDIF}
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
    property State: Integer read GetState;
    property AutoStartWhenPrepared: boolean read FAutoStartWhenPrepared write FAutoStartWhenPrepared;
    property Tag: int64 read FTag write FTag default 0;
    property TagObject: TObject read FTagObject write FTagObject;
    property TagFloat: Double read FTagFloat write FTagFloat;
    property TagString: String read FTagString write FTagString;
  end;

  {*****************************************}
  TALVideoPlayerSurface = class(TALRectangle)
  private
    fVideoPlayer: TALVideoPlayer;
    procedure OnFrameAvailable(Sender: Tobject);
    function GetVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    procedure SetVideoSizeChangedEvent(const Value: TALVideoSizeChangedNotifyEvent);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure resetVideoPlayer;
    property VideoPlayer: TALVideoPlayer read fVideoPlayer;
  published
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read GetVideoSizeChangedEvent write SetVideoSizeChangedEvent;
  end;

procedure register;

implementation

uses system.SysUtils,
     system.Types,
     {$IF defined(ANDROID)}
     system.Math,
     Androidapi.Helpers,
     androidapi.jni.net,
     FMX.Canvas.GPU,
     AlString,
     ALGraphics,
     {$ENDIF}
     {$IF defined(IOS)}
     System.RTLConsts,
     Macapi.CoreFoundation,
     iOSapi.CoreMedia,
     iOSapi.OpenGLES,
     Macapi.Helpers,
     Macapi.ObjCRuntime,
     FMX.Canvas.GPU,
     FMX.Context.GLES.iOS,
     FMX.Types3D,
     AlString,
     ALGraphics,
     {$ENDIF}
     fmx.controls,
     AlCommon;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}

{*******************************************************************************************************************}
constructor TALAndroidVideoPlayer.TALFrameAvailableListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  {$IF defined(DEBUG)}
  fTotalFramesProcessed := 0;
  fFpsStopWatch := TStopWatch.StartNew;
  {$ENDIF}
  fVideoPlayerControl := aVideoPlayerControl;
  fSdkInt := TJBuild_VERSION.JavaClass.SDK_INT;
end;

{************************************************************************************************************}
procedure TALAndroidVideoPlayer.TALFrameAvailableListener.DoOnFrameAvailable(surfaceTexture: JSurfaceTexture);
{$IF defined(DEBUG)}
var aStopWatch: TStopWatch;
{$ENDIF}
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerControl = nil then exit;

  {$IF defined(DEBUG)}
  if (fTotalFramesProcessed mod 1000 = 0) then begin
    fFpsStopWatch.Stop;
    ALLog('TALAndroidVideoPlayer.onFrameAvailable', 'fps: ' + ALFormatFloatU('0.00', (fTotalFramesProcessed) / max(1, (ffpsStopWatch.Elapsed.TotalMilliseconds / 1000)), AlDefaultFormatSettingsU) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
    fTotalFramesProcessed := 0;
    fFpsStopWatch := TStopWatch.StartNew;
  end;
  inc(fTotalFramesProcessed);
  {$ENDIF}

  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.SetSize is still the same and adjust the IFDEF'}
  {$ENDIF}
  if (fVideoPlayerControl.fbitmap.Width <> fVideoPlayerControl.fVideoWidth) or
     (fVideoPlayerControl.fbitmap.Height <> fVideoPlayerControl.fVideoHeight) then begin
    TALTextureAccessPrivate(fVideoPlayerControl.fBitmap).FWidth := fVideoPlayerControl.fVideoWidth;
    TALTextureAccessPrivate(fVideoPlayerControl.fBitmap).FHeight := fVideoPlayerControl.fVideoHeight; // we can't use setsize because it's will finalise the texture
                                                                                                      // but with/height are used only in
                                                                                                      // procedure TCanvasHelper.TexRect(const DestCorners, SrcCorners: TCornersF; const Texture: TTexture; const Color1, Color2, Color3, Color4: TAlphaColor);
                                                                                                      // begin
                                                                                                      //   ...
                                                                                                      //   if (Texture = nil) or (Texture.Width < 1) or (Texture.Height < 1) then Exit
                                                                                                      //   ...
                                                                                                      //   InvTexSize := PointF(1 / Texture.Width, 1 / Texture.Height);
                                                                                                      //   ...
                                                                                                      // end
                                                                                                      // so i don't need to finalize the texture !!
  end;

  {$IF defined(DEBUG)}
  if (fTotalFramesProcessed mod 1000 = 0) then aStopWatch := TStopWatch.StartNew;
  {$ENDIF}

  fVideoPlayerControl.fSurfaceTexture.updateTexImage;

  {$IFDEF DEBUG}
  if (fTotalFramesProcessed mod 1000 = 0) then begin
    aStopWatch.Stop;
    ALLog('TALAndroidVideoPlayer.onFrameAvailable.updateTexImage', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  end;
  {$ENDIF}

  if assigned(fVideoPlayerControl.fOnFrameAvailableEvent) then
    fVideoPlayerControl.fOnFrameAvailableEvent(fVideoPlayerControl);

end;

{**********************************************************************************************************}
procedure TALAndroidVideoPlayer.TALFrameAvailableListener.onFrameAvailable(surfaceTexture: JSurfaceTexture);
begin

  // https://developer.android.com/reference/android/graphics/SurfaceTexture.html
  // SurfaceTexture objects may be created on any thread. updateTexImage() may only be called on the
  // thread with the OpenGL ES context that contains the texture object. The frame-available callback
  // is called on an arbitrary thread, so unless special care is taken updateTexImage() should not be
  // called directly from the callback.
  // so i as understand i can call updateTexImage in other thread than the current thread, it's
  // seam to be thread safe - i already make opengl multithread however the updateTexImage seam
  // to take around 1ms only so their is no really purpose to run it in a different thread than
  // the main thread (who already have the OpenGL ES context)
  //
  // NOTE: as i do setOnFrameAvailableListener(SurfaceTexture.OnFrameAvailableListener listener,Handler handler)
  //       with handler = TJHandler.JavaClass.init(TJLooper.javaclass.getMainLooper()) then this event will be
  //       always called from the main UI thread

  if fSdkInt >= 21 {LOLLIPOP} then DoOnFrameAvailable(surfaceTexture)
  else begin
    TThread.Synchronize(nil,
      Procedure
      begin
        DoOnFrameAvailable(surfaceTexture);
      end);
  end;

end;

{**********************************************************************************************************}
constructor TALAndroidVideoPlayer.TALEventListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onTimelineChanged(timeline: JTimeline; manifest: JObject);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onTimelineChanged','ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{*************************************************************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onTracksChanged(trackGroups: JTrackGroupArray; trackSelections: JTrackSelectionArray);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onTracksChanged','ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onLoadingChanged(isLoading: Boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onLoadingChanged','isLoading: ' + ALBoolToStrU(isLoading) +
                                                 ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onPlayerStateChanged(playWhenReady: Boolean; playbackState: Integer);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerControl = nil then exit;

  //int STATE_IDLE = 1;  => The player does not have any media to play.
  //int STATE_BUFFERING = 2; => The player is not able to immediately play from its current position. This state typically occurs when more data needs to be loaded.
  //int STATE_READY = 3; => The player is able to immediately play from its current position. The player will be playing if getPlayWhenReady() is true, and paused otherwise.
  //int STATE_ENDED = 4; => The player has finished playing the media.

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onPlayerStateChanged','playWhenReady: ' + ALBoolToStrU(playWhenReady) +
                                                     ' - playbackState: ' + ALIntToStrU(playbackState) +
                                                     ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if (playbackState = TJPlayer.JavaClass.STATE_READY) and
     (fVideoPlayerControl.SetState(vpsPrepared, vpsPreparing)) and
     (assigned(fVideoPlayerControl.fOnPreparedEvent)) then fVideoPlayerControl.fOnPreparedEvent(fVideoPlayerControl);

  if (playbackState = TJPlayer.JavaClass.STATE_ENDED) and
     (fVideoPlayerControl.SetState(vpsPlaybackCompleted, vpsStarted)) and
      assigned(fVideoPlayerControl.fOnCompletionEvent) then fVideoPlayerControl.fOnCompletionEvent(fVideoPlayerControl);

end;

{****************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onRepeatModeChanged(repeatMode: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onRepeatModeChanged','repeatMode: ' + ALIntToStrU(repeatMode) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{********************************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onShuffleModeEnabledChanged(shuffleModeEnabled: Boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onShuffleModeEnabledChanged','shuffleModeEnabled: ' + ALBoolToStrU(shuffleModeEnabled) +
                                                            ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onPlayerError(error: JExoPlaybackException);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerControl = nil then exit;

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onPlayerError', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}

  if fVideoPlayerControl.SetState(vpsError) and
     assigned(fVideoPlayerControl.fOnErrorEvent) then fVideoPlayerControl.fOnErrorEvent(fVideoPlayerControl);

end;

{****************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onPositionDiscontinuity(reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onPositionDiscontinuity','reason: ' + ALIntToStrU(reason) +
                                                        ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onPlaybackParametersChanged(playbackParameters: JPlaybackParameters);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onPlaybackParametersChanged', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{***************************************************************}
procedure TALAndroidVideoPlayer.TALEventListener.onSeekProcessed;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onSeekProcessed', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{**********************************************************************************************************}
constructor TALAndroidVideoPlayer.TALVideoListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{*********************************************************************************************************************************************************************}
procedure TALAndroidVideoPlayer.TALVideoListener.onVideoSizeChanged(width: Integer; height: Integer; unappliedRotationDegrees: Integer; pixelWidthHeightRatio: Single);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerControl = nil then exit;

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onVideoSizeChanged', 'width: ' + ALinttostrU(width) +
                                                    ' - height: ' + ALinttostrU(height) +
                                                    ' - unappliedRotationDegrees: ' + ALinttostrU(unappliedRotationDegrees) +
                                                    ' - pixelWidthHeightRatio: ' + AlFloatToStrU(pixelWidthHeightRatio, ALDefaultFormatSettingsU) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  fVideoPlayerControl.FVideoWidth := width;
  fVideoPlayerControl.fVideoHeight := height;
  if assigned(fVideoPlayerControl.fonVideoSizeChangedEvent) then
    fVideoPlayerControl.fonVideoSizeChangedEvent(fVideoPlayerControl, width, height);

end;

{********************************************************************}
procedure TALAndroidVideoPlayer.TALVideoListener.onRenderedFirstFrame;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onRenderedFirstFrame','ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{***************************************}
constructor TALAndroidVideoPlayer.Create;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //-----
  inherited create;

  //-----
  fVideoWidth := 0;
  fVideoHeight := 0;

  //-----
  fState := vpsIdle;

  //----
  if DefaultBandwidthMeter = nil then DefaultBandwidthMeter := TJDefaultBandwidthMeter.JavaClass.init;
  //-----
  fHandler := TJHandler.JavaClass.init(TJLooper.javaclass.getMainLooper());
  //-----
  fvideoTrackSelectionFactory := TJAdaptiveTrackSelection_Factory.JavaClass.init(DefaultBandwidthMeter);
  fTrackSelector := TJDefaultTrackSelector.JavaClass.init(fvideoTrackSelectionFactory);
  fSimpleExoPlayer := TJExoPlayerFactory.JavaClass.newSimpleInstance(TAndroidHelper.Context, fTrackSelector);
  //-----
  fonVideoSizeChangedEvent := nil;
  fVideoListener := TALVideoListener.Create(Self);
  fSimpleExoPlayer.addVideoListener(fVideoListener);
  //----
  fDataSourceFactory := TJDefaultDataSourceFactory.javaclass.init(TAndroidHelper.Context,
                                                                  TJUtil.javaclass.getUserAgent(TAndroidHelper.Context, TandroidHelper.Context.getPackageName),
                                                                  TJTransferListener.Wrap(DefaultBandwidthMeter));
  //-----
  fOnErrorEvent := nil;
  fOnPreparedEvent := nil;
  fOnCompletionEvent := nil;
  fEventListener := TALEventListener.Create(Self);
  fSimpleExoPlayer.AddListener(fEventListener);
  //-----
  fBitmap := TalTexture.Create;
  ALInitializeEXTERNALOESTexture(fBitmap);
  fSurfaceTexture := TJSurfaceTexture.JavaClass.init(fBitmap.Handle);
  //-----
  fOnFrameAvailableEvent := nil;
  FOnFrameAvailableListener := TALFrameAvailableListener.Create(Self);
  if (TJBuild_VERSION.JavaClass.SDK_INT >= 21 {LOLLIPOP}) then fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener, fHandler)
  else fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener);
  //-----
  fSurface := TJSurface.JavaClass.init(fSurfaceTexture);
  fSimpleExoPlayer.setVideoSurface(fSurface);

  //----
  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.Create', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;


{***************************************}
destructor TALAndroidVideoPlayer.Destroy;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //-----
  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found?noredirect=1#comment84156414_48577801
  fOnFrameAvailableListener.FVideoPlayerControl := nil;
  fEventListener.FVideoPlayerControl := nil;
  fVideoListener.FVideoPlayerControl := nil;
  fSurfaceTexture.setOnFrameAvailableListener(nil);
  fSimpleExoPlayer.removeVideoListener(fVideoListener);
  fSimpleExoPlayer.removeListener(fEventListener);

  //-----
  fSimpleExoPlayer.stop;
  fSimpleExoPlayer.clearVideoSurface;
  fSimpleExoPlayer.release;
  fSimpleExoPlayer := nil;
  fTrackSelector := nil;
  fVideoTrackSelectionFactory := nil;
  fDataSourceFactory := nil;

  //----
  fSurface.release;
  fSurface := nil;
  fSurfaceTexture.release;
  fSurfaceTexture := nil;
  alFreeandNil(fbitmap);

  //-----
  alfreeAndNil(fOnFrameAvailableListener);
  alFreeAndNil(fEventListener);
  alFreeAndNil(fVideoListener);

  //-----
  fHandler := nil;

  //-----
  inherited;

  //----
  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.Destroy', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*******************************************************}
function TALAndroidVideoPlayer.getCurrentPosition: int64;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  Result := FSimpleExoPlayer.getCurrentPosition; // Returns the playback position in the current window, in milliseconds.
end;

{************************************************}
function TALAndroidVideoPlayer.getDuration: int64;
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  Result := FSimpleExoPlayer.getDuration; // Returns the duration of the current window in milliseconds, or TIME_UNSET if the
                                          // duration is not known.
end;

{*****************************************************}
function TALAndroidVideoPlayer.getVideoHeight: integer;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  Result := fVideoHeight;
end;

{****************************************************}
function TALAndroidVideoPlayer.getVideoWidth: integer;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  Result := fVideoWidth;
end;

{************************************************}
function TALAndroidVideoPlayer.isPlaying: boolean;
begin
  result := GetState = vpsStarted;
end;

{*****************************************************************}
procedure TALAndroidVideoPlayer.prepare(Const aDataSource: String);
var aVideoSource: JMediaSource;
    aExtractorMediaSourceFactory: JExtractorMediaSource_Factory;
    aHlsMediaSourceFactory: JHlsMediaSource_Factory;
    {$IFDEF DEBUG}
    aStopWatch: TstopWatch;
    {$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //----
  if not SetState(vpsPreparing, vpsIdle) then raise Exception.Create('prepare can be call only in the idle state');

  //HLS datasource
  if alSameTextU(ExtractFileExt(aDataSource), '.m3u8') then begin
    aHlsMediaSourceFactory := TJHlsMediaSource_Factory.JavaClass.init(fDataSourceFactory);
    aVideoSource := aHlsMediaSourceFactory.createMediaSource(StrToJURI(aDataSource));
    aHlsMediaSourceFactory := nil;
  end

  //Other datasource
  else begin
    aExtractorMediaSourceFactory := TJExtractorMediaSource_Factory.JavaClass.init(fDataSourceFactory);
    aVideoSource := aExtractorMediaSourceFactory.createMediaSource(StrToJURI(aDataSource));
    aExtractorMediaSourceFactory := nil;
  end;

  //-----
  fSimpleExoPlayer.prepare(aVideoSource);

  //----
  aVideoSource := nil;

  //----
  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.prepare', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;


{************************************}
procedure TALAndroidVideoPlayer.pause;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsPaused, vpsStarted) then exit;
  FSimpleExoPlayer.setPlayWhenReady(false); // Sets whether playback should proceed when getPlaybackState() == STATE_READY.
                                            // If the player is already in the ready state then this method can be used to pause and resume
                                            // playback

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.pause', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{************************************}
procedure TALAndroidVideoPlayer.start;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsStarted, [vpsPrepared, vpsPaused, vpsPlaybackCompleted]) then exit;
  FSimpleExoPlayer.setPlayWhenReady(true); // Sets whether playback should proceed when getPlaybackState() == STATE_READY.
                                           // If the player is already in the ready state then this method can be used to pause and resume
                                           // playback

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.start', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{***********************************}
procedure TALAndroidVideoPlayer.stop;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsStopped, [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  FSimpleExoPlayer.stop;   // Stops playback. Use setPlayWhenReady(false) rather than this method if the intention
                           // is to pause playback.
                           // Calling this method will cause the playback state to transition to STATE_IDLE. The
                           // player instance can still be used, and release() must still be called on the player if
                           // it's no longer required.
                           // Calling this method does not reset the playback position.

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.stop', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************************************}
procedure TALAndroidVideoPlayer.seekTo(const msec: Int64);
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  FSimpleExoPlayer.seekTo(msec); // The seek position in the current window, or TIME_UNSET to seek to the window's default position.
end;

{*****************************************************************}
procedure TALAndroidVideoPlayer.setLooping(const looping: Boolean);
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //int REPEAT_MODE_OFF = 0; => Normal playback without repetition.
  //int REPEAT_MODE_ONE = 1; => repeat the currently playing window infinitely.
  //int REPEAT_MODE_ALL = 2; => repeat the entire timeline infinitely.

  if looping then FSimpleExoPlayer.setRepeatMode(TJPlayer.JavaClass.REPEAT_MODE_ALL)
  else FSimpleExoPlayer.setRepeatMode(TJPlayer.JavaClass.REPEAT_MODE_OFF);

  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.setLooping', 'looping: ' + alBoolToStrU(looping) +
                                            ' - timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*************************************************************}
procedure TALAndroidVideoPlayer.setVolume(const Value: Single);
var aVolume: Single;
    {$IFDEF DEBUG}
    aStopWatch: TstopWatch;
    {$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  aVolume := Value;
  if aVolume < 0 then aVolume := 0
  else if aVolume > 1 then aVolume := 1;
  FSimpleExoPlayer.setVolume(aVolume); // Sets the audio volume, with 0 being silence and 1 being unity gain.

  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.setVolume', 'Value: ' + alFloattostrU(Value, ALDefaultFormatSettingsU) +
                                           ' - timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************************************************}
procedure TALAndroidVideoPlayer.setPlaybackSpeed(const Value: single);
var aPlaybackParameters: JPlaybackParameters;
begin
  aPlaybackParameters := tJPlaybackParameters.JavaClass.init(Value, 1);
  FSimpleExoPlayer.setPlaybackParameters(aPlaybackParameters);
  aPlaybackParameters := Nil;
end;

{***********************************************}
function TALAndroidVideoPlayer.getState: integer;
begin
  result := AtomicCmpExchange(fState, -1, -1);
end;

{**********************************************************************}
function TALAndroidVideoPlayer.setState(const aValue: integer): boolean;
begin
  result := AtomicExchange(fState, aValue) <> aValue;
end;

{**************************************************************************************************}
function TALAndroidVideoPlayer.setState(const aValue: integer; const aCompareAnd: integer): boolean;
begin
  AtomicCmpExchange(fState, aValue, aCompareAnd, Result);
end;

{***********************************************************************************************************}
function TALAndroidVideoPlayer.setState(const aValue: integer; const aCompareAnd: array of integer): boolean;
var i: integer;
begin
  result := False;
  for I := Low(aCompareAnd) to High(aCompareAnd) do begin
    AtomicCmpExchange(fState, aValue, aCompareAnd[i], Result);
    if result then break;
  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{***}
const
  _FrameRefreshInterval: integer = 28; // << will get a roundly fps of 30
                                       // << not 34 ms (1000 / 30 = 34) because we need also to take in account the
                                       // << timetaken to build the texture (around 2-4 ms)

{************************************************************************************************}
constructor TALIOSVideoPlayer.TALKVODelegate.Create(const aVideoPlayerControl: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{**********************************************************************************************************************************************}
procedure TALIOSVideoPlayer.TALKVODelegate.observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer);
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.observeValueForKeyPath', 'Status:' +  alinttostrU(TNSNumber.Wrap(change.allvalues.objectAtIndex(0)).integerValue()) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  fVideoPlayerControl.doOnReady;

end;

{**********************************************************************************************************}
constructor TALIOSVideoPlayer.TALNotificationsDelegate.Create(const aVideoPlayerControl: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{************************************************}
//Posted when the item has played to its end time.
procedure TALIOSVideoPlayer.TALNotificationsDelegate.ItemDidPlayToEndTime;
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemDidPlayToEndTime', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  fVideoPlayerControl.DoOnItemDidPlayToEndTime;

end;

{****************************************************}
//Posted when the item failed to play to its end time.
//The user info dictionary contains an error object that describes
//the problem—see AVPlayerItemFailedToPlayToEndTimeErrorKey.
procedure TALIOSVideoPlayer.TALNotificationsDelegate.ItemFailedToPlayToEndTime;
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemFailedToPlayToEndTime', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  fVideoPlayerControl.DoOnItemFailedToPlayToEndTime;

end;

{****************************************************************}
//Posted when the item’s current time has changed discontinuously.
procedure TALIOSVideoPlayer.TALNotificationsDelegate.ItemTimeJumped;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemTimeJumped', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*******************************************************************}
//Posted when some media did not arrive in time to continue playback.
//The notification’s object is the AVPlayerItem instance whose playback was
//unable to continue because the necessary streaming media wasn’t delivered
//in a timely fashion over a network. Playback of streaming media continues
//once a sufficient amount of data is delivered. File-based playback does
//not continue.
procedure TALIOSVideoPlayer.TALNotificationsDelegate.ItemPlaybackStalled;
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemPlaybackStalled', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  //we do nothing here, the fVideoPlayerControl.FFrameRefreshTimer will do himself the job
  //to detect when the playback stalled and will do himself a pause / restart

end;

{**************************************************}
//Posted when a new access log entry has been added.
procedure TALIOSVideoPlayer.TALNotificationsDelegate.ItemNewAccessLogEntry;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemNewAccessLogEntry', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*************************************************}
//Posted when a new error log entry has been added.
//The notification’s object is the player item. The new log entry is
//accessible via errorLog(), respectively.
procedure TALIOSVideoPlayer.TALNotificationsDelegate.ItemNewErrorLogEntry;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemNewErrorLogEntry', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}
end;

{***********************************}
constructor TALIOSVideoPlayer.Create;
var aAudioSession: AVAudioSession;
    {$IFDEF DEBUG}
    aStopWatch: TstopWatch;
    {$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  inherited;

  //-----
  fState := vpsIdle;
  fLooping := False;
  fVolume := -2;

  //-----
  if not appAudioSessionActivated then begin
    appAudioSessionActivated := True;
    aAudioSession := TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance);
    aAudioSession.setCategory(AVAudioSessionCategoryPlayback, nil);
    aAudioSession.setActive(True, nil);
  end;

  //-----
  fBitmap := TalTexture.Create;
  fBitmap.PixelFormat := TCustomContextIOS.PixelFormat;

  //-----
  fTextureRef := 0;
  if CVOpenGLESTextureCacheCreate(kCFAllocatorDefault, // allocator: The CFAllocatorRef to use for allocating the texture cache. This parameter can be NULL.
                                  nil, // cacheAttributes: A CFDictionaryRef containing the attributes of the texture cache itself. This parameter can be NULL.
                                  (TCustomContextIOS.SharedContext as ILocalObject).GetObjectID, // eaglContext: The OpenGLES 2.0 context into which the texture objects will be created. OpenGLES 1.x contexts are not supported.
                                  nil, // textureAttributes: A CFDictionaryRef containing the attributes to be used for creating the CVOpenGLESTextureRef objects. This parameter can be NULL.
                                  @fvideoTextureCacheRef) <> kCVReturnSuccess then raise Exception.Create('CVOpenGLESTextureCacheCreate failed!'); // cacheOut: A pointer to a CVOpenGLESTextureCacheRef where the newly created texture cache will be placed.

  //-----
  FFrameRefreshTimer := TTimer.Create(nil);
  FFrameRefreshTimer.Interval := _FrameRefreshInterval; // equivalent to a fps of 30
  FFrameRefreshTimer.OnTimer := doOnFrameRefresh;
  FFrameRefreshTimer.Enabled := False;

  //-----
  fPrepareThread := nil;
  FPlayer := nil;
  FPlayerItem := nil;
  FPlayerItemVideoOutput := nil;
  FKVODelegate := nil;
  FNotificationsDelegate := nil;
  fhasNONewPixelBufferForItemTimeCounter := 0;
  {$IFDEF DEBUG}
  fFrameRefreshCounter := 0;
  {$ENDIF}

  //-----
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fonVideoSizeChangedEvent := nil;

  //----
  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Create', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{***********************************}
destructor TALIOSVideoPlayer.Destroy;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //-----
  ALFreeAndNil(fPrepareThread);
  //-----
  FFrameRefreshTimer.Enabled := False;
  alFreeAndNil(FFrameRefreshTimer);
  //-----
  if fNotificationsDelegate <> nil then begin
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(FNotificationsDelegate.GetObjectID);
    AlFreeAndNil(FNotificationsDelegate);
  end;
  //-----
  if FPlayer <> nil then begin
    FPlayer.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status')); // << i do it inside synchronize because as Observers are always called in mainthread better to remove it from here
    FPlayer.release;
    FPlayer := nil;
  end;
  //-----
  if FPlayerItem <> nil then begin
    FPlayerItem.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status')); // << i do it inside synchronize because as Observers are always called in mainthread better to remove it from here
    FPlayerItem.release;
    FPlayerItem := nil;
  end;
  //-----
  AlFreeAndNil(FKVODelegate);
  //-----
  ITextureAccess(fBitmap).Handle := 0;
  alfreeAndNil(fbitmap);
  //-----
  if fTextureRef <> 0 then CFRelease(pointer(fTextureRef));
  //-----
  CVOpenGLESTextureCacheFlush(fvideoTextureCacheRef, 0);  // The texture cache automatically flushes currently unused resources when you call the
                                                          // CVOpenGLESTextureCacheCreateTextureFromImage function, but can you can also flush the
                                                          // cache explicitly by calling this function. The EAGLContext associated with the cache
                                                          // may be used to delete or unbind textures.
  CFrelease(pointer(fvideoTextureCacheRef));
  //-----
  if FPlayerItemVideoOutput <> nil then begin
    FPlayerItemVideoOutput.release;
    FPlayerItemVideoOutput := nil;
  end;

  //-----
  inherited;

  //----
  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Destroy', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{****************************************************}
//Returns the current time of the current player item.
function TALIOSVideoPlayer.getCurrentPosition: int64;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  if (FPlayerItem <> nil) then Result := Trunc(CMTimeGetSeconds(FPlayerItem.currentTime) * 1000)
  else result := 0;
end;

{********************************************}
function TALIOSVideoPlayer.getDuration: int64;
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  if (FPlayerItem <> nil) then Result := Trunc(CMTimeGetSeconds(FPlayerItem.duration) * 1000)
  else result := 0;
end;

{*************************************************}
function TALIOSVideoPlayer.getVideoHeight: integer;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  if FPlayerItem <> nil then Result := round(FPlayerItem.presentationSize.height)
  else result := 0;
end;

{************************************************}
function TALIOSVideoPlayer.getVideoWidth: integer;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  if FPlayerItem <> nil then Result := round(FPlayerItem.presentationSize.width)
  else result := 0;
end;

{********************************************}
function TALIOSVideoPlayer.isPlaying: boolean;
begin
  result := GetState = vpsStarted;
end;

{*************************************************************}
procedure TALIOSVideoPlayer.prepare(Const aDataSource: String);
begin

  //----
  if not SetState(vpsPreparing, vpsIdle) then raise Exception.Create('prepare can be call only in the idle state');

  //-----
  //https://stackoverflow.com/questions/30363502/maintaining-good-scroll-performance-when-using-avplayer
  fPrepareThread := TThread.CreateAnonymousThread(
    procedure
    var aURL: NSUrl;
        P: Pointer;
        {$IFDEF DEBUG}
        aStopWatch: TstopWatch;
        {$ENDIF}
    begin

      try

        {$IFDEF DEBUG}
        aStopWatch := TstopWatch.StartNew;
        {$ENDIF}

        P := TNSUrl.OCClass.URLWithString(StrToNSStr(aDataSource)); // Creates and returns an NSURL object initialized with a provided URL string
        if P = nil then exit; // << we can't call synchronize from here (else possible trouble when we will free the object) so we can't call onErrorEvent :(
        aURL := TNSUrl.Wrap(P);
        FPlayerItem := TAVPlayerItem.Wrap(TAVPlayerItem.OCClass.playerItemWithURL(aURL)); // return A new player item, prepared to use URL.
                                                                                          // This method immediately returns the item, but with the status AVPlayerItemStatusUnknown.
                                                                                          // Associating the player item with an AVPlayer immediately begins enqueuing its media
                                                                                          // and preparing it for playback. If the URL contains valid data that can be used by
                                                                                          // the player item, its status later changes to AVPlayerItemStatusReadyToPlay. If the
                                                                                          // URL contains no valid data or otherwise can't be used by the player item, its status
                                                                                          // later changes to AVPlayerItemStatusFailed. You can determine the nature of the failure
                                                                                          // by querying the player item’s error property.
        FPlayerItem.retain;
        //aURL.release;   | >> we can't do this else we will have an eaccessViolation when we will free the FPlayerItem
        //aURL := nil;    | >> http://stackoverflow.com/questions/42222508/why-we-need-to-do-retain-for-objective-c-object-field

        //-----
        FPlayer := TAVPlayer.Wrap(TAVPlayer.OCClass.playerWithPlayerItem(FPlayerItem)); // Returns a new player initialized to play the specified player item.
        FPlayer.retain;

        //-----
        fNotificationsDelegate := TALNotificationsDelegate.Create(self);
        TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemDidPlayToEndTime'), StringToID('AVPlayerItemDidPlayToEndTimeNotification'), (FPlayerItem as ILocalObject).GetObjectID);
        TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemFailedToPlayToEndTime'), StringToID('AVPlayerItemFailedToPlayToEndTimeNotification'), (FPlayerItem as ILocalObject).GetObjectID);
        TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemTimeJumped'), StringToID('AVPlayerItemTimeJumpedNotification'), (FPlayerItem as ILocalObject).GetObjectID);
        TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemPlaybackStalled'), StringToID('AVPlayerItemPlaybackStalledNotification'), (FPlayerItem as ILocalObject).GetObjectID);
        TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewAccessLogEntry'), StringToID('AVPlayerItemNewAccessLogEntryNotification'), (FPlayerItem as ILocalObject).GetObjectID);
        TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewErrorLogEntry'), StringToID('AVPlayerItemNewErrorLogEntryNotification'), (FPlayerItem as ILocalObject).GetObjectID);

        //-----
        FKVODelegate := TALKVODelegate.Create(self);
        FPlayer.addObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), // observer: The object to register for KVO notifications. The observer must implement the key-value observing method observeValue(forKeyPath:of:change:context:).
                            StrToNSStr('status'), // keyPath: The key path, relative to the object receiving this message, of the property to observe. This value must not be nil.
                            NSKeyValueObservingOptionNew, // options: A combination of the NSKeyValueObservingOptions values that specifies what is included in observation notifications. For possible values, see NSKeyValueObservingOptions.
                            nil); // context: Arbitrary data that is passed to observer in observeValue(forKeyPath:of:change:context:).
        FPlayerItem.addObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), // observer: The object to register for KVO notifications. The observer must implement the key-value observing method observeValue(forKeyPath:of:change:context:).
                                StrToNSStr('status'), // keyPath: The key path, relative to the object receiving this message, of the property to observe. This value must not be nil.
                                NSKeyValueObservingOptionNew, // options: A combination of the NSKeyValueObservingOptions values that specifies what is included in observation notifications. For possible values, see NSKeyValueObservingOptions.
                                nil); // context: Arbitrary data that is passed to observer in observeValue(forKeyPath:of:change:context:).

        //-----
        {$IFDEF DEBUG}
        aStopWatch.Stop;
        ALLog('TALIOSVideoPlayer.prepare', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU) +
                                           ' - FPlayer.status: ' + alinttostrU(FPlayer.status) +
                                           ' - FPlayerItem.status: ' + alinttostrU(FPlayerItem.status) +
                                           ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
        {$ENDIF}

      except
        // << we can't call synchronize from here (else possible trouble when we will free the object) so we can't call onErrorEvent :(
      end;

    end);
    fPrepareThread.FreeOnTerminate := False;
    fPrepareThread.Start;

end;

{********************************}
procedure TALIOSVideoPlayer.pause;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsPaused, vpsStarted) then exit;
  FPlayer.pause; // Pauses playback of the current item.
                 // Calling this method is the same as setting the rate to 0.0.
  FFrameRefreshTimer.Enabled := False;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.pause', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************}
procedure TALIOSVideoPlayer.Start;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsStarted, [vpsPrepared, vpsPaused, vpsPlaybackCompleted]) then exit;
  FPlayer.play; // Begins playback of the current item.
                // Calling this method is the same as setting the rate to 1.0.
  fhasNONewPixelBufferForItemTimeCounter := 0;
  FFrameRefreshTimer.Enabled := True;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.start', 'TimeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*******************************}
procedure TALIOSVideoPlayer.Stop;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsStopped, [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  FPlayer.pause; // Pauses playback of the current item.
                 // Calling this method is the same as setting the rate to 0.0.
  FFrameRefreshTimer.Enabled := False;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Stop', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{****************************************************}
procedure TALIOSVideoPlayer.seekTo(const msec: Int64);
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  FPlayer.seekToTime(CMTimeMake(msec, 1));
end;

{*************************************************************}
procedure TALIOSVideoPlayer.setLooping(const looping: Boolean);
begin
  fLooping := Looping;
end;

{*********************************************************}
procedure TALIOSVideoPlayer.setVolume(const Value: Single);
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  fVolume := Value;
  if fVolume < 0 then fVolume := 0
  else if fVolume > 1 then fVolume := 1;
  if (FPlayerItem <> nil) then FPlayer.setVolume(fVolume); // The audio playback volume for the player, ranging from 0.0 through 1.0 on a linear scale.
                                                           // A value of 0.0 indicates silence; a value of 1.0 (the default) indicates full audio volume for the player instance.

  {$IFDEF DEBUG}
  ALLog('TALIOSVideoPlayer.setVolume', 'Value: ' + alFloattostrU(Value, ALDefaultFormatSettingsU) +
                                       ' - timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{****************************************************************}
procedure TALIOSVideoPlayer.setPlaybackSpeed(const Value: single);
begin
  // not yet implemented
end;

{************************************************************}
procedure TALIOSVideoPlayer.doOnFrameRefresh(Sender: TObject);
var aPixelBuffer: CVPixelBufferRef;
    {$IFDEF DEBUG}
    aStopWatch: TstopWatch;
    {$ENDIF}
    aPrevTextureRef: CVOpenGLESTextureRef;
    aWidth, aHeight: integer;
    T: CMTime;
begin

  //stop the timer if we encoutered some error
  if GetState <> vpsStarted then begin
    FFrameRefreshTimer.Enabled := False;
    Exit;
  end;

  //in case
  if FPlayerItemVideoOutput = nil then exit;

  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  T := fPlayer.currentTime; // << Returns the current time of the current player item
  if FPlayerItemVideoOutput.hasNewPixelBufferForItemTime(T) then begin // Returns a Boolean value indicating whether video output is available for the specified item time.
                                                                       // itemTime: The item time to query. The time value is relative to the AVPlayerItem object with which the receiver is associated.
                                                                       // Return Value: YES if there is available video output that has not been previously acquired or NO if there is not.
                                                                       // Note: This method returns YES if the video data at the specified time has not yet been acquired or is different from the video
                                                                       // that was acquired previously. If you require multiple objects to acquire video output from the same AVPlayerItem object,
                                                                       // you should create separate AVPlayerItemVideoOutput objects for each.

    aPixelBuffer := FPlayerItemVideoOutput.copyPixelBufferForItemTime(T, nil); // Acquires and returns an image that is appropriate to display at the specified time.
                                                                               // itemTime: The time at which you want to retrieve the image from the item.
                                                                               // outItemTimeForDisplay: The time by which you intend to use the returned pixel buffer. You may specify nil for this
                                                                               //                        parameter if you do not have a specific deadline.
                                                                               // NODE: A pixel buffer containing the image data to display or nil if nothing should be displayed at the specified time.
                                                                               //       The caller is responsible for calling CVBufferRelease on the returned data when it is no longer needed.
    if aPixelBuffer = 0 then begin // could be nil if nothing should be displayed
      {$IFDEF DEBUG}
      ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'copyPixelBufferForItemTime:nil', TalLogType.warn);
      {$ENDIF}
      exit; // could be nil if nothing should be displayed
    end;
    try

      //-----
      aPrevTextureRef := fTextureRef;
      aWidth := CVPixelBufferGetWidth(aPixelBuffer); // Returns the width of the pixel buffer.
      aHeight := CVPixelBufferGetHeight(aPixelBuffer); // Returns the height of the pixel buffer.

      //-----
      // This function either creates a new or returns a cached CVOpenGLESTexture texture object mapped to the
      // CVImageBuffer and associated parameters. This operation creates a live binding between the image buffer
      // and the underlying texture object. The EAGLContext associated with the cache may be modified to create,
      // delete, or bind textures. When used as a source texture or GL_COLOR_ATTACHMENT, the image buffer must be
      // unlocked before rendering. The source or render buffer texture should not be re-used until the rendering
      // has completed. This can be guaranteed by calling glFlush()
      //
      // The texture cache automatically flushes currently unused resources when you call the
      // CVOpenGLESTextureCacheCreateTextureFromImage function
      if CVOpenGLESTextureCacheCreateTextureFromImage(kCFAllocatorDefault, // allocator: The CFAllocator to use for allocating the texture object. This parameter can be NULL.
                                                      fvideoTextureCacheRef, // textureCache: The texture cache object that will manage the texture.
                                                      aPixelBuffer, // sourceImage: The CVImageBuffer that you want to create a texture from.
                                                      nil,  // textureAttributes: A CFDictionary containing the attributes to be used for creating the CVOpenGLESTexture objects. This parameter can be NULL.
                                                      GL_TEXTURE_2D, // target: The target texture. GL_TEXTURE_2D and GL_RENDERBUFFER are the only targets currently supported.
                                                      GL_RGBA,  // internalFormat: The number of color components in the texture. Examples are GL_RGBA, GL_LUMINANCE, GL_RGBA8_OES, GL_RED, and GL_RG.
                                                      aWidth, // width: The width of the texture image.
                                                      aHeight, // height The height of the texture image.
                                                      GL_BGRA_EXT,  // format: The format of the pixel data. Examples are GL_RGBA and GL_LUMINANCE.
                                                      GL_UNSIGNED_BYTE, // type: The data type of the pixel data. One example is GL_UNSIGNED_BYTE.
                                                      0,  // planeIndex: The plane of the CVImageBuffer to map bind. Ignored for non-planar CVImageBuffers.
                                                      @fTextureRef) <> kCVReturnSuccess then begin // textureOut: A pointer to a CVOpenGLESTexture where the newly created texture object will be placed.
        {$IFDEF DEBUG}
        ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'CVOpenGLESTextureCacheCreateTextureFromImage failed!', TalLogType.Error);
        {$ENDIF}
        exit;
      end;

      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, CVOpenGLESTextureGetName(fTextureRef));
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      case fBitmap.MagFilter of
        TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      end;
      case fBitmap.MinFilter of
        TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      end;
      glBindTexture(GL_TEXTURE_2D, 0);

      //-----
      if aPrevTextureRef <> 0 then
        cfRElease(pointer(aPrevTextureRef));

      //-----
      {$IF CompilerVersion > 33} // rio
        {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.SetSize is still the same and adjust the IFDEF'}
      {$ENDIF}
      TALTextureAccessPrivate(fBitmap).FWidth := aWidth;
      TALTextureAccessPrivate(fBitmap).FHeight := aHeight; // we can't use setsize because it's will finalise the texture
                                                           // but with/height are used only in
                                                           // procedure TCanvasHelper.TexRect(const DestCorners, SrcCorners: TCornersF; const Texture: TTexture; const Color1, Color2, Color3, Color4: TAlphaColor);
                                                           // begin
                                                           //   ...
                                                           //   if (Texture = nil) or (Texture.Width < 1) or (Texture.Height < 1) then Exit
                                                           //   ...
                                                           //   InvTexSize := PointF(1 / Texture.Width, 1 / Texture.Height);
                                                           //   ...
                                                           // end
                                                           // so i don't need to finalize the texture !!
      ITextureAccess(fBitmap).Handle := CVOpenGLESTextureGetName(fTextureRef);

      //-----
      if assigned(FonFrameAvailableEvent) then
        FonFrameAvailableEvent(self);

    finally
      CVPixelBufferRelease(aPixelBuffer);
    end;

    //-----
    fhasNONewPixelBufferForItemTimeCounter := 0;

    {$IFDEF DEBUG}
    inc(fFrameRefreshCounter);
    if fFrameRefreshCounter mod 1000 = 0 then begin
      aStopWatch.Stop;
      fFrameRefreshCounter := 0;
      ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU) +
                                                     ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
    end;
    {$ENDIF}

  end
  else begin

    //sometime we continusly receive hasNewPixelBufferForItemTime:NO
    //for exemple cut the wifi off during playback, they you will have stalled (normal)
    //but later when you will put the player on you will continue to receive hasNewPixelBufferForItemTime:NO
    inc(fhasNONewPixelBufferForItemTimeCounter);
    if (fhasNONewPixelBufferForItemTimeCounter > 30) and // << with an interval of 34 ms mean more than 1s without anything
       ((fPlayerItem.isPlaybackBufferFull) or // Indicates whether the internal media buffer is full and that further I/O is suspended.
                                              // Despite the playback buffer reaching capacity there might not exist sufficient statistical
                                              // data to support a playbackLikelyToKeepUp prediction of YES.
        (fPlayerItem.isPlaybackLikelyToKeepUp)) // Indicates whether the item will likely play through without stalling.
                                                // This property communicates a prediction of playability. Factors considered
                                                // in this prediction include I/O throughput and media decode performance.
                                                // It is possible for playbackLikelyToKeepUp to indicate NO while the
                                                // property playbackBufferFull indicates YES. In this event the playback
                                                // buffer has reached capacity but there isn't the statistical data to support
                                                // a prediction that playback is likely to keep up in the future. It is up to
                                                // you to decide whether to continue media playback.
    then begin
      {$IFDEF DEBUG}
      ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'hasNewPixelBufferForItemTime:NO '+
                                                     '- Pause and Restart '+
                                                     ' - currentTime: ' + alinttostrU(fPlayer.currentTime.value) +
                                                     ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
      {$ENDIF}
      fPlayer.pause;
      fPlayer.play;
      fhasNONewPixelBufferForItemTimeCounter := 0;
    end;

  end;

end;

{************************************}
procedure TALIOSVideoPlayer.doOnReady;
var aPixelBufferAttributes: NSMutableDictionary;
begin

  //AVPlayerItemStatusFailed = 2;
  //AVPlayerItemStatusReadyToPlay = 1;
  //AVPlayerItemStatusUnknown = 0;
  //AVPlayerStatusFailed = 2;
  //AVPlayerStatusReadyToPlay = 1;
  //AVPlayerStatusUnknown = 0;
  if (fPlayer.status = AVPlayerStatusReadyToPlay) and // The player’s status indicates whether the player can be used for playback.
                                                      // When the value of this property is failed, you can no longer use the player
                                                      // for playback and you need to create a new instance to replace it. If this
                                                      // happens, you can check the value of the player’s error property to determine
                                                      // the nature of the failure.
                                                      // This property is key value observable using Key-value observing.
                                                      // NOTE: The player’s status does not indicate its readiness to play a specific
                                                      // player item. You should instead use the status property of AVPlayerItem to make
                                                      // that determination.
     (FPlayerItem.status = AVPlayerItemStatusReadyToPlay) // When a player item is created, its status is unknown, meaning its media hasn’t
                                                          // been loaded and has not yet been enqueued for playback. Associating a player
                                                          // item with an AVPlayer immediately begins enqueuing the item’s media and preparing
                                                          // it for playback. When the player item’s media has been loaded and is ready
                                                          // for use, its status will change to readyToPlay. You can observe this
                                                          // change using key-value observing.
  then begin

    {$IFDEF DEBUG}
    ALLog('TALIOSVideoPlayer.doOnReady', 'Ready' +
                                         ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
    {$ENDIF}

    //i need to do this here because of bug like :
    //https://forums.developer.apple.com/thread/27589
    //http://stackoverflow.com/questions/24800742/iosavplayeritemvideooutput-hasnewpixelbufferforitemtime-doesnt-work-correctly
    if FPlayerItemVideoOutput = nil then begin
      aPixelBufferAttributes := TNSMutableDictionary.Create;
      try
        aPixelBufferAttributes.setObject(TNSNumber.OCClass.numberWithInt(kCVPixelFormatType_32BGRA), Pointer(kCVPixelBufferPixelFormatTypeKey));
        FPlayerItemVideoOutput := TAVPlayerItemVideoOutput.Wrap(TAVPlayerItemVideoOutput.Alloc.initWithPixelBufferAttributes(aPixelBufferAttributes)); // Initializes and returns a video output object using the specified
                                                                                                                                                       // pixel buffer attributes.
                                                                                                                                                       // The pixel buffer attributes required for video output. For a list
                                                                                                                                                       // of pixel buffer attributes you can include in this dictionary, see
                                                                                                                                                       // the CVPixelBuffer.h header file in the Core Video framework.
      finally
        aPixelBufferAttributes.release;
        aPixelBufferAttributes := nil;
      end;
      FPlayerItemVideoOutput.retain;
      FPlayerItem.addOutput(FPlayerItemVideoOutput);
    end;

    //fire the fOnPreparedEvent
    if SetState(vpsPrepared, vpsPreparing) then begin
      if fVolume > -1 then setVolume(fVolume);
      if assigned(fOnPreparedEvent) then fOnPreparedEvent(self);
    end;

  end
  else if (fPlayer.status = AVPlayerStatusFailed) or
          (FPlayerItem.status = AVPlayerItemStatusFailed) then begin

    {$IFDEF DEBUG}
    ALLog('TALIOSVideoPlayer.doOnReady', 'Failed' +
                                         ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
    {$ENDIF}

    //fire the fOnErrorEvent
    if setState(vpsError) and
       assigned(fOnErrorEvent) then fOnErrorEvent(self);

  end;

end;

{***************************************************}
procedure TALIOSVideoPlayer.doOnItemDidPlayToEndTime;
begin
  if fLooping then begin
    FPlayer.seekToTime(CMTimeMake(0, 1));
    FPlayer.play;
  end
  else begin
    FFrameRefreshTimer.Enabled := False;
    If SetState(vpsPlaybackCompleted, vpsStarted) and
       assigned(fOnCompletionEvent) then fOnCompletionEvent(self);
  end;
end;

{********************************************************}
procedure TALIOSVideoPlayer.doOnItemFailedToPlayToEndTime;
begin
  if SetState(vpsError) then begin
    FFrameRefreshTimer.Enabled := False;
    if assigned(fOnErrorEvent) then fOnErrorEvent(self);
  end;
end;

{*******************************************}
function TALIOSVideoPlayer.getState: integer;
begin
  result := AtomicCmpExchange(fState, -1, -1);
end;

{******************************************************************}
function TALIOSVideoPlayer.setState(const aValue: integer): boolean;
begin
  result := AtomicExchange(fState, aValue) <> aValue;
end;

{**********************************************************************************************}
function TALIOSVideoPlayer.setState(const aValue: integer; const aCompareAnd: integer): boolean;
begin
  AtomicCmpExchange(fState, aValue, aCompareAnd, Result);
end;

{*******************************************************************************************************}
function TALIOSVideoPlayer.setState(const aValue: integer; const aCompareAnd: array of integer): boolean;
var i: integer;
begin
  result := False;
  for I := Low(aCompareAnd) to High(aCompareAnd) do begin
    AtomicCmpExchange(fState, aValue, aCompareAnd[i], Result);
    if result then break;
  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION 'MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{***********************************}
constructor TALWinVideoPlayer.Create;
begin
  inherited;
  fbitmap := nil;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fonVideoSizeChangedEvent := nil;
end;

{***********************************}
destructor TALWinVideoPlayer.Destroy;
begin
  ALFreeAndNil(fbitmap);
  inherited;
end;

{***************************************************}
function TALWinVideoPlayer.getCurrentPosition: int64;
begin
  result := 0;
end;

{********************************************}
function TALWinVideoPlayer.getDuration: int64;
begin
  result := 0;
end;

{*************************************************}
function TALWinVideoPlayer.getVideoHeight: integer;
begin
  result := 0;
end;

{************************************************}
function TALWinVideoPlayer.getVideoWidth: integer;
begin
  result := 0;
end;

{********************************************}
function TALWinVideoPlayer.isPlaying: boolean;
begin
  result := False;
end;

{********************************}
procedure TALWinVideoPlayer.pause;
begin
end;

{*************************************************************}
procedure TALWinVideoPlayer.prepare(Const aDataSource: String);
begin
end;

{********************************}
procedure TALWinVideoPlayer.Start;
begin
end;

{*******************************}
procedure TALWinVideoPlayer.Stop;
begin
end;

{****************************************************}
procedure TALWinVideoPlayer.seekTo(const msec: Int64);
begin
end;

{*************************************************************}
procedure TALWinVideoPlayer.setLooping(const looping: Boolean);
begin
end;

{*********************************************************}
procedure TALWinVideoPlayer.setVolume(const Value: Single);
begin
end;

{****************************************************************}
procedure TALWinVideoPlayer.setPlaybackSpeed(const Value: single);
begin
end;

{*******************************************}
function TALWinVideoPlayer.getState: integer;
begin
  result := vpsIdle;
end;

{$ENDIF}
{$ENDREGION}

{$REGION '_MACOS'}
{$IF defined(_MACOS)}

{*************************************}
constructor TALMacOSVideoPlayer.Create;
begin
  inherited;
  fbitmap := nil;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fonVideoSizeChangedEvent := nil;
end;

{*************************************}
destructor TALMacOSVideoPlayer.Destroy;
begin
  ALFreeAndNil(fbitmap);
  inherited;
end;

{*****************************************************}
function TALMacOSVideoPlayer.getCurrentPosition: int64;
begin
  result := 0;
end;

{**********************************************}
function TALMacOSVideoPlayer.getDuration: int64;
begin
  result := 0;
end;

{***************************************************}
function TALMacOSVideoPlayer.getVideoHeight: integer;
begin
  result := 0;
end;

{**************************************************}
function TALMacOSVideoPlayer.getVideoWidth: integer;
begin
  result := 0;
end;

{**********************************************}
function TALMacOSVideoPlayer.isPlaying: boolean;
begin
  result := False;
end;

{**********************************}
procedure TALMacOSVideoPlayer.pause;
begin
end;

{***************************************************************}
procedure TALMacOSVideoPlayer.prepare(Const aDataSource: String);
begin
end;

{**********************************}
procedure TALMacOSVideoPlayer.Start;
begin
end;

{*********************************}
procedure TALMacOSVideoPlayer.Stop;
begin
end;

{******************************************************}
procedure TALMacOSVideoPlayer.seekTo(const msec: Int64);
begin
end;

{***************************************************************}
procedure TALMacOSVideoPlayer.setLooping(const looping: Boolean);
begin
end;

{***********************************************************}
procedure TALMacOSVideoPlayer.setVolume(const Value: Single);
begin
end;

{*********************************************}
function TALMacOSVideoPlayer.getState: integer;
begin
  result := vpsIdle;
end;

{$ENDIF}
{$ENDREGION}

{********************************}
constructor TALVideoPlayer.Create;
begin

  inherited create;
  //-----
  {$IF defined(android)}
  fVideoPlayerControl := TALAndroidVideoPlayer.Create;
  {$ELSEIF defined(IOS)}
  fVideoPlayerControl := TALIOSVideoPlayer.Create;
  {$ELSEIF defined(MSWINDOWS)}
  fVideoPlayerControl := TALWinVideoPlayer.Create;
  {$ELSEIF defined(_MACOS)}
  fVideoPlayerControl := TALMacOSVideoPlayer.Create;
  {$ENDIF}
  //-----
  fVideoPlayerControl.OnError := DoOnError;
  fVideoPlayerControl.OnPrepared := doOnPrepared;
  fVideoPlayerControl.OnFrameAvailable := doOnFrameAvailable;
  fVideoPlayerControl.OnCompletion := doOnCompletion;
  fVideoPlayerControl.onVideoSizeChanged := doonVideoSizeChanged;
  //-----
  fOnErrorEvent := nil;
  fOnPreparedEvent := nil;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fonVideoSizeChangedEvent := nil;
  //-----
  FAutoStartWhenPrepared := False;
  //-----
  fTag := 0;
  FTagObject := nil;
  FTagFloat := 0.0;
  FTagString := '';

end;

{********************************}
destructor TALVideoPlayer.Destroy;
begin
  AlFreeAndNil(fVideoPlayerControl);
  inherited;
end;

{************************************************}
function TALVideoPlayer.getCurrentPosition: int64;
begin
  result := fVideoPlayerControl.getCurrentPosition;
end;

{*****************************************}
function TALVideoPlayer.getDuration: int64;
begin
  result := fVideoPlayerControl.getDuration;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALVideoPlayer.GetBitmap: TalTexture;
{$ELSE}
function TALVideoPlayer.GetBitmap: Tbitmap;
{$ENDIF}
begin
  result := fVideoPlayerControl.Bitmap;
end;

{**********************************************}
function TALVideoPlayer.getVideoHeight: integer;
begin
  result := fVideoPlayerControl.getVideoHeight;
end;

{*********************************************}
function TALVideoPlayer.getVideoWidth: integer;
begin
  result := fVideoPlayerControl.getVideoWidth;
end;

{****************************************}
function TALVideoPlayer.getState: integer;
begin
  result := fVideoPlayerControl.getState;
end;

{*****************************************}
function TALVideoPlayer.isPlaying: boolean;
begin
  result := fVideoPlayerControl.isplaying;
end;

{*****************************}
procedure TALVideoPlayer.pause;
begin
  fVideoPlayerControl.pause;
end;

{*******************************************************************************************************}
procedure TALVideoPlayer.prepare(Const aDataSource: String; const aAutoStartWhenPrepared: Boolean=False);
begin
  FAutoStartWhenPrepared := aAutoStartWhenPrepared;
  fVideoPlayerControl.prepare(aDataSource);
end;

{*************************************************}
procedure TALVideoPlayer.seekTo(const msec: Int64);
begin
  fVideoPlayerControl.seekTo(msec);
end;

{**********************************************************}
procedure TALVideoPlayer.setLooping(const looping: Boolean);
begin
  fVideoPlayerControl.setLooping(looping);
end;

{******************************************************}
procedure TALVideoPlayer.setVolume(const Value: Single);
begin
  fVideoPlayerControl.setVolume(Value);
end;

{*************************************************************}
procedure TALVideoPlayer.setPlaybackSpeed(const Value: single);
begin
  fVideoPlayerControl.setPlaybackSpeed(Value);
end;

{*****************************}
procedure TALVideoPlayer.Start;
begin
  fVideoPlayerControl.Start;
end;

{****************************}
procedure TALVideoPlayer.Stop;
begin
  fVideoPlayerControl.Stop;
end;

{*******************************************************}
procedure TALVideoPlayer.doOnCompletion(Sender: TObject);
begin
  if assigned(fOnCompletionEvent) then fOnCompletionEvent(Self);
end;

{**************************************************}
procedure TALVideoPlayer.doOnError(Sender: TObject);
begin
  if assigned(fOnErrorEvent) then fOnErrorEvent(Self);
end;

{*****************************************************}
procedure TALVideoPlayer.doOnPrepared(Sender: TObject);
begin
  if FAutoStartWhenPrepared then start;
  if assigned(fOnPreparedEvent) then fOnPreparedEvent(Self);
end;

{***********************************************************}
procedure TALVideoPlayer.doOnFrameAvailable(Sender: TObject);
begin
  if assigned(fOnFrameAvailableEvent) then fOnFrameAvailableEvent(Self);
end;

{****************************************************************************************************************}
procedure TALVideoPlayer.doOnVideoSizeChanged(const Sender: TObject; const width: Integer; const height: Integer);
begin
  if assigned(fOnVideoSizeChangedEvent) then fOnVideoSizeChangedEvent(Self, width, height);
end;

{***********************************************************}
constructor TALVideoPlayerSurface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fVideoPlayer := TALVideoPlayer.create;
  fVideoPlayer.OnFrameAvailable := OnFrameAvailable;
  fill.DefaultColor := $ff000000;
  fill.Color := $ff000000;
  stroke.DefaultKind := TBrushKind.none;
  stroke.kind := TBrushKind.none;
end;

{***************************************}
destructor TALVideoPlayerSurface.Destroy;
begin
  ALFreeAndNil(fVideoPlayer);
  inherited;
end;

{***********************************************}
procedure TALVideoPlayerSurface.resetVideoPlayer;
begin
  alFreeAndNil(fVideoPlayer);
  fVideoPlayer := TALVideoPlayer.create;
  fVideoPlayer.OnFrameAvailable := OnFrameAvailable;
end;

{****************************************************************************************************}
procedure TALVideoPlayerSurface.SetVideoSizeChangedEvent(const Value: TALVideoSizeChangedNotifyEvent);
begin
  fVideoPlayer.onVideoSizeChanged := Value;
end;

{**************************************************************************************}
function TALVideoPlayerSurface.GetVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
begin
  result := fVideoPlayer.onVideoSizeChanged;
end;

{****************************************************************}
procedure TALVideoPlayerSurface.OnFrameAvailable(Sender: Tobject);
begin
  repaint;
end;

{************************************}
procedure TALVideoPlayerSurface.Paint;
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
var aDestRect: TrectF;
{$endif}
begin

  inherited paint;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  if (fVideoPlayer.bitmap = nil) or
     (fVideoPlayer.bitmap.IsEmpty) then exit;

  aDestRect := canvas.AlignToPixel(
                 TRectF.Create(0, 0, fVideoPlayer.bitmap.Width, fVideoPlayer.bitmap.Height).
                   FitInto(LocalRect));

  TCustomCanvasGpu(Canvas).DrawTexture(aDestRect, // ATexRect
                                       TRectF.Create(0,
                                                     0,
                                                     fVideoPlayer.bitmap.Width,
                                                     fVideoPlayer.bitmap.Height), // ARect
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fVideoPlayer.bitmap);
  {$endif}

end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALVideoPlayerSurface]);
end;

initialization

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  TALAndroidVideoPlayer.DefaultBandwidthMeter := nil;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TALIosVideoPlayer.appAudioSessionActivated := False;
  {$ENDIF}
  {$ENDREGION}

finalization

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  TALAndroidVideoPlayer.DefaultBandwidthMeter := nil;
  {$ENDIF}
  {$ENDREGION}

end.
