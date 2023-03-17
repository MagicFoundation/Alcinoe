(*******************************************************************************
ALVideoPlayer will render a video on a TEXTURE. this is really important because
you can fully integrate the video in the delphi form and you can place any
controls you want on the top of it as it's support Z-ORDER. Official delphi
video player are just native video player window on the top of the form and
thus not supporting Z-ORDER.

Under android I use ExoPlayer. ExoPlayer is an open source project that is not
part of the Android framework and is distributed separately from the Android
SDK. ExoPlayer’s standard audio and video components are built on Android’s
MediaCodec API, which was released in Android 4.1 (API level 16). Because
ExoPlayer is a library, you can easily take advantage of new features as
they become available by updating your app. ExoPlayer supports features like
Dynamic adaptive streaming over HTTP (DASH), HLS, SmoothStreaming and Common
Encryption, which are not supported by MediaPlayer. It's designed to be easy
to customize and extend.

Under Ios i use AVPlayer with support also HLS like exoplayer do.

INSTALLATION
1) On android you will need to patch the RTL because it's not support
   GL_TEXTURE_EXTERNAL_OES https://quality.embarcadero.com/browse/RSP-16830
   Run Embarcadero/<YourDelphiVersion>/update.bat and include the generated
   pas in your search path.
2) On android we use exoplayer so you must add the exoplayer libraries
   com.google.android.exoplayer:exoplayer-core:2.18.2 and if you need HLS support
   com.google.android.exoplayer:exoplayer-hls:2.18.2 with the help of
   Tools\AndroidMerger. Take a look of how to do with
   Demos\ALFmxControls\_source\android\MergeLibraries.bat
3) After it's must be quite easy, just read the source code or look the sample
   located at Demos\ALFmxControls
*******************************************************************************)
unit Alcinoe.FMX.VideoPlayer;

interface

{$I Alcinoe.inc}

uses
  system.Classes,
  System.SyncObjs,
  {$IF defined(DEBUG)}
  system.diagnostics,
  {$endIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.ExoPlayer,
  Alcinoe.FMX.Types3D,
  {$endIF}
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.AVFoundation,
  iOSapi.CoreVideo,
  Alcinoe.FMX.Types3D,
  {$endIF}
  Fmx.types,
  Fmx.graphics,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Objects;

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

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  TALAndroidVideoPlayer = class(Tobject)
  private

    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TFrameAvailableListener = class(TJavaLocal, JSurfaceTexture_OnFrameAvailableListener)
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

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TPlayerListener = class(TJavaLocal, JPlayer_Listener)
      private
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onEvents(player: JPlayer; events: JPlayer_Events); cdecl;
        procedure onTimelineChanged(timeline: JTimeline; reason: Integer); cdecl;
        procedure onMediaItemTransition(mediaItem: JMediaItem; reason: Integer); cdecl;
        procedure onTracksChanged(tracks: JTracks); cdecl;
        procedure onMediaMetadataChanged(mediaMetadata: JMediaMetadata); cdecl;
        procedure onPlaylistMetadataChanged(mediaMetadata: JMediaMetadata); cdecl;
        procedure onIsLoadingChanged(isLoading: boolean); cdecl;
        procedure onLoadingChanged(isLoading: boolean); cdecl;
        procedure onAvailableCommandsChanged(availableCommands: JPlayer_Commands); cdecl;
        procedure onTrackSelectionParametersChanged(parameters: JTrackSelectionParameters); cdecl;
        procedure onPlayerStateChanged(playWhenReady: boolean; playbackState: Integer); cdecl;
        procedure onPlaybackStateChanged(playbackState: Integer); cdecl;
        procedure onPlayWhenReadyChanged(playWhenReady: boolean; reason: Integer); cdecl;
        procedure onPlaybackSuppressionReasonChanged(playbackSuppressionReason: Integer); cdecl;
        procedure onIsPlayingChanged(isPlaying: boolean); cdecl;
        procedure onRepeatModeChanged(repeatMode: Integer); cdecl;
        procedure onShuffleModeEnabledChanged(shuffleModeEnabled: boolean); cdecl;
        procedure onPlayerError(error: JPlaybackException); cdecl;
        procedure onPlayerErrorChanged(error: JPlaybackException); cdecl;
        procedure onPositionDiscontinuity(reason: Integer); overload; cdecl;
        procedure onPositionDiscontinuity(oldPosition: JPlayer_PositionInfo; newPosition: JPlayer_PositionInfo; reason: Integer); overload; cdecl;
        procedure onPlaybackParametersChanged(playbackParameters: JPlaybackParameters); cdecl;
        procedure onSeekBackIncrementChanged(seekBackIncrementMs: Int64); cdecl;
        procedure onSeekForwardIncrementChanged(seekForwardIncrementMs: Int64); cdecl;
        procedure onMaxSeekToPreviousPositionChanged(maxSeekToPreviousPositionMs: Int64); cdecl;
        procedure onSeekProcessed; cdecl;
        procedure onAudioSessionIdChanged(audioSessionId: Integer); cdecl;
        procedure onAudioAttributesChanged(audioAttributes: JAudioAttributes); cdecl;
        procedure onVolumeChanged(volume: Single); cdecl;
        procedure onSkipSilenceEnabledChanged(skipSilenceEnabled: boolean); cdecl;
        procedure onDeviceInfoChanged(deviceInfo: JDeviceInfo); cdecl;
        procedure onDeviceVolumeChanged(volume: Integer; muted: boolean); cdecl;
        procedure onVideoSizeChanged(videoSize: JVideoSize); cdecl;
        procedure onSurfaceSizeChanged(width: Integer; height: Integer); cdecl;
        procedure onRenderedFirstFrame; cdecl;
        procedure onCues(cues: JList); overload; cdecl;
        procedure onCues(cueGroup: JCueGroup); overload; cdecl;
        procedure onMetadata(metadata: JMetadata); cdecl;
      end;

  private
    fHandler: JHandler;
    fExoPlayer: JExoPlayer;
    fDataSourceFactory: JDataSource_Factory;
    fSurface: JSurface;
    fSurfaceTexture: JSurfaceTexture;
    fbitmap: TALTexture;
    fOnFrameAvailableEvent: TNotifyEvent;
    FOnFrameAvailableListener: TFrameAvailableListener;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    FPlayerListener: TPlayerListener;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
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

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TALIOSVideoPlayer = class(Tobject)
  private

    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      IKVODelegate = interface(IObjectiveC)
        ['{B0C6720A-79B7-4FD6-B9E3-77594E905B3C}']
        procedure observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TKVODelegate = class(TOCLocal, IKVODelegate)
      private
        [Weak] FVideoPlayerControl: TALIOSVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALIOSVideoPlayer);
        procedure observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      INotificationsDelegate = interface(IObjectiveC)
        ['{EF778992-B249-474F-B1AD-D64903EF7947}']
        procedure ItemDidPlayToEndTime; cdecl;
        procedure ItemFailedToPlayToEndTime; cdecl;
        procedure ItemTimeJumped; cdecl;
        procedure ItemPlaybackStalled; cdecl;
        procedure ItemNewAccessLogEntry; cdecl;
        procedure ItemNewErrorLogEntry; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TNotificationsDelegate = class(TOCLocal, INotificationsDelegate)
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
    FKVODelegate: TKVODelegate;
    fNotificationsDelegate: TNotificationsDelegate;
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

  {$REGION 'ALMacOS'}
  {$IF defined(ALMacOS)}
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
    {$ELSEIF defined(ALMacOS)}
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
    function GetBitmap: TALRasterImage;
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
    property Bitmap: TALRasterImage read Getbitmap;
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

  {*************************}
  [ComponentPlatforms($FFFF)]
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

uses
  system.SysUtils,
  system.Types,
  {$IF defined(ANDROID)}
  system.Math,
  Androidapi.Helpers,
  androidapi.jni.net,
  FMX.Canvas.GPU,
  Alcinoe.StringUtils,
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
  Alcinoe.StringUtils,
  {$ENDIF}
  fmx.controls,
  Alcinoe.Common;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}

{*****************************************************************************************************************}
constructor TALAndroidVideoPlayer.TFrameAvailableListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  {$IF defined(DEBUG)}
  fTotalFramesProcessed := 0;
  fFpsStopWatch := TStopWatch.StartNew;
  {$ENDIF}
  fVideoPlayerControl := aVideoPlayerControl;
  fSdkInt := TJBuild_VERSION.JavaClass.SDK_INT;
end;

{**********************************************************************************************************}
procedure TALAndroidVideoPlayer.TFrameAvailableListener.DoOnFrameAvailable(surfaceTexture: JSurfaceTexture);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerControl = nil then exit;

  {$IF defined(DEBUG)}
  if (fTotalFramesProcessed mod 1000 = 0) then begin
    fFpsStopWatch.Stop;
    ALLog('TALAndroidVideoPlayer.TFrameAvailableListener.DoOnFrameAvailable', 'fps: ' + ALFormatFloatW('0.00', (fTotalFramesProcessed) / max(1, (ffpsStopWatch.Elapsed.TotalMilliseconds / 1000)), ALDefaultFormatSettingsW), TalLogType.VERBOSE);
    fTotalFramesProcessed := 0;
    fFpsStopWatch := TStopWatch.StartNew;
  end;
  inc(fTotalFramesProcessed);
  {$ENDIF}

  if (fVideoPlayerControl.fbitmap.Width <> fVideoPlayerControl.fVideoWidth) or
     (fVideoPlayerControl.fbitmap.Height <> fVideoPlayerControl.fVideoHeight) then begin
    {$IFNDEF ALCompilerVersionSupported}
      {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.SetSize is still the same and adjust the IFDEF'}
    {$ENDIF}
    // we can't use setsize because it's will finalise the texture
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
    TALTextureAccessPrivate(fVideoPlayerControl.fBitmap).FWidth := fVideoPlayerControl.fVideoWidth;
    TALTextureAccessPrivate(fVideoPlayerControl.fBitmap).FHeight := fVideoPlayerControl.fVideoHeight;
  end;

  {$IF defined(DEBUG)}
  var LStopWatch: TStopWatch;
  if (fTotalFramesProcessed mod 1000 = 0) then LStopWatch := TStopWatch.StartNew;
  {$ENDIF}

  fVideoPlayerControl.fSurfaceTexture.updateTexImage;

  {$IFDEF DEBUG}
  if (fTotalFramesProcessed mod 1000 = 0) then begin
    LStopWatch.Stop;
    ALLog('TALAndroidVideoPlayer.onFrameAvailable.updateTexImage', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  end;
  {$ENDIF}

  if assigned(fVideoPlayerControl.fOnFrameAvailableEvent) then
    fVideoPlayerControl.fOnFrameAvailableEvent(fVideoPlayerControl);

end;

{********************************************************************************************************}
procedure TALAndroidVideoPlayer.TFrameAvailableListener.onFrameAvailable(surfaceTexture: JSurfaceTexture);
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
  // with handler = TJHandler.JavaClass.init(TJLooper.javaclass.getMainLooper()) then this event will be
  // always called from the main UI thread

  if fSdkInt >= 21 {LOLLIPOP} then DoOnFrameAvailable(surfaceTexture)
  else begin
    TThread.Synchronize(nil,
      Procedure
      begin
        DoOnFrameAvailable(surfaceTexture);
      end);
  end;

end;

{*********************************************************************************************************}
constructor TALAndroidVideoPlayer.TPlayerListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onEvents(player: JPlayer; events: JPlayer_Events);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onEvents', TalLogType.verbose);
  {$ENDIF}
end;

{******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTimelineChanged(timeline: JTimeline; reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onTimelineChanged', TalLogType.verbose);
  {$ENDIF}
end;

{************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMediaItemTransition(mediaItem: JMediaItem; reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onMediaItemTransition', TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTracksChanged(tracks: JTracks);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onTracksChanged', TalLogType.verbose);
  {$ENDIF}
end;

{****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMediaMetadataChanged(mediaMetadata: JMediaMetadata);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onMediaMetadataChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaylistMetadataChanged(mediaMetadata: JMediaMetadata);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaylistMetadataChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onIsLoadingChanged(isLoading: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onIsLoadingChanged', TalLogType.verbose);
  {$ENDIF}
end;

{***********************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onLoadingChanged(isLoading: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onLoadingChanged', TalLogType.verbose);
  {$ENDIF}
end;

{**************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAvailableCommandsChanged(availableCommands: JPlayer_Commands);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onAvailableCommandsChanged', TalLogType.verbose);
  {$ENDIF}
end;

{***********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTrackSelectionParametersChanged(parameters: JTrackSelectionParameters);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onTrackSelectionParametersChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayerStateChanged(playWhenReady: boolean; playbackState: Integer);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerControl = nil then exit;

  //int STATE_IDLE = 1;  => The player does not have any media to play.
  //int STATE_BUFFERING = 2; => The player is not able to immediately play from its current position. This state typically occurs when more data needs to be loaded.
  //int STATE_READY = 3; => The player is able to immediately play from its current position. The player will be playing if getPlayWhenReady() is true, and paused otherwise.
  //int STATE_ENDED = 4; => The player has finished playing the media.

  {$IF defined(DEBUG)}
  ALLog(
    'TALAndroidVideoPlayer.onPlayerStateChanged',
    'playWhenReady: ' + ALBoolToStrW(playWhenReady) + ' | ' +
    'playbackState: ' + ALIntToStrW(playbackState),
    TalLogType.verbose);
  {$ENDIF}

  if (playbackState = TJPlayer.JavaClass.STATE_READY) and
     (fVideoPlayerControl.SetState(vpsPrepared, vpsPreparing)) and
     (assigned(fVideoPlayerControl.fOnPreparedEvent)) then fVideoPlayerControl.fOnPreparedEvent(fVideoPlayerControl);

  if (playbackState = TJPlayer.JavaClass.STATE_ENDED) and
     (fVideoPlayerControl.SetState(vpsPlaybackCompleted, vpsStarted)) and
      assigned(fVideoPlayerControl.fOnCompletionEvent) then fVideoPlayerControl.fOnCompletionEvent(fVideoPlayerControl);

end;

{*********************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackStateChanged(playbackState: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackStateChanged', TalLogType.verbose);
  {$ENDIF}
end;

{**************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayWhenReadyChanged(playWhenReady: boolean; reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlayWhenReadyChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackSuppressionReasonChanged(playbackSuppressionReason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackSuppressionReasonChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onIsPlayingChanged(isPlaying: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onIsPlayingChanged', TalLogType.verbose);
  {$ENDIF}
end;

{***************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onRepeatModeChanged(repeatMode: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onRepeatModeChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onShuffleModeEnabledChanged(shuffleModeEnabled: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onShuffleModeEnabledChanged', TalLogType.verbose);
  {$ENDIF}
end;

{***************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayerError(error: JPlaybackException);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerControl = nil then exit;

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onPlayerError', TalLogType.error);
  {$ENDIF}

  if fVideoPlayerControl.SetState(vpsError) and
     assigned(fVideoPlayerControl.fOnErrorEvent) then fVideoPlayerControl.fOnErrorEvent(fVideoPlayerControl);

end;

{**********************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayerErrorChanged(error: JPlaybackException);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlayerErrorChanged', TalLogType.verbose);
  {$ENDIF}
end;

{***************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity(reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity', TalLogType.verbose);
  {$ENDIF}
end;

{*************************************************************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity(oldPosition: JPlayer_PositionInfo; newPosition: JPlayer_PositionInfo; reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity', TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackParametersChanged(playbackParameters: JPlaybackParameters);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackParametersChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSeekBackIncrementChanged(seekBackIncrementMs: Int64);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSeekBackIncrementChanged', TalLogType.verbose);
  {$ENDIF}
end;

{***********************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSeekForwardIncrementChanged(seekForwardIncrementMs: Int64);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSeekForwardIncrementChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMaxSeekToPreviousPositionChanged(maxSeekToPreviousPositionMs: Int64);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onMaxSeekToPreviousPositionChanged', TalLogType.verbose);
  {$ENDIF}
end;

{**************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSeekProcessed;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSeekProcessed', TalLogType.verbose);
  {$ENDIF}
end;

{***********************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAudioSessionIdChanged(audioSessionId: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onAudioSessionIdChanged', TalLogType.verbose);
  {$ENDIF}
end;

{**********************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAudioAttributesChanged(audioAttributes: JAudioAttributes);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onAudioAttributesChanged', TalLogType.verbose);
  {$ENDIF}
end;

{******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onVolumeChanged(volume: Single);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onVolumeChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSkipSilenceEnabledChanged(skipSilenceEnabled: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSkipSilenceEnabledChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onDeviceInfoChanged(deviceInfo: JDeviceInfo);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onDeviceInfoChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onDeviceVolumeChanged(volume: Integer; muted: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onDeviceVolumeChanged', TalLogType.verbose);
  {$ENDIF}
end;

{****************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onVideoSizeChanged(videoSize: JVideoSize);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerControl = nil then exit;

  {$IF defined(DEBUG)}
  ALLog(
    'TALAndroidVideoPlayer.onVideoSizeChanged',
    'width: ' + ALIntToStrW(videoSize.width) + ' | ' +
    'height: ' + ALIntToStrW(videoSize.height),
    TalLogType.verbose);
  {$ENDIF}

  fVideoPlayerControl.FVideoWidth := videoSize.width;
  fVideoPlayerControl.fVideoHeight := videoSize.height;
  if assigned(fVideoPlayerControl.fonVideoSizeChangedEvent) then
    fVideoPlayerControl.fonVideoSizeChangedEvent(fVideoPlayerControl, videoSize.width, videoSize.height);

end;

{****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSurfaceSizeChanged(width: Integer; height: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSurfaceSizeChanged', TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onRenderedFirstFrame;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onRenderedFirstFrame', TalLogType.verbose);
  {$ENDIF}
end;

{******************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onCues(cues: JList);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onCues', TalLogType.verbose);
  {$ENDIF}
end;

{**************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onCues(cueGroup: JCueGroup);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onCues', TalLogType.verbose);
  {$ENDIF}
end;

{******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMetadata(metadata: JMetadata);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onMetadata', TalLogType.verbose);
  {$ENDIF}
end;

{***************************************}
constructor TALAndroidVideoPlayer.Create;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //-----
  inherited create;

  //-----
  fVideoWidth := 0;
  fVideoHeight := 0;
  fState := vpsIdle;
  //-----
  fExoPlayer := TJExoPlayer_Builder.JavaClass.init(TAndroidHelper.Context).Build;
  //-----
  fonVideoSizeChangedEvent := nil;
  //----
  fDataSourceFactory := TJDefaultDataSource_Factory.javaclass.init(TAndroidHelper.Context);
  //-----
  fOnErrorEvent := nil;
  fOnPreparedEvent := nil;
  fOnCompletionEvent := nil;
  FPlayerListener := TPlayerListener.Create(Self);
  fExoPlayer.AddListener(FPlayerListener);
  //-----
  fBitmap := TalTexture.Create;
  ALInitializeEXTERNALOESTexture(fBitmap);
  fSurfaceTexture := TJSurfaceTexture.JavaClass.init(fBitmap.Handle);
  //-----
  fOnFrameAvailableEvent := nil;
  FOnFrameAvailableListener := TFrameAvailableListener.Create(Self);
  if (TJBuild_VERSION.JavaClass.SDK_INT >= 21 {LOLLIPOP}) then begin
    fHandler := TJHandler.JavaClass.init(TJLooper.javaclass.getMainLooper());
    fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener, fHandler)
  end
  else begin
    fHandler := nil;
    fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener);
  end;
  //-----
  fSurface := TJSurface.JavaClass.init(fSurfaceTexture);
  fExoPlayer.setVideoSurface(fSurface);

  //----
  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.Create', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;


{***************************************}
destructor TALAndroidVideoPlayer.Destroy;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //-----
  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found?noredirect=1#comment84156414_48577801
  fOnFrameAvailableListener.FVideoPlayerControl := nil;
  FPlayerListener.FVideoPlayerControl := nil;
  fSurfaceTexture.setOnFrameAvailableListener(nil);
  fExoPlayer.removeListener(FPlayerListener);

  //-----
  fExoPlayer.stop;
  fExoPlayer.clearVideoSurface;
  fExoPlayer.release;
  fExoPlayer := nil;
  fDataSourceFactory := nil;

  //----
  fSurface.release;
  fSurface := nil;
  fSurfaceTexture.release;
  fSurfaceTexture := nil;
  alFreeandNil(fbitmap);

  //-----
  alfreeAndNil(fOnFrameAvailableListener);
  alFreeAndNil(FPlayerListener);

  //-----
  fHandler := nil;

  //-----
  inherited;

  //----
  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.Destroy', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*******************************************************}
function TALAndroidVideoPlayer.getCurrentPosition: int64;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  Result := fExoPlayer.getCurrentPosition; // Returns the playback position in the current window, in milliseconds.
end;

{************************************************}
function TALAndroidVideoPlayer.getDuration: int64;
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  Result := fExoPlayer.getDuration; // Returns the duration of the current window in milliseconds, or TIME_UNSET if the
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
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //----
  if not SetState(vpsPreparing, vpsIdle) then raise Exception.Create('prepare can be call only in the idle state');
  var LMediaItem := TJMediaItem.JavaClass.fromUri(StringToJString(aDataSource));
  var LMediaSource: JMediaSource;

  //HLS datasource
  if ALSameTextW(ExtractFileExt(aDataSource), '.m3u8') then begin
    var LHlsMediaSourceFactory := TJHlsMediaSource_Factory.JavaClass.init(fDataSourceFactory);
    LMediaSource := LHlsMediaSourceFactory.createMediaSource(LMediaItem);
  end

  //Other datasource
  else begin
    var LProgressiveMediaSourceFactory := TJProgressiveMediaSource_Factory.JavaClass.init(fDataSourceFactory);
    LMediaSource := LProgressiveMediaSourceFactory.createMediaSource(LMediaItem);
  end;

  //-----
  fExoPlayer.setMediaSource(LMediaSource);
  fExoPlayer.prepare;

  //----
  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.prepare', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;

{************************************}
procedure TALAndroidVideoPlayer.pause;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsPaused, vpsStarted) then exit;
  fExoPlayer.setPlayWhenReady(false); // Sets whether playback should proceed when getPlaybackState() == STATE_READY.
                                      // If the player is already in the ready state then this method can be used to pause and resume
                                      // playback

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.pause', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;

{************************************}
procedure TALAndroidVideoPlayer.start;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsStarted, [vpsPrepared, vpsPaused, vpsPlaybackCompleted]) then exit;
  fExoPlayer.setPlayWhenReady(true); // Sets whether playback should proceed when getPlaybackState() == STATE_READY.
                                     // If the player is already in the ready state then this method can be used to pause and resume
                                     // playback

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.start', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;

{***********************************}
procedure TALAndroidVideoPlayer.stop;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsStopped, [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  fExoPlayer.stop;   // Stops playback. Use setPlayWhenReady(false) rather than this method if the intention
                     // is to pause playback.
                     // Calling this method will cause the playback state to transition to STATE_IDLE. The
                     // player instance can still be used, and release() must still be called on the player if
                     // it's no longer required.
                     // Calling this method does not reset the playback position.

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.stop', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************************************}
procedure TALAndroidVideoPlayer.seekTo(const msec: Int64);
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  fExoPlayer.seekTo(msec); // The seek position in the current window, or TIME_UNSET to seek to the window's default position.
end;

{*****************************************************************}
procedure TALAndroidVideoPlayer.setLooping(const looping: Boolean);
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //int REPEAT_MODE_OFF = 0; => Normal playback without repetition.
  //int REPEAT_MODE_ONE = 1; => repeat the currently playing window infinitely.
  //int REPEAT_MODE_ALL = 2; => repeat the entire timeline infinitely.

  if looping then fExoPlayer.setRepeatMode(TJPlayer.JavaClass.REPEAT_MODE_ALL)
  else fExoPlayer.setRepeatMode(TJPlayer.JavaClass.REPEAT_MODE_OFF);

  {$IFDEF DEBUG}
  ALLog(
    'TALAndroidVideoPlayer.setLooping',
    'looping: ' + ALBoolToStrW(looping) + ' | ' +
    'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW),
    TalLogType.VERBOSE);
  {$ENDIF}
end;

{*************************************************************}
procedure TALAndroidVideoPlayer.setVolume(const Value: Single);
var LVolume: Single;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  LVolume := Value;
  if LVolume < 0 then LVolume := 0
  else if LVolume > 1 then LVolume := 1;
  fExoPlayer.setVolume(LVolume); // Sets the audio volume, with 0 being silence and 1 being unity gain.

  {$IFDEF DEBUG}
  ALLog(
    'TALAndroidVideoPlayer.setVolume',
    'Value: ' + ALFloatToStrW(Value, ALDefaultFormatSettingsW) + ' | ' +
    'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW),
    TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************************************************}
procedure TALAndroidVideoPlayer.setPlaybackSpeed(const Value: single);
var LPlaybackParameters: JPlaybackParameters;
begin
  LPlaybackParameters := tJPlaybackParameters.JavaClass.init(Value, 1);
  fExoPlayer.setPlaybackParameters(LPlaybackParameters);
  LPlaybackParameters := Nil;
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

{**********************************************************************************************}
constructor TALIOSVideoPlayer.TKVODelegate.Create(const aVideoPlayerControl: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{********************************************************************************************************************************************}
procedure TALIOSVideoPlayer.TKVODelegate.observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer);
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.observeValueForKeyPath', 'Status:' +  ALIntToStrW(TNSNumber.Wrap(change.allvalues.objectAtIndex(0)).integerValue()), TalLogType.VERBOSE);
  {$ENDIF}

  fVideoPlayerControl.doOnReady;

end;

{********************************************************************************************************}
constructor TALIOSVideoPlayer.TNotificationsDelegate.Create(const aVideoPlayerControl: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{************************************************}
//Posted when the item has played to its end time.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemDidPlayToEndTime;
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemDidPlayToEndTime', TalLogType.VERBOSE);
  {$ENDIF}

  fVideoPlayerControl.DoOnItemDidPlayToEndTime;

end;

{****************************************************}
//Posted when the item failed to play to its end time.
//The user info dictionary contains an error object that describes
//the problem—see AVPlayerItemFailedToPlayToEndTimeErrorKey.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemFailedToPlayToEndTime;
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemFailedToPlayToEndTime', TalLogType.VERBOSE);
  {$ENDIF}

  fVideoPlayerControl.DoOnItemFailedToPlayToEndTime;

end;

{****************************************************************}
//Posted when the item’s current time has changed discontinuously.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemTimeJumped;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemTimeJumped', TalLogType.VERBOSE);
  {$ENDIF}
end;

{*******************************************************************}
//Posted when some media did not arrive in time to continue playback.
//The notification’s object is the AVPlayerItem instance whose playback was
//unable to continue because the necessary streaming media wasn’t delivered
//in a timely fashion over a network. Playback of streaming media continues
//once a sufficient amount of data is delivered. File-based playback does
//not continue.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemPlaybackStalled;
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemPlaybackStalled', TalLogType.VERBOSE);
  {$ENDIF}

  //we do nothing here, the fVideoPlayerControl.FFrameRefreshTimer will do himself the job
  //to detect when the playback stalled and will do himself a pause / restart

end;

{**************************************************}
//Posted when a new access log entry has been added.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemNewAccessLogEntry;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemNewAccessLogEntry', TalLogType.VERBOSE);
  {$ENDIF}
end;

{*************************************************}
//Posted when a new error log entry has been added.
//The notification’s object is the player item. The new log entry is
//accessible via errorLog(), respectively.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemNewErrorLogEntry;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemNewErrorLogEntry', TalLogType.VERBOSE);
  {$ENDIF}
end;

{***********************************}
constructor TALIOSVideoPlayer.Create;
var LAudioSession: AVAudioSession;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  inherited;

  //-----
  fState := vpsIdle;
  fLooping := False;
  fVolume := -2;

  //-----
  if not appAudioSessionActivated then begin
    appAudioSessionActivated := True;
    LAudioSession := TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance);
    LAudioSession.setCategory(AVAudioSessionCategoryPlayback, nil);
    LAudioSession.setActive(True, nil);
  end;

  //-----
  fBitmap := TalTexture.Create;
  fBitmap.PixelFormat := TCustomContextIOS.PixelFormat;

  //-----
  fTextureRef := 0;
  if CVOpenGLESTextureCacheCreate(
       kCFAllocatorDefault, // allocator: The CFAllocatorRef to use for allocating the texture cache. This parameter can be NULL.
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
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Create', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;

{***********************************}
destructor TALIOSVideoPlayer.Destroy;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
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
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Destroy', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
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
                      var LURL: NSUrl;
                          LLowerDataSource: String;
                          P: Pointer;
                      begin

                        try

                          {$IFDEF DEBUG}
                          var LStopWatch := TstopWatch.StartNew;
                          {$ENDIF}

                          LLowerDataSource := AlLowerCase(aDataSource);
                          if (ALPosW('http://',LLowerDataSource) = 1) or
                             (ALPosW('https://',LLowerDataSource) = 1) then P := TNSUrl.OCClass.URLWithString(StrToNSStr(aDataSource)) // Creates and returns an NSURL object initialized with a provided URL string
                          else P := TNSUrl.OCClass.fileURLWithPath(StrToNSStr(aDataSource)); // Initializes and returns a newly created NSURL object as a file URL with a specified path.
                          if P = nil then exit; // << we can't call synchronize from here (else possible trouble when we will free the object) so we can't call onErrorEvent :(
                          LURL := TNSUrl.Wrap(P);
                          FPlayerItem := TAVPlayerItem.Wrap(TAVPlayerItem.OCClass.playerItemWithURL(LURL)); // return A new player item, prepared to use URL.
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
                          fNotificationsDelegate := TNotificationsDelegate.Create(self);
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemDidPlayToEndTime'), StringToID('AVPlayerItemDidPlayToEndTimeNotification'), (FPlayerItem as ILocalObject).GetObjectID);
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemFailedToPlayToEndTime'), StringToID('AVPlayerItemFailedToPlayToEndTimeNotification'), (FPlayerItem as ILocalObject).GetObjectID);
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemTimeJumped'), StringToID('AVPlayerItemTimeJumpedNotification'), (FPlayerItem as ILocalObject).GetObjectID);
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemPlaybackStalled'), StringToID('AVPlayerItemPlaybackStalledNotification'), (FPlayerItem as ILocalObject).GetObjectID);
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewAccessLogEntry'), StringToID('AVPlayerItemNewAccessLogEntryNotification'), (FPlayerItem as ILocalObject).GetObjectID);
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewErrorLogEntry'), StringToID('AVPlayerItemNewErrorLogEntryNotification'), (FPlayerItem as ILocalObject).GetObjectID);

                          //-----
                          FKVODelegate := TKVODelegate.Create(self);
                          FPlayer.addObserver(
                            TNSObject.Wrap(FKVODelegate.GetObjectID), // observer: The object to register for KVO notifications. The observer must implement the key-value observing method observeValue(forKeyPath:of:change:context:).
                            StrToNSStr('status'), // keyPath: The key path, relative to the object receiving this message, of the property to observe. This value must not be nil.
                            NSKeyValueObservingOptionNew, // options: A combination of the NSKeyValueObservingOptions values that specifies what is included in observation notifications. For possible values, see NSKeyValueObservingOptions.
                            nil); // context: Arbitrary data that is passed to observer in observeValue(forKeyPath:of:change:context:).
                          FPlayerItem.addObserver(
                            TNSObject.Wrap(FKVODelegate.GetObjectID), // observer: The object to register for KVO notifications. The observer must implement the key-value observing method observeValue(forKeyPath:of:change:context:).
                            StrToNSStr('status'), // keyPath: The key path, relative to the object receiving this message, of the property to observe. This value must not be nil.
                            NSKeyValueObservingOptionNew, // options: A combination of the NSKeyValueObservingOptions values that specifies what is included in observation notifications. For possible values, see NSKeyValueObservingOptions.
                            nil); // context: Arbitrary data that is passed to observer in observeValue(forKeyPath:of:change:context:).

                          //-----
                          {$IFDEF DEBUG}
                          LStopWatch.Stop;
                          ALLog(
                            'TALIOSVideoPlayer.prepare',
                            'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' | ' +
                            'FPlayer.status: ' + ALIntToStrW(FPlayer.status) + ' | ' +
                            'FPlayerItem.status: ' + ALIntToStrW(FPlayerItem.status),
                            TalLogType.VERBOSE);
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
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsPaused, vpsStarted) then exit;
  FPlayer.pause; // Pauses playback of the current item.
                 // Calling this method is the same as setting the rate to 0.0.
  FFrameRefreshTimer.Enabled := False;

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.pause', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************}
procedure TALIOSVideoPlayer.Start;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsStarted, [vpsPrepared, vpsPaused, vpsPlaybackCompleted]) then exit;
  FPlayer.play; // Begins playback of the current item.
                // Calling this method is the same as setting the rate to 1.0.
  fhasNONewPixelBufferForItemTimeCounter := 0;
  FFrameRefreshTimer.Enabled := True;

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.start', 'TimeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*******************************}
procedure TALIOSVideoPlayer.Stop;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  if not SetState(vpsStopped, [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  FPlayer.pause; // Pauses playback of the current item.
                 // Calling this method is the same as setting the rate to 0.0.
  FFrameRefreshTimer.Enabled := False;

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Stop', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
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
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  fVolume := Value;
  if fVolume < 0 then fVolume := 0
  else if fVolume > 1 then fVolume := 1;
  if (FPlayerItem <> nil) then FPlayer.setVolume(fVolume); // The audio playback volume for the player, ranging from 0.0 through 1.0 on a linear scale.
                                                           // A value of 0.0 indicates silence; a value of 1.0 (the default) indicates full audio volume for the player instance.

  {$IFDEF DEBUG}
  ALLog(
    'TALIOSVideoPlayer.setVolume',
    'Value: ' + ALFloatToStrW(Value, ALDefaultFormatSettingsW) + ' | ' +
    'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW),
    TalLogType.VERBOSE);
  {$ENDIF}
end;

{****************************************************************}
procedure TALIOSVideoPlayer.setPlaybackSpeed(const Value: single);
begin
  // not yet implemented
end;

{************************************************************}
procedure TALIOSVideoPlayer.doOnFrameRefresh(Sender: TObject);
var LPixelBuffer: CVPixelBufferRef;
    LPrevTextureRef: CVOpenGLESTextureRef;
    LWidth, LHeight: integer;
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
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  T := fPlayer.currentTime; // << Returns the current time of the current player item
  if FPlayerItemVideoOutput.hasNewPixelBufferForItemTime(T) then begin // Returns a Boolean value indicating whether video output is available for the specified item time.
                                                                       // itemTime: The item time to query. The time value is relative to the AVPlayerItem object with which the receiver is associated.
                                                                       // Return Value: YES if there is available video output that has not been previously acquired or NO if there is not.
                                                                       // Note: This method returns YES if the video data at the specified time has not yet been acquired or is different from the video
                                                                       // that was acquired previously. If you require multiple objects to acquire video output from the same AVPlayerItem object,
                                                                       // you should create separate AVPlayerItemVideoOutput objects for each.

    LPixelBuffer := FPlayerItemVideoOutput.copyPixelBufferForItemTime(T, nil); // Acquires and returns an image that is appropriate to display at the specified time.
                                                                               // itemTime: The time at which you want to retrieve the image from the item.
                                                                               // outItemTimeForDisplay: The time by which you intend to use the returned pixel buffer. You may specify nil for this
                                                                               //                        parameter if you do not have a specific deadline.
                                                                               // NODE: A pixel buffer containing the image data to display or nil if nothing should be displayed at the specified time.
                                                                               //       The caller is responsible for calling CVBufferRelease on the returned data when it is no longer needed.
    if LPixelBuffer = 0 then begin // could be nil if nothing should be displayed
      {$IFDEF DEBUG}
      ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'copyPixelBufferForItemTime:nil', TalLogType.warn);
      {$ENDIF}
      exit; // could be nil if nothing should be displayed
    end;
    try

      //-----
      LPrevTextureRef := fTextureRef;
      LWidth := CVPixelBufferGetWidth(LPixelBuffer); // Returns the width of the pixel buffer.
      LHeight := CVPixelBufferGetHeight(LPixelBuffer); // Returns the height of the pixel buffer.

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
      if CVOpenGLESTextureCacheCreateTextureFromImage(
           kCFAllocatorDefault, // allocator: The CFAllocator to use for allocating the texture object. This parameter can be NULL.
           fvideoTextureCacheRef, // textureCache: The texture cache object that will manage the texture.
           LPixelBuffer, // sourceImage: The CVImageBuffer that you want to create a texture from.
           nil,  // textureAttributes: A CFDictionary containing the attributes to be used for creating the CVOpenGLESTexture objects. This parameter can be NULL.
           GL_TEXTURE_2D, // target: The target texture. GL_TEXTURE_2D and GL_RENDERBUFFER are the only targets currently supported.
           GL_RGBA,  // internalFormat: The number of color components in the texture. Examples are GL_RGBA, GL_LUMINANCE, GL_RGBA8_OES, GL_RED, and GL_RG.
           LWidth, // width: The width of the texture image.
           LHeight, // height The height of the texture image.
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
      if LPrevTextureRef <> 0 then
        cfRElease(pointer(LPrevTextureRef));

      //-----
      {$IFNDEF ALCompilerVersionSupported}
        {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.SetSize is still the same and adjust the IFDEF'}
      {$ENDIF}
      TALTextureAccessPrivate(fBitmap).FWidth := LWidth;
      TALTextureAccessPrivate(fBitmap).FHeight := LHeight; // we can't use setsize because it's will finalise the texture
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
      CVPixelBufferRelease(LPixelBuffer);
    end;

    //-----
    fhasNONewPixelBufferForItemTimeCounter := 0;

    {$IFDEF DEBUG}
    inc(fFrameRefreshCounter);
    if fFrameRefreshCounter mod 1000 = 0 then begin
      LStopWatch.Stop;
      fFrameRefreshCounter := 0;
      ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.VERBOSE);
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
      ALLog(
        'TALIOSVideoPlayer.FrameRefreshOnTimer',
        'hasNewPixelBufferForItemTime:NO ' + ' | ' +
        'Pause and Restart ' + ' | ' +
        'currentTime: ' + ALIntToStrW(fPlayer.currentTime.value),
        TalLogType.VERBOSE);
      {$ENDIF}
      fPlayer.pause;
      fPlayer.play;
      fhasNONewPixelBufferForItemTimeCounter := 0;
    end;

  end;

end;

{************************************}
procedure TALIOSVideoPlayer.doOnReady;
var LPixelBufferAttributes: NSMutableDictionary;
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
    ALLog('TALIOSVideoPlayer.doOnReady', 'Ready', TalLogType.VERBOSE);
    {$ENDIF}

    //i need to do this here because of bug like :
    //https://forums.developer.apple.com/thread/27589
    //http://stackoverflow.com/questions/24800742/iosavplayeritemvideooutput-hasnewpixelbufferforitemtime-doesnt-work-correctly
    if FPlayerItemVideoOutput = nil then begin
      LPixelBufferAttributes := TNSMutableDictionary.Create;
      try
        LPixelBufferAttributes.setObject(TNSNumber.OCClass.numberWithInt(kCVPixelFormatType_32BGRA), Pointer(kCVPixelBufferPixelFormatTypeKey));
        FPlayerItemVideoOutput := TAVPlayerItemVideoOutput.Wrap(TAVPlayerItemVideoOutput.Alloc.initWithPixelBufferAttributes(LPixelBufferAttributes)); // Initializes and returns a video output object using the specified
                                                                                                                                                       // pixel buffer attributes.
                                                                                                                                                       // The pixel buffer attributes required for video output. For a list
                                                                                                                                                       // of pixel buffer attributes you can include in this dictionary, see
                                                                                                                                                       // the CVPixelBuffer.h header file in the Core Video framework.
      finally
        LPixelBufferAttributes.release;
        LPixelBufferAttributes := nil;
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
    ALLog('TALIOSVideoPlayer.doOnReady', 'Failed', TalLogType.VERBOSE);
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

{$REGION 'ALMacOS'}
{$IF defined(ALMacOS)}

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

{******************************************************************}
procedure TALMacOSVideoPlayer.setPlaybackSpeed(const Value: single);
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
  {$ELSEIF defined(ALMacOS)}
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

{************************************************}
function TALVideoPlayer.GetBitmap: TALRasterImage;
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
begin

  inherited paint;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  if (fVideoPlayer.bitmap = nil) or
     (fVideoPlayer.bitmap.IsEmpty) then exit;

  var LDestRect := canvas.AlignToPixel(
                     TRectF.Create(0, 0, fVideoPlayer.bitmap.Width, fVideoPlayer.bitmap.Height).
                       FitInto(LocalRect));

  TCustomCanvasGpu(Canvas).DrawTexture(
    LDestRect, // ATexRect
    TRectF.Create(
      0, 0,
      fVideoPlayer.bitmap.Width,
      fVideoPlayer.bitmap.Height), // ARect
    ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    fVideoPlayer.bitmap);
  {$endif}

end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALVideoPlayerSurface]);
end;

initialization

  RegisterFmxClasses([TALVideoPlayerSurface]);

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TALIosVideoPlayer.appAudioSessionActivated := False;
  {$ENDIF}
  {$ENDREGION}

end.
