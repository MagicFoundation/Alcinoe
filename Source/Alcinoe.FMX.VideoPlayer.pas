(*******************************************************************************
ALVideoPlayer displays video on a texture, enabling seamless integration into a
Delphi form. This approach allows you to overlay any controls on top of the
video with proper Z-ordering. In contrast, the official Delphi video players
use native video windows that simply float above the form, lacking support for
Z-order layering.
*******************************************************************************)
unit Alcinoe.FMX.VideoPlayer;

interface

{$I Alcinoe.inc}

uses
  system.Classes,
  system.UITypes,
  System.Generics.Collections,
  System.SyncObjs,
  System.Types,
  System.Messaging,
  System.Net.HttpClient,
  {$IF defined(DEBUG)}
  system.diagnostics,
  {$endIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.Gles2ext,
  Alcinoe.AndroidApi.AndroidX.Media3,
  {$endIF}
  {$IF defined(IOS)}
  System.TypInfo,
  Macapi.ObjectiveC,
  iOSapi.QuartzCore,
  iOSapi.Foundation,
  iOSapi.AVFoundation,
  iOSapi.CoreVideo,
  Alcinoe.iOSapi.AVFoundation,
  Alcinoe.iOSapi.CoreVideo,
  Alcinoe.FMX.Ani,
  {$endIF}
  {$IF defined(ALSkiaCanvas)}
  System.Skia.API,
  {$ENDIF}
  FMX.Platform,
  Alcinoe.FMX.CacheEngines,
  Alcinoe.fmx.Common,
  Alcinoe.FMX.Types3D,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Objects;

type

  TALVideoSizeChangedEvent = procedure(const Sender: TObject; const width: Integer; const height: Integer) of object;

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

  {*********************************}
  TALBaseVideoPlayer = class(TObject)
  protected
    fOnErrorEvent: TNotifyEvent;
    FOnPreparedEvent: TnotifyEvent;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fOnVideoSizeChangedEvent: TALVideoSizeChangedEvent;
    FTag: NativeInt;
    function GetState: Integer; virtual; abstract;
    function GetIsPlaying: boolean; virtual; abstract;
    function GetLooping: Boolean; virtual; abstract;
    procedure SetLooping(const Value: Boolean); virtual; abstract;
    function GetVolume: Single; virtual; abstract;
    procedure SetVolume(const Value: Single); virtual; abstract;
    function GetPlaybackSpeed: single; virtual; abstract;
    procedure SetPlaybackSpeed(const Value: single); virtual; abstract;
    function GetDrawable: TALDrawable; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetCurrentPosition: Int64; virtual; abstract;
    function GetDuration: Int64; virtual; abstract;
    function GetVideoHeight: Integer; virtual; abstract;
    function GetVideoWidth: Integer; virtual; abstract;
    procedure Prepare(Const ADataSource: String); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure SeekTo(const msec: Int64); virtual; abstract;
    property State: Integer read GetState;
    property IsPlaying: boolean read GetIsPlaying;
    property Looping: Boolean read GetLooping write SetLooping;
    property Volume: Single read GetVolume write SetVolume;
    property PlaybackSpeed: Single read GetPlaybackSpeed write SetPlaybackSpeed;
    property Drawable: TALDrawable read GetDrawable;
    property OnError: TNotifyEvent read FOnErrorEvent write FOnErrorEvent;
    property OnPrepared: TNotifyEvent read FOnPreparedEvent write FOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read FOnFrameAvailableEvent write FOnFrameAvailableEvent;
    property OnCompletion: TNotifyEvent read FOnCompletionEvent write FOnCompletionEvent;
    property OnVideoSizeChanged: TALVideoSizeChangedEvent read FOnVideoSizeChangedEvent write FOnVideoSizeChangedEvent;
    property Tag: NativeInt read FTag write FTag;
  end;

  TALDummyVideoPlayer = class(TALBaseVideoPlayer)
  private
    FLooping: Boolean;
    FVolume: Single;
    FPlaybackSpeed: single;
  protected
    function GetState: Integer; override;
    function GetIsPlaying: boolean; override;
    function GetLooping: Boolean; override;
    procedure SetLooping(const Value: Boolean); override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    function GetPlaybackSpeed: single; override;
    procedure SetPlaybackSpeed(const Value: single); override;
    function GetDrawable: TALDrawable; override;
  public
    constructor Create; override;
    function GetCurrentPosition: Int64; override;
    function GetDuration: Int64; override;
    function GetVideoHeight: Integer; override;
    function GetVideoWidth: Integer; override;
    procedure Prepare(Const ADataSource: String); override;
    procedure Start; override;
    procedure Pause; override;
    procedure Stop; override;
    procedure SeekTo(const msec: Int64); override;
  end;

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  TALAndroidVideoPlayer = class(TALBaseVideoPlayer)
  private

    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TFrameAvailableListener = class(TJavaLocal, JSurfaceTexture_OnFrameAvailableListener)
      private
        FVideoPlayerEngine: TALAndroidVideoPlayer;
      public
        constructor Create(const AVideoPlayerEngine: TALAndroidVideoPlayer);
        procedure onFrameAvailable(surfaceTexture: JSurfaceTexture); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TPlayerListener = class(TJavaLocal, JPlayer_Listener)
      private
        FVideoPlayerEngine: TALAndroidVideoPlayer;
      public
        constructor Create(const AVideoPlayerEngine: TALAndroidVideoPlayer);
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
    fSurfaceTexture: JSurfaceTexture;
    fSurface: JSurface;
    FOnFrameAvailableListener: TFrameAvailableListener;
    FPlayerListener: TPlayerListener;
    {$IF defined(ALSkiaCanvas)}
    fGrBackEndTexture: gr_backendtexture_t;
    fTexture: TALTexture;
    {$ENDIF}
    fDrawable: TALDrawable;
    FDrawableReady: Boolean;
    fVideoWidth: Integer;
    fVideoHeight: Integer;
    fState: Integer;
    FAutoStartWhenPrepared: Boolean;
    function SetState(const aValue: Integer): boolean; overload;
    function SetState(const aValue: Integer; const aCompareAnd: Integer): boolean; overload;
    function SetState(const aValue: Integer; const aCompareAnd: array of Integer): boolean; overload;
  protected
    function GetState: Integer; override;
    function GetIsPlaying: boolean; override;
    function GetLooping: Boolean; override;
    procedure SetLooping(const Value: Boolean); override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    function GetPlaybackSpeed: single; override;
    procedure SetPlaybackSpeed(const Value: single); override;
    function GetDrawable: TALDrawable; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetCurrentPosition: Int64; override;
    function GetDuration: Int64; override;
    function GetVideoHeight: Integer; override;
    function GetVideoWidth: Integer; override;
    procedure Prepare(Const ADataSource: String); override;
    procedure Start; override;
    procedure Pause; override;
    procedure Stop; override;
    procedure SeekTo(const msec: Int64); override;
  end;
  {$endIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TALIOSVideoPlayer = class(TALBaseVideoPlayer)
  private
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      IDisplayLinkListener = interface(NSObject)
      ['{7A4B5B03-EA4A-49D8-8692-145EC510C438}']
        procedure displayLinkUpdated; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TDisplayLinkListener = class(TOCLocal)
      private
        FVideoPlayerEngine: TALIOSVideoPlayer;
      protected
        function GetObjectiveCClass: PTypeInfo; override;
      public
        constructor Create(const AVideoPlayerEngine: TALIOSVideoPlayer);
        procedure displayLinkUpdated; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      IKVODelegate = interface(IObjectiveC)
        ['{B0C6720A-79B7-4FD6-B9E3-77594E905B3C}']
        procedure observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TKVODelegate = class(TOCLocal, IKVODelegate)
      private
        FVideoPlayerEngine: TALIOSVideoPlayer;
      public
        constructor Create(const AVideoPlayerEngine: TALIOSVideoPlayer);
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
        FVideoPlayerEngine: TALIOSVideoPlayer;
      public
        constructor Create(const AVideoPlayerEngine: TALIOSVideoPlayer);
        procedure ItemDidPlayToEndTime; cdecl;
        procedure ItemFailedToPlayToEndTime; cdecl;
        procedure ItemTimeJumped; cdecl;
        procedure ItemPlaybackStalled; cdecl;
        procedure ItemNewAccessLogEntry; cdecl;
        procedure ItemNewErrorLogEntry; cdecl;
      end;

  private
    class var AppAudioSessionLock: Tobject;
    class var AppAudioSessionActivated: Boolean;
  private
    FPlayer: AVPlayer;
    FPlayerItem: ALAVPlayerItem;
    FPlayerItemVideoOutput: AVPlayerItemVideoOutput;
    fDisplayLink: CADisplayLink;
    fDisplayLinkListener: TDisplayLinkListener;
    FKVODelegate: TKVODelegate;
    fNotificationsDelegate: TNotificationsDelegate;
    fhasNONewPixelBufferForItemTimeCounter: Integer;
    {$IF defined(ALSkiaCanvas)}
    fGrBackEndTexture: gr_backendtexture_t;
    {$ENDIF}
    fDrawable: TALDrawable;
    FDrawableReady: Boolean;
    fOpenGLTextureRef: CVOpenGLESTextureRef;
    fOpenGLVideoTextureCacheRef: CVOpenGLESTextureCacheRef;
    fMetalTextureRef: CVMetalTextureRef;
    fMetalVideoTextureCacheRef: CVMetalTextureCacheRef;
    fState: Integer;
    FAutoStartWhenPrepared: Boolean;
    fLooping: boolean;
    fVolume: single;
    procedure DoOnFrameRefresh;
    procedure DoOnReady;
    procedure DoOnItemDidPlayToEndTime;
    procedure DoOnItemFailedToPlayToEndTime;
    function SetState(const aValue: Integer): boolean; overload;
    function SetState(const aValue: Integer; const aCompareAnd: Integer): boolean; overload;
    function SetState(const aValue: Integer; const aCompareAnd: array of Integer): boolean; overload;
  protected
    function GetState: Integer; override;
    function GetIsPlaying: boolean; override;
    function GetLooping: Boolean; override;
    procedure SetLooping(const Value: Boolean); override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    function GetPlaybackSpeed: single; override;
    procedure SetPlaybackSpeed(const Value: single); override;
    function GetDrawable: TALDrawable; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetCurrentPosition: Int64; override;
    function GetDuration: Int64; override;
    function GetVideoHeight: Integer; override;
    function GetVideoWidth: Integer; override;
    procedure Prepare(Const ADataSource: String); override;
    procedure Start; override;
    procedure Pause; override;
    procedure Stop; override;
    procedure SeekTo(const msec: Int64); override;
  end;
  {$endIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF defined(MSWINDOWS)}
  TALWinVideoPlayer = class(TALDummyVideoPlayer);
  {$endIF}
  {$ENDREGION}

  {$REGION 'ALMacOS'}
  {$IF defined(ALMacOS)}
  TALMacOSVideoPlayer = class(TALDummyVideoPlayer);
  {$endIF}
  {$ENDREGION}

type

  {********************}
  {$IF defined(android)}
  TALVideoPlayer = class(TALAndroidVideoPlayer);
  {$ELSEIF defined(IOS)}
  TALVideoPlayer = class(TALIOSVideoPlayer);
  {$ELSEIF defined(MSWINDOWS)}
  TALVideoPlayer = class(TALWinVideoPlayer);
  {$ELSEIF defined(ALMacOS)}
  TALVideoPlayer = class(TALMacOSVideoPlayer);
  {$ENDIF}

  {*********************************************}
  TALAsyncVideoPlayer = class(TALBaseVideoPlayer)
  private
    FEngineIndex: Integer;
    FCoreVideoPlayer: TALBaseVideoPlayer;
  protected
    function GetState: Integer; override;
    function GetIsPlaying: boolean; override;
    function GetLooping: Boolean; override;
    procedure SetLooping(const Value: Boolean); override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    function GetPlaybackSpeed: single; override;
    procedure SetPlaybackSpeed(const Value: single); override;
    function GetDrawable: TALDrawable; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetCurrentPosition: Int64; override;
    function GetDuration: Int64; override;
    function GetVideoHeight: Integer; override;
    function GetVideoWidth: Integer; override;
    procedure Prepare(Const ADataSource: String); override;
    procedure Start; override;
    procedure Pause; override;
    procedure Stop; override;
    procedure SeekTo(const msec: Int64); override;
  end;

  {*********************************************}
  TALVideoPlayerControllerThread = class(TThread)
  private
    class function CreateInstance: TALVideoPlayerControllerThread;
    class function GetInstance: TALVideoPlayerControllerThread; static;
  protected
    class var FInstance: TALVideoPlayerControllerThread;
  public
    type
      TCreateInstanceFunc = function: TALVideoPlayerControllerThread;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALVideoPlayerControllerThread read GetInstance;
    class function HasInstance: Boolean; inline;
  private
  {$IF defined(ANDROID)}
  private
    type
      THandlerCallback = class(TJavaLocal, JHandler_Callback)
      private
        FVideoPlayerControllerThread: TALVideoPlayerControllerThread;
      public
        constructor Create(const AVideoPlayerControllerThread: TALVideoPlayerControllerThread);
        function handleMessage(msg: JMessage): Boolean; cdecl;
      end;
  private
    FLooper: JLooper;
    FHandlerCallback: THandlerCallback;
    FHandler: JHandler;
  {$ENDIF}
  private
    Type
      // ----------
      // TOperation
      TOperation = (
        CreateEngine,
        ReleaseEngine,
        GetState,
        GetIsPlaying,
        GetLooping,
        SetLooping,
        GetVolume,
        SetVolume,
        GetPlaybackSpeed,
        SetPlaybackSpeed,
        GetCurrentPosition,
        GetDuration,
        GetVideoHeight,
        GetVideoWidth,
        Prepare,
        Start,
        Pause,
        Stop,
        SeekTo);
      // --------
      // TRequest
      TRequest = record
        Operation: TOperation;
        Param1Int64: Int64;
        Param1Single: Single;
        Param1Boolean: Boolean;
        Param1String: String;
      end;
      // ---------
      // TResponse
      TResponse = record
        ResultSingle: Single;
        ResultInt64: Int64;
        ResultBoolean: Boolean;
      end;
      // --------
      // TCommand
      TCommand = Class(TObject)
      public
        EngineIndex: Integer;
        WaitResponse: Boolean;
        Request: TRequest;
        Response: TResponse;
      end;
      // -------
      // TEngine
      TEngine = Class(Tobject)
      public
        EmptySlot: Boolean;
        Signal: TEvent;
        CoreVideoPlayer: TALBaseVideoPlayer;
        ProxyVideoPlayer: TALBaseVideoPlayer;
        constructor Create; virtual;
        destructor Destroy; override;
      end;
  private
    FReady: Boolean;
    FLock: TLightweightMREW;
    {$IF not defined(ANDROID)}
    FSignal: TEvent;
    {$ENDIF}
    FEngines: Tarray<TEngine>;
    FCommandQueue: TQueue<TCommand>;
    procedure ProcessCommandQueue;
    function EnqueueCommand(
               const AEngineIndex: Integer;
               const AOperation: TOperation;
               const AWaitResponse: Boolean;
               const AParamInt64: Int64;
               const AParamSingle: Single;
               const AParamBoolean: Boolean;
               const AParamString: String): TResponse; overload;
    function EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean): TResponse; overload;
    function EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean; const AParamInt64: Int64): TResponse; overload;
    function EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean; const AParamSingle: Single): TResponse; overload;
    function EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean; const AParamBoolean: Boolean): TResponse; overload;
    function EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean; const AParamString: String): TResponse; overload;
    procedure DoOnError(Sender: TObject);
    procedure DoOnPrepared(Sender: TObject);
    procedure DoOnFrameAvailable(Sender: TObject);
    procedure DoOnCompletion(Sender: TObject);
    procedure DoOnVideoSizeChanged(const Sender: TObject; const width: Integer; const height: Integer);
    Function GetEngine(const AEngineIndex: Integer): TEngine;
  protected
    procedure Execute; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AcquireEngine(const AProxyVideoPlayer: TALBaseVideoPlayer): Integer;
    procedure ReleaseEngine(const AEngineIndex: Integer);
    function GetCoreVideoPlayer(const AEngineIndex: Integer): TALBaseVideoPlayer;
    function GetState(const AEngineIndex: Integer): Integer;
    function GetIsPlaying(const AEngineIndex: Integer): boolean;
    function GetLooping(const AEngineIndex: Integer): Boolean;
    procedure SetLooping(const AEngineIndex: Integer; const Value: Boolean);
    function GetVolume(const AEngineIndex: Integer): Single;
    procedure SetVolume(const AEngineIndex: Integer; const Value: Single);
    function GetPlaybackSpeed(const AEngineIndex: Integer): single;
    procedure SetPlaybackSpeed(const AEngineIndex: Integer; const Value: single);
    function GetCurrentPosition(const AEngineIndex: Integer): Int64;
    function GetDuration(const AEngineIndex: Integer): Int64;
    function GetVideoHeight(const AEngineIndex: Integer): Integer;
    function GetVideoWidth(const AEngineIndex: Integer): Integer;
    procedure Prepare(const AEngineIndex: Integer; Const ADataSource: String);
    procedure Start(const AEngineIndex: Integer); overload;
    procedure Pause(const AEngineIndex: Integer);
    procedure Stop(const AEngineIndex: Integer);
    procedure SeekTo(const AEngineIndex: Integer; const msec: Int64);
  end;

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALVideoPlayerSurface = class(TALControl)
  public
    type
      TAutoStartMode = (None, WhenPrepared, WhenDisplayed);
  protected
    type
      TPreviewDownloadContext = Class(TObject)
      private
        Lock: TObject;
        FreeByThread: Boolean;
      public
        Owner: TALVideoPlayerSurface;
        Rect: TRectF;
        Scale: Single;
        AlignToPixel: Boolean;
        ResourceName: String;
        ResourceStream: TStream;
        WrapMode: TALImageWrapMode;
        constructor Create(const AOwner: TALVideoPlayerSurface); virtual;
        destructor Destroy; override;
      End;
  private
    class var AutoStartedVideoPlayerSurface: TALVideoPlayerSurface;
  private
    fVideoPlayerEngine: TALBaseVideoPlayer; // 8 bytes
    FDataSource: String; // 8 bytes
    fPreviewResourceName: String; // 8 bytes
    FBackgroundColor: TAlphaColor; // 4 bytes
    FLoadingColor: TAlphaColor; // 4 bytes
    FInternalState: Integer; // 4 Bytes
    FIsFirstFrame: Boolean; // 1 Byte
    FAutoStartMode: TAutoStartMode; // 1 Byte
    FWrapMode: TALImageWrapMode; // 1 bytes
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    FPreviewDownloadContext: TPreviewDownloadContext; // [MultiThread] | 8 bytes
    FFadeInDuration: Single; // 4 bytes
    FFadeInStartTimeNano: Int64; // 8 bytes
    procedure ApplicationEventHandler(const Sender: TObject; const M : TMessage);
    procedure DoOnFrameAvailable(Sender: Tobject);
    procedure setPreviewResourceName(const Value: String);
    procedure SetDataSource(const Value: String);
    procedure SetWrapMode(const Value: TALImageWrapMode);
    function GetState: Integer;
    procedure SetAutoStartMode(const Value: TAutoStartMode);
    function GetIsPlaying: boolean;
    function GetLooping: Boolean;
    procedure SetLooping(const Value: Boolean);
    function GetVolume: Single;
    procedure SetVolume(const Value: Single);
    function GetPlaybackSpeed: single;
    procedure SetPlaybackSpeed(const Value: single);
    function GetOnErrorEvent: TNotifyEvent;
    procedure SetOnErrorEvent(const Value: TNotifyEvent);
    function GetOnPreparedEvent: TNotifyEvent;
    procedure SetOnPreparedEvent(const Value: TNotifyEvent);
    function GetOnCompletionEvent: TNotifyEvent;
    procedure SetOnCompletionEvent(const Value: TNotifyEvent);
    function GetOnVideoSizeChangedEvent: TALVideoSizeChangedEvent;
    procedure SetOnVideoSizeChangedEvent(const Value: TALVideoSizeChangedEvent);
    function IsBackgroundColorStored: Boolean;
    function IsLoadingColorStored: Boolean;
    function IsFadeInDurationStored: Boolean;
    function IsDataSourceStored: Boolean;
    function IsPlaybackSpeedStored: Boolean;
    function IsVolumeStored: Boolean;
  protected
    fBufDrawable: TALDrawable; // 8 bytes
    fBufDrawableRect: TRectF; // 16 bytes
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    function GetDefaultBackgroundColor: TalphaColor; virtual;
    function GetDefaultLoadingColor: TalphaColor; virtual;
    function GetDefaultFadeInDuration: Single; virtual;
    procedure CancelPreviewDownload;
    class function CanStartPreviewDownload(var AContext: Tobject): boolean; virtual; // [MultiThread]
    class procedure HandlePreviewDownloadSuccess(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject); virtual; // [MultiThread]
    class procedure HandlePreviewDownloadError(const AErrMessage: string; var AContext: Tobject); virtual; // [MultiThread]
    class function GetPreviewDownloadPriority(const AContext: Tobject): Int64; virtual; // [MultiThread]
    class Procedure CreateBufDrawable(var AContext: TObject); overload; virtual; // [MultiThread]
    class Procedure CreateBufDrawable(
                      var ABufDrawable: TALDrawable;
                      out ABufDrawableRect: TRectF;
                      const ARect: TRectF;
                      const AScale: Single;
                      const AAlignToPixel: Boolean;
                      const AResourceName: String;
                      const AResourceStream: TStream;
                      const AWrapMode: TALImageWrapMode); overload; virtual; // [MultiThread]
    procedure Paint; override;
    procedure Loaded; override;
    procedure DoResized; override;
    property VideoPlayerEngine: TALBaseVideoPlayer read fVideoPlayerEngine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function IsReadyToDisplay: Boolean; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DefaultBackgroundColor: TAlphaColor read GetDefaultBackgroundColor;
    property DefaultLoadingColor: TAlphaColor read GetDefaultLoadingColor;
    property DefaultFadeInDuration: Single read GetDefaultFadeInDuration;
    function GetCurrentPosition: Int64;
    function GetDuration: Int64;
    function GetVideoHeight: Integer;
    function GetVideoWidth: Integer;
    procedure Start;
    procedure Pause;
    procedure Stop;
    procedure SeekTo(const msec: Int64);
    property State: Integer read GetState;
    property IsPlaying: boolean read GetIsPlaying;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    property AutoStartMode: TAutoStartMode read FAutoStartMode write SetAutoStartMode default TAutoStartMode.None;
    property BackgroundColor: TAlphaColor read fBackgroundColor write fBackgroundColor Stored IsBackgroundColorStored;
    property LoadingColor: TAlphaColor read FLoadingColor write FLoadingColor Stored IsLoadingColorStored;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DataSource: String read FDataSource Write SetDataSource stored IsDataSourceStored nodefault;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property FadeInDuration: Single read FFadeInDuration write FFadeInDuration stored IsFadeInDurationStored nodefault;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    Property Looping: Boolean read GetLooping Write SetLooping default false;
    property Margins;
    property Opacity;
    property Padding;
    Property PlaybackSpeed: Single read GetPlaybackSpeed Write SetPlaybackSpeed stored IsPlaybackSpeedStored nodefault;
    property PopupMenu;
    property Position;
    // If a file extension (e.g., .png) is detected in ResourceName, the image is loaded from the
    // specified file (With the full path of the file obtained using ALGetResourceFilename).
    // If ResourceName is a URL, the image is downloaded in the background from the internet.
    // In debug mode, the image is loaded from a file located in the /Resources/ sub-folder of the
    // project directory (with the extensions .png or .jpg).
    property PreviewResourceName: String read fPreviewResourceName write setPreviewResourceName;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    Property Volume: Single read GetVolume Write SetVolume stored IsVolumeStored nodefault;
    property Width;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode default TALImageWrapMode.Fit;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
    property OnError: TNotifyEvent read GetOnErrorEvent write SetOnErrorEvent;
    property OnPrepared: TNotifyEvent read GetOnPreparedEvent write SetOnPreparedEvent;
    property OnCompletion: TNotifyEvent read GetOnCompletionEvent write SetOnCompletionEvent;
    property OnVideoSizeChanged: TALVideoSizeChangedEvent read GetOnVideoSizeChangedEvent write SetOnVideoSizeChangedEvent;
  end;

procedure register;

implementation

uses
  system.SysUtils,
  system.Math,
  system.Math.Vectors,
  {$IF defined(ANDROID)}
  Androidapi.Helpers,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.CoreFoundation,
  iOSapi.UIKit,
  iOSapi.CoreMedia,
  iOSapi.OpenGLES,
  Macapi.Helpers,
  Macapi.ObjCRuntime,
  FMX.Context.GLES.iOS,
  FMX.Context.Metal,
  Alcinoe.iOSApi.Foundation,
  Alcinoe.iOSapi.QuartzCore,
  {$ENDIF}
  {$IF defined(ALSkiaCanvas)}
  FMX.Skia.Canvas,
  {$ENDIF}
  {$IF defined(MSWindows)}
  Winapi.Windows,
  FMX.Platform.Win,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  Fmx.graphics,
  Fmx.types,
  fmx.controls,
  fmx.Types3d,
  Alcinoe.StringUtils,
  Alcinoe.HTTP.Client,
  Alcinoe.HTTP.Client.Net.Pool,
  Alcinoe.Common;

{************************************}
constructor TALBaseVideoPlayer.Create;
begin
  {$IF defined(DEBUG)}
  ALLog(Classname+'.Create');
  {$ENDIF}
  inherited Create;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnVideoSizeChangedEvent := nil;
  FTag := 0;
end;

{************************************}
destructor TALBaseVideoPlayer.Destroy;
begin
  inherited destroy;
  {$IF defined(DEBUG)}
  ALLog(Classname+'.Destroy');
  {$ENDIF}
end;

{*************************************}
constructor TALDummyVideoPlayer.Create;
begin
  inherited;
  FLooping := False;
  FVolume := 1;
  FPlaybackSpeed := 1;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fOnVideoSizeChangedEvent := nil;
end;

{*********************************************}
function TALDummyVideoPlayer.GetState: Integer;
begin
  result := vpsIdle;
end;

{*************************************************}
function TALDummyVideoPlayer.GetIsPlaying: boolean;
begin
  result := False;
end;

{***********************************************}
function TALDummyVideoPlayer.GetLooping: Boolean;
begin
  Result := FLooping;
end;

{*************************************************************}
procedure TALDummyVideoPlayer.SetLooping(const Value: Boolean);
begin
  FLooping := Value;
end;

{*********************************************}
function TALDummyVideoPlayer.GetVolume: Single;
begin
  Result := FVolume;
end;

{***********************************************************}
procedure TALDummyVideoPlayer.SetVolume(const Value: Single);
begin
  FVolume := Value;
end;

{****************************************************}
function TALDummyVideoPlayer.GetPlaybackSpeed: Single;
begin
  Result := FPlayBackSpeed;
end;

{******************************************************************}
procedure TALDummyVideoPlayer.SetPlaybackSpeed(const Value: single);
begin
  FPlayBackSpeed := Value;
end;

{****************************************************}
function TALDummyVideoPlayer.GetDrawable: TALDrawable;
begin
  Result := ALNullDrawable;
end;

{*****************************************************}
function TALDummyVideoPlayer.GetCurrentPosition: Int64;
begin
  result := 0;
end;

{**********************************************}
function TALDummyVideoPlayer.GetDuration: Int64;
begin
  result := 0;
end;

{***************************************************}
function TALDummyVideoPlayer.GetVideoHeight: Integer;
begin
  result := 0;
end;

{**************************************************}
function TALDummyVideoPlayer.GetVideoWidth: Integer;
begin
  result := 0;
end;

{***************************************************************}
procedure TALDummyVideoPlayer.Prepare(Const ADataSource: String);
begin
end;

{**********************************}
procedure TALDummyVideoPlayer.Start;
begin
end;

{**********************************}
procedure TALDummyVideoPlayer.Pause;
begin
end;

{*********************************}
procedure TALDummyVideoPlayer.Stop;
begin
end;

{******************************************************}
procedure TALDummyVideoPlayer.SeekTo(const msec: Int64);
begin
end;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}

{****************************************************************************************************************}
constructor TALAndroidVideoPlayer.TFrameAvailableListener.Create(const AVideoPlayerEngine: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerEngine := AVideoPlayerEngine;
end;

{********************************************************************************************************}
procedure TALAndroidVideoPlayer.TFrameAvailableListener.onFrameAvailable(surfaceTexture: JSurfaceTexture);
begin

  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TFrameAvailableListener.onFrameAvailable');
  {$ENDIF}

  // https://developer.android.com/reference/android/graphics/SurfaceTexture.html
  // SurfaceTexture objects may be created on any thread. However, updateTexImage() may only be called on the
  // thread with the OpenGL ES context that contains the texture object. The frame-available callback is
  // called on an arbitrary thread, so unless special care is taken, updateTexImage() should not be called
  // directly from the callback.
  //
  // Based on this, it appears that updateTexImage can be invoked from a thread other than the current thread.
  // This operation seems to be thread safe—multithreaded OpenGL usage is already in place; however, since
  // updateTexImage takes only about 1ms, there is little advantage in running it on a different thread than
  // the main thread (which already holds the OpenGL ES context).
  //
  // NOTE: Since setOnFrameAvailableListener(SurfaceTexture.OnFrameAvailableListener listener, Handler handler)
  // is called with handler = TJHandler.JavaClass.init(TJLooper.javaclass.getMainLooper()), this event will always be
  // dispatched on the main UI thread.

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerEngine = nil then begin
    ALLog('TALAndroidVideoPlayer.TFrameAvailableListener.onFrameAvailable', 'fVideoPlayerEngine = nil', TALLogType.warn);
    exit;
  end;

  fVideoPlayerEngine.fSurfaceTexture.updateTexImage;

  {$IF defined(ALSkiaCanvas)}
  if GlobalUseVulkan then begin
    // not yet supported
    // https://stackoverflow.com/questions/78854486/how-to-create-a-vulkan-vkimage-from-an-android-surfacetexture-bound-to-exoplayer
  end
  else begin
    if (fVideoPlayerEngine.fGrBackEndTexture = 0) or
       (gr4d_backendtexture_get_width(fVideoPlayerEngine.fGrBackEndTexture) <> fVideoPlayerEngine.fVideoWidth) or
       (gr4d_backendtexture_get_height(fVideoPlayerEngine.fGrBackEndTexture) <> fVideoPlayerEngine.fVideoHeight) then begin
      if fVideoPlayerEngine.fGrBackEndTexture <> 0 then
        gr4d_backendtexture_destroy(fVideoPlayerEngine.fGrBackEndTexture);
      var LGLTextureinfo: gr_gl_textureinfo_t;
      LGLTextureinfo.target := GL_TEXTURE_EXTERNAL_OES;
      LGLTextureinfo.id := fVideoPlayerEngine.FTexture.Handle;
      LGLTextureinfo.format := GL_RGBA8_OES;
      fVideoPlayerEngine.fGrBackEndTexture := ALSkCheckHandle(
                                                gr4d_backendtexture_create_gl(
                                                  fVideoPlayerEngine.fVideoWidth, // width,
                                                  fVideoPlayerEngine.fVideoHeight, // height: int32_t;
                                                  false, // is_mipmapped: _bool;
                                                  @LGLTextureinfo)); // const texture_info: pgr_gl_textureinfo_t
    end;
    var LImageInfo := ALGetSkImageinfo(0, 0);
    ALFreeAndNilDrawable(fVideoPlayerEngine.fDrawable);
    fVideoPlayerEngine.fDrawable := sk4d_image_make_from_texture(
                                      TGrCanvas.SharedContext.GrDirectContext.handle, // context: gr_directcontext_t;
                                      fVideoPlayerEngine.fGrBackEndTexture, // const texture: gr_backendtexture_t;
                                      gr_surfaceorigin_t.TOP_LEFT_GR_SURFACEORIGIN, // origin: gr_surfaceorigin_t;
                                      LImageInfo.color_type, // color_type: sk_colortype_t;
                                      LImageInfo.alpha_type, //: sk_alphatype_t;
                                      LImageInfo.color_space, // color_space: sk_colorspace_t): sk_image_t; cdecl;
                                      nil, // proc: sk_image_texture_release_proc;
                                      nil); // proc_context: Pointer
    fVideoPlayerEngine.FDrawableReady := (fVideoPlayerEngine.fVideoWidth > 0) and
                                         (fVideoPlayerEngine.fVideoHeight > 0);
  end;
  {$ELSE}
  if (fVideoPlayerEngine.fDrawable.Width <> fVideoPlayerEngine.fVideoWidth) or
     (fVideoPlayerEngine.fDrawable.Height <> fVideoPlayerEngine.fVideoHeight) then begin
    {$IFNDEF ALCompilerVersionSupported123}
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
    TALTextureAccessPrivate(fVideoPlayerEngine.fDrawable).FWidth := fVideoPlayerEngine.fVideoWidth;
    TALTextureAccessPrivate(fVideoPlayerEngine.fDrawable).FHeight := fVideoPlayerEngine.fVideoHeight;
  end;
  fVideoPlayerEngine.FDrawableReady := (fVideoPlayerEngine.fVideoWidth > 0) and
                                       (fVideoPlayerEngine.fVideoHeight > 0);
  {$ENDIF}

  if assigned(fVideoPlayerEngine.fOnFrameAvailableEvent) then
    fVideoPlayerEngine.fOnFrameAvailableEvent(fVideoPlayerEngine);

end;

{********************************************************************************************************}
constructor TALAndroidVideoPlayer.TPlayerListener.Create(const AVideoPlayerEngine: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerEngine := AVideoPlayerEngine;
end;

{************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onEvents(player: JPlayer; events: JPlayer_Events);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onEvents');
  {$ENDIF}
end;

{******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTimelineChanged(timeline: JTimeline; reason: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onTimelineChanged');
  {$ENDIF}
end;

{************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMediaItemTransition(mediaItem: JMediaItem; reason: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onMediaItemTransition');
  {$ENDIF}
end;

{*******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTracksChanged(tracks: JTracks);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onTracksChanged');
  {$ENDIF}
end;

{****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMediaMetadataChanged(mediaMetadata: JMediaMetadata);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onMediaMetadataChanged');
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaylistMetadataChanged(mediaMetadata: JMediaMetadata);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaylistMetadataChanged');
  {$ENDIF}
end;

{*************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onIsLoadingChanged(isLoading: boolean);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onIsLoadingChanged');
  {$ENDIF}
end;

{***********************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onLoadingChanged(isLoading: boolean);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onLoadingChanged');
  {$ENDIF}
end;

{**************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAvailableCommandsChanged(availableCommands: JPlayer_Commands);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onAvailableCommandsChanged');
  {$ENDIF}
end;

{***********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTrackSelectionParametersChanged(parameters: JTrackSelectionParameters);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onTrackSelectionParametersChanged');
  {$ENDIF}
end;

{*******************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayerStateChanged(playWhenReady: boolean; playbackState: Integer);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerEngine = nil then begin
    ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlayerStateChanged', 'fVideoPlayerEngine = nil', TALLogType.warn);
    exit;
  end;

  //int STATE_IDLE = 1;  => The player does not have any media to play.
  //int STATE_BUFFERING = 2; => The player is not able to immediately play from its current position. This state typically occurs when more data needs to be loaded.
  //int STATE_READY = 3; => The player is able to immediately play from its current position. The player will be playing if getPlayWhenReady() is true, and paused otherwise.
  //int STATE_ENDED = 4; => The player has finished playing the media.

  {$IF defined(DEBUG)}
  //ALLog(
  //  'TALAndroidVideoPlayer.TPlayerListener.onPlayerStateChanged',
  //  'playWhenReady: ' + ALBoolToStrW(playWhenReady) + ' | ' +
  //  'playbackState: ' + ALIntToStrW(playbackState));
  {$ENDIF}

  if (playbackState = TJPlayer.JavaClass.STATE_READY) and
     (fVideoPlayerEngine.SetState(vpsPrepared, vpsPreparing)) and
     (assigned(fVideoPlayerEngine.fOnPreparedEvent)) then begin
    fVideoPlayerEngine.fOnPreparedEvent(fVideoPlayerEngine);
    If fVideoPlayerEngine.FAutoStartWhenPrepared then fVideoPlayerEngine.start;
  end;

  if (playbackState = TJPlayer.JavaClass.STATE_ENDED) and
     (fVideoPlayerEngine.SetState(vpsPlaybackCompleted, vpsStarted)) and
      assigned(fVideoPlayerEngine.fOnCompletionEvent) then fVideoPlayerEngine.fOnCompletionEvent(fVideoPlayerEngine);

end;

{*********************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackStateChanged(playbackState: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackStateChanged');
  {$ENDIF}
end;

{**************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayWhenReadyChanged(playWhenReady: boolean; reason: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlayWhenReadyChanged');
  {$ENDIF}
end;

{*********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackSuppressionReasonChanged(playbackSuppressionReason: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackSuppressionReasonChanged');
  {$ENDIF}
end;

{*************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onIsPlayingChanged(isPlaying: boolean);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onIsPlayingChanged');
  {$ENDIF}
end;

{***************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onRepeatModeChanged(repeatMode: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onRepeatModeChanged');
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onShuffleModeEnabledChanged(shuffleModeEnabled: boolean);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onShuffleModeEnabledChanged');
  {$ENDIF}
end;

{***************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayerError(error: JPlaybackException);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerEngine = nil then begin
    ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlayerError', 'fVideoPlayerEngine = nil', TALLogType.warn);
    exit;
  end;

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlayerError', TalLogType.error);
  {$ENDIF}

  if fVideoPlayerEngine.SetState(vpsError) and
     assigned(fVideoPlayerEngine.fOnErrorEvent) then fVideoPlayerEngine.fOnErrorEvent(fVideoPlayerEngine);

end;

{**********************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayerErrorChanged(error: JPlaybackException);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlayerErrorChanged');
  {$ENDIF}
end;

{***************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity(reason: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity');
  {$ENDIF}
end;

{*************************************************************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity(oldPosition: JPlayer_PositionInfo; newPosition: JPlayer_PositionInfo; reason: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity');
  {$ENDIF}
end;

{*******************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackParametersChanged(playbackParameters: JPlaybackParameters);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackParametersChanged');
  {$ENDIF}
end;

{*****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSeekBackIncrementChanged(seekBackIncrementMs: Int64);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onSeekBackIncrementChanged');
  {$ENDIF}
end;

{***********************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSeekForwardIncrementChanged(seekForwardIncrementMs: Int64);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onSeekForwardIncrementChanged');
  {$ENDIF}
end;

{*********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMaxSeekToPreviousPositionChanged(maxSeekToPreviousPositionMs: Int64);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onMaxSeekToPreviousPositionChanged');
  {$ENDIF}
end;

{***********************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAudioSessionIdChanged(audioSessionId: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onAudioSessionIdChanged');
  {$ENDIF}
end;

{**********************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAudioAttributesChanged(audioAttributes: JAudioAttributes);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onAudioAttributesChanged');
  {$ENDIF}
end;

{******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onVolumeChanged(volume: Single);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onVolumeChanged');
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSkipSilenceEnabledChanged(skipSilenceEnabled: boolean);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onSkipSilenceEnabledChanged');
  {$ENDIF}
end;

{*******************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onDeviceInfoChanged(deviceInfo: JDeviceInfo);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onDeviceInfoChanged');
  {$ENDIF}
end;

{*****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onDeviceVolumeChanged(volume: Integer; muted: boolean);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onDeviceVolumeChanged');
  {$ENDIF}
end;

{****************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onVideoSizeChanged(videoSize: JVideoSize);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerEngine = nil then begin
    ALLog('TALAndroidVideoPlayer.TPlayerListener.onVideoSizeChanged', 'fVideoPlayerEngine = nil', TALLogType.warn);
    exit;
  end;

  {$IF defined(DEBUG)}
  //ALLog(
  //  'TALAndroidVideoPlayer.TPlayerListener.onVideoSizeChanged',
  //  'width: ' + ALIntToStrW(videoSize.width) + ' | ' +
  //  'height: ' + ALIntToStrW(videoSize.height));
  {$ENDIF}

  fVideoPlayerEngine.FVideoWidth := videoSize.width;
  fVideoPlayerEngine.fVideoHeight := videoSize.height;
  if assigned(fVideoPlayerEngine.fOnVideoSizeChangedEvent) then
    fVideoPlayerEngine.fOnVideoSizeChangedEvent(fVideoPlayerEngine, videoSize.width, videoSize.height);

end;

{****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSurfaceSizeChanged(width: Integer; height: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onSurfaceSizeChanged');
  {$ENDIF}
end;

{*******************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onRenderedFirstFrame;
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onRenderedFirstFrame');
  {$ENDIF}
end;

{******************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onCues(cues: JList);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onCues');
  {$ENDIF}
end;

{**************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onCues(cueGroup: JCueGroup);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onCues');
  {$ENDIF}
end;

{******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMetadata(metadata: JMetadata);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALAndroidVideoPlayer.TPlayerListener.onMetadata');
  {$ENDIF}
end;

{***************************************}
constructor TALAndroidVideoPlayer.Create;
begin
  inherited;
  //--
  fVideoWidth := 0;
  fVideoHeight := 0;
  fState := vpsIdle;
  FAutoStartWhenPrepared := False;
  fOnErrorEvent := nil;
  fOnPreparedEvent := nil;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnVideoSizeChangedEvent := nil;
  //--
  {$IF defined(ALSkiaCanvas)}
  fGrBackEndTexture := 0;
  fTexture := TALTexture.Create;
  fTexture.SetSize(1,1);
  ALInitializeEXTERNALOESTexture(fTexture);
  fSurfaceTexture := TJSurfaceTexture.JavaClass.init(fTexture.Handle);
  fDrawable := ALNullDrawable;
  {$ELSE}
  fDrawable := TALTexture.Create;
  fDrawable.SetSize(1,1);
  ALInitializeEXTERNALOESTexture(fDrawable);
  fSurfaceTexture := TJSurfaceTexture.JavaClass.init(fDrawable.Handle);
  {$ENDIF}
  FDrawableReady := false;
  //--
  fExoPlayer := TJExoPlayer_Builder.JavaClass.init(TAndroidHelper.Context).Build;
  FPlayerListener := TPlayerListener.Create(Self);
  fExoPlayer.AddListener(FPlayerListener);
  //--
  fHandler := TJHandler.JavaClass.init(TJLooper.javaclass.getMainLooper());
  FOnFrameAvailableListener := TFrameAvailableListener.Create(Self);
  fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener, fHandler);
  //--
  fSurface := TJSurface.JavaClass.init(fSurfaceTexture);
  fExoPlayer.setVideoSurface(fSurface);
end;


{***************************************}
destructor TALAndroidVideoPlayer.Destroy;
begin
  Stop;
  //--
  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  fOnFrameAvailableListener.FVideoPlayerEngine := nil;
  FPlayerListener.FVideoPlayerEngine := nil;
  fSurfaceTexture.setOnFrameAvailableListener(nil);
  fExoPlayer.removeListener(FPlayerListener);
  //--
  fExoPlayer.clearVideoSurface;
  fExoPlayer.release;
  fExoPlayer := nil;
  //--
  fSurface.release;
  fSurface := nil;
  fSurfaceTexture.release;
  fSurfaceTexture := nil;
  {$IF defined(ALSkiaCanvas)}
  if fGrBackEndTexture <> 0 then
    gr4d_backendtexture_destroy(fGrBackEndTexture);
  ALFreeAndNil(fTexture);
  {$ENDIF}
  alFreeandNilDrawable(fDrawable);
  //--
  alfreeAndNil(fOnFrameAvailableListener);
  alFreeAndNil(FPlayerListener);
  //--
  fHandler := nil;
  //--
  inherited;
end;

{**********************************************************************}
function TALAndroidVideoPlayer.SetState(const aValue: Integer): boolean;
begin
  result := AtomicExchange(fState, aValue) <> aValue;
end;

{**************************************************************************************************}
function TALAndroidVideoPlayer.SetState(const aValue: Integer; const aCompareAnd: Integer): boolean;
begin
  AtomicCmpExchange(fState, aValue, aCompareAnd, Result);
end;

{***********************************************************************************************************}
function TALAndroidVideoPlayer.SetState(const aValue: Integer; const aCompareAnd: array of Integer): boolean;
begin
  result := False;
  for var I := Low(aCompareAnd) to High(aCompareAnd) do begin
    AtomicCmpExchange(fState, aValue, aCompareAnd[i], Result);
    if result then break;
  end;
end;

{***********************************************}
function TALAndroidVideoPlayer.GetState: Integer;
begin
  result := AtomicCmpExchange(fState, -1, -1);
end;

{***************************************************}
function TALAndroidVideoPlayer.GetIsPlaying: boolean;
begin
  result := GetState = vpsStarted;
end;

{*************************************************}
function TALAndroidVideoPlayer.GetLooping: Boolean;
begin
  result := fExoPlayer.GetRepeatMode = TJPlayer.JavaClass.REPEAT_MODE_ALL;
end;

{***************************************************************}
procedure TALAndroidVideoPlayer.SetLooping(const Value: Boolean);
begin
  //int REPEAT_MODE_OFF = 0; => Normal playback without repetition.
  //int REPEAT_MODE_ONE = 1; => repeat the currently playing window infinitely.
  //int REPEAT_MODE_ALL = 2; => repeat the entire timeline infinitely.
  if Value then fExoPlayer.setRepeatMode(TJPlayer.JavaClass.REPEAT_MODE_ALL)
  else fExoPlayer.setRepeatMode(TJPlayer.JavaClass.REPEAT_MODE_OFF);
end;

{***********************************************}
function TALAndroidVideoPlayer.GetVolume: Single;
begin
  Result := fExoPlayer.GetVolume;
end;

{*************************************************************}
procedure TALAndroidVideoPlayer.SetVolume(const Value: Single);
begin
  var LVolume: Single := Value;
  if LVolume < 0 then LVolume := 0
  else if LVolume > 1 then LVolume := 1;
  // Sets the audio volume, with 0 being silence and 1 being unity gain.
  fExoPlayer.SetVolume(LVolume);
end;

{******************************************************}
function TALAndroidVideoPlayer.GetPlaybackSpeed: single;
begin
  Result := fExoPlayer.getPlaybackParameters.speed;
end;

{********************************************************************}
procedure TALAndroidVideoPlayer.SetPlaybackSpeed(const Value: single);
begin
  var LPlaybackParameters := tJPlaybackParameters.JavaClass.init(Value);
  fExoPlayer.setPlaybackParameters(LPlaybackParameters);
  LPlaybackParameters := Nil;
end;

{******************************************************}
function TALAndroidVideoPlayer.GetDrawable: TALDrawable;
begin
  If FDrawableReady then Result := FDrawable
  else Result := ALNullDrawable;
end;

{*******************************************************}
function TALAndroidVideoPlayer.GetCurrentPosition: Int64;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  // Returns the playback position in the current window, in milliseconds.
  Result := fExoPlayer.GetCurrentPosition;
end;

{************************************************}
function TALAndroidVideoPlayer.GetDuration: Int64;
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  // Returns the duration of the current window in milliseconds, or TIME_UNSET if the
  // duration is not known.
  Result := fExoPlayer.GetDuration;
end;

{*****************************************************}
function TALAndroidVideoPlayer.GetVideoHeight: Integer;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  Result := fVideoHeight;
end;

{****************************************************}
function TALAndroidVideoPlayer.GetVideoWidth: Integer;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  Result := fVideoWidth;
end;

{*****************************************************************}
procedure TALAndroidVideoPlayer.Prepare(Const ADataSource: String);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.Prepare');
  {$ENDIF}
  if not SetState(vpsPreparing, vpsIdle) then raise Exception.Create('Prepare can be call only in the idle state');
  var LMediaItem := TJMediaItem.JavaClass.fromUri(StringToJString(ADataSource));
  fExoPlayer.setMediaItem(LMediaItem);
  fExoPlayer.Prepare;
end;

{************************************}
procedure TALAndroidVideoPlayer.start;
begin
  if not SetState(vpsStarted, [vpsPrepared, vpsPaused, vpsPlaybackCompleted]) then begin
    if GetState = vpsPreparing then FAutoStartWhenPrepared := True;
    exit;
  end;
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.start');
  {$ENDIF}
  // Sets whether playback should proceed when getPlaybackState() == STATE_READY.
  // If the player is already in the ready state then this method can be used to pause and resume
  // playback
  fExoPlayer.setPlayWhenReady(true);
end;

{************************************}
procedure TALAndroidVideoPlayer.Pause;
begin
  FAutoStartWhenPrepared := False;
  if not SetState(vpsPaused, vpsStarted) then exit;
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.Pause');
  {$ENDIF}
  // Sets whether playback should proceed when getPlaybackState() == STATE_READY.
  // If the player is already in the ready state then this method can be used to pause and resume
  // playback
  fExoPlayer.setPlayWhenReady(false);
end;

{***********************************}
procedure TALAndroidVideoPlayer.stop;
begin
  FAutoStartWhenPrepared := False;
  if not SetState(vpsStopped, [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.stop');
  {$ENDIF}
  // Stops playback. Use setPlayWhenReady(false) rather than this method if the intention
  // is to pause playback.
  // Calling this method will cause the playback state to transition to STATE_IDLE. The
  // player instance can still be used, and release() must still be called on the player if
  // it's no longer required.
  // Calling this method does not reset the playback position.
  fExoPlayer.stop;
end;

{********************************************************}
procedure TALAndroidVideoPlayer.SeekTo(const msec: Int64);
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  fExoPlayer.SeekTo(msec); // The seek position in the current window, or TIME_UNSET to seek to the window's default position.
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{*****************************************************************************************************}
constructor TALIOSVideoPlayer.TDisplayLinkListener.Create(const AVideoPlayerEngine: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerEngine := AVideoPlayerEngine;
end;

{******************************************************************}
procedure TALIOSVideoPlayer.TDisplayLinkListener.displayLinkUpdated;
begin
  {$IFDEF DEBUG}
  //ALLog('TALIOSVideoPlayer.TDisplayLinkListener.displayLinkUpdated');
  {$ENDIF}
  fVideoPlayerEngine.DoOnFrameRefresh;
end;

{****************************************************************************}
function TALIOSVideoPlayer.TDisplayLinkListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IDisplayLinkListener);
end;

{*********************************************************************************************}
constructor TALIOSVideoPlayer.TKVODelegate.Create(const AVideoPlayerEngine: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerEngine := AVideoPlayerEngine;
end;

{********************************************************************************************************************************************}
procedure TALIOSVideoPlayer.TKVODelegate.observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer);
begin
  var LkeyPath := NSStrToStr(keyPath);
  if LkeyPath = 'presentationSize' then begin
    var LNewSizeValue := iOSapi.UIKit.TNSValue.Wrap(change.objectForKey((NSKeyValueChangeNewKey as ILocalObject).GetObjectID)).CGSizeValue;
    {$IF defined(DEBUG)}
    //ALLog(
    //  'TALIOSVideoPlayer.TKVODelegate.observeValueForKeyPath',
    //  'presentationSize | ' +
    //  'width: ' + ALFloatToStrW(LNewSizeValue.width, AlDefaultFormatSettingsW) + ' | ' +
    //  'height: ' + ALFloatToStrW(LNewSizeValue.height, AlDefaultFormatSettingsW));
    {$ENDIF}
    if assigned(fVideoPlayerEngine.fOnVideoSizeChangedEvent) then
      fVideoPlayerEngine.fOnVideoSizeChangedEvent(fVideoPlayerEngine, round(LNewSizeValue.width), round(LNewSizeValue.height));
  end
  else if LkeyPath = 'status' then begin
    {$IF defined(DEBUG)}
    //ALLog('TALIOSVideoPlayer.TKVODelegate.observeValueForKeyPath', 'status');
    {$ENDIF}
    fVideoPlayerEngine.DoOnReady;
  end;
end;

{*******************************************************************************************************}
constructor TALIOSVideoPlayer.TNotificationsDelegate.Create(const AVideoPlayerEngine: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerEngine := AVideoPlayerEngine;
end;

{************************************************}
//Posted when the item has played to its end time.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemDidPlayToEndTime;
begin
  {$IF defined(DEBUG)}
  //allog('TALIOSVideoPlayer.ItemDidPlayToEndTime');
  {$ENDIF}
  fVideoPlayerEngine.DoOnItemDidPlayToEndTime;
end;

{****************************************************}
//Posted when the item failed to play to its end time.
//The user info dictionary contains an error object that describes
//the problem—see AVPlayerItemFailedToPlayToEndTimeErrorKey.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemFailedToPlayToEndTime;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemFailedToPlayToEndTime', TALLogType.Error);
  {$ENDIF}
  fVideoPlayerEngine.DoOnItemFailedToPlayToEndTime;
end;

{****************************************************************}
//Posted when the item’s current time has changed discontinuously.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemTimeJumped;
begin
  {$IF defined(DEBUG)}
  //allog('TALIOSVideoPlayer.ItemTimeJumped');
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
  allog('TALIOSVideoPlayer.ItemPlaybackStalled', TALLogType.warn);
  {$ENDIF}
  // we do nothing here, the DisplayLink will do himself the job
  // to detect when the playback stalled and will do himself a pause / restart
end;

{**************************************************}
//Posted when a new access log entry has been added.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemNewAccessLogEntry;
begin
  {$IF defined(DEBUG)}
  //allog('TALIOSVideoPlayer.ItemNewAccessLogEntry');
  {$ENDIF}
end;

{*************************************************}
//Posted when a new error log entry has been added.
//The notification’s object is the player item. The new log entry is
//accessible via errorLog(), respectively.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemNewErrorLogEntry;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemNewErrorLogEntry', TALLogType.Error);
  {$ENDIF}
end;

{***********************************}
constructor TALIOSVideoPlayer.Create;
begin
  inherited;
  //--
  fState := vpsIdle;
  FAutoStartWhenPrepared := False;
  fLooping := False;
  fVolume := -2;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnVideoSizeChangedEvent := nil;
  //--
  if not AppAudioSessionActivated then begin
    Tmonitor.Enter(AppAudioSessionLock);
    Try
      if not AppAudioSessionActivated then begin
        AppAudioSessionActivated := True;
        var LAudioSession := TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance);
        LAudioSession.setCategory(AVAudioSessionCategoryPlayback, nil);
        LAudioSession.setActive(True, nil);
      end;
    finally
      TMonitor.Exit(AppAudioSessionLock);
    end;
  end;
  //--
  {$IF defined(ALSkiaCanvas)}
  fGrBackEndTexture := 0;
  fDrawable := ALNullDrawable;
  {$ELSE}
  fDrawable := TalTexture.Create;
  if GlobalUseMetal then fDrawable.PixelFormat := TCustomContextMetal.PixelFormat
  else fDrawable.PixelFormat := TCustomContextIOS.PixelFormat;
  {$ENDIF}
  FDrawableReady := False;
  //--
  If GlobalUseMetal then begin
    fOpenGlVideoTextureCacheRef := 0;
    fMetalTextureRef := 0;
    if CVMetalTextureCacheCreate(
         kCFAllocatorDefault, // allocator: The memory allocator for the texture.
         nil, // cacheAttributes: A dictionary specifying options for the cache’s behavior, or NULL to use default options. For applicable keys and values, see Cache Attributes.
         NSObjectToID(TCustomContextMetal.SharedDevice), // metalDevice: The Metal device used to create texture objects.
         nil, // textureAttributes: A dictionary specifying options for creating textures from the cache, or NULL to use default options.
         @fMetalVideoTextureCacheRef) <> kCVReturnSuccess then raise Exception.Create('CVMetalTextureCacheCreate failed!'); // cacheOut: Upon return, contains the newly created texture cache. When this value is NULL, an error occurred in texture creation.
  end
  else begin
    fMetalvideoTextureCacheRef := nil;
    fOpenGLTextureRef := 0;
    if CVOpenGLESTextureCacheCreate(
         kCFAllocatorDefault, // allocator: The CFAllocatorRef to use for allocating the texture cache. This parameter can be NULL.
         nil, // cacheAttributes: A CFDictionaryRef containing the attributes of the texture cache itself. This parameter can be NULL.
         NSObjectToID(TCustomContextIOS.SharedContext), // eaglContext: The OpenGLES 2.0 context into which the texture objects will be created. OpenGLES 1.x contexts are not supported.
         nil, // textureAttributes: A CFDictionaryRef containing the attributes to be used for creating the CVOpenGLESTextureRef objects. This parameter can be NULL.
         @fOpenGlVideoTextureCacheRef) <> kCVReturnSuccess then raise Exception.Create('CVOpenGLESTextureCacheCreate failed!'); // cacheOut: A pointer to a CVOpenGLESTextureCacheRef where the newly created texture cache will be placed.
  end;
  //--
  fDisplayLinkListener := TDisplayLinkListener.Create(self);
  fDisplayLink := TCADisplayLink.Wrap(TCADisplayLink.OCClass.displayLinkWithTarget(fDisplayLinkListener.GetObjectID, sel_getUid('displayLinkUpdated')));
  fDisplayLink.retain;
  if GlobalUseMetal then begin
    // In OpenGL, the animation appears more jerky when using
    // a high frame rate
    if TOSVersion.Check(17) then begin
      var LFrameRateRange: CAFrameRateRange;
      LFrameRateRange.minimum := ALMinimumFramesPerSecond;
      LFrameRateRange.maximum := ALMaximumFramesPerSecond;
      LFrameRateRange.preferred := ALPreferredFramesPerSecond;
      TALCADisplayLink.Wrap(NSObjectToID(fDisplayLink)).setPreferredFrameRateRange(LFrameRateRange);
    end
    else
      TALCADisplayLink.Wrap(NSObjectToID(fDisplayLink)).setPreferredFramesPerSecond(ALPreferredFramesPerSecond);
  end;
  fDisplayLink.addToRunLoop(TNSRunLoop.Wrap(TNSRunLoop.OCClass.mainRunLoop), NSRunLoopCommonModes); // I don't really know with is the best, NSDefaultRunLoopMode or NSRunLoopCommonModes
  fDisplayLink.setPaused(true);
  //--
  FPlayer := nil;
  FPlayerItem := nil;
  FPlayerItemVideoOutput := nil;
  FKVODelegate := nil;
  FNotificationsDelegate := nil;
  fhasNONewPixelBufferForItemTimeCounter := 0;
end;

{***********************************}
destructor TALIOSVideoPlayer.Destroy;
begin
  Stop;
  //--
  // Removes the display link from all run loop modes.
  // Removing the display link from all run loop modes causes it to be released by the run loop. The display link also releases the target.
  // invalidate is thread safe meaning that it can be called from a thread separate to the one in which the display link is running.
  fDisplayLink.invalidate;
  fDisplayLink.release;
  AlFreeAndNil(fDisplayLinkListener);
  //--
  if fNotificationsDelegate <> nil then begin
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(FNotificationsDelegate.GetObjectID);
    AlFreeAndNil(FNotificationsDelegate);
  end;
  //--
  if FPlayer <> nil then begin
    FPlayer.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status'));
    FPlayer.release;
    FPlayer := nil;
  end;
  //--
  if FPlayerItem <> nil then begin
    FPlayerItem.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status'));
    FPlayerItem.release;
    FPlayerItem := nil;
  end;
  //--
  AlFreeAndNil(FKVODelegate);
  //--
  {$IF defined(ALSkiaCanvas)}
  if fGrBackEndTexture <> 0 then
    gr4d_backendtexture_destroy(fGrBackEndTexture);
  alFreeandNilDrawable(fDrawable);
  {$ELSE}
  ITextureAccess(fDrawable).Handle := 0;
  alfreeAndNil(fDrawable);
  {$ENDIF}
  //--
  if fOpenGLTextureRef <> 0 then CFRelease(pointer(fOpenGLTextureRef));
  // The texture cache automatically flushes currently unused resources when you call the
  // CVOpenGLESTextureCacheCreateTextureFromImage function, but can you can also flush the
  // cache explicitly by calling this function. The EAGLContext associated with the cache
  // may be used to delete or unbind textures.
  if fOpenGLvideoTextureCacheRef <> 0 then begin
    CVOpenGLESTextureCacheFlush(fOpenGLvideoTextureCacheRef, 0);
    CFrelease(pointer(fOpenGLVideoTextureCacheRef));
  end;
  //--
  if fMetalTextureRef <> 0 then CFRelease(pointer(fMetalTextureRef));
  If fMetalvideoTextureCacheRef <> nil then begin
    CVMetalTextureCacheFlush(fMetalvideoTextureCacheRef, 0);
    CFrelease(pointer(fMetalVideoTextureCacheRef));
  end;
  //--
  if FPlayerItemVideoOutput <> nil then begin
    FPlayerItemVideoOutput.release;
    FPlayerItemVideoOutput := nil;
  end;
  //--
  inherited;
end;

{*******************************************}
procedure TALIOSVideoPlayer.DoOnFrameRefresh;
begin

  // Stop the timer if we encoutered some error
  if GetState <> vpsStarted then begin
    fDisplayLink.setPaused(True);
    Exit;
  end;

  // In case
  if FPlayerItemVideoOutput = nil then exit;

  // Returns the current time of the current player item
  var T: CMTime := fPlayer.currentTime;

  // Returns a Boolean value indicating whether video output is available for the specified item time.
  // itemTime: The item time to query. The time value is relative to the AVPlayerItem object with which the receiver is associated.
  // Return Value: YES if there is available video output that has not been previously acquired or NO if there is not.
  // Note: This method returns YES if the video data at the specified time has not yet been acquired or is different from the video
  // that was acquired previously. If you require multiple objects to acquire video output from the same AVPlayerItem object,
  // you should create separate AVPlayerItemVideoOutput objects for each.
  if FPlayerItemVideoOutput.hasNewPixelBufferForItemTime(T) then begin

    // Acquires and returns an image that is appropriate to display at the specified time.
    // itemTime: The time at which you want to retrieve the image from the item.
    // outItemTimeForDisplay: The time by which you intend to use the returned pixel buffer. You may specify nil for this
    //                        parameter if you do not have a specific deadline.
    // NODE: A pixel buffer containing the image data to display or nil if nothing should be displayed at the specified time.
    //       The caller is responsible for calling CVBufferRelease on the returned data when it is no longer needed.
    var LPixelBuffer := FPlayerItemVideoOutput.copyPixelBufferForItemTime(T, nil);
    if LPixelBuffer = 0 then begin // could be nil if nothing should be displayed
      {$IFDEF DEBUG}
      ALLog('TALIOSVideoPlayer.DoOnFrameRefresh', 'copyPixelBufferForItemTime:nil', TalLogType.warn);
      {$ENDIF}
      exit; // could be nil if nothing should be displayed
    end;
    try

      var LWidth := CVPixelBufferGetWidth(LPixelBuffer);
      var LHeight := CVPixelBufferGetHeight(LPixelBuffer);
      var LPrevMetalTextureRef := fMetalTextureRef;
      var LPrevOpenGLTextureRef := fOpenGLTextureRef;

      if GlobalUseMetal then begin
        // This function creates a cached Core Video Metal texture object mapped
        // to an image buffer, and a live binding to the underlying MTLTexture object.
        // Important: You need to maintain a strong reference to textureOut until
        // the GPU finishes execution of commands accessing the texture, because
        // the system doesn’t automatically retain it. Developers typically
        // release these references in a block passed to addCompletedHandler:.
        if CVMetalTextureCacheCreateTextureFromImage(
             kCFAllocatorDefault, // allocator: The memory allocator for the texture.
             fMetalVideoTextureCacheRef, // textureCache: The texture cache used to create and manage the texture.
             LPixelBuffer, // sourceImage: The Core Video image buffer from which to create a Metal texture.
             nil, // textureAttributes: A dictionary specifying options for creating the texture from the cache, or NULL to use default options.
             PixelFormatToMTLPixelFormat(TCustomContextMetal.PixelFormat), // pixelFormat: The Metal pixel format constant describing the image buffer’s data.
             LWidth, // width: The width, in pixels, of the texture image.
             LHeight, // height: The height, in pixels, of the texture image.
             0, // planeIndex: If the image buffer is planar, the index of the plane from which to map texture data. Ignored for non-planar image buffers.
             @fMetalTextureRef) <> kCVReturnSuccess then begin // textureOut: PCVMetalTextureRef) <> kCVReturnSuccess then begin Upon return, contains the newly created Metal texture buffer. When this value is NULL, an error occurred in texture creation.
          {$IFDEF DEBUG}
          ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'CVMetalTextureCacheCreateTextureFromImage failed!', TalLogType.Error);
          {$ENDIF}
          exit;
        end;
      end
      else begin
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
             fOpenGLVideoTextureCacheRef, // textureCache: The texture cache object that will manage the texture.
             LPixelBuffer, // sourceImage: The CVImageBuffer that you want to create a texture from.
             nil,  // textureAttributes: A CFDictionary containing the attributes to be used for creating the CVOpenGLESTexture objects. This parameter can be NULL.
             GL_TEXTURE_2D, // target: The target texture. GL_TEXTURE_2D and GL_RENDERBUFFER are the only targets currently supported.
             GL_RGBA,  // internalFormat: The number of color components in the texture. Examples are GL_RGBA, GL_LUMINANCE, GL_RGBA8_OES, GL_RED, and GL_RG.
             LWidth, // width: The width of the texture image.
             LHeight, // height The height of the texture image.
             GL_BGRA_EXT,  // format: The format of the pixel data. Examples are GL_RGBA and GL_LUMINANCE.
             GL_UNSIGNED_BYTE, // type: The data type of the pixel data. One example is GL_UNSIGNED_BYTE.
             0,  // planeIndex: The plane of the CVImageBuffer to map bind. Ignored for non-planar CVImageBuffers.
             @fOpenGLTextureRef) <> kCVReturnSuccess then begin // textureOut: A pointer to a CVOpenGLESTexture where the newly created texture object will be placed.
          {$IFDEF DEBUG}
          ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'CVOpenGLESTextureCacheCreateTextureFromImage failed!', TalLogType.Error);
          {$ENDIF}
          exit;
        end;
      end;

      {$IF defined(ALSkiaCanvas)}

      if GlobalUseMetal then begin
        // The value of CVMetalTextureGetTexture(fMetalTextureRef) changes with each frame,
        // so I cannot cache fGrBackendTexture because it becomes immutable once created.
        // Fortunately, it's not a big deal since recreating it is very fast.
        //
        //if (fGrBackEndTexture = 0) or
        //   (gr4d_backendtexture_get_width(fGrBackEndTexture) <> LWidth) or
        //   (gr4d_backendtexture_get_height(fGrBackEndTexture) <> LHeight) then begin
        if fGrBackEndTexture <> 0 then
          gr4d_backendtexture_destroy(fGrBackEndTexture);
        var LGRMtlTextureInfo_t: gr_mtl_textureinfo_t;
        LGRMtlTextureInfo_t.texture := CVMetalTextureGetTexture(fMetalTextureRef);
        fGrBackEndTexture := ALSkCheckHandle(
                               gr4d_backendtexture_create_mtl(
                                 LWidth, // width,
                                 LHeight, // height: int32_t;
                                 false, // is_mipmapped: _bool;
                                 @LGRMtlTextureInfo_t)); // const texture_info: pgr_mtl_textureinfo_t
        var LImageInfo := ALGetSkImageinfo(0, 0);
        ALFreeAndNilDrawable(fDrawable);
        fDrawable := sk4d_image_make_from_texture(
                       TGrCanvas.SharedContext.GrDirectContext.handle, // context: gr_directcontext_t;
                       fGrBackEndTexture, // const texture: gr_backendtexture_t;
                       gr_surfaceorigin_t.TOP_LEFT_GR_SURFACEORIGIN, // origin: gr_surfaceorigin_t;
                       LImageInfo.color_type, // color_type: sk_colortype_t;
                       LImageInfo.alpha_type, //: sk_alphatype_t;
                       LImageInfo.color_space, // color_space: sk_colorspace_t): sk_image_t; cdecl;
                       nil, // proc: sk_image_texture_release_proc;
                       nil); // proc_context: Pointer
      end
      else begin
        // The value of CVOpenGLESTextureGetName(fOpenGLTextureRef) changes with each frame,
        // so I cannot cache fGrBackendTexture because it becomes immutable once created.
        // Fortunately, it's not a big deal since recreating it is very fast.
        //
        //if (fGrBackEndTexture = 0) or
        //   (gr4d_backendtexture_get_width(fGrBackEndTexture) <> LWidth) or
        //   (gr4d_backendtexture_get_height(fGrBackEndTexture) <> LHeight) then begin
        if fGrBackEndTexture <> 0 then
          gr4d_backendtexture_destroy(fGrBackEndTexture);
        var LGLTextureinfo: gr_gl_textureinfo_t;
        LGLTextureinfo.target := GL_TEXTURE_2D;
        LGLTextureinfo.id := CVOpenGLESTextureGetName(fOpenGLTextureRef);
        LGLTextureinfo.format := GL_RGBA8_OES;
        fGrBackEndTexture := ALSkCheckHandle(
                               gr4d_backendtexture_create_gl(
                                 LWidth, // width,
                                 LHeight, // height: int32_t;
                                 false, // is_mipmapped: _bool;
                                 @LGLTextureinfo)); // const texture_info: pgr_gl_textureinfo_t
        var LImageInfo := ALGetSkImageinfo(0, 0);
        ALFreeAndNilDrawable(fDrawable);
        fDrawable := sk4d_image_make_from_texture(
                       TGrCanvas.SharedContext.GrDirectContext.handle, // context: gr_directcontext_t;
                       fGrBackEndTexture, // const texture: gr_backendtexture_t;
                       gr_surfaceorigin_t.TOP_LEFT_GR_SURFACEORIGIN, // origin: gr_surfaceorigin_t;
                       LImageInfo.color_type, // color_type: sk_colortype_t;
                       LImageInfo.alpha_type, //: sk_alphatype_t;
                       LImageInfo.color_space, // color_space: sk_colorspace_t): sk_image_t; cdecl;
                       nil, // proc: sk_image_texture_release_proc;
                       nil); // proc_context: Pointer
      end;
      FDrawableReady := true;

      {$ELSE}

      if GlobalUseMetal then begin
        TALTextureAccessPrivate(fDrawable).FWidth := LWidth;
        TALTextureAccessPrivate(fDrawable).FHeight := LHeight;
        ITextureAccess(fDrawable).Handle := THandle(CVMetalTextureGetTexture(fMetalTextureRef));
      end
      else begin
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, CVOpenGLESTextureGetName(fOpenGLTextureRef));
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        case fDrawable.MagFilter of
          TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
          TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        end;
        case fDrawable.MinFilter of
          TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
          TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        end;
        glBindTexture(GL_TEXTURE_2D, 0);

        {$IFNDEF ALCompilerVersionSupported123}
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
        TALTextureAccessPrivate(fDrawable).FWidth := LWidth;
        TALTextureAccessPrivate(fDrawable).FHeight := LHeight;
        ITextureAccess(fDrawable).Handle := CVOpenGLESTextureGetName(fOpenGLTextureRef);
      end;
      FDrawableReady := true;

      {$ENDIF}

      if LPrevMetalTextureRef <> 0 then
        CfRElease(pointer(LPrevMetalTextureRef));
      if LPrevOpenGLTextureRef <> 0 then
        CfRElease(pointer(LPrevOpenGLTextureRef));

      if assigned(FonFrameAvailableEvent) then
        FonFrameAvailableEvent(self);

    finally
      CVPixelBufferRelease(LPixelBuffer);
    end;

    //--
    fhasNONewPixelBufferForItemTimeCounter := 0;

  end
  else begin

    // Occasionally, we continuously receive hasNewPixelBufferForItemTime:NO.
    // For example, if the Wi-Fi is disconnected during playback, the player will stall (which is expected).
    // However, when playback is resumed, hasNewPixelBufferForItemTime:NO will still be received.
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
        'currentTime: ' + ALIntToStrW(fPlayer.currentTime.value));
      {$ENDIF}
      fPlayer.Pause;
      fPlayer.play;
      fhasNONewPixelBufferForItemTimeCounter := 0;
    end;

  end;

end;

{************************************}
procedure TALIOSVideoPlayer.DoOnReady;
begin

  // The player’s status indicates whether the player can be used for playback.
  // When the value of this property is failed, you can no longer use the player
  // for playback and you need to create a new instance to replace it. If this
  // happens, you can check the value of the player’s error property to determine
  // the nature of the failure.
  // This property is key value observable using Key-value observing.
  // NOTE: The player’s status does not indicate its readiness to play a specific
  // player item. You should instead use the status property of AVPlayerItem to make
  // that determination.
  //
  // When a player item is created, its status is unknown, meaning its media hasn’t
  // been loaded and has not yet been enqueued for playback. Associating a player
  // item with an AVPlayer immediately begins enqueuing the item’s media and preparing
  // it for playback. When the player item’s media has been loaded and is ready
  // for use, its status will change to readyToPlay. You can observe this
  // change using key-value observing.
  //
  // AVPlayerItemStatusFailed = 2;
  // AVPlayerItemStatusReadyToPlay = 1;
  // AVPlayerItemStatusUnknown = 0;
  // AVPlayerStatusFailed = 2;
  // AVPlayerStatusReadyToPlay = 1;
  // AVPlayerStatusUnknown = 0;
  if (fPlayer.status = AVPlayerStatusReadyToPlay) and
     (FPlayerItem.status = AVPlayerItemStatusReadyToPlay) then begin

    {$IFDEF DEBUG}
    ALLog('TALIOSVideoPlayer.DoOnReady', 'Ready');
    {$ENDIF}

    //i need to do this here because of bug like :
    //https://forums.developer.apple.com/thread/27589
    //http://stackoverflow.com/questions/24800742/iosavplayeritemvideooutput-hasnewpixelbufferforitemtime-doesnt-work-correctly
    if FPlayerItemVideoOutput = nil then begin
      var LPixelBufferAttributes: NSMutableDictionary := TNSMutableDictionary.Create;
      try
        LPixelBufferAttributes.setObject(TNSNumber.OCClass.numberWithInt(kCVPixelFormatType_32BGRA), Pointer(kCVPixelBufferPixelFormatTypeKey));
        // Initializes and returns a video output object using the specified
        // pixel buffer attributes.
        // The pixel buffer attributes required for video output. For a list
        // of pixel buffer attributes you can include in this dictionary, see
        // the CVPixelBuffer.h header file in the Core Video framework.
        FPlayerItemVideoOutput := TAVPlayerItemVideoOutput.Wrap(TAVPlayerItemVideoOutput.Alloc.initWithPixelBufferAttributes(LPixelBufferAttributes));
      finally
        LPixelBufferAttributes.release;
        LPixelBufferAttributes := nil;
      end;
      FPlayerItemVideoOutput.retain;
      FPlayerItem.addOutput(FPlayerItemVideoOutput);
    end;

    //fire the fOnPreparedEvent
    if SetState(vpsPrepared, vpsPreparing) then begin
      if fVolume > -1 then SetVolume(fVolume);
      if assigned(fOnPreparedEvent) then fOnPreparedEvent(self);
      If FAutoStartWhenPrepared then start;
    end;

  end
  else if (fPlayer.status = AVPlayerStatusFailed) or
          (FPlayerItem.status = AVPlayerItemStatusFailed) then begin

    {$IFDEF DEBUG}
    ALLog('TALIOSVideoPlayer.DoOnReady', 'Failed');
    {$ENDIF}

    //fire the fOnErrorEvent
    if SetState(vpsError) and
       assigned(fOnErrorEvent) then fOnErrorEvent(self);

  end;

end;

{***************************************************}
procedure TALIOSVideoPlayer.DoOnItemDidPlayToEndTime;
begin
  if fLooping then begin
    FPlayer.SeekToTime(CMTimeMake(0, 1));
    FPlayer.play;
  end
  else begin
    fDisplayLink.setPaused(True);
    If SetState(vpsPlaybackCompleted, vpsStarted) and
       assigned(fOnCompletionEvent) then fOnCompletionEvent(self);
  end;
end;

{********************************************************}
procedure TALIOSVideoPlayer.DoOnItemFailedToPlayToEndTime;
begin
  if SetState(vpsError) then begin
    fDisplayLink.setPaused(True);
    if assigned(fOnErrorEvent) then fOnErrorEvent(self);
  end;
end;

{******************************************************************}
function TALIOSVideoPlayer.SetState(const aValue: Integer): boolean;
begin
  result := AtomicExchange(fState, aValue) <> aValue;
end;

{**********************************************************************************************}
function TALIOSVideoPlayer.SetState(const aValue: Integer; const aCompareAnd: Integer): boolean;
begin
  AtomicCmpExchange(fState, aValue, aCompareAnd, Result);
end;

{*******************************************************************************************************}
function TALIOSVideoPlayer.SetState(const aValue: Integer; const aCompareAnd: array of Integer): boolean;
begin
  result := False;
  for var I := Low(aCompareAnd) to High(aCompareAnd) do begin
    AtomicCmpExchange(fState, aValue, aCompareAnd[i], Result);
    if result then break;
  end;
end;

{*******************************************}
function TALIOSVideoPlayer.GetState: Integer;
begin
  result := AtomicCmpExchange(fState, -1, -1);
end;

{***********************************************}
function TALIOSVideoPlayer.GetIsPlaying: boolean;
begin
  result := GetState = vpsStarted;
end;

{************************************}
function TALIOSVideoPlayer.GetLooping;
begin
  Result := fLooping;
end;

{***********************************************************}
procedure TALIOSVideoPlayer.SetLooping(const Value: Boolean);
begin
  fLooping := Value;
end;

{*******************************************}
function TALIOSVideoPlayer.GetVolume: Single;
begin
  Result := FPlayer.volume;
end;

{*********************************************************}
procedure TALIOSVideoPlayer.SetVolume(const Value: Single);
begin
  fVolume := Value;
  if fVolume < 0 then fVolume := 0
  else if fVolume > 1 then fVolume := 1;
  // The audio playback volume for the player, ranging from 0.0 through 1.0 on a linear scale.
  // A value of 0.0 indicates silence; a value of 1.0 (the default) indicates full audio volume for the player instance.
  if (FPlayerItem <> nil) then FPlayer.SetVolume(fVolume);
end;

{**************************************************}
function TALIOSVideoPlayer.GetPlaybackSpeed: single;
begin
  Result := 1;
end;

{****************************************************************}
procedure TALIOSVideoPlayer.SetPlaybackSpeed(const Value: single);
begin
  // not yet implemented
end;

{**************************************************}
function TALIOSVideoPlayer.GetDrawable: TALDrawable;
begin
  If FDrawableReady then Result := FDrawable
  else Result := ALNullDrawable;
end;

{****************************************************}
//Returns the current time of the current player item.
function TALIOSVideoPlayer.GetCurrentPosition: Int64;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  if (FPlayerItem <> nil) then Result := Trunc(CMTimeGetSeconds(FPlayerItem.currentTime) * 1000)
  else result := 0;
end;

{********************************************}
function TALIOSVideoPlayer.GetDuration: Int64;
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  if (FPlayerItem <> nil) then Result := Trunc(CMTimeGetSeconds(FPlayerItem.duration) * 1000)
  else result := 0;
end;

{*************************************************}
function TALIOSVideoPlayer.GetVideoHeight: Integer;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  if FPlayerItem <> nil then Result := round(FPlayerItem.presentationSize.height)
  else result := 0;
end;

{************************************************}
function TALIOSVideoPlayer.GetVideoWidth: Integer;
begin
  if not (GetState in [vpsIdle, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    exit;
  end;
  if FPlayerItem <> nil then Result := round(FPlayerItem.presentationSize.width)
  else result := 0;
end;

{*************************************************************}
procedure TALIOSVideoPlayer.Prepare(Const ADataSource: String);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIOSVideoPlayer.Prepare');
  {$ENDIF}
  if not SetState(vpsPreparing, vpsIdle) then raise Exception.Create('Prepare can be call only in the idle state');
  //--
  var P: Pointer;
  if AlIsHttpOrHttpsUrl(ADataSource) then P := TNSUrl.OCClass.URLWithString(StrToNSStr(ADataSource)) // Creates and returns an NSURL object initialized with a provided URL string
  else P := TNSUrl.OCClass.fileURLWithPath(StrToNSStr(ADataSource)); // Initializes and returns a newly created NSURL object as a file URL with a specified path.
  if P = nil then begin
    ALLog('TALIOSVideoPlayer.Prepare', 'Failed to create NSURL from the provided data source (%s)', [ADataSource], TALLogType.ERROR);
    exit;
  end;
  var LURL := TNSUrl.Wrap(P);
  // return A new player item, Prepared to use URL.
  // This method immediately returns the item, but with the status AVPlayerItemStatusUnknown.
  // Associating the player item with an AVPlayer immediately begins enqueuing its media
  // and preparing it for playback. If the URL contains valid data that can be used by
  // the player item, its status later changes to AVPlayerItemStatusReadyToPlay. If the
  // URL contains no valid data or otherwise can't be used by the player item, its status
  // later changes to AVPlayerItemStatusFailed. You can determine the nature of the failure
  // by querying the player item’s error property.
  FPlayerItem := TALAVPlayerItem.Wrap(TAVPlayerItem.OCClass.playerItemWithURL(LURL));
  FPlayerItem.retain;
  //aURL.release;   | >> we can't do this else we will have an eaccessViolation when we will free the FPlayerItem
  //aURL := nil;    | >> http://stackoverflow.com/questions/42222508/why-we-need-to-do-retain-for-objective-c-object-field
  //--
  FPlayer := TAVPlayer.Wrap(TAVPlayer.OCClass.playerWithPlayerItem(FPlayerItem)); // Returns a new player initialized to play the specified player item.
  FPlayer.retain;
  //--
  fNotificationsDelegate := TNotificationsDelegate.Create(Self);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemDidPlayToEndTime'), StringToID('AVPlayerItemDidPlayToEndTimeNotification'), NSObjectToID(FPlayerItem));
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemFailedToPlayToEndTime'), StringToID('AVPlayerItemFailedToPlayToEndTimeNotification'), NSObjectToID(FPlayerItem));
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemTimeJumped'), StringToID('AVPlayerItemTimeJumpedNotification'), NSObjectToID(FPlayerItem));
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemPlaybackStalled'), StringToID('AVPlayerItemPlaybackStalledNotification'), NSObjectToID(FPlayerItem));
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewAccessLogEntry'), StringToID('AVPlayerItemNewAccessLogEntryNotification'), NSObjectToID(FPlayerItem));
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewErrorLogEntry'), StringToID('AVPlayerItemNewErrorLogEntryNotification'), NSObjectToID(FPlayerItem));
  //--
  FKVODelegate := TKVODelegate.Create(Self);
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
  FPlayerItem.addObserver(
    TNSObject.Wrap(FKVODelegate.GetObjectID), // observer: The object to register for KVO notifications. The observer must implement the key-value observing method observeValue(forKeyPath:of:change:context:).
    StrToNSStr('presentationSize'), // keyPath: The key path, relative to the object receiving this message, of the property to observe. This value must not be nil.
    NSKeyValueObservingOptionNew, // options: A combination of the NSKeyValueObservingOptions values that specifies what is included in observation notifications. For possible values, see NSKeyValueObservingOptions.
    nil); // context: Arbitrary data that is passed to observer in observeValue(forKeyPath:of:change:context:).
end;

{********************************}
procedure TALIOSVideoPlayer.Start;
begin
  if not SetState(vpsStarted, [vpsPrepared, vpsPaused, vpsPlaybackCompleted]) then begin
    if GetState = vpsPreparing then FAutoStartWhenPrepared := True;
    exit;
  end;
  {$IF defined(DEBUG)}
  ALLog('TALIOSVideoPlayer.Start');
  {$ENDIF}
  // Begins playback of the current item.
  // Calling this method is the same as setting the rate to 1.0.
  FPlayer.play;
  fhasNONewPixelBufferForItemTimeCounter := 0;
  fDisplayLink.setPaused(False);
end;

{********************************}
procedure TALIOSVideoPlayer.Pause;
begin
  FAutoStartWhenPrepared := False;
  if not SetState(vpsPaused, vpsStarted) then exit;
  {$IF defined(DEBUG)}
  ALLog('TALIOSVideoPlayer.Pause');
  {$ENDIF}
  // Pauses playback of the current item.
  // Calling this method is the same as setting the rate to 0.0.
  FPlayer.Pause;
  fDisplayLink.setPaused(True);
end;

{*******************************}
procedure TALIOSVideoPlayer.Stop;
begin
  FAutoStartWhenPrepared := False;
  if not SetState(vpsStopped, [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  {$IF defined(DEBUG)}
  ALLog('TALIOSVideoPlayer.Stop');
  {$ENDIF}
  // Pauses playback of the current item.
  // Calling this method is the same as setting the rate to 0.0.
  FPlayer.Pause;
  fDisplayLink.setPaused(True);
end;

{****************************************************}
procedure TALIOSVideoPlayer.SeekTo(const msec: Int64);
begin
  if not (GetState in [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  FPlayer.SeekToTime(CMTimeMake(msec, 1));
end;

{$ENDIF}
{$ENDREGION}

{*************************************}
constructor TALAsyncVideoPlayer.Create;
begin
  inherited;
  FEngineIndex := TALVideoPlayerControllerThread.Instance.AcquireEngine(Self);
  FCoreVideoPlayer := nil;
end;

{*************************************}
destructor TALAsyncVideoPlayer.Destroy;
begin
  TALVideoPlayerControllerThread.Instance.ReleaseEngine(FEngineIndex);
  inherited;
end;

{*********************************************}
function TALAsyncVideoPlayer.GetState: Integer;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetState(FEngineIndex);
end;

{*************************************************}
function TALAsyncVideoPlayer.GetIsPlaying: boolean;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetIsPlaying(FEngineIndex);
end;

{***********************************************}
function TALAsyncVideoPlayer.GetLooping: Boolean;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetLooping(FEngineIndex);
end;

{*************************************************************}
procedure TALAsyncVideoPlayer.SetLooping(const Value: Boolean);
begin
  TALVideoPlayerControllerThread.Instance.SetLooping(FEngineIndex, Value);
end;

{*********************************************}
function TALAsyncVideoPlayer.GetVolume: Single;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetVolume(FEngineIndex);
end;

{***********************************************************}
procedure TALAsyncVideoPlayer.SetVolume(const Value: Single);
begin
  TALVideoPlayerControllerThread.Instance.SetVolume(FEngineIndex, Value);
end;

{****************************************************}
function TALAsyncVideoPlayer.GetPlaybackSpeed: single;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetPlaybackSpeed(FEngineIndex);
end;

{******************************************************************}
procedure TALAsyncVideoPlayer.SetPlaybackSpeed(const Value: single);
begin
  TALVideoPlayerControllerThread.Instance.SetPlaybackSpeed(FEngineIndex, Value);
end;

{****************************************************}
function TALAsyncVideoPlayer.GetDrawable: TALDrawable;
begin
  if FCoreVideoPlayer = nil then
    FCoreVideoPlayer := TALVideoPlayerControllerThread.Instance.GetCoreVideoPlayer(FEngineIndex);
  if FCoreVideoPlayer <> nil then result := FCoreVideoPlayer.Drawable
  else result := ALNullDrawable;
end;

{*****************************************************}
function TALAsyncVideoPlayer.GetCurrentPosition: Int64;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetCurrentPosition(FEngineIndex);
end;

{**********************************************}
function TALAsyncVideoPlayer.GetDuration: Int64;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetDuration(FEngineIndex);
end;

{***************************************************}
function TALAsyncVideoPlayer.GetVideoHeight: Integer;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetVideoHeight(FEngineIndex);
end;

{**************************************************}
function TALAsyncVideoPlayer.GetVideoWidth: Integer;
begin
  Result := TALVideoPlayerControllerThread.Instance.GetVideoWidth(FEngineIndex);
end;

{***************************************************************}
procedure TALAsyncVideoPlayer.Prepare(Const ADataSource: String);
begin
  TALVideoPlayerControllerThread.Instance.Prepare(FEngineIndex, ADataSource);
end;

{**********************************}
procedure TALAsyncVideoPlayer.Start;
begin
  TALVideoPlayerControllerThread.Instance.Start(FEngineIndex);
end;

{**********************************}
procedure TALAsyncVideoPlayer.Pause;
begin
  TALVideoPlayerControllerThread.Instance.Pause(FEngineIndex);
end;

{*********************************}
procedure TALAsyncVideoPlayer.Stop;
begin
  TALVideoPlayerControllerThread.Instance.Stop(FEngineIndex);
end;

{******************************************************}
procedure TALAsyncVideoPlayer.SeekTo(const msec: Int64);
begin
  TALVideoPlayerControllerThread.Instance.SeekTo(FEngineIndex, msec);
end;

{********************}
{$IF defined(ANDROID)}
constructor TALVideoPlayerControllerThread.THandlerCallback.Create(const AVideoPlayerControllerThread: TALVideoPlayerControllerThread);
begin
  inherited Create;
  FVideoPlayerControllerThread := AVideoPlayerControllerThread;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function TALVideoPlayerControllerThread.THandlerCallback.handleMessage(msg: JMessage): Boolean;
begin
  FVideoPlayerControllerThread.ProcessCommandQueue;
  Result := True;
end;
{$ENDIF}

{********************************************************}
constructor TALVideoPlayerControllerThread.TEngine.Create;
begin
  inherited Create;
  EmptySlot := True;
  Signal := TEvent.Create(nil, false{ManualReset}, false, '');
  CoreVideoPlayer := nil;
  ProxyVideoPlayer := nil;
end;

{********************************************************}
destructor TALVideoPlayerControllerThread.TEngine.Destroy;
begin
  ALFreeAndNil(Signal);
  ALFreeAndNil(CoreVideoPlayer);
  Inherited Destroy;
end;

{************************************************}
constructor TALVideoPlayerControllerThread.Create;
begin
  inherited Create(True{CreateSuspended});
  FReady := False;
  {$IF defined(android)}
  FLooper := nil;
  FHandlerCallback := Nil;
  FHandler := Nil;
  {$ELSE}
  FSignal := TEvent.Create(nil, false{ManualReset}, false, '');
  {$ENDIF}
  //FLock := ??; their is no TLightweightMREW.create but instead an ugly class operator TLightweightMREW.Initialize :(
  Setlength(FEngines, 10);
  for var I := Low(FEngines) to High(FEngines) do
    FEngines[I] := TEngine.Create;
  FCommandQueue := TQueue<TCommand>.Create;
end;

{************************************************}
destructor TALVideoPlayerControllerThread.Destroy;
begin
  Terminate;
  {$IF defined(android)}
  FLooper.quit;
  WaitFor;
  ALFreeAndNil(FHandlerCallback);
  FHandler := nil;
  FLooper := nil;
  {$ELSE}
  FSignal.setevent;
  WaitFor;
  ALfreeandNil(FSignal);
  {$ENDIF}
  // Note: Accessing the current engine from this thread goes against ExoPlayer’s
  // guidelines. However, since we are in the process of closing the application
  // (destroying TALVideoPlayerControllerThread), this constraint no longer matters.
  for var I := Low(FEngines) to High(FEngines) do
    ALFreeAndNil(FEngines[I]);
  While FCommandQueue.Count > 0 do
    FCommandQueue.Dequeue.Free;
  ALFreeAndNil(FCommandQueue);
  inherited Destroy;
end;

{*************}
//[MultiThread]
class function TALVideoPlayerControllerThread.CreateInstance: TALVideoPlayerControllerThread;
begin
  result := TALVideoPlayerControllerThread.Create;
end;

{*************}
//[MultiThread]
class function TALVideoPlayerControllerThread.GetInstance: TALVideoPlayerControllerThread;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
    else FInstance.start;
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALVideoPlayerControllerThread.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{***********************************************}
procedure TALVideoPlayerControllerThread.Execute;
begin
  {$IF defined(android)}
  TJLooper.JavaClass.Prepare;
  FLooper := TJLooper.JavaClass.myLooper;
  FHandlerCallback := TALVideoPlayerControllerThread.THandlerCallback.Create(Self);
  FHandler := TJHandler.JavaClass.init(FLooper, FHandlerCallback);
  FReady := True;
  FHandler.sendEmptyMessage(0{what});
  TJLooper.JavaClass.loop;
  {$ELSE}
  FReady := True;
  While not terminated do begin
    ProcessCommandQueue;
    FSignal.WaitFor(INFINITE);
  end;
  {$ENDIF}
end;

{***********************************************************}
procedure TALVideoPlayerControllerThread.ProcessCommandQueue;
begin
  While true do begin

    var LCommand: TCommand;
    Tmonitor.Enter(FCommandQueue);
    Try
      if FCommandQueue.Count = 0 then
        Exit;
      LCommand := FCommandQueue.Dequeue;
    Finally
      Tmonitor.Exit(FCommandQueue);
    End;

    var LEngine := GetEngine(LCommand.EngineIndex);
    case LCommand.Request.Operation of

      TOperation.CreateEngine: begin
        var LVideoPlayer := TALVideoPlayer.Create;
        LVideoPlayer.Tag := LCommand.EngineIndex;
        LVideoPlayer.OnError := DoOnError;
        LVideoPlayer.OnPrepared := DoOnPrepared;
        LVideoPlayer.OnFrameAvailable := DoOnFrameAvailable;
        LVideoPlayer.OnCompletion := DoOnCompletion;
        LVideoPlayer.OnVideoSizeChanged := DoOnVideoSizeChanged;
        TMonitor.Enter(LEngine);
        Try
          {$IF defined(DEBUG)}
          if LEngine.EmptySlot then Raise Exception.Create('Error 077B6931-5B11-4DCB-B716-93FB469C3999');
          if LEngine.CoreVideoPlayer <> nil then Raise Exception.Create('Error 25024169-E620-44E9-A739-8813EDC26D56');
          {$ENDIF}
          LEngine.CoreVideoPlayer := LVideoPlayer;
        Finally
          Tmonitor.Exit(LEngine);
        End;
      end;

      TOperation.ReleaseEngine: begin
        TMonitor.Enter(LEngine);
        Try
          {$IF defined(DEBUG)}
          if LEngine.EmptySlot then Raise Exception.Create('Error FE6C5764-EFC5-4DE7-815F-4F12B469B2FE');
          if LEngine.ProxyVideoPlayer <> nil then Raise Exception.Create('Error A6BAEDE9-E779-45E4-9581-7D4851505EF2');
          {$ENDIF}
          ALFreeAndNil(LEngine.CoreVideoPlayer);
          LEngine.EmptySlot := True;
        Finally
          Tmonitor.Exit(LEngine);
        End;
      end;

      TOperation.GetState:
        LCommand.Response.ResultInt64 := LEngine.CoreVideoPlayer.GetState;

      TOperation.GetIsPlaying:
        LCommand.Response.Resultboolean := LEngine.CoreVideoPlayer.GetIsPlaying;

      TOperation.GetLooping:
        LCommand.Response.ResultBoolean := LEngine.CoreVideoPlayer.GetLooping;

      TOperation.SetLooping:
        LEngine.CoreVideoPlayer.SetLooping(LCommand.Request.Param1Boolean);

      TOperation.GetVolume:
        LCommand.Response.ResultSingle := LEngine.CoreVideoPlayer.GetVolume;

      TOperation.SetVolume:
        LEngine.CoreVideoPlayer.SetVolume(LCommand.Request.Param1Single);

      TOperation.GetPlaybackSpeed:
        LCommand.Response.ResultSingle := LEngine.CoreVideoPlayer.GetPlaybackSpeed;

      TOperation.SetPlaybackSpeed:
        LEngine.CoreVideoPlayer.SetPlaybackSpeed(LCommand.Request.Param1single);

      TOperation.GetCurrentPosition:
        LCommand.Response.ResultInt64 := LEngine.CoreVideoPlayer.GetCurrentPosition;

      TOperation.GetDuration:
        LCommand.Response.ResultInt64 := LEngine.CoreVideoPlayer.GetDuration;

      TOperation.GetVideoHeight:
        LCommand.Response.ResultInt64 := LEngine.CoreVideoPlayer.GetVideoHeight;

      TOperation.GetVideoWidth:
        LCommand.Response.ResultInt64 := LEngine.CoreVideoPlayer.GetVideoWidth;

      TOperation.Prepare:
        LEngine.CoreVideoPlayer.Prepare(LCommand.Request.Param1String);

      TOperation.Start:
        LEngine.CoreVideoPlayer.Start;

      TOperation.Pause:
        LEngine.CoreVideoPlayer.Pause;

      TOperation.Stop:
        LEngine.CoreVideoPlayer.Stop;

      TOperation.SeekTo:
        LEngine.CoreVideoPlayer.SeekTo(LCommand.Request.Param1Int64);

      else
        raise Exception.Create('Error 9911E409-C4EB-432C-A56C-44F9EE3CA8EC');

    end;

    if LCommand.WaitResponse then
      LEngine.Signal.setEvent
    else
      ALFreeAndNil(LCommand);

  end;
end;

{*****************************************************}
function TALVideoPlayerControllerThread.EnqueueCommand(
           const AEngineIndex: Integer;
           const AOperation: TOperation;
           const AWaitResponse: Boolean;
           const AParamInt64: Int64;
           const AParamSingle: Single;
           const AParamBoolean: Boolean;
           const AParamString: String): TResponse;
begin
  var LCommand := TCommand.Create;
  Try

    LCommand.EngineIndex := AEngineIndex;
    LCommand.WaitResponse := AWaitResponse;
    LCommand.Request.Operation := AOperation;
    LCommand.Request.Param1Int64 := AParamInt64;
    LCommand.Request.Param1Single := AParamSingle;
    LCommand.Request.Param1Boolean := AParamBoolean;
    LCommand.Request.Param1String := AParamString;

    Tmonitor.Enter(FCommandQueue);
    try
      FCommandQueue.Enqueue(LCommand);
    finally
      Tmonitor.Exit(FCommandQueue);
    end;

    {$IF defined(android)}
    if FReady then
      FHandler.sendEmptyMessage(0{what});
    {$ELSE}
    FSignal.SetEvent;
    {$ENDIF}

    if AWaitResponse then begin
      GetEngine(AEngineIndex).Signal.WaitFor(INFINITE);
      Result := LCommand.Response;
    end
    else
      // When not waiting for a response, ProcessCommandQueue
      // is responsible for releasing LCommand.
      LCommand := nil;

  finally
    ALFreeAndNil(LCommand);
  End;
end;

{*********************************************************************************************************************************************************}
function TALVideoPlayerControllerThread.EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean): TResponse;
begin
  Result := EnqueueCommand(
              AEngineIndex, // const AEngineIndex: Integer;
              AOperation, // const AOperation: TOperation;
              AWaitResponse, // const AWaitResponse: Boolean;
              0, // AParamInt64: Int64;
              0, // AParamSingle: Single;
              False, // AParamBoolean: Boolean;
              ''); // AParamString: String);
end;

{***********************************************************************************************************************************************************************************}
function TALVideoPlayerControllerThread.EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean; const AParamInt64: Int64): TResponse;
begin
  Result := EnqueueCommand(
              AEngineIndex, // const AEngineIndex: Integer;
              AOperation, // const AOperation: TOperation;
              AWaitResponse, // const AWaitResponse: Boolean;
              AParamInt64, // AParamInt64: Int64;
              0, // AParamSingle: Single;
              False, // AParamBoolean: Boolean;
              ''); // AParamString: String);
end;

{*************************************************************************************************************************************************************************************}
function TALVideoPlayerControllerThread.EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean; const AParamSingle: Single): TResponse;
begin
  Result := EnqueueCommand(
              AEngineIndex, // const AEngineIndex: Integer;
              AOperation, // const AOperation: TOperation;
              AWaitResponse, // const AWaitResponse: Boolean;
              0, // AParamInt64: Int64;
              AParamSingle, // AParamSingle: Single;
              False, // AParamBoolean: Boolean;
              ''); // AParamString: String);
end;

{***************************************************************************************************************************************************************************************}
function TALVideoPlayerControllerThread.EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean; const AParamBoolean: Boolean): TResponse;
begin
  Result := EnqueueCommand(
              AEngineIndex, // const AEngineIndex: Integer;
              AOperation, // const AOperation: TOperation;
              AWaitResponse, // const AWaitResponse: Boolean;
              0, // AParamInt64: Int64;
              0, // AParamSingle: Single;
              AParamBoolean, // AParamBoolean: Boolean;
              ''); // AParamString: String);
end;

{*************************************************************************************************************************************************************************************}
function TALVideoPlayerControllerThread.EnqueueCommand(const AEngineIndex: Integer; const AOperation: TOperation; const AWaitResponse: Boolean; const AParamString: String): TResponse;
begin
  Result := EnqueueCommand(
              AEngineIndex, // const AEngineIndex: Integer;
              AOperation, // const AOperation: TOperation;
              AWaitResponse, // const AWaitResponse: Boolean;
              0, // AParamInt64: Int64;
              0, // AParamSingle: Single;
              False, // AParamBoolean: Boolean;
              AParamString); // AParamString: String);
end;

{******************************************************************}
procedure TALVideoPlayerControllerThread.DoOnError(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALVideoPlayerControllerThread.DoOnError');
  {$ENDIF}
  var LEngine := GetEngine(TALBaseVideoPlayer(Sender).tag);
  TMonitor.Enter(LEngine);
  try
    if LEngine.ProxyVideoPlayer = nil then exit;
    If assigned(LEngine.ProxyVideoPlayer.OnError) then begin
      // On iOS, TThread.Current.ThreadID is always MainThreadID;
      // on Android, the current thread ID may differ.
      If TThread.Current.ThreadID = MainThreadID then begin
        LEngine.ProxyVideoPlayer.OnError(LEngine.ProxyVideoPlayer);
      end
      else begin
        var LProxyVideoPlayer := LEngine.ProxyVideoPlayer;
        TThread.Queue(nil,
          procedure
          begin
            TMonitor.Enter(LEngine);
            try
              if LEngine.ProxyVideoPlayer <> LProxyVideoPlayer then exit;
              If assigned(LEngine.ProxyVideoPlayer.OnError) then
                LEngine.ProxyVideoPlayer.OnError(LEngine.ProxyVideoPlayer);
            finally
              TMonitor.Exit(LEngine);
            end;
          end);
      end;
    end;
  finally
    TMonitor.Exit(LEngine);
  end;
end;

{*********************************************************************}
procedure TALVideoPlayerControllerThread.DoOnPrepared(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALVideoPlayerControllerThread.DoOnPrepared');
  {$ENDIF}
  var LEngine := GetEngine(TALBaseVideoPlayer(Sender).tag);
  TMonitor.Enter(LEngine);
  try
    if LEngine.ProxyVideoPlayer = nil then exit;
    If assigned(LEngine.ProxyVideoPlayer.OnPrepared) then begin
      // On iOS, TThread.Current.ThreadID is always MainThreadID;
      // on Android, the current thread ID may differ.
      If TThread.Current.ThreadID = MainThreadID then begin
        LEngine.ProxyVideoPlayer.OnPrepared(LEngine.ProxyVideoPlayer);
      end
      else begin
        var LProxyVideoPlayer := LEngine.ProxyVideoPlayer;
        TThread.Queue(nil,
          procedure
          begin
            TMonitor.Enter(LEngine);
            try
              if LEngine.ProxyVideoPlayer <> LProxyVideoPlayer then exit;
              If assigned(LEngine.ProxyVideoPlayer.OnPrepared) then
                LEngine.ProxyVideoPlayer.OnPrepared(LEngine.ProxyVideoPlayer);
            finally
              TMonitor.Exit(LEngine);
            end;
          end);
      end;
    end;
  finally
    TMonitor.Exit(LEngine);
  end;
end;

{***************************************************************************}
procedure TALVideoPlayerControllerThread.DoOnFrameAvailable(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALVideoPlayerControllerThread.DoOnFrameAvailable');
  If TThread.Current.ThreadID <> MainThreadID then
    Raise Exception.Create('Error 30414840-A389-48FC-B1C8-2DB3E4B29B70');
  {$ENDIF}
  var LEngine := GetEngine(TALBaseVideoPlayer(Sender).tag);
  TMonitor.Enter(LEngine);
  try
    if LEngine.ProxyVideoPlayer = nil then exit;
    If assigned(LEngine.ProxyVideoPlayer.OnFrameAvailable) then
      LEngine.ProxyVideoPlayer.OnFrameAvailable(LEngine.ProxyVideoPlayer);
  finally
    TMonitor.Exit(LEngine);
  end;
end;

{***********************************************************************}
procedure TALVideoPlayerControllerThread.DoOnCompletion(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALVideoPlayerControllerThread.DoOnCompletion');
  {$ENDIF}
  var LEngine := GetEngine(TALBaseVideoPlayer(Sender).tag);
  TMonitor.Enter(LEngine);
  try
    if LEngine.ProxyVideoPlayer = nil then exit;
    If assigned(LEngine.ProxyVideoPlayer.OnCompletion) then begin
      // On iOS, TThread.Current.ThreadID is always MainThreadID;
      // on Android, the current thread ID may differ.
      If TThread.Current.ThreadID = MainThreadID then begin
        LEngine.ProxyVideoPlayer.OnCompletion(LEngine.ProxyVideoPlayer);
      end
      else begin
        var LProxyVideoPlayer := LEngine.ProxyVideoPlayer;
        TThread.Queue(nil,
          procedure
          begin
            TMonitor.Enter(LEngine);
            try
              if LEngine.ProxyVideoPlayer <> LProxyVideoPlayer then exit;
              If assigned(LEngine.ProxyVideoPlayer.OnCompletion) then
                LEngine.ProxyVideoPlayer.OnCompletion(LEngine.ProxyVideoPlayer);
            finally
              TMonitor.Exit(LEngine);
            end;
          end);
      end;
    end;
  finally
    TMonitor.Exit(LEngine);
  end;
end;

{********************************************************************************************************************************}
procedure TALVideoPlayerControllerThread.DoOnVideoSizeChanged(const Sender: TObject; const width: Integer; const height: Integer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALVideoPlayerControllerThread.DoOnVideoSizeChanged');
  {$ENDIF}
  var LEngine := GetEngine(TALBaseVideoPlayer(Sender).tag);
  TMonitor.Enter(LEngine);
  try
    if LEngine.ProxyVideoPlayer = nil then exit;
    If assigned(LEngine.ProxyVideoPlayer.OnVideoSizeChanged) then begin
      // On iOS, TThread.Current.ThreadID is always MainThreadID;
      // on Android, the current thread ID may differ.
      If TThread.Current.ThreadID = MainThreadID then begin
        LEngine.ProxyVideoPlayer.OnVideoSizeChanged(LEngine.ProxyVideoPlayer, width, height);
      end
      else begin
        var LProxyVideoPlayer := LEngine.ProxyVideoPlayer;
        TThread.Queue(nil,
          procedure
          begin
            TMonitor.Enter(LEngine);
            try
              if LEngine.ProxyVideoPlayer <> LProxyVideoPlayer then exit;
              If assigned(LEngine.ProxyVideoPlayer.OnVideoSizeChanged) then
                LEngine.ProxyVideoPlayer.OnVideoSizeChanged(LEngine.ProxyVideoPlayer, width, height);
            finally
              TMonitor.Exit(LEngine);
            end;
          end);
      end;
    end;
  finally
    TMonitor.Exit(LEngine);
  end;
end;

{**************************************************************************************}
Function TALVideoPlayerControllerThread.GetEngine(const AEngineIndex: Integer): TEngine;
begin
  FLock.BeginRead;
  Try
    If (AEngineIndex < low(FEngines)) or (AEngineIndex > High(FEngines)) then
      Raise Exception.Create('Engine index out of bounds');
    Result := FEngines[AEngineIndex];
  Finally
    FLock.EndRead;
  End;
end;

{**********************************************************************************************************}
function TALVideoPlayerControllerThread.AcquireEngine(const AProxyVideoPlayer: TALBaseVideoPlayer): Integer;
begin
  FLock.BeginWrite;
  Try

    Result := -1;
    For var I := low(FEngines) to High(FEngines) do begin
      If FEngines[I].EmptySlot then begin
        Result := I;
        break;
      end;
    end;

    If result = -1 then begin
      Result := high(FEngines) + 1;
      Setlength(FEngines, length(FEngines) + 10);
      for var I := Result to High(FEngines) do
        FEngines[I] := TEngine.Create;
    end;

    {$IF defined(DEBUG)}
    if FEngines[Result].CoreVideoPlayer <> nil then Raise Exception.Create('Error DF9B829D-0F3D-408F-A8E2-D53D81EEE6B6');
    if FEngines[Result].ProxyVideoPlayer <> nil then Raise Exception.Create('Error 4019D4AD-E3C0-4D07-A09E-4A78B41B7839');
    {$ENDIF}
    FEngines[Result].EmptySlot := False;
    FEngines[Result].ProxyVideoPlayer := AProxyVideoPlayer;

  Finally
    FLock.EndWrite;
  End;

  EnqueueCommand(Result, TOperation.CreateEngine, False{AWaitResponse});
end;

{**********************************************************************************}
procedure TALVideoPlayerControllerThread.ReleaseEngine(const AEngineIndex: Integer);
begin
  var LEngine := GetEngine(AEngineIndex);
  TMonitor.Enter(LEngine);
  try
    {$IF defined(DEBUG)}
    if LEngine.CoreVideoPlayer = nil then Raise Exception.Create('Error 564F7439-CD74-45BC-B420-A0728B2EA3CB');
    if LEngine.ProxyVideoPlayer = nil then Raise Exception.Create('Error B0E9E26E-959E-4426-B82B-7406DBDDE659');
    {$ENDIF}
    LEngine.ProxyVideoPlayer := nil;
  finally
    TMonitor.Exit(LEngine);
  end;
  EnqueueCommand(AEngineIndex, TOperation.ReleaseEngine, False{AWaitResponse});
end;

{**********************************************************************************************************}
function TALVideoPlayerControllerThread.GetCoreVideoPlayer(const AEngineIndex: Integer): TALBaseVideoPlayer;
begin
  var LEngine := GetEngine(AEngineIndex);
  TMonitor.Enter(LEngine);
  try
    Result := LEngine.CoreVideoPlayer;
  finally
    TMonitor.Exit(LEngine);
  end;
end;

{*************************************************************************************}
function TALVideoPlayerControllerThread.GetState(const AEngineIndex: Integer): Integer;
begin
  Result := Integer(EnqueueCommand(AEngineIndex, TOperation.GetState, True{AWaitResponse}).ResultInt64);
end;

{*****************************************************************************************}
function TALVideoPlayerControllerThread.GetIsPlaying(const AEngineIndex: Integer): boolean;
begin
  Result := EnqueueCommand(AEngineIndex, TOperation.GetIsPlaying, True{AWaitResponse}).Resultboolean;
end;

{***************************************************************************************}
function TALVideoPlayerControllerThread.GetLooping(const AEngineIndex: Integer): Boolean;
begin
  Result := EnqueueCommand(AEngineIndex, TOperation.GetLooping, True{AWaitResponse}).ResultBoolean;
end;

{*****************************************************************************************************}
procedure TALVideoPlayerControllerThread.SetLooping(const AEngineIndex: Integer; const Value: Boolean);
begin
  EnqueueCommand(AEngineIndex, TOperation.SetLooping, False{AWaitResponse}, Value);
end;

{*************************************************************************************}
function TALVideoPlayerControllerThread.GetVolume(const AEngineIndex: Integer): Single;
begin
  Result := EnqueueCommand(AEngineIndex, TOperation.GetVolume, True{AWaitResponse}).ResultSingle;
end;

{***************************************************************************************************}
procedure TALVideoPlayerControllerThread.SetVolume(const AEngineIndex: Integer; const Value: Single);
begin
  EnqueueCommand(AEngineIndex, TOperation.SetVolume, False{AWaitResponse}, Value);
end;

{********************************************************************************************}
function TALVideoPlayerControllerThread.GetPlaybackSpeed(const AEngineIndex: Integer): single;
begin
  Result := EnqueueCommand(AEngineIndex, TOperation.GetPlaybackSpeed, True{AWaitResponse}).Resultsingle;
end;

{**********************************************************************************************************}
procedure TALVideoPlayerControllerThread.SetPlaybackSpeed(const AEngineIndex: Integer; const Value: single);
begin
  EnqueueCommand(AEngineIndex, TOperation.SetPlaybackSpeed, False{AWaitResponse}, Value);
end;

{*********************************************************************************************}
function TALVideoPlayerControllerThread.GetCurrentPosition(const AEngineIndex: Integer): Int64;
begin
  Result := EnqueueCommand(AEngineIndex, TOperation.GetCurrentPosition, True{AWaitResponse}).ResultInt64;
end;

{**************************************************************************************}
function TALVideoPlayerControllerThread.GetDuration(const AEngineIndex: Integer): Int64;
begin
  Result := EnqueueCommand(AEngineIndex, TOperation.GetDuration, True{AWaitResponse}).ResultInt64;
end;

{*******************************************************************************************}
function TALVideoPlayerControllerThread.GetVideoHeight(const AEngineIndex: Integer): Integer;
begin
  Result := Integer(EnqueueCommand(AEngineIndex, TOperation.GetVideoHeight, True{AWaitResponse}).ResultInt64);
end;

{******************************************************************************************}
function TALVideoPlayerControllerThread.GetVideoWidth(const AEngineIndex: Integer): Integer;
begin
  Result := Integer(EnqueueCommand(AEngineIndex, TOperation.GetVideoWidth, True{AWaitResponse}).ResultInt64)
end;

{*******************************************************************************************************}
procedure TALVideoPlayerControllerThread.Prepare(const AEngineIndex: Integer; Const ADataSource: String);
begin
  EnqueueCommand(AEngineIndex, TOperation.Prepare, False{AWaitResponse}, ADataSource);
end;

{**************************************************************************}
procedure TALVideoPlayerControllerThread.Start(const AEngineIndex: Integer);
begin
  EnqueueCommand(AEngineIndex, TOperation.Start, False{AWaitResponse});
end;

{**************************************************************************}
procedure TALVideoPlayerControllerThread.Pause(const AEngineIndex: Integer);
begin
  EnqueueCommand(AEngineIndex, TOperation.Pause, False{AWaitResponse});
end;

{*************************************************************************}
procedure TALVideoPlayerControllerThread.Stop(const AEngineIndex: Integer);
begin
  EnqueueCommand(AEngineIndex, TOperation.Stop, False{AWaitResponse});
end;

{**********************************************************************************************}
procedure TALVideoPlayerControllerThread.SeekTo(const AEngineIndex: Integer; const msec: Int64);
begin
  EnqueueCommand(AEngineIndex, TOperation.SeekTo, False{AWaitResponse}, msec);
end;

{****************************************************************************************************}
constructor TALVideoPlayerSurface.TPreviewDownloadContext.Create(const AOwner: TALVideoPlayerSurface);
begin
  inherited Create;
  Lock := TObject.Create;
  FreeByThread := True;
  Owner := AOwner;
  Rect := Owner.LocalRect;
  Scale := ALGetScreenScale;
  AlignToPixel := Owner.IsPixelAlignmentEnabled;
  ResourceName := Owner.PreviewResourceName;
  ResourceStream := nil;
  WrapMode := Owner.WrapMode;
end;

{***************************************************************}
destructor TALVideoPlayerSurface.TPreviewDownloadContext.Destroy;
begin
  ALFreeAndNil(Lock);
  ALFreeAndNil(ResourceStream);
  inherited
end;

{***********************************************************}
constructor TALVideoPlayerSurface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IF defined(ALDPK)}
  fVideoPlayerEngine := TALDummyVideoPlayer.create;
  {$ELSE}
  fVideoPlayerEngine := TALAsyncVideoPlayer.create;
  {$ENDIF}
  fVideoPlayerEngine.OnFrameAvailable := DoOnFrameAvailable;
  FDataSource := '';
  fPreviewResourceName := '';
  FBackgroundColor := DefaultBackgroundColor;
  FLoadingColor := DefaultLoadingColor;
  FInternalState := VPSIdle;
  FIsFirstFrame := true;
  FAutoStartMode := TAutoStartMode.None;
  FWrapMode := TALImageWrapMode.Fit;
  FCacheIndex := 0;
  FCacheEngine := nil;
  FPreviewDownloadContext := nil;
  FFadeInDuration := DefaultFadeInDuration;
  FFadeInStartTimeNano := 0;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
  fBufDrawable := ALNullDrawable;
end;

{***************************************}
destructor TALVideoPlayerSurface.Destroy;
begin
  ALFreeAndNil(fVideoPlayerEngine);
  inherited; // Will call CancelResourceDownload via ClearBufDrawable
end;

{************************************************}
procedure TALVideoPlayerSurface.BeforeDestruction;
begin
  // Unsubscribe from TALScrollCapturedMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  if not (csDestroying in ComponentState) then begin
    TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
    Stop;
  end;
  inherited;
end;

{*******************************************************}
function TALVideoPlayerSurface.IsReadyToDisplay: Boolean;
begin
  Result := Inherited and
            (FPreviewDownloadContext = nil) and
            ((FFadeInStartTimeNano <= 0) or
             ((ALElapsedTimeNano - FFadeInStartTimeNano) / ALNanosPerSec > FFadeInDuration));
end;

{*************************************}
procedure TALVideoPlayerSurface.Loaded;
begin
  inherited;
  If FDataSource <> '' then begin
    FVideoPlayerEngine.Prepare(FDataSource);
    if FAutoStartMode = TAutoStartMode.WhenPrepared then start;
  end;
end;

{*******************************************************}
function TALVideoPlayerSurface.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{********************************************************}
function TALVideoPlayerSurface.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{********************************************************************}
function TALVideoPlayerSurface.GetDefaultBackgroundColor: TalphaColor;
begin
  Result := TalphaColors.Null;
end;

{*****************************************************************}
function TALVideoPlayerSurface.GetDefaultLoadingColor: TalphaColor;
begin
  Result := $FFe0e4e9;
end;

{**************************************************************}
function TALVideoPlayerSurface.GetDefaultFadeInDuration: Single;
begin
  Result := 0.250;
end;

{**************************************************************************}
procedure TALVideoPlayerSurface.setPreviewResourceName(const Value: String);
begin
  if FPreviewResourceName <> Value then begin
    ClearBufDrawable;
    FPreviewResourceName := Value;
    Repaint;
  end;
end;

{*****************************************************************}
procedure TALVideoPlayerSurface.SetDataSource(const Value: String);
begin
  if Value <> FDataSource then begin
    FDataSource := Value;
    {$IF not defined(ALDPK)}
    if not (csLoading in ComponentState) then begin
      if FInternalState <> vpsIdle then begin
        var LVideoPlayerEngine: TALBaseVideoPlayer := TALAsyncVideoPlayer.create;
        LVideoPlayerEngine.Looping := fVideoPlayerEngine.Looping;
        LVideoPlayerEngine.PlaybackSpeed := fVideoPlayerEngine.PlaybackSpeed;
        LVideoPlayerEngine.Volume := fVideoPlayerEngine.Volume;
        LVideoPlayerEngine.OnError := fVideoPlayerEngine.OnError;
        LVideoPlayerEngine.OnPrepared := fVideoPlayerEngine.OnPrepared;
        LVideoPlayerEngine.OnCompletion := fVideoPlayerEngine.OnCompletion;
        LVideoPlayerEngine.OnVideoSizeChanged := fVideoPlayerEngine.OnVideoSizeChanged;
        LVideoPlayerEngine.OnFrameAvailable := DoOnFrameAvailable;
        //--
        ALFreeAndNil(fVideoPlayerEngine);
        fVideoPlayerEngine := LVideoPlayerEngine;
      end;
      if FDataSource <> '' then begin
        FVideoPlayerEngine.Prepare(FDataSource);
        if AutoStartMode = TAutoStartMode.WhenPrepared then
          FVideoPlayerEngine.Start;
      end;
    end;
    {$ENDIF}
  end;
end;

{*************************************************************************}
procedure TALVideoPlayerSurface.SetWrapMode(const Value: TALImageWrapMode);
begin
  if FWrapMode <> Value then begin
    ClearBufDrawable;
    FWrapMode := Value;
    Repaint;
  end;
end;

{***********************************************}
function TALVideoPlayerSurface.GetState: Integer;
begin
  Result := fVideoPlayerEngine.State;
end;

{****************************************************************************}
procedure TALVideoPlayerSurface.SetAutoStartMode(const Value: TAutoStartMode);
begin
  if value <> FAutoStartMode then begin
    FAutoStartMode := Value;
    {$IF not defined(ALDPK)}
    If (not (csLoading in ComponentState)) and
       (FAutoStartMode = TAutoStartMode.WhenPrepared) and
       (DataSource <> '') and
       (FInternalState = VPSIdle) then Start;
    {$ENDIF}
  end;
end;

{***************************************************}
function TALVideoPlayerSurface.GetIsPlaying: boolean;
begin
  Result := fVideoPlayerEngine.IsPlaying;
end;

{*************************************************}
function TALVideoPlayerSurface.GetLooping: Boolean;
begin
  Result := fVideoPlayerEngine.Looping;
end;

{***************************************************************}
procedure TALVideoPlayerSurface.SetLooping(const Value: Boolean);
begin
  fVideoPlayerEngine.Looping := Value;
end;

{***********************************************}
function TALVideoPlayerSurface.GetVolume: Single;
begin
  Result := fVideoPlayerEngine.Volume;
end;

{*************************************************************}
procedure TALVideoPlayerSurface.SetVolume(const Value: Single);
begin
  fVideoPlayerEngine.Volume := Value;
end;

{******************************************************}
function TALVideoPlayerSurface.GetPlaybackSpeed: single;
begin
  Result := fVideoPlayerEngine.PlaybackSpeed;
end;

{********************************************************************}
procedure TALVideoPlayerSurface.SetPlaybackSpeed(const Value: single);
begin
  fVideoPlayerEngine.PlaybackSpeed := Value;
end;

{*******************************************************}
function TALVideoPlayerSurface.GetCurrentPosition: Int64;
begin
  Result := fVideoPlayerEngine.GetCurrentPosition;
end;

{************************************************}
function TALVideoPlayerSurface.GetDuration: Int64;
begin
  Result := fVideoPlayerEngine.GetDuration;
end;

{*****************************************************}
function TALVideoPlayerSurface.GetVideoHeight: Integer;
begin
  Result := fVideoPlayerEngine.GetVideoHeight;
end;

{****************************************************}
function TALVideoPlayerSurface.GetVideoWidth: Integer;
begin
  Result := fVideoPlayerEngine.GetVideoWidth;
end;

{************************************}
procedure TALVideoPlayerSurface.Start;
begin
  if FInternalState = VPSStarted then exit;
  FInternalState := VPSStarted;
  fVideoPlayerEngine.Start;
end;

{************************************}
procedure TALVideoPlayerSurface.Pause;
begin
  if FInternalState = VPSPaused then exit;
  FInternalState := VPSPaused;
  if AutoStartedVideoPlayerSurface = self then AutoStartedVideoPlayerSurface := nil;
  fVideoPlayerEngine.Pause;
end;

{***********************************}
procedure TALVideoPlayerSurface.Stop;
begin
  if FInternalState = VPSStopped then exit;
  FInternalState := VPSStopped;
  if AutoStartedVideoPlayerSurface = self then AutoStartedVideoPlayerSurface := nil;
  fVideoPlayerEngine.Stop;
end;

{********************************************************}
procedure TALVideoPlayerSurface.SeekTo(const msec: Int64);
begin
  fVideoPlayerEngine.SeekTo(msec);
end;

{***********************************************************}
function TALVideoPlayerSurface.GetOnErrorEvent: TNotifyEvent;
begin
  result := fVideoPlayerEngine.OnError;
end;

{*************************************************************************}
procedure TALVideoPlayerSurface.SetOnErrorEvent(const Value: TNotifyEvent);
begin
  fVideoPlayerEngine.OnError := Value;
end;

{**************************************************************}
function TALVideoPlayerSurface.GetOnPreparedEvent: TNotifyEvent;
begin
  result := fVideoPlayerEngine.OnPrepared;
end;

{****************************************************************************}
procedure TALVideoPlayerSurface.SetOnPreparedEvent(const Value: TNotifyEvent);
begin
  fVideoPlayerEngine.OnPrepared := Value;
end;

{****************************************************************}
function TALVideoPlayerSurface.GetOnCompletionEvent: TNotifyEvent;
begin
  result := fVideoPlayerEngine.OnCompletion;
end;

{******************************************************************************}
procedure TALVideoPlayerSurface.SetOnCompletionEvent(const Value: TNotifyEvent);
begin
  fVideoPlayerEngine.OnCompletion := Value;
end;

{**********************************************************************************}
function TALVideoPlayerSurface.GetOnVideoSizeChangedEvent: TALVideoSizeChangedEvent;
begin
  result := fVideoPlayerEngine.OnVideoSizeChanged;
end;

{************************************************************************************************}
procedure TALVideoPlayerSurface.SetOnVideoSizeChangedEvent(const Value: TALVideoSizeChangedEvent);
begin
  fVideoPlayerEngine.OnVideoSizeChanged := Value;
end;

{**************************************************************}
function TALVideoPlayerSurface.IsBackgroundColorStored: Boolean;
begin
  Result := FBackgroundColor <> DefaultBackgroundColor;
end;

{***********************************************************}
function TALVideoPlayerSurface.IsLoadingColorStored: Boolean;
begin
  Result := FLoadingColor <> DefaultLoadingColor;
end;

{*************************************************************}
function TALVideoPlayerSurface.IsFadeInDurationStored: Boolean;
begin
  Result := not SameValue(FFadeInDuration, DefaultFadeInDuration, TEpsilon.Vector);
end;

{*********************************************************}
function TALVideoPlayerSurface.IsDataSourceStored: Boolean;
begin
  result := FDataSource <> '';
end;

{************************************************************}
function TALVideoPlayerSurface.IsPlaybackSpeedStored: Boolean;
begin
  Result := not SameValue(PlaybackSpeed, 1);
end;

{*****************************************************}
function TALVideoPlayerSurface.IsVolumeStored: Boolean;
begin
  Result := not SameValue(Volume, 1);
end;

{****************************************}
procedure TALVideoPlayerSurface.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{***********************************************}
procedure TALVideoPlayerSurface.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  CancelPreviewDownload;
  FFadeInStartTimeNano := 0;
  ALFreeAndNilDrawable(fBufDrawable);
end;

{****************************************************}
procedure TALVideoPlayerSurface.CancelPreviewDownload;
begin
  // The FPreviewDownloadContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FPreviewDownloadContext <> nil then begin
    var LContextToFree: TPreviewDownloadContext;
    var LLock := FPreviewDownloadContext.lock;
    TMonitor.Enter(LLock);
    try
      if not FPreviewDownloadContext.FreeByThread then LContextToFree := FPreviewDownloadContext
      else LContextToFree := nil;
      FPreviewDownloadContext.Owner := nil;
      FPreviewDownloadContext := nil;
    Finally
      TMonitor.Exit(LLock);
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{*************}
//[MultiThread]
class function TALVideoPlayerSurface.CanStartPreviewDownload(var AContext: Tobject): boolean;
begin
  result := TPreviewDownloadContext(AContext).owner <> nil;
end;

{*************}
//[MultiThread]
class procedure TALVideoPlayerSurface.HandlePreviewDownloadSuccess(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject);
begin
  var LContext := TPreviewDownloadContext(AContext);
  if LContext.owner = nil then exit;
  LContext.ResourceStream := AContentStream;
  TALGraphicThreadPool.Instance.ExecuteProc(
    CreateBufDrawable, // const AProc: TALWorkerThreadProc;
    LContext, // const AContext: Tobject; TALGraphicThreadPool.Instance will own and release the Context object
    GetPreviewDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  AContentStream := nil; // AContentStream Will be free by AContext
  AContext := nil; // AContext will be free by TALGraphicThreadPool.Instance
end;

{*************}
//[MultiThread]
class procedure TALVideoPlayerSurface.HandlePreviewDownloadError(const AErrMessage: string; var AContext: Tobject);
begin
  var LContext := TPreviewDownloadContext(AContext);
  if LContext.owner = nil then exit;
  {$IFDEF ALDPK}
  TMonitor.Enter(LContext.Lock);
  try
    if LContext.Owner <> nil then begin
      LContext.FreeByThread := False;
      AContext := nil; // AContext will be free by CancelResourceDownload
    end;
  finally
    TMonitor.Exit(LContext.Lock);
  end;
  exit;
  {$ENDIF}
  if LContext.ResourceName = ALBrokenImageResourceName then begin
    ALLog(
      'TALVideoPlayerSurface.HandlePreviewDownloadError',
      'BrokenImage resource is missing or incorrect | ' +
      AErrMessage,
      TalLogType.error);
    TMonitor.Enter(LContext.Lock);
    try
      if LContext.Owner <> nil then begin
        LContext.FreeByThread := False;
        AContext := nil; // AContext will be free by CancelResourceDownload
      end;
    finally
      TMonitor.Exit(LContext.Lock);
    end;
    exit;
  end;
  ALLog(
    'TALVideoPlayerSurface.HandlePreviewDownloadError',
    'Url: ' + LContext.ResourceName + ' | ' +
    AErrMessage,
    TalLogType.warn);
  LContext.Rect := TRectF.Create(
                     LContext.Rect.TopLeft,
                     ALBrokenImageWidth,
                     ALBrokenImageHeight);
  //LContext.Scale: Single;
  //LContext.AlignToPixel: Boolean;
  LContext.ResourceName := ALBrokenImageResourceName;
  ALFreeAndNil(LContext.ResourceStream);
  LContext.WrapMode := TALImageWrapMode.Fit;
  TALGraphicThreadPool.Instance.ExecuteProc(
    CreateBufDrawable, // const AProc: TALWorkerThreadProc;
    LContext, // const AContext: Tobject; TALGraphicThreadPool.Instance will own and release the Context object
    GetPreviewDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  AContext := nil; // AContext will be free by TALGraphicThreadPool.Instance
end;

{*************}
//[MultiThread]
class function TALVideoPlayerSurface.GetPreviewDownloadPriority(const AContext: Tobject): Int64;
begin
  result := TALNetHttpClientPool.Instance.PriorityStartingPoint;
end;

{*************}
//[MultiThread]
class Procedure TALVideoPlayerSurface.CreateBufDrawable(var AContext: TObject);
begin
  var LContext := TPreviewDownloadContext(AContext);
  if LContext.owner = nil then exit;
  var LBufDrawable: TALDrawable := ALNullDrawable;
  var LBufDrawableRect: TRectF;
  Try
    CreateBufDrawable(
      LBufDrawable, // var ABufDrawable: TALDrawable;
      LBufDrawableRect, // out ABufDrawableRect: TRectF;
      LContext.Rect, // const ARect: TRectF;
      LContext.Scale, // const AScale: Single;
      LContext.AlignToPixel, // const AAlignToPixel: Boolean;
      LContext.ResourceName, // const AResourceName: String;
      LContext.ResourceStream, // const AResourceStream: TStream;
      LContext.WrapMode); // const AWrapMode: TALImageWrapMode;
  except
    On E: Exception do begin
      HandlePreviewDownloadError(E.Message, AContext);
      exit;
    end;
  End;
  TThread.queue(nil,
    procedure
    begin
      if LContext.Owner <> nil then begin
        if (LContext.Owner.FFadeInDuration > 0) and
           (LContext.ResourceName <> ALBrokenImageResourceName) and
           (not ALIsDrawableNull(LBufDrawable)) and
           (ALIsDrawableNull(LContext.Owner.fVideoPlayerEngine.Drawable)) then
          LContext.Owner.FFadeInStartTimeNano := ALElapsedTimeNano
        else
          LContext.Owner.FFadeInStartTimeNano := 0;
        ALFreeAndNilDrawable(LContext.Owner.fBufDrawable);
        LContext.Owner.fBufDrawable := LBufDrawable;
        LContext.Owner.FBufDrawableRect := LBufDrawableRect;
        LContext.Owner.FPreviewDownloadContext := nil;
        LContext.Owner.Repaint;
      end;
      ALFreeAndNil(LContext);
    end);
  AContext := nil; // AContext will be free by TThread.queue
end;

{*************}
//[MultiThread]
class Procedure TALVideoPlayerSurface.CreateBufDrawable(
                  var ABufDrawable: TALDrawable;
                  out ABufDrawableRect: TRectF;
                  const ARect: TRectF;
                  const AScale: Single;
                  const AAlignToPixel: Boolean;
                  const AResourceName: String;
                  const AResourceStream: TStream;
                  const AWrapMode: TALImageWrapMode);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var lResourceName: String;
  if ALIsHttpOrHttpsUrl(AResourceName) then lResourceName := ''
  else lResourceName := AResourceName;
  var LFileName := ALGetResourceFilename(lResourceName);

  ABufDrawableRect := ARect;

  if (AResourceStream <> nil) or (LFileName <> '') or (LResourceName <> '') then begin

    {$IFDEF ALDPK}
    try
    {$ENDIF}

      ABufDrawable := ALCreateDrawableFromResource(
                        AResourceName, // const AResourceName: String;
                        AResourceStream, // const AResourceStream: TStream;
                        '', // AMaskResourceName, // const AMaskResourceName: String;
                        ALNullBitmap, // AMaskBitmap, // const AMaskBitmap: TALBitmap;
                        AScale, // const AScale: Single;
                        ARect.Width, ARect.Height, // const W, H: single;
                        AWrapMode, // const AWrapMode: TALImageWrapMode;
                        TpointF.Create(-50,-50), // const ACropCenter: TpointF;
                        0, // const ABlurRadius: single;
                        0, // const AXRadius: Single;
                        0); // const AYRadius: Single);

    {$IFDEF ALDPK}
    except
      ABufDrawable := ALCreateEmptyDrawable1x1;
      Exit;
    end;
    {$ENDIF}

  end;

  if not ALIsDrawableNull(ABufDrawable) then
    ABufDrawableRect := TrectF.Create(0,0, ALGetDrawableWidth(ABufDrawable)/AScale, ALGetDrawableHeight(ABufDrawable)/AScale).CenterAt(ARect);

end;

{**********************************************}
procedure TALVideoPlayerSurface.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if fResourceName is empty
     (fPreviewResourceName = '')
  then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) or
     (FPreviewDownloadContext <> nil) or
     (not ALIsDrawableNull(fVideoPlayerEngine.Drawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  if (FPreviewDownloadContext = nil) and
     (ALIsHttpOrHttpsUrl(PreviewResourceName)) then begin

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Starting download | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
    {$endif}

    FPreviewDownloadContext := TPreviewDownloadContext.Create(Self);
    Try
      TALNetHttpClientPool.Instance.Get(
        PreviewResourceName, // const AUrl: String;
        CanStartPreviewDownload, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
        HandlePreviewDownloadSuccess, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
        HandlePreviewDownloadError, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
        FPreviewDownloadContext, // const AContext: Tobject; // Context will be free by the worker thread
        true, // const AUseCache: Boolean = True;
        GetPreviewDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
    except
      ALFreeAndNil(FPreviewDownloadContext);
      Raise;
    End;

    exit;

  end;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // out ABufDrawableRect: TRectF;
    LocalRect, // const ARect: TRectF;
    ALGetScreenScale, // const AScale: Single;
    IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
    PreviewResourceName, // const AResourceName: String;
    nil, // const AResourceStream: TStream;
    WrapMode); // const AWrapMode: TALImageWrapMode;

end;

{*************************************************************************************************}
procedure TALVideoPlayerSurface.ApplicationEventHandler(const Sender: TObject; const M : TMessage);
begin
  if FAutoStartMode = TAutoStartMode.WhenDisplayed then begin
    if (M is TApplicationEventMessage) then begin
      case (M as TApplicationEventMessage).Value.Event of
        TApplicationEvent.EnteredBackground,
        TApplicationEvent.WillTerminate: Pause;
      end;
    end;
  end;
end;

{******************************************************************}
procedure TALVideoPlayerSurface.DoOnFrameAvailable(Sender: Tobject);
begin
  var LAbsoluteDisplayedRect := GetAbsoluteDisplayedRect;
  If FAutoStartMode = TAutoStartMode.WhenDisplayed then begin
    // If less than 20% of the surface is visible, then pause this video.
    If (CompareValue(LAbsoluteDisplayedRect.Width * LAbsoluteDisplayedRect.Height, Width * Height * 0.2, TEpsilon.position) <= 0) then
      Pause;
  end;

  if not LAbsoluteDisplayedRect.IsEmpty then
    repaint;
end;

{************************************}
procedure TALVideoPlayerSurface.Paint;
begin

  If (FAutoStartMode = TAutoStartMode.WhenDisplayed) and (FInternalState <> VPSStarted) then begin
    var LAbsoluteDisplayedRect := GetAbsoluteDisplayedRect;
    // If 80% of the surface is visible and another video is currently playing,
    // pause the other video and start this one.
    If (AutoStartedVideoPlayerSurface <> nil) and
       (AutoStartedVideoPlayerSurface <> self) and
       (CompareValue(LAbsoluteDisplayedRect.Width * LAbsoluteDisplayedRect.Height, Width * Height * 0.8, TEpsilon.position) > 0) then begin
      AutoStartedVideoPlayerSurface.Pause;
      Start;
      AutoStartedVideoPlayerSurface := Self;
    end
    // If 20% of the surface is visible and no other video is currently playing,
    // then start this video.
    else if (AutoStartedVideoPlayerSurface = nil) and
            (CompareValue(LAbsoluteDisplayedRect.Width * LAbsoluteDisplayedRect.Height, Width * Height * 0.2, TEpsilon.position) > 0) then begin
      Start;
      AutoStartedVideoPlayerSurface := Self;
    end
  end;

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    var R := LocalRect;
    System.types.InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  // Calculate the opacity based on FFadeInStartTimeNano.
  var LOpacity: Single := AbsoluteOpacity;
  if FFadeInStartTimeNano > 0 then begin
    {$IF defined(DEBUG)}
    // Not possible; we are initiating a FadeIn
    // effect while still downloading the image.
    if (FPreviewDownloadContext <> nil) then
      Raise Exception.Create('Error 821554A1-5E7D-4920-BA1A-CECA32A9D967');
    {$ENDIF}
    var LElapsedTime := (ALElapsedTimeNano - FFadeInStartTimeNano) / ALNanosPerSec;
    if LElapsedTime > FFadeInDuration then FFadeInStartTimeNano := 0
    else begin
      LOpacity := LOpacity * (LElapsedTime / FFadeInDuration);
      // We cannot call Repaint from within a paint method,
      // but we can call Form.Invalidate. We use Form.Invalidate
      // to avoid using any TALFloatAnimation object
      {$IF defined(MSWindows)}
      If Form <> nil then begin
        var LWnd := FormToHWND(Form);
        TThread.ForceQueue(nil,
          procedure
          begin
            Winapi.Windows.InvalidateRect(LWnd, nil, False);
          end);
      end;
      {$ELSE}
      If Form <> nil then
        Form.Invalidate;
      {$ENDIF}
    end;
  end;

  if ALIsDrawableNull(fVideoPlayerEngine.Drawable) then begin

    var LDrawable: TALDrawable;
    var LDrawableRect: TRectF;
    if (CacheIndex <= 0) or
       (CacheEngine = nil) or
       (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
      MakeBufDrawable;
      if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
        if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
        else fBufDrawable := ALNullDrawable;
        if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
          raise Exception.Create('Error 6FFD96FE-A985-4F3B-AF0A-2A5C03615E27');
      end
      else begin
        LDrawable := FBufDrawable;
        LDrawableRect := FBufDrawableRect;
      end;
    end;

    if ((ALIsDrawableNull(LDrawable) and (FPreviewDownloadContext <> nil)) or
        (FFadeInStartTimeNano > 0)) and
       (LoadingColor <> TAlphaColors.Null) then begin
      {$IF DEFINED(ALSkiaCanvas)}
      TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LocalRect)
        .SetOpacity(AbsoluteOpacity)
        .SetFillColor(FloadingColor)
        .Draw;
      {$ELSE}
      Canvas.Fill.kind := TBrushKind.solid;
      Canvas.Fill.color := FloadingColor;
      Canvas.FillRect(ALAlignToPixelRound(LocalRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position), 0, 0, AllCorners, AbsoluteOpacity, TCornerType.Round);
      {$ENDIF}
    end;

    if BackgroundColor <> TAlphaColors.Null then begin
      {$IF DEFINED(ALSkiaCanvas)}
      TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LocalRect)
        .SetOpacity(LOpacity)
        .SetFillColor(FBackgroundColor)
        .Draw;
      {$ELSE}
      Canvas.Fill.kind := TBrushKind.solid;
      Canvas.Fill.color := BackGroundColor;
      Canvas.FillRect(ALAlignToPixelRound(LocalRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position), 0, 0, ALLCorners, LOpacity, TCornerType.Round);
      {$ENDIF}
    end;

    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      LDrawable, // const ADrawable: TALDrawable;
      LDrawableRect.TopLeft, // const ATopLeft: TpointF;
      LOpacity); // const AOpacity: Single);

    if FIsFirstFrame then
      FIsFirstFrame := ALIsDrawableNull(LDrawable);

  end

  else begin

    if FIsFirstFrame then begin
      CancelPreviewDownload;
      ALFreeAndNilDrawable(fBufDrawable);
      if (FFadeInDuration > 0) and (FFadeInStartTimeNano <= 0) then begin
        FFadeInStartTimeNano := ALElapsedTimeNano;
        LOpacity := 0;
        // We cannot call Repaint from within a paint method,
        // but we can call Form.Invalidate. We use Form.Invalidate
        // to avoid using any TALFloatAnimation object
        {$IF defined(MSWindows)}
        If Form <> nil then begin
          var LWnd := FormToHWND(Form);
          TThread.ForceQueue(nil,
            procedure
            begin
              Winapi.Windows.InvalidateRect(LWnd, nil, False);
            end);
        end;
        {$ELSE}
        If Form <> nil then
          Form.Invalidate;
        {$ENDIF}
      end;
      FIsFirstFrame := false;
    end;

    if (FFadeInStartTimeNano > 0) and
       (LoadingColor <> TAlphaColors.Null) then begin
      {$IF DEFINED(ALSkiaCanvas)}
      TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LocalRect)
        .SetOpacity(AbsoluteOpacity)
        .SetFillColor(FloadingColor)
        .Draw;
      {$ELSE}
      Canvas.Fill.kind := TBrushKind.solid;
      Canvas.Fill.color := FloadingColor;
      Canvas.FillRect(ALAlignToPixelRound(LocalRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position), 0, 0, AllCorners, AbsoluteOpacity, TCornerType.Round);
      {$ENDIF}
    end;

    if BackgroundColor <> TAlphaColors.Null then begin
      {$IF DEFINED(ALSkiaCanvas)}
      TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LocalRect)
        .SetOpacity(LOpacity)
        .SetFillColor(FBackgroundColor)
        .Draw;
      {$ELSE}
      Canvas.Fill.kind := TBrushKind.solid;
      Canvas.Fill.color := BackGroundColor;
      Canvas.FillRect(ALAlignToPixelRound(LocalRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position), 0, 0, ALLCorners, LOpacity, TCornerType.Round);
      {$ENDIF}
    end;

    var LSrcRect: TrectF;
    var LDstRect: TrectF;
    case WrapMode of

      TALImageWrapMode.Fit: begin
        LSrcRect := Trectf.Create(
                      0, 0,
                      ALGetDrawableWidth(fVideoPlayerEngine.Drawable),
                      ALGetDrawableHeight(fVideoPlayerEngine.Drawable));
        LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(fVideoPlayerEngine.Drawable), ALGetDrawableHeight(fVideoPlayerEngine.Drawable)).
                      FitInto(LocalRect);
      end;

      TALImageWrapMode.Stretch: begin
        LSrcRect := Trectf.Create(
                      0, 0,
                      ALGetDrawableWidth(fVideoPlayerEngine.Drawable),
                      ALGetDrawableHeight(fVideoPlayerEngine.Drawable));
        LDstRect := LocalRect;
      end;

      TALImageWrapMode.Place: begin
        LSrcRect := Trectf.Create(
                      0, 0,
                      ALGetDrawableWidth(fVideoPlayerEngine.Drawable),
                      ALGetDrawableHeight(fVideoPlayerEngine.Drawable));
        LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(fVideoPlayerEngine.Drawable), ALGetDrawableHeight(fVideoPlayerEngine.Drawable)).
                      PlaceInto(LocalRect);
      end;

      TALImageWrapMode.FitAndCrop: begin
        LDstRect := TRectF.Create(0, 0, Width, Height);
        LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
        LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, ALGetDrawableWidth(fVideoPlayerEngine.Drawable), ALGetDrawableHeight(fVideoPlayerEngine.Drawable)));
      end;

      else
        Raise Exception.Create('Error B0DE069F-2CFD-4719-9130-0D69A647EE2D')

    end;

    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      fVideoPlayerEngine.Drawable, // const ADrawable: TALDrawable;
      LSrcRect, // const ASrcRect: TrectF; // IN REAL PIXEL !
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      LOpacity); // const AOpacity: Single);

  end;

end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALVideoPlayerSurface]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALVideoPlayerSurface, 'Size');
  UnlistPublishedProperty(TALVideoPlayerSurface, 'StyleName');
  UnlistPublishedProperty(TALVideoPlayerSurface, 'OnTap');
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALVideoPlayerSurface]);
  TALVideoPlayerControllerThread.FInstance := nil;
  TALVideoPlayerControllerThread.CreateInstanceFunc := @TALVideoPlayerControllerThread.CreateInstance;
  TALVideoPlayerSurface.AutoStartedVideoPlayerSurface := Nil;

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TALIosVideoPlayer.AppAudioSessionLock := TObject.Create;
  TALIosVideoPlayer.AppAudioSessionActivated := False;
  {$ENDIF}
  {$ENDREGION}


finalization
  ALFreeAndNil(TALVideoPlayerControllerThread.FInstance);

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  ALFreeAndNil(TALIosVideoPlayer.AppAudioSessionLock);
  {$ENDIF}
  {$ENDREGION}

end.
