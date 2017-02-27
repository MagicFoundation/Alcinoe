unit ALVideoPlayer;

interface

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE _MACOS}
{$IFEND}

uses system.Classes,
     System.SyncObjs,
     {$IF defined(DEBUG)}
     system.diagnostics,
     {$endIF}
     {$IF defined(ANDROID)}
     System.Messaging,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNI.Media,
     Androidapi.JNIBridge,
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

  {********************************************************************************************}
  TALBufferingUpdateNotifyEvent = procedure(const Sender: TObject; const mp: integer) of object;
  TALVideoSizeChangedNotifyEvent = procedure(const Sender: TObject; const width: Integer; const height: Integer) of object;
  TVideoPlayerState = (vpsInitialized, // << Successful invoke of setDataSource in a valid state transfers the object to the Initialized state
                       vpsPreparing, // << a call to prepareAsync() (asynchronous) which first transfers the object to the Preparing state
                       vpsPrepared,
                       vpsStarted,
                       vpsPaused,
                       vpsStopped,
                       vpsPlaybackCompleted,
                       vpsIdle, // << When a MediaPlayer object is just created using new or after reset() is called, it is in the Idle state
                       vpsError);

  {********************}
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
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onFrameAvailable(surfaceTexture: JSurfaceTexture); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALErrorListener = class(TJavaLocal, JMediaPlayer_OnErrorListener)
      private
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        function onError(mp: JMediaPlayer; what: Integer; extra: Integer): Boolean; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALPreparedListener = class(TJavaLocal, JMediaPlayer_OnPreparedListener)
      private
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onPrepared(mp: JMediaPlayer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALVideoSizeChangedListener = class(TJavaLocal, JMediaPlayer_OnVideoSizeChangedListener)
      private
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onVideoSizeChanged(mp: JMediaPlayer; width: Integer; height: Integer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALBufferingUpdateListener = class(TJavaLocal, JMediaPlayer_OnBufferingUpdateListener)
      private
        {$IF defined(DEBUG)}
        fPercentBuffered: integer;
        {$ENDIF}
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onBufferingUpdate(mp: JMediaPlayer; percent: Integer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALCompletionListener = class(TJavaLocal, JMediaPlayer_OnCompletionListener)
      private
        [Weak] FVideoPlayerControl: TALAndroidVideoPlayer;
      public
        constructor Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
        procedure onCompletion(mp: JMediaPlayer); cdecl;
      end;

  private
    fLifeObj: TALLifeObj;
    FMediaPlayer: JMediaPlayer;
    fSurfaceTexture: JSurfaceTexture;
    fbitmap: TALTexture;
    fOnFrameAvailableEvent: TNotifyEvent;
    FOnFrameAvailableListener: TALFrameAvailableListener;
    fOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
    fOnBufferingUpdateListener: TALBufferingUpdateListener;
    fOnCompletionEvent: TNotifyEvent;
    FOnCompletionListener: TALCompletionListener;
    fOnErrorEvent: TNotifyEvent;
    FOnErrorListener: TALErrorListener;
    FOnPreparedEvent: TnotifyEvent;
    FOnPreparedListener: TALPreparedListener;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    fonVideoSizeChangedListener: TALVideoSizeChangedListener;
    fVideoWidth: integer;
    fVideoHeight: integer;
    FOpenGLContextLostId: Integer;
    FOpenGLContextResetId: Integer;
    FIsOpenGLContextLost: Boolean;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage);
    procedure removeListeners;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: integer;
    function getDuration: integer;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare;
    procedure prepareAsync;
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    property bitmap: TalTexture read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
  end;
  {$endIF}

  {****************}
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
    fState: TVideoPlayerState;
    fLooping: boolean;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    fonReadyEvent: TnotifyEvent;
    fOnItemDidPlayToEndTimeEvent: TnotifyEvent;
    fOnItemFailedToPlayToEndTimeEvent: TnotifyEvent;
  protected
    procedure doOnFrameRefresh(Sender: TObject);
    procedure doOnReady;
    procedure doOnItemDidPlayToEndTime;
    procedure doOnItemFailedToPlayToEndTime;
    property onReady: TnotifyEvent read fonReadyEvent write fonReadyEvent;
    property OnItemDidPlayToEndTime: TnotifyEvent read fOnItemDidPlayToEndTimeEvent write fOnItemDidPlayToEndTimeEvent;
    property OnItemFailedToPlayToEndTime: TnotifyEvent read fOnItemFailedToPlayToEndTimeEvent write fOnItemFailedToPlayToEndTimeEvent;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: integer;
    function getDuration: integer;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare;
    procedure prepareAsync;
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    property bitmap: TalTexture read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
  end;
  {$endIF}

  {**********************}
  {$IF defined(MSWINDOWS)}
  TALWinVideoPlayer = class(Tobject)
  private
    fbitmap: TBitmap;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: integer;
    function getDuration: integer;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare;
    procedure prepareAsync;
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    property bitmap: TBitmap read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
  end;
  {$endIF}

  {*******************}
  {$IF defined(_MACOS)}
  TALMacOSVideoPlayer = class(Tobject)
  private
    fbitmap: TBitmap;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: integer;
    function getDuration: integer;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare;
    procedure prepareAsync;
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    property bitmap: TBitmap read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
  end;
  {$endIF}

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
    fOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    //-----
    FAutoStartWhenPrepared: Boolean;
    fState: TVideoPlayerState;
    //-----
    FTag: int64;
    [Weak] FTagObject: TObject;
    FTagFloat: Double;
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
    procedure doOnBufferingUpdate(const Sender: TObject; const mp: integer);
    procedure doOnVideoSizeChanged(const Sender: TObject; const width: Integer; const height: Integer);
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getCurrentPosition: integer;
    function getDuration: integer;
    function getVideoHeight: integer;
    function getVideoWidth: integer;
    function isPlaying: boolean;
    procedure pause;
    procedure prepare(const aAutoStartWhenPrepared: Boolean=False);
    procedure prepareAsync(const aAutoStartWhenPrepared: Boolean=False);
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property Bitmap: TALTexture read Getbitmap;
    {$ELSE}
    property Bitmap: TBitmap read Getbitmap;
    {$ENDIF}
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
    property State: TVideoPlayerState read fState;
    property AutoStartWhenPrepared: boolean read FAutoStartWhenPrepared write FAutoStartWhenPrepared;
    property Tag: int64 read FTag write FTag default 0;
    property TagObject: TObject read FTagObject write FTagObject;
    property TagFloat: Double read FTagFloat write FTagFloat;
  end;

  {***********************************}
  TALVideoPlayerAsync = class(TThread)
  private
    fSignal: Tevent;
    FReady: Boolean;
    //-----
    {$IF defined(android)}
    fVideoPlayerControl: TALAndroidVideoPlayer;
    {$ELSEIF defined(IOS)}
    fVideoPlayerControl: TALIOSVideoPlayer;
    {$ELSEIF defined(MSWINDOWS)}
    fVideoPlayerControl: TALWinVideoPlayer;
    {$ELSEIF defined(_MACOS)}
    fVideoPlayerControl: TALMacOsVideoPlayer;
    {$ENDIF}
    //-----
    fOnErrorEvent: TNotifyEvent;
    fOnPreparedEvent: TNotifyEvent;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    //-----
    FAutoStartWhenPrepared: Boolean;
    fState: integer; // the integer representation of TVideoPlayerState;
    //-----
    FTag: int64;
    [Weak] FTagObject: TObject;
    FTagFloat: Double;
    //-----
    fDoSetDataSource: Boolean;
    fDoSetDataSourceValue: String;
    fDoPrepare: Boolean;
    fDoPause: Boolean;
    fDoStart: Boolean;
    fDoStop: Boolean;
    fDoSetVolume: Boolean;
    fDoSetVolumeValue: integer;
    fDoSetLooping: boolean;
    fDoSetLoopingValue: boolean;
    fDoSeekTo: boolean;
    fDoSeekToValue: integer;
    fDoGetDuration: Boolean;
    fDoGetDurationSignal: Tevent;
    fDoGetDurationValue: integer;
    {$IF defined(IOS)}
    fDoOnReady: boolean;
    fDoOnItemDidPlayToEndTime: Boolean;
    fDoOnItemFailedToPlayToEndTime: boolean;
    {$endif}
    //-----
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function GetBitmap: TalTexture;
    {$ELSE}
    function GetBitmap: TBitmap;
    {$ENDIF}
    function getState: TVideoPlayerState;
    procedure doOnCompletion(Sender: TObject);
    procedure doOnError(Sender: TObject);
    procedure doOnPrepared(Sender: TObject);
    procedure doOnFrameAvailable(Sender: TObject);
    procedure doOnBufferingUpdate(const Sender: TObject; const mp: integer);
    procedure doOnVideoSizeChanged(const Sender: TObject; const width: Integer; const height: Integer);
    {$IF defined(IOS)}
    procedure OnReady(Sender: TObject);
    procedure OnItemDidPlayToEndTime(Sender: TObject);
    procedure OnItemFailedToPlayToEndTime(Sender: TObject);
    {$endif}
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; override;
    //function getCurrentPosition: integer;
    function getDuration: integer;
    //function getVideoHeight: integer;
    //function getVideoWidth: integer;
    //function isPlaying: boolean;
    procedure pause;
    procedure prepare(const aAutoStartWhenPrepared: Boolean=False);
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    procedure setVolume(const Value: Single);
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property Bitmap: TALTexture read Getbitmap;
    {$ELSE}
    property Bitmap: TBitmap read Getbitmap;
    {$ENDIF}
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent; // << always fired in the main thread
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent; // << always fired in the main thread
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent; // << always fired in the main thread
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent; // << always fired in the main thread
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent; // << always fired in the main thread
    property onVideoSizeChanged: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent; // << always fired in the main thread
    property State: TVideoPlayerState read GetState;
    property AutoStartWhenPrepared: boolean read FAutoStartWhenPrepared write FAutoStartWhenPrepared;
    property Tag: int64 read FTag write FTag default 0;
    property TagObject: TObject read FTagObject write FTagObject;
    property TagFloat: Double read FTagFloat write FTagFloat;
  end;

  {*****************************************}
  TALVideoPlayerSurface = class(TALRectangle)
  private
    fVideoPlayer: TALVideoPlayer;
    procedure OnFrameAvailable(Sender: Tobject);
    procedure SetVideoPlayer(const Value: TALVideoPlayer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property VideoPlayer: TALVideoPlayer read fVideoPlayer write SetVideoPlayer;
  end;

procedure register;

implementation

uses system.SysUtils,
     system.Types,
     {$IF defined(ANDROID)}
     system.Math,
     Androidapi.Helpers,
     Androidapi.jni.JavaTypes,
     FMX.Helpers.Android,
     FMX.Canvas.GPU,
     fmx.types3D,
     AlString,
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
     {$ENDIF}
     fmx.controls,
     AlCommon;

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
end;

{**********************************************************************************************************}
procedure TALAndroidVideoPlayer.TALFrameAvailableListener.onFrameAvailable(surfaceTexture: JSurfaceTexture);
var aLifeObj: TALLifeObj;
begin

  {$IF defined(DEBUG)}
  if (fTotalFramesProcessed = 0) or (fTotalFramesProcessed >= 100) then begin
    fFpsStopWatch.Stop;
    ALLog('TALAndroidVideoPlayer.onFrameAvailable', 'fps: ' + ALFormatFloatU('0.00', (fTotalFramesProcessed) / max(1, (ffpsStopWatch.Elapsed.TotalMilliseconds / 1000)), AlDefaultFormatSettingsU) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
    fTotalFramesProcessed := 0;
    fFpsStopWatch := TStopWatch.StartNew;
  end;
  inc(fTotalFramesProcessed);
  {$ENDIF}

  //SurfaceTexture objects may be created on any thread. updateTexImage() may only be called on the
  //thread with the OpenGL ES context that contains the texture object. The frame-available callback
  //is called on an arbitrary thread, so unless special care is taken updateTexImage() should not be
  //called directly from the callback.
  // >> so i as understand i can call updateTexImage in other thread than the current thread, it's
  // >> seam to be thread safe - i already make opengl multithread however the updateTexImage seam
  // >> seam to take around 1ms only so their is no really purpose to run it in a different thread than
  // >> the main thread (who already have the OpenGL ES context)
  aLifeObj := fVideoPlayerControl.fLifeObj;
  tthread.queue(nil,
    procedure
    {$IF defined(DEBUG)}
    var aStopWatch: TStopWatch;
    {$ENDIF}
    begin
      if not aLifeObj.alive then exit;

      if fVideoPlayerControl.FIsOpenGLContextLost then exit;

      if (fVideoPlayerControl.fSurfaceTexture <> nil) and
         (fVideoPlayerControl.fbitmap <> nil) then begin
        if (fVideoPlayerControl.fbitmap.Width <> fVideoPlayerControl.fVideoWidth) or
           (fVideoPlayerControl.fbitmap.Height <> fVideoPlayerControl.fVideoHeight) then begin
          TALTextureAccessPrivate(fVideoPlayerControl.fBitmap).FWidth := fVideoPlayerControl.fVideoWidth;
          TALTextureAccessPrivate(fVideoPlayerControl.fBitmap).FHeight := fVideoPlayerControl.fVideoHeight; // we can't use setsize because it's fill finalise the texture
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
        if (fTotalFramesProcessed mod 100 = 0) then aStopWatch := TStopWatch.StartNew;
        {$ENDIF}

        fVideoPlayerControl.fSurfaceTexture.updateTexImage;

        {$IFDEF DEBUG}
        if (fTotalFramesProcessed mod 100 = 0) then begin
          aStopWatch.Stop;
          ALLog('TALAndroidVideoPlayer.onFrameAvailable.updateTexImage', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
        end;
        {$ENDIF}

      end;

      if assigned(fVideoPlayerControl.fOnFrameAvailableEvent) then
        fVideoPlayerControl.fOnFrameAvailableEvent(fVideoPlayerControl);

    end);

end;

{**********************************************************************************************************}
constructor TALAndroidVideoPlayer.TALErrorListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{*******************************************************************************************************}
function TALAndroidVideoPlayer.TALErrorListener.onError(mp: JMediaPlayer; what, extra: Integer): Boolean;
var aLifeObj: TALLifeObj;
begin

  //what int: the type of error that has occurred:
  //MEDIA_ERROR_UNKNOWN (1)
  //MEDIA_ERROR_SERVER_DIED (100)
  //
  //extra	int: an extra code, specific to the error. Typically implementation dependent.
  //MEDIA_ERROR_IO (-1004)
  //MEDIA_ERROR_MALFORMED (-1007)
  //MEDIA_ERROR_UNSUPPORTED (-1010)
  //MEDIA_ERROR_TIMED_OUT (-110)
  //MEDIA_ERROR_SYSTEM (-2147483648) - low-level system error.

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onError', 'what: ' + alinttostrU(what) +
                                         ' - extra: ' + alinttostrU(extra) +
                                         ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}

  fVideoPlayerControl.FMediaPlayer.reset; // << else i have infinite loop here, onError is continuasly called (try to disconnect the wifi and you will see) - stupid behavior !!
  result := True; // True if the method handled the error, false if it didn't. Returning false, or not having an
                  // OnErrorListener at all, will cause the OnCompletionListener to be called.

  if assigned(fVideoPlayerControl.fOnErrorEvent) then begin
    aLifeObj := fVideoPlayerControl.fLifeObj;
    tthread.queue(nil,
      procedure
      begin
        if not aLifeObj.alive then exit;
        if assigned(fVideoPlayerControl.fOnErrorEvent) then
          fVideoPlayerControl.fOnErrorEvent(fVideoPlayerControl);
      end);
  end;

end;

{*************************************************************************************************************}
constructor TALAndroidVideoPlayer.TALPreparedListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{*******************************************************************************}
procedure TALAndroidVideoPlayer.TALPreparedListener.onPrepared(mp: JMediaPlayer);
var aLifeObj: TALLifeObj;
begin

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onPrepared', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if assigned(fVideoPlayerControl.fOnPreparedEvent) then begin
    aLifeObj := fVideoPlayerControl.fLifeObj;
    tthread.queue(nil,
      procedure
      begin
        if not aLifeObj.alive then exit;
        if assigned(fVideoPlayerControl.fOnPreparedEvent) then
          fVideoPlayerControl.fOnPreparedEvent(fVideoPlayerControl);
      end);
  end;

end;

{*********************************************************************************************************************}
constructor TALAndroidVideoPlayer.TALVideoSizeChangedListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{***********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TALVideoSizeChangedListener.onVideoSizeChanged(mp: JMediaPlayer; width, height: Integer);
var aLifeObj: TALLifeObj;
begin

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onVideoSizeChanged', 'width: ' + ALinttostrU(width) +
                                                    ' - height: ' + ALinttostrU(height) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  aLifeObj := fVideoPlayerControl.fLifeObj;
  tthread.queue(nil,
    procedure
    begin
      if not aLifeObj.alive then exit;
      fVideoPlayerControl.FVideoWidth := width;
      fVideoPlayerControl.fVideoHeight := height;
      if assigned(fVideoPlayerControl.fonVideoSizeChangedEvent) then
        fVideoPlayerControl.fonVideoSizeChangedEvent(fVideoPlayerControl, width, height);
    end);

end;

{********************************************************************************************************************}
constructor TALAndroidVideoPlayer.TALBufferingUpdateListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  {$IF defined(DEBUG)}
  fPercentBuffered := 0;
  {$ENDIF}
  fVideoPlayerControl := aVideoPlayerControl;
end;

{***************************************************************************************************************}
procedure TALAndroidVideoPlayer.TALBufferingUpdateListener.onBufferingUpdate(mp: JMediaPlayer; percent: Integer);
var aLifeObj: TALLifeObj;
begin

  {$IF defined(DEBUG)}
  if abs(fPercentBuffered - percent) > 5 then begin // i don't know why percent bufferend go up and go down under android
    ALLog('TALAndroidVideoPlayer.onBufferingUpdate', 'percent buffered: ' + ALinttostrU(percent) + '%'+
                                                     ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
    fPercentBuffered := percent;
  end;
  {$ENDIF}

  if assigned(fVideoPlayerControl.fOnBufferingUpdateEvent) then begin
    aLifeObj := fVideoPlayerControl.fLifeObj;
    tthread.queue(nil,
      procedure
      begin
        if not aLifeObj.alive then exit;
        if assigned(fVideoPlayerControl.fOnBufferingUpdateEvent) then
          fVideoPlayerControl.fOnBufferingUpdateEvent(fVideoPlayerControl, percent);
      end);
  end;

end;

{***************************************************************************************************************}
constructor TALAndroidVideoPlayer.TALCompletionListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{***********************************************************************************}
procedure TALAndroidVideoPlayer.TALCompletionListener.onCompletion(mp: JMediaPlayer);
var aLifeObj: TALLifeObj;
begin

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onCompletion', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if assigned(fVideoPlayerControl.fOnCompletionEvent) then begin
    aLifeObj := fVideoPlayerControl.fLifeObj;
    tthread.queue(nil,
      procedure
      begin
        if not aLifeObj.alive then exit;
        if assigned(fVideoPlayerControl.fOnCompletionEvent) then
          fVideoPlayerControl.fOnCompletionEvent(fVideoPlayerControl);
      end);
  end;

end;

{***************************************}
constructor TALAndroidVideoPlayer.Create;
begin

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.Create', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  //-----
  inherited create;

  //-----
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  FIsOpenGLContextLost := False;

  //-----
  fLifeObj := TALLifeObj.Create;

  //-----
  // i use synchronize in case the TALAndroidVideoPlayer was
  // created in a different thread than the main thread because
  // only the main thread have the openGL ES context (except if
  // you modified the delphi source code to make opengl ES multithread)
  TThread.Synchronize(nil,
    procedure
    begin
      fBitmap := TalTexture.Create(False);
      ALInitializeEXTERNALOESTexture(fBitmap);
    end);

  //-----
  fVideoWidth := 0;
  fVideoHeight := 0;

  //-----
  FMediaPlayer := TJMediaPlayer.JavaClass.init;
  //-----
  fOnErrorEvent := nil;
  FOnErrorListener := TALErrorListener.Create(Self);
  FMediaPlayer.setOnErrorListener(FOnErrorListener);
  //-----
  fOnPreparedEvent := nil;
  FOnPreparedListener := TALPreparedListener.Create(Self);
  FMediaPlayer.setOnPreparedListener(FOnPreparedListener);
  //-----
  fonVideoSizeChangedEvent := nil;
  fonVideoSizeChangedListener := TALVideoSizeChangedListener.Create(Self);
  FMediaPlayer.setOnVideoSizeChangedListener(fonVideoSizeChangedListener);
  //-----
  fOnBufferingUpdateEvent := nil;
  fOnBufferingUpdateListener := TALBufferingUpdateListener.Create(Self);
  FMediaPlayer.setOnBufferingUpdateListener(fOnBufferingUpdateListener);
  //-----
  fOnCompletionEvent := nil;
  fOnCompletionListener := TALCompletionListener.Create(Self);
  FMediaPlayer.setOnCompletionListener(fOnCompletionListener);
  //-----
  fSurfaceTexture := TJSurfaceTexture.JavaClass.init(fBitmap.Handle);
  //-----
  fOnFrameAvailableEvent := nil;
  FOnFrameAvailableListener := TALFrameAvailableListener.Create(Self);
  fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener);
  //-----
  FMediaPlayer.setSurface(TJSurface.JavaClass.init((fSurfaceTexture)));

end;

{***************************************}
destructor TALAndroidVideoPlayer.Destroy;
begin

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.Destroy', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  //-----
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);

  //stop all futur TThread.queue to execute
  //and also signal that the object is destroying
  fLifeObj.alive := False;

  // to make sure none of the listeners get called anymore
  // FVideoPlayer.reset will do
  //
  //   mEventHandler.removeCallbacksAndMessages(null);
  //
  // i do it inside the UIThread BECAUSe it's seam that the
  // eventHandler is bound to the UI THread :
  //
  //   Looper looper;
  //   if ((looper = Looper.myLooper()) != null) {
  //     mEventHandler = new EventHandler(this, looper);
  //   } else if ((looper = Looper.getMainLooper()) != null) {
  //     mEventHandler = new EventHandler(this, looper);
  //   } else {
  //     mEventHandler = null;
  //   }
  //
  //
  CallInUIThreadAndWaitFinishing(removeListeners); // i must use removeListeners else
                                                   // a strong ref to FMediaPlayer is keep and
                                                   // later their is an exception when delphi
                                                   // try (again) to free the object (who is already freed)

  //----
  alfreeandNil(FOnErrorListener);
  alfreeandNil(FOnPreparedListener);
  alfreeandNil(fonVideoSizeChangedListener);
  alfreeandNil(fOnBufferingUpdateListener);
  alfreeandNil(fOnCompletionListener);
  //-----
  FMediaPlayer.setSurface(nil);
  FMediaPlayer.release;
  FMediaPlayer := nil;
  //-----
  alfreeAndNil(FOnFrameAvailableListener);
  fSurfaceTexture.release;
  fSurfaceTexture := nil;

  //-----
  // i use synchronize in case the TALAndroidVideoPlayer was
  // created in a different thread than the main thread because
  // only the main thread have the openGL ES context (except if
  // you modified the delphi source code to make opengl ES multithread)
  TThread.Synchronize(nil,
    procedure
    begin
      alFreeandNil(fbitmap);
    end);

  //-----
  fLifeObj := nil; // << it's the autorefcount process that must free this object

  //-----
  inherited;

end;

{**********************************************}
Procedure TALAndroidVideoPlayer.removeListeners;
begin
  FMediaPlayer.setOnErrorListener(nil);
  FMediaPlayer.setOnPreparedListener(nil);
  FMediaPlayer.setOnVideoSizeChangedListener(nil);
  FMediaPlayer.setOnBufferingUpdateListener(nil);
  FMediaPlayer.setOnCompletionListener(nil);
  fSurfaceTexture.setOnFrameAvailableListener(nil);
end;

{***************************************************************************************************}
procedure TALAndroidVideoPlayer.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.OpenGLContextLostHandler', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
  FIsOpenGLContextLost := True;
end;

{****************************************************************************************************}
procedure TALAndroidVideoPlayer.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.OpenGLContextResetHandler', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
  //FIsOpenGLContextLost := False; // normally i must recreate the fbitmap.handle but i don't have time to debug (i need to know how to recreate an open GL texture with a defined name)
                                   // and in my app i don't need this because on OpenGLContextLostHandler i simply destroy all the videoplayer to
                                   // recreate them on OpenGLContextResetHandler
end;

{******************************************************}
//Gets the current playback position. return the current
//position in milliseconds
function TALAndroidVideoPlayer.getCurrentPosition: integer;
begin
  Result := FMediaPlayer.getCurrentPosition;
end;

{*******************************************************************}
//Gets the duration of the file. return the duration in milliseconds,
//if no duration is available (for example, if streaming live
//content), -1 is returned.
function TALAndroidVideoPlayer.getDuration: integer;
begin
  Result := FMediaPlayer.getDuration;
end;

{*****************************************************************}
//Returns the height of the video. the height of the video, or 0 if
//there is no video, no display surface was set, or the height has not
//been determined yet. The OnVideoSizeChangedListener can be registered
//via setOnVideoSizeChangedListener(OnVideoSizeChangedListener) to
//provide a notification when the height is available.
function TALAndroidVideoPlayer.getVideoHeight: integer;
begin
  Result := FMediaPlayer.getVideoHeight;
end;

{*****************************************************************}
//Returns the width  of the video. the width  of the video, or 0 if
//there is no video, no display surface was set, or the width has not
//been determined yet. The OnVideoSizeChangedListener can be registered
//via setOnVideoSizeChangedListener(OnVideoSizeChangedListener) to
//provide a notification when the width is available.
function TALAndroidVideoPlayer.getVideoWidth: integer;
begin
  Result := FMediaPlayer.getVideoWidth;
end;

{******************************************}
//Checks whether the MediaPlayer is playing.
function TALAndroidVideoPlayer.isPlaying: boolean;
begin
  Result := FMediaPlayer.isPlaying;
end;

{****************************************}
//Pauses playback. Call start() to resume.
//When the call to pause() returns, the
//MediaPlayer object enters the Paused state.
//Note that the transition from the Started state
//to the Paused state and vice versa happens
//asynchronously in the player engine. It may
//take some time before the state is updated in
//calls to isPlaying(), and it can be a number
//of seconds in the case of streamed content.
//Calling start() to resume playback for a
//paused MediaPlayer object, and the resumed
//playback position is the same as where it
//was paused. When the call to start() returns,
//the paused MediaPlayer object goes back to
//the Started state.
procedure TALAndroidVideoPlayer.pause;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.pause', 'Begin', TalLogType.VERBOSE);
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  FMediaPlayer.pause;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.pause', 'End - timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{***************************************}
//Starts or resumes playback. If playback
//had previously been paused, playback will
//continue from where it was paused. If
//playback had been stopped, or never started
//before, playback will start at the beginning.
procedure TALAndroidVideoPlayer.start;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.start', 'Begin', TalLogType.VERBOSE);
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  FMediaPlayer.start;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.start', 'End - timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{**************************************}
//Stops playback after playback has been
//stopped or paused.
//Calling stop() stops playback and causes a
//MediaPlayer in the Started, Paused, Prepared
//or PlaybackCompleted state to enter the Stopped state.
//Once in the Stopped state, playback cannot be
//started until prepare() or prepareAsync() are
//called to set the MediaPlayer object to the
//Prepared state again.
procedure TALAndroidVideoPlayer.stop;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.stop', 'Begin', TalLogType.VERBOSE);
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  FMediaPlayer.stop;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.stop', 'End - timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{************************************************}
//Prepares the player for playback, synchronously.
//After setting the datasource and the display surface,
//you need to either call prepare() or prepareAsync().
//For files, it is OK to call prepare(), which blocks
//until MediaPlayer is ready for playback.
procedure TALAndroidVideoPlayer.prepare;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.prepare', 'Begin', TalLogType.VERBOSE);
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  FMediaPlayer.prepare;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.prepare', 'End - timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*************************************************}
//Prepares the player for playback, asynchronously.
//After setting the datasource and the display surface,
//you need to either call prepare() or prepareAsync().
//For streams, you should call prepareAsync(), which
//returns immediately, rather than blocking until
//enough data has been buffered.
procedure TALAndroidVideoPlayer.prepareAsync;
{$IFDEF DEBUG}
var aStopWatch: TstopWatch;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aStopWatch := TstopWatch.StartNew;
  ALLog('TALAndroidVideoPlayer.prepareAsync', 'Begin', TalLogType.VERBOSE);
  {$ENDIF}

  FMediaPlayer.prepareAsync;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.prepareAsync', 'End - timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*********************************}
//Seeks to specified time position.
procedure TALAndroidVideoPlayer.seekTo(const msec: Integer);
begin
  FMediaPlayer.seekTo(msec);
end;

{********************************************************}
//Sets the data source (file-path or http/rtsp URL) to use.
procedure TALAndroidVideoPlayer.setDataSource(const aDataSource: String);
begin
  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.setDataSource', 'aDataSource: ' + aDataSource, TalLogType.VERBOSE);
  {$ENDIF}
  FMediaPlayer.setDataSource(StringToJString(aDataSource));
end;

{*********************************************}
//Sets the player to be looping or non-looping.
procedure TALAndroidVideoPlayer.setLooping(const looping: Boolean);
begin
  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.setLooping', 'looping: ' + alBoolTostrU(looping), TalLogType.VERBOSE);
  {$ENDIF}
  FMediaPlayer.setLooping(looping);
end;

{***********************************************************}
//Sets the volume on this player. This API is recommended for
//balancing the output of audio streams within an application.
//Unless you are writing an application to control user settings,
//this API should be used in preference to setStreamVolume(int, int, int)
//which sets the volume of ALL streams of a particular type. Note
//that the passed volume values are raw scalars in range 0.0 to 1.0.
//UI controls should be scaled logarithmically.
procedure TALAndroidVideoPlayer.setVolume(const Value: Single);
var aVolume: Single;
begin
  {$IFDEF DEBUG}
  ALLog('TALAndroidVideoPlayer.setVolume', 'Value: ' + alFloattostrU(Value, ALDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
  aVolume := Value;
  if aVolume < 0 then aVolume := 0
  else if aVolume > 1 then aVolume := 1;
  FMediaPlayer.setVolume(aVolume, aVolume);
end;

{$ENDIF}

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

  //this is always fired in the main thread
  //http://stackoverflow.com/questions/7880742/ios-are-methods-called-by-delegates-and-observers-executed-on-the-main-thread

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.observeValueForKeyPath', 'Status:' +  alinttostrU(TNSNumber.Wrap(change.allvalues.objectAtIndex(0)).integerValue()) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fVideoPlayerControl.fonReadyEvent) then fVideoPlayerControl.fonReadyEvent(self)
  else fVideoPlayerControl.doOnReady;

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

  //this is always fired in the main thread
  //http://stackoverflow.com/questions/7880742/ios-are-methods-called-by-delegates-and-observers-executed-on-the-main-thread

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemDidPlayToEndTime', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fVideoPlayerControl.fOnItemDidPlayToEndTimeEvent) then fVideoPlayerControl.fOnItemDidPlayToEndTimeEvent(self)
  else fVideoPlayerControl.DoOnItemDidPlayToEndTime;

end;

{****************************************************}
//Posted when the item failed to play to its end time.
//The user info dictionary contains an error object that describes
//the problem—see AVPlayerItemFailedToPlayToEndTimeErrorKey.
procedure TALIOSVideoPlayer.TALNotificationsDelegate.ItemFailedToPlayToEndTime;
begin

  //this is always fired in the main thread
  //http://stackoverflow.com/questions/7880742/ios-are-methods-called-by-delegates-and-observers-executed-on-the-main-thread

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemFailedToPlayToEndTime', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fVideoPlayerControl.fOnItemFailedToPlayToEndTimeEvent) then fVideoPlayerControl.fOnItemFailedToPlayToEndTimeEvent(self)
  else fVideoPlayerControl.DoOnItemFailedToPlayToEndTime;

end;

{****************************************************************}
//Posted when the item’s current time has changed discontinuously.
procedure TALIOSVideoPlayer.TALNotificationsDelegate.ItemTimeJumped;
begin

  //this is always fired in the main thread
  //http://stackoverflow.com/questions/7880742/ios-are-methods-called-by-delegates-and-observers-executed-on-the-main-thread

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

  //this is always fired in the main thread
  //http://stackoverflow.com/questions/7880742/ios-are-methods-called-by-delegates-and-observers-executed-on-the-main-thread

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

  //this is always fired in the main thread
  //http://stackoverflow.com/questions/7880742/ios-are-methods-called-by-delegates-and-observers-executed-on-the-main-thread

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

  //this is always fired in the main thread
  //http://stackoverflow.com/questions/7880742/ios-are-methods-called-by-delegates-and-observers-executed-on-the-main-thread

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemNewErrorLogEntry', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

end;

{***********************************}
constructor TALIOSVideoPlayer.Create;
begin

  {$IF defined(DEBUG)}
  ALLog('TALIOSVideoPlayer.Create', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  inherited;
  FPlayer := nil;
  FPlayerItem := nil;
  FPlayerItemVideoOutput := nil;
  FKVODelegate := nil;
  FNotificationsDelegate := nil;
  FFrameRefreshTimer := nil;
  fhasNONewPixelBufferForItemTimeCounter := 0;
  {$IFDEF DEBUG}
  fFrameRefreshCounter := 0;
  {$ENDIF}
  fbitmap := nil;
  fTextureRef := 0;
  fvideoTextureCacheRef := 0;
  fState := vpsIdle;
  fLooping := False;
  fOnFrameAvailableEvent := nil;
  fOnBufferingUpdateEvent := nil;
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fonVideoSizeChangedEvent := nil;
  fonReadyEvent := nil;
  fOnItemDidPlayToEndTimeEvent := nil;
  fonItemFailedToPlayToEndTimeEvent := nil;

end;

{***********************************}
destructor TALIOSVideoPlayer.Destroy;
begin

  {$IF defined(DEBUG)}
  ALLog('TALIOSVideoPlayer.Destroy', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  //-----
  if FFrameRefreshTimer <> nil then begin
    FFrameRefreshTimer.Enabled := False;
    alFreeAndNil(FFrameRefreshTimer);
  end;

  //-----
  TThread.Synchronize(nil,
    procedure
    begin
      if FNotificationsDelegate <> nil then begin
        TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(FNotificationsDelegate.GetObjectID);
        AlFreeAndNil(FNotificationsDelegate);
      end;
      //-----
      if FPlayer <> nil then
        FPlayer.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status')); // << i do it inside synchronize because as Observers are always called in mainthread better to remove it from here
      //-----
      if FPlayerItem <> nil then
        FPlayerItem.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status')); // << i do it inside synchronize because as Observers are always called in mainthread better to remove it from here
      //-----
      if fbitmap <> nil then begin
        ITextureAccess(fBitmap).Handle := 0;
        alfreeAndNil(fbitmap);
      end;
      //-----
      if fTextureRef <> 0 then begin
        CFRelease(pointer(fTextureRef));
        fTextureRef := 0;
      end;
      //-----
      if fvideoTextureCacheRef <> 0 then begin
        CVOpenGLESTextureCacheFlush(fvideoTextureCacheRef, 0);  // The texture cache automatically flushes currently unused resources when you call the
                                                                // CVOpenGLESTextureCacheCreateTextureFromImage function, but can you can also flush the
                                                                // cache explicitly by calling this function. The EAGLContext associated with the cache
                                                                // may be used to delete or unbind textures.
        CFrelease(pointer(fvideoTextureCacheRef));
        fvideoTextureCacheRef := 0;
      end;
    end);

  //-----
  if FPlayer <> nil then begin
    FPlayer.release;
    FPlayer := nil;
  end;
  //-----
  if FPlayerItem <> nil then begin
    FPlayerItem.release;
    FPlayerItem := nil;
  end;
  //-----
  if FPlayerItemVideoOutput <> nil then begin
    FPlayerItemVideoOutput.release;
    FPlayerItemVideoOutput := nil;
  end;
  //-----
  AlFreeAndNil(FKVODelegate);

  //-----
  inherited;

end;

{****************************************************}
//Returns the current time of the current player item.
function TALIOSVideoPlayer.getCurrentPosition: integer;
begin
  if not (fState in [vpsIdle, vpsInitialized, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;
  if (FPlayerItem <> nil) then Result := Trunc(CMTimeGetSeconds(FPlayerItem.currentTime) * 1000)
  else result := 0;
end;

{**********************************************}
function TALIOSVideoPlayer.getDuration: integer;
begin
  if not (fState in [vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;
  Result := Trunc(CMTimeGetSeconds(FPlayerItem.duration) * 1000);
end;

{*************************************************}
function TALIOSVideoPlayer.getVideoHeight: integer;
begin
  if not (fState in [vpsIdle, vpsInitialized, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;
  if FPlayerItem <> nil then Result := round(FPlayerItem.presentationSize.height)
  else result := 0;
end;

{************************************************}
function TALIOSVideoPlayer.getVideoWidth: integer;
begin
  if not (fState in [vpsIdle, vpsInitialized, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := 0;
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;
  if FPlayerItem <> nil then Result := round(FPlayerItem.presentationSize.width)
  else result := 0;
end;

{********************************************}
function TALIOSVideoPlayer.isPlaying: boolean;
begin
  if not (fState in [vpsIdle, vpsInitialized, vpsPrepared, vpsStarted, vpsPaused, vpsStopped, vpsPlaybackCompleted]) then begin
    result := false;
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;
  result := fState = vpsStarted;
end;

{**********************************}
procedure TALIOSVideoPlayer.prepare;
begin
  if not (fState in [vpsInitialized, vpsStopped]) then raise Exception.Create('prepare/prepareAsync can be call only in the Initialized or Stopped state');
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
    fState := vpsPrepared;
    if assigned(fOnPreparedEvent) then
      fOnPreparedEvent(self);
  end
  else fState := vpsPreparing;
end;

{***************************************}
procedure TALIOSVideoPlayer.prepareAsync;
begin
  prepare;
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

  if fState = vpsPaused then exit;
  if not (fState in [vpsStarted, vpsPlaybackCompleted]) then begin
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;

  FPlayer.pause; // Pauses playback of the current item.
                 // Calling this method is the same as setting the rate to 0.0.
  if FFrameRefreshTimer <> nil then FFrameRefreshTimer.Enabled := False;
  fState := vpsPaused;

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

  if fState = vpsStarted then exit;
  if not (fState in [vpsPrepared, vpsPaused, vpsPlaybackCompleted]) then begin
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;

  TThread.queue(nil,
    procedure
    var aAudioSession: AVAudioSession;
    begin
      aAudioSession := TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance);
      aAudioSession.setCategory(AVAudioSessionCategoryPlayback, nil);
      aAudioSession.setActive(True, nil);
    end);

  FPlayer.play; // Begins playback of the current item.
                // Calling this method is the same as setting the rate to 1.0.
  fhasNONewPixelBufferForItemTimeCounter := 0;
  if FFrameRefreshTimer <> nil then FFrameRefreshTimer.Enabled := True;
  fState := vpsStarted;

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

  if fState = vpsStopped then exit;
  if not (fState in [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then begin
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;

  FPlayer.pause; // Pauses playback of the current item.
                 // Calling this method is the same as setting the rate to 0.0.
  if FFrameRefreshTimer <> nil then FFrameRefreshTimer.Enabled := False;
  fState := vpsStopped;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Stop', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{******************************************************}
procedure TALIOSVideoPlayer.seekTo(const msec: Integer);
begin
  if not (fState in [vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then begin
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;
  FPlayer.seekToTime(CMTimeMake(msec, 1));
end;

{*******************************************************************}
procedure TALIOSVideoPlayer.setDataSource(Const aDataSource: String);
var aURL: NSUrl;
    P: Pointer;
begin

  //-----
  if fState <> vpsIdle then raise Exception.Create('setDataSource can be call only in the idle state');

  //-----
  P := TNSUrl.OCClass.URLWithString(StrToNSStr(aDataSource)); // Creates and returns an NSURL object initialized with a provided URL string
  if P = nil then raise EFileNotFoundException.Create(SFileNotFound); // If the URL string was malformed or nil, returns nil.
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
  if TThread.Current.ThreadID = MainThreadID then begin
    FFrameRefreshTimer := TTimer.Create(nil);
    FFrameRefreshTimer.Interval := _FrameRefreshInterval; // equivalent to a fps of 30
    FFrameRefreshTimer.OnTimer := doOnFrameRefresh;
    FFrameRefreshTimer.Enabled := False;
  end;

  TThread.Synchronize(nil,
  procedure
  begin

    //-----
    fBitmap := TalTexture.Create(False);
    fBitmap.PixelFormat := TCustomContextIOS.PixelFormat;

    //-----
    fNotificationsDelegate := TALNotificationsDelegate.Create(self);
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemDidPlayToEndTime'), StringToID('AVPlayerItemDidPlayToEndTimeNotification'), nil);
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemFailedToPlayToEndTime'), StringToID('AVPlayerItemFailedToPlayToEndTimeNotification'), nil);
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemTimeJumped'), StringToID('AVPlayerItemTimeJumpedNotification'), nil);
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemPlaybackStalled'), StringToID('AVPlayerItemPlaybackStalledNotification'), nil);
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewAccessLogEntry'), StringToID('AVPlayerItemNewAccessLogEntryNotification'), nil);
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewErrorLogEntry'), StringToID('AVPlayerItemNewErrorLogEntryNotification'), nil);

    //-----
    if CVOpenGLESTextureCacheCreate(kCFAllocatorDefault, // allocator: The CFAllocatorRef to use for allocating the texture cache. This parameter can be NULL.
                                    nil, // cacheAttributes: A CFDictionaryRef containing the attributes of the texture cache itself. This parameter can be NULL.
                                    (TCustomContextIOS.SharedContext as ILocalObject).GetObjectID, // eaglContext: The OpenGLES 2.0 context into which the texture objects will be created. OpenGLES 1.x contexts are not supported.
                                    nil, // textureAttributes: A CFDictionaryRef containing the attributes to be used for creating the CVOpenGLESTextureRef objects. This parameter can be NULL.
                                    @fvideoTextureCacheRef) <> kCVReturnSuccess then begin // cacheOut: A pointer to a CVOpenGLESTextureCacheRef where the newly created texture cache will be placed.
      fState := vpsError;
      raise Exception.Create('CVOpenGLESTextureCacheCreate failed!');
    end;

  end);

  //-----
  fState := vpsInitialized;

  {$IFDEF DEBUG}
  ALLog('TALIOSVideoPlayer.setDataSource', 'FPlayer.status: ' + alinttostrU(FPlayer.status) + ' - FPlayerItem.status: ' + alinttostrU(FPlayerItem.status), TalLogType.VERBOSE);
  {$ENDIF}

end;

{*************************************************************}
procedure TALIOSVideoPlayer.setLooping(const looping: Boolean);
begin
  if not (fState in [vpsIdle, vpsInitialized, vpsStopped, vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then begin
    fState := vpsError;
    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);
    exit;
  end;
  fLooping := Looping;
end;

{*********************************************************}
procedure TALIOSVideoPlayer.setVolume(const Value: Single);
var aVolume: Single;
begin
  if not (fState in [vpsIdle, vpsInitialized, vpsStopped, vpsPrepared, vpsStarted, vpsPaused, vpsPlaybackCompleted]) then exit;
  aVolume := Value;
  if aVolume < 0 then aVolume := 0
  else if aVolume > 1 then aVolume := 1;
  FPlayer.setVolume(aVolume); // The audio playback volume for the player, ranging from 0.0 through 1.0 on a linear scale.
                              // A value of 0.0 indicates silence; a value of 1.0 (the default) indicates full audio volume for the player instance.
end;

{************************************************************}
procedure TALIOSVideoPlayer.doOnFrameRefresh(Sender: TObject);
var aPixelBuffer: CVPixelBufferRef;
    {$IFDEF DEBUG}
    aStopWatch1, aStopWatch2: TstopWatch;
    {$ENDIF}
    aPrevTextureRef: CVOpenGLESTextureRef;
    aWidth, aHeight: integer;
    T: CMTime;
begin

  //stop the timer if we encoutered some error
  if fState <> vpsStarted then begin
    if FFrameRefreshTimer <> nil then FFrameRefreshTimer.Enabled := False;
    Exit;
  end;

  //in case
  if FPlayerItemVideoOutput = nil then exit;

  {$IFDEF DEBUG}
  aStopWatch1 := TstopWatch.StartNew;
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

      TThread.Synchronize(nil,
        procedure
        begin

          {$IFDEF DEBUG}
          aStopWatch2 := TstopWatch.StartNew;
          {$ENDIF}

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
          TALTextureAccessPrivate(fBitmap).FWidth := aWidth;
          TALTextureAccessPrivate(fBitmap).FHeight := aHeight; // we can't use setsize because it's fill finalise the texture
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

          {$IFDEF DEBUG}
          aStopWatch2.stop;
          {$ENDIF}

        end);

    finally
      CVPixelBufferRelease(aPixelBuffer);
    end;

    //-----
    fhasNONewPixelBufferForItemTimeCounter := 0;

    {$IFDEF DEBUG}
    inc(fFrameRefreshCounter);
    if fFrameRefreshCounter mod 100 = 0 then begin
      aStopWatch1.Stop;
      fFrameRefreshCounter := 0;
      ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'Total TimeTaken: ' + ALFormatFloatU('0.00', aStopWatch1.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU) +
                                                     ' - Synchronize TimeTaken: ' + ALFormatFloatU('0.00', aStopWatch2.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
    end;
    {$ENDIF}

  end
  else begin

    {$IFDEF DEBUG}
    ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'hasNewPixelBufferForItemTime:NO', TalLogType.VERBOSE);
    {$ENDIF}

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
                                                     '- currentTime: ' + alinttostrU(fPlayer.currentTime.value), TalLogType.VERBOSE);
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
    ALLog('TALIOSVideoPlayer.doOnReady', 'Ready', TalLogType.VERBOSE);
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
    if fState = vpsPreparing then begin
      fState := vpsPrepared;
      if assigned(fOnPreparedEvent) then
        fOnPreparedEvent(self);
    end;

  end
  else if (fPlayer.status = AVPlayerStatusFailed) or
          (FPlayerItem.status = AVPlayerItemStatusFailed) then begin

    {$IFDEF DEBUG}
    ALLog('TALIOSVideoPlayer.doOnReady', 'Failed', TalLogType.Error);
    {$ENDIF}

    //fire the fOnErrorEvent
    if fState <> vpsError then begin
      fState := vpsError;
      if assigned(fOnErrorEvent) then
        fOnErrorEvent(self);
    end;

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
    if FFrameRefreshTimer <> nil then FFrameRefreshTimer.Enabled := False;
    fState := vpsPlaybackCompleted;
    if assigned(fOnCompletionEvent) then
      fOnCompletionEvent(self);
  end;
end;

{********************************************************}
procedure TALIOSVideoPlayer.doOnItemFailedToPlayToEndTime;
begin
  if fState <> vpsError then begin

    if FFrameRefreshTimer <> nil then
      FFrameRefreshTimer.Enabled := False;

    fState := vpsError;

    if assigned(fOnErrorEvent) then
      fOnErrorEvent(self);

  end;
end;

{$ENDIF}

{$IF defined(MSWINDOWS)}

{***********************************}
constructor TALWinVideoPlayer.Create;
begin
  inherited;
  fbitmap := nil;
  fOnFrameAvailableEvent := nil;
  fOnBufferingUpdateEvent := nil;
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

{*****************************************************}
function TALWinVideoPlayer.getCurrentPosition: integer;
begin
  result := 0;
end;

{**********************************************}
function TALWinVideoPlayer.getDuration: integer;
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

{**********************************}
procedure TALWinVideoPlayer.prepare;
begin
end;

{***************************************}
procedure TALWinVideoPlayer.prepareAsync;
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

{******************************************************}
procedure TALWinVideoPlayer.seekTo(const msec: Integer);
begin
end;

{*******************************************************************}
procedure TALWinVideoPlayer.setDataSource(Const aDataSource: String);
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

{$ENDIF}

{$IF defined(_MACOS)}

{*************************************}
constructor TALMacOSVideoPlayer.Create;
begin
  inherited;
  fbitmap := nil;
  fOnFrameAvailableEvent := nil;
  fOnBufferingUpdateEvent := nil;
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

{*******************************************************}
function TALMacOSVideoPlayer.getCurrentPosition: integer;
begin
  result := 0;
end;

{************************************************}
function TALMacOSVideoPlayer.getDuration: integer;
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

{************************************}
procedure TALMacOSVideoPlayer.prepare;
begin
end;

{*****************************************}
procedure TALMacOSVideoPlayer.prepareAsync;
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

{********************************************************}
procedure TALMacOSVideoPlayer.seekTo(const msec: Integer);
begin
end;

{*********************************************************************}
procedure TALMacOSVideoPlayer.setDataSource(Const aDataSource: String);
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

{$ENDIF}

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
  fVideoPlayerControl.OnBufferingUpdate := doOnBufferingUpdate;
  fVideoPlayerControl.OnCompletion := doOnCompletion;
  fVideoPlayerControl.onVideoSizeChanged := doonVideoSizeChanged;
  //-----
  fOnErrorEvent := nil;
  fOnPreparedEvent := nil;
  fOnFrameAvailableEvent := nil;
  fOnBufferingUpdateEvent := nil;
  fOnCompletionEvent := nil;
  fonVideoSizeChangedEvent := nil;
  //-----
  FAutoStartWhenPrepared := False;
  fState := vpsIdle; // << When a MediaPlayer object is just created using new or after reset() is called, it is in the Idle state
  //-----
  fTag := 0;
  FTagObject := nil;
  FTagFloat := 0.0;
end;

{********************************}
destructor TALVideoPlayer.Destroy;
begin
  AlFreeAndNil(fVideoPlayerControl);
  inherited;
end;

{*************************************************************************************}
//Valid Sates: Idle, Initialized, Prepared, Started, Paused, Stopped, PlaybackCompleted
//Invalid states: Error
//On Success State:  -
//On Error State: Error
function TALVideoPlayer.getCurrentPosition: integer;
begin
  result := fVideoPlayerControl.getCurrentPosition;
end;

{******************************************************************}
//Valid Sates: Prepared, Started, Paused, Stopped, PlaybackCompleted
//Invalid states: Idle, Initialized, Error
//On Success State: -
//On Error State: Error
function TALVideoPlayer.getDuration: integer;
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

{*************************************************************************************}
//Valid Sates: Idle, Initialized, Prepared, Started, Paused, Stopped, PlaybackCompleted
//Invalid states: Error
//On Success State: -
//On Error State: Error
function TALVideoPlayer.getVideoHeight: integer;
begin
  result := fVideoPlayerControl.getVideoHeight;
end;

{*************************************************************************************}
//Valid Sates: Idle, Initialized, Prepared, Started, Paused, Stopped, PlaybackCompleted
//Invalid states: Error
//On Success State: -
//On Error State: Error
function TALVideoPlayer.getVideoWidth: integer;
begin
  result := fVideoPlayerControl.getVideoWidth;
end;

{*************************************************************************************}
//Valid Sates: Idle, Initialized, Prepared, Started, Paused, Stopped, PlaybackCompleted
//Invalid states: Error
//On Success State: -
//On Error State: Error
function TALVideoPlayer.isPlaying: boolean;
begin
  result := fVideoPlayerControl.isplaying;
end;

{************************************************}
//Valid Sates: Started, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Prepared, Stopped, Error
//On Success State: Paused
//On Error State: Error
procedure TALVideoPlayer.pause;
begin
  if fState = vpsPaused then exit;
  fState := vpsPaused;
  fVideoPlayerControl.pause;
end;

{*********************************}
//Valid Sates: Initialized, Stopped
//Invalid states: Idle, Prepared, Started, Paused, PlaybackCompleted, Error
//On Success State: Preparing
//On Error State: Calling this method in an invalid state throws an IllegalStateException.
procedure TALVideoPlayer.prepare(const aAutoStartWhenPrepared: Boolean);
begin
  FAutoStartWhenPrepared := aAutoStartWhenPrepared;
  fState := vpsPreparing;
  fVideoPlayerControl.prepare;
end;

{*********************************}
//Valid Sates: Initialized, Stopped
//Invalid states: Idle, Prepared, Started, Paused, PlaybackCompleted, Error
//On Success State: Preparing
//On Error State: Calling this method in an invalid state throws an IllegalStateException.
procedure TALVideoPlayer.prepareAsync(const aAutoStartWhenPrepared: Boolean);
begin
  FAutoStartWhenPrepared := aAutoStartWhenPrepared;
  fState := vpsPreparing;
  fVideoPlayerControl.prepareAsync;
end;

{*********************************************************}
//Valid Sates: Prepared, Started, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Stopped, Error
//On Success State: -
//On Error State: Error
procedure TALVideoPlayer.seekTo(const msec: Integer);
begin
  fVideoPlayerControl.seekTo(msec);
end;

{*****************}
//Valid Sates: Idle
//Invalid states: Initialized, Prepared, Started, Paused, Stopped, PlaybackCompleted, Error
//On Success State: Initialized
//On Error State: Calling this method in an invalid state throws an IllegalStateException.
procedure TALVideoPlayer.setDataSource(const aDataSource: String);
begin
  fState := vpsInitialized;
  fVideoPlayerControl.setDataSource(aDataSource);
end;

{*************************************************************************************}
//Valid Sates: Idle, Initialized, Stopped, Prepared, Started, Paused, PlaybackCompleted
//Invalid states: Error
//On Success State: -
//On Error State: Error
procedure TALVideoPlayer.setLooping(const looping: Boolean);
begin
  fVideoPlayerControl.setLooping(looping);
end;

{**************************************************************************************}
//Valid Sates: Idle, Initialized, Stopped, Prepared, Started, Paused, PlaybackCompleted
//Invalid states: Error
//On Success State: -
//On Error State: -
procedure TALVideoPlayer.setVolume(const Value: Single);
begin
  fVideoPlayerControl.setVolume(Value);
end;

{*********************************************************}
//Valid Sates: Prepared, Started, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Stopped, Error
//On Success State: Started
//On Error State: Error
procedure TALVideoPlayer.Start;
begin
  if fState = vpsStarted then exit;
  fState := vpsStarted;
  fVideoPlayerControl.Start;
end;

{******************************************************************}
//Valid Sates: Prepared, Started, Stopped, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Error
//On Success State: Stopped
//On Error State: Error
procedure TALVideoPlayer.Stop;
begin
  if fState = vpsStopped then exit;
  fState := vpsStopped;
  fVideoPlayerControl.Stop;
end;

{*******************************************************}
procedure TALVideoPlayer.doOnCompletion(Sender: TObject);
begin
  fState := vpsPlayBackCompleted;
  if assigned(fOnCompletionEvent) then fOnCompletionEvent(Self);
end;

{**************************************************}
procedure TALVideoPlayer.doOnError(Sender: TObject);
begin
  fState := vpsError;
  if assigned(fOnErrorEvent) then fOnErrorEvent(Self);
end;

{*****************************************************}
procedure TALVideoPlayer.doOnPrepared(Sender: TObject);
begin
  fState := vpsPrepared;
  if FAutoStartWhenPrepared then start;
  if assigned(fOnPreparedEvent) then fOnPreparedEvent(Self);
end;

{***********************************************************}
procedure TALVideoPlayer.doOnFrameAvailable(Sender: TObject);
begin
  if assigned(fOnFrameAvailableEvent) then fOnFrameAvailableEvent(Self);
end;

{*************************************************************************************}
procedure TALVideoPlayer.doOnBufferingUpdate(const Sender: TObject; const mp: integer);
begin
  if assigned(fOnBufferingUpdateEvent) then fOnBufferingUpdateEvent(Self, mp);
end;

{****************************************************************************************************************}
procedure TALVideoPlayer.doOnVideoSizeChanged(const Sender: TObject; const width: Integer; const height: Integer);
begin
  if assigned(fOnVideoSizeChangedEvent) then fOnVideoSizeChangedEvent(Self, width, height);
end;

{*************************************}
constructor TALVideoPlayerAsync.Create;
begin

  fSignal := TEvent.Create(nil, false{ManualReset}, false, '');
  FReady := False;
  //-----
  fVideoPlayerControl := nil;
  //-----
  fOnErrorEvent := nil;
  fOnPreparedEvent := nil;
  fOnFrameAvailableEvent := nil;
  fOnBufferingUpdateEvent := nil;
  fOnCompletionEvent := nil;
  fonVideoSizeChangedEvent := nil;
  //-----
  FAutoStartWhenPrepared := False;
  fState := integer(vpsIdle); // << When a MediaPlayer object is just created using new or after reset() is called, it is in the Idle state
  //-----
  fTag := 0;
  FTagObject := nil;
  FTagFloat := 0.0;

  //-----
  fDoSetDataSource := False;
  //fDoSetDataSourceValue: String;
  fDoPrepare := False;
  fDoPause := False;
  fDoStart := False;
  fDoStop := False;
  fDoSetVolume := False;
  //fDoSetVolumeValue: single;
  fDoSetLooping := False;
  //fDoSetLoopingValue: boolean;
  fDoSeekTo := False;
  //fDoSeekToValue: integer;
  fDoGetDuration := False;
  fDoGetDurationSignal := TEvent.Create(nil, True{ManualReset}, false, '');
  //fDoGetDurationValue: integer;
  {$IF defined(IOS)}
  fDoOnReady := False;
  fDoOnItemDidPlayToEndTime := False;
  fDoOnItemFailedToPlayToEndTime := False;
  {$ENDIF}

  //-----
  inherited Create(False); // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010

end;

{*************************************}
destructor TALVideoPlayerAsync.Destroy;
begin
  Terminate;
  fSignal.SetEvent;
  WaitFor;
  ALFreeAndNil(fSignal);
  ALFreeAndNil(fDoGetDurationSignal);
  inherited;
end;

{************************************}
procedure TALVideoPlayerAsync.Execute;
{$IF defined(IOS)}
var aTimeout: LongWord;
{$ENDIF}
begin

  {$IF defined(IOS)}
  aTimeout := INFINITE;
  {$ENDIF}

  {$IF defined(android)}
  fVideoPlayerControl := TALAndroidVideoPlayer.Create;
  {$ELSEIF defined(IOS)}
  fVideoPlayerControl := TALIOSVideoPlayer.Create;
  {$ELSEIF defined(MSWINDOWS)}
  fVideoPlayerControl := TALWinVideoPlayer.Create;
  {$ELSEIF defined(_MACOS)}
  fVideoPlayerControl := TALMacOSVideoPlayer.Create;
  {$ENDIF}
  try

    //init fVideoPlayerControl
    fVideoPlayerControl.OnError := DoOnError;
    fVideoPlayerControl.OnPrepared := doOnPrepared;
    fVideoPlayerControl.OnFrameAvailable := doOnFrameAvailable;
    fVideoPlayerControl.OnBufferingUpdate := doOnBufferingUpdate;
    fVideoPlayerControl.OnCompletion := doOnCompletion;
    fVideoPlayerControl.onVideoSizeChanged := doOnVideoSizeChanged;
    {$IF defined(IOS)}
    fVideoPlayerControl.onReady := OnReady;
    fVideoPlayerControl.OnItemDidPlayToEndTime := OnItemDidPlayToEndTime;
    fVideoPlayerControl.OnItemFailedToPlayToEndTime := OnItemFailedToPlayToEndTime;
    {$endif}

    //open FReady
    FReady := True;

    //loop still not terminated
    while not Terminated do begin
      Try

        //wait the signal
        FSignal.WaitFor({$IF defined(IOS)}aTimeout{$ELSE}INFINITE{$ENDIF});

        //fDoSetDataSource
        if fDoSetDataSource then begin
          if Terminated then Break;
          fDoSetDataSource := False;
          AtomicExchange(fState, integer(vpsInitialized));
          fVideoPlayerControl.setDataSource(fDoSetDataSourceValue);
        end;

        //fDoOnReady
        {$IF defined(IOS)}
        if fDoOnReady then begin
          if Terminated then Break;
          fDoOnReady := False;
          fVideoPlayerControl.doOnReady;
        end;
        {$endif}

        //fDoOnItemDidPlayToEndTime
        {$IF defined(IOS)}
        if fDoOnItemDidPlayToEndTime then begin
          if Terminated then Break;
          fDoOnItemDidPlayToEndTime := False;
          fVideoPlayerControl.DoOnItemDidPlayToEndTime;
          if fVideoPlayerControl.fState = vpsPlaybackCompleted then aTimeout := INFINITE;
        end;
        {$endif}

        //fDoOnItemFailedToPlayToEndTime
        {$IF defined(IOS)}
        if fDoOnItemFailedToPlayToEndTime then begin
          if Terminated then Break;
          fDoOnItemFailedToPlayToEndTime := False;
          fVideoPlayerControl.DoOnItemFailedToPlayToEndTime;
          aTimeout := INFINITE;
        end;
        {$endif}

        //fDoSetVolume
        if fDoSetVolume then begin
          if Terminated then Break;
          fDoSetVolume := False;
          fVideoPlayerControl.setVolume(AtomicCmpExchange(fDoSetVolumeValue, -1, -1) / 100);
        end;

        //fDoSetLooping
        if fDoSetLooping then begin
          if Terminated then Break;
          fDoSetLooping := False;
          fVideoPlayerControl.setLooping(fDoSetLoopingValue);
        end;

        //fDoPrepare
        if fDoPrepare then begin
          if Terminated then Break;
          fDoPrepare := False;
          AtomicExchange(fState, integer(vpsPreparing));
          fVideoPlayerControl.prepare;
        end;

        //fDoStart
        if fDoStart then begin
          if Terminated then Break;
          fDoStart := False;
          if State <> vpsStarted then begin
            AtomicExchange(fState, integer(vpsStarted));
            fVideoPlayerControl.Start;
          end;
          {$IF defined(IOS)}
          aTimeout := _FrameRefreshInterval;
          {$ENDIF}
        end;

        //fDoSeekTo
        if fDoSeekTo then begin
          if Terminated then Break;
          fDoSeekTo := False;
          fVideoPlayerControl.seekTo(AtomicCmpExchange(fDoSeekToValue, -1, -1));
        end;

        //fDoGetDuration
        if fDoGetDuration then begin
          if Terminated then Break;
          fDoGetDuration := False;
          AtomicExchange(fDoGetDurationValue, fVideoPlayerControl.GetDuration);
          fDoGetDurationSignal.SetEvent;
        end;

        //fDoPause
        if fDoPause then begin
          if Terminated then Break;
          fDoPause := False;
          if State <> vpsPaused then begin
            AtomicExchange(fState, integer(vpsPaused));
            fVideoPlayerControl.pause;
          end;
          {$IF defined(IOS)}
          aTimeout := INFINITE;
          {$ENDIF}
        end;

        //fDoStop
        if fDoStop then begin
          if Terminated then Break;
          fDoStop := False;
          if State <> vpsStopped then begin
            AtomicExchange(fState, integer(vpsStopped));
            fVideoPlayerControl.Stop;
          end;
          {$IF defined(IOS)}
          aTimeout := INFINITE;
          {$ENDIF}
        end;

        //doOnFrameRefresh
        {$IF defined(IOS)}
        if Terminated then Break;
        fVideoPlayerControl.doOnFrameRefresh(nil);
        {$ENDIF}

      Except
        on E: Exception do begin
          {$IF defined(DEBUG)}
          ALLog('TALVideoPlayerAsync.Execute', 'Error: ' + E.Message, TalLogType.Error);
          {$ENDIF}
          {$IF defined(IOS)}
          aTimeout := INFINITE;
          {$ENDIF}
          AtomicExchange(fState, integer(vpsError));
          DoOnError(self);
        end;
      End;
    end;

    //close FReady
    FReady := False;

  finally
    AlFreeAndNil(fVideoPlayerControl);
  end;

end;

{******************************************************************}
//Valid Sates: Prepared, Started, Paused, Stopped, PlaybackCompleted
//Invalid states: Idle, Initialized, Error
//On Success State: -
//On Error State: Error
function TALVideoPlayerAsync.getDuration: integer;
begin
  fDoGetDurationSignal.ResetEvent;
  fDogetDuration := True;
  fSignal.SetEvent;
  fDoGetDurationSignal.WaitFor;
  result := AtomicCmpExchange(fDogetDurationValue, -1, -1);
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALVideoPlayerAsync.GetBitmap: TalTexture;
{$ELSE}
function TALVideoPlayerAsync.GetBitmap: Tbitmap;
{$ENDIF}
begin
  if not FReady then result := nil
  else result := fVideoPlayerControl.Bitmap;
end;

{*******************************************************}
function TALVideoPlayerAsync.getState: TVideoPlayerState;
begin
  result := TVideoPlayerState(AtomicCmpExchange(Fstate, -1, -1));
end;

{************************************************}
//Valid Sates: Started, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Prepared, Stopped, Error
//On Success State: Paused
//On Error State: Error
procedure TALVideoPlayerAsync.pause;
begin
  fDoStart := False;
  if fDoPause then exit;
  fDoPause := True;
  fSignal.SetEvent;
end;

{*********************************}
//Valid Sates: Initialized, Stopped
//Invalid states: Idle, Prepared, Started, Paused, PlaybackCompleted, Error
//On Success State: Preparing
//On Error State: Calling this method in an invalid state throws an IllegalStateException.
procedure TALVideoPlayerAsync.prepare(const aAutoStartWhenPrepared: Boolean);
begin
  FAutoStartWhenPrepared := aAutoStartWhenPrepared;
  fDoPrepare := True;
  fSignal.SetEvent;
end;

{*********************************************************}
//Valid Sates: Prepared, Started, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Stopped, Error
//On Success State: -
//On Error State: Error
procedure TALVideoPlayerAsync.seekTo(const msec: Integer);
begin
  AtomicExchange(fDoSeekToValue, msec);
  fDoSeekTo := True;
  fSignal.SetEvent;
end;

{*****************}
//Valid Sates: Idle
//Invalid states: Initialized, Prepared, Started, Paused, Stopped, PlaybackCompleted, Error
//On Success State: Initialized
//On Error State: Calling this method in an invalid state throws an IllegalStateException.
procedure TALVideoPlayerAsync.setDataSource(const aDataSource: String);
begin
  fDoSetDataSourceValue := aDataSource;
  fDoSetDataSource := True;
  fSignal.SetEvent;
end;

{*************************************************************************************}
//Valid Sates: Idle, Initialized, Stopped, Prepared, Started, Paused, PlaybackCompleted
//Invalid states: Error
//On Success State: -
//On Error State: Error
procedure TALVideoPlayerAsync.setLooping(const looping: Boolean);
begin
  fDoSetLoopingValue := looping;
  fDoSetLooping := true;
  fSignal.SetEvent;
end;

{**************************************************************************************}
//Valid Sates: Idle, Initialized, Stopped, Prepared, Started, Paused, PlaybackCompleted
//Invalid states: Error
//On Success State: -
//On Error State: -
procedure TALVideoPlayerAsync.setVolume(const Value: Single);
begin
  AtomicExchange(fDoSetVolumeValue, round(Value * 100));
  fDoSetVolume := True;
  fSignal.SetEvent;
end;

{*********************************************************}
//Valid Sates: Prepared, Started, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Stopped, Error
//On Success State: Started
//On Error State: Error
procedure TALVideoPlayerAsync.Start;
begin
  fDoPause := False;
  fDoStop := False;
  if fDoStart then exit;
  fDoStart := True;
  fSignal.SetEvent;
end;

{******************************************************************}
//Valid Sates: Prepared, Started, Stopped, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Error
//On Success State: Stopped
//On Error State: Error
procedure TALVideoPlayerAsync.Stop;
begin
  fDoStart := False;
  if fDoStop then exit;
  fDoStop := True;
  fSignal.SetEvent;
end;

{*******************************************}
//launched from the main or background thread
procedure TALVideoPlayerAsync.doOnCompletion(Sender: TObject);
begin
  AtomicExchange(fState, integer(vpsPlayBackCompleted));
  if assigned(fOnCompletionEvent) then begin
    if TThread.Current.ThreadID = MainThreadID then fOnCompletionEvent(Self)
    else begin
      TThread.synchronize(nil,
        procedure
        begin
          fOnCompletionEvent(Self)
        end);
    end;
  end;
end;

{*******************************************}
//launched from the main or background thread
procedure TALVideoPlayerAsync.doOnError(Sender: TObject);
begin
  AtomicExchange(fState, integer(vpsError));
  if assigned(fOnErrorEvent) then begin
    if TThread.Current.ThreadID = MainThreadID then fOnErrorEvent(Self)
    else begin
      TThread.synchronize(nil,
        procedure
        begin
          fOnErrorEvent(Self)
        end);
    end;
  end;
end;

{*******************************************}
//launched from the main or background thread
procedure TALVideoPlayerAsync.doOnPrepared(Sender: TObject);
begin
  AtomicExchange(fState, integer(vpsPrepared));
  if FAutoStartWhenPrepared then start;
  if assigned(fOnPreparedEvent) then begin
    if TThread.Current.ThreadID = MainThreadID then fOnPreparedEvent(Self)
    else begin
      TThread.synchronize(nil,
        procedure
        begin
          fOnPreparedEvent(Self)
        end);
    end;
  end;
end;

{************************************}
//always launched from the main thread
procedure TALVideoPlayerAsync.doOnFrameAvailable(Sender: TObject);
begin
  if assigned(fOnFrameAvailableEvent) then fOnFrameAvailableEvent(Self);
end;

{************************************}
//always launched from the main thread
procedure TALVideoPlayerAsync.doOnBufferingUpdate(const Sender: TObject; const mp: integer);
begin
  if assigned(fOnBufferingUpdateEvent) then fOnBufferingUpdateEvent(Self, mp);
end;

{************************************}
//always launched from the main thread
procedure TALVideoPlayerAsync.doOnVideoSizeChanged(const Sender: TObject; const width: Integer; const height: Integer);
begin
  if assigned(fOnVideoSizeChangedEvent) then fOnVideoSizeChangedEvent(Self, width, height);
end;

{****************}
{$IF defined(IOS)}
procedure TALVideoPlayerAsync.OnReady(Sender: TObject);
begin
  fDoOnReady := True;
  fSignal.SetEvent;
end;
{$endif}

{****************}
{$IF defined(IOS)}
procedure TALVideoPlayerAsync.OnItemDidPlayToEndTime(Sender: TObject);
begin
  fDoOnItemDidPlayToEndTime := True;
  fSignal.setEvent;
end;
{$endif}

{****************}
{$IF defined(IOS)}
procedure TALVideoPlayerAsync.OnItemFailedToPlayToEndTime(Sender: TObject);
begin
  fDoOnItemFailedToPlayToEndTime := True;
  fSignal.setEvent;
end;
{$endif}

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

{****************************************************************}
procedure TALVideoPlayerSurface.OnFrameAvailable(Sender: Tobject);
begin
  repaint;
end;

{**************************************************************************}
procedure TALVideoPlayerSurface.SetVideoPlayer(const Value: TALVideoPlayer);
begin
  if fVideoPlayer <> Value then begin
    if (fVideoPlayer <> nil) then ALfreeAndNil(fVideoPlayer);
    fVideoPlayer := Value;
    if fVideoPlayer <> nil then fVideoPlayer.OnFrameAvailable := OnFrameAvailable;
  end;
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

end.
