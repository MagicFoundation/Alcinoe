unit ALVideoPlayer;

interface

uses system.Classes,
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
        fStopWatch: TstopWatch;
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
    fStartMediaPlayerOnOpenGLContextReset: boolean;
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
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
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
    procedure prepareAsync;
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    property bitmap: TalTexture read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
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
    procedure FrameRefreshOnTimer(Sender: TObject);
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
    procedure prepareAsync;
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    property bitmap: TalTexture read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
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
    procedure prepareAsync;
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    property bitmap: TBitmap read fbitmap;
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read fOnFrameAvailableEvent write fOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read fOnBufferingUpdateEvent write fOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent read fonVideoSizeChangedEvent write fonVideoSizeChangedEvent;
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
    {$ELSE definied(MSWINDOWS)}
    fVideoPlayerControl: TALWinVideoPlayer;
    {$ENDIF}
    //-----
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    fOnPreparedEvent: TNotifyEvent;
    //-----
    FAutoStartWhenPrepared: Boolean;
    fState: TVideoPlayerState;
    //-----
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function GetBitmap: TalTexture;
    {$ELSE}
    function GetBitmap: TBitmap;
    {$ENDIF}
    function GetOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
    function GetOnFrameAvailableEvent: TNotifyEvent;
    function GetonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    procedure SetOnBufferingUpdateEvent(const Value: TALBufferingUpdateNotifyEvent);
    procedure SetOnFrameAvailableEvent(const Value: TNotifyEvent);
    procedure SetonVideoSizeChangedEvent(const Value: TALVideoSizeChangedNotifyEvent);
    procedure doOnCompletion(Sender: TObject);
    procedure doOnError(Sender: TObject);
    procedure doOnPrepared(Sender: TObject);
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
    procedure prepareAsync(const aAutoStartWhenPrepared: Boolean=False);
    procedure Start;
    procedure Stop;
    procedure seekTo(const msec: Integer);
    procedure setDataSource(Const aDataSource: String); // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean);
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property Bitmap: TALTexture read Getbitmap;
    {$ELSE}
    property Bitmap: TBitmap read Getbitmap;
    {$ENDIF}
    property OnError: TNotifyEvent read fOnErrorEvent write fOnErrorEvent;
    property OnPrepared: TNotifyEvent read fOnPreparedEvent write fOnPreparedEvent;
    property OnFrameAvailable: TNotifyEvent read GetOnFrameAvailableEvent write SetOnFrameAvailableEvent;
    property OnBufferingUpdate: TALBufferingUpdateNotifyEvent read GetOnBufferingUpdateEvent write SetOnBufferingUpdateEvent;
    property OnCompletion: TNotifyEvent read fOnCompletionEvent write fOnCompletionEvent;
    property onVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent read GetonVideoSizeChangedEvent write SetonVideoSizeChangedEvent;
    property State: TVideoPlayerState read fState;
    property AutoStartWhenPrepared: boolean read FAutoStartWhenPrepared write FAutoStartWhenPrepared;
  end;

  {*****************************************}
  TALVideoPlayerSurface = class(TALRectangle)
  private
    fVideoPlayer: TALVideoPlayer;
    procedure OnFrameAvailable(Sender: Tobject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property VideoPlayer: TALVideoPlayer read fVideoPlayer write fVideoPlayer;
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
  fStopWatch := TStopWatch.StartNew;
  {$ENDIF}
  fVideoPlayerControl := aVideoPlayerControl;
end;

{**********************************************************************************************************}
procedure TALAndroidVideoPlayer.TALFrameAvailableListener.onFrameAvailable(surfaceTexture: JSurfaceTexture);
var aLifeObj: TALLifeObj;
begin

  {$IF defined(DEBUG)}
  inc(fTotalFramesProcessed);
  if fTotalFramesProcessed >= 100 then begin
    fStopWatch.Stop;
    ALLog('TALAndroidVideoPlayer.onFrameAvailable', 'fps: ' + ALFormatFloatU('0.00', (fTotalFramesProcessed) / max(1, (fStopWatch.Elapsed.TotalMilliseconds / 1000)), AlDefaultFormatSettingsU) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
    fTotalFramesProcessed := 0;
    fStopWatch := TStopWatch.StartNew;
  end;
  {$ENDIF}

  //SurfaceTexture objects may be created on any thread. updateTexImage() may only be called on the
  //thread with the OpenGL ES context that contains the texture object. The frame-available callback
  //is called on an arbitrary thread, so unless special care is taken updateTexImage() should not be
  //called directly from the callback.
  // >> so i as understand i can call updateTexImage in other thread than the current thread, it's
  // >> seam to be thread safe
  aLifeObj := fVideoPlayerControl.fLifeObj;
  tthread.queue(nil,
    procedure
    begin
      if not aLifeObj.alive then exit;
      if fVideoPlayerControl.fSurfaceTexture <> nil then
        fVideoPlayerControl.fSurfaceTexture.updateTexImage;
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
  //MEDIA_ERROR_UNKNOWN
  //MEDIA_ERROR_SERVER_DIED
  //
  //extra	int: an extra code, specific to the error. Typically implementation dependent.
  //MEDIA_ERROR_IO
  //MEDIA_ERROR_MALFORMED
  //MEDIA_ERROR_UNSUPPORTED
  //MEDIA_ERROR_TIMED_OUT
  //MEDIA_ERROR_SYSTEM (-2147483648) - low-level system error.

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onError', 'what: ' + alinttostrU(what) +
                                         ' - extra: ' + alinttostrU(extra) +
                                         ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}

  result := false; // True if the method handled the error, false if it didn't. Returning false, or not having an
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
      if assigned(TALTextureAccessPrivate(fVideoPlayerControl.fBitmap)) then begin
        TALTextureAccessPrivate(fVideoPlayerControl.fBitmap).FWidth := width;
        TALTextureAccessPrivate(fVideoPlayerControl.fBitmap).FHeight := height; // we can't use setsize because it's fill finalise the texture
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
      if assigned(fVideoPlayerControl.fonVideoSizeChangedEvent) then
        fVideoPlayerControl.fonVideoSizeChangedEvent(fVideoPlayerControl, width, height);
    end);

end;

{********************************************************************************************************************}
constructor TALAndroidVideoPlayer.TALBufferingUpdateListener.Create(const aVideoPlayerControl: TALAndroidVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{***************************************************************************************************************}
procedure TALAndroidVideoPlayer.TALBufferingUpdateListener.onBufferingUpdate(mp: JMediaPlayer; percent: Integer);
var aLifeObj: TALLifeObj;
begin

  {$IF defined(DEBUG)}
  //if percent > 0 then
  //  ALLog('TALAndroidVideoPlayer.onBufferingUpdate', 'percent buffered: ' + ALinttostrU(percent) + '%'+
  //                                                   ' - percent played: ' + ALIntToStrU(round((100 / fVideoPlayerControl.FVideoPlayer.getDuration) * fVideoPlayerControl.FVideoPlayer.getCurrentPosition))+'%' +
  //                                                   ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
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

  //-----
  inherited create;

  //-----
  fStartMediaPlayerOnOpenGLContextReset := False;

  //-----
  fLifeObj := TALLifeObj.Create;

  //-----
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);

  //-----
  fBitmap := TalTexture.Create(False);
  ALInitializeEXTERNALOESTexture(fBitmap);

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

  //stop all futur TThread.queue to execute
  //and also signal that the object is destroying
  fLifeObj.alive := False;

  //to make sure none of the listeners get called anymore
  //FVideoPlayer.reset will do
  //
  //  mEventHandler.removeCallbacksAndMessages(null);
  //
  //i do it inside the UIThread BECAUSe it's seam that the
  //eventHandler is bound to the UI THread :
  //
  //  Looper looper;
  //  if ((looper = Looper.myLooper()) != null) {
  //    mEventHandler = new EventHandler(this, looper);
  //  } else if ((looper = Looper.getMainLooper()) != null) {
  //    mEventHandler = new EventHandler(this, looper);
  //  } else {
  //    mEventHandler = null;
  //  }
  //
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FMediaPlayer.reset;
    end);

  //-----
  FMediaPlayer.setOnErrorListener(nil);
  alfreeandNil(FOnErrorListener);
  //-----
  FMediaPlayer.setOnPreparedListener(nil);
  alfreeandNil(FOnPreparedListener);
  //-----
  FMediaPlayer.setOnVideoSizeChangedListener(nil);
  alfreeandNil(fonVideoSizeChangedListener);
  //-----
  FMediaPlayer.setOnBufferingUpdateListener(nil);
  alfreeandNil(fOnBufferingUpdateListener);
  //-----
  FMediaPlayer.setOnCompletionListener(nil);
  alfreeandNil(fOnCompletionListener);
  //-----
  FMediaPlayer.setSurface(nil);
  FMediaPlayer.release;
  FMediaPlayer := nil;
  //-----
  fSurfaceTexture.setOnFrameAvailableListener(nil);
  alfreeAndNil(FOnFrameAvailableListener);
  fSurfaceTexture.release;
  fSurfaceTexture := nil;

  //-----
  alFreeandNil(fbitmap);

  //-----
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);

  //-----
  fLifeObj := nil; // << it's the autorefcount process that must free this object

  //-----
  inherited;

end;

{***************************************************************************************************}
procedure TALAndroidVideoPlayer.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin

  //----
  if FMediaPlayer.isPlaying then begin
    FMediaPlayer.pause;
    fStartMediaPlayerOnOpenGLContextReset := True;
  end
  else fStartMediaPlayerOnOpenGLContextReset := false;

  //----
  FMediaPlayer.setSurface(nil);

  //----
  fSurfaceTexture.setOnFrameAvailableListener(nil);
  fSurfaceTexture.release;
  fSurfaceTexture := nil;

  //----
  AlFReeAndNil(fBitmap);

end;

{****************************************************************************************************}
procedure TALAndroidVideoPlayer.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin

  //-----
  alfreeAndNil(fBitmap); // << in case
  fBitmap := TalTexture.Create(False);
  ALInitializeEXTERNALOESTexture(fBitmap);

  //-----
  if fSurfaceTexture <> nil then begin
    fSurfaceTexture.setOnFrameAvailableListener(nil);
    fSurfaceTexture.release;
    fSurfaceTexture := nil;
  end;
  fSurfaceTexture := TJSurfaceTexture.JavaClass.init(fBitmap.Handle);
  fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener);

  //-----
  FMediaPlayer.setSurface(TJSurface.JavaClass.init((fSurfaceTexture)));

  //----
  if fStartMediaPlayerOnOpenGLContextReset then FMediaPlayer.start;

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
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  FMediaPlayer.pause;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.pause', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
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
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  FMediaPlayer.start;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.start', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
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
  aStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  FMediaPlayer.stop;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.stop', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
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
  {$ENDIF}

  FMediaPlayer.prepareAsync;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.prepareAsync', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
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
  FMediaPlayer.setDataSource(StringToJString(aDataSource));
end;

{*********************************************}
//Sets the player to be looping or non-looping.
procedure TALAndroidVideoPlayer.setLooping(const looping: Boolean);
begin
  FMediaPlayer.setLooping(looping);
end;

{$ENDIF}

{$IF defined(IOS)}

{************************************************************************************************}
constructor TALIOSVideoPlayer.TALKVODelegate.Create(const aVideoPlayerControl: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerControl := aVideoPlayerControl;
end;

{**********************************************************************************************************************************************}
procedure TALIOSVideoPlayer.TALKVODelegate.observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer);
var aPixelBufferAttributes: NSMutableDictionary;
begin

  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.observeValueForKeyPath', 'Status:' +  alinttostrU(TNSNumber.Wrap(change.allvalues.objectAtIndex(0)).integerValue()) +
                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  //AVPlayerItemStatusFailed = 2;
  //AVPlayerItemStatusReadyToPlay = 1;
  //AVPlayerItemStatusUnknown = 0;
  //AVPlayerStatusFailed = 2;
  //AVPlayerStatusReadyToPlay = 1;
  //AVPlayerStatusUnknown = 0;
  if (fVideoPlayerControl.fPlayer.status = AVPlayerStatusReadyToPlay) and // The player’s status indicates whether the player can be used for playback.
                                                                          // When the value of this property is failed, you can no longer use the player
                                                                          // for playback and you need to create a new instance to replace it. If this
                                                                          // happens, you can check the value of the player’s error property to determine
                                                                          // the nature of the failure.
                                                                          // This property is key value observable using Key-value observing.
                                                                          // NOTE: The player’s status does not indicate its readiness to play a specific
                                                                          // player item. You should instead use the status property of AVPlayerItem to make
                                                                          // that determination.
     (fVideoPlayerControl.FPlayerItem.status = AVPlayerItemStatusReadyToPlay) // When a player item is created, its status is unknown, meaning its media hasn’t
                                                                              // been loaded and has not yet been enqueued for playback. Associating a player
                                                                              // item with an AVPlayer immediately begins enqueuing the item’s media and preparing
                                                                              // it for playback. When the player item’s media has been loaded and is ready
                                                                              // for use, its status will change to readyToPlay. You can observe this
                                                                              // change using key-value observing.
  then begin

    //i need to do this here because of bug like :
    //https://forums.developer.apple.com/thread/27589
    //http://stackoverflow.com/questions/24800742/iosavplayeritemvideooutput-hasnewpixelbufferforitemtime-doesnt-work-correctly
    if fVideoPlayerControl.FPlayerItemVideoOutput = nil then begin
      aPixelBufferAttributes := TNSMutableDictionary.Create;
      try
        aPixelBufferAttributes.setObject(TNSNumber.OCClass.numberWithInt(kCVPixelFormatType_32BGRA), Pointer(kCVPixelBufferPixelFormatTypeKey));
        fVideoPlayerControl.FPlayerItemVideoOutput := TAVPlayerItemVideoOutput.Wrap(TAVPlayerItemVideoOutput.Alloc.initWithPixelBufferAttributes(aPixelBufferAttributes)); // Initializes and returns a video output object using the specified
                                                                                                                                                                           // pixel buffer attributes.
                                                                                                                                                                           // The pixel buffer attributes required for video output. For a list
                                                                                                                                                                           // of pixel buffer attributes you can include in this dictionary, see
                                                                                                                                                                           // the CVPixelBuffer.h header file in the Core Video framework.
      finally
        aPixelBufferAttributes.release;
        aPixelBufferAttributes := nil;
      end;
      fVideoPlayerControl.FPlayerItemVideoOutput.retain;
      fVideoPlayerControl.FPlayerItem.addOutput(fVideoPlayerControl.FPlayerItemVideoOutput);
    end;

    //fire the fOnPreparedEvent
    if fVideoPlayerControl.fState = vpsPreparing then begin
      fVideoPlayerControl.fState := vpsPrepared;
      if assigned(fVideoPlayerControl.fOnPreparedEvent) then
        fVideoPlayerControl.fOnPreparedEvent(fVideoPlayerControl);
    end;

  end
  else if (fVideoPlayerControl.fPlayer.status = AVPlayerStatusFailed) or
          (fVideoPlayerControl.FPlayerItem.status = AVPlayerItemStatusFailed) then begin

    //fire the fOnErrorEvent
    if fVideoPlayerControl.fState <> vpsError then begin
      fVideoPlayerControl.fState := vpsError;
      if assigned(fVideoPlayerControl.fOnErrorEvent) then
        fVideoPlayerControl.fOnErrorEvent(fVideoPlayerControl);
    end;

  end;

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

  if fVideoPlayerControl.fLooping then begin
    fVideoPlayerControl.FPlayer.seekToTime(CMTimeMake(0, 1));
    fVideoPlayerControl.FPlayer.play;
  end
  else begin
    fVideoPlayerControl.FFrameRefreshTimer.Enabled := False;
    fVideoPlayerControl.fState := vpsPlaybackCompleted;
  end;

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

  //fire the fOnErrorEvent
  if fVideoPlayerControl.fState <> vpsError then begin
    fVideoPlayerControl.FFrameRefreshTimer.Enabled := False;
    fVideoPlayerControl.fState := vpsError;
    if assigned(fVideoPlayerControl.fOnErrorEvent) then
      fVideoPlayerControl.fOnErrorEvent(fVideoPlayerControl);
  end;

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
begin
  inherited;
  FPlayer := nil;
  FPlayerItem := nil;
  FPlayerItemVideoOutput := nil;
  FKVODelegate := nil;
  FNotificationsDelegate := nil;
  FFrameRefreshTimer := nil;
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
end;

{***********************************}
destructor TALIOSVideoPlayer.Destroy;
begin
  if FNotificationsDelegate <> nil then begin
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(FNotificationsDelegate.GetObjectID);
    AlFreeAndNil(FNotificationsDelegate);
  end;
  //-----
  if FFrameRefreshTimer <> nil then begin
    FFrameRefreshTimer.Enabled := False;
    alFreeAndNil(FFrameRefreshTimer);
  end;
  //-----
  if FPlayer <> nil then begin
    FPlayer.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status'));
    FPlayer.release;
    FPlayer := nil;
  end;
  //-----
  if FPlayerItem <> nil then begin
    FPlayerItem.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status'));
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
  alfreeAndNil(fbitmap);
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

{***************************************}
procedure TALIOSVideoPlayer.prepareAsync;
begin
  if not (fState in [vpsInitialized, vpsStopped]) then raise Exception.Create('prepareAsync can be call only in the Initialized or Stopped state');
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
  FFrameRefreshTimer.Enabled := False;
  fState := vpsPaused;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.pause', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************}
procedure TALIOSVideoPlayer.Start;
var aAudioSession: AVAudioSession;
    {$IFDEF DEBUG}
    aStopWatch: TstopWatch;
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

  aAudioSession := TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance);
  aAudioSession.setCategory(AVAudioSessionCategoryPlayback, nil);
  aAudioSession.setActive(True, nil);
  FPlayer.play; // Begins playback of the current item.
                // Calling this method is the same as setting the rate to 1.0.
  FFrameRefreshTimer.Tag := 0;
  FFrameRefreshTimer.Enabled := True;
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
  FFrameRefreshTimer.Enabled := False;
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
  try

    FPlayerItem := TAVPlayerItem.Wrap(TAVPlayerItem.OCClass.playerItemWithURL(aURL)); // return A new player item, prepared to use URL.
                                                                                      // This method immediately returns the item, but with the status AVPlayerItemStatusUnknown.
                                                                                      // Associating the player item with an AVPlayer immediately begins enqueuing its media
                                                                                      // and preparing it for playback. If the URL contains valid data that can be used by
                                                                                      // the player item, its status later changes to AVPlayerItemStatusReadyToPlay. If the
                                                                                      // URL contains no valid data or otherwise can't be used by the player item, its status
                                                                                      // later changes to AVPlayerItemStatusFailed. You can determine the nature of the failure
                                                                                      // by querying the player item’s error property.
    FPlayerItem.retain;

  finally
    aURL.release;
    aURL := nil;
  end;

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
  fNotificationsDelegate := TALNotificationsDelegate.Create(self);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemDidPlayToEndTime'), StringToID('AVPlayerItemDidPlayToEndTimeNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemFailedToPlayToEndTime'), StringToID('AVPlayerItemFailedToPlayToEndTimeNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemTimeJumped'), StringToID('AVPlayerItemTimeJumpedNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemPlaybackStalled'), StringToID('AVPlayerItemPlaybackStalledNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewAccessLogEntry'), StringToID('AVPlayerItemNewAccessLogEntryNotification'), nil);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewErrorLogEntry'), StringToID('AVPlayerItemNewErrorLogEntryNotification'), nil);

  //-----
  FFrameRefreshTimer := TTimer.Create(nil);
  FFrameRefreshTimer.Interval := 34; // equivalent to a fps of 30
  FFrameRefreshTimer.OnTimer := FrameRefreshOnTimer;
  FFrameRefreshTimer.Enabled := False;

  //-----
  fBitmap := TalTexture.Create(False);
  fBitmap.PixelFormat := TCustomContextIOS.PixelFormat;

  //-----
  if CVOpenGLESTextureCacheCreate(kCFAllocatorDefault, // allocator: The CFAllocatorRef to use for allocating the texture cache. This parameter can be NULL.
                                  nil, // cacheAttributes: A CFDictionaryRef containing the attributes of the texture cache itself. This parameter can be NULL.
                                  (TCustomContextIOS.SharedContext as ILocalObject).GetObjectID, // eaglContext: The OpenGLES 2.0 context into which the texture objects will be created. OpenGLES 1.x contexts are not supported.
                                  nil, // textureAttributes: A CFDictionaryRef containing the attributes to be used for creating the CVOpenGLESTextureRef objects. This parameter can be NULL.
                                  @fvideoTextureCacheRef) <> kCVReturnSuccess then begin // cacheOut: A pointer to a CVOpenGLESTextureCacheRef where the newly created texture cache will be placed.
    fState := vpsError;
    raise Exception.Create('CVOpenGLESTextureCacheCreate failed!');
  end;

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

{***************************************************************}
procedure TALIOSVideoPlayer.FrameRefreshOnTimer(Sender: TObject);
var aPixelBuffer: CVPixelBufferRef;
    {$IFDEF DEBUG}
    aStopWatch: TstopWatch;
    {$ENDIF}
    aPrevTextureRef: CVOpenGLESTextureRef;
    aWidth, aHeight: integer;
    T: CMTime;
begin

  //stop the timer if we encoutered some error
  if fState <> vpsStarted then begin
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

    finally
      CVPixelBufferRelease(aPixelBuffer);
    end;

    //-----
    FFrameRefreshTimer.Tag := 0;

    //-----
    if assigned(FonFrameAvailableEvent) then
      FonFrameAvailableEvent(self);

    {$IFDEF DEBUG}
    FFrameRefreshTimer.TagFloat := FFrameRefreshTimer.TagFloat + 1;
    if round(FFrameRefreshTimer.TagFloat) mod 100 = 0 then begin
      aStopWatch.Stop;
      FFrameRefreshTimer.TagFloat := 0;
      ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'TimeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
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
    FFrameRefreshTimer.Tag := FFrameRefreshTimer.Tag + 1;
    if (FFrameRefreshTimer.tag > 30) and // << with an interval of 34 ms mean more than 1s without anything
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
      FFrameRefreshTimer.tag := 0;
    end;

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
  {$ENDIF}
  //-----
  fVideoPlayerControl.OnCompletion := doOnCompletion;
  fVideoPlayerControl.OnError := DoOnError;
  fVideoPlayerControl.OnPrepared := doOnPrepared;
  //-----
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  fOnPreparedEvent := nil;
  //-----
  FAutoStartWhenPrepared := False;
  fState := vpsIdle; // << When a MediaPlayer object is just created using new or after reset() is called, it is in the Idle state
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
  if assigned(fOnCompletionEvent) then fOnCompletionEvent(Sender);
end;

{**************************************************}
procedure TALVideoPlayer.doOnError(Sender: TObject);
begin
  fState := vpsError;
  if assigned(fOnErrorEvent) then fOnErrorEvent(Sender);
end;

{*****************************************************}
procedure TALVideoPlayer.doOnPrepared(Sender: TObject);
begin
  fState := vpsPrepared;
  if FAutoStartWhenPrepared then start;
  if assigned(fOnPreparedEvent) then fOnPreparedEvent(Sender);
end;

{*******************************************************************************}
function TALVideoPlayer.GetOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
begin
  result := fVideoPlayerControl.fOnBufferingUpdateEvent;
end;

{*********************************************************************************************}
procedure TALVideoPlayer.SetOnBufferingUpdateEvent(const Value: TALBufferingUpdateNotifyEvent);
begin
  fVideoPlayerControl.fOnBufferingUpdateEvent := Value;
end;

{*************************************************************}
function TALVideoPlayer.GetOnFrameAvailableEvent: TNotifyEvent;
begin
  result := fVideoPlayerControl.fOnFrameAvailableEvent;
end;

{***************************************************************************}
procedure TALVideoPlayer.SetOnFrameAvailableEvent(const Value: TNotifyEvent);
begin
  fVideoPlayerControl.fOnFrameAvailableEvent := Value;
end;

{*********************************************************************************}
function TALVideoPlayer.GetonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
begin
  result := fVideoPlayerControl.fonVideoSizeChangedEvent;
end;

{***********************************************************************************************}
procedure TALVideoPlayer.SetonVideoSizeChangedEvent(const Value: TALVideoSizeChangedNotifyEvent);
begin
  fVideoPlayerControl.fonVideoSizeChangedEvent := Value;
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

end.
