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
    fLifeObj: TALLifeObj;
    FVideoPlayer: JMediaPlayer;
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
  private
    fbitmap: TALTexture;
    fOnFrameAvailableEvent: TNotifyEvent;
    fOnBufferingUpdateEvent: TALBufferingUpdateNotifyEvent;
    fOnCompletionEvent: TNotifyEvent;
    fOnErrorEvent: TNotifyEvent;
    FOnPreparedEvent: TnotifyEvent;
    fonVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
  protected
  public
    function getCurrentPosition: integer; virtual; abstract;
    function getDuration: integer; virtual; abstract;
    function getVideoHeight: integer; virtual; abstract;
    function getVideoWidth: integer; virtual; abstract;
    function isPlaying: boolean; virtual; abstract;
    procedure pause; virtual; abstract;
    procedure prepareAsync(const aStartWhenReady: Boolean=False); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure seekTo(const msec: Integer); virtual; abstract;
    procedure setDataSource(Const aDataSource: String); virtual; abstract; // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean); virtual; abstract;
    property bitmap: Talbitmap read fbitmap;
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
    function getCurrentPosition: integer; virtual; abstract;
    function getDuration: integer; virtual; abstract;
    function getVideoHeight: integer; virtual; abstract;
    function getVideoWidth: integer; virtual; abstract;
    function isPlaying: boolean; virtual; abstract;
    procedure pause; virtual; abstract;
    procedure prepareAsync(const aStartWhenReady: Boolean=False); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure seekTo(const msec: Integer); virtual; abstract;
    procedure setDataSource(Const aDataSource: String); virtual; abstract; // Sets or get the data source (file-path or http/rtsp URL) to use.
    procedure setLooping(const looping: Boolean); virtual; abstract;
    property bitmap: Tbitmap read fBitmap;
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
    {$IF defined(android)}
    procedure OnFrameAvailable(Sender: Tobject);
    {$ENDIF}
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
  fLifeObj := TALLifeObj.Create;

  //-----
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);

  //-----
  fBitmap := TalTexture.Create(False);
  ALInitializeEXTERNALOESTexture(fBitmap);

  //-----
  FVideoPlayer := TJMediaPlayer.JavaClass.init;
  //-----
  fOnErrorEvent := nil;
  FOnErrorListener := TALErrorListener.Create(Self);
  FVideoPlayer.setOnErrorListener(FOnErrorListener);
  //-----
  fOnPreparedEvent := nil;
  FOnPreparedListener := TALPreparedListener.Create(Self);
  FVideoPlayer.setOnPreparedListener(FOnPreparedListener);
  //-----
  fonVideoSizeChangedEvent := nil;
  fonVideoSizeChangedListener := TALVideoSizeChangedListener.Create(Self);
  FVideoPlayer.setOnVideoSizeChangedListener(fonVideoSizeChangedListener);
  //-----
  fOnBufferingUpdateEvent := nil;
  fOnBufferingUpdateListener := TALBufferingUpdateListener.Create(Self);
  FVideoPlayer.setOnBufferingUpdateListener(fOnBufferingUpdateListener);
  //-----
  fOnCompletionEvent := nil;
  fOnCompletionListener := TALCompletionListener.Create(Self);
  FVideoPlayer.setOnCompletionListener(fOnCompletionListener);
  //-----
  fSurfaceTexture := TJSurfaceTexture.JavaClass.init(fBitmap.Handle);
  //-----
  fOnFrameAvailableEvent := nil;
  FOnFrameAvailableListener := TALFrameAvailableListener.Create(Self);
  fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener);
  //-----
  FVideoPlayer.setSurface(TJSurface.JavaClass.init((fSurfaceTexture)));

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
      FVideoPlayer.reset;
    end);

  //-----
  FVideoPlayer.setOnErrorListener(nil);
  alfreeandNil(FOnErrorListener);
  //-----
  FVideoPlayer.setOnPreparedListener(nil);
  alfreeandNil(FOnPreparedListener);
  //-----
  FVideoPlayer.setOnVideoSizeChangedListener(nil);
  alfreeandNil(fonVideoSizeChangedListener);
  //-----
  FVideoPlayer.setOnBufferingUpdateListener(nil);
  alfreeandNil(fOnBufferingUpdateListener);
  //-----
  FVideoPlayer.setOnCompletionListener(nil);
  alfreeandNil(fOnCompletionListener);
  //-----
  FVideoPlayer.setSurface(nil);
  FVideoPlayer.release;
  FVideoPlayer := nil;
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
  FVideoPlayer.setSurface(nil);

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
  FVideoPlayer.setSurface(TJSurface.JavaClass.init((fSurfaceTexture)));

end;

{******************************************************}
//Gets the current playback position. return the current
//position in milliseconds
function TALAndroidVideoPlayer.getCurrentPosition: integer;
begin
  Result := FVideoPlayer.getCurrentPosition;
end;

{*******************************************************************}
//Gets the duration of the file. return the duration in milliseconds,
//if no duration is available (for example, if streaming live
//content), -1 is returned.
function TALAndroidVideoPlayer.getDuration: integer;
begin
  Result := FVideoPlayer.getDuration;
end;

{*****************************************************************}
//Returns the height of the video. the height of the video, or 0 if
//there is no video, no display surface was set, or the height has not
//been determined yet. The OnVideoSizeChangedListener can be registered
//via setOnVideoSizeChangedListener(OnVideoSizeChangedListener) to
//provide a notification when the height is available.
function TALAndroidVideoPlayer.getVideoHeight: integer;
begin
  Result := FVideoPlayer.getVideoHeight;
end;

{*****************************************************************}
//Returns the width  of the video. the width  of the video, or 0 if
//there is no video, no display surface was set, or the width has not
//been determined yet. The OnVideoSizeChangedListener can be registered
//via setOnVideoSizeChangedListener(OnVideoSizeChangedListener) to
//provide a notification when the width is available.
function TALAndroidVideoPlayer.getVideoWidth: integer;
begin
  Result := FVideoPlayer.getVideoWidth;
end;

{******************************************}
//Checks whether the MediaPlayer is playing.
function TALAndroidVideoPlayer.isPlaying: boolean;
begin
  Result := FVideoPlayer.isPlaying;
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

  FVideoPlayer.pause;

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

  FVideoPlayer.start;

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

  FVideoPlayer.stop;

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

  FVideoPlayer.prepareAsync;

  {$IFDEF DEBUG}
  aStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.prepareAsync', 'timeTaken: ' + ALFormatFloatU('0.00', aStopWatch.Elapsed.TotalMilliseconds, AlDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*********************************}
//Seeks to specified time position.
procedure TALAndroidVideoPlayer.seekTo(const msec: Integer);
begin
  FVideoPlayer.seekTo(msec);
end;

{********************************************************}
//Sets the data source (file-path or http/rtsp URL) to use.
procedure TALAndroidVideoPlayer.setDataSource(const aDataSource: String);
begin
  FVideoPlayer.setDataSource(StringToJString(aDataSource));
end;

{*********************************************}
//Sets the player to be looping or non-looping.
procedure TALAndroidVideoPlayer.setLooping(const looping: Boolean);
begin
  FVideoPlayer.setLooping(looping);
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
  fVideoPlayerControl := nil; exit;
  {$ELSEIF defined(MSWINDOWS)}
  fVideoPlayerControl := nil; exit;
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
  fVideoPlayerControl.pause;
  fState := vpsPaused;
end;

{*********************************}
//Valid Sates: Initialized, Stopped
//Invalid states: Idle, Prepared, Started, Paused, PlaybackCompleted, Error
//On Success State: Preparing
//On Error State: Calling this method in an invalid state throws an IllegalStateException.
procedure TALVideoPlayer.prepareAsync(const aAutoStartWhenPrepared: Boolean);
begin
  FAutoStartWhenPrepared := aAutoStartWhenPrepared;
  fVideoPlayerControl.prepareAsync;
  fState := vpsPreparing;
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
  fVideoPlayerControl.setDataSource(aDataSource);
  fState := vpsInitialized;
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
  fVideoPlayerControl.Start;
  fState := vpsStarted;
end;

{******************************************************************}
//Valid Sates: Prepared, Started, Stopped, Paused, PlaybackCompleted
//Invalid states: Idle, Initialized, Error
//On Success State: Stopped
//On Error State: Error
procedure TALVideoPlayer.Stop;
begin
  if fState = vpsStopped then exit;
  fVideoPlayerControl.Stop;
  fState := vpsStopped;
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
  {$IF defined(android)}
  fVideoPlayer.OnFrameAvailable := OnFrameAvailable;
  {$endif}
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

{********************}
{$IF defined(android)}
procedure TALVideoPlayerSurface.OnFrameAvailable(Sender: Tobject);
begin
  repaint;
end;
{$endif}

{************************************}
procedure TALVideoPlayerSurface.Paint;
{$IF defined(android)}
var aDestRect: TrectF;
{$endif}
begin

  inherited paint;

  {$IF defined(android)}
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
