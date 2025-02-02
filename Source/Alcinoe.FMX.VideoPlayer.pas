(*******************************************************************************
ALVideoPlayer renders a video on a texture, which is crucial because it allows
full integration of the video into the Delphi form. This means you can place any
controls you want on top of the video, as it supports Z-ordering. In contrast,
the official Delphi video players are just native video player windows that sit
on top of the form, and therefore do not support Z-ordering.

On Android, I use ExoPlayer. ExoPlayer is an open-source project that is not
part of the Android framework and is distributed separately from the Android SDK.
ExoPlayer’s standard audio and video components are built on Android’s
MediaCodec API, which was introduced in Android 4.1 (API level 16). Since
ExoPlayer is a library, you can easily benefit from new features as they become
available by updating your app. ExoPlayer supports advanced features like
Dynamic Adaptive Streaming over HTTP (DASH), HLS, SmoothStreaming, and Common
Encryption, which are not supported by MediaPlayer. It is designed to be easy
to customize and extend.

On iOS, I use AVPlayer, which also supports HLS, similar to ExoPlayer.
*******************************************************************************)
unit Alcinoe.FMX.VideoPlayer;

interface

{$I Alcinoe.inc}

uses
  system.Classes,
  system.UITypes,
  {$IF defined(DEBUG)}
  system.diagnostics,
  {$endIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.Gles2ext,
  Alcinoe.AndroidApi.ExoPlayer,
  {$endIF}
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
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
  Alcinoe.fmx.Common,
  Alcinoe.FMX.Types3D,
  Alcinoe.FMX.Graphics,
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
        FVideoPlayerEngine: TALAndroidVideoPlayer;
        procedure DoOnFrameAvailable(surfaceTexture: JSurfaceTexture);
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
    fDataSourceFactory: JDataSource_Factory;
    fSurface: JSurface;
    fSurfaceTexture: JSurfaceTexture;
    {$IF defined(ALSkiaCanvas)}
    fGrBackEndTexture: gr_backendtexture_t;
    fTexture: TALTexture;
    {$ENDIF}
    fDrawable: TALDrawable;
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
    property Drawable: TALDrawable read fDrawable;
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
    class var appAudioSessionActivated: Boolean;
  private
    FPlayer: AVPlayer;
    FPlayerItem: ALAVPlayerItem;
    FPlayerItemVideoOutput: AVPlayerItemVideoOutput;
    FKVODelegate: TKVODelegate;
    fNotificationsDelegate: TNotificationsDelegate;
    FFrameRefreshAnimation: TALDisplayAnimation;
    fhasNONewPixelBufferForItemTimeCounter: integer;
    {$IFDEF DEBUG}
    fFrameRefreshCounter: integer;
    {$ENDIF}
    {$IF defined(ALSkiaCanvas)}
    fGrBackEndTexture: gr_backendtexture_t;
    {$ENDIF}
    fDrawable: TALDrawable;
    fOpenGLTextureRef: CVOpenGLESTextureRef;
    fOpenGLVideoTextureCacheRef: CVOpenGLESTextureCacheRef;
    fMetalTextureRef: CVMetalTextureRef;
    fMetalVideoTextureCacheRef: CVMetalTextureCacheRef;
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
    procedure DoOnFrameRefresh(Sender: TObject);
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
    property Drawable: TALDrawable read fDrawable;
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
    fDrawable: TALDrawable;
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
    property Drawable: TALDrawable read fDrawable;
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
    fDrawable: TALDrawable;
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
    property Drawable: TALDrawable read fDrawable;
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
    fVideoPlayerEngine: TALAndroidVideoPlayer;
    {$ELSEIF defined(IOS)}
    fVideoPlayerEngine: TALIOSVideoPlayer;
    {$ELSEIF defined(MSWINDOWS)}
    fVideoPlayerEngine: TALWinVideoPlayer;
    {$ELSEIF defined(ALMacOS)}
    fVideoPlayerEngine: TALMacOSVideoPlayer;
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
    FTagObject: TObject;
    FTagFloat: Double;
    FTagString: String;
    //-----
    function GetDrawable: TALDrawable;
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
    property Drawable: TALDrawable read GetDrawable;
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
  TALVideoPlayerSurface = class(TALBaseRectangle)
  public
    type
      TFill = class(TALBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
  private
    fVideoPlayer: TALVideoPlayer;
    procedure OnFrameAvailable(Sender: Tobject);
    function GetVideoSizeChangedEvent: TALVideoSizeChangedNotifyEvent;
    procedure SetVideoSizeChangedEvent(const Value: TALVideoSizeChangedNotifyEvent);
  protected
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure resetVideoPlayer;
    property VideoPlayer: TALVideoPlayer read fVideoPlayer;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    property Sides;
    property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
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
    property OnVideoSizeChanged: TALVideoSizeChangedNotifyEvent read GetVideoSizeChangedEvent write SetVideoSizeChangedEvent;
  end;

procedure register;

implementation

uses
  system.SysUtils,
  system.Types,
  {$IF defined(ANDROID)}
  system.Math,
  Androidapi.Helpers,
  Alcinoe.StringUtils,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.CoreFoundation,
  iOSapi.CoreMedia,
  iOSapi.OpenGLES,
  Macapi.Helpers,
  Macapi.ObjCRuntime,
  FMX.Context.GLES.iOS,
  FMX.Context.Metal,
  Alcinoe.StringUtils,
  Alcinoe.HTTP.Client,
  {$ENDIF}
  {$IF defined(ALSkiaCanvas)}
  FMX.Skia.Canvas,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  Fmx.graphics,
  Fmx.types,
  fmx.controls,
  fmx.Types3d,
  Alcinoe.Common;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}

{****************************************************************************************************************}
constructor TALAndroidVideoPlayer.TFrameAvailableListener.Create(const AVideoPlayerEngine: TALAndroidVideoPlayer);
begin
  inherited Create;
  {$IF defined(DEBUG)}
  fTotalFramesProcessed := 0;
  fFpsStopWatch := TStopWatch.StartNew;
  {$ENDIF}
  fVideoPlayerEngine := AVideoPlayerEngine;
end;

{**********************************************************************************************************}
procedure TALAndroidVideoPlayer.TFrameAvailableListener.DoOnFrameAvailable(surfaceTexture: JSurfaceTexture);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerEngine = nil then exit;

  {$IF defined(DEBUG)}
  if (fTotalFramesProcessed mod 1000 = 0) then begin
    fFpsStopWatch.Stop;
    ALLog('TALAndroidVideoPlayer.TFrameAvailableListener.DoOnFrameAvailable', 'fps: ' + ALFormatFloatW('0.00', (fTotalFramesProcessed) / max(1, (ffpsStopWatch.Elapsed.TotalMilliseconds / 1000)), ALDefaultFormatSettingsW));
    fTotalFramesProcessed := 0;
    fFpsStopWatch := TStopWatch.StartNew;
  end;
  inc(fTotalFramesProcessed);
  {$ENDIF}

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
  end;
  {$ELSE}
  if (fVideoPlayerEngine.fDrawable.Width <> fVideoPlayerEngine.fVideoWidth) or
     (fVideoPlayerEngine.fDrawable.Height <> fVideoPlayerEngine.fVideoHeight) then begin
    {$IFNDEF ALCompilerVersionSupported122}
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
  {$ENDIF}

  {$IF defined(DEBUG)}
  var LStopWatch: TStopWatch;
  if (fTotalFramesProcessed mod 1000 = 0) then LStopWatch := TStopWatch.StartNew;
  {$ENDIF}

  {$IFDEF DEBUG}
  if (fTotalFramesProcessed mod 1000 = 0) then begin
    LStopWatch.Stop;
    ALLog('TALAndroidVideoPlayer.onFrameAvailable.updateTexImage', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
  end;
  {$ENDIF}

  if assigned(fVideoPlayerEngine.fOnFrameAvailableEvent) then
    fVideoPlayerEngine.fOnFrameAvailableEvent(fVideoPlayerEngine);

end;

{********************************************************************************************************}
procedure TALAndroidVideoPlayer.TFrameAvailableListener.onFrameAvailable(surfaceTexture: JSurfaceTexture);
begin

  // https://developer.android.com/reference/android/graphics/SurfaceTexture.html
  // SurfaceTexture objects may be created on any thread. updateTexImage() may only be called on the
  // thread with the OpenGL ES context that contains the texture object. The frame-available callback
  // is called on an arbitrary thread, so unless special care is taken updateTexImage() should not be
  // called directly from the callback.
  // So I as understand I can call updateTexImage in other thread than the current thread, it's
  // seam to be thread safe - I already make opengl multithread however the updateTexImage seam
  // to take around 1ms only so their is no really purpose to run it in a different thread than
  // the main thread (who already have the OpenGL ES context)
  //
  // NOTE: as i do setOnFrameAvailableListener(SurfaceTexture.OnFrameAvailableListener listener,Handler handler)
  // with handler = TJHandler.JavaClass.init(TJLooper.javaclass.getMainLooper()) then this event will be
  // always called from the main UI thread

  DoOnFrameAvailable(surfaceTexture);

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
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onEvents');
  {$ENDIF}
end;

{******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTimelineChanged(timeline: JTimeline; reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onTimelineChanged');
  {$ENDIF}
end;

{************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMediaItemTransition(mediaItem: JMediaItem; reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onMediaItemTransition');
  {$ENDIF}
end;

{*******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTracksChanged(tracks: JTracks);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onTracksChanged');
  {$ENDIF}
end;

{****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMediaMetadataChanged(mediaMetadata: JMediaMetadata);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onMediaMetadataChanged');
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaylistMetadataChanged(mediaMetadata: JMediaMetadata);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaylistMetadataChanged');
  {$ENDIF}
end;

{*************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onIsLoadingChanged(isLoading: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onIsLoadingChanged');
  {$ENDIF}
end;

{***********************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onLoadingChanged(isLoading: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onLoadingChanged');
  {$ENDIF}
end;

{**************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAvailableCommandsChanged(availableCommands: JPlayer_Commands);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onAvailableCommandsChanged');
  {$ENDIF}
end;

{***********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onTrackSelectionParametersChanged(parameters: JTrackSelectionParameters);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onTrackSelectionParametersChanged');
  {$ENDIF}
end;

{*******************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayerStateChanged(playWhenReady: boolean; playbackState: Integer);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerEngine = nil then exit;

  //int STATE_IDLE = 1;  => The player does not have any media to play.
  //int STATE_BUFFERING = 2; => The player is not able to immediately play from its current position. This state typically occurs when more data needs to be loaded.
  //int STATE_READY = 3; => The player is able to immediately play from its current position. The player will be playing if getPlayWhenReady() is true, and paused otherwise.
  //int STATE_ENDED = 4; => The player has finished playing the media.

  {$IF defined(DEBUG)}
  ALLog(
    'TALAndroidVideoPlayer.onPlayerStateChanged',
    'playWhenReady: ' + ALBoolToStrW(playWhenReady) + ' | ' +
    'playbackState: ' + ALIntToStrW(playbackState));
  {$ENDIF}

  if (playbackState = TJPlayer.JavaClass.STATE_READY) and
     (fVideoPlayerEngine.SetState(vpsPrepared, vpsPreparing)) and
     (assigned(fVideoPlayerEngine.fOnPreparedEvent)) then fVideoPlayerEngine.fOnPreparedEvent(fVideoPlayerEngine);

  if (playbackState = TJPlayer.JavaClass.STATE_ENDED) and
     (fVideoPlayerEngine.SetState(vpsPlaybackCompleted, vpsStarted)) and
      assigned(fVideoPlayerEngine.fOnCompletionEvent) then fVideoPlayerEngine.fOnCompletionEvent(fVideoPlayerEngine);

end;

{*********************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackStateChanged(playbackState: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackStateChanged');
  {$ENDIF}
end;

{**************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayWhenReadyChanged(playWhenReady: boolean; reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlayWhenReadyChanged');
  {$ENDIF}
end;

{*********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackSuppressionReasonChanged(playbackSuppressionReason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackSuppressionReasonChanged');
  {$ENDIF}
end;

{*************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onIsPlayingChanged(isPlaying: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onIsPlayingChanged');
  {$ENDIF}
end;

{***************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onRepeatModeChanged(repeatMode: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onRepeatModeChanged');
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onShuffleModeEnabledChanged(shuffleModeEnabled: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onShuffleModeEnabledChanged');
  {$ENDIF}
end;

{***************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlayerError(error: JPlaybackException);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerEngine = nil then exit;

  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.onPlayerError', TalLogType.error);
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
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity');
  {$ENDIF}
end;

{*************************************************************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity(oldPosition: JPlayer_PositionInfo; newPosition: JPlayer_PositionInfo; reason: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPositionDiscontinuity');
  {$ENDIF}
end;

{*******************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onPlaybackParametersChanged(playbackParameters: JPlaybackParameters);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onPlaybackParametersChanged');
  {$ENDIF}
end;

{*****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSeekBackIncrementChanged(seekBackIncrementMs: Int64);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSeekBackIncrementChanged');
  {$ENDIF}
end;

{***********************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSeekForwardIncrementChanged(seekForwardIncrementMs: Int64);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSeekForwardIncrementChanged');
  {$ENDIF}
end;

{*********************************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMaxSeekToPreviousPositionChanged(maxSeekToPreviousPositionMs: Int64);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onMaxSeekToPreviousPositionChanged');
  {$ENDIF}
end;

{***********************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAudioSessionIdChanged(audioSessionId: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onAudioSessionIdChanged');
  {$ENDIF}
end;

{**********************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onAudioAttributesChanged(audioAttributes: JAudioAttributes);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onAudioAttributesChanged');
  {$ENDIF}
end;

{******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onVolumeChanged(volume: Single);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onVolumeChanged');
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSkipSilenceEnabledChanged(skipSilenceEnabled: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSkipSilenceEnabledChanged');
  {$ENDIF}
end;

{*******************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onDeviceInfoChanged(deviceInfo: JDeviceInfo);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onDeviceInfoChanged');
  {$ENDIF}
end;

{*****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onDeviceVolumeChanged(volume: Integer; muted: boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onDeviceVolumeChanged');
  {$ENDIF}
end;

{****************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onVideoSizeChanged(videoSize: JVideoSize);
begin

  //https://stackoverflow.com/questions/48577801/java-arc-on-the-top-of-delphi-invoke-error-method-xxx-not-found
  if fVideoPlayerEngine = nil then exit;

  {$IF defined(DEBUG)}
  ALLog(
    'TALAndroidVideoPlayer.onVideoSizeChanged',
    'width: ' + ALIntToStrW(videoSize.width) + ' | ' +
    'height: ' + ALIntToStrW(videoSize.height));
  {$ENDIF}

  fVideoPlayerEngine.FVideoWidth := videoSize.width;
  fVideoPlayerEngine.fVideoHeight := videoSize.height;
  if assigned(fVideoPlayerEngine.fonVideoSizeChangedEvent) then
    fVideoPlayerEngine.fonVideoSizeChangedEvent(fVideoPlayerEngine, videoSize.width, videoSize.height);

end;

{****************************************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onSurfaceSizeChanged(width: Integer; height: Integer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onSurfaceSizeChanged');
  {$ENDIF}
end;

{*******************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onRenderedFirstFrame;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onRenderedFirstFrame');
  {$ENDIF}
end;

{******************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onCues(cues: JList);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onCues');
  {$ENDIF}
end;

{**************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onCues(cueGroup: JCueGroup);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onCues');
  {$ENDIF}
end;

{******************************************************************************}
procedure TALAndroidVideoPlayer.TPlayerListener.onMetadata(metadata: JMetadata);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidVideoPlayer.TPlayerListener.onMetadata');
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
  //-----
  fOnFrameAvailableEvent := nil;
  FOnFrameAvailableListener := TFrameAvailableListener.Create(Self);
  fHandler := TJHandler.JavaClass.init(TJLooper.javaclass.getMainLooper());
  fSurfaceTexture.setOnFrameAvailableListener(FOnFrameAvailableListener, fHandler);
  //-----
  fSurface := TJSurface.JavaClass.init(fSurfaceTexture);
  fExoPlayer.setVideoSurface(fSurface);

  //----
  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALAndroidVideoPlayer.Create', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
  fOnFrameAvailableListener.FVideoPlayerEngine := nil;
  FPlayerListener.FVideoPlayerEngine := nil;
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
  {$IF defined(ALSkiaCanvas)}
  if fGrBackEndTexture <> 0 then
    gr4d_backendtexture_destroy(fGrBackEndTexture);
  ALFreeAndNil(fTexture);
  {$ENDIF}
  alFreeandNilDrawable(fDrawable);

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
  ALLog('TALAndroidVideoPlayer.Destroy', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
  ALLog('TALAndroidVideoPlayer.prepare', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
  ALLog('TALAndroidVideoPlayer.pause', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
  ALLog('TALAndroidVideoPlayer.start', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
  ALLog('TALAndroidVideoPlayer.stop', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
    'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
  {$ENDIF}
end;

{*************************************************************}
procedure TALAndroidVideoPlayer.setVolume(const Value: Single);
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  var LVolume: Single := Value;
  if LVolume < 0 then LVolume := 0
  else if LVolume > 1 then LVolume := 1;
  fExoPlayer.setVolume(LVolume); // Sets the audio volume, with 0 being silence and 1 being unity gain.

  {$IFDEF DEBUG}
  ALLog(
    'TALAndroidVideoPlayer.setVolume',
    'Value: ' + ALFloatToStrW(Value, ALDefaultFormatSettingsW) + ' | ' +
    'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
  {$ENDIF}
end;

{********************************************************************}
procedure TALAndroidVideoPlayer.setPlaybackSpeed(const Value: single);
begin
  var LPlaybackParameters := tJPlaybackParameters.JavaClass.init(Value, 1);
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
begin
  result := False;
  for var I := Low(aCompareAnd) to High(aCompareAnd) do begin
    AtomicCmpExchange(fState, aValue, aCompareAnd[i], Result);
    if result then break;
  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{*********************************************************************************************}
constructor TALIOSVideoPlayer.TKVODelegate.Create(const AVideoPlayerEngine: TALIOSVideoPlayer);
begin
  inherited Create;
  fVideoPlayerEngine := AVideoPlayerEngine;
end;

{********************************************************************************************************************************************}
procedure TALIOSVideoPlayer.TKVODelegate.observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer);
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.observeValueForKeyPath', 'Status:' +  ALIntToStrW(TNSNumber.Wrap(change.allvalues.objectAtIndex(0)).integerValue()));
  {$ENDIF}
  fVideoPlayerEngine.doOnReady;
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
  allog('TALIOSVideoPlayer.ItemDidPlayToEndTime');
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
  allog('TALIOSVideoPlayer.ItemFailedToPlayToEndTime');
  {$ENDIF}
  fVideoPlayerEngine.DoOnItemFailedToPlayToEndTime;
end;

{****************************************************************}
//Posted when the item’s current time has changed discontinuously.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemTimeJumped;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemTimeJumped');
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
  allog('TALIOSVideoPlayer.ItemPlaybackStalled');
  {$ENDIF}
  //we do nothing here, the fVideoPlayerEngine.FFrameRefreshAnimation will do himself the job
  //to detect when the playback stalled and will do himself a pause / restart
end;

{**************************************************}
//Posted when a new access log entry has been added.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemNewAccessLogEntry;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemNewAccessLogEntry');
  {$ENDIF}
end;

{*************************************************}
//Posted when a new error log entry has been added.
//The notification’s object is the player item. The new log entry is
//accessible via errorLog(), respectively.
procedure TALIOSVideoPlayer.TNotificationsDelegate.ItemNewErrorLogEntry;
begin
  {$IF defined(DEBUG)}
  allog('TALIOSVideoPlayer.ItemNewErrorLogEntry');
  {$ENDIF}
end;

{***********************************}
constructor TALIOSVideoPlayer.Create;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

  //--
  inherited;

  //--
  fState := vpsIdle;
  fLooping := False;
  fVolume := -2;

  //--
  if not appAudioSessionActivated then begin
    appAudioSessionActivated := True;
    var LAudioSession := TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance);
    LAudioSession.setCategory(AVAudioSessionCategoryPlayback, nil);
    LAudioSession.setActive(True, nil);
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

  //--
  If GlobalUseMetal then begin
    fMetalTextureRef := 0;
    if CVMetalTextureCacheCreate(
         kCFAllocatorDefault, // allocator: The memory allocator for the texture.
         nil, // cacheAttributes: A dictionary specifying options for the cache’s behavior, or NULL to use default options. For applicable keys and values, see Cache Attributes.
         NSObjectToID(TCustomContextMetal.SharedDevice), // metalDevice: The Metal device used to create texture objects.
         nil, // textureAttributes: A dictionary specifying options for creating textures from the cache, or NULL to use default options.
         @fMetalVideoTextureCacheRef) <> kCVReturnSuccess then raise Exception.Create('CVMetalTextureCacheCreate failed!'); // cacheOut: Upon return, contains the newly created texture cache. When this value is NULL, an error occurred in texture creation.
  end
  else begin
    fOpenGLTextureRef := 0;
    if CVOpenGLESTextureCacheCreate(
         kCFAllocatorDefault, // allocator: The CFAllocatorRef to use for allocating the texture cache. This parameter can be NULL.
         nil, // cacheAttributes: A CFDictionaryRef containing the attributes of the texture cache itself. This parameter can be NULL.
         NSObjectToID(TCustomContextIOS.SharedContext), // eaglContext: The OpenGLES 2.0 context into which the texture objects will be created. OpenGLES 1.x contexts are not supported.
         nil, // textureAttributes: A CFDictionaryRef containing the attributes to be used for creating the CVOpenGLESTextureRef objects. This parameter can be NULL.
         @fOpenGlVideoTextureCacheRef) <> kCVReturnSuccess then raise Exception.Create('CVOpenGLESTextureCacheCreate failed!'); // cacheOut: A pointer to a CVOpenGLESTextureCacheRef where the newly created texture cache will be placed.
  end;

  //--
  FFrameRefreshAnimation := TALDisplayAnimation.Create;
  FFrameRefreshAnimation.OnProcess := DoOnFrameRefresh;
  FFrameRefreshAnimation.Enabled := False;

  //--
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

  //--
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fonVideoSizeChangedEvent := nil;

  //--
  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Create', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
  {$ENDIF}
end;

{***********************************}
destructor TALIOSVideoPlayer.Destroy;
begin
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}
  //--
  ALFreeAndNil(fPrepareThread);
  //--
  FFrameRefreshAnimation.Enabled := False;
  alFreeAndNil(FFrameRefreshAnimation);
  //--
  if fNotificationsDelegate <> nil then begin
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(FNotificationsDelegate.GetObjectID);
    AlFreeAndNil(FNotificationsDelegate);
  end;
  //--
  if FPlayer <> nil then begin
    FPlayer.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status')); // << i do it inside synchronize because as Observers are always called in mainthread better to remove it from here
    FPlayer.release;
    FPlayer := nil;
  end;
  //--
  if FPlayerItem <> nil then begin
    FPlayerItem.removeObserver(TNSObject.Wrap(FKVODelegate.GetObjectID), StrToNSStr('status')); // << i do it inside synchronize because as Observers are always called in mainthread better to remove it from here
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
  CVOpenGLESTextureCacheFlush(fOpenGLvideoTextureCacheRef, 0);
  CFrelease(pointer(fOpenGLVideoTextureCacheRef));
  //--
  if fMetalTextureRef <> 0 then CFRelease(pointer(fMetalTextureRef));
  CVMetalTextureCacheFlush(fMetalvideoTextureCacheRef, 0);
  CFrelease(pointer(fMetalVideoTextureCacheRef));
  //--
  if FPlayerItemVideoOutput <> nil then begin
    FPlayerItemVideoOutput.release;
    FPlayerItemVideoOutput := nil;
  end;
  //--
  inherited;
  //--
  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Destroy', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
                      begin

                        try

                          {$IFDEF DEBUG}
                          var LStopWatch := TstopWatch.StartNew;
                          {$ENDIF}

                          var P: Pointer;
                          if AlIsHttpOrHttpsUrl(aDataSource) then P := TNSUrl.OCClass.URLWithString(StrToNSStr(aDataSource)) // Creates and returns an NSURL object initialized with a provided URL string
                          else P := TNSUrl.OCClass.fileURLWithPath(StrToNSStr(aDataSource)); // Initializes and returns a newly created NSURL object as a file URL with a specified path.
                          if P = nil then exit; // << we can't call synchronize from here (else possible trouble when we will free the object) so we can't call onErrorEvent :(
                          var LURL := TNSUrl.Wrap(P);
                          // return A new player item, prepared to use URL.
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

                          //-----
                          FPlayer := TAVPlayer.Wrap(TAVPlayer.OCClass.playerWithPlayerItem(FPlayerItem)); // Returns a new player initialized to play the specified player item.
                          FPlayer.retain;

                          //-----
                          fNotificationsDelegate := TNotificationsDelegate.Create(self);
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemDidPlayToEndTime'), StringToID('AVPlayerItemDidPlayToEndTimeNotification'), NSObjectToID(FPlayerItem));
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemFailedToPlayToEndTime'), StringToID('AVPlayerItemFailedToPlayToEndTimeNotification'), NSObjectToID(FPlayerItem));
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemTimeJumped'), StringToID('AVPlayerItemTimeJumpedNotification'), NSObjectToID(FPlayerItem));
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemPlaybackStalled'), StringToID('AVPlayerItemPlaybackStalledNotification'), NSObjectToID(FPlayerItem));
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewAccessLogEntry'), StringToID('AVPlayerItemNewAccessLogEntryNotification'), NSObjectToID(FPlayerItem));
                          TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(fNotificationsDelegate.GetObjectID, sel_getUid('ItemNewErrorLogEntry'), StringToID('AVPlayerItemNewErrorLogEntryNotification'), NSObjectToID(FPlayerItem));

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
                            'FPlayerItem.status: ' + ALIntToStrW(FPlayerItem.status));
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
  // Pauses playback of the current item.
  // Calling this method is the same as setting the rate to 0.0.
  FPlayer.pause;
  FFrameRefreshAnimation.Enabled := False;

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.pause', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
  FFrameRefreshAnimation.Enabled := True;

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.start', 'TimeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
  FFrameRefreshAnimation.Enabled := False;

  {$IFDEF DEBUG}
  LStopWatch.Stop;
  ALLog('TALIOSVideoPlayer.Stop', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
    'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
  {$ENDIF}
end;

{****************************************************************}
procedure TALIOSVideoPlayer.setPlaybackSpeed(const Value: single);
begin
  // not yet implemented
end;

{************************************************************}
procedure TALIOSVideoPlayer.DoOnFrameRefresh(Sender: TObject);
begin

  // Stop the timer if we encoutered some error
  if GetState <> vpsStarted then begin
    FFrameRefreshAnimation.Enabled := False;
    Exit;
  end;

  // In case
  if FPlayerItemVideoOutput = nil then exit;

  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.StartNew;
  {$ENDIF}

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

        {$IFNDEF ALCompilerVersionSupported122}
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

    {$IFDEF DEBUG}
    inc(fFrameRefreshCounter);
    if fFrameRefreshCounter mod 1000 = 0 then begin
      LStopWatch.Stop;
      fFrameRefreshCounter := 0;
      ALLog('TALIOSVideoPlayer.FrameRefreshOnTimer', 'timeTaken: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
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
        'currentTime: ' + ALIntToStrW(fPlayer.currentTime.value));
      {$ENDIF}
      fPlayer.pause;
      fPlayer.play;
      fhasNONewPixelBufferForItemTimeCounter := 0;
    end;

  end;

end;

{************************************}
procedure TALIOSVideoPlayer.doOnReady;
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
    ALLog('TALIOSVideoPlayer.doOnReady', 'Ready');
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
      if fVolume > -1 then setVolume(fVolume);
      if assigned(fOnPreparedEvent) then fOnPreparedEvent(self);
    end;

  end
  else if (fPlayer.status = AVPlayerStatusFailed) or
          (FPlayerItem.status = AVPlayerItemStatusFailed) then begin

    {$IFDEF DEBUG}
    ALLog('TALIOSVideoPlayer.doOnReady', 'Failed');
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
    FFrameRefreshAnimation.Enabled := False;
    If SetState(vpsPlaybackCompleted, vpsStarted) and
       assigned(fOnCompletionEvent) then fOnCompletionEvent(self);
  end;
end;

{********************************************************}
procedure TALIOSVideoPlayer.doOnItemFailedToPlayToEndTime;
begin
  if SetState(vpsError) then begin
    FFrameRefreshAnimation.Enabled := False;
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
begin
  result := False;
  for var I := Low(aCompareAnd) to High(aCompareAnd) do begin
    AtomicCmpExchange(fState, aValue, aCompareAnd[i], Result);
    if result then break;
  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{***********************************}
constructor TALWinVideoPlayer.Create;
begin
  inherited;
  fDrawable := ALNullDrawable;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fonVideoSizeChangedEvent := nil;
end;

{***********************************}
destructor TALWinVideoPlayer.Destroy;
begin
  ALFreeAndNilDrawable(fDrawable);
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

{$REGION ' ALMacOS'}
{$IF defined(ALMacOS)}

{*************************************}
constructor TALMacOSVideoPlayer.Create;
begin
  inherited;
  fDrawable := ALNullDrawable;
  fOnFrameAvailableEvent := nil;
  fOnCompletionEvent := nil;
  fOnErrorEvent := nil;
  FOnPreparedEvent := nil;
  fonVideoSizeChangedEvent := nil;
end;

{*************************************}
destructor TALMacOSVideoPlayer.Destroy;
begin
  ALFreeAndNilDrawable(fDrawable);
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
  fVideoPlayerEngine := TALAndroidVideoPlayer.Create;
  {$ELSEIF defined(IOS)}
  fVideoPlayerEngine := TALIOSVideoPlayer.Create;
  {$ELSEIF defined(MSWINDOWS)}
  fVideoPlayerEngine := TALWinVideoPlayer.Create;
  {$ELSEIF defined(ALMacOS)}
  fVideoPlayerEngine := TALMacOSVideoPlayer.Create;
  {$ENDIF}
  //-----
  fVideoPlayerEngine.OnError := DoOnError;
  fVideoPlayerEngine.OnPrepared := doOnPrepared;
  fVideoPlayerEngine.OnFrameAvailable := doOnFrameAvailable;
  fVideoPlayerEngine.OnCompletion := doOnCompletion;
  fVideoPlayerEngine.onVideoSizeChanged := doonVideoSizeChanged;
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
  AlFreeAndNil(fVideoPlayerEngine);
  inherited;
end;

{************************************************}
function TALVideoPlayer.getCurrentPosition: int64;
begin
  result := fVideoPlayerEngine.getCurrentPosition;
end;

{*****************************************}
function TALVideoPlayer.getDuration: int64;
begin
  result := fVideoPlayerEngine.getDuration;
end;

{***********************************************}
function TALVideoPlayer.GetDrawable: TALDrawable;
begin
  result := fVideoPlayerEngine.Drawable;
end;

{**********************************************}
function TALVideoPlayer.getVideoHeight: integer;
begin
  result := fVideoPlayerEngine.getVideoHeight;
end;

{*********************************************}
function TALVideoPlayer.getVideoWidth: integer;
begin
  result := fVideoPlayerEngine.getVideoWidth;
end;

{****************************************}
function TALVideoPlayer.getState: integer;
begin
  result := fVideoPlayerEngine.getState;
end;

{*****************************************}
function TALVideoPlayer.isPlaying: boolean;
begin
  result := fVideoPlayerEngine.isplaying;
end;

{*****************************}
procedure TALVideoPlayer.pause;
begin
  fVideoPlayerEngine.pause;
end;

{*******************************************************************************************************}
procedure TALVideoPlayer.prepare(Const aDataSource: String; const aAutoStartWhenPrepared: Boolean=False);
begin
  FAutoStartWhenPrepared := aAutoStartWhenPrepared;
  fVideoPlayerEngine.prepare(aDataSource);
end;

{*************************************************}
procedure TALVideoPlayer.seekTo(const msec: Int64);
begin
  fVideoPlayerEngine.seekTo(msec);
end;

{**********************************************************}
procedure TALVideoPlayer.setLooping(const looping: Boolean);
begin
  fVideoPlayerEngine.setLooping(looping);
end;

{******************************************************}
procedure TALVideoPlayer.setVolume(const Value: Single);
begin
  fVideoPlayerEngine.setVolume(Value);
end;

{*************************************************************}
procedure TALVideoPlayer.setPlaybackSpeed(const Value: single);
begin
  fVideoPlayerEngine.setPlaybackSpeed(Value);
end;

{*****************************}
procedure TALVideoPlayer.Start;
begin
  fVideoPlayerEngine.Start;
end;

{****************************}
procedure TALVideoPlayer.Stop;
begin
  fVideoPlayerEngine.Stop;
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

{****************************************************************}
function TALVideoPlayerSurface.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphacolors.Black;
end;

{******************************************************************}
function TALVideoPlayerSurface.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TalphaColors.null;
end;

{***********************************************************}
constructor TALVideoPlayerSurface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fVideoPlayer := TALVideoPlayer.create;
  fVideoPlayer.OnFrameAvailable := OnFrameAvailable;
end;

{***************************************}
destructor TALVideoPlayerSurface.Destroy;
begin
  ALFreeAndNil(fVideoPlayer);
  inherited;
end;

{**************************************************}
function TALVideoPlayerSurface.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{**********************************************************}
function TALVideoPlayerSurface.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
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
  if IsVisibleWithinFormBounds then
    repaint;
end;

{************************************}
procedure TALVideoPlayerSurface.Paint;
begin

  inherited paint;

  if ALIsDrawableNull(fVideoPlayer.Drawable) then
    exit;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fVideoPlayer.Drawable, // const ADrawable: TALDrawable;
    TRectF.Create(0, 0, ALGetDrawableWidth(fVideoPlayer.Drawable), ALGetDrawableHeight(fVideoPlayer.Drawable)).
      FitInto(LocalRect), // const ADstRect: TrectF; // IN Virtual pixels !
    AbsoluteOpacity); // const AOpacity: Single);

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

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TALIosVideoPlayer.appAudioSessionActivated := False;
  {$ENDIF}
  {$ENDREGION}

end.
