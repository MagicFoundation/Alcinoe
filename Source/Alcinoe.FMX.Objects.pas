unit Alcinoe.FMX.Objects;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported130}
  {$MESSAGE WARN 'Check if FMX.Objects.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Net.URLClient,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Rtti,
  System.Net.HttpClient,
  {$IFDEF DEBUG}
  System.Diagnostics,
  {$ENDIF}
  {$IF defined(ANDROID)}
  system.Messaging,
  FMX.TextLayout.GPU,
  FMX.types3D,
  {$ENDIF}
  {$IF defined(IOS)}
  system.Messaging,
  FMX.TextLayout.GPU,
  FMX.types3D,
  {$ENDIF}
  {$IF defined(ALSkiaAvailable)}
  System.Skia.API,
  {$ENDIF}
  FMX.controls,
  FMX.types,
  FMX.graphics,
  FMX.objects,
  Alcinoe.Common,
  Alcinoe.FMX.CacheEngines,
  Alcinoe.FMX.Types3D,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.BreakText,
  Alcinoe.fmx.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALShape = class(TALControl)
  strict private
    FFill: TALBrush;
    FStroke: TALStrokeBrush;
    FShadow: TALShadow;
    function GetFill: TALBrush;
    procedure SetFill(const Value: TALBrush);
    function GetStroke: TALStrokeBrush;
    procedure SetStroke(const Value: TALStrokeBrush);
    function GetShadow: TALShadow;
    procedure SetShadow(const Value: TALShadow);
  protected
    function CreateFill: TALBrush; virtual;
    function CreateStroke: TALStrokeBrush; virtual;
    function CreateShadow: TALShadow; virtual;
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure ShadowChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    property Fill: TALBrush read GetFill write SetFill;
    property Stroke: TALStrokeBrush read GetStroke write SetStroke;
    property Shadow: TALShadow read GetShadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // In Delphi, multi-resolution bitmaps for TImage or TGlyph allow us to
  // provide different bitmap sizes for various screen scales. For example, a
  // screen scale of 1 might use a 100x100 bitmap, while a scale of 1.5 uses a
  // 150x150 bitmap, etc. Typically, most screen scales are 1, 1.5, 2, 3, and 4,
  // requiring up to 4 different images. In most cases (99.9999%), developers
  // will simply resize these images using a tool like Photoshop to match one
  // of these five scales. Rarely does anyone create radically different images
  // for each scale.
  //
  // However, the resizing algorithms available are quite effective and the
  // differences are often negligible. Thus, providing just one high-resolution
  // bitmap (at the largest scale of 4) could be sufficient and would help
  // reduce the application's size.
  [ComponentPlatforms($FFFF)]
  TALImage = class(TALControl)
  public
    type
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      TCropCenter = class(TALPosition)
      protected
        function GetDefaultValue: TPointF; override;
      end;
  protected
    type
      TResourceDownloadContext = Class(TALWorkerContext)
      private
        function GetOwner: TALImage;
      public
        Rect: TRectF;
        Scale: Single;
        AlignToPixel: Boolean;
        Color: TAlphaColor;
        TintColor: TAlphaColor;
        ResourceName: String;
        ResourceStream: TStream;
        MaskResourceName: String;
        WrapMode: TALImageWrapMode;
        CropCenter: TpointF;
        ApplyMetadataOrientation: Boolean;
        StrokeColor: TAlphaColor;
        StrokeThickness: Single;
        ShadowBlur: Single;
        ShadowOffsetX: Single;
        ShadowOffsetY: Single;
        ShadowColor: TAlphaColor;
        Corners: TCorners;
        Sides: TSides;
        XRadius: Single;
        YRadius: Single;
        BlurRadius: single;
        constructor Create(const AOwner: TALImage); reintroduce; virtual;
        destructor Destroy; override;
        Property Owner: TALImage read GetOwner;
      End;
  private
    FOwnsResourceStream: Boolean; // 1 byte
    FBackgroundColor: TAlphaColor; // 4 bytes
    FLoadingColor: TAlphaColor; // 4 bytes
    FTintColor: TAlphaColor; // 4 bytes
    FBackgroundColorKey: String; // 8 bytes
    FLoadingColorKey: String; // 8 bytes
    FTintColorKey: String; // 8 bytes
    FResourceName: String; // 8 bytes
    FResourceStream: TStream; // 8 bytes
    FMaskResourceName: String; // 8 bytes
    FHTTPHeaders: TNetHeaders; // 8 bytes
    FWrapMode: TALImageWrapMode; // 1 bytes
    FApplyMetadataOrientation: Boolean; // 1 bytes
    FCorners: TCorners; // 1 bytes
    FSides: TSides; // 1 bytes
    FXRadius: Single; // 4 bytes
    FYRadius: Single; // 4 bytes
    FBlurRadius: single; // 4 bytes
    FCacheIndex: Integer; // 4 bytes
    FLoadingCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    FCropCenter: TALPosition; // 8 bytes
    FStroke: TALStrokeBrush; // 8 bytes
    FShadow: TALShadow; // 8 bytes
    FResourceDownloadContext: TResourceDownloadContext; // [MultiThread] | 8 bytes
    FFadeInDuration: Single; // 4 bytes
    FFadeInStartTimeNano: Int64; // 8 bytes
    function GetCropCenter: TALPosition;
    procedure SetCropCenter(const Value: TALPosition);
    function GetStroke: TALStrokeBrush;
    procedure SetStroke(const Value: TALStrokeBrush);
    function GetShadow: TALShadow;
    procedure SetShadow(const Value: TALShadow);
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure SetApplyMetadataOrientation(const Value: Boolean);
    procedure setResourceName(const Value: String);
    procedure setResourceStream(const Value: TStream);
    procedure setMaskResourceName(const Value: String);
    procedure setBackgroundColor(const Value: TAlphaColor);
    procedure setBackgroundColorKey(const Value: String);
    procedure setLoadingColor(const Value: TAlphaColor);
    procedure setLoadingColorKey(const Value: String);
    procedure SetTintColor(const Value: TAlphaColor);
    procedure setTintColorKey(const Value: String);
    function IsBackgroundColorStored: Boolean;
    function IsBackgroundColorKeyStored: Boolean;
    function IsLoadingColorStored: Boolean;
    function IsLoadingColorKeyStored: Boolean;
    function IsTintColorStored: Boolean;
    function IsTintColorKeyStored: Boolean;
    function IsFadeInDurationStored: Boolean;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
    function IsBlurRadiusStored: Boolean;
  protected
    fBufDrawable: TALDrawable; // 8 bytes
    fBufDrawableRect: TRectF; // 16 bytes
    fBufLoadingDrawable: TALDrawable; // 8 bytes
    fBufLoadingDrawableRect: TRectF; // 16 bytes
    function CreateCropCenter: TALPosition; virtual;
    function CreateStroke: TALStrokeBrush; virtual;
    function CreateShadow: TALShadow; virtual;
    procedure ApplyBackgroundColorScheme; virtual;
    procedure ApplyLoadingColorScheme; virtual;
    procedure ApplyTintColorScheme; virtual;
    function GetCacheSubIndex: Integer; virtual;
    function GetLoadingCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    function GetDefaultBackgroundColor: TalphaColor; virtual;
    function GetDefaultBackgroundColorKey: String; virtual;
    function GetDefaultLoadingColor: TalphaColor; virtual;
    function GetDefaultLoadingColorKey: String; virtual;
    function GetDefaultTintColor: TAlphaColor; virtual;
    function GetDefaultTintColorKey: String; virtual;
    function GetDefaultFadeInDuration: Single; virtual;
    function GetDefaultCorners: TCorners; virtual;
    function GetDefaultSides: TSides; virtual;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
    function GetDefaultBlurRadius: Single; virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetBlurRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure CropCenterChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure CancelResourceDownload;
    class function CanStartResourceDownload(var AContext: Tobject): boolean; virtual; // [MultiThread]
    class procedure HandleResourceDownloadSuccess(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject); virtual; // [MultiThread]
    class procedure HandleResourceDownloadError(const AResponse: IHTTPResponse; const AErrMessage: string; var AContext: Tobject); virtual; // [MultiThread]
    class function GetResourceDownloadPriority(const AContext: Tobject): Int64; virtual; // [MultiThread]
    class Procedure CreateBufDrawable(var AContext: TObject); overload; virtual; // [MultiThread]
    class Procedure CreateBufDrawable(
                      var ABufDrawable: TALDrawable;
                      out ABufDrawableRect: TRectF;
                      const ARect: TRectF;
                      const AScale: Single;
                      const AAlignToPixel: Boolean;
                      const AColor: TAlphaColor;
                      const ATintColor: TAlphaColor;
                      const AResourceName: String;
                      const AResourceStream: TStream;
                      const AMaskResourceName: String;
                      const AWrapMode: TALImageWrapMode;
                      const ACropCenter: TpointF;
                      const AApplyMetadataOrientation: Boolean;
                      const AStrokeColor: TAlphaColor;
                      const AStrokeThickness: Single;
                      const AShadowBlur: Single;
                      const AShadowOffsetX: Single;
                      const AShadowOffsetY: Single;
                      const AShadowColor: TAlphaColor;
                      const ACorners: TCorners;
                      const ASides: TSides;
                      const AXRadius: Single;
                      const AYRadius: Single;
                      const ABlurRadius: Single); overload; virtual; // [MultiThread]
    procedure Paint; override;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    function IsReadyToDisplay(const AStrict: Boolean = False): Boolean; override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DefaultBackgroundColor: TAlphaColor read GetDefaultBackgroundColor;
    property DefaultBackgroundColorKey: String read GetDefaultBackgroundColorKey;
    property DefaultLoadingColor: TAlphaColor read GetDefaultLoadingColor;
    property DefaultLoadingColorKey: String read GetDefaultLoadingColorKey;
    property DefaultTintColor: TAlphaColor read GetDefaultTintColor;
    property DefaultTintColorKey: String read GetDefaultTintColorKey;
    property DefaultFadeInDuration: Single read GetDefaultFadeInDuration;
    property DefaultCorners: TCorners read GetDefaultCorners;
    property DefaultSides: TSides read GetDefaultSides;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
    property DefaultBlurRadius: Single read GetDefaultBlurRadius;
    /// <summary>
    ///   When you assign a stream to ResourceStream, TALImage takes ownership and will free it.
    /// </summary>
    property ResourceStream: TStream read FResourceStream write setResourceStream;
    property OwnsResourceStream: Boolean read FOwnsResourceStream write FOwnsResourceStream;
    /// <summary>
    ///   Extra HTTP request headers to apply when downloading the image.
    /// </summary>
    property HTTPHeaders: TNetHeaders read FHTTPHeaders write FHTTPHeaders;
    /// <summary>
    ///   CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    ///   prevent duplicate drawables across multiple identical controls.
    ///   CacheIndex specifies the slot in the cache engine where an existing
    ///   drawable can be retrieved.
    /// </summary>
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    property LoadingCacheIndex: Integer read FLoadingCacheIndex write FLoadingCacheIndex;
    /// <summary>
    ///   CacheEngine is not owned by the current control.
    /// </summary>
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    property BackgroundColor: TAlphaColor read fBackgroundColor write setBackgroundColor Stored IsBackgroundColorStored;
    property BackgroundColorKey: String read fBackgroundColorKey write setBackgroundColorKey Stored IsBackgroundColorKeyStored;
    property LoadingColor: TAlphaColor read FLoadingColor write setLoadingColor Stored IsLoadingColorStored;
    property LoadingColorKey: String read FLoadingColorKey write setLoadingColorKey Stored IsLoadingColorKeyStored;
    property TintColor: TAlphaColor read FTintColor write SetTintColor stored IsTintColorStored;
    property TintColorKey: String read FTintColorKey write setTintColorKey Stored IsTintColorKeyStored;
    property BlurRadius: Single read FBlurRadius write SetBlurRadius stored IsBlurRadiusStored nodefault;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    /// <summary>
    ///   CropCenter is use when wrapmode = FitIntoAndCrop. It's define the center of
    ///   crop in the source image (ex: center the result on a face instead
    ///   of the middle of the bounds).
    /// </summary>
    property CropCenter: TALPosition read GetCropCenter write SetCropCenter;
    property Cursor;
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
    property Margins;
    property MaskResourceName: String read fMaskResourceName write setMaskResourceName;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    /// <summary>
    ///   If a file extension (e.g., .png) is detected in ResourceName, the image is loaded from the
    ///   specified file (With the full path of the file obtained using ALGetResourceFilename).
    ///   If ResourceName is a URL, the image is downloaded in the background from the internet.
    ///   In debug mode, the image is loaded from a file located in the /Resources/ sub-folder of the
    ///   project directory (with the extensions .png or .jpg).
    /// </summary>
    property ResourceName: String read FResourceName write setResourceName;
    /// <summary>
    ///   When <c>True</c>, applies the image’s EXIF orientation (rotate/flip)
    ///   so the bitmap is rendered upright. When <c>False</c> (default),
    ///   the raw pixels are used as-is without any EXIF-based transform.
    /// </summary>
    /// <remarks>
    ///   On a Skia canvas this behavior is always enforced as <c>True</c>
    ///   (the Delphi IDE uses a Skia canvas), so the property is effectively
    ///   ignored there and images are displayed with EXIF orientation applied.
    /// </remarks>
    property ApplyMetadataOrientation: Boolean read FApplyMetadataOrientation write SetApplyMetadataOrientation default false;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow: TALShadow read GetShadow write SetShadow;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property Size;
    property Stroke: TALStrokeBrush read GetStroke write SetStroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode default TALImageWrapMode.Fit;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
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
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALAnimatedImage = class(TALControl)
  Public
    type
      TAnimation = class(TALInterpolatedAnimation)
      private
        fOwner: TALAnimatedImage;
        FDuration: Single;
        FSpeed: Single;
        FStartProgress: Single;
        FStopProgress: Single;
        fCurrentProgress: Single;
        FEnabled: Boolean;
        function GetDuration: Single;
        {$IF defined(ALSkiaAvailable)}
        procedure SetDuration(const Value: Single);
        {$ENDIF}
        function GetCurrentTime: Single;
        function GetSpeed: Single;
        procedure SetSpeed(const Value: Single);
        procedure SetEnabled(const Value: Boolean);
        procedure SetStartProgress(const Value: Single);
        procedure SetStopProgress(const Value: Single);
        function IsStopProgressStored: Boolean;
        function IsSpeedStored: Boolean;
        procedure UpdateInheritedAnimationDuration;
        procedure repaint;
      protected
        procedure ProcessAnimation; override;
        procedure DoFirstFrame; override;
        procedure DoProcess; override;
        procedure DoFinish; override;
        function GetDefaultLoop: Boolean; override;
      public
        constructor Create(const AOwner: TALAnimatedImage); reintroduce; virtual;
        procedure Assign(Source: TPersistent); override;
        procedure Start; override;
        property CurrentProgress: Single read FCurrentProgress;
        property CurrentTime: Single read GetCurrentTime;
      published
        property AutoReverse;
        property Delay;
        property Duration: Single read getDuration stored false;
        property Enabled Read FEnabled write SetEnabled stored true default True;
        property Inverse;
        property Loop;
        property Speed: Single read GetSpeed write setSpeed stored IsSpeedStored nodefault;
        property StartProgress: Single read FStartProgress write SetStartProgress;
        property StopProgress: Single read FStopProgress write SetStopProgress stored IsStopProgressStored nodefault;
      end;
  private
    fAnimation: TAnimation;
    {$IF defined(ALSkiaAvailable)}
      fSkottieAnimation: sk_skottieanimation_t;
      fAnimcodecplayer: sk_animcodecplayer_t;
      FRenderRect: TRectF;
      {$IF not defined(ALSkiaCanvas)}
        FBufSurface: sk_surface_t;
        FBufCanvas: sk_canvas_t;
        {$IF defined(ALGPUCanvas)}
        FbufTexture: TALTexture;
        {$ELSE}
        FBufBitmap: Tbitmap;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    FResourceName: String; // 8 bytes
    FTintColor: TAlphaColor; // 4 bytes
    FTintColorKey: String; // 8 bytes
    FWrapMode: TALImageWrapMode; // 1 byte
    FOnAnimationFirstFrame: TNotifyEvent; // 16 bytes
    FOnAnimationProcess: TNotifyEvent; // 16 bytes
    FOnAnimationFinish: TNotifyEvent; // 16 bytes
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure setResourceName(const Value: String);
    procedure SetAnimation(const Value: TAnimation);
    procedure SetTintColor(const Value: TAlphaColor);
    procedure setTintColorKey(const Value: String);
    function IsTintColorStored: Boolean;
    function IsTintColorKeyStored: Boolean;
  protected
    procedure ApplyTintColorScheme; virtual;
    function GetDefaultTintColor: TAlphaColor; virtual;
    function GetDefaultTintColorKey: String; virtual;
    procedure Paint; override;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure ApplyColorScheme; override;
    procedure CreateCodec; virtual;
    procedure ReleaseCodec; virtual;
    property DefaultTintColor: TAlphaColor read GetDefaultTintColor;
    property DefaultTintColorKey: String read GetDefaultTintColorKey;
  published
    //property Action;
    property Align;
    property Anchors;
    property Animation: TAnimation read fAnimation write SetAnimation;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
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
    // If a file extension (e.g., .xxx) is detected in ResourceName, the image is loaded from the
    // specified file (With the full path of the file obtained using ALGetResourceFilename).
    // In debug mode, the image is loaded from a file located in the /Resources/ sub-folder of the
    // project directory (with the extensions .png or .jpg).
    property ResourceName: String read FResourceName write setResourceName;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    property TintColor: TAlphaColor read FTintColor write SetTintColor stored IsTintColorStored;
    property TintColorKey: String read FTintColorKey write setTintColorKey Stored IsTintColorKeyStored;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode default TALImageWrapMode.Fit;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnAnimationFirstFrame: TNotifyEvent read FOnAnimationFirstFrame write FOnAnimationFirstFrame;
    property OnAnimationProcess: TNotifyEvent read FOnAnimationProcess write FOnAnimationProcess;
    property OnAnimationFinish: TNotifyEvent read FOnAnimationFinish write FOnAnimationFinish;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseRectangle = class(TALShape)
  private
    fDoubleBuffered: boolean;
    FXRadius: Single;
    FYRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function HasCustomDraw: Boolean; virtual;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    function GetDefaultCorners: TCorners; virtual;
    function GetDefaultSides: TSides; virtual;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    function IsSimpleRenderPossible: Boolean;
    procedure Paint; override;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                const AScale: Single;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStateLayerContentColor: TAlphaColor;
                const ADrawStateLayerOnTop: Boolean;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow); virtual;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    procedure DoResized; override;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DoubleBuffered default true;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property DefaultCorners: TCorners read GetDefaultCorners;
    property DefaultSides: TSides read GetDefaultSides;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALRectangle = class(TALBaseRectangle)
  public
    property CacheEngine;
    property CacheIndex;
  published
    //property Action;
    property Align;
    property Anchors;
    property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor;
    property DoubleBuffered;
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
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALEllipse = class(TALShape)
  private
    fDoubleBuffered: boolean;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure Paint; override;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                const AScale: Single;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStateLayerContentColor: TAlphaColor;
                const ADrawStateLayerOnTop: Boolean;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow); virtual;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    function PointInObjectLocal(X, Y: Single): Boolean; override;
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
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered default true;
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
    property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
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
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCircle = class(TALShape)
  private
    fDoubleBuffered: boolean;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure Paint; override;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                const AScale: Single;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStateLayerContentColor: TAlphaColor;
                const ADrawStateLayerOnTop: Boolean;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow); virtual;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    function PointInObjectLocal(X, Y: Single): Boolean; override;
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
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered default true;
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
    property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
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
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALLineType = (TopLeftToBottomRight, BottomLeftToTopRight, Top, Left, Bottom, Right);

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALLine = class(TALShape)
  private
    fDoubleBuffered: boolean;
    FLineType: TALLineType;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    procedure SetLineType(const Value: TALLineType);
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
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
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered default true;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property LineType: TALLineType read FLineType write SetLineType;
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
    property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
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
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseText = class(TALShape)
  public
    const DefaultMaxWidth = 65535;
    const DefaultMaxHeight = 65535;
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
  public
    type
      TElementMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; const Element: TALTextElement) of object;
      TElementMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Single; const Element: TALTextElement) of object;
      TElementNotifyEvent = procedure(Sender: TObject; const Element: TALTextElement) of object;
  private
    fDoubleBuffered: boolean;
    FMultiLineTextOptions: TALMultiLineTextOptions;
    FMaxContainedSize: TSizeF; // 4 bytes
    FOnElementClick: TElementNotifyEvent;
    FOnElementMouseDown: TElementMouseEvent;
    FOnElementMouseMove: TElementMouseMoveEvent;
    FOnElementMouseUp: TElementMouseEvent;
    FOnElementMouseEnter: TElementNotifyEvent;
    FOnElementMouseLeave: TElementNotifyEvent;
    FHoveredElement: TALTextElement;
    FPressedElement: TALTextElement;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
    fTextBroken: Boolean;
    fAllTextDrawn: Boolean;
    fElements: TALTextElements;
    FAutoTranslate: Boolean;
    FText: String;
    FTextSettings: TALBaseTextSettings;
    FMaxWidth: Single;
    FMaxHeight: Single;
    FYRadius: Single;
    FXRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    procedure SetText(const Value: string);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure SetAlign(const Value: TALAlignLayout); override;
    procedure SetAutoSize(const Value: TALAutoSizeMode); override;
    function GetEffectiveMaxSize: TSizeF; Virtual;
    function GetElementAtPos(const APos: TPointF): TALTextElement;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoClickSound; override;
    procedure Click; override;
    procedure PaddingChanged; override;
    procedure TextSettingsChanged(Sender: TObject); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    function GetDefaultCorners: TCorners; virtual;
    function GetDefaultSides: TSides; virtual;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure SetTextSettings(const Value: TALBaseTextSettings); virtual;
    function CreateTextSettings: TALBaseTextSettings; virtual; abstract;
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure DoResized; override;
    procedure ParentRealigning; override;
    procedure AdjustSize; override;
    function GetMultiLineTextOptions(
               const AScale: Single;
               const AOpacity: Single;
               const AFont: TALFont;
               const ADecoration: TALTextDecoration;
               const AEllipsisFont: TALFont;
               const AEllipsisDecoration: TALTextDecoration;
               const AFill: TALBrush;
               const AStateLayer: TALStateLayer;
               const AStroke: TALStrokeBrush;
               const AShadow: TALShadow;
               const AXRadius: Single;
               const AYRadius: Single): TALMultiLineTextOptions;
    Procedure DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF); virtual;
    Procedure DrawMultilineTextBeforeDrawBackground(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF); virtual;
    Procedure DrawMultilineTextBeforeDrawParagraph(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF); virtual;
    Procedure DrawMultilineText(
                const ACanvas: TALCanvas;
                var ARect: TRectF;
                out ATextBroken: Boolean;
                out AAllTextDrawn: Boolean;
                out AElements: TALTextElements;
                const AScale: Single;
                const AOpacity: Single;
                const AText: String;
                const AFont: TALFont;
                const ADecoration: TALTextDecoration;
                const AEllipsisFont: TALFont;
                const AEllipsisDecoration: TALTextDecoration;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow;
                const AXRadius: Single;
                const AYRadius: Single);
    Procedure MeasureMultilineText(
                out ARect: TRectF;
                out ATextBroken: Boolean;
                out AAllTextDrawn: Boolean;
                out AElements: TALTextElements;
                const AScale: Single;
                const AText: String;
                const AFont: TALFont;
                const ADecoration: TALTextDecoration;
                const AEllipsisFont: TALFont;
                const AEllipsisDecoration: TALTextDecoration;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow;
                const AXRadius: Single;
                const AYRadius: Single);
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                out ATextBroken: Boolean;
                out AAllTextDrawn: Boolean;
                out AElements: TALTextElements;
                const AScale: Single;
                const AText: String;
                const AFont: TALFont;
                const ADecoration: TALTextDecoration;
                const AEllipsisFont: TALFont;
                const AEllipsisDecoration: TALTextDecoration;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow;
                const AXRadius: Single;
                const AYRadius: Single);
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    property Elements: TALTextElements read fElements;
    property OnElementClick: TElementNotifyEvent read FOnElementClick write FOnElementClick;
    property OnElementMouseDown: TElementMouseEvent read FOnElementMouseDown write FOnElementMouseDown;
    property OnElementMouseMove: TElementMouseMoveEvent read FOnElementMouseMove write FOnElementMouseMove;
    property OnElementMouseUp: TElementMouseEvent read FOnElementMouseUp write FOnElementMouseUp;
    property OnElementMouseEnter: TElementNotifyEvent read FOnElementMouseEnter write FOnElementMouseEnter;
    property OnElementMouseLeave: TElementNotifyEvent read FOnElementMouseLeave write FOnElementMouseLeave;
    property TextSettings: TALBaseTextSettings read fTextSettings write SetTextSettings;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    function TextBroken: Boolean;
    property AutoSize default TALAutoSizeMode.Both;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property HitTest default False;
    /// <summary>
    ///   If MaxWidth = 0, there is no explicit maximum; the control grows to the
    ///   largest width that still fully fits inside its parent container.
    /// </summary>
    property MaxWidth: Single read fMaxWidth write SetMaxWidth stored IsMaxWidthStored nodefault;
    /// <summary>
    ///   If MaxHeight = 0, there is no explicit maximum; the control grows to the
    ///   largest height that still fully fits inside its parent container.
    /// </summary>
    property MaxHeight: Single read fMaxHeight write SetMaxHeight stored IsMaxHeightStored nodefault;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property Text: string read FText write SetText;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property DoubleBuffered default true;
    property DefaultCorners: TCorners read GetDefaultCorners;
    property DefaultSides: TSides read GetDefaultSides;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALText = class(TALBaseText)
  private
    function GetTextSettings: TALTextSettings;
  protected
    procedure SetTextSettings(const Value: TALTextSettings); reintroduce;
    function CreateTextSettings: TALBaseTextSettings; override;
  {$IF defined(ALBackwardCompatible)}
  private
    procedure ReadTextIsHtml(Reader: TReader);
    procedure ReadLineSpacing(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  {$ENDIF}
  public
    property CacheEngine;
    property CacheIndex;
    property Elements;
  published
    //property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property AutoTranslate;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor;
    property DoubleBuffered;
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
    property MaxWidth;
    property MaxHeight;
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
    property Text;
    property TextSettings: TALTextSettings read GetTextSettings write SetTextSettings;
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
    property OnElementClick;
    property OnElementMouseDown;
    property OnElementMouseMove;
    property OnElementMouseUp;
    property OnElementMouseEnter;
    property OnElementMouseLeave;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

procedure Register;

implementation

uses
  system.SysUtils,
  system.Math,
  system.Math.Vectors,
  FMX.utils,
  FMX.platform,
  FMX.Forms,
  {$IF defined(ALSkiaAvailable)}
  FMX.Skia,
  FMX.Skia.Canvas,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.Bitmap,
  FMX.Canvas.GPU,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.UIKit,
  FMX.Canvas.GPU,
  FMX.Surfaces,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  system.ioutils,
  {$ENDIF}
  Alcinoe.Url,
  Alcinoe.FMX.Styles,
  Alcinoe.HTTP.Client.Net.Pool,
  Alcinoe.Localization,
  Alcinoe.StringUtils;

{**********************************************}
constructor TALShape.Create(AOwner: TComponent);
begin
  inherited;
  FFill := CreateFill;
  FFill.OnChanged := FillChanged;
  FStroke := CreateStroke;
  FStroke.OnChanged := StrokeChanged;
  FShadow := CreateShadow;
  FShadow.OnChanged := ShadowChanged;
end;

{**************************}
destructor TALShape.Destroy;
begin
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FShadow);
  inherited;
end;

{*********************************************************}
procedure TALShape.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALShape then begin
      Fill.Assign(TALShape(Source).Fill);
      Stroke.Assign(TALShape(Source).Stroke);
      Shadow.Assign(TALShape(Source).Shadow);
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*************************************}
function TALShape.CreateFill: TALBrush;
begin
  Result := TALBrush.Create;
end;

{*********************************************}
function TALShape.CreateStroke: TALStrokeBrush;
begin
  Result := TALStrokeBrush.Create;
end;

{****************************************}
function TALShape.CreateShadow: TALShadow;
begin
  Result := TALShadow.Create;
end;

{******************************}
procedure TALShape.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    Fill.AlignToPixel;
    Stroke.AlignToPixel;
    Shadow.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{**********************************}
procedure TALShape.ApplyColorScheme;
begin
  beginUpdate;
  try
    inherited;
    Fill.ApplyColorScheme;
    Stroke.ApplyColorScheme;
    Shadow.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{**********************************************}
procedure TALShape.FillChanged(Sender: TObject);
begin
  Repaint;
end;

{************************************************}
procedure TALShape.StrokeChanged(Sender: TObject);
begin
  Repaint;
end;

{************************************************}
procedure TALShape.ShadowChanged(Sender: TObject);
begin
  Repaint;
end;

{**********************************}
function TALShape.GetFill: TALBrush;
begin
  Result := FFill;
end;

{************************************************}
procedure TALShape.SetFill(const Value: TALBrush);
begin
  FFill.Assign(Value);
end;

{******************************************}
function TALShape.GetStroke: TALStrokeBrush;
begin
  Result := FStroke;
end;

{********************************************************}
procedure TALShape.SetStroke(const Value: TALStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{*************************************}
function TALShape.GetShadow: TALShadow;
begin
  Result := FShadow;
end;

{***************************************************}
procedure TALShape.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
end;

{*****************************************************}
function TALImage.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*****************************************************}
function TALImage.TCropCenter.GetDefaultValue: TPointF;
begin
  Result := TpointF.Create(0.5,0.5);
end;

{***************************************************************************}
constructor TALImage.TResourceDownloadContext.Create(const AOwner: TALImage);
begin
  inherited Create(AOwner);
  Rect := AOwner.LocalRect;
  Scale := ALGetScreenScale;
  AlignToPixel := AOwner.AutoAlignToPixel;
  Color := AOwner.BackgroundColor;
  TintColor := AOwner.TintColor;
  ResourceName := AOwner.ResourceName;
  ResourceStream := nil;
  MaskResourceName := AOwner.MaskResourceName;
  WrapMode := AOwner.WrapMode;
  CropCenter := AOwner.CropCenter.Point;
  ApplyMetadataOrientation := AOwner.ApplyMetadataOrientation;
  StrokeColor := AOwner.Stroke.Color;
  StrokeThickness := AOwner.Stroke.Thickness;
  ShadowBlur := AOwner.Shadow.Blur;
  ShadowOffsetX := AOwner.Shadow.OffsetX;
  ShadowOffsetY := AOwner.Shadow.OffsetY;
  ShadowColor := AOwner.Shadow.Color;
  Corners := AOwner.Corners;
  Sides := AOwner.Sides;
  XRadius := AOwner.XRadius;
  YRadius := AOwner.YRadius;
  BlurRadius := AOwner.BlurRadius;
end;

{***************************************************}
destructor TALImage.TResourceDownloadContext.Destroy;
begin
  ALFreeAndNil(ResourceStream);
  inherited
end;

{************************************************************}
function TALImage.TResourceDownloadContext.GetOwner: TALImage;
begin
  Result := TALImage(FOwner);
end;

{**********************************************}
constructor TALImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwnsResourceStream := True;
  FBackgroundColor := DefaultBackgroundColor;
  FLoadingColor := DefaultLoadingColor;
  FTintColor := DefaultTintColor;
  FBackgroundColorKey := DefaultBackgroundColorKey;
  FLoadingColorKey := DefaultLoadingColorKey;
  FTintColorKey := DefaultTintColorKey;
  FResourceName := '';
  FResourceStream := nil;
  FMaskResourceName := '';
  FHTTPHeaders := nil;
  FWrapMode := TALImageWrapMode.Fit;
  FApplyMetadataOrientation := False;
  FCorners := DefaultCorners;
  FSides := DefaultSides;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FBlurRadius := DefaultBlurRadius;
  FCacheIndex := 0;
  FLoadingCacheIndex := 0;
  FCacheEngine := nil;
  FCropCenter := CreateCropCenter;
  FCropCenter.OnChanged := CropCenterChanged;
  FStroke := CreateStroke;
  FStroke.OnChanged := StrokeChanged;
  FShadow := CreateShadow;
  FShadow.OnChanged := ShadowChanged;
  FResourceDownloadContext := nil;
  FFadeInDuration := DefaultFadeInDuration;
  FFadeInStartTimeNano := 0;
  fBufDrawable := ALNullDrawable;
  fBufLoadingDrawable := ALNullDrawable;
end;

{**************************}
destructor TALImage.Destroy;
begin
  if FOwnsResourceStream then
    ALFreeAndNil(FResourceStream);
  ALFreeAndNil(fCropCenter);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FShadow);
  inherited; // Will call CancelResourceDownload via ClearBufDrawable
end;

{***********************************}
procedure TALImage.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  CancelResourceDownload;
  inherited;
end;

{*********************************************************}
procedure TALImage.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALImage then begin
      BackgroundColor := TALImage(Source).BackgroundColor;
      BackgroundColorKey := TALImage(Source).BackgroundColorKey;
      LoadingColor := TALImage(Source).LoadingColor;
      LoadingColorKey := TALImage(Source).LoadingColorKey;
      TintColor := TALImage(Source).TintColor;
      TintColorKey := TALImage(Source).TintColorKey;
      ResourceName := TALImage(Source).ResourceName;
      if (TALImage(Source).OwnsResourceStream) and
         (TALImage(Source).ResourceStream <> nil) then begin
        var LStream := TMemoryStream.Create;
        try
          LStream.CopyFrom(TALImage(Source).ResourceStream);
          ResourceStream := LStream;
          OwnsResourceStream := True;
        except
          ALFreeAndNil(LStream);
          raise;
        end;
      end
      else begin
        ResourceStream := TALImage(Source).ResourceStream;
        OwnsResourceStream := TALImage(Source).OwnsResourceStream;
      end;
      MaskResourceName := TALImage(Source).MaskResourceName;
      HTTPHeaders := TALImage(Source).HTTPHeaders;
      WrapMode := TALImage(Source).WrapMode;
      ApplyMetadataOrientation := TALImage(Source).ApplyMetadataOrientation;
      Corners := TALImage(Source).Corners;
      Sides := TALImage(Source).Sides;
      XRadius := TALImage(Source).XRadius;
      YRadius := TALImage(Source).YRadius;
      BlurRadius := TALImage(Source).BlurRadius;
      CacheIndex := TALImage(Source).CacheIndex;
      LoadingCacheIndex := TALImage(Source).LoadingCacheIndex;
      CacheEngine := TALImage(Source).CacheEngine;
      CropCenter.Assign(TALImage(Source).CropCenter);
      Stroke.Assign(TALImage(Source).Stroke);
      Shadow.Assign(TALImage(Source).Shadow);
      FadeInDuration := TALImage(Source).FadeInDuration;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**************************************************************************}
function TALImage.IsReadyToDisplay(const AStrict: Boolean = False): Boolean;
begin
  Result := Inherited and
            ((not AStrict) or (FResourceDownloadContext = nil)) and
            ((FFadeInStartTimeNano <= 0) or
             ((ALElapsedTimeNano - FFadeInStartTimeNano) / ALNanosPerSec > FFadeInDuration));
end;

{**********************************************}
function TALImage.CreateCropCenter: TALPosition;
begin
  Result := TCropCenter.create;
end;

{*********************************************}
function TALImage.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{****************************************}
function TALImage.CreateShadow: TALShadow;
begin
  Result := TALShadow.Create;
end;

{******************************}
procedure TALImage.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    Stroke.AlignToPixel;
    Shadow.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{********************************************}
procedure TALImage.ApplyBackgroundColorScheme;
begin
  if FBackgroundColorKey <> '' then begin
    var LBackgroundColor := TALStyleManager.Instance.GetColor(FBackgroundColorKey);
    if FBackgroundColor <> LBackgroundColor then begin
      FBackgroundColor := LBackgroundColor;
      ClearBufDrawable;
      Repaint;
    end;
  end;
end;

{*****************************************}
procedure TALImage.ApplyLoadingColorScheme;
begin
  if FLoadingColorKey <> '' then begin
    var LLoadingColor := TALStyleManager.Instance.GetColor(FLoadingColorKey);
    if FLoadingColor <> LLoadingColor then begin
      FLoadingColor := LLoadingColor;
      ClearBufDrawable;
      Repaint;
    end;
  end;
end;

{**************************************}
procedure TALImage.ApplyTintColorScheme;
begin
  if FTintColorKey <> '' then begin
    var LTintColor := TALStyleManager.Instance.GetColor(FTintColorKey);
    if FTintColor <> LTintColor then begin
      FTintColor := LTintColor;
      ClearBufDrawable;
      Repaint;
    end;
  end;
end;

{**********************************}
procedure TALImage.ApplyColorScheme;
begin
  beginUpdate;
  try
    inherited;
    Stroke.ApplyColorScheme;
    Shadow.ApplyColorScheme;
    ApplyBackgroundColorScheme;
    ApplyLoadingColorScheme;
    ApplyTintColorScheme;
  finally
    EndUpdate;
  end;
end;

{******************************************}
function TALImage.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{*************************************************}
function TALImage.GetLoadingCacheSubIndex: Integer;
begin
  Result := 0;
end;

{*******************************************}
function TALImage.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{*******************************************************}
function TALImage.GetDefaultBackgroundColor: TalphaColor;
begin
  Result := TalphaColors.Null;
end;

{*****************************************************}
function TALImage.GetDefaultBackgroundColorKey: String;
begin
  Result := '';
end;

{****************************************************}
function TALImage.GetDefaultLoadingColor: TalphaColor;
begin
  Result := $FFe0e4e9;
end;

{**************************************************}
function TALImage.GetDefaultLoadingColorKey: String;
begin
  Result := '';
end;

{*************************************************}
function TALImage.GetDefaultTintColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{***********************************************}
function TALImage.GetDefaulttintColorKey: String;
begin
  Result := '';
end;

{*************************************************}
function TALImage.GetDefaultFadeInDuration: Single;
begin
  Result := 0.250;
end;

{********************************************}
function TALImage.GetDefaultCorners: TCorners;
begin
  Result := AllCorners;
end;

{****************************************}
function TALImage.GetDefaultSides: TSides;
begin
  Result := AllSides;
end;

{******************************************}
function TALImage.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{******************************************}
function TALImage.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{*********************************************}
function TALImage.GetDefaultBlurRadius: Single;
begin
  Result := 0;
end;

{*******************************************}
function TALImage.GetCropCenter: TALPosition;
begin
  Result := FCropCenter;
end;

{*********************************************************}
procedure TALImage.SetCropCenter(const Value: TALPosition);
begin
  FCropCenter.Assign(Value);
end;

{******************************************}
function TALImage.GetStroke: TALStrokeBrush;
begin
  Result := FStroke;
end;

{********************************************************}
procedure TALImage.SetStroke(const Value: TALStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{*************************************}
function TALImage.GetShadow: TALShadow;
begin
  Result := FShadow;
end;

{***************************************************}
procedure TALImage.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
end;

{************************************************************}
procedure TALImage.SetWrapMode(const Value: TALImageWrapMode);
begin
  if FWrapMode <> Value then begin
    ClearBufDrawable;
    FWrapMode := Value;
    Repaint;
  end;
end;

{*******************************************************************}
procedure TALImage.SetApplyMetadataOrientation(const Value: Boolean);
begin
  if FApplyMetadataOrientation <> Value then begin
    ClearBufDrawable;
    FApplyMetadataOrientation := Value;
    Repaint;
  end;
end;

{******************************************************}
procedure TALImage.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    ClearBufDrawable;
    FResourceName := Value;
    Repaint;
  end;
end;

{*********************************************************}
procedure TALImage.setResourceStream(const Value: TStream);
begin
  if FResourceStream <> Value then begin
    if FOwnsResourceStream then
      ALFreeAndNil(FResourceStream);
    ClearBufDrawable;
    FResourceStream := Value;
    Repaint;
  end;
end;

{**********************************************************}
procedure TALImage.setMaskResourceName(const Value: String);
begin
  if FMaskResourceName <> Value then begin
    ClearBufDrawable;
    FMaskResourceName := Value;
    Repaint;
  end;
end;

{**************************************************************}
procedure TALImage.setBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor <> Value then begin
    FBackgroundColor := Value;
    FBackgroundColorKey := '';
    ClearBufDrawable;
    Repaint;
  end;
end;

{************************************************************}
procedure TALImage.setBackgroundColorKey(const Value: String);
begin
  if FBackgroundColorKey <> Value then begin
    FBackgroundColorKey := Value;
    ApplyBackgroundColorScheme;
  end;
end;

{***********************************************************}
procedure TALImage.setLoadingColor(const Value: TAlphaColor);
begin
  if FLoadingColor <> Value then begin
    FLoadingColor := Value;
    FLoadingColorKey := '';
    ClearBufDrawable;
    Repaint;
  end;
end;

{*********************************************************}
procedure TALImage.setLoadingColorKey(const Value: String);
begin
  if FLoadingColorKey <> Value then begin
    FLoadingColorKey := Value;
    ApplyLoadingColorScheme;
  end;
end;

{********************************************************}
procedure TALImage.setTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then begin
    FTintColor := Value;
    FTintColorKey := '';
    ClearBufDrawable;
    Repaint;
  end;
end;

{******************************************************}
procedure TALImage.setTintColorKey(const Value: String);
begin
  if FTintColorKey <> Value then begin
    FTintColorKey := Value;
    ApplyTintColorScheme;
  end;
end;

{*************************************************}
procedure TALImage.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{*************************************************}
procedure TALImage.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{****************************************************}
procedure TALImage.SetBlurRadius(const Value: Single);
begin
  if not SameValue(FBlurRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FBlurRadius := Value;
    Repaint;
  end;
end;

{***************************************************}
procedure TALImage.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    ClearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{***********************************************}
procedure TALImage.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    ClearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{*************************************************}
function TALImage.IsBackgroundColorStored: Boolean;
begin
  Result := FBackgroundColor <> DefaultBackgroundColor;
end;

{****************************************************}
function TALImage.IsBackgroundColorKeyStored: Boolean;
begin
  Result := FBackgroundColorKey <> DefaultBackgroundColorKey;
end;

{**********************************************}
function TALImage.IsLoadingColorStored: Boolean;
begin
  Result := FLoadingColor <> DefaultLoadingColor;
end;

{*************************************************}
function TALImage.IsLoadingColorKeyStored: Boolean;
begin
  Result := FLoadingColorKey <> DefaultLoadingColorKey;
end;

{*******************************************}
function TALImage.IsTintColorStored: Boolean;
begin
  Result := FTintColor <> DefaultTintColor;
end;

{**********************************************}
function TALImage.IsTintColorKeyStored: Boolean;
begin
  Result := FTintColorKey <> DefaultTintColorKey;
end;

{************************************************}
function TALImage.IsFadeInDurationStored: Boolean;
begin
  Result := not SameValue(FFadeInDuration, DefaultFadeInDuration, TEpsilon.Vector);
end;

{*****************************************}
function TALImage.IsCornersStored: Boolean;
begin
  Result := FCorners <> DefaultCorners;
end;

{***************************************}
function TALImage.IsSidesStored: Boolean;
begin
  Result := FSides <> DefaultSides;
end;

{*****************************************}
function TALImage.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{*****************************************}
function TALImage.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{********************************************}
function TALImage.IsBlurRadiusStored: Boolean;
begin
  Result := not SameValue(FBlurRadius, DefaultBlurRadius, TEpsilon.Vector);
end;

{****************************************************}
procedure TALImage.CropCenterChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{************************************************}
procedure TALImage.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{************************************************}
procedure TALImage.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{***************************}
procedure TALImage.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************}
procedure TALImage.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     ((not ALIsDrawableNull(fBufDrawable)) or
      (not ALIsDrawableNull(fBufLoadingDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  CancelResourceDownload;
  FFadeInStartTimeNano := 0;
  ALFreeAndNilDrawable(fBufDrawable);
  ALFreeAndNilDrawable(fBufLoadingDrawable);
end;

{****************************************}
procedure TALImage.CancelResourceDownload;
begin
  // The FResourceDownloadContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FResourceDownloadContext <> nil then begin
    var LContextToFree: TResourceDownloadContext;
    var LLock := FResourceDownloadContext.FLock;
    ALMonitorEnter(LLock{$IF defined(DEBUG)}, 'TALImage.CancelResourceDownload'{$ENDIF});
    try
      if not FResourceDownloadContext.FManagedByWorkerThread then LContextToFree := FResourceDownloadContext
      else LContextToFree := nil;
      FResourceDownloadContext.FOwner := nil;
      FResourceDownloadContext := nil;
    Finally
      ALMonitorExit(LLock{$IF defined(DEBUG)}, 'TALImage.CancelResourceDownload'{$ENDIF});
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{*************}
//[MultiThread]
class function TALImage.CanStartResourceDownload(var AContext: Tobject): boolean;
begin
  result := TResourceDownloadContext(AContext).FOwner <> nil;
end;

{*************}
//[MultiThread]
class procedure TALImage.HandleResourceDownloadSuccess(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject);
begin
  var LContext := TResourceDownloadContext(AContext);
  if LContext.FOwner = nil then exit;
  LContext.ResourceStream := AContentStream;
  TALGraphicThreadPool.Instance.ExecuteProc(
    CreateBufDrawable, // const AProc: TALWorkerThreadProc;
    LContext, // const AContext: Tobject; TALGraphicThreadPool.Instance will own and release the Context object
    GetResourceDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  AContentStream := nil; // AContentStream Will be free by AContext
  AContext := nil; // AContext will be free by TALGraphicThreadPool.Instance
end;

{*************}
//[MultiThread]
class procedure TALImage.HandleResourceDownloadError(const AResponse: IHTTPResponse; const AErrMessage: string; var AContext: Tobject);
begin
  var LContext := TResourceDownloadContext(AContext);
  if LContext.FOwner = nil then exit;
  {$IFDEF ALDPK}
  ALMonitorEnter(LContext.FLock{$IF defined(DEBUG)}, 'TALImage.HandleResourceDownloadError (1)'{$ENDIF});
  try
    if LContext.Owner <> nil then begin
      LContext.FManagedByWorkerThread := False;
      AContext := nil; // AContext will be free by CancelResourceDownload
    end;
  finally
    ALMonitorExit(LContext.FLock{$IF defined(DEBUG)}, 'TALImage.HandleResourceDownloadError (1)'{$ENDIF});
  end;
  exit;
  {$ENDIF}
  if LContext.ResourceName = ALBrokenImageResourceName then begin
    ALLog(
      'TALImage.HandleResourceDownloadError',
      'BrokenImage resource is missing or incorrect | ' +
      AErrMessage,
      TalLogType.error);
    ALMonitorEnter(LContext.FLock{$IF defined(DEBUG)}, 'TALImage.HandleResourceDownloadError (2)'{$ENDIF});
    try
      if LContext.FOwner <> nil then begin
        LContext.FManagedByWorkerThread := False;
        AContext := nil; // AContext will be free by CancelResourceDownload
      end;
    finally
      ALMonitorExit(LContext.FLock{$IF defined(DEBUG)}, 'TALImage.HandleResourceDownloadError (2)'{$ENDIF});
    end;
    exit;
  end;
  ALLog(
    'TALImage.HandleResourceDownloadError',
    'Url: ' + LContext.ResourceName + ' | ' +
    AErrMessage,
    TalLogType.warn);
  LContext.Rect := TRectF.Create(
                     LContext.Rect.TopLeft,
                     ALBrokenImageWidth,
                     ALBrokenImageHeight);
  //LContext.Scale: Single;
  //LContext.AlignToPixel: Boolean;
  LContext.Color := TalphaColors.Null;
  LContext.TintColor := TAlphaColors.Null;
  LContext.ResourceName := ALBrokenImageResourceName;
  ALFreeAndNil(LContext.ResourceStream);
  LContext.MaskResourceName := '';
  LContext.WrapMode := TALImageWrapMode.Fit;
  //LContext.CropCenter: TpointF;
  //LContext.ApplyMetadataOrientation: Boolean;
  LContext.StrokeColor := TalphaColors.Null;
  //LContext.StrokeThickness: Single;
  //LContext.ShadowBlur: Single;
  //LContext.ShadowOffsetX: Single;
  //LContext.ShadowOffsetY: Single;
  LContext.ShadowColor := TAlphaColors.Null;
  LContext.Corners := AllCorners;
  LContext.Sides := AllSides;
  LContext.XRadius := 0;
  LContext.YRadius := 0;
  LContext.BlurRadius := 0;
  TALGraphicThreadPool.Instance.ExecuteProc(
    CreateBufDrawable, // const AProc: TALWorkerThreadProc;
    LContext, // const AContext: Tobject; TALGraphicThreadPool.Instance will own and release the Context object
    GetResourceDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  AContext := nil; // AContext will be free by TALGraphicThreadPool.Instance
end;

{*************}
//[MultiThread]
class function TALImage.GetResourceDownloadPriority(const AContext: Tobject): Int64;
begin
  result := TALNetHttpClientPool.Instance.PriorityStartingPoint;
end;

{*************}
//[MultiThread]
class Procedure TALImage.CreateBufDrawable(var AContext: TObject);
begin
  var LContext := TResourceDownloadContext(AContext);
  if LContext.FOwner = nil then exit;
  var LBufDrawable: TALDrawable := ALNullDrawable;
  var LBufDrawableRect: TRectF;
  Try
    CreateBufDrawable(
      LBufDrawable, // var ABufDrawable: TALDrawable;
      LBufDrawableRect, // out ABufDrawableRect: TRectF;
      LContext.Rect, // const ARect: TRectF;
      LContext.Scale, // const AScale: Single;
      LContext.AlignToPixel, // const AAlignToPixel: Boolean;
      LContext.Color, // const AColor: TAlphaColor;
      LContext.TintColor, // const ATintColor: TAlphaColor;
      LContext.ResourceName, // const AResourceName: String;
      LContext.ResourceStream, // const AResourceStream: TStream;
      LContext.MaskResourceName, // const AMaskResourceName: String;
      LContext.WrapMode, // const AWrapMode: TALImageWrapMode;
      LContext.CropCenter, // const ACropCenter: TpointF;
      LContext.ApplyMetadataOrientation, // const AApplyMetadataOrientation: Boolean;
      LContext.StrokeColor, // const AStrokeColor: TAlphaColor;
      LContext.StrokeThickness, // const AStrokeThickness: Single;
      LContext.ShadowBlur, // const AShadowBlur: Single;
      LContext.ShadowOffsetX, // const AShadowOffsetX: Single;
      LContext.ShadowOffsetY, // const AShadowOffsetY: Single;
      LContext.ShadowColor, // const AShadowColor: TAlphaColor;
      LContext.Corners, // const ACorners: TCorners;
      LContext.Sides, // const ASides: TSides;
      LContext.XRadius, // const AXRadius: Single;
      LContext.YRadius, // const AYRadius: Single;
      LContext.BlurRadius); // const ABlurRadius: Single)
  except
    On E: Exception do begin
      HandleResourceDownloadError(nil{AResponse}, E.Message, AContext);
      exit;
    end;
  End;
  TThread.queue(nil,
    procedure
    begin
      if LContext.FOwner <> nil then begin
        var LOwner := LContext.Owner;
        if (LOwner.FFadeInDuration > 0) and
           (LContext.ResourceName <> ALBrokenImageResourceName) and
           (not ALIsDrawableNull(LBufDrawable)) then begin
          LOwner.FFadeInStartTimeNano := ALElapsedTimeNano;
        end
        else begin
          ALFreeAndNilDrawable(LOwner.fBufLoadingDrawable);
          LOwner.FFadeInStartTimeNano := 0;
        end;
        ALFreeAndNilDrawable(LOwner.fBufDrawable);
        LOwner.fBufDrawable := LBufDrawable;
        LOwner.FBufDrawableRect := LBufDrawableRect;
        LOwner.FResourceDownloadContext := nil;
        LOwner.Repaint;
      end;
      ALFreeAndNil(LContext);
    end);
  AContext := nil; // AContext will be free by TThread.queue
end;

{*************}
//[MultiThread]
class Procedure TALImage.CreateBufDrawable(
                  var ABufDrawable: TALDrawable;
                  out ABufDrawableRect: TRectF;
                  const ARect: TRectF;
                  const AScale: Single;
                  const AAlignToPixel: Boolean;
                  const AColor: TAlphaColor;
                  const ATintColor: TAlphaColor;
                  const AResourceName: String;
                  const AResourceStream: TStream;
                  const AMaskResourceName: String;
                  const AWrapMode: TALImageWrapMode;
                  const ACropCenter: TpointF;
                  const AApplyMetadataOrientation: Boolean;
                  const AStrokeColor: TAlphaColor;
                  const AStrokeThickness: Single;
                  const AShadowBlur: Single;
                  const AShadowOffsetX: Single;
                  const AShadowOffsetY: Single;
                  const AShadowColor: TAlphaColor;
                  const ACorners: TCorners;
                  const ASides: TSides;
                  const AXRadius: Single;
                  const AYRadius: Single;
                  const ABlurRadius: Single);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var lResourceName: String;
  if ALIsHttpOrHttpsUrl(AResourceName) then lResourceName := ''
  else lResourceName := AResourceName;
  var LFileName := ALGetResourceFilename(lResourceName);

  ABufDrawableRect := ARect;

  {$REGION 'Use ALCreateDrawableFromResource'}
  if ((AResourceStream <> nil) or (LFileName <> '') or (LResourceName <> '')) and
     (ACorners = AllCorners) and
     (Acolor = TalphaColors.Null) and
     ((AShadowColor = TalphaColors.Null) or (CompareValue(AShadowBlur, 0, TEpsilon.position) <= 0)) and
     ((AStrokeColor = TalphaColors.Null) or (CompareValue(AStrokeThickness, 0, TEpsilon.position) <= 0)) then begin

    {$IFDEF ALDPK}
    try
    {$ENDIF}

      ABufDrawable := ALCreateDrawableFromResource(
                        AResourceName, // const AResourceName: String;
                        AResourceStream, // const AResourceStream: TStream;
                        AMaskResourceName, // const AMaskResourceName: String;
                        AScale, // const AScale: Single;
                        ABufDrawableRect.Width, ABufDrawableRect.Height, // const W, H: single;
                        AApplyMetadataOrientation, // const AApplyMetadataOrientation: Boolean;
                        AWrapMode, // const AWrapMode: TALImageWrapMode;
                        ACropCenter, // const ACropCenter: TpointF;
                        ATintColor, // const ATintColor: TAlphaColor;
                        ABlurRadius, // const ABlurRadius: single;
                        AXRadius, // const AXRadius: Single;
                        AYRadius); // const AYRadius: Single);

    {$IFDEF ALDPK}
    except
      ABufDrawable := ALCreateEmptyDrawable1x1;
      Exit;
    end;
    {$ENDIF}

  end;
  {$ENDREGION}

  if not ALIsDrawableNull(ABufDrawable) then begin
    ABufDrawableRect := TrectF.Create(0,0, ALGetDrawableWidth(ABufDrawable)/AScale, ALGetDrawableHeight(ABufDrawable)/AScale).CenterAt(ARect);
    Exit;
  end;

  {$REGION 'Use TALDrawRectangleHelper'}
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AAlignToPixel, // const AAlignToPixel: Boolean;
                        AColor, // const AFillColor: TAlphaColor;
                        [], // const AFillGradientColors: TArray<TAlphaColor>;
                        LResourceName, // const AFillResourceName: String;
                        AResourceStream, // const AFillResourceStream: TStream;
                        TRectF.Empty, // Const AFillBackgroundMarginsRect: TRectF;
                        TRectF.Empty, // Const AFillImageMarginsRect: TRectF;
                        0, // const AStateLayerOpacity: Single;
                        TAlphaColors.Null, // const AStateLayerColor: TAlphaColor;
                        False, // const AStateLayerUseContentColor: Boolean;
                        TRectF.Empty, // Const AStateLayerMarginsRect: TRectF;
                        AShadowColor, // const AShadowColor: TAlphaColor;
                        AShadowBlur, // const AShadowBlur: Single;
                        AShadowOffsetX, // const AShadowOffsetX: Single;
                        AShadowOffsetY); // const AShadowOffsetY: Single): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(AAlignToPixel)
        .SetDstRect(ABufDrawableRect)
        .SetFillColor(AColor)
        .SetFillResourceName(LResourceName)
        .SetFillResourceStream(AResourceStream)
        .SetFillMaskResourceName(AMaskResourceName)
        .SetFillApplyMetadataOrientation(AApplyMetadataOrientation)
        .SetFillImageTintColor(ATintColor)
        .SetFillWrapMode(AWrapMode)
        .SetFillCropCenter(ACropCenter)
        .SetFillBlurRadius(ABlurRadius)
        .SetStrokeColor(AStrokecolor)
        .SetStrokeThickness(AStrokeThickness)
        .SetShadowColor(AShadowColor)
        .SetShadowBlur(AShadowBlur)
        .SetShadowOffsetX(AShadowOffsetX)
        .SetShadowOffsetY(AShadowOffsetY)
        .SetSides(ASides)
        .SetCorners(ACorners)
        .SetXRadius(AXRadius)
        .SetYRadius(AYRadius)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;
  {$ENDREGION}

end;

{*********************************}
procedure TALImage.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if FResourceName and FResourceStream are empty
     ((FResourceName = '') and (FResourceStream = nil))
  then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) or
     (FResourceDownloadContext <> nil) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  if (FResourceDownloadContext = nil) and
     (FResourceStream = nil) and
     (ALIsHttpOrHttpsUrl(ResourceName)) then begin

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Starting download | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
    {$endif}

    FResourceDownloadContext := TResourceDownloadContext.Create(Self);
    Try
      TALNetHttpClientPool.Instance.Get(
        ResourceName, // const AUrl: String;
        CanStartResourceDownload, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
        HandleResourceDownloadSuccess, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
        HandleResourceDownloadError, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
        FResourceDownloadContext, // const AContext: Tobject; // Context will be free by the worker thread
        true, // const AUseCache: Boolean = True;
        GetResourceDownloadPriority, // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
        HTTPHeaders); // const AHeaders: TNetHeaders = nil;
    except
      ALFreeAndNil(FResourceDownloadContext);
      Raise;
    End;

    if (LoadingColor <> TAlphaColors.Null) and
       ((MaskResourceName <> '') or
        (not SameValue(xRadius, 0, TEpsilon.Vector)) or
        (not SameValue(yRadius, 0, TEpsilon.Vector))) then begin

      if (LoadingCacheIndex > 0) and
         (CacheEngine <> nil) and
         (CacheEngine.HasEntry(LoadingCacheIndex{AIndex}, GetLoadingCacheSubIndex{ASubIndex})) then Exit;

      {$IFDEF debug}
      ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Creating loading content | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
      {$endif}

      CreateBufDrawable(
        FBufLoadingDrawable, // var ABufDrawable: TALDrawable;
        FBufLoadingDrawableRect, // out ABufDrawableRect: TRectF;
        LocalRect, // const ARect: TRectF;
        ALGetScreenScale, // const AScale: Single;
        AutoAlignToPixel, // const AAlignToPixel: Boolean;
        ALIfThen(MaskResourceName <> '', TAlphaColors.null, LoadingColor), // const AColor: TAlphaColor;
        ALIfThen(MaskResourceName <> '', LoadingColor, TAlphaColors.null), // const ATintColor: TAlphaColor;
        MaskResourceName, // const AResourceName: String;
        nil, // const AResourceStream: TStream;
        '', // const AMaskResourceName: String;
        WrapMode, // const AWrapMode: TALImageWrapMode;
        TpointF.Zero, // const ACropCenter: TpointF;
        false, // const AApplyMetadataOrientation: Boolean;
        TAlphaColors.Null, // const AStrokeColor: TAlphaColor;
        0, // const AStrokeThickness: Single;
        0, // const AShadowBlur: Single;
        0, // const AShadowOffsetX: Single;
        0, // const AShadowOffsetY: Single;
        TAlphaColors.Null, // const AShadowColor: TAlphaColor;
        Corners, // const ACorners: TCorners;
        AllSides, // const ASides: TSides;
        XRadius, // const AXRadius: Single;
        YRadius, // const AYRadius: Single;
        0); // const ABlurRadius: Single);
    end;

    exit;

  end;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // out ABufDrawableRect: TRectF;
    LocalRect, // const ARect: TRectF;
    ALGetScreenScale, // const AScale: Single;
    AutoAlignToPixel, // const AAlignToPixel: Boolean;
    BackGroundColor, // const AColor: TAlphaColor;
    TintColor, // const ATintColor: TAlphaColor;
    ResourceName, // const AResourceName: String;
    ResourceStream, // const AResourceStream: TStream;
    MaskResourceName, // const AMaskResourceName: String;
    WrapMode, // const AWrapMode: TALImageWrapMode;
    CropCenter.Point, // const ACropCenter: TpointF;
    ApplyMetadataOrientation, // const AApplyMetadataOrientation: Boolean;
    Stroke.Color, // const AStrokeColor: TAlphaColor;
    Stroke.Thickness, // const AStrokeThickness: Single;
    Shadow.Blur, // const AShadowBlur: Single;
    Shadow.OffsetX, // const AShadowOffsetX: Single;
    Shadow.OffsetY, // const AShadowOffsetY: Single;
    Shadow.Color, // const AShadowColor: TAlphaColor;
    Corners, // const ACorners: TCorners;
    Sides, // const ASides: TSides;
    XRadius, // const AXRadius: Single;
    YRadius, // const AYRadius: Single;
    BlurRadius); // const ABlurRadius: Single);

end;

{***********************}
procedure TALImage.Paint;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure _Invalidate;
  begin
    // We cannot call Repaint from within a paint method,
    // but we can call Form.Invalidate. We use Form.Invalidate
    // to avoid using any TALFloatAnimation object
    {$IF defined(ANDROID)}
    If Form <> nil then
      Form.Invalidate;
    {$ELSE}
    If Form <> nil then begin
      var LForm := Form;
      TThread.ForceQueue(nil,
        procedure
        begin
          If (Screen <> nil) then
            for var I := 0 to Screen.FormCount - 1 do
              if LForm = Screen.Forms[I] then begin
                LForm.Invalidate;
                Break;
              end;
        end);
    end;
    {$ENDIF}
  end;

begin

  // Draw a dashed rectangle in design mode
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    var R := LocalRect;
    system.types.InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  // Calculate the opacity based on FFadeInStartTimeNano.
  var LOpacity: Single := AbsoluteOpacity;
  if FFadeInStartTimeNano > 0 then begin
    {$IF defined(DEBUG)}
    // Not possible; we are initiating a FadeIn
    // effect while still downloading the image.
    if (FResourceDownloadContext <> nil) then
      Raise Exception.Create('Error 821554A1-5E7D-4920-BA1A-CECA32A9D967');
    {$ENDIF}
    var LElapsedTime := (ALElapsedTimeNano - FFadeInStartTimeNano) / ALNanosPerSec;
    if LElapsedTime > FFadeInDuration then FFadeInStartTimeNano := 0
    else begin
      LOpacity := LOpacity * (LElapsedTime / FFadeInDuration);
      _invalidate;
    end;
  end;

  // Retrieve the image drawable in LDrawable.
  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if (CacheIndex <= 0) or
     (CacheEngine = nil) or
     (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
    MakeBufDrawable;
    if (LoadingCacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufLoadingDrawable)) then begin
      if not CacheEngine.TrySetEntry(LoadingCacheIndex{AIndex}, GetLoadingCacheSubIndex{ASubIndex}, fBufLoadingDrawable{ADrawable}, fBufLoadingDrawableRect{ARect}) then ALFreeAndNiLDrawable(fBufLoadingDrawable)
      else fBufLoadingDrawable := ALNullDrawable;
    end;
    if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
      if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
      else fBufDrawable := ALNullDrawable;
      if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  // Retrieve the loading drawable in LLoadingDrawable.
  var LLoadingDrawable: TALDrawable := ALNullDrawable;
  var LLoadingDrawableRect: TRectF;
  if (((ALIsDrawableNull(LDrawable)) and (FResourceDownloadContext <> nil)) or
      (FFadeInStartTimeNano > 0)) and
     ((LoadingCacheIndex <= 0) or
      (CacheEngine = nil) or
      (not CacheEngine.TryGetEntry(LoadingCacheIndex{AIndex}, GetLoadingCacheSubIndex{ASubIndex}, LLoadingDrawable{ADrawable}, LLoadingDrawableRect{ARect}))) then begin
    LLoadingDrawable := fBufLoadingDrawable;
    LLoadingDrawableRect := fBufLoadingDrawableRect;
  end;

  // Draw LLoadingDrawable if there is one.
  If not ALIsDrawableNull(LLoadingDrawable) then begin
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      LLoadingDrawable, // const ADrawable: TALDrawable;
      LLoadingDrawableRect.TopLeft, // const ATopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single);
  end

  // Otherwise, draw a loading background if necessary.
  else if ((ALIsDrawableNull(LDrawable) and (FResourceDownloadContext <> nil)) or
           (FFadeInStartTimeNano > 0)) and
          (LoadingColor <> TAlphaColors.Null) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(LocalRect)
      .SetOpacity(AbsoluteOpacity)
      .SetFillColor(FloadingColor)
      .SetCorners(FCorners)
      .SetXRadius(FXRadius)
      .SetYRadius(FYRadius)
      .Draw;
    {$ELSE}
    Canvas.Fill.kind := TBrushKind.solid;
    Canvas.Fill.color := FloadingColor;
    Canvas.FillRect(ALAlignToPixelRound(LocalRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position), XRadius, YRadius, FCorners, AbsoluteOpacity, TCornerType.Round);
    {$ENDIF}
  end;

  // Exit if LDrawable is null
  If ALIsDrawableNull(LDrawable) then
    Exit;

  // Clear fBufLoadingDrawable
  if FFadeInStartTimeNano <= 0 then
    ALFreeAndNilDrawable(fBufLoadingDrawable);

  // Draw the LDrawable
  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    LOpacity); // const AOpacity: Single);

end;

{*****************************************************************************}
constructor TALAnimatedImage.TAnimation.Create(const AOwner: TALAnimatedImage);
begin
  inherited create;
  fOwner := AOwner;
  inherited Duration := MaxSingle;
  FDuration := 0.0;
  FSpeed := 1.0;
  FStartProgress := 0;
  FStopProgress := 1.0;
  fCurrentProgress := 0;
  FEnabled := True;
end;

{****************************************************************}
procedure TALAnimatedImage.TAnimation.Assign(Source: TPersistent);
begin
  if Source is TALAnimatedImage.TAnimation then begin
    Speed := TALAnimatedImage.TAnimation(Source).Speed;
    StartProgress := TALAnimatedImage.TAnimation(Source).StartProgress;
    StopProgress := TALAnimatedImage.TAnimation(Source).StopProgress;
    inherited Assign(Source);
    Enabled := TALAnimatedImage.TAnimation(Source).Enabled;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{******************************************}
procedure TALAnimatedImage.TAnimation.Start;
begin
  if (Running) then
    Exit;
  FEnabled := True;
  fCurrentProgress := FStartProgress;
  inherited Start;
end;

{*********************************************************************}
procedure TALAnimatedImage.TAnimation.UpdateInheritedAnimationDuration;
begin
  if not SameValue(FSpeed, 0.0, Single.Epsilon) then
    inherited Duration := (FDuration / FSpeed) * abs(StopProgress - StartProgress)
  else
    inherited Duration := maxSingle;
end;

{********************************************}
procedure TALAnimatedImage.TAnimation.repaint;
begin
  if Fowner.IsDisplayed then
    Fowner.Repaint
  else if Loop then
    Pause;
end;

{***********************************************************}
function TALAnimatedImage.TAnimation.GetDefaultLoop: Boolean;
begin
  Result := True;
end;

{*******************************************************}
function TALAnimatedImage.TAnimation.GetDuration: Single;
begin
  Result := FDuration;
end;

{**********************************************************}
function TALAnimatedImage.TAnimation.GetCurrentTime: Single;
begin
  if not Enabled then begin
    if Inverse then Result := StopProgress * Duration
    else Result := StartProgress * Duration;
  end
  else begin
    {$IFDEF ALDPK}
    Result := StartProgress * Duration;
    {$ELSE}
    Result := CurrentProgress * Duration;
    {$ENDIF}
  end;
end;

{****************************************************}
function TALAnimatedImage.TAnimation.GetSpeed: Single;
begin
  Result := FSpeed;
end;

{******************************************************************}
procedure TALAnimatedImage.TAnimation.SetSpeed(const Value: Single);
begin
  if not SameValue(FSpeed, Value, Single.Epsilon) then begin
    FSpeed := Value;
    UpdateInheritedAnimationDuration;
  end;
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure TALAnimatedImage.TAnimation.SetDuration(const Value: Single);
begin
  FDuration := Value;
  UpdateInheritedAnimationDuration;
end;
{$ENDIF}

{*********************************************************************}
procedure TALAnimatedImage.TAnimation.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    if not FEnabled then
      inherited Enabled := False;
  end;
end;

{**************************************************************************}
procedure TALAnimatedImage.TAnimation.SetStartProgress(const Value: Single);
begin
  FStartProgress := Min(Max(Value, 0), 1);
  UpdateInheritedAnimationDuration;
  Repaint;
end;

{*************************************************************************}
procedure TALAnimatedImage.TAnimation.SetStopProgress(const Value: Single);
begin
  FStopProgress := Min(Max(Value, 0), 1);
  UpdateInheritedAnimationDuration;
  Repaint;
end;

{*****************************************************************}
function TALAnimatedImage.TAnimation.IsStopProgressStored: Boolean;
begin
  Result := Not SameValue(FStopProgress, 1.0, Single.Epsilon);
end;

{**********************************************************}
function TALAnimatedImage.TAnimation.IsSpeedStored: Boolean;
begin
  Result := Not SameValue(FSpeed, 1.0, Single.Epsilon);
end;

{*****************************************************}
procedure TALAnimatedImage.TAnimation.ProcessAnimation;
begin
  fCurrentProgress := FStartProgress + (FStopProgress - FStartProgress) * NormalizedTime;
end;

{*************************************************}
procedure TALAnimatedImage.TAnimation.DoFirstFrame;
begin
  inherited;
  if Enabled then begin
    if assigned(FOwner.FOnAnimationFirstFrame) then
      FOwner.FOnAnimationFirstFrame(FOwner);
    Repaint;
  end;
end;

{**********************************************}
procedure TALAnimatedImage.TAnimation.DoProcess;
begin
  inherited;
  if Enabled then begin
    if assigned(FOwner.FOnAnimationProcess) then
      FOwner.FOnAnimationProcess(FOwner);
    Repaint;
  end;
end;

{*********************************************}
procedure TALAnimatedImage.TAnimation.DoFinish;
begin
  inherited;
  if Enabled then begin
    if assigned(FOwner.FOnAnimationFinish) then
      FOwner.FOnAnimationFinish(FOwner);
    Repaint;
  end;
end;

{******************************************************}
constructor TALAnimatedImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAnimation := TAnimation.create(Self);
  {$IF defined(ALSkiaAvailable)}
    fSkottieAnimation := 0;
    fAnimcodecplayer := 0;
    //FRenderRect := TrectF.Empty;
    {$IF not defined(ALSkiaCanvas)}
      FBufSurface := 0;
      FBufCanvas := 0;
      {$IF defined(ALGPUCanvas)}
      FbufTexture := nil;
      {$ELSE}
      FBufBitmap := nil;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  FResourceName := '';
  FTintColor := DefaultTintColor;
  FTintColorKey := DefaultTintColorKey;
  FWrapMode := TALImageWrapMode.Fit;
  FOnAnimationFirstFrame := nil;
  FOnAnimationProcess := nil;
  FOnAnimationFinish := nil;
  SetAcceptsControls(False);
end;

{**********************************}
destructor TALAnimatedImage.Destroy;
begin
  ReleaseCodec;
  AlFreeAndNil(FAnimation);
  inherited;
end;

{*******************************************}
procedure TALAnimatedImage.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Necessary if the control is destroyed using
  // AlFreeAndNil with the delayed flag
  FAnimation.Enabled := False;
  inherited;
end;

{*****************************************************************}
procedure TALAnimatedImage.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALAnimatedImage then begin
      Animation.Assign(TALAnimatedImage(Source).Animation);
      ResourceName := TALAnimatedImage(Source).ResourceName;
      TintColor := TALAnimatedImage(Source).TintColor;
      TintColorKey := TALAnimatedImage(Source).TintColorKey;
      WrapMode := TALAnimatedImage(Source).WrapMode;
      OnAnimationFirstFrame := TALAnimatedImage(Source).OnAnimationFirstFrame;
      OnAnimationProcess := TALAnimatedImage(Source).OnAnimationProcess;
      OnAnimationFinish := TALAnimatedImage(Source).OnAnimationFinish;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**********************************************}
procedure TALAnimatedImage.ApplyTintColorScheme;
begin
  if FTintColorKey <> '' then begin
    var LTintColor := TALStyleManager.Instance.GetColor(FTintColorKey);
    if FTintColor <> LTintColor then begin
      FTintColor := LTintColor;
      releaseCodec;
      Repaint;
    end;
  end;
end;

{******************************************}
procedure TALAnimatedImage.ApplyColorScheme;
begin
  beginUpdate;
  try
    inherited;
    ApplyTintColorScheme;
  finally
    EndUpdate;
  end;
end;

{*************************************}
procedure TALAnimatedImage.CreateCodec;
begin
  {$IF defined(ALSkiaAvailable)}

  if //--- Do not create Codec if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create Codec if FResourceName is empty
     (FResourceName = '')
  then begin
    ReleaseCodec;
    exit;
  end;

  if (fSkottieAnimation <> 0) or (fAnimcodecplayer <> 0) then exit;

  var LFileName := ALGetResourceFilename(FResourceName);

  if (LFileName <> '') and (FTintColor = TAlphaColors.Null) then begin
    fSkottieAnimation := sk4d_skottieanimation_make_from_file(MarshaledAString(UTF8String(LFileName)), TSkDefaultProviders.TypefaceFont.Handle)
  end
  else begin
    {$IFDEF ALDPK}
    fSkottieAnimation := 0
    {$ELSE}
    var LStream: TStream;
    if (FTintColor <> TAlphaColors.Null) then begin
      LStream := TALStringStreamA.Create('');
      try
        if (LFileName <> '') then TALStringStreamA(LStream).LoadFromFile(LFileName)
        else begin
          var LResourceStream := ALCreateResourceStream(FResourceName);
          try
            TALStringStreamA(LStream).LoadFromStream(LResourceStream);
          finally
            ALFreeAndNil(LResourceStream);
          end;
        end;
        var LDataString := TALStringStreamA(LStream).DataString;
        // Note: The pattern '{"ty":"fl","c":{"a":0,"k":[' is likely too restrictive.
        // It currently matches our needs, but may fail in future cases.
        // If issues arise, the code below should be updated accordingly.
        var P1 := ALPosA('{"ty":"fl","c":{"a":0,"k":[', LDataString); // ...{"ty":"fl","c":{"a":0,"k":[0.544964001225,0.42151740579,0.713083424288,1],"ix":4},"o":{"a":0,"k":100,"ix":5},"r":1,"bm":0,"nm":"Fill 1","mn":"ADBE Vector Graphic - Fill","hd":false}
        //                                                                  ^P1
        While P1 > 0 do begin
          inc(P1, 27{length('{"ty":"fl","c":{"a":0,"k":[')}); // ...{"ty":"fl","c":{"a":0,"k":[0.544964001225,0.42151740579,0.713083424288,1],"ix":4},"o":{"a":0,"k":100,"ix":5},"r":1,"bm":0,"nm":"Fill 1","mn":"ADBE Vector Graphic - Fill","hd":false}
          //                                                                                   ^P1
          var P2 := ALPosA(']', LDataString, P1); // ...{"ty":"fl","c":{"a":0,"k":[0.544964001225,0.42151740579,0.713083424288,1],"ix":4},"o":{"a":0,"k":100,"ix":5},"r":1,"bm":0,"nm":"Fill 1","mn":"ADBE Vector Graphic - Fill","hd":false}
          //                                                                       ^P1                                          ^P2
          if P2 <= 0 then raise Exception.Create('Error 3D3E0DFB-E8E3-4A11-A91F-83A66F0F63FB');
          var LAlphaColorRec := TAlphacolorRec.Create(FTintColor);
          var LTotalAvailableLength := P2 - P1; // = length of 0.544964001225,0.42151740579,0.713083424288,1
          var LAvailablePrecisionByChannel: Integer;
          if LAlphaColorRec.A = 255 then
            LAvailablePrecisionByChannel := (LTotalAvailableLength - 10{'0.' - ',0.' - ',0.' - ',1'}) div 3
          else
            LAvailablePrecisionByChannel := (LTotalAvailableLength - 11{'0.' - ',0.' - ',0.' - ',0.'}) div 4;
          if LAvailablePrecisionByChannel >= 3 then begin
            var LNewColorsStr: AnsiString;
            if LAlphaColorRec.A = 255 then
              LNewColorsStr := ALFormatA(
                                 '%.'+ALInttostrA(LAvailablePrecisionByChannel)+'f,%.'+ALInttostrA(LAvailablePrecisionByChannel)+'f,%.'+ALInttostrA(LAvailablePrecisionByChannel)+'f,1',
                                 [LAlphaColorRec.R / 255,
                                  LAlphaColorRec.G / 255,
                                  LAlphaColorRec.B / 255])
            else
              LNewColorsStr := ALFormatA(
                                 '%.'+ALInttostrA(LAvailablePrecisionByChannel)+'f,%.'+ALInttostrA(LAvailablePrecisionByChannel)+'f,%.'+ALInttostrA(LAvailablePrecisionByChannel)+'f,%.'+ALInttostrA(LAvailablePrecisionByChannel)+'f',
                                 [LAlphaColorRec.R / 255,
                                  LAlphaColorRec.G / 255,
                                  LAlphaColorRec.B / 255,
                                  LAlphaColorRec.A / 255]);
            {$IF defined(debug)}
            if Length(LNewColorsStr) > P2 - P1 then
             raise Exception.Create('Error DED1DB9C-A723-4240-AE50-4FBF02293B2F');
            {$ENDIF}
            if Length(LNewColorsStr) < P2 - P1 then begin
              var LOldLength := Length(LNewColorsStr);
              SetLength(LNewColorsStr, P2 - P1);
              FillChar(PAnsiChar(LNewColorsStr)[LOldLength], (P2 - P1) - LOldLength, Ord(' '));
            end;
            ALMove(PAnsiChar(LNewColorsStr)^, PAnsiChar(LDataString)[P1-1], length(LNewColorsStr));
          end
          else begin
            var LNewColorsStr := ALFormatA(
                                   '%.6f,%.6f,%.6f,%.6f',
                                   [LAlphaColorRec.R / 255,
                                    LAlphaColorRec.G / 255,
                                    LAlphaColorRec.B / 255,
                                    LAlphaColorRec.A / 255]);
            delete(LDataString,P1, P2-P1);
            insert(LNewColorsStr, LDataString, P1);
          end;
          P1 := ALPosA('{"ty":"fl","c":{"a":0,"k":[', LDataString, P1);
        end;
        TALStringStreamA(LStream).DataString := LDataString;
      except
        ALFreeAndNil(LStream);
        Raise;
      end;
    end
    else LStream := ALCreateResourceStream(FResourceName);
    try

      var LSkStream := ALSkCheckHandle(sk4d_streamadapter_create(LStream));
      try
        var LStreamadapterProcs: sk_streamadapter_procs_t;
        LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
        LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
        LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
        LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
        sk4d_streamadapter_set_procs(@LStreamadapterProcs);
        fSkottieAnimation := sk4d_skottieanimation_make_from_stream(
                               LSkStream, // stream: sk_stream_t
                               0{TSkDefaultProviders.Resource.Handle}, // resource_provider: sk_resourceprovider_t;
                               TSkDefaultProviders.TypefaceFont.Handle);  // font_provider: sk_fontmgr_t
      finally
        sk4d_streamadapter_destroy(LSKStream);
      end;
    finally
      ALfreeandNil(LStream);
    end;
    {$ENDIF}
  end;

  if fSkottieAnimation = 0 then begin
    if LFileName <> '' then begin
      fAnimCodecPlayer := sk4d_animcodecplayer_make_from_file(MarshaledAString(UTF8String(LFileName)))
    end
    else begin
      {$IFDEF ALDPK}
      fAnimCodecPlayer := 0
      {$ELSE}
      var LResourceStream := ALCreateResourceStream(FResourceName);
      try
        var LSkStream := ALSkCheckHandle(sk4d_streamadapter_create(LResourceStream));
        try
          var LStreamadapterProcs: sk_streamadapter_procs_t;
          LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
          LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
          LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
          LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
          sk4d_streamadapter_set_procs(@LStreamadapterProcs);
          fAnimCodecPlayer := sk4d_animcodecplayer_make_from_stream(LSkStream);
        finally
          sk4d_streamadapter_destroy(LSKStream);
        end;
      finally
        ALfreeandNil(LResourceStream);
      end;
      {$ENDIF}
    end;
  end;

  if (fSkottieAnimation = 0) and (fAnimCodecPlayer = 0) then
    {$IF not defined(ALDPK)}
    Raise Exception.CreateFmt('Failed to create the animation codec for resource "%s". Please ensure the resource exists and is in a valid format', [FResourceName]);
    {$ELSE}
    Exit;
    {$ENDIF}

  var LSize: TSizeF;
  if fSkottieAnimation <> 0 then begin
    var LSizeT: sk_size_t;
    sk4d_skottieanimation_get_size(fSkottieAnimation, LSizeT);
    LSize.Width := LSizeT.width;
    LSize.Height := LSizeT.height;
  end
  else begin
    var LIsizeT: sk_isize_t;
    sk4d_animcodecplayer_get_dimensions(fAnimCodecPlayer, LIsizeT);
    LSize.Width := LIsizeT.width;
    LSize.Height := LIsizeT.height;
  end;
  if SameValue(LSize.width, 0, Tepsilon.Position) or
     SameValue(LSize.Height, 0, Tepsilon.Position) then begin
    {$IF not defined(ALDPK)}
    Raise Exception.CreateFmt('The animation "%s" has invalid dimensions (width or height is zero)', [FResourceName]);
    {$ELSE}
    ReleaseCodec;
    Exit;
    {$ENDIF}
  end;
  FRenderRect := TRectF.Create(0,0,LSize.width, LSize.height);

  case FWrapMode of
    //TALImageWrapMode.Original: FRenderRect := FRenderRect.PlaceInto(LocalRect);
    TALImageWrapMode.Fit: FRenderRect := FRenderRect.FitInto(LocalRect);
    TALImageWrapMode.Stretch: FRenderRect := FRenderRect.FitInto(LocalRect); // TALImageWrapMode.Stretch not yet supported, use TALImageWrapMode.Fit instead
    TALImageWrapMode.Place: FRenderRect := FRenderRect.PlaceInto(LocalRect);
    TALImageWrapMode.FitAndCrop: FRenderRect := FRenderRect.FitInto(LocalRect); // TALImageWrapMode.FitAndCrop not yet supported, use TALImageWrapMode.Fit instead
    else
      Raise Exception.Create('Error 822CE359-8404-40CE-91B9-1CFC3DBA259F')
  end;
  FRenderRect := ALAlignDimensionToPixelRound(FRenderRect, ALGetScreenScale); // to have the pixel aligned width and height

  {$IF not defined(ALSkiaCanvas)}
  var LBufRect: Trect;
  if fSkottieAnimation <> 0 then LBufRect := TRectf.Create(0,0,FRenderRect.width * ALGetScreenScale, FRenderRect.height * ALGetScreenScale).Round
  else LBufRect := TRect.Create(0,0,Round(LSize.width), round(LSize.height));
  FBufSurface := ALCreateSkSurface(
                   ALCeil(LBufRect.width, TEpsilon.Position),
                   ALCeil(LBufRect.height, TEpsilon.Position));
  FbufCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(FBufSurface));
  {$IF defined(ALGPUCanvas)}
  FbufTexture := TALTexture.Create;
  FbufTexture.Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
  FbufTexture.SetSize(LBufRect.width, LBufRect.height);
  FbufTexture.PixelFormat := ALGetDefaultPixelFormat;
  {$ELSE}
  FBufBitmap := Tbitmap.create(LBufRect.width, LBufRect.height);
  {$ENDIF}
  {$ENDIF}

  var LDuration: Single;
  if fSkottieAnimation <> 0 then
    LDuration := Single(sk4d_skottieanimation_get_duration(fSkottieAnimation))
  else
    LDuration := Single(sk4d_animcodecplayer_get_duration(fAnimCodecPlayer) / 1000);
  FAnimation.SetDuration(LDuration);

  {$IFDEF debug}
  ALLog(
    'TALAnimatedImage.CreateCodec',
    'ResourceName: '+ FResourceName + ' | '+
    'Duration: '+ALFloatTostrW(LDuration) + ' | '+
    'Width: ' + ALFloatTostrW(LSize.Width) + ' | '+
    'Height: ' + ALFloatTostrW(LSize.Height),
    TalLogType.debug);
  {$ENDIF}

  {$ENDIF}
end;

{**************************************}
procedure TALAnimatedImage.ReleaseCodec;
begin
  {$IF defined(ALSkiaAvailable)}
    if fSkottieAnimation <> 0 then begin
      sk4d_skottieanimation_unref(fSkottieAnimation);
      fSkottieAnimation := 0;
    end;
    if fAnimCodecPlayer <> 0 then begin
      sk4d_animcodecplayer_destroy(fAnimCodecPlayer);
      fAnimCodecPlayer := 0;
    end;
    FAnimation.SetDuration(0.0);
    {$IF (not defined(ALSkiaCanvas))}
      FBufCanvas := 0; // ACanvas is linked inside ASurface
      if FBufSurface <> 0 then begin
        sk4d_refcnt_unref(FBufSurface);
        FBufSurface := 0;
      end;
      {$IF defined(ALGPUCanvas)}
      ALFreeAndNil(FbufTexture);
      {$ELSE}
      ALFreeAndNil(FBufBitmap);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

{***********************************}
procedure TALAnimatedImage.DoResized;
begin
  releaseCodec;
  inherited;
end;

{*******************************}
procedure TALAnimatedImage.Paint;
begin

  if FAnimation.Enabled then begin
    if not TALFloatAnimation(FAnimation).Enabled then
      TALFloatAnimation(FAnimation).Enabled := True
    else
      FAnimation.Resume;
  end;

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    var R := LocalRect;
    system.types.InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  CreateCodec;

  {$IF defined(ALSkiaAvailable)}
  if fSkottieAnimation <> 0 then begin
    sk4d_skottieanimation_seek_frame_time(fSkottieAnimation, fAnimation.CurrentTime);
    {$IF defined(ALSkiaCanvas)}
    var LNeedSaveLayer := not SameValue(AbsoluteOpacity, 1, TEpsilon.Position);
    if LNeedSaveLayer then
      sk4d_canvas_save_layer_alpha(TSkCanvasCustom(Canvas).Canvas.Handle, nil, Round(AbsoluteOpacity * 255));
    try
      sk4d_skottieanimation_render(
        fSkottieAnimation, // const self: sk_skottieanimation_t;
        TSkCanvasCustom(Canvas).Canvas.Handle, // canvas: sk_canvas_t;
        @FRenderRect, // const dest: psk_rect_t;
        0); // render_flags: uint32_t); cdecl;
    finally
      if LNeedSaveLayer then
        sk4d_canvas_restore(TSkCanvasCustom(Canvas).Canvas.Handle);
    end;
    {$ELSEIF defined(ALGPUCanvas)}
    var LRect := FRenderRect;
    Lrect.Width := Lrect.Width * ALGetScreenScale;
    Lrect.Height := Lrect.Height * ALGetScreenScale;
    LRect.Offset(-LRect.Left,-LRect.Top);
    sk4d_canvas_clear(FBufCanvas, TAlphaColors.Null);
    sk4d_skottieanimation_render(
      fSkottieAnimation, // const self: sk_skottieanimation_t;
      FBufCanvas, // canvas: sk_canvas_t;
      @LRect, // const dest: psk_rect_t;
      0); // render_flags: uint32_t); cdecl;
    ALUpdateTextureFromSkSurface(FBufSurface, FBufTexture);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FBufTexture, // const ADrawable: TALDrawable;
      FRenderRect.TopLeft, // const ADstTopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single)
    {$ELSE}
    var LRect := FRenderRect;
    Lrect.Width := Lrect.Width * ALGetScreenScale;
    Lrect.Height := Lrect.Height * ALGetScreenScale;
    LRect.Offset(-LRect.Left,-LRect.Top);
    sk4d_canvas_clear(FBufCanvas, TAlphaColors.Null);
    sk4d_skottieanimation_render(
      fSkottieAnimation, // const self: sk_skottieanimation_t;
      FBufCanvas, // canvas: sk_canvas_t;
      @LRect, // const dest: psk_rect_t;
      0); // render_flags: uint32_t); cdecl;
    ALUpdateTBitmapFromSkSurface(FBufSurface, FBufBitmap);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FBufBitmap, // const ADrawable: TALDrawable;
      FRenderRect.TopLeft, // const ADstTopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single)
    {$ENDIF}
  end
  else if fAnimcodecplayer <> 0 then Begin
    sk4d_animcodecplayer_seek(fAnimcodecplayer, round(fAnimation.CurrentTime * 1000));
    {$IF defined(ALSkiaCanvas)}
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      sk4d_animcodecplayer_get_frame(fAnimcodecplayer), // const ADrawable: TALDrawable;
      FRenderRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)
    {$ELSEIF defined(ALGPUCanvas)}
    ALUpdateTextureFromSkImage(sk4d_animcodecplayer_get_frame(fAnimcodecplayer), FBufTexture);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FBufTexture, // const ADrawable: TALDrawable;
      FRenderRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)
    {$ELSE}
    ALUpdateTBitmapFromSkImage(sk4d_animcodecplayer_get_frame(fAnimcodecplayer), FBufBitmap);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FBufBitmap, // const ADrawable: TALDrawable;
      FRenderRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)
    {$ENDIF}
  end;
  {$ELSE}
  ALLog(
    'TALAnimatedImage.Paint',
    '''
      The Skia Engine is required to use AnimatedImage.
      First, enable Skia for the project. Then, if you prefer not to
      use a Skia canvas for the main form, go to the project options
      and replace the "SKIA" definition with "ALSkiaAvailable." Additionally,
      add the line "GlobalUseSkia := False" to the DPR file.
    ''',
    TALLogType.ERROR);
  {$ENDIF}

end;

{*********************************************************}
function TALAnimatedImage.GetDefaultTintColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{*******************************************************}
function TALAnimatedImage.GetDefaulttintColorKey: String;
begin
  Result := '';
end;

{********************************************************************}
procedure TALAnimatedImage.SetWrapMode(const Value: TALImageWrapMode);
begin
  if FWrapMode <> Value then begin
    releaseCodec;
    FWrapMode := Value;
    Repaint;
  end;
end;

{**************************************************************}
procedure TALAnimatedImage.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    releaseCodec;
    FResourceName := Value;
    Repaint;
  end;
end;

{***************************************************************}
procedure TALAnimatedImage.SetAnimation(const Value: TAnimation);
begin
  FAnimation.Assign(Value);
end;

{****************************************************************}
procedure TALAnimatedImage.setTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then begin
    FTintColor := Value;
    FTintColorKey := '';
    releaseCodec;
    Repaint;
  end;
end;

{**************************************************************}
procedure TALAnimatedImage.setTintColorKey(const Value: String);
begin
  if FTintColorKey <> Value then begin
    FTintColorKey := Value;
    ApplyTintColorScheme;
  end;
end;

{***************************************************}
function TALAnimatedImage.IsTintColorStored: Boolean;
begin
  Result := FTintColor <> DefaultTintColor;
end;

{******************************************************}
function TALAnimatedImage.IsTintColorKeyStored: Boolean;
begin
  Result := FTintColorKey <> DefaultTintColorKey;
end;

{******************************************************}
constructor TALBaseRectangle.Create(AOwner: TComponent);
begin
  inherited;
  fDoubleBuffered := true;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FCorners := DefaultCorners;
  FSides := DefaultSides;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  fBufDrawable := ALNullDrawable;
end;

{**********************************}
destructor TALBaseRectangle.Destroy;
begin
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{*****************************************************************}
procedure TALBaseRectangle.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALBaseRectangle then begin
      XRadius := TALBaseRectangle(Source).XRadius;
      YRadius := TALBaseRectangle(Source).YRadius;
      Corners := TALBaseRectangle(Source).Corners;
      Sides := TALBaseRectangle(Source).Sides;
      CacheIndex := TALBaseRectangle(Source).CacheIndex;
      CacheEngine := TALBaseRectangle(Source).CacheEngine;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{***********************************}
procedure TALBaseRectangle.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{******************************************}
procedure TALBaseRectangle.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*************************************************}
function TALBaseRectangle.IsCornersStored: Boolean;
begin
  Result := FCorners <> DefaultCorners;
end;

{***********************************************}
function TALBaseRectangle.IsSidesStored: Boolean;
begin
  Result := FSides <> DefaultSides;
end;

{*************************************************}
function TALBaseRectangle.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{*************************************************}
function TALBaseRectangle.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{***********************************************}
function TALBaseRectangle.HasCustomDraw: Boolean;
begin
  Result := False;
end;

{**************************************************}
function TALBaseRectangle.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{***************************************************}
function TALBaseRectangle.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{******************************************************************}
procedure TALBaseRectangle.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{****************************************************}
function TALBaseRectangle.GetDefaultCorners: TCorners;
begin
  Result := AllCorners;
end;

{************************************************}
function TALBaseRectangle.GetDefaultSides: TSides;
begin
  Result := AllSides;
end;

{**************************************************}
function TALBaseRectangle.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{**************************************************}
function TALBaseRectangle.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{*********************************************************}
procedure TALBaseRectangle.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{*********************************************************}
procedure TALBaseRectangle.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{***********************************************************}
procedure TALBaseRectangle.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    ClearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{*******************************************************}
procedure TALBaseRectangle.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    ClearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{******************************************************}
procedure TALBaseRectangle.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALBaseRectangle.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALBaseRectangle.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{********************************************************}
function TALBaseRectangle.IsSimpleRenderPossible: Boolean;
begin
  Result := (not HasCustomDraw)
            and
            ((not Stroke.HasStroke) or
             (sides = []))
            and
            ((SameValue(xRadius, 0, TEpsilon.Vector)) or
             (SameValue(yRadius, 0, TEpsilon.Vector)) or
             (corners=[]))
            and
            (not Shadow.HasShadow)
            and
            ((not Fill.HasFill) or
             (Fill.Styles = [TALBrushStyle.solid]));
end;

{*******************************************}
Procedure TALBaseRectangle.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AutoAlignToPixel, // const AAlignToPixel: Boolean;
                        AFill, // const AFill: TALBrush;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, AStateLayerContentColor)
        .SetDrawStateLayerOnTop(ADrawStateLayerOnTop)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetSides(Sides)
        .SetCorners(Corners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{*****************************************}
procedure TALBaseRectangle.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     {$IF not DEFINED(ALDPK)}(not DoubleBuffered) or{$ENDIF}
     //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if only fill with solid color
     (IsSimpleRenderPossible) then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    ALGetScreenScale, // const AScale: Single;
    Fill, // const AFill: TALBrush;
    nil, // const AStateLayer: TALStateLayer;
    TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
    True, // const ADrawStateLayerOnTop: Boolean;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALBaseRectangle.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  Result := ALGetShapeSurfaceRect(
              ARect, // const ARect: TRectF;
              AutoAlignToPixel, // const AAlignToPixel: Boolean;
              Fill, // const AFill: TALBrush;
              nil, // const AStateLayer: TALStateLayer;
              Shadow); // const AShadow: TALShadow): TRectF;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALBaseRectangle.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALBaseRectangle.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{*******************************}
procedure TALBaseRectangle.Paint;
begin

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
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(LocalRect)
      .SetOpacity(AbsoluteOpacity)
      .SetFill(Fill)
      .SetStroke(Stroke)
      .SetShadow(Shadow)
      .SetSides(Sides)
      .SetCorners(Corners)
      .SetXRadius(XRadius)
      .SetYRadius(YRadius)
      .Draw;
    {$ELSE}
    if IsSimpleRenderPossible then begin
      If Fill.Styles = [TALBrushStyle.Solid] then begin
        Canvas.Fill.kind := TBrushKind.solid;
        Canvas.Fill.color := Fill.color;
        Canvas.FillRect(ALAlignToPixelRound(LocalRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position), XRadius, YRadius, FCorners, AbsoluteOpacity, TCornerType.Round);
      end;
    end
    else begin
      var LRect := LocalRect;
      InitRenderTargets(LRect);
      if ALCanvasBeginScene(FRenderTargetCanvas) then
      try
        ALClearCanvas(FRenderTargetCanvas, TAlphaColors.Null);
        TALDrawRectangleHelper.Create(FRenderTargetCanvas)
          .SetScale(ALGetScreenScale)
          .SetAlignToPixel(AutoAlignToPixel)
          .SetDstRect(LRect)
          .SetFill(Fill)
          .SetStroke(Stroke)
          .SetShadow(Shadow)
          .SetSides(Sides)
          .SetCorners(Corners)
          .SetXRadius(XRadius)
          .SetYRadius(YRadius)
          .Draw;
      finally
        ALCanvasEndScene(FRenderTargetCanvas)
      end;
      ALUpdateDrawableFromSurface(FRenderTargetSurface, FRenderTargetDrawable);
      // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
      // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
      LRect.Offset(-2*LRect.Left, -2*LRect.Top);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        FRenderTargetDrawable, // const ADrawable: TALDrawable;
        LRect.TopLeft, // const ADstTopLeft: TpointF;
        AbsoluteOpacity); // const AOpacity: Single)
    end;
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{************************************************}
constructor TALEllipse.Create(AOwner: TComponent);
begin
  inherited;
  fDoubleBuffered := true;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  fBufDrawable := ALNullDrawable;
end;

{****************************}
destructor TALEllipse.Destroy;
begin
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{************************************}
procedure TALEllipse.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{********************************************}
function TALEllipse.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{*********************************************}
function TALEllipse.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{************************************************************}
procedure TALEllipse.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{************************************************}
procedure TALEllipse.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**************************************************}
procedure TALEllipse.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**************************************************}
procedure TALEllipse.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*************************************}
Procedure TALEllipse.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AutoAlignToPixel, // const AAlignToPixel: Boolean;
                        AFill, // const AFill: TALBrush;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.Height); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, AStateLayerContentColor)
        .SetDrawStateLayerOnTop(ADrawStateLayerOnTop)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetXRadius(ABufDrawableRect.Width / 2)
        .SetYRadius(ABufDrawableRect.Height / 2)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{***********************************}
procedure TALEllipse.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     {$IF not DEFINED(ALDPK)}(not DoubleBuffered) or{$ENDIF}
     //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    ALGetScreenScale, // const AScale: Single;
    Fill, // const AFill: TALBrush;
    nil, // const AStateLayer: TALStateLayer;
    TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
    True, // const ADrawStateLayerOnTop: Boolean;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALEllipse.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  Result := ALGetShapeSurfaceRect(
              ARect, // const ARect: TRectF;
              AutoAlignToPixel, // const AAlignToPixel: Boolean;
              Fill, // const AFill: TALBrush;
              nil, // const AStateLayer: TALStateLayer;
              Shadow); // const AShadow: TALShadow): TRectF;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALEllipse.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALEllipse.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{*************************}
procedure TALEllipse.Paint;
begin

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
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(LocalRect)
      .SetOpacity(AbsoluteOpacity)
      .SetFill(Fill)
      .SetStroke(Stroke)
      .SetShadow(Shadow)
      .SetXRadius(Width / 2)
      .SetYRadius(Height / 2)
      .Draw;
    {$ELSE}
    var LRect := LocalRect;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(FRenderTargetCanvas) then
    try
      ALClearCanvas(FRenderTargetCanvas, TAlphaColors.Null);
      TALDrawRectangleHelper.Create(FRenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(LRect)
        .SetFill(Fill)
        .SetStroke(Stroke)
        .SetShadow(Shadow)
        .SetXRadius(Width / 2)
        .SetYRadius(Height / 2)
        .Draw;
    finally
      ALCanvasEndScene(FRenderTargetCanvas)
    end;
    ALUpdateDrawableFromSurface(FRenderTargetSurface, FRenderTargetDrawable);
    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FRenderTargetDrawable, // const ADrawable: TALDrawable;
      LRect.TopLeft, // const ADstTopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single)
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{************************************************************}
function TALEllipse.PointInObjectLocal(X, Y: Single): Boolean;
begin
  Result := False;
  if Width * Height = 0 then Exit;
  var LShapeRect := LocalRect;
  var LRadius := TSizeF.Create(LShapeRect.Width / 2, LShapeRect.Height / 2);
  var LCenter := LShapeRect.CenterPoint;
  Result := Sqr((X - LCenter.X) / LRadius.Width) + Sqr((Y - LCenter.Y) / LRadius.Height) <= 1;
end;

{*****************************}
procedure TALEllipse.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{***********************************************}
constructor TALCircle.Create(AOwner: TComponent);
begin
  inherited;
  fDoubleBuffered := true;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  fBufDrawable := ALNullDrawable;
end;

{***************************}
destructor TALCircle.Destroy;
begin
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{***********************************}
procedure TALCircle.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*******************************************}
function TALCircle.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{********************************************}
function TALCircle.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{***********************************************************}
procedure TALCircle.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{***********************************************}
procedure TALCircle.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*************************************************}
procedure TALCircle.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*************************************************}
procedure TALCircle.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{************************************}
Procedure TALCircle.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AutoAlignToPixel, // const AAlignToPixel: Boolean;
                        AFill, // const AFill: TALBrush;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.Height); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(ABufDrawableRect))
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, AStateLayerContentColor)
        .SetDrawStateLayerOnTop(ADrawStateLayerOnTop)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetXRadius(-50)
        .SetYRadius(-50)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{**********************************}
procedure TALCircle.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     {$IF not DEFINED(ALDPK)}(not DoubleBuffered) or{$ENDIF}
     //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    ALGetScreenScale, // const AScale: Single;
    Fill, // const AFill: TALBrush;
    nil, // const AStateLayer: TALStateLayer;
    TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
    True, // const ADrawStateLayerOnTop: Boolean;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALCircle.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  Result := ALGetShapeSurfaceRect(
              ARect, // const ARect: TRectF;
              AutoAlignToPixel, // const AAlignToPixel: Boolean;
              Fill, // const AFill: TALBrush;
              nil, // const AStateLayer: TALStateLayer;
              Shadow); // const AShadow: TALShadow): TRectF;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALCircle.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALCircle.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{************************}
procedure TALCircle.Paint;
begin

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
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect))
      .SetOpacity(AbsoluteOpacity)
      .SetFill(Fill)
      .SetStroke(Stroke)
      .SetShadow(Shadow)
      .SetXRadius(-50)
      .SetYRadius(-50)
      .Draw;
    {$ELSE}
    var LRect := LocalRect;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(FRenderTargetCanvas) then
    try
      ALClearCanvas(FRenderTargetCanvas, TAlphaColors.Null);
      TALDrawRectangleHelper.Create(FRenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(LRect))
        .SetFill(Fill)
        .SetStroke(Stroke)
        .SetShadow(Shadow)
        .SetXRadius(-50)
        .SetYRadius(-50)
        .Draw;
    finally
      ALCanvasEndScene(FRenderTargetCanvas)
    end;
    ALUpdateDrawableFromSurface(FRenderTargetSurface, FRenderTargetDrawable);
    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FRenderTargetDrawable, // const ADrawable: TALDrawable;
      LRect.TopLeft, // const ADstTopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single)
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***********************************************************}
function TALCircle.PointInObjectLocal(X, Y: Single): Boolean;
begin
  Result := False;
  if Width * Height = 0 then Exit;
  var LShapeRect := TRectF.Create(0, 0, 1, 1).FitInto(LocalRect);
  var LRadius := TSizeF.Create(LShapeRect.Width / 2, LShapeRect.Height / 2);
  var LCenter := LShapeRect.CenterPoint;
  Result := Sqr((X - LCenter.X) / LRadius.Width) + Sqr((Y - LCenter.Y) / LRadius.Height) <= 1;
end;

{****************************}
procedure TALCircle.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{*********************************************}
constructor TALLine.Create(AOwner: TComponent);
begin
  inherited;
  fDoubleBuffered := true;
  FLineType := TALLineType.TopLeftToBottomRight;
  FCacheIndex := 0;
  FCacheEngine := nil;
  fBufDrawable := ALNullDrawable;
end;

{*********************************}
procedure TALLine.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*****************************************}
function TALLine.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{******************************************}
function TALLine.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{*********************************************************}
procedure TALLine.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable;
  end;
end;

{*********************************************}
procedure TALLine.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{***********************************************}
procedure TALLine.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{********************************}
procedure TALLine.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     {$IF not DEFINED(ALDPK)}(not DoubleBuffered) or{$ENDIF}
     //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if linetype <> TALLineType.Diagonal
     (not (lineType in [TALLineType.TopLeftToBottomRight, TALLineType.BottomLeftToTopRight])) or
     //--- // Do not create BufDrawable if no stroke
     (Not Stroke.HasStroke) then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
  {$endif}

  //init fBufDrawableRect / LRect
  case lineType of
    TALLineType.TopLeftToBottomRight,
    TALLineType.BottomLeftToTopRight: fBufDrawableRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
    TALLineType.Top: fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, Stroke.Thickness), ALGetScreenScale); // to have the pixel aligned width and height
    TALLineType.Left: fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Stroke.Thickness, height), ALGetScreenScale); // to have the pixel aligned width and height
    TALLineType.Bottom: fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - Stroke.Thickness, Width, height), ALGetScreenScale); // to have the pixel aligned width and height
    TALLineType.Right: fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(width - Stroke.Thickness, 0, width, height), ALGetScreenScale); // to have the pixel aligned width and height
    else
      raise Exception.Create('Error C251AE86-B150-43E8-80DE-A6D96F085AF2');
  end;
  var LRect := TrectF.Create(0, 0, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.height * ALGetScreenScale);

  {$IF DEFINED(ALSkiaEngine)}

  var LSurface: sk_surface_t;
  var LCanvas: sk_canvas_t;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALCeil(LRect.Width, TEpsilon.Position), // const w: integer;
    ALCeil(LRect.Height, TEpsilon.Position)); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      var LPaint := ALSkCheckHandle(sk4d_paint_create);
      try
        sk4d_paint_set_antialias(LPaint, true);
        sk4d_paint_set_dither(LPaint, true);

        //stroke the line
        if Stroke.HasStroke then begin

          //init LPaint
          sk4d_paint_set_stroke_width(LPaint, Stroke.Thickness * ALGetScreenScale);

          //stroke with solid color
          sk4d_paint_set_color(LPaint, Stroke.Color);
          case lineType of
            TALLineType.TopLeftToBottomRight: begin
              var Lpoint1 := TPointF.Create(LRect.left, LRect.top);
              var Lpoint2 := TPointF.Create(LRect.right, LRect.Bottom);
              sk4d_canvas_draw_line(
                LCanvas,
                @Lpoint1,
                @Lpoint2,
                LPaint);
            end;
            //--
            TALLineType.BottomLeftToTopRight: begin
              var Lpoint1 := TPointF.Create(LRect.left, LRect.Bottom);
              var Lpoint2 := TPointF.Create(LRect.right, LRect.top);
              sk4d_canvas_draw_line(
                LCanvas,
                @Lpoint1,
                @Lpoint2,
                LPaint);
            end;
            //--
            TALLineType.Top,
            TALLineType.Bottom: begin
              var Lpoint1 := TPointF.Create(LRect.left, (LRect.bottom - LRect.top) / 2);
              var Lpoint2 := TPointF.Create(LRect.right, (LRect.bottom - LRect.top) / 2);
              sk4d_canvas_draw_line(
                LCanvas,
                @Lpoint1,
                @Lpoint2,
                LPaint);
            end;
            //--
            TALLineType.Left,
            TALLineType.Right: begin
              var Lpoint1 := TPointF.Create((LRect.right - LRect.left) / 2, LRect.top);
              var Lpoint2 := TPointF.Create((LRect.right - LRect.left) / 2, LRect.bottom);
              sk4d_canvas_draw_line(
                LCanvas,
                @Lpoint1,
                @Lpoint2,
                LPaint);
            end;
          end;

        end;

      finally
        sk4d_paint_destroy(LPaint);
      end;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    fBufDrawable := ALCreateDrawableFromSurface(LSurface);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

  {$ELSEIF DEFINED(ANDROID)}

  var LSurface: Jbitmap;
  var LCanvas: Jcanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALCeil(LRect.Width, TEpsilon.Position), // const w: integer;
    ALCeil(LRect.Height, TEpsilon.Position)); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      var LPaint := TJPaint.JavaClass.init;
      LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

      //stroke the line
      if Stroke.HasStroke then begin

        //init LPaint
        LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
        LPaint.setStrokeWidth(Stroke.Thickness * ALGetScreenScale);

        //stroke with solid color
        LPaint.setColor(integer(Stroke.Color));
        case lineType of
          TALLineType.TopLeftToBottomRight: LCanvas.drawLine(
                                              LRect.left {startX},
                                              LRect.top {startY},
                                              LRect.right {stopX},
                                              LRect.Bottom {stopY},
                                              LPaint);
          TALLineType.BottomLeftToTopRight: LCanvas.drawLine(
                                              LRect.left {startX},
                                              LRect.Bottom {startY},
                                              LRect.right {stopX},
                                              LRect.Top {stopY},
                                              LPaint);
          TALLineType.Top,
          TALLineType.Bottom: LCanvas.drawLine(
                                LRect.left {startX},
                                (LRect.bottom - LRect.top) / 2 {startY},
                                LRect.right {stopX},
                                (LRect.bottom - LRect.top) / 2 {stopY},
                                LPaint);
          TALLineType.Left,
          TALLineType.Right: LCanvas.drawLine(
                               (LRect.right - LRect.left) / 2 {startX},
                               LRect.top {startY},
                               (LRect.right - LRect.left) / 2 {stopX},
                               LRect.bottom {stopY},
                               LPaint);
        end;

      end;

      //free the paint
      LPaint := nil;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    fBufDrawable := ALCreateDrawableFromSurface(LSurface);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

  {$ELSEIF DEFINED(IOS)}

  var LGridHeight := ALCeil(LRect.Height, TEpsilon.Position);
  var LSurface: CGContextRef;
  var LCanvas: CGContextRef;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALCeil(LRect.Width, TEpsilon.Position), // const w: integer;
    LGridHeight); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      //stroke the line
      if Stroke.HasStroke then begin

        //stroke with solid color
        var LAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
        CGContextSetRGBStrokeColor(LCanvas, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
        CGContextSetLineWidth(LCanvas, Stroke.Thickness * ALGetScreenScale);
        case lineType of
          TALLineType.TopLeftToBottomRight: begin
                                              CGContextBeginPath(LCanvas);
                                              CGContextMoveToPoint(LCanvas, LRect.left, LGridHeight - LRect.top);
                                              CGContextAddLineToPoint(LCanvas, LRect.right, LGridHeight - LRect.Bottom);
                                            end;
          TALLineType.BottomLeftToTopRight: begin
                                              CGContextBeginPath(LCanvas);
                                              CGContextMoveToPoint(LCanvas, LRect.left, LGridHeight - LRect.Bottom);
                                              CGContextAddLineToPoint(LCanvas, LRect.right, LGridHeight - LRect.top);
                                            end;
          TALLineType.Top,
          TALLineType.Bottom: begin
                              CGContextBeginPath(LCanvas);
                              CGContextMoveToPoint(LCanvas, LRect.left, LGridHeight - ((LRect.bottom - LRect.top) / 2));
                              CGContextAddLineToPoint(LCanvas, LRect.right, LGridHeight - ((LRect.bottom - LRect.top) / 2));
                            end;
          TALLineType.Left,
          TALLineType.Right: begin
                             CGContextBeginPath(LCanvas);
                             CGContextMoveToPoint(LCanvas, (LRect.right - LRect.left) / 2, LGridHeight - LRect.top);
                             CGContextAddLineToPoint(LCanvas, (LRect.right - LRect.left) / 2, LGridHeight - LRect.Bottom);
                           end;
        end;
        CGContextStrokePath(LCanvas);

      end;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    fBufDrawable := ALCreateDrawableFromSurface(LCanvas);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas); // Var aContext: CGContextRef;
  end;

  {$ENDIF}

end;

{**********************}
procedure TALLine.Paint;
begin

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
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) and Stroke.HasStroke then begin
    var LPt1, LPt2: TPointF;
    case lineType of
      TALLineType.TopLeftToBottomRight: begin
                                          LPt1 := TpointF.Create(0,     0);
                                          LPt2 := TpointF.Create(Width, Height);
                                        end;
      TALLineType.BottomLeftToTopRight: begin
                                          LPt1 := TpointF.Create(0,     Height);
                                          LPt2 := TpointF.Create(Width, 0);
                                        end;
      TALLineType.Top: begin
                       LPt1 := TpointF.Create((Stroke.Thickness / 2),         Stroke.Thickness / 2);
                       LPt2 := TpointF.Create(Width - (Stroke.Thickness / 2), Stroke.Thickness / 2);
                     end;
      TALLineType.Left: begin
                        LPt1 := TpointF.Create(Stroke.Thickness / 2, Stroke.Thickness / 2);
                        LPt2 := TpointF.Create(Stroke.Thickness / 2, height-(Stroke.Thickness / 2));
                      end;
      TALLineType.Bottom: begin
                          LPt1 := TpointF.Create((Stroke.Thickness / 2),         height-(Stroke.Thickness / 2));
                          LPt2 := TpointF.Create(Width - (Stroke.Thickness / 2), height-(Stroke.Thickness / 2));
                        end;
      TALLineType.Right: begin
                         LPt1 := TpointF.Create(Width-(Stroke.Thickness / 2), Stroke.Thickness / 2);
                         LPt2 := TpointF.Create(Width-(Stroke.Thickness / 2), height-(Stroke.Thickness / 2));
                       end;
      else
        raise Exception.Create('Error E353EE34-4D44-4487-9C1C-21BC44E36B40');
    end;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := Stroke.Color;
    Canvas.Stroke.Thickness := Stroke.Thickness;
    Canvas.DrawLine(
      ALAlignToPixelRound(LPt1, Canvas.Matrix, Canvas.Scale, TEpsilon.position),
      ALAlignToPixelRound(LPt2, Canvas.Matrix, Canvas.Scale, TEpsilon.position),
      AbsoluteOpacity);
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{**************************}
procedure TALLine.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{******************************************************}
procedure TALLine.SetLineType(const Value: TALLineType);
begin
  if FLineType <> Value then
  begin
    ClearBufDrawable;
    FLineType := Value;
    Repaint;
  end;
end;

{******************************************************}
function TALBaseText.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{********************************************************}
function TALBaseText.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*************************************************}
constructor TALBaseText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //-----
  fDoubleBuffered := true;
  FMultiLineTextOptions := TALMultiLineTextOptions.Create;
  //-----
  FMaxContainedSize := TSizeF.Create(-1,-1);
  //-----
  FOnElementClick := nil;
  FOnElementMouseDown := nil;
  FOnElementMouseMove := nil;
  FOnElementMouseUp := nil;
  FOnElementMouseEnter := nil;
  FOnElementMouseLeave := nil;
  // No need to do this, as it was already initialized via InitInstance.
  // It's a slightly expensive operation.
  //FHoveredElement := TALTextElement.Empty;
  //FPressedElement := TALTextElement.Empty;
  //-----
  FCacheIndex := 0;
  FCacheEngine := nil;
  //-----
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  fBufDrawable := ALNullDrawable;
  //-----
  fTextBroken := False;
  fAllTextDrawn := False;
  SetLength(fElements, 0);
  //-----
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FCorners := DefaultCorners;
  FSides := DefaultSides;
  //-----
  HitTest := False;
  //-----
  FAutoTranslate := true;
  FMaxWidth := DefaultMaxWidth;
  FMaxHeight := DefaultMaxHeight;
  FText := '';
  //-----
  FTextSettings := CreateTextSettings;
  FTextSettings.OnChanged := TextSettingsChanged;
  //-----
  FAutosize := TALAutoSizeMode.Both;
end;

{*****************************}
destructor TALBaseText.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FMultiLineTextOptions);
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{****************************************}
function TALBaseText.CreateFill: TALBrush;
begin
  result := TFill.Create;
end;

{************************************************}
function TALBaseText.CreateStroke: TALStrokeBrush;
begin
  result := TStroke.Create;
end;

{***************************}
procedure TALBaseText.Loaded;
begin
  {$IF defined(ALBackwardCompatible)}
  if ALSametextW(TextSettings.Font.Family, 'sans-serif-thin') then begin
    TextSettings.Font.Family := 'sans-serif';
    if TextSettings.Font.Weight = TFontWeight.Bold then TextSettings.Font.Weight := TFontWeight.regular
    else TextSettings.Font.Weight := TFontWeight.thin;
  end
  else if ALSametextW(TextSettings.Font.Family, 'sans-serif-light') then begin
    TextSettings.Font.Family := 'sans-serif';
    if TextSettings.Font.Weight = TFontWeight.Bold then TextSettings.Font.Weight := TFontWeight.Semibold
    else TextSettings.Font.Weight := TFontWeight.light;
  end
  else if ALSametextW(TextSettings.Font.Family, 'sans-serif-medium') then begin
    TextSettings.Font.Family := 'sans-serif';
    if TextSettings.Font.Weight = TFontWeight.Bold then TextSettings.Font.Weight := TFontWeight.UltraBold
    else TextSettings.Font.Weight := TFontWeight.medium;
  end
  else if ALSametextW(TextSettings.Font.Family, 'sans-serif-black') then begin
    TextSettings.Font.Family := 'sans-serif';
    if TextSettings.Font.Weight = TFontWeight.Bold then TextSettings.Font.Weight := TFontWeight.UltraBlack
    else TextSettings.Font.Weight := TFontWeight.black;
  end;
  {$ENDIF}

  if (AutoTranslate) and
     (Text <> '') and
     (not (csDesigning in ComponentState)) then
    Text := ALTranslate(Text);

  inherited Loaded;
end;

{************************************************************}
procedure TALBaseText.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALBaseText then begin
      OnElementClick := TALBaseText(Source).OnElementClick;
      OnElementMouseDown := TALBaseText(Source).OnElementMouseDown;
      OnElementMouseMove := TALBaseText(Source).OnElementMouseMove;
      OnElementMouseUp := TALBaseText(Source).OnElementMouseUp;
      OnElementMouseEnter := TALBaseText(Source).OnElementMouseEnter;
      OnElementMouseLeave := TALBaseText(Source).OnElementMouseLeave;
      CacheIndex := TALBaseText(Source).CacheIndex;
      CacheEngine := TALBaseText(Source).CacheEngine;
      AutoTranslate := TALBaseText(Source).AutoTranslate;
      Text := TALBaseText(Source).Text;
      TextSettings.Assign(TALBaseText(Source).TextSettings);
      MaxWidth := TALBaseText(Source).MaxWidth;
      MaxHeight := TALBaseText(Source).MaxHeight;
      YRadius := TALBaseText(Source).YRadius;
      XRadius := TALBaseText(Source).XRadius;
      Corners := TALBaseText(Source).Corners;
      Sides := TALBaseText(Source).Sides;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*********************************}
procedure TALBaseText.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    MaxWidth := ALAlignDimensionToPixelRound(MaxWidth, ALGetScreenScale, Tepsilon.position);
    MaxHeight := ALAlignDimensionToPixelRound(MaxHeight, ALGetScreenScale, Tepsilon.position);
    TextSettings.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*************************************}
procedure TALBaseText.ApplyColorScheme;
begin
  beginUpdate;
  try
    inherited;
    TextSettings.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{******************************}
procedure TALBaseText.DoResized;
begin
  if not FIsAdjustingSize then begin
    ClearBufDrawable;
    inherited;
  end
  else
    inherited;
end;

{*************************************}
procedure TALBaseText.ParentRealigning;
begin
  var LAutosize := GetAutoSize;
  var LMaxWidthIsZero := SameValue(MaxWidth, 0, TEpsilon.Position);
  var LMaxHeightIsZero := SameValue(MaxHeight, 0, TEpsilon.Position);
  if (LAutosize = TALAutoSizeMode.None) or
     ((not LMaxWidthIsZero) and (not LMaxHeightIsZero)) then begin
    FMaxContainedSize.Width := -1;
    FMaxContainedSize.Height := -1;
    exit;
  end;

  var LMaxContainedSize := GetMaxContainedSize;
  if ((LAutoSize in [TALAutoSizeMode.Both, TALAutoSizeMode.Height]) and
      (Align in [TALAlignLayout.Top,
                 TALAlignLayout.TopCenter,
                 TALAlignLayout.TopLeft,
                 TALAlignLayout.TopRight,
                 TALAlignLayout.Bottom,
                 TALAlignLayout.BottomCenter,
                 TALAlignLayout.BottomLeft,
                 TALAlignLayout.BottomRight,
                 TALAlignLayout.MostTop,
                 TALAlignLayout.MostTopCenter,
                 TALAlignLayout.MostTopLeft,
                 TALAlignLayout.MostTopRight,
                 TALAlignLayout.MostBottom,
                 TALAlignLayout.MostBottomCenter,
                 TALAlignLayout.MostBottomLeft,
                 TALAlignLayout.MostBottomRight]) and
      (not sameValue(LMaxContainedSize.Height, FMaxContainedSize.Height, TEpsilon.Position)) and
      (LMaxHeightIsZero)) or
     ((LAutoSize in [TALAutoSizeMode.Both, TALAutoSizeMode.Width]) and
      (Align in [TALAlignLayout.Left,
                 TALAlignLayout.LeftCenter,
                 TALAlignLayout.LeftTop,
                 TALAlignLayout.LeftBottom,
                 TALAlignLayout.Right,
                 TALAlignLayout.RightCenter,
                 TALAlignLayout.RightTop,
                 TALAlignLayout.RightBottom,
                 TALAlignLayout.MostLeft,
                 TALAlignLayout.MostLeftCenter,
                 TALAlignLayout.MostLeftTop,
                 TALAlignLayout.MostLeftBottom,
                 TALAlignLayout.MostRight,
                 TALAlignLayout.MostRightCenter,
                 TALAlignLayout.MostRightTop,
                 TALAlignLayout.MostRightBottom]) and
      (not sameValue(LMaxContainedSize.Width, FMaxContainedSize.Width, TEpsilon.Position)) and
      (LMaxWidthIsZero)) then begin
    FMaxContainedSize := LMaxContainedSize;
    ClearBufDrawable;
    AdjustSize;
  end
  else
    FMaxContainedSize := LMaxContainedSize;
end;

{*******************************}
procedure TALBaseText.AdjustSize;
begin
  var LHasUnconstrainedAutosizeWidth := HasUnconstrainedAutosizeWidth;
  var LHasUnconstrainedAutosizeHeight := HasUnconstrainedAutosizeHeight;
  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // if csDestroying do not do autosize
     (Text <> '') and // if Text is empty do not do autosize
     (LHasUnconstrainedAutosizeWidth or LHasUnconstrainedAutosizeHeight) and // if AutoSize is false nothing to adjust
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // non-reantrant
    try

      if isupdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      {$IF defined(debug)}
      //ALLog(ClassName + '.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(LHasUnconstrainedAutosizeWidth)+'/'+ALBoolToStrW(LHasUnconstrainedAutosizeHeight));
      {$ENDIF}

      var R: TrectF;
      If {$IF not DEFINED(ALDPK)}DoubleBuffered{$ELSE}True{$ENDIF} then begin
        if (CacheIndex <= 0) or
           (CacheEngine = nil) or
           (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, R{ARect})) then begin
          MakeBufDrawable;
          R := FBufDrawableRect;
        end;
      end
      else begin
        MeasureMultilineText(
          R, // out ARect: TRectF;
          FTextBroken, // out ATextBroken: Boolean;
          FAllTextDrawn, // out AAllTextDrawn: Boolean;
          FElements, // out AElements: TALTextElements;
          1, // const AScale: Single;
          Text, // const AText: String;
          TextSettings.Font, // const AFont: TALFont;
          TextSettings.Decoration, // const ADecoration: TALTextDecoration;
          TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
          TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
          Fill, // const AFill: TALBrush;
          nil, // const AStateLayer: TALStateLayer;
          Stroke, // const AStroke: TALStrokeBrush;
          Shadow, // const AShadow: TALShadow;
          XRadius, // const AXRadius: Single;
          YRadius); // const AYRadius: Single
      end;

      if not LHasUnconstrainedAutosizeWidth then begin
        r.Left := 0;
        r.Width := Width;
      end;
      if not LHasUnconstrainedAutosizeHeight then begin
        r.Top := 0;
        r.height := height;
      end;

      SetFixedSizeBounds(Position.X, Position.Y, R.Width, R.Height);

    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{************************************************************************}
function TALBaseText.GetElementAtPos(const APos: TPointF): TALTextElement;
begin
  for var I := Low(fElements) to High(fElements) do
    if fElements[i].Rect.Contains(APos) then begin
      Result := fElements[i];
      Exit;
    end;
  Result.Id := '';
  Result.Rect := TrectF.Empty;
end;

{**************************************************************************************}
procedure TALBaseText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(FOnElementMouseDown) or
     assigned(FOnElementMouseUp) or
     assigned(FOnElementClick) then begin

    FPressedElement := GetElementAtPos(TPointF.Create(X, Y));
    if assigned(FOnElementMouseDown) and (FPressedElement.ID <> '') then
      FOnElementMouseDown(Self, Button, Shift, X, Y, FPressedElement);

  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

{****************************************************************}
procedure TALBaseText.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited MouseMove(Shift, X, Y);

  if (assigned(FOnElementMouseMove) or
      assigned(FOnElementMouseEnter) or
      assigned(FOnElementMouseLeave)) then begin

    var LElement := GetElementAtPos(TPointF.Create(X, Y));
    if (FHoveredElement.ID <> LElement.ID) or (FHoveredElement.Rect <> LElement.Rect) then begin
      if assigned(FOnElementMouseLeave) and (FHoveredElement.ID <> '') then
        FOnElementMouseLeave(self, FHoveredElement);
      FHoveredElement := LElement;
      if assigned(FOnElementMouseEnter) and (FHoveredElement.ID <> '') then
        FOnElementMouseEnter(self, FHoveredElement);
    end;
    if assigned(FOnElementMouseMove) and (FHoveredElement.ID <> '') then
      FOnElementMouseMove(self, Shift, X, Y, LElement);

  end;
end;

{************************************************************************************}
procedure TALBaseText.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if assigned(FOnElementMouseUp) and (FPressedElement.ID <> '') then
    FOnElementMouseUp(Self, Button, Shift, X, Y, FPressedElement);
  FPressedElement := TALTextElement.Empty;
end;

{*********************************}
procedure TALBaseText.DoMouseEnter;
begin
  FHoveredElement := TALTextElement.Empty;
  inherited DoMouseEnter;
end;

{*********************************}
procedure TALBaseText.DoMouseLeave;
begin
  inherited DoMouseLeave;
  if assigned(FOnElementMouseLeave) and (FHoveredElement.ID <> '') then
    FOnElementMouseLeave(self, FHoveredElement);
  FHoveredElement := TALTextElement.Empty;
end;

{*********************************}
procedure TALBaseText.DoClickSound;
begin
  if (ClickSound=TALClickSoundMode.Always) or
     ((assigned(OnClick) or (assigned(FOnElementClick) and (FPressedElement.ID <> ''))) and
      (ClickSound=TALClickSoundMode.Default) and
      (ALGlobalClickSoundEnabled)) then
    ALPlayClickSound;
end;

{**************************}
procedure TALBaseText.Click;
begin
  inherited Click;
  if assigned(FOnElementClick) and (FPressedElement.ID <> '') then
    FOnElementClick(Self, FPressedElement);
end;

{***********************************}
procedure TALBaseText.PaddingChanged;
begin
  ClearBufDrawable;
  inherited;
  Repaint;
end;

{*************************************************}
procedure TALBaseText.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{***************************************************}
procedure TALBaseText.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{***************************************************}
procedure TALBaseText.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{***********************************************}
function TALBaseText.GetDefaultCorners: TCorners;
begin
  Result := AllCorners;
end;

{*******************************************}
function TALBaseText.GetDefaultSides: TSides;
begin
  Result := AllSides;
end;

{********************************************}
function TALBaseText.IsCornersStored: Boolean;
begin
  Result := FCorners <> DefaultCorners;
end;

{******************************************}
function TALBaseText.IsSidesStored: Boolean;
begin
  Result := FSides <> DefaultSides;
end;

{******************************************************}
procedure TALBaseText.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then begin
    ClearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{*********************************************}
function TALBaseText.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{*********************************************}
function TALBaseText.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{****************************************************}
procedure TALBaseText.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{****************************************************}
procedure TALBaseText.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{**************************************************}
procedure TALBaseText.SetSides(const Value: TSides);
begin
  if FSides <> Value then begin
    ClearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{*********************************************************}
procedure TALBaseText.TextSettingsChanged(Sender: TObject);
begin
  ClearBufDrawable;
  AdjustSize;
  Repaint;
end;

{**********************************************************************}
procedure TALBaseText.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALBaseText.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  Result := ALGetShapeSurfaceRect(
              ARect, // const ARect: TRectF;
              AutoAlignToPixel, // const AAlignToPixel: Boolean;
              Fill, // const AFill: TALBrush;
              nil, // const AStateLayer: TALStateLayer;
              Shadow); // const AShadow: TALShadow): TRectF;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALBaseText.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALBaseText.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{**************************}
procedure TALBaseText.Paint;
begin

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
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    var LRect := LocalRect;
    DrawMultilineText(
      TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
      LRect, // out ARect: TRectF;
      FTextBroken, // out ATextBroken: Boolean;
      FAllTextDrawn, // out AAllTextDrawn: Boolean;
      FElements, // out AElements: TALTextElements;
      1, // const AScale: Single;
      AbsoluteOpacity, // const AOpacity: Single;
      Text, // const AText: String;
      TextSettings.Font, // const AFont: TALFont;
      TextSettings.Decoration, // const ADecoration: TALTextDecoration;
      TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
      TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
      Fill, // const AFill: TALBrush;
      nil, // const AStateLayer: TALStateLayer;
      Stroke, // const AStroke: TALStrokeBrush;
      Shadow, // const AShadow: TALShadow;
      XRadius, // const AXRadius: Single;
      YRadius); // const AYRadius: Single
    {$ELSE}
    var LRect := LocalRect;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(FRenderTargetCanvas) then
    try
      ALClearCanvas(FRenderTargetCanvas, TAlphaColors.Null);
      DrawMultilineText(
        FRenderTargetCanvas, // const ACanvas: TALCanvas;
        LRect, // out ARect: TRectF;
        FTextBroken, // out ATextBroken: Boolean;
        FAllTextDrawn, // out AAllTextDrawn: Boolean;
        FElements, // out AElements: TALTextElements;
        ALGetScreenScale, // const AScale: Single;
        1, // const AOpacity: Single;
        Text, // const AText: String;
        TextSettings.Font, // const AFont: TALFont;
        TextSettings.Decoration, // const ADecoration: TALTextDecoration;
        TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
        TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
        Fill, // const AFill: TALBrush;
        nil, // const AStateLayer: TALStateLayer;
        Stroke, // const AStroke: TALStrokeBrush;
        Shadow, // const AShadow: TALShadow;
        XRadius, // const AXRadius: Single;
        YRadius); // const AYRadius: Single
    finally
      ALCanvasEndScene(FRenderTargetCanvas)
    end;
    ALUpdateDrawableFromSurface(FRenderTargetSurface, FRenderTargetDrawable);
    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FRenderTargetDrawable, // const ADrawable: TALDrawable;
      LRect.TopLeft, // const ADstTopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single)
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;

end;

{*********************************************}
function TALBaseText.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{**********************************************}
function TALBaseText.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{*************************************************************}
procedure TALBaseText.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{**********************************************************}
procedure TALBaseText.SetAlign(const Value: TALAlignLayout);
begin
  if Align <> Value then ClearBufDrawable;
  inherited SetAlign(Value);
end;

{**************************************************************}
procedure TALBaseText.SetAutoSize(const Value: TALAutoSizeMode);
begin
  if FAutoSize <> Value then
  begin
    ClearBufDrawable;
    Repaint;
    inherited;
  end;
end;

{***********************************************}
function TALBaseText.GetEffectiveMaxSize: TSizeF;
begin
  Result := TSizeF.Create(MaxWidth, MaxHeight);
  var LMaxWidthIsZero := SameValue(Result.Width, 0, TEpsilon.Position);
  var LMaxHeightIsZero := SameValue(Result.Height, 0, TEpsilon.Position);
  if LMaxWidthIsZero or LMaxHeightIsZero then begin
    if (FMaxContainedSize.Width < 0) or
       (FMaxContainedSize.Height < 0) then FMaxContainedSize := GetMaxContainedSize;
    if LMaxWidthIsZero then Result.Width := FMaxContainedSize.Width;
    if LMaxHeightIsZero then Result.Height := FMaxContainedSize.Height;
  end;
  Result.Width := Max(Result.Width, 0);
  Result.Height := Max(Result.Height, 0);
end;

{*************************************************}
procedure TALBaseText.SetText(const Value: string);
begin
  if FText <> Value then begin
    ClearBufDrawable;
    FText := Value;
    AdjustSize;
    Repaint;
  end;
end;

{*************************************}
procedure TALBaseText.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*******************************************}
function TALBaseText.GetMultiLineTextOptions(
           const AScale: Single;
           const AOpacity: Single;
           const AFont: TALFont;
           const ADecoration: TALTextDecoration;
           const AEllipsisFont: TALFont;
           const AEllipsisDecoration: TALTextDecoration;
           const AFill: TALBrush;
           const AStateLayer: TALStateLayer;
           const AStroke: TALStrokeBrush;
           const AShadow: TALShadow;
           const AXRadius: Single;
           const AYRadius: Single): TALMultiLineTextOptions;
begin
  Result := FMultiLineTextOptions;
  Result.Scale := AScale;
  Result.AlignToPixel := AutoAlignToPixel;
  Result.Opacity := AOpacity;
  //--
  if Afont <> nil then begin
    Result.FontFamily := Afont.Family;
    Result.FontSize := Afont.Size;
    Result.FontWeight := Afont.Weight;
    Result.FontSlant := Afont.Slant;
    Result.FontStretch := Afont.Stretch;
    Result.FontColor := AFont.Color;
  end
  else begin
    Result.FontFamily := '';
    Result.FontSize := 14;
    Result.FontWeight := TfontWeight.Regular;
    Result.FontSlant := TFontSlant.Regular;
    Result.FontStretch := TfontStretch.Regular;
    Result.FontColor := TAlphaColors.Black;
  end;
  //--
  if ADecoration <> nil then begin
    Result.DecorationKinds := ADecoration.Kinds;
    Result.DecorationStyle := ADecoration.Style;
    Result.DecorationThicknessMultiplier := ADecoration.ThicknessMultiplier;
    Result.DecorationColor := ADecoration.Color;
  end
  else begin
    Result.DecorationKinds := [];
    Result.DecorationStyle := TALTextDecorationStyle.Solid;
    Result.DecorationThicknessMultiplier := 1;
    Result.DecorationColor := Talphacolors.Null;
  end;
  //--
  Result.EllipsisText := TextSettings.Ellipsis;
  Result.EllipsisInheritSettings := TextSettings.EllipsisSettings.inherit;
  //--
  if AEllipsisfont <> nil then begin
    Result.EllipsisFontFamily := AEllipsisfont.Family;
    Result.EllipsisFontSize := AEllipsisfont.Size;
    Result.EllipsisFontWeight := AEllipsisfont.Weight;
    Result.EllipsisFontSlant := AEllipsisfont.Slant;
    Result.EllipsisFontStretch := AEllipsisfont.Stretch;
    Result.EllipsisFontColor := AEllipsisFont.Color;
  end
  else begin
    Result.EllipsisFontFamily := '';
    Result.EllipsisFontSize := 14;
    Result.EllipsisFontWeight := TfontWeight.Regular;
    Result.EllipsisFontSlant := TfontSlant.Regular;
    Result.EllipsisFontStretch := TfontStretch.Regular;
    Result.EllipsisFontColor := TAlphaColors.black;
  end;
  //--
  if AEllipsisDecoration <> nil then begin
    Result.EllipsisDecorationKinds := AEllipsisDecoration.Kinds;
    Result.EllipsisDecorationStyle := AEllipsisDecoration.Style;
    Result.EllipsisDecorationThicknessMultiplier := AEllipsisDecoration.ThicknessMultiplier;
    Result.EllipsisDecorationColor := AEllipsisDecoration.Color;
  end
  else begin
    Result.EllipsisDecorationKinds := [];
    Result.EllipsisDecorationStyle := TALTextDecorationStyle.Solid;
    Result.EllipsisDecorationThicknessMultiplier := 1;
    Result.EllipsisDecorationColor := TAlphaColors.Null;
  end;
  //--
  Result.AutoSize := TALAutoSizeMode.None;
  var LHasUnconstrainedAutosizeWidth := HasUnconstrainedAutosizeWidth;
  var LHasUnconstrainedAutosizeHeight := HasUnconstrainedAutosizeHeight;
  if LHasUnconstrainedAutosizeWidth and LHasUnconstrainedAutosizeHeight then Result.AutoSize := TALAutoSizeMode.Both
  else if LHasUnconstrainedAutosizeWidth then Result.Autosize := TALAutoSizeMode.Width
  else if LHasUnconstrainedAutosizeHeight then Result.Autosize := TALAutoSizeMode.Height;
  //--
  Result.MaxLines := TextSettings.MaxLines;
  Result.LineHeightMultiplier := TextSettings.LineHeightMultiplier;
  Result.LetterSpacing := TextSettings.LetterSpacing;
  Result.FailIfTextBroken := false;
  //--
  if TFillTextFlag.RightToLeft in FillTextFlags then Result.Direction := TALTextDirection.RightToLeft
  else Result.Direction := TALTextDirection.LeftToRight;
  Result.HTextAlign := TextSettings.HorzAlign;
  Result.VTextAlign := TextSettings.VertAlign;
  //--
  if AFill <> nil then begin
    Result.FillColor := AFill.Color;
    Result.FillGradientStyle := AFill.Gradient.Style;
    Result.FillGradientAngle := AFill.Gradient.Angle;
    Result.FillGradientColors := AFill.Gradient.Colors;
    Result.FillGradientOffsets := AFill.Gradient.Offsets;
    Result.FillResourceName := AFill.ResourceName;
    Result.FillResourceStream := AFill.ResourceStream;
    Result.FillMaskResourceName := '';
    Result.FillBackgroundMargins := AFill.BackgroundMargins.Rect;
    Result.FillImageMargins := AFill.ImageMargins.Rect;
    Result.FillImageNoRadius := AFill.ImageNoRadius;
    Result.FillImageTintColor := AFill.ImageTintColor;
    Result.FillWrapMode := AFill.WrapMode;
    Result.FillCropCenter := TPointF.create(0.5,0.5);
    Result.FillBlurRadius := 0;
  end
  else begin
    Result.FillColor := TAlphaColors.null;
    Result.FillGradientStyle := TGradientStyle.Linear;
    Result.FillGradientAngle := 180;
    Result.FillGradientColors := [];
    Result.FillGradientOffsets := [];
    Result.FillResourceName := '';
    Result.FillResourceStream := nil;
    Result.FillMaskResourceName := '';
    Result.FillBackgroundMargins := TRectF.Empty;
    Result.FillImageMargins := TRectF.Empty;
    Result.FillImageNoRadius := False;
    Result.FillImageTintColor := TAlphaColors.null;
    Result.FillWrapMode := TALImageWrapMode.Fit;
    Result.FillCropCenter := TPointF.create(0.5,0.5);
    Result.FillBlurRadius := 0;
  end;
  //--
  if AStateLayer <> nil then begin
    Result.StateLayerOpacity := AStateLayer.Opacity;
    if AStateLayer.UseContentColor then Result.StateLayerColor := AFont.Color
    else Result.StateLayerColor := AStateLayer.Color;
    Result.StateLayerMargins := AStateLayer.Margins.Rect;
    Result.StateLayerXRadius := AStateLayer.XRadius;
    Result.StateLayerYRadius := AStateLayer.YRadius;
  end
  else begin
    Result.StateLayerOpacity := 0;
    Result.StateLayerColor := TalphaColors.Null;
    Result.StateLayerMargins := TRectF.Empty;
    Result.StateLayerXRadius := NaN;
    Result.StateLayerYRadius := NaN;
  end;
  //--
  if AStroke <> nil then begin
    Result.StrokeColor := AStroke.Color;
    Result.StrokeThickness := AStroke.Thickness;
  end
  else begin
    Result.StrokeColor := TalphaColors.Null;
    Result.StrokeThickness := 1;
  end;
  //--
  if AShadow <> nil then begin
    Result.ShadowColor := AShadow.Color;
    Result.ShadowBlur := AShadow.Blur;
    Result.ShadowOffsetX := AShadow.OffsetX;
    Result.ShadowOffsetY := AShadow.OffsetY;
  end
  else begin
    Result.ShadowColor := TalphaColors.Null;
    Result.ShadowBlur := 12;
    Result.ShadowOffsetX := 0;
    Result.ShadowOffsetY := 0;
  end;
  //--
  Result.Sides := Sides;
  Result.XRadius := AXRadius;
  Result.YRadius := AYRadius;
  Result.Corners := Corners;
  Result.Padding := padding.Rect;
  //--
  Result.TextIsHtml := TextSettings.IsHtml;
  //--
  Result.OnAdjustRect := DrawMultilineTextAdjustRect;
  Result.OnBeforeDrawBackground := DrawMultilineTextBeforeDrawBackground;
  Result.OnBeforeDrawParagraph := DrawMultilineTextBeforeDrawParagraph;
end;

{****************************************************************************************************************************************************************}
Procedure TALBaseText.DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF);
begin
  // Virtual
end;

{**************************************************************************************************************************************************}
Procedure TALBaseText.DrawMultilineTextBeforeDrawBackground(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF);
begin
  // Virtual
end;

{*************************************************************************************************************************************************}
Procedure TALBaseText.DrawMultilineTextBeforeDrawParagraph(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF);
begin
  // Virtual
end;

{**************************************}
Procedure TALBaseText.DrawMultilineText(
            const ACanvas: TALCanvas;
            var ARect: TRectF;
            out ATextBroken: Boolean;
            out AAllTextDrawn: Boolean;
            out AElements: TALTextElements;
            const AScale: Single;
            const AOpacity: Single;
            const AText: String;
            const AFont: TALFont;
            const ADecoration: TALTextDecoration;
            const AEllipsisFont: TALFont;
            const AEllipsisDecoration: TALTextDecoration;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow;
            const AXRadius: Single;
            const AYRadius: Single);
begin

  if ALIsCanvasNull(ACanvas) then
    Raise Exception.Create('Canvas cannot be null');

  var LMultiLineTextOptions := GetMultiLineTextOptions(
                                 AScale,
                                 AOpacity,
                                 AFont,
                                 ADecoration,
                                 AEllipsisFont,
                                 AEllipsisDecoration,
                                 AFill,
                                 AStateLayer,
                                 AStroke,
                                 AShadow,
                                 AXRadius,
                                 AYRadius);

  if (AText <> '') then begin

    var LMaxSize: TSizeF;
    var LHasUnconstrainedAutosizeWidth := HasUnconstrainedAutosizeWidth;
    var LHasUnconstrainedAutosizeHeight := HasUnconstrainedAutosizeHeight;
    if LHasUnconstrainedAutosizeWidth and LHasUnconstrainedAutosizeHeight then LMaxSize := GetEffectiveMaxSize
    else if LHasUnconstrainedAutosizeWidth then LMaxSize := TSizeF.Create(GetEffectiveMaxSize.Width, Height)
    else if LHasUnconstrainedAutosizeHeight then LMaxSize := TSizeF.Create(Width, GetEffectiveMaxSize.Height)
    else LMaxSize := TSizeF.Create(width, height);

    ARect.Width := LMaxSize.cX;
    ARect.Height := LMaxSize.cY;

    var LSurface: TALSurface := ALNullSurface;
    var LCanvas: TALCanvas := ACanvas;
    ALDrawMultiLineText(
      LSurface,
      LCanvas,
      AText,
      ARect,
      ATextBroken,
      AAllTextDrawn,
      AElements,
      LMultiLineTextOptions);

  end
  else begin

    var LEffectiveMaxSize := GetEffectiveMaxSize;
    ARect.Width := Min(LEffectiveMaxSize.Width, ARect.Width);
    ARect.Height := Min(LEffectiveMaxSize.Height, ARect.Height);

    Var LSurfaceSize := ARect.Size;
    DrawMultilineTextAdjustRect(
      ACanvas, // const ACanvas: TALCanvas;
      LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
      ARect, // var ARect: TrectF;
      LSurfaceSize); // var ASurfaceSize: TSizeF

    DrawMultilineTextBeforeDrawBackground(
      ACanvas, //const ACanvas: TALCanvas;
      LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
      ARect); // Const ARect: TrectF

    TALDrawRectangleHelper.Create(ACanvas)
      .SetScale(AScale)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(ARect)
      .SetOpacity(AOpacity)
      .SetFill(AFill)
      .SetStateLayer(AStateLayer, LMultiLineTextOptions.Fontcolor)
      .SetDrawStateLayerOnTop(AText <> '')
      .SetStroke(AStroke)
      .SetShadow(AShadow)
      .SetSides(Sides)
      .SetCorners(Corners)
      .SetXRadius(AXRadius)
      .SetYRadius(AYRadius)
      .Draw;

    DrawMultilineTextBeforeDrawParagraph(
      ACanvas, //const ACanvas: TALCanvas;
      LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
      ARect); // Const ARect: TrectF

  end;

end;

{*****************************************}
Procedure TALBaseText.MeasureMultilineText(
            out ARect: TRectF;
            out ATextBroken: Boolean;
            out AAllTextDrawn: Boolean;
            out AElements: TALTextElements;
            const AScale: Single;
            const AText: String;
            const AFont: TALFont;
            const ADecoration: TALTextDecoration;
            const AEllipsisFont: TALFont;
            const AEllipsisDecoration: TALTextDecoration;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow;
            const AXRadius: Single;
            const AYRadius: Single);
begin
  var LMaxSize: TSizeF;
  var LHasUnconstrainedAutosizeWidth := HasUnconstrainedAutosizeWidth;
  var LHasUnconstrainedAutosizeHeight := HasUnconstrainedAutosizeHeight;
  if LHasUnconstrainedAutosizeWidth and LHasUnconstrainedAutosizeHeight then LMaxSize := GetEffectiveMaxSize
  else if LHasUnconstrainedAutosizeWidth then LMaxSize := TSizeF.Create(GetEffectiveMaxSize.Width, Height)
  else if LHasUnconstrainedAutosizeHeight then LMaxSize := TSizeF.Create(Width, GetEffectiveMaxSize.Height)
  else LMaxSize := TSizeF.Create(width, height);

  ARect := TRectF.Create(0, 0, LMaxSize.cX, LMaxSize.cY);

  ALMeasureMultiLineText(
    AText,
    ARect,
    ATextBroken,
    AAllTextDrawn,
    AElements,
    GetMultiLineTextOptions(
      AScale,
      AbsoluteOpacity,
      AFont,
      ADecoration,
      AEllipsisFont,
      AEllipsisDecoration,
      AFill,
      AStateLayer,
      AStroke,
      AShadow,
      AXRadius,
      AYRadius));
end;

{**************************************}
Procedure TALBaseText.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            out ATextBroken: Boolean;
            out AAllTextDrawn: Boolean;
            out AElements: TALTextElements;
            const AScale: Single;
            const AText: String;
            const AFont: TALFont;
            const ADecoration: TALTextDecoration;
            const AEllipsisFont: TALFont;
            const AEllipsisDecoration: TALTextDecoration;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow;
            const AXRadius: Single;
            const AYRadius: Single);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var LMultiLineTextOptions := GetMultiLineTextOptions(
                                 AScale,
                                 1{AOpacity},
                                 AFont,
                                 ADecoration,
                                 AEllipsisFont,
                                 AEllipsisDecoration,
                                 AFill,
                                 AStateLayer,
                                 AStroke,
                                 AShadow,
                                 AXRadius,
                                 AYRadius);

  if (AText <> '') then begin

    var LMaxSize: TSizeF;
    var LHasUnconstrainedAutosizeWidth := HasUnconstrainedAutosizeWidth;
    var LHasUnconstrainedAutosizeHeight := HasUnconstrainedAutosizeHeight;
    if LHasUnconstrainedAutosizeWidth and LHasUnconstrainedAutosizeHeight then LMaxSize := GetEffectiveMaxSize
    else if LHasUnconstrainedAutosizeWidth then LMaxSize := TSizeF.Create(GetEffectiveMaxSize.Width, Height)
    else if LHasUnconstrainedAutosizeHeight then LMaxSize := TSizeF.Create(Width, GetEffectiveMaxSize.Height)
    else LMaxSize := TSizeF.Create(width, height);

    ABufDrawableRect := TRectF.Create(0, 0, LMaxSize.cX, LMaxSize.cY);
    ABufDrawable := ALCreateMultiLineTextDrawable(
                      AText,
                      ABufDrawableRect,
                      ATextBroken,
                      AAllTextDrawn,
                      AElements,
                      LMultiLineTextOptions);

    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  end;

  if (ALIsDrawableNull(ABufDrawable)) then begin

    if (AText <> '') or (LocalRect.IsEmpty) then begin
      ABufDrawable := ALCreateEmptyDrawable1x1;
      ABufDrawableRect := TRectF.Create(0,0,1/AScale,1/AScale);
      exit;
    end;

    ABufDrawableRect := LocalRect;
    var LSurfaceRect := ALGetShapeSurfaceRect(
                          ABufDrawableRect, // const ARect: TRectF;
                          AutoAlignToPixel, // const AAlignToPixel: Boolean;
                          AFill, // const AFill: TALBrush;
                          AStateLayer, // const AStateLayer: TALStateLayer;
                          AShadow); // const AShadow: TALShadow): TRectF;
    var LEffectiveMaxSize := GetEffectiveMaxSize;
    LSurfaceRect.Width := Min(LEffectiveMaxSize.Width, LSurfaceRect.Width);
    LSurfaceRect.Height := Min(LEffectiveMaxSize.Height, LSurfaceRect.Height);
    ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

    Var LSurfaceSize := LSurfaceRect.Size;
    DrawMultilineTextAdjustRect(
      ALNullCanvas, // const ACanvas: TALCanvas;
      LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
      ABufDrawableRect, // var ARect: TrectF;
      LSurfaceSize); // var ASurfaceSize: TSizeF
    LSurfaceRect.Width := LSurfaceSize.Width;
    LSurfaceRect.Height := LSurfaceSize.Height;

    var LSurface: TALSurface;
    var LCanvas: TALCanvas;
    ALCreateSurface(
      LSurface, // out ASurface: TALSurface;
      LCanvas, // out ACanvas: TALCanvas;
      AScale, // const AScale: Single;
      LSurfaceRect.Width, // const w: integer;
      LSurfaceRect.height);// const h: integer)
    try

      if ALCanvasBeginScene(LCanvas) then
      try

        DrawMultilineTextBeforeDrawBackground(
          LCanvas, //const ACanvas: TALCanvas;
          LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
          ABufDrawableRect); // Const ARect: TrectF

          TALDrawRectangleHelper.Create(LCanvas)
            .SetScale(AScale)
            .SetAlignToPixel(AutoAlignToPixel)
            .SetDstRect(ABufDrawableRect)
            .SetFill(AFill)
            .SetStateLayer(AStateLayer, AFont.color)
            .SetDrawStateLayerOnTop(Atext <> '')
            .SetStroke(AStroke)
            .SetShadow(AShadow)
            .SetSides(Sides)
            .SetCorners(Corners)
            .SetXRadius(AXRadius)
            .SetYRadius(AYRadius)
            .Draw;

        DrawMultilineTextBeforeDrawParagraph(
          LCanvas, //const ACanvas: TALCanvas;
          LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
          ABufDrawableRect); // Const ARect: TrectF

      finally
        ALCanvasEndScene(LCanvas)
      end;

      ABufDrawable := ALCreateDrawableFromSurface(LSurface);
      // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
      // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
      ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

    finally
      ALFreeAndNilSurface(LSurface, LCanvas);
    end;

  end;

end;

{************************************}
procedure TALBaseText.MakeBufDrawable;
begin

  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // out ABufDrawableRect: TRectF;
    FTextBroken, // out ATextBroken: Boolean;
    FAllTextDrawn, // out AAllTextDrawn: Boolean;
    FElements, // out AElements: TALTextElements;
    ALGetScreenScale, // const AScale: Single;
    Text, // const AText: String;
    TextSettings.Font, // const AFont: TALFont;
    TextSettings.Decoration, // const ADecoration: TALTextDecoration;
    TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
    TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
    Fill, // const AFill: TALBrush;
    nil, // const AStateLayer: TALStateLayer;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow, // const AShadow: TALShadow;
    XRadius, // const AXRadius: Single;
    YRadius); // const AYRadius: Single

end;

{***************************************}
function TALBaseText.TextBroken: Boolean;
begin
  result := fTextBroken;
end;

{*****************************************************}
procedure TALBaseText.SetMaxWidth(const Value: Single);
begin
  if compareValue(fMaxWidth, Value, Tepsilon.position) <> 0 then begin
    ClearBufDrawable;
    fMaxWidth := Value;
    AdjustSize;
  end;
end;

{******************************************************}
procedure TALBaseText.SetMaxHeight(const Value: Single);
begin
  if compareValue(fMaxHeight, Value, Tepsilon.position) <> 0 then begin
    ClearBufDrawable;
    fMaxHeight := Value;
    AdjustSize;
  end;
end;

{*********************************************}
function TALBaseText.IsMaxWidthStored: Boolean;
begin
  result := compareValue(fMaxWidth, DefaultMaxWidth, Tepsilon.position) <> 0;
end;

{**********************************************}
function TALBaseText.IsMaxHeightStored: Boolean;
begin
  result := compareValue(fMaxHeight, DefaultMaxHeight, Tepsilon.position) <> 0;
end;

{********************************************}
function TALBaseText.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{********************************************}
function TALBaseText.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALText.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TextIsHtml', ReadTextIsHtml{ReadData}, nil{WriteData}, false{hasdata});
  Filer.DefineProperty('LineSpacing', ReadLineSpacing{ReadData}, nil{WriteData}, false{hasdata});
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALText.ReadTextIsHtml(Reader: TReader);
begin
  TextSettings.IsHtml := Reader.ReadBoolean;
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALText.ReadLineSpacing(Reader: TReader);
begin
  var LLineSpacing: Extended := Reader.ReadFloat;
  // Luckily, TextSettings was defined before LineSpacing,
  // so its properties are already read.
  var LFontFamily := TextSettings.Font.Family;
  if LFontFamily = '' then LFontFamily := 'sans-serif';
  var LFontWeight := TextSettings.Font.Weight;
  // There is a slight discrepancy in line height between platforms: Windows (Segoe UI),
  // iOS (Helvetica Neue), and Android (Roboto). However, the line spacing remains consistent
  // across all operating systems. Given the need to choose a font, we prefer to use the
  // Roboto (Android) font for consistency.
  if ALSametextW(LFontFamily, 'sans-serif') then LFontFamily := 'Roboto'
  else if ALSametextW(LFontFamily, 'sans-serif-thin') then begin
    LFontFamily := 'Roboto';
    if LFontWeight = TFontWeight.Bold then LFontWeight := TFontWeight.regular
    else LFontWeight := TFontWeight.thin;
  end
  else if ALSametextW(LFontFamily, 'sans-serif-light') then begin
    LFontFamily := 'Roboto';
    if LFontWeight = TFontWeight.Bold then LFontWeight := TFontWeight.Semibold
    else LFontWeight := TFontWeight.light;
  end
  else if ALSametextW(LFontFamily, 'sans-serif-medium') then begin
    LFontFamily := 'Roboto';
    if LFontWeight = TFontWeight.Bold then LFontWeight := TFontWeight.UltraBold
    else LFontWeight := TFontWeight.medium;
  end
  else if ALSametextW(LFontFamily, 'sans-serif-black') then begin
    LFontFamily := 'Roboto';
    if LFontWeight = TFontWeight.Bold then LFontWeight := TFontWeight.UltraBlack
    else LFontWeight := TFontWeight.black;
  end
  else LFontFamily := ALResolveFontFamily(LFontFamily);
  var LFontMetrics := ALGetFontMetrics(
                        LFontFamily, // TextSettings const AFontFamily: String;
                        TextSettings.Font.Size, // const AFontSize: single;
                        LFontWeight, // const AFontWeight: TFontWeight;
                        TextSettings.Font.Slant); // const AFontSlant: TFontSlant;
  var LLineHeight := -LFontMetrics.Ascent + LFontMetrics.Descent;
  // The LineHeightMultiplier property allows manual adjustment
  // of the height of the line as a multiple of fontSize.
  TextSettings.LineHeightMultiplier := RoundTo((LLineSpacing + LLineHeight) / TextSettings.Font.Size, -1);
end;
{$ENDIF}

{*******************************************************}
function TALText.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TALTextSettings.Create;
end;

{************************************************}
function TALText.GetTextSettings: TALTextSettings;
begin
  Result := TALTextSettings(Inherited TextSettings);
end;

{**************************************************************}
procedure TALText.SetTextSettings(const Value: TALTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALImage, TALAnimatedImage, TALRectangle, TALEllipse, TALCircle, TALLine, TALText]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALImage, 'Size');
  UnlistPublishedProperty(TALImage, 'StyleName');
  UnlistPublishedProperty(TALImage, 'OnTap');
  //--
  UnlistPublishedProperty(TALAnimatedImage, 'Size');
  UnlistPublishedProperty(TALAnimatedImage, 'StyleName');
  UnlistPublishedProperty(TALAnimatedImage, 'OnTap');
  //--
  UnlistPublishedProperty(TALRectangle, 'Size');
  UnlistPublishedProperty(TALRectangle, 'StyleName');
  UnlistPublishedProperty(TALRectangle, 'OnTap');
  //--
  UnlistPublishedProperty(TALEllipse, 'Size');
  UnlistPublishedProperty(TALEllipse, 'StyleName');
  UnlistPublishedProperty(TALEllipse, 'OnTap');
  //--
  UnlistPublishedProperty(TALCircle, 'Size');
  UnlistPublishedProperty(TALCircle, 'StyleName');
  UnlistPublishedProperty(TALCircle, 'OnTap');
  //--
  UnlistPublishedProperty(TALLine, 'Size');
  UnlistPublishedProperty(TALLine, 'StyleName');
  UnlistPublishedProperty(TALLine, 'OnTap');
  //--
  UnlistPublishedProperty(TALText, 'Size');
  UnlistPublishedProperty(TALText, 'StyleName');
  UnlistPublishedProperty(TALText, 'OnTap');
  {$ENDIF}
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Objects','initialization');
  {$ENDIF}
  RegisterFmxClasses([TALImage, TALRectangle, TALEllipse, TALCircle, TALLine, TALText]);

end.