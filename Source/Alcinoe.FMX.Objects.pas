unit Alcinoe.FMX.Objects;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if FMX.Objects.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.Rtti,
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
  Alcinoe.FMX.Types3D,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALShape = class(TALControl, IALShapeControl)
  strict private
    FFill: TALBrush;
    FStroke: TALStrokeBrush;
    fShadow: TALShadow;
    function GetFill: TALBrush;
    procedure SetFill(const Value: TALBrush);
    function GetStroke: TALStrokeBrush;
    procedure SetStroke(const Value: TALStrokeBrush);
    function GetShadow: TALShadow;
    procedure SetShadow(const Value: TALShadow);
  protected
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure ShadowChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    property Fill: TALBrush read GetFill write SetFill;
    property Stroke: TALStrokeBrush read GetStroke write SetStroke;
    property Shadow: TALShadow read GetShadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
  //
  // Moreover, transitioning from smartphones to tablets, I've noticed that
  // maintaining proper proportions requires increasing the font and image
  // sizes by 15%. Therefore, to avoid resizing and truly leverage
  // multi-resolution bitmaps, one might need up to 10 different bitmaps per
  // image. This leads me to conclude that the concept of multi-resolution
  // bitmaps is fundamentally flawed.
  [ComponentPlatforms($FFFF)]
  TALImage = class(TALControl, IALDoubleBufferedControl)
  private
    fExifOrientationInfo: TalExifOrientationInfo;
    fRotateAccordingToExifOrientation: Boolean;
    fResourceName: String;
    FWrapMode: TALImageWrapMode;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure setResourceName(const Value: String);
  protected
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
  published
    //property Action;
    property Align;
    property Anchors;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
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
    property ResourceName: String read fResourceName write setResourceName;
    // under Android, work only when setting a file in ResourceName
    property RotateAccordingToExifOrientation: Boolean read fRotateAccordingToExifOrientation write fRotateAccordingToExifOrientation default false;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
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
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALAnimatedImage = class(TALControl)
  Public
    type
      TAnimation = class(TALPersistentObserver)
      private
        fOwner: TALAnimatedImage;
        FFloatAnimation: TALFloatAnimation;
        FSpeed: Single;
        FDuration: Single;
        function GetAutoReverse: Boolean;
        function GetEnabled: Boolean; virtual;
        function GetDelay: Single;
        function GetDuration: Single;
        function GetInverse: Boolean;
        function GetLoop: Boolean;
        function GetCurrentTime: Single;
        function GetPause: Boolean;
        function GetRunning: Boolean;
        function GetStartProgress: Single;
        function GetStopProgress: Single;
        function GetCurrentProgress: Single;
        function GetSpeed: Single;
        procedure SetEnabled(const Value: Boolean); virtual;
        procedure SetAutoReverse(const Value: Boolean);
        procedure SetDelay(const Value: Single);
        procedure SetInverse(const Value: Boolean);
        procedure SetLoop(const Value: Boolean);
        procedure SetPause(const Value: Boolean);
        procedure SetStartProgress(const Value: Single);
        procedure SetStopProgress(const Value: Single);
        procedure SetSpeed(const Value: Single);
        function IsStopProgressStored: Boolean;
        function IsSpeedStored: Boolean;
        procedure UpdateFloatAnimationDuration;
        procedure repaint;
      protected
        procedure SetDuration(const Value: Single);
        procedure doFirstFrame(Sender: TObject);
        procedure doProcess(Sender: TObject);
        procedure doFinish(Sender: TObject);
      public
        constructor Create(AOwner: TALAnimatedImage); reintroduce; virtual;
        destructor Destroy; override;
        procedure Start; virtual;
        procedure Stop; virtual;
        procedure StopAtCurrent; virtual;
        property Running: Boolean read getRunning;
        property Pause: Boolean read getPause write setPause;
        property CurrentProgress: Single read GetCurrentProgress;
        property CurrentTime: Single read GetCurrentTime;
      published
        property AutoReverse: Boolean read getAutoReverse write setAutoReverse default False;
        property Delay: Single read getDelay write setDelay;
        property Duration: Single read getDuration;
        property Enabled: Boolean read getEnabled write SetEnabled default False;
        property Inverse: Boolean read getInverse write setInverse default False;
        property Loop: Boolean read getLoop write setLoop default True;
        property Speed: Single read GetSpeed write setSpeed stored IsSpeedStored nodefault;
        property StartProgress: Single read GetStartProgress write SetStartProgress;
        property StopProgress: Single read GetStopProgress write setStopProgress stored IsStopProgressStored nodefault;
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
    fResourceName: String;
    FWrapMode: TALImageWrapMode;
    FOnAnimationFirstFrame: TNotifyEvent;
    FOnAnimationProcess: TNotifyEvent;
    FOnAnimationFinish: TNotifyEvent;
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure setResourceName(const Value: String);
    procedure SetAnimation(const Value: TAnimation);
  protected
    procedure Paint; override;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateCodec; virtual;
    procedure ReleaseCodec; virtual;
  published
    //property Action;
    property Align;
    property Anchors;
    property Animation: TAnimation read fAnimation write SetAnimation;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
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
    property ResourceName: String read fResourceName write setResourceName;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
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
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseRectangle = class(TALShape, IALDoubleBufferedControl)
  private
    fDoubleBuffered: boolean;
    FXRadius: Single;
    FYRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    FDefaultXRadius: Single;
    FDefaultYRadius: Single;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    function HasCustomDraw: Boolean; virtual;
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
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
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property DefaultXRadius: Single read FDefaultXRadius write FDefaultXRadius;
    property DefaultYRadius: Single read FDefaultYRadius write FDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALRectangle = class(TALBaseRectangle, IALAutosizeControl)
  private
    FAutoSize: Boolean;
  protected
    function GetAutoSize: Boolean; virtual;
    procedure SetAutoSize(const Value: Boolean); virtual;
    procedure DoRealign; override;
    procedure AdjustSize; virtual;
    { IALAutosizeControl }
    function HasUnconstrainedAutosizeX: Boolean; virtual;
    function HasUnconstrainedAutosizeY: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    //property Action;
    property Align;
    property Anchors;
    // Dynamically adjusts the dimensions to accommodate child controls,
    // considering their sizes, positions, margins, and alignments.
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
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
    property RotationCenter;
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
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCircle = class(TALShape, IALDoubleBufferedControl)
  private
    fDoubleBuffered: boolean;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
  protected
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
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
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    function PointInObjectLocal(X, Y: Single): Boolean; override;
  published
    //property Action;
    property Align;
    property Anchors;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
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
    property RotationCenter;
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
    //property OnDblClick;
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
  TALLine = class(TALShape, IALDoubleBufferedControl)
  private
    fDoubleBuffered: boolean;
    FLineType: TALLineType;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    procedure SetLineType(const Value: TALLineType);
  protected
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
  published
    //property Action;
    property Align;
    property Anchors;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
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
    property RotationCenter;
    property Scale;
    property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
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
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseText = class(TALShape, IALAutosizeControl, IALDoubleBufferedControl)
  public
    type
      TElementMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; const Element: TALTextElement) of object;
      TElementMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Single; const Element: TALTextElement) of object;
      TElementNotifyEvent = procedure(Sender: TObject; const Element: TALTextElement) of object;
  private
    fDoubleBuffered: boolean;
    FMultiLineTextOptions: TALMultiLineTextOptions;
    FOnElementClick: TElementNotifyEvent;
    FOnElementDblClick: TElementNotifyEvent;
    FOnElementMouseDown: TElementMouseEvent;
    FOnElementMouseMove: TElementMouseMoveEvent;
    FOnElementMouseUp: TElementMouseEvent;
    FOnElementMouseEnter: TElementNotifyEvent;
    FOnElementMouseLeave: TElementNotifyEvent;
    FHoveredElement: TALTextElement;
    FPressedElement: TALTextElement;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
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
    FAutoSize: Boolean;
    FDefaultXRadius: Single;
    FDefaultYRadius: Single;
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
    FIsAdjustingSize: Boolean;
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure SetAlign(const Value: TAlignLayout); override;
    function GetAutoSize: Boolean; virtual;
    procedure SetAutoSize(const Value: Boolean); virtual;
    function GetElementAtPos(const APos: TPointF): TALTextElement;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure Click; override;
    procedure DblClick; override;
    property BufDrawable: TALDrawable read FBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
    procedure PaddingChanged; override;
    procedure TextSettingsChanged(Sender: TObject); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure SetTextSettings(const Value: TALBaseTextSettings); virtual;
    function CreateTextSettings: TALBaseTextSettings; virtual; abstract;
    procedure Paint; override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure Loaded; override;
    procedure DoResized; override;
    procedure DoEndUpdate; override;
    procedure AdjustSize; virtual;
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
               const AShadow: TALShadow): TALMultiLineTextOptions;
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
                const AShadow: TALShadow);
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
                const AShadow: TALShadow);
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
                const AShadow: TALShadow);
    property Elements: TALTextElements read fElements;
    property OnElementClick: TElementNotifyEvent read FOnElementClick write FOnElementClick;
    property OnElementDblClick: TElementNotifyEvent read FOnElementDblClick write FOnElementDblClick;
    property OnElementMouseDown: TElementMouseEvent read FOnElementMouseDown write FOnElementMouseDown;
    property OnElementMouseMove: TElementMouseMoveEvent read FOnElementMouseMove write FOnElementMouseMove;
    property OnElementMouseUp: TElementMouseEvent read FOnElementMouseUp write FOnElementMouseUp;
    property OnElementMouseEnter: TElementNotifyEvent read FOnElementMouseEnter write FOnElementMouseEnter;
    property OnElementMouseLeave: TElementNotifyEvent read FOnElementMouseLeave write FOnElementMouseLeave;
    property TextSettings: TALBaseTextSettings read fTextSettings write SetTextSettings;
    { IALAutosizeControl }
    function HasUnconstrainedAutosizeX: Boolean; virtual;
    function HasUnconstrainedAutosizeY: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    procedure SetNewScene(AScene: IScene); override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    function TextBroken: Boolean;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property HitTest default False;
    property MaxWidth: Single read fMaxWidth write SetMaxWidth stored IsMaxWidthStored nodefault;
    property MaxHeight: Single read fMaxHeight write SetMaxHeight stored IsMaxHeightStored nodefault;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property Text: string read FText write SetText;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
    property DefaultXRadius: Single read FDefaultXRadius write FDefaultXRadius;
    property DefaultYRadius: Single read FDefaultYRadius write FDefaultYRadius;
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
    property RotationCenter;
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
    //property OnElementDblClick;
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
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

procedure ALLockTexts(const aParentControl: Tcontrol);
procedure ALUnLockTexts(const aParentControl: Tcontrol);

procedure Register;

implementation

uses
  system.SysUtils,
  system.Math,
  system.Math.Vectors,
  FMX.platform,
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
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**********************************************}
constructor TALShape.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TALBrush.Create($FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TALStrokeBrush.Create($FF000000);
  FStroke.OnChanged := StrokeChanged;
  fShadow := TALShadow.Create;
  fShadow.OnChanged := ShadowChanged;
end;

{**************************}
destructor TALShape.Destroy;
begin
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FFill);
  ALFreeAndNil(fShadow);
  inherited;
end;

{******************************}
procedure TALShape.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    Stroke.AlignToPixel;
    Fill.AlignToPixel;
    Shadow.AlignToPixel;
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

{**********************************************}
constructor TALImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;
  fRotateAccordingToExifOrientation := False;
  fResourceName := '';
  FWrapMode := TALImageWrapMode.Fit;
  fBufDrawable := ALNullDrawable;
  SetAcceptsControls(False);
end;

{**************************}
destructor TALImage.Destroy;
begin
  clearBufDrawable;
  inherited;
end;

{**********************************}
procedure TALImage.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*********************************}
procedure TALImage.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if fResourceName is empty
     (fResourceName = '')
  then begin
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) then exit;

  var LFileName := ALGetResourceFilename(FResourceName);

  if (fRotateAccordingToExifOrientation) and (LFileName <> '') then fExifOrientationInfo := AlGetExifOrientationInfo(LFilename)
  else fExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;

  if fExifOrientationInfo in [TalExifOrientationInfo.TRANSPOSE,
                              TalExifOrientationInfo.ROTATE_90,
                              TalExifOrientationInfo.TRANSVERSE,
                              TalExifOrientationInfo.ROTATE_270] then fBufDrawableRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, Height, Width), ALGetScreenScale) // to have the pixel aligned width and height
  // TalExifOrientationInfo.FLIP_HORIZONTAL
  // TalExifOrientationInfo.FLIP_VERTICAL
  // TalExifOrientationInfo.NORMAL
  // TalExifOrientationInfo.ROTATE_180
  // TalExifOrientationInfo.UNDEFINED
  else fBufDrawableRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height

  {$IFDEF ALDPK}
  try
  {$ENDIF}
    if LFileName <> '' then fBufDrawable := ALLoadFromFileAndWrapToDrawable(LFileName, FWrapMode, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
    else fBufDrawable := {$IFDEF ALDPK}ALNullDrawable{$ELSE}ALLoadFromResourceAndWrapToDrawable(fResourceName, FWrapMode, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale){$ENDIF};
  {$IFDEF ALDPK}
  except
    fBufDrawable := ALNullDrawable;
  end;
  {$ENDIF}

  {$IFDEF ALDPK}if not ALIsDrawableNull(fBufDrawable) then{$ENDIF}
    fBufDrawableRect := TrectF.Create(0,0, ALGetDrawableWidth(fBufDrawable)/ALGetScreenScale, ALGetDrawableHeight(fBufDrawable)/ALGetScreenScale).
                          CenterAt(LocalRect);

end;

{***********************}
procedure TALImage.Paint;
begin

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    var R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  MakeBufDrawable;

  var LMatrix: Tmatrix;
  var LMatrixRotationCenter: TpointF;
  case fExifOrientationInfo of
    TalExifOrientationInfo.FLIP_HORIZONTAL: begin
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.FLIP_VERTICAL: begin
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateScaling(1, -1); // matrix.setRotate(180); matrix.setScale(-1, 1);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.NORMAL:;
    TalExifOrientationInfo.ROTATE_180: begin
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(180)); // matrix.setRotate(180);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.ROTATE_270: begin
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(-90)); // matrix.setRotate(-90);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.ROTATE_90: begin
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(90)); // matrix.setRotate(90);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.TRANSPOSE: begin
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(90)); // matrix.setRotate(90);
      LMatrix := LMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.TRANSVERSE: begin
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(-90)); // matrix.setRotate(-90);
      LMatrix := LMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.UNDEFINED:;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{************************************************************}
procedure TALImage.SetWrapMode(const Value: TALImageWrapMode);
begin
  if FWrapMode <> Value then begin
    clearBufDrawable;
    FWrapMode := Value;
    Repaint;
  end;
end;

{******************************************************}
procedure TALImage.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    clearBufDrawable;
    FResourceName := Value;
    Repaint;
  end;
end;

{***************************}
procedure TALImage.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{*******************************************}
function TALImage.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{**********************************************************}
procedure TALImage.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not yet supported
end;

{***********************************************************************}
constructor TALAnimatedImage.TAnimation.Create(AOwner: TALAnimatedImage);
begin
  inherited create;
  fOwner := AOwner;
  fFloatAnimation := TALFloatAnimation.Create;
  fFloatAnimation.Loop := True;
  fFloatAnimation.StopValue := 1.0;
  fFloatAnimation.Duration := MaxSingle;
  fFloatAnimation.OnFirstFrame := DoFirstFrame;
  fFloatAnimation.OnProcess := DoProcess;
  fFloatAnimation.OnFinish := DoFinish;
  FSpeed := 1.0;
  FDuration := 0.0;
end;

{*********************************************}
destructor TALAnimatedImage.TAnimation.Destroy;
begin
  ALFreeAndNil(fFloatAnimation);
  inherited;
end;

{*****************************************************************}
procedure TALAnimatedImage.TAnimation.UpdateFloatAnimationDuration;
begin
  if not SameValue(FSpeed, 0.0, Single.Epsilon) then
    FFloatAnimation.Duration := (FDuration / FSpeed) * abs(FFloatAnimation.StopValue - FFloatAnimation.StartValue)
  else
    FFloatAnimation.Duration := maxSingle;
end;

{********************************************}
procedure TALAnimatedImage.TAnimation.repaint;
begin
  if Fowner.IsVisibleWithinFormBounds then
    Fowner.Repaint;
end;

{***********************************************************}
function TALAnimatedImage.TAnimation.GetAutoReverse: Boolean;
begin
  Result := FFloatAnimation.AutoReverse;
end;

{****************************************************}
function TALAnimatedImage.TAnimation.GetDelay: Single;
begin
  Result := FFloatAnimation.Delay;
end;

{*******************************************************}
function TALAnimatedImage.TAnimation.GetDuration: Single;
begin
  Result := FDuration;
end;

{*******************************************************}
function TALAnimatedImage.TAnimation.GetInverse: Boolean;
begin
  Result := FFloatAnimation.Inverse;
end;

{****************************************************}
function TALAnimatedImage.TAnimation.GetLoop: Boolean;
begin
  Result := FFloatAnimation.Loop;
end;

{**********************************************************}
function TALAnimatedImage.TAnimation.GetCurrentTime: Single;
begin
  if not Enabled then begin
    if Inverse then Result := StopProgress * Duration
    else Result := StartProgress * Duration;
  end
  else
    Result := CurrentProgress * Duration
end;

{*****************************************************}
function TALAnimatedImage.TAnimation.GetPause: Boolean;
begin
  Result := FFloatAnimation.Pause;
end;

{*******************************************************}
function TALAnimatedImage.TAnimation.GetRunning: Boolean;
begin
  Result := FFloatAnimation.Running;
end;

{************************************************************}
function TALAnimatedImage.TAnimation.GetStartProgress: Single;
begin
  Result := FFloatAnimation.StartValue;
end;

{***********************************************************}
function TALAnimatedImage.TAnimation.GetStopProgress: Single;
begin
  Result := FFloatAnimation.StopValue;
end;

{**************************************************************}
function TALAnimatedImage.TAnimation.GetCurrentProgress: Single;
begin
  Result := FFloatAnimation.CurrentValue;
end;

{****************************************************}
function TALAnimatedImage.TAnimation.GetSpeed: Single;
begin
  Result := FSpeed;
end;

{*************************************************************************}
procedure TALAnimatedImage.TAnimation.SetAutoReverse(const Value: Boolean);
begin
  FFloatAnimation.AutoReverse := Value;
end;

{******************************************************************}
procedure TALAnimatedImage.TAnimation.SetDelay(const Value: Single);
begin
  FFloatAnimation.Delay := Value;
end;

{*********************************************************************}
procedure TALAnimatedImage.TAnimation.SetDuration(const Value: Single);
begin
  FDuration := Value;
  UpdateFloatAnimationDuration;
end;

{*********************************************************************}
procedure TALAnimatedImage.TAnimation.SetInverse(const Value: Boolean);
begin
  FFloatAnimation.Inverse := Value;
  Repaint;
end;

{******************************************************************}
procedure TALAnimatedImage.TAnimation.SetLoop(const Value: Boolean);
begin
  FFloatAnimation.Loop := Value;
end;

{*******************************************************************}
procedure TALAnimatedImage.TAnimation.SetPause(const Value: Boolean);
begin
  FFloatAnimation.Pause := Value;
end;

{**************************************************************************}
procedure TALAnimatedImage.TAnimation.SetStartProgress(const Value: Single);
begin
  FFloatAnimation.StartValue := Min(Max(Value, 0), 1);
  UpdateFloatAnimationDuration;
  Repaint;
end;

{*************************************************************************}
procedure TALAnimatedImage.TAnimation.SetStopProgress(const Value: Single);
begin
  FFloatAnimation.StopValue := Min(Max(Value, 0), 1);
  UpdateFloatAnimationDuration;
  Repaint;
end;

{******************************************************************}
procedure TALAnimatedImage.TAnimation.SetSpeed(const Value: Single);
begin
  if not SameValue(FSpeed, Value, Single.Epsilon) then begin
    FSpeed := Value;
    UpdateFloatAnimationDuration;
  end;
end;

{*****************************************************************}
function TALAnimatedImage.TAnimation.IsStopProgressStored: Boolean;
begin
  Result := Not SameValue(FFloatAnimation.StopValue, 1.0, Single.Epsilon);
end;

{**********************************************************}
function TALAnimatedImage.TAnimation.IsSpeedStored: Boolean;
begin
  Result := Not SameValue(FSpeed, 1.0, Single.Epsilon);
end;

{*******************************************************}
function TALAnimatedImage.TAnimation.getEnabled: Boolean;
begin
  Result := FFloatAnimation.Enabled;
end;

{*********************************************************************}
procedure TALAnimatedImage.TAnimation.SetEnabled(const Value: Boolean);
begin
  FFloatAnimation.Enabled := Value;
end;

{******************************************************************}
procedure TALAnimatedImage.TAnimation.doFirstFrame(Sender: TObject);
begin
  if assigned(FOwner.FOnAnimationFirstFrame) then
    FOwner.FOnAnimationFirstFrame(FOwner);
  Repaint;
end;

{***************************************************************}
procedure TALAnimatedImage.TAnimation.doProcess(Sender: TObject);
begin
  if assigned(FOwner.FOnAnimationProcess) then
    FOwner.FOnAnimationProcess(FOwner);
  Repaint;
end;

{**************************************************************}
procedure TALAnimatedImage.TAnimation.doFinish(Sender: TObject);
begin
  if assigned(FOwner.FOnAnimationFinish) then
    FOwner.FOnAnimationFinish(FOwner);
  Repaint;
end;

{******************************************}
procedure TALAnimatedImage.TAnimation.Start;
begin
  FFloatAnimation.Start;
end;

{*****************************************}
procedure TALAnimatedImage.TAnimation.Stop;
begin
  FFloatAnimation.Stop;
end;

{**************************************************}
procedure TALAnimatedImage.TAnimation.StopAtCurrent;
begin
  FFloatAnimation.StopAtCurrent;
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
  fResourceName := '';
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

{*************************************}
procedure TALAnimatedImage.CreateCodec;
begin
  {$IF defined(ALSkiaAvailable)}

  if //--- Do not create Codec if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create Codec if fResourceName is empty
     (fResourceName = '')
  then begin
    ReleaseCodec;
    exit;
  end;

  if (fSkottieAnimation <> 0) or (fAnimcodecplayer <> 0) then exit;

  var LFileName := ALGetResourceFilename(FResourceName);

  if LFileName <> '' then begin
    fSkottieAnimation := sk4d_skottieanimation_make_from_file(MarshaledAString(UTF8String(LFileName)), TSkDefaultProviders.TypefaceFont.Handle)
  end
  else begin
    {$IFDEF ALDPK}
    fSkottieAnimation := 0
    {$ELSE}
    var LResourceStream := TResourceStream.Create(HInstance, fResourceName, RT_RCDATA);
    try
      var LSkStream := ALSkCheckHandle(sk4d_streamadapter_create(LResourceStream));
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
      ALfreeandNil(LResourceStream);
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
      var LResourceStream := TResourceStream.Create(HInstance, fResourceName, RT_RCDATA);
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
    Raise Exception.CreateFmt('Failed to create the animation codec for resource "%s". Please ensure the resource exists and is in a valid format', [fResourceName]);
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
    Raise Exception.CreateFmt('The animation "%s" has invalid dimensions (width or height is zero)', [fResourceName]);
    {$ELSE}
    ReleaseCodec;
    Exit;
    {$ENDIF}
  end;
  FRenderRect := TRectF.Create(0,0,LSize.width, LSize.height);

  case FWrapMode of
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
    'Duration: '+ALFloatTostrW(LDuration, ALDefaultFormatSettingsW) + ' | '+
    'Width: ' + ALFloatTostrW(LSize.Width, ALDefaultFormatSettingsW) + ' | '+
    'Height: ' + ALFloatTostrW(LSize.Height, ALDefaultFormatSettingsW),
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

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    var R := LocalRect;
    InflateRect(R, -0.5, -0.5);
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
    ALUpdateBitmapFromSkSurface(FBufSurface, FBufBitmap);
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
    ALUpdateBitmapFromSkImage(sk4d_animcodecplayer_get_frame(fAnimcodecplayer), FBufBitmap);
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

{******************************************************}
constructor TALBaseRectangle.Create(AOwner: TComponent);
begin
  inherited;
  fDoubleBuffered := true;
  FDefaultXRadius := 0;
  FDefaultYRadius := 0;
  FXRadius := FDefaultXRadius;
  FYRadius := FDefaultYRadius;
  FCorners := AllCorners;
  FSides := AllSides;
  fBufDrawable := ALNullDrawable;
end;

{**********************************}
destructor TALBaseRectangle.Destroy;
begin
  ClearBufDrawable;
  inherited;
end;

{***********************************}
procedure TALBaseRectangle.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{******************************************}
procedure TALBaseRectangle.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*************************************************}
function TALBaseRectangle.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{***********************************************}
function TALBaseRectangle.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

{*************************************************}
function TALBaseRectangle.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, FDefaultXRadius, TEpsilon.Vector);
end;

{*************************************************}
function TALBaseRectangle.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, FDefaultYRadius, TEpsilon.Vector);
end;

{***********************************************}
function TALBaseRectangle.HasCustomDraw: Boolean;
begin
  Result := False;
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
    if not fDoubleBuffered then clearBufDrawable;
  end;
end;

{*********************************************************}
procedure TALBaseRectangle.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FXRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FXRadius := NewValue;
    Repaint;
  end;
end;

{*********************************************************}
procedure TALBaseRectangle.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FYRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FYRadius := NewValue;
    Repaint;
  end;
end;

{***********************************************************}
procedure TALBaseRectangle.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    clearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{*******************************************************}
procedure TALBaseRectangle.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    clearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{******************************************************}
procedure TALBaseRectangle.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALBaseRectangle.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALBaseRectangle.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
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

      ALDrawRectangle(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
        ABufDrawableRect, // const Rect: TrectF;
        1, // const AOpacity: Single;
        AFill, // const Fill: TALBrush;
        AStateLayer, // const StateLayer: TALStateLayer;
        AStateLayerContentColor, // const AStateLayerContentColor: TAlphaColor;
        ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow, // const Shadow: TALShadow
        Sides, // const Sides: TSides;
        Corners, // const Corners: TCorners;
        XRadius, // const XRadius: Single = 0;
        YRadius); // const YRadius: Single = 0);

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
     ((not HasCustomDraw)
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
       (Fill.Styles = [TALBrushStyle.solid])))
  then begin
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

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

{*******************************}
procedure TALBaseRectangle.Paint;
begin

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    ALDrawRectangle(
      TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
      LocalRect, // const Rect: TrectF;
      AbsoluteOpacity, // const AOpacity: Single;
      Fill, // const Fill: TALBrush;
      nil, // const StateLayer: TALStateLayer;
      TAlphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
      True, // const ADrawStateLayerOnTop: Boolean;
      Stroke, // const Stroke: TALStrokeBrush;
      Shadow, // const Shadow: TALShadow
      Sides, // const Sides: TSides;
      Corners, // const Corners: TCorners;
      XRadius, // const XRadius: Single = 0;
      YRadius); // const YRadius: Single = 0);
    {$ELSE}
    {$IF defined(DEBUG)}
    if not doublebuffered then begin
      ALLog('TALBaseRectangle.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end;
    {$ENDIF}
    If Fill.Styles = [TALBrushStyle.Solid] then begin
      Canvas.Fill.kind := TBrushKind.solid;
      Canvas.Fill.color := Fill.color;
      Canvas.FillRect(ALAlignToPixelRound(LocalRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position), XRadius, YRadius, FCorners, AbsoluteOpacity, TCornerType.Round);
    end
    else if Fill.HasFill then raise Exception.Create('Error 87B5E7C8-55AF-41C7-88D4-B840C7D0F78F');
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{**************************************************}
constructor TALRectangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize := False;
end;

{*****************************************}
function TALRectangle.GetAutoSize: Boolean;
begin
  result := FAutoSize;
end;

{*******************************************************}
function TALRectangle.HasUnconstrainedAutosizeX: Boolean;
begin
  result := GetAutoSize;
end;

{*******************************************************}
function TALRectangle.HasUnconstrainedAutosizeY: Boolean;
begin
  result := GetAutoSize;
end;

{*******************************************************}
procedure TALRectangle.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
    repaint;
  end;
end;

{*******************************}
procedure TALRectangle.DoRealign;
begin
  inherited DoRealign;
  AdjustSize;
end;

{********************************}
procedure TALRectangle.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // if csDestroying do not do autosize
     (FAutoSize) then // if FAutoSize is false nothing to adjust
    ALAutoSize(Self);
end;

{***********************************************}
constructor TALCircle.Create(AOwner: TComponent);
begin
  inherited;
  fDoubleBuffered := true;
  fBufDrawable := ALNullDrawable;
end;

{***************************}
destructor TALCircle.Destroy;
begin
  clearBufDrawable;
  inherited;
end;

{***********************************}
procedure TALCircle.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
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
    if not fDoubleBuffered then clearBufDrawable;
  end;
end;

{***********************************************}
procedure TALCircle.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{*************************************************}
procedure TALCircle.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{*************************************************}
procedure TALCircle.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
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

      ALDrawCircle(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
        ABufDrawableRect, // const Rect: TrectF;
        1, // const AOpacity: Single;
        AFill, // const Fill: TALBrush;
        AStateLayer, // const StateLayer: TALStateLayer;
        AStateLayerContentColor, // const AStateLayerContentColor: TAlphaColor;
        ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow); // const Shadow: TALShadow

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
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

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

{************************}
procedure TALCircle.Paint;
begin

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    ALDrawCircle(
      TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
      TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), // const Rect: TrectF;
      AbsoluteOpacity, // const AOpacity: Single;
      Fill, // const Fill: TALBrush;
      nil, // const StateLayer: TALStateLayer;
      TAlphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
      True, // const ADrawStateLayerOnTop: Boolean;
      Stroke, // const Stroke: TALStrokeBrush;
      Shadow); // const Shadow: TALShadow
    {$ELSE}
    {$IF defined(DEBUG)}
    if not doublebuffered then begin
      ALLog('TALCircle.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end;
    {$ENDIF}
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***********************************************************}
function TALCircle.PointInObjectLocal(X, Y: Single): Boolean;
begin
  var LRect := TRectF.Create(0, 0, 1, 1).FitInto(LocalRect);
  if LRect.Width * LRect.Height = 0 then Result := False
  else Result := (Sqr((X * 2 - LRect.Width) / LRect.Width) + Sqr((Y * 2 - LRect.Height) / LRect.Height) <= 1);
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
  fBufDrawable := ALNullDrawable;
end;

{*************************}
destructor TALLine.Destroy;
begin
  clearBufDrawable;
  inherited;
end;

{*********************************}
procedure TALLine.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
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
    if not fDoubleBuffered then clearBufDrawable;
  end;
end;

{*********************************************}
procedure TALLine.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{***********************************************}
procedure TALLine.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
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
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) then exit;

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

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) and Stroke.HasStroke then begin
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
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
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

{*************************************************}
constructor TALBaseText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //-----
  fDoubleBuffered := true;
  FMultiLineTextOptions := TALMultiLineTextOptions.Create;
  //-----
  FOnElementClick := nil;
  FOnElementDblClick := nil;
  FOnElementMouseDown := nil;
  FOnElementMouseMove := nil;
  FOnElementMouseUp := nil;
  FOnElementMouseEnter := nil;
  FOnElementMouseLeave := nil;
  FHoveredElement := TALTextElement.Empty;
  FPressedElement := TALTextElement.Empty;
  //-----
  fBufDrawable := ALNullDrawable;
  //-----
  fTextBroken := False;
  fAllTextDrawn := False;
  SetLength(fElements, 0);
  //-----
  var LPrevFillOnchanged := Fill.OnChanged;
  Fill.OnChanged := nil;
  Fill.DefaultColor := TAlphaColors.Null;
  Fill.Color := Fill.DefaultColor;
  Fill.OnChanged := LPrevFillOnchanged;
  var LPrevStrokeOnchanged := Stroke.OnChanged;
  Stroke.OnChanged := nil;
  Stroke.DefaultColor := TAlphaColors.Null;
  Stroke.Color := Stroke.DefaultColor;
  Stroke.OnChanged := LPrevStrokeOnchanged;
  FCorners := AllCorners;
  FDefaultXRadius := 0;
  FDefaultYRadius := 0;
  FXRadius := FDefaultXRadius;
  FYRadius := FDefaultYRadius;
  FSides := AllSides;
  //-----
  HitTest := False;
  //-----
  FAutoTranslate := true;
  FAutoSize := False;
  FIsAdjustingSize := False;
  FMaxWidth := 65535;
  FMaxHeight := 65535;
  FText := '';
  //-----
  FTextSettings := CreateTextSettings;
  FTextSettings.OnChanged := TextSettingsChanged;
end;

{*****************************}
destructor TALBaseText.Destroy;
begin
  ClearBufDrawable;
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FMultiLineTextOptions);
  inherited;
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

  if (TextSettings.Font.AutoConvert) and
     (TextSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
    TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family);

  if (TextSettings.EllipsisSettings.Font.AutoConvert) and
     (TextSettings.EllipsisSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
    TextSettings.EllipsisSettings.Font.Family := ALConvertFontFamily(TextSettings.EllipsisSettings.Font.Family);

  inherited Loaded;

  AdjustSize;
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

{************************************************}
procedure TALBaseText.SetNewScene(AScene: IScene);
begin
  inherited SetNewScene(AScene);
  // At design time, when a new TALBaseText control with AutoSize=true
  // is added to the form, the size will not adjust and will remain
  // at its default (200x50). Calling AdjustSize here will correct this.
  AdjustSize;
end;

{******************************}
procedure TALBaseText.DoResized;
begin
  if not FIsAdjustingSize then begin
    ClearBufDrawable;
    inherited;
    AdjustSize;
  end
  else
    inherited;
end;

{********************************}
procedure TALBaseText.DoEndUpdate;
begin
  AdjustSize;
  inherited DoEndUpdate;
end;

{*******************************}
procedure TALBaseText.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // if csDestroying do not do autosize
     (not isupdating) and // DoEndUpdate will call again AdjustSize
     (HasUnconstrainedAutosizeX or HasUnconstrainedAutosizeY) and // if AutoSize is false nothing to adjust
     (Text <> '') and // if Text is empty do not do autosize
     (scene <> nil) then begin // SetNewScene will call again AdjustSize
    FIsAdjustingSize := True;
    try

      var R: TrectF;
      If {$IF not DEFINED(ALDPK)}DoubleBuffered{$ELSE}True{$ENDIF} then begin
        MakeBufDrawable;
        R := FBufDrawableRect;
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
          Shadow); // const AShadow: TALShadow);
      end;

      if not HasUnconstrainedAutosizeX then begin
        r.Left := 0;
        r.Width := Width;
      end;
      if not HasUnconstrainedAutosizeY then begin
        r.Top := 0;
        r.height := height;
      end;

      SetBounds(Position.X, Position.Y, R.Width, R.Height);

    finally
      FIsAdjustingSize := False;
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
     assigned(FOnElementClick) or
     assigned(FOnElementDblClick) then begin

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

{**************************}
procedure TALBaseText.Click;
begin
  inherited Click;
  if assigned(FOnElementClick) and (FPressedElement.ID <> '') then
    FOnElementClick(Self, FPressedElement);
end;

{*****************************}
procedure TALBaseText.DblClick;
begin
  inherited DblClick;
  if assigned(FOnElementDblClick) and (FPressedElement.ID <> '') then
    FOnElementDblClick(Self, FPressedElement);
end;

{***********************************}
procedure TALBaseText.PaddingChanged;
begin
  clearBufDrawable;
  inherited;
  AdjustSize;
  Repaint;
end;

{*************************************************}
procedure TALBaseText.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{***************************************************}
procedure TALBaseText.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{***************************************************}
procedure TALBaseText.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{********************************************}
function TALBaseText.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{******************************************}
function TALBaseText.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

{******************************************************}
procedure TALBaseText.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then begin
    clearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{****************************************************}
procedure TALBaseText.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FXRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FXRadius := NewValue;
    Repaint;
  end;
end;

{****************************************************}
procedure TALBaseText.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FYRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FYRadius := NewValue;
    Repaint;
  end;
end;

{**************************************************}
procedure TALBaseText.SetSides(const Value: TSides);
begin
  if FSides <> Value then begin
    clearBufDrawable;
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

{***********************************}
function TALBaseText.GetData: TValue;
begin
  Result := Text;
end;

{*************************************************}
procedure TALBaseText.SetData(const Value: TValue);
begin
  Text := Value.ToString;
end;

{**************************}
procedure TALBaseText.Paint;
begin

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) then begin
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
      Shadow); // const AShadow: TALShadow);
    {$ELSE}
    {$IF defined(DEBUG)}
    if not doublebuffered then begin
      ALLog('TALBaseText.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end;
    {$ENDIF}
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;

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
    if not fDoubleBuffered then clearBufDrawable;
  end;
end;

{********************************************************}
procedure TALBaseText.SetAlign(const Value: TAlignLayout);
begin
  if Align <> Value then clearBufDrawable;
  inherited SetAlign(Value);
end;

{****************************************}
function TALBaseText.GetAutoSize: Boolean;
begin
  Result := FAutoSize;
end;

{******************************************************}
function TALBaseText.HasUnconstrainedAutosizeX: Boolean;
begin
  Result := (GetAutoSize) and
            (not (Align in [TAlignLayout.Client,
                            TAlignLayout.Contents,
                            TAlignLayout.Top,
                            TAlignLayout.Bottom,
                            TAlignLayout.MostTop,
                            TAlignLayout.MostBottom,
                            TAlignLayout.Horizontal,
                            TAlignLayout.VertCenter]));
end;

{******************************************************}
function TALBaseText.HasUnconstrainedAutosizeY: Boolean;
begin
  Result := (GetAutoSize) and
            (not (Align in [TAlignLayout.Client,
                            TAlignLayout.Contents,
                            TAlignLayout.Left,
                            TAlignLayout.Right,
                            TAlignLayout.MostLeft,
                            TAlignLayout.MostRight,
                            TAlignLayout.Vertical,
                            TAlignLayout.HorzCenter]));
end;

{******************************************************}
procedure TALBaseText.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    clearBufDrawable;
    FAutoSize := Value;
    AdjustSize;
    repaint;
  end;
end;

{*************************************************}
procedure TALBaseText.SetText(const Value: string);
begin
  if FText <> Value then begin
    clearBufDrawable;
    FText := Value;
    AdjustSize;
    Repaint;
  end;
end;

{*************************************}
procedure TALBaseText.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
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
           const AShadow: TALShadow): TALMultiLineTextOptions;
begin
  Result := FMultiLineTextOptions;
  Result.Scale := AScale;
  Result.AlignToPixel := IsPixelAlignmentEnabled;
  Result.Opacity := AOpacity;
  //--
  Result.FontFamily := Afont.Family;
  Result.FontSize := Afont.Size;
  Result.FontWeight := Afont.Weight;
  Result.FontSlant := Afont.Slant;
  Result.FontStretch := Afont.Stretch;
  Result.FontColor := AFont.Color;
  //--
  Result.DecorationKinds := ADecoration.Kinds;
  Result.DecorationStyle := ADecoration.Style;
  Result.DecorationThicknessMultiplier := ADecoration.ThicknessMultiplier;
  Result.DecorationColor := ADecoration.Color;
  //--
  Result.EllipsisText := TextSettings.Ellipsis;
  Result.EllipsisInheritSettings := TextSettings.EllipsisSettings.inherit;
  //--
  Result.EllipsisFontFamily := AEllipsisfont.Family;
  Result.EllipsisFontSize := AEllipsisfont.Size;
  Result.EllipsisFontWeight := AEllipsisfont.Weight;
  Result.EllipsisFontSlant := AEllipsisfont.Slant;
  Result.EllipsisFontStretch := AEllipsisfont.Stretch;
  Result.EllipsisFontColor := AEllipsisFont.Color;
  //--
  Result.EllipsisDecorationKinds := AEllipsisDecoration.Kinds;
  Result.EllipsisDecorationStyle := AEllipsisDecoration.Style;
  Result.EllipsisDecorationThicknessMultiplier := AEllipsisDecoration.ThicknessMultiplier;
  Result.EllipsisDecorationColor := AEllipsisDecoration.Color;
  //--
  Result.AutoSize := False;
  Result.AutoSizeX := False;
  Result.AutoSizeY := False;
  if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then Result.AutoSize := True
  else if HasUnconstrainedAutosizeX then Result.AutoSizeX := True
  else if HasUnconstrainedAutosizeY then Result.AutoSizeY := True;
  //--
  Result.MaxLines := TextSettings.MaxLines;
  Result.LineHeightMultiplier := TextSettings.LineHeightMultiplier;
  Result.LetterSpacing := TextSettings.LetterSpacing;
  Result.Trimming := TextSettings.Trimming;
  Result.FailIfTextBroken := false;
  //--
  if TFillTextFlag.RightToLeft in FillTextFlags then Result.Direction := TALTextDirection.RightToLeft
  else Result.Direction := TALTextDirection.LeftToRight;
  Result.HTextAlign := TextSettings.HorzAlign;
  Result.VTextAlign := TextSettings.VertAlign;
  //--
  Result.FillColor := AFill.Color;
  Result.FillGradientStyle := AFill.Gradient.Style;
  Result.FillGradientColors := AFill.Gradient.Colors;
  Result.FillGradientOffsets := AFill.Gradient.Offsets;
  Result.FillGradientAngle := AFill.Gradient.Angle;
  Result.FillResourceName := AFill.ResourceName;
  Result.FillWrapMode := AFill.WrapMode;
  Result.FillBackgroundMargins := AFill.BackgroundMargins.Rect;
  Result.FillImageMargins := AFill.ImageMargins.Rect;
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
    Result.StateLayerXRadius := 0;
    Result.StateLayerYRadius := 0;
  end;
  //--
  Result.StrokeColor := AStroke.Color;
  Result.StrokeThickness := AStroke.Thickness;
  //--
  Result.ShadowColor := AShadow.Color;
  Result.ShadowBlur := AShadow.Blur;
  Result.ShadowOffsetX := AShadow.OffsetX;
  Result.ShadowOffsetY := AShadow.OffsetY;
  //--
  Result.Sides := Sides;
  Result.XRadius := XRadius;
  Result.YRadius := YRadius;
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
            const AShadow: TALShadow);
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
                                 AShadow);

  if (AText <> '') then begin

    var LMaxSize: TSizeF;
    if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(maxWidth, maxHeight)
    else if HasUnconstrainedAutosizeX then LMaxSize := TSizeF.Create(maxWidth, Height)
    else if HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(Width, maxHeight)
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

    ARect.Width := Min(MaxWidth, ARect.Width);
    ARect.Height := Min(MaxHeight, ARect.Height);

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

    ALDrawRectangle(
      ACanvas, // const ACanvas: TALCanvas;
      AScale, // const AScale: Single;
      IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
      ARect, // const Rect: TrectF;
      AOpacity, // const AOpacity: Single;
      AFill, // const Fill: TALBrush;
      AStateLayer, // const StateLayer: TALStateLayer;
      AFont.color, // const AStateLayerContentColor: TAlphaColor;
      AText <> '', // const ADrawStateLayerOnTop: Boolean;
      AStroke, // const Stroke: TALStrokeBrush;
      AShadow, // const Shadow: TALShadow
      Sides, // const Sides: TSides;
      Corners, // const Corners: TCorners;
      XRadius, // const XRadius: Single = 0;
      YRadius); // const YRadius: Single = 0);

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
            const AShadow: TALShadow);
begin
  var LMaxSize: TSizeF;
  if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(maxWidth, maxHeight)
  else if HasUnconstrainedAutosizeX then LMaxSize := TSizeF.Create(maxWidth, Height)
  else if HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(Width, maxHeight)
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
      AShadow));
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
            const AShadow: TALShadow);
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
                                 AShadow);

  if (AText <> '') then begin

    var LMaxSize: TSizeF;
    if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(maxWidth, maxHeight)
    else if HasUnconstrainedAutosizeX then LMaxSize := TSizeF.Create(maxWidth, Height)
    else if HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(Width, maxHeight)
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
                          AFill, // const AFill: TALBrush;
                          AStateLayer, // const AStateLayer: TALStateLayer;
                          AShadow); // const AShadow: TALShadow): TRectF;
    LSurfaceRect.Width := Min(MaxWidth, LSurfaceRect.Width);
    LSurfaceRect.Height := Min(MaxHeight, LSurfaceRect.Height);
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

        ALDrawRectangle(
          LCanvas, // const ACanvas: TALCanvas;
          AScale, // const AScale: Single;
          IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
          ABufDrawableRect, // const Rect: TrectF;
          1, // const AOpacity: Single;
          AFill, // const Fill: TALBrush;
          AStateLayer, // const StateLayer: TALStateLayer;
          AFont.color, // const AStateLayerContentColor: TAlphaColor;
          Atext <> '', // const ADrawStateLayerOnTop: Boolean;
          AStroke, // const Stroke: TALStrokeBrush;
          AShadow, // const Shadow: TALShadow
          Sides, // const Sides: TSides;
          Corners, // const Corners: TCorners;
          XRadius, // const XRadius: Single = 0;
          YRadius); // const YRadius: Single = 0);

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
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

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
    Shadow); // const AShadow: TALShadow);

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
    clearBufDrawable;
    fMaxWidth := Value;
    AdjustSize;
  end;
end;

{******************************************************}
procedure TALBaseText.SetMaxHeight(const Value: Single);
begin
  if compareValue(fMaxHeight, Value, Tepsilon.position) <> 0 then begin
    clearBufDrawable;
    fMaxHeight := Value;
    AdjustSize;
  end;
end;

{*********************************************}
function TALBaseText.IsMaxWidthStored: Boolean;
begin
  result := compareValue(fMaxWidth, 65535, Tepsilon.position) <> 0;
end;

{**********************************************}
function TALBaseText.IsMaxHeightStored: Boolean;
begin
  result := compareValue(fMaxHeight, 65535, Tepsilon.position) <> 0;
end;

{********************************************}
function TALBaseText.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, FDefaultXRadius, TEpsilon.Vector);
end;

{********************************************}
function TALBaseText.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, FDefaultYRadius, TEpsilon.Vector);
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
  else LFontFamily := ALConvertFontFamily(LFontFamily);
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

{**************************************************}
//unfortunatly the way the beginupdate/endupdate and
//realign work is not very efficient for TALText.
//because when we do endupdate then we will first
//call endupdate to the most far away childreen:
//  Control1
//    Control2
//      AlText1
//So when we do Control1.endupdate we will do in this order :
//      AlText1.endupdate => adjustsize and realign
//    Control2.endupdate => realign and then maybe again AlText1.adjustsize
//  Control1.endupdate => realign and then maybe again AlText1.adjustsize
//this is a problem because we will calculate several time the BufDrawable
//to mitigate this we can do
//  ALLockTexts(Control1);
//  Control1.endupdate;
//  ALUnLockTexts(Control1);
procedure ALLockTexts(const aParentControl: Tcontrol);
begin
  if aParentControl is TalText then begin
    aParentControl.BeginUpdate;
    exit;
  end;
  for var I := 0 to aParentControl.Controls.Count - 1 do
    ALLockTexts(aParentControl.Controls[i]);
end;

{******************************************************}
procedure ALUnLockTexts(const aParentControl: Tcontrol);
begin
  if aParentControl is TalText then begin
    aParentControl.EndUpdate;
    exit;
  end;
  for var I := 0 to aParentControl.Controls.Count - 1 do
    ALUnLockTexts(aParentControl.Controls[i]);
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALImage, TALAnimatedImage, TALRectangle, TALCircle, TALLine, TALText]);
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
  RegisterFmxClasses([TALImage, TALRectangle, TALCircle, TALLine, TALText]);

end.
