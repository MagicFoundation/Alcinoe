unit Alcinoe.FMX.Objects;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported120}
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
  FMX.controls,
  FMX.types,
  FMX.graphics,
  FMX.objects,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALShape = class(TALControl)
  strict private
    FFill: TALBrush;
    FStroke: TALStrokeBrush;
    fShadow: TALShadow;
    procedure SetFill(const Value: TALBrush);
    procedure SetStroke(const Value: TALStrokeBrush);
    procedure SetShadow(const Value: TALShadow);
  protected
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure ShadowChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Fill: TALBrush read FFill write SetFill;
    property Stroke: TALStrokeBrush read FStroke write SetStroke;
    property Shadow: TALShadow read fshadow write SetShadow;
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
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    // under Android, work only when setting a file in ResourceName
    property RotateAccordingToExifOrientation: Boolean read fRotateAccordingToExifOrientation write fRotateAccordingToExifOrientation default false;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    // If a file extension (e.g., .xxx) is detected in ResourceName, the image is loaded from the
    // specified file (With the full path of the file obtained using ALGetResourceFilename).
    // In debug mode, the image is loaded from a file located in the /Resources/ sub-folder of the
    // project directory (with the extensions .png or .jpg).
    property ResourceName: String read fResourceName write setResourceName;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode default TALImageWrapMode.Fit;
    property ParentShowHint;
    property ShowHint;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
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
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
  protected
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
                var ABufDrawableRect: TRectF;
                const AFill: TALBrush;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow); virtual;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Fill;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Shadow;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property Size;
    property Stroke;
    property Visible default True;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Width;
    property ParentShowHint;
    property ShowHint;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
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
    // Dynamically adjusts the dimensions to accommodate child controls,
    // considering their sizes, positions, margins, and alignments.
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    property DoubleBuffered;
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
                var ABufDrawableRect: TRectF;
                const AFill: TALBrush;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow);
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    function PointInObjectLocal(X, Y: Single): Boolean; override;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Fill;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Shadow;
    property Size;
    property Stroke;
    property Visible default True;
    property Width;
    property ParentShowHint;
    property ShowHint;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
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
    procedure SeTALLineType(const Value: TALLineType);
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
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property LineType: TALLineType read FLineType write SeTALLineType;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Stroke;
    property Visible default True;
    property Width;
    property ParentShowHint;
    property ShowHint;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
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
    fBufTextBroken: Boolean;
    fBufAllTextDrawn: Boolean;
    fBufElements: TALTextElements;
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
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    procedure SetText(const Value: string);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
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
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                var ABufDrawableRect: TRectF;
                var ABufTextBroken: Boolean;
                var ABufAllTextDrawn: Boolean;
                var ABufElements: TALTextElements;
                const AText: String;
                const AFont: TALFont;
                const ADecoration: TALTextDecoration;
                const AEllipsisFont: TALFont;
                const AEllipsisDecoration: TALTextDecoration;
                const AFill: TALBrush;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow);
    property Elements: TALTextElements read fBufElements;
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
    procedure SetNewScene(AScene: IScene); override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    function TextBroken: Boolean;
  published
    property Align;
    property Anchors;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Text: string read FText write SetText;
    property Visible default True;
    property Width;
    property MaxWidth: Single read fMaxWidth write SetMaxWidth stored IsMaxWidthStored nodefault;
    property MaxHeight: Single read fMaxHeight write SetMaxHeight stored IsMaxHeightStored nodefault;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Mouse events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property Fill;
    property Stroke;
    property shadow;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property TouchTargetExpansion;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALText = class(TALBaseText)
  private
    function GetTextSettings: TALTextSettings;
  protected
    procedure SetTextSettings(const Value: TALTextSettings); reintroduce;
    function CreateTextSettings: TALBaseTextSettings; override;
  public
    property Elements;
  published
    property TextSettings: TALTextSettings read GetTextSettings write SetTextSettings;
    property OnElementClick;
    property OnElementDblClick;
    property OnElementMouseDown;
    property OnElementMouseMove;
    property OnElementMouseUp;
    property OnElementMouseEnter;
    property OnElementMouseLeave;
  end;

procedure ALLockTexts(const aParentControl: Tcontrol);
procedure ALUnLockTexts(const aParentControl: Tcontrol);

procedure Register;

implementation

uses
  system.SysUtils,
  system.Math,
  system.Math.Vectors,
  fmx.platform,
  {$IF defined(ALSkiaEngine)}
  System.Skia.API,
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
  Alcinoe.FMX.Types3D,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  system.ioutils,
  {$ENDIF}
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

{**********************************************}
destructor TALShape.Destroy;
begin
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FFill);
  ALFreeAndNil(fShadow);
  inherited;
end;

{**********************************************}
procedure TALShape.FillChanged(Sender: TObject);
begin
  if FUpdating = 0 then
    Repaint;
end;

{**********************************************}
procedure TALShape.StrokeChanged(Sender: TObject);
begin
  if FUpdating = 0 then
    Repaint;
end;

{************************************************}
procedure TALShape.ShadowChanged(Sender: TObject);
begin
  if FUpdating = 0 then
    Repaint;
end;

{**********************************************}
procedure TALShape.SetFill(const Value: TALBrush);
begin
  FFill.Assign(Value);
end;

{******************************************************}
procedure TALShape.SetStroke(const Value: TALStrokeBrush);
begin
  FStroke.Assign(Value);
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

{********************************}
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

  if (Size.Size.IsZero) or // Do not create BufDrawable if the size is 0
     (fResourceName = '') // Do not create BufDrawable if fResourceName is empty
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
  else fBufDrawableRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
                                                                                      // TalExifOrientationInfo.FLIP_HORIZONTAL:;
                                                                                      // TalExifOrientationInfo.FLIP_VERTICAL:;
                                                                                      // TalExifOrientationInfo.NORMAL:;
                                                                                      // TalExifOrientationInfo.ROTATE_180:;
                                                                                      // TalExifOrientationInfo.UNDEFINED:;

  if LFileName <> '' then fBufDrawable := ALLoadFromFileAndWrapToDrawable(LFileName, FWrapMode, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
  else fBufDrawable := {$IFDEF ALDPK}ALNullDrawable{$ELSE}ALLoadFromResourceAndWrapToDrawable(fResourceName, FWrapMode, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale){$ENDIF};

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

{***********************************************}
function TALImage.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{**************************************************************}
procedure TALImage.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not yet supported
end;

{**************************************************}
constructor TALBaseRectangle.Create(AOwner: TComponent);
begin
  inherited;
  fDoubleBuffered := true;
  FXRadius := 0;
  FYRadius := 0;
  FCorners := AllCorners;
  FSides := AllSides;
  fBufDrawable := ALNullDrawable;
end;

{******************************}
destructor TALBaseRectangle.Destroy;
begin
  ClearBufDrawable;
  inherited;
end;

{***************************}
procedure TALBaseRectangle.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{************************************}
procedure TALBaseRectangle.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{************************************}
function TALBaseRectangle.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{************************************}
function TALBaseRectangle.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

{***********************************************}
function TALBaseRectangle.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{**************************************************************}
procedure TALBaseRectangle.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then clearBufDrawable;
  end;
end;

{****************************************************}
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

{****************************************************}
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

{*******************************************************}
procedure TALBaseRectangle.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    clearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{***************************************************}
procedure TALBaseRectangle.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    clearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{**************************************************}
procedure TALBaseRectangle.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{****************************************************}
procedure TALBaseRectangle.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{****************************************************}
procedure TALBaseRectangle.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{***************************************}
Procedure TALBaseRectangle.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            var ABufDrawableRect: TRectF;
            const AFill: TALBrush;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
  var LRect := TrectF.Create(0, 0, ABufDrawableRect.Width, ABufDrawableRect.height);
  if AShadow.HasShadow then begin
    var LShadowWidth := ALGetShadowWidth(AShadow.blur);
    var LShadowRect := ABufDrawableRect;
    LShadowRect.Inflate(LShadowWidth, LShadowWidth);
    LShadowRect.Offset(AShadow.OffsetX, AShadow.OffsetY);
    ABufDrawableRect := TRectF.Union(LShadowRect, ABufDrawableRect); // add the extra space needed to draw the shadow
    ABufDrawableRect := ALAlignDimensionToPixelRound(ABufDrawableRect, ALGetScreenScale); // to have the pixel aligned width and height
    LRect.Offset(Max(0, LShadowWidth - AShadow.OffsetX), Max(0, LShadowWidth - AShadow.OffsetY));
  end;

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALGetScreenScale, // const AScale: Single;
    ABufDrawableRect.Width, // const w: integer;
    ABufDrawableRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      ALDrawRectangle(
        LCanvas, // const ACanvas: TALCanvas;
        ALGetScreenScale, // const AScale: Single;
        LRect, // const Rect: TrectF;
        AFill, // const Fill: TALBrush;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow, // const Shadow: TALShadow
        Sides, // const Sides: TSides;
        Corners, // const Corners: TCorners;
        XRadius, // const XRadius: Single = 0;
        YRadius); // const YRadius: Single = 0);

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALSurfaceToDrawable(LSurface);

  finally
    ALFreeSurface(LSurface, LCanvas);
  end;

end;

{*************************************}
procedure TALBaseRectangle.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     (not DoubleBuffered) or
     //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if only fill with solid color
     (((not Stroke.HasStroke) or
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
    Fill, // const AFill: TALBrush;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{***************************}
procedure TALBaseRectangle.Paint;
begin

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    var LRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
    LRect := TrectF.Create(0, 0, LRect.Width, LRect.height);
    ALDrawRectangle(
      TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      Canvas.AlignToPixel(LRect), // const Rect: TrectF;
      Fill, // const Fill: TALBrush;
      Stroke, // const Stroke: TALStrokeBrush;
      Shadow, // const Shadow: TALShadow
      Sides, // const Sides: TSides;
      Corners, // const Corners: TCorners;
      XRadius, // const XRadius: Single = 0;
      YRadius); // const YRadius: Single = 0);
    {$ELSE}
    {$IF defined(DEBUG)}
    if not doublebuffered then
      raise Exception.Create('Controls that are not double-buffered only work when SKIA is enabled.');
    {$ENDIF}
    If Fill.Styles = [TALBrushStyle.Solid] then begin
      Canvas.Fill.kind := TBrushKind.solid;
      Canvas.Fill.color := Fill.color;
      Canvas.FillRect(Canvas.AlignToPixel(LocalRect), XRadius, YRadius, FCorners, AbsoluteOpacity, TCornerType.Round);
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

{****************************************************}
procedure TALRectangle.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
    repaint;
  end;
end;

{****************************}
procedure TALRectangle.DoRealign;
begin
  inherited DoRealign;
  AdjustSize;
end;

{*****************************}
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

{*********************************}
procedure TALCircle.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{***********************************************}
function TALCircle.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{**************************************************************}
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

{***************************************}
Procedure TALCircle.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            var ABufDrawableRect: TRectF;
            const AFill: TALBrush;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), ALGetScreenScale); // to have the pixel aligned width and height
  var LRect := TrectF.Create(0, 0, ABufDrawableRect.Width, ABufDrawableRect.height);
  if AShadow.HasShadow then begin
    var LShadowWidth := ALGetShadowWidth(AShadow.blur);
    var LShadowRect := ABufDrawableRect;
    LShadowRect.Inflate(LShadowWidth, LShadowWidth);
    LShadowRect.Offset(AShadow.OffsetX, AShadow.OffsetY);
    ABufDrawableRect := TRectF.Union(LShadowRect, ABufDrawableRect); // add the extra space needed to draw the shadow
    ABufDrawableRect := ALAlignDimensionToPixelRound(ABufDrawableRect, ALGetScreenScale); // to have the pixel aligned width and height
    LRect.Offset(Max(0, LShadowWidth - AShadow.OffsetX), Max(0, LShadowWidth - AShadow.OffsetY));
  end;

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALGetScreenScale, // const AScale: Single;
    ABufDrawableRect.Width, // const w: integer;
    ABufDrawableRect.Height); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      ALDrawCircle(
        LCanvas, // const ACanvas: TALCanvas;
        ALGetScreenScale, // const AScale: Single;
        LRect, // const Rect: TrectF;
        AFill, // const Fill: TALBrush;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow); // const Shadow: TALShadow

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALSurfaceToDrawable(LSurface);

  finally
    ALFreeSurface(LSurface, LCanvas);
  end;

end;

{**********************************}
procedure TALCircle.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     (not DoubleBuffered) or
     //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) then begin
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    Fill, // const AFill: TALBrush;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{************************}
procedure TALCircle.Paint;
begin

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    var LRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), ALGetScreenScale); // to have the pixel aligned width and height
    LRect := TrectF.Create(0, 0, LRect.Width, LRect.height);
    ALDrawCircle(
      TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      Canvas.AlignToPixel(LRect), // const Rect: TrectF;
      Fill, // const Fill: TALBrush;
      Stroke, // const Stroke: TALStrokeBrush;
      Shadow); // const Shadow: TALShadow
    {$ELSE}
    {$IF defined(DEBUG)}
    if not doublebuffered then
      raise Exception.Create('Controls that are not double-buffered only work when SKIA is enabled.');
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

{************************************************************}
function TALCircle.PointInObjectLocal(X, Y: Single): Boolean;
begin
  var LRect := TRectF.Create(0, 0, 1, 1).FitInto(LocalRect);
  if LRect.Width * LRect.Height = 0 then Result := False
  else Result := (Sqr((X * 2 - LRect.Width) / LRect.Width) + Sqr((Y * 2 - LRect.Height) / LRect.Height) <= 1);
end;

{***************************}
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

{*******************************}
procedure TALLine.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{***********************************************}
function TALLine.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{**************************************************************}
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
     (not DoubleBuffered) or
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
  var LRect := TrectF.Create(0, 0, round(fBufDrawableRect.Width * ALGetScreenScale), round(fBufDrawableRect.height * ALGetScreenScale));

  {$IF DEFINED(ALSkiaEngine)}

  var LSurface: sk_surface_t;
  var LCanvas: sk_canvas_t;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    round(LRect.Width), // const w: integer;
    round(LRect.Height)); // const h: integer)
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

    fBufDrawable := ALSurfaceToDrawable(LSurface);

  finally
    ALFreeSurface(LSurface, LCanvas);
  end;

  {$ELSEIF DEFINED(ANDROID)}

  var LSurface: Jbitmap;
  var LCanvas: Jcanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    round(LRect.Width), // const w: integer;
    round(LRect.Height)); // const h: integer)
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

    fBufDrawable := ALSurfaceToDrawable(LSurface);

  finally
    ALFreeSurface(LSurface, LCanvas);
  end;

  {$ELSEIF DEFINED(IOS)}

  var LGridHeight := round(LRect.Height);
  var LSurface: CGContextRef;
  var LCanvas: CGContextRef;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    round(LRect.Width), // const w: integer;
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

    fBufDrawable := ALSurfaceToDrawable(LCanvas);

  finally
    ALFreeSurface(LSurface, LCanvas); // Var aContext: CGContextRef;
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
    Canvas.DrawLine(Canvas.AlignToPixel(LPt1), Canvas.AlignToPixel(LPt2), AbsoluteOpacity);
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

{****************************************************}
procedure TALLine.SeTALLineType(const Value: TALLineType);
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
  FXRadius := 0;
  FYRadius := 0;
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
  inherited;
end;

{***************************}
procedure TALBaseText.Loaded;
begin
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

{************************************************}
procedure TALBaseText.SetNewScene(AScene: IScene);
begin
  inherited SetNewScene(AScene);
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

      MakeBufDrawable;
      var R := FBufDrawableRect;

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
  if (not ALIsDrawableNull(fBufDrawable)) then begin
    for var I := Low(fBufElements) to High(fBufElements) do
      if fBufElements[i].Rect.Contains(APos) then begin
        Result := fBufElements[i];
        Exit;
      end;
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

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;

end;

{***********************************************}
function TALBaseText.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{**************************************************************}
procedure TALBaseText.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not yet supported
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
  setlength(fBufElements, 0);
end;

{**************************************}
Procedure TALBaseText.CreateBufDrawable(
           var ABufDrawable: TALDrawable;
           var ABufDrawableRect: TRectF;
           var ABufTextBroken: Boolean;
           var ABufAllTextDrawn: Boolean;
           var ABufElements: TALTextElements;
           const AText: String;
           const AFont: TALFont;
           const ADecoration: TALTextDecoration;
           const AEllipsisFont: TALFont;
           const AEllipsisDecoration: TALTextDecoration;
           const AFill: TALBrush;
           const AStroke: TALStrokeBrush;
           const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var LMaxSize: TSizeF;
  if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(maxWidth, maxHeight)
  else if HasUnconstrainedAutosizeX then LMaxSize := TSizeF.Create(maxWidth, Height)
  else if HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(Width, maxHeight)
  else LMaxSize := TSizeF.Create(width, height);

  ABufDrawableRect := TRectF.Create(0, 0, LMaxSize.cX, LMaxSize.cY);

  var LOptions := TALMultiLineTextOptions.Create;
  Try

    LOptions.Scale := ALGetScreenScale;
    //--
    LOptions.FontFamily := Afont.Family;
    LOptions.FontSize := Afont.Size;
    LOptions.FontWeight := Afont.Weight;
    LOptions.FontSlant := Afont.Slant;
    LOptions.FontStretch := Afont.Stretch;
    LOptions.FontColor := AFont.Color;
    //--
    LOptions.DecorationKinds := ADecoration.Kinds;
    LOptions.DecorationStyle := ADecoration.Style;
    LOptions.DecorationThicknessMultiplier := ADecoration.ThicknessMultiplier;
    LOptions.DecorationColor := ADecoration.Color;
    //--
    LOptions.EllipsisText := TextSettings.Ellipsis;
    LOptions.EllipsisInheritSettings := TextSettings.EllipsisSettings.inherit;
    //--
    LOptions.EllipsisFontFamily := AEllipsisfont.Family;
    LOptions.EllipsisFontSize := AEllipsisfont.Size;
    LOptions.EllipsisFontWeight := AEllipsisfont.Weight;
    LOptions.EllipsisFontSlant := AEllipsisfont.Slant;
    LOptions.EllipsisFontStretch := AEllipsisfont.Stretch;
    LOptions.EllipsisFontColor := AEllipsisFont.Color;
    //--
    LOptions.EllipsisDecorationKinds := AEllipsisDecoration.Kinds;
    LOptions.EllipsisDecorationStyle := AEllipsisDecoration.Style;
    LOptions.EllipsisDecorationThicknessMultiplier := AEllipsisDecoration.ThicknessMultiplier;
    LOptions.EllipsisDecorationColor := AEllipsisDecoration.Color;
    //--
    LOptions.AutoSize := False;
    LOptions.AutoSizeX := False;
    LOptions.AutoSizeY := False;
    if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then LOptions.AutoSize := True
    else if HasUnconstrainedAutosizeX then LOptions.AutoSizeX := True
    else if HasUnconstrainedAutosizeY then LOptions.AutoSizeY := True;
    //--
    LOptions.MaxLines := TextSettings.MaxLines;
    LOptions.LineHeightMultiplier := TextSettings.LineHeightMultiplier;
    LOptions.LetterSpacing := TextSettings.LetterSpacing;
    LOptions.Trimming := TextSettings.Trimming;
    //LOptions.FailIfTextBroken: boolean; // default = false
    //--
    if TFillTextFlag.RightToLeft in FillTextFlags then LOptions.Direction := TALTextDirection.RightToLeft
    else LOptions.Direction := TALTextDirection.LeftToRight;
    LOptions.HTextAlign := TextSettings.HorzAlign;
    LOptions.VTextAlign := TextSettings.VertAlign;
    //--
    LOptions.FillColor := Fill.Color;
    LOptions.FillGradientStyle := Fill.Gradient.Style;
    LOptions.FillGradientColors := Fill.Gradient.Colors;
    LOptions.FillGradientOffsets := Fill.Gradient.Offsets;
    LOptions.FillGradientAngle := Fill.Gradient.Angle;
    LOptions.FillResourceName := Fill.ResourceName;
    LOptions.FillWrapMode := Fill.WrapMode;
    LOptions.StrokeColor := Stroke.Color;
    LOptions.StrokeThickness := Stroke.Thickness;
    LOptions.ShadowColor := Shadow.Color;
    LOptions.ShadowBlur := Shadow.Blur;
    LOptions.ShadowOffsetX := Shadow.OffsetX;
    LOptions.ShadowOffsetY := Shadow.OffsetY;
    //--
    LOptions.Sides := Sides;
    LOptions.XRadius := XRadius;
    LOptions.YRadius := YRadius;
    LOptions.Corners := Corners;
    LOptions.Padding := padding.Rect;
    //--
    LOptions.TextIsHtml := TextSettings.IsHtml;

    //build ABufDrawable
    ABufDrawable := ALCreateMultiLineTextDrawable(
                      AText,
                      ABufDrawableRect,
                      ABufTextBroken,
                      ABufAllTextDrawn,
                      ABufElements,
                      LOptions);

    //Special case where it's impossible to draw at least one char.
    //To avoid to call again ALDrawMultiLineText on each paint
    //return a "blank" drawable. Also to avoid to have a control
    //with a "zero" size, do not do any autoresize
    if (ALIsDrawableNull(ABufDrawable)) then begin
      if (not LocalRect.IsEmpty) then begin
        ABufDrawableRect := LocalRect;
        ABufDrawableRect.Width := Min(MaxWidth, ABufDrawableRect.Width);
        ABufDrawableRect.Height := Min(MaxHeight, ABufDrawableRect.Height);
        ABufDrawableRect := ALAlignDimensionToPixelRound(ABufDrawableRect, ALGetScreenScale);
        var LSurface: TALSurface;
        var LCanvas: TALCanvas;
        ALCreateSurface(
          LSurface, // out ASurface: sk_surface_t;
          LCanvas, // out ACanvas: sk_canvas_t;
          ALGetScreenScale, // const AScale: Single;
          ABufDrawableRect.Width, // const w: integer;
          ABufDrawableRect.height);// const h: integer)
        try
          if ALCanvasBeginScene(LCanvas) then
          try
            ALDrawRectangle(
              LCanvas, // const ACanvas: sk_canvas_t;
              ALGetScreenScale, // const AScale: Single;
              ABufDrawableRect, // const Rect: TrectF;
              Fill, // const Fill: TALBrush;
              Stroke, // const Stroke: TALStrokeBrush;
              Shadow, // const Shadow: TALShadow
              Sides, // const Sides: TSides;
              Corners, // const Corners: TCorners;
              XRadius, // const XRadius: Single = 0;
              YRadius); // const YRadius: Single = 0);
          finally
            ALCanvasEndScene(LCanvas)
          end;
          ABufDrawable := ALSurfaceToDrawable(LSurface);
        finally
          ALFreeSurface(LSurface, LCanvas);
        end;
      end;
    end;

  finally
    ALFreeAndNil(LOptions);
  end;

end;

{************************************}
procedure TALBaseText.MakeBufDrawable;
begin

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    FBufTextBroken, // var ABufTextBroken: Boolean;
    FBufAllTextDrawn, // var ABufAllTextDrawn: Boolean;
    FBufElements, // var ABufElements: TALTextElements;
    Text, // const AText: String;
    TextSettings.Font, // const AFont: TALFont;
    TextSettings.Decoration, // const ADecoration: TALTextDecoration;
    TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
    TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
    Fill, // const AFill: TALBrush;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

  // The shadow effect is not included in the fBufDrawableRect rectangle's dimensions.
  // However, the fBufDrawableRect rectangle is offset by the shadow's dx and dy values,
  // if a shadow is applied, to adjust for the visual shift caused by the shadow.
  fBufDrawableRect.Offset(-2*fBufDrawableRect.Left, -2*fBufDrawableRect.Top);

end;

{***************************************}
function TALBaseText.TextBroken: Boolean;
begin
  result := (not ALIsDrawableNull(fBufDrawable)) and (fBufTextBroken);
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

{*************************************************************}
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
  RegisterComponents('Alcinoe', [TALImage, TALRectangle, TALCircle, TALLine, TALText]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALRectangle, 'CornerType');
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALImage, TALRectangle, TALCircle, TALLine, TALText]);

end.
