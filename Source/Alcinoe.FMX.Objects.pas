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

  {~~~~~~~~~~~~~~~~~~}
  TALImageWrapMode = (
    // Display the image with its original dimensions:
    // * The image is placed in the upper-left corner of the rectangle of the control.
    // * If the image is larger than the control's rectangle, then only the upper-left part of the image,
    //   which fits in the rectangle of the control, is shown. The image is not resized.
    Original,

    // Best fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image
    //   (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
    //   of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
    //   or the height of the resized image is equal to the height of the rectangle of the control. The whole image
    //   should be displayed. The image is displayed centered in the rectangle of the control.
    //  * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    //   the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    Fit,

    // Stretch the image to fill the entire rectangle of the control.
    Stretch,

    // Tile (multiply) the image to cover the entire rectangle of the control:
    // * If the image is larger than the rectangle of the control, then only the
    //   upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
    // * If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
    //   (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
    //   the upper-left corner of the rectangle of the control.
    Tile,

    // Center the image to the rectangle of the control:
    // * The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
    Center,

    // Fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
    //   to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
    //   resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    // * If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
    Place,

    // Best fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image
    //   (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
    //   of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
    //   AND the height of the resized image is equal to the height of the rectangle of the control.
    //  * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    //   the rectangle of the control. Whole the image should be displayed.
    FitAndCrop);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //Under delphi, we have multi-res bitmap for Timage or Tglyph. this mean that we can gave several bitmap for several screen scale.
  //Exemple with a screen scale of 1 i will gave a bitmap of 100x100, for the screen scale of 1.5 i will gave a bitmap of 150*150, etc..
  //so taking care that most screen scale are 1, 1.5, 2, 3, and 4 we must provide 4 pictures. In 99.9999 % of the case the developer will
  //simply do a normal resize of the image (under photoshop or similar app) in the 5 of fewer screen scale (seriously is their any developer
  //who will gave radically different image between scale 1 and scale n ?)
  //But the resize algo to resize picture is quite powerful and often negligible. So if we gave only one bitmap (at the most biggest scale, 4)
  //it's must be good/powerfull and it's will reduce also the size of the app.
  //also from smartphone to tablet i notice that to keep a good ratio i must increase all the font size, and image by 15%. So using multires
  //bitmap and if i want to avoid any resize (the purpose of multires bitmap as i understand) i must have 10 bitmaps per image !!
  //so all of this to say that multi-res bitmap is a fundamentally wrong concept
  [ComponentPlatforms($FFFF)]
  TALImage = class(TALControl, IALDoubleBufferedControl)
  private
    fExifOrientationInfo: TalExifOrientationInfo;
    fRotateAccordingToExifOrientation: Boolean;
    fFileName: String;
    fResourceName: String;
    FWrapMode: TALImageWrapMode;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure setFileName(const Value: String);
    procedure setResourceName(const Value: String);
  protected
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
    property RotateAccordingToExifOrientation: Boolean read fRotateAccordingToExifOrientation write fRotateAccordingToExifOrientation default false; // << under Android, work only when using filename
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    property FileName: String read fFileName write setFileName;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseRectangle = class(TRectangle, IALDoubleBufferedControl)
  private
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    fShadow: TALShadow;
    procedure SetShadow(const Value: TALShadow);
  protected
    procedure SetXRadius(const Value: Single); override;
    procedure SetYRadius(const Value: Single); override;
    procedure SetCorners(const Value: TCorners); override;
    procedure SetSides(const Value: TSides); override;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                var ABufDrawableRect: TRectF;
                const AFill: TBrush;
                const AStroke: TStrokeBrush;
                const AShadow: TALShadow);
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
  published
    property Shadow: TALShadow read fshadow write SetShadow;
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
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCircle = class(TCircle, IALDoubleBufferedControl)
  private
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    fShadow: TALShadow;
    procedure SetShadow(const Value: TALShadow);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                var ABufDrawableRect: TRectF;
                const AFill: TBrush;
                const AStroke: TStrokeBrush;
                const AShadow: TALShadow);
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
  published
    property Shadow: TALShadow read fshadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALLine = class(TLine, IALDoubleBufferedControl)
  private
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
  protected
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
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseText = class(TALControl, IALAutosizeControl, IALDoubleBufferedControl)
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
    FFill: TBrush;
    FStroke: TStrokeBrush;
    fShadow: TALShadow;
    FAutoSize: Boolean;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
    procedure SetShadow(const Value: TALShadow);
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    procedure SetText(const Value: string);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
  protected
    FIsAdjustingSize: Boolean;
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
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure ShadowChanged(Sender: TObject); virtual;
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
                const AFill: TBrush;
                const AStroke: TStrokeBrush;
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
    property MaxWidth: Single read fMaxWidth write SetMaxWidth stored IsMaxWidthStored;
    property MaxHeight: Single read fMaxHeight write SetMaxHeight stored IsMaxHeightStored;
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
    property Fill: TBrush read FFill write SetFill;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
    property shadow: TALShadow read fshadow write SetShadow;
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
constructor TALImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;
  fRotateAccordingToExifOrientation := False;
  fFileName := '';
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
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*********************************}
procedure TALImage.MakeBufDrawable;
begin

  if (Size.Size.IsZero) or // Do not create BufDrawable if the size is 0
     ((fFileName = '') and (fResourceName = '')) // Do not create BufDrawable if  fFileName or fResourceName is empty
  then begin
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) then exit;

  {$IFDEF ALDPK}
  var LFileName: String := '';
  if fresourceName = '' then begin
    LFileName := fFileName;
    if not TFile.Exists(LFileName) then LFileName := '';
  end
  else LFileName := ALDPKGetResourceFilename(FResourceName);
  {$ENDIF}

  if (fRotateAccordingToExifOrientation) and
     (fResourceName = '') and
     (fFileName <> '') then fExifOrientationInfo := AlGetExifOrientationInfo(ffilename)
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

  case FWrapMode of

    // Display the image with its original dimensions:
    // * The image is placed in the upper-left corner of the rectangle of the control.
    // * If the image is larger than the control's rectangle, then only the upper-left part of the image,
    //   which fits in the rectangle of the control, is shown. The image is not resized.
    TALImageWrapMode.Original:
      begin
        // todo
      end;

    // Best fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image
    //   (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
    //   of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
    //   or the height of the resized image is equal to the height of the rectangle of the control. The whole image
    //   should be displayed. The image is displayed centered in the rectangle of the control.
    //  * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    //   the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    TALImageWrapMode.Fit:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufDrawable := ALLoadFromFileAndFitIntoToDrawable(LFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALNullDrawable;
        {$ELSE}
        if fResourceName <> '' then fBufDrawable := ALLoadFromResourceAndFitIntoToDrawable(fResourceName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALLoadFromFileAndFitIntoToDrawable(fFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale);
        {$ENDIF}
      end;

    // Stretch the image to fill the entire rectangle of the control.
    TALImageWrapMode.Stretch:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufDrawable := ALLoadFromFileAndStretchToDrawable(LFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALNullDrawable;
        {$ELSE}
        if fResourceName <> '' then fBufDrawable := ALLoadFromResourceAndStretchToDrawable(fResourceName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALLoadFromFileAndStretchToDrawable(fFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale);
        {$ENDIF}
      end;

    // Tile (multiply) the image to cover the entire rectangle of the control:
    // * If the image is larger than the rectangle of the control, then only the
    //   upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
    // * If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
    //   (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
    //   the upper-left corner of the rectangle of the control.
    TALImageWrapMode.Tile:
      begin
        // todo
      end;

    // Center the image to the rectangle of the control:
    // * The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
    TALImageWrapMode.Center:
      begin
        // todo
      end;

    // Fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
    //   to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
    //   resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    // * If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
    TALImageWrapMode.Place:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufDrawable := ALLoadFromFileAndPlaceIntoToDrawable(LFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALNullDrawable;
        {$ELSE}
        if fResourceName <> '' then fBufDrawable := ALLoadFromResourceAndPlaceIntoToDrawable(fResourceName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALLoadFromFileAndPlaceIntoToDrawable(fFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale);
        {$ENDIF}
      end;

    // Best fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image
    //   (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
    //   of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
    //   AND the height of the resized image is equal to the height of the rectangle of the control.
    //  * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    //   the rectangle of the control. Whole the image should be displayed.
    TALImageWrapMode.FitAndCrop:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufDrawable := ALLoadFromFileAndFitIntoAndCropToDrawable(LFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALNullDrawable;
        {$ELSE}
        if fResourceName <> '' then fBufDrawable := ALLoadFromResourceAndFitIntoAndCropToDrawable(fResourceName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALLoadFromFileAndFitIntoAndCropToDrawable(fFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale);
        {$ENDIF}
      end;

    // Error
    else
      Raise exception.Create('Error 9F61789B-F9A8-45D8-BEF4-8C7DABCCFCE7')

  end;

  if not ALIsDrawableNull(fBufDrawable) then
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

{**************************************************}
procedure TALImage.setFileName(const Value: String);
begin
  if FFileName <> Value then begin
    clearBufDrawable;
    FFileName := Value;
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

{**************************************************}
constructor TALBaseRectangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBufDrawable := ALNullDrawable;
  fShadow := TALShadow.Create;
  fShadow.OnChanged := ShadowChanged;
end;

{******************************}
destructor TALBaseRectangle.Destroy;
begin
  ClearBufDrawable;
  ALFreeAndNil(fShadow);
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
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*****************************************************}
procedure TALBaseRectangle.SetXRadius(const Value: Single);
begin
  clearBufDrawable;
  if csDesigning in ComponentState then
    inherited SetXRadius(max(-50, Value))
  else
    inherited SetXRadius(Value);
end;

{*****************************************************}
procedure TALBaseRectangle.SetYRadius(const Value: Single);
begin
  clearBufDrawable;
  if csDesigning in ComponentState then
    inherited SetYRadius(max(-50, Value))
  else
    inherited SetYRadius(Value);
end;

{*******************************************************}
procedure TALBaseRectangle.SetCorners(const Value: TCorners);
begin
  clearBufDrawable;
  inherited;
end;

{***************************************************}
procedure TALBaseRectangle.SetSides(const Value: TSides);
begin
  clearBufDrawable;
  inherited;
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
  Repaint;
end;

{***************************************}
Procedure TALBaseRectangle.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            var ABufDrawableRect: TRectF;
            const AFill: TBrush;
            const AStroke: TStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
  var LRect := TrectF.Create(0, 0, ABufDrawableRect.Width, ABufDrawableRect.height);
  if AShadow.enabled then begin
    var LShadowRect := ABufDrawableRect;
    LShadowRect.Inflate(AShadow.blur, AShadow.blur);
    LShadowRect.Offset(AShadow.OffsetX, AShadow.OffsetY);
    ABufDrawableRect := TRectF.Union(LShadowRect, ABufDrawableRect); // add the extra space needed to draw the shadow
    ABufDrawableRect := ALAlignDimensionToPixelRound(ABufDrawableRect, ALGetScreenScale); // to have the pixel aligned width and height
    LRect.Offset(Max(0, AShadow.blur - AShadow.OffsetX), Max(0, AShadow.blur - AShadow.OffsetY));
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
        AFill, // const Fill: TBrush;
        AStroke, // const Stroke: TStrokeBrush;
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

  if //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if only fill with solid color
     (((Stroke.Kind = TBrushKind.None) or
       (sides = []))
      and
      ((SameValue(xRadius, 0, TEpsilon.Vector)) or
       (SameValue(yRadius, 0, TEpsilon.Vector)) or
       (corners=[]))
      and
      (not fShadow.enabled)
      and
      (Fill.Kind in [TBrushKind.None, TBrushKind.Solid]))
  then begin
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    Fill, // const AFill: TBrush;
    Stroke, // const AStroke: TStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{***************************}
procedure TALBaseRectangle.Paint;
begin

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) then begin
    inherited paint;
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*******************************************************}
procedure TALBaseRectangle.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
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
  fBufDrawable := ALNullDrawable;
  fShadow := TalShadow.Create;
  fShadow.OnChanged := ShadowChanged;
end;

{***************************}
destructor TALCircle.Destroy;
begin
  clearBufDrawable;
  alFreeAndNil(fShadow);
  inherited;
end;

{*********************************}
procedure TALCircle.clearBufDrawable;
begin
  ALFreeAndNilDrawable(fBufDrawable);
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
  Repaint;
end;

{***************************************}
Procedure TALCircle.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            var ABufDrawableRect: TRectF;
            const AFill: TBrush;
            const AStroke: TStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), ALGetScreenScale); // to have the pixel aligned width and height
  var LRect := TrectF.Create(0, 0, ABufDrawableRect.Width, ABufDrawableRect.height);
  if AShadow.enabled then begin
    var LShadowRect := ABufDrawableRect;
    LShadowRect.Inflate(AShadow.blur, AShadow.blur);
    LShadowRect.Offset(AShadow.OffsetX, AShadow.OffsetY);
    ABufDrawableRect := TRectF.Union(LShadowRect, ABufDrawableRect); // add the extra space needed to draw the shadow
    ABufDrawableRect := ALAlignDimensionToPixelRound(ABufDrawableRect, ALGetScreenScale); // to have the pixel aligned width and height
    LRect.Offset(Max(0, AShadow.blur - AShadow.OffsetX), Max(0, AShadow.blur - AShadow.OffsetY));
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
        AFill, // const Fill: TBrush;
        AStroke, // const Stroke: TStrokeBrush;
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

  if (Size.Size.IsZero) then begin // Do not create BufDrawable if the size is 0
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    Fill, // const AFill: TBrush;
    Stroke, // const AStroke: TStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{************************}
procedure TALCircle.Paint;
begin

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) then begin
    inherited paint;
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***************************}
procedure TALCircle.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{****************************************************}
procedure TALCircle.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
end;

{*********************************************}
constructor TALLine.Create(AOwner: TComponent);
begin
  inherited;
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
  ALFreeAndNilDrawable(fBufDrawable);
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

  if (Size.Size.IsZero) or // Do not create BufDrawable if the size is 0
     (Stroke.Kind = TBrushKind.None) or // Do not create BufDrawable if stroke is none
     (SameValue(Stroke.Thickness, 0, TEpsilon.position)) then begin // Do not create BufDrawable if if Thickness is 0
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) then exit;

  //init LStrokeWidth
  var LStrokeWidth: Single;
  if (LineLocation = TLineLocation.InnerWithin) then LStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else LStrokeWidth := Stroke.Thickness;

  //init fBufDrawableRect / LRect
  case lineType of
    TLineType.Diagonal: fBufDrawableRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, LStrokeWidth), ALGetScreenScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufDrawableRect.Offset(0, -LStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, LStrokeWidth, height), ALGetScreenScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufDrawableRect.Offset(-LStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - LStrokeWidth, Width, height), ALGetScreenScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufDrawableRect.Offset(0, LStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(width - LStrokeWidth, 0, width, height), ALGetScreenScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufDrawableRect.Offset(LStrokeWidth/2, 0);
                     end;
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

        //stroke the circle
        if Stroke.Kind <> TBrushKind.None then begin

          //init LPaint
          sk4d_paint_set_stroke_width(LPaint, LStrokeWidth * ALGetScreenScale);

          //stroke with solid color
          if Stroke.Kind = TBrushKind.Solid then begin
            sk4d_paint_set_color(LPaint, Stroke.Color);
            case lineType of
              TLineType.Diagonal: begin
                var Lpoint1 := TPointF.Create(LRect.left, LRect.top);
                var Lpoint2 := TPointF.Create(LRect.right, LRect.Bottom);
                sk4d_canvas_draw_line(
                  LCanvas,
                  @Lpoint1,
                  @Lpoint2,
                  LPaint);
              end;
              //--
              TLineType.Top,
              TLineType.Bottom: begin
                var Lpoint1 := TPointF.Create(LRect.left, (LRect.bottom - LRect.top) / 2);
                var Lpoint2 := TPointF.Create(LRect.right, (LRect.bottom - LRect.top) / 2);
                sk4d_canvas_draw_line(
                  LCanvas,
                  @Lpoint1,
                  @Lpoint2,
                  LPaint);
              end;
              //--
              TLineType.Left,
              TLineType.Right: begin
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

      //stroke the circle
      if Stroke.Kind <> TBrushKind.None then begin

        //init LPaint
        LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
        LPaint.setStrokeWidth(LStrokeWidth * ALGetScreenScale);

        //stroke with solid color
        if Stroke.Kind = TBrushKind.Solid then begin
          LPaint.setColor(integer(Stroke.Color));
          case lineType of
            TLineType.Diagonal: LCanvas.drawLine(
                                  LRect.left {startX},
                                  LRect.top {startY},
                                  LRect.right {stopX},
                                  LRect.Bottom {stopY},
                                  LPaint);
            TLineType.Top,
            TLineType.Bottom: LCanvas.drawLine(
                                LRect.left {startX},
                                (LRect.bottom - LRect.top) / 2 {startY},
                                LRect.right {stopX},
                                (LRect.bottom - LRect.top) / 2 {stopY},
                                LPaint);
            TLineType.Left,
            TLineType.Right: LCanvas.drawLine(
                               (LRect.right - LRect.left) / 2 {startX},
                               LRect.top {startY},
                               (LRect.right - LRect.left) / 2 {stopX},
                               LRect.bottom {stopY},
                               LPaint);
          end;
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

      //stroke the circle
      if Stroke.Kind <> TBrushKind.None then begin

        //stroke with solid color
        if Stroke.Kind = TBrushKind.Solid then begin
          var LAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
          CGContextSetRGBStrokeColor(LCanvas, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
          CGContextSetLineWidth(LCanvas, Stroke.Thickness * ALGetScreenScale);
          case lineType of
            TLineType.Diagonal: begin
                                  CGContextBeginPath(LCanvas);
                                  CGContextMoveToPoint(LCanvas, LRect.left, LGridHeight - LRect.top);
                                  CGContextAddLineToPoint(LCanvas, LRect.right, LGridHeight - LRect.Bottom);
                                end;
            TLineType.Top,
            TLineType.Bottom: begin
                                CGContextBeginPath(LCanvas);
                                CGContextMoveToPoint(LCanvas, LRect.left, LGridHeight - ((LRect.bottom - LRect.top) / 2));
                                CGContextAddLineToPoint(LCanvas, LRect.right, LGridHeight - ((LRect.bottom - LRect.top) / 2));
                              end;
            TLineType.Left,
            TLineType.Right: begin
                               CGContextBeginPath(LCanvas);
                               CGContextMoveToPoint(LCanvas, (LRect.right - LRect.left) / 2, LGridHeight - LRect.top);
                               CGContextAddLineToPoint(LCanvas, (LRect.right - LRect.left) / 2, LGridHeight - LRect.Bottom);
                             end;
          end;
          CGContextStrokePath(LCanvas);
        end;

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

  if ALIsDrawableNull(fBufDrawable) then begin
    inherited paint;
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
  FFill := TBrush.Create(TBrushKind.none, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TStrokeBrush.Create(TBrushKind.none, $FF000000);
  FStroke.OnChanged := StrokeChanged;
  fShadow := TALShadow.Create;
  fShadow.OnChanged := ShadowChanged;
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
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(fShadow);
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
  Repaint;
end;

{***************************************************}
procedure TALBaseText.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  Repaint;
end;

{***************************************************}
procedure TALBaseText.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
  Repaint;
end;

{*************************************************}
procedure TALBaseText.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

{*********************************************************}
procedure TALBaseText.SetStroke(const Value: TStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{******************************************************}
procedure TALBaseText.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
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
           const AFill: TBrush;
           const AStroke: TStrokeBrush;
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
    //LOptions.FillColor: TAlphaColor; // default = TAlphaColors.null - not used if Fill is provided
    //LOptions.StrokeColor: TalphaColor; // default = TAlphaColors.null - not used if Stroke is provided
    //LOptions.StrokeThickness: Single; // default = 1 - not used if Stroke is provided
    //LOptions.ShadowColor: TAlphaColor; // default = TAlphaColors.null - not used if Shadow is provided
    //LOptions.ShadowBlur: Single; // default = 12 - not used if Shadow is provided
    //LOptions.ShadowOffsetX: Single; // default = 0 - not used if Shadow is provided
    //LOptions.ShadowOffsetY: Single; // default = 0 - not used if Shadow is provided
    //--
    LOptions.Fill.assign(AFill);
    LOptions.Stroke.assign(AStroke);
    LOptions.Shadow.assign(AShadow);
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
              Fill, // const Fill: TBrush;
              Stroke, // const Stroke: TStrokeBrush;
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
    Fill, // const AFill: TBrush;
    Stroke, // const AStroke: TStrokeBrush;
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
