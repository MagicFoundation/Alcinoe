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
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Common;

type

  {~~~~~~~~~~~~~~~~~~}
  TALImageWrapMode = (
      //Display the image with its original dimensions:
      //* The image is placed in the upper-left corner of the rectangle of the control.
      //* If the image is larger than the control's rectangle, then only the upper-left part of the image,
      //  which fits in the rectangle of the control, is shown. The image is not resized.
      Original,

      //Best fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
      //  (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
      //  of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
      //  or the height of the resized image is equal to the height of the rectangle of the control. The whole image
      //  should be displayed. The image is displayed centered in the rectangle of the control.
      // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
      //  the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      Fit,

      //Stretch the image to fill the entire rectangle of the control.
      Stretch,

      //Tile (multiply) the image to cover the entire rectangle of the control:
      //* If the image is larger than the rectangle of the control, then only the
      //  upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
      //* If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
      //  (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
      //  the upper-left corner of the rectangle of the control.
      Tile,

      //Center the image to the rectangle of the control:
      //* The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
      Center,

      //Fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
      //  to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
      //  resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      //* If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
      Place,

      //Best fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
      //  (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
      //  of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
      //  AND the height of the resized image is equal to the height of the rectangle of the control.
      // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
      //  the rectangle of the control. Whole the image should be displayed.
      FitAndCrop
  );

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
  TALImage = class(TControl)
  private
    fExifOrientationInfo: TalExifOrientationInfo;
    fRotateAccordingToExifOrientation: Boolean;
    fFileName: String;
    fResourceName: String;
    FWrapMode: TALImageWrapMode;
    fBufBitmap: TALDrawable;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure setFileName(const Value: String);
    procedure setResourceName(const Value: String);
  protected
    procedure Paint; override;
    property BufBitmap: TALDrawable read fBufBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MakeBufBitmap: TALDrawable; virtual;
    procedure clearBufBitmap; virtual;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALRectangle = class(TRectangle)
  private
    fdoubleBuffered: boolean;
    fBufBitmap: TALDrawable;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fShadow: TALShadow;
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetShadow(const Value: TALShadow);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure Paint; override;
    property BufBitmap: TALDrawable read fBufBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MakeBufBitmap: TALDrawable; virtual;
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
    property shadow: TALShadow read fshadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCircle = class(TCircle)
  private
    fdoubleBuffered: boolean;
    fBufBitmap: TALDrawable;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fShadow: TALShadow;
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetShadow(const Value: TALShadow);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure Paint; override;
    property BufBitmap: TALDrawable read fBufBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MakeBufBitmap: TALDrawable; virtual;
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
    property shadow: TALShadow read fshadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALLine = class(TLine)
  private
    fdoubleBuffered: boolean;
    fBufBitmap: TALDrawable;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    property BufBitmap: TALDrawable read fBufBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    function MakeBufBitmap: TALDrawable; virtual;
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALText = class(TControl)
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
    fBufBitmap: TALDrawable;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fBufTextBroken: Boolean;
    fBufAllTextDrawn: Boolean;
    fBufElements: TALTextElements;
    FAutoTranslate: Boolean;
    FText: String;
    FTextSettings: TALTextSettings;
    FAutoSize: Boolean;
    FMaxWidth: Single;
    FMaxHeight: Single;
    FYRadius: Single;
    FXRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    procedure SetText(const Value: string);
    procedure SetAutoSize(const Value: Boolean);
    procedure OnTextSettingsChanged(Sender: TObject);
    procedure SetTextSettings(const Value: TALTextSettings);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
  protected
    function GetElementAtPos(const APos: TPointF): TALTextElement;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure Click; override;
    procedure DblClick; override;
    property BufBitmap: TALDrawable read FBufBitmap;
    procedure PaddingChanged; override;
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure Paint; override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure Loaded; override;
    procedure DoResized; override;
    procedure DoEndUpdate; override;
    procedure AdjustSize; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNewScene(AScene: IScene); override;
    function MakeBufBitmap: TALDrawable; virtual;
    procedure clearBufBitmap; virtual;
    function TextBroken: Boolean;
    property Elements: TALTextElements read fBufElements;
  published
    property Align;
    property Anchors;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
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
    property TextSettings: TALTextSettings read fTextSettings write SetTextSettings;
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
    property OnElementClick: TElementNotifyEvent read FOnElementClick write FOnElementClick;
    property OnElementDblClick: TElementNotifyEvent read FOnElementDblClick write FOnElementDblClick;
    property OnElementMouseDown: TElementMouseEvent read FOnElementMouseDown write FOnElementMouseDown;
    property OnElementMouseMove: TElementMouseMoveEvent read FOnElementMouseMove write FOnElementMouseMove;
    property OnElementMouseUp: TElementMouseEvent read FOnElementMouseUp write FOnElementMouseUp;
    property OnElementMouseEnter: TElementNotifyEvent read FOnElementMouseEnter write FOnElementMouseEnter;
    property OnElementMouseLeave: TElementNotifyEvent read FOnElementMouseLeave write FOnElementMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property Fill: TBrush read FFill write SetFill;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property TouchTargetExpansion;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALButton = class(TALText)
  protected
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HitTest default True;
    property AutoSize default False;
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
  fBufBitmap := ALNullDrawable;
  SetAcceptsControls(False);
end;

{**************************}
destructor TALImage.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{********************************}
procedure TALImage.clearBufBitmap;
begin
  ALFreeAndNilDrawable(fBufBitmap);
end;

{*******************************************}
function TALImage.MakeBufBitmap: TALDrawable;
begin

  if (Scene = nil) or
     //--- don't do bufbitmap if size=0
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     //--- don't do bufbitmap if fFileName or fResourceName is empty
     ((fFileName = '') and (fResourceName = ''))
  then begin
    clearBufBitmap;
    exit(ALNullDrawable);
  end;

  if (not ALIsDrawableNull(fBufBitmap)) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  {$IFDEF debug}
  if not ALIsDrawableNull(fBufBitmap) then
    ALLog('TALImage.MakeBufBitmap', 'BufBitmap is being recreated | Name: ' + Name, TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := Size.Size;

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
                              TalExifOrientationInfo.ROTATE_270] then fBufBitmapRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, Height, Width), ALGetScreenScale) // to have the pixel aligned width and height
  else fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
                                                                                    // TalExifOrientationInfo.FLIP_HORIZONTAL:;
                                                                                    // TalExifOrientationInfo.FLIP_VERTICAL:;
                                                                                    // TalExifOrientationInfo.NORMAL:;
                                                                                    // TalExifOrientationInfo.ROTATE_180:;
                                                                                    // TalExifOrientationInfo.UNDEFINED:;

  case FWrapMode of

    //Display the image with its original dimensions:
    //* The image is placed in the upper-left corner of the rectangle of the control.
    //* If the image is larger than the control's rectangle, then only the upper-left part of the image,
    //  which fits in the rectangle of the control, is shown. The image is not resized.
    TALImageWrapMode.Original:
      begin
        Result := ALNullDrawable; // todo
      end;

    //Best fit the image in the rectangle of the control:
    //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
    //  (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
    //  of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
    //  or the height of the resized image is equal to the height of the rectangle of the control. The whole image
    //  should be displayed. The image is displayed centered in the rectangle of the control.
    // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    //  the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    TALImageWrapMode.Fit:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufBitmap := ALLoadFromFileAndFitIntoToDrawable(LFileName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale)
        else fBufBitmap := ALNullDrawable;
        {$ELSE}
        if fResourceName <> '' then fBufBitmap := ALLoadFromResourceAndFitIntoToDrawable(fResourceName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale)
        else fBufBitmap := ALLoadFromFileAndFitIntoToDrawable(fFileName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale);
        {$ENDIF}
        result := fBufBitmap;
      end;

    //Stretch the image to fill the entire rectangle of the control.
    TALImageWrapMode.Stretch:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufBitmap := ALLoadFromFileAndStretchToDrawable(LFileName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale)
        else fBufBitmap := ALNullDrawable;
        {$ELSE}
        if fResourceName <> '' then fBufBitmap := ALLoadFromResourceAndStretchToDrawable(fResourceName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale)
        else fBufBitmap := ALLoadFromFileAndStretchToDrawable(fFileName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale);
        {$ENDIF}
        result := fBufBitmap;
      end;

    //Tile (multiply) the image to cover the entire rectangle of the control:
    //* If the image is larger than the rectangle of the control, then only the
    //  upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
    //* If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
    //  (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
    //  the upper-left corner of the rectangle of the control.
    TALImageWrapMode.Tile:
      begin
        Result := ALNullDrawable; // todo
      end;

    //Center the image to the rectangle of the control:
    //* The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
    TALImageWrapMode.Center:
      begin
        Result := ALNullDrawable; // todo
      end;

    //Fit the image in the rectangle of the control:
    //* If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
    //  to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
    //  resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    //* If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
    TALImageWrapMode.Place:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufBitmap := ALLoadFromFileAndPlaceIntoToDrawable(LFileName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale)
        else fBufBitmap := ALNullDrawable;
        {$ELSE}
        if fResourceName <> '' then fBufBitmap := ALLoadFromResourceAndPlaceIntoToDrawable(fResourceName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale)
        else fBufBitmap := ALLoadFromFileAndPlaceIntoToDrawable(fFileName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale);
        {$ENDIF}
        result := fBufBitmap;
      end;

    //Best fit the image in the rectangle of the control:
    //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
    //  (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
    //  of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
    //  AND the height of the resized image is equal to the height of the rectangle of the control.
    // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    //  the rectangle of the control. Whole the image should be displayed.
    TALImageWrapMode.FitAndCrop:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufBitmap := ALLoadFromFileAndFitIntoAndCropToDrawable(LFileName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale)
        else fBufBitmap := ALNullDrawable;
        {$ELSE}
        if fResourceName <> '' then fBufBitmap := ALLoadFromResourceAndFitIntoAndCropToDrawable(fResourceName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale)
        else fBufBitmap := ALLoadFromFileAndFitIntoAndCropToDrawable(fFileName, fBufBitmapRect.Width * ALGetScreenScale, fBufBitmapRect.Height * ALGetScreenScale);
        {$ENDIF}
        result := fBufBitmap;
      end;

    //to hide a stupid warning else
    else Result := ALNullDrawable;

  end;

  if not ALIsDrawableNull(Result) then
    fBufBitmapRect := TrectF.Create(0,0, ALGetDrawableWidth(result)/ALGetScreenScale, ALGetDrawableHeight(result)/ALGetScreenScale).
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

  MakeBufBitmap;

  if ALIsDrawableNull(fBufBitmap) then begin
    inherited paint;
    exit;
  end;

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
    fBufBitmap, // const ADrawable: TALDrawable;
    fBufBitmapRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{************************************************************}
procedure TALImage.SetWrapMode(const Value: TALImageWrapMode);
begin
  if FWrapMode <> Value then begin
    clearBufBitmap;
    FWrapMode := Value;
    Repaint;
  end;
end;

{**************************************************}
procedure TALImage.setFileName(const Value: String);
begin
  if FFileName <> Value then begin
    clearBufBitmap;
    FFileName := Value;
    Repaint;
  end;
end;

{******************************************************}
procedure TALImage.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    clearBufBitmap;
    FResourceName := Value;
    Repaint;
  end;
end;

{**************************************************}
constructor TALRectangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fdoubleBuffered := true;
  fBufBitmap := ALNullDrawable;
  fShadow := TalShadow.Create;
  fShadow.OnChanged := ShadowChanged;
end;

{******************************}
destructor TALRectangle.Destroy;
begin
  clearBufBitmap;
  alFreeAndNil(fShadow);
  inherited;
end;

{************************************}
procedure TALRectangle.clearBufBitmap;
begin
  ALFreeAndNilDrawable(fBufBitmap);
end;

{**************************************************}
procedure TALRectangle.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{****************************************************}
procedure TALRectangle.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{****************************************************}
procedure TALRectangle.ShadowChanged(Sender: TObject);
begin
  clearBufBitmap;
  Repaint;
end;

{***********************************************}
function TALRectangle.MakeBufBitmap: TALDrawable;
begin

  if (not fdoubleBuffered) or
     (Scene = nil) or
     //--- don't do bufbitmap if size=0
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     //--- don't do bufbitmap if only fill with solid color
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
    clearBufBitmap;
    exit(ALNullDrawable);
  end;

  if (not ALIsDrawableNull(fBufBitmap)) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  {$IFDEF debug}
  if not ALIsDrawableNull(fBufBitmap) then
    ALLog('TALRectangle.MakeBufBitmap', 'BufBitmap is being recreated | Name: ' + Name, TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := Size.Size;

  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
  var LRect := TrectF.Create(0, 0, fBufBitmapRect.Width, fBufBitmapRect.height);
  if Shadow.enabled then begin
    fBufBitmapRect.Inflate(Shadow.blur, Shadow.blur); // add the extra space needed to draw the shadow
    fBufBitmapRect := ALAlignDimensionToPixelRound(fBufBitmapRect, ALGetScreenScale); // to have the pixel aligned width and height
    LRect.Offset(Shadow.blur, Shadow.blur);
  end;

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALGetScreenScale, // const AScale: Single;
    fBufBitmapRect.Width, // const w: integer;
    fBufBitmapRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      ALDrawRectangle(
        LCanvas, // const ACanvas: TALCanvas;
        ALGetScreenScale, // const AScale: Single;
        LRect, // const Rect: TrectF;
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

    fBufBitmap := ALSurfaceToDrawable(LSurface);

  finally
    ALFreeSurface(LSurface, LCanvas);
  end;

  result := fBufBitmap;

end;

{***************************}
procedure TALRectangle.Paint;
begin

  MakeBufBitmap;

  if ALIsDrawableNull(fBufBitmap) then begin
    inherited paint;
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufBitmap, // const ADrawable: TALDrawable;
    fBufBitmapRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*************************************************************}
procedure TALRectangle.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
end;

{*******************************************************}
procedure TALRectangle.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
end;

{***********************************************}
constructor TALCircle.Create(AOwner: TComponent);
begin
  inherited;
  fdoubleBuffered := true;
  fBufBitmap := ALNullDrawable;
  fShadow := TalShadow.Create;
  fShadow.OnChanged := ShadowChanged;
end;

{***************************}
destructor TALCircle.Destroy;
begin
  clearBufBitmap;
  alFreeAndNil(fShadow);
  inherited;
end;

{*********************************}
procedure TALCircle.clearBufBitmap;
begin
  ALFreeAndNilDrawable(fBufBitmap);
end;

{***********************************************}
procedure TALCircle.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{*************************************************}
procedure TALCircle.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{*************************************************}
procedure TALCircle.ShadowChanged(Sender: TObject);
begin
  clearBufBitmap;
  Repaint;
end;

{********************************************}
function TALCircle.MakeBufBitmap: TALDrawable;
begin

  if (not fdoubleBuffered) or
     (Scene = nil) or
     //--- don't do bufbitmap if size=0
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) then begin
    clearBufBitmap;
    exit(ALNullDrawable);
  end;

  if (not ALIsDrawableNull(fBufBitmap)) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  {$IFDEF debug}
  if not ALIsDrawableNull(fBufBitmap) then
    ALLog('TALCircle.MakeBufBitmap', 'BufBitmap is being recreated | Name: ' + Name, TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := Size.Size;

  fBufBitmapRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), ALGetScreenScale); // to have the pixel aligned width and height
  var LRect := TrectF.Create(0, 0, fBufBitmapRect.Width, fBufBitmapRect.height);
  if Shadow.enabled then begin
    fBufBitmapRect.Inflate(Shadow.blur, Shadow.blur); // add the extra space needed to draw the shadow
    fBufBitmapRect := ALAlignDimensionToPixelRound(fBufBitmapRect, ALGetScreenScale); // to have the pixel aligned width and height
    LRect.Offset(Shadow.blur, Shadow.blur);
  end;

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALGetScreenScale, // const AScale: Single;
    fBufBitmapRect.Width, // const w: integer;
    fBufBitmapRect.Height); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      ALDrawCircle(
        LCanvas, // const ACanvas: TALCanvas;
        ALGetScreenScale, // const AScale: Single;
        LRect, // const Rect: TrectF;
        Fill, // const Fill: TBrush;
        Stroke, // const Stroke: TStrokeBrush;
        Shadow); // const Shadow: TALShadow

    finally
      ALCanvasEndScene(LCanvas)
    end;

    fBufBitmap := ALSurfaceToDrawable(LSurface);

  finally
    ALFreeSurface(LSurface, LCanvas);
  end;

  result := fBufBitmap;

end;

{************************}
procedure TALCircle.Paint;
begin

  MakeBufBitmap;

  if ALIsDrawableNull(fBufBitmap) then begin
    inherited paint;
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufBitmap, // const ADrawable: TALDrawable;
    fBufBitmapRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{**********************************************************}
procedure TALCircle.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
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
  fdoubleBuffered := true;
  fBufBitmap := ALNullDrawable;
end;

{*************************}
destructor TALLine.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{*******************************}
procedure TALLine.clearBufBitmap;
begin
  ALFreeAndNilDrawable(fBufBitmap);
end;

{*********************************************}
procedure TALLine.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{***********************************************}
procedure TALLine.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  inherited;
end;

{******************************************}
function TALLine.MakeBufBitmap: TALDrawable;
begin

  if (not fdoubleBuffered) or
     (Scene = nil) or
     //--- don't do bufbitmap if size=0
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     //--- don't do bufbitmap if stroke is none
     (Stroke.Kind = TBrushKind.None) or
     //--- don't do bufbitmap if Thickness is 0
     (SameValue(Stroke.Thickness, 0, TEpsilon.position)) then begin
    clearBufBitmap;
    exit(ALNullDrawable);
  end;

  if (not ALIsDrawableNull(fBufBitmap)) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  {$IFDEF debug}
  if not ALIsDrawableNull(fBufBitmap) then
    ALLog('TALLine.MakeBufBitmap', 'BufBitmap is being recreated | Name: ' + Name, TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := Size.Size;

  //init LStrokeWidth
  var LStrokeWidth: Single;
  if (LineLocation = TLineLocation.InnerWithin) then LStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else LStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / LRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, LStrokeWidth), ALGetScreenScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -LStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, LStrokeWidth, height), ALGetScreenScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-LStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - LStrokeWidth, Width, height), ALGetScreenScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, LStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(width - LStrokeWidth, 0, width, height), ALGetScreenScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(LStrokeWidth/2, 0);
                     end;
  end;
  var LRect := TrectF.Create(0, 0, round(fBufBitmapRect.Width * ALGetScreenScale), round(fBufBitmapRect.height * ALGetScreenScale));

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

    fBufBitmap := ALSurfaceToDrawable(LSurface);

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

    fBufBitmap := ALSurfaceToDrawable(LSurface);

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

    fBufBitmap := ALSurfaceToDrawable(LCanvas);

  finally
    ALFreeSurface(LSurface, LCanvas); // Var aContext: CGContextRef;
  end;

  {$ENDIF}

  result := fBufBitmap;

end;

{**********************}
procedure TALLine.Paint;
begin

  MakeBufBitmap;

  if ALIsDrawableNull(fBufBitmap) then begin
    inherited paint;
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufBitmap, // const ADrawable: TALDrawable;
    fBufBitmapRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{********************************************************}
procedure TALLine.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
end;

{*********************************************}
constructor TALText.Create(AOwner: TComponent);
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
  fBufBitmap := ALNullDrawable;
  //-----
  FFill := TBrush.Create(TBrushKind.none, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TStrokeBrush.Create(TBrushKind.none, $FF000000);
  FStroke.OnChanged := StrokeChanged;
  FCorners := AllCorners;
  FXRadius := 0;
  FYRadius := 0;
  FSides := AllSides;
  //-----
  HitTest := False;
  //-----
  FAutoTranslate := true;
  FAutoSize := False;
  FMaxWidth := 65535;
  FMaxHeight := 65535;
  FText := '';
  //-----
  FTextSettings := TALTextSettings.Create;
  FTextSettings.OnChanged := OnTextSettingsChanged;
end;

{*************************}
destructor TALText.Destroy;
begin
  clearBufBitmap;
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FFill);
  inherited;
end;

{***********************}
procedure TALText.Loaded;
begin
  if (AutoTranslate) and
     (Text <> '') and
     (not (csDesigning in ComponentState)) then
    Text := ALTranslate(Text);

  if (TextSettings.Font.AutoConvert) and
     (not (csDesigning in ComponentState)) then begin
    if (TextSettings.Font.Family <> '') then
      TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family);
    if (TextSettings.EllipsisSettings.Font.Family <> '') then
      TextSettings.EllipsisSettings.Font.Family := ALConvertFontFamily(TextSettings.EllipsisSettings.Font.Family);
  end;

  inherited Loaded;

  AdjustSize;
end;

{********************************************}
procedure TALText.SetNewScene(AScene: IScene);
begin
  inherited SetNewScene(AScene);
  AdjustSize;
end;

{**************************}
procedure TALText.DoResized;
begin
  inherited DoResized;
  AdjustSize;
end;

{****************************}
procedure TALText.DoEndUpdate;
begin
  AdjustSize;
  inherited DoEndUpdate;
end;

{***************************}
procedure TALText.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // if csDestroying do not do autosize
     (not isupdating) and // DoEndUpdate will call again AdjustSize
     (FAutoSize) and // if FAutoSize is false nothing to adjust
     (Text <> '') and // if Text is empty do not do autosize
     (scene <> nil) then begin // SetNewScene will call again AdjustSize

    MakeBufBitmap;
    var R := FBufBitmapRect;

    // This to take care of the align constraint
    if Align in [TAlignLayout.Client,
                 TAlignLayout.Contents,
                 TAlignLayout.Top,
                 TAlignLayout.Bottom,
                 TAlignLayout.MostTop,
                 TAlignLayout.MostBottom,
                 TAlignLayout.Horizontal,
                 TAlignLayout.VertCenter] then begin
      r.Left := 0;
      r.Width := Width;
    end;
    if Align in [TAlignLayout.Client,
                 TAlignLayout.Contents,
                 TAlignLayout.Left,
                 TAlignLayout.Right,
                 TAlignLayout.MostLeft,
                 TAlignLayout.MostRight,
                 TAlignLayout.Vertical,
                 TAlignLayout.HorzCenter] then begin
      r.Top := 0;
      r.height := height;
    end;

    SetBounds(Position.X, Position.Y, R.Width + R.Left * 2, R.Height + R.Top * 2);

  end;
end;

{********************************************************************}
function TALText.GetElementAtPos(const APos: TPointF): TALTextElement;
begin
  if (not ALIsDrawableNull(fBufBitmap)) then begin
    for var I := Low(fBufElements) to High(fBufElements) do
      if fBufElements[i].Rect.Contains(APos) then begin
        Result := fBufElements[i];
        Exit;
      end;
  end;
  Result.Id := '';
  Result.Rect := TrectF.Empty;
end;

{**********************************************************************************}
procedure TALText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
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

{************************************************************}
procedure TALText.MouseMove(Shift: TShiftState; X, Y: Single);
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

{********************************************************************************}
procedure TALText.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if assigned(FOnElementMouseUp) and (FPressedElement.ID <> '') then
    FOnElementMouseUp(Self, Button, Shift, X, Y, FPressedElement);
  FPressedElement := TALTextElement.Empty;
end;

{*****************************}
procedure TALText.DoMouseEnter;
begin
  FHoveredElement := TALTextElement.Empty;
  inherited DoMouseEnter;
end;

{*****************************}
procedure TALText.DoMouseLeave;
begin
  inherited DoMouseLeave;
  if assigned(FOnElementMouseLeave) and (FHoveredElement.ID <> '') then
    FOnElementMouseLeave(self, FHoveredElement);
  FHoveredElement := TALTextElement.Empty;
end;

{**********************}
procedure TALText.Click;
begin
  inherited Click;
  if assigned(FOnElementClick) and (FPressedElement.ID <> '') then
    FOnElementClick(Self, FPressedElement);
end;

{*************************}
procedure TALText.DblClick;
begin
  inherited DblClick;
  if assigned(FOnElementDblClick) and (FPressedElement.ID <> '') then
    FOnElementDblClick(Self, FPressedElement);
end;

{*******************************}
procedure TALText.PaddingChanged;
begin
  clearBufBitmap;
  AdjustSize;
  Repaint;
end;

{*********************************************}
procedure TALText.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  Repaint;
end;

{***********************************************}
procedure TALText.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  Repaint;
end;

{*********************************************}
procedure TALText.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

{*****************************************************}
procedure TALText.SetStroke(const Value: TStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{****************************************}
function TALText.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{**************************************}
function TALText.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

{**************************************************}
procedure TALText.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then begin
    clearBufBitmap;
    FCorners := Value;
    Repaint;
  end;
end;

{************************************************}
procedure TALText.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Min(Value, Min(Width / 2, Height / 2))
  else NewValue := Value;
  if not SameValue(FXRadius, NewValue, TEpsilon.Vector) then begin
    clearBufBitmap;
    FXRadius := NewValue;
    Repaint;
  end;
end;

{************************************************}
procedure TALText.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Min(Value, Min(Width / 2, Height / 2))
  else NewValue := Value;
  if not SameValue(FYRadius, NewValue, TEpsilon.Vector) then begin
    clearBufBitmap;
    FYRadius := NewValue;
    Repaint;
  end;
end;

{**********************************************}
procedure TALText.SetSides(const Value: TSides);
begin
  if FSides <> Value then begin
    clearBufBitmap;
    FSides := Value;
    Repaint;
  end;
end;

{*******************************************************}
procedure TALText.OnTextSettingsChanged(Sender: TObject);
begin
  ClearBufBitmap;
  AdjustSize;
  Repaint;
end;

{**************************************************************}
procedure TALText.SetTextSettings(const Value: TALTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*******************************}
function TALText.GetData: TValue;
begin
  Result := Text;
end;

{*********************************************}
procedure TALText.SetData(const Value: TValue);
begin
  Text := Value.ToString;
end;

{**********************}
procedure TALText.Paint;
begin

  MakeBufBitmap;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufBitmap, // const ADrawable: TALDrawable;
    fBufBitmapRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;

end;

{**************************************************}
procedure TALText.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    clearBufBitmap;
    FAutoSize := Value;
    AdjustSize;
    repaint;
  end;
end;

{*********************************************}
procedure TALText.SetText(const Value: string);
begin
  if FText <> Value then begin
    clearBufBitmap;
    FText := Value;
    AdjustSize;
    Repaint;
  end;
end;

{*******************************}
procedure TALText.clearBufBitmap;
begin
  ALFreeAndNilDrawable(fBufBitmap);
  setlength(fBufElements, 0);
end;

{******************************************}
function TALText.MakeBufBitmap: TALDrawable;
begin

  if (Scene = nil) then begin
    clearBufBitmap;
    exit(ALNullDrawable);
  end;

  var LMaxSize: TSizeF;
  if (FAutoSize) then begin
    if (Align in [TAlignLayout.Top,
                  TAlignLayout.Bottom,
                  TAlignLayout.MostTop,
                  TAlignLayout.MostBottom,
                  TAlignLayout.Horizontal,
                  TAlignLayout.VertCenter]) then LMaxSize := TSizeF.Create(Width, maxHeight)
    else if (Align in [TAlignLayout.Left,
                       TAlignLayout.Right,
                       TAlignLayout.MostLeft,
                       TAlignLayout.MostRight,
                       TAlignLayout.Vertical,
                       TAlignLayout.HorzCenter]) then LMaxSize := TSizeF.Create(maxWidth, Height)
    else if (Align in [TAlignLayout.Client,
                       TAlignLayout.Contents]) then LMaxSize := TSizeF.Create(Width, Height)
    else LMaxSize := TSizeF.Create(maxWidth, maxHeight);

  end
  else LMaxSize := TSizeF.Create(width, height);

  if (not ALIsDrawableNull(fBufBitmap)) and
     (SameValue(fBufSize.cx, LMaxSize.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, LMaxSize.cy, TEpsilon.position)) then exit(fBufBitmap);

  {$IFDEF debug}
  if not ALIsDrawableNull(fBufBitmap) then
    ALLog(
      'TALText.MakeBufBitmap',
      'BufBitmap is being recreated | ' +
      'Name: ' + Name + ' | ' +
      'text:' + Text + ' | ' +
      'MaxSize: '+floattostr(LMaxSize.cX)+'x'+floattostr(LMaxSize.cY),
      TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := LMaxSize;

  //init fBufBitmapRect
  fBufBitmapRect := TRectF.Create(0, 0, fBufSize.cX, fBufSize.cY);

  //create LOptions
  var LOptions := TALMultiLineTextOptions.Create;
  Try

    LOptions.Scale := ALGetScreenScale;
    //--
    LOptions.FontFamily := TextSettings.font.Family;
    LOptions.FontSize := TextSettings.font.Size;
    LOptions.FontWeight := TextSettings.font.Weight;
    LOptions.FontSlant := TextSettings.font.Slant;
    LOptions.FontStretch := TextSettings.font.Stretch;
    LOptions.FontColor := TextSettings.Font.Color;
    //--
    LOptions.DecorationKinds := TextSettings.Decoration.Kinds;
    LOptions.DecorationStyle := TextSettings.Decoration.Style;
    LOptions.DecorationThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
    LOptions.DecorationColor := TextSettings.Decoration.Color;
    //--
    LOptions.EllipsisText := TextSettings.Ellipsis;
    LOptions.EllipsisFontFamily := TextSettings.EllipsisSettings.font.Family;
    LOptions.EllipsisFontSize := TextSettings.EllipsisSettings.font.Size;
    LOptions.EllipsisFontWeight := TextSettings.EllipsisSettings.font.Weight;
    LOptions.EllipsisFontSlant := TextSettings.EllipsisSettings.font.Slant;
    LOptions.EllipsisFontStretch := TextSettings.EllipsisSettings.font.Stretch;
    LOptions.EllipsisFontColor := TextSettings.EllipsisSettings.Font.Color;
    //--
    LOptions.EllipsisDecorationKinds := TextSettings.EllipsisSettings.Decoration.Kinds;
    LOptions.EllipsisDecorationStyle := TextSettings.EllipsisSettings.Decoration.Style;
    LOptions.EllipsisDecorationThicknessMultiplier := TextSettings.EllipsisSettings.Decoration.ThicknessMultiplier;
    LOptions.EllipsisDecorationColor := TextSettings.EllipsisSettings.Decoration.Color;
    //--
    if (not Autosize) then LOptions.AutoSize := false // if we ask autosize = false then autosize to false
    else if (Align in [TAlignLayout.Top,
                       TAlignLayout.Bottom,
                       TAlignLayout.MostTop,
                       TAlignLayout.MostBottom,
                       TAlignLayout.Horizontal,
                       TAlignLayout.VertCenter]) then begin
        LOptions.AutoSize := false;   // if we ask autosize = true and Width is aligned
        LOptions.AutoSizeY := True;   // then autosize only the Y
    end
    else if (Align in [TAlignLayout.Left,
                       TAlignLayout.Right,
                       TAlignLayout.MostLeft,
                       TAlignLayout.MostRight,
                       TAlignLayout.Vertical,
                       TAlignLayout.HorzCenter]) then begin
      LOptions.AutoSize := false; // if we ask autosize = true and Height is aligned
      LOptions.AutoSizeX := True; // then autosize only the X
    end
    else if (Align in [TAlignLayout.Client,
                       TAlignLayout.Contents]) then LOptions.AutoSize := false  // if we ask autosize = true and Width & Height are aligned then don't autosize anything
    else LOptions.AutoSize := True; // // if we ask autosize = true and Width & Height are not aligned then autosize to true
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
    //--
    LOptions.Fill.assign(Fill);
    LOptions.Stroke.assign(Stroke);
    //--
    LOptions.Sides := Sides;
    LOptions.XRadius := XRadius;
    LOptions.YRadius := YRadius;
    LOptions.Corners := Corners;
    LOptions.Padding := padding.Rect;
    //--
    LOptions.TextIsHtml := TextSettings.IsHtml;

    //build fBufBitmap
    fBufBitmap := ALCreateMultiLineTextDrawable(
                    Text,
                    fBufBitmapRect,
                    fBufTextBroken,
                    fBufAllTextDrawn,
                    fBufElements,
                    LOptions);

    //Special case where it's impossible to draw at least one char.
    //To avoid to call again ALDrawMultiLineText on each paint
    //return a "blank" drawable. Also to avoid to have a control
    //with a "zero" size, do not do any autoresize
    if (ALIsDrawableNull(fBufBitmap)) and (not LocalRect.IsEmpty) then begin
      fBufBitmapRect := LocalRect;
      fBufBitmapRect.Width := Min(MaxWidth, fBufBitmapRect.Width);
      fBufBitmapRect.Height := Min(MaxHeight, fBufBitmapRect.Height);
      fBufBitmapRect := ALAlignDimensionToPixelRound(fBufBitmapRect, ALGetScreenScale);
      var LSurface: TALSurface;
      var LCanvas: TALCanvas;
      ALCreateSurface(
        LSurface, // out ASurface: sk_surface_t;
        LCanvas, // out ACanvas: sk_canvas_t;
        ALGetScreenScale, // const AScale: Single;
        fBufBitmapRect.Width, // const w: integer;
        fBufBitmapRect.height);// const h: integer)
      try
        if ALCanvasBeginScene(LCanvas) then
        try
          ALDrawRectangle(
            LCanvas, // const ACanvas: sk_canvas_t;
            ALGetScreenScale, // const AScale: Single;
            fBufBitmapRect, // const Rect: TrectF;
            Fill, // const Fill: TBrush;
            Stroke, // const Stroke: TStrokeBrush;
            nil, // const Shadow: TALShadow
            Sides, // const Sides: TSides;
            Corners, // const Corners: TCorners;
            XRadius, // const XRadius: Single = 0;
            YRadius); // const YRadius: Single = 0);
        finally
          ALCanvasEndScene(LCanvas)
        end;
        fBufBitmap := ALSurfaceToDrawable(LSurface);
      finally
        ALFreeSurface(LSurface, LCanvas);
      end;
    end;

  finally
    ALFreeAndNil(LOptions);
  end;

  //update the result
  result := fBufBitmap;

end;

{***********************************}
function TALText.TextBroken: Boolean;
begin
  result := (not ALIsDrawableNull(fBufBitmap)) and (fBufTextBroken);
end;

{*************************************************}
procedure TALText.SetMaxWidth(const Value: Single);
begin
  if compareValue(fMaxWidth, Value, Tepsilon.position) <> 0 then begin
    clearBufBitmap;
    fMaxWidth := Value;
    AdjustSize;
  end;
end;

{**************************************************}
procedure TALText.SetMaxHeight(const Value: Single);
begin
  if compareValue(fMaxHeight, Value, Tepsilon.position) <> 0 then begin
    clearBufBitmap;
    fMaxHeight := Value;
    AdjustSize;
  end;
end;

{*****************************************}
function TALText.IsMaxWidthStored: Boolean;
begin
  result := compareValue(fMaxWidth, 65535, Tepsilon.position) <> 0;
end;

{******************************************}
function TALText.IsMaxHeightStored: Boolean;
begin
  result := compareValue(fMaxHeight, 65535, Tepsilon.position) <> 0;
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
//this is a problem because we will calculate several time the bufbitmap
//to mitigate this we can do
//  ALLockTexts(Control1);
//  Control1.endupdate;
//  ALUnLockTexts(Control1);
procedure ALLockTexts(const aParentControl: Tcontrol);
var I: Integer;
begin
  if aParentControl is TalText then begin
    aParentControl.BeginUpdate;
    exit;
  end;
  for I := 0 to aParentControl.Controls.Count - 1 do
    ALLockTexts(aParentControl.Controls[i]);
end;

{******************************************************}
procedure ALUnLockTexts(const aParentControl: Tcontrol);
var I: Integer;
begin
  if aParentControl is TalText then begin
    aParentControl.EndUpdate;
    exit;
  end;
  for I := 0 to aParentControl.Controls.Count - 1 do
    ALUnLockTexts(aParentControl.Controls[i]);
end;

{***********************************************}
constructor TALButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //--
  FFill.Kind := TBrushKind.Solid;
  FFill.Color := $FFE1E1E1;
  FFill.DefaultKind := FFill.Kind;
  FFill.DefaultColor := FFill.Color;
  //--
  FStroke.Kind := TBrushKind.Solid;
  FStroke.Color := $FFADADAD;
  FStroke.DefaultKind := FStroke.Kind;
  FStroke.DefaultColor := FStroke.Color;
  //--
  HitTest := True;
  FAutoSize := True;
  TextSettings.HorzAlign := TALTextHorzAlign.center;
  Padding.rect := TRectF.create(24,8,24,8);
  Padding.DefaultValue := Padding.rect;
end;

{*******************************************************}
procedure TALButton.SetName(const Value: TComponentName);
begin
  var LChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if LChangeText then
    Text := Value;
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALImage, TALRectangle, TALCircle, TALLine, TALText, TALButton]);
end;

initialization
  RegisterFmxClasses([TALImage, TALRectangle, TALCircle, TALLine, TALText, TALButton]);

end.
