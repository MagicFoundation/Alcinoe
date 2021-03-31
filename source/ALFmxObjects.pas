unit ALFmxObjects;

{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if FMX.Objects.pas was not updated and adjust the IFDEF'}
{$ENDIF}

{$I Alcinoe.inc}

interface

uses
  System.Classes,
  System.Types,
  System.UITypes, // [DCC Hint] ALFmxObjects.pas(1418): H2443 Inline function 'TAlphaColorCGFloat.Create' has not been expanded because unit 'System.UITypes' is not specified in USES list
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
  {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
  FMX.effects,
  {$ENDIF}
  FMX.controls,
  FMX.types,
  FMX.textlayout,
  FMX.graphics,
  FMX.objects,
  alGraphics,
  ALFmxCommon;

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
    FScreenScale: single;
    {$IF DEFINED(ALUseTexture)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure setFileName(const Value: String);
    procedure setResourceName(const Value: String);
  protected
    procedure Paint; override;
    {$IF DEFINED(ALUseTexture)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(ALUseTexture)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
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
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALRectangle = class(TRectangle)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(ALUseTexture)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fShadow: TALShadow;
    {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
    fShadowEffect: TshadowEffect;
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetShadow(const Value: TALShadow);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure Paint; override;
    {$IF DEFINED(ALUseTexture)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(ALUseTexture)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
    property shadow: TALShadow read fshadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCircle = class(TCircle)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(ALUseTexture)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fShadow: TALShadow;
    {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
    fShadowEffect: TshadowEffect;
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetShadow(const Value: TALShadow);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure Paint; override;
    {$IF DEFINED(ALUseTexture)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(ALUseTexture)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
    property shadow: TALShadow read fshadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALLine = class(TLine)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(ALUseTexture)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    {$IF DEFINED(ALUseTexture)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    {$IF DEFINED(ALUseTexture)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
  end;

  {~~~~~~~~~~~~~~}
  TALText = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDoubleBufferedTextLayout = class(TTextLayout)
  private
    FScreenScale: single;
    [weak] fTextControl: TALText;
    {$IF DEFINED(ALUseTexture)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    //-----
    fBufHorizontalAlign: TTextAlign;
    fBufVerticalAlign: TTextAlign;
    fBuffontColor: TAlphaColor;
    fBuffontFamily: TFontName;
    fBuffontStyle: TFontStyles;
    fBuffontSize: Single;
    fBufLineSpacing: Single;
    fBufXRadius: Single;
    fBufYRadius: Single;
    fBufTextIsHTML: Boolean;
    fBufWordWrap: Boolean;
    fBufAutosize: Boolean;
    fBufTrimming: TTextTrimming;
    fBufSize: TsizeF;
    fBufText: string;
    fBufTextBreaked: Boolean;
    fBufAllTextDrawed: Boolean;
  protected
    procedure DoRenderLayout; override;
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    function GetTextHeight: Single; override;
    function GetTextWidth: Single; override;
    function GetTextRect: TRectF; override;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
  public
    constructor Create(const ACanvas: TCanvas; const aTextControl: TALText); reintroduce;
    destructor Destroy; override;
    {$IF DEFINED(ALUseTexture)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure ConvertToPath(const APath: TPathData); override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // Note: we can use this class in for exemple Tlabel
  //       by overriding the Tlabel default Style but i
  //       do not recomend to use Tlabel, because it's simply
  //       a painless class that is in the top of the TText !
  //       this class use also it's own TTextLayoutNG to calculate
  //       the size, making everythink duplicate for .. nothing in fact !
  //       but i made some test and it's work with tlabel (but check carefully
  //       that you define well the properties of the TALText in the
  //       style to not have MakeBufBitmap called several times (applystyle
  //       don't call beginupdate/endupdate (crazy!!), so everytime a property of the
  //       TALText is updated, MakeBufBitmap is call again)
  [ComponentPlatforms($FFFF)]
  TALText = class(TControl)
  private
    fRestoreLayoutUpdateAfterLoaded: boolean;
    FAutoTranslate: Boolean;
    FAutoConvertFontFamily: boolean;
    FTextSettings: TTextSettings;
    FLayout: TTextLayout;
    FAutoSize: Boolean;
    fMaxWidth: Single;
    fMaxHeight: Single;
    FYRadius: Single;
    FXRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    fLineSpacing: single;
    fTextIsHtml: boolean;
    fMustCallResized: Boolean;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    {$IF DEFINED(ALUseTexture)}
    function GetBufBitmap: TTexture;
    {$ELSE}
    function GetBufBitmap: Tbitmap;
    {$ENDIF}
    function GetdoubleBuffered: Boolean;
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetHorzTextAlign(const Value: TTextAlign);
    procedure SetVertTextAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure OnFontChanged(Sender: TObject);
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function GetColor: TAlphaColor;
    function GetFont: TFont;
    function GetHorzTextAlign: TTextAlign;
    function GetTrimming: TTextTrimming;
    function GetVertTextAlign: TTextAlign;
    function GetWordWrap: Boolean;
    function GetText: string;
    procedure SetMaxWidth(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
  protected
    {$IF DEFINED(ALUseTexture)}
    property BufBitmap: TTexture read GetBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read GetBufBitmap;
    {$ENDIF}
    procedure PaddingChanged; override;
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetLineSpacing(const Value: Single); virtual;
    procedure SetTextIsHtml(const Value: Boolean); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure SetParent(const Value: TFmxObject); override;
    procedure FontChanged; virtual;
    function SupportsPaintStage(const Stage: TPaintStage): Boolean; override;
    function GetTextSettingsClass: TTextSettingsClass; virtual;
    procedure Paint; override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure DoRealign; override;
    procedure AdjustSize;
    procedure Resize; override;
    {$IF CompilerVersion >= 32} // tokyo
    procedure DoResized; override;
    {$ENDIF}
    procedure Loaded; override;
    property Layout: TTextLayout read FLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNewScene(AScene: IScene); override;
    {$IF DEFINED(ALUseTexture)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure BeginUpdate; override; // this is neccessary because the MakeBufBitmap is not only call during the paint,
    procedure EndUpdate; override;   // but also when any property changed because need to retrieve the dimension
    function TextBreaked: Boolean;
    property Font: TFont read GetFont write SetFont;
    property Color: TAlphaColor read GetColor write SetColor;
    property HorzTextAlign: TTextAlign read GetHorzTextAlign write SetHorzTextAlign;
    property Trimming: TTextTrimming read GetTrimming write SetTrimming;
    property VertTextAlign: TTextAlign read GetVertTextAlign write SetVertTextAlign;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
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
    property Text: string read GetText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property Visible default True;
    property Width;
    property MaxWidth: single read fMaxWidth write SetMaxWidth stored IsMaxWidthStored;       // these properties are usefull when used
    property MaxHeight: single read fMaxHeight write SetMaxHeight stored IsMaxHeightStored;      // with autosize
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
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
    property doubleBuffered: Boolean read GetdoubleBuffered write setdoubleBuffered default true;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property AutoConvertFontFamily: Boolean read FAutoConvertFontFamily write fAutoConvertFontFamily default true;
    property Fill: TBrush read FFill write SetFill;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property LineSpacing: single read fLineSpacing write SetLineSpacing;
    property TextIsHtml: boolean read fTextIsHtml write SetTextIsHtml default false;
    property TouchTargetExpansion;
  end;

procedure ALLockTexts(const aParentControl: Tcontrol);
procedure ALUnLockTexts(const aParentControl: Tcontrol);

{$IFDEF debug}
var
  AlDebugImageMakeBufBitmapCount: integer;
  AlDebugRectangleMakeBufBitmapCount: integer;
  AlDebugCircleMakeBufBitmapCount: integer;
  AlDebugLineMakeBufBitmapCount: integer;
  AlDebugTextMakeBufBitmapCount: integer;
  AlDebugTextInheritedDoRenderLayoutCount: integer;
  AlDebugTextInheritedDoDrawLayoutCount: integer;

  AlDebugImageMakeBufBitmapStopWatch: TstopWatch;
  AlDebugRectangleMakeBufBitmapStopWatch: TstopWatch;
  AlDebugCircleMakeBufBitmapStopWatch: TstopWatch;
  AlDebugLineMakeBufBitmapStopWatch: TstopWatch;
  AlDebugTextMakeBufBitmapStopWatch: TstopWatch;
{$endif}

procedure Register;

implementation

uses
  system.SysUtils,
  system.Math,
  system.Math.Vectors,
  fmx.consts,
  fmx.platform,
  {$IFDEF ALDPK}
  system.ioutils,
  ToolsAPI,
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
  ALFmxTypes3D,
  {$ENDIF}
  ALCommon;

{**********************************************}
constructor TALImage.Create(AOwner: TComponent);
var LScreenSrv: IFMXScreenService;
begin
  inherited Create(AOwner);
  fExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;
  fRotateAccordingToExifOrientation := False;
  fFileName := '';
  fResourceName := '';
  FWrapMode := TALImageWrapMode.Fit;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then FScreenScale := LScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fBufBitmap := nil;
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
  ALFreeAndNil(fBufBitmap);
end;

{*************************}
{$IF DEFINED(ALUseTexture)}
function TALImage.MakeBufBitmap: TTexture;
{$ELSE}
function TALImage.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IFDEF ALDPK}
var LFileName: String;
{$ENDIF}

begin

  if (Scene = nil) or
     //--- don't do bufbitmap is size=0
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     //--- don't do bufbitmap if fFileName or fResourceName is empty
     ((fFileName = '') and (fResourceName = ''))
  then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  ALLog('TALImage.MakeBufBitmap', 'Name: ' + Name, TalLogType.verbose);
  inc(AlDebugImageMakeBufBitmapCount);
  AlDebugImageMakeBufBitmapStopWatch.Start;
  try
  {$endif}

    {$IFDEF ALDPK}
    if fresourceName = '' then LFileName := fFileName
    else begin
      LFileName := extractFilePath(getActiveProject.fileName) + 'resources\' + fResourceName; // by default all the resources files must be located in the sub-folder /resources/ of the project
      if not TFile.Exists(LFileName) then LFileName := LFileName + '.png';
    end;
    if not TFile.Exists(LFileName) then LFileName := '';
    {$ENDIF}

    if (fRotateAccordingToExifOrientation) and
       (fResourceName = '') and
       (fFileName <> '') then fExifOrientationInfo := AlGetExifOrientationInfo(ffilename)
    else fExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;

    if fExifOrientationInfo in [TalExifOrientationInfo.TRANSPOSE,
                                TalExifOrientationInfo.ROTATE_90,
                                TalExifOrientationInfo.TRANSVERSE,
                                TalExifOrientationInfo.ROTATE_270] then fBufBitmapRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, Height, Width), FScreenScale) // to have the pixel aligned width and height
    else fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
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
          Result := nil; // todo
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
          if LFileName <> '' then fBufBitmap := ALLoadFitIntoFileImageV3(LFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := nil;
          {$ELSE}
          if fResourceName <> '' then fBufBitmap := ALLoadFitIntoResourceImageV3(fResourceName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := ALLoadFitIntoFileImageV3(fFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale);
          {$ENDIF}
          result := fBufBitmap;
        end;

      //Stretch the image to fill the entire rectangle of the control.
      TALImageWrapMode.Stretch:
        begin
          {$IFDEF ALDPK}
          if LFileName <> '' then fBufBitmap := ALLoadStretchFileImageV3(LFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := nil;
          {$ELSE}
          if fResourceName <> '' then fBufBitmap := ALLoadStretchResourceImageV3(fResourceName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := ALLoadStretchFileImageV3(fFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale);
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
          Result := nil; // todo
        end;

      //Center the image to the rectangle of the control:
      //* The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
      TALImageWrapMode.Center:
        begin
          Result := nil; // todo
        end;

      //Fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
      //  to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
      //  resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      //* If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
      TALImageWrapMode.Place:
        begin
          Result := nil; // todo
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
          if LFileName <> '' then fBufBitmap := ALLoadFitIntoAndCropFileImageV3(LFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := nil;
          {$ELSE}
          if fResourceName <> '' then fBufBitmap := ALLoadFitIntoAndCropResourceImageV3(fResourceName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := ALLoadFitIntoAndCropFileImageV3(fFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale);
          {$ENDIF}
          result := fBufBitmap;
        end;

      //to hide a stupid warning else
      else Result := nil;

    end;

    if result <> nil then fBufBitmapRect := TrectF.Create(0,0, result.Width/FScreenScale, result.Height/FScreenScale).
                                              CenterAt(LocalRect);

  {$IFDEF debug}
  finally
    AlDebugImageMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{***********************}
procedure TALImage.Paint;
var LMatrix: Tmatrix;
    LMatrixRotationCenter: TpointF;
    R: TRectF;
begin

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

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

  {$IF DEFINED(ALUseTexture)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

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
var LScreenSrv: IFMXScreenService;
begin
  inherited Create(AOwner);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then FScreenScale := LScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  fShadow := TalShadow.Create;
  fShadow.OnChanged := ShadowChanged;
  {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
  fShadowEffect := nil;
  {$ENDIF}
end;

{******************************}
destructor TALRectangle.Destroy;
begin
  clearBufBitmap;
  alFreeAndNil(fShadow);
  {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
  AlFreeAndNil(fShadowEffect);
  {$ENDIF}
  inherited;
end;

{************************************}
procedure TALRectangle.clearBufBitmap;
begin
  ALFreeAndNil(fBufBitmap);
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
  {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
  if shadow.enabled then begin
    if not assigned(fShadowEffect) then begin
      fShadowEffect := TshadowEffect.Create(self);
      fShadowEffect.Parent := self;
      fShadowEffect.SetSubComponent(true);
      fShadowEffect.stored := False;
    end;
    fShadowEffect.ShadowColor := shadow.ShadowColor;
    fShadowEffect.distance := 0; // Specifies the distance between the shadow and the visual object to which TShadowEffect is applied.
                                 // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
    fShadowEffect.Direction := 0;  // Specifies the direction (in degrees) of the shadow.
                                   // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
    fShadowEffect.Opacity := 1; // Opacity is a System.Single value that takes values in the range from 0 through 1.
                                // we use the opacity of the color instead
    fShadowEffect.softness := fShadow.blur / 24; // Specifies the amount of blur applied to the shadow.
                                                 // Softness is a System.Single value that takes values in the range from 0 through 9.
                                                 // i calculate approximatly that 0.5 = around 12 for blur
  end
  else AlFreeAndNil(fShadowEffect);
  {$ENDIF}
  if FUpdating = 0 then Repaint;
end;

{*************************}
{$IF DEFINED(ALUseTexture)}
function TALRectangle.MakeBufBitmap: TTexture;
{$ELSE}
function TALRectangle.MakeBufBitmap: Tbitmap;
{$ENDIF}

var LSaveStrokeThickness: single;
    LSaveShadowOffsetX: single;
    LSaveShadowOffsetY: single;
    LSaveShadowBlur: single;
    LRect: TRectf;
    {$IF defined(ANDROID)}
    LBitmap: Jbitmap;
    LCanvas: Jcanvas;
    {$ELSEIF defined(IOS)}
    LBitmapSurface: TbitmapSurface;
    LColorSpace: CGColorSpaceRef;
    LContext: CGContextRef;
    {$ENDIF}

begin

  if (csDesigning in ComponentState) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     //--- don't do bufbitmap is size=0
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
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  ALLog('TALRectangle.MakeBufBitmap', 'Name: ' + Name, TalLogType.verbose);
  inc(AlDebugRectangleMakeBufBitmapCount);
  AlDebugRectangleMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  //init fBufBitmapRect / LRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
  LRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));
  if Shadow.enabled then begin
    fBufBitmapRect.Inflate(Shadow.blur, Shadow.blur); // add the extra space needed to draw the shadow
    fBufBitmapRect := ALAlignDimensionToPixelRound(fBufBitmapRect, FScreenScale); // to have the pixel aligned width and height
    LRect.Offset(Shadow.blur * FScreenScale, Shadow.blur * FScreenScale);
  end;

  //translate Stroke.Thickness from virtual to real pixel
  Stroke.OnChanged := Nil;
  LSaveStrokeThickness := Stroke.Thickness;
  Stroke.Thickness := Stroke.Thickness * fScreenScale;
  //-----
  Shadow.OnChanged := nil;
  LSaveShadowOffsetX := Shadow.OffsetX;
  LSaveShadowOffsetY := Shadow.OffsetY;
  LSaveShadowBlur := Shadow.Blur;
  Shadow.OffsetX := Shadow.OffsetX * fScreenScale;
  Shadow.OffsetY := Shadow.OffsetY * fScreenScale;
  Shadow.Blur := Shadow.Blur * fScreenScale;
  try

    {$IFDEF ANDROID}

    //create the drawing surface
    ALCreateDrawingSurface(LBitmap, // Var aBitmap: Jbitmap;
                           LCanvas, // var aCanvas: Jcanvas;
                           round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
                           round(fBufBitmapRect.height * FScreenScale));// const h: integer)
    try

       ALPaintRectangle(LCanvas, // const aBitmap: Jbitmap;
                        LRect, // const Rect: TrectF;
                        Fill, // const Fill: TBrush;
                        Stroke, // const Stroke: TStrokeBrush;
                        Shadow, // const Shadow: TALShadow
                        Sides, // const Sides: TSides;
                        Corners, // const Corners: TCorners;
                        XRadius * fScreenScale, // const XRadius: Single = 0;
                        YRadius * fScreenScale); // const YRadius: Single = 0);

      fBufBitmap := ALJBitmaptoTexture(LBitmap);

    finally
      ALFreeDrawingSurface(LBitmap, LCanvas);
    end;

    {$ELSEIF DEFINED(IOS)}

    //create the drawing surface
    ALCreateDrawingSurface(LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                           LContext, //    Var aContext: CGContextRef;
                           LColorSpace, // Var aColorSpace: CGColorSpaceRef;
                           round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
                           round(fBufBitmapRect.height * FScreenScale));// const h: integer)
    try

       ALPaintRectangle(LContext, // const aContext: CGContextRef;
                        LColorSpace, // const aColorSpace: CGColorSpaceRef;
                        LBitmapSurface.Height, // const aGridHeight: Single;
                        LRect, // const Rect: TrectF;
                        Fill, // const Fill: TBrush;
                        Stroke, // const Stroke: TStrokeBrush;
                        Shadow, // const Shadow: TALShadow
                        Sides, // const Sides: TSides;
                        Corners, // const Corners: TCorners;
                        XRadius * fScreenScale, // const XRadius: Single = 0;
                        YRadius * fScreenScale); // const YRadius: Single = 0);

      fBufBitmap := ALBitmapSurfacetoTexture(LBitmapSurface);

    finally
      ALFreeDrawingSurface(LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                           LContext, // Var aContext: CGContextRef;
                           LColorSpace); // Var aColorSpace: CGColorSpaceRef;
    end;

    {$ENDIF}

  finally
    Stroke.Thickness := LSaveStrokeThickness;
    Stroke.OnChanged := StrokeChanged;
    //-----
    Shadow.OffsetX := LSaveShadowOffsetX;
    Shadow.OffsetY := LSaveShadowOffsetY;
    Shadow.Blur := LSaveShadowBlur;
    Shadow.OnChanged := ShadowChanged;
  end;

  //set the result
  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugRectangleMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{***************************}
procedure TALRectangle.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(ALUseTexture)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

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
var LScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then FScreenScale := LScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  fShadow := TalShadow.Create;
  fShadow.OnChanged := ShadowChanged;
  {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
  fShadowEffect := nil;
  {$ENDIF}
end;

{***************************}
destructor TALCircle.Destroy;
begin
  clearBufBitmap;
  alFreeAndNil(fShadow);
  {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
  AlFreeandNil(fShadowEffect);
  {$ENDIF}
  inherited;
end;

{*********************************}
procedure TALCircle.clearBufBitmap;
begin
  ALFreeAndNil(fBufBitmap);
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
  {$IF DEFINED(MSWindows) or DEFINED(ALMacOS)}
  if shadow.enabled then begin
    if not assigned(fShadowEffect) then begin
      fShadowEffect := TshadowEffect.Create(self);
      fShadowEffect.Parent := self;
      fShadowEffect.SetSubComponent(true);
      fShadowEffect.stored := False;
    end;
    fShadowEffect.ShadowColor := shadow.ShadowColor;
    fShadowEffect.distance := 0; // Specifies the distance between the shadow and the visual object to which TShadowEffect is applied.
                                 // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
    fShadowEffect.Direction := 0;  // Specifies the direction (in degrees) of the shadow.
                                   // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
    fShadowEffect.Opacity := 1; // Opacity is a System.Single value that takes values in the range from 0 through 1.
                                // we use the opacity of the color instead
    fShadowEffect.softness := fShadow.blur / 24; // Specifies the amount of blur applied to the shadow.
                                                 // Softness is a System.Single value that takes values in the range from 0 through 9.
                                                 // i calculate approximatly that 0.5 = around 12 for blur
  end
  else AlFreeAndNil(fShadowEffect);
  {$ENDIF}
  if FUpdating = 0 then Repaint;
end;

{*************************}
{$IF DEFINED(ALUseTexture)}
function TALCircle.MakeBufBitmap: TTexture;
{$ELSE}
function TALCircle.MakeBufBitmap: Tbitmap;
{$ENDIF}

var LSaveStrokeThickness: single;
    LSaveShadowOffsetX: single;
    LSaveShadowOffsetY: single;
    LSaveShadowBlur: single;
    LRect: TRectf;
    {$IF defined(ANDROID)}
    LBitmap: Jbitmap;
    LCanvas: Jcanvas;
    {$ELSEIF defined(IOS)}
    LBitmapSurface: TbitmapSurface;
    LColorSpace: CGColorSpaceRef;
    LContext: CGContextRef;
    {$ENDIF}

begin

  if (csDesigning in ComponentState) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  ALLog('TALCircle.MakeBufBitmap', 'Name: ' + Name, TalLogType.verbose);
  inc(AlDebugCircleMakeBufBitmapCount);
  AlDebugCircleMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  //init fBufBitmapRect / LRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), FScreenScale); // to have the pixel aligned width and height
  LRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));
  if Shadow.enabled then begin
    fBufBitmapRect.Inflate(Shadow.blur, Shadow.blur); // add the extra space needed to draw the shadow
    fBufBitmapRect := ALAlignDimensionToPixelRound(fBufBitmapRect, FScreenScale); // to have the pixel aligned width and height
    LRect.Offset(Shadow.blur * FScreenScale, Shadow.blur * FScreenScale);
  end;

  //translate Stroke.Thickness from virtual to real pixel
  Stroke.OnChanged := Nil;
  LSaveStrokeThickness := Stroke.Thickness;
  Stroke.Thickness := Stroke.Thickness * fScreenScale;
  //-----
  Shadow.OnChanged := nil;
  LSaveShadowOffsetX := Shadow.OffsetX;
  LSaveShadowOffsetY := Shadow.OffsetY;
  LSaveShadowBlur := Shadow.Blur;
  Shadow.OffsetX := Shadow.OffsetX * fScreenScale;
  Shadow.OffsetY := Shadow.OffsetY * fScreenScale;
  Shadow.Blur := Shadow.Blur * fScreenScale;
  try

    {$IFDEF ANDROID}

    //create the drawing surface
    ALCreateDrawingSurface(LBitmap, // Var aBitmap: Jbitmap;
                           LCanvas, // var aCanvas: Jcanvas;
                           round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
                           round(fBufBitmapRect.Height * FScreenScale));// const h: integer)
    try

      ALPaintCircle(LCanvas, // const aBitmap: Jbitmap;
                    LRect, // const Rect: TrectF;
                    Fill, // const Fill: TBrush;
                    Stroke, // const Stroke: TStrokeBrush;
                    Shadow); // const Shadow: TALShadow

      fBufBitmap := ALJBitmaptoTexture(LBitmap);

    finally
      ALFreeDrawingSurface(LBitmap, LCanvas);
    end;

    {$ELSEIF DEFINED(IOS)}

     //create the drawing surface
    ALCreateDrawingSurface(LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                           LContext, //    Var aContext: CGContextRef;
                           LColorSpace, // Var aColorSpace: CGColorSpaceRef;
                           round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
                           round(fBufBitmapRect.Height * FScreenScale));// const h: integer)
    try

      ALPaintCircle(LContext, // const aContext: CGContextRef;
                    LColorSpace, // const aColorSpace: CGColorSpaceRef;
                    LBitmapSurface.Height, // const aGridHeight: Single;
                    LRect, // const Rect: TrectF;
                    Fill, // const Fill: TBrush;
                    Stroke, // const Stroke: TStrokeBrush;
                    Shadow); // const Shadow: TALShadow

      fBufBitmap := ALBitmapSurfacetoTexture(LBitmapSurface);

    finally
      ALFreeDrawingSurface(LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                           LContext, // Var aContext: CGContextRef;
                           LColorSpace); // Var aColorSpace: CGColorSpaceRef;
    end;

    {$ENDIF}

  finally
    Stroke.Thickness := LSaveStrokeThickness;
    Stroke.OnChanged := StrokeChanged;
    //-----
    Shadow.OffsetX := LSaveShadowOffsetX;
    Shadow.OffsetY := LSaveShadowOffsetY;
    Shadow.Blur := LSaveShadowBlur;
    Shadow.OnChanged := ShadowChanged;
  end;

  //set the result
  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugCircleMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{************************}
procedure TALCircle.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(ALUseTexture)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

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
var LScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then FScreenScale := LScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
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
  ALFreeAndNil(fBufBitmap);
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

{*************************}
{$IF DEFINED(ALUseTexture)}
function TALLine.MakeBufBitmap: TTexture;
{$ELSE}
function TALLine.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IF defined(ANDROID)}
var LBitmap: Jbitmap;
    LCanvas: Jcanvas;
    LPaint: JPaint;
    LRect: TRectf;
    LStrokeWidth: Single;
{$ELSEIF defined(IOS)}
var LBitmapSurface: TbitmapSurface;
    LColorSpace: CGColorSpaceRef;
    LContext: CGContextRef;
    LAlphaColor: TAlphaColorCGFloat;
    LRect: TRectf;
    LStrokeWidth: Single;
{$ENDIF}

begin

  if (csDesigning in ComponentState) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     (Stroke.Kind = TBrushKind.None) or
     (SameValue(Stroke.Thickness, 0, TEpsilon.position)) then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  ALLog('TALLine.MakeBufBitmap', 'Name: ' + Name, TalLogType.verbose);
  inc(AlDebugLineMakeBufBitmapCount);
  AlDebugLineMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  {$IFDEF ANDROID}

  //init LStrokeWidth
  if (LineLocation = TLineLocation.InnerWithin) then LStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else LStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / LRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, LStrokeWidth), FScreenScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -LStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, LStrokeWidth, height), FScreenScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-LStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - LStrokeWidth, Width, height), FScreenScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, LStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(width - LStrokeWidth, 0, width, height), FScreenScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(LStrokeWidth/2, 0);
                     end;
  end;
  LRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the drawing surface
  ALCreateDrawingSurface(LBitmap, // Var aBitmap: Jbitmap;
                         LCanvas, // var aCanvas: Jcanvas;
                         round(LRect.Width), // const w: integer;
                         round(LRect.Height));// const h: integer)
  try

    //create the canvas and the paint
    LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    //stroke the circle
    if Stroke.Kind <> TBrushKind.None then begin

      //init LPaint
      LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
      LPaint.setStrokeWidth(LStrokeWidth * FScreenScale);

      //stroke with solid color
      if Stroke.Kind = TBrushKind.Solid then begin
        LPaint.setColor(integer(Stroke.Color));
        case lineType of
          TLineType.Diagonal: LCanvas.drawLine(LRect.left {startX},
                                               LRect.top {startY},
                                               LRect.right {stopX},
                                               LRect.Bottom {stopY},
                                               LPaint);
          TLineType.Top,
          TLineType.Bottom: LCanvas.drawLine(LRect.left {startX},
                                             (LRect.bottom - LRect.top) / 2 {startY},
                                             LRect.right {stopX},
                                             (LRect.bottom - LRect.top) / 2 {stopY},
                                             LPaint);
          TLineType.Left,
          TLineType.Right: LCanvas.drawLine((LRect.right - LRect.left) / 2 {startX},
                                            LRect.top {startY},
                                            (LRect.right - LRect.left) / 2 {stopX},
                                            LRect.bottom {stopY},
                                            LPaint);
        end;
      end;

    end;

    //free the paint and the canvas
    LPaint := nil;

    //convert LBitmap to TALTexture
    fBufBitmap := ALJBitmaptoTexture(LBitmap);

  finally
    ALFreeDrawingSurface(LBitmap, LCanvas);
  end;

  {$ELSEIF DEFINED(IOS)}

  //init LStrokeWidth
  if (LineLocation = TLineLocation.InnerWithin) then LStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else LStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / LRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, LStrokeWidth), FScreenScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -LStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, LStrokeWidth, height), FScreenScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-LStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - LStrokeWidth, Width, height), FScreenScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, LStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(width - LStrokeWidth, 0, width, height), FScreenScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(LStrokeWidth/2, 0);
                     end;
  end;
  LRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the drawing surface
  ALCreateDrawingSurface(LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                         LContext, //    Var aContext: CGContextRef;
                         LColorSpace, // Var aColorSpace: CGColorSpaceRef;
                         round(LRect.Width), // const w: integer;
                         round(LRect.Height));// const h: integer)
  try

    //stroke the circle
    if Stroke.Kind <> TBrushKind.None then begin

      //stroke with solid color
      if Stroke.Kind = TBrushKind.Solid then begin
        LAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
        CGContextSetRGBStrokeColor(LContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
        CGContextSetLineWidth(LContext, Stroke.Thickness * FScreenScale);
        case lineType of
          TLineType.Diagonal: begin
                                CGContextBeginPath(LContext);
                                CGContextMoveToPoint(LContext, LRect.left, LBitmapSurface.height - LRect.top);
                                CGContextAddLineToPoint(LContext, LRect.right, LBitmapSurface.height - LRect.Bottom);
                              end;
          TLineType.Top,
          TLineType.Bottom: begin
                              CGContextBeginPath(LContext);
                              CGContextMoveToPoint(LContext, LRect.left, LBitmapSurface.height - ((LRect.bottom - LRect.top) / 2));
                              CGContextAddLineToPoint(LContext, LRect.right, LBitmapSurface.height - ((LRect.bottom - LRect.top) / 2));
                            end;
          TLineType.Left,
          TLineType.Right: begin
                             CGContextBeginPath(LContext);
                             CGContextMoveToPoint(LContext, (LRect.right - LRect.left) / 2, LBitmapSurface.height - LRect.top);
                             CGContextAddLineToPoint(LContext, (LRect.right - LRect.left) / 2, LBitmapSurface.height - LRect.Bottom);
                           end;
        end;
        CGContextStrokePath(LContext);
      end;

    end;

    //convert the LBitmapSurface to texture
    fBufBitmap := ALBitmapSurfacetoTexture(LBitmapSurface);

  finally
    ALFreeDrawingSurface(LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                         LContext, // Var aContext: CGContextRef;
                         LColorSpace); // Var aColorSpace: CGColorSpaceRef;
  end;

  {$ENDIF}

  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugLineMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{**********************}
procedure TALLine.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(ALUseTexture)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

{********************************************************}
procedure TALLine.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
end;

{**************************************************************************************************}
constructor TALDoubleBufferedTextLayout.Create(const ACanvas: TCanvas; const aTextControl: TALText);
var LScreenSrv: IFMXScreenService;
begin
  inherited Create(ACanvas);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then FScreenScale := LScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fBufBitmap := nil;
  fTextControl := aTextControl;
end;

{*********************************************}
destructor TALDoubleBufferedTextLayout.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{*************************}
{$IF DEFINED(ALUseTexture)}
function TALDoubleBufferedTextLayout.MakeBufBitmap: TTexture;
{$ELSE}
function TALDoubleBufferedTextLayout.MakeBufBitmap: Tbitmap;
{$ENDIF}
var LOptions: TALDrawMultiLineTextOptions;
begin

  if (fTextControl.Scene = nil) or // << fTextControl.Scene = nil mean mostly the fTextControl (or his parent) is not yet assigned to any form
     (fTextControl.text.IsEmpty) then begin
    clearBufBitmap;
    exit(nil);
  end;

  // we need to use the value of the fTextControl and not of the current TTextLayout
  // because TText update some value on each call to adjustsize with different value that will
  // be used in paint
  if (fBufBitmap <> nil) and
     (fBufHorizontalAlign = fTextControl.TextSettings.HorzAlign) and  // TText.adjustsize always use TTextAlign.Leading
     (fBufVerticalAlign = fTextControl.TextSettings.VertAlign) and // TText.adjustsize always use TTextAlign.Leading
     (fBufFontColor = fTextControl.TextSettings.FontColor) and
     (sametext(fBuffontFamily, fTextControl.TextSettings.Font.Family)) and
     (fBuffontStyle = fTextControl.TextSettings.Font.Style) and
     (SameValue(fBuffontSize, fTextControl.TextSettings.Font.Size, TEpsilon.FontSize)) and
     (SameValue(fBufLineSpacing, fTextControl.LineSpacing, TEpsilon.FontSize)) and
     (SameValue(fBufXRadius, fTextControl.XRadius, TEpsilon.Vector)) and
     (SameValue(fBufYRadius, fTextControl.YRadius, TEpsilon.Vector)) and
     (fBufTextIsHTML = fTextControl.TextIsHTML) and
     (fBufWordWrap = fTextControl.TextSettings.WordWrap) and
     (fBufAutosize = fTextControl.AutoSize) and
     (fBufTrimming = fTextControl.TextSettings.Trimming) and
     (
      (
       (not fTextControl.AutoSize) and                              // if NOT autosize then the dimensions returned
       (SameValue(fBufSize.cx, MaxSize.X, TEpsilon.position)) and   // by this function will depend of MaxSize.X / MaxSize.Y
       (SameValue(fBufSize.cy, MaxSize.Y, TEpsilon.position))       // so if fBufSize = MaxSize do nothing
      )
      OR
      (
       (fTextControl.AutoSize) and                                                 // * IF autosize
       (
        (SameValue(fBufSize.cx, MaxSize.x, TEpsilon.position)) or                  // * if we already did the job with maxsize.x then do nothing
        (SameValue(fbufBitmapRect.width, MaxSize.x, TEpsilon.position)) or         // * if fbufBitmapRect.width = MaxSize.x we can not do anything better ;)
        ((fTextControl.Fill.Kind = TbrushKind.None) and                            // * if we don't draw any background =>
         (fTextControl.stroke.Kind = TbrushKind.None) and                          //                                   =>
         (not (fTextControl.Align in [TAlignLayout.Client,                         //                                   =>
                                      TAlignLayout.Contents,                       //                                   => Else we must have
                                      TAlignLayout.Top,                            //                                   => fbufBitmapRect.width = MaxSize.x
                                      TAlignLayout.Bottom,                         //                                   =>
                                      TAlignLayout.MostTop,                        //                                   =>
                                      TAlignLayout.MostBottom,                     //                                   =>
                                      TAlignLayout.VertCenter])) and               //   and if the Width is not aligned =>
         (not fBufTextBreaked) and                                                 //   and if text wasn't breaked                                            => else if fbufBitmapRect.width > MaxSize.x
         (CompareValue(fbufBitmapRect.width, MaxSize.x, TEpsilon.position) <= 0))  //   and if fbufBitmapRect.width <= MaxSize.x we can't do anything better  => then we must recalculate the width
       ) and
       (
        (SameValue(fBufSize.cy, MaxSize.y, TEpsilon.position)) or                  // * if we already did the job with maxsize.y then do nothing
        (SameValue(fbufBitmapRect.height, MaxSize.y, TEpsilon.position)) or        // * if fbufBitmapRect.height = MaxSize.y we can not do anything better ;)
        ((fTextControl.Fill.Kind = TbrushKind.None) and                            // * if we don't draw any background  =>
         (fTextControl.stroke.Kind = TbrushKind.None) and                          //                                    =>
         (not (fTextControl.Align in [TAlignLayout.Client,                         //                                    =>
                                      TAlignLayout.Contents,                       //                                    => Else we must have
                                      TAlignLayout.Left,                           //                                    => fbufBitmapRect.height = MaxSize.y
                                      TAlignLayout.Right,                          //                                    =>
                                      TAlignLayout.MostLeft,                       //                                    =>
                                      TAlignLayout.MostRight,                      //                                    =>
                                      TAlignLayout.HorzCenter])) and               //   and if the Height is not aligned =>
         (fBufAllTextDrawed) and                                                   //   and IF all text was drawed                                                 => else if fbufBitmapRect.height > MaxSize.y
         (CompareValue(fbufBitmapRect.height, MaxSize.y, TEpsilon.position) <= 0)) //   and if fbufBitmapRect.height <= MaxSize.y then we can't do anything better => then we must recalculate the height
       )
      )
     ) and
     (fBufText = fTextControl.Text) then exit(fBufBitmap);

  clearBufBitmap;
  fBufHorizontalAlign := fTextControl.TextSettings.HorzAlign;
  fBufVerticalAlign := fTextControl.TextSettings.VertAlign;
  fBufFontColor := fTextControl.TextSettings.FontColor;
  fBuffontFamily := fTextControl.TextSettings.Font.Family;
  fBuffontStyle := fTextControl.TextSettings.Font.Style;
  fBuffontSize := fTextControl.TextSettings.Font.Size;
  fBufLineSpacing := fTextControl.LineSpacing;
  fBufXRadius := fTextControl.XRadius;
  fBufYRadius := fTextControl.YRadius;
  fBufTextIsHtml := fTextControl.TextIsHTML;
  fBufWordWrap := fTextControl.TextSettings.WordWrap;
  fBufAutosize := fTextControl.AutoSize;
  fBufTrimming := fTextControl.TextSettings.Trimming;
  fBufSize := MaxSize;
  fBufText := fTextControl.Text;

  {$IFDEF debug}
  ALLog('TALDoubleBufferedTextLayout.MakeBufBitmap', 'Name: ' + fTextControl.Name +
                                                     'text:' + fBufText +
                                                     ' - MaxSize: '+floattostr(fBufSize.cX)+'x'+floattostr(fBufSize.cY), TalLogType.verbose);
  inc(AlDebugTextMakeBufBitmapCount);
  AlDebugTextMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  //init fBufBitmapRect
  fBufBitmapRect := TRectF.Create(0, 0, fBufSize.cX * FScreenScale, fBufSize.cY * FScreenScale);

  //create aOptions
  LOptions := TALDrawMultiLineTextOptions.Create;
  Try

    //init aOptions
    LOptions.FontName := fBuffontFamily;
    LOptions.FontSize := fBuffontSize * FScreenScale;
    LOptions.FontStyle := fBuffontStyle;
    LOptions.FontColor := fBufFontColor;
    //-----
    //LOptions.EllipsisText: String; // default = '…';
    //LOptions.EllipsisFontStyle: TFontStyles; // default = [];
    //LOptions.EllipsisFontColor: TalphaColor; // default = TAlphaColorRec.Null;
    //-----
    if ((fTextControl.Fill.Kind <> TbrushKind.None) or            // if we don't draw any background then
        (fTextControl.stroke.Kind <> TbrushKind.None)) then begin // always set autosize = true

      if (not fBufAutosize) then LOptions.AutoSize := false // if we ask autosize = false then autosize to false
      //-----
      else if (fTextControl.Align in [TAlignLayout.Top,
                                      TAlignLayout.Bottom,
                                      TAlignLayout.MostTop,
                                      TAlignLayout.MostBottom,
                                      TAlignLayout.VertCenter]) then begin
          LOptions.AutoSize := false;   // if we ask autosize = true and Width is aligned
          LOptions.AutoSizeY := True;   // then autosize only the Y
      end
      //-----
      else if (fTextControl.Align in [TAlignLayout.Left,
                                      TAlignLayout.Right,
                                      TAlignLayout.MostLeft,
                                      TAlignLayout.MostRight,
                                      TAlignLayout.HorzCenter]) then begin
        LOptions.AutoSize := false; // if we ask autosize = true and Height is aligned
        LOptions.AutoSizeX := True; // then autosize only the X
      end
      //-----
      else if (fTextControl.Align in [TAlignLayout.Client,
                                      TAlignLayout.Contents]) then LOptions.AutoSize := false  // if we ask autosize = true and Width & Height are aligned then don't autosize anything
      //-----
      else LOptions.AutoSize := True; // // if we ask autosize = true and Width & Height are not aligned then autosize to true

    end
    else LOptions.AutoSize := True;
    //-----
    LOptions.WordWrap := fBufWordWrap;
    //LOptions.MaxLines: integer; // default = 0;
    LOptions.LineSpacing := FBufLineSpacing * FScreenScale;
    LOptions.Trimming := fBufTrimming;
    //LOptions.FirstLineIndent: TpointF; // default = Tpointf.create(0,0);
    //LOptions.FailIfTextBreaked: boolean; // default = false
    //-----
    LOptions.HTextAlign := fBufHorizontalAlign;
    LOptions.VTextAlign := fBufVerticalAlign;
    //-----
    LOptions.Fill.assign(fTextControl.Fill);
    LOptions.Stroke.assign(fTextControl.Stroke);
    LOptions.Stroke.Thickness := LOptions.Stroke.Thickness * FScreenScale;
    LOptions.Sides := fTextControl.Sides;
    LOptions.XRadius := fBufXRadius * FScreenScale;
    LOptions.YRadius := fBufYRadius * FScreenScale;
    LOptions.Corners := fTextControl.Corners;
    LOptions.Padding := fTextControl.padding.Rect;
    LOptions.Padding.Top := LOptions.Padding.Top * FScreenScale;
    LOptions.Padding.right := LOptions.Padding.right * FScreenScale;
    LOptions.Padding.left := LOptions.Padding.left * FScreenScale;
    LOptions.Padding.bottom := LOptions.Padding.bottom * FScreenScale;
    //-----
    LOptions.TextIsHtml := FBufTextIsHtml;

    //build fBufBitmap
    fBufBitmap := ALDrawMultiLineText(fBufText, // const aText: String; // support only basic html tag like <b>...</b>, <i>...</i>, <font color="#ffffff">...</font> and <span id="xxx">...</span>
                                      fBufBitmapRect, // var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                                      fBufTextBreaked,
                                      fBufAllTextDrawed,
                                      LOptions);
    {$IFDEF debug}
    ALLog('TALDoubleBufferedTextLayout.MakeBufBitmap.ALDrawMultiLineText', 'Name: ' + fTextControl.Name +
                                                                           'text:' + fBufText +
                                                                           ' - fBufBitmapRect: '+floattostr(fBufBitmapRect.width)+'x'+floattostr(fBufBitmapRect.height) +
                                                                           ' - fBufTextBreaked: '+ BoolToStr(fBufTextBreaked, true) +
                                                                           ' - fBufAllTextDrawed: '+ BoolToStr(fBufAllTextDrawed, true), TalLogType.verbose);
    {$endif}

    //align fbufBitmapRect
    if LOptions.AutoSize and (not fBufAutosize) then begin
      case LOptions.HTextAlign of
        TTextAlign.Center: begin
                             fbufBitmapRect.Offset(((fBufSize.cx * FScreenScale) - fbufBitmapRect.width) / 2, 0);
                           end;
        TTextAlign.Trailing: begin
                               fbufBitmapRect.Offset((fBufSize.cx * FScreenScale) - fbufBitmapRect.width, 0);
                             end;
      end;
      case LOptions.VTextAlign of
        TTextAlign.Center: begin
                             fbufBitmapRect.Offset(0, ((fBufSize.cy * FScreenScale) - fbufBitmapRect.Height) / 2);
                           end;
        TTextAlign.Trailing: begin
                               fbufBitmapRect.Offset(0, (fBufSize.cy * FScreenScale) - fbufBitmapRect.Height);
                             end;
      end;
    end;
    if fBufAutosize then fBufBitmapRect.Offset(-fBufBitmapRect.left, -fBufBitmapRect.top);

    //convert fbufBitmapRect do virtual pixel
    fbufBitmapRect.Top := fbufBitmapRect.Top / FScreenScale;
    fbufBitmapRect.right := fbufBitmapRect.right / FScreenScale;
    fbufBitmapRect.left := fbufBitmapRect.left / FScreenScale;
    fbufBitmapRect.bottom := fbufBitmapRect.bottom / FScreenScale;

  finally
    ALFreeAndNil(LOptions);
  end;

  //update the result
  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugTextMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{***************************************************}
procedure TALDoubleBufferedTextLayout.clearBufBitmap;
begin
  ALFreeAndNil(fBufBitmap);
end;

{***************************************************}
procedure TALDoubleBufferedTextLayout.DoRenderLayout;
begin
  MakeBufBitmap; // recreate the fBufBitmap
end;

{*************************************************************************}
procedure TALDoubleBufferedTextLayout.DoDrawLayout(const ACanvas: TCanvas);
var LDestRect: TrectF;
    LDesignatedArea: TrectF;
    LLocation: TPointF;
begin

  MakeBufBitmap;
  if fBufBitmap = nil then exit;

  LDestRect := fBufBitmapRect;
  if fBufAutosize then begin
    LDesignatedArea := FTextControl.localrect;
    case FTextControl.HorzTextAlign of
      TTextAlign.Center: LLocation.X := (LDesignatedArea.Left + LDesignatedArea.Right - LDestRect.Width) / 2;
      TTextAlign.Leading: LLocation.X := LDesignatedArea.Left;
      TTextAlign.Trailing: LLocation.X := LDesignatedArea.Right - LDestRect.Width;
    end;
    case FTextControl.VertTextAlign of
      TTextAlign.Center: LLocation.Y := (LDesignatedArea.Top + LDesignatedArea.Bottom - LDestRect.Height) / 2;
      TTextAlign.Leading: LLocation.Y := LDesignatedArea.Top;
      TTextAlign.Trailing: LLocation.Y := LDesignatedArea.Bottom - LDestRect.Height;
    end;
    LDestRect.SetLocation(LLocation);
  end;

  {$IF DEFINED(ALUseTexture)}

  TCustomCanvasGpu(ACanvas).DrawTexture(ACanvas.AlignToPixel(LDestRect), // ATexRect (destRec)
                                        TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                        ALPrepareColor(TCustomCanvasGpu.ModulateColor, Opacity), // https://quality.embarcadero.com/browse/RSP-15432
                                        fBufBitmap);


  {$ELSE}

  aCanvas.DrawBitmap(fBufBitmap,
                     TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                     aCanvas.AlignToPixel(LDestRect), {DestRect}
                     Opacity, {opacity}
                     true{highSpeed});

  {$ENDIF}

end;

{*******************************************************}
function TALDoubleBufferedTextLayout.GetTextRect: TRectF;
begin
  if fBufBitmap = nil then result := TrectF.Create(0,0,0,0)
  else result := fBufBitmapRect;
end;

{*********************************************************}
function TALDoubleBufferedTextLayout.GetTextHeight: Single;
begin
  result := 0;
end;

{********************************************************}
function TALDoubleBufferedTextLayout.GetTextWidth: Single;
begin
  result := 0;
end;

{*************************************************************************************}
function TALDoubleBufferedTextLayout.DoPositionAtPoint(const APoint: TPointF): Integer;
begin
  result := 0;
end;

{***************************************************************************************}
function TALDoubleBufferedTextLayout.DoRegionForRange(const ARange: TTextRange): TRegion;
begin
  setlength(result, 0);
end;

{**************************************************************************}
procedure TALDoubleBufferedTextLayout.ConvertToPath(const APath: TPathData);
begin
  //do nothing - virtual
end;

{**}
type
  TALTextTextSettings = class(TTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
  published
    property Font;
    property FontColor;
    property Trimming default TTextTrimming.Character;
    property WordWrap default false;
    property HorzAlign default TTextAlign.Leading;
    property VertAlign default TTextAlign.Center;
  end;

{****************************************************************}
constructor TALTextTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  Trimming := TTextTrimming.Character;
  WordWrap := false;
  HorzAlign := TTextAlign.Leading;
  VertAlign := TTextAlign.Center;
end;

{*********************************************}
constructor TALText.Create(AOwner: TComponent);
var LClass: TTextSettingsClass;
begin
  inherited;
  //-----
  FFill := TBrush.Create(TBrushKind.none, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TStrokeBrush.Create(TBrushKind.none, $FF000000);
  FStroke.OnChanged := StrokeChanged;
  FCorners := AllCorners;
  FXRadius := 0;
  FYRadius := 0;
  FSides := AllSides;
  fLineSpacing := 0;
  fTextIsHtml := False;
  fMustCallResized := False;
  //-----
  HitTest := False;
  //-----
  FAutoConvertFontFamily := True;
  FAutoTranslate := true;
  FAutoSize := False;
  fMaxWidth := 65535;
  fMaxHeight := 65535;
  //-----
  LClass := GetTextSettingsClass;
  if LClass = nil then LClass := TALTextTextSettings;
  //-----
  FLayout := TALdoubleBufferedTextLayout.Create(nil, self);
  //-----
  //i use this way to know that the compoment
  //will load it's properties from the dfm
  if (aOwner <> nil) and
     (csloading in aOwner.ComponentState) then begin
    fRestoreLayoutUpdateAfterLoaded := True;
    Layout.BeginUpdate;
  end
  else fRestoreLayoutUpdateAfterLoaded := False;
  //-----
  FTextSettings := LClass.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  FTextSettings.BeginUpdate;
  try
    FTextSettings.IsAdjustChanged := True;
  finally
    FTextSettings.EndUpdate; // << this will not call adjustsize because at this time
                             // << fautosize = false. This will only update the textsettings
                             // << of the flayout, and if we are in csloading, this will not even
                             // << call any DoRenderLayout (else yes it's will call DoRenderLayout
                             // << but with empty text)
  end;
end;

{*************************}
destructor TALText.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FLayout);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FFill);
  inherited;
end;

{***********************}
procedure TALText.Loaded;
begin
  //-----
  if (AutoTranslate) and
     (Text <> '') and
     (not (csDesigning in ComponentState)) then
      Text := ALTranslate(Text);
  //-----
  if (AutoConvertFontFamily) and
     (TextSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
      TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family, TextSettings.Font.Style);
  //-----
  inherited;
  //-----
  if fRestoreLayoutUpdateAfterLoaded then begin
    if (FAutoSize) and
       (Text <> '') then begin

      //Originally, if WordWrap then the algo take in account the current width of the TALText
      //to calculate the autosized width (mean the size can be lower than maxwidth if the current width
      //is already lower than maxwidth). problem with that is if we for exemple change the text (or font
      //dimension) of an already calculated TALText, then it's the old width (that correspond to the
      //previous text/font) that will be taken in account. finally the good way is to alway use the
      //the property maxwidth if we desir a max width and don't rely on the current width
      //if WordWrap then Layout.MaxSize := TPointF.Create(Min(Width, maxWidth), maxHeight)
      //else Layout.MaxSize := TPointF.Create(maxWidth, MaxHeight);

      if (Align in [TAlignLayout.Top,
                    TAlignLayout.Bottom,
                    TAlignLayout.MostTop,
                    TAlignLayout.MostBottom,
                    TAlignLayout.VertCenter]) then Layout.MaxSize := TPointF.Create(Width, maxHeight)
      else if (Align in [TAlignLayout.Left,
                         TAlignLayout.Right,
                         TAlignLayout.MostLeft,
                         TAlignLayout.MostRight,
                         TAlignLayout.HorzCenter]) then Layout.MaxSize := TPointF.Create(maxWidth, Height)
      else if (Align in [TAlignLayout.Client,
                         TAlignLayout.Contents]) then Layout.MaxSize := TPointF.Create(Width, Height)
      else Layout.MaxSize := TPointF.Create(maxWidth, maxHeight);

    end
    else Layout.MaxSize := TPointF.Create(width, height);  // << this is important because else when the component is loaded then
                                                           // << we will call DoRenderLayout that will use the original maxsise (ie: 65535, 65535)
                                                           // << and then after when we will paint the control, we will again call DoRenderLayout
                                                           // << but this time with maxsize = aTextControl.size and off course if wordwrap we will
                                                           // << need to redo the bufbitmap
    Layout.endUpdate;
    AdjustSize;
  end;
  fRestoreLayoutUpdateAfterLoaded := False;
end;

{*******************************}
procedure TALText.PaddingChanged;
begin
  clearBufBitmap;
  if FUpdating = 0 then Repaint;
end;

{*********************************************}
procedure TALText.FillChanged(Sender: TObject);
begin
  clearBufBitmap;
  if FUpdating = 0 then Repaint;
end;

{***********************************************}
procedure TALText.StrokeChanged(Sender: TObject);
begin
  clearBufBitmap;
  if FUpdating = 0 then Repaint;
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
    FYRadius := NewValue;
    Repaint;
  end;
end;
{****************************************************}
procedure TALText.SetLineSpacing(const Value: Single);
begin
  if not sameValue(Value,FlineSpacing,Tepsilon.FontSize) then begin
    FlineSpacing := Value;
    AdjustSize;
    repaint;
  end;
end;

{****************************************************}
procedure TALText.SetTextIsHtml(const Value: Boolean);
begin
  if Value <> FTextIsHTML then begin
    FTextIsHTML := Value;
    AdjustSize;
    repaint;
  end;
end;

{**********************************************}
procedure TALText.SetSides(const Value: TSides);
begin
  if FSides <> Value then begin
    FSides := Value;
    Repaint;
  end;
end;

{***********************************************}
procedure TALText.OnFontChanged(Sender: TObject);
begin
  FontChanged;
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

{****************************}
procedure TALText.FontChanged;
begin
  FLayout.BeginUpdate;
  try
    FLayout.WordWrap := WordWrap;
    FLayout.HorizontalAlign := HorzTextAlign;
    FLayout.VerticalAlign := VertTextAlign;
    FLayout.Color := Color;
    FLayout.Font := Font;
    FLayout.Opacity := AbsoluteOpacity;
    FLayout.Trimming := Trimming;
  finally
    FLayout.EndUpdate;
  end;
  //-----
  if FTextSettings.IsAdjustChanged then AdjustSize;
  Repaint;
end;

{**************************}
procedure TALText.DoRealign;
begin
  //in original delphi source code it's was
  //inherited;
  //AdjustSize;
  //but i think it's must be the oposite !
  //https://quality.embarcadero.com/browse/RSP-15761
  AdjustSize;
  inherited;
end;

{**********************************************}
function TALText.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

{********************************************************}
function TALText.GetTextSettingsClass: TTextSettingsClass;
begin
  Result := nil;
end;

{************************************************************}
procedure TALText.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*********************************************************************}
function TALText.SupportsPaintStage(const Stage: TPaintStage): Boolean;
begin
  Result := Stage in [TPaintStage.All, TPaintStage.Text];
end;

{**********************}
procedure TALText.Paint;
begin
  FLayout.BeginUpdate;
  try
    FLayout.LayoutCanvas := Self.Canvas;
    FLayout.TopLeft := LocalRect.TopLeft;
    FLayout.Opacity := AbsoluteOpacity;
    FLayout.MaxSize := PointF(LocalRect.Width, LocalRect.Height);
  finally
    FLayout.EndUpdate;
  end;
  FLayout.RenderLayout(Canvas);
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{*************************************}
function TALText.GetColor: TAlphaColor;
begin
  Result := FTextSettings.FontColor;
end;

{***************************************************}
procedure TALText.SetColor(const Value: TAlphaColor);
begin
  FTextSettings.FontColor := Value;
end;

{************************************}
function TALText.GetWordWrap: Boolean;
begin
  Result := FTextSettings.WordWrap;
end;

{**************************************************}
procedure TALText.SetWordWrap(const Value: Boolean);
begin
  FTextSettings.WordWrap := Value;
end;

{******************************}
function TALText.GetFont: TFont;
begin
  Result := FTextSettings.Font;
end;

{********************************************}
procedure TALText.SetFont(const Value: TFont);
begin
  FTextSettings.Font := Value;
end;

{********************************************}
function TALText.GetHorzTextAlign: TTextAlign;
begin
  Result := FTextSettings.HorzAlign;
end;

{**********************************************************}
procedure TALText.SetHorzTextAlign(const Value: TTextAlign);
begin
  FTextSettings.HorzAlign := Value;
end;

{********************************************}
function TALText.GetVertTextAlign: TTextAlign;
begin
  Result := FTextSettings.VertAlign;
end;

{**********************************************************}
procedure TALText.SetVertTextAlign(const Value: TTextAlign);
begin
  FTextSettings.VertAlign := Value;
end;

{******************************************}
function TALText.GetTrimming: TTextTrimming;
begin
  Result := FTextSettings.Trimming;
end;

{********************************************************}
procedure TALText.SetTrimming(const Value: TTextTrimming);
begin
  FTextSettings.Trimming := Value;
end;

{**************************************************}
procedure TALText.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
  end;
end;

{***********************}
procedure TALText.Resize;
begin
  inherited;
  AdjustSize;
end;

{*************************}
{$IF CompilerVersion >= 32} // tokyo
procedure TALText.DoResized;
begin
  if not FDisableAlign then inherited DoResized
  else fMustCallResized := True;
end;
{$endif}

{***************************}
procedure TALText.AdjustSize;
var R: TRectF;
    AlignRoot: IAlignRoot;
    LHorzAlign: TTextAlign;
    LVertAlign: TTextAlign;
    LOpacity: Single;
begin
  if (not FDisableAlign) and
     (not (csLoading in ComponentState)) and
     (not (csDestroying in ComponentState)) and
     (not isupdating) and
     (FAutoSize) and
     (Text <> '') and
     (scene <> nil) then begin

    fMustCallResized := False;
    FDisableAlign := True;  // i don't understand why in the original delphi source code they do like this - but i feel lazzy to fully study why so i leave it
                            // this mean that we can't add aligned control inside the TalText because when we will update the size of the taltext via adjustsize
                            // then we will not realign all the childs
                            // NOTE: as this fucking FDisableAlign piss me off because no way to resize the control inside the OnResize event (for exemple by changing the
                            //       font size, I introduce fMustCallResized to call DoResized AFTER we release the FDisableAlign (because stupid to call resized when
                            //       FDisableAlign := True
    try

      LHorzAlign := FLayout.HorizontalAlign;
      LVertAlign := FLayout.VerticalAlign;
      LOpacity := FLayout.Opacity;
      try

        //Originally, if WordWrap then the algo take in account the current width of the TALText
        //to calculate the autosized width (mean the size can be lower than maxwidth if the current width
        //is already lower than maxwidth). problem with that is if we for exemple change the text (or font
        //dimension) of an already calculated TALText, then it's the old width (that correspond to the
        //previous text/font) that will be taken in account. finally the good way is to alway use the
        //the property maxwidth if we desir a max width and don't rely on the current width
        //if WordWrap then Layout.MaxSize := TPointF.Create(Min(Width, maxWidth), maxHeight)
        //else Layout.MaxSize := TPointF.Create(maxWidth, MaxHeight);

        if (Align in [TAlignLayout.Top,
                      TAlignLayout.Bottom,
                      TAlignLayout.MostTop,
                      TAlignLayout.MostBottom,
                      TAlignLayout.VertCenter]) then R := TRectF.Create(0, 0, Width, maxHeight)
        else if (Align in [TAlignLayout.Left,
                           TAlignLayout.Right,
                           TAlignLayout.MostLeft,
                           TAlignLayout.MostRight,
                           TAlignLayout.HorzCenter]) then R := TRectF.Create(0, 0, maxWidth, Height)
        else if (Align in [TAlignLayout.Client,
                           TAlignLayout.Contents]) then R := TRectF.Create(0, 0, Width, Height)
        else R := TRectF.Create(0, 0, maxWidth, maxHeight);

        FLayout.BeginUpdate;
        try
          FLayout.TopLeft := R.TopLeft;
          FLayout.MaxSize := PointF(R.Width, R.Height);
          FLayout.Opacity := AbsoluteOpacity;
          FLayout.HorizontalAlign := TTextAlign.Leading;
          FLayout.VerticalAlign := TTextAlign.Leading;
        finally
          FLayout.EndUpdate;
        end;

        R := FLayout.TextRect;

      finally
        FLayout.BeginUpdate;
        try
          FLayout.Opacity := LOpacity;
          FLayout.HorizontalAlign := LHorzAlign;
          FLayout.VerticalAlign := LVertAlign;
        finally
          FLayout.EndUpdate;
        end;
      end;

      //this to take care of the align constraint of the ftextLayout
      if Align in [TAlignLayout.Client,
                   TAlignLayout.Contents,
                   TAlignLayout.Top,
                   TAlignLayout.Bottom,
                   TAlignLayout.MostTop,
                   TAlignLayout.MostBottom,
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
                   TAlignLayout.HorzCenter] then begin
        r.Top := 0;
        r.height := height;
      end;

      //SetBounds(Position.X, Position.Y, R.Width + R.Left * 2 + FTextSettings.Font.Size / 3, R.Height + R.Top * 2);
      SetBounds(Position.X, Position.Y, R.Width + R.Left * 2, R.Height + R.Top * 2);
      if Supports(Parent, IAlignRoot, AlignRoot) then AlignRoot.Realign;

    finally
      FDisableAlign := False;
    end;

    {$IF CompilerVersion >= 32} // tokyo
    if fMustCallResized then DoResized;
    {$ENDIF}

  end;
end;

{*******************************}
function TALText.GetText: string;
begin
  Result := FLayout.Text;
end;

{*********************************************}
procedure TALText.SetText(const Value: string);
begin
  if Text <> Value then begin
    FLayout.LayoutCanvas := Canvas;
    FLayout.Text := Value;
    AdjustSize;
    Repaint;
  end;
end;

{***************************************************}
procedure TALText.SetParent(const Value: TFmxObject);
begin
  if FParent <> Value then begin
    inherited;
    // stupidly when we do setparent the
    // FisUpdating will be set to the value of the parent BUT without any
    // notifications nor any call to beginupdate :(
    // but when the parent will call endupdate then our EndUpdate will be also called
    if (Value <> nil) and
       (Value <> self) and
       (Value is TControl) then begin
      if TALControlAccessPrivate(Value).FUpdating > TALTextLayoutAccessPrivate(Layout).fupdating then begin
        while TALControlAccessPrivate(Value).FUpdating > TALTextLayoutAccessPrivate(Layout).fupdating do
          Layout.BeginUpdate;
      end
      else if TALControlAccessPrivate(Value).FUpdating < TALTextLayoutAccessPrivate(Layout).fupdating then begin
        while (TALControlAccessPrivate(Value).FUpdating < TALTextLayoutAccessPrivate(Layout).fupdating) and
              (TALControlAccessPrivate(Value).FUpdating > 0) do
          Layout.EndUpdate;
      end;
    end;
  end;
end;

{********************************************}
procedure TALText.SetNewScene(AScene: IScene);
var LParentControl: Tcontrol;
begin
  inherited SetNewScene(AScene);

  LParentControl := parentControl;
  while LParentControl <> nil do begin
    if LParentControl.IsUpdating then exit
    else LParentControl := LParentControl.parentControl;
  end;

  AdjustSize; // << because before scene was maybe nil so adjustsize returned 0
end;

{*******************************}
procedure TALText.clearBufBitmap;
begin
  if not doubleBuffered then exit;
  TALDoubleBufferedTextLayout(Layout).clearBufBitmap;
end;

{*************************}
{$IF DEFINED(ALUseTexture)}
function TALText.MakeBufBitmap: TTexture;
{$ELSE}
function TALText.MakeBufBitmap: Tbitmap;
{$ENDIF}
begin
  if not doubleBuffered then exit(nil);
  FLayout.BeginUpdate;
  try
    FLayout.LayoutCanvas := Self.Canvas;  // useless
    FLayout.TopLeft := LocalRect.TopLeft; // useless
    FLayout.Opacity := AbsoluteOpacity;  // useless
    FLayout.MaxSize := PointF(LocalRect.Width, LocalRect.Height);
  finally
    FLayout.EndUpdate;
  end;
  result := TALDoubleBufferedTextLayout(Layout).MakeBufBitmap;
end;

{******************************************}
function TALText.GetdoubleBuffered: Boolean;
begin
  result := Layout is TALDoubleBufferedTextLayout;
end;

{********************************************************}
procedure TALText.SetdoubleBuffered(const Value: Boolean);
var LText: String;
begin
  if value <> doubleBuffered  then begin
    LText := fLayout.Text;
    ALFreeAndNil(fLayout);
    if value then fLayout := TALdoubleBufferedTextLayout.Create(nil, self)
    else fLayout := TTextLayoutManager.DefaultTextLayout.Create;
    if fRestoreLayoutUpdateAfterLoaded then Layout.BeginUpdate;
    FontChanged;
    FLayout.Text := LText;
    AdjustSize;
  end;
end;

{*************************}
{$IF DEFINED(ALUseTexture)}
function TALText.GetBufBitmap: TTexture;
{$ELSE}
function TALText.GetBufBitmap: Tbitmap;
{$ENDIF}
begin
  if not doubleBuffered then exit(nil);
  result := TALDoubleBufferedTextLayout(Layout).fBufBitmap;
end;

{****************************}
procedure TALText.BeginUpdate;
begin
  inherited;
  Layout.BeginUpdate;
end;

{**************************}
procedure TALText.EndUpdate;
begin
  if (FAutoSize) and
     (Text <> '') then begin

    //Originally, if WordWrap then the algo take in account the current width of the TALText
    //to calculate the autosized width (mean the size can be lower than maxwidth if the current width
    //is already lower than maxwidth). problem with that is if we for exemple change the text (or font
    //dimension) of an already calculated TALText, then it's the old width (that correspond to the
    //previous text/font) that will be taken in account. finally the good way is to alway use the
    //the property maxwidth if we desir a max width and don't rely on the current width
    //if WordWrap then Layout.MaxSize := TPointF.Create(Min(Width, maxWidth), maxHeight)
    //else Layout.MaxSize := TPointF.Create(maxWidth, MaxHeight);

    if (Align in [TAlignLayout.Top,
                  TAlignLayout.Bottom,
                  TAlignLayout.MostTop,
                  TAlignLayout.MostBottom,
                  TAlignLayout.VertCenter]) then Layout.MaxSize := TPointF.Create(Width, maxHeight)
    else if (Align in [TAlignLayout.Left,
                       TAlignLayout.Right,
                       TAlignLayout.MostLeft,
                       TAlignLayout.MostRight,
                       TAlignLayout.HorzCenter]) then Layout.MaxSize := TPointF.Create(maxWidth, Height)
    else if (Align in [TAlignLayout.Client,
                       TAlignLayout.Contents]) then Layout.MaxSize := TPointF.Create(Width, Height)
    else Layout.MaxSize := TPointF.Create(maxWidth, maxHeight);

  end
  else Layout.MaxSize := TPointF.Create(width, height);  // << this is important because else when the component is loaded then
                                                         // << we will call DoRenderLayout that will use the original maxsise (ie: 65535, 65535)
                                                         // << and then after when we will paint the control, we will again call DoRenderLayout
                                                         // << but this time with maxsize = aTextControl.size and off course if wordwrap we will
                                                         // << need to redo the bufbitmap
  Layout.EndUpdate;
  inherited; // will call dorealign that will call AdjustSize
end;

{************************************}
function TALText.TextBreaked: Boolean;
begin
  if not doubleBuffered then exit(false);
  result := (TALdoubleBufferedTextLayout(fLayout).fbufBitmap <> nil) and
            (TALdoubleBufferedTextLayout(fLayout).fBufTextBreaked);
end;

{*************************************************}
procedure TALText.SetMaxWidth(const Value: Single);
begin
  if compareValue(fMaxWidth, Value, Tepsilon.position) <> 0 then begin
    fMaxWidth := Value;
    AdjustSize;
  end;
end;

{**************************************************}
procedure TALText.SetMaxHeight(const Value: Single);
begin
  if compareValue(fMaxHeight, Value, Tepsilon.position) <> 0 then begin
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
//call endupdate to the most far away childreen, and
//go up like :
//  acontrol1
//    acontrol2
//      aalText1
//then doing acontrol1.endupdate then we will do in this order :
//      aalText1.endupdate => realign and then adjustsize
//    acontrol2.endupdate => realign and then maybe again TalText1.adjustsize
//  acontrol1.endupdate => realign and then maybe again TalText1.adjustsize
//this is a problem because we will calculate several time the bufbitmap
//to mitigate this we can do
//  ALLockTexts(acontrol1);
//  acontrol1.endupdate;
//  ALUnLockTexts(acontrol1);
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

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALImage, TALRectangle, TALCircle, TALLine, TALText]);
end;

initialization
  RegisterFmxClasses([TALImage, TALRectangle, TALCircle, TALLine, TALText]);
  {$IFDEF debug}
  AlDebugImageMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugRectangleMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugCircleMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugLineMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugTextMakeBufBitmapStopWatch := TstopWatch.Create;
  {$endif}

end.

