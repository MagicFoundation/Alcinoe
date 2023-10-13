unit Alcinoe.FMX.Objects;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported}
  {$MESSAGE WARN 'Check if FMX.Objects.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Classes,
  System.Types,
  System.UITypes, // [DCC Hint] Alcinoe.FMX.Objects.pas(1418): H2443 Inline function 'TAlphaColorCGFloat.Create' has not been expanded because unit 'System.UITypes' is not specified in USES list
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
  Alcinoe.FMX.Graphics,
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
    FScreenScale: single;
    fBufBitmap: TALRasterImage;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure setFileName(const Value: String);
    procedure setResourceName(const Value: String);
  protected
    procedure Paint; override;
    property BufBitmap: TALRasterImage read fBufBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MakeBufBitmap: TALRasterImage; virtual;
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
    FScreenScale: single;
    fdoubleBuffered: boolean;
    fBufBitmap: TALRasterImage;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fShadow: TALShadow;
    FAutoSize: Boolean;
    procedure SetdoubleBuffered(const Value: Boolean);
    procedure SetShadow(const Value: TALShadow);
    procedure SetAutoSize(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure DoRealign; override;
    procedure AdjustSize; virtual;
    procedure Paint; override;
    property BufBitmap: TALRasterImage read fBufBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MakeBufBitmap: TALRasterImage; virtual;
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
    property shadow: TALShadow read fshadow write SetShadow;
    // Dynamically adjusts the dimensions to accommodate child controls,
    // considering their sizes, positions, margins, and alignments.
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCircle = class(TCircle)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    fBufBitmap: TALRasterImage;
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
    property BufBitmap: TALRasterImage read fBufBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MakeBufBitmap: TALRasterImage; virtual;
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
    fBufBitmap: TALRasterImage;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    property BufBitmap: TALRasterImage read fBufBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    function MakeBufBitmap: TALRasterImage; virtual;
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALText = class(TControl)
  private
    FScreenScale: single;
    fBufBitmap: TALRasterImage;
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    fBufTextBroken: Boolean;
    fBufAllTextDrawn: Boolean;
    FAutoTranslate: Boolean;
    FAutoConvertFontFamily: boolean;
    FText: String;
    FTextSettings: TTextSettings;
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
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetHorzTextAlign(const Value: TTextAlign);
    procedure SetVertTextAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure OnFontChanged(Sender: TObject);
    procedure SetTextSettings(const Value: TTextSettings);
    function GetColor: TAlphaColor;
    function GetFont: TFont;
    function GetHorzTextAlign: TTextAlign;
    function GetTrimming: TTextTrimming;
    function GetVertTextAlign: TTextAlign;
    function GetWordWrap: Boolean;
    procedure SetMaxWidth(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
  protected
    property BufBitmap: TALRasterImage read FBufBitmap;
    procedure PaddingChanged; override;
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetLineSpacing(const Value: Single); virtual;
    procedure SetTextIsHtml(const Value: Boolean); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure FontChanged; virtual;
    function GetTextSettingsClass: TTextSettingsClass; virtual;
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
    function MakeBufBitmap: TALRasterImage; virtual;
    procedure clearBufBitmap; virtual;
    function TextBroken: Boolean;
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
    property Text: string read FText write SetText;
    property TextSettings: TTextSettings read fTextSettings write SetTextSettings;
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
    property OnResized;
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
  Alcinoe.FMX.Types3D,
  {$ENDIF}
  Alcinoe.StringUtils,
  Alcinoe.FMX.BreakText,
  Alcinoe.Common;

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

{**********************************************}
function TALImage.MakeBufBitmap: TALRasterImage;
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

  {$IFDEF debug}
  if FBufBitmap <> nil then
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
  else begin
    if TFile.Exists(getActiveProject.fileName) then begin
      var LDProjSrc := ALGetStringFromFile(getActiveProject.fileName, TEncoding.utf8);
      //<RcItem Include="resources\account_100x100.png">
      //    <ResourceType>RCDATA</ResourceType>
      //    <ResourceId>account_100x100</ResourceId>
      //</RcItem>
      Var P1: Integer := ALposIgnoreCaseW('<ResourceId>'+fResourceName+'</ResourceId>', LDProjSrc);
      While (P1 > 1) and ((LDProjSrc[P1-1] <> '=') or (LDProjSrc[P1] <> '"')) do dec(P1);
      if (P1 > 0) then begin
        var P2: Integer := ALPosW('"', LDProjSrc, P1+1);
        if P2 > P1 then begin
          LFileName := extractFilePath(getActiveProject.fileName) + ALcopyStr(LDProjSrc, P1+1, P2-P1-1);
          if not TFile.Exists(LFileName) then LFileName := '';
        end;
      end;
    end;
    if LFileName = '' then begin
      LFileName := extractFilePath(getActiveProject.fileName) + 'Resources\' + fResourceName; // by default all the resources files must be located in the sub-folder /Resources/ of the project
      if not TFile.Exists(LFileName) then begin
        LFileName := LFileName + '.png';
        if not TFile.Exists(LFileName) then LFileName := '';
      end;
    end;
  end;
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

  if result <> nil then
    fBufBitmapRect := TrectF.Create(0,0, result.Width/FScreenScale, result.Height/FScreenScale).
                        CenterAt(LocalRect);

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

  TCustomCanvasGpu(Canvas).DrawTexture(
    canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
    ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(
    fBufBitmap,
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
  FAutoSize := False;
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
  ALFreeAndNil(fBufBitmap);
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
     (FAutoSize) then begin // if FAutoSize is false nothing to adjust

    var LSize := TSizeF.Create(0,0);
    for var Lcontrol in Controls do begin
      case Lcontrol.Align of

        //--
        TAlignLayout.None,
        TAlignLayout.Center:;

        //--
        TAlignLayout.Top,
        TAlignLayout.MostTop,
        TAlignLayout.Bottom,
        TAlignLayout.MostBottom: begin
          LSize.Width := Max(LSize.Width, Width);
          LSize.height := Max(LSize.height, Lcontrol.Position.Y + Lcontrol.Height + Lcontrol.Margins.bottom + padding.bottom);
        end;

        //--
        TAlignLayout.Left,
        TAlignLayout.MostLeft,
        TAlignLayout.Right,
        TAlignLayout.MostRight: Begin
          LSize.Width := Max(LSize.Width, Lcontrol.Position.X + Lcontrol.width + Lcontrol.Margins.right + padding.right);
          LSize.height := Max(LSize.Height, Height);
        End;

        //--
        TAlignLayout.Client,
        TAlignLayout.Contents,
        TAlignLayout.Scale,
        TAlignLayout.Fit,
        TAlignLayout.FitLeft,
        TAlignLayout.FitRight: Begin
          LSize.Width := Max(LSize.Width, Width);
          LSize.height := Max(LSize.Height, Height);
        End;

        //--
        TAlignLayout.Horizontal,
        TAlignLayout.VertCenter: Begin
          LSize.Width := Max(LSize.Width, Width);
        End;

        //--
        TAlignLayout.Vertical,
        TAlignLayout.HorzCenter: Begin
          LSize.height := Max(LSize.Height, Height);
        End;

      end;
    end;

    // This to take care of the align constraint
    if Align in [TAlignLayout.Client,
                 TAlignLayout.Contents,
                 TAlignLayout.Top,
                 TAlignLayout.Bottom,
                 TAlignLayout.MostTop,
                 TAlignLayout.MostBottom,
                 TAlignLayout.Horizontal,
                 TAlignLayout.VertCenter] then begin
      LSize.Width := Width;
    end;
    if Align in [TAlignLayout.Client,
                 TAlignLayout.Contents,
                 TAlignLayout.Left,
                 TAlignLayout.Right,
                 TAlignLayout.MostLeft,
                 TAlignLayout.MostRight,
                 TAlignLayout.Vertical,
                 TAlignLayout.HorzCenter] then begin
      LSize.height := height;
    end;

    if LSize.Width = 0 then LSize.Width := Width;
    if LSize.Height = 0 then LSize.Height := Height;
    SetBounds(Position.X, Position.Y, LSize.Width, LSize.Height);

  end;
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

{**************************************************}
function TALRectangle.MakeBufBitmap: TALRasterImage;

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

  {$IFDEF debug}
  if FBufBitmap <> nil then
    ALLog('TALRectangle.MakeBufBitmap', 'BufBitmap is being recreated | Name: ' + Name, TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := Size.Size;

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
    ALCreateDrawingSurface(
      LBitmap, // Var aBitmap: Jbitmap;
      LCanvas, // var aCanvas: Jcanvas;
      round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
      round(fBufBitmapRect.height * FScreenScale));// const h: integer)
    try

       ALPaintRectangle(
         LCanvas, // const aBitmap: Jbitmap;
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
    ALCreateDrawingSurface(
      LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
      LContext, //    Var aContext: CGContextRef;
      LColorSpace, // Var aColorSpace: CGColorSpaceRef;
      round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
      round(fBufBitmapRect.height * FScreenScale));// const h: integer)
    try

       ALPaintRectangle(
         LContext, // const aContext: CGContextRef;
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
      ALFreeDrawingSurface(
        LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
        LContext, // Var aContext: CGContextRef;
        LColorSpace); // Var aColorSpace: CGColorSpaceRef;
    end;

    {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

    //create the drawing surface
    ALCreateDrawingSurface(
      fBufBitmap, // Var aBitmap: Tbitmap;
      true, // const aClearBitmap: boolean;
      round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
      round(fBufBitmapRect.height * FScreenScale));// const h: integer)
    try

      //begin the scene
      if fBufBitmap.Canvas.BeginScene then
      try

        ALPaintRectangle(
          fBufBitmap.Canvas, // const aBitmap: Jbitmap;
          LRect, // const Rect: TrectF;
          Fill, // const Fill: TBrush;
          Stroke, // const Stroke: TStrokeBrush;
          Shadow, // const Shadow: TALShadow
          Sides, // const Sides: TSides;
          Corners, // const Corners: TCorners;
          XRadius * fScreenScale, // const XRadius: Single = 0;
          YRadius * fScreenScale); // const YRadius: Single = 0);

      finally
        fBufBitmap.Canvas.EndScene;
      end;

    Except
      ALFreeDrawingSurface(fBufBitmap);
      raise;
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

  TCustomCanvasGpu(Canvas).DrawTexture(
    canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
    ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(
    fBufBitmap,
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
  Repaint;
end;

{***********************************************}
function TALCircle.MakeBufBitmap: TALRasterImage;

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

  {$IFDEF debug}
  if FBufBitmap <> nil then
    ALLog('TALCircle.MakeBufBitmap', 'BufBitmap is being recreated | Name: ' + Name, TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := Size.Size;

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
    ALCreateDrawingSurface(
      LBitmap, // Var aBitmap: Jbitmap;
      LCanvas, // var aCanvas: Jcanvas;
      round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
      round(fBufBitmapRect.Height * FScreenScale));// const h: integer)
    try

      ALPaintCircle(
        LCanvas, // const aBitmap: Jbitmap;
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
    ALCreateDrawingSurface(
      LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
      LContext, //    Var aContext: CGContextRef;
      LColorSpace, // Var aColorSpace: CGColorSpaceRef;
      round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
      round(fBufBitmapRect.Height * FScreenScale));// const h: integer)
    try

      ALPaintCircle(
        LContext, // const aContext: CGContextRef;
        LColorSpace, // const aColorSpace: CGColorSpaceRef;
        LBitmapSurface.Height, // const aGridHeight: Single;
        LRect, // const Rect: TrectF;
        Fill, // const Fill: TBrush;
        Stroke, // const Stroke: TStrokeBrush;
        Shadow); // const Shadow: TALShadow

      fBufBitmap := ALBitmapSurfacetoTexture(LBitmapSurface);

    finally
      ALFreeDrawingSurface(
        LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
        LContext, // Var aContext: CGContextRef;
        LColorSpace); // Var aColorSpace: CGColorSpaceRef;
    end;

    {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

    //create the drawing surface
    ALCreateDrawingSurface(
      fBufBitmap, // Var aBitmap: Tbitmap;
      true, // const aClearBitmap: boolean;
      round(fBufBitmapRect.Width * FScreenScale), // const w: integer;
      round(fBufBitmapRect.height * FScreenScale));// const h: integer)
    try

      //begin the scene
      if fBufBitmap.Canvas.BeginScene then
      try

        ALPaintCircle(
          fBufBitmap.Canvas, // const aBitmap: Jbitmap;
          LRect, // const Rect: TrectF;
          Fill, // const Fill: TBrush;
          Stroke, // const Stroke: TStrokeBrush;
          Shadow); // const Shadow: TALShadow

      finally
        fBufBitmap.Canvas.EndScene;
      end;

    Except
      ALFreeDrawingSurface(fBufBitmap);
      raise;
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

  TCustomCanvasGpu(Canvas).DrawTexture(
    canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
    ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(
    fBufBitmap,
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

{*********************************************}
function TALLine.MakeBufBitmap: TALRasterImage;

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

  {$IFDEF debug}
  if FBufBitmap <> nil then
    ALLog('TALLine.MakeBufBitmap', 'BufBitmap is being recreated | Name: ' + Name, TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := Size.Size;

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
  ALCreateDrawingSurface(
    LBitmap, // Var aBitmap: Jbitmap;
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
  ALCreateDrawingSurface(
    LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
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
    ALFreeDrawingSurface(
      LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
      LContext, // Var aContext: CGContextRef;
      LColorSpace); // Var aColorSpace: CGColorSpaceRef;
  end;

  {$ENDIF}

  result := fBufBitmap;

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

  TCustomCanvasGpu(Canvas).DrawTexture(
    canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
    ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(
    fBufBitmap,
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
begin
  inherited Create(AOwner);
  var LScreenSrv: IFMXScreenService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then FScreenScale := LScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fBufBitmap := nil;
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
  //-----
  HitTest := False;
  //-----
  FAutoConvertFontFamily := True;
  FAutoTranslate := true;
  FAutoSize := False;
  fMaxWidth := 65535;
  fMaxHeight := 65535;
  FText := '';
  //-----
  var LClass := GetTextSettingsClass;
  if LClass = nil then LClass := TALTextTextSettings;
  FTextSettings := LClass.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
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

  if (AutoConvertFontFamily) and
     (TextSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
    TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family);

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
{****************************************************}
procedure TALText.SetLineSpacing(const Value: Single);
begin
  if not sameValue(Value,FlineSpacing,Tepsilon.FontSize) then begin
    clearBufBitmap;
    FlineSpacing := Value;
    AdjustSize;
    repaint;
  end;
end;

{****************************************************}
procedure TALText.SetTextIsHtml(const Value: Boolean);
begin
  if Value <> FTextIsHTML then begin
    clearBufBitmap;
    FTextIsHTML := Value;
    AdjustSize;
    repaint;
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
  ClearBufBitmap;
  if FTextSettings.IsAdjustChanged then AdjustSize;
  Repaint;
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

{**********************}
procedure TALText.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  var LDestRect := fBufBitmapRect;
  if Autosize then begin
    var LLocation: TPointF;
    var LDesignatedArea := localrect;
    case HorzTextAlign of
      TTextAlign.Center: LLocation.X := (LDesignatedArea.Left + LDesignatedArea.Right - LDestRect.Width) / 2;
      TTextAlign.Leading: LLocation.X := LDesignatedArea.Left;
      TTextAlign.Trailing: LLocation.X := LDesignatedArea.Right - LDestRect.Width;
    end;
    case VertTextAlign of
      TTextAlign.Center: LLocation.Y := (LDesignatedArea.Top + LDesignatedArea.Bottom - LDestRect.Height) / 2;
      TTextAlign.Leading: LLocation.Y := LDesignatedArea.Top;
      TTextAlign.Trailing: LLocation.Y := LDesignatedArea.Bottom - LDestRect.Height;
    end;
    LDestRect.SetLocation(LLocation);
  end;

  {$IF DEFINED(ALUseTexture)}

  TCustomCanvasGpu(Canvas).DrawTexture(
    Canvas.AlignToPixel(LDestRect), // ATexRect (destRec)
    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
    ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    fBufBitmap);

  {$ELSE}

  Canvas.DrawBitmap(
    fBufBitmap,
    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
    Canvas.AlignToPixel(LDestRect), {DestRect}
    AbsoluteOpacity, {opacity}
    true{highSpeed});

  {$ENDIF}

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
  ALFreeAndNil(fBufBitmap);
end;

{*********************************************}
function TALText.MakeBufBitmap: TALRasterImage;
begin

  if (Scene = nil) or // Scene = nil mean mostly the fTextControl (or his parent) is not yet assigned to any form
     (text.IsEmpty) then begin
    clearBufBitmap;
    exit(nil);
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

  if (fBufBitmap <> nil) and
     (
      (
       (not AutoSize) and                                            // if NOT autosize then the dimensions returned
       (SameValue(fBufSize.cx, LMaxSize.CX, TEpsilon.position)) and  // by this function will depend of MaxSize.X / MaxSize.Y
       (SameValue(fBufSize.cy, LMaxSize.CY, TEpsilon.position))      // so if fBufSize = MaxSize do nothing
      )
      OR
      (
       (AutoSize) and                                                                // * IF autosize
       (
        (SameValue(fBufSize.cx, LMaxSize.cx, TEpsilon.position)) or                  // * if we already did the job with maxsize.x then do nothing
        (SameValue(fbufBitmapRect.width, LMaxSize.cx, TEpsilon.position)) or         // * if fbufBitmapRect.width = MaxSize.x we can not do anything better ;)
        ((Fill.Kind = TbrushKind.None) and                                           // * if we don't draw any background =>
         (stroke.Kind = TbrushKind.None) and                                         //                                   =>
         (not (Align in [TAlignLayout.Client,                                        //                                   =>
                         TAlignLayout.Contents,                                      //                                   => Else we must have
                         TAlignLayout.Top,                                           //                                   => fbufBitmapRect.width = MaxSize.x
                         TAlignLayout.Bottom,                                        //                                   =>
                         TAlignLayout.MostTop,                                       //                                   =>
                         TAlignLayout.MostBottom,                                    //                                   =>
                         TAlignLayout.Horizontal,                                    //                                   =>
                         TAlignLayout.VertCenter])) and                              //   and if the Width is not aligned =>
         (not fBufTextBroken) and                                                    //   and If the text wasn't broken                                         => else if fbufBitmapRect.width > MaxSize.x
         (CompareValue(fbufBitmapRect.width, LMaxSize.cx, TEpsilon.position) <= 0))  //   and if fbufBitmapRect.width <= MaxSize.x we can't do anything better  => then we must recalculate the width
       ) and
       (
        (SameValue(fBufSize.cy, LMaxSize.cy, TEpsilon.position)) or                  // * if we already did the job with maxsize.y then do nothing
        (SameValue(fbufBitmapRect.height, LMaxSize.cy, TEpsilon.position)) or        // * if fbufBitmapRect.height = MaxSize.y we can not do anything better ;)
        ((Fill.Kind = TbrushKind.None) and                                           // * if we don't draw any background  =>
         (stroke.Kind = TbrushKind.None) and                                         //                                    =>
         (not (Align in [TAlignLayout.Client,                                        //                                    =>
                         TAlignLayout.Contents,                                      //                                    => Else we must have
                         TAlignLayout.Left,                                          //                                    => fbufBitmapRect.height = MaxSize.y
                         TAlignLayout.Right,                                         //                                    =>
                         TAlignLayout.MostLeft,                                      //                                    =>
                         TAlignLayout.MostRight,                                     //                                    =>
                         TAlignLayout.Vertical,                                      //                                    =>
                         TAlignLayout.HorzCenter])) and                              //   and if the Height is not aligned =>
         (fBufAllTextDrawn) and                                                      //   and IF all text was Drawn                                                  => else if fbufBitmapRect.height > MaxSize.y
         (CompareValue(fbufBitmapRect.height, LMaxSize.cy, TEpsilon.position) <= 0)) //   and if fbufBitmapRect.height <= MaxSize.y then we can't do anything better => then we must recalculate the height
       )
      )
     ) then
    exit(fBufBitmap);

  {$IFDEF debug}
  if FBufBitmap <> nil then
    ALLog(
      'TALText.MakeBufBitmap',
      'BufBitmap is being recreated | ' +
      'Name: ' + Name + ' | ' +
      'text:' + Text + ' | ' +
      'MaxSize: '+floattostr(fBufSize.cX)+'x'+floattostr(fBufSize.cY),
      TalLogType.warn);
  {$endif}
  clearBufBitmap;
  fBufSize := LMaxSize;

  //init fBufBitmapRect
  fBufBitmapRect := TRectF.Create(0, 0, fBufSize.cX * FScreenScale, fBufSize.cY * FScreenScale);

  //create aOptions
  var LOptions := TALDrawMultiLineTextOptions.Create;
  Try

    //init aOptions
    LOptions.FontName := TextSettings.font.Family;
    LOptions.FontSize := TextSettings.font.Size * FScreenScale;
    LOptions.FontStyle := TextSettings.font.Style;
    LOptions.FontColor := TextSettings.FontColor;
    //-----
    //LOptions.EllipsisText: String; // default = '…';
    //LOptions.EllipsisFontStyle: TFontStyles; // default = [];
    //LOptions.EllipsisFontColor: TalphaColor; // default = TAlphaColorRec.Null;
    //-----
    if ((Fill.Kind <> TbrushKind.None) or            // if we don't draw any background then
        (stroke.Kind <> TbrushKind.None)) then begin // always set autosize = true

      if (not Autosize) then LOptions.AutoSize := false // if we ask autosize = false then autosize to false
      //-----
      else if (Align in [TAlignLayout.Top,
                         TAlignLayout.Bottom,
                         TAlignLayout.MostTop,
                         TAlignLayout.MostBottom,
                         TAlignLayout.Horizontal,
                         TAlignLayout.VertCenter]) then begin
          LOptions.AutoSize := false;   // if we ask autosize = true and Width is aligned
          LOptions.AutoSizeY := True;   // then autosize only the Y
      end
      //-----
      else if (Align in [TAlignLayout.Left,
                         TAlignLayout.Right,
                         TAlignLayout.MostLeft,
                         TAlignLayout.MostRight,
                         TAlignLayout.Vertical,
                         TAlignLayout.HorzCenter]) then begin
        LOptions.AutoSize := false; // if we ask autosize = true and Height is aligned
        LOptions.AutoSizeX := True; // then autosize only the X
      end
      //-----
      else if (Align in [TAlignLayout.Client,
                         TAlignLayout.Contents]) then LOptions.AutoSize := false  // if we ask autosize = true and Width & Height are aligned then don't autosize anything
      //-----
      else LOptions.AutoSize := True; // // if we ask autosize = true and Width & Height are not aligned then autosize to true

    end
    else LOptions.AutoSize := True;
    //-----
    LOptions.WordWrap := TextSettings.WordWrap;
    //LOptions.MaxLines: integer; // default = 0;
    LOptions.LineSpacing := LineSpacing * FScreenScale;
    LOptions.Trimming := TextSettings.Trimming;
    //LOptions.FirstLineIndent: TpointF; // default = Tpointf.create(0,0);
    //LOptions.FailIfTextBroken: boolean; // default = false
    //-----
    LOptions.HTextAlign := TextSettings.HorzAlign;
    LOptions.VTextAlign := TextSettings.VertAlign;
    //-----
    LOptions.Fill.assign(Fill);
    LOptions.Stroke.assign(Stroke);
    LOptions.Stroke.Thickness := LOptions.Stroke.Thickness * FScreenScale;
    LOptions.Sides := Sides;
    LOptions.XRadius := XRadius * FScreenScale;
    LOptions.YRadius := YRadius * FScreenScale;
    LOptions.Corners := Corners;
    LOptions.Padding := padding.Rect;
    LOptions.Padding.Top := LOptions.Padding.Top * FScreenScale;
    LOptions.Padding.right := LOptions.Padding.right * FScreenScale;
    LOptions.Padding.left := LOptions.Padding.left * FScreenScale;
    LOptions.Padding.bottom := LOptions.Padding.bottom * FScreenScale;
    //-----
    LOptions.TextIsHtml := TextIsHtml;

    //build fBufBitmap
    fBufBitmap := ALDrawMultiLineText(
                    Text, // const aText: String; // support only basic html tag like <b>...</b>, <i>...</i>, <font color="#ffffff">...</font> and <span id="xxx">...</span>
                    fBufBitmapRect, // var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                    fBufTextBroken,
                    fBufAllTextDrawn,
                    LOptions);

    //align fbufBitmapRect
    if LOptions.AutoSize and (not Autosize) then begin
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
    if Autosize then fBufBitmapRect.Offset(-fBufBitmapRect.left, -fBufBitmapRect.top);

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

end;

{***********************************}
function TALText.TextBroken: Boolean;
begin
  result := (fbufBitmap <> nil) and (fBufTextBroken);
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

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALImage, TALRectangle, TALCircle, TALLine, TALText]);
end;

initialization
  RegisterFmxClasses([TALImage, TALRectangle, TALCircle, TALLine, TALText]);

end.
