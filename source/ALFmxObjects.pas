unit ALFmxObjects;

interface

uses System.Classes,
     System.Types,
     System.UITypes, // [DCC Hint] ALFmxObjects.pas(1418): H2443 Inline function 'TAlphaColorCGFloat.Create' has not been expanded because unit 'System.UITypes' is not specified in USES list
     System.Rtti,
     {$IFDEF DEBUG}
     System.Diagnostics,
     {$ENDIF}
     {$IF defined(ANDROID)}
     system.Messaging,
     system.Generics.collections,
     Androidapi.JNI.JavaTypes,
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
     FMX.textlayout,
     FMX.graphics,
     FMX.objects;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRectangle = class(TRectangle)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCircle = class(TCircle)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  TALLine = class(TLine)
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IF defined(android) or defined(IOS)}
  TALDoubleBufferedTextLayoutNG = class(TTextLayoutNG)
  private
    FScreenScale: single;
    [weak] fTextControl: TALText;
    fdoubleBuffered: boolean;
    fBufBitmap: TTexture;
    fBufBitmapRect: TRectF;
    //-----
    fBufHorizontalAlign: TTextAlign;
    fBufVerticalAlign: TTextAlign;
    fBuffontColor: TAlphaColor;
    fBuffontFamily: TFontName;
    fBuffontStyle: TFontStyles;
    fBuffontSize: Single;
    fBufWordWrap: Boolean;
    fBufAutosize: Boolean;
    fBufTrimming: TTextTrimming;
    fBufSize: TsizeF;
    fBufText: string;
    fBufTextBreaked: Boolean;
    //-----
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure DoRenderLayout; override;
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    function GetTextRect: TRectF; override;
  public
    constructor Create(const ACanvas: TCanvas; const aTextControl: TALText); reintroduce;
    destructor Destroy; override;
    function MakeBufBitmap: TTexture; virtual;
    procedure clearBufBitmap; virtual;
    property doubleBuffered: Boolean read fdoubleBuffered write SetdoubleBuffered;
  end;
  {$ENDIF}

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
  TALText = class(TControl)
  private
    fRestoreLayoutUpdateAfterLoaded: boolean;
    {$IF (not DEFINED(IOS)) and (not DEFINED(ANDROID))}
    fdoubleBuffered: boolean;
    {$ENDIF}
    FAutoTranslate: Boolean;
    FAutoConvertFontFamily: boolean;
    FTextSettings: TTextSettings;
    FLayout: TTextLayout;
    FAutoSize: Boolean;
    fMaxWidth: Single;
    fMaxHeight: Single;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
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
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
  protected
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read GetBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read GetBufBitmap;
    {$ENDIF}
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
    procedure Loaded; override;
    property Layout: TTextLayout read FLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNewScene(AScene: IScene); override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure BeginUpdate; override; // this is neccessary because the MakeBufBitmap is not only call during the paint,
    procedure EndUpdate; override;   // but also when any property changed because need to retrieve the dimension
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
    property MaxWidth: single read fMaxWidth write fMaxWidth stored IsMaxWidthStored;       // these properties are usefull when used
    property MaxHeight: single read fMaxHeight write fMaxHeight stored IsMaxHeightStored;      // with autosize
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
    property doubleBuffered: Boolean read GetdoubleBuffered write setdoubleBuffered default true;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property AutoConvertFontFamily: Boolean read FAutoConvertFontFamily write fAutoConvertFontFamily default true;
  end;

procedure ALLockTexts(const aParentControl: Tcontrol);
procedure ALUnLockTexts(const aParentControl: Tcontrol);

{$IFDEF debug}
var
  AlDebugRectangleMakeBufBitmapCount: integer;
  AlDebugCircleMakeBufBitmapCount: integer;
  AlDebugLineMakeBufBitmapCount: integer;
  AlDebugTextMakeBufBitmapCount: integer;
  AlDebugTextInheritedDoRenderLayoutCount: integer;
  AlDebugTextInheritedDoDrawLayoutCount: integer;

  AlDebugRectangleMakeBufBitmapStopWatch: TstopWatch;
  AlDebugCircleMakeBufBitmapStopWatch: TstopWatch;
  AlDebugLineMakeBufBitmapStopWatch: TstopWatch;
  AlDebugTextMakeBufBitmapStopWatch: TstopWatch;
{$endif}

procedure Register;

implementation

uses system.SysUtils,
     system.Math,
     system.Math.Vectors,
     fmx.consts,
     fmx.platform,
     {$IF defined(ANDROID)}
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.Bitmap,
     Androidapi.Helpers,
     FMX.Canvas.GPU,
     FMX.Helpers.Android,
     FMX.Surfaces,
     ALFmxTypes3D,
     {$ENDIF}
     {$IF defined(IOS)}
     iOSapi.CocoaTypes,
     iOSapi.CoreGraphics,
     iOSapi.CoreText,
     iOSapi.UIKit,
     FMX.Canvas.GPU,
     FMX.Surfaces,
     ALFmxTypes3D,
     {$ENDIF}
     ALCommon,
     ALFmxCommon;

{**************************************************}
constructor TALRectangle.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{******************************}
destructor TALRectangle.Destroy;
begin
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
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

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALRectangle.MakeBufBitmap: TTexture;
{$ELSE}
function TALRectangle.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IF defined(IOS)}
const aDefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var aBitmap: Jbitmap;
    aTmpBitmap: Jbitmap;
    aBitmapSurface: TBitmapSurface;
    aShader: JRadialGradient;
    aCanvas: Jcanvas;
    aPaint: JPaint;
    aRect: TRectf;
    aColors: TJavaArray<Integer>;
    aStops: TJavaArray<Single>;
    aPorterDuffXfermode: jPorterDuffXfermode;
    aBitmapInfo: AndroidBitmapInfo;
    aPixelBuffer: Pointer;
    aBitmapData: TBitmapData;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    i: integer;
{$ELSEIF defined(IOS)}
var aBitmapSurface: TbitmapSurface;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    aAlphaColor: TAlphaColorCGFloat;
    aCallback: CGFunctionCallbacks;
    aShading: CGShadingRef;
    aFunc: CGFunctionRef;
    aRect: TRectf;
    aBitmapData: TBitmapData;
    aTMPContext: CGContextRef;
    aImageRef: CGImageRef;
    aImage: UIImage;
{$ENDIF}

  {$IF defined(ANDROID)}
  procedure _drawRect(Const aDrawOnlyBorder: Boolean);
  var aJRect: JRectF;
      aPath: JPath;
      aXRadius: single;
      aYradius: Single;
      aWidthMinusCorners: single;
      aHeightMinusCorners: Single;
      aCorners: TCorners;
      aHalfStrokeWidth: Single;
  begin

    // use drawRoundRect
    if ((compareValue(xRadius, 0, TEpsilon.position) > 0) and
        (compareValue(YRadius, 0, TEpsilon.position) > 0)) and
       (corners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
       (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
      //-----
      aJRect := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
      aCanvas.drawRoundRect(aJRect{rect},
                            xRadius * FScreenScale {rx},
                            yRadius * FScreenScale {ry},
                            apaint);
      aJRect := nil;
      //-----
    end

    // use drawRect
    else if ((compareValue(xRadius, 0, TEpsilon.position) = 0) or
             (compareValue(YRadius, 0, TEpsilon.position) = 0) or
             (corners=[])) and
            (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
      //-----
      aCanvas.drawRect(aRect.left{left},
                       aRect.top{top},
                       aRect.right{right},
                       aRect.bottom{bottom},
                       apaint);
      //-----
    end

    // use drawPath
    else begin

      aPath := TJPath.Create;
      //----
      aXRadius := xRadius * FScreenScale;
      aYradius := yRadius * FScreenScale;
      if (aXRadius > aRect.width / 2) then aXRadius := aRect.width / 2;
      if (aYradius > aRect.height / 2) then aYradius := aRect.height / 2;
      //----
      if (compareValue(aXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(aYRadius, 0, TEpsilon.position) > 0) then aCorners := corners
      else aCorners := [];
      //----
      aWidthMinusCorners := (aRect.width - (2 * aXRadius));
      aHeightMinusCorners := (aRect.height - (2 * aYradius));
      //----
      if (Stroke.Kind <> TBrushKind.None) then aHalfStrokeWidth := (Stroke.Thickness * FScreenScale) / 2
      else aHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in aCorners) then begin
        aPath.moveTo(aRect.right, aRect.top + aYradius);
        aPath.rQuadTo(0, -aYradius, -aXRadius, -aYradius);
        if not aDrawOnlyBorder then aPath.rlineTo(0, -aHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then aPath.moveTo(aRect.right + aHalfStrokeWidth, aRect.top + aYradius)
        else aPath.moveTo(aRect.right, aRect.top + aYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           aPath.rLineTo(0, -aYradius -aHalfStrokeWidth);
           if aDrawOnlyBorder then aPath.rMoveTo(0, aHalfStrokeWidth);
        end
        else aPath.rMoveTo(0, -aYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then aPath.rLineTo(-aXRadius -aHalfStrokeWidth,0)
          else begin
            aPath.rMoveTo(+aHalfStrokeWidth,0);
            aPath.rLineTo(-aXRadius -aHalfStrokeWidth,0);
          end;
        end
        else aPath.rMoveTo(-aXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then aPath.rLineTo(-awidthMinusCorners, 0)
      else aPath.rMoveTo(-awidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in aCorners) then begin
        if not aDrawOnlyBorder then aPath.rlineTo(0, +aHalfStrokeWidth);
        aPath.rQuadTo(-aXRadius, 0, -aXRadius, aYradius);
        if not aDrawOnlyBorder then aPath.rlineTo(-aHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          aPath.rLineTo(-aXRadius -aHalfStrokeWidth, 0);
          if aDrawOnlyBorder then aPath.rMoveTo(aHalfStrokeWidth, 0);
        end
        else aPath.rMoveTo(-aXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then aPath.rLineTo(0,aYradius +aHalfStrokeWidth)
          else begin
            aPath.rMoveTo(0,-aHalfStrokeWidth);
            aPath.rLineTo(0,+aYradius +aHalfStrokeWidth);
          end;
        end
        else aPath.rMoveTo(0,aYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then aPath.rLineTo(0, aheightMinusCorners)
      else aPath.rMoveTo(0, aheightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in aCorners) then begin
        if not aDrawOnlyBorder then aPath.rlineTo(aHalfStrokeWidth, 0);
        aPath.rQuadTo(0, aYradius, aXRadius, aYradius);
        if not aDrawOnlyBorder then aPath.rlineTo(0, aHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          aPath.rLineTo(0, aYradius +aHalfStrokeWidth);
          if aDrawOnlyBorder then aPath.rMoveTo(0, -aHalfStrokeWidth);
        end
        else aPath.rMoveTo(0, aYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then aPath.rLineTo(aXRadius +aHalfStrokeWidth,0)
          else begin
            aPath.rMoveTo(-aHalfStrokeWidth,0);
            aPath.rLineTo(+aXRadius +aHalfStrokeWidth,0);
          end;
        end
        else aPath.rMoveTo(aXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then aPath.rLineTo(awidthMinusCorners, 0)
      else aPath.rMoveTo(awidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in aCorners) then begin
        if not aDrawOnlyBorder then aPath.rlineTo(0, -aHalfStrokeWidth);
        aPath.rQuadTo(aXRadius, 0, aXRadius, -aYradius);
        if not aDrawOnlyBorder then aPath.rlineTo(aHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          aPath.rLineTo(aXRadius +aHalfStrokeWidth,0);
          if aDrawOnlyBorder then aPath.rMoveTo(-aHalfStrokeWidth, 0);
        end
        else aPath.rMoveTo(aXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then aPath.rLineTo(0, -aYradius -aHalfStrokeWidth)
          else begin
            aPath.rMoveTo(0,+aHalfStrokeWidth);
            aPath.rLineTo(0,-aYradius -aHalfStrokeWidth);
          end;
        end
        else aPath.rMoveTo(0, -aYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then aPath.rLineTo(0, -aheightMinusCorners)
      else aPath.rMoveTo(0, -aheightMinusCorners);

      //-----
      aCanvas.drawPath(apath,aPaint);
      aPath := nil;

    end;
  end;
  {$ENDIF}

  {$IF defined(IOS)}
  procedure _DrawPath(Const aDrawOnlyBorder: Boolean);
  var aXRadius: single;
      aYradius: Single;
      aWidthMinusCorners: single;
      aHeightMinusCorners: Single;
      aCorners: TCorners;
      aHalfStrokeWidth: Single;
      aCurPoint: TpointF;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(aContext, X, aBitmapSurface.Height - Y);
      aCurPoint.X := x;
      aCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(aContext,
                                   aCurPoint.X + dx1{cpx},
                                   aBitmapSurface.Height - (aCurPoint.Y + dy1){cpy},
                                   aCurPoint.X + dx2{x},
                                   aBitmapSurface.Height - (aCurPoint.Y + dy2){y});
      aCurPoint.X := aCurPoint.X + dx2;
      aCurPoint.Y := aCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(aContext, aCurPoint.X + dx{x}, aBitmapSurface.Height - (aCurPoint.Y + dy{y}));
      aCurPoint.X := aCurPoint.X + dx;
      aCurPoint.Y := aCurPoint.Y + dy;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rMoveTo(dx: Single; dy: Single);
    begin
      CGContextMoveToPoint(aContext, aCurPoint.X + dx{x}, aBitmapSurface.Height - (aCurPoint.Y + dy{y}));
      aCurPoint.X := aCurPoint.X + dx;
      aCurPoint.Y := aCurPoint.Y + dy;
    end;

  begin

    // Creates a new empty path in a graphics context.
    CGContextBeginPath(aContext);

    // use drawRect
    if ((compareValue(xRadius, 0, TEpsilon.position) = 0) or
        (compareValue(YRadius, 0, TEpsilon.position) = 0) or
        (corners=[])) and
       (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
     //-----
     CGContextAddRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                  aRect.Width,
                                                  aRect.Height,
                                                  aBitmapSurface.Height));
     //-----
    end

    // use drawPath
    else begin

      aXRadius := xRadius * FScreenScale;
      aYradius := yRadius * FScreenScale;
      if (aXRadius > aRect.width / 2) then aXRadius := aRect.width / 2;
      if (aYradius > aRect.height / 2) then aYradius := aRect.height / 2;
      //----
      if (compareValue(aXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(aYRadius, 0, TEpsilon.position) > 0) then aCorners := corners
      else aCorners := [];
      //----
      aWidthMinusCorners := (aRect.width - (2 * aXRadius));
      aHeightMinusCorners := (aRect.height - (2 * aYradius));
      //----
      if (Stroke.Kind <> TBrushKind.None) then aHalfStrokeWidth := (Stroke.Thickness * FScreenScale) / 2
      else aHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in aCorners) then begin
        _moveTo(aRect.right, aRect.top + aYradius);
        _rQuadTo(0, -aYradius, -aXRadius, -aYradius);
        if not aDrawOnlyBorder then _rlineTo(0, -aHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then _moveTo(aRect.right + aHalfStrokeWidth, aRect.top + aYradius)
        else _moveTo(aRect.right, aRect.top + aYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           _rLineTo(0, -aYradius -aHalfStrokeWidth);
           if aDrawOnlyBorder then _rMoveTo(0, aHalfStrokeWidth);
        end
        else _rMoveTo(0, -aYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(-aXRadius -aHalfStrokeWidth,0)
          else begin
            _rMoveTo(+aHalfStrokeWidth,0);
            _rLineTo(-aXRadius -aHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(-aXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then _rLineTo(-awidthMinusCorners, 0)
      else _rMoveTo(-awidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in aCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, +aHalfStrokeWidth);
        _rQuadTo(-aXRadius, 0, -aXRadius, aYradius);
        if not aDrawOnlyBorder then _rlineTo(-aHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          _rLineTo(-aXRadius -aHalfStrokeWidth, 0);
          if aDrawOnlyBorder then _rMoveTo(aHalfStrokeWidth, 0);
        end
        else _rMoveTo(-aXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0,aYradius +aHalfStrokeWidth)
          else begin
            _rMoveTo(0,-aHalfStrokeWidth);
            _rLineTo(0,+aYradius +aHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0,aYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then _rLineTo(0, aheightMinusCorners)
      else _rMoveTo(0, aheightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in aCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(aHalfStrokeWidth, 0);
        _rQuadTo(0, aYradius, aXRadius, aYradius);
        if not aDrawOnlyBorder then _rlineTo(0, aHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          _rLineTo(0, aYradius +aHalfStrokeWidth);
          if aDrawOnlyBorder then _rMoveTo(0, -aHalfStrokeWidth);
        end
        else _rMoveTo(0, aYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(aXRadius +aHalfStrokeWidth,0)
          else begin
            _rMoveTo(-aHalfStrokeWidth,0);
            _rLineTo(+aXRadius +aHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(aXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then _rLineTo(awidthMinusCorners, 0)
      else _rMoveTo(awidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in aCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, -aHalfStrokeWidth);
        _rQuadTo(aXRadius, 0, aXRadius, -aYradius);
        if not aDrawOnlyBorder then _rlineTo(aHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          _rLineTo(aXRadius +aHalfStrokeWidth,0);
          if aDrawOnlyBorder then _rMoveTo(-aHalfStrokeWidth, 0);
        end
        else _rMoveTo(aXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0, -aYradius -aHalfStrokeWidth)
          else begin
            _rMoveTo(0,+aHalfStrokeWidth);
            _rLineTo(0,-aYradius -aHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0, -aYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then _rLineTo(0, -aheightMinusCorners)
      else _rMoveTo(0, -aheightMinusCorners);

    end;

  end;
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
      ((SameValue(xRadius, 0, TEpsilon.position)) or
       (SameValue(yRadius, 0, TEpsilon.position)) or
       (corners=[]))
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
  ALLog('TALRectangle.MakeBufBitmap', 'TALRectangle.MakeBufBitmap', TalLogType.verbose);
  inc(AlDebugRectangleMakeBufBitmapCount);
  AlDebugRectangleMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  {$IFDEF ANDROID}

  //init fBufBitmapRect / aRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the main bitmap on with we will draw
  aBitmap := TJBitmap.JavaClass.createBitmap(round(aRect.Width),
                                             round(aRect.Height),
                                             TJBitmap_Config.JavaClass.ARGB_8888);
  try

    //create the canvas and the paint
    aCanvas := TJCanvas.JavaClass.init(aBitmap);
    aPaint := TJPaint.JavaClass.init;
    aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    //init aRect
    if Stroke.Kind <> TBrushKind.None then aRect.Inflate((-(Stroke.Thickness * FScreenScale) / 2), (-(Stroke.Thickness * FScreenScale) / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles

    //fill the rectangle
    if Fill.Kind <> TBrushKind.None then begin

      //init aPaint
      aPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full aRect + Stroke.Thickness :( this result&ing in border if the fill is for exemple black and border white

      //fill with gradient
      if Fill.Kind = TBrushKind.Gradient then begin
        if Fill.Gradient.Style = TGradientStyle.Radial then begin
          aColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
          aStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
          for i := 0 to Fill.Gradient.Points.Count - 1 do begin
            aColors[Fill.Gradient.Points.Count - 1 - i] := integer(Fill.Gradient.Points[i].Color);
            aStops[Fill.Gradient.Points.Count - 1 - i] := 1 - Fill.Gradient.Points[i].Offset;
          end;
          aShader := TJRadialGradient.JavaClass.init(aRect.CenterPoint.x{x}, aRect.CenterPoint.y{y}, aRect.width / 2{radius},  aColors, aStops, TJShader_TileMode.JavaClass.CLAMP{tile});
          aPaint.setShader(aShader);
          _drawRect(false{aDrawOnlyBorder});
          aPaint.setShader(nil);
          aShader := nil;
          ALfreeandNil(aColors);
          ALfreeandNil(aStops);
        end;
      end

      //fill with bitmap
      else if Fill.Kind = TBrushKind.Bitmap then begin
        if not fill.Bitmap.Bitmap.IsEmpty then begin
          if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
            //-----
            aTmpBitmap := TJBitmap.JavaClass.createBitmap(fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888);
            //-----
            FillChar(aBitmapInfo, SizeOf(aBitmapInfo), 0);
            if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID, @aBitmapInfo) = 0) and
               (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID, @aPixelBuffer) = 0) then
            try
              if fill.Bitmap.Bitmap.Map(TMapAccess.Read, aBitmapData) then
              try
                System.Move(aBitmapData.Data^, aPixelBuffer^, aBitmapData.Pitch * aBitmapData.Height);
              finally
                fill.Bitmap.Bitmap.Unmap(aBitmapData);
              end;
            finally
              AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID);
            end;
            //-----
            _drawRect(false{aDrawOnlyBorder});
            aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
            aJDestRectf := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
            aJSrcRect := TJRect.JavaClass.init(0, 0, fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height);
            aPaint.setXfermode(aPorterDuffXfermode);
            aCanvas.drawBitmap(aTMPBitmap, aJSrcRect, aJDestRectf, apaint);
            aPaint.setXfermode(nil);
            aPorterDuffXfermode := nil;
            aJSrcRect := nil;
            aJDestRectf := nil;
            //-----
            aTmpBitmap.recycle;
            aTmpBitmap := nil;
            //-----
          end;
        end;
      end

      //fill with solid color
      else if Fill.Kind = TBrushKind.Solid then begin
        aPaint.setColor(Fill.Color);
        _drawRect(false{aDrawOnlyBorder});
      end;

    end;

    //stroke the rectangle
    if Stroke.Kind <> TBrushKind.None then begin

      //init aPaint
      aPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
      aPaint.setStrokeWidth(Stroke.Thickness * FScreenScale);

      //stroke with solid color
      if Stroke.Kind = TBrushKind.Solid then begin
        aPaint.setColor(Stroke.Color);
        _drawRect(true{aDrawOnlyBorder});
      end;

    end;

    //free the paint and the canvas
    aPaint := nil;
    aCanvas := nil;

    //init the bitmapSurface that we will use to convert the jbitmap
    aBitmapSurface := TBitmapSurface.Create;
    try

      //convert the JBitmapToSurface to the bitmapSurface
      if JBitmapToSurface(aBitmap, aBitmapSurface) then begin

        //convert the bitmapSurface to a TTexture
        fBufBitmap := TALTexture.Create(True{aVolatile});
        try
          fBufBitmap.Assign(aBitmapSurface);
        except
          ALFreeAndNil(fBufBitmap);
          raise;
        end;

      end
      else fBufBitmap := nil;

    finally
      ALFreeAndNil(abitmapSurface);
    end;

  finally
    aBitmap.recycle;
    aBitmap := nil;
  end;

  {$ELSEIF DEFINED(IOS)}

  //init fBufBitmapRect / aRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the bitmapSurface
  aBitmapSurface := TbitmapSurface.Create;
  try

    //init aBitmapSurface
    aBitmapSurface.SetSize(round(aRect.Width),
                           round(aRect.Height));

    //init the color space
    aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
    if aColorSpace = nil then exit(nil);         // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
    try

      //create the context
      aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                             //       memory block should be at least (bytesPerRow*height) bytes.
                                                             //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                             //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                        aBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                                        aBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                                        8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                           //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                           //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                           //                   chapter of Quartz 2D Programming Guide.
                                        aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                              //              a value of 0 causes the value to be calculated automatically.
                                        aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                     //             bitmap graphics contexts.
                                        kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                          // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                          // kCGImageAlphaPremultipliedNone =  For example, RGB
                                        kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                  // kCGBitmapByteOrder32Little = Little-endian
                                                                  // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                  //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                  //             values. The constants for specifying the alpha channel information are declared with the
                                                                  //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                  //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                  //             and CGImageAlphaInfo constants.)
                                                                  //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                  //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                  //             Graphics Contexts chapter of Quartz 2D Programming Guide.
      if aContext = nil then exit(nil);
      try

        //set the paint default properties
        CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
        //-----
        CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
        CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.

        //init aRect
        if Stroke.Kind <> TBrushKind.None then aRect.Inflate((-(Stroke.Thickness * FScreenScale) / 2), (-(Stroke.Thickness * FScreenScale) / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles

        //fill the rectangle
        if Fill.Kind <> TBrushKind.None then begin

          //fill with gradient
          if Fill.Kind = TBrushKind.Gradient then begin
            if Fill.Gradient.Style = TGradientStyle.Radial then begin
              CGContextSaveGState(aContext);
              //-----
              aCallback.version := 0;
              aCallback.evaluate := @ALGradientEvaluateCallback;
              aCallback.releaseInfo:= nil;
              aFunc := CGFunctionCreate(fill.Gradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                                        1, // domainDimension - The number of inputs.
                                        @aDefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                                        4, // rangeDimension - The number of outputs.
                                        nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                                        @aCallback); // callbacks - A pointer to a callback function table.
              try
                aShading := CGShadingCreateRadial(aColorSpace, // colorspace
                                                  CGPoint.Create(TPointF.Create(aRect.Width / 2, aRect.height / 2)), // start - The center of the starting circle, in the shading's target coordinate space.
                                                  aRect.Width / 2, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                                                  CGPoint.Create(TPointF.Create(aRect.Width / 2, aRect.Height / 2)), // end - The center of the ending circle, in the shading's target coordinate space.
                                                  0, // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                                                  aFunc, // function
                                                  1, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                                                  1); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
                try
                  _DrawPath(false{aDrawOnlyBorder});
                  CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                           // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                           // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                           // save the graphics state before you clip and restore the graphics state after you’ve completed
                                           // any clipped drawing.
                  CGContextDrawShading(aContext, aShading);
                finally
                  CGShadingRelease(aShading);
                end;
              finally
                CGFunctionRelease(aFunc);
              end;
              //-----
              CGContextRestoreGState(aContext);
            end;
          end

          //fill with bitmap
          else if Fill.Kind = TBrushKind.Bitmap then begin
            if not fill.Bitmap.Bitmap.IsEmpty then begin
              if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
                if fill.Bitmap.Bitmap.Map(TMapAccess.Read, aBitmapData) then
                try
                  aTMPContext := CGBitmapContextCreate(aBitmapData.Data, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                         //       memory block should be at least (bytesPerRow*height) bytes.
                                                                         //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                         //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                       aBitmapData.Width, // width: The width, in pixels, of the required bitmap.
                                                       aBitmapData.Height, // height: The height, in pixels, of the required bitmap.
                                                       8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                          //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                          //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                          //                   chapter of Quartz 2D Programming Guide.
                                                       aBitmapData.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                                     //              a value of 0 causes the value to be calculated automatically.
                                                       aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                                    //             bitmap graphics contexts.
                                                       kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                                         // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                                         // kCGImageAlphaPremultipliedNone =  For example, RGB
                                                       kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                                 // kCGBitmapByteOrder32Little = Little-endian
                                                                                 // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                                 //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                                 //             values. The constants for specifying the alpha channel information are declared with the
                                                                                 //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                                 //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                                 //             and CGImageAlphaInfo constants.)
                                                                                 //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                                 //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                                 //             Graphics Contexts chapter of Quartz 2D Programming Guide.
                  if aContext <> nil then begin
                    try
                      aImageRef := CGBitmapContextCreateImage(aTMPContext);
                      if aImageRef <> nil then
                      try
                        aImage := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(aImageRef));
                        if aImage <> nil then
                        try
                          CGContextSaveGState(aContext);
                          //-----
                          _DrawPath(false{aDrawOnlyBorder});
                          CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                                   // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                                   // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                                   // save the graphics state before you clip and restore the graphics state after you’ve completed
                                                   // any clipped drawing.
                          CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                             ALLowerLeftCGRect(aRect.TopLeft,
                                                               aRect.Width,
                                                               aRect.Height,
                                                               aBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                             aImage.CGImage); // image The image to draw.
                          //-----
                          CGContextRestoreGState(aContext);
                        finally
                          aImage.release;
                        end;
                      finally
                        CGImageRelease(aImageRef);
                      end;
                    finally
                      CGContextRelease(aTMPContext);
                    end;
                  end;
                finally
                  fill.Bitmap.Bitmap.Unmap(aBitmapData);
                end;
              end;
            end;
          end

          //fill with solid color
          else if Fill.Kind = TBrushKind.Solid then begin
            aAlphaColor := TAlphaColorCGFloat.Create(Fill.Color);
            CGContextSetRGBFillColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
            _DrawPath(false{aDrawOnlyBorder});
            CGContextFillPath(aContext);
          end;

        end;

        //stroke the rectangle
        if Stroke.Kind <> TBrushKind.None then begin

          //stroke with solid color
          if Stroke.Kind = TBrushKind.Solid then begin
            aAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
            CGContextSetRGBStrokeColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
            CGContextSetLineWidth(aContext, Stroke.Thickness * FScreenScale);
            _DrawPath(True{aDrawOnlyBorder});
            CGContextStrokePath(aContext);
          end;

        end;

      finally
        CGContextRelease(aContext);
      end;

    finally
      CGColorSpaceRelease(aColorSpace);
    end;

    //convert the aBitmapSurface to texture
    fBufBitmap := TALTexture.Create(True{aVolatile});
    try
      fBufBitmap.Assign(aBitmapSurface);
    except
      ALFreeAndNil(fBufBitmap);
      raise;
    end;

  finally
    ALFreeAndNil(aBitmapSurface);
  end;

  {$ENDIF}

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

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    opacity, {opacity}
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

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALRectangle.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALRectangle.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{***********************************************}
constructor TALCircle.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{***************************}
destructor TALCircle.Destroy;
begin
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
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

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALCircle.MakeBufBitmap: TTexture;
{$ELSE}
function TALCircle.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IF defined(IOS)}
const aDefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var aBitmap: Jbitmap;
    aTmpBitmap: Jbitmap;
    aBitmapSurface: TBitmapSurface;
    aShader: JRadialGradient;
    aCanvas: Jcanvas;
    aPaint: JPaint;
    aRect: TRectf;
    aColors: TJavaArray<Integer>;
    aStops: TJavaArray<Single>;
    aPorterDuffXfermode: jPorterDuffXfermode;
    aBitmapInfo: AndroidBitmapInfo;
    aPixelBuffer: Pointer;
    aBitmapData: TBitmapData;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    i: integer;
{$ELSEIF defined(IOS)}
var aBitmapSurface: TbitmapSurface;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    aAlphaColor: TAlphaColorCGFloat;
    aCallback: CGFunctionCallbacks;
    aShading: CGShadingRef;
    aFunc: CGFunctionRef;
    aRect: TRectf;
    aBitmapData: TBitmapData;
    aTMPContext: CGContextRef;
    aImageRef: CGImageRef;
    aImage: UIImage;
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
  ALLog('TALCircle.MakeBufBitmap', 'TALCircle.MakeBufBitmap', TalLogType.verbose);
  inc(AlDebugCircleMakeBufBitmapCount);
  AlDebugCircleMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  {$IFDEF ANDROID}

  //init fBufBitmapRect / aRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), FScreenScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the main bitmap on with we will draw
  aBitmap := TJBitmap.JavaClass.createBitmap(round(aRect.Width),
                                             round(aRect.Height),
                                             TJBitmap_Config.JavaClass.ARGB_8888);
  try

    //create the canvas and the paint
    aCanvas := TJCanvas.JavaClass.init(aBitmap);
    aPaint := TJPaint.JavaClass.init;
    aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    //init aRect
    if Stroke.Kind <> TBrushKind.None then aRect.Inflate((-(Stroke.Thickness * FScreenScale) / 2), (-(Stroke.Thickness * FScreenScale) / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles

    //fill the circle
    if Fill.Kind <> TBrushKind.None then begin

      //init aPaint
      aPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full aRect + Stroke.Thickness :( this result&ing in border if the fill is for exemple black and border white

      //fill with gradient
      if Fill.Kind = TBrushKind.Gradient then begin
        if Fill.Gradient.Style = TGradientStyle.Radial then begin
          aColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
          aStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
          for i := 0 to Fill.Gradient.Points.Count - 1 do begin
            aColors[Fill.Gradient.Points.Count - 1 - i] := integer(Fill.Gradient.Points[i].Color);
            aStops[Fill.Gradient.Points.Count - 1 - i] := 1 - Fill.Gradient.Points[i].Offset;
          end;
          aShader := TJRadialGradient.JavaClass.init(aRect.CenterPoint.x{x}, aRect.CenterPoint.y{y}, aRect.width / 2{radius},  aColors, aStops, TJShader_TileMode.JavaClass.CLAMP{tile});
          aPaint.setShader(aShader);
          aCanvas.drawCircle(aRect.CenterPoint.x{cx}, aRect.CenterPoint.y{cy}, aRect.width / 2{radius}, apaint);
          aPaint.setShader(nil);
          aShader := nil;
          alfreeandNil(aColors);
          alfreeandNil(aStops);
        end;
      end

      //fill with bitmap
      else if Fill.Kind = TBrushKind.Bitmap then begin
        if not fill.Bitmap.Bitmap.IsEmpty then begin
          if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
            //-----
            aTmpBitmap := TJBitmap.JavaClass.createBitmap(fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888);
            //-----
            FillChar(aBitmapInfo, SizeOf(aBitmapInfo), 0);
            if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID, @aBitmapInfo) = 0) and
               (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID, @aPixelBuffer) = 0) then
            try
              if fill.Bitmap.Bitmap.Map(TMapAccess.Read, aBitmapData) then
              try
                System.Move(aBitmapData.Data^, aPixelBuffer^, aBitmapData.Pitch * aBitmapData.Height);
              finally
                fill.Bitmap.Bitmap.Unmap(aBitmapData);
              end;
            finally
              AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID);
            end;
            //-----
            aCanvas.drawCircle(aRect.CenterPoint.x{cx}, aRect.CenterPoint.y{cy}, aRect.width / 2{radius}, apaint);
            aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
            aJDestRectf := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
            aJSrcRect := TJRect.JavaClass.init(0, 0, fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height);
            aPaint.setXfermode(aPorterDuffXfermode);
            aCanvas.drawBitmap(aTMPBitmap, aJSrcRect, aJDestRectf, apaint);
            aPaint.setXfermode(nil);
            aPorterDuffXfermode := nil;
            aJSrcRect := nil;
            aJDestRectf := nil;
            //-----
            aTmpBitmap.recycle;
            aTmpBitmap := nil;
            //-----
          end;
        end;
      end

      //fill with solid color
      else if Fill.Kind = TBrushKind.Solid then begin
        aPaint.setColor(Fill.Color);
        aCanvas.drawCircle(aRect.CenterPoint.x{cx}, aRect.CenterPoint.y{cy}, aRect.width / 2{radius}, apaint);
      end;

    end;

    //stroke the circle
    if Stroke.Kind <> TBrushKind.None then begin

      //init aPaint
      aPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
      aPaint.setStrokeWidth(Stroke.Thickness * FScreenScale);

      //stroke with solid color
      if Stroke.Kind = TBrushKind.Solid then begin
        aPaint.setColor(Stroke.Color);
        aCanvas.drawCircle(aRect.CenterPoint.x{cx}, aRect.CenterPoint.y{cy}, aRect.width / 2{radius}, apaint);
      end;

    end;

    //free the paint and the canvas
    aPaint := nil;
    aCanvas := nil;

    //init the bitmapSurface that we will use to convert the jbitmap
    aBitmapSurface := TBitmapSurface.Create;
    try

      //convert the JBitmapToSurface to the bitmapSurface
      if JBitmapToSurface(aBitmap, aBitmapSurface) then begin

        //convert the bitmapSurface to a TTexture
        fBufBitmap := TALTexture.Create(True{aVolatile});
        try
          fBufBitmap.Assign(aBitmapSurface);
        except
          ALFreeAndNil(fBufBitmap);
          raise;
        end;

      end
      else fBufBitmap := nil;

    finally
      ALFreeAndNil(abitmapSurface);
    end;

  finally
    aBitmap.recycle;
    aBitmap := nil;
  end;

  {$ELSEIF DEFINED(IOS)}

  //init fBufBitmapRect / aRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), FScreenScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the bitmapSurface
  aBitmapSurface := TbitmapSurface.Create;
  try

    //init aBitmapSurface
    aBitmapSurface.SetSize(round(aRect.Width),
                           round(aRect.Height));

    //init the color space
    aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
    if aColorSpace = nil then exit(nil);         // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
    try

      //create the context
      aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                             //       memory block should be at least (bytesPerRow*height) bytes.
                                                             //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                             //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                        aBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                                        aBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                                        8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                           //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                           //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                           //                   chapter of Quartz 2D Programming Guide.
                                        aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                              //              a value of 0 causes the value to be calculated automatically.
                                        aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                     //             bitmap graphics contexts.
                                        kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                          // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                          // kCGImageAlphaPremultipliedNone =  For example, RGB
                                        kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                  // kCGBitmapByteOrder32Little = Little-endian
                                                                  // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                  //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                  //             values. The constants for specifying the alpha channel information are declared with the
                                                                  //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                  //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                  //             and CGImageAlphaInfo constants.)
                                                                  //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                  //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                  //             Graphics Contexts chapter of Quartz 2D Programming Guide.
      if aContext = nil then exit(nil);
      try

        //set the paint default properties
        CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
        //-----
        CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
        CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.

        //init aRect
        if Stroke.Kind <> TBrushKind.None then aRect.Inflate((-(Stroke.Thickness * FScreenScale) / 2), (-(Stroke.Thickness * FScreenScale) / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles

        //fill the circle
        if Fill.Kind <> TBrushKind.None then begin

          //fill with gradient
          if Fill.Kind = TBrushKind.Gradient then begin
            if Fill.Gradient.Style = TGradientStyle.Radial then begin
              CGContextSaveGState(aContext);
              //-----
              aCallback.version := 0;
              aCallback.evaluate := @ALGradientEvaluateCallback;
              aCallback.releaseInfo:= nil;
              aFunc := CGFunctionCreate(fill.Gradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                                        1, // domainDimension - The number of inputs.
                                        @aDefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                                        4, // rangeDimension - The number of outputs.
                                        nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                                        @aCallback); // callbacks - A pointer to a callback function table.
              try
                aShading := CGShadingCreateRadial(aColorSpace, // colorspace
                                                  CGPoint.Create(TPointF.Create(aRect.Width / 2, aRect.height / 2)), // start - The center of the starting circle, in the shading's target coordinate space.
                                                  aRect.Width / 2, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                                                  CGPoint.Create(TPointF.Create(aRect.Width / 2, aRect.Height / 2)), // end - The center of the ending circle, in the shading's target coordinate space.
                                                  0, // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                                                  aFunc, // function
                                                  1, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                                                  1); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
                try
                  CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
                  CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                                        aRect.Width,
                                                                        aRect.Height,
                                                                        aBitmapSurface.Height));
                  CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
                  CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                           // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                           // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                           // save the graphics state before you clip and restore the graphics state after you’ve completed
                                           // any clipped drawing.
                  CGContextDrawShading(aContext, aShading);
                finally
                  CGShadingRelease(aShading);
                end;
              finally
                CGFunctionRelease(aFunc);
              end;
              //-----
              CGContextRestoreGState(aContext);
            end;
          end

          //fill with bitmap
          else if Fill.Kind = TBrushKind.Bitmap then begin
            if not fill.Bitmap.Bitmap.IsEmpty then begin
              if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
                if fill.Bitmap.Bitmap.Map(TMapAccess.Read, aBitmapData) then
                try
                  aTMPContext := CGBitmapContextCreate(aBitmapData.Data, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                         //       memory block should be at least (bytesPerRow*height) bytes.
                                                                         //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                         //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                       aBitmapData.Width, // width: The width, in pixels, of the required bitmap.
                                                       aBitmapData.Height, // height: The height, in pixels, of the required bitmap.
                                                       8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                          //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                          //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                          //                   chapter of Quartz 2D Programming Guide.
                                                       aBitmapData.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                                     //              a value of 0 causes the value to be calculated automatically.
                                                       aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                                    //             bitmap graphics contexts.
                                                       kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                                         // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                                         // kCGImageAlphaPremultipliedNone =  For example, RGB
                                                       kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                                 // kCGBitmapByteOrder32Little = Little-endian
                                                                                 // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                                 //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                                 //             values. The constants for specifying the alpha channel information are declared with the
                                                                                 //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                                 //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                                 //             and CGImageAlphaInfo constants.)
                                                                                 //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                                 //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                                 //             Graphics Contexts chapter of Quartz 2D Programming Guide.
                  if aContext <> nil then begin
                    try
                      aImageRef := CGBitmapContextCreateImage(aTMPContext);
                      if aImageRef <> nil then
                      try
                        aImage := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(aImageRef));
                        if aImage <> nil then
                        try
                          CGContextSaveGState(aContext);
                          //-----
                          CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
                          CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                                                aRect.Width,
                                                                                aRect.Height,
                                                                                aBitmapSurface.Height)); // Adds an ellipse that fits inside the specified rectangle.
                          CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
                          CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                                   // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                                   // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                                   // save the graphics state before you clip and restore the graphics state after you’ve completed
                                                   // any clipped drawing.
                          CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                             ALLowerLeftCGRect(aRect.TopLeft,
                                                               aRect.Width,
                                                               aRect.Height,
                                                               aBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                             aImage.CGImage); // image The image to draw.
                          //-----
                          CGContextRestoreGState(aContext);
                        finally
                          aImage.release;
                        end;
                      finally
                        CGImageRelease(aImageRef);
                      end;
                    finally
                      CGContextRelease(aTMPContext);
                    end;
                  end;
                finally
                  fill.Bitmap.Bitmap.Unmap(aBitmapData);
                end;
              end;
            end;
          end

          //fill with solid color
          else if Fill.Kind = TBrushKind.Solid then begin
            aAlphaColor := TAlphaColorCGFloat.Create(Fill.Color);
            CGContextSetRGBFillColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
            CGContextFillEllipseInRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                                   aRect.Width,
                                                                   aRect.Height,
                                                                   aBitmapSurface.Height));
          end;

        end;

        //stroke the circle
        if Stroke.Kind <> TBrushKind.None then begin

          //stroke with solid color
          if Stroke.Kind = TBrushKind.Solid then begin
            aAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
            CGContextSetRGBStrokeColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
            CGContextSetLineWidth(aContext, Stroke.Thickness * FScreenScale);
            CGContextStrokeEllipseInRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                                     aRect.Width,
                                                                     aRect.Height,
                                                                     aBitmapSurface.Height));
          end;

        end;

      finally
        CGContextRelease(aContext);
      end;

    finally
      CGColorSpaceRelease(aColorSpace);
    end;

    //convert the aBitmapSurface to texture
    fBufBitmap := TALTexture.Create(True{aVolatile});
    try
      fBufBitmap.Assign(aBitmapSurface);
    except
      ALFreeAndNil(fBufBitmap);
      raise;
    end;

  finally
    ALFreeAndNil(aBitmapSurface);
  end;

  {$ENDIF}

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

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

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

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALCircle.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALCircle.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{*********************************************}
constructor TALLine.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{*************************}
destructor TALLine.Destroy;
begin
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
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

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALLine.MakeBufBitmap: TTexture;
{$ELSE}
function TALLine.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IF defined(IOS)}
const aDefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var aBitmap: Jbitmap;
    aBitmapSurface: TBitmapSurface;
    aCanvas: Jcanvas;
    aPaint: JPaint;
    aRect: TRectf;
    aStrokeWidth: Single;
{$ELSEIF defined(IOS)}
var aBitmapSurface: TbitmapSurface;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    aAlphaColor: TAlphaColorCGFloat;
    aRect: TRectf;
    aStrokeWidth: Single;
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
  ALLog('TALLine.MakeBufBitmap', 'TALLine.MakeBufBitmap', TalLogType.verbose);
  inc(AlDebugLineMakeBufBitmapCount);
  AlDebugLineMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  {$IFDEF ANDROID}

  //init aStrokeWidth
  if (LineLocation = TLineLocation.InnerWithin) then aStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else aStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / aRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, aStrokeWidth), FScreenScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -aStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, aStrokeWidth, height), FScreenScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-aStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - aStrokeWidth, Width, height), FScreenScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, aStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(width - aStrokeWidth, 0, width, height), FScreenScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(aStrokeWidth/2, 0);
                     end;
  end;
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the main bitmap on with we will draw
  aBitmap := TJBitmap.JavaClass.createBitmap(round(aRect.Width),
                                             round(aRect.Height),
                                             TJBitmap_Config.JavaClass.ARGB_8888);
  try

    //create the canvas and the paint
    aCanvas := TJCanvas.JavaClass.init(aBitmap);
    aPaint := TJPaint.JavaClass.init;
    aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    //stroke the circle
    if Stroke.Kind <> TBrushKind.None then begin

      //init aPaint
      aPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
      aPaint.setStrokeWidth(aStrokeWidth * FScreenScale);

      //stroke with solid color
      if Stroke.Kind = TBrushKind.Solid then begin
        aPaint.setColor(Stroke.Color);
        case lineType of
          TLineType.Diagonal: aCanvas.drawLine(aRect.left {startX},
                                               aRect.top {startY},
                                               aRect.right {stopX},
                                               aRect.Bottom {stopY},
                                               apaint);
          TLineType.Top,
          TLineType.Bottom: aCanvas.drawLine(aRect.left {startX},
                                             (aRect.bottom - aRect.top) / 2 {startY},
                                             aRect.right {stopX},
                                             (aRect.bottom - aRect.top) / 2 {stopY},
                                             apaint);
          TLineType.Left,
          TLineType.Right: aCanvas.drawLine((aRect.right - aRect.left) / 2 {startX},
                                            aRect.top {startY},
                                            (aRect.right - aRect.left) / 2 {stopX},
                                            aRect.bottom {stopY},
                                            apaint);
        end;
      end;

    end;

    //free the paint and the canvas
    aPaint := nil;
    aCanvas := nil;

    //init the bitmapSurface that we will use to convert the jbitmap
    aBitmapSurface := TBitmapSurface.Create;
    try

      //convert the JBitmapToSurface to the bitmapSurface
      if JBitmapToSurface(aBitmap, aBitmapSurface) then begin

        //convert the bitmapSurface to a TTexture
        fBufBitmap := TALTexture.Create(True{aVolatile});
        try
          fBufBitmap.Assign(aBitmapSurface);
        except
          ALFreeAndNil(fBufBitmap);
          raise;
        end;

      end
      else fBufBitmap := nil;

    finally
      ALFreeAndNil(abitmapSurface);
    end;

  finally
    aBitmap.recycle;
    aBitmap := nil;
  end;

  {$ELSEIF DEFINED(IOS)}

  //init aStrokeWidth
  if (LineLocation = TLineLocation.InnerWithin) then aStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else aStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / aRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, aStrokeWidth), FScreenScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -aStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, aStrokeWidth, height), FScreenScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-aStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - aStrokeWidth, Width, height), FScreenScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, aStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(width - aStrokeWidth, 0, width, height), FScreenScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(aStrokeWidth/2, 0);
                     end;
  end;
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

  //create the bitmapSurface
  aBitmapSurface := TbitmapSurface.Create;
  try

    //init aBitmapSurface
    aBitmapSurface.SetSize(round(aRect.Width),
                           round(aRect.Height));

    //init the color space
    aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
    if aColorSpace = nil then exit(nil);         // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
    try

      //create the context
      aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                             //       memory block should be at least (bytesPerRow*height) bytes.
                                                             //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                             //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                        aBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                                        aBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                                        8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                           //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                           //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                           //                   chapter of Quartz 2D Programming Guide.
                                        aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                              //              a value of 0 causes the value to be calculated automatically.
                                        aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                     //             bitmap graphics contexts.
                                        kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                          // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                          // kCGImageAlphaPremultipliedNone =  For example, RGB
                                        kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                  // kCGBitmapByteOrder32Little = Little-endian
                                                                  // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                  //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                  //             values. The constants for specifying the alpha channel information are declared with the
                                                                  //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                  //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                  //             and CGImageAlphaInfo constants.)
                                                                  //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                  //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                  //             Graphics Contexts chapter of Quartz 2D Programming Guide.
      if aContext = nil then exit(nil);
      try

        //set the paint default properties
        CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
        //-----
        CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
        CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.

        //stroke the circle
        if Stroke.Kind <> TBrushKind.None then begin

          //stroke with solid color
          if Stroke.Kind = TBrushKind.Solid then begin
            aAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
            CGContextSetRGBStrokeColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
            CGContextSetLineWidth(aContext, Stroke.Thickness * FScreenScale);
            case lineType of
              TLineType.Diagonal: begin
                                    CGContextBeginPath(acontext);
                                    CGContextMoveToPoint(acontext, aRect.left, aBitmapSurface.height - aRect.top);
                                    CGContextAddLineToPoint(acontext, aRect.right, aBitmapSurface.height - aRect.Bottom);
                                  end;
              TLineType.Top,
              TLineType.Bottom: begin
                                  CGContextBeginPath(acontext);
                                  CGContextMoveToPoint(acontext, aRect.left, aBitmapSurface.height - ((aRect.bottom - aRect.top) / 2));
                                  CGContextAddLineToPoint(acontext, aRect.right, aBitmapSurface.height - ((aRect.bottom - aRect.top) / 2));
                                end;
              TLineType.Left,
              TLineType.Right: begin
                                 CGContextBeginPath(acontext);
                                 CGContextMoveToPoint(acontext, (aRect.right - aRect.left) / 2, aBitmapSurface.height - aRect.top);
                                 CGContextAddLineToPoint(acontext, (aRect.right - aRect.left) / 2, aBitmapSurface.height - aRect.Bottom);
                               end;
            end;
            CGContextStrokePath(acontext);
          end;

        end;

      finally
        CGContextRelease(aContext);
      end;

    finally
      CGColorSpaceRelease(aColorSpace);
    end;

    //convert the aBitmapSurface to texture
    fBufBitmap := TALTexture.Create(True{aVolatile});
    try
      fBufBitmap.Assign(aBitmapSurface);
    except
      ALFreeAndNil(fBufBitmap);
      raise;
    end;

  finally
    ALFreeAndNil(aBitmapSurface);
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

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

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

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALLine.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALLine.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
constructor TALDoubleBufferedTextLayoutNG.Create(const ACanvas: TCanvas; const aTextControl: TALText);
var aScreenSrv: IFMXScreenService;
begin
  inherited Create(ACanvas);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  fTextControl := aTextControl;
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
destructor TALDoubleBufferedTextLayoutNG.Destroy;
begin
  clearBufBitmap;
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  inherited;
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
function TALDoubleBufferedTextLayoutNG.MakeBufBitmap: TTexture;

{$IF defined(android)}
var aBitmap: Jbitmap;
    aBitmapSurface: TBitmapSurface;
    aRect: TRectf;
    aPaint: JPaint;
    aTypeface: JTypeface;
    aStyle: integer;
    aCanvas: Jcanvas;
    aBreakedTextItems: TALBreakTextItems;
    aBreakedTextItem: TALBreakTextItem;
    i: integer;
{$ELSEIF defined(IOS)}
var aBitmapSurface: TbitmapSurface;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    aRect: TRectf;
    aBreakedTextItems: TALBreakTextItems;
    aBreakedTextItem: TALBreakTextItem;
    i: integer;
{$ENDIF}

begin

  if (csDesigning in fTextControl.ComponentState) or
     (not fdoubleBuffered) or
     (fTextControl.Scene = nil) or // << fTextControl.Scene = nil mean mostly the fTextControl (or his parent) is not yet assigned to any form
     (fTextControl.text.IsEmpty) then begin
    clearBufBitmap;
    exit(nil);
  end;

  // we need to use the value of the fTextControl and not of the inherited TTextLayoutNG
  // because TText update some value on each call to adjustsize with different value that will
  // be used in paint
  if (fBufBitmap <> nil) and
     (fBufHorizontalAlign = fTextControl.TextSettings.HorzAlign) and  // TText.adjustsize always use TTextAlign.Leading
     (fBufVerticalAlign = fTextControl.TextSettings.VertAlign) and // TText.adjustsize always use TTextAlign.Leading
     (fBufFontColor = fTextControl.TextSettings.FontColor) and
     (sametext(fBuffontFamily, fTextControl.TextSettings.Font.Family)) and
     (fBuffontStyle = fTextControl.TextSettings.Font.Style) and
     (SameValue(fBuffontSize, fTextControl.TextSettings.Font.Size, TEpsilon.FontSize)) and
     (fBufWordWrap = fTextControl.TextSettings.WordWrap) and
     (fBufAutosize = fTextControl.AutoSize) and
     (fBufTrimming = fTextControl.TextSettings.Trimming) and
     (
      (
       (not fTextControl.AutoSize) and // if not autosize then the dimensions returned by this function will depend of MaxSize.X / MaxSize.Y
       (SameValue(fBufSize.cx, MaxSize.X, TEpsilon.position)) and
       (SameValue(fBufSize.cy, MaxSize.Y, TEpsilon.position))
      )
      OR
      (
       (fTextControl.AutoSize) and
       (
        (SameValue(fBufSize.cx, MaxSize.x, TEpsilon.position)) or // if we already calculate the buf for maxsize.x
        (SameValue(fbufBitmapRect.width, MaxSize.x, TEpsilon.position)) or // if fbufBitmapRect.width = MaxSize.x we can not do anything better ;)
        ((not fBufTextBreaked) and
         (CompareValue(fbufBitmapRect.width, MaxSize.x, TEpsilon.position) <= 0)) // if fbufBitmapRect.width <= MaxSize.x and text wasn't breaked we can't do anything better
       ) and
       (
        (SameValue(fBufSize.cy, MaxSize.y, TEpsilon.position)) or // if we already calculate the buf for maxsize.y
        (SameValue(fbufBitmapRect.height, MaxSize.y, TEpsilon.position)) or // if fbufBitmapRect.height = MaxSize.y we can not do anything better ;)
        ((not fBufTextBreaked) and
         (CompareValue(fbufBitmapRect.height, MaxSize.y, TEpsilon.position) <= 0)) // if fbufBitmapRect.height <= MaxSize.y and text wasn't breaked we can't do anything better
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
  fBufWordWrap := fTextControl.TextSettings.WordWrap;
  fBufAutosize := fTextControl.AutoSize;
  fBufTrimming := fTextControl.TextSettings.Trimming;
  fBufSize := MaxSize;
  fBufText := fTextControl.Text;

  {$IFDEF debug}
  ALLog('TALDoubleBufferedTextLayoutNG.MakeBufBitmap', 'TALDoubleBufferedTextLayoutNG.MakeBufBitmap - text:' + fBufText, TalLogType.verbose);
  inc(AlDebugTextMakeBufBitmapCount);
  AlDebugTextMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  {$IF defined(android)}

  //init fBufBitmapRect / aRect
  fBufBitmapRect := TRectF.Create(0, 0, fBufSize.cX * FScreenScale, fBufSize.cY * FScreenScale);

  //create aPaint
  aPaint := TJPaint.JavaClass.init;
  aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  aPaint.setSubpixelText(true); // Enabling this flag causes glyph advances to be computed with subpixel accuracy.
  aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
  aPaint.setColor(fBufFontColor);
  aPaint.setTextSize(fBuffontSize * FScreenScale);
  if (TFontStyle.fsBold in fBuffontStyle) and
     (TFontStyle.fsItalic in fBuffontStyle) then aStyle := TJTypeface.JavaClass.BOLD_ITALIC
  else if (TFontStyle.fsBold in fBuffontStyle) then aStyle := TJTypeface.JavaClass.BOLD
  else if (TFontStyle.fsItalic in fBuffontStyle) then aStyle := TJTypeface.JavaClass.ITALIC
  else aStyle := TJTypeface.JavaClass.NORMAL;
  aTypeface := TJTypeface.JavaClass.create(StringToJString(fBuffontFamily), aStyle);
  aPaint.setTypeface(aTypeface);
  aTypeface := nil;

  //create the aBreakedTextItems
  aBreakedTextItems := TalBreakTextItems.Create(true{aOwnsObjects});
  try

    //break the text
    fBufTextBreaked := ALBreakText(aPaint, // const aPaint: JPaint;
                                   fBufBitmapRect, // var ARect: TRectF;
                                   StringtoJString(fBufText), // const AText: JString;
                                   fBufWordWrap, //const aWordWrap: Boolean;
                                   fBufHorizontalAlign, fBufVerticalAlign, //const AHTextAlign, AVTextAlign: TTextAlign;
                                   fBufTrimming,
                                   aBreakedTextItems); // var aBreakedTexts: Tarray<Tpair<JString, TpointF>>);
    fbufBitmapRect.Top := fbufBitmapRect.Top / FScreenScale;
    fbufBitmapRect.right := fbufBitmapRect.right / FScreenScale;
    fbufBitmapRect.left := fbufBitmapRect.left / FScreenScale;
    fbufBitmapRect.bottom := fbufBitmapRect.bottom / FScreenScale;
    fBufBitmapRect := ALAlignDimensionToPixelCeil(fBufBitmapRect, FScreenScale);
    if fBufAutosize then fBufBitmapRect.Offset(-fBufBitmapRect.left, -fBufBitmapRect.top);
    aRect := TrectF.Create(0,0,round((fBufBitmapRect.Width)  * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

    //create the main bitmap on with we will draw
    aBitmap := TJBitmap.JavaClass.createBitmap(round(max(1, aRect.Width)),  // max because aRect.Width could = 0 if breaked at 1rt char
                                               round(max(1, aRect.Height)), // max no possible but more beautifull to write like this
                                               TJBitmap_Config.JavaClass.ARGB_8888);
    try

      //create the canvas and the paint
      aCanvas := TJCanvas.JavaClass.init(aBitmap);

      //draw all texts
      for i := 0 to aBreakedTextItems.count - 1 do begin
        aBreakedTextItem := aBreakedTextItems[i];
        aCanvas.drawText(aBreakedTextItem.line{text},
                         aBreakedTextItem.pos.x {x},
                         aBreakedTextItem.pos.y {y},
                         apaint {paint});
      end;

      //free the paint and the canvas
      aPaint := nil;
      aCanvas := nil;

      //init the bitmapSurface that we will use to convert the jbitmap
      aBitmapSurface := TBitmapSurface.Create;
      try

        //convert the JBitmapToSurface to the bitmapSurface
        if JBitmapToSurface(aBitmap, aBitmapSurface) then begin

          //convert the bitmapSurface to a TTexture
          fBufBitmap := TALTexture.Create(True{aVolatile});
          try
            fBufBitmap.Assign(aBitmapSurface);
          except
            ALFreeAndNil(fBufBitmap);
            raise;
          end;

        end
        else fBufBitmap := nil;

      finally
        ALFreeAndNil(abitmapSurface);
      end;

    finally
      aBitmap.recycle;
      aBitmap := nil;
    end;

  finally
    ALFreeAndNil(aBreakedTextItems);
  end;

  {$ELSEIF DEFINED(IOS)}

  //init the color space
  aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
  if aColorSpace = nil then exit(nil);         // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
  try

    //init fBufBitmapRect / aRect
    fBufBitmapRect := TRectF.Create(0, 0, fBufSize.cX * FScreenScale, fBufSize.cY * FScreenScale);

    //create the aBreakedTextItems
    aBreakedTextItems := TALBreakTextItems.Create(true{aOwnsObjects});
    try

      //break the text
      fBufTextBreaked := ALBreakText(aColorSpace, // const aColorSpace: CGColorSpaceRef;
                                     fBufFontColor, //const aFontColor: TalphaColor;
                                     fBuffontSize * FScreenScale, //const aFontSize: single;
                                     fBuffontStyle, //const aFontStyle: TFontStyles;
                                     fBuffontFamily, //const aFontName: String;
                                     fBufBitmapRect, //var ARect: TRectF;
                                     fBufText, // const AText: string;
                                     fBufWordWrap, //const aWordWrap: Boolean;
                                     fBufHorizontalAlign, fBufVerticalAlign, //const AHTextAlign, AVTextAlign: TTextAlign;
                                     fBufTrimming, //const aTrimming: TTextTrimming;
                                     aBreakedTextItems); //var aBreakTextItems: TALBreakTextItems
      fbufBitmapRect.Top := fbufBitmapRect.Top / FScreenScale;
      fbufBitmapRect.right := fbufBitmapRect.right / FScreenScale;
      fbufBitmapRect.left := fbufBitmapRect.left / FScreenScale;
      fbufBitmapRect.bottom := fbufBitmapRect.bottom / FScreenScale;
      fBufBitmapRect := ALAlignDimensionToPixelCeil(fBufBitmapRect, FScreenScale);
      if fBufAutosize then fBufBitmapRect.Offset(-fBufBitmapRect.left, -fBufBitmapRect.top);
      aRect := TrectF.Create(0,0,round((fBufBitmapRect.Width)  * FScreenScale), round(fBufBitmapRect.height * FScreenScale));

      //create the bitmapSurface
      aBitmapSurface := TbitmapSurface.Create;
      try

        //init aBitmapSurface
        aBitmapSurface.SetSize(round(max(1, aRect.Width)), // max because aRect.Width could = 0 if breaked at 1rt char
                               round(max(1, aRect.Height))); // max because aRect.height could = 0


        //create the context
        aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                               //       memory block should be at least (bytesPerRow*height) bytes.
                                                               //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                               //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                          aBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                                          aBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                                          8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                             //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                             //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                             //                   chapter of Quartz 2D Programming Guide.
                                          aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                //              a value of 0 causes the value to be calculated automatically.
                                          aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                       //             bitmap graphics contexts.
                                          kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                            // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                            // kCGImageAlphaPremultipliedNone =  For example, RGB
                                          kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                    // kCGBitmapByteOrder32Little = Little-endian
                                                                    // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                    //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                    //             values. The constants for specifying the alpha channel information are declared with the
                                                                    //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                    //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                    //             and CGImageAlphaInfo constants.)
                                                                    //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                    //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                    //             Graphics Contexts chapter of Quartz 2D Programming Guide.
        if aContext = nil then exit(nil);
        try

          //set the paint default properties
          CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
          //-----
          CGContextSetShouldAntialias(aContext, 1); // default: ON
                                                    // Sets anti-aliasing on or off for a graphics context.
          CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
          //-----
          //CGContextSetShouldSmoothFonts(aContext, 1); // There are cases, such as rendering to a bitmap, when font smoothing is not appropriate and should be disabled.
                                                        // Note that some contexts (such as PostScript contexts) do not support font smoothing.
                                                        // -----
                                                        // Enables or disables font smoothing in a graphics context.
                                                        // When drawing text on a context attached to a color LCD display, Quartz takes advantage of the nature of
                                                        // LCD monitors to improve the legibility of text. This technique is called Font Smoothing. The pixels
                                                        // of an LCD monitor are made up of red, green, and blue sub-pixels. If you take these sub-pixels into
                                                        // account the screen appears to have three times the resolution commonly attributed to it, at least in
                                                        // one dimension. Font smoothing takes advantage of this increased resolution to improve the rendering of
                                                        // text. Quartz turns different sub-pixels off and on by changing the color of a pixels along the edge of
                                                        // letter shapes. Because your eye expects to see a hard line at the edge of the glyphs, the computer tricks
                                                        // it into ignoring the color in favor of perceiving a smooth edge. One disadvantage of font smoothing is
                                                        // that it relies on the fixed ordering of the sub-pixels of an LCD display. That makes the technique of
                                                        // limited use on other types of monitors. Font smoothing is also of limited use on offscreen bitmaps.
          //CGContextSetAllowsFontSmoothing(aContext, 1); // Sets whether or not to allow font smoothing for a graphics context.
          //-----
          CGContextSetShouldSubpixelPositionFonts(aContext, 1); // default: ON
                                                                // When enabled, the graphics context may position glyphs on nonintegral pixel boundaries. When disabled,
                                                                // the position of glyphs are always forced to integral pixel boundaries.
                                                                // -----
                                                                // Enables or disables subpixel positioning in a graphics context.
                                                                // Subpixel positioning concerns whether or not the glyphs in a line of
                                                                // text will be aligned to pixel boundaries or not. If subpixel positioning is
                                                                // off then when glyphs are drawn their positions might be shifted slightly to
                                                                // take pixel boundaries in account. This can improve the visual definition of
                                                                // the glyphs (making them slightly less "blurry") at the expense of honoring
                                                                // the font metrics.
          CGContextSetAllowsFontSubpixelPositioning(aContext, 1); // Sets whether or not to allow subpixel positioning for a graphics context
          //-----
          CGContextSetShouldSubpixelQuantizeFonts(aContext, 1); // default: ON
                                                                // Enables or disables subpixel quantization in a graphics context.
                                                                // -----
                                                                // Subpixel quantization is only enabled if subpixel positioning is enabled. Subpixel
                                                                // quantization improves the rendering of fonts whose glyphs are at subpixel positions
                                                                // by more closely examining how the shapes that make up the glyphs cover an individual pixel.
                                                                // This improvement, requires additional processing so changing this value can affect text
                                                                // drawing performance.
          CGContextSetAllowsFontSubpixelQuantization(aContext, 1);  // Sets whether or not to allow subpixel quantization for a graphics context

          //draw all texts
          for i := 0 to aBreakedTextItems.count - 1 do begin
            aBreakedTextItem := aBreakedTextItems[i];
            CGContextSetTextPosition(acontext,
                                     aBreakedTextItem.pos.x {x},
                                     aBitmapSurface.Height - aBreakedTextItem.pos.Y);{y}
            CTLineDraw(aBreakedTextItem.Line, acontext); // Draws a complete line.
          end;

        finally
          CGContextRelease(aContext);
        end;

        //convert the aBitmapSurface to texture
        fBufBitmap := TALTexture.Create(True{aVolatile});
        try
          fBufBitmap.Assign(aBitmapSurface);
        except
          ALFreeAndNil(fBufBitmap);
          raise;
        end;

      finally
        ALFreeAndNil(aBitmapSurface);
      end;

    finally
      ALFreeAndNil(aBreakedTextItems);
    end;

  finally
    CGColorSpaceRelease(aColorSpace);
  end;

  {$ENDIF}

  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugTextMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
procedure TALDoubleBufferedTextLayoutNG.clearBufBitmap;
begin
  ALFreeAndNil(fBufBitmap);
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
procedure TALDoubleBufferedTextLayoutNG.DoRenderLayout;
begin
  MakeBufBitmap; // recreate the fBufBitmap
  if (fBufBitmap = nil) and
     (fTextControl.Scene <> nil) and // << fTextControl.Scene = nil mean mostly the fTextControl (or his parent) is not yet assigned to any form so do nothing
     (not fTextControl.text.IsEmpty) then begin
    {$IFDEF debug}
    inc(AlDebugTextInheritedDoRenderLayoutCount);
    {$endif}
    inherited DoRenderLayout; // if no fBufBitmap then inherited
  end;
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
procedure TALDoubleBufferedTextLayoutNG.DoDrawLayout(const ACanvas: TCanvas);
var aDestRect: TrectF;
    ADesignatedArea: TrectF;
    aLocation: TPointF;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin

    if (fTextControl.Scene = nil) or // << fTextControl.Scene = nil mean mostly the fTextControl (or his parent) is not yet assigned to any form so i don't even know how we can be here !
       (fTextControl.text.IsEmpty) then exit;

    {$IFDEF debug}
    inc(AlDebugTextInheritedDoDrawLayoutCount);
    {$endif}

    inherited DoDrawLayout(ACanvas);

    exit;

  end;

  aDestRect := fBufBitmapRect;
  if fBufAutosize then begin
    ADesignatedArea := FTextControl.localrect;
    case FTextControl.HorzTextAlign of
      TTextAlign.Center: aLocation.X := (ADesignatedArea.Left + ADesignatedArea.Right - aDestRect.Width) / 2;
      TTextAlign.Leading: aLocation.X := ADesignatedArea.Left;
      TTextAlign.Trailing: aLocation.X := ADesignatedArea.Right - aDestRect.Width;
    end;
    case FTextControl.VertTextAlign of
      TTextAlign.Center: aLocation.Y := (ADesignatedArea.Top + ADesignatedArea.Bottom - aDestRect.Height) / 2;
      TTextAlign.Leading: aLocation.Y := ADesignatedArea.Top;
      TTextAlign.Trailing: aLocation.Y := ADesignatedArea.Bottom - aDestRect.Height;
    end;
    aDestRect.SetLocation(aLocation);
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(ACanvas).DrawTexture(ACanvas.AlignToPixel(aDestRect), // ATexRect (destRec)
                                        TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                        ALPrepareColor(TCustomCanvasGpu.ModulateColor, Opacity), // https://quality.embarcadero.com/browse/RSP-15432
                                        fBufBitmap);


  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(aDestRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
function TALDoubleBufferedTextLayoutNG.GetTextRect: TRectF;
begin
  if fBufBitmap = nil then begin
    if (fTextControl.Scene = nil) or // << fTextControl.Scene = nil mean mostly the fTextControl (or his parent) is not yet assigned to any form so return 0
       (fTextControl.text.IsEmpty) then result := TrectF.Create(0,0,0,0)
    else result := inherited GetTextRect  // << if fBufBitmap = nil it's mean last call to DoRenderLayout was inherited
  end
  else result := fBufBitmapRect;
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
procedure TALDoubleBufferedTextLayoutNG.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then begin
      clearbufBitmap;
      SetNeedUpdate; // << will force to call DoRenderLayout on the next call to RenderLayout
    end;
  end;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALDoubleBufferedTextLayoutNG.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALDoubleBufferedTextLayoutNG.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

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
  HitTest := False;
  //-----
  FAutoConvertFontFamily := True;
  FAutoTranslate := true;
  {$IF (not DEFINED(IOS)) and (not DEFINED(ANDROID))}
  fdoubleBuffered := true;
  {$ENDIF}
  FAutoSize := False;
  fMaxWidth := 65535;
  fMaxHeight := 65535;
  //-----
  LClass := GetTextSettingsClass;
  if LClass = nil then LClass := TALTextTextSettings;
  //-----
  {$IF defined(android) or defined(IOS)}
  FLayout := TALdoubleBufferedTextLayoutNG.Create(nil, self);
  {$else}
  FLayout := TTextLayoutManager.DefaultTextLayout.Create;
  {$endif}
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
    Layout.endUpdate;
    AdjustSize;
  end;
  fRestoreLayoutUpdateAfterLoaded := False;
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

    FDisableAlign := True;  // i don't understand why in the original delphi source code they do like this - but i feel lazzy to fully study why so i leave it
                            // this mean that we can't add aligned control inside the TalText because when we will update the size of the taltext via adjustsize
                            // then we will not realign all the childs
    try

      LHorzAlign := FLayout.HorizontalAlign;
      LVertAlign := FLayout.VerticalAlign;
      LOpacity := FLayout.Opacity;
      try

        if WordWrap then R := TRectF.Create(0, 0, Min(Width, maxWidth), maxHeight)
        else R := TRectF.Create(0, 0, maxWidth, MaxHeight);
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
var aParentControl: Tcontrol;
begin
  inherited SetNewScene(AScene);

  aParentControl := parentControl;
  while aParentControl <> nil do begin
    if aParentControl.IsUpdating then exit
    else aParentControl := aParentControl.parentControl;
  end;

  AdjustSize; // << because before scene was maybe nil so adjustsize returned 0
end;

{*******************************}
procedure TALText.clearBufBitmap;
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  TALDoubleBufferedTextLayoutNG(Layout).clearBufBitmap;
  {$ENDIF}
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALText.MakeBufBitmap: TTexture;
{$ELSE}
function TALText.MakeBufBitmap: Tbitmap;
{$ENDIF}
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  FLayout.BeginUpdate;
  try
    FLayout.LayoutCanvas := Self.Canvas;  // useless
    FLayout.TopLeft := LocalRect.TopLeft; // useless
    FLayout.Opacity := AbsoluteOpacity;  // useless
    FLayout.MaxSize := PointF(LocalRect.Width, LocalRect.Height);
  finally
    FLayout.EndUpdate;
  end;
  result := TALDoubleBufferedTextLayoutNG(Layout).MakeBufBitmap;
  {$ELSE}
  result := nil;
  {$ENDIF}
end;

{******************************************}
function TALText.GetdoubleBuffered: Boolean;
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  result := TALDoubleBufferedTextLayoutNG(Layout).doubleBuffered;
  {$ELSE}
  result := fdoubleBuffered;
  {$ENDIF}
end;

{********************************************************}
procedure TALText.SetdoubleBuffered(const Value: Boolean);
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  if value <> doubleBuffered  then begin
    TALDoubleBufferedTextLayoutNG(Layout).doubleBuffered := value;
    AdjustSize;
  end;
  {$ELSE}
  fdoubleBuffered := Value;
  {$ENDIF}
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALText.GetBufBitmap: TTexture;
{$ELSE}
function TALText.GetBufBitmap: Tbitmap;
{$ENDIF}
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  result := TALDoubleBufferedTextLayoutNG(Layout).fBufBitmap;
  {$ELSE}
  result := nil;
  {$ENDIF}
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
    if WordWrap then Layout.MaxSize := TPointF.Create(Min(Width, maxWidth), maxHeight)
    else Layout.MaxSize := TPointF.Create(maxWidth, MaxHeight);
  end
  else Layout.MaxSize := TPointF.Create(width, height);  // << this is important because else when the component is loaded then
                                                         // << we will call DoRenderLayout that will use the original maxsise (ie: 65535, 65535)
                                                         // << and then after when we will paint the control, we will again call DoRenderLayout
                                                         // << but this time with maxsize = aTextControl.size and off course if wordwrap we will
                                                         // << need to redo the bufbitmap
  Layout.EndUpdate;
  inherited; // will call dorealign that will call AdjustSize
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
  RegisterComponents('Alcinoe', [TALRectangle, TALCircle, TALLine, TALText]);
end;

initialization
  RegisterFmxClasses([TALRectangle, TALCircle, TALLine, TALText]);
  {$IFDEF debug}
  AlDebugRectangleMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugCircleMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugLineMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugTextMakeBufBitmapStopWatch := TstopWatch.Create;
  {$endif}

end.

