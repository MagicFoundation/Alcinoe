unit ALFmxObjects;

interface

uses System.Classes,
     System.Types,
     System.UITypes, // [DCC Hint] ALFmxObjects.pas(1418): H2443 Inline function 'TAlphaColorCGFloat.Create' has not been expanded because unit 'System.UITypes' is not specified in USES list
     {$IF defined(ANDROID)}
     system.Generics.collections,
     Androidapi.JNI.JavaTypes,
     FMX.TextLayout.GPU,
     FMX.types3D,
     {$ENDIF}
     {$IF defined(IOS)}
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
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
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
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
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
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
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
    //-----
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTextAccessPrivate = class(TControl, ITextSettings, IObjectState)
  protected
    {$IF CompilerVersion <> 31}
      {$MESSAGE WARN 'Check if FMX.Objects.TText still has the exact same fields and adjust the IFDEF'}
    {$ENDIF}
    // Do not remove these fields, although they are not used.
    FTextSettings: TTextSettings;
    FDefaultTextSettings: TTextSettings;
    FStyledSettings: TStyledSettings;
    FSavedTextSettings: TTextSettings;
    FLayout: TTextLayout; // << this field can't be write ( https://quality.embarcadero.com/browse/RSP-15469 )
    FAutoSize: Boolean;
    FStretch: Boolean;
    FIsChanging: Boolean;
    FAcceleratorKeyInfo: tObject; // << tObject instead of TAcceleratorInfo because i don't have access to the interface of TAcceleratorInfo here
    { ITextSettings }
    function GetDefaultTextSettings: TTextSettings; virtual; abstract;
    function GetTextSettings: TTextSettings; virtual; abstract;
    function ITextSettings.GetResultingTextSettings = GetTextSettings;
    procedure SetTextSettings(const Value: TTextSettings); virtual; abstract;
    procedure SetStyledSettings(const Value: TStyledSettings); virtual; abstract;
    function GetStyledSettings: TStyledSettings; virtual; abstract;
    { IObjectState }
    function SaveState: Boolean; virtual; abstract;
    function RestoreState: Boolean; virtual; abstract;
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
  //       don't call beginupdate/endupdate, so everytime a property of the
  //       TALText is updated, MakeBufBitmap is call again)
  TALText = class(TText)
  private
    {$IF (not DEFINED(IOS)) and (not DEFINED(ANDROID))}
    fdoubleBuffered: boolean;
    {$ENDIF}
    FAutoTranslate: Boolean;
    FAutoConvertFontFamily: boolean;
    fSaveDisableAlign: Boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fRestoreLayoutUpdate: boolean;
    {$ENDIF}
    fFontchangeDeactivated: boolean;
    function GetdoubleBuffered: Boolean;
    procedure SetdoubleBuffered(const Value: Boolean);
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function GetBufBitmap: TTexture;
    {$ELSE}
    function GetBufBitmap: Tbitmap;
    {$ENDIF}
  protected
    procedure FontChanged; override;
    procedure Loaded; override;
    procedure SetParent(const Value: TFmxObject); override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read GetBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read GetBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure BeginUpdate; override; // this is neccessary because the MakeBufBitmap is not only call during the paint,
    procedure EndUpdate; override;   // but also when any property changed because need to retrieve the dimension
    {$IF (not DEFINED(IOS)) and (not DEFINED(ANDROID))}
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    {$ENDIF}
  published
    property doubleBuffered: Boolean read GetdoubleBuffered write setdoubleBuffered default true;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property AutoConvertFontFamily: Boolean read FAutoConvertFontFamily write fAutoConvertFontFamily default true;
  end;

procedure Register;

implementation

uses system.SysUtils,
     system.Math,
     system.Math.Vectors,
     fmx.consts,
     {$IF defined(ANDROID)}
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.Bitmap,
     Androidapi.Helpers,
     FMX.Canvas.GPU,
     FMX.Helpers.Android,
     FMX.Surfaces,
     {$ENDIF}
     {$IF defined(IOS)}
     iOSapi.CocoaTypes,
     iOSapi.CoreGraphics,
     iOSapi.CoreText,
     iOSapi.UIKit,
     FMX.Canvas.GPU,
     FMX.Surfaces,
     {$ENDIF}
     ALFmxCommon;

{**************************************************}
constructor TALRectangle.Create(AOwner: TComponent);
begin
  inherited;
  fdoubleBuffered := true;
  fBufBitmap := nil;
end;

{******************************}
destructor TALRectangle.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{************************************}
procedure TALRectangle.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
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
var aSceneScale: Single;
    aBitmap: Jbitmap;
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
var aSceneScale: Single;
    aBitmapSurface: TbitmapSurface;
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
                            xRadius * aSceneScale {rx},
                            yRadius * aSceneScale {ry},
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
      aXRadius := xRadius * aSceneScale;
      aYradius := yRadius * aSceneScale;
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
      if (Stroke.Kind <> TBrushKind.None) then aHalfStrokeWidth := (Stroke.Thickness * aSceneScale) / 2
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

      aXRadius := xRadius * aSceneScale;
      aYradius := yRadius * aSceneScale;
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
      if (Stroke.Kind <> TBrushKind.None) then aHalfStrokeWidth := (Stroke.Thickness * aSceneScale) / 2
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

  {$IFDEF ANDROID}

  //init aSceneScale
  if Scene <> nil then aSceneScale := Scene.GetSceneScale
  else aSceneScale := 1;

  //init fBufBitmapRect / aRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, aSceneScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * aSceneScale), round(fBufBitmapRect.height * aSceneScale));

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
    if Stroke.Kind <> TBrushKind.None then aRect.Inflate((-(Stroke.Thickness * aSceneScale) / 2), (-(Stroke.Thickness * aSceneScale) / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles

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
          freeandNil(aColors);
          freeandNil(aStops);
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
      aPaint.setStrokeWidth(Stroke.Thickness * aSceneScale);

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
        fBufBitmap := TTexture.Create;
        try
          fBufBitmap.Assign(aBitmapSurface);
        except
          fBufBitmap.Free;
          fBufBitmap := nil;
          raise;
        end;

      end
      else fBufBitmap := nil;

    finally
      abitmapSurface.Free;
    end;

  finally
    aBitmap.recycle;
    aBitmap := nil;
  end;

  {$ELSEIF DEFINED(IOS)}

  //init aSceneScale
  if Scene <> nil then aSceneScale := Scene.GetSceneScale
  else aSceneScale := 1;

  //init fBufBitmapRect / aRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, aSceneScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * aSceneScale), round(fBufBitmapRect.height * aSceneScale));

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
        if Stroke.Kind <> TBrushKind.None then aRect.Inflate((-(Stroke.Thickness * aSceneScale) / 2), (-(Stroke.Thickness * aSceneScale) / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles

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
            CGContextSetLineWidth(aContext, Stroke.Thickness * aSceneScale);
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
    fBufBitmap := TTexture.Create;
    try
      fBufBitmap.Assign(aBitmapSurface);
    except
      fBufBitmap.Free;
      fBufBitmap := nil;
      raise;
    end;

  finally
    aBitmapSurface.Free;
    aBitmapSurface := nil;
  end;

  {$ENDIF}

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

{***********************************************}
constructor TALCircle.Create(AOwner: TComponent);
begin
  inherited;
  fdoubleBuffered := true;
  fBufBitmap := nil;
end;

{***************************}
destructor TALCircle.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{*********************************}
procedure TALCircle.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
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
var aSceneScale: Single;
    aBitmap: Jbitmap;
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
var aSceneScale: Single;
    aBitmapSurface: TbitmapSurface;
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

  {$IFDEF ANDROID}

  //init aSceneScale
  if Scene <> nil then aSceneScale := Scene.GetSceneScale
  else aSceneScale := 1;

  //init fBufBitmapRect / aRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), aSceneScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * aSceneScale), round(fBufBitmapRect.height * aSceneScale));

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
    if Stroke.Kind <> TBrushKind.None then aRect.Inflate((-(Stroke.Thickness * aSceneScale) / 2), (-(Stroke.Thickness * aSceneScale) / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles

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
          freeandNil(aColors);
          freeandNil(aStops);
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
      aPaint.setStrokeWidth(Stroke.Thickness * aSceneScale);

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
        fBufBitmap := TTexture.Create;
        try
          fBufBitmap.Assign(aBitmapSurface);
        except
          fBufBitmap.Free;
          fBufBitmap := nil;
          raise;
        end;

      end
      else fBufBitmap := nil;

    finally
      abitmapSurface.Free;
    end;

  finally
    aBitmap.recycle;
    aBitmap := nil;
  end;

  {$ELSEIF DEFINED(IOS)}

  //init aSceneScale
  if Scene <> nil then aSceneScale := Scene.GetSceneScale
  else aSceneScale := 1;

  //init fBufBitmapRect / aRect
  fBufBitmapRect := ALAlignDimensionToPixelRound(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect), aSceneScale); // to have the pixel aligned width and height
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * aSceneScale), round(fBufBitmapRect.height * aSceneScale));

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
        if Stroke.Kind <> TBrushKind.None then aRect.Inflate((-(Stroke.Thickness * aSceneScale) / 2), (-(Stroke.Thickness * aSceneScale) / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles

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
            CGContextSetLineWidth(aContext, Stroke.Thickness * aSceneScale);
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
    fBufBitmap := TTexture.Create;
    try
      fBufBitmap.Assign(aBitmapSurface);
    except
      fBufBitmap.Free;
      fBufBitmap := nil;
      raise;
    end;

  finally
    aBitmapSurface.Free;
    aBitmapSurface := nil;
  end;

  {$ENDIF}

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

{*********************************************}
constructor TALLine.Create(AOwner: TComponent);
begin
  inherited;
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
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
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
var aSceneScale: Single;
    aBitmap: Jbitmap;
    aBitmapSurface: TBitmapSurface;
    aCanvas: Jcanvas;
    aPaint: JPaint;
    aRect: TRectf;
    aStrokeWidth: Single;
{$ELSEIF defined(IOS)}
var aSceneScale: Single;
    aBitmapSurface: TbitmapSurface;
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

  {$IFDEF ANDROID}

  //init aSceneScale
  if Scene <> nil then aSceneScale := Scene.GetSceneScale
  else aSceneScale := 1;

  //init aStrokeWidth
  if (LineLocation = TLineLocation.InnerWithin) then aStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else aStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / aRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, aSceneScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, aStrokeWidth), aSceneScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -aStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, aStrokeWidth, height), aSceneScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-aStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - aStrokeWidth, Width, height), aSceneScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, aStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(width - aStrokeWidth, 0, width, height), aSceneScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(aStrokeWidth/2, 0);
                     end;
  end;
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * aSceneScale), round(fBufBitmapRect.height * aSceneScale));

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
      aPaint.setStrokeWidth(aStrokeWidth * aSceneScale);

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
        fBufBitmap := TTexture.Create;
        try
          fBufBitmap.Assign(aBitmapSurface);
        except
          fBufBitmap.Free;
          fBufBitmap := nil;
          raise;
        end;

      end
      else fBufBitmap := nil;

    finally
      abitmapSurface.Free;
    end;

  finally
    aBitmap.recycle;
    aBitmap := nil;
  end;

  {$ELSEIF DEFINED(IOS)}

  //init aSceneScale
  if Scene <> nil then aSceneScale := Scene.GetSceneScale
  else aSceneScale := 1;

  //init aStrokeWidth
  if (LineLocation = TLineLocation.InnerWithin) then aStrokeWidth := Min(Stroke.Thickness, Min(Width, Height))
  else aStrokeWidth := Stroke.Thickness;

  //init fBufBitmapRect / aRect
  case lineType of
    TLineType.Diagonal: fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, aSceneScale); // to have the pixel aligned width and height
    TLineType.Top: begin
                     fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, aStrokeWidth), aSceneScale); // to have the pixel aligned width and height
                     if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, -aStrokeWidth/2);
                   end;
    TLineType.Left: begin
                      fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, aStrokeWidth, height), aSceneScale); // to have the pixel aligned width and height
                      if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(-aStrokeWidth/2, 0);
                    end;
    TLineType.Bottom: begin
                        fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - aStrokeWidth, Width, height), aSceneScale); // to have the pixel aligned width and height
                        if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(0, aStrokeWidth/2);
                      end;
    TLineType.Right: begin
                       fBufBitmapRect := ALAlignDimensionToPixelRound(TrectF.Create(width - aStrokeWidth, 0, width, height), aSceneScale); // to have the pixel aligned width and height
                       if LineLocation = TlineLocation.Boundary then fBufBitmapRect.Offset(aStrokeWidth/2, 0);
                     end;
  end;
  aRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * aSceneScale), round(fBufBitmapRect.height * aSceneScale));

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
            CGContextSetLineWidth(aContext, Stroke.Thickness * aSceneScale);
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
    fBufBitmap := TTexture.Create;
    try
      fBufBitmap.Assign(aBitmapSurface);
    except
      fBufBitmap.Free;
      fBufBitmap := nil;
      raise;
    end;

  finally
    aBitmapSurface.Free;
    aBitmapSurface := nil;
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
{$IF defined(android) or defined(IOS)}
constructor TALDoubleBufferedTextLayoutNG.Create(const ACanvas: TCanvas; const aTextControl: TALText);
begin
  inherited Create(ACanvas);
  fdoubleBuffered := true;
  fBufBitmap := nil;
  fTextControl := aTextControl;
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
destructor TALDoubleBufferedTextLayoutNG.Destroy;
begin
  clearBufBitmap;
  inherited;
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
function TALDoubleBufferedTextLayoutNG.MakeBufBitmap: TTexture;

{$IF defined(android)}
var aSceneScale: Single;
    aBitmap: Jbitmap;
    aBitmapSurface: TBitmapSurface;
    aRect: TRectf;
    aPaint: JPaint;
    aTypeface: JTypeface;
    aStyle: integer;
    aCanvas: Jcanvas;
    aBreakedTextItems: TALBreakTextItems;
    aBreakedTextItem: TALBreakTextItem;
{$ELSEIF defined(IOS)}
var aSceneScale: Single;
    aBitmapSurface: TbitmapSurface;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    aRect: TRectf;
    aBreakedTextItems: TALBreakTextItems;
    aBreakedTextItem: TALBreakTextItem;
{$ENDIF}

begin

  if (csDesigning in fTextControl.ComponentState) or
     (not fdoubleBuffered) or
     (fTextControl.Scene = nil) or
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
       ((not fTextControl.WordWrap) or                                   // if not wordwrap we don't care because the dimensions returned by this function will not change   << because in TText.AdjustSize:
        (SameValue(fBufSize.cx, MaxSize.x, TEpsilon.position)) or        // if WordWrap the dimensions returned by this function will depend of the MaxSize.x                << if FAutoSize and (Text <> '') then
        (SameValue(fbufBitmapRect.width, MaxSize.x, TEpsilon.position))) //                                                                                                  << if WordWrap then R := TRectF.Create(0, 0, Width, MaxSingle)
      )                                                                  //                                                                                                  << else R := TRectF.Create(0, 0, MaxSingle, MaxSingle);
     ) and                                                               //
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
  if fBufAutosize then begin
    if fBufWordWrap then fBufSize := TsizeF.Create(MaxSize.x, 10000)
    else fBufSize := TsizeF.Create(10000, 10000);
  end
  else fBufSize := MaxSize;
  fBufText := fTextControl.Text;

  {$IF defined(android)}

  //init aSceneScale
  if fTextControl.Scene <> nil then aSceneScale := fTextControl.Scene.GetSceneScale
  else aSceneScale := 1;

  //init fBufBitmapRect / aRect
  fBufBitmapRect := TRectF.Create(0, 0, fBufSize.cX * aSceneScale, fBufSize.cY * aSceneScale);

  //create aPaint
  aPaint := TJPaint.JavaClass.init;
  aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  aPaint.setSubpixelText(true); // Enabling this flag causes glyph advances to be computed with subpixel accuracy.
  aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
  aPaint.setColor(fBufFontColor);
  aPaint.setTextSize(fBuffontSize * aSceneScale);
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
    ALBreakText(aPaint, // const aPaint: JPaint;
                fBufBitmapRect, // var ARect: TRectF;
                StringtoJString(fBufText), // const AText: JString;
                fBufWordWrap, //const aWordWrap: Boolean;
                fBufHorizontalAlign, fBufVerticalAlign, //const AHTextAlign, AVTextAlign: TTextAlign;
                fBufTrimming,
                aBreakedTextItems); // var aBreakedTexts: Tarray<Tpair<JString, TpointF>>);
    fbufBitmapRect.Top := fbufBitmapRect.Top / aSceneScale;
    fbufBitmapRect.right := fbufBitmapRect.right / aSceneScale;
    fbufBitmapRect.left := fbufBitmapRect.left / aSceneScale;
    fbufBitmapRect.bottom := fbufBitmapRect.bottom / aSceneScale;
    fBufBitmapRect := ALAlignDimensionToPixelCeil(fBufBitmapRect, aSceneScale);
    if fBufAutosize then fBufBitmapRect.Offset(-fBufBitmapRect.left, -fBufBitmapRect.top);
    aRect := TrectF.Create(0,0,round((fBufBitmapRect.Width)  * aSceneScale), round(fBufBitmapRect.height * aSceneScale));

    //create the main bitmap on with we will draw
    aBitmap := TJBitmap.JavaClass.createBitmap(round(max(1, aRect.Width)),  // max because aRect.Width could = 0 if breaked at 1rt char
                                               round(max(1, aRect.Height)), // max no possible but more beautifull to write like this
                                               TJBitmap_Config.JavaClass.ARGB_8888);
    try

      //create the canvas and the paint
      aCanvas := TJCanvas.JavaClass.init(aBitmap);

      //draw all texts
      for aBreakedTextItem in aBreakedTextItems do
        aCanvas.drawText(aBreakedTextItem.line{text},
                         aBreakedTextItem.pos.x {x},
                         aBreakedTextItem.pos.y {y},
                         apaint {paint});

      //free the paint and the canvas
      aPaint := nil;
      aCanvas := nil;

      //init the bitmapSurface that we will use to convert the jbitmap
      aBitmapSurface := TBitmapSurface.Create;
      try

        //convert the JBitmapToSurface to the bitmapSurface
        if JBitmapToSurface(aBitmap, aBitmapSurface) then begin

          //convert the bitmapSurface to a TTexture
          fBufBitmap := TTexture.Create;
          try
            fBufBitmap.Assign(aBitmapSurface);
          except
            fBufBitmap.Free;
            fBufBitmap := nil;
            raise;
          end;

        end
        else fBufBitmap := nil;

      finally
        abitmapSurface.Free;
      end;

    finally
      aBitmap.recycle;
      aBitmap := nil;
    end;

  finally
    aBreakedTextItems.Free;
    aBreakedTextItems := nil;
  end;

  {$ELSEIF DEFINED(IOS)}

  //init the color space
  aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
  if aColorSpace = nil then exit(nil);         // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
  try

    //init aSceneScale
    if fTextControl.Scene <> nil then aSceneScale := fTextControl.Scene.GetSceneScale
    else aSceneScale := 1;

    //init fBufBitmapRect / aRect
    fBufBitmapRect := TRectF.Create(0, 0, fBufSize.cX * aSceneScale, fBufSize.cY * aSceneScale);

    //create the aBreakedTextItems
    aBreakedTextItems := TALBreakTextItems.Create(true{aOwnsObjects});
    try

      //break the text
      ALBreakText(aColorSpace, // const aColorSpace: CGColorSpaceRef;
                  fBufFontColor, //const aFontColor: TalphaColor;
                  fBuffontSize * aSceneScale, //const aFontSize: single;
                  fBuffontStyle, //const aFontStyle: TFontStyles;
                  fBuffontFamily, //const aFontName: String;
                  fBufBitmapRect, //var ARect: TRectF;
                  fBufText, // const AText: string;
                  fBufWordWrap, //const aWordWrap: Boolean;
                  fBufHorizontalAlign, fBufVerticalAlign, //const AHTextAlign, AVTextAlign: TTextAlign;
                  fBufTrimming, //const aTrimming: TTextTrimming;
                  aBreakedTextItems); //var aBreakTextItems: TALBreakTextItems
      fbufBitmapRect.Top := fbufBitmapRect.Top / aSceneScale;
      fbufBitmapRect.right := fbufBitmapRect.right / aSceneScale;
      fbufBitmapRect.left := fbufBitmapRect.left / aSceneScale;
      fbufBitmapRect.bottom := fbufBitmapRect.bottom / aSceneScale;
      fBufBitmapRect := ALAlignDimensionToPixelCeil(fBufBitmapRect, aSceneScale);
      if fBufAutosize then fBufBitmapRect.Offset(-fBufBitmapRect.left, -fBufBitmapRect.top);
      aRect := TrectF.Create(0,0,round((fBufBitmapRect.Width)  * aSceneScale), round(fBufBitmapRect.height * aSceneScale));

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
          for aBreakedTextItem in aBreakedTextItems do begin
            CGContextSetTextPosition(acontext,
                                     aBreakedTextItem.pos.x {x},
                                     aBitmapSurface.Height - aBreakedTextItem.pos.Y);{y}
            CTLineDraw(aBreakedTextItem.Line, acontext); // Draws a complete line.
          end;

        finally
          CGContextRelease(aContext);
        end;

        //convert the aBitmapSurface to texture
        fBufBitmap := TTexture.Create;
        try
          fBufBitmap.Assign(aBitmapSurface);
        except
          fBufBitmap.Free;
          fBufBitmap := nil;
          raise;
        end;

      finally
        aBitmapSurface.Free;
        aBitmapSurface := nil;
      end;

    finally
      aBreakedTextItems.Free;
      aBreakedTextItems := nil;
    end;

  finally
    CGColorSpaceRelease(aColorSpace);
  end;

  {$ENDIF}

  result := fBufBitmap;

end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
procedure TALDoubleBufferedTextLayoutNG.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
procedure TALDoubleBufferedTextLayoutNG.DoRenderLayout;
begin
  MakeBufBitmap; // recreate the fBufBitmap
  if fBufBitmap = nil then inherited DoRenderLayout; // if no fBufBitmap then inherited
end;
{$ENDIF}

{************************************}
{$IF defined(android) or defined(IOS)}
procedure TALDoubleBufferedTextLayoutNG.DoDrawLayout(const ACanvas: TCanvas);
var aDestRect: TrectF;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited DoDrawLayout(ACanvas);
    exit;
  end;

  aDestRect := fBufBitmapRect;
  if fBufAutosize then aDestRect := aDestRect.CenterAt(FTextControl.LocalRect); // for for 2 raisons:
                                                                                // 1) when for exemple when align of FTextControl is set then aDestRect could be lower than FTextControl.LocalRect
                                                                                // 2) because TText add always some extra space in TText.adjusteSize (FTextSettings.Font.Size / 3)
                                                                                //    i find this behavior ugly but {@#{^ TText.adjusteSize is not virtual so i can't change it :(


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

  if fBufBitmap = nil then result := inherited GetTextRect  // << if fBufBitmap = nil it's mean last call to DoRenderLayout was inherited
  else result := fBufBitmapRect;

  //this to take care of the align constraint of the ftextLayout
  if fTextControl.Align in [TAlignLayout.Client,
                            TAlignLayout.Contents,
                            TAlignLayout.Top,
                            TAlignLayout.Bottom,
                            TAlignLayout.MostTop,
                            TAlignLayout.MostBottom,
                            TAlignLayout.VertCenter] then begin
    result.Left := 0;
    result.Width := fTextControl.Width;
  end;
  if fTextControl.Align in [TAlignLayout.Client,
                            TAlignLayout.Contents,
                            TAlignLayout.Left,
                            TAlignLayout.Right,
                            TAlignLayout.MostLeft,
                            TAlignLayout.MostRight,
                            TAlignLayout.HorzCenter] then begin
    result.Top := 0;
    result.height := fTextControl.height;
  end;

  //this to remove the FTextSettings.Font.Size / 3 from SetBounds(Position.X, Position.Y, R.Width + R.Left * 2 + FTextSettings.Font.Size / 3, R.Height + R.Top * 2);
  //that is done inside TText.AdjustSize
  {$IF CompilerVersion <> 31}
    {$MESSAGE WARN 'Check if FMX.Objects.TText.adjustsize still doing FTextSettings.Font.Size / 3 and adjust the IFDEF'}
  {$ENDIF}
  if (FTextControl.AutoSize) and
     (FTextControl.Text <> '') then result.Inflate(0, 0, -FTextControl.TextSettings.Font.Size / 3, 0);

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

{*********************************************}
constructor TALText.Create(AOwner: TComponent);
begin

  fFontchangeDeactivated := True; // << to deactivate fontchange
  inherited; (* constructor TText.Create(AOwner: TComponent);
                var
                  LClass: TTextSettingsClass;
                begin
                  inherited;
                  LClass := GetTextSettingsClass;
                  if LClass = nil then
                    LClass := TTextTextSettings;
                  FLayout := TTextLayoutManager.DefaultTextLayout.Create;
                  FDefaultTextSettings := LClass.Create(Self);
                  FDefaultTextSettings.OnChanged := OnFontChanged;
                  FTextSettings := LClass.Create(Self);
                  FTextSettings.OnChanged := OnFontChanged;
                  FTextSettings.BeginUpdate;
                  try
                    FTextSettings.IsAdjustChanged := True;
                  finally
                    FTextSettings.EndUpdate; // << will call fontchanged but the TALdoubleBufferedTextLayoutNG is not yet
                                                << assigned. so i introduce the flag fFontchangeDeactivated to deactivate
                                                << the FontChanged.
                  end;
                end; *)
  fFontchangeDeactivated := False; // << to reactivate fontchange

  FAutoConvertFontFamily := True;
  FAutoTranslate := true;
  {$IF (not DEFINED(IOS)) and (not DEFINED(ANDROID))}
  fdoubleBuffered := true;
  {$ENDIF}

  {$IF defined(android) or defined(IOS)}

  //create the overriden FLayout
  TALTextAccessPrivate(self).FLayout.Free;
  TALTextAccessPrivate(self).FLayout := TALdoubleBufferedTextLayoutNG.Create(nil, self);

  //i use this way to know that the compoment is created via form creation
  if (aOwner <> nil) and
     (csloading in aOwner.ComponentState) then begin
    fRestoreLayoutUpdate := True;
    Layout.BeginUpdate;
  end
  else fRestoreLayoutUpdate := False;

  //update the Flayout.TextSettings
  TextSettings.BeginUpdate;
  try
    TextSettings.IsAdjustChanged := True;
  finally
    TextSettings.EndUpdate;
  end;

  {$ENDIF}

end;

{****************************}
procedure TALText.FontChanged;
begin
  {$IF defined(android) or defined(IOS)}
  if not fFontchangeDeactivated then inherited;
  {$ELSE}
  inherited;
  {$ENDIF}
end;

{***********************}
procedure TALText.Loaded;
begin
  {$IF defined(android) or defined(IOS)}
  if fRestoreLayoutUpdate then begin
    Layout.MaxSize := TPointF.Create(width, height); // << this is important because else when the component is loaded then
                                                     // << we will call DoRenderLayout that will use the original maxsise (ie: 65535, 65535)
                                                     // << and then after when we will paint the control, we will again call DoRenderLayout
                                                     // << but this time with maxsize = aTextControl.size and off course if wordwrap we will
                                                     // << need to redo the bufbitmap
    if (AutoTranslate) and
       (Text <> '') and
       (not (csDesigning in ComponentState)) then
        Text := Translate(Text);
    if (AutoConvertFontFamily) and
       (TextSettings.Font.Family <> '') and
       (not (csDesigning in ComponentState)) then
        TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family);
    Layout.endUpdate;
  end;
  fRestoreLayoutUpdate := False;
  {$ENDIF}
  inherited;
end;

{***************************************************}
procedure TALText.SetParent(const Value: TFmxObject);
begin
  if FParent <> Value then begin
    inherited;
    if (Value <> nil) and
       (Value <> self) and
       (Value is TControl) and
       (TControl(Value).isupdating) then Layout.BeginUpdate; // stupidly when we do setparent the
                                                             // FisUpdating set to the value of the parent BUT without any
                                                             // notifications nor any call to beginupdate :(
  end;
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
  fSaveDisableAlign := FDisableAlign;
  FDisableAlign := True; // to deactivate the TText.AdjustSize
end;

{**************************}
procedure TALText.EndUpdate;
begin
  FDisableAlign := fSaveDisableAlign; // to reactivate the TText.AdjustSize
  Layout.EndUpdate;
  inherited; // will call dorealign that will call AdjustSize
end;

{*************************************************}
{$IF (not DEFINED(IOS)) and (not DEFINED(ANDROID))}
procedure TALText.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  if FDisableAlign then begin // i use this to know that we are here because of a call to !!#@{^!! adjustsize that is not virtual

    //this to take care of the align constraint of the ftextLayout
    if Align in [TAlignLayout.Client,
                 TAlignLayout.Contents,
                 TAlignLayout.Top,
                 TAlignLayout.Bottom,
                 TAlignLayout.MostTop,
                 TAlignLayout.MostBottom,
                 TAlignLayout.VertCenter] then begin
      x := 0;
      aWidth := Width;
    end;
    if Align in [TAlignLayout.Client,
                 TAlignLayout.Contents,
                 TAlignLayout.Left,
                 TAlignLayout.Right,
                 TAlignLayout.MostLeft,
                 TAlignLayout.MostRight,
                 TAlignLayout.HorzCenter] then begin
      y := 0;
      aheight := height;
    end;

    //this to remove the FTextSettings.Font.Size / 3 from SetBounds(Position.X, Position.Y, R.Width + R.Left * 2 + FTextSettings.Font.Size / 3, R.Height + R.Top * 2);
    //that is done inside TText.AdjustSize
    {$IF CompilerVersion <> 31}
      {$MESSAGE WARN 'Check if FMX.Objects.TText.adjustsize still doing FTextSettings.Font.Size / 3 and adjust the IFDEF'}
    {$ENDIF}
    if (AutoSize) and
       (Text <> '') then AWidth := AWidth - (TextSettings.Font.Size / 3);

  end;
  inherited SetBounds(X, Y, AWidth, AHeight);
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALRectangle, TALCircle, TALLine, TALText]);
end;

initialization
  RegisterFmxClasses([TALRectangle, TALCircle, TALLine, TALText]);

end.

