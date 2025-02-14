unit Alcinoe.FMX.Graphics;

interface

{$I Alcinoe.inc}

uses
  {$IF defined(MSWINDOWS)}
  Winapi.GDIPOBJ,
  Winapi.GDIPAPI,
  {$ENDIF}
  system.classes,
  system.sysutils,
  system.types,
  system.uitypes,
  System.Math.Vectors,
  {$IF defined(ALSkiaAvailable)}
  System.Skia.API,
  {$ENDIF}
  {$IF defined(ios)}
  iOSapi.CoreGraphics,
  iOSapi.CocoaTypes,
  iOSapi.UIKit,
  fmx.surfaces,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Macapi.CoreGraphics,
  Macapi.CocoaTypes,
  Macapi.AppKit,
  fmx.surfaces,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  fmx.surfaces,
  {$ENDIF}
  FMX.types,
  fmx.types3D,
  FMX.graphics,
  Alcinoe.FMX.Common,
  Alcinoe.Common;

Type
  // !! Note: The declaration of TALSurface/TALCanvas/TALBitmap/TALDrawable is duplicated in Alcinoe.FMX.Common to avoid a circular unit reference.
  TALSurface =  {$IF defined(ALSkiaEngine)}sk_surface_t{$ELSEIF defined(ANDROID)}Jbitmap{$ELSEIF defined(ALAppleOS)}CGContextRef{$ELSE}Tbitmap{$ENDIF};
  TALCanvas =   {$IF defined(ALSkiaEngine)}sk_canvas_t{$ELSEIF defined(ANDROID)}Jcanvas{$ELSEIF defined(ALAppleOS)}CGContextRef{$ELSE}Tcanvas{$ENDIF};
  TALBitmap =   {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
  TALDrawable = {$IF defined(ALSkiaCanvas)}sk_image_t{$ELSEIF defined(ALGpuCanvas)}TTexture{$ELSE}Tbitmap{$ENDIF};

const
  ALNullSurface = {$IF defined(ALSkiaEngine)}0{$ELSE}Nil{$ENDIF};
  ALNullCanvas = {$IF defined(ALSkiaEngine)}0{$ELSE}Nil{$ENDIF};
  ALNullDrawable = {$IF defined(ALSkiaCanvas)}0{$ELSE}Nil{$ENDIF};
  ALNullBitmap = {$IF defined(ALSkiaEngine)}0{$ELSE}Nil{$ENDIF};

function  ALIsSurfaceNull(const aSurface: TALSurface): Boolean; inline;
function  ALIsCanvasNull(const aCanvas: TALCanvas): Boolean; inline;
function  ALIsDrawableNull(const aDrawable: TALDrawable): Boolean; inline;
function  ALIsBitmapNull(const aBitmap: TALBitmap): Boolean; inline;
procedure ALFreeAndNilDrawable(var aDrawable: TALDrawable); inline;
procedure ALFreeAndNilBitmap(var aBitmap: TALBitmap); inline;
function  ALGetDrawableWidth(const aDrawable: TALDrawable): integer; inline;
function  ALGetDrawableHeight(const aDrawable: TALDrawable): integer; inline;
function  ALCanvasBeginScene(const aCanvas: TALCanvas): Boolean; inline;
procedure ALCanvasEndScene(const aCanvas: TALCanvas); inline;
procedure ALClearCanvas(const aCanvas: TALCanvas; const AColor: TAlphaColor);
procedure ALBeginTransparencyLayer(const aCanvas: TALCanvas; const ARect: TRectF; const AOpacity: Single; const AAddPixelForAlignment: Boolean = true);
procedure ALEndTransparencyLayer(const aCanvas: TALCanvas);
function  ALCreateDrawableFromSurface(const ASurface: TALSurface): TALDrawable;
procedure ALUpdateDrawableFromSurface(const aSurface: TALSurface; const aDrawable: TALDrawable);
function  ALGetDefaultPixelFormat: TPixelFormat; inline;
{$IF defined(ALSkiaAvailable)}
procedure ALExtractMatrixFromSkCanvas(const ACanvas: sk_canvas_t; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
{$ENDIF}
{$IF defined(ANDROID)}
procedure ALExtractMatrixFromJCanvas(const ACanvas: JCanvas; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
{$ENDIF}
{$IF defined(ALAppleOS)}
procedure ALExtractMatrixFromCGContextRef(const ACanvas: CGContextRef; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
{$ENDIF}
procedure ALExtractMatrixFromTCanvas(const ACanvas: TCanvas; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
procedure ALExtractMatrixFromCanvas(const ACanvas: TALCanvas; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
function  ALScaleAndCenterCanvas(
            Const ACanvas: TCanvas;
            Const AAbsoluteRect: TRectF;
            Const AScale: Single;
            Const ASaveState: Boolean): TCanvasSaveState;

{$IF defined(ALSkiaAvailable)}
// ALGlobalSkColorSpace represents the color space used in all drawing operations
var ALGlobalSkColorSpace: sk_colorspace_t;
var ALGlobalSkColorSpaceInitialized: Boolean;
function ALGetGlobalSkColorSpace: sk_colorspace_t;
function ALCreateDisplayP3SkColorSpace: sk_colorspace_t;

// ALGlobalSkPaint is utilized for drawing images on the form's canvas
var ALGlobalSkPaint: sk_paint_t;
function ALGetGlobalSkPaint(const AOpacity: Single): sk_paint_t;

function ALSkCheckHandle(const AHandle: sk_handle_t): sk_handle_t; inline;
procedure ALSkCheckCanvas(const ACanvas: TCanvas); inline;
function ALSkStreamAdapterGetLengthProc(context: Pointer): size_t; cdecl;
function ALSkStreamAdapterGetPositionProc(context: Pointer): size_t; cdecl;
function ALSkStreamAdapterReadProc(context: Pointer; buffer: Pointer; size: size_t): size_t; cdecl;
function ALSkStreamAdapterSeekProc(context: Pointer; position: size_t): _bool; cdecl;

var
  ALGlobalUseRasterSkSurface: Boolean;
  ALGlobalUseRasterSkImage: Boolean;

function ALGetSkImageinfo(const W, H: int32_t): sk_imageinfo_t;
function ALCreateSkSurface(Const W, H: integer): sk_surface_t;
function ALCreateTBitmapFromSkPixmap(Const APixmap: sk_pixmap_t): TBitmap;
function ALCreateTBitmapFromSkSurface(Const ASurface: sk_surface_t): TBitmap;
Function ALCreateTBitmapFromSkImage(const AImage: sk_image_t): TBitmap;
procedure ALUpdateTBitmapFromSkPixmap(Const APixmap: sk_pixmap_t; const ABitmap: Tbitmap);
procedure ALUpdateTBitmapFromSkSurface(Const ASurface: sk_surface_t; const ABitmap: TBitmap);
procedure ALUpdateTBitmapFromSkImage(const AImage: sk_image_t; const ABitmap: TBitmap);
function ALCreateSkImageFromSkSurface(Const ASurface: sk_surface_t): sk_image_t;
function ALCreateTextureFromSkSurface(Const ASurface: sk_surface_t): TTexture;
function ALCreateTextureFromSkImage(Const AImage: sk_image_t): TTexture;
procedure ALUpdateTextureFromSkPixmap(Const APixmap: sk_pixmap_t; const ATexture: TTexture);
procedure ALUpdateTextureFromSkSurface(Const ASurface: sk_surface_t; const ATexture: TTexture);
procedure ALUpdateTextureFromSkImage(Const AImage: sk_image_t; const ATexture: TTexture);
function ALGetCubicMitchellNetravaliSkSamplingoptions: sk_samplingoptions_t;
function ALGetLinearSkSamplingoptions: sk_samplingoptions_t;
function ALGetNearestSkSamplingoptions: sk_samplingoptions_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALCreateTextureFromJBitmap(const aBitmap: Jbitmap): TTexture;
procedure ALUpdateTextureFromJBitmap(const aBitmap: Jbitmap; const ATexture: TTexture);
{$ENDIF}
{$IF defined(ALAppleOS)}
function ALCreateTextureFromCGContextRef(const aCGContextRef: CGContextRef): TTexture;
function ALCreateTBitmapFromCGContextRef(const aCGContextRef: CGContextRef): TBitmap;
procedure ALUpdateTextureFromCGContextRef(const aCGContextRef: CGContextRef; const ATexture: TTexture);
procedure ALUpdateTBitmapFromCGContextRef(const aCGContextRef: CGContextRef; const ABitmap: TBitmap);
{$ENDIF}
{$IFDEF ALGpuCanvas}
function ALCreateTextureFromTBitmapSurface(const aBitmapSurface: TbitmapSurface): TTexture;
function ALCreateTextureFromTBitmap(const aBitmap: Tbitmap): TTexture;
procedure ALUpdateTextureFromTBitmapSurface(const aBitmapSurface: TbitmapSurface; const ATexture: TTexture);
procedure ALUpdateTextureFromTBitmap(const aBitmap: Tbitmap; const ATexture: TTexture);
{$ENDIF}

{$IF defined(ANDROID)}
// ALGlobalJColorSpace represents the color space used in all drawing operations
var ALGlobalJColorSpace: JColorSpace;
var ALGlobalJColorSpaceInitialized: Boolean;
function ALGetGlobalJColorSpace: JColorSpace;
{$ENDIF}

{$IF defined(ALAppleOS)}

Type
  ALOSImage = {$IF defined(IOS)}UIImage{$ELSE}NSImage{$ENDIF};
  TALOSImage = {$IF defined(IOS)}TUIImage{$ELSE}TNSImage{$ENDIF};

// ALGlobalCGColorSpace represents the color space used in all drawing operations
var ALGlobalCGColorSpace: CGColorSpaceRef;
var ALGlobalCGColorSpaceInitialized: Boolean;
function ALGetGlobalCGColorSpace: CGColorSpaceRef;

function ALCreateCGContextRef(const W, H: integer; const AData: Pointer = nil; const ABytesPerRow: Integer = -1): CGContextRef;
function ALOSImageGetCgImage(const AImage: ALOSImage): cgImageRef; inline;
function ALOSImageGetWidth(const AImage: ALOSImage): Integer; inline;
function ALOSImageGetHeight(const AImage: ALOSImage): Integer; inline;
procedure ALGradientEvaluateCallback(info: Pointer; inData: PCGFloat; outData: PAlphaColorCGFloat); cdecl;

{$ENDIF}

type
  TalExifOrientationInfo = (
    FLIP_HORIZONTAL,
    FLIP_VERTICAL,
    NORMAL,
    ROTATE_180,
    ROTATE_270,
    ROTATE_90,
    TRANSPOSE,
    TRANSVERSE,
    UNDEFINED);

function ALIsDefaultContextOpenGL: Boolean;
function ALGetImageDimensions(const aStream: TStream): TSize;
function AlGetExifOrientationInfo(const aFilename: String): TalExifOrientationInfo; overload;
function AlGetExifOrientationInfo(const aStream: TStream): TalExifOrientationInfo; overload;
function AlGetImageSignature(const aStream: TStream; const aSignatureLength: integer = 12): Tbytes; overload;
function AlGetImageSignature(const aFileName: string; const aSignatureLength: integer = 12): Tbytes; overload;
function AlDetectImageExtension(const aStream: TStream): String; overload;
function AlDetectImageExtension(const aFileName: string): String; overload;
function ALModulateColor(const SrcColor: TAlphaColor; const Opacity: Single): TAlphaColor;
function ALBlendColor(const ABaseColor, AOverlayColor: TAlphaColor): TAlphaColor; overload;
function ALBlendColor(const ABaseColor, AOverlayColor: TAlphaColor; const AOverlayOpacity: Single): TAlphaColor; overload;
function ALSetColorAlpha(const AColor: TAlphaColor; const AOpacity: Single): TAlphaColor;
function ALMultiplyColorAlpha(const AColor: TAlphaColor; const AOpacity: Single): TAlphaColor;
function ALConvertRadiusToSigma(const ARadius: Single): Single;
function ALConvertSigmaToRadius(const ASigma: Single): Single;
procedure ALNormalizeAndScaleRadii(var AXRadius, AYRadius: Single; const AScale: Single; const AScaledRect: TRectF);
function ALGetShadowWidth(const AShadowBlur: Single): Single;
procedure ALGetLinearGradientCoordinates(const ASize: TSizeF; const AAngle: Single; out AStartPoint: TPointF; out AEndPoint: TPointF; const ACssAngleConvention: Boolean = True);
function ALGetShapeSurfaceRect(
           const ARect: TRectF;
           const AFillColor: TAlphaColor;
           const AFillGradientColors: TArray<TAlphaColor>;
           const AFillResourceName: String;
           const AFillResourceStream: TStream;
           Const AFillBackgroundMarginsRect: TRectF;
           Const AFillImageMarginsRect: TRectF;
           const AStateLayerOpacity: Single;
           const AStateLayerColor: TAlphaColor;
           const AStateLayerUseContentColor: Boolean;
           Const AStateLayerMarginsRect: TRectF;
           const AShadowColor: TAlphaColor;
           const AShadowBlur: Single;
           const AShadowOffsetX: Single;
           const AShadowOffsetY: Single): TRectF; overload;
function ALGetShapeSurfaceRect(
           const ARect: TrectF;
           const AFill: TALBrush;
           const AFillResourceStream: TStream;
           const AStateLayer: TALStateLayer;
           const AShadow: TALShadow): TRectF; overload;
function ALCreateEmptyDrawable1x1: TALDrawable;
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure ALInitControlRenderTargets(
            Const ARect: TrectF;
            var ARenderTargetSurface: TALSurface;
            var ARenderTargetCanvas: TALCanvas;
            var ARenderTargetDrawable: TALDrawable);
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALDrawSkImage(
            const ACanvas: sk_canvas_t;
            const AImage: sk_image_t;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In AImage coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskImage: sk_image_t;
            const ACropCenter: TpointF; // Used only when AMaskImage is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);
function ALCreateSkSurfaceFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskImage: sk_image_t;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): sk_surface_t;
function ALCreateSkImageFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskImage: sk_image_t;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
procedure ALDrawJBitmap(
            const ACanvas: JCanvas;
            const ABitmap: JBitmap;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In ABitmap coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskBitmap: JBitmap;
            const ACropCenter: TpointF; // Used only when AMaskBitmap is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);
function ALCreateJBitmapFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskBitmap: JBitmap;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
procedure ALDrawCGImageRef(
            const ACanvas: CGContextRef;
            const AImage: CGImageRef;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In AImage coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskImage: CGImageRef;
            const ACropCenter: TpointF; // Used only when AMaskImage is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);
function ALCreateCGContextRefFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskImage: CGImageRef;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): CGContextRef;
function ALCreateCGImageRefFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskImage: CGImageRef;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): CGImageRef;
{$ENDIF}

{**********************}
procedure ALDrawTBitmap(
            const ACanvas: TCanvas;
            const ABitmap: TBitmap;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In ABitmap coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskBitmap: TBitmap;
            const ACropCenter: TpointF; // Used only when AMaskBitmap is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);
function ALCreateTBitmapFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskBitmap: TBitmap;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): TBitmap;

{*********************}
procedure ALDrawBitmap(
            const ACanvas: TALCanvas;
            const ABitmap: TALBitmap;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In ABitmap coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskBitmap: TALBitmap;
            const ACropCenter: TpointF; // Used only when AMaskBitmap is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single); inline;
function ALCreateBitmapFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskBitmap: TALBitmap;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): TALBitmap; inline;

{***********************}
procedure ALDrawDrawable(
            const ACanvas: Tcanvas;
            const ADrawable: TALDrawable;
            const ADstTopLeft: TpointF;
            const AOpacity: Single); overload;
procedure ALDrawDrawable(
            const ACanvas: Tcanvas;
            const ADrawable: TALDrawable;
            const ASrcRect: TrectF; // IN REAL PIXEL !
            const ADstRect: TrectF; // IN Virtual pixels !
            const AOpacity: Single); overload;
procedure ALDrawDrawable(
            const ACanvas: Tcanvas;
            const ADrawable: TALDrawable;
            const ADstRect: TrectF; // IN Virtual pixels !
            const AOpacity: Single); overload;
function ALCreateDrawableFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskBitmap: TALBitmap;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): TALDrawable;

Type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  PALDrawRectangleHelper = ^TALDrawRectangleHelper;
  TALDrawRectangleHelper = record
  private
    FCanvas: TALCanvas;
    FScale: Single;
    FAlignToPixel: Boolean;
    FDstRect: TrectF;
    FOpacity: Single;
    FFillColor: TAlphaColor;
    FFillGradientStyle: TGradientStyle;
    FFillGradientAngle: Single;
    FFillGradientStartPoint: TPointF;
    FFillGradientEndPoint: TPointF;
    FFillGradientColors: TArray<TAlphaColor>;
    FFillGradientOffsets: TArray<Single>;
    FFillResourceName: String;
    FFillResourceStream: TStream;
    FFillMaskResourceName: String;
    FFillMaskBitmap: TALBitmap;
    FFillBackgroundMarginsRect: TRectF;
    FFillImageMarginsRect: TRectF;
    FFillImageNoRadius: Boolean;
    FFillWrapMode: TALImageWrapMode;
    FFillCropCenter: TpointF;
    FFillBlurRadius: single;
    FStateLayerOpacity: Single;
    FStateLayerColor: TAlphaColor;
    FStateLayerMarginsRect: TRectF;
    FStateLayerXRadius: Single;
    FStateLayerYRadius: Single;
    FDrawStateLayerOnTop: Boolean;
    FStrokeColor: TalphaColor;
    FStrokeThickness: Single;
    FShadowColor: TAlphaColor;
    FShadowBlur: Single;
    FShadowOffsetX: Single;
    FShadowOffsetY: Single;
    FSides: TSides;
    FCorners: TCorners;
    FXRadius: Single;
    FYRadius: Single;
  public
    class operator Initialize (out Dest: TALDrawRectangleHelper);
    constructor Create(const ACanvas: TALCanvas);
    function SetScale(const AValue: Single): PALDrawRectangleHelper;
    function SetAlignToPixel(const AValue: Boolean): PALDrawRectangleHelper;
    function SetDstRect(const AValue: TrectF): PALDrawRectangleHelper;
    function SetOpacity(const AValue: Single): PALDrawRectangleHelper;
    function SetFill(const AFill: TALBrush): PALDrawRectangleHelper;
    function SetFillColor(const AValue: TAlphaColor): PALDrawRectangleHelper;
    function SetFillGradientStyle(const AValue: TGradientStyle): PALDrawRectangleHelper;
    function SetFillGradientAngle(const AValue: Single): PALDrawRectangleHelper;
    /// <summary>
    ///   Coordinates in DstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
    /// </summary>
    function SetFillGradientStartPoint(const AValue: TPointF): PALDrawRectangleHelper;
    /// <summary>
    ///   Coordinates in DstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
    /// </summary>
    function SetFillGradientEndPoint(const AValue: TPointF): PALDrawRectangleHelper;
    function SetFillGradientColors(const AValue: TArray<TAlphaColor>): PALDrawRectangleHelper;
    function SetFillGradientOffsets(const AValue: TArray<Single>): PALDrawRectangleHelper;
    function SetFillResourceName(const AValue: String): PALDrawRectangleHelper;
    function SetFillResourceStream(const AValue: TStream): PALDrawRectangleHelper;
    function SetFillMaskResourceName(const AValue: String): PALDrawRectangleHelper;
    function SetFillMaskBitmap(const AValue: TALBitmap): PALDrawRectangleHelper;
    function SetFillBackgroundMarginsRect(const AValue: TRectF): PALDrawRectangleHelper;
    function SetFillImageMarginsRect(const AValue: TRectF): PALDrawRectangleHelper;
    function SetFillImageNoRadius(const AValue: Boolean): PALDrawRectangleHelper;
    function SetFillWrapMode(const AValue: TALImageWrapMode): PALDrawRectangleHelper;
    function SetFillCropCenter(const AValue: TpointF): PALDrawRectangleHelper;
    function SetFillBlurRadius(const AValue: single): PALDrawRectangleHelper;
    function SetStateLayer(const AStateLayer: TALStateLayer; const AContentColor: TAlphaColor): PALDrawRectangleHelper;
    function SetStateLayerOpacity(const AValue: Single): PALDrawRectangleHelper;
    function SetStateLayerColor(const AValue: TAlphaColor): PALDrawRectangleHelper;
    function SetStateLayerMarginsRect(const AValue: TRectF): PALDrawRectangleHelper;
    function SetStateLayerXRadius(const AValue: Single): PALDrawRectangleHelper;
    function SetStateLayerYRadius(const AValue: Single): PALDrawRectangleHelper;
    function SetDrawStateLayerOnTop(const AValue: Boolean): PALDrawRectangleHelper;
    function SetStroke(const AStroke: TALStrokeBrush): PALDrawRectangleHelper;
    function SetStrokeColor(const AValue: TalphaColor): PALDrawRectangleHelper;
    function SetStrokeThickness(const AValue: Single): PALDrawRectangleHelper;
    function SetShadow(const AShadow: TALShadow): PALDrawRectangleHelper;
    /// <summary>
    ///   If ShadowColor is not null, the Canvas should have adequate space to
    ///   accommodate the shadow. You can use the ALGetShadowWidth function to
    ///   estimate the required width.
    /// </summary>
    function SetShadowColor(const AValue: TAlphaColor): PALDrawRectangleHelper;
    function SetShadowBlur(const AValue: Single): PALDrawRectangleHelper;
    function SetShadowOffsetX(const AValue: Single): PALDrawRectangleHelper;
    function SetShadowOffsetY(const AValue: Single): PALDrawRectangleHelper;
    function SetSides(const AValue: TSides): PALDrawRectangleHelper;
    function SetCorners(const AValue: TCorners): PALDrawRectangleHelper;
    function SetXRadius(const AValue: Single): PALDrawRectangleHelper;
    function SetYRadius(const AValue: Single): PALDrawRectangleHelper;
    procedure Draw;
  End;

{************************}
Procedure ALCreateSurface(
            out ASurface: TALSurface;
            out ACanvas: TALCanvas;
            const AScale: Single;
            const w: Single;
            const h: Single;
            const AAddPixelForAlignment: Boolean = true); overload;
Procedure ALCreateSurface(
            out ASurface: TALSurface;
            out ACanvas: TALCanvas;
            const w: Integer;
            const h: Integer;
            const AAddPixelForAlignment: Boolean = true); overload;
procedure ALFreeAndNilSurface(
            Var ASurface: TALSurface;
            Var ACanvas: TALCanvas);

type

  //
  //OpenGL need to be initialised in each thread that
  //use it. This is done automatiquelly in the
  //framework (ie: TCustomAndroidContext.ActivateSharedContext).
  //The problem is that this initialisation
  //can be a little costly (eglCreateContext, CreateEGLSurface)
  //and also their is no uninitialisation made by the framework
  //when we destroy the thread. So we create a pool of opengl
  //threads that we initialize one time and that we will re-use
  //

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALGraphicThreadPool = class(TALWorkerThreadPool)
  private
    class function CreateInstance: TALGraphicThreadPool;
    class function GetInstance: TALGraphicThreadPool; static;
  protected
    class var FInstance: TALGraphicThreadPool;
  public
    type
      TCreateInstanceFunc = function: TALGraphicThreadPool;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALGraphicThreadPool read GetInstance;
    class function HasInstance: Boolean; inline;
  public
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                const AContext: Tobject; // Context will be free by the worker thread
                const APriority: Int64;
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); override;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                const AContext: Tobject; // Context will be free by the worker thread
                const APriority: Int64;
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); override;
  end;

implementation

uses
  system.math,
  {$IF defined(ALSkiaAvailable)}
  System.Skia,
  FMX.Skia,
  fmx.Skia.Canvas,
  {$ENDIF}
  {$IFDEF ALGpuCanvas}
  FMX.Canvas.GPU,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Hardware,
  Androidapi.Helpers,
  Androidapi.JNI.Media,
  Androidapi.Bitmap,
  Androidapi.Gles2,
  FMX.Context.GLES,
  Alcinoe.AndroidApi.Common,
  Alcinoe.Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IF defined(IOS)}
  Fmx.Utils,
  FMX.Context.GLES,
  iOSapi.OpenGLES,
  iOSapi.Foundation,
  iOSapi.CoreImage,
  iOSapi.Helpers,
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Alcinoe.iOSapi.ImageIO,
  Alcinoe.iOSapi.CoreImage,
  Alcinoe.iOSapi.CoreFoundation,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Fmx.Utils,
  Macapi.Foundation,
  Macapi.QuartzCore,
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Macapi.ImageIO,
  Alcinoe.Macapi.QuartzCore,
  {$ENDIF}
  FMX.Effects,
  System.UIConsts,
  Alcinoe.http.client,
  Alcinoe.FMX.Types3D;

{********************}
{$IF defined(ANDROID)}
function ALCreateTextureFromJBitmap(const aBitmap: Jbitmap): TTexture;
begin
  result := TALTexture.Create;
  try
    ALUpdateTextureFromJBitmap(aBitmap, Result);
  except
    ALFreeAndNil(result);
    raise;
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
procedure ALUpdateTextureFromJBitmap(const aBitmap: Jbitmap; const ATexture: TTexture);
begin
  if ATexture is TALTexture then begin
    TALTexture(ATexture).Assign(aBitmap);
    // Without calling glFinish on devices like the Samsung S24,
    // if a texture is created in a background thread and later used
    // on the main thread, the texture may not be available.
    if (ALIsDefaultContextOpenGL) and (TThread.Current.ThreadID <> MainThreadID) then
      glFinish;
  end
  else
    raise Exception.Create('Only TALTexture is supported currently');
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALCreateTextureFromCGContextRef(const aCGContextRef: CGContextRef): TTexture;
begin
  result := TALTexture.Create;
  try
    result.Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
    result.SetSize(CGBitmapContextGetWidth(aCGContextRef), CGBitmapContextGetHeight(aCGContextRef));
    ALUpdateTextureFromCGContextRef(aCGContextRef, result);
  except
    ALFreeAndNil(result);
    raise;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALCreateTBitmapFromCGContextRef(const aCGContextRef: CGContextRef): TBitmap;
begin
  Result := TBitmap.Create(CGBitmapContextGetWidth(aCGContextRef), CGBitmapContextGetHeight(aCGContextRef));
  try
    ALUpdateTBitmapFromCGContextRef(aCGContextRef, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
procedure ALUpdateTextureFromCGContextRef(const aCGContextRef: CGContextRef; const ATexture: TTexture);
begin
  ATexture.UpdateTexture(CGBitmapContextGetData(aCGContextRef), CGBitmapContextGetBytesPerRow(aCGContextRef));
  {$IF defined(IOS)}
  // Without calling glFinish on devices like the Samsung S24 (uncertain about iPhone behavior),
  // a texture created in a background thread may not be available when used later on the main thread.
  if (ALIsDefaultContextOpenGL) and (TThread.Current.ThreadID <> MainThreadID) then
    glFinish;
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
procedure ALUpdateTBitmapFromCGContextRef(const aCGContextRef: CGContextRef; const ABitmap: TBitmap);
begin
  var LBitmapData: TBitmapData;
  if ABitmap.Map(TMapAccess.Write, LBitmapData) then
  try
    ALMove(CGBitmapContextGetData(aCGContextRef)^, LBitmapData.Data^, CGBitmapContextGetBytesPerRow(aCGContextRef) * cardinal(ABitmap.Height));
  finally
    ABitmap.Unmap(LBitmapData);
  end;
end;
{$ENDIF}

{******************}
{$IFDEF ALGpuCanvas}
function ALCreateTextureFromTBitmapSurface(const aBitmapSurface: TbitmapSurface): TTexture;
begin
  result := TALTexture.Create;
  try
    ALUpdateTextureFromTBitmapSurface(aBitmapSurface, Result);
  except
    ALFreeAndNil(result);
    raise;
  end;
end;
{$ENDIF}

{******************}
{$IFDEF ALGpuCanvas}
function ALCreateTextureFromTBitmap(const aBitmap: Tbitmap): TTexture;
begin
  Result := TalTexture.Create;
  try
    ALUpdateTextureFromTBitmap(aBitmap, Result);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;
{$ENDIF}

{******************}
{$IFDEF ALGpuCanvas}
procedure ALUpdateTextureFromTBitmapSurface(const aBitmapSurface: TbitmapSurface; const ATexture: TTexture);
begin
  ATexture.Assign(aBitmapSurface);
  {$IF defined(IOS) or defined(ANDROID)}
  // Without calling glFinish on devices like the Samsung S24,
  // if a texture is created in a background thread and later used
  // on the main thread, the texture may not be available.
  if (ALIsDefaultContextOpenGL) and (TThread.Current.ThreadID <> MainThreadID) then
    glFinish;
  {$ENDIF}
end;
{$ENDIF}

{******************}
{$IFDEF ALGpuCanvas}
procedure ALUpdateTextureFromTBitmap(const aBitmap: Tbitmap; const ATexture: TTexture);
begin
  ATexture.assign(aBitmap);
  {$IF defined(IOS) or defined(ANDROID)}
  // Without calling glFinish on devices like the Samsung S24,
  // if a texture is created in a background thread and later used
  // on the main thread, the texture may not be available.
  if (ALIsDefaultContextOpenGL) and (TThread.Current.ThreadID <> MainThreadID) then
    glFinish;
  {$ENDIF}
end;
{$ENDIF}

var
  ALDefaultContextIsOpenGL: Boolean;
  ALIsDefaultContextOpenGLDetermined: Boolean;

{*****************************************}
function ALIsDefaultContextOpenGL: Boolean;
begin
  if not ALIsDefaultContextOpenGLDetermined then begin
    {$IF defined(IOS) or defined(ANDROID)}
    ALDefaultContextIsOpenGL := TContextManager.DefaultContextClass.InheritsFrom(TCustomContextOpenGL);
    {$ELSE}
    ALDefaultContextIsOpenGL := False;
    {$ENDIF}
    ALIsDefaultContextOpenGLDetermined := True;
  end;
  result := ALDefaultContextIsOpenGL;
end;

{***********************************************************}
function ALGetImageDimensions(const aStream: TStream): TSize;
begin

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  var LSavedPosition := AStream.Position;
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      result := TSize.Create(LBitmap.getWidth, LBitmap.getHeight);
    finally
      LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
    AStream.Position := LSavedPosition;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF defined(ALAppleOS)}
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      var LSavedPosition := AStream.Position;
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
      AStream.Position := LSavedPosition;
    end;
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        Result := TSize.Create(
                    ALOSImageGetWidth(LImage),
                    ALOSImageGetHeight(LImage));
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF defined(MSWINDOWS)}
  var LSavedPosition := AStream.Position;
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    Result := TSize.Create(LBitmap.Width, LBitmap.height);
  finally
    AlFreeAndNil(LBitmap);
    AStream.Position := LSavedPosition;
  end;
  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************************************************}
function AlGetExifOrientationInfo(const aFilename: String): TalExifOrientationInfo;
begin

  // You can download sample images at : https://github.com/recurser/exif-orientation-examples

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  var LExifInterface := TJExifInterface.javaclass.init(StringToJString(aFilename));
  var LOrientation := LExifInterface.getAttributeInt(TJExifInterface.JavaClass.TAG_ORIENTATION, TJExifInterface.JavaClass.ORIENTATION_NORMAL);
  if LOrientation = TJExifInterface.JavaClass.ORIENTATION_FLIP_HORIZONTAL then result := TalExifOrientationInfo.FLIP_HORIZONTAL
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_FLIP_VERTICAL then result := TalExifOrientationInfo.FLIP_VERTICAL
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_NORMAL then result := TalExifOrientationInfo.NORMAL
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_180 then result := TalExifOrientationInfo.ROTATE_180
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_270 then result := TalExifOrientationInfo.ROTATE_270
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_90 then result := TalExifOrientationInfo.ROTATE_90
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_TRANSPOSE then result := TalExifOrientationInfo.TRANSPOSE
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_TRANSVERSE then result := TalExifOrientationInfo.TRANSVERSE
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_UNDEFINED then result := TalExifOrientationInfo.UNDEFINED
  else result := TalExifOrientationInfo.UNDEFINED;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF defined(ALAppleOS)}
  result := TalExifOrientationInfo.UNDEFINED;
  var LPath := CFStringCreateWithCString(nil{alloc}, MarshaledAString(UTF8Encode(AFileName)){cStr}, kCFStringEncodingUTF8{encoding});
  if LPath = nil then raise Exception.Create('Failed to create CFString from file name');
  try
    var LUrl := CFURLCreateWithFileSystemPath(nil{allocator}, LPath{filePath}, kCFURLPOSIXPathStyle{pathStyle}, False{isDirectory});
    if LUrl = nil then raise Exception.Create('Failed to create CFURL from file path');
    try
      var LImgSourceRef := CGImageSourceCreateWithURL(LUrl{url}, nil{options});
      if LImgSourceRef = nil then raise Exception.Create('Failed to create CGImageSource from URL');
      try
        var LDictionaryRef := CGImageSourceCopyPropertiesAtIndex(LImgSourceRef{isrc}, 0{index}, nil{options});
        if LDictionaryRef = nil then raise Exception.Create('Failed to retrieve image properties');
        try
          var LOrientation := TNSNumber.Wrap(CFDictionaryGetValue(LDictionaryRef, kCGImagePropertyOrientation));
          if LOrientation <> nil then begin

            //typedef CF_ENUM(uint32_t, CGImagePropertyOrientation) {
            //    kCGImagePropertyOrientationUp = 1,        // 0th row at top,    0th column on left   - default orientation
            //    kCGImagePropertyOrientationUpMirrored,    // 0th row at top,    0th column on right  - horizontal flip
            //    kCGImagePropertyOrientationDown,          // 0th row at bottom, 0th column on right  - 180 deg rotation
            //    kCGImagePropertyOrientationDownMirrored,  // 0th row at bottom, 0th column on left   - vertical flip
            //    kCGImagePropertyOrientationLeftMirrored,  // 0th row on left,   0th column at top
            //    kCGImagePropertyOrientationRight,         // 0th row on right,  0th column at top    - 90 deg CW
            //    kCGImagePropertyOrientationRightMirrored, // 0th row on right,  0th column on bottom
            //    kCGImagePropertyOrientationLeft           // 0th row on left,   0th column at bottom - 90 deg CCW
            //};

            case LOrientation.integerValue of

              //Top, left (UIImageOrientationUp)
              1: result := TalExifOrientationInfo.NORMAL;

              //Top, right (UIImageOrientationUpMirrored)
              2: result := TalExifOrientationInfo.FLIP_HORIZONTAL;

              //Bottom, right (UIImageOrientationDown)
              3: result := TalExifOrientationInfo.ROTATE_180;

              //Bottom, left (UIImageOrientationDownMirrored)
              4: result := TalExifOrientationInfo.FLIP_VERTICAL;

              //Left, top (UIImageOrientationLeftMirrored)
              5: result := TalExifOrientationInfo.transpose;

              //Right, top (UIImageOrientationRight)
              6: result := TalExifOrientationInfo.ROTATE_90;

              //Right, bottom (UIImageOrientationRightMirrored)
              7: result := TalExifOrientationInfo.transverse;

              //Left, bottom (UIImageOrientationLeft)
              8: result := TalExifOrientationInfo.ROTATE_270;

            end;
          end;
        finally
          CGImageRelease(LDictionaryRef);
        end;
      finally
        CFRelease(LImgSourceRef);
      end;
    finally
      CFRelease(LUrl);
    end;
  finally
    CFRelease(LPath);
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF defined(MSWINDOWS)}
  var LImage := TGPImage.Create(AFileName);
  try
    var LPropSize := LImage.GetPropertyItemSize(PropertyTagOrientation);
    if LPropSize > 0 then begin
      var LPropItem: PPropertyItem;
      GetMem(LPropItem, LPropSize);
      try
        if LImage.GetPropertyItem(PropertyTagOrientation, LPropSize, LPropItem) = Ok then begin
          var LValue := PWord(LPropItem^.Value)^;
          case LValue of
            1: result := TalExifOrientationInfo.NORMAL;
            2: result := TalExifOrientationInfo.FLIP_HORIZONTAL;
            3: result := TalExifOrientationInfo.ROTATE_180;
            4: result := TalExifOrientationInfo.FLIP_VERTICAL;
            5: result := TalExifOrientationInfo.TRANSPOSE;
            6: result := TalExifOrientationInfo.ROTATE_90;
            7: result := TalExifOrientationInfo.TRANSVERSE;
            8: result := TalExifOrientationInfo.ROTATE_270;
            else result := TalExifOrientationInfo.UNDEFINED;
          end;
        end
        else
          result := TalExifOrientationInfo.UNDEFINED;
      finally
        FreeMem(LPropItem);
      end;
    end
    else
      result := TalExifOrientationInfo.UNDEFINED;
  finally
    ALFreeAndNil(LImage);
  end;
  {$ENDIF}
  {$ENDREGION}

end;

{********************************************************************************}
function AlGetExifOrientationInfo(const aStream: TStream): TalExifOrientationInfo;
begin

  // You can download sample images at : https://github.com/recurser/exif-orientation-examples

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  var LSavedPosition := AStream.Position;
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LByteArrayInputStream: JByteArrayInputStream := TJByteArrayInputStream.JavaClass.init(LArray);
    try
      var LExifInterface := TJExifInterface.javaclass.init(LByteArrayInputStream);
      var LOrientation := LExifInterface.getAttributeInt(TJExifInterface.JavaClass.TAG_ORIENTATION, TJExifInterface.JavaClass.ORIENTATION_NORMAL);
      if LOrientation = TJExifInterface.JavaClass.ORIENTATION_FLIP_HORIZONTAL then result := TalExifOrientationInfo.FLIP_HORIZONTAL
      else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_FLIP_VERTICAL then result := TalExifOrientationInfo.FLIP_VERTICAL
      else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_NORMAL then result := TalExifOrientationInfo.NORMAL
      else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_180 then result := TalExifOrientationInfo.ROTATE_180
      else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_270 then result := TalExifOrientationInfo.ROTATE_270
      else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_90 then result := TalExifOrientationInfo.ROTATE_90
      else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_TRANSPOSE then result := TalExifOrientationInfo.TRANSPOSE
      else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_TRANSVERSE then result := TalExifOrientationInfo.TRANSVERSE
      else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_UNDEFINED then result := TalExifOrientationInfo.UNDEFINED
      else result := TalExifOrientationInfo.UNDEFINED;
    finally
      LByteArrayInputStream.close;
      LByteArrayInputStream := nil;
    end;
  finally
    ALfreeandNil(LArray);
    AStream.Position := LSavedPosition;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF defined(ALAppleOS)}
  result := TalExifOrientationInfo.UNDEFINED;
  var LBuffer: Pointer := nil;
  var LLength: Int64 := 0;
  var LMemoryStream: TCustomMemoryStream := nil;
  if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LLength := AStream.Size;
  end
  else LMemoryStream := TMemoryStream.Create;
  try
    if LMemoryStream <> nil then begin
      var LSavedPosition := AStream.Position;
      LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      LBuffer := LMemoryStream.Memory;
      LLength := LMemoryStream.Size;
      AStream.Position := LSavedPosition;
    end;
    var LDataRef := CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, LBuffer, LLength, kCFAllocatorNull);
    if LDataRef = nil then raise Exception.Create('Failed to create CFDataRef from given stream');
    try
      var LImgSourceRef := CGImageSourceCreateWithData(LDataRef{CFDataRef}, nil{options});
      if LImgSourceRef = nil then raise Exception.Create('Failed to create CGImageSource from CFDataRef');
      try
        var LDictionaryRef := CGImageSourceCopyPropertiesAtIndex(LImgSourceRef{isrc}, 0{index}, nil{options});
        if LDictionaryRef = nil then raise Exception.Create('Failed to retrieve image properties');
        try
          var LOrientation := TNSNumber.Wrap(CFDictionaryGetValue(LDictionaryRef, kCGImagePropertyOrientation));
          if LOrientation <> nil then begin

            //typedef CF_ENUM(uint32_t, CGImagePropertyOrientation) {
            //    kCGImagePropertyOrientationUp = 1,        // 0th row at top,    0th column on left   - default orientation
            //    kCGImagePropertyOrientationUpMirrored,    // 0th row at top,    0th column on right  - horizontal flip
            //    kCGImagePropertyOrientationDown,          // 0th row at bottom, 0th column on right  - 180 deg rotation
            //    kCGImagePropertyOrientationDownMirrored,  // 0th row at bottom, 0th column on left   - vertical flip
            //    kCGImagePropertyOrientationLeftMirrored,  // 0th row on left,   0th column at top
            //    kCGImagePropertyOrientationRight,         // 0th row on right,  0th column at top    - 90 deg CW
            //    kCGImagePropertyOrientationRightMirrored, // 0th row on right,  0th column on bottom
            //    kCGImagePropertyOrientationLeft           // 0th row on left,   0th column at bottom - 90 deg CCW
            //};

            case LOrientation.integerValue of

              //Top, left (UIImageOrientationUp)
              1: result := TalExifOrientationInfo.NORMAL;

              //Top, right (UIImageOrientationUpMirrored)
              2: result := TalExifOrientationInfo.FLIP_HORIZONTAL;

              //Bottom, right (UIImageOrientationDown)
              3: result := TalExifOrientationInfo.ROTATE_180;

              //Bottom, left (UIImageOrientationDownMirrored)
              4: result := TalExifOrientationInfo.FLIP_VERTICAL;

              //Left, top (UIImageOrientationLeftMirrored)
              5: result := TalExifOrientationInfo.transpose;

              //Right, top (UIImageOrientationRight)
              6: result := TalExifOrientationInfo.ROTATE_90;

              //Right, bottom (UIImageOrientationRightMirrored)
              7: result := TalExifOrientationInfo.transverse;

              //Left, bottom (UIImageOrientationLeft)
              8: result := TalExifOrientationInfo.ROTATE_270;

            end;
          end;
        finally
          CGImageRelease(LDictionaryRef);
        end;
      finally
        CFRelease(LImgSourceRef);
      end;
    finally
      CFRelease(LDataRef);
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF defined(MSWINDOWS)}
  var LSavedPosition := AStream.Position;
  var LImage := TGPImage.Create(TStreamAdapter.Create(aStream));
  try
    var LPropSize := LImage.GetPropertyItemSize(PropertyTagOrientation);
    if LPropSize > 0 then begin
      var LPropItem: PPropertyItem;
      GetMem(LPropItem, LPropSize);
      try
        if LImage.GetPropertyItem(PropertyTagOrientation, LPropSize, LPropItem) = Ok then begin
          var LValue := PWord(LPropItem^.Value)^;
          case LValue of
            1: result := TalExifOrientationInfo.NORMAL;
            2: result := TalExifOrientationInfo.FLIP_HORIZONTAL;
            3: result := TalExifOrientationInfo.ROTATE_180;
            4: result := TalExifOrientationInfo.FLIP_VERTICAL;
            5: result := TalExifOrientationInfo.TRANSPOSE;
            6: result := TalExifOrientationInfo.ROTATE_90;
            7: result := TalExifOrientationInfo.TRANSVERSE;
            8: result := TalExifOrientationInfo.ROTATE_270;
            else result := TalExifOrientationInfo.UNDEFINED;
          end;
        end
        else
          result := TalExifOrientationInfo.UNDEFINED;
      finally
        FreeMem(LPropItem);
      end;
    end
    else
      result := TalExifOrientationInfo.UNDEFINED;
  finally
    ALFreeAndNil(LImage);
    AStream.Position := LSavedPosition;
  end;
  {$ENDIF}
  {$ENDREGION}

end;

{*************************************************************************************************}
function AlGetImageSignature(const aStream: TStream; const aSignatureLength: integer = 12): Tbytes;
begin
  var LSavedPosition := AStream.Position;
  SetLength(result, aSignatureLength);
  aStream.ReadBuffer(result[0], min(length(result),aStream.Size));
  if aStream.Size < length(Result) then
    for var I := aStream.Size to High(result) do
      result[i] := $00;
  AStream.Position := LSavedPosition;
end;

{**************************************************************************************************}
function AlGetImageSignature(const aFileName: string; const aSignatureLength: integer = 12): Tbytes;
begin
  var LFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    result := AlGetImageSignature(LFileStream, aSignatureLength);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{***************************************************}
// https://www.garykessler.net/library/file_sigs.html
// https://en.wikipedia.org/wiki/List_of_file_signatures
// https://github.com/strukturag/libheif/issues/83
// https://nokiatech.github.io/heif/technical.html
function AlDetectImageExtension(const aStream: Tstream): String;
begin

  var LFirstBytes := AlGetImageSignature(aStream);
  if length(LFirstBytes) < 12 then exit('');

  if (LFirstBytes[0] = $FF) and
     (LFirstBytes[1] = $D8) then result := 'jpg'  // ÿØ
  else if (LFirstBytes[0] = $89) and
          (LFirstBytes[1] = $50) and
          (LFirstBytes[2] = $4E) and
          (LFirstBytes[3] = $47) and
          (LFirstBytes[4] = $0D) and
          (LFirstBytes[5] = $0A) and
          (LFirstBytes[6] = $1A ) and
          (LFirstBytes[7] = $0A) then result := 'png' // .PNG....
  else if (LFirstBytes[0] = $47) and
          (LFirstBytes[1] = $49) and
          (LFirstBytes[2] = $46) then result := 'gif' // GIF
  else if (LFirstBytes[0] = $42) and
          (LFirstBytes[1] = $4D) then result := 'bmp' // BM
  else if (LFirstBytes[0] = $00) and
          (LFirstBytes[1] = $00) and
          (LFirstBytes[2] = $00) and
          //(aFirstBytes[3] = $18) => I encounter $18, $20, $24 and I don't know the purpose of this byte so ignore it
          (LFirstBytes[4] = $66) and
          (LFirstBytes[5] = $74) and
          (LFirstBytes[6] = $79) and
          (LFirstBytes[7] = $70) and
          (LFirstBytes[8] = $68) and
          (LFirstBytes[9] = $65) and
          (LFirstBytes[10] = $69) and
          ((LFirstBytes[11] = $63{c}) or
           (LFirstBytes[11] = $78{x})) then result := 'heic' // ftypheic or ftypheix....
  else if (LFirstBytes[0] = $00) and
          (LFirstBytes[1] = $00) and
          (LFirstBytes[2] = $00) and
          //(aFirstBytes[3] = $18) => I encounter $18, $20, $24 and I don't know the purpose of this byte so ignore it
          (LFirstBytes[4] = $66) and
          (LFirstBytes[5] = $74) and
          (LFirstBytes[6] = $79) and
          (LFirstBytes[7] = $70) and
          (LFirstBytes[8] = $6D) and
          (LFirstBytes[9] = $69) and
          (LFirstBytes[10] = $66) and
          (LFirstBytes[11] = $31) then result := 'heif' // ftypmif1....
  else if (LFirstBytes[0] = $52) and
          (LFirstBytes[1] = $49) and
          (LFirstBytes[2] = $46) and
          (LFirstBytes[3] = $46) and
          //(aFirstBytes[4] = $3c) and //
          //(aFirstBytes[5] = $db) and // => The size of the file
          //(aFirstBytes[6] = $00) and // => in bytes
          //(aFirstBytes[7] = $00) and //
          (LFirstBytes[8] = $57) and
          (LFirstBytes[9] = $45) and
          (LFirstBytes[10] = $42) and
          (LFirstBytes[11] = $50) then result := 'webp' // RIFF....WEBP
  else result := '';

end;

{***************************************************************}
function AlDetectImageExtension(const aFileName: string): String;
begin
  var LFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    result := AlDetectImageExtension(LFileStream);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{****************************************************************************************}
function ALModulateColor(const SrcColor: TAlphaColor; const Opacity: Single): TAlphaColor;
begin
  if Opacity < 1 then
  begin
    var A := TAlphaColorRec(SrcColor).A / $FF;
    TAlphaColorRec(Result).R := Round(TAlphaColorRec(SrcColor).R * Opacity * A);
    TAlphaColorRec(Result).G := Round(TAlphaColorRec(SrcColor).G * Opacity * A);
    TAlphaColorRec(Result).B := Round(TAlphaColorRec(SrcColor).B * Opacity * A);
    TAlphaColorRec(Result).A := Round(TAlphaColorRec(SrcColor).A * Opacity);
  end
  else if (TAlphaColorRec(SrcColor).A < $FF) then
    Result := PremultiplyAlpha(SrcColor)
  else
    Result := SrcColor;
end;

{*******************************************************************************}
function ALBlendColor(const ABaseColor, AOverlayColor: TAlphaColor): TAlphaColor;
begin
  var LBaseColorRec := TAlphaColorRec(ABaseColor);
  var LOverlayColorRec := TAlphaColorRec(AOverlayColor);
  //--
  var LBaseAlpha: Single := LBaseColorRec.A / 255;
  var LOverlayAlpha: Single := LOverlayColorRec.A / 255;
  var LOutAlpha: Single := LOverlayAlpha + (LBaseAlpha * (1 - LOverlayAlpha));
  //--
  if LOutAlpha > 0 then begin
    TAlphaColorRec(Result).R := Round(((LOverlayColorRec.R * LOverlayAlpha) + (LBaseColorRec.R * LBaseAlpha * (1 - LOverlayAlpha))) / LOutAlpha);
    TAlphaColorRec(Result).G := Round(((LOverlayColorRec.G * LOverlayAlpha) + (LBaseColorRec.G * LBaseAlpha * (1 - LOverlayAlpha))) / LOutAlpha);
    TAlphaColorRec(Result).B := Round(((LOverlayColorRec.B * LOverlayAlpha) + (LBaseColorRec.B * LBaseAlpha * (1 - LOverlayAlpha))) / LOutAlpha);
    TAlphaColorRec(Result).A := Round(LOutAlpha * 255);
  end
  else
    Result := TAlphacolors.Null;
end;

{**************************************************************************************************************}
function ALBlendColor(const ABaseColor, AOverlayColor: TAlphaColor; const AOverlayOpacity: Single): TAlphaColor;
begin
  var LOverlayColorRec := TAlphaColorRec(AOverlayColor);
  LOverlayColorRec.A := round(LOverlayColorRec.A * Max(Min(AOverlayOpacity, 1), 0));
  result := ALBlendColor(ABaseColor, LOverlayColorRec.Color);
end;

{***************************************************************************************}
function ALSetColorAlpha(const AColor: TAlphaColor; const AOpacity: Single): TAlphaColor;
begin
  TAlphaColorRec(Result).R := TAlphaColorRec(AColor).R;
  TAlphaColorRec(Result).G := TAlphaColorRec(AColor).G;
  TAlphaColorRec(Result).B := TAlphaColorRec(AColor).B;
  TAlphaColorRec(Result).A := Round(Max(Min(AOpacity, 1), 0) * 255);
end;

{********************************************************************************************}
function ALMultiplyColorAlpha(const AColor: TAlphaColor; const AOpacity: Single): TAlphaColor;
begin
  TAlphaColorRec(Result).R := TAlphaColorRec(AColor).R;
  TAlphaColorRec(Result).G := TAlphaColorRec(AColor).G;
  TAlphaColorRec(Result).B := TAlphaColorRec(AColor).B;
  TAlphaColorRec(Result).A := Round(TAlphaColorRec(AColor).A * Max(Min(AOpacity, 1), 0));
end;

{*************************************************************}
function ALConvertRadiusToSigma(const ARadius: Single): Single;
begin
  //Taken from SkBlurMask.h
  if ARadius > 0 then result := 0.57735 * ARadius + 0.5
  else result := 0;
end;

{************************************************************}
function ALConvertSigmaToRadius(const ASigma: Single): Single;
begin
  //Taken from SkBlurMask.h
  if ASigma > 0.5 then result := (ASigma - 0.5) / 0.57735
  else Result := 0;
end;

{******************************************************************************************************************}
procedure ALNormalizeAndScaleRadii(var AXRadius, AYRadius: Single; const AScale: Single; const AScaledRect: TRectF);
begin
  var LScaledXRadius: Single := AXRadius;
  var LScaledYRadius: Single := AYRadius;
  if LScaledXRadius < 0 then LScaledXRadius := (AScaledRect.Width/100)*abs(LScaledXRadius)
  else LScaledXRadius := LScaledXRadius * AScale;
  if LScaledYRadius < 0 then LScaledYRadius := (AScaledRect.Height/100)*abs(LScaledYRadius)
  else LScaledYRadius := LScaledYRadius * AScale;
  if (AXRadius < 0) or (AYRadius < 0) then begin
    var LMaxRadius := Min(AScaledRect.Width / 2, AScaledRect.Height / 2);
    LScaledXRadius := min(LScaledXRadius, LMaxRadius);
    LScaledYRadius := min(LScaledYRadius, LMaxRadius);
  end;
  AXRadius := Min(AScaledRect.Width / 2, LScaledXRadius);
  AYRadius := Min(AScaledRect.Height / 2, LScaledYRadius);
end;

{***********************************************************}
function ALGetShadowWidth(const AShadowBlur: Single): Single;
begin
  // To be on the safe side
  Result := AShadowBlur * 2;
end;

{*******************************************************************************************************************************************************************************}
procedure ALGetLinearGradientCoordinates(const ASize: TSizeF; const AAngle: Single; out AStartPoint: TPointF; out AEndPoint: TPointF; const ACssAngleConvention: Boolean = True);
begin

  // handle edge cases like: -1235, ...
  var LNormalizedAngle: Single;
  if (ACssAngleConvention) then LNormalizedAngle := fmod(fmod((90 - AAngle), 360) + 360, 360)
  else LNormalizedAngle := fmod(fmod(AAngle, 360) + 360, 360);

  var LAngleInRadians: Single := DegToRad(LNormalizedAngle);

  var LDiagonal: Double := sqrt(Power(ASize.width,2) + Power(ASize.height,2));
  var LAngleBetweenDiagonalAndWidth: Double := ArcCos(ASize.width / LDiagonal);
  var LAngleBetweenDiagonalAndGradientLine: Double;
  if ((LNormalizedAngle > 90) and (LNormalizedAngle < 180)) or
     ((LNormalizedAngle > 270) and (LNormalizedAngle < 360)) then LAngleBetweenDiagonalAndGradientLine := PI - LAngleInRadians - LAngleBetweenDiagonalAndWidth
  else LAngleBetweenDiagonalAndGradientLine := LAngleInRadians - LAngleBetweenDiagonalAndWidth;

  var LHalfGradientLine: Double := abs(cos(LAngleBetweenDiagonalAndGradientLine) * LDiagonal) / 2;

  var LHorizontalOffset: Double := LHalfGradientLine * cos(LAngleInRadians);
  var LVerticalOffset: Double := LHalfGradientLine * sin(LAngleInRadians);

  AStartPoint := TPointF.create((ASize.Width / 2) - LHorizontalOffset, (ASize.Height / 2) + LVerticalOffset);
  AEndPoint := TPointF.create((ASize.Width / 2) + LHorizontalOffset, (ASize.Height / 2) - LVerticalOffset);

end;

{*****************************}
function ALGetShapeSurfaceRect(
           const ARect: TRectF;
           const AFillColor: TAlphaColor;
           const AFillGradientColors: TArray<TAlphaColor>;
           const AFillResourceName: String;
           const AFillResourceStream: TStream;
           Const AFillBackgroundMarginsRect: TRectF;
           Const AFillImageMarginsRect: TRectF;
           const AStateLayerOpacity: Single;
           const AStateLayerColor: TAlphaColor;
           const AStateLayerUseContentColor: Boolean;
           Const AStateLayerMarginsRect: TRectF;
           const AShadowColor: TAlphaColor;
           const AShadowBlur: Single;
           const AShadowOffsetX: Single;
           const AShadowOffsetY: Single): TRectF; overload;
begin
  Result := ARect;
  //--
  if (AFillColor <> TalphaColorRec.Null) or
     (length(AFillGradientColors) > 0) then begin
    var LBackgroundRect := ARect;
    LBackgroundRect.Inflate(-AFillBackgroundMarginsRect.Left, -AFillBackgroundMarginsRect.Top, -AFillBackgroundMarginsRect.Right, -AFillBackgroundMarginsRect.Bottom);
    Result := TRectF.Union(LBackgroundRect, Result);
  end;
  //--
  if (AStateLayerColor <> TalphaColorRec.Null) or (AStateLayerUseContentColor) and
     (CompareValue(AStateLayerOpacity, 0, TEpsilon.Scale) > 0) then begin
    var LStateLayerRect := ARect;
    LStateLayerRect.Inflate(-AStateLayerMarginsRect.Left, -AStateLayerMarginsRect.Top, -AStateLayerMarginsRect.Right, -AStateLayerMarginsRect.Bottom);
    Result := TRectF.Union(LStateLayerRect, Result);
  end;
  //--
  if (AFillResourceName <> '') or (AFillResourceStream <> nil) then begin
    var LImageRect := ARect;
    LImageRect.Inflate(-AFillImageMarginsRect.Left, -AFillImageMarginsRect.Top, -AFillImageMarginsRect.Right, -AFillImageMarginsRect.Bottom);
    Result := TRectF.Union(LImageRect, Result);
  end;
  //--
  if (AShadowColor <> TalphaColorRec.Null) and
     (CompareValue(AShadowBlur, 0, TEpsilon.position) > 0) then begin
    var LShadowRect := Result;
    var LShadowWidth := ALGetShadowWidth(AShadowBlur);
    LShadowRect.Inflate(LShadowWidth, LShadowWidth);
    LShadowRect.Offset(AShadowOffsetX, AShadowOffsetY);
    Result := TRectF.Union(LShadowRect, Result);
  end;
end;

{*****************************}
function ALGetShapeSurfaceRect(
           const ARect: TrectF;
           const AFill: TALBrush;
           const AFillResourceStream: TStream;
           const AStateLayer: TALStateLayer;
           const AShadow: TALShadow): TRectF;
begin
  // AFill
  var LFillColor: TAlphaColor;
  var LFillGradientColors: TArray<TAlphaColor>;
  var LFillResourceName: String;
  var LFillBackgroundMarginsRect: TRectF;
  var LFillImageMarginsRect: TRectF;
  if AFill <> nil then begin
    LFillColor := AFill.Color;
    LFillGradientColors := Afill.Gradient.Colors;
    LFillResourceName := AFill.ResourceName;
    LFillBackgroundMarginsRect := AFill.BackgroundMargins.Rect;
    LFillImageMarginsRect := AFill.ImageMargins.Rect;
  end
  else begin
    LFillColor := TAlphaColors.Null;
    LFillGradientColors := [];
    LFillResourceName := '';
    LFillBackgroundMarginsRect := TRectF.Empty;
    LFillImageMarginsRect := TRectF.Empty;
  end;

  // AStateLayer
  var LStateLayerOpacity: Single;
  var LStateLayerColor: TAlphaColor;
  var LStateLayerUseContentColor: Boolean;
  var LStateLayerMarginsRect: TRectF;
  if AStateLayer <> nil then begin
    LStateLayerOpacity := AStateLayer.Opacity;
    LStateLayerColor := AStateLayer.Color;
    LStateLayerUseContentColor := AStateLayer.UseContentColor;
    LStateLayerMarginsRect := AStateLayer.Margins.Rect;
  end
  else begin
    LStateLayerOpacity := 0;
    LStateLayerColor := TAlphaColors.Null;
    LStateLayerUseContentColor := False;
    LStateLayerMarginsRect := TRectF.Empty;
  end;

  // AShadow
  var LShadowColor: TAlphaColor;
  var LShadowBlur: Single;
  var LShadowOffsetX: Single;
  var LShadowOffsetY: Single;
  if AShadow <> nil then begin
    LShadowColor := AShadow.Color;
    LShadowBlur := AShadow.Blur;
    LShadowOffsetX := AShadow.OffsetX;
    LShadowOffsetY := AShadow.OffsetY;
  end
  else begin
    LShadowColor := TalphaColors.Null;
    LShadowBlur := 0;
    LShadowOffsetX := 0;
    LShadowOffsetY := 0;
  end;

  Result := ALGetShapeSurfaceRect(
              ARect, // const ARect: TrectF;
              LFillColor, // const AFillColor: TAlphaColor;
              LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
              LFillResourceName, // const AFillResourceName: String;
              AFillResourceStream, // const AFillResourceStream: TStream;
              LFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
              LFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
              LStateLayerOpacity, // const AStateLayerOpacity: Single;
              LStateLayerColor, // const AStateLayerColor: TAlphaColor;
              LStateLayerUseContentColor, // const AStateLayerUseContentColor: Boolean;
              LStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
              LShadowColor, // const AShadowColor: TAlphaColor;
              LShadowBlur, // const AShadowBlur: Single;
              LShadowOffsetX, // const AShadowOffsetX: Single;
              LShadowOffsetY); // const AShadowOffsetY: Single)
end;

{*********************************************}
function ALCreateEmptyDrawable1x1: TALDrawable;
begin
  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    1, // const w: integer;
    1, // const h: integer)
    false); // const AAddPixelForAlignment: Boolean = true
  try
    if ALCanvasBeginScene(LCanvas) then begin
      try
        ALClearCanvas(LCanvas, TAlphaColors.Null);
      finally
        ALCanvasEndScene(LCanvas)
      end;
    end;
    Result := ALCreateDrawableFromSurface(LSurface);
  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;
end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure ALInitControlRenderTargets(
            Const ARect: TrectF;
            var ARenderTargetSurface: TALSurface;
            var ARenderTargetCanvas: TALCanvas;
            var ARenderTargetDrawable: TALDrawable);
begin
  if (ALIsDrawableNull(ARenderTargetDrawable)) or
     (CompareValue(ALGetDrawableWidth(ARenderTargetDrawable), ARect.Width * ALGetScreenScale, TEpsilon.Position) < 0) or
     (CompareValue(ALGetDrawableHeight(ARenderTargetDrawable), ARect.Height * ALGetScreenScale, TEpsilon.Position) < 0) then begin
    var LSurfaceRect := ARect;
    LSurfaceRect.Width := LSurfaceRect.Width * 1.5{to be on the safe side};
    LSurfaceRect.height := LSurfaceRect.height * 1.5{to be on the safe side};
    ALFreeAndNilDrawable(ARenderTargetDrawable);
    ALFreeAndNilSurface(ARenderTargetSurface, ARenderTargetCanvas);
    ALCreateSurface(
      ARenderTargetSurface, // out ASurface: TALSurface;
      ARenderTargetCanvas, // out ACanvas: TALCanvas;
      ALGetScreenScale{Ascale},
      LSurfaceRect.width, // const w: Single;
      LSurfaceRect.height, // const h: Single);
      false); // const AAddPixelForAlignment: Boolean = true
    {$IF defined(ALSkiaCanvas)}
    Raise Exception.create('Error 71AD6B8B-AF32-468D-818A-168EFC96C368')
    {$ELSEIF defined(ALGpuCanvas)}
    ARenderTargetDrawable := TALTexture.Create;
    ARenderTargetDrawable.Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
    ARenderTargetDrawable.SetSize(ALCeil(LSurfaceRect.width * ALGetScreenScale, TEpsilon.Position), ALCeil(LSurfaceRect.height * ALGetScreenScale, TEpsilon.Position));
    ARenderTargetDrawable.PixelFormat := ALGetDefaultPixelFormat;
    {$ELSE}
    ARenderTargetDrawable := FMX.Graphics.TBitmap.Create(ALCeil(LSurfaceRect.width * ALGetScreenScale, TEpsilon.Position), ALCeil(LSurfaceRect.height * ALGetScreenScale, TEpsilon.Position));
    {$ENDIF};
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALDrawSkImage(
            const ACanvas: sk_canvas_t;
            const AImage: sk_image_t;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In AImage coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskImage: sk_image_t;
            const ACropCenter: TpointF; // Used only when AMaskImage is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);
begin

  // Init LCanvasMatrix/LCanvasScale
  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AAlignToPixel then ALExtractMatrixFromSkCanvas(ACanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;

  // Init LScaledDstRect
  var LScaledDstRect := ADstRect;
  LScaledDstRect.Top := LScaledDstRect.Top * AScale;
  LScaledDstRect.right := LScaledDstRect.right * AScale;
  LScaledDstRect.left := LScaledDstRect.left * AScale;
  LScaledDstRect.bottom := LScaledDstRect.bottom * AScale;
  if AAlignToPixel then
    LScaledDstRect := ALAlignToPixelRound(LScaledDstRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);

  // Init LScaledBlurRadius
  var LScaledBlurRadius: Single := ABlurRadius * AScale;

  // Init LScaledXRadius and LScaledYRadius
  var LScaledXRadius: Single := AXRadius;
  var LScaledYRadius: Single := AYRadius;
  ALNormalizeAndScaleRadii(LScaledXRadius, LScaledYRadius, AScale, LScaledDstRect);

  // Init LPaint
  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    // Init LRRect
    var LRRect: sk_rrect_t := 0;
    try

      // Init LBlender
      var LBlender: sk_blender_t := 0;
      try

        // Mask
        if AMaskImage <> 0 then begin
          var LMaskScrRect := TrectF.Create(0, 0, sk4d_image_get_width(AMaskImage), sk4d_image_get_Height(AMaskImage));
          var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
          sk4d_canvas_draw_image_rect(
            ACanvas, // self: sk_canvas_t;
            AMaskImage, // const image: sk_image_t;
            @LMaskScrRect, // const src: psk_rect_t;
            @LScaledDstRect,  // const dest: psk_rect_t;
            @LSamplingoptions, // const sampling: psk_samplingoptions_t;
            LPaint, // const paint: sk_paint_t;
            FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
          LBlender := ALSkCheckHandle(
                        sk4d_blender_make_mode(
                          sk_blendmode_t.SRC_IN_SK_BLENDMODE));
          sk4d_paint_set_blender(LPaint, LBlender);
        end

        // Oval
        else if SameValue(LScaledXRadius, LScaledDstRect.Width / 2, TEpsilon.position) and
                SameValue(LScaledYRadius, LScaledDstRect.Height / 2, TEpsilon.position) then begin
          LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
          sk4d_rrect_set_oval(
            LRRect, // self: sk_rrect_t;
            @LScaledDstRect); // const rect: psk_rect_t;
          sk4d_canvas_clip_rrect(
            ACanvas, // self: sk_canvas_t;
            LRRect, // const rrect: sk_rrect_t;
            sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
            true); // anti_alias: _bool);
        end

        // RoundRect
        else if (compareValue(LScaledXRadius, 0, TEpsilon.Position) > 0) and
                (compareValue(LScaledYRadius, 0, TEpsilon.position) > 0) then begin
          LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
          sk4d_rrect_set_rect3(
            LRRect, // self: sk_rrect_t;
            @LScaledDstRect, // const rect: psk_rect_t;
            LScaledXRadius, // radius_x,
            LScaledYRadius); // radius_y: float)
          sk4d_canvas_clip_rrect(
            ACanvas, // self: sk_canvas_t;
            LRRect, // const rrect: sk_rrect_t;
            sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
            true); // anti_alias: _bool);
        end

        // Rect
        else if (compareValue(LScaledBlurRadius, 0, Tepsilon.Vector) > 0) then begin
          // The issue with the blur effect is that at the edges, it uses transparent
          // pixels to calculate the color. To prevent this, we clip the rect and
          // later draw a slightly larger version of the image. This ensures that
          // the algorithm doesn't sample transparent pixels at the edges when
          // calculating the blur effect.
          LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
          sk4d_rrect_set_rect(
            LRRect, // self: sk_rrect_t;
            @LScaledDstRect); // const rect: psk_rect_t;
          sk4d_canvas_clip_rrect(
            ACanvas, // self: sk_canvas_t;
            LRRect, // const rrect: sk_rrect_t;
            sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
            true); // anti_alias: _bool);
        end;

        // Init LImageFilter
        var LImageFilter: sk_imagefilter_t := 0;
        try

          // Blur
          if compareValue(LScaledBlurRadius, 0, Tepsilon.Vector) > 0 then begin
            // The issue with the blur effect is that it samples transparent pixels at the edges,
            // which affects the color calculation. To mitigate this, we clip the rect and draw
            // a slightly enlarged version of the image. This helps ensure that the algorithm
            // avoids sampling transparent pixels at the edges when applying the blur effect.
            // However, this is not an ideal method to dilate the image; I would prefer to use
            // a mirroring approach, but I haven't found a way to implement it yet.
            var LDelta: Single := ALConvertRadiusToSigma(LScaledBlurRadius) * 2;
            LScaledDstRect.Inflate(LDelta,LDelta,LDelta,LDelta);
            LImageFilter := ALSkCheckHandle(
                              sk4d_imagefilter_make_blur(
                                ALConvertRadiusToSigma(LScaledBlurRadius), //sigma_x,
                                ALConvertRadiusToSigma(LScaledBlurRadius), //sigma_y: float;
                                sk_tilemode_t.CLAMP_SK_TILEMODE, //tile_mode: sk_tilemode_t;
                                0, //input: sk_imagefilter_t;
                                @LScaledDstRect));//const crop_rect: psk_rect_t
            sk4d_paint_set_Image_filter(LPaint, LImageFilter);
          end;

          var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
          sk4d_canvas_draw_image_rect(
            ACanvas, // self: sk_canvas_t;
            AImage, // const image: sk_image_t;
            @ASrcRect, // const src: psk_rect_t;
            @LScaledDstRect,  // const dest: psk_rect_t;
            @LSamplingoptions, // const sampling: psk_samplingoptions_t;
            LPaint, // const paint: sk_paint_t;
            FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

        finally
          if LImageFilter <> 0 then
            sk4d_refcnt_unref(LImageFilter);
        end;

      finally
        If LBlender <> 0 then
          sk4d_refcnt_unref(LBlender)
      end;

    finally
      if LRRect <> 0 then begin
        sk4d_canvas_restore(ACanvas);
        sk4d_rrect_destroy(LRRect);
      end;
    end;

  finally
    sk4d_paint_destroy(LPaint);
  end;

end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateSkSurfaceFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskImage: sk_image_t;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): sk_surface_t;
begin

  var LImage: sk_image_t;
  if AResourceStream <> nil then begin
    var LSkStream: sk_streamadapter_t := ALSkCheckHandle(sk4d_streamadapter_create(AResourceStream));
    try
      LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LSkStream));
    finally
      sk4d_streamadapter_destroy(LSkStream);
    end;
  end
  else if AResourceName <> '' then begin
    var LFileName := ALGetResourceFilename(AResourceName);
    if LFileName <> '' then LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(LFileName))))
    else begin
      var LResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
      try
        var LSkStream: sk_streamadapter_t := ALSkCheckHandle(sk4d_streamadapter_create(LResourceStream));
        try
          LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LSkStream));
        finally
          sk4d_streamadapter_destroy(LSkStream);
        end;
      finally
        ALfreeandNil(LResourceStream);
      end;
    end;
  end
  else
    Raise Exception.Create('Either AResourceName or AResourceStream must be provided');

  try

    // handle AMaskImage / AMaskImage
    var LOwnMaskImage := False;
    var LMaskImage := AMaskImage;
    try

      // Create the MaskImage
      if (AMaskResourceName <> '') and (LMaskImage = 0) then begin
        LMaskImage := ALCreateSkImageFromResource(
                        AMaskResourceName, // const AResourceName: String;
                        nil, // const AResourceStream: TStream;
                        '', // const AMaskResourceName: String;
                        0, // const AMaskImage: sk_image_t;
                        AScale, // const AScale: Single;
                        W, H, // const W, H: single;
                        AWrapMode, // const AWrapMode: TALImageWrapMode;
                        TpointF.Create(-50, -50), // const ACropCenter: TpointF;
                        0, // const ABlurRadius: single;
                        AXRadius, // const AXRadius: Single;
                        AYRadius); // const AYRadius: Single)
        LOwnMaskImage := True;
      end;

      var LSrcRect: TRectF;
      var LDstRect: TrectF;
      if LMaskImage <> 0 then begin
        // When LMaskImage is not null, the W, H provided as input
        // are ignored, and LDstRect is set to the size of LMaskImage.
        // ASrcRect is always calculated using the FitAndCrop wrap mode.
        LDstRect := TrectF.Create(0, 0, sk4d_image_get_width(LMaskImage), sk4d_image_get_Height(LMaskImage));
        LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
        LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, sk4d_image_get_width(LImage), sk4d_image_get_Height(LImage)), ACropCenter);
      end
      else begin
        case AWrapMode of
          //TALImageWrapMode.Original: begin
          //  LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
          //  LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          //  LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(LImage), sk4d_image_get_Height(LImage));
          //  if LSrcRect.Width > LDstRect.Width then LSrcRect.Width := LDstRect.Width
          //  else LDstRect.Width := LSrcRect.Width;
          //  if LSrcRect.Height > LDstRect.Height then LSrcRect.Height := LDstRect.Height
          //  else LDstRect.Height := LSrcRect.Height;
          //  LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          //end;
          TALImageWrapMode.Fit: Begin
            LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(LImage), sk4d_image_get_Height(LImage));
            LDstRect := LSrcRect.FitInto(TRectF.Create(0, 0, W * AScale, H * AScale));
            LDstRect.SetLocation(0,0);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.Stretch: Begin
            LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(LImage), sk4d_image_get_Height(LImage));
            LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.Place: Begin
            LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(LImage), sk4d_image_get_Height(LImage));
            LDstRect := LSrcRect.PlaceInto(TRectF.Create(0, 0, W * AScale, H * AScale));
            LDstRect.SetLocation(0,0);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.FitAndCrop: Begin
            LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
            LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, sk4d_image_get_width(LImage), sk4d_image_get_Height(LImage)), ACropCenter);
          end;
          Else
            Raise Exception.Create('Error FE241032-F199-4700-9F65-A32E408A71F0');
        end;
      end;
      LDstRect.Top := LDstRect.Top / AScale;
      LDstRect.right := LDstRect.right / AScale;
      LDstRect.left := LDstRect.left / AScale;
      LDstRect.bottom := LDstRect.bottom / AScale;

      Result := ALCreateSkSurface(
                  Round(LDstRect.Width * AScale),
                  Round(LDstRect.Height * AScale));
      try

        Var LCanvas: sk_canvas_t := ALSkCheckHandle(sk4d_surface_get_canvas(Result));
        ALDrawSkImage(
          LCanvas, // const ACanvas: sk_canvas_t;
          LImage, // const AImage: sk_image_t;
          AScale, // const AScale: Single;
          false, // const AAlignToPixel: Boolean;
          LSrcRect, // const ASrcRect: TrectF;
          LDstRect, // const ADstRect: TrectF;
          1, // const AOpacity: Single;
          LMaskImage, // const AMaskImage: sk_image_t;
          ACropCenter, // const ACropCenter: TpointF;
          ABlurRadius, // const ABlurRadius: single;
          AXRadius, // const AXRadius: Single;
          AYRadius); // const AYRadius: Single)

      except
        sk4d_refcnt_unref(Result);
        Raise;
      end;

    finally
      if LOwnMaskImage then
        sk4d_refcnt_unref(LMaskImage);
    end;

  finally
    sk4d_refcnt_unref(LImage);
  end;

end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateSkImageFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskImage: sk_image_t;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): sk_image_t;
begin
  var LSurface := ALCreateSkSurfaceFromResource(
                    AResourceName, // const AResourceName: String;
                    AResourceStream, // const AResourceStream: TStream;
                    AMaskResourceName, // const AMaskResourceName: String;
                    AMaskImage, // const AMaskImage: sk_image_t;
                    AScale, // const AScale: Single;
                    W, H, // const W, H: single;
                    AWrapMode, // const AWrapMode: TALImageWrapMode;
                    ACropCenter, // const ACropCenter: TpointF;
                    ABlurRadius, // const ABlurRadius: single;
                    AXRadius, // const AXRadius: Single;
                    AYRadius); // const AYRadius: Single)
  try
    Result := ALCreateSkImageFromSkSurface(LSurface);
  finally
    sk4d_refcnt_unref(LSurface);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
procedure ALDrawJBitmap(
            const ACanvas: JCanvas;
            const ABitmap: JBitmap;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In ABitmap coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskBitmap: JBitmap;
            const ACropCenter: TpointF; // Used only when AMaskBitmap is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);
begin

  // Init LCanvasMatrix/LCanvasScale
  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AAlignToPixel then ALExtractMatrixFromJCanvas(ACanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;

  // Init LScaledDstRect
  var LScaledDstRect := ADstRect;
  LScaledDstRect.Top := LScaledDstRect.Top * AScale;
  LScaledDstRect.right := LScaledDstRect.right * AScale;
  LScaledDstRect.left := LScaledDstRect.left * AScale;
  LScaledDstRect.bottom := LScaledDstRect.bottom * AScale;
  if AAlignToPixel then
    LScaledDstRect := ALAlignToPixelRound(LScaledDstRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);

  // Init LScaledBlurRadius
  var LScaledBlurRadius: Single := ABlurRadius * AScale;

  // Init LScaledXRadius and LScaledYRadius
  var LScaledXRadius: Single := AXRadius;
  var LScaledYRadius: Single := AYRadius;
  ALNormalizeAndScaleRadii(LScaledXRadius, LScaledYRadius, AScale, LScaledDstRect);

  // Init LBlurredBitmap
  var LBlurredBitmap: JBitmap := nil;
  try

    if compareValue(LScaledBlurRadius, 0, Tepsilon.Vector) > 0 then begin

      // It's approximately 1.5x faster on a resized bitmap
      var LRoundedSrcRect := ASrcRect.Round;
      var LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LScaledDstRect.width/LRoundedSrcRect.width, LScaledDstRect.height/LRoundedSrcRect.height);
      var LTmpBitmap := TJBitmap.JavaClass.createBitmap(ABitmap{src}, LRoundedSrcRect.Left{X}, LRoundedSrcRect.top{Y}, LRoundedSrcRect.width{Width}, LRoundedSrcRect.height{height}, LMatrix{m}, True{filter});
      LMatrix := nil;

      try

        // RenderEffect is approximately 2.5x slower than the deprecated RenderScript :(
        if TOSVersion.Check(12, 0) and
           TJHardwareBuffer.javaclass.isSupported(
             LTmpBitmap.getWidth, // width: Integer;
             LTmpBitmap.getHeight, // height: Integer;
             TJPixelFormat.JavaClass.RGBA_8888, // format: Integer;
             1, // layers: Integer;
             TJHardwareBuffer.javaclass.USAGE_GPU_SAMPLED_IMAGE or
             TJHardwareBuffer.javaclass.USAGE_GPU_COLOR_OUTPUT) then begin //usage: Int64
          Var LImageReader := TJImageReader.JavaClass.newInstance(
                                LTmpBitmap.getWidth, // width: Integer;
                                LTmpBitmap.getHeight,// height: Integer;
                                TJPixelFormat.JavaClass.RGBA_8888, // format: Integer;
                                1, // maxImages: Integer
                                TJHardwareBuffer.javaclass.USAGE_GPU_SAMPLED_IMAGE or
                                TJHardwareBuffer.javaclass.USAGE_GPU_COLOR_OUTPUT); // usage: Int64
          try
            var LRenderNode := TJRenderNode.JavaClass.init(StringToJString('BlurEffect'));
            try
              var LHardwareRenderer := TJHardwareRenderer.JavaClass.init;
              try
                LHardwareRenderer.setSurface(LImageReader.getSurface);
                LHardwareRenderer.setContentRoot(LRenderNode);
                LRenderNode.setPosition(0, 0, LImageReader.GetWidth, LImageReader.GetHeight);
                var LBlurRenderEffect := TJRenderEffect.JavaClass.createBlurEffect(
                                           LScaledBlurRadius,
                                           LScaledBlurRadius,
                                           TJShader_TileMode.JavaClass.MIRROR);
                LRenderNode.setRenderEffect(LBlurRenderEffect);
                var LrenderCanvas := TJALRecordingCanvas.wrap(LrenderNode.beginRecording);
                LRenderCanvas.drawBitmap(LTmpBitmap, 0{left}, 0{top}, nil{paint});
                LRenderNode.endRecording;
                LHardwareRenderer.createRenderRequest.setWaitForPresent(true).syncAndDraw;
                var LImage := LImageReader.acquireNextImage;
                if LImage = nil then raise Exception.Create('No Image');
                try
                  var LHardwareBuffer := LImage.GetHardwareBuffer;
                  if LHardwareBuffer = nil then raise Exception.Create('No HardwareBuffer');
                  try
                    var LHardwareBitmap := TJALBitmap.javaclass.wrapHardwareBuffer(LhardwareBuffer, ALGetGlobalJColorSpace);
                    if LHardwareBitmap=nil then raise Exception.Create('Create Bitmap Failed');
                    try
                      // This step is necessary to later convert the JBitmap into a texture using texImage2D.
                      // It is also required to ensure compatibility when calling ACanvas.drawBitmap(LBlurredBitmap, ...).
                      // Without this conversion, the error "Software rendering doesn't support the hardware bitmaps" will occur.
                      LBlurredBitmap := LHardwareBitmap.copy(TJBitmap_Config.JavaClass.ARGB_8888, false{isMutable});
                    finally
                      if not LHardwareBitmap.equals(LBlurredBitmap) then LHardwareBitmap.recycle;
                      LHardwareBitmap := nil;
                    end;
                  finally
                    LhardwareBuffer.close;
                    LhardwareBuffer := nil;
                  end;
                finally
                  LImage.close;
                  LImage := nil;
                end;
              finally
                LHardwareRenderer.destroy;
                LHardwareRenderer := nil;
              end;
            finally
              LRenderNode.discardDisplayList;
              LRenderNode := nil;
            end;
          finally
            LImageReader.close;
            LImageReader := nil;
          end;
        end
        else begin
          var LRS := getRenderScript;
          var LInput := TJAllocation.JavaClass.createFromBitmap(LRS, LTmpBitmap);
          var LOutPut := TJAllocation.JavaClass.createTyped(LRS, LInput.getType());
          var LScript :=  TJScriptIntrinsicBlur.javaclass.create(LRS, TJElement.javaclass.U8_4(LRS));
          LScript.setRadius(Min(25, LScaledBlurRadius)); // Set the radius of the Blur. Supported range 0 < radius <= 25
          LScript.setInput(LInput);
          LScript.forEach(LOutPut);
          LBlurredBitmap := TJBitmap.JavaClass.createBitmap(
                              LTmpBitmap.getWidth,
                              LTmpBitmap.getHeight,
                              TJBitmap_Config.JavaClass.ARGB_8888,
                              true{hasAlpha},
                              ALGetGlobalJColorSpace);
          LOutPut.copyTo(LBlurredBitmap);
          LScript := nil;
          LInput := nil;
          LOutPut := nil;
          LRS := nil;
        end;

      finally
        if not LTmpBitmap.equals(ABitmap) then LtmpBitmap.recycle;
        LTmpBitmap := nil;
      end;

    end;

    // Init LPaint
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    // Init LPorterDuffXfermode
    var LPorterDuffXfermode: JPorterDuffXfermode := nil;

    // Mask
    if AMaskbitmap <> nil then begin
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      var LJScaledDestRect := TJRectF.JavaClass.init(LScaledDstRect.left, LScaledDstRect.top, LScaledDstRect.right, LScaledDstRect.bottom);
      var LJSrcRect := TJRect.JavaClass.init(0, 0, AMaskBitmap.getWidth, AMaskBitmap.getHeight);
      ACanvas.drawBitmap(AMaskBitmap, LJSrcRect, LJScaledDestRect, LPaint);
      LJScaledDestRect := nil;
      LJSrcRect := nil;
      LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      LPaint.setXfermode(LPorterDuffXfermode);
    end

    // Circle/Oval
    else if SameValue(LScaledXRadius, LScaledDstRect.Width / 2, TEpsilon.position) and
            SameValue(LScaledYRadius, LScaledDstRect.Height / 2, TEpsilon.position) then begin
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      if SameValue(LScaledDstRect.Width, LScaledDstRect.Height, TEpsilon.position) then
        ACanvas.drawCircle(
          LScaledDstRect.Width/2, // cx: Single;
          LScaledDstRect.Height/2, // cy: Single;
          LScaledDstRect.Width/2, // radius: Single;
          LPaint)
      else
        ACanvas.drawOval(
          LScaledDstRect.left, // left: Single;
          LScaledDstRect.top, // top: Single;
          LScaledDstRect.right, // right: Single;
          LScaledDstRect.bottom, // bottom: Single
          LPaint);
      LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      LPaint.setXfermode(LPorterDuffXfermode);
    end

    // RoundRect
    else if (compareValue(LScaledXRadius, 0, TEpsilon.Position) > 0) and
            (compareValue(LScaledYRadius, 0, TEpsilon.position) > 0) then begin
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      ACanvas.drawRoundRect(
        LScaledDstRect.left, // left: Single;
        LScaledDstRect.top, // top: Single;
        LScaledDstRect.right, // right: Single;
        LScaledDstRect.bottom, // bottom: Single
        LScaledxRadius {rx},
        LScaledyRadius {ry},
        LPaint);
      LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      LPaint.setXfermode(LPorterDuffXfermode);
    end;

    // Draw the bitmap
    if LBlurredBitmap <> nil then begin
      var LJScaledDestRect := TJRectF.JavaClass.init(LScaledDstRect.left, LScaledDstRect.top, LScaledDstRect.right, LScaledDstRect.bottom);
      var LJSrcRect := TJRect.JavaClass.init(0, 0, LBlurredBitmap.getWidth, LBlurredBitmap.getHeight);
      ACanvas.drawBitmap(LBlurredBitmap, LJSrcRect, LJScaledDestRect, LPaint);
      LJScaledDestRect := nil;
      LJSrcRect := nil;
    end
    else begin
      var LJScaledDestRect := TJRectF.JavaClass.init(LScaledDstRect.left, LScaledDstRect.top, LScaledDstRect.right, LScaledDstRect.bottom);
      var LJSrcRect := TJRect.JavaClass.init(Round(ASrcRect.left), Round(ASrcRect.top), Round(ASrcRect.right), Round(ASrcRect.bottom));
      ACanvas.drawBitmap(ABitmap, LJSrcRect, LJScaledDestRect, LPaint);
      LJScaledDestRect := nil;
      LJSrcRect := nil;
    end;

    LPorterDuffXfermode := nil;
    LPaint := nil;

  finally
    if LBlurredBitmap <> nil then begin
      LBlurredBitmap.recycle;
      LBlurredBitmap := nil;
    end;
  end;

end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALCreateJBitmapFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskBitmap: JBitmap;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): JBitmap;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function _CreateJBitmapFromStream(Const AStream: Tstream): JBitmap;
  begin
    var LLength := AStream.Size-AStream.Position;
    var LArray := TJavaArray<Byte>.Create(LLength);
    try
      AStream.ReadBuffer(LArray.Data^, LLength);
      var LOptions := TJBitmapFactory_Options.Javaclass.Init;
      if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
      Result := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
      if Result = nil then raise Exception.create('Failed to decode bitmap from stream');
      LOptions := nil;
    finally
      ALfreeandNil(LArray);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function _CreateJBitmapFromFile(Const AFilename: String): JBitmap;
  begin
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    Result := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
    if Result = nil then raise Exception.create('Failed to load bitmap from file');
    LOptions := nil;
  end;

begin

  var LBitmap: JBitmap;
  if AResourceStream <> nil then LBitmap := _CreateJBitmapFromStream(AResourceStream)
  else if AResourceName <> '' then begin
    var LFileName := ALGetResourceFilename(AResourceName);
    if LFileName <> '' then LBitmap := _CreateJBitmapFromFile(LFileName)
    else begin
      var LResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
      try
        LBitmap := _CreateJBitmapFromStream(LResourceStream)
      finally
        ALfreeandNil(LResourceStream);
      end;
    end;
  end
  else
    Raise Exception.Create('Either AResourceName or AResourceStream must be provided');

  try

    // handle AMaskBitmap / AMaskBitmap
    var LOwnMaskBitmap := False;
    var LMaskBitmap := AMaskBitmap;
    try

      // Create the MaskBitmap
      if (AMaskResourceName <> '') and (LMaskBitmap = nil) then begin
        LMaskBitmap := ALCreateJbitmapFromResource(
                         AMaskResourceName, // const AResourceName: String;
                         nil, // const AResourceStream: TStream;
                         '', // const AMaskResourceName: String;
                         nil, // const AMaskBitmap: JBitmap;
                         AScale, // const AScale: Single;
                         W, H, // const W, H: single;
                         AWrapMode, // const AWrapMode: TALImageWrapMode;
                         TpointF.Create(-50, -50), // const ACropCenter: TpointF;
                         0, // const ABlurRadius: single;
                         AXRadius, // const AXRadius: Single;
                         AYRadius); // const AYRadius: Single)
        LOwnMaskBitmap := True;
      end;

      var LSrcRect: TRectF;
      var LDstRect: TrectF;
      if LMaskBitmap <> nil then begin
        // When LMaskBitmap is not null, the W, H provided as input
        // are ignored, and LDstRect is set to the size of LMaskBitmap.
        // ASrcRect is always calculated using the FitAndCrop wrap mode.
        LDstRect := TrectF.Create(0, 0, LMaskBitmap.getWidth, LMaskBitmap.getHeight);
        LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
        LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), ACropCenter);
      end
      else begin
        case AWrapMode of
          //TALImageWrapMode.Original: begin
          //  LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
          //  LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          //  LSrcRect := TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
          //  if LSrcRect.Width > LDstRect.Width then LSrcRect.Width := LDstRect.Width
          //  else LDstRect.Width := LSrcRect.Width;
          //  if LSrcRect.Height > LDstRect.Height then LSrcRect.Height := LDstRect.Height
          //  else LDstRect.Height := LSrcRect.Height;
          //  LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          //end;
          TALImageWrapMode.Fit: Begin
            LSrcRect := TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
            LDstRect := LSrcRect.FitInto(TRectF.Create(0, 0, W * AScale, H * AScale));
            LDstRect.SetLocation(0,0);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.Stretch: Begin
            LSrcRect := TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
            LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.Place: Begin
            LSrcRect := TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
            LDstRect := LSrcRect.PlaceInto(TRectF.Create(0, 0, W * AScale, H * AScale));
            LDstRect.SetLocation(0,0);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.FitAndCrop: Begin
            LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
            LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), ACropCenter);
          end;
          Else
            Raise Exception.Create('Error FE241032-F199-4700-9F65-A32E408A71F0');
        end;
      end;
      LDstRect.Top := LDstRect.Top / AScale;
      LDstRect.right := LDstRect.right / AScale;
      LDstRect.left := LDstRect.left / AScale;
      LDstRect.bottom := LDstRect.bottom / AScale;

      If (LMaskBitmap = nil) and
         (SameValue(ABlurRadius, 0, TEpsilon.Vector)) and
         (SameValue(AXRadius, 0, TEpsilon.Vector)) and
         (SameValue(AYRadius, 0, TEpsilon.Vector)) then begin

        // Using createBitmap with matrix is a little more faster (around 10-20%)
        // than calling ALDrawJBitmap
        var LRoundedSrcRect := LSrcRect.Round;
        var LMatrix := TJMatrix.JavaClass.init;
        LMatrix.postScale((LDstRect.width * AScale)/LRoundedSrcRect.width, (LDstRect.height * AScale)/LRoundedSrcRect.height);
        result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, LRoundedSrcRect.Left{X}, LRoundedSrcRect.top{Y}, LRoundedSrcRect.width{Width}, LRoundedSrcRect.height{height}, LMatrix{m}, True{filter});
        LMatrix := nil;

      end
      else begin

        Result := TJBitmap.JavaClass.createBitmap(
                    Round(LDstRect.Width * AScale),
                    Round(LDstRect.Height * AScale),
                    TJBitmap_Config.JavaClass.ARGB_8888,
                    true{hasAlpha},
                    ALGetGlobalJColorSpace);
        try

          var LCanvas := TJCanvas.JavaClass.init(result);
          ALDrawJBitmap(
            LCanvas, // const ACanvas: JCanvas;
            LBitmap, // const ABitmap: JBitmap;
            AScale, // const AScale: Single;
            false, // const AAlignToPixel: Boolean;
            LSrcRect, // const ASrcRect: TrectF;
            LDstRect, // const ADstRect: TrectF;
            1, // const AOpacity: Single;
            LMaskBitmap, // const AMaskBitmap: JBitmap;
            ACropCenter, // const ACropCenter: TpointF;
            ABlurRadius, // const ABlurRadius: single;
            AXRadius, // const AXRadius: Single;
            AYRadius); // const AYRadius: Single)
          LCanvas := nil;

        except
          Result.recycle;
          Result := nil;
          Raise;
        end;

      end;

    finally
      if LOwnMaskBitmap then begin
        LMaskBitmap.recycle;
        LMaskBitmap := nil;
      end;
    end;

  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;

end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
procedure ALDrawCGImageRef(
            const ACanvas: CGContextRef;
            const AImage: CGImageRef;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In AImage coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskImage: CGImageRef;
            const ACropCenter: TpointF; // Used only when AMaskImage is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);

var
  LGridHeight: Single;
  LCurPoint: TpointF;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _moveTo(const x: Single; const y: Single);
  begin
    CGContextMoveToPoint(ACanvas, X, LGridHeight - Y);
    LCurPoint.X := x;
    LCurPoint.Y := Y;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _rQuadTo(const dx1: Single; const dy1: Single; const dx2: Single; const dy2: Single);
  begin
    CGContextAddQuadCurveToPoint(
      ACanvas,
      LCurPoint.X + dx1{cpx},
      LGridHeight - (LCurPoint.Y + dy1){cpy},
      LCurPoint.X + dx2{x},
      LGridHeight - (LCurPoint.Y + dy2){y});
    LCurPoint.X := LCurPoint.X + dx2;
    LCurPoint.Y := LCurPoint.Y + dy2;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _rLineTo(const dx: Single; const dy: Single);
  begin
    CGContextAddLineToPoint(ACanvas, LCurPoint.X + dx{x}, LGridHeight - (LCurPoint.Y + dy{y}));
    LCurPoint.X := LCurPoint.X + dx;
    LCurPoint.Y := LCurPoint.Y + dy;
  end;

begin

  // Init LCanvasMatrix/LCanvasScale
  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AAlignToPixel then ALExtractMatrixFromCGContextRef(ACanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;

  // Init LScaledDstRect
  var LScaledDstRect := ADstRect;
  LScaledDstRect.Top := LScaledDstRect.Top * AScale;
  LScaledDstRect.right := LScaledDstRect.right * AScale;
  LScaledDstRect.left := LScaledDstRect.left * AScale;
  LScaledDstRect.bottom := LScaledDstRect.bottom * AScale;
  if AAlignToPixel then
    LScaledDstRect := ALAlignToPixelRound(LScaledDstRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);

  // Init LScaledBlurRadius
  var LScaledBlurRadius: Single := ABlurRadius * AScale;

  // Init LScaledXRadius and LScaledYRadius
  var LScaledXRadius: Single := AXRadius;
  var LScaledYRadius: Single := AYRadius;
  ALNormalizeAndScaleRadii(LScaledXRadius, LScaledYRadius, AScale, LScaledDstRect);

  // init LGridHeight
  LGridHeight := CGBitmapContextGetHeight(ACanvas);

  // Init LBlurredBitmap
  var LBlurredImage: CGImageRef := nil;
  try

    if compareValue(LScaledBlurRadius, 0, Tepsilon.Vector) > 0 then begin
      var LCroppedImage := CGImageCreateWithImageInRect(
                             AImage,
                             ALLowerLeftCGRect(
                               ASrcRect.TopLeft,
                               ASrcRect.width,
                               ASrcRect.height,
                               CGImageGetHeight(AImage)));
      try
        var LCIImage := TCIImage.Wrap(TCIImage.OCClass.imageWithCGImage(LCroppedImage));
        Try
          // Gaussian blur CIFilter naturally creates artifacts at the borders of the
          // output image. It is happening because the gaussian blur filter samples
          // pixels outside the edges of the image. But because there are no pixels,
          // you get this weird artefact. You can use "CIAffineClamp" filter to
          // "extend" your image infinitely in all directions.
          var LClampFilter := {$IF defined(ALMacOS)}TALCIFilter{$ELSE}TCIFilter{$ENDIF}.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
          Try
            LClampFilter.setDefaults;
            LClampFilter.setValueforKey(NSObjectToID(LCIImage), kCIInputImageKey);
            var LBlurFilter := {$IF defined(ALMacOS)}TALCIFilter{$ELSE}TCIFilter{$ENDIF}.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
            try
              LBlurFilter.setValueforKey(NSObjectToID(LClampFilter.outputImage), kCIInputImageKey);
              LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(LScaledBlurRadius), kCIInputRadiusKey);
              var LCIContext := TCIContext.Wrap({$IF defined(ALMacOS)}TALCIContext{$ELSE}TCIContext{$ENDIF}.OCClass.contextWithOptions(nil));
              try
                LBlurredImage := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);
                if LBlurredImage = nil then raise Exception.Create('Failed to create CGImageRef from CIContext');
              finally
                LCIContext.release;
              end;
            finally
              LBlurFilter.release;
            end
          finally
            LClampFilter.release;
          end;
        finally
          LCIImage.release;
        end;
      finally
        CGImageRelease(LCroppedImage);
      end;
    end;

    // Save GState
    CGContextSaveGState(ACanvas);
    try

      // Mask
      if AMaskImage <> nil then begin
        CGContextClipToMask(
          ACanvas,
          ALLowerLeftCGRect(
            LScaledDstRect.TopLeft,
            LScaledDstRect.width,
            LScaledDstRect.height,
            LGridHeight),
          AMaskImage);
      end

      // Oval
      else if SameValue(LScaledXRadius, LScaledDstRect.Width / 2, TEpsilon.position) and
              SameValue(LScaledYRadius, LScaledDstRect.Height / 2, TEpsilon.position) then begin
        CGContextBeginPath(ACanvas);
        CGContextAddEllipseInRect(
          ACanvas,
          ALLowerLeftCGRect(
            LScaledDstRect.TopLeft,
            LScaledDstRect.Width,
            LScaledDstRect.Height,
            LGridHeight));
        CGContextClip(ACanvas);
      end

      // RoundRect
      else if (compareValue(LScaledXRadius, 0, TEpsilon.Position) > 0) and
              (compareValue(LScaledYRadius, 0, TEpsilon.position) > 0) then begin
        CGContextBeginPath(ACanvas);
        //--
        var LWidthMinusCorners: single := (LScaledDstRect.width - (2 * LScaledXRadius));
        var LHeightMinusCorners: single := (LScaledDstRect.height - (2 * LScaledYRadius));
        // TopRight
        _moveTo(LScaledDstRect.right, LScaledDstRect.top + LScaledYRadius);
        _rQuadTo(0, -LScaledYRadius, -LScaledXRadius, -LScaledYRadius);
        _rLineTo(-LWidthMinusCorners, 0);
        // TopLeft
        _rQuadTo(-LScaledXRadius, 0, -LScaledXRadius, LScaledYRadius);
        _rLineTo(0, LHeightMinusCorners);
        // BottomLeft
        _rQuadTo(0, LScaledYRadius, LScaledXRadius, LScaledYRadius);
        _rLineTo(LWidthMinusCorners, 0);
        // BottomRight
        _rQuadTo(LScaledXRadius, 0, LScaledXRadius, -LScaledYRadius);
        _rLineTo(0, -LHeightMinusCorners);
        //--
        CGContextClosePath(ACanvas);
        CGContextClip(ACanvas);
      end

      // Rect
      else begin
        CGContextBeginPath(ACanvas);
        CGContextAddRect(
          ACanvas,
          ALLowerLeftCGRect(
            LScaledDstRect.TopLeft,
            LScaledDstRect.Width,
            LScaledDstRect.Height,
            LGridHeight));
        CGContextClip(ACanvas);
      end;

      // Draw the bitmap
      if LBlurredImage <> nil then begin
        CGContextDrawImage(
          ACanvas, // c: The graphics context in which to draw the image.
          ALLowerLeftCGRect(
            LScaledDstRect.TopLeft,
            LScaledDstRect.Width,
            LScaledDstRect.Height,
            LGridHeight),
          LBlurredImage);
      end
      else begin
        var LRatioX: Single := LScaledDstRect.width / ASrcRect.width;
        var LRatioY: Single := LScaledDstRect.height / ASrcRect.height;
        CGContextDrawImage(
          ACanvas, // c: The graphics context in which to draw the image.
          ALLowerLeftCGRect(
            TpointF.Create(
              LScaledDstRect.left-(ASrcRect.Left*LRatioX),
              LScaledDstRect.top-(ASrcRect.top*LRatioY)),
            LScaledDstRect.width + (ASrcRect.Left*LRatioX) + ((CGImageGetWidth(AImage)-ASrcRect.right)*LRatioX),
            LScaledDstRect.height + (ASrcRect.top*LRatioY)  + ((CGImageGetHeight(AImage)-ASrcRect.bottom)*LRatioY),
            LGridHeight),
          AImage);
      end;

    finally
      CGContextRestoreGState(ACanvas);
    end;

  finally
    if LBlurredImage <> nil then
      CGImageRelease(LBlurredImage);
  end;

end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALCreateCGContextRefFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskImage: CGImageRef;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): CGContextRef;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function _CreateOSImageFromStream(Const AStream: Tstream): ALOSImage;
  begin
    var LBuffer: Pointer := nil;
    var LLength: Int64 := 0;
    var LMemoryStream: TCustomMemoryStream := nil;
    if (AStream is TCustomMemoryStream) and (AStream.Position = 0) then begin
      LBuffer := TCustomMemoryStream(AStream).Memory;
      LLength := AStream.Size;
      AStream.Position := AStream.Size;
    end
    else LMemoryStream := TMemoryStream.Create;
    try
      if LMemoryStream <> nil then begin
        LMemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
        LBuffer := LMemoryStream.Memory;
        LLength := LMemoryStream.Size;
      end;
      var LData := TNSData.Wrap(
                     TNSData.alloc.initWithBytesNoCopy(
                       LBuffer, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                       LLength, // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                       False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
      try
        Result := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
        if Result = nil then raise Exception.create('Failed to decode image from stream');
      finally
        LData.release;
      end;
    finally
      ALFreeAndNil(LMemoryStream);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function _CreateOSImageFromFile(Const AFilename: String): ALOSImage;
  begin
    Result := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
    if Result = nil then raise Exception.create('Failed to load image from file');
  end;

begin

  var LOSImage: ALOSImage;
  if AResourceStream <> nil then LOSImage := _CreateOSImageFromStream(AResourceStream)
  else if AResourceName <> '' then begin
    var LFileName := ALGetResourceFilename(AResourceName);
    if LFileName <> '' then LOSImage := _CreateOSImageFromFile(LFileName)
    else begin
      var LResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
      try
        LOSImage := _CreateOSImageFromStream(LResourceStream)
      finally
        ALfreeandNil(LResourceStream);
      end;
    end;
  end
  else
    Raise Exception.Create('Either AResourceName or AResourceStream must be provided');

  try

    // Init LImage
    var LImage := ALOSImageGetCgImage(LOSImage);

    // handle AMaskImage / AMaskImage
    var LOwnMaskImage := False;
    var LMaskImage := AMaskImage;
    try

      // Create the MaskImage
      if (AMaskResourceName <> '') and (LMaskImage = nil) then begin
        LMaskImage := ALCreateCGImageRefFromResource(
                        AMaskResourceName, // const AResourceName: String;
                        nil, // const AResourceStream: TStream;
                        '', // const AMaskResourceName: String;
                        nil, // const AMaskImage: CGImageRef;
                        AScale, // const AScale: Single;
                        W, H, // const W, H: single;
                        AWrapMode, // const AWrapMode: TALImageWrapMode;
                        TpointF.Create(-50, -50), // const ACropCenter: TpointF;
                        0, // const ABlurRadius: single;
                        AXRadius, // const AXRadius: Single;
                        AYRadius); // const AYRadius: Single)
        LOwnMaskImage := True;
      end;

      var LSrcRect: TRectF;
      var LDstRect: TrectF;
      if LMaskImage <> nil then begin
        // When LMaskImage is not null, the W, H provided as input
        // are ignored, and LDstRect is set to the size of LMaskImage.
        // ASrcRect is always calculated using the FitAndCrop wrap mode.
        LDstRect := TrectF.Create(0, 0, CGImageGetWidth(LMaskImage), CGImageGetHeight(LMaskImage));
        LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
        LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)), ACropCenter);
      end
      else begin
        case AWrapMode of
          //TALImageWrapMode.Original: begin
          //  LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
          //  LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          //  LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage), CGImageGetHeight(LImage));
          //  if LSrcRect.Width > LDstRect.Width then LSrcRect.Width := LDstRect.Width
          //  else LDstRect.Width := LSrcRect.Width;
          //  if LSrcRect.Height > LDstRect.Height then LSrcRect.Height := LDstRect.Height
          //  else LDstRect.Height := LSrcRect.Height;
          //  LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          //end;
          TALImageWrapMode.Fit: Begin
            LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage), CGImageGetHeight(LImage));
            LDstRect := LSrcRect.FitInto(TRectF.Create(0, 0, W * AScale, H * AScale));
            LDstRect.SetLocation(0,0);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.Stretch: Begin
            LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage), CGImageGetHeight(LImage));
            LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.Place: Begin
            LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage), CGImageGetHeight(LImage));
            LDstRect := LSrcRect.PlaceInto(TRectF.Create(0, 0, W * AScale, H * AScale));
            LDstRect.SetLocation(0,0);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.FitAndCrop: Begin
            LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
            LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)), ACropCenter);
          end;
          Else
            Raise Exception.Create('Error FE241032-F199-4700-9F65-A32E408A71F0');
        end;
      end;
      LDstRect.Top := LDstRect.Top / AScale;
      LDstRect.right := LDstRect.right / AScale;
      LDstRect.left := LDstRect.left / AScale;
      LDstRect.bottom := LDstRect.bottom / AScale;

      Result := ALCreateCGContextRef(
                  Round(LDstRect.Width * AScale),
                  Round(LDstRect.Height * AScale));
      try

        ALDrawCGImageRef(
          Result, // const ACanvas: CGContextRef;
          LImage, // const AImage: CGImageRef;
          AScale, // const AScale: Single;
          false, // const AAlignToPixel: Boolean;
          LSrcRect, // const ASrcRect: TrectF;
          LDstRect, // const ADstRect: TrectF;
          1, // const AOpacity: Single;
          LMaskImage, // const AMaskImage: CGImageRef;
          ACropCenter, // const ACropCenter: TpointF;
          ABlurRadius, // const ABlurRadius: single;
          AXRadius, // const AXRadius: Single;
          AYRadius); // const AYRadius: Single)

      except
        CGContextRelease(Result);
        Raise;
      end;

    finally
      if LOwnMaskImage then
        CGImageRelease(LMaskImage);
    end;

  finally
    LOSImage.release;
  end;

end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALCreateCGImageRefFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskImage: CGImageRef;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): CGImageRef;
begin
  var LCGContextRef := ALCreateCGContextRefFromResource(
                         AResourceName, // const AResourceName: String;
                         AResourceStream, // const AResourceStream: TStream;
                         AMaskResourceName, // const AMaskResourceName: String;
                         AMaskImage, // const AMaskImage: CGImageRef;
                         AScale, // const AScale: Single;
                         W, H, // const W, H: single;
                         AWrapMode, // const AWrapMode: TALImageWrapMode;
                         ACropCenter, // const ACropCenter: TpointF;
                         ABlurRadius, // const ABlurRadius: single;
                         AXRadius, // const AXRadius: Single;
                         AYRadius); // const AYRadius: Single)
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LCGContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LCGContextRef);
  end;
end;
{$ENDIF}

{**********************}
procedure ALDrawTBitmap(
            const ACanvas: TCanvas;
            const ABitmap: TBitmap;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In ABitmap coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskBitmap: TBitmap;
            const ACropCenter: TpointF; // Used only when AMaskBitmap is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);
begin

  // Init LCanvasMatrix/LCanvasScale
  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AAlignToPixel then ALExtractMatrixFromTCanvas(ACanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;

  // Init LScaledDstRect
  var LScaledDstRect := ADstRect;
  LScaledDstRect.Top := LScaledDstRect.Top * AScale;
  LScaledDstRect.right := LScaledDstRect.right * AScale;
  LScaledDstRect.left := LScaledDstRect.left * AScale;
  LScaledDstRect.bottom := LScaledDstRect.bottom * AScale;
  if AAlignToPixel then
    LScaledDstRect := ALAlignToPixelRound(LScaledDstRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);

  // Init LScaledBlurRadius
  var LScaledBlurRadius: Single := ABlurRadius * AScale;

  // Init LScaledXRadius and LScaledYRadius
  var LScaledXRadius: Single := AXRadius;
  var LScaledYRadius: Single := AYRadius;
  ALNormalizeAndScaleRadii(LScaledXRadius, LScaledYRadius, AScale, LScaledDstRect);

  //Mask
  if aMaskBitmap <> nil then begin
    //var D, B, M: TBitmapData;
    //if Result.Map(TMapAccess.Write, D) then
    //try
    //  if LBitmap.Map(TMapAccess.Read, B) then
    //  try
    //    if aMask.Map(TMapAccess.Read, M) then
    //    try
    //      for var J := 0 to Result.Height - 1 do
    //        for var I := 0 to Result.Width - 1 do
    //        begin
    //          var C := B.GetPixel(I, J);
    //          TAlphaColorRec(C).A := TAlphaColorRec(M.GetPixel(I, J)).A;
    //          if TAlphaColorRec(C).A < 255 then begin  // << don't ask me why we need to do this :(
    //            var ratio: single := TAlphaColorRec(C).A / 255;
    //            TAlphaColorRec(C).R := round(TAlphaColorRec(C).R * ratio);
    //            TAlphaColorRec(C).G := round(TAlphaColorRec(C).G * ratio);
    //            TAlphaColorRec(C).B := round(TAlphaColorRec(C).B * ratio);
    //          end;
    //          D.SetPixel(I, J, C);
    //        end;
    //    finally
    //      aMask.Unmap(M);
    //    end;
    //  finally
    //    LBitmap.Unmap(B);
    //  end;
    //finally
    //  Result.Unmap(D);
    //end;
  end

   // Oval
  else if SameValue(LScaledXRadius, LScaledDstRect.Width / 2, TEpsilon.position) and
          SameValue(LScaledYRadius, LScaledDstRect.Height / 2, TEpsilon.position) then begin

    if ACanvas.BeginScene then
    try
      ACanvas.Clear(TAlphaColorRec.Null);
      ACanvas.Fill.Bitmap.Bitmap.Assign(ABitmap);
      ACanvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
      ACanvas.Fill.Kind := TBrushKind.Bitmap;
      ACanvas.FillEllipse(LScaledDstRect, AOpacity);
    finally
      ACanvas.EndScene;
    end;

  end

  // RoundRect
  else if (compareValue(LScaledXRadius, 0, TEpsilon.Position) > 0) and
          (compareValue(LScaledYRadius, 0, TEpsilon.position) > 0) then begin

    if ACanvas.BeginScene then
    try
      ACanvas.Clear(TAlphaColorRec.Null);
      ACanvas.Fill.Bitmap.Bitmap.Assign(ABitmap);
      ACanvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
      ACanvas.Fill.Kind := TBrushKind.Bitmap;
      ACanvas.FillRect(
        LScaledDstRect,
        LScaledXRadius,
        LScaledYRadius,
        AllCorners,
        AOpacity);
    finally
      ACanvas.EndScene;
    end;

  end

  // Rect
  else begin
    if ACanvas.BeginScene then
    try
      ACanvas.DrawBitmap(
        ABitmap, // const ABitmap: TBitmap;
        ASrcRect, //const SrcRect,
        LScaledDstRect, //const DstRect: TRectF;
        AOpacity, //const AOpacity: Single;
        false); // const HighSpeed: Boolean => disable interpolation
    finally
      ACanvas.EndScene;
    end;
  end;

  // Blur
  if compareValue(LScaledBlurRadius, 0, Tepsilon.Vector) > 0 then begin
    ACanvas.BeginScene;
    try
      var LBlurEffect := TBlurEffect.Create(nil);
      try
        // Specifies the amount of blur applied to the shadow.
        // Softness is a System.Single value that takes values in the range from 0 through 9.
        // I calculate approximatly that 0.5 = around 12 for blur
        LBlurEffect.softness := LScaledBlurRadius / 24;
        ACanvas.Flush;
        LBlurEffect.ProcessEffect(ACanvas, ACanvas.Bitmap, 1);
      finally
        ALFreeAndNil(LBlurEffect);
      end;
    finally
      ACanvas.EndScene;
    end;
  end;

end;

{***********************************}
function ALCreateTBitmapFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskBitmap: TBitmap;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): TBitmap;
begin

  var LBitmap: TBitmap;
  if AResourceStream <> nil then LBitmap := Tbitmap.CreateFromStream(AResourceStream)
  else if AResourceName <> '' then begin
    var LFileName := ALGetResourceFilename(AResourceName);
    if LFileName <> '' then LBitmap := Tbitmap.CreateFromFile(LFileName)
    else begin
      var LResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
      try
        LBitmap := Tbitmap.CreateFromStream(LResourceStream);
      finally
        ALfreeandNil(LResourceStream);
      end;
    end;
  end
  else
    Raise Exception.Create('Either AResourceName or AResourceStream must be provided');

  try

    // handle AMaskBitmap / AMaskBitmap
    var LOwnMaskBitmap := False;
    var LMaskBitmap := AMaskBitmap;
    try

      // Create the MaskBitmap
      if (AMaskResourceName <> '') and (LMaskBitmap = nil) then begin
        LMaskBitmap := ALCreateTBitmapFromResource(
                         AMaskResourceName, // const AResourceName: String;
                         nil, // const AResourceStream: TStream;
                         '', // const AMaskResourceName: String;
                         nil, // const AMaskBitmap: TBitmap;
                         AScale, // const AScale: Single;
                         W, H, // const W, H: single;
                         AWrapMode, // const AWrapMode: TALImageWrapMode;
                         TpointF.Create(-50, -50), // const ACropCenter: TpointF;
                         0, // const ABlurRadius: single;
                         AXRadius, // const AXRadius: Single;
                         AYRadius); // const AYRadius: Single)
        LOwnMaskBitmap := True;
      end;

      var LSrcRect: TRectF;
      var LDstRect: TrectF;
      if LMaskBitmap <> nil then begin
        // When LMaskBitmap is not null, the W, H provided as input
        // are ignored, and LDstRect is set to the size of LMaskBitmap.
        // ASrcRect is always calculated using the FitAndCrop wrap mode.
        LDstRect := TrectF.Create(0, 0, LMaskBitmap.Width, LMaskBitmap.Height);
        LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
        LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, LBitmap.Width, Lbitmap.Height), ACropCenter);
      end
      else begin
        case AWrapMode of
          //TALImageWrapMode.Original: begin
          //  LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
          //  LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          //  LSrcRect := TrectF.Create(0, 0, LBitmap.Width, LBitmap.Height);
          //  if LSrcRect.Width > LDstRect.Width then LSrcRect.Width := LDstRect.Width
          //  else LDstRect.Width := LSrcRect.Width;
          //  if LSrcRect.Height > LDstRect.Height then LSrcRect.Height := LDstRect.Height
          //  else LDstRect.Height := LSrcRect.Height;
          //  LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          //end;
          TALImageWrapMode.Fit: Begin
            LSrcRect := TrectF.Create(0, 0, LBitmap.Width, LBitmap.Height);
            LDstRect := LSrcRect.FitInto(TRectF.Create(0, 0, W * AScale, H * AScale));
            LDstRect.SetLocation(0,0);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.Stretch: Begin
            LSrcRect := TrectF.Create(0, 0, LBitmap.Width, LBitmap.Height);
            LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.Place: Begin
            LSrcRect := TrectF.Create(0, 0, LBitmap.Width, LBitmap.Height);
            LDstRect := LSrcRect.PlaceInto(TRectF.Create(0, 0, W * AScale, H * AScale));
            LDstRect.SetLocation(0,0);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
          end;
          TALImageWrapMode.FitAndCrop: Begin
            LDstRect := TRectF.Create(0, 0, W * AScale, H * AScale);
            LDstRect := ALAlignDimensionToPixelRound(LDstRect, 1{Scale}, TEpsilon.Position);
            LSrcRect := ALRectFitInto(LDstRect, TrectF.Create(0, 0, LBitmap.Width, LBitmap.Height), ACropCenter);
          end;
          Else
            Raise Exception.Create('Error FE241032-F199-4700-9F65-A32E408A71F0');
        end;
      end;
      LDstRect.Top := LDstRect.Top / AScale;
      LDstRect.right := LDstRect.right / AScale;
      LDstRect.left := LDstRect.left / AScale;
      LDstRect.bottom := LDstRect.bottom / AScale;

      Result := TBitmap.Create(
                  Round(LDstRect.Width * AScale),
                  Round(LDstRect.Height * AScale));
      try

        Var LCanvas := Result.Canvas;
        ALDrawTBitmap(
          LCanvas, // const ACanvas: TCanvas;
          LBitmap, // const ABitmap: TBitmap;
          AScale, // const AScale: Single;
          false, // const AAlignToPixel: Boolean;
          LSrcRect, // const ASrcRect: TrectF;
          LDstRect, // const ADstRect: TrectF;
          1, // const AOpacity: Single;
          LMaskBitmap, // const AMaskBitmap: TBitmap;
          ACropCenter, // const ACropCenter: TpointF;
          ABlurRadius, // const ABlurRadius: single;
          AXRadius, // const AXRadius: Single;
          AYRadius); // const AYRadius: Single)

      except
        ALFreeAndNil(Result);
        Raise;
      end;

    finally
      if LOwnMaskBitmap then
        ALFreeAndNil(LMaskBitmap);
    end;

  finally
    ALFreeAndNil(LBitmap);
  end;

end;

{*********************}
procedure ALDrawBitmap(
            const ACanvas: TALCanvas;
            const ABitmap: TALBitmap;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ASrcRect: TrectF; // In ABitmap coordinates (real pixels)
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AMaskBitmap: TALBitmap;
            const ACropCenter: TpointF; // Used only when AMaskBitmap is not nil to center the image on the mask
            const ABlurRadius: single;
            const AXRadius: Single;
            const AYRadius: Single);
begin
  {$IF defined(ALSkiaEngine)}
  ALDrawSkImage(
    ACanvas, // const ACanvas: sk_canvas_t;
    ABitmap, // const AImage: sk_image_t;
    AScale, // const AScale: Single;
    AAlignToPixel, // const AAlignToPixel: Boolean;
    ASrcRect, // const ASrcRect: TrectF; // In AImage coordinates (real pixels)
    ADstRect, // const ADstRect: TrectF;
    AOpacity, // const AOpacity: Single;
    AMaskBitmap, // const AMaskImage: sk_image_t;
    ACropCenter, // const ACropCenter: TpointF; // Used only when AMaskImage is not nil to center the image on the mask
    ABlurRadius, // const ABlurRadius: single;
    AXRadius, // const AXRadius: Single;
    AYRadius); // const AYRadius: Single);
  {$ELSEIF defined(ANDROID)}
  ALDrawJBitmap(
    ACanvas, // const ACanvas: JCanvas;
    ABitmap, // const ABitmap: JBitmap;
    AScale, // const AScale: Single;
    AAlignToPixel, // const AAlignToPixel: Boolean;
    ASrcRect, // const ASrcRect: TrectF; // In ABitmap coordinates (real pixels)
    ADstRect, // const ADstRect: TrectF;
    AOpacity, // const AOpacity: Single;
    AMaskBitmap, // const AMaskBitmap: JBitmap;
    ACropCenter, // const ACropCenter: TpointF; // Used only when AMaskBitmap is not nil to center the image on the mask
    ABlurRadius, // const ABlurRadius: single;
    AXRadius, // const AXRadius: Single;
    AYRadius); // const AYRadius: Single);
  {$ELSEIF defined(ALAppleOS)}
  ALDrawCGImageRef(
    ACanvas, // const ACanvas: CGContextRef;
    ABitmap, // const AImage: CGImageRef;
    AScale, // const AScale: Single;
    AAlignToPixel, // const AAlignToPixel: Boolean;
    ASrcRect, // const ASrcRect: TrectF; // In AImage coordinates (real pixels)
    ADstRect, // const ADstRect: TrectF;
    AOpacity, // const AOpacity: Single;
    AMaskBitmap, // const AMaskImage: CGImageRef;
    ACropCenter, // const ACropCenter: TpointF; // Used only when AMaskImage is not nil to center the image on the mask
    ABlurRadius, // const ABlurRadius: single;
    AXRadius, // const AXRadius: Single;
    AYRadius); // const AYRadius: Single);
  {$ELSE}
  ALDrawTBitmap(
    ACanvas, // const ACanvas: TCanvas;
    ABitmap, // const ABitmap: TBitmap;
    AScale, // const AScale: Single;
    AAlignToPixel, // const AAlignToPixel: Boolean;
    ASrcRect, // const ASrcRect: TrectF; // In ABitmap coordinates (real pixels)
    ADstRect, // const ADstRect: TrectF;
    AOpacity, // const AOpacity: Single;
    AMaskBitmap, // const AMaskBitmap: TBitmap;
    ACropCenter, // const ACropCenter: TpointF; // Used only when AMaskBitmap is not nil to center the image on the mask
    ABlurRadius, // const ABlurRadius: single;
    AXRadius, // const AXRadius: Single;
    AYRadius); // const AYRadius: Single);
  {$ENDIF}
end;

{**********************************}
function ALCreateBitmapFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskBitmap: TALBitmap;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): TALBitmap;
begin
  {$IF defined(ALSkiaEngine)}
  Result := ALCreateSkImageFromResource(
              AResourceName, // const AResourceName: String;
              AResourceStream, // const AResourceStream: TStream;
              AMaskResourceName, // const AMaskResourceName: String;
              AMaskBitmap, // const AMaskImage: sk_image_t;
              AScale, // const AScale: Single;
              W, H, // const W, H: single;
              AWrapMode, // const AWrapMode: TALImageWrapMode;
              ACropCenter, // const ACropCenter: TpointF;
              ABlurRadius, // const ABlurRadius: single;
              AXRadius, // const AXRadius: Single;
              AYRadius); // const AYRadius: Single);
  {$ELSEIF defined(ANDROID)}
  Result := ALCreateJBitmapFromResource(
              AResourceName, // const AResourceName: String;
              AResourceStream, // const AResourceStream: TStream;
              AMaskResourceName, // const AMaskResourceName: String;
              AMaskBitmap, // const AMaskBitmap: JBitmap;
              AScale, // const AScale: Single;
              W, H, // const W, H: single;
              AWrapMode, // const AWrapMode: TALImageWrapMode;
              ACropCenter, // const ACropCenter: TpointF;
              ABlurRadius, // const ABlurRadius: single;
              AXRadius, // const AXRadius: Single;
              AYRadius); // const AYRadius: Single);
  {$ELSEIF defined(ALAppleOS)}
  Result := ALCreateCGImageRefFromResource(
              AResourceName, // const AResourceName: String;
              AResourceStream, // const AResourceStream: TStream;
              AMaskResourceName, // const AMaskResourceName: String;
              AMaskBitmap, // const AMaskImage: CGImageRef;
              AScale, // const AScale: Single;
              W, H, // const W, H: single;
              AWrapMode, // const AWrapMode: TALImageWrapMode;
              ACropCenter, // const ACropCenter: TpointF;
              ABlurRadius, // const ABlurRadius: single;
              AXRadius, // const AXRadius: Single;
              AYRadius); // const AYRadius: Single);
  {$ELSE}
  Result := ALCreateTBitmapFromResource(
              AResourceName, // const AResourceName: String;
              AResourceStream, // const AResourceStream: TStream;
              AMaskResourceName, // const AMaskResourceName: String;
              AMaskBitmap, // const AMaskBitmap: TBitmap;
              AScale, // const AScale: Single;
              W, H, // const W, H: single;
              AWrapMode, // const AWrapMode: TALImageWrapMode;
              ACropCenter, // const ACropCenter: TpointF;
              ABlurRadius, // const ABlurRadius: single;
              AXRadius, // const AXRadius: Single;
              AYRadius); // const AYRadius: Single);;
  {$ENDIF}
end;

{***********************}
procedure ALDrawDrawable(
            const ACanvas: TCanvas;
            const ADrawable: TALDrawable;
            const ADstTopLeft: TpointF;
            const AOpacity: Single);
begin

  if ALIsDrawableNull(ADrawable) then
    Exit;

  var LSrcRect := TRectF.Create(0, 0, ALGetDrawableWidth(ADrawable), ALGetDrawableHeight(ADrawable));
  var LDstRect := LSrcRect;
  LDstRect.Width := LDstRect.Width / ACanvas.Scale;
  LDstRect.height := LDstRect.height / ACanvas.Scale;
  LDstRect.SetLocation(ADstTopLeft);
  LDstRect := ALAlignToPixelRound(LDstRect, ACanvas.Matrix, ACanvas.Scale, TEpsilon.position);

  {$IF DEFINED(ALSkiaCanvas)}

  {$IF defined(DEBUG)}
  ALSkCheckCanvas(ACanvas);
  {$ENDIF}

  var LPaint := ALGetGlobalSkPaint(AOpacity);
  var LSamplingoptions := ALGetNearestSkSamplingoptions;
  sk4d_canvas_draw_image_rect(
    TSkCanvasCustom(ACanvas).Canvas.Handle, // self: sk_canvas_t;
    ADrawable, // const image: sk_image_t;
    @LSrcRect, // const src: psk_rect_t;
    @LDstRect,  // const dest: psk_rect_t;
    @LSamplingoptions, // const sampling: psk_samplingoptions_t;
    LPaint, // const paint: sk_paint_t;
    FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

  {$ELSEIF DEFINED(ALGpuCanvas)}

  TCustomCanvasGpu(ACanvas).DrawTexture(
    LDstRect, // ATexRect (destRec)
    LSrcRect, // ARect (srcRec)
    ALModulateColor(TCustomCanvasGpu.ModulateColor, AOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    ADrawable);

  {$ELSE}

  ACanvas.DrawBitmap(
    ADrawable,
    LSrcRect, {SrcRect}
    LDstRect, {DestRect}
    AOpacity, {opacity}
    true{highSpeed});

  {$ENDIF}

end;

{***********************}
procedure ALDrawDrawable(
            const ACanvas: Tcanvas;
            const ADrawable: TALDrawable;
            const ASrcRect: TrectF; // IN REAL PIXEL !
            const ADstRect: TrectF; // IN Virtual pixels !
            const AOpacity: Single);
begin

  if ALIsDrawableNull(ADrawable) then
    Exit;

  var LDstRect := ALAlignToPixelRound(ADstRect, ACanvas.Matrix, ACanvas.Scale, TEpsilon.position);

  {$IF DEFINED(ALSkiaCanvas)}

  {$IF defined(DEBUG)}
  ALSkCheckCanvas(ACanvas);
  {$ENDIF}

  var LPaint := ALGetGlobalSkPaint(AOpacity);
  var LSamplingoptions: sk_samplingoptions_t;
  if SameValue(ASrcRect.Width, ADstRect.Width * ACanvas.Scale, 1) and
     SameValue(ASrcRect.Height, ADstRect.Height * ACanvas.Scale, 1) then
    LSamplingoptions := ALGetNearestSkSamplingoptions
  else
    LSamplingoptions := ALGetLinearSkSamplingoptions;
  sk4d_canvas_draw_image_rect(
    TSkCanvasCustom(ACanvas).Canvas.Handle, // self: sk_canvas_t;
    ADrawable, // const image: sk_image_t;
    @ASrcRect, // const src: psk_rect_t;
    @LDstRect,  // const dest: psk_rect_t;
    @LSamplingoptions, // const sampling: psk_samplingoptions_t;
    LPaint, // const paint: sk_paint_t;
    FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

  {$ELSEIF DEFINED(ALGpuCanvas)}

  TCustomCanvasGpu(ACanvas).DrawTexture(
    LDstRect, // ATexRect (destRec)
    ASrcRect, // ARect (srcRec)
    ALModulateColor(TCustomCanvasGpu.ModulateColor, AOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    ADrawable);

  {$ELSE}

  ACanvas.DrawBitmap(
    ADrawable,
    ASrcRect, {SrcRect}
    LDstRect, {DestRect}
    AOpacity, {opacity}
    true{highSpeed});

  {$ENDIF}

end;

{***********************}
procedure ALDrawDrawable(
            const ACanvas: Tcanvas;
            const ADrawable: TALDrawable;
            const ADstRect: TrectF; // IN Virtual pixels !
            const AOpacity: Single);
begin

  if ALIsDrawableNull(ADrawable) then
    Exit;

  ALDrawDrawable(
    ACanvas, // const ACanvas: Tcanvas;
    ADrawable, // const ADrawable: TALDrawable;
    Trectf.Create(
      0, 0,
      ALGetDrawableWidth(ADrawable),
      ALGetDrawableHeight(ADrawable)), // const ASrcRect: TrectF; // IN REAL PIXEL !
    ADstRect, // const ADstRect: TrectF; // IN Virtual pixels !
    AOpacity); // const AOpacity: Single)

end;

{************************************}
function ALCreateDrawableFromResource(
           const AResourceName: String;
           const AResourceStream: TStream;
           const AMaskResourceName: String;
           const AMaskBitmap: TALBitmap;
           const AScale: Single;
           const W, H: single;
           const AWrapMode: TALImageWrapMode;
           const ACropCenter: TpointF;
           const ABlurRadius: single;
           const AXRadius: Single;
           const AYRadius: Single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALCreateSkImageFromResource(
                AResourceName, // const AResourceName: String;
                AResourceStream, // const AResourceStream: TStream;
                AMaskResourceName, // const AMaskResourceName: String;
                AMaskBitmap, // const AMaskImage: sk_image_t;
                AScale, // const AScale: Single;
                W, H, // const W, H: single;
                AWrapMode, // const AWrapMode: TALImageWrapMode;
                ACropCenter, // const ACropCenter: TpointF;
                ABlurRadius, // const ABlurRadius: single;
                AXRadius, // const AXRadius: Single;
                AYRadius); // const AYRadius: Single);
    {$ELSE}
    var LSurface := ALCreateSkSurfaceFromResource(
                      AResourceName, // const AResourceName: String;
                      AResourceStream, // const AResourceStream: TStream;
                      AMaskResourceName, // const AMaskResourceName: String;
                      AMaskBitmap, // const AMaskImage: sk_image_t;
                      AScale, // const AScale: Single;
                      W, H, // const W, H: single;
                      AWrapMode, // const AWrapMode: TALImageWrapMode;
                      ACropCenter, // const ACropCenter: TpointF;
                      ABlurRadius, // const ABlurRadius: single;
                      AXRadius, // const AXRadius: Single;
                      AYRadius); // const AYRadius: Single);
    try
      {$IF defined(ALGPUCanvas)}
      result := ALCreateTextureFromSkSurface(LSurface);
      {$ELSE}
      result := ALCreateTBitmapFromSkSurface(LSurface);
      {$ENDIF}
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALCreateJBitmapFromResource(
                   AResourceName, // const AResourceName: String;
                   AResourceStream, // const AResourceStream: TStream;
                   AMaskResourceName, // const AMaskResourceName: String;
                   AMaskBitmap, // const AMaskBitmap: JBitmap;
                   AScale, // const AScale: Single;
                   W, H, // const W, H: single;
                   AWrapMode, // const AWrapMode: TALImageWrapMode;
                   ACropCenter, // const ACropCenter: TpointF;
                   ABlurRadius, // const ABlurRadius: single;
                   AXRadius, // const AXRadius: Single;
                   AYRadius); // const AYRadius: Single);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALCreateCGContextRefFromResource(
                         AResourceName, // const AResourceName: String;
                         AResourceStream, // const AResourceStream: TStream;
                         AMaskResourceName, // const AMaskResourceName: String;
                         AMaskBitmap, // const AMaskImage: CGImageRef;
                         AScale, // const AScale: Single;
                         W, H, // const W, H: single;
                         AWrapMode, // const AWrapMode: TALImageWrapMode;
                         ACropCenter, // const ACropCenter: TpointF;
                         ABlurRadius, // const ABlurRadius: single;
                         AXRadius, // const AXRadius: Single;
                         AYRadius); // const AYRadius: Single);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateTBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALCreateTBitmapFromResource(
              AResourceName, // const AResourceName: String;
              AResourceStream, // const AResourceStream: TStream;
              AMaskResourceName, // const AMaskResourceName: String;
              AMaskBitmap, // const AMaskBitmap: TBitmap;
              AScale, // const AScale: Single;
              W, H, // const W, H: single;
              AWrapMode, // const AWrapMode: TALImageWrapMode;
              ACropCenter, // const ACropCenter: TpointF;
              ABlurRadius, // const ABlurRadius: single;
              AXRadius, // const AXRadius: Single;
              AYRadius); // const AYRadius: Single);;
  {$ENDIF}
end;

{**********************************************************************************}
class operator TALDrawRectangleHelper.Initialize (out Dest: TALDrawRectangleHelper);
begin
  // Because of :
  // https://stackoverflow.com/questions/79318689/why-does-copyrecord-get-called-in-some-cases-with-tmyrec-but-not-others
end;

{******************************************************************}
constructor TALDrawRectangleHelper.Create(const ACanvas: TALCanvas);
begin
  FCanvas := ACanvas;
  FScale := 1;
  FAlignToPixel := True;
  FDstRect := TRectF.Empty;
  FOpacity := 1;
  FFillColor := TAlphaColors.Null;
  FFillGradientStyle := TGradientStyle.Linear;
  FFillGradientAngle := 999; // Invalid angle
  FFillGradientStartPoint := TPointF.Zero;
  FFillGradientEndPoint := TPointF.Zero;
  FFillGradientColors := [];
  FFillGradientOffsets := [];
  FFillResourceName := '';
  FFillResourceStream := nil;
  FFillMaskResourceName := '';
  FFillMaskBitmap := ALNullBitmap;
  FFillBackgroundMarginsRect := TRectF.Empty;
  FFillImageMarginsRect := TRectF.Empty;
  FFillImageNoRadius := False;
  FFillWrapMode := TALImageWrapMode.Fit;
  FFillCropCenter := TpointF.Create(-50, -50);
  FFillBlurRadius := 0;
  FStateLayerOpacity := 0;
  FStateLayerColor := TAlphaColors.Null;
  FStateLayerMarginsRect := TRectF.Empty;
  FStateLayerXRadius := 0;
  FStateLayerYRadius := 0;
  FDrawStateLayerOnTop := True;
  FStrokeColor := TAlphaColors.Null;
  FStrokeThickness := 0;
  FShadowColor := TAlphaColors.Null;
  FShadowBlur := 0;
  FShadowOffsetX := 0;
  FShadowOffsetY := 0;
  FSides := AllSides;
  FCorners := AllCorners;
  FXRadius := 0;
  FYRadius := 0;
end;

{*************************************************************************************}
function TALDrawRectangleHelper.SetScale(const AValue: Single): PALDrawRectangleHelper;
begin
  FScale := AValue;
  Result := @Self;
end;

{*********************************************************************************************}
function TALDrawRectangleHelper.SetAlignToPixel(const AValue: Boolean): PALDrawRectangleHelper;
begin
  FAlignToPixel := AValue;
  Result := @Self;
end;

{***************************************************************************************}
function TALDrawRectangleHelper.SetDstRect(const AValue: TrectF): PALDrawRectangleHelper;
begin
  FDstRect := AValue;
  Result := @Self;
end;

{***************************************************************************************}
function TALDrawRectangleHelper.SetOpacity(const AValue: Single): PALDrawRectangleHelper;
begin
  FOpacity := AValue;
  Result := @Self;
end;

{*************************************************************************************}
function TALDrawRectangleHelper.SetFill(const AFill: TALBrush): PALDrawRectangleHelper;
begin
  Result := @Self;
  if AFill = nil then exit;
  FFillColor := AFill.Color;
  FFillGradientStyle := Afill.Gradient.Style;
  FFillGradientAngle := Afill.Gradient.Angle;
  FFillGradientColors := Afill.Gradient.Colors;
  FFillGradientOffsets := Afill.Gradient.Offsets;
  FFillResourceName := AFill.ResourceName;
  FFillBackgroundMarginsRect := AFill.BackgroundMargins.Rect;
  FFillImageMarginsRect := AFill.ImageMargins.Rect;
  FFillImageNoRadius := AFill.ImageNoRadius;
  FFillWrapMode := AFill.WrapMode;
end;

{**********************************************************************************************}
function TALDrawRectangleHelper.SetFillColor(const AValue: TAlphaColor): PALDrawRectangleHelper;
begin
  FFillColor := AValue;
  Result := @Self;
end;

{*********************************************************************************************************}
function TALDrawRectangleHelper.SetFillGradientStyle(const AValue: TGradientStyle): PALDrawRectangleHelper;
begin
  FFillGradientStyle := AValue;
  Result := @Self;
end;

{*************************************************************************************************}
function TALDrawRectangleHelper.SetFillGradientAngle(const AValue: Single): PALDrawRectangleHelper;
begin
  FFillGradientAngle := AValue;
  Result := @Self;
end;

{*******************************************************************************************************}
function TALDrawRectangleHelper.SetFillGradientStartPoint(const AValue: TPointF): PALDrawRectangleHelper;
begin
  FFillGradientStartPoint := AValue;
  Result := @Self;
end;

{*****************************************************************************************************}
function TALDrawRectangleHelper.SetFillGradientEndPoint(const AValue: TPointF): PALDrawRectangleHelper;
begin
  FFillGradientEndPoint := AValue;
  Result := @Self;
end;

{***************************************************************************************************************}
function TALDrawRectangleHelper.SetFillGradientColors(const AValue: TArray<TAlphaColor>): PALDrawRectangleHelper;
begin
  FFillGradientColors := AValue;
  Result := @Self;
end;

{***********************************************************************************************************}
function TALDrawRectangleHelper.SetFillGradientOffsets(const AValue: TArray<Single>): PALDrawRectangleHelper;
begin
  FFillGradientOffsets := AValue;
  Result := @Self;
end;

{************************************************************************************************}
function TALDrawRectangleHelper.SetFillResourceName(const AValue: String): PALDrawRectangleHelper;
begin
  FFillResourceName := AValue;
  Result := @Self;
end;

{***************************************************************************************************}
function TALDrawRectangleHelper.SetFillResourceStream(const AValue: TStream): PALDrawRectangleHelper;
begin
  FFillResourceStream := AValue;
  Result := @Self;
end;

{****************************************************************************************************}
function TALDrawRectangleHelper.SetFillMaskResourceName(const AValue: String): PALDrawRectangleHelper;
begin
  FFillMaskResourceName := AValue;
  Result := @Self;
end;

{*************************************************************************************************}
function TALDrawRectangleHelper.SetFillMaskBitmap(const AValue: TALBitmap): PALDrawRectangleHelper;
begin
  FFillMaskBitmap := AValue;
  Result := @Self;
end;

{*********************************************************************************************************}
function TALDrawRectangleHelper.SetFillBackgroundMarginsRect(const AValue: TRectF): PALDrawRectangleHelper;
begin
  FFillBackgroundMarginsRect := AValue;
  Result := @Self;
end;

{****************************************************************************************************}
function TALDrawRectangleHelper.SetFillImageMarginsRect(const AValue: TRectF): PALDrawRectangleHelper;
begin
  FFillImageMarginsRect := AValue;
  Result := @Self;
end;

{**************************************************************************************************}
function TALDrawRectangleHelper.SetFillImageNoRadius(const AValue: Boolean): PALDrawRectangleHelper;
begin
  FFillImageNoRadius := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALDrawRectangleHelper.SetFillWrapMode(const AValue: TALImageWrapMode): PALDrawRectangleHelper;
begin
  FFillWrapMode := AValue;
  Result := @Self;
end;

{***********************************************************************************************}
function TALDrawRectangleHelper.SetFillCropCenter(const AValue: TpointF): PALDrawRectangleHelper;
begin
  FFillCropCenter := AValue;
  Result := @Self;
end;

{**********************************************************************************************}
function TALDrawRectangleHelper.SetFillBlurRadius(const AValue: single): PALDrawRectangleHelper;
begin
  FFillBlurRadius := AValue;
  Result := @Self;
end;

{****************************************************************************************************************************************}
function TALDrawRectangleHelper.SetStateLayer(const AStateLayer: TALStateLayer; const AContentColor: TAlphaColor): PALDrawRectangleHelper;
begin
  Result := @Self;
  if AStateLayer = nil then exit;
  FStateLayerOpacity := AStateLayer.Opacity;
  if AStateLayer.UseContentColor then FStateLayerColor := AContentColor
  else FStateLayerColor := AStateLayer.Color;
  FStateLayerMarginsRect := AStateLayer.Margins.Rect;
  FStateLayerXRadius := AStateLayer.XRadius;
  FStateLayerYRadius := AStateLayer.YRadius;
end;

{*************************************************************************************************}
function TALDrawRectangleHelper.SetStateLayerOpacity(const AValue: Single): PALDrawRectangleHelper;
begin
  FStateLayerOpacity := AValue;
  Result := @Self;
end;

{****************************************************************************************************}
function TALDrawRectangleHelper.SetStateLayerColor(const AValue: TAlphaColor): PALDrawRectangleHelper;
begin
  FStateLayerColor := AValue;
  Result := @Self;
end;

{*****************************************************************************************************}
function TALDrawRectangleHelper.SetStateLayerMarginsRect(const AValue: TRectF): PALDrawRectangleHelper;
begin
  FStateLayerMarginsRect := AValue;
  Result := @Self;
end;

{*************************************************************************************************}
function TALDrawRectangleHelper.SetStateLayerXRadius(const AValue: Single): PALDrawRectangleHelper;
begin
  FStateLayerXRadius := AValue;
  Result := @Self;
end;

{*************************************************************************************************}
function TALDrawRectangleHelper.SetStateLayerYRadius(const AValue: Single): PALDrawRectangleHelper;
begin
  FStateLayerYRadius := AValue;
  Result := @Self;
end;

{****************************************************************************************************}
function TALDrawRectangleHelper.SetDrawStateLayerOnTop(const AValue: Boolean): PALDrawRectangleHelper;
begin
  FDrawStateLayerOnTop := AValue;
  Result := @Self;
end;

{***********************************************************************************************}
function TALDrawRectangleHelper.SetStroke(const AStroke: TALStrokeBrush): PALDrawRectangleHelper;
begin
  Result := @Self;
  if AStroke = nil then exit;
  FStrokeColor := AStroke.Color;
  FStrokeThickness := AStroke.Thickness;
end;

{************************************************************************************************}
function TALDrawRectangleHelper.SetStrokeColor(const AValue: TalphaColor): PALDrawRectangleHelper;
begin
  FStrokeColor := AValue;
  Result := @Self;
end;

{***********************************************************************************************}
function TALDrawRectangleHelper.SetStrokeThickness(const AValue: Single): PALDrawRectangleHelper;
begin
  FStrokeThickness := AValue;
  Result := @Self;
end;

{******************************************************************************************}
function TALDrawRectangleHelper.SetShadow(const AShadow: TALShadow): PALDrawRectangleHelper;
begin
  Result := @Self;
  if AShadow = nil then exit;
  FShadowColor := AShadow.Color;
  FShadowBlur := AShadow.Blur;
  FShadowOffsetX := AShadow.OffsetX;
  FShadowOffsetY := AShadow.OffsetY;
end;

{************************************************************************************************}
function TALDrawRectangleHelper.SetShadowColor(const AValue: TAlphaColor): PALDrawRectangleHelper;
begin
  FShadowColor := AValue;
  Result := @Self;
end;

{******************************************************************************************}
function TALDrawRectangleHelper.SetShadowBlur(const AValue: Single): PALDrawRectangleHelper;
begin
  FShadowBlur := AValue;
  Result := @Self;
end;

{*********************************************************************************************}
function TALDrawRectangleHelper.SetShadowOffsetX(const AValue: Single): PALDrawRectangleHelper;
begin
  FShadowOffsetX := AValue;
  Result := @Self;
end;

{*********************************************************************************************}
function TALDrawRectangleHelper.SetShadowOffsetY(const AValue: Single): PALDrawRectangleHelper;
begin
  FShadowOffsetY := AValue;
  Result := @Self;
end;

{*************************************************************************************}
function TALDrawRectangleHelper.SetSides(const AValue: TSides): PALDrawRectangleHelper;
begin
  FSides := AValue;
  Result := @Self;
end;

{*****************************************************************************************}
function TALDrawRectangleHelper.SetCorners(const AValue: TCorners): PALDrawRectangleHelper;
begin
  FCorners := AValue;
  Result := @Self;
end;

{***************************************************************************************}
function TALDrawRectangleHelper.SetXRadius(const AValue: Single): PALDrawRectangleHelper;
begin
  FXRadius := AValue;
  Result := @Self;
end;

{***************************************************************************************}
function TALDrawRectangleHelper.SetYRadius(const AValue: Single): PALDrawRectangleHelper;
begin
  FYRadius := AValue;
  Result := @Self;
end;

{************************************}
procedure TALDrawRectangleHelper.Draw;

var
  LScaledDstRect: TRectF;
  LScaledStrokeThickness: Single;
  LFillGradientColors: TArray<TAlphaColor>;
  LStrokeColor: TalphaColor;
  {$IF (defined(ANDROID)) or (defined(ALAppleOS)) or (defined(ALSkiaEngine))}
  LShadowColor: TalphaColor;
  LScaledShadowBlur: Single;
  LScaledShadowOffsetX: Single;
  LScaledShadowOffsetY: Single;
  {$ENDIF}
  LStateLayerColor: TAlphaColor;
  LScaledStateLayerDstRect: TrectF;
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}
  LPathClipped: Boolean;
  {$ENDIF}

  {$REGION '_DrawRect (SKIA)'}
  {$IF defined(ALSkiaEngine)}
  procedure _DrawRect(
              const ACanvas: sk_canvas_t;
              const aPaint: sk_Paint_t;
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean;
              const aNoRadius: Boolean);

  var
    LCurPoint: TpointF;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _MoveTo(const APathBuilder: sk_pathbuilder_t; const x: Single; const y: Single);
    begin
      LCurPoint := TPointF.Create(x, y);
      sk4d_pathbuilder_move_to(APathBuilder, @LCurPoint);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _RMoveTo(const APathBuilder: sk_pathbuilder_t; const dx: Single; const dy: Single);
    begin
      LCurPoint := LCurPoint + TPointF.Create(dx, dy);
      sk4d_pathbuilder_move_to(APathBuilder, @LCurPoint);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _RLineTo(const APathBuilder: sk_pathbuilder_t; const dx: Single; const dy: Single);
    begin
      LCurPoint := LCurPoint + TPointF.Create(dx, dy);
      sk4d_pathbuilder_line_to(APathBuilder, @LCurPoint);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _RQuadTo(const APathBuilder: sk_pathbuilder_t; const dx1: Single; const dy1: Single; const dx2: Single; const dy2: Single);
    begin
      var LPoint1 := LCurPoint + TPointF.Create(dx1, dy1);
      LCurPoint := LCurPoint + TPointF.Create(dx2, dy2);
      sk4d_pathbuilder_quad_to(APathBuilder, @LPoint1, @LCurPoint);
    end;

  begin

    // Init LRect
    var LRect := aRect;

    // Init LScaledXRadius and LScaledYRadius
    var LScaledXRadius: Single;
    var LScaledYRadius: Single;
    if not aNoRadius then begin
      LScaledXRadius := FXRadius;
      LScaledYRadius := FYRadius;
      ALNormalizeAndScaleRadii(LScaledXRadius, LScaledYRadius, FScale, LRect);
    end
    else begin
      LScaledXRadius := 0;
      LScaledYRadius := 0;
    end;

    // use drawcircle/drawoval
    if SameValue(LScaledXRadius, LRect.Width / 2, TEpsilon.position) and
       SameValue(LScaledYRadius, LRect.Height / 2, TEpsilon.position) and
       (FCorners = AllCorners) and
       (FSides=AllSides) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      // drawcircle
      if SameValue(LRect.Width, LRect.Height, TEpsilon.position) then begin
        var LCenterPoint := LRect.CenterPoint;
        sk4d_canvas_draw_circle(ACanvas, @LCenterPoint{center}, LRect.width / 2{radius}, aPaint);
      end
      // drawoval
      else
        sk4d_canvas_draw_oval(ACanvas, @LRect{oval}, aPaint);

    end

    // use drawRoundRect
    else if ((compareValue(LScaledXRadius, 0, TEpsilon.Position) > 0) and
             (compareValue(LScaledYRadius, 0, TEpsilon.position) > 0)) and
            (FCorners<>[]) and
            (FSides=AllSides) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      var LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
      try
        var LRadii: array[0..4] of TPointF;
        if TCorner.TopLeft in FCorners then LRadii[0] := TPointF.Create(LScaledXRadius, LScaledYRadius)
        else LRadii[0] := TPointF.Create(0, 0);

        if TCorner.TopRight in FCorners then LRadii[1] := TPointF.Create(LScaledXRadius, LScaledYRadius)
        else LRadii[1] := TPointF.Create(0, 0);

        if TCorner.BottomRight in FCorners then LRadii[2] := TPointF.Create(LScaledXRadius, LScaledYRadius)
        else LRadii[2] := TPointF.Create(0, 0);

        if TCorner.BottomLeft in FCorners then LRadii[3] := TPointF.Create(LScaledXRadius, LScaledYRadius)
        else LRadii[3] := TPointF.Create(0, 0);

        sk4d_rrect_set_rect2(
          LRRect, // self: sk_rrect_t;
          @LRect, // const rect: psk_rect_t;
          @LRadii); // const radii: psk_vector_t
        sk4d_canvas_draw_rrect(ACanvas, LRRect, APaint);
      finally
        sk4d_rrect_destroy(LRRect);
      end;

    end

    // use drawRect
    else if ((compareValue(LScaledXRadius, 0, TEpsilon.Position) = 0) or
             (compareValue(LScaledYRadius, 0, TEpsilon.position) = 0) or
             (FCorners=[])) and
            (FSides=AllSides) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      sk4d_canvas_draw_Rect(ACanvas, @LRect, APaint);

    end

    // use drawPath
    else begin

      var LPathBuilder := ALSkCheckHandle(sk4d_pathbuilder_create);
      try
        var LHalfStrokeThickness: Single := 0;
        if (LStrokeColor <> TalphaColorRec.Null) then begin
          var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
          if (aDrawOnlyBorder) or
             ((LRectIsEqualsToStrokeRect) and
              (LShadowcolor = TalphaColorRec.Null) and
              (FCorners=AllCorners) and
              (FSides=AllSides)) then begin
            LHalfStrokeThickness := LScaledStrokeThickness / 2;
            LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
          end
          else if (LRectIsEqualsToStrokeRect) and
                  (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) and
                  (FCorners=AllCorners) and
                  (FSides=AllSides) then begin
            LHalfStrokeThickness := 1;
            LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
          end;
        end;
        //--
        var LXRadius: single := LScaledXRadius;
        var LYradius: single := LScaledYRadius;
        if (LXRadius > LRect.width / 2) then LXRadius := LRect.width / 2;
        if (LYradius > LRect.height / 2) then LYradius := LRect.height / 2;
        //--
        var LCorners: TCorners;
        if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
           (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := FCorners
        else LCorners := [];
        //--
        If aDrawOnlyBorder then begin
          If (TCorner.TopRight in LCorners)    and (not (TSide.Top in FSides))    and (not (Tside.Right in FSides)) then LCorners := LCorners - [TCorner.TopRight];
          If (TCorner.TopLeft in LCorners)     and (not (TSide.Top in FSides))    and (not (Tside.Left in FSides))  then LCorners := LCorners - [TCorner.TopLeft];
          If (TCorner.BottomRight in LCorners) and (not (TSide.Bottom in FSides)) and (not (Tside.Right in FSides)) then LCorners := LCorners - [TCorner.BottomRight];
          If (TCorner.BottomLeft in LCorners)  and (not (TSide.Bottom in FSides)) and (not (Tside.Left in FSides))  then LCorners := LCorners - [TCorner.BottomLeft];
        end;
        //--
        var LWidthMinusCorners: single := (LRect.width - (2 * LXRadius));
        var LHeightMinusCorners: single := (LRect.height - (2 * LYradius));


        //----- TopRight
        if (TCorner.TopRight in LCorners) then begin
          _MoveTo(LPathBuilder, LRect.right, LRect.top + LYradius);
          _RQuadTo(LPathBuilder, 0, -LYradius, -LXRadius, -LYradius);
        end
        else begin
          _MoveTo(LPathBuilder, LRect.right, LRect.top + LYradius);
          //----
          if (not aDrawOnlyBorder) or
             (TSide.right in FSides) then begin
            _RLineTo(LPathBuilder, 0, -LYradius);
            if (aDrawOnlyBorder) and
               (TSide.right in FSides) and
               (not (TSide.top in FSides)) then begin
              _RLineTo(LPathBuilder, 0, -LHalfStrokeThickness);
              _RMoveTo(LPathBuilder, 0, LHalfStrokeThickness);
            end;
          end
          else _RMoveTo(LPathBuilder, 0, -LYradius); // aDrawOnlyBorder AND not TSide.right
          //----
          if (not aDrawOnlyBorder) or
             (TSide.top in FSides) then begin
            if (aDrawOnlyBorder) and
               (TSide.top in FSides) and
               (not (TSide.right in FSides)) then begin
              _RMoveTo(LPathBuilder, LHalfStrokeThickness, 0);
              _RLineTo(LPathBuilder, -LHalfStrokeThickness, 0);
            end;
            _RLineTo(LPathBuilder, -LXRadius,0);
          end
          else _RMoveTo(LPathBuilder, -LXRadius,0); // aDrawOnlyBorder AND not TSide.top
        end;
        //-----
        if (not aDrawOnlyBorder) or
           (TSide.Top in FSides) then _RLineTo(LPathBuilder, -LWidthMinusCorners, 0)
        else _RMoveTo(LPathBuilder, -LWidthMinusCorners, 0);

        //----- TopLeft
        if (TCorner.TopLeft in LCorners) then begin
          _RQuadTo(LPathBuilder, -LXRadius, 0, -LXRadius, LYradius);
        end
        else begin
          if (not aDrawOnlyBorder) or
             (TSide.top in FSides) then begin
            _RLineTo(LPathBuilder, -LXRadius, 0);
            if (aDrawOnlyBorder) and
               (TSide.top in FSides) and
               (not (TSide.left in FSides)) then begin
              _RLineTo(LPathBuilder, -LHalfStrokeThickness, 0);
              _RMoveTo(LPathBuilder, LHalfStrokeThickness, 0);
            end;
          end
          else _RMoveTo(LPathBuilder, -LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
          //----
          if (not aDrawOnlyBorder) or
             (TSide.left in FSides) then begin
            if (aDrawOnlyBorder) and
               (TSide.left in FSides) and
               (not (TSide.top in FSides)) then begin
              _RMoveTo(LPathBuilder, 0, -LHalfStrokeThickness);
              _RLineTo(LPathBuilder, 0, LHalfStrokeThickness);
            end;
            _RLineTo(LPathBuilder, 0,LYradius);
          end
          else _RMoveTo(LPathBuilder, 0,LYradius); // aDrawOnlyBorder AND not TSide.left
        end;
        //-----
        if (not aDrawOnlyBorder) or
           (TSide.left in FSides) then _RLineTo(LPathBuilder, 0, LHeightMinusCorners)
        else _RMoveTo(LPathBuilder, 0, LHeightMinusCorners);

        //----- BottomLeft
        if (TCorner.BottomLeft in LCorners) then begin
          _RQuadTo(LPathBuilder, 0, LYradius, LXRadius, LYradius);
        end
        else begin
          if (not aDrawOnlyBorder) or
             (TSide.left in FSides) then begin
            _RLineTo(LPathBuilder, 0, LYradius);
            if (aDrawOnlyBorder) and
               (TSide.left in FSides) and
               (not (TSide.bottom in FSides)) then begin
              _RLineTo(LPathBuilder, 0, LHalfStrokeThickness);
              _RMoveTo(LPathBuilder, 0, -LHalfStrokeThickness);
            end;
          end
          else _RMoveTo(LPathBuilder, 0, LYradius); // aDrawOnlyBorder AND not TSide.left
          //----
          if (not aDrawOnlyBorder) or
             (TSide.bottom in FSides) then begin
            if (aDrawOnlyBorder) and
               (TSide.bottom in FSides) and
               (not (TSide.left in FSides)) then begin
              _RMoveTo(LPathBuilder, -LHalfStrokeThickness, 0);
              _RLineTo(LPathBuilder, LHalfStrokeThickness, 0);
            end;
            _RLineTo(LPathBuilder, LXRadius,0);
          end
          else _RMoveTo(LPathBuilder, LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        end;
        //-----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in FSides) then _RLineTo(LPathBuilder, LWidthMinusCorners, 0)
        else _RMoveTo(LPathBuilder, LWidthMinusCorners, 0);

        //----- BottomRight
        if (TCorner.BottomRight in LCorners) then begin
          _RQuadTo(LPathBuilder, LXRadius, 0, LXRadius, -LYradius);
        end
        else begin
          if (not aDrawOnlyBorder) or
             (TSide.bottom in FSides) then begin
            _RLineTo(LPathBuilder, LXRadius,0);
            if (aDrawOnlyBorder) and
               (TSide.bottom in FSides) and
               (not (TSide.right in FSides)) then begin
              _RLineTo(LPathBuilder, LHalfStrokeThickness, 0);
              _RMoveTo(LPathBuilder, -LHalfStrokeThickness, 0);
            end;
          end
          else _RMoveTo(LPathBuilder, LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
          //----
          if (not aDrawOnlyBorder) or
             (TSide.right in FSides) then begin
            if (aDrawOnlyBorder) and
               (TSide.right in FSides) and
               (not (TSide.bottom in FSides)) then begin
              _RMoveTo(LPathBuilder, 0, LHalfStrokeThickness);
              _RLineTo(LPathBuilder, 0, -LHalfStrokeThickness);
            end;
            _RLineTo(LPathBuilder, 0,-LYradius);
          end
          else _RMoveTo(LPathBuilder, 0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        end;
        //-----
        if (not aDrawOnlyBorder) or
           (TSide.right in FSides) then _RLineTo(LPathBuilder, 0, -LHeightMinusCorners)
        else _RMoveTo(LPathBuilder, 0, -LHeightMinusCorners);
        //-----
        // We cannot close the path because it would connect the last position
        // of a moveTo to the current endpoint. For example, if we want to skip the
        // bottom side (sides=[Top, Left, Right] and xRadius/yRadius > 0) by
        // performing a moveTo from the BottomLeft to the BottomRight,
        // closing the path when reaching the TopRight would incorrectly connect back
        // to the BottomRight instead of the desired TopRight position.
        //if (TSide.right in FSides) and
        //   (TSide.top in FSides) then sk4d_pathbuilder_close(LPathBuilder);


        var LPath := sk4d_pathbuilder_detach(LPathBuilder);
        try
          sk4d_canvas_draw_Path(ACanvas, LPath, APaint);
        finally
          sk4d_path_destroy(LPath);
        end;

      finally
        sk4d_pathbuilder_destroy(LPathBuilder);
      end;

    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_DrawRect (ANDROID)'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}
  procedure _DrawRect(
              const ACanvas: Jcanvas;
              const aPaint: JPaint;
              const aRect: TrectF;
              const aDrawOnlyBorder: Boolean;
              const aForceDrawPath: Boolean;
              const aClipPath: Boolean;
              const aNoRadius: Boolean);
  begin

    // Init LRect
    var LRect := aRect;

    // Init LScaledXRadius and LScaledYRadius
    var LScaledXRadius: Single;
    var LScaledYRadius: Single;
    if not aNoRadius then begin
      LScaledXRadius := FXRadius;
      LScaledYRadius := FYRadius;
      ALNormalizeAndScaleRadii(LScaledXRadius, LScaledYRadius, FScale, LRect);
    end
    else begin
      LScaledXRadius := 0;
      LScaledYRadius := 0;
    end;

    // use drawcircle/drawOval/addCircle+drawPath/addOval+drawPath
    if SameValue(LScaledXRadius, LRect.Width / 2, TEpsilon.position) and
       SameValue(LScaledYRadius, LRect.Height / 2, TEpsilon.position) and
       (FCorners = AllCorners) and
       (FSides=AllSides) then begin

      // use drawcircle/drawOval
      if (not aForceDrawPath) and
         (not aClipPath) then begin
        if (LStrokeColor <> TalphaColorRec.Null) then begin
          var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
          if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
            LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
          else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
            LRect.Inflate(-1, -1);
        end;
        //--
        if SameValue(LRect.Width, LRect.Height, TEpsilon.position) then
          ACanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, aPaint)
        else
          ACanvas.drawOval(LRect.Left{left}, LRect.Top{top}, LRect.Right{right}, LRect.Bottom{bottom}, aPaint);
      end

      // use addCircle+drawPath/addOval+drawPath
      else begin
        var LPath := TJPath.Create;
        //--
        if (LStrokeColor <> TalphaColorRec.Null) then begin
          var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
          if (aDrawOnlyBorder) or
             ((LRectIsEqualsToStrokeRect) and
              (LShadowcolor = TalphaColorRec.Null)) then begin
            LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2));
          end
          else if (LRectIsEqualsToStrokeRect) and
                  (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then begin
            LRect.Inflate(-1, -1);
          end;
        end;
        //--
        if SameValue(LRect.Width, LRect.Height, TEpsilon.position) then
          LPath.addCircle(LRect.CenterPoint.x{x}, LRect.CenterPoint.y{y}, LRect.width / 2{radius}, TJPath_Direction.JavaClass.CW{dir})
        else
          LPath.addOval(LRect.Left{left}, LRect.Top{top}, LRect.Right{right}, LRect.Bottom{bottom}, TJPath_Direction.JavaClass.CW{dir});
        //--
        if aPaint <> nil then ACanvas.drawPath(LPath,aPaint);
        if aClipPath then begin
          ACanvas.save;
          ACanvas.clipPath(LPath);
          {$IF defined(DEBUG)}
          if LPathClipped then
            raise Exception.Create('Error 2001FF24-310A-48D3-9D9C-34B8C3358130');
          {$ENDIF}
          LPathClipped := True;
        end;
        LPath := nil;
      end;

    end

    // use drawRoundRect
    else if (not aForceDrawPath) and
            (not aClipPath) and
            ((compareValue(LScaledXRadius, 0, TEpsilon.Position) > 0) and
             (compareValue(LScaledYRadius, 0, TEpsilon.position) > 0)) and
            (FCorners=AllCorners) and
            (FSides=AllSides) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      var LJRect := TJRectf.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);
      ACanvas.drawRoundRect(
        LJRect{rect},
        LScaledXRadius {rx},
        LScaledYRadius {ry},
        apaint);
      LJRect := nil;

    end

    // use drawRect
    else if (not aForceDrawPath) and
            (not aClipPath) and
            ((compareValue(LScaledXRadius, 0, TEpsilon.Position) = 0) or
             (compareValue(LScaledYRadius, 0, TEpsilon.position) = 0) or
             (FCorners=[])) and
            (FSides=AllSides) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      ACanvas.drawRect(
        LRect.left{left},
        LRect.top{top},
        LRect.right{right},
        LRect.bottom{bottom},
        apaint);

    end

    // use drawPath
    else begin

      var LPath := TJPath.Create;
      //--
      var LHalfStrokeThickness: Single := 0;
      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if (aDrawOnlyBorder) or
           ((LRectIsEqualsToStrokeRect) and
            (LShadowcolor = TalphaColorRec.Null) and
            (FCorners=AllCorners) and
            (FSides=AllSides)) then begin
          LHalfStrokeThickness := LScaledStrokeThickness / 2;
          LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
        end
        else if (LRectIsEqualsToStrokeRect) and
                (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) and
                (FCorners=AllCorners) and
                (FSides=AllSides) then begin
          LHalfStrokeThickness := 1;
          LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
        end;
      end;
      //--
      var LXRadius: single := LScaledXRadius;
      var LYradius: single := LScaledYRadius;
      if (LXRadius > LRect.width / 2) then LXRadius := LRect.width / 2;
      if (LYradius > LRect.height / 2) then LYradius := LRect.height / 2;
      //--
      var LCorners: TCorners;
      if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := FCorners
      else LCorners := [];
      //--
      If aDrawOnlyBorder then begin
        If (TCorner.TopRight in LCorners)    and (not (TSide.Top in FSides))    and (not (Tside.Right in FSides)) then LCorners := LCorners - [TCorner.TopRight];
        If (TCorner.TopLeft in LCorners)     and (not (TSide.Top in FSides))    and (not (Tside.Left in FSides))  then LCorners := LCorners - [TCorner.TopLeft];
        If (TCorner.BottomRight in LCorners) and (not (TSide.Bottom in FSides)) and (not (Tside.Right in FSides)) then LCorners := LCorners - [TCorner.BottomRight];
        If (TCorner.BottomLeft in LCorners)  and (not (TSide.Bottom in FSides)) and (not (Tside.Left in FSides))  then LCorners := LCorners - [TCorner.BottomLeft];
      end;
      //--
      var LWidthMinusCorners: single := (LRect.width - (2 * LXRadius));
      var LHeightMinusCorners: single := (LRect.height - (2 * LYradius));


      //----- TopRight
      if (TCorner.TopRight in LCorners) then begin
        LPath.moveTo(LRect.right, LRect.top + LYradius);
        LPath.rQuadTo(0, -LYradius, -LXRadius, -LYradius);
      end
      else begin
        LPath.moveTo(LRect.right, LRect.top + LYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in FSides) then begin
          LPath.rLineTo(0, -LYradius);
          if (aDrawOnlyBorder) and
             (TSide.right in FSides) and
             (not (TSide.top in FSides)) then begin
            LPath.rLineTo(0, -LHalfStrokeThickness);
            LPath.rMoveTo(0, LHalfStrokeThickness);
          end;
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in FSides) then begin
          if (aDrawOnlyBorder) and
             (TSide.top in FSides) and
             (not (TSide.right in FSides)) then begin
            LPath.rMoveTo(LHalfStrokeThickness, 0);
            LPath.rLineTo(-LHalfStrokeThickness, 0);
          end;
          LPath.rLineTo(-LXRadius,0);
        end
        else LPath.rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in FSides) then LPath.rLineTo(-LWidthMinusCorners, 0)
      else LPath.rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        LPath.rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in FSides) then begin
          LPath.rLineTo(-LXRadius, 0);
          if (aDrawOnlyBorder) and
             (TSide.top in FSides) and
             (not (TSide.left in FSides)) then begin
            LPath.rLineTo(-LHalfStrokeThickness, 0);
            LPath.rMoveTo(LHalfStrokeThickness, 0);
          end;
        end
        else LPath.rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in FSides) then begin
          if (aDrawOnlyBorder) and
             (TSide.left in FSides) and
             (not (TSide.top in FSides)) then begin
            LPath.rMoveTo(0, -LHalfStrokeThickness);
            LPath.rLineTo(0, LHalfStrokeThickness);
          end;
          LPath.rLineTo(0,LYradius);
        end
        else LPath.rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in FSides) then LPath.rLineTo(0, LHeightMinusCorners)
      else LPath.rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        LPath.rQuadTo(0, LYradius, LXRadius, LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in FSides) then begin
          LPath.rLineTo(0, LYradius);
          if (aDrawOnlyBorder) and
             (TSide.left in FSides) and
             (not (TSide.bottom in FSides)) then begin
            LPath.rLineTo(0, LHalfStrokeThickness);
            LPath.rMoveTo(0, -LHalfStrokeThickness);
          end;
        end
        else LPath.rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in FSides) then begin
          if (aDrawOnlyBorder) and
             (TSide.bottom in FSides) and
             (not (TSide.left in FSides)) then begin
            LPath.rMoveTo(-LHalfStrokeThickness, 0);
            LPath.rLineTo(LHalfStrokeThickness, 0);
          end;
          LPath.rLineTo(LXRadius,0);
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in FSides) then LPath.rLineTo(LWidthMinusCorners, 0)
      else LPath.rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        LPath.rQuadTo(LXRadius, 0, LXRadius, -LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in FSides) then begin
          LPath.rLineTo(LXRadius,0);
          if (aDrawOnlyBorder) and
             (TSide.bottom in FSides) and
             (not (TSide.right in FSides)) then begin
            LPath.rLineTo(LHalfStrokeThickness, 0);
            LPath.rMoveTo(-LHalfStrokeThickness, 0);
          end;
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in FSides) then begin
          if (aDrawOnlyBorder) and
             (TSide.right in FSides) and
             (not (TSide.bottom in FSides)) then begin
            LPath.rMoveTo(0, LHalfStrokeThickness);
            LPath.rLineTo(0, -LHalfStrokeThickness);
          end;
          LPath.rLineTo(0,-LYradius);
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in FSides) then LPath.rLineTo(0, -LHeightMinusCorners)
      else LPath.rMoveTo(0, -LHeightMinusCorners);
      //-----
      // We cannot close the path because it would connect the last position
      // of a moveTo to the current endpoint. For example, if we want to skip the
      // bottom side (sides=[Top, Left, Right] and xRadius/yRadius > 0) by
      // performing a moveTo from the BottomLeft to the BottomRight,
      // closing the path when reaching the TopRight would incorrectly connect back
      // to the BottomRight instead of the desired TopRight position.
      //if (TSide.right in FSides) and
      //   (TSide.top in FSides) then LPath.close;


      if aPaint <> nil then ACanvas.drawPath(LPath,aPaint);
      if aClipPath then begin
        ACanvas.save;
        ACanvas.clipPath(LPath);
        {$IF defined(DEBUG)}
        if LPathClipped then
          raise Exception.Create('Error 2001FF24-310A-48D3-9D9C-34B8C3358130');
        {$ENDIF}
        LPathClipped := True;
      end;
      LPath := nil;

    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_DrawRect (APPLEOS)'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}
  procedure _DrawRect(
              const ACanvas: CGContextRef;
              const aGridHeight: Integer;
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean;
              const aClipPath: Boolean;
              const aNoRadius: Boolean);

  var
    LCurPoint: TpointF;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(const x: Single; const y: Single);
    begin
      CGContextMoveToPoint(ACanvas, X, AGridHeight - Y);
      LCurPoint.X := x;
      LCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(const dx1: Single; const dy1: Single; const dx2: Single; const dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(
        ACanvas,
        LCurPoint.X + dx1{cpx},
        AGridHeight - (LCurPoint.Y + dy1){cpy},
        LCurPoint.X + dx2{x},
        AGridHeight - (LCurPoint.Y + dy2){y});
      LCurPoint.X := LCurPoint.X + dx2;
      LCurPoint.Y := LCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(const dx: Single; const dy: Single);
    begin
      CGContextAddLineToPoint(ACanvas, LCurPoint.X + dx{x}, AGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rMoveTo(const dx: Single; const dy: Single);
    begin
      CGContextMoveToPoint(ACanvas, LCurPoint.X + dx{x}, AGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
    end;

  begin

    // Init LRect
    var LRect := aRect;

    // Init LScaledXRadius and LScaledYRadius
    var LScaledXRadius: Single;
    var LScaledYRadius: Single;
    if not aNoRadius then begin
      LScaledXRadius := FXRadius;
      LScaledYRadius := FYRadius;
      ALNormalizeAndScaleRadii(LScaledXRadius, LScaledYRadius, FScale, LRect);
    end
    else begin
      LScaledXRadius := 0;
      LScaledYRadius := 0;
    end;

    // use AddEllipseInRect
    if SameValue(LScaledXRadius, LRect.Width / 2, TEpsilon.position) and
       SameValue(LScaledYRadius, LRect.Height / 2, TEpsilon.position) and
       (FCorners = AllCorners) and
       (FSides=AllSides) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      if aClipPath then CGContextSaveGState(ACanvas);
      CGContextBeginPath(ACanvas);
      CGContextAddEllipseInRect(
        ACanvas,
        ALLowerLeftCGRect(
          LRect.TopLeft,
          LRect.Width,
          LRect.Height,
          AGridHeight));
      if aClipPath then CGContextClip(ACanvas)
      else if not aDrawOnlyBorder then CGContextFillPath(ACanvas)
      else CGContextStrokePath(ACanvas);

    end

    // use AddRect
    else if ((compareValue(LScaledXRadius, 0, TEpsilon.Position) = 0) or
             (compareValue(LScaledYRadius, 0, TEpsilon.position) = 0) or
             (FCorners=[])) and
            (FSides=AllSides) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      if aClipPath then CGContextSaveGState(ACanvas);
      CGContextBeginPath(ACanvas);
      CGContextAddRect(
        ACanvas,
        ALLowerLeftCGRect(
          LRect.TopLeft,
          LRect.Width,
          LRect.Height,
          AGridHeight));
      if aClipPath then CGContextClip(ACanvas)
      else if not aDrawOnlyBorder then CGContextFillPath(ACanvas)
      else CGContextStrokePath(ACanvas);

    end

    // use drawPath
    else begin

      if aClipPath then CGContextSaveGState(ACanvas);
      CGContextBeginPath(ACanvas);
      //--
      var LHalfStrokeThickness: Single := 0;
      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := LRect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if (aDrawOnlyBorder) or
           ((LRectIsEqualsToStrokeRect) and
            (LShadowcolor = TalphaColorRec.Null) and
            (FCorners=AllCorners) and
            (FSides=AllSides)) then begin
          LHalfStrokeThickness := LScaledStrokeThickness / 2;
          LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
        end
        else if (LRectIsEqualsToStrokeRect) and
                (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) and
                (FCorners=AllCorners) and
                (FSides=AllSides) then begin
          LHalfStrokeThickness := 1;
          LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
        end;
      end;
      //--
      var LXRadius: single := LScaledXRadius;
      var LYradius: single := LScaledYRadius;
      if (LXRadius > LRect.width / 2) then LXRadius := LRect.width / 2;
      if (LYradius > LRect.height / 2) then LYradius := LRect.height / 2;
      //--
      var LCorners: TCorners;
      if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := FCorners
      else LCorners := [];
      //--
      If aDrawOnlyBorder then begin
        If (TCorner.TopRight in LCorners)    and (not (TSide.Top in FSides))    and (not (Tside.Right in FSides)) then LCorners := LCorners - [TCorner.TopRight];
        If (TCorner.TopLeft in LCorners)     and (not (TSide.Top in FSides))    and (not (Tside.Left in FSides))  then LCorners := LCorners - [TCorner.TopLeft];
        If (TCorner.BottomRight in LCorners) and (not (TSide.Bottom in FSides)) and (not (Tside.Right in FSides)) then LCorners := LCorners - [TCorner.BottomRight];
        If (TCorner.BottomLeft in LCorners)  and (not (TSide.Bottom in FSides)) and (not (Tside.Left in FSides))  then LCorners := LCorners - [TCorner.BottomLeft];
      end;
      //--
      var LWidthMinusCorners: single := (LRect.width - (2 * LXRadius));
      var LHeightMinusCorners: single := (LRect.height - (2 * LYradius));


      //----- TopRight
      if (TCorner.TopRight in LCorners) then begin
        _moveTo(LRect.right, LRect.top + LYradius);
        _rQuadTo(0, -LYradius, -LXRadius, -LYradius);
      end
      else begin
        _moveTo(LRect.right, LRect.top + LYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in FSides) then begin
          _rLineTo(0, -LYradius);
          if (aDrawOnlyBorder) and
             (TSide.right in FSides) and
             (not (TSide.top in FSides)) then begin
            _rLineTo(0, -LHalfStrokeThickness);
            _rMoveTo(0, LHalfStrokeThickness);
          end;
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in FSides) then begin
          if (aDrawOnlyBorder) and
             (TSide.top in FSides) and
             (not (TSide.right in FSides)) then begin
            _rMoveTo(LHalfStrokeThickness, 0);
            _rLineTo(-LHalfStrokeThickness, 0);
          end;
          _rLineTo(-LXRadius,0);
        end
        else _rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in FSides) then _rLineTo(-LWidthMinusCorners, 0)
      else _rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        _rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in FSides) then begin
          _rLineTo(-LXRadius, 0);
          if (aDrawOnlyBorder) and
             (TSide.top in FSides) and
             (not (TSide.left in FSides)) then begin
            _rLineTo(-LHalfStrokeThickness, 0);
            _rMoveTo(LHalfStrokeThickness, 0);
          end;
        end
        else _rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in FSides) then begin
          if (aDrawOnlyBorder) and
             (TSide.left in FSides) and
             (not (TSide.top in FSides)) then begin
            _rMoveTo(0, -LHalfStrokeThickness);
            _rLineTo(0, LHalfStrokeThickness);
          end;
          _rLineTo(0,LYradius);
        end
        else _rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in FSides) then _rLineTo(0, LHeightMinusCorners)
      else _rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        _rQuadTo(0, LYradius, LXRadius, LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in FSides) then begin
          _rLineTo(0, LYradius);
          if (aDrawOnlyBorder) and
             (TSide.left in FSides) and
             (not (TSide.bottom in FSides)) then begin
            _rLineTo(0, LHalfStrokeThickness);
            _rMoveTo(0, -LHalfStrokeThickness);
          end;
        end
        else _rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in FSides) then begin
          if (aDrawOnlyBorder) and
             (TSide.bottom in FSides) and
             (not (TSide.left in FSides)) then begin
            _rMoveTo(-LHalfStrokeThickness, 0);
            _rLineTo(LHalfStrokeThickness, 0);
          end;
          _rLineTo(LXRadius,0);
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in FSides) then _rLineTo(LWidthMinusCorners, 0)
      else _rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        _rQuadTo(LXRadius, 0, LXRadius, -LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in FSides) then begin
          _rLineTo(LXRadius,0);
          if (aDrawOnlyBorder) and
             (TSide.bottom in FSides) and
             (not (TSide.right in FSides)) then begin
            _rLineTo(LHalfStrokeThickness, 0);
            _rMoveTo(-LHalfStrokeThickness, 0);
          end;
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in FSides) then begin
          if (aDrawOnlyBorder) and
             (TSide.right in FSides) and
             (not (TSide.bottom in FSides)) then begin
            _rMoveTo(0, LHalfStrokeThickness);
            _rLineTo(0, -LHalfStrokeThickness);
          end;
          _rLineTo(0,-LYradius);
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in FSides) then _rLineTo(0, -LHeightMinusCorners)
      else _rMoveTo(0, -LHeightMinusCorners);
      //-----
      // We cannot close the path because it would connect the last position
      // of a moveTo to the current endpoint. For example, if we want to skip the
      // bottom side (sides=[Top, Left, Right] and xRadius/yRadius > 0) by
      // performing a moveTo from the BottomLeft to the BottomRight,
      // closing the path when reaching the TopRight would incorrectly connect back
      // to the BottomRight instead of the desired TopRight position.
      //if (TSide.right in FSides) and
      //   (TSide.top in FSides) then CGContextClosePath(ACanvas);


      if aClipPath then CGContextClip(ACanvas)
      else if not aDrawOnlyBorder then CGContextFillPath(ACanvas)
      else CGContextStrokePath(ACanvas);

    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_SetShadow (SKIA)'}
  {$IF defined(ALSkiaEngine)}
  procedure _SetShadow(const aPaint: sk_Paint_t);
  begin
    //In skia we can use this to convert from the (legacy) idea of specify
    //the blur "radius" to the standard notion of specifying its sigma.
    //  static const SkScalar kBLUR_SIGMA_SCALE = 0.57735f;
    //  SkScalar SkBlurMask::ConvertRadiusToSigma(SkScalar radius) {
    //     return radius > 0 ? 0.57735f * radius + 0.5f : 0.0f;
    //  }
    //But it's not very good, I think that a better value is just Shadow.blur / 2
    if (LShadowColor <> TalphaColorRec.Null) then begin
      var LImagefilter := ALSkCheckHandle(
                            sk4d_imagefilter_make_drop_shadow(
                              LScaledShadowOffsetX, // dx,
                              LScaledShadowOffsetY, // dy,
                              LScaledShadowBlur / 2, // sigma_x,
                              LScaledShadowBlur / 2, // sigma_y: float;
                              LShadowColor, // color: sk_color_t;
                              0, // input: sk_imagefilter_t;
                              nil)); // const crop_rect: psk_rect_t)
      try
        // Sets SkImageFilter to imageFilter, decreasing SkRefCnt of the previous SkImageFilter. Increments imageFilter SkRefCnt by one.
        sk4d_paint_set_image_filter(aPaint, LImagefilter);
      finally
        sk4d_refcnt_unref(LImagefilter);
      end;
    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_SetShadow (ANDROID)'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}
  procedure _SetShadow(const aPaint: JPaint);
  begin
    if LShadowColor <> TalphaColorRec.Null then
      APaint.setShadowLayer(
        LScaledShadowBlur{radius},
        LScaledShadowOffsetX{dx},
        LScaledShadowOffsetY{dy},
        integer(LShadowColor){shadowColor});
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_SetShadow (APPLEOS)'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}
  procedure _SetShadow(const ACanvas: CGContextRef);
  begin
    if LShadowColor <> TalphaColorRec.Null then begin
      var LShadowColorF := TAlphaColorCGFloat.Create(LShadowColor);
      var LShadowColorCG := CGColorCreate(ALGetGlobalCGColorSpace, @LShadowColorF);
      try
        CGContextSetShadowWithColor(
          ACanvas,
          CGSizeMake(LScaledShadowOffsetX, -LScaledShadowOffsetY), // offset
          LScaledShadowBlur, // blur
          LShadowColorCG); // color
      finally
        CGColorRelease(LShadowColorCG);
      end;
    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_ClearShadow (SKIA)'}
  {$IF defined(ALSkiaEngine)}
  procedure _ClearShadow(const aPaint: sk_Paint_t);
  begin
    if LShadowColor <> TalphaColorRec.Null then
      sk4d_paint_set_image_filter(aPaint, 0);
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_ClearShadow (ANDROID)'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}
  procedure _ClearShadow(const aPaint: JPaint);
  begin
      if LShadowColor <> TalphaColorRec.Null then
        APaint.clearShadowLayer;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_ClearShadow (APPLEOS)'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}
  procedure _ClearShadow(const ACanvas: CGContextRef);
  begin
    if LShadowColor <> TalphaColorRec.Null then
      CGContextSetShadowWithColor(
        ACanvas,
        CGSizeMake(0, 0), // offset
        0, // blur
        nil); // color
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_FillGradientIsTransparent'}
  function _FillGradientIsTransparent: boolean;
  begin
    if length(LFillGradientColors) = 0 then exit(true);
    Result := False;
    for var I := Low(LFillGradientColors) to High(LFillGradientColors) do begin
      Result := TAlphaColorRec(LFillGradientColors[i]).A < 255;
      if Result then exit;
    end;
  end;
  {$ENDREGION}

  {$REGION '_DrawStateLayer'}
  procedure _DrawStateLayer;
  begin
    if LStateLayerColor = TAlphaColors.Null then exit;

    var LScaledStateLayerXRadius: Single := FStateLayerXRadius;
    var LScaledStateLayerYRadius: Single := FStateLayerYRadius;
    if LScaledStateLayerXRadius > 0 then LScaledStateLayerXRadius := LScaledStateLayerXRadius * FScale;
    if LScaledStateLayerYRadius > 0 then LScaledStateLayerYRadius := LScaledStateLayerYRadius* FScale;

    TALDrawRectangleHelper.Create(FCanvas)
      .SetAlignToPixel(FAlignToPixel)
      .SetDstRect(LScaledStateLayerDstRect)
      .SetOpacity(FStateLayerOpacity)
      .SetFillColor(LStateLayerColor)
      .SetXRadius(LScaledStateLayerXRadius)
      .SetYRadius(LScaledStateLayerYRadius)
      .Draw;
  end;
  {$ENDREGION}

begin

  if CompareValue(FOpacity, 0, TEpsilon.Scale) <= 0 then exit;
  //--
  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if FAlignToPixel then ALExtractMatrixFromCanvas(FCanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;
  //--
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}
  var LGridHeight := CGBitmapContextGetHeight(FCanvas);
  {$ENDIF}
  //--
  LScaledDstRect := FDstRect;
  LScaledDstRect.Top := LScaledDstRect.Top * FScale;
  LScaledDstRect.right := LScaledDstRect.right * FScale;
  LScaledDstRect.left := LScaledDstRect.left * FScale;
  LScaledDstRect.bottom := LScaledDstRect.bottom * FScale;
  if FAlignToPixel then
    LScaledDstRect := ALAlignToPixelRound(LScaledDstRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);
  //--
  LScaledStrokeThickness := FStrokeThickness * FScale;
  if FAlignToPixel then
    LScaledStrokeThickness := ALAlignDimensionToPixelRound(LScaledStrokeThickness, LCanvasScale, TEpsilon.Position);
  //--
  {$IF (defined(ANDROID)) or (defined(ALAppleOS)) or (defined(ALSkiaEngine))}
  LScaledShadowBlur := FShadowBlur * FScale;
  LScaledShadowOffsetX := FShadowOffsetX * FScale;
  LScaledShadowOffsetY := FShadowOffsetY * FScale;
  if FAlignToPixel then begin
    LScaledShadowBlur := ALAlignDimensionToPixelRound(LScaledShadowBlur, LCanvasScale, Tepsilon.Vector);
    LScaledShadowOffsetX := ALAlignDimensionToPixelRound(LScaledShadowOffsetX, LCanvasScale, TEpsilon.Position);
    LScaledShadowOffsetY := ALAlignDimensionToPixelRound(LScaledShadowOffsetY, LCanvasScale, TEpsilon.Position);
  end;
  LShadowColor := FShadowColor;
  if CompareValue(LScaledShadowBlur, 0, TEpsilon.position) <= 0 then
    LShadowColor := TAlphaColors.Null;
  {$ENDIF}
  //--
  LStrokeColor := FStrokeColor;
  if CompareValue(LScaledStrokeThickness, 0, TEpsilon.position) <= 0 then
    LStrokeColor := TAlphaColors.Null;
  //--
  if length(FFillGradientColors) = 1 then
    raise Exception.Create('Invalid gradient: A gradient requires at least two colors');
  var LFillGradientOffsets := FFillGradientOffsets;
  if (length(LFillGradientOffsets) = 0) and (length(FFillGradientColors) > 0) then begin
    setlength(LFillGradientOffsets, length(FFillGradientColors));
    for Var I := 0 to length(FFillGradientColors) - 1 do
      LFillGradientOffsets[i] := I * (1 / (length(FFillGradientColors) - 1));
  end
  else if (length(LFillGradientOffsets) <> length(FFillGradientColors)) then
    raise Exception.Create('Invalid gradient: The number of gradient offsets does not match the number of gradient colors');
  //--
  var LScaledFillGradientStartPoint := FFillGradientStartPoint;
  LScaledFillGradientStartPoint.X := LScaledFillGradientStartPoint.X * FScale;
  LScaledFillGradientStartPoint.Y := LScaledFillGradientStartPoint.Y * FScale;
  var LScaledFillGradientEndPoint := FFillGradientEndPoint;
  LScaledFillGradientEndPoint.X := LScaledFillGradientEndPoint.X * FScale;
  LScaledFillGradientEndPoint.Y := LScaledFillGradientEndPoint.Y * FScale;
  //--
  var LScaledFillBackgroundMarginsRect: TRectF;
  if (FFillColor <> TalphaColorRec.Null) or
     (length(FFillGradientColors) > 0) then begin
    LScaledFillBackgroundMarginsRect := FFillBackgroundMarginsRect;
    LScaledFillBackgroundMarginsRect.Top := LScaledFillBackgroundMarginsRect.Top * FScale;
    LScaledFillBackgroundMarginsRect.right := LScaledFillBackgroundMarginsRect.right * FScale;
    LScaledFillBackgroundMarginsRect.left := LScaledFillBackgroundMarginsRect.left * FScale;
    LScaledFillBackgroundMarginsRect.bottom := LScaledFillBackgroundMarginsRect.bottom * FScale;
    if FAlignToPixel then
      LScaledFillBackgroundMarginsRect := ALAlignEdgesToPixelRound(LScaledFillBackgroundMarginsRect, LCanvasScale, TEpsilon.Position);
  end
  else
    LScaledFillBackgroundMarginsRect := TRectF.Empty;
  //--
  var LFillResourceStream := FFillResourceStream;
  var LFillResourceName := FFillResourceName;
  if ALIsHttpOrHttpsUrl(LFillResourceName) then LFillResourceName := '';
  var LFillWithImage := (LFillResourceName <> '') or (LFillResourceStream <> nil);
  //--
  var LScaledFillImageMarginsRect: TRectF;
  if LFillWithImage then begin
    LScaledFillImageMarginsRect := FFillImageMarginsRect;
    LScaledFillImageMarginsRect.Top := LScaledFillImageMarginsRect.Top * FScale;
    LScaledFillImageMarginsRect.right := LScaledFillImageMarginsRect.right * FScale;
    LScaledFillImageMarginsRect.left := LScaledFillImageMarginsRect.left * FScale;
    LScaledFillImageMarginsRect.bottom := LScaledFillImageMarginsRect.bottom * FScale;
    if FAlignToPixel then
      LScaledFillImageMarginsRect := ALAlignEdgesToPixelRound(LScaledFillImageMarginsRect, LCanvasScale, TEpsilon.Position);
  end
  else
    LScaledFillImageMarginsRect := TRectF.Empty;
  //--
  var LScaledBackgroundDstRect := LScaledDstRect;
  LScaledBackgroundDstRect.Inflate(-LScaledFillBackgroundMarginsRect.Left, -LScaledFillBackgroundMarginsRect.Top, -LScaledFillBackgroundMarginsRect.Right, -LScaledFillBackgroundMarginsRect.Bottom);
  if (CompareValue(LScaledBackgroundDstRect.Width, 0, TEpsilon.Position) <= 0) or
     (CompareValue(LScaledBackgroundDstRect.height, 0, TEpsilon.Position) <= 0) then
    LScaledBackgroundDstRect := TRectf.Empty;
  //--
  var LScaledImageDstRect := LScaledDstRect;
  LScaledImageDstRect.Inflate(-LScaledFillImageMarginsRect.Left, -LScaledFillImageMarginsRect.Top, -LScaledFillImageMarginsRect.Right, -LScaledFillImageMarginsRect.Bottom);
  if (CompareValue(LScaledImageDstRect.Width, 0, TEpsilon.Position) <= 0) or
     (CompareValue(LScaledImageDstRect.height, 0, TEpsilon.Position) <= 0) then
    LScaledImageDstRect := TRectf.Empty;
  //--
  var LFillColor := FFillColor;
  LFillGradientColors := FFillGradientColors;
  if LScaledBackgroundDstRect.IsEmpty then begin
    LFillColor := TALphaColors.Null;
    setlength(LFillGradientColors, 0);
    setlength(LFillGradientOffsets, 0);
  end;
  //--
  if (CompareValue(FFillGradientAngle, 360, TEpsilon.Angle) <= 0) and (CompareValue(FFillGradientAngle, -360, TEpsilon.Angle) > 0) then begin
    if (length(LFillGradientColors) > 0) then begin
      case FFillGradientStyle of
        TGradientStyle.Linear: begin
          ALGetLinearGradientCoordinates(
            LScaledBackgroundDstRect.Size, // const ASize: TSizeF;
            FFillGradientAngle, // const AAngle: Single;
            LScaledFillGradientStartPoint, // out AStartPoint: TPointF;
            LScaledFillGradientEndPoint); // out AEndPoint: TPointF;
        end;
        TGradientStyle.Radial: begin
          LScaledFillGradientStartPoint := LScaledBackgroundDstRect.CenterPoint;
          LScaledFillGradientEndPoint := LScaledBackgroundDstRect.TopLeft;
        end
        else
          Raise Exception.Create('Error 69B128A0-83FC-4FF1-AC60-3BDF6044258D')
      end;
    end
    else begin
      LScaledFillGradientStartPoint := TPointF.Zero;
      LScaledFillGradientEndPoint := TPointF.Zero;
    end;
  end;
  //--
  if LScaledImageDstRect.IsEmpty then begin
    LFillResourceName := '';
    LFillResourceStream := nil;
  end;
  LFillWithImage := (LFillResourceName <> '') or (LFillResourceStream <> nil);
  //--
  var LScaledStateLayerMarginsRect: TRectF;
  if (FStateLayerColor <> TalphaColorRec.Null) and
     (CompareValue(FStateLayerOpacity, 0, TEpsilon.Scale) > 0) then begin
    LScaledStateLayerMarginsRect := FStateLayerMarginsRect;
    LScaledStateLayerMarginsRect.Top := LScaledStateLayerMarginsRect.Top * FScale;
    LScaledStateLayerMarginsRect.right := LScaledStateLayerMarginsRect.right * FScale;
    LScaledStateLayerMarginsRect.left := LScaledStateLayerMarginsRect.left * FScale;
    LScaledStateLayerMarginsRect.bottom := LScaledStateLayerMarginsRect.bottom * FScale;
    if FAlignToPixel then
      LScaledStateLayerMarginsRect := ALAlignEdgesToPixelRound(LScaledStateLayerMarginsRect, LCanvasScale, TEpsilon.Position);
  end
  else
    LScaledStateLayerMarginsRect := TRectF.Empty;
  //--
  LScaledStateLayerDstRect := LScaledDstRect;
  LScaledStateLayerDstRect.Inflate(-LScaledStateLayerMarginsRect.Left, -LScaledStateLayerMarginsRect.Top, -LScaledStateLayerMarginsRect.Right, -LScaledStateLayerMarginsRect.Bottom);
  if (CompareValue(LScaledStateLayerDstRect.Width, 0, TEpsilon.Position) <= 0) or
     (CompareValue(LScaledStateLayerDstRect.height, 0, TEpsilon.Position) <= 0) then
    LScaledStateLayerDstRect := TRectf.Empty;
  //--
  LStateLayerColor := FStateLayerColor;
  if (LScaledStateLayerDstRect.IsEmpty) or
     (CompareValue(FStateLayerOpacity, 0, TEpsilon.Scale) <= 0) then
    LStateLayerColor := TALphaColors.Null;
  //--
  if (LStateLayerColor <> TALphaColors.Null) and
     (CompareValue(FStateLayerOpacity, 0, TEpsilon.Scale) > 0) and
     (LScaledStateLayerDstRect.EqualsTo(LScaledBackgroundDstRect)) and
     (LScaledStateLayerDstRect.EqualsTo(LScaledDstRect)) and
     (sameValue(FStateLayerXRadius, FXRadius, TEpsilon.Vector)) and
     (sameValue(FStateLayerYRadius, FYRadius, TEpsilon.Vector)) then begin
    LFillColor := ALblendColor(LfillColor, LStateLayerColor, FStateLayerOpacity);
    if (FDrawStateLayerOnTop) and
       (LStrokeColor <> TAlphaColors.Null) then
      LStrokeColor := ALblendColor(LStrokeColor, LStateLayerColor, FStateLayerOpacity);
    LStateLayerColor := TALphaColors.Null;
  end;
  //--
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}
  LPathClipped := False;
  {$ENDIF}

  {$REGION 'SKIA'}
  {$IF defined(ALSkiaEngine)}

  // Create the alpha layer
  if compareValue(FOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LFillResourceStream, // const AFillResourceStream: TStream;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        FStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(FCanvas, LLayerRect, FOpacity);
  end;
  try

    // Create LPaint
    var LPaint := ALSkCheckHandle(sk4d_paint_create);
    try

      // Requests, but does not require, that edge pixels draw opaque or with partial transparency.
      sk4d_paint_set_antialias(LPaint, true);
      // Requests, but does not require, to distribute color error.
      sk4d_paint_set_dither(LPaint, true);

      // Fill the rectangle
      if (LFillColor <> TalphaColorRec.Null) or
         (length(LFillGradientColors) > 0) or
         (LFillWithImage) or
         (LShadowColor <> TalphaColorRec.Null) then begin

        // FILL_SK_PAINTSTYLE
        sk4d_paint_set_style(LPaint, sk_paintstyle_t.FILL_SK_PAINTSTYLE);

        //Fill with bitmap
        //if FFill.Kind = TALBrushKind.Bitmap then begin
        //  if FFill.Bitmap.Bitmap.HandleAllocated then begin
        //    var LBitmapData: TBitmapData;
        //    if FFill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then begin
        //      try
        //        var LImage: sk_image_t;
        //        var LImageInfo := ALGetSkImageinfo(LBitmapData.Width, LBitmapData.Height, sk_colortype_t(SkFmxColorType[LBitmapData.PixelFormat]));
        //        var LPixmap := ALSkCheckHandle(
        //                         sk4d_pixmap_create(
        //                           @LImageInfo, // const image_info: psk_imageinfo_t;
        //                           LBitmapData.Data, // const pixels: Pointer;
        //                           LBitmapData.Pitch)); // row_bytes: size_t
        //        try
        //          LImage := ALSkCheckHandle(
        //                      sk4d_image_make_from_raster(
        //                        LPixmap, // const pixmap: sk_pixmap_t;
        //                        nil, // proc: sk_image_raster_release_proc;
        //                        nil));// proc_context: Pointer
        //          try
        //            ...
        //          finally
        //            sk4d_refcnt_unref(LImage);
        //          end;
        //        finally
        //          sk4d_refcnt_unref(LPixmap);
        //        end;
        //      finally
        //        FFill.Bitmap.Bitmap.Unmap(LBitmapData);
        //      end
        //    end;
        //  end;
        //end

        // Init LDrawnWithSolidColor
        var LDrawnWithSolidColor := False;

        // Fill with transparent solid color and shadow
        if (LShadowColor <> TalphaColorRec.Null) and // If null, skip drawing the shadow
           (TAlphaColorRec(LFillColor).A < 255) and // Else, fill with solid color and shadow in one pass
           (_FillGradientIsTransparent) and // Else, fill with gradient and shadow in one pass
           ((not LFillWithImage) or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
            (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
            (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
            (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

          // First pass draw the shadow
          sk4d_paint_set_color(LPaint, ALSetColorAlpha(LFillColor, 1{AOpacity}));
          _SetShadow(LPaint);
          _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, False{aNoRadius});
          _ClearShadow(LPaint);

          // Second pass fill the rect
          // NOTE: The blend mode will not function correctly if we draw directly on the form
          // and LFillColor has transparency, as the form itself does not support transparency.
          var LBlender := ALSkCheckHandle(
                            sk4d_blender_make_mode(
                              sk_blendmode_t.SRC_SK_BLENDMODE));
          try
            sk4d_paint_set_blender(LPaint, LBlender);
            sk4d_paint_set_color(LPaint, LFillColor);
            _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, False{aNoRadius});
            sk4d_paint_set_blender(LPaint, 0);
          finally
            sk4d_refcnt_unref(LBlender)
          end;
          LDrawnWithSolidColor := True;

        end

        // Fill with solid color
        else if (LFillColor <> TalphaColors.Null) then begin
          sk4d_paint_set_color(LPaint, LFillColor);
          _SetShadow(LPaint);
          _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, False{aNoRadius});
          _ClearShadow(LPaint);
          LDrawnWithSolidColor := True;
        end;

        // Fill with gradient
        if length(LFillGradientColors) > 0 then begin
          case FFillGradientStyle of
            TGradientStyle.Linear: begin
              var LPoints: array[0..1] of sk_point_t;
              LPoints[0].x := LScaledFillGradientStartPoint.X;
              LPoints[0].y := LScaledFillGradientStartPoint.Y;
              LPoints[1].x := LScaledFillGradientEndPoint.X;
              LPoints[1].y := LScaledFillGradientEndPoint.Y;
              var LShader := ALSkCheckHandle(
                               sk4d_shader_make_gradient_linear(
                                 @LPoints[0], // const points: psk_point_t;
                                 @LFillGradientColors[0], // const colors: psk_color_t;
                                 @LFillGradientOffsets[0], // const positions: pfloat;
                                 Length(LFillGradientColors), // count: int32_t;
                                 sk_tilemode_t.CLAMP_SK_TILEMODE, // tile_mode: sk_tilemode_t;
                                 nil)); // const local_matrix: psk_matrix_t
              try
                // Sets SkShader to shader, decreasing SkRefCnt of the previous SkShader. Increments shader SkRefCnt by one.
                sk4d_paint_set_shader(LPaint, LShader);
              finally
                sk4d_refcnt_unref(LShader);
              end;
            end;
            TGradientStyle.Radial: begin
              var LShader := ALSkCheckHandle(
                               sk4d_shader_make_gradient_radial(
                                 @LScaledFillGradientStartPoint, // const center: psk_point_t;
                                 LScaledFillGradientStartPoint.Distance(LScaledFillGradientEndPoint), // radius: float;
                                 @LFillGradientColors[0], // const colors: psk_color_t;
                                 @LFillGradientOffsets[0], // const positions: pfloat;
                                 Length(LFillGradientColors), // count: int32_t;
                                 sk_tilemode_t.CLAMP_SK_TILEMODE, // tile_mode: sk_tilemode_t;
                                 nil));// const local_matrix: psk_matrix_t
              try
                // Sets SkShader to shader, decreasing SkRefCnt of the previous SkShader. Increments shader SkRefCnt by one.
                sk4d_paint_set_shader(LPaint, LShader);
              finally
                sk4d_refcnt_unref(LShader);
              end;
            end;
            else
              raise Exception.Create('Error EADEA97C-5440-46F0-A44A-47BCF4CFAC2F');
          end;
          // Specify an opaque color to ensure the gradient is drawn without the
          // transparency effect of the current color.
          sk4d_paint_set_color(LPaint, TAlphaColors.White);
          if not LDrawnWithSolidColor then _SetShadow(LPaint);
          _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, False{aNoRadius});
          if not LDrawnWithSolidColor then _ClearShadow(LPaint);
          sk4d_paint_set_shader(LPaint, 0);
          LDrawnWithSolidColor := True;
        end;

        // Draw the StateLayer
        if not FDrawStateLayerOnTop then
          _DrawStateLayer;

        // Fill with image
        if LFillWithImage then begin
          var LImage: sk_image_t;
          {$IFDEF ALDPK}
          if (LFillResourceStream = nil) and (ALGetResourceFilename(LFillResourceName) = '') then
            LImage := 0
          else
          try
          {$ENDIF}
            LImage := ALCreateSkImageFromResource(
                        LFillResourceName, // const AResourceName: String;
                        LFillResourceStream, // const AResourceStream: TStream;
                        FFillMaskResourceName, // const AMaskResourceName: String;
                        FFillMaskBitmap, // const AMaskImage: sk_image_t;
                        1, // const AScale: Single;
                        LScaledImageDstRect.Width, LScaledImageDstRect.Height, // const W, H: single;
                        FFillWrapMode, // const AWrapMode: TALImageWrapMode;
                        FFillCropCenter, // const ACropCenter: TpointF;
                        FFillBlurRadius * FScale, // const ABlurRadius: single;
                        0, // const AXRadius: Single;
                        0); // const AYRadius: Single);
          {$IFDEF ALDPK}
          except
            LImage := 0;
          end;
          If LImage <> 0 then
          {$ENDIF}
            try

              var LDestRect := TrectF.Create(0,0, sk4d_image_get_width(LImage), sk4d_image_get_Height(LImage)).CenterAt(LScaledImageDstRect);
              var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
              var Lshader: sk_shader_t;
              var LMatrix := TMatrix.CreateTranslation(LScaledImageDstRect.Left + LDestRect.Left - LScaledImageDstRect.Left, LScaledImageDstRect.Top + LDestRect.Top - LScaledImageDstRect.Top);
              Lshader := ALSkCheckHandle(
                           sk4d_image_make_shader(
                             LImage, // const self: sk_image_t;
                             sk_tilemode_t.DECAL_SK_TILEMODE, // tile_mode_x,
                             sk_tilemode_t.DECAL_SK_TILEMODE,// tile_mode_y: sk_tilemode_t;
                             @LSamplingoptions, // const sampling: psk_samplingoptions_t;
                             @LMatrix)); // const local_matrix: psk_matrix_t)
              try
                // Sets SkShader to shader, decreasing SkRefCnt of the previous SkShader. Increments shader SkRefCnt by one.
                sk4d_paint_set_shader(LPaint, LShader);
              finally
                sk4d_refcnt_unref(LShader);
              end;

              // Specify an opaque color to ensure the image is drawn without the
              // transparency effect of the current color.
              sk4d_paint_set_color(LPaint, TAlphaColors.White);
              if not LDrawnWithSolidColor then _SetShadow(LPaint);
              _DrawRect(FCanvas, LPaint, LScaledImageDstRect, false{aDrawOnlyBorder}, FFillImageNoRadius{aNoRadius});
              if not LDrawnWithSolidColor then _ClearShadow(LPaint);
              sk4d_paint_set_shader(LPaint, 0);

            finally
              sk4d_refcnt_unref(LImage);
            end;
        end;

      end

      // Draw the StateLayer
      else if not FDrawStateLayerOnTop then
        _DrawStateLayer;

      // Stroke the rectangle
      if LStrokeColor <> TalphaColorRec.Null then begin
        sk4d_paint_set_style(LPaint, sk_paintstyle_t.STROKE_SK_PAINTSTYLE);
        sk4d_paint_set_stroke_width(LPaint, LScaledStrokeThickness);
        sk4d_paint_set_color(LPaint, LStrokeColor);
        _DrawRect(FCanvas, LPaint, LScaledDstRect, true{aDrawOnlyBorder}, False{aNoRadius});
      end;

      // Draw the StateLayer
      if FDrawStateLayerOnTop then
        _DrawStateLayer;

    finally
      sk4d_paint_destroy(LPaint);
    end;

  finally
    // Remove the alpha layer
    if compareValue(FOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(FCanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ANDROID'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

  // Create the alpha layer
  if compareValue(FOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LFillResourceStream, // const AFillResourceStream: TStream;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        FStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(FCanvas, LLayerRect, FOpacity);
  end;
  try

    //create the canvas and the paint
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    // Fill the rectangle
    if (LFillColor <> TalphaColorRec.Null) or
       (length(LFillGradientColors) > 0) or
       (LFillWithImage) or
       (LShadowColor <> TalphaColorRec.Null) then begin

      //init LPaint
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LScaledDstRect + StrokeThickness :( this result&ing in border if the fill is for exemple black and border white

      //fill with bitmap
      //if FFill.Kind = TALBrushKind.Bitmap then begin
      //  if not FFill.Bitmap.Bitmap.IsEmpty then begin
      //    if FFill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
      //      //--
      //      var LTmpBitmap := TJBitmap.JavaClass.createBitmap(FFill.Bitmap.Bitmap.Width, FFill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);
      //      //--
      //      var LPixelBuffer: Pointer;
      //      var LBitmapInfo: AndroidBitmapInfo;
      //      FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
      //      if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LBitmapInfo) = 0) and
      //         (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LPixelBuffer) = 0) then
      //      try
      //        var LBitmapData: TBitmapData;
      //        if FFill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
      //        try
      //          ALMove(LBitmapData.Data^, LPixelBuffer^, LBitmapData.Pitch * LBitmapData.Height);
      //        finally
      //          FFill.Bitmap.Bitmap.Unmap(LBitmapData);
      //        end;
      //      finally
      //        AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID);
      //      end;
      //      //--
      //      ...
      //      //--
      //      LTmpBitmap.recycle;
      //      LTmpBitmap := nil;
      //      //--
      //    end;
      //  end;
      //end

      // Init LDrawnWithSolidColor
      var LDrawnWithSolidColor := False;

      // Fill with transparent solid color and shadow
      if (LShadowColor <> TalphaColorRec.Null) and // If null, skip drawing the shadow
         (TAlphaColorRec(LFillColor).A < 255) and // Else, fill with solid color and shadow in one pass
         (_FillGradientIsTransparent) and // Else, fill with gradient and shadow in one pass
         ((not LFillWithImage) or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
          (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
          (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
          (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

        // First pass draw the shadow
        LPaint.setColor(integer(ALSetColorAlpha(LFillColor, 1{AOpacity})));
        _SetShadow(LPaint);
        _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillWithImage{aForceDrawPath}, false{aClipPath}, False{aNoRadius});
        _ClearShadow(LPaint);

        // Second pass fill the rect
        var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC);
        LPaint.setXfermode(LPorterDuffXfermode);
        LPaint.setColor(integer(LFillColor));
        _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillWithImage{aForceDrawPath}, (LFillWithImage) and (length(LFillGradientColors) = 0) and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath}, False{aNoRadius});
        LPaint.setXfermode(nil);
        LPorterDuffXfermode := nil;
        LDrawnWithSolidColor := True;

      end

      // Fill with solid color
      else if (LFillColor <> TalphaColors.Null) then begin
        LPaint.setColor(integer(LFillColor));
        _SetShadow(LPaint);
        _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillWithImage{aForceDrawPath}, (LFillWithImage) and (length(LFillGradientColors) = 0) and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath}, False{aNoRadius});
        _ClearShadow(LPaint);
        LDrawnWithSolidColor := True;
      end;

      //fill with gradient
      if length(LFillGradientColors) > 0 then begin

        // First pass draw the shadow if not already drawn
        // We must do this because else the shadow will be drawn with
        // the color of the gradient :(
        if (LShadowColor <> TalphaColorRec.Null) and (not LDrawnWithSolidColor) then begin
          LPaint.setColor(integer(ALSetColorAlpha(LShadowColor, 1{AOpacity})));
          _SetShadow(LPaint);
          _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillWithImage{aForceDrawPath}, false{aClipPath}, False{aNoRadius});
          _ClearShadow(LPaint);
          // We must now remove the solid color in case the
          // background contains transparent colors.
          var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.CLEAR);
          LPaint.setXfermode(LPorterDuffXfermode);
          _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillWithImage{aForceDrawPath}, false{aClipPath}, False{aNoRadius});
          LPaint.setXfermode(nil);
          LPorterDuffXfermode := nil;
        end;

        // Second pass fill the rect
        var LJColors := TJavaArray<Integer>.Create(length(LFillGradientColors));
        var LJOffsets := TJavaArray<Single>.Create(length(LFillGradientOffsets));
        Try
          for var i := low(LFillGradientColors) to high(LFillGradientColors) do begin
            LJColors[i] := integer(LFillGradientColors[i]);
            LJOffsets[i] := LFillGradientOffsets[i];
          end;
          var LShader: JShader;
          case FFillGradientStyle of
            TGradientStyle.Linear: begin
              LShader := TJLinearGradient.JavaClass.init(
                           LScaledFillGradientStartPoint.X{x0},
                           LScaledFillGradientStartPoint.Y{y0},
                           LScaledFillGradientEndPoint.X{x1},
                           LScaledFillGradientEndPoint.Y{y1},
                           LJColors{colors},
                           LJOffsets{positions},
                           TJShader_TileMode.JavaClass.CLAMP{tile});
            end;
            TGradientStyle.Radial: begin
              LShader := TJRadialGradient.JavaClass.init(
                           LScaledFillGradientStartPoint.X{x},
                           LScaledFillGradientStartPoint.Y{y},
                           LScaledFillGradientStartPoint.Distance(LScaledFillGradientEndPoint){radius},
                           LJColors{colors},
                           LJOffsets{positions},
                           TJShader_TileMode.JavaClass.CLAMP{tile});
            end;
            else
              raise Exception.Create('Error EAB4DED3-CF02-495B-9CB8-8F82479D2839');
          end;
          // On android the gradient is drawed with the opacity of the paint color
          // so set the color to white to make the bitmap fully opaque
          LPaint.setColor(integer(TAlphaColors.White));
          LPaint.setShader(LShader);
          _DrawRect(FCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillWithImage{aForceDrawPath}, (LFillWithImage) and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath}, False{aNoRadius});
          LPaint.setShader(nil);
          LShader := nil;
          LDrawnWithSolidColor := True;
        finally
          ALfreeandNil(LJColors);
          ALfreeandNil(LJOffsets);
        end;
      end;

      // Draw the StateLayer
      if not FDrawStateLayerOnTop then
        _DrawStateLayer;

      // Fill with image
      if LFillWithImage then begin
        var LBitmap: JBitmap := ALCreateJBitmapFromResource(
                                  LFillResourceName, // const AResourceName: String;
                                  LFillResourceStream, // const AResourceStream: TStream;
                                  FFillMaskResourceName, // const AMaskResourceName: String;
                                  FFillMaskBitmap, // const AMaskBitmap: JBitmap;
                                  1, // const AScale: Single;
                                  LScaledImageDstRect.Width, LScaledImageDstRect.Height, // const W, H: single;
                                  FFillWrapMode, // const AWrapMode: TALImageWrapMode;
                                  FFillCropCenter, // const ACropCenter: TpointF;
                                  FFillBlurRadius * FScale, // const ABlurRadius: single;
                                  0, // const AXRadius: Single;
                                  0); // const AYRadius: Single);
        try

          // On android the bitmap is drawed with the opacity of the paint color
          // so set the color to white to make the bitmap fully opaque
          LPaint.setColor(integer(TAlphaColors.White));

          // The shadow is made directly on the bitmap
          if (not LDrawnWithSolidColor) and (LShadowColor <> TalphaColorRec.Null) then begin

            // Their is corners so remove them from the LBitmap
            if (not FFillImageNoRadius) and
               ((compareValue(FXRadius, 0, TEpsilon.Position) <> 0) or
                (compareValue(FYRadius, 0, TEpsilon.position) <> 0)) then begin
              // Draw the shape of the rect in LDestBitmap
              var LDestRect := TRectF.Create(0,0,LScaledImageDstRect.Width,LScaledImageDstRect.height);
              var LDestBitmap := TJBitmap.JavaClass.createBitmap(Round(LDestRect.Width), round(LDestRect.Height), TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);
              var LDestCanvas := TJCanvas.JavaClass.init(LDestBitmap);
              _DrawRect(LDestCanvas, LPaint, LDestRect, false{aDrawOnlyBorder}, False{aForceDrawPath}, False{aClipPath}, FFillImageNoRadius{aNoRadius});
              // Copy LBitmap in LSrcBitmap and extend LSrcBitmap to the size of LDestBitmap
              var LSrcBitmap := TJBitmap.JavaClass.createBitmap(LDestBitmap.getWidth, LDestBitmap.getHeight, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);
              var LSrcCanvas := TJCanvas.JavaClass.init(LSrcBitmap);
              LDestRect := TrectF.Create(0,0, LBitmap.getWidth, LBitmap.getheight).CenterAt(TRectF.Create(0,0,LDestBitmap.getWidth,LDestBitmap.getHeight));
              var LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
              var LJSrcRect := TJRect.JavaClass.init(0, 0, LBitmap.getWidth, LBitmap.getheight);
              LSrcCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
              // draw the LSrcBitmap onto the LDestBitmap with SRC_IN
              LJDestRectf := TJRectf.JavaClass.init(0, 0, LSrcBitmap.getWidth, LSrcBitmap.getheight);
              LJSrcRect := TJRect.JavaClass.init(0, 0, LSrcBitmap.getWidth, LSrcBitmap.getheight);
              var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
              LPaint.setXfermode(LPorterDuffXfermode);
              LDestCanvas.drawBitmap(LSrcBitmap, LJSrcRect, LJDestRectf, LPaint);
              LPaint.setXfermode(nil);
              LPorterDuffXfermode := nil;
              // swap LDestBitmap with LBitmap
              LJSrcRect := nil;
              LJDestRectf := nil;
              LSrcCanvas := nil;
              LDestCanvas := nil;
              if not LBitmap.equals(LDestBitmap) then LBitmap.recycle;
              if not LSrcBitmap.equals(LDestBitmap) then LSrcBitmap.recycle;
              LBitmap := LDestBitmap;
              LSrcBitmap := nil;
              LDestBitmap := nil;
            end;

            var LDestRect := TrectF.Create(0,0, LBitmap.getWidth, LBitmap.getheight).CenterAt(LScaledImageDstRect);
            LDestRect.Offset(LScaledShadowOffsetX, LScaledShadowOffsetY);
            var LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
            var LJSrcRect := TJRect.JavaClass.init(0, 0, LBitmap.getWidth, LBitmap.getheight);
            //--
            var LBlurMaskFilter := TJBlurMaskFilter.JavaClass.init(FShadowblur*2.5, TJBlurMaskFilter_Blur.JavaClass.NORMAL);
            LPaint.setColor(integer(LShadowColor));
            LPaint.setMaskFilter(LBlurMaskFilter);
            FCanvas.drawBitmap(LBitmap.extractAlpha, LJSrcRect, LJDestRectf, LPaint);
            LPaint.setMaskFilter(nil);
            LDestRect.Offset(-LScaledShadowOffsetX, -LScaledShadowOffsetY);
            LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
            LPaint.setColor(integer(TAlphaColors.Black));
            FCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);

          end

          // The shadow is made on the rectangle shape
          else begin
            If not LPathClipped then _DrawRect(FCanvas, nil{APaint}, LScaledImageDstRect, false{aDrawOnlyBorder}, true{aForceDrawPath}, true{aClipPath}, FFillImageNoRadius{aNoRadius});
            var LDestRect := TrectF.Create(0,0, LBitmap.getWidth, LBitmap.getheight).CenterAt(LScaledImageDstRect);
            var LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
            var LJSrcRect := TJRect.JavaClass.init(0, 0, LBitmap.getWidth, LBitmap.getheight);
            FCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
            FCanvas.restore;
            LJSrcRect := nil;
            LJDestRectf := nil;
          end;

        finally
          LBitmap.recycle;
          LBitmap := nil;
        end;
      end;

    end

    // Draw the StateLayer
    else if not FDrawStateLayerOnTop then
      _DrawStateLayer;

    //stroke the rectangle
    if LStrokeColor <> TalphaColorRec.Null then begin
      LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
      LPaint.setStrokeWidth(LScaledStrokeThickness);
      LPaint.setColor(integer(LStrokeColor));
      _DrawRect(FCanvas, LPaint, LScaledDstRect, true{aDrawOnlyBorder}, LFillWithImage{aForceDrawPath}, False{aClipPath}, False{aNoRadius});
    end;

    // Draw the StateLayer
    if FDrawStateLayerOnTop then
      _DrawStateLayer;

    //free the paint and the canvas
    LPaint := nil;

  finally
    // Remove the alpha layer
    if compareValue(FOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(FCanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  // Create the alpha layer
  if compareValue(FOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LFillResourceStream, // const AFillResourceStream: TStream;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        FStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(FCanvas, LLayerRect, FOpacity);
  end;
  try

    // Fill the rectangle
    if (LFillColor <> TalphaColorRec.Null) or
       (length(LFillGradientColors) > 0) or
       (LFillWithImage) or
       (LShadowColor <> TalphaColorRec.Null) then begin

      //fill with bitmap
      //if FFill.Kind = TALBrushKind.Bitmap then begin
      //  if not FFill.Bitmap.Bitmap.IsEmpty then begin
      //    if FFill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
      //      var LBitmapData: TBitmapData;
      //      if FFill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
      //      try
      //        var LTmpContext := ALCreateCGContextRef(LBitmapData.Width, LBitmapData.Height, LBitmapData.Data, LBitmapData.Pitch);
      //        try
      //          var LImageRef := CGBitmapContextCreateImage(LTmpContext);
      //          if LImageRef = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
      //          try
      //            var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithCGImage(LImageRef{$IF defined(ALMacOS)}, CGSizeMake(LBitmapData.width, LBitmapData.height){$ENDIF}));
      //            if LImage = nil then raise Exception.create('Failed to initialize UIImage/NSImage from CGImageRef');
      //            try
      //              ...
      //            finally
      //              LImage.release;
      //            end;
      //          finally
      //            CGImageRelease(LImageRef);
      //          end;
      //        finally
      //          CGContextRelease(LTmpContext);
      //        end;
      //      finally
      //        FFill.Bitmap.Bitmap.Unmap(LBitmapData);
      //      end;
      //    end;
      //  end;
      //end

      // Init LDrawnWithSolidColor
      var LDrawnWithSolidColor := False;

      // Fill with transparent solid color and shadow
      if (LShadowColor <> TalphaColorRec.Null) and // If null, skip drawing the shadow
         (TAlphaColorRec(LFillColor).A < 255) and // Else, fill with solid color and shadow in one pass
         (_FillGradientIsTransparent) and // Else, fill with gradient and shadow in one pass
         ((not LFillWithImage) or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
          (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
          (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
          (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

        // First pass draw the shadow
        var LFillColorF := TAlphaColorCGFloat.Create(LFillColor);
        CGContextSetRGBFillColor(FCanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, 1{A});
        _SetShadow(FCanvas);
        _DrawRect(FCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath}, False{aNoRadius});
        _ClearShadow(FCanvas);

        // Second pass fill the rect
        CGContextSetBlendMode(FCanvas, kCGBlendModeCopy);
        CGContextSetRGBFillColor(FCanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, LFillColorF.A);
        _DrawRect(FCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath}, False{aNoRadius});
        CGContextSetBlendMode(FCanvas, kCGBlendModeNormal);
        LDrawnWithSolidColor := True;

      end

      // Fill with solid color
      else if (LFillColor <> TalphaColors.Null) then begin
        var LFillColorF := TAlphaColorCGFloat.Create(LFillColor);
        CGContextSetRGBFillColor(FCanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, LFillColorF.A);
        _SetShadow(FCanvas);
        _DrawRect(FCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath}, False{aNoRadius});
        _ClearShadow(FCanvas);
        LDrawnWithSolidColor := True;
      end;

      //fill with gradient
      if length(LFillGradientColors) > 0 then begin

        // First pass draw the shadow if not already drawn
        // We must do this because else the shadow will not be drawn
        if (LShadowColor <> TalphaColorRec.Null) and (not LDrawnWithSolidColor) then begin
          var LFillColorF := TAlphaColorCGFloat.Create(LShadowColor);
          CGContextSetRGBFillColor(FCanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, 1{A});
          _SetShadow(FCanvas);
          _DrawRect(FCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath}, False{aNoRadius});
          _ClearShadow(FCanvas);
          // We must now remove the solid color in case the
          // background contains transparent colors.
          CGContextSetBlendMode(FCanvas, kCGBlendModeClear);
          _DrawRect(FCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath}, False{aNoRadius});
          CGContextSetBlendMode(FCanvas, kCGBlendModeNormal);
        end;

        // Second pass fill the rect
        var LGradient := TALGradient.create;
        try
          LGradient.Colors := LFillGradientColors;
          LGradient.Offsets := LFillGradientOffsets;
          var LDefaultInputRange: array[0..1] of CGFloat;
          LDefaultInputRange[0] := 0;
          LDefaultInputRange[1] := 1;
          var LCallback: CGFunctionCallbacks;
          LCallback.version := 0;
          LCallback.evaluate := @ALGradientEvaluateCallback;
          LCallback.releaseInfo := nil;
          var LFunc := CGFunctionCreate(
                         LGradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                         1, // domainDimension - The number of inputs.
                         @LDefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                         4, // rangeDimension - The number of outputs.
                         nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                         @LCallback); // callbacks - A pointer to a callback function table.
          try
            var LShading: CGShadingRef;
            case FFillGradientStyle of
              TGradientStyle.Linear: begin
                LShading := CGShadingCreateAxial(
                              ALGetGlobalCGColorSpace, // colorspace
                              CGPointMake(
                                LScaledFillGradientStartPoint.X,
                                LGridHeight - LScaledFillGradientStartPoint.y), // start - The starting point of the axis, in the shading's target coordinate space.
                              CGPointMake(
                                LScaledFillGradientEndPoint.x,
                                LGridHeight - LScaledFillGradientEndPoint.y), // end - The ending point of the axis, in the shading's target coordinate space.
                              LFunc, // function
                              True, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting point of the axis.
                              True); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending point of the axis.
              end;
              TGradientStyle.Radial: begin
                LShading := CGShadingCreateRadial(
                              ALGetGlobalCGColorSpace, // colorspace
                              CGPoint.Create(TPointF.Create(LScaledFillGradientStartPoint.X, LGridHeight - LScaledFillGradientStartPoint.Y)), // start - The center of the starting circle, in the shading's target coordinate space.
                              0, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                              CGPoint.Create(TPointF.Create(LScaledFillGradientStartPoint.X, LGridHeight - LScaledFillGradientStartPoint.Y)), // end - The center of the ending circle, in the shading's target coordinate space.
                              LScaledFillGradientStartPoint.Distance(LScaledFillGradientEndPoint), // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                              LFunc, // function
                              True, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                              True); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
              end;
              else
                raise Exception.Create('Error 23128453-626D-49A6-AD49-D6CA8AC35ACF');
            end;
            try
              _DrawRect(FCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, true{aClipPath}, False{aNoRadius});
              CGContextDrawShading(FCanvas, LShading);
              CGContextRestoreGState(FCanvas);
              LDrawnWithSolidColor := True;
            finally
              CGShadingRelease(LShading);
            end;
          finally
            CGFunctionRelease(LFunc);
          end;
        finally
          ALFreeAndNil(LGradient);
        end;
      end;

      // Draw the StateLayer
      if not FDrawStateLayerOnTop then
        _DrawStateLayer;

      // Fill with image
      if LFillWithImage then begin
        var LImage: CGImageRef := ALCreateCGImageRefFromResource(
                                    LFillResourceName, // const AResourceName: String;
                                    LFillResourceStream, // const AResourceStream: TStream;
                                    FFillMaskResourceName, // const AMaskResourceName: String;
                                    FFillMaskBitmap, // const AMaskImage: CGImageRef;
                                    1, // const AScale: Single;
                                    LScaledImageDstRect.Width, LScaledImageDstRect.Height, // const W, H: single;
                                    FFillWrapMode, // const AWrapMode: TALImageWrapMode;
                                    FFillCropCenter, // const ACropCenter: TpointF;
                                    FFillBlurRadius * FScale, // const ABlurRadius: single;
                                    0, // const AXRadius: Single;
                                    0); // const AYRadius: Single);
        try

          // The shadow is made directly on the bitmap
          if (not LDrawnWithSolidColor) and (LShadowColor <> TalphaColorRec.Null) then begin

            // Their is corners so remove them from the LBitmap
            if (not FFillImageNoRadius) and
               ((compareValue(FXRadius, 0, TEpsilon.Position) <> 0) or
                (compareValue(FYRadius, 0, TEpsilon.position) <> 0)) then begin
              var LTmpRect := TRectF.Create(0,0,LScaledImageDstRect.Width,LScaledImageDstRect.height).Round;
              var LTmpCGContextRef := ALCreateCGContextRef(LTmpRect.Width, LTmpRect.Height);
              try
                _DrawRect(LTmpCGContextRef, LTmpRect.Height, LTmpRect, false{aDrawOnlyBorder}, true{aClipPath}, FFillImageNoRadius{aNoRadius});
                var LDestRect := TrectF.Create(0,0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)).CenterAt(LTmpRect);
                CGContextDrawImage(
                  LTmpCGContextRef, // c: The graphics context in which to draw the image.
                  ALLowerLeftCGRect(
                    LDestRect.TopLeft,
                    LDestRect.Width,
                    LDestRect.Height,
                    LTmpRect.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                  LImage); // image The image to draw.
                CGContextRestoreGState(LTmpCGContextRef);
                var LTmpImage := CGBitmapContextCreateImage(LTmpCGContextRef);
                if LTmpImage = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
                CGImageRelease(LImage);
                LImage := LTmpImage;
              finally
                CGContextRelease(LTmpCGContextRef);
              end;
            end;

            _SetShadow(FCanvas);
            var LDestRect := TrectF.Create(0,0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)).CenterAt(LScaledImageDstRect);
            CGContextDrawImage(
              FCanvas, // c: The graphics context in which to draw the image.
              ALLowerLeftCGRect(
                LDestRect.TopLeft,
                LDestRect.Width,
                LDestRect.Height,
                LGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              LImage); // image The image to draw.
             _ClearShadow(FCanvas);

          end

          // The shadow is made on the rectangle shape
          else begin
            _DrawRect(FCanvas, LGridHeight, LScaledImageDstRect, false{aDrawOnlyBorder}, true{aClipPath}, FFillImageNoRadius{aNoRadius});
            var LDestRect := TrectF.Create(0,0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)).CenterAt(LScaledImageDstRect);
            CGContextDrawImage(
              FCanvas, // c: The graphics context in which to draw the image.
              ALLowerLeftCGRect(
                LDestRect.TopLeft,
                LDestRect.Width,
                LDestRect.Height,
                LGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              LImage); // image The image to draw.
            CGContextRestoreGState(FCanvas);
          end;

        finally
          CGImageRelease(LImage);
        end;
      end;

    end

    // Draw the StateLayer
    else if not FDrawStateLayerOnTop then
      _DrawStateLayer;

    //stroke the rectangle
    if LStrokeColor <> TalphaColorRec.Null then begin
      CGContextSetLineWidth(FCanvas, LScaledStrokeThickness);
      var LStrokeColorF := TAlphaColorCGFloat.Create(LStrokeColor);
      CGContextSetRGBStrokeColor(FCanvas, LStrokeColorF.R, LStrokeColorF.G, LStrokeColorF.B, LStrokeColorF.A);
      _DrawRect(FCanvas, LGridHeight, LScaledDstRect, True{aDrawOnlyBorder}, false, False{aNoRadius});
    end;

    // Draw the StateLayer
    if FDrawStateLayerOnTop then
      _DrawStateLayer;

  finally
    // Remove the alpha layer
    if compareValue(FOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(FCanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  var LSaveState := FCanvas.SaveState;
  try

    if LFillColor <> TAlphaColorRec.Null then begin
      FCanvas.Fill.Kind := TBrushKind.Solid;
      FCanvas.Fill.Color := LFillColor;
    end
    else if LFillWithImage then begin
      FCanvas.Fill.Kind := TBrushKind.None;
    end
    else FCanvas.Fill.Kind := TBrushKind.None;
    If LStrokeColor <> TalphaColorRec.Null then begin
      FCanvas.Stroke.Kind := TBrushKind.Solid;
      FCanvas.Stroke.Color := LStrokeColor;
      FCanvas.Stroke.Thickness := LScaledStrokeThickness;
    end
    else FCanvas.Stroke.Kind := TBrushKind.None;

    if LStrokeColor <> TalphaColorRec.Null then
      LScaledDstRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2));

    var LScaledXRadius: Single := FXRadius;
    var LScaledYRadius: Single := FYRadius;
    ALNormalizeAndScaleRadii(LScaledXRadius, LScaledYRadius, FScale, LScaledDstRect);

    if LFillColor <> TAlphaColorRec.Null then begin
      FCanvas.FillRect(LScaledDstRect, LScaledXRadius, LScaledYRadius, FCorners, FOpacity, FCanvas.Fill, TCornerType.Round{CornerType});
    end;
    If LStrokeColor <> TalphaColorRec.Null then
      FCanvas.DrawRect(LScaledDstRect, LScaledXRadius, LScaledYRadius, FCorners, FOpacity, FCanvas.Stroke, TCornerType.Round{CornerType});

  finally
    FCanvas.RestoreState(LSaveState)
  end;

  {$ENDIF}
  {$ENDREGION}

end;

{************************}
Procedure ALCreateSurface(
            out ASurface: TALSurface;
            out ACanvas: TALCanvas;
            const AScale: Single;
            const w: Single;
            const h: Single;
            const AAddPixelForAlignment: Boolean = true);
begin

  {$REGION 'SKIA'}
  {$IF defined(ALSkiaEngine)}

  ASurface := ALCreateSkSurface(
                ALCeil(W * AScale, TEpsilon.Position) + ALIfThen(AAddPixelForAlignment, 1, 0),
                ALCeil(H * AScale, TEpsilon.Position) + ALIfThen(AAddPixelForAlignment, 1, 0));
  ACanvas := ALSkCheckHandle(sk4d_surface_get_canvas(aSurface));

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ANDROID'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

  ASurface := TJBitmap.JavaClass.createBitmap(
                ALCeil(W * AScale, TEpsilon.Position) + ALIfThen(AAddPixelForAlignment, 1, 0),
                ALCeil(H * AScale, TEpsilon.Position) + ALIfThen(AAddPixelForAlignment, 1, 0),
                TJBitmap_Config.JavaClass.ARGB_8888,
                true{hasAlpha},
                ALGetGlobalJColorSpace);
  ACanvas := TJCanvas.JavaClass.init(ASurface);

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  ASurface := ALCreateCGContextRef(
                ALCeil(W * AScale, TEpsilon.Position) + ALIfThen(AAddPixelForAlignment, 1, 0),
                ALCeil(H * AScale, TEpsilon.Position) + ALIfThen(AAddPixelForAlignment, 1, 0));
  ACanvas := ASurface;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  ASurface := Tbitmap.Create(
                ALCeil(W * AScale, TEpsilon.Position) + ALIfThen(AAddPixelForAlignment, 1, 0),
                ALCeil(H * AScale, TEpsilon.Position) + ALIfThen(AAddPixelForAlignment, 1, 0));
  ASurface.Clear(TAlphaColorRec.Null);
  ACanvas := ASurface.Canvas;

  {$ENDIF}
  {$ENDREGION}

end;

{************************}
Procedure ALCreateSurface(
            out ASurface: TALSurface;
            out ACanvas: TALCanvas;
            const w: Integer;
            const h: Integer;
            const AAddPixelForAlignment: Boolean = true);
begin
  ALCreateSurface(
    ASurface, // out ASurface: TALSurface;
    ACanvas, // out ACanvas: TALCanvas;
    1, // const AScale: Single;
    w, // const w: Single;
    h, // const h: Single
    AAddPixelForAlignment); // const AAddPixelForAlignment: Boolean = true);
end;

{****************************}
procedure ALFreeAndNilSurface(
            Var ASurface: TALSurface;
            Var ACanvas: TALCanvas);
begin

  {$REGION 'SKIA'}
  {$IF defined(ALSkiaEngine)}

  ACanvas := 0; // ACanvas is linked inside ASurface
  if aSurface <> 0 then begin
    sk4d_refcnt_unref(aSurface);
    aSurface := 0;
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ANDROID'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

  aCanvas := nil;
  if ASurface <> nil then begin
    ASurface.recycle;
    ASurface := nil;
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  ACanvas := Nil; // ACanvas = ASurface
  if ASurface <> nil then begin
    CGContextRelease(ASurface);
    ASurface := nil;
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  ACanvas := nil; // ACanvas is linked inside ASurface
  ALFreeAndNil(ASurface);

  {$ENDIF}
  {$ENDREGION}

end;

{*************************************************************}
function  ALIsSurfaceNull(const aSurface: TALSurface): Boolean;
begin
  {$IF defined(ALSkiaEngine)}
  result := aSurface = 0
  {$ELSE}
  result := aSurface = nil;
  {$ENDIF}
end;

{**********************************************************}
function  ALIsCanvasNull(const aCanvas: TALCanvas): Boolean;
begin
  {$IF defined(ALSkiaEngine)}
  result := aCanvas = 0
  {$ELSE}
  result := aCanvas = nil;
  {$ENDIF}
end;

{****************************************************************}
function  ALIsDrawableNull(const aDrawable: TALDrawable): Boolean;
begin
  {$IF defined(ALSkiaCanvas)}
  result := aDrawable = 0
  {$ELSE}
  result := aDrawable = nil;
  {$ENDIF}
end;

{**********************************************************}
function  ALIsBitmapNull(const aBitmap: TALBitmap): Boolean;
begin
  {$IF defined(ALSkiaEngine)}
  result := aBitmap = 0
  {$ELSE}
  result := aBitmap = nil;
  {$ENDIF}
end;

{*********************************************************}
procedure ALFreeAndNilDrawable(var aDrawable: TALDrawable);
begin
  {$IF defined(ALSkiaCanvas)}
  if aDrawable <> 0 then begin
    sk4d_refcnt_unref(aDrawable);
    aDrawable := 0;
  end;
  {$ELSE}
  ALFreeAndNil(aDrawable);
  {$ENDIF}
end;

{***************************************************}
procedure ALFreeAndNilBitmap(var aBitmap: TALBitmap);
begin
  {$IF defined(ALSkiaEngine)}
  if aBitmap <> 0 then begin
    sk4d_refcnt_unref(aBitmap);
    aBitmap := 0;
  end;
  {$ELSEIF defined(ANDROID)}
  aBitmap.recycle;
  aBitmap := nil;
  {$ELSEIF defined(ALAppleOS)}
  CGImageRelease(aBitmap);
  {$ELSE}
  ALFreeAndNil(aBitmap);
  {$ENDIF};
end;

{******************************************************************}
function  ALGetDrawableWidth(const aDrawable: TALDrawable): integer;
begin
  {$IF defined(ALSkiaCanvas)}
  result := sk4d_image_get_width(aDrawable);
  {$ELSE}
  result := aDrawable.Width;
  {$ENDIF}
end;

{*******************************************************************}
function  ALGetDrawableHeight(const aDrawable: TALDrawable): integer;
begin
  {$IF defined(ALSkiaCanvas)}
  result := sk4d_image_get_height(aDrawable);
  {$ELSE}
  result := aDrawable.height;
  {$ENDIF}
end;

{*************************************************************}
function ALCanvasBeginScene(const aCanvas: TALCanvas): Boolean;
begin
  {$IF defined(ALSkiaEngine)}
  Result := true;
  {$ELSEIF defined(ANDROID)}
  Result := true;
  {$ELSEIF defined(ALAppleOS)}
  Result := true;
  {$ELSE}
  Result := ACanvas.BeginScene;
  {$ENDIF};
end;

{***************************************************}
procedure ALCanvasEndScene(const aCanvas: TALCanvas);
begin
  {$IF defined(ALSkiaEngine)}
  // Nothing to do
  {$ELSEIF defined(ANDROID)}
  // Nothing to do
  {$ELSEIF defined(ALAppleOS)}
  // Nothing to do
  {$ELSE}
  ACanvas.EndScene;
  {$ENDIF};
end;

{***************************************************************************}
procedure ALClearCanvas(const aCanvas: TALCanvas; const AColor: TAlphaColor);
begin
  {$IF defined(ALSkiaEngine)}
  sk4d_canvas_clear(aCanvas, AColor);
  {$ELSEIF defined(ANDROID)}
  aCanvas.drawColor(AColor, TJPorterDuff_Mode.JavaClass.CLEAR);
  {$ELSEIF defined(ALAppleOS)}
  var LColorF := TAlphaColorCGFloat.Create(AColor);
  var LColorCG := CGColorCreate(ALGetGlobalCGColorSpace, @LColorF);
  try
    var LRect := CGContextGetClipBoundingBox(aCanvas);
    CGContextSetFillColorWithColor(aCanvas, LColorCG);
    CGContextClearRect(aCanvas, LRect);
    CGContextFillRect(aCanvas, LRect);
  finally
    CGColorRelease(LColorCG);
  end;
  {$ELSE}
  aCanvas.Clear(AColor);
  {$ENDIF};
end;

{*****************************************************************************************************************************************************}
procedure ALBeginTransparencyLayer(const aCanvas: TALCanvas; const ARect: TRectF; const AOpacity: Single; const AAddPixelForAlignment: Boolean = true);
begin

  var LRect := Arect;
  if AAddPixelForAlignment then LRect.Inflate(1,1,1,1);

  {$REGION 'SKIA'}
  {$IF defined(ALSkiaEngine)}

  sk4d_canvas_save_layer_alpha(ACanvas, @LRect, round(255 * AOpacity));

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ANDROID'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

  var LJRect := TJRectF.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);
  aCanvas.saveLayerAlpha(LJRect, round(255 * AOpacity));
  LJRect := nil;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  CGContextSaveGState(ACanvas);
  CGContextSetAlpha(ACanvas, AOpacity);
  CGContextBeginTransparencyLayerWithRect(
    ACanvas,
    ALLowerLeftCGRect(
      LRect.TopLeft,
      LRect.Width,
      LRect.Height,
      CGBitmapContextGetHeight(ACanvas)),
      nil{auxiliaryInfo});

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  // not supported

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************************}
procedure ALEndTransparencyLayer(const aCanvas: TALCanvas);
begin

  {$REGION 'SKIA'}
  {$IF defined(ALSkiaEngine)}

  sk4d_canvas_restore(ACanvas);

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ANDROID'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

  ACanvas.restore;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  CGContextEndTransparencyLayer(ACanvas);
  CGContextRestoreGState(ACanvas)

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  // not supported

  {$ENDIF}
  {$ENDREGION}

end;

{****************************************************************************}
function ALCreateDrawableFromSurface(const ASurface: TALSurface): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALCreateSkImageFromSkSurface(ASurface);
    {$ELSEIF defined(ALGPUCanvas)}
    result := ALCreateTextureFromSkSurface(ASurface);
    {$ELSE}
    result := ALCreateTBitmapFromSkSurface(ASurface);
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  result := ALCreateTextureFromJBitmap(ASurface);
  {$ELSEIF defined(ALAppleOS)}
    {$IF defined(ALGpuCanvas)}
    result := ALCreateTextureFromCGContextRef(ASurface);
    {$ELSE}
    result := ALCreateTBitmapFromCGContextRef(ASurface);
    {$ENDIF}
  {$ELSE}
  Result := Tbitmap.Create;
  Result.Assign(ASurface);
  {$ENDIF}
end;

{**********************************************************************************************}
procedure ALUpdateDrawableFromSurface(const aSurface: TALSurface; const aDrawable: TALDrawable);
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Raise Exception.Create('Updating SkImage from an SkSurface is unsupported')
    {$ELSEIF defined(ALGPUCanvas)}
    ALUpdateTextureFromSkSurface(ASurface, aDrawable);
    {$ELSE}
    ALUpdateTBitmapFromSkSurface(ASurface, aDrawable);
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  ALUpdateTextureFromJBitmap(ASurface, aDrawable);
  {$ELSEIF defined(ALAppleOS)}
    {$IF defined(ALGpuCanvas)}
    ALUpdateTextureFromCGContextRef(ASurface, aDrawable);
    {$ELSE}
    ALUpdateTBitmapFromCGContextRef(ASurface, aDrawable);
    {$ENDIF}
  {$ELSE}
  // To force CopyToNewReference
  aSurface.Canvas.BeginScene;
  try
    aDrawable.Assign(aSurface);
  finally
    aSurface.Canvas.EndScene;
  end;
  {$ENDIF}
end;

{**********************************************}
function  ALGetDefaultPixelFormat: TPixelFormat;
begin
  {$IF DEFINED(MSWINDOWS)}
  Result := TPixelFormat.BGRA;
  {$ELSEIF DEFINED(IOS)}
  if GlobalUseMetal then
    Result := TPixelFormat.BGRA
  else
    Result := TPixelFormat.RGBA;
  {$ELSEIF DEFINED(MACOS)}
  if GlobalUseMetal then
    Result := TPixelFormat.BGRA
  else
    Result := TPixelFormat.RGBA;
  {$ELSEIF DEFINED(LINUX)}
  Result := TPixelFormat.BGRA;
  {$ELSE}
  Result := TPixelFormat.RGBA;
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALExtractMatrixFromSkCanvas(const ACanvas: sk_canvas_t; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
begin
  ACanvasMatrix := TMatrix.Identity;
  ACanvasScale := 1;
  if ACanvas <> 0 then begin
    sk4d_canvas_get_local_to_device_as_3x3(Acanvas, sk_matrix_t(ACanvasMatrix));
    ACanvasScale := ACanvasMatrix.m11;
    ACanvasMatrix.m11 := ACanvasMatrix.m11 / ACanvasScale;
    ACanvasMatrix.m12 := ACanvasMatrix.m12 / ACanvasScale;
    ACanvasMatrix.m13 := ACanvasMatrix.m13 / ACanvasScale;
    ACanvasMatrix.m21 := ACanvasMatrix.m21 / ACanvasScale;
    ACanvasMatrix.m22 := ACanvasMatrix.m22 / ACanvasScale;
    ACanvasMatrix.m23 := ACanvasMatrix.m23 / ACanvasScale;
    ACanvasMatrix.m31 := ACanvasMatrix.m31 / ACanvasScale;
    ACanvasMatrix.m32 := ACanvasMatrix.m32 / ACanvasScale;
    ACanvasMatrix.m33 := ACanvasMatrix.m33 / ACanvasScale;
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
procedure ALExtractMatrixFromJCanvas(const ACanvas: JCanvas; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
begin

  // In API level 16 and later, the behavior and reliability of Canvas.getMatrix()
  // have changed due to the introduction of hardware acceleration in the
  // Android graphics pipeline. You should be cautious when relying on the
  // matrix retrieved from a Canvas because its state might not be well-defined
  // or consistent in all cases.
  //
  // Matrix State Is Implementation-Defined:
  // With hardware-accelerated canvases, the transformation matrix (Canvas.getMatrix())
  // may vary depending on the implementation and may not represent the actual
  // transformation applied during drawing.
  // The matrix could be affected by optimizations or intermediate transformations
  // within the rendering pipeline.
  //
  // Recommended Alternatives:
  // * Draw Without Relying on the Matrix: Focus on drawing content relative to
  //   the current drawing context (e.g., Canvas coordinates) without assuming or
  //   manipulating the matrix.
  // * Track Transformations Manually: Maintain and apply your own transformation
  //   state outside the Canvas if you need precise control.

  ACanvasMatrix := TMatrix.Identity;
  ACanvasScale := 1;

end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
procedure ALExtractMatrixFromCGContextRef(const ACanvas: CGContextRef; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
begin
  ACanvasMatrix := TMatrix.Identity;
  ACanvasScale := 1;
  if ACanvas <> nil then begin
    var LCTM := CGContextGetCTM(ACanvas);
    ACanvasScale := LCTM.a;
    ACanvasMatrix.m11 := LCTM.a / ACanvasScale;
    ACanvasMatrix.m12 := LCTM.b / ACanvasScale;
    ACanvasMatrix.m21 := LCTM.c / ACanvasScale;
    ACanvasMatrix.m22 := LCTM.d / ACanvasScale;
    ACanvasMatrix.m31 := LCTM.tx / ACanvasScale;
    ACanvasMatrix.m32 := LCTM.ty / ACanvasScale;
  end;
end;
{$ENDIF}

{*****************************************************************************************************************}
procedure ALExtractMatrixFromTCanvas(const ACanvas: TCanvas; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
begin
  if ACanvas <> nil then begin
    ACanvasMatrix := ACanvas.Matrix;
    ACanvasScale := ACanvas.Scale;
  end
  else begin
    ACanvasMatrix := TMatrix.Identity;
    ACanvasScale := 1;
  end;
end;

{******************************************************************************************************************}
procedure ALExtractMatrixFromCanvas(const ACanvas: TALCanvas; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
begin
  {$IF defined(ALSkiaEngine)}
  ALExtractMatrixFromSkCanvas(ACanvas, ACanvasMatrix, ACanvasScale);
  {$ELSEIF defined(ANDROID)}
  ALExtractMatrixFromJCanvas(ACanvas, ACanvasMatrix, ACanvasScale);
  {$ELSEIF defined(ALAppleOS)}
  ALExtractMatrixFromCGContextRef(ACanvas, ACanvasMatrix, ACanvasScale);
  {$ELSE}
  ALExtractMatrixFromTCanvas(ACanvas, ACanvasMatrix, ACanvasScale);
  {$ENDIF};
end;

{*******************************}
function  ALScaleAndCenterCanvas(
            Const ACanvas: TCanvas;
            Const AAbsoluteRect: TRectF;
            Const AScale: Single;
            Const ASaveState: Boolean): TCanvasSaveState;
begin
  Result := nil;
  if not samevalue(AScale, 1, TEpsilon.Scale) then begin
    if ASaveState then
      Result := ACanvas.SaveState;
    var LMatrix := TMatrix.CreateTranslation(-AAbsoluteRect.Left, -AAbsoluteRect.Top);
    LMatrix := LMatrix * TMatrix.CreateScaling(AScale, AScale);
    LMatrix := LMatrix * TMatrix.CreateTranslation(
                           AAbsoluteRect.Left - (((AAbsoluteRect.Width * AScale) - AAbsoluteRect.Width) / 2),
                           AAbsoluteRect.Top - (((AAbsoluteRect.height * AScale) - AAbsoluteRect.Height) / 2));
    ACanvas.SetMatrix(ACanvas.Matrix * LMatrix);
  end;
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALGetGlobalSkPaint(const AOpacity: Single): sk_paint_t;
begin
  {$IF defined(DEBUG)}
  if TThread.Current.ThreadID <> MainThreadID then
    raise Exception.Create('ALGetGlobalSkPaint can only be called from within the main UI thread');
  {$ENDIF}
  if ALGlobalSkPaint = 0 then begin
    ALGlobalSkPaint := ALSkCheckHandle(sk4d_paint_create);
    // Requests, but does not require, that edge pixels draw opaque or with partial transparency.
    sk4d_paint_set_antialias(ALGlobalSkPaint, false);
    // Requests, but does not require, to distribute color error.
    sk4d_paint_set_dither(ALGlobalSkPaint, true);
    // Sets whether the geometry is filled, stroked, or filled and stroked.
    sk4d_paint_set_style(ALGlobalSkPaint, sk_paintstyle_t.FILL_SK_PAINTSTYLE);
  end;
  result := ALGlobalSkPaint;
  sk4d_paint_set_alphaf(result, AOpacity);
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALSkCheckHandle(const AHandle: sk_handle_t): sk_handle_t;
begin
  If AHandle = 0 then
    raise Exception.Create('Skia API call failed');
  result := AHandle;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALSkCheckCanvas(const ACanvas: TCanvas);
begin
  if not (ACanvas is TSkCanvasCustom) then begin
    if GlobalUseSkia then
      raise Exception.Create(
              'Your declaration of GlobalUseSkia has no effect because the ' +
              'canvas service has already been started. In this case, just ' +
              'create a unit in the project like "Project.Startup.pas", '    +
              'place the GlobalUseSkia declaration in the initialization of ' +
              'this new unit, and declare this new unit before any other '  +
              'unit of yours in the .dpr, that is, right after FMX.Forms.')
    else
      raise Exception.Create(
              'You must enable Skia for app rendering by setting '+
              'FMX.Skia.GlobalUseSkia to true in the initialization section '+
              'of your project.');
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALSkStreamAdapterGetLengthProc(context: Pointer): size_t;
begin
  Result := TStream(context).Size;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALSkStreamAdapterGetPositionProc(context: Pointer): size_t;
begin
  Result := TStream(context).Position;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALSkStreamAdapterReadProc(context: Pointer; buffer: Pointer; size: size_t): size_t;
begin
  Result := TStream(context).Read(buffer^, size);
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALSkStreamAdapterSeekProc(context: Pointer; position: size_t): _bool;
begin
  TStream(context).Position := position;
  Result := True;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALGetSkImageinfo(const W, H: int32_t): sk_imageinfo_t;
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if declaration of System.Skia.API.sk_imageinfo_t didn''t changed'}
  {$ENDIF}
  Result.width := W;
  Result.height := H;
  Result.color_type := sk_colortype_t(SkFmxColorType[ALGetDefaultPixelFormat]);
  Result.alpha_type := sk_alphatype_t.PREMUL_SK_ALPHATYPE;
  // https://skia.org/docs/user/color/
  // how do nullptr SkColorSpace defaults work? :
  // if (srcCS == nullptr) { srcCS = sRGB; }
  // if (dstCS == nullptr) { dstCS = srcCS; }
  Result.color_space := ALGetGlobalSkColorSpace;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateSkSurface(Const W, H: integer): sk_surface_t;
begin

  if (not ALGlobalUseRasterSkSurface) then begin

    // https://groups.google.com/g/skia-discuss/c/rq8ucR2OyQ0
    // * Ganesh is not thread safe. Thus it can only be used on one thread at a
    //   time and it is the clients responsibility to ensure this happens. This
    //   includes drawing to GPU surfaces as well as things like creating and
    //   freeing resources owning GPU objects (e.g. gpu texture backed SkImages/Surfaces).
    // * You cannot share objects that wrap GPU objects between different
    //   GrDirectContexts. So an SkSurface made on one GrDirectContext cannot be
    //   used on a different one*.
    // * GL does allow you to make share groups which allow you to use GL objects
    //   between different GL Contexts. In general Skia does not test this so we
    //   cannot recommend using this system ourselves. Now saying that, we do
    //   know in the past we've seen driver bugs when trying to play around with
    //   share groups especially when rendering to the objects on different
    //   contexts (with appropriate synchronization of course). It does seem like
    //   things like texture creation and uploads do tend to work with less bugs
    //   across share groups.

    Raise exception.Create('GPU Surface is not yet supported');

    //if not TGrCanvas.Initialized then
    //  Raise exception.Create('The shared context has not been initialized');
    //var LImageInfo := ALGetSkImageinfo(W, H);
    //var LGrBackEndTexture := ALSkCheckHandle(
    //                           gr4d_directcontext_create_texture(
    //                             TGrCanvas.SharedContext.GrDirectContext.Handle, // self: gr_directcontext_t;
    //                             LImageInfo.width, // width,
    //                             LImageInfo.Height, // height: int32_t;
    //                             LImageInfo.color_type, // color_type: sk_colortype_t;
    //                             False, // is_mipmapped,
    //                             True, // is_renderable,
    //                             False)); //is_protected: _bool
    //
    //result := ALSkCheckHandle(
    //            sk4d_surface_make_from_texture(
    //              TGrCanvas.SharedContext.GrDirectContext.Handle, // context: gr_directcontext_t;
    //              LGrBackEndTexture, // const texture: gr_backendtexture_t;
    //              gr_surfaceorigin_t.TOP_LEFT_GR_SURFACEORIGIN, // origin: gr_surfaceorigin_t;
    //              1, // sample_count: int32_t;
    //              LImageInfo.color_type, // color_type: sk_colortype_t;
    //              LImageInfo.color_space, // color_space: sk_colorspace_t;
    //              nil)); //const props: psk_surfaceprops_t));

  end
  else begin
    var LImageInfo := ALGetSkImageinfo(W, H);
    Result := ALSkCheckHandle(
                sk4d_surface_make_raster(
                  @LImageInfo, // image_info: psk_imageinfo_t;
                  LImageInfo.width * SkBytesPerPixel[TSkColorType(LImageInfo.color_type)], // row_bytes: size_t;
                  nil)); // const props: psk_surfaceprops_t

  end;

end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateTBitmapFromSkPixmap(Const APixmap: sk_pixmap_t): TBitmap;
begin
  var LWidth := sk4d_pixmap_get_width(APixmap);
  var LHeight := sk4d_pixmap_get_Height(APixmap);
  Result := TBitmap.Create(LWidth, LHeight);
  Try
    ALUpdateTBitmapFromSkPixmap(APixmap, Result);
  except
    ALFreeAndNil(Result);
    Raise;
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateTBitmapFromSkSurface(Const ASurface: sk_surface_t): TBitmap;
begin
  var LPixmap := ALSkCheckHandle(sk4d_surface_peek_pixels(ASurface));
  try
    Result := ALCreateTBitmapFromSkPixmap(LPixmap);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
Function ALCreateTBitmapFromSkImage(const AImage: sk_image_t): TBitmap;
begin
  var LPixmap := ALSkCheckHandle(sk4d_image_peek_pixels(AImage));
  try
    Result := ALCreateTBitmapFromSkPixmap(LPixmap);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateTBitmapFromSkPixmap(Const APixmap: sk_pixmap_t; const ABitmap: Tbitmap);
begin
  var LBitmapData: TBitmapData;
  if ABitmap.Map(TMapAccess.ReadWrite, LBitmapData) then begin
    try
      {$IF defined(DEBUG)}
      var Lcolortype := sk4d_pixmap_get_color_type(APixmap);
      if Lcolortype <> sk_colortype_t(SkFmxColorType[LBitmapData.PixelFormat]) then
        Raise Exception.Create('Pixmap and Bitmap color types mismatch');
      {$ENDIF}
      var LWidth := ABitmap.Width;
      var LHeight := ABitmap.Height;
      If (sk4d_pixmap_get_width(APixmap) <> LWidth) or
         (sk4d_pixmap_get_height(APixmap) <> LHeight) then
        Raise Exception.Create('Pixmap and Bitmap dimensions mismatch');
      var LBytesPerPixel := LBitmapData.BytesPerPixel;
      var LSourceRowBytes := Integer(sk4d_pixmap_get_row_bytes(APixmap));
      var LDestRowBytes := LBitmapData.Pitch;
      var LSourceData := sk4d_pixmap_get_pixels(APixmap);
      if LSourceRowBytes <> LDestRowBytes then begin
        for var Y := 0 to LHeight - 1 do
          ALMove(PByte(LSourceData)[Y * LSourceRowBytes], PByte(LBitmapData.Data)[Y * LDestRowBytes], LWidth * LBytesPerPixel);
      end
      else begin
        ALMove(PByte(LSourceData)[0], PByte(LBitmapData.Data)[0], LWidth * LHeight * LBytesPerPixel);
      end;
    finally
      ABitmap.Unmap(LBitmapData);
    end
  end
  else
    Raise Exception.create('Failed to map the bitmap for read/write access');
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateTBitmapFromSkSurface(Const ASurface: sk_surface_t; const ABitmap: TBitmap);
begin
  var LPixmap := ALSkCheckHandle(sk4d_surface_peek_pixels(ASurface));
  try
    ALUpdateTBitmapFromSkPixmap(LPixmap, ABitmap);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateTBitmapFromSkImage(const AImage: sk_image_t; const ABitmap: TBitmap);
begin
  var LPixmap := ALSkCheckHandle(sk4d_image_peek_pixels(AImage));
  try
    ALUpdateTBitmapFromSkPixmap(LPixmap, ABitmap);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateSkImageFromSkSurface(Const ASurface: sk_surface_t): sk_image_t;
begin
  if (not ALGlobalUseRasterSkImage) then begin
    if not TGrCanvas.Initialized then
      Raise exception.Create('The shared context has not been initialized');
    var LPixmap := ALSkCheckHandle(sk4d_surface_peek_pixels(ASurface));
    try
      Result := ALSkCheckHandle(
                  sk4d_image_make_cross_context(
                    TGrCanvas.SharedContext.GrDirectContext.handle, // context: gr_directcontext_t;
                    LPixmap, // const pixmap: sk_pixmap_t;
                    false, // build_mips,
                    false)); // limit_to_max_texture_size
    finally
      sk4d_pixmap_destroy(LPixmap);
    end;
  end
  else begin
    Result := ALSkCheckHandle(sk4d_surface_make_image_snapshot(ASurface));
    //if (not ALGlobalUseRasterSkImage) and
    //   (not sk4d_image_is_texture_backed(Result)) then begin
    //  if not TGrCanvas.Initialized then
    //    Raise exception.Create('The shared context has not been initialized');
    //  Result := ALSkCheckHandle(
    //              sk4d_image_make_texture_image(
    //                Result, // const self: sk_image_t;
    //                TGrCanvas.SharedContext.GrDirectContext.handle, // context: gr_directcontext_t;
    //                false)); //is_mipmapped: _bool
    //end;
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateTextureFromSkSurface(Const ASurface: sk_surface_t): TTexture;
begin
  var LPixmap := ALSkCheckHandle(sk4d_surface_peek_pixels(ASurface));
  try
    result := TALTexture.Create;
    try
      result.Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
      result.SetSize(sk4d_pixmap_get_Width(LPixmap), sk4d_pixmap_get_Height(LPixmap));
      result.PixelFormat := SkFmxPixelFormat[TSkColorType(sk4d_pixmap_get_color_type(LPixmap))];
      ALUpdateTextureFromSkPixmap(LPixmap, Result);
    except
      ALFreeAndNil(result);
      raise;
    end;
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateTextureFromSkImage(Const AImage: sk_image_t): TTexture;
begin
  var LPixmap := ALSkCheckHandle(sk4d_image_peek_pixels(AImage));
  try
    result := TALTexture.Create;
    try
      result.Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
      result.SetSize(sk4d_pixmap_get_Width(LPixmap), sk4d_pixmap_get_Height(LPixmap));
      result.PixelFormat := SkFmxPixelFormat[TSkColorType(sk4d_pixmap_get_color_type(LPixmap))];
      ALUpdateTextureFromSkPixmap(LPixmap, Result);
    except
      ALFreeAndNil(result);
      raise;
    end;
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateTextureFromSkPixmap(Const APixmap: sk_pixmap_t; const ATexture: TTexture);
begin
  {$IF defined(DEBUG)}
  if ATexture.PixelFormat <> SkFmxPixelFormat[TSkColorType(sk4d_pixmap_get_color_type(APixmap))] then
    Raise Exception.Create('Pixmap and Texture color types mismatch');
  {$ENDIF}
  If (sk4d_pixmap_get_width(APixmap) <> ATexture.Width) or
     (sk4d_pixmap_get_height(APixmap) <> ATexture.Height) then
    Raise Exception.Create('Pixmap and Texture dimensions mismatch');
  ATexture.UpdateTexture(sk4d_pixmap_get_pixels(APixmap), sk4d_pixmap_get_row_bytes(APixmap));
  {$IF defined(IOS) or defined(ANDROID)}
  // Without calling glFinish on devices like the Samsung S24,
  // if a texture is created in a background thread and later used
  // on the main thread, the texture may not be available.
  if (ALIsDefaultContextOpenGL) and (TThread.Current.ThreadID <> MainThreadID) then
    glFinish;
  {$ENDIF}
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateTextureFromSkSurface(Const ASurface: sk_surface_t; const ATexture: TTexture);
begin
  var LPixmap := ALSkCheckHandle(sk4d_surface_peek_pixels(ASurface));
  try
    ALUpdateTextureFromSkPixmap(LPixmap, ATexture);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateTextureFromSkImage(Const AImage: sk_image_t; const ATexture: TTexture);
begin
  var LPixmap := ALSkCheckHandle(sk4d_image_peek_pixels(AImage));
  try
    ALUpdateTextureFromSkPixmap(LPixmap, ATexture);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALGetCubicMitchellNetravaliSkSamplingoptions: sk_samplingoptions_t;
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if declaration of System.Skia.API.sk_samplingoptions_t didn''t changed'}
  {$ENDIF}
  Result.max_anisotropic := 0;
  Result.use_cubic := True;
  // Mitchell-Netravali Filter: A well-balanced choice with b = 1/3 and c = 1/3.
  // It provides a good balance between blurring and ringing.
  Result.Cubic.b := 1 / 3;
  Result.Cubic.c := 1 / 3;
  Result.Filter := sk_filtermode_t.LINEAR_SK_FILTERMODE; // Not used unless use_cubic is false
  Result.Mipmap := sk_mipmapmode_t.LINEAR_SK_MIPMAPMODE; // Use linear mipmap interpolation
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALGetLinearSkSamplingoptions: sk_samplingoptions_t;
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if declaration of System.Skia.API.sk_samplingoptions_t didn''t changed'}
  {$ENDIF}
  Result.max_anisotropic := 0;
  Result.use_cubic := False;
  Result.Cubic.b := 0;
  Result.Cubic.c := 0;
  Result.Filter := sk_filtermode_t.LINEAR_SK_FILTERMODE;
  Result.Mipmap := sk_mipmapmode_t.LINEAR_SK_MIPMAPMODE; // Use linear mipmap interpolation
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALGetNearestSkSamplingoptions: sk_samplingoptions_t;
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if declaration of System.Skia.API.sk_samplingoptions_t didn''t changed'}
  {$ENDIF}
  Result.max_anisotropic := 0;
  Result.use_cubic := False;
  Result.Cubic.b := 0;
  Result.Cubic.c := 0;
  Result.Filter := sk_filtermode_t.NEAREST_SK_FILTERMODE;
  Result.Mipmap := sk_mipmapmode_t.NONE_SK_MIPMAPMODE;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateDisplayP3SkColorSpace: sk_colorspace_t;
begin
  //Taken from SkColorSpace.h
  //static constexpr skcms_TransferFunction kSRGB = { 2.4f, (float)(1/1.055), (float)(0.055/1.055), (float)(1/12.92), 0.04045f, 0.0f, 0.0f };
  var LSkColorSpaceTransferFn: sk_colorspacetransferfn_t;
  LSkColorSpaceTransferFn.G := 2.4;
  LSkColorSpaceTransferFn.A := 1/1.055;
  LSkColorSpaceTransferFn.B := 0.055/1.055;
  LSkColorSpaceTransferFn.C := 1/12.92;
  LSkColorSpaceTransferFn.D := 0.04045;
  LSkColorSpaceTransferFn.E := 0.0;
  LSkColorSpaceTransferFn.F := 0.0;

  //Taken from SkColorSpace.h
  //static constexpr skcms_Matrix3x3 kDisplayP3 = {{
  //    {  0.515102f,   0.291965f,  0.157153f  },
  //    {  0.241182f,   0.692236f,  0.0665819f },
  //    { -0.00104941f, 0.0418818f, 0.784378f  },
  //}};
  //Taken from SkNDKConversions.h
  //static constexpr skcms_Matrix3x3 kDCIP3 = {{
  //        {0.486143, 0.323835, 0.154234},
  //        {0.226676, 0.710327, 0.0629966},
  //        {0.000800549, 0.0432385, 0.78275},
  //}};
  var LSkColorSpaceXYZ: sk_colorspacexyz_t;
  LSkColorSpaceXYZ.M_11 := 0.515102;
  LSkColorSpaceXYZ.M_12 := 0.291965;
  LSkColorSpaceXYZ.M_13 := 0.157153;
  LSkColorSpaceXYZ.M_21 := 0.241182;
  LSkColorSpaceXYZ.M_22 := 0.692236;
  LSkColorSpaceXYZ.M_23 := 0.0665819;
  LSkColorSpaceXYZ.M_31 := -0.00104941;
  LSkColorSpaceXYZ.M_32 := 0.0418818;
  LSkColorSpaceXYZ.M_33 := 0.784378;

  // Display P3
  Result := ALSkCheckHandle(sk4d_colorspace_make_rgb(@LSkColorSpaceTransferFn, @LSkColorSpaceXYZ));
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALGetGlobalSkColorSpace: sk_colorspace_t;
begin
  if not ALGlobalSkColorSpaceInitialized then begin

    // https://stackoverflow.com/questions/79021609/handling-mixed-color-spaces-drawing-srgb-and-display-p3-images-correctly-in-and
    // It seems that the OS automatically converts sRGB colors to their Display P3 equivalents (e.g., #FF0000 becomes #EA3323)
    // when rendering the surface on the screen. However, in the IDE, such as in the Object Inspector, all colors are specified
    // in sRGB (e.g., red is #FF0000, not its Display P3 equivalent #EA3323), which creates an issue! If I decode an image with
    // an sRGB color profile, red #FF0000 will be converted to #EA3323, and when it’s displayed on the screen, #EA3323 will
    // be converted to #D84532, which is not the desired outcome! There may be a way to prevent the OS from converting sRGB or
    // Display P3 colors, but there’s no way to apply this selectively to parts of the screen. As a result, the entire app must
    // either define colors in Display P3 or stick with sRGB :(

    var LSKColorSpace := ALSkCheckHandle(sk4d_colorspace_make_srgb);

    //var LSkColorSpace: sk_colorspace_t;
    //
    //{$IF defined(ANDROID)}
    //
    //if TOSVersion.Check(10, 0) and
    //   TAndroidHelper.Display.isWideColorGamut then begin
    //  LSKColorSpace := ALCreateDisplayP3SkColorSpace;
    //  {$IF defined(debug)}
    //  ALLog('GlobalSkColorSpace', 'Display P3');
    //  {$ENDIF}
    //end
    //else begin
    //  LSKColorSpace := ALSkCheckHandle(sk4d_colorspace_make_srgb);
    //  {$IF defined(debug)}
    //  ALLog('GlobalSkColorSpace', 'sRGB');
    //  {$ENDIF}
    //end;
    //
    //{$ELSEIF defined(IOS)}
    //
    //var LUITraitEnvironment := TUITraitEnvironment.Wrap(NSObjectToId(TiOSHelper.MainScreen));
    //case LUITraitEnvironment.traitCollection.displayGamut of
    //  UIDisplayGamutSRGB: begin
    //    LSKColorSpace := ALSkCheckHandle(sk4d_colorspace_make_srgb);
    //    {$IF defined(debug)}
    //    ALLog('GlobalSkColorSpace', 'sRGB');
    //    {$ENDIF}
    //  end;
    //  UIDisplayGamutP3: begin
    //    LSKColorSpace := ALCreateDisplayP3SkColorSpace;
    //    {$IF defined(debug)}
    //    ALLog('GlobalSkColorSpace', 'Display P3');
    //    {$ENDIF}
    //  end;
    //  UIDisplayGamutUnspecified: begin
    //    LSKColorSpace := ALSkCheckHandle(sk4d_colorspace_make_srgb);
    //    {$IF defined(debug)}
    //    ALLog('GlobalSkColorSpace', 'Unspecified (Default to sRGB)');
    //    {$ENDIF}
    //  end;
    //  else
    //    raise Exception.Create('Unknown UITraitEnvironment.displayGamut');
    //end;
    //
    //{$ELSEIF defined(ALMacOS)}
    //
    //LSkColorSpace := ALSkCheckHandle(sk4d_colorspace_make_srgb);
    //{$IF defined(debug)}
    //ALLog('GlobalSkColorSpace', 'sRGB');
    //{$ENDIF}
    //
    //{$ELSE}
    //
    //LSkColorSpace := ALSkCheckHandle(sk4d_colorspace_make_srgb);
    //{$IF defined(debug)}
    //ALLog('GlobalSkColorSpace', 'sRGB');
    //{$ENDIF}
    //
    //{$ENDIF}

    if AtomicCmpExchange(ALGlobalSkColorSpace, LSkColorSpace, 0) <> 0 then sk4d_colorspace_unref(LSkColorSpace);
    ALGlobalSkColorSpaceInitialized := true;

  end;
  result := ALGlobalSkColorSpace;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALGetGlobalJColorSpace: JColorSpace;
begin
  if not ALGlobalJColorSpaceInitialized then begin

    // https://stackoverflow.com/questions/79021609/handling-mixed-color-spaces-drawing-srgb-and-display-p3-images-correctly-in-and
    // It seems that the OS automatically converts sRGB colors to their Display P3 equivalents (e.g., #FF0000 becomes #EA3323)
    // when rendering the surface on the screen. However, in the IDE, such as in the Object Inspector, all colors are specified
    // in sRGB (e.g., red is #FF0000, not its Display P3 equivalent #EA3323), which creates an issue! If I decode an image with
    // an sRGB color profile, red #FF0000 will be converted to #EA3323, and when it’s displayed on the screen, #EA3323 will
    // be converted to #D84532, which is not the desired outcome! There may be a way to prevent the OS from converting sRGB or
    // Display P3 colors, but there’s no way to apply this selectively to parts of the screen. As a result, the entire app must
    // either define colors in Display P3 or stick with sRGB :(

    var LColorSpace := TJColorSpace.JavaClass.get(TJColorSpace_Named.JavaClass.SRGB);

    //var LColorSpace: JColorSpace.;
    //if TOSVersion.Check(10, 0) then LColorSpace := TAndroidHelper.Display.getPreferredWideGamutColorSpace;
    //if LColorSpace = nil then LColorSpace := TJColorSpace.JavaClass.get(TJColorSpace_Named.JavaClass.SRGB);

    if LColorSpace = nil then raise Exception.Create('Failed to create JColorSpace');
    if AtomicCmpExchange(Pointer(ALGlobalJColorSpace), Pointer(LColorSpace), nil) <> nil then LColorSpace := nil
    else ALGlobalJColorSpace._AddRef;
    ALGlobalJColorSpaceInitialized := true;
    {$IF defined(debug)}
    ALLog('GlobalJColorSpace', JStringToString(ALGlobalJColorSpace.getName));
    {$ENDIF}

  end;
  result := ALGlobalJColorSpace;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALGetGlobalCGColorSpace: CGColorSpaceRef;
begin
  if not ALGlobalCGColorSpaceInitialized then begin

    // https://stackoverflow.com/questions/79021609/handling-mixed-color-spaces-drawing-srgb-and-display-p3-images-correctly-in-and
    // https://stackoverflow.com/questions/79019697/why-does-using-cgcolorspacecreatewithnamekcgcolorspacedisplayp3-in-cgbitmapcon
    // It seems that the OS automatically converts sRGB colors to their Display P3 equivalents (e.g., #FF0000 becomes #EA3323)
    // when rendering the surface on the screen. However, in the IDE, such as in the Object Inspector, all colors are specified
    // in sRGB (e.g., red is #FF0000, not its Display P3 equivalent #EA3323), which creates an issue! If I decode an image with
    // an sRGB color profile, red #FF0000 will be converted to #EA3323, and when it’s displayed on the screen, #EA3323 will
    // be converted to #D84532, which is not the desired outcome! There may be a way to prevent the OS from converting sRGB or
    // Display P3 colors, but there’s no way to apply this selectively to parts of the screen. As a result, the entire app must
    // either define colors in Display P3 or stick with sRGB :(

    var LCGColorSpace := CGColorSpaceCreateDeviceRGB;

    //var LCGColorSpace: CGColorSpaceRef;
    //
    //{$IF defined(IOS)}
    //
    //var LUITraitEnvironment := TUITraitEnvironment.Wrap(NSObjectToId(TiOSHelper.MainScreen));
    //case LUITraitEnvironment.traitCollection.displayGamut of
    //  UIDisplayGamutSRGB: begin
    //    LCGColorSpace := CGColorSpaceCreateDeviceRGB;
    //    {$IF defined(debug)}
    //    ALLog('ALGetGlobalCGColorSpace', 'sRGB');
    //    {$ENDIF}
    //  end;
    //  UIDisplayGamutP3: begin
    //    LCGColorSpace := CGColorSpaceCreateWithName(CFStringRef(CocoaObjectIdConst(libCoreGraphics, 'kCGColorSpaceDisplayP3')));
    //    {$IF defined(debug)}
    //    ALLog('ALGetGlobalCGColorSpace', 'Display P3');
    //    {$ENDIF}
    //  end;
    //  UIDisplayGamutUnspecified: begin
    //    LCGColorSpace := CGColorSpaceCreateDeviceRGB;
    //    {$IF defined(debug)}
    //    ALLog('ALGetGlobalCGColorSpace', 'Unspecified (Default to sRGB)');
    //    {$ENDIF}
    //  end;
    //  else
    //    raise Exception.Create('Unknown UITraitEnvironment.displayGamut');
    //end;
    //
    //{$ELSE}
    //
    //LCGColorSpace := CGColorSpaceCreateDeviceRGB;
    //{$IF defined(debug)}
    //ALLog('ALGetGlobalCGColorSpace', 'sRGB');
    //{$ENDIF}
    //
    //{$ENDIF}

    if LCGColorSpace = nil then raise Exception.Create('Failed to create CGColorSpace');
    if AtomicCmpExchange(ALGlobalCGColorSpace, LCGColorSpace, nil) <> nil then CGColorSpaceRelease(LCGColorSpace);
    ALGlobalCGColorSpaceInitialized := True;

  end;
  result := ALGlobalCGColorSpace;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALCreateCGContextRef(const W, H: integer; const AData: Pointer = nil; const ABytesPerRow: Integer = -1): CGContextRef;
begin
  var LbitmapInfo: CGBitmapInfo;
  if GlobalUseMetal then LbitmapInfo := kCGImageAlphaPremultipliedFirst or kCGBitmapByteOrder32Little{Little-endian} // BGRA (The pixelformat of Metal)
  else LbitmapInfo := kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big{Big-endian}; // RGBA (The pixelformat of OpenGL)

  var LBytesPerRow: Integer;
  if ABytesPerRow < 0 then LBytesPerRow := W * 4
  else LBytesPerRow := ABytesPerRow;

  Result := CGBitmapContextCreate(
              AData, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                     //       memory block should be at least (bytesPerRow*height) bytes.
                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
              W, // width: The width, in pixels, of the required bitmap.
              H, // height: The height, in pixels, of the required bitmap.
              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                 //                   chapter of Quartz 2D Programming Guide.
              LBytesPerRow, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                            //              a value of 0 causes the value to be calculated automatically.
                            //              It's necessary to specify 'W * 4' as the 'bytesPerRow' parameter when creating the bitmap context.
                            //              Using '0' for 'bytesPerRow' allows the system to automatically determine the value, which often
                            //              includes additional padding for each row. This automatic padding can lead to issues when converting
                            //              the CGContextRef to a texture (in TCustomContextOpenGL.DoUpdateTexture)
              ALGetGlobalCGColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                       //             bitmap graphics contexts.
              LbitmapInfo); // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                            //             location in a pixel, and information about whether the pixel components are floating-point or integer
                            //             values. The constants for specifying the alpha channel information are declared with the
                            //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                            //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                            //             and CGImageAlphaInfo constants.)
                            //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                            //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                            //             Graphics Contexts chapter of Quartz 2D Programming Guide.
  if Result = nil then raise Exception.Create('Failed to create bitmap context');
  try
    // Sets the level of interpolation quality for a graphics context.
    // http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
    CGContextSetInterpolationQuality(Result, kCGInterpolationHigh);
    // Sets anti-aliasing on or off for a graphics context.
    // default: ON
    CGContextSetShouldAntialias(Result, True);
    // Sets whether or not to allow anti-aliasing for a graphics context.
    CGContextSetAllowsAntialiasing(Result, True);
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
    // CGContextSetShouldSmoothFonts(ASurface, True);
    // --
    // Sets whether or not to allow font smoothing for a graphics context.
    // CGContextSetAllowsFontSmoothing(ASurface, True);
    // --
    // Enables or disables subpixel positioning in a graphics context.
    // Subpixel positioning concerns whether or not the glyphs in a line of
    // text will be aligned to pixel boundaries or not. If subpixel positioning is
    // off then when glyphs are drawn their positions might be shifted slightly to
    // take pixel boundaries in account. This can improve the visual definition of
    // the glyphs (making them slightly less "blurry") at the expense of honoring
    // the font metrics.
    // When enabled, the graphics context may position glyphs on nonintegral pixel boundaries. When disabled,
    // the position of glyphs are always forced to integral pixel boundaries.
    // default: ON
    CGContextSetShouldSubpixelPositionFonts(Result, True);
    // Sets whether or not to allow subpixel positioning for a graphics context
    CGContextSetAllowsFontSubpixelPositioning(Result, True);
    // Enables or disables subpixel quantization in a graphics context.
    // Subpixel quantization is only enabled if subpixel positioning is enabled. Subpixel
    // quantization improves the rendering of fonts whose glyphs are at subpixel positions
    // by more closely examining how the shapes that make up the glyphs cover an individual pixel.
    // This improvement, requires additional processing so changing this value can affect text
    // drawing performance.
    // default: ON
    CGContextSetShouldSubpixelQuantizeFonts(Result, True);
    // Sets whether or not to allow subpixel quantization for a graphics context
    CGContextSetAllowsFontSubpixelQuantization(Result, True);
  except
    CGContextRelease(Result);
    raise;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALOSImageGetCgImage(const AImage: ALOSImage): cgImageRef;
begin
  {$IF defined(IOS)}
  result := AImage.cgImage;
  {$ELSE}
  result := AImage.CGImageForProposedRect(nil{proposedDestRect},nil{context},nil{hints});
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALOSImageGetWidth(const AImage: ALOSImage): Integer; inline;
begin
  {$IF defined(IOS)}
  result := CGImageGetWidth(AImage.cgImage);
  {$ELSE}
  result := CGImageGetWidth(AImage.CGImageForProposedRect(nil{proposedDestRect},nil{context},nil{hints}));
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALOSImageGetHeight(const AImage: ALOSImage): Integer; inline;
begin
  {$IF defined(IOS)}
  result := CGImageGetHeight(AImage.cgImage);
  {$ELSE}
  result := CGImageGetHeight(AImage.CGImageForProposedRect(nil{proposedDestRect},nil{context},nil{hints}));
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
//static void ALGradientEvaluateCallback(void *info, const float *in, float *out)
//{
//  /*
//  The domain of this function is 0 - 1. For an input value of 0
//  this function returns the color to paint at the start point
//  of the shading. For an input value of 1 this function returns
//  the color to paint at the end point of the shading. This
//  is a 1 in, 4 out function where the output values correspond
//  to an r,g,b,a color.
//
//  This function evaluates to produce a blend from startColor to endColor.
//  Note that the returned results are clipped to the range
//  by Quartz so this function doesn't worry about values
//  that are outside the range 0-1.
//  */
//
//  MyStartEndColor *startEndColorP = (MyStartEndColor * )info;
//  float *startColor = startEndColorP->startColor;
//  float *endColor = startEndColorP->endColor;
//  float input = in[0];
//  // Weight the starting and ending color components depending
//  // on what position in the blend the input value specifies.
//  out[0] = (startColor[0]*(1-input) + endColor[0]*input);
//  out[1] = (startColor[1]*(1-input) + endColor[1]*input);
//  out[2] = (startColor[2]*(1-input) + endColor[2]*input);
//  // The alpha component is always 1, the shading is always opaque.
//  out[3] = 1;
//}
procedure ALGradientEvaluateCallback(info: Pointer; inData: PCGFloat; outData: PAlphaColorCGFloat); cdecl;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _InterpolateColor(const AGradient: TALGradient; AOffset: Single): TAlphaColor;
  begin
    Result := 0;
    if length(AGradient.Offsets) > 1 then
    begin
      if AOffset < 0 then
        AOffset := 0;
      if AOffset > 1 then
        AOffset := 1;
      if AOffset < AGradient.Offsets[0] then
      begin
        Result := AGradient.Colors[0];
        Exit;
      end;
      if AOffset > AGradient.Offsets[High(AGradient.Offsets)] then
      begin
        Result := AGradient.Colors[High(AGradient.Offsets)];
        Exit;
      end;
      for var I := 0 to length(AGradient.Offsets) - 2 do
      begin
        if (AOffset < AGradient.Offsets[I]) then
          Continue;
        if AOffset > AGradient.Offsets[I + 1] then
          Continue;
        if AGradient.Offsets[I + 1] - AGradient.Offsets[I] <= 0 then
          Result := AGradient.Colors[I]
        else if (I = length(AGradient.Offsets) - 2) and (AOffset > AGradient.Offsets[High(AGradient.Offsets)]) then // last
          Result := AGradient.Colors[High(AGradient.Offsets)]
        else
          Result := FMX.Utils.InterpolateColor(
                      AGradient.Colors[I], AGradient.Colors[I + 1],
                      (AOffset - AGradient.Offsets[I]) / (AGradient.Offsets[I + 1] - AGradient.Offsets[I]));
      end;
    end;
  end;

begin
  if info <> nil then
    outData^ := TAlphaColorCGFloat.Create(_InterpolateColor(TALGradient(info),inData^));
end;
{$ENDIF}

{***********************************************************************}
class function TALGraphicThreadPool.CreateInstance: TALGraphicThreadPool;
begin
  result := TALGraphicThreadPool.Create(TThread.ProcessorCount);
end;

{********************************************************************}
class function TALGraphicThreadPool.GetInstance: TALGraphicThreadPool;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALGraphicThreadPool.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*****************************************}
procedure TALGraphicThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const APriority: Int64;
            const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
            Const AAsync: Boolean = True);
begin
  {$IF (not defined(ALSkiaEngine)) and (not defined(ALGpuCanvas))}
  // TCanvas/TBitmap do not work from a background thread. On Android/iOS,
  // and with Skia, we use platform API functions to draw images. However,
  // on other platforms, we resort to using TCanvas. :(
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-19673 is corrected and if yes remove the Synchronize'}
  {$ENDIF}
  TThread.Synchronize(nil,
    procedure
    begin
      try
        var LContext := AContext;
        Try
          if assigned(AProc) then aProc(LContext);
        finally
          ALFreeAndNil(LContext);
        end;
      except
        //hide the exception
      end;
    end);
  {$ELSE}
  inherited ExecuteProc(AProc, AContext, APriority, AGetPriorityFunc, AAsync);
  {$endif}
end;

{*****************************************}
procedure TALGraphicThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const APriority: Int64;
            const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
            Const AAsync: Boolean = True);
begin
  {$IF (not defined(ALSkiaEngine)) and (not defined(ALGpuCanvas))}
  // TCanvas/TBitmap do not work from a background thread. On Android/iOS,
  // and with Skia, we use platform API functions to draw images. However,
  // on other platforms, we resort to using TCanvas. :(
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-19673 is corrected and if yes remove the Synchronize'}
  {$ENDIF}
  TThread.Synchronize(nil,
    procedure
    begin
      try
        var LContext := AContext;
        Try
          if assigned(AProc) then aProc(LContext);
        finally
          ALFreeAndNil(LContext);
        end;
      except
        //hide the exception
      end;
    end);
  {$ELSE}
  inherited ExecuteProc(AProc, AContext, APriority, AGetPriorityFunc, AAsync);
  {$endif}
end;

initialization
  {$IF defined(ALSkiaAvailable)}
  ALGlobalUseRasterSkSurface := True;
  ALGlobalUseRasterSkImage := True;
  ALGlobalSkColorSpace := 0;
  ALGlobalSkColorSpaceInitialized := False;
  ALGlobalSkPaint := 0;
  //--
  var LStreamadapterProcs: sk_streamadapter_procs_t;
  LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
  LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
  LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
  LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
  sk4d_streamadapter_set_procs(@LStreamadapterProcs);
  {$ENDIF}
  {$IF defined(ANDROID)}
  ALGlobalJColorSpace := nil;
  ALGlobalJColorSpaceInitialized := False;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  ALGlobalCGColorSpace := nil;
  ALGlobalCGColorSpaceInitialized := False;
  {$ENDIF}
  TALGraphicThreadPool.FInstance := nil;
  TALGraphicThreadPool.CreateInstanceFunc := @TALGraphicThreadPool.CreateInstance;
  ALIsDefaultContextOpenGLDetermined := False;

finalization
  ALFreeAndNil(TALGraphicThreadPool.FInstance);
  {$IF defined(ALSkiaAvailable)}
  if ALGlobalSkColorSpace <> 0 then begin
    sk4d_colorspace_unref(ALGlobalSkColorSpace);
    ALGlobalSkColorSpace := 0;
  end;
  if ALGlobalSkPaint <> 0 then begin
    sk4d_paint_destroy(ALGlobalSkPaint);
    ALGlobalSkPaint := 0;
  end;
  {$ENDIF}
  {$IF defined(ANDROID)}
  ALGlobalJColorSpace := nil;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  if ALGlobalCGColorSpace <> nil then begin
    CGColorSpaceRelease(ALGlobalCGColorSpace);
    ALGlobalCGColorSpace := nil;
  end;
  {$ENDIF}

end.
