unit Alcinoe.FMX.Graphics;

interface

{$I Alcinoe.inc}

uses
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
  // !! Note: The declaration of TALSurface/TALCanvas/TALDrawable is duplicated in Alcinoe.FMX.Common to avoid a circular unit reference.
  TALSurface =  {$IF defined(ALSkiaEngine)}sk_surface_t{$ELSEIF defined(ANDROID)}Jbitmap{$ELSEIF defined(ALAppleOS)}CGContextRef{$ELSE}Tbitmap{$ENDIF};
  TALCanvas =   {$IF defined(ALSkiaEngine)}sk_canvas_t{$ELSEIF defined(ANDROID)}Jcanvas{$ELSEIF defined(ALAppleOS)}CGContextRef{$ELSE}Tcanvas{$ENDIF};
  TALDrawable = {$IF defined(ALSkiaCanvas)}sk_image_t{$ELSEIF defined(ALGpuCanvas)}TTexture{$ELSE}Tbitmap{$ENDIF};

const
  ALNullSurface = {$IF defined(ALSkiaEngine)}0{$ELSE}Nil{$ENDIF};
  ALNullCanvas = {$IF defined(ALSkiaEngine)}0{$ELSE}Nil{$ENDIF};
  ALNullDrawable = {$IF defined(ALSkiaCanvas)}0{$ELSE}Nil{$ENDIF};

function  ALIsSurfaceNull(const aSurface: TALSurface): Boolean; inline;
function  ALIsCanvasNull(const aCanvas: TALCanvas): Boolean; inline;
function  ALIsDrawableNull(const aDrawable: TALDrawable): Boolean; inline;
procedure ALFreeAndNilDrawable(var aDrawable: TALDrawable); inline;
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
procedure ALExtractMatrixFromCanvas(const ACanvas: TALCanvas; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
function  ALScaleAndCenterCanvas(
            Const ACanvas: TCanvas;
            Const AAbsoluteRect: TRectF;
            Const AScaleX: Single;
            Const AScaleY: Single;
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
function ALCreateBitmapFromSkPixmap(Const APixmap: sk_pixmap_t): TBitmap;
function ALCreateBitmapFromSkSurface(Const ASurface: sk_surface_t): TBitmap;
Function ALCreateBitmapFromSkImage(const AImage: sk_image_t): TBitmap;
procedure ALUpdateBitmapFromSkPixmap(Const APixmap: sk_pixmap_t; const ABitmap: Tbitmap);
procedure ALUpdateBitmapFromSkSurface(Const ASurface: sk_surface_t; const ABitmap: TBitmap);
procedure ALUpdateBitmapFromSkImage(const AImage: sk_image_t; const ABitmap: TBitmap);
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
function ALCreateBitmapFromCGContextRef(const aCGContextRef: CGContextRef): TBitmap;
procedure ALUpdateTextureFromCGContextRef(const aCGContextRef: CGContextRef; const ATexture: TTexture);
procedure ALUpdateBitmapFromCGContextRef(const aCGContextRef: CGContextRef; const ABitmap: TBitmap);
{$ENDIF}
{$IFDEF ALGpuCanvas}
function ALCreateTextureFromBitmapSurface(const aBitmapSurface: TbitmapSurface): TTexture;
function ALCreateTextureFromBitmap(const aBitmap: Tbitmap): TTexture;
procedure ALUpdateTextureFromBitmapSurface(const aBitmapSurface: TbitmapSurface; const ATexture: TTexture);
procedure ALUpdateTextureFromBitmap(const aBitmap: Tbitmap; const ATexture: TTexture);
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
  TalExifOrientationInfo = (FLIP_HORIZONTAL,
                            FLIP_VERTICAL,
                            NORMAL,
                            ROTATE_180,
                            ROTATE_270,
                            ROTATE_90,
                            TRANSPOSE,
                            TRANSVERSE,
                            UNDEFINED);

function ALGetImageSize(const aStream: TStream): TSize;
function AlGetExifOrientationInfo(const aFilename: String): TalExifOrientationInfo;
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
function ALGetShadowWidth(const AShadowBlur: Single): Single;
procedure ALGetLinearGradientCoordinates(const ASize: TSizeF; const AAngle: Single; out AStartPoint: TPointF; out AEndPoint: TPointF; const ACssAngleConvention: Boolean = True);
function ALGetShapeSurfaceRect(
           const ARect: TRectF;
           const AFillColor: TAlphaColor;
           const AFillGradientColors: TArray<TAlphaColor>;
           const AFillResourceName: String;
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
           var ARect: TrectF;
           const AFill: TALBrush;
           const AStateLayer: TALStateLayer;
           const AShadow: TALShadow): TRectF; overload;
function ALCreateEmptyDrawable1x1: TALDrawable;

////////////////////////////////////////////////////////////////////////////////
/// THE CODE BELOW (INTERFACE + IMPLEMENTATION) WAS AUTO-GENERATED FROM      ///
/// <ALCINOE>\References\FMXGraphicsBuilder.                                 ///
////////////////////////////////////////////////////////////////////////////////

{$REGION ' Load and FitInto'}
// Resize the src image to make that one side fit w or h keeping the other side equal or lower than w or h
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
function ALLoadFromStreamAndFitIntoToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
function ALLoadFromResourceAndFitIntoToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
function ALLoadFromFileAndFitIntoToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
//--
function ALLoadFromStreamAndFitIntoToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
function ALLoadFromResourceAndFitIntoToSkImage(const AResName: String; const W, H: single): sk_image_t;
function ALLoadFromFileAndFitIntoToSkImage(const AFileName: String; const W, H: single): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
function ALLoadFromStreamAndFitIntoToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
function ALLoadFromResourceAndFitIntoToJBitmap(const AResName: String; const W, H: single): JBitmap;
function ALLoadFromFileAndFitIntoToJBitmap(const AFileName: String; const W, H: single): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
function ALLoadFromStreamAndFitIntoToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
function ALLoadFromResourceAndFitIntoToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
function ALLoadFromFileAndFitIntoToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
//--
function ALLoadFromStreamAndFitIntoToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
function ALLoadFromResourceAndFitIntoToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
function ALLoadFromFileAndFitIntoToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndFitIntoToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
function ALLoadFromStreamAndFitIntoToBitmap(const AStream: TStream; const W, H: single): TBitmap;
function ALLoadFromResourceAndFitIntoToBitmap(const AResName: String; const W, H: single): TBitmap;
function ALLoadFromFileAndFitIntoToBitmap(const AFileName: String; const W, H: single): TBitmap;
//--
function ALLoadFromStreamAndFitIntoToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
function ALLoadFromResourceAndFitIntoToDrawable(const AResName: String; const W, H: single): TALDrawable;
function ALLoadFromFileAndFitIntoToDrawable(const AFileName: String; const W, H: single): TALDrawable;
{$ENDREGION}

{$REGION ' Load and FitInto and Crop'}
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as rect
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToSkSurface(const AImage: sk_image_t; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromStreamAndFitIntoAndCropToSkSurface(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromResourceAndFitIntoAndCropToSkSurface(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromFileAndFitIntoAndCropToSkSurface(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
//--
function ALLoadFromStreamAndFitIntoAndCropToSkImage(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromResourceAndFitIntoAndCropToSkImage(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromFileAndFitIntoAndCropToSkImage(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(const ABitmap: JBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromStreamAndFitIntoAndCropToJBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromResourceAndFitIntoAndCropToJBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromFileAndFitIntoAndCropToJBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(const AImage: ALOSImage; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromStreamAndFitIntoAndCropToCGContextRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromResourceAndFitIntoAndCropToCGContextRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromFileAndFitIntoAndCropToCGContextRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
//--
function ALLoadFromStreamAndFitIntoAndCropToCGImageRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromResourceAndFitIntoAndCropToCGImageRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromFileAndFitIntoAndCropToCGImageRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndFitIntoAndCropToBitmap(const ABitmap: TBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromStreamAndFitIntoAndCropToBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromResourceAndFitIntoAndCropToBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromFileAndFitIntoAndCropToBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
//--
function ALLoadFromStreamAndFitIntoAndCropToDrawable(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromResourceAndFitIntoAndCropToDrawable(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromFileAndFitIntoAndCropToDrawable(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
{$ENDREGION}

{$REGION ' Load and FitInto and Crop to RoundRect'}
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as round rect
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(const AImage: sk_image_t; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromResourceAndFitIntoAndCropToRoundRectSkSurface(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromFileAndFitIntoAndCropToRoundRectSkSurface(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
//--
function ALLoadFromStreamAndFitIntoAndCropToRoundRectSkImage(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromResourceAndFitIntoAndCropToRoundRectSkImage(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromFileAndFitIntoAndCropToRoundRectSkImage(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToRoundRectJBitmap(const ABitmap: JBitmap; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromStreamAndFitIntoAndCropToRoundRectJBitmap(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromResourceAndFitIntoAndCropToRoundRectJBitmap(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromFileAndFitIntoAndCropToRoundRectJBitmap(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToRoundRectCGContextRef(const AImage: ALOSImage; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromResourceAndFitIntoAndCropToRoundRectCGContextRef(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromFileAndFitIntoAndCropToRoundRectCGContextRef(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
//--
function ALLoadFromStreamAndFitIntoAndCropToRoundRectCGImageRef(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromResourceAndFitIntoAndCropToRoundRectCGImageRef(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromFileAndFitIntoAndCropToRoundRectCGImageRef(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndFitIntoAndCropToRoundRectBitmap(const ABitmap: TBitmap; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromStreamAndFitIntoAndCropToRoundRectBitmap(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromResourceAndFitIntoAndCropToRoundRectBitmap(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromFileAndFitIntoAndCropToRoundRectBitmap(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
//--
function ALLoadFromStreamAndFitIntoAndCropToRoundRectDrawable(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromResourceAndFitIntoAndCropToRoundRectDrawable(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromFileAndFitIntoAndCropToRoundRectDrawable(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
{$ENDREGION}

{$REGION ' Load and FitInto and Crop to Circle'}
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as circle
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(const AImage: sk_image_t; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromResourceAndFitIntoAndCropToCircleSkSurface(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromFileAndFitIntoAndCropToCircleSkSurface(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
//--
function ALLoadFromStreamAndFitIntoAndCropToCircleSkImage(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromResourceAndFitIntoAndCropToCircleSkImage(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromFileAndFitIntoAndCropToCircleSkImage(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToCircleJBitmap(const ABitmap: JBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromStreamAndFitIntoAndCropToCircleJBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromResourceAndFitIntoAndCropToCircleJBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromFileAndFitIntoAndCropToCircleJBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToCircleCGContextRef(const AImage: ALOSImage; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromResourceAndFitIntoAndCropToCircleCGContextRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromFileAndFitIntoAndCropToCircleCGContextRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
//--
function ALLoadFromStreamAndFitIntoAndCropToCircleCGImageRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromResourceAndFitIntoAndCropToCircleCGImageRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromFileAndFitIntoAndCropToCircleCGImageRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndFitIntoAndCropToCircleBitmap(const ABitmap: TBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromStreamAndFitIntoAndCropToCircleBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromResourceAndFitIntoAndCropToCircleBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromFileAndFitIntoAndCropToCircleBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
//--
function ALLoadFromStreamAndFitIntoAndCropToCircleDrawable(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromResourceAndFitIntoAndCropToCircleDrawable(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromFileAndFitIntoAndCropToCircleDrawable(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
{$ENDREGION}

{$REGION ' Load and FitInto and Crop and Blur'}
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as rect
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromFileAndFitIntoAndCropAndBlurToSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
//--
function ALLoadFromStreamAndFitIntoAndCropAndBlurToSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToSkImage(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromFileAndFitIntoAndCropAndBlurToSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromStreamAndFitIntoAndCropAndBlurToJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromFileAndFitIntoAndCropAndBlurToJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
//--
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromStreamAndFitIntoAndCropAndBlurToBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromFileAndFitIntoAndCropAndBlurToBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
//--
function ALLoadFromStreamAndFitIntoAndCropAndBlurToDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToDrawable(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromFileAndFitIntoAndCropAndBlurToDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
{$ENDREGION}

{$REGION ' Load and FitInto and Crop and Blur to Circle'}
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as circle
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
//--
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkImage(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndBlurToCircleJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndBlurToCircleCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
//--
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndFitIntoAndCropAndBlurToCircleBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
//--
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleDrawable(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
{$ENDREGION}

{$REGION ' Load and FitInto and Crop and Mask'}
// https://i.stack.imgur.com/CcESX.png - transparent pixel in the mask are removed from the resulting image
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(const AImage: sk_image_t; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(const AStream: TStream; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromResourceAndFitIntoAndCropAndMaskToSkSurface(const AResName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromFileAndFitIntoAndCropAndMaskToSkSurface(const AFileName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
//--
function ALLoadFromStreamAndFitIntoAndCropAndMaskToSkImage(const AStream: TStream; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromResourceAndFitIntoAndCropAndMaskToSkImage(const AResName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromFileAndFitIntoAndCropAndMaskToSkImage(const AFileName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndMaskToJBitmap(const ABitmap: JBitmap; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromStreamAndFitIntoAndCropAndMaskToJBitmap(const AStream: TStream; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromResourceAndFitIntoAndCropAndMaskToJBitmap(const AResName: String; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromFileAndFitIntoAndCropAndMaskToJBitmap(const AFileName: String; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndMaskToCGContextRef(const AImage: ALOSImage; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(const AStream: TStream; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromResourceAndFitIntoAndCropAndMaskToCGContextRef(const AResName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromFileAndFitIntoAndCropAndMaskToCGContextRef(const AFileName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
//--
function ALLoadFromStreamAndFitIntoAndCropAndMaskToCGImageRef(const AStream: TStream; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromResourceAndFitIntoAndCropAndMaskToCGImageRef(const AResName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromFileAndFitIntoAndCropAndMaskToCGImageRef(const AFileName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndFitIntoAndCropAndMaskToBitmap(const ABitmap: TBitmap; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromStreamAndFitIntoAndCropAndMaskToBitmap(const AStream: TStream; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromResourceAndFitIntoAndCropAndMaskToBitmap(const AResName: String; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromFileAndFitIntoAndCropAndMaskToBitmap(const AFileName: String; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
//--
function ALLoadFromStreamAndFitIntoAndCropAndMaskToDrawable(const AStream: TStream; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromResourceAndFitIntoAndCropAndMaskToDrawable(const AResName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromFileAndFitIntoAndCropAndMaskToDrawable(const AFileName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
{$ENDREGION}

{$REGION ' Load and FitInto and Crop and Mask and Blur'}
// https://i.stack.imgur.com/CcESX.png - transparent pixel in the mask are removed from the resulting image
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AImage: sk_image_t; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AStream: TStream; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AResName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AFileName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
//--
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkImage(const AStream: TStream; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkImage(const AResName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkImage(const AFileName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndMaskAndBlurToJBitmap(const ABitmap: JBitmap; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AStream: TStream; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AResName: String; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AFileName: String; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AImage: ALOSImage; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AStream: TStream; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AResName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AFileName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
//--
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AStream: TStream; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AResName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AFileName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndFitIntoAndCropAndMaskAndBlurToBitmap(const ABitmap: TBitmap; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToBitmap(const AStream: TStream; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToBitmap(const AResName: String; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToBitmap(const AFileName: String; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
//--
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToDrawable(const AStream: TStream; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToDrawable(const AResName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToDrawable(const AFileName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
{$ENDREGION}

{$REGION ' Load and PlaceInto'}
// If any dimension of the image is greater than W or H then the image is scaled down to best fit W and H
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndPlaceIntoToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
function ALLoadFromStreamAndPlaceIntoToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
function ALLoadFromResourceAndPlaceIntoToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
function ALLoadFromFileAndPlaceIntoToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
//--
function ALLoadFromStreamAndPlaceIntoToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
function ALLoadFromResourceAndPlaceIntoToSkImage(const AResName: String; const W, H: single): sk_image_t;
function ALLoadFromFileAndPlaceIntoToSkImage(const AFileName: String; const W, H: single): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndPlaceIntoToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
function ALLoadFromStreamAndPlaceIntoToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
function ALLoadFromResourceAndPlaceIntoToJBitmap(const AResName: String; const W, H: single): JBitmap;
function ALLoadFromFileAndPlaceIntoToJBitmap(const AFileName: String; const W, H: single): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndPlaceIntoToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
function ALLoadFromStreamAndPlaceIntoToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
function ALLoadFromResourceAndPlaceIntoToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
function ALLoadFromFileAndPlaceIntoToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
//--
function ALLoadFromStreamAndPlaceIntoToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
function ALLoadFromResourceAndPlaceIntoToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
function ALLoadFromFileAndPlaceIntoToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndPlaceIntoToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
function ALLoadFromStreamAndPlaceIntoToBitmap(const AStream: TStream; const W, H: single): TBitmap;
function ALLoadFromResourceAndPlaceIntoToBitmap(const AResName: String; const W, H: single): TBitmap;
function ALLoadFromFileAndPlaceIntoToBitmap(const AFileName: String; const W, H: single): TBitmap;
//--
function ALLoadFromStreamAndPlaceIntoToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
function ALLoadFromResourceAndPlaceIntoToDrawable(const AResName: String; const W, H: single): TALDrawable;
function ALLoadFromFileAndPlaceIntoToDrawable(const AFileName: String; const W, H: single): TALDrawable;
{$ENDREGION}

{$REGION ' Load and PlaceInto and Blur'}
// If any dimension of the image is greater than W or H then the image is scaled down to best fit W and H
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single): sk_surface_t;
function ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single): sk_surface_t;
function ALLoadFromResourceAndPlaceIntoAndBlurToSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single): sk_surface_t;
function ALLoadFromFileAndPlaceIntoAndBlurToSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single): sk_surface_t;
//--
function ALLoadFromStreamAndPlaceIntoAndBlurToSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single): sk_image_t;
function ALLoadFromResourceAndPlaceIntoAndBlurToSkImage(const AResName: String; const W, H: single; const ABlurRadius: single): sk_image_t;
function ALLoadFromFileAndPlaceIntoAndBlurToSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndPlaceIntoAndBlurToJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single): JBitmap;
function ALLoadFromStreamAndPlaceIntoAndBlurToJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single): JBitmap;
function ALLoadFromResourceAndPlaceIntoAndBlurToJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single): JBitmap;
function ALLoadFromFileAndPlaceIntoAndBlurToJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndPlaceIntoAndBlurToCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single): CGContextRef;
function ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single): CGContextRef;
function ALLoadFromResourceAndPlaceIntoAndBlurToCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single): CGContextRef;
function ALLoadFromFileAndPlaceIntoAndBlurToCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single): CGContextRef;
//--
function ALLoadFromStreamAndPlaceIntoAndBlurToCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single): CGImageRef;
function ALLoadFromResourceAndPlaceIntoAndBlurToCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single): CGImageRef;
function ALLoadFromFileAndPlaceIntoAndBlurToCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndPlaceIntoAndBlurToBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single): TBitmap;
function ALLoadFromStreamAndPlaceIntoAndBlurToBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single): TBitmap;
function ALLoadFromResourceAndPlaceIntoAndBlurToBitmap(const AResName: String; const W, H: single; const ABlurRadius: single): TBitmap;
function ALLoadFromFileAndPlaceIntoAndBlurToBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single): TBitmap;
//--
function ALLoadFromStreamAndPlaceIntoAndBlurToDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single): TALDrawable;
function ALLoadFromResourceAndPlaceIntoAndBlurToDrawable(const AResName: String; const W, H: single; const ABlurRadius: single): TALDrawable;
function ALLoadFromFileAndPlaceIntoAndBlurToDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single): TALDrawable;
{$ENDREGION}

{$REGION ' Load and Stretch'}
// Resize the src image to make that width = w and height = h
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndStretchToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
function ALLoadFromStreamAndStretchToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
function ALLoadFromResourceAndStretchToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
function ALLoadFromFileAndStretchToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
//--
function ALLoadFromStreamAndStretchToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
function ALLoadFromResourceAndStretchToSkImage(const AResName: String; const W, H: single): sk_image_t;
function ALLoadFromFileAndStretchToSkImage(const AFileName: String; const W, H: single): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndStretchToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
function ALLoadFromStreamAndStretchToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
function ALLoadFromResourceAndStretchToJBitmap(const AResName: String; const W, H: single): JBitmap;
function ALLoadFromFileAndStretchToJBitmap(const AFileName: String; const W, H: single): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndStretchToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
function ALLoadFromStreamAndStretchToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
function ALLoadFromResourceAndStretchToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
function ALLoadFromFileAndStretchToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
//--
function ALLoadFromStreamAndStretchToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
function ALLoadFromResourceAndStretchToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
function ALLoadFromFileAndStretchToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndStretchToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
function ALLoadFromStreamAndStretchToBitmap(const AStream: TStream; const W, H: single): TBitmap;
function ALLoadFromResourceAndStretchToBitmap(const AResName: String; const W, H: single): TBitmap;
function ALLoadFromFileAndStretchToBitmap(const AFileName: String; const W, H: single): TBitmap;
//--
function ALLoadFromStreamAndStretchToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
function ALLoadFromResourceAndStretchToDrawable(const AResName: String; const W, H: single): TALDrawable;
function ALLoadFromFileAndStretchToDrawable(const AFileName: String; const W, H: single): TALDrawable;
{$ENDREGION}

{$REGION ' Load and Wrap'}
// Wrap the image inside w and h
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndWrapToSkSurface(const AImage: sk_image_t; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
function ALLoadFromStreamAndWrapToSkSurface(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
function ALLoadFromResourceAndWrapToSkSurface(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
function ALLoadFromFileAndWrapToSkSurface(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
//--
function ALLoadFromStreamAndWrapToSkImage(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
function ALLoadFromResourceAndWrapToSkImage(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
function ALLoadFromFileAndWrapToSkImage(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndWrapToJBitmap(const ABitmap: JBitmap; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
function ALLoadFromStreamAndWrapToJBitmap(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
function ALLoadFromResourceAndWrapToJBitmap(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
function ALLoadFromFileAndWrapToJBitmap(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndWrapToCGContextRef(const AImage: ALOSImage; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
function ALLoadFromStreamAndWrapToCGContextRef(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
function ALLoadFromResourceAndWrapToCGContextRef(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
function ALLoadFromFileAndWrapToCGContextRef(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
//--
function ALLoadFromStreamAndWrapToCGImageRef(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
function ALLoadFromResourceAndWrapToCGImageRef(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
function ALLoadFromFileAndWrapToCGImageRef(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
{$ENDIF}

function ALLoadFromBitmapAndWrapToBitmap(const ABitmap: TBitmap; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
function ALLoadFromStreamAndWrapToBitmap(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
function ALLoadFromResourceAndWrapToBitmap(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
function ALLoadFromFileAndWrapToBitmap(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
//--
function ALLoadFromStreamAndWrapToDrawable(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
function ALLoadFromResourceAndWrapToDrawable(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
function ALLoadFromFileAndWrapToDrawable(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
{$ENDREGION}

{$REGION ' Load and NormalizeOrientation'}
// Normalize the orientation
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndNormalizeOrientationToSkSurface(const AImage: sk_image_t; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
function ALLoadFromStreamAndNormalizeOrientationToSkSurface(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
function ALLoadFromResourceAndNormalizeOrientationToSkSurface(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
function ALLoadFromFileAndNormalizeOrientationToSkSurface(const AFileName: String): sk_surface_t;
//--
function ALLoadFromStreamAndNormalizeOrientationToSkImage(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): sk_image_t;
function ALLoadFromResourceAndNormalizeOrientationToSkImage(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): sk_image_t;
function ALLoadFromFileAndNormalizeOrientationToSkImage(const AFileName: String): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndNormalizeOrientationToJBitmap(const ABitmap: JBitmap; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
function ALLoadFromStreamAndNormalizeOrientationToJBitmap(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
function ALLoadFromResourceAndNormalizeOrientationToJBitmap(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
function ALLoadFromFileAndNormalizeOrientationToJBitmap(const AFileName: String): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndNormalizeOrientationToCGContextRef(const AImage: ALOSImage; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
function ALLoadFromStreamAndNormalizeOrientationToCGContextRef(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
function ALLoadFromResourceAndNormalizeOrientationToCGContextRef(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
function ALLoadFromFileAndNormalizeOrientationToCGContextRef(const AFileName: String): CGContextRef;
//--
function ALLoadFromStreamAndNormalizeOrientationToCGImageRef(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): CGImageRef;
function ALLoadFromResourceAndNormalizeOrientationToCGImageRef(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): CGImageRef;
function ALLoadFromFileAndNormalizeOrientationToCGImageRef(const AFileName: String): CGImageRef;
{$ENDIF}

function ALLoadFromStreamAndNormalizeOrientationToBitmap(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): TBitmap;
function ALLoadFromResourceAndNormalizeOrientationToBitmap(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): TBitmap;
function ALLoadFromFileAndNormalizeOrientationToBitmap(const AFileName: String): TBitmap;
//--
function ALLoadFromStreamAndNormalizeOrientationToDrawable(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): TALDrawable;
function ALLoadFromResourceAndNormalizeOrientationToDrawable(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): TALDrawable;
function ALLoadFromFileAndNormalizeOrientationToDrawable(const AFileName: String): TALDrawable;
{$ENDREGION}

{$REGION ' Load'}
// Do not resize anything
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageToSkSurface(const AImage: sk_image_t): sk_surface_t;
function ALLoadFromStreamToSkSurface(const AStream: TStream): sk_surface_t;
function ALLoadFromResourceToSkSurface(const AResName: String): sk_surface_t;
function ALLoadFromFileToSkSurface(const AFileName: String): sk_surface_t;
//--
function ALLoadFromStreamToSkImage(const AStream: TStream): sk_image_t;
function ALLoadFromResourceToSkImage(const AResName: String): sk_image_t;
function ALLoadFromFileToSkImage(const AFileName: String): sk_image_t;
{$ENDIF}

{$IF defined(ANDROID)}
function ALLoadFromStreamToJBitmap(const AStream: TStream): JBitmap;
function ALLoadFromResourceToJBitmap(const AResName: String): JBitmap;
function ALLoadFromFileToJBitmap(const AFileName: String): JBitmap;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALLoadFromOSImageToCGContextRef(const AImage: ALOSImage): CGContextRef;
function ALLoadFromStreamToCGContextRef(const AStream: TStream): CGContextRef;
function ALLoadFromResourceToCGContextRef(const AResName: String): CGContextRef;
function ALLoadFromFileToCGContextRef(const AFileName: String): CGContextRef;
//--
function ALLoadFromStreamToCGImageRef(const AStream: TStream): CGImageRef;
function ALLoadFromResourceToCGImageRef(const AResName: String): CGImageRef;
function ALLoadFromFileToCGImageRef(const AFileName: String): CGImageRef;
{$ENDIF}

function ALLoadFromStreamToBitmap(const AStream: TStream): TBitmap;
function ALLoadFromResourceToBitmap(const AResName: String): TBitmap;
function ALLoadFromFileToBitmap(const AFileName: String): TBitmap;
//--
function ALLoadFromStreamToDrawable(const AStream: TStream): TALDrawable;
function ALLoadFromResourceToDrawable(const AResName: String): TALDrawable;
function ALLoadFromFileToDrawable(const AFileName: String): TALDrawable;
{$ENDREGION}

////////////////////////////////////////////////////////////////////////////////
/// THE CODE ABOVE (INTERFACE + IMPLEMENTATION) WAS AUTO-GENERATED FROM      ///
/// <ALCINOE>\References\FMXGraphicsBuilder.                                 ///
////////////////////////////////////////////////////////////////////////////////

{************************}
procedure ALDrawRectangle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFillColor: TAlphaColor;
            const AFillGradientStyle: TGradientStyle;
            const AFillGradientColors: TArray<TAlphaColor>;
            const AFillGradientOffsets: TArray<Single>;
            const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
            const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
            const AFillResourceName: String;
            Const AFillWrapMode: TALImageWrapMode;
            Const AFillBackgroundMarginsRect: TRectF;
            Const AFillImageMarginsRect: TRectF;
            const AStateLayerOpacity: Single;
            const AStateLayerColor: TAlphaColor;
            Const AStateLayerMarginsRect: TRectF;
            const AStateLayerXRadius: Single;
            const AStateLayerYRadius: Single;
            const ADrawStateLayerOnTop: Boolean;
            const AStrokeColor: TalphaColor;
            const AStrokeThickness: Single;
            const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
            const AShadowBlur: Single;
            const AShadowOffsetX: Single;
            const AShadowOffsetY: Single;
            const ASides: TSides;
            const ACorners: TCorners;
            const AXRadius: Single;
            const AYRadius: Single); overload;
procedure ALDrawRectangle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFillColor: TAlphaColor;
            const AFillGradientStyle: TGradientStyle;
            const AFillGradientColors: TArray<TAlphaColor>;
            const AFillGradientOffsets: TArray<Single>;
            const AFillGradientAngle: Single;
            const AFillResourceName: String;
            Const AFillWrapMode: TALImageWrapMode;
            Const AFillBackgroundMarginsRect: TRectF;
            Const AFillImageMarginsRect: TRectF;
            const AStateLayerOpacity: Single;
            const AStateLayerColor: TAlphaColor;
            Const AStateLayerMarginsRect: TRectF;
            const AStateLayerXRadius: Single;
            const AStateLayerYRadius: Single;
            const ADrawStateLayerOnTop: Boolean;
            const AStrokeColor: TalphaColor;
            const AStrokeThickness: Single;
            const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
            const AShadowBlur: Single;
            const AShadowOffsetX: Single;
            const AShadowOffsetY: Single;
            const ASides: TSides;
            const ACorners: TCorners;
            const AXRadius: Single;
            const AYRadius: Single); overload;
procedure ALDrawRectangle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow; // If shadow is not nil, then the Canvas must have enough space to draw the shadow (approximately Shadow.blur on each side of the rectangle)
            const ASides: TSides;
            const ACorners: TCorners;
            const AXRadius: Single;
            const AYRadius: Single); overload;

{*********************}
procedure ALDrawCircle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFillColor: TAlphaColor;
            const AFillGradientStyle: TGradientStyle;
            const AFillGradientColors: TArray<TAlphaColor>;
            const AFillGradientOffsets: TArray<Single>;
            const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
            const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
            const AFillResourceName: String;
            Const AFillWrapMode: TALImageWrapMode;
            Const AFillBackgroundMarginsRect: TRectF;
            Const AFillImageMarginsRect: TRectF;
            const AStateLayerOpacity: Single;
            const AStateLayerColor: TAlphaColor;
            Const AStateLayerMarginsRect: TRectF;
            const AStateLayerXRadius: Single;
            const AStateLayerYRadius: Single;
            const ADrawStateLayerOnTop: Boolean;
            const AStrokeColor: TalphaColor;
            const AStrokeThickness: Single;
            const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
            const AShadowBlur: Single;
            const AShadowOffsetX: Single;
            const AShadowOffsetY: Single); overload;
procedure ALDrawCircle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFillColor: TAlphaColor;
            const AFillGradientStyle: TGradientStyle;
            const AFillGradientColors: TArray<TAlphaColor>;
            const AFillGradientOffsets: TArray<Single>;
            const AFillGradientAngle: Single;
            const AFillResourceName: String;
            Const AFillWrapMode: TALImageWrapMode;
            Const AFillBackgroundMarginsRect: TRectF;
            Const AFillImageMarginsRect: TRectF;
            const AStateLayerOpacity: Single;
            const AStateLayerColor: TAlphaColor;
            Const AStateLayerMarginsRect: TRectF;
            const AStateLayerXRadius: Single;
            const AStateLayerYRadius: Single;
            const ADrawStateLayerOnTop: Boolean;
            const AStrokeColor: TalphaColor;
            const AStrokeThickness: Single;
            const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
            const AShadowBlur: Single;
            const AShadowOffsetX: Single;
            const AShadowOffsetY: Single); overload;
procedure ALDrawCircle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow); overload; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.

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
  public
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const APriority: Int64;
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); override;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                const AExtData: Tobject; // ExtData will be free by the worker thread
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
  Alcinoe.AndroidApi.Common,
  Alcinoe.Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IF defined(IOS)}
  Fmx.Utils,
  iOSapi.Foundation,
  iOSapi.CoreImage,
  iOSapi.Helpers,
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Alcinoe.iOSapi.ImageIO,
  Alcinoe.iOSapi.CoreImage,
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
  if ATexture is TALTexture then
    TALTexture(ATexture).Assign(aBitmap)
  else
    raise Exception.Create('Only TALTexture is supported currently');
end;
{$ENDIF}

{****************}
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

{****************}
{$IF defined(ALAppleOS)}
function ALCreateBitmapFromCGContextRef(const aCGContextRef: CGContextRef): TBitmap;
begin
  Result := TBitmap.Create(CGBitmapContextGetWidth(aCGContextRef), CGBitmapContextGetHeight(aCGContextRef));
  try
    ALUpdateBitmapFromCGContextRef(aCGContextRef, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;
{$ENDIF}

{****************}
{$IF defined(ALAppleOS)}
procedure ALUpdateTextureFromCGContextRef(const aCGContextRef: CGContextRef; const ATexture: TTexture);
begin
  ATexture.UpdateTexture(CGBitmapContextGetData(aCGContextRef), CGBitmapContextGetBytesPerRow(aCGContextRef));
end;
{$ENDIF}

{****************}
{$IF defined(ALAppleOS)}
procedure ALUpdateBitmapFromCGContextRef(const aCGContextRef: CGContextRef; const ABitmap: TBitmap);
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
function ALCreateTextureFromBitmapSurface(const aBitmapSurface: TbitmapSurface): TTexture;
begin
  result := TALTexture.Create;
  try
    ALUpdateTextureFromBitmapSurface(aBitmapSurface, Result);
  except
    ALFreeAndNil(result);
    raise;
  end;
end;
{$ENDIF}

{******************}
{$IFDEF ALGpuCanvas}
function ALCreateTextureFromBitmap(const aBitmap: Tbitmap): TTexture;
begin
  Result := TalTexture.Create;
  try
    ALUpdateTextureFromBitmap(aBitmap, Result);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;
{$ENDIF}

{******************}
{$IFDEF ALGpuCanvas}
procedure ALUpdateTextureFromBitmapSurface(const aBitmapSurface: TbitmapSurface; const ATexture: TTexture);
begin
  ATexture.Assign(aBitmapSurface);
end;
{$ENDIF}

{******************}
{$IFDEF ALGpuCanvas}
procedure ALUpdateTextureFromBitmap(const aBitmap: Tbitmap; const ATexture: TTexture);
begin
  ATexture.assign(aBitmap);
end;
{$ENDIF}



////////////////////////////////////////////////////////////////////////////////
/// THE CODE BELOW (INTERFACE + IMPLEMENTATION) WAS AUTO-GENERATED FROM      ///
/// <ALCINOE>\References\FMXGraphicsBuilder.                                 ///
////////////////////////////////////////////////////////////////////////////////

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
begin
  var LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
  Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(AImage, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoToSkSurface(LImage, W, H);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToSkSurface(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoToSkSurface(LImage, W, H);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoToSkSurface(LImage, W, H);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoToSkImage(const AResName: String; const W, H: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToSkImage(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoToSkImage(const AFileName: String; const W, H: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoToSkSurface(LImage, W, H);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight);
  var LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
  Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(ABitmap, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoToJBitmap(LBitmap, W, H);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoToJBitmap(const AResName: String; const W, H: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToJBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoToJBitmap(const AFileName: String; const W, H: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoToJBitmap(LBitmap, W, H);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
begin
  var LSrcRect := TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage));
  var LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
  result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoToCGContextRef(LImage, W, H);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToCGContextRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoToCGContextRef(LImage, W, H);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoToCGContextRef(AStream, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToCGImageRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoToCGContextRef(AFileName, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{***********************************************************************************************}
function ALLoadFromBitmapAndFitIntoToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.width, ABitmap.height);
  var LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
  Result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(ABitmap, LDestRect.Width, LDestRect.Height);
end;

{***********************************************************************************************}
function ALLoadFromStreamAndFitIntoToBitmap(const AStream: TStream; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*************************************************************************************************}
function ALLoadFromResourceAndFitIntoToBitmap(const AResName: String; const W, H: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoToBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************}
function ALLoadFromFileAndFitIntoToBitmap(const AFileName: String; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*****************************************************************************************************}
function ALLoadFromStreamAndFitIntoToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoToSkImage(AStream, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoToSkSurface(AStream, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoToSkSurface(AStream, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoToJBitmap(AStream, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoToCGContextRef(AStream, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoToBitmap(AStream, W, H);
  {$ENDIF}
end;

{*******************************************************************************************************}
function ALLoadFromResourceAndFitIntoToDrawable(const AResName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoToSkImage(AResName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoToSkSurface(AResName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoToSkSurface(AResName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoToJBitmap(AResName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoToCGContextRef(AResName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoToBitmap(AResName, W, H);
  {$ENDIF}
end;

{****************************************************************************************************}
function ALLoadFromFileAndFitIntoToDrawable(const AFileName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoToSkImage(AFileName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoToSkSurface(AFileName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoToSkSurface(AFileName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoToJBitmap(AFileName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoToCGContextRef(AFileName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoToBitmap(AFileName, W, H);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToSkSurface(const AImage: sk_image_t; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AImage, // const image: sk_image_t;
      @LSrcRect, // const src: psk_rect_t;
      @LDestRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToSkSurface(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(LImage, W, H, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToSkSurface(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToSkSurface(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToSkSurface(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(LImage, W, H, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToSkImage(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(LImage, W, H, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToSkImage(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToSkImage(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToSkImage(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(LImage, W, H, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(const ABitmap: JBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).Round;

  var LMatrix := TJMatrix.JavaClass.init;
  LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
  result := TJBitmap.JavaClass.createBitmap(ABitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
  LMatrix := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropToJBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropToJBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToJBitmap(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropToJBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(const AImage: ALOSImage; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LRatio: single;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage)), TpointF.create(XCropCenter, YCropCenter), LRatio);
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(
        0-(LSrcRect.Left*LRatio),
        0-(LSrcRect.top*LRatio)),
      LDestRect.width + (LSrcRect.Left*LRatio) + ((ALOSImageGetWidth(AImage)-LSrcRect.right)*LRatio),
      LDestRect.height + (LSrcRect.top*LRatio)  + ((ALOSImageGetHeight(AImage)-LSrcRect.bottom)*LRatio),
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToCGContextRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(LImage, W, H, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToCGContextRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCGContextRef(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToCGContextRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(LImage, W, H, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToCGImageRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropToCGContextRef(AStream, W, H, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToCGImageRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCGImageRef(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToCGImageRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropToCGContextRef(AFileName, W, H, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropToBitmap(const ABitmap: TBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.Width, ABitmap.height), TpointF.create(XCropCenter, YCropCenter));

  Result := TBitmap.Create(LDestRect.Width,LDestRect.Height);
  try

    if Result.Canvas.BeginScene then
    try
      Result.Canvas.DrawBitmap(
        ABitmap, // const ABitmap: TBitmap;
        LSrcRect, //const SrcRect,
        LDestRect, //const DstRect: TRectF;
        1, //const AOpacity: Single;
        false); // const HighSpeed: Boolean => disable interpolation
    finally
      Result.Canvas.EndScene;
    end;

  except
    AlFreeAndNil(Result);
    raise;
  end;
end;

{************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{**************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToBitmap(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{***********************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{******************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToDrawable(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropToSkImage(AStream, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToSkSurface(AStream, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToSkSurface(AStream, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropToJBitmap(AStream, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropToCGContextRef(AStream, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropToBitmap(AStream, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{********************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToDrawable(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropToSkImage(AResName, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToSkSurface(AResName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToSkSurface(AResName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropToJBitmap(AResName, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropToCGContextRef(AResName, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropToBitmap(AResName, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{*****************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToDrawable(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropToSkImage(AFileName, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToSkSurface(AFileName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToSkSurface(AFileName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropToJBitmap(AFileName, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropToCGContextRef(AFileName, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropToBitmap(AFileName, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(const AImage: sk_image_t; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
    try
      sk4d_rrect_set_rect3(
        LRRect, // self: sk_rrect_t;
        @LDestRectF, // const rect: psk_rect_t;
        XRadius, // radius_x,
        YRadius); // radius_y: float)

      sk4d_canvas_clip_rrect(
        LCanvas, // self: sk_canvas_t;
        LRRect, // const rrect: sk_rrect_t;
        sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
        true); // anti_alias: _bool);

      var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
      sk4d_canvas_draw_image_rect(
        LCanvas, // self: sk_canvas_t;
        AImage, // const image: sk_image_t;
        @LSrcRect, // const src: psk_rect_t;
        @LDestRectF,  // const dest: psk_rect_t;
        @LSamplingoptions, // const sampling: psk_samplingoptions_t;
        LPaint, // const paint: sk_paint_t;
        FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

      sk4d_canvas_restore(LCanvas);
    finally
      sk4d_rrect_destroy(LRRect);
    end;
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectSkSurface(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectSkSurface(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectSkImage(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectSkImage(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkImage(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectSkImage(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropToRoundRectSkSurface(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToRoundRectJBitmap(const ABitmap: JBitmap; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).round;
  var LJDestRect := TJRect.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
  var LJSrcRect := TJRect.JavaClass.init(LSrcRect.left, LSrcRect.top, LSrcRect.right, LSrcRect.bottom);

  Result := TJBitmap.JavaClass.createBitmap(LDestRect.Width, LDestRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

  var LCanvas := TJCanvas.JavaClass.init(result);
  var LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
  LCanvas.drawRoundRect(
    LDestRect.left, // left: Single;
    LDestRect.top, // top: Single;
    LDestRect.right, // right: Single;
    LDestRect.bottom, // bottom: Single
    xRadius {rx},
    yRadius {ry},
    LPaint);

  var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
  LPaint.setXfermode(LPorterDuffXfermode);
  LCanvas.drawBitmap(ABitmap, LJSrcRect, LJDestRect, LPaint);
  LPaint.setXfermode(nil);
  LPorterDuffXfermode := nil;

  LPaint := nil;
  LCanvas := nil;
  LJSrcRect := nil;
  LJDestRect := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectJBitmap(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropToRoundRectJBitmap(LBitmap, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectJBitmap(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectJBitmap(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectJBitmap(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropToRoundRectJBitmap(LBitmap, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToRoundRectCGContextRef(const AImage: ALOSImage; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;

var
  LGridHeight: Integer;
  LCurPoint: TpointF;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _moveTo(const x: Single; const y: Single);
  begin
    CGContextMoveToPoint(Result, X, LGridHeight - Y);
    LCurPoint.X := x;
    LCurPoint.Y := Y;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _rQuadTo(const dx1: Single; const dy1: Single; const dx2: Single; const dy2: Single);
  begin
    CGContextAddQuadCurveToPoint(
      Result,
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
    CGContextAddLineToPoint(Result, LCurPoint.X + dx{x}, LGridHeight - (LCurPoint.Y + dy{y}));
    LCurPoint.X := LCurPoint.X + dx;
    LCurPoint.Y := LCurPoint.Y + dy;
  end;

begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LRatio: single;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage)), TpointF.create(XCropCenter, YCropCenter), LRatio);
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextBeginPath(Result);  // Creates a new empty path in a graphics context.

  LGridHeight := LDestRect.Height;
  var LXRadius: single := xRadius;
  var LYRadius: single := yRadius;
  if (LXRadius > LDestRect.width / 2) then LXRadius := LDestRect.width / 2;
  if (LYRadius > LDestRect.height / 2) then LYRadius := LDestRect.height / 2;
  var LWidthMinusCorners: single := (LDestRect.width - (2 * LXRadius));
  var LHeightMinusCorners: single := (LDestRect.height - (2 * LYRadius));

  //----- TopRight
  _moveTo(LDestRect.right, LDestRect.top + LYRadius);
  _rQuadTo(0, -LYRadius, -LXRadius, -LYRadius);
  _rLineTo(-LWidthMinusCorners, 0);

  //----- TopLeft
  _rQuadTo(-LXRadius, 0, -LXRadius, LYRadius);
  _rLineTo(0, LHeightMinusCorners);

  //----- BottomLeft
  _rQuadTo(0, LYRadius, LXRadius, LYRadius);
  _rLineTo(LWidthMinusCorners, 0);

  //----- BottomRight
  _rQuadTo(LXRadius, 0, LXRadius, -LYRadius);
  _rLineTo(0, -LHeightMinusCorners);

  CGContextClosePath(Result); // Closes and terminates the current path’s subpath.
  CGContextClip(Result); // Modifies the current clipping path, using the nonzero winding number rule.
                         // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                         // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                         // save the graphics state before you clip and restore the graphics state after you’ve completed
                         // any clipped drawing.
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(
        0-(LSrcRect.Left*LRatio),
        0-(LSrcRect.top*LRatio)),
      LDestRect.width + (LSrcRect.Left*LRatio) + ((ALOSImageGetWidth(AImage)-LSrcRect.right)*LRatio),
      LDestRect.height + (LSrcRect.top*LRatio)  + ((ALOSImageGetHeight(AImage)-LSrcRect.bottom)*LRatio),
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropToRoundRectCGContextRef(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectCGContextRef(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectCGContextRef(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropToRoundRectCGContextRef(LImage, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectCGImageRef(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectCGImageRef(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectCGImageRef(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToRoundRectCGImageRef(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropToRoundRectCGContextRef(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*****************************************************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropToRoundRectBitmap(const ABitmap: TBitmap; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropToBitmap(aBitmap, W, H, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TBrushKind.Bitmap;
        Result.Canvas.FillRect(
          TRectF.Create(0,0, Result.Width,Result.Height),
          XRadius,
          YRadius,
          AllCorners,
          1 {AOpacity});
      finally
        Result.Canvas.EndScene;
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{*****************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectBitmap(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToRoundRectBitmap(LBitmap, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*******************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectBitmap(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToRoundRectBitmap(LStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToRoundRectBitmap(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToRoundRectBitmap(LBitmap, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{***********************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToRoundRectDrawable(const AStream: TStream; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkImage(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToRoundRectSkSurface(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropToRoundRectJBitmap(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropToRoundRectCGContextRef(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropToRoundRectBitmap(AStream, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{*************************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToRoundRectDrawable(const AResName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropToRoundRectSkImage(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToRoundRectSkSurface(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToRoundRectSkSurface(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropToRoundRectJBitmap(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropToRoundRectCGContextRef(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropToRoundRectBitmap(AResName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{**********************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToRoundRectDrawable(const AFileName: String; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropToRoundRectSkImage(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToRoundRectSkSurface(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToRoundRectSkSurface(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropToRoundRectJBitmap(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropToRoundRectCGContextRef(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropToRoundRectBitmap(AFileName, W, H, XRadius, YRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(const AImage: sk_image_t; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
    try
      sk4d_rrect_set_oval(
        LRRect, // self: sk_rrect_t;
        @LDestRectF); // const rect: psk_rect_t;

      sk4d_canvas_clip_rrect(
        LCanvas, // self: sk_canvas_t;
        LRRect, // const rrect: sk_rrect_t;
        sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
        true); // anti_alias: _bool);

      var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
      sk4d_canvas_draw_image_rect(
        LCanvas, // self: sk_canvas_t;
        AImage, // const image: sk_image_t;
        @LSrcRect, // const src: psk_rect_t;
        @LDestRectF,  // const dest: psk_rect_t;
        @LSamplingoptions, // const sampling: psk_samplingoptions_t;
        LPaint, // const paint: sk_paint_t;
        FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

      sk4d_canvas_restore(LCanvas);
    finally
      sk4d_rrect_destroy(LRRect);
    end;
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(LImage, W, H, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToCircleSkSurface(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToCircleSkSurface(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(LImage, W, H, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropToCircleSkImage(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(LImage, W, H, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropToCircleSkImage(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleSkImage(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropToCircleSkImage(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropToCircleSkSurface(LImage, W, H, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropToCircleJBitmap(const ABitmap: JBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).round;
  var LJDestRect := TJRect.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
  var LJSrcRect := TJRect.JavaClass.init(LSrcRect.left, LSrcRect.top, LSrcRect.right, LSrcRect.bottom);

  Result := TJBitmap.JavaClass.createBitmap(LDestRect.Width, LDestRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

  var LCanvas := TJCanvas.JavaClass.init(result);
  var LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
  LCanvas.drawCircle(LDestRect.Width/2, LDestRect.Height/2, LDestRect.Width/2, LPaint);

  var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
  LPaint.setXfermode(LPorterDuffXfermode);
  LCanvas.drawBitmap(ABitmap, LJSrcRect, LJDestRect, LPaint);
  LPaint.setXfermode(nil);
  LPorterDuffXfermode := nil;

  LPaint := nil;
  LCanvas := nil;
  LJSrcRect := nil;
  LJDestRect := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropToCircleJBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropToCircleJBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropToCircleJBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleJBitmap(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropToCircleJBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropToCircleJBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropToCircleCGContextRef(const AImage: ALOSImage; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LRatio: single;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage)), TpointF.create(XCropCenter, YCropCenter), LRatio);
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextBeginPath(Result);  // Creates a new empty path in a graphics context.
  CGContextAddEllipseInRect(
    Result,
    ALLowerLeftCGRect(
      TPointF.Create(LDestRect.Left, LDestRect.Top),
      LDestRect.Width,
      LDestRect.Height,
      LDestRect.Height)); // Adds an ellipse that fits inside the specified rectangle.
  CGContextClosePath(Result); // Closes and terminates the current path’s subpath.
  CGContextClip(Result); // Modifies the current clipping path, using the nonzero winding number rule.
                         // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                         // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                         // save the graphics state before you clip and restore the graphics state after you’ve completed
                         // any clipped drawing.
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(
        0-(LSrcRect.Left*LRatio),
        0-(LSrcRect.top*LRatio)),
      LDestRect.width + (LSrcRect.Left*LRatio) + ((ALOSImageGetWidth(AImage)-LSrcRect.right)*LRatio),
      LDestRect.height + (LSrcRect.top*LRatio)  + ((ALOSImageGetHeight(AImage)-LSrcRect.bottom)*LRatio),
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropToCircleCGContextRef(LImage, W, H, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToCircleCGContextRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToCircleCGContextRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropToCircleCGContextRef(LImage, W, H, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropToCircleCGImageRef(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(AStream, W, H, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropToCircleCGImageRef(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleCGImageRef(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropToCircleCGImageRef(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropToCircleCGContextRef(AFileName, W, H, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{******************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropToCircleBitmap(const ABitmap: TBitmap; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropToBitmap(ABitmap, W, H, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TBrushKind.Bitmap;
        Result.Canvas.FillEllipse(TRectF.Create(0,0, Result.Width, Result.Height), 1 {AOpacity});
      finally
        Result.Canvas.EndScene;
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{******************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToCircleBitmap(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToCircleBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{********************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToCircleBitmap(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropToCircleBitmap(LStream, W, H, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToCircleBitmap(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropToCircleBitmap(LBitmap, W, H, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropToCircleDrawable(const AStream: TStream; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropToCircleSkImage(AStream, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(AStream, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropToCircleSkSurface(AStream, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropToCircleJBitmap(AStream, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropToCircleCGContextRef(AStream, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropToCircleBitmap(AStream, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{**************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropToCircleDrawable(const AResName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropToCircleSkImage(AResName, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToCircleSkSurface(AResName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropToCircleSkSurface(AResName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropToCircleJBitmap(AResName, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropToCircleCGContextRef(AResName, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropToCircleBitmap(AResName, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{***********************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropToCircleDrawable(const AFileName: String; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropToCircleSkImage(AFileName, W, H, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToCircleSkSurface(AFileName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropToCircleSkSurface(AFileName, W, H, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropToCircleJBitmap(AFileName, W, H, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropToCircleCGContextRef(AFileName, W, H, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropToCircleBitmap(AFileName, W, H, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    sk4d_paint_set_color(LPaint, TalphaColorRec.White);
    sk4d_canvas_draw_Rect(LCanvas, @LDestRectF, LPaint);

    var LImageFilter := ALSkCheckHandle(
                          sk4d_imagefilter_make_blur(
                            ALConvertRadiusToSigma(ABlurRadius), //sigma_x,
                            ALConvertRadiusToSigma(ABlurRadius), //sigma_y: float;
                            sk_tilemode_t.CLAMP_SK_TILEMODE, //tile_mode: sk_tilemode_t;
                            0, //input: sk_imagefilter_t;
                            @LDestRectF));//const crop_rect: psk_rect_t
    try
      sk4d_paint_set_Image_filter(LPaint, LImageFilter);
      var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
      sk4d_canvas_draw_image_rect(
        LCanvas, // self: sk_canvas_t;
        AImage, // const image: sk_image_t;
        @LSrcRect, // const src: psk_rect_t;
        @LDestRectF,  // const dest: psk_rect_t;
        @LSamplingoptions, // const sampling: psk_samplingoptions_t;
        LPaint, // const paint: sk_paint_t;
        FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
    finally
      sk4d_refcnt_unref(LImageFilter)
    end;
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToSkImage(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkImage(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).Round;

  var LMatrix := TJMatrix.JavaClass.init;
  LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
  var LBitmap := TJBitmap.JavaClass.createBitmap(ABitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
  LMatrix := nil;

  Try

    if TOSVersion.Check(12, 0) and
       TJHardwareBuffer.javaclass.isSupported(
         LBitmap.getWidth, // width: Integer;
         LBitmap.getHeight, // height: Integer;
         TJPixelFormat.JavaClass.RGBA_8888, // format: Integer;
         1, // layers: Integer;
         TJHardwareBuffer.javaclass.USAGE_GPU_SAMPLED_IMAGE or
         TJHardwareBuffer.javaclass.USAGE_GPU_COLOR_OUTPUT) then begin //usage: Int64
      Var LImageReader := TJImageReader.JavaClass.newInstance(
                            LBitmap.getWidth, // width: Integer;
                            LBitmap.getHeight,// height: Integer;
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
                                       ABlurRadius,
                                       ABlurRadius,
                                       TJShader_TileMode.JavaClass.MIRROR);
            LRenderNode.setRenderEffect(LBlurRenderEffect);
            var LrenderCanvas := TJALRecordingCanvas.wrap(LrenderNode.beginRecording);
            LRenderCanvas.drawBitmap(LBitmap, 0{left}, 0{top}, nil{paint});
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
                  //This is necessary to convert later the JBitmap in texture via texImage2D
                  Result := LHardwareBitmap.copy(TJBitmap_Config.JavaClass.ARGB_8888, false{isMutable});
                finally
                  if not LHardwareBitmap.equals(Result) then LHardwareBitmap.recycle;
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
      var LInput := TJAllocation.JavaClass.createFromBitmap(LRS, LBitmap);
      var LOutPut := TJAllocation.JavaClass.createTyped(LRS, LInput.getType());
      var LScript :=  TJScriptIntrinsicBlur.javaclass.create(LRS, TJElement.javaclass.U8_4(LRS));
      LScript.setRadius(Min(25, ABlurRadius)); // Set the radius of the Blur. Supported range 0 < radius <= 25
      LScript.setInput(LInput);
      LScript.forEach(LOutPut);
      LOutPut.copyTo(LBitmap);
      Result := LBitmap;
      LScript := nil;
      LInput := nil;
      LOutPut := nil;
      LRS := nil;
    end;

  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToJBitmap(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, W, H, XCropCenter, YCropCenter);
  try
    var LDestRect := Trect.Create(0, 0, CGBitmapContextGetWidth(Result), CGBitmapContextGetHeight(Result));
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     CGBitmapContextGetData(Result), // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     CGBitmapContextGetBytesPerRow(Result) * NSUInteger(LDestRect.Height), // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var Lformat: CIFormat;
      if GlobalUseMetal then Lformat := kCIFormatBGRA8
      else Lformat := kCIFormatRGBA8;
      var LCIImage := TCIImage.Wrap(
                        TCIImage.OCClass.imageWithBitmapData(
                          LData, // d: NSData;
                          CGBitmapContextGetBytesPerRow(Result), // bytesPerRow: size_t;
                          CGSizeMake(LDestRect.Width, LDestRect.Height), // size: CGSize;
                          Lformat, // format: CIFormat;
                          ALGetGlobalCGColorSpace)); // colorSpace: CGColorSpaceRef));

      // Gaussian blur CIFilter naturally creates artifacts at the borders of the
      // output image. It is happening because the gaussian blur filter samples
      // pixels outside the edges of the image. But because there are no pixels,
      // you get this weird artefact. You can use "CIAffineClamp" filter to
      // "extend" your image infinitely in all directions.
      var LClampFilter := {$IF defined(ALMacOS)}TALCIFilter{$ELSE}TCIFilter{$ENDIF}.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
      LClampFilter.setDefaults;
      LClampFilter.setValueforKey(NSObjectToID(LCIImage), kCIInputImageKey);

      var LBlurFilter := {$IF defined(ALMacOS)}TALCIFilter{$ELSE}TCIFilter{$ENDIF}.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
      LBlurFilter.setValueforKey(NSObjectToID(LClampFilter.outputImage), kCIInputImageKey);
      LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aBlurRadius), kCIInputRadiusKey);

      var LCIContext := TCIContext.Wrap({$IF defined(ALMacOS)}TALCIContext{$ELSE}TCIContext{$ENDIF}.OCClass.contextWithOptions(nil));
      var LCGImageRef := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);
      if LCGImageRef = nil then raise Exception.Create('Failed to create CGImageRef from CIContext');
      try

        CGContextDrawImage(
          Result, // c: The graphics context in which to draw the image.
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
          LCGImageRef); // image The image to draw.

      finally
        CGImageRelease(LCGImageRef);
      end;

      LCIImage := nil; // no need to call LCIImage.release; (i try => exception)
      LCIContext := nil; // no need to call LCIContext.release; (i try => exception)
      LBlurFilter := nil; // no need to call LBlurFilter.release (i try => exception)
      LClampFilter := nil; // no need to call LClampFilter.release (i try => exception)
    finally
      LData.release;
    end;
  Except
    On Exception Do begin
      CGContextRelease(Result);
      Raise;
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCGImageRef(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropAndBlurToCGContextRef(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************************************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.Width, ABitmap.height), TpointF.create(XCropCenter, YCropCenter));

  Result := TBitmap.Create(LDestRect.Width,LDestRect.Height);
  try

    if Result.Canvas.BeginScene then
    try
      Result.Canvas.DrawBitmap(
        ABitmap, // const ABitmap: TBitmap;
        LSrcRect, //const SrcRect,
        LDestRect, //const DstRect: TRectF;
        1, //const AOpacity: Single;
        false); // const HighSpeed: Boolean => disable interpolation

      var LBlurEffect := TBlurEffect.Create(nil);
      try
        // Specifies the amount of blur applied to the shadow.
        // Softness is a System.Single value that takes values in the range from 0 through 9.
        // I calculate approximatly that 0.5 = around 12 for blur
        LBlurEffect.softness := ABlurRadius / 24;
        Result.Canvas.Flush;
        LBlurEffect.ProcessEffect(Result.Canvas, Result.Canvas.Bitmap, 1);
      finally
        ALFreeAndNil(LBlurEffect);
      end;

    finally
      Result.Canvas.EndScene;
    end;

  except
    AlFreeAndNil(Result);
    raise;
  end;
end;

{**********************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToBitmap(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndBlurToBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{****************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkImage(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndBlurToSkSurface(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropAndBlurToJBitmap(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropAndBlurToCGContextRef(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropAndBlurToBitmap(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{******************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToDrawable(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropAndBlurToSkImage(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndBlurToSkSurface(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndBlurToSkSurface(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropAndBlurToJBitmap(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropAndBlurToCGContextRef(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropAndBlurToBitmap(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{***************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndBlurToDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropAndBlurToSkImage(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndBlurToSkSurface(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndBlurToSkSurface(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropAndBlurToJBitmap(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropAndBlurToCGContextRef(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropAndBlurToBitmap(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
    try
      sk4d_rrect_set_oval(
        LRRect, // self: sk_rrect_t;
        @LDestRectF); // const rect: psk_rect_t;

      sk4d_canvas_clip_rrect(
        LCanvas, // self: sk_canvas_t;
        LRRect, // const rrect: sk_rrect_t;
        sk_clipop_t.INTERSECT_SK_CLIPOP, // op: sk_clipop_t;
        true); // anti_alias: _bool);

      sk4d_paint_set_color(LPaint, TalphaColorRec.White);
      sk4d_canvas_draw_Rect(LCanvas, @LDestRectF, LPaint);

      var LImageFilter := ALSkCheckHandle(
                            sk4d_imagefilter_make_blur(
                              ALConvertRadiusToSigma(ABlurRadius), //sigma_x,
                              ALConvertRadiusToSigma(ABlurRadius), //sigma_y: float;
                              sk_tilemode_t.CLAMP_SK_TILEMODE, //tile_mode: sk_tilemode_t;
                              0, //input: sk_imagefilter_t;
                              @LDestRectF));//const crop_rect: psk_rect_t
      try
        sk4d_paint_set_Image_filter(LPaint, LImageFilter);
        var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
        sk4d_canvas_draw_image_rect(
          LCanvas, // self: sk_canvas_t;
          AImage, // const image: sk_image_t;
          @LSrcRect, // const src: psk_rect_t;
          @LDestRectF,  // const dest: psk_rect_t;
          @LSamplingoptions, // const sampling: psk_samplingoptions_t;
          LPaint, // const paint: sk_paint_t;
          FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
      finally
        sk4d_refcnt_unref(LImageFilter)
      end;

      sk4d_canvas_restore(LCanvas);
    finally
      sk4d_rrect_destroy(LRRect);
    end;
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkImage(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkImage(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndBlurToCircleSkSurface(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndBlurToCircleJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LBitmap := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(ABitmap, W, H, aBlurRadius, XCropCenter, YCropCenter);
  try

    var LRect := Trect.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
    var LJRect := TJRect.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);

    Result := TJBitmap.JavaClass.createBitmap(LRect.Width, LRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

    var LCanvas := TJCanvas.JavaClass.init(result);
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
    LCanvas.drawCircle(LRect.Width/2, LRect.Height/2, LRect.Width/2, LPaint);

    var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
    LPaint.setXfermode(LPorterDuffXfermode);
    LCanvas.drawBitmap(LBitmap, LJRect, LJRect, LPaint);
    LPaint.setXfermode(nil);
    LPorterDuffXfermode := nil;

    LPaint := nil;
    LCanvas := nil;
    LJRect := nil;

  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToCircleJBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleJBitmap(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToCircleJBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndBlurToCircleCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, W, H, XCropCenter, YCropCenter);
  try
    var LDestRect := Trect.Create(0, 0, CGBitmapContextGetWidth(Result), CGBitmapContextGetHeight(Result));
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     CGBitmapContextGetData(Result), // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     CGBitmapContextGetBytesPerRow(Result) * NSUInteger(LDestRect.Height), // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var Lformat: CIFormat;
      if GlobalUseMetal then Lformat := kCIFormatBGRA8
      else Lformat := kCIFormatRGBA8;
      var LCIImage := TCIImage.Wrap(
                        TCIImage.OCClass.imageWithBitmapData(
                          LData, // d: NSData;
                          CGBitmapContextGetBytesPerRow(Result), // bytesPerRow: size_t;
                          CGSizeMake(LDestRect.Width, LDestRect.Height), // size: CGSize;
                          Lformat, // format: CIFormat;
                          ALGetGlobalCGColorSpace)); // colorSpace: CGColorSpaceRef));

      // Gaussian blur CIFilter naturally creates artifacts at the borders of the
      // output image. It is happening because the gaussian blur filter samples
      // pixels outside the edges of the image. But because there are no pixels,
      // you get this weird artefact. You can use "CIAffineClamp" filter to
      // "extend" your image infinitely in all directions.
      var LClampFilter := {$IF defined(ALMacOS)}TALCIFilter{$ELSE}TCIFilter{$ENDIF}.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
      LClampFilter.setDefaults;
      LClampFilter.setValueforKey(NSObjectToID(LCIImage), kCIInputImageKey);

      var LBlurFilter := {$IF defined(ALMacOS)}TALCIFilter{$ELSE}TCIFilter{$ENDIF}.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
      LBlurFilter.setValueforKey(NSObjectToID(LClampFilter.outputImage), kCIInputImageKey);
      LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aBlurRadius), kCIInputRadiusKey);

      var LCIContext := TCIContext.Wrap({$IF defined(ALMacOS)}TALCIContext{$ELSE}TCIContext{$ENDIF}.OCClass.contextWithOptions(nil));
      var LCGImageRef := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);
      if LCGImageRef = nil then raise Exception.Create('Failed to create CGImageRef from CIContext');
      try

        CGContextClearRect(
          Result,
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height)); // Paints a transparent rectangle.

        CGContextBeginPath(Result);  // Creates a new empty path in a graphics context.
        CGContextAddEllipseInRect(
          Result,
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height)); // Adds an ellipse that fits inside the specified rectangle.
        CGContextClosePath(Result); // Closes and terminates the current path’s subpath.
        CGContextClip(Result); // Modifies the current clipping path, using the nonzero winding number rule.
                               // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                               // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                               // save the graphics state before you clip and restore the graphics state after you’ve completed
                               // any clipped drawing.
        CGContextDrawImage(
          Result, // c: The graphics context in which to draw the image.
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
          LCGImageRef); // image The image to draw.

      finally
        CGImageRelease(LCGImageRef);
      end;

      LCIImage := nil; // no need to call LCIImage.release; (i try => exception)
      LCIContext := nil; // no need to call LCIContext.release; (i try => exception)
      LBlurFilter := nil; // no need to call LBlurFilter.release (i try => exception)
      LClampFilter := nil; // no need to call LClampFilter.release (i try => exception)
    finally
      LData.release;
    end;
  Except
    On Exception Do begin
      CGContextRelease(Result);
      Raise;
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCircleCGContextRef(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCircleCGContextRef(LImage, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGImageRef(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGContextRef(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{****************************************************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropAndBlurToCircleBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(abitmap, W, H, aBlurRadius, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TBrushKind.Bitmap;
        Result.Canvas.FillEllipse(TRectF.Create(0,0, Result.Width, Result.Height), 1 {AOpacity});
      finally
        Result.Canvas.EndScene;
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{****************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToCircleBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{******************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleBitmap(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleBitmap(LStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{***************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToCircleBitmap(LBitmap, W, H, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{**********************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkImage(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleSkSurface(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleJBitmap(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleCGContextRef(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropAndBlurToCircleBitmap(AStream, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{************************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleDrawable(const AResName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkImage(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkSurface(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleSkSurface(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleJBitmap(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleCGContextRef(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleBitmap(AResName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{*********************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndBlurToCircleDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkImage(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkSurface(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleSkSurface(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleJBitmap(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleCGContextRef(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropAndBlurToCircleBitmap(AFileName, W, H, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(const AImage: sk_image_t; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, sk4d_image_get_width(AMask), sk4d_image_get_Height(AMask)).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AMask, // const image: sk_image_t;
      @LDestRectF, // const src: psk_rect_t;
      @LDestRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

    var LBlender := ALSkCheckHandle(
                      sk4d_blender_make_mode(
                        sk_blendmode_t.SRC_IN_SK_BLENDMODE));
    try
      sk4d_paint_set_blender(LPaint, LBlender);
      sk4d_canvas_draw_image_rect(
        LCanvas, // self: sk_canvas_t;
        AImage, // const image: sk_image_t;
        @LSrcRect, // const src: psk_rect_t;
        @LDestRectF,  // const dest: psk_rect_t;
        @LSamplingoptions, // const sampling: psk_samplingoptions_t;
        LPaint, // const paint: sk_paint_t;
        FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
    finally
      sk4d_refcnt_unref(LBlender)
    end;

  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(const AStream: TStream; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(LImage, AMask, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToSkSurface(const AResName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToSkSurface(const AFileName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(LImage, AMask, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToSkImage(const AStream: TStream; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(LImage, AMask, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToSkImage(const AResName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkImage(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToSkImage(const AFileName: String; const AMask: sk_image_t; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndMaskToSkSurface(LImage, AMask, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndMaskToJBitmap(const ABitmap: JBitmap; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LDestRect := TRect.Create(0, 0, aMask.getWidth, aMask.getHeight);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight), TpointF.create(XCropCenter, YCropCenter)).round;
  var LJDestRect := TJRect.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
  var LJSrcRect := TJRect.JavaClass.init(LSrcRect.left, LSrcRect.top, LSrcRect.right, LSrcRect.bottom);

  Result := TJBitmap.JavaClass.createBitmap(LDestRect.Width, LDestRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

  var LCanvas := TJCanvas.JavaClass.init(result);
  var LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
  LCanvas.drawBitmap(aMask, 0{left}, 0{top}, LPaint);

  var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
  LPaint.setXfermode(LPorterDuffXfermode);
  LCanvas.drawBitmap(ABitmap, LJSrcRect, LJDestRect, LPaint);
  LPaint.setXfermode(nil);
  LPorterDuffXfermode := nil;

  LPaint := nil;
  LCanvas := nil;
  LJSrcRect := nil;
  LJDestRect := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToJBitmap(const AStream: TStream; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropAndMaskToJBitmap(LBitmap, AMask, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToJBitmap(const AResName: String; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToJBitmap(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToJBitmap(const AFileName: String; const AMask: JBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropAndMaskToJBitmap(LBitmap, AMask, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndMaskToCGContextRef(const AImage: ALOSImage; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LDestRect := Trect.Create(0, 0, CGImageGetWidth(aMask), CGImageGetHeight(aMask));
  var LRatio: single;
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage)), TpointF.create(XCropCenter, YCropCenter), LRatio);
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextClipToMask(
    Result,
    ALLowerLeftCGRect(
      TpointF.Create(0, 0),
      LDestRect.width,
      LDestRect.height,
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(
        0-(LSrcRect.Left*LRatio),
        0-(LSrcRect.top*LRatio)),
      LDestRect.width + (LSrcRect.Left*LRatio) + ((ALOSImageGetWidth(AImage)-LSrcRect.right)*LRatio),
      LDestRect.height + (LSrcRect.top*LRatio)  + ((ALOSImageGetHeight(AImage)-LSrcRect.bottom)*LRatio),
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(const AStream: TStream; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropAndMaskToCGContextRef(LImage, AMask, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToCGContextRef(const AResName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToCGContextRef(const AFileName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropAndMaskToCGContextRef(LImage, AMask, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToCGImageRef(const AStream: TStream; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(AStream, AMask, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToCGImageRef(const AResName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToCGImageRef(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndMaskToCGImageRef(const AFileName: String; const AMask: CGImageRef; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropAndMaskToCGContextRef(AFileName, AMask, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*********************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropAndMaskToBitmap(const ABitmap: TBitmap; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropToBitmap(aBitmap, aMask.Width, aMask.height, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      var D, B, M: TBitmapData;
      if Result.Map(TMapAccess.Write, D) then
      try
        if LBitmap.Map(TMapAccess.Read, B) then
        try
          if aMask.Map(TMapAccess.Read, M) then
          try
            for var J := 0 to Result.Height - 1 do
              for var I := 0 to Result.Width - 1 do
              begin
                var C := B.GetPixel(I, J);
                TAlphaColorRec(C).A := TAlphaColorRec(M.GetPixel(I, J)).A;
                if TAlphaColorRec(C).A < 255 then begin  // << don't ask me why we need to do this :(
                  var ratio: single := TAlphaColorRec(C).A / 255;
                  TAlphaColorRec(C).R := round(TAlphaColorRec(C).R * ratio);
                  TAlphaColorRec(C).G := round(TAlphaColorRec(C).G * ratio);
                  TAlphaColorRec(C).B := round(TAlphaColorRec(C).B * ratio);
                end;
                D.SetPixel(I, J, C);
              end;
          finally
            aMask.Unmap(M);
          end;
        finally
          LBitmap.Unmap(B);
        end;
      finally
        Result.Unmap(D);
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{*********************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToBitmap(const AStream: TStream; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndMaskToBitmap(LBitmap, AMask, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{***********************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToBitmap(const AResName: String; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskToBitmap(LStream, AMask, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{********************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndMaskToBitmap(const AFileName: String; const AMask: TBitmap; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndMaskToBitmap(LBitmap, AMask, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{******************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndMaskToDrawable(const AStream: TStream; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkImage(AStream, AMask, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(AStream, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndMaskToSkSurface(AStream, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropAndMaskToJBitmap(AStream, AMask, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropAndMaskToCGContextRef(AStream, AMask, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropAndMaskToBitmap(AStream, AMask, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{********************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndMaskToDrawable(const AResName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropAndMaskToSkImage(AResName, AMask, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndMaskToSkSurface(AResName, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndMaskToSkSurface(AResName, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropAndMaskToJBitmap(AResName, AMask, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropAndMaskToCGContextRef(AResName, AMask, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropAndMaskToBitmap(AResName, AMask, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{*****************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndMaskToDrawable(const AFileName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropAndMaskToSkImage(AFileName, AMask, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndMaskToSkSurface(AFileName, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndMaskToSkSurface(AFileName, AMask, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropAndMaskToJBitmap(AFileName, AMask, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropAndMaskToCGContextRef(AFileName, AMask, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropAndMaskToBitmap(AFileName, AMask, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AImage: sk_image_t; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LDestRect := TrectF.Create(0, 0, sk4d_image_get_width(AMask), sk4d_image_get_Height(AMask)).Round;
  var LDestRectF := TRectF.Create(LDestRect);
  var LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage)), TpointF.create(XCropCenter, YCropCenter));

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AMask, // const image: sk_image_t;
      @LDestRectF, // const src: psk_rect_t;
      @LDestRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)

    var LBlender := ALSkCheckHandle(
                      sk4d_blender_make_mode(
                        sk_blendmode_t.SRC_IN_SK_BLENDMODE));
    try
      sk4d_paint_set_blender(LPaint, LBlender);

      var LImageFilter := ALSkCheckHandle(
                            sk4d_imagefilter_make_blur(
                              ALConvertRadiusToSigma(ABlurRadius), //sigma_x,
                              ALConvertRadiusToSigma(ABlurRadius), //sigma_y: float;
                              sk_tilemode_t.CLAMP_SK_TILEMODE, //tile_mode: sk_tilemode_t;
                              0, //input: sk_imagefilter_t;
                              @LDestRectF));//const crop_rect: psk_rect_t
      try
        sk4d_paint_set_Image_filter(LPaint, LImageFilter);
        sk4d_canvas_draw_image_rect(
          LCanvas, // self: sk_canvas_t;
          AImage, // const image: sk_image_t;
          @LSrcRect, // const src: psk_rect_t;
          @LDestRectF,  // const dest: psk_rect_t;
          @LSamplingoptions, // const sampling: psk_samplingoptions_t;
          LPaint, // const paint: sk_paint_t;
          FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
      finally
        sk4d_refcnt_unref(LImageFilter)
      end;

    finally
      sk4d_refcnt_unref(LBlender)
    end;

  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AStream: TStream; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AResName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkSurface(const AFileName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkImage(const AStream: TStream; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkImage(const AResName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkImage(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkImage(const AFileName: String; const AMask: sk_image_t; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndFitIntoAndCropAndMaskAndBlurToSkSurface(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndFitIntoAndCropAndMaskAndBlurToJBitmap(const ABitmap: JBitmap; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LBitmap := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(ABitmap, aMask.getWidth, aMask.getHeight, aBlurRadius, XCropCenter, YCropCenter);
  try
    var LRect := TRect.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
    var LJRect := TJRect.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);

    Result := TJBitmap.JavaClass.createBitmap(LRect.Width, LRect.Height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);

    var LCanvas := TJCanvas.JavaClass.init(result);
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
    LCanvas.drawBitmap(aMask, 0{left}, 0{top}, LPaint);

    var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
    LPaint.setXfermode(LPorterDuffXfermode);
    LCanvas.drawBitmap(LBitmap, LJRect, LJRect, LPaint);
    LPaint.setXfermode(nil);
    LPorterDuffXfermode := nil;

    LPaint := nil;
    LCanvas := nil;
    LJRect := nil;
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AStream: TStream; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndFitIntoAndCropAndMaskAndBlurToJBitmap(LBitmap, AMask, ABlurRadius, XCropCenter, YCropCenter);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AResName: String; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToJBitmap(const AFileName: String; const AMask: JBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndFitIntoAndCropAndMaskAndBlurToJBitmap(LBitmap, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AImage: ALOSImage; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, CGImageGetWidth(aMask), CGImageGetHeight(aMask), XCropCenter, YCropCenter);
  try
    var LDestRect := Trect.Create(0, 0, CGBitmapContextGetWidth(Result), CGBitmapContextGetHeight(Result));
    var LData := TNSData.Wrap(
                   TNSData.alloc.initWithBytesNoCopy(
                     CGBitmapContextGetData(Result), // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                     CGBitmapContextGetBytesPerRow(Result) * NSUInteger(LDestRect.Height), // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                     False)); // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
    try
      var Lformat: CIFormat;
      if GlobalUseMetal then Lformat := kCIFormatBGRA8
      else Lformat := kCIFormatRGBA8;
      var LCIImage := TCIImage.Wrap(
                        TCIImage.OCClass.imageWithBitmapData(
                          LData, // d: NSData;
                          CGBitmapContextGetBytesPerRow(Result), // bytesPerRow: size_t;
                          CGSizeMake(LDestRect.Width, LDestRect.Height), // size: CGSize;
                          Lformat, // format: CIFormat;
                          ALGetGlobalCGColorSpace)); // colorSpace: CGColorSpaceRef));

      // Gaussian blur CIFilter naturally creates artifacts at the borders of the
      // output image. It is happening because the gaussian blur filter samples
      // pixels outside the edges of the image. But because there are no pixels,
      // you get this weird artefact. You can use "CIAffineClamp" filter to
      // "extend" your image infinitely in all directions.
      var LClampFilter := {$IF defined(ALMacOS)}TALCIFilter{$ELSE}TCIFilter{$ENDIF}.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
      LClampFilter.setDefaults;
      LClampFilter.setValueforKey(NSObjectToID(LCIImage), kCIInputImageKey);

      var LBlurFilter := {$IF defined(ALMacOS)}TALCIFilter{$ELSE}TCIFilter{$ENDIF}.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
      LBlurFilter.setValueforKey(NSObjectToID(LClampFilter.outputImage), kCIInputImageKey);
      LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aBlurRadius), kCIInputRadiusKey);

      var LCIContext := TCIContext.Wrap({$IF defined(ALMacOS)}TALCIContext{$ELSE}TCIContext{$ENDIF}.OCClass.contextWithOptions(nil));
      var LCGImageRef := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);
      if LCGImageRef = nil then raise Exception.Create('Failed to create CGImageRef from CIContext');
      try

        CGContextClearRect(
          Result,
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height)); // Paints a transparent rectangle.

        CGContextClipToMask(
          Result,
          ALLowerLeftCGRect(
            TpointF.Create(0, 0),
            LDestRect.width,
            LDestRect.height,
            LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
          aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.

        CGContextDrawImage(
          Result, // c: The graphics context in which to draw the image.
          ALLowerLeftCGRect(
            TpointF.Create(0,0),
              LDestRect.Width,
              LDestRect.Height,
              LDestRect.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
          LCGImageRef); // image The image to draw.

      finally
        CGImageRelease(LCGImageRef);
      end;

      LCIImage := nil; // no need to call LCIImage.release; (i try => exception)
      LCIContext := nil; // no need to call LCIContext.release; (i try => exception)
      LBlurFilter := nil; // no need to call LBlurFilter.release (i try => exception)
      LClampFilter := nil; // no need to call LClampFilter.release (i try => exception)
    finally
      LData.release;
    end;
  Except
    On Exception Do begin
      CGContextRelease(Result);
      Raise;
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AStream: TStream; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndFitIntoAndCropAndMaskAndBlurToCGContextRef(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AResName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGContextRef(const AFileName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndFitIntoAndCropAndMaskAndBlurToCGContextRef(LImage, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AStream: TStream; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AResName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGImageRef(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGImageRef(const AFileName: String; const AMask: CGImageRef; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*******************************************************************************************************************************************************************************************************************}
function ALLoadFromBitmapAndFitIntoAndCropAndMaskAndBlurToBitmap(const ABitmap: TBitmap; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(aBitmap, aMask.Width, aMask.height, ABlurRadius, XCropCenter, YCropCenter);
  try

    Result := TBitmap.Create(LBitmap.Width,LBitmap.Height);
    try

      var D, B, M: TBitmapData;
      if Result.Map(TMapAccess.Write, D) then
      try
        if LBitmap.Map(TMapAccess.Read, B) then
        try
          if aMask.Map(TMapAccess.Read, M) then
          try
            for var J := 0 to Result.Height - 1 do
              for var I := 0 to Result.Width - 1 do
              begin
                var C := B.GetPixel(I, J);
                TAlphaColorRec(C).A := TAlphaColorRec(M.GetPixel(I, J)).A;
                if TAlphaColorRec(C).A < 255 then begin  // << don't ask me why we need to do this :(
                  var ratio: single := TAlphaColorRec(C).A / 255;
                  TAlphaColorRec(C).R := round(TAlphaColorRec(C).R * ratio);
                  TAlphaColorRec(C).G := round(TAlphaColorRec(C).G * ratio);
                  TAlphaColorRec(C).B := round(TAlphaColorRec(C).B * ratio);
                end;
                D.SetPixel(I, J, C);
              end;
          finally
            aMask.Unmap(M);
          end;
        finally
          LBitmap.Unmap(B);
        end;
      finally
        Result.Unmap(D);
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(LBitmap);
  end;
end;

{*******************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToBitmap(const AStream: TStream; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndMaskAndBlurToBitmap(LBitmap, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*********************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToBitmap(const AResName: String; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToBitmap(LStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{******************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToBitmap(const AFileName: String; const AMask: TBitmap; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndFitIntoAndCropAndMaskAndBlurToBitmap(LBitmap, AMask, ABlurRadius, XCropCenter, YCropCenter);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{****************************************************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToDrawable(const AStream: TStream; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkImage(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToSkSurface(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToJBitmap(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndFitIntoAndCropAndMaskAndBlurToBitmap(AStream, AMask, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{******************************************************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToDrawable(const AResName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkImage(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkSurface(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToSkSurface(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToJBitmap(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToBitmap(AResName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{***************************************************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToDrawable(const AFileName: String; const AMask: {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkImage(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkSurface(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToSkSurface(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToJBitmap(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToCGContextRef(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndFitIntoAndCropAndMaskAndBlurToBitmap(AFileName, AMask, ABlurRadius, XCropCenter, YCropCenter);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndPlaceIntoToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
begin
  var LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(AImage, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndPlaceIntoToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndPlaceIntoToSkSurface(LImage, W, H);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndPlaceIntoToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToSkSurface(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndPlaceIntoToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndPlaceIntoToSkSurface(LImage, W, H);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndPlaceIntoToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndPlaceIntoToSkSurface(LImage, W, H);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndPlaceIntoToSkImage(const AResName: String; const W, H: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToSkImage(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndPlaceIntoToSkImage(const AFileName: String; const W, H: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndPlaceIntoToSkSurface(LImage, W, H);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndPlaceIntoToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.GetWidth, ABitmap.Getheight);
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(ABitmap, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndPlaceIntoToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndPlaceIntoToJBitmap(LBitmap, W, H);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndPlaceIntoToJBitmap(const AResName: String; const W, H: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToJBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndPlaceIntoToJBitmap(const AFileName: String; const W, H: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndPlaceIntoToJBitmap(LBitmap, W, H);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndPlaceIntoToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
begin
  var LSrcRect := TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage));
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, LDestRect.Width, LDestRect.Height);
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndPlaceIntoToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndPlaceIntoToCGContextRef(LImage, W, H);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndPlaceIntoToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToCGContextRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndPlaceIntoToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndPlaceIntoToCGContextRef(LImage, W, H);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndPlaceIntoToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndPlaceIntoToCGContextRef(AStream, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndPlaceIntoToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToCGImageRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndPlaceIntoToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndPlaceIntoToCGContextRef(AFileName, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*************************************************************************************************}
function ALLoadFromBitmapAndPlaceIntoToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.Width, ABitmap.height);
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(ABitmap, LDestRect.Width, LDestRect.Height);
end;

{*************************************************************************************************}
function ALLoadFromStreamAndPlaceIntoToBitmap(const AStream: TStream; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndPlaceIntoToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{***************************************************************************************************}
function ALLoadFromResourceAndPlaceIntoToBitmap(const AResName: String; const W, H: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoToBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{************************************************************************************************}
function ALLoadFromFileAndPlaceIntoToBitmap(const AFileName: String; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndPlaceIntoToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*******************************************************************************************************}
function ALLoadFromStreamAndPlaceIntoToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndPlaceIntoToSkImage(AStream, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndPlaceIntoToSkSurface(AStream, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndPlaceIntoToSkSurface(AStream, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndPlaceIntoToJBitmap(AStream, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndPlaceIntoToCGContextRef(AStream, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndPlaceIntoToBitmap(AStream, W, H);
  {$ENDIF}
end;

{*********************************************************************************************************}
function ALLoadFromResourceAndPlaceIntoToDrawable(const AResName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndPlaceIntoToSkImage(AResName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndPlaceIntoToSkSurface(AResName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndPlaceIntoToSkSurface(AResName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndPlaceIntoToJBitmap(AResName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndPlaceIntoToCGContextRef(AResName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndPlaceIntoToBitmap(AResName, W, H);
  {$ENDIF}
end;

{******************************************************************************************************}
function ALLoadFromFileAndPlaceIntoToDrawable(const AFileName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndPlaceIntoToSkImage(AFileName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndPlaceIntoToSkSurface(AFileName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndPlaceIntoToSkSurface(AFileName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndPlaceIntoToJBitmap(AFileName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndPlaceIntoToCGContextRef(AFileName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndPlaceIntoToBitmap(AFileName, W, H);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(const AImage: sk_image_t; const W, H: single; const ABlurRadius: single): sk_surface_t;
begin
  var LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromSkImageAndFitIntoAndCropAndBlurToSkSurface(AImage, LDestRect.Width, LDestRect.Height, ABlurRadius);
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(const AStream: TStream; const W, H: single; const ABlurRadius: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(LImage, W, H, ABlurRadius);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndPlaceIntoAndBlurToSkSurface(const AResName: String; const W, H: single; const ABlurRadius: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndPlaceIntoAndBlurToSkSurface(const AFileName: String; const W, H: single; const ABlurRadius: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(LImage, W, H, ABlurRadius);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndPlaceIntoAndBlurToSkImage(const AStream: TStream; const W, H: single; const ABlurRadius: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(LImage, W, H, ABlurRadius);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndPlaceIntoAndBlurToSkImage(const AResName: String; const W, H: single; const ABlurRadius: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToSkImage(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndPlaceIntoAndBlurToSkImage(const AFileName: String; const W, H: single; const ABlurRadius: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndPlaceIntoAndBlurToSkSurface(LImage, W, H, ABlurRadius);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndPlaceIntoAndBlurToJBitmap(const ABitmap: JBitmap; const W, H: single; const ABlurRadius: single): JBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.GetWidth, ABitmap.Getheight);
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromJBitmapAndFitIntoAndCropAndBlurToJBitmap(ABitmap, LDestRect.Width, LDestRect.Height, ABlurRadius);
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndPlaceIntoAndBlurToJBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndPlaceIntoAndBlurToJBitmap(LBitmap, W, H, ABlurRadius);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndPlaceIntoAndBlurToJBitmap(const AResName: String; const W, H: single; const ABlurRadius: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToJBitmap(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndPlaceIntoAndBlurToJBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndPlaceIntoAndBlurToJBitmap(LBitmap, W, H, ABlurRadius);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndPlaceIntoAndBlurToCGContextRef(const AImage: ALOSImage; const W, H: single; const ABlurRadius: single): CGContextRef;
begin
  var LSrcRect := TrectF.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage));
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromOSImageAndFitIntoAndCropAndBlurToCGContextRef(AImage, LDestRect.Width, LDestRect.Height, ABlurRadius);
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(const AStream: TStream; const W, H: single; const ABlurRadius: single): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndPlaceIntoAndBlurToCGContextRef(LImage, W, H, ABlurRadius);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndPlaceIntoAndBlurToCGContextRef(const AResName: String; const W, H: single; const ABlurRadius: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndPlaceIntoAndBlurToCGContextRef(const AFileName: String; const W, H: single; const ABlurRadius: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndPlaceIntoAndBlurToCGContextRef(LImage, W, H, ABlurRadius);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndPlaceIntoAndBlurToCGImageRef(const AStream: TStream; const W, H: single; const ABlurRadius: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(AStream, W, H, ABlurRadius);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndPlaceIntoAndBlurToCGImageRef(const AResName: String; const W, H: single; const ABlurRadius: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToCGImageRef(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndPlaceIntoAndBlurToCGImageRef(const AFileName: String; const W, H: single; const ABlurRadius: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndPlaceIntoAndBlurToCGContextRef(AFileName, W, H, ABlurRadius);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{***********************************************************************************************************************************}
function ALLoadFromBitmapAndPlaceIntoAndBlurToBitmap(const ABitmap: TBitmap; const W, H: single; const ABlurRadius: single): TBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.Width, ABitmap.height);
  var LDestRect := ALRectPlaceInto(LSrcRect, TrectF.Create(0, 0, W, H));
  Result := ALLoadFromBitmapAndFitIntoAndCropAndBlurToBitmap(ABitmap, LDestRect.Width, LDestRect.Height, ABlurRadius);
end;

{***********************************************************************************************************************************}
function ALLoadFromStreamAndPlaceIntoAndBlurToBitmap(const AStream: TStream; const W, H: single; const ABlurRadius: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndPlaceIntoAndBlurToBitmap(LBitmap, W, H, ABlurRadius);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*************************************************************************************************************************************}
function ALLoadFromResourceAndPlaceIntoAndBlurToBitmap(const AResName: String; const W, H: single; const ABlurRadius: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndPlaceIntoAndBlurToBitmap(LStream, W, H, ABlurRadius);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************************************************}
function ALLoadFromFileAndPlaceIntoAndBlurToBitmap(const AFileName: String; const W, H: single; const ABlurRadius: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndPlaceIntoAndBlurToBitmap(LBitmap, W, H, ABlurRadius);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*****************************************************************************************************************************************}
function ALLoadFromStreamAndPlaceIntoAndBlurToDrawable(const AStream: TStream; const W, H: single; const ABlurRadius: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndPlaceIntoAndBlurToSkImage(AStream, W, H, ABlurRadius);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(AStream, W, H, ABlurRadius);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndPlaceIntoAndBlurToSkSurface(AStream, W, H, ABlurRadius);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndPlaceIntoAndBlurToJBitmap(AStream, W, H, ABlurRadius);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndPlaceIntoAndBlurToCGContextRef(AStream, W, H, ABlurRadius);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndPlaceIntoAndBlurToBitmap(AStream, W, H, ABlurRadius);
  {$ENDIF}
end;

{*******************************************************************************************************************************************}
function ALLoadFromResourceAndPlaceIntoAndBlurToDrawable(const AResName: String; const W, H: single; const ABlurRadius: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndPlaceIntoAndBlurToSkImage(AResName, W, H, ABlurRadius);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndPlaceIntoAndBlurToSkSurface(AResName, W, H, ABlurRadius);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndPlaceIntoAndBlurToSkSurface(AResName, W, H, ABlurRadius);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndPlaceIntoAndBlurToJBitmap(AResName, W, H, ABlurRadius);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndPlaceIntoAndBlurToCGContextRef(AResName, W, H, ABlurRadius);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndPlaceIntoAndBlurToBitmap(AResName, W, H, ABlurRadius);
  {$ENDIF}
end;

{****************************************************************************************************************************************}
function ALLoadFromFileAndPlaceIntoAndBlurToDrawable(const AFileName: String; const W, H: single; const ABlurRadius: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndPlaceIntoAndBlurToSkImage(AFileName, W, H, ABlurRadius);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndPlaceIntoAndBlurToSkSurface(AFileName, W, H, ABlurRadius);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndPlaceIntoAndBlurToSkSurface(AFileName, W, H, ABlurRadius);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndPlaceIntoAndBlurToJBitmap(AFileName, W, H, ABlurRadius);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndPlaceIntoAndBlurToCGContextRef(AFileName, W, H, ABlurRadius);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndPlaceIntoAndBlurToBitmap(AFileName, W, H, ABlurRadius);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndStretchToSkSurface(const AImage: sk_image_t; const W, H: single): sk_surface_t;
begin
  var LSrcRect := TrectF.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  var LDestRectF := TRectF.Create(LDestRect);

  Result := ALCreateSkSurface(LDestRect.Width, LDestRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AImage, // const image: sk_image_t;
      @LSrcRect, // const src: psk_rect_t;
      @LDestRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndStretchToSkSurface(const AStream: TStream; const W, H: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndStretchToSkSurface(LImage, W, H);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndStretchToSkSurface(const AResName: String; const W, H: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToSkSurface(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndStretchToSkSurface(const AFileName: String; const W, H: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndStretchToSkSurface(LImage, W, H);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndStretchToSkImage(const AStream: TStream; const W, H: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndStretchToSkSurface(LImage, W, H);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndStretchToSkImage(const AResName: String; const W, H: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToSkImage(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndStretchToSkImage(const AFileName: String; const W, H: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndStretchToSkSurface(LImage, W, H);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndStretchToJBitmap(const ABitmap: JBitmap; const W, H: single): JBitmap;
begin
  var LSrcRect := TRect.Create(0, 0, ABitmap.getWidth, ABitmap.getHeight);
  var LDestRect := TrectF.Create(0, 0, W, H).Round;

  var LMatrix := TJMatrix.JavaClass.init;
  LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
  result := TJBitmap.JavaClass.createBitmap(ABitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
  LMatrix := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndStretchToJBitmap(const AStream: TStream; const W, H: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndStretchToJBitmap(LBitmap, W, H);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndStretchToJBitmap(const AResName: String; const W, H: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToJBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndStretchToJBitmap(const AFileName: String; const W, H: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndStretchToJBitmap(LBitmap, W, H);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndStretchToCGContextRef(const AImage: ALOSImage; const W, H: single): CGContextRef;
begin
  var LDestRect := TrectF.Create(0, 0, W, H).Round;
  //-----
  Result := ALCreateCGContextRef(LDestRect.Width, LDestRect.Height);
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    ALLowerLeftCGRect(
      TpointF.Create(0,0),
      LDestRect.width,
      LDestRect.Height,
      LDestRect.height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndStretchToCGContextRef(const AStream: TStream; const W, H: single): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndStretchToCGContextRef(LImage, W, H);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndStretchToCGContextRef(const AResName: String; const W, H: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToCGContextRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndStretchToCGContextRef(const AFileName: String; const W, H: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndStretchToCGContextRef(LImage, W, H);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndStretchToCGImageRef(const AStream: TStream; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndStretchToCGContextRef(AStream, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndStretchToCGImageRef(const AResName: String; const W, H: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToCGImageRef(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndStretchToCGImageRef(const AFileName: String; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndStretchToCGContextRef(AFileName, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{***********************************************************************************************}
function ALLoadFromBitmapAndStretchToBitmap(const ABitmap: TBitmap; const W, H: single): TBitmap;
begin
  var LSrcRect := TrectF.Create(0, 0, ABitmap.width, ABitmap.height);
  var LDestRect := TrectF.Create(0, 0, w, h).Round;

  Result := TBitmap.Create(LDestRect.Width,LDestRect.Height);
  try

    if Result.Canvas.BeginScene then
    try
      Result.Canvas.DrawBitmap(
        ABitmap, // const ABitmap: TBitmap;
        LSrcRect, //const SrcRect,
        LDestRect, //const DstRect: TRectF;
        1, //const AOpacity: Single;
        false); // const HighSpeed: Boolean => disable interpolation
    finally
      Result.Canvas.EndScene;
    end;

  except
    AlFreeAndNil(Result);
    raise;
  end;
end;

{***********************************************************************************************}
function ALLoadFromStreamAndStretchToBitmap(const AStream: TStream; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndStretchToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*************************************************************************************************}
function ALLoadFromResourceAndStretchToBitmap(const AResName: String; const W, H: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndStretchToBitmap(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************}
function ALLoadFromFileAndStretchToBitmap(const AFileName: String; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndStretchToBitmap(LBitmap, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*****************************************************************************************************}
function ALLoadFromStreamAndStretchToDrawable(const AStream: TStream; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndStretchToSkImage(AStream, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndStretchToSkSurface(AStream, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndStretchToSkSurface(AStream, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndStretchToJBitmap(AStream, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndStretchToCGContextRef(AStream, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndStretchToBitmap(AStream, W, H);
  {$ENDIF}
end;

{*******************************************************************************************************}
function ALLoadFromResourceAndStretchToDrawable(const AResName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndStretchToSkImage(AResName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndStretchToSkSurface(AResName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndStretchToSkSurface(AResName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndStretchToJBitmap(AResName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndStretchToCGContextRef(AResName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndStretchToBitmap(AResName, W, H);
  {$ENDIF}
end;

{****************************************************************************************************}
function ALLoadFromFileAndStretchToDrawable(const AFileName: String; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndStretchToSkImage(AFileName, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndStretchToSkSurface(AFileName, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndStretchToSkSurface(AFileName, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndStretchToJBitmap(AFileName, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndStretchToCGContextRef(AFileName, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndStretchToBitmap(AFileName, W, H);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndWrapToSkSurface(const AImage: sk_image_t; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
begin
  case AWrapMode of
    TALImageWrapMode.Fit: Result := ALLoadFromSkImageAndFitIntoToSkSurface(AImage, W, H);
    TALImageWrapMode.Stretch: Result := ALLoadFromSkImageAndStretchToSkSurface(AImage, W, H);
    TALImageWrapMode.Place: Result := ALLoadFromSkImageAndPlaceIntoToSkSurface(AImage, W, H);
    TALImageWrapMode.FitAndCrop: Result := ALLoadFromSkImageAndFitIntoAndCropToSkSurface(AImage, W, H);
    else Raise exception.Create('Error 4CE56031-47CC-4532-ABC2-49C939A186A6')
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndWrapToSkSurface(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndWrapToSkSurface(LImage, AWrapMode, W, H);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndWrapToSkSurface(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToSkSurface(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndWrapToSkSurface(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndWrapToSkSurface(LImage, AWrapMode, W, H);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndWrapToSkImage(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndWrapToSkSurface(LImage, AWrapMode, W, H);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndWrapToSkImage(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToSkImage(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndWrapToSkImage(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndWrapToSkSurface(LImage, AWrapMode, W, H);
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndWrapToJBitmap(const ABitmap: JBitmap; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
begin
  case AWrapMode of
    TALImageWrapMode.Fit: Result := ALLoadFromJBitmapAndFitIntoToJBitmap(ABitmap, W, H);
    TALImageWrapMode.Stretch: Result := ALLoadFromJBitmapAndStretchToJBitmap(ABitmap, W, H);
    TALImageWrapMode.Place: Result := ALLoadFromJBitmapAndPlaceIntoToJBitmap(ABitmap, W, H);
    TALImageWrapMode.FitAndCrop: Result := ALLoadFromJBitmapAndFitIntoAndCropToJBitmap(ABitmap, W, H);
    else Raise exception.Create('Error AB4C9111-6649-45D7-8116-70758938CD47')
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndWrapToJBitmap(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndWrapToJBitmap(LBitmap, AWrapMode, W, H);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndWrapToJBitmap(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToJBitmap(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndWrapToJBitmap(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndWrapToJBitmap(LBitmap, AWrapMode, W, H);
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndWrapToCGContextRef(const AImage: ALOSImage; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
begin
  case AWrapMode of
    TALImageWrapMode.Fit: Result := ALLoadFromOSImageAndFitIntoToCGContextRef(AImage, W, H);
    TALImageWrapMode.Stretch: Result := ALLoadFromOSImageAndStretchToCGContextRef(AImage, W, H);
    TALImageWrapMode.Place: Result := ALLoadFromOSImageAndPlaceIntoToCGContextRef(AImage, W, H);
    TALImageWrapMode.FitAndCrop: Result := ALLoadFromOSImageAndFitIntoAndCropToCGContextRef(AImage, W, H);
    else Raise exception.Create('Error 71B29EF2-207C-479A-B891-42075C545EA8')
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndWrapToCGContextRef(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndWrapToCGContextRef(LImage, AWrapMode, W, H);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndWrapToCGContextRef(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToCGContextRef(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndWrapToCGContextRef(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndWrapToCGContextRef(LImage, AWrapMode, W, H);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndWrapToCGImageRef(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndWrapToCGContextRef(AStream, AWrapMode, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndWrapToCGImageRef(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToCGImageRef(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndWrapToCGImageRef(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndWrapToCGContextRef(AFileName, AWrapMode, W, H);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*******************************************************************************************************************************}
function ALLoadFromBitmapAndWrapToBitmap(const ABitmap: TBitmap; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
begin
  case AWrapMode of
    TALImageWrapMode.Fit: Result := ALLoadFromBitmapAndFitIntoToBitmap(ABitmap, W, H);
    TALImageWrapMode.Stretch: Result := ALLoadFromBitmapAndStretchToBitmap(ABitmap, W, H);
    TALImageWrapMode.Place: Result := ALLoadFromBitmapAndPlaceIntoToBitmap(ABitmap, W, H);
    TALImageWrapMode.FitAndCrop: Result := ALLoadFromBitmapAndFitIntoAndCropToBitmap(ABitmap, W, H);
    else Raise exception.Create('Error 68B96273-FB54-47B5-BD9B-A2DD75BEE07F')
  end;
end;

{*******************************************************************************************************************************}
function ALLoadFromStreamAndWrapToBitmap(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    result := ALLoadFromBitmapAndWrapToBitmap(LBitmap, AWrapMode, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*********************************************************************************************************************************}
function ALLoadFromResourceAndWrapToBitmap(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndWrapToBitmap(LStream, AWrapMode, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{******************************************************************************************************************************}
function ALLoadFromFileAndWrapToBitmap(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TBitmap;
begin
  var LBitmap := Tbitmap.CreateFromFile(AFileName);
  try
    result := ALLoadFromBitmapAndWrapToBitmap(LBitmap, AWrapMode, W, H);
  finally
    ALFreeAndNil(LBitmap);
  end;
end;

{*************************************************************************************************************************************}
function ALLoadFromStreamAndWrapToDrawable(const AStream: TStream; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndWrapToSkImage(AStream, AWrapMode, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndWrapToSkSurface(AStream, AWrapMode, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndWrapToSkSurface(AStream, AWrapMode, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndWrapToJBitmap(AStream, AWrapMode, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndWrapToCGContextRef(AStream, AWrapMode, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndWrapToBitmap(AStream, AWrapMode, W, H);
  {$ENDIF}
end;

{***************************************************************************************************************************************}
function ALLoadFromResourceAndWrapToDrawable(const AResName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndWrapToSkImage(AResName, AWrapMode, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndWrapToSkSurface(AResName, AWrapMode, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndWrapToSkSurface(AResName, AWrapMode, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndWrapToJBitmap(AResName, AWrapMode, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndWrapToCGContextRef(AResName, AWrapMode, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndWrapToBitmap(AResName, AWrapMode, W, H);
  {$ENDIF}
end;

{************************************************************************************************************************************}
function ALLoadFromFileAndWrapToDrawable(const AFileName: String; const AWrapMode: TALImageWrapMode; const W, H: single): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndWrapToSkImage(AFileName, AWrapMode, W, H);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndWrapToSkSurface(AFileName, AWrapMode, W, H);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndWrapToSkSurface(AFileName, AWrapMode, W, H);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndWrapToJBitmap(AFileName, AWrapMode, W, H);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndWrapToCGContextRef(AFileName, AWrapMode, W, H);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndWrapToBitmap(AFileName, AWrapMode, W, H);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageAndNormalizeOrientationToSkSurface(const AImage: sk_image_t; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
begin
  //No need to care about AExifOrientationInfo with skimage
  //because skimage already loaded the image with the good orientation.
  var LRect := Trect.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LRectF := TRectF.Create(LRect);

  Result := ALCreateSkSurface(LRect.Width, LRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AImage, // const image: sk_image_t;
      @LRectF, // const src: psk_rect_t;
      @LRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndNormalizeOrientationToSkSurface(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageAndNormalizeOrientationToSkSurface(LImage, AExifOrientationInfo);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndNormalizeOrientationToSkSurface(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToSkSurface(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndNormalizeOrientationToSkSurface(const AFileName: String): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageAndNormalizeOrientationToSkSurface(LImage, AlGetExifOrientationInfo(aFileName));
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamAndNormalizeOrientationToSkImage(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      var LSurface := ALLoadFromSkImageAndNormalizeOrientationToSkSurface(LImage, AExifOrientationInfo);
      try
        Result := ALCreateSkImageFromSkSurface(LSurface);
      finally
        sk4d_refcnt_unref(LSurface);
      end;
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceAndNormalizeOrientationToSkImage(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToSkImage(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileAndNormalizeOrientationToSkImage(const AFileName: String): sk_image_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    var LSurface := ALLoadFromSkImageAndNormalizeOrientationToSkSurface(LImage, AlGetExifOrientationInfo(aFileName));
    try
      Result := ALCreateSkImageFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromJBitmapAndNormalizeOrientationToJBitmap(const ABitmap: JBitmap; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
begin
  var LMatrix := TJMatrix.JavaClass.init;
  case aExifOrientationInfo of
    TalExifOrientationInfo.NORMAL:;
    TalExifOrientationInfo.FLIP_HORIZONTAL: LMatrix.setScale(-1, 1);
    TalExifOrientationInfo.ROTATE_180: LMatrix.setRotate(180);
    TalExifOrientationInfo.FLIP_VERTICAL: begin
                                            LMatrix.setRotate(180);
                                            LMatrix.postScale(-1, 1);
                                          end;
    TalExifOrientationInfo.TRANSPOSE: begin
                                        LMatrix.setRotate(90);
                                        LMatrix.postScale(-1, 1);
                                      end;
    TalExifOrientationInfo.ROTATE_90: LMatrix.setRotate(90);
    TalExifOrientationInfo.TRANSVERSE: begin
                                         LMatrix.setRotate(-90);
                                         LMatrix.postScale(-1, 1);
                                       end;
    TalExifOrientationInfo.ROTATE_270: LMatrix.setRotate(-90);
    TalExifOrientationInfo.UNDEFINED:;
    else
      raise exception.Create('Error 49B8D091-6743-426E-8E2F-0802A9B681E7');
  end;
  Result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, 0{X}, 0{Y}, aBitmap.getwidth{Width}, aBitmap.getheight{height}, LMatrix{m}, True{filter});
  LMatrix := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamAndNormalizeOrientationToJBitmap(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
begin
  var LLength := AStream.Size-AStream.Position;
  var LArray := TJavaArray<Byte>.Create(LLength);
  try
    AStream.ReadBuffer(LArray.Data^, LLength);
    var LOptions := TJBitmapFactory_Options.Javaclass.Init;
    if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
    var LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, LLength, LOptions);
    if LBitmap = nil then raise Exception.create('Failed to decode bitmap from stream');
    try
      Result := ALLoadFromJBitmapAndNormalizeOrientationToJBitmap(LBitmap, AExifOrientationInfo);
    finally
      if not LBitmap.equals(Result) then LBitmap.recycle;
      LBitmap := nil;
    end;
    LOptions := nil;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceAndNormalizeOrientationToJBitmap(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToJBitmap(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileAndNormalizeOrientationToJBitmap(const AFileName: String): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  var LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if LBitmap = nil then raise Exception.create('Failed to load bitmap from file');
  try
    Result := ALLoadFromJBitmapAndNormalizeOrientationToJBitmap(LBitmap, AlGetExifOrientationInfo(aFileName));
  finally
    if not LBitmap.equals(Result) then LBitmap.recycle;
    LBitmap := nil;
  end;
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageAndNormalizeOrientationToCGContextRef(const AImage: ALOSImage; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
begin
  var w, h: integer;
  if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270,{UIImageOrientationLeft}
                              TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                              TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                              TalExifOrientationInfo.TRANSVERSE {UIImageOrientationRightMirrored}] then begin
    w := ALOSImageGetHeight(AImage);
    h := ALOSImageGetWidth(AImage);
  end
  else begin
    w := ALOSImageGetWidth(AImage);
    h := ALOSImageGetHeight(AImage);
  end;
  var LMatrix := CGAffineTransformIdentity;
  case aExifOrientationInfo of

    //UIImageOrientationUp: The original pixel data matches the image's intended display orientation.
    TalExifOrientationInfo.NORMAL:;

    //UIImageOrientationUpMirrored: The image has been horizontally flipped from the orientation of its original pixel data
    TalExifOrientationInfo.FLIP_HORIZONTAL: begin
                                              //LMatrix.setScale(-1, 1);
                                              LMatrix := CGAffineTransformTranslate(LMatrix, w, 0);
                                              LMatrix := CGAffineTransformScale(LMatrix, -1, 1);
                                            end;

    //UIImageOrientationDown: The image has been rotated 180° from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_180: begin
                                         //LMatrix.setRotate(180);
                                         LMatrix := CGAffineTransformTranslate(LMatrix, w, h);
                                         LMatrix := CGAffineTransformRotate(LMatrix, degToRad(180));
                                       end;

    //UIImageOrientationDownMirrored: The image has been vertically flipped from the orientation of its original pixel data.
    TalExifOrientationInfo.FLIP_VERTICAL: begin
                                            //LMatrix.setRotate(180);
                                            //LMatrix.postScale(-1, 1);
                                            LMatrix := CGAffineTransformTranslate(LMatrix, w, h);
                                            LMatrix := CGAffineTransformRotate(LMatrix, degToRad(180));
                                            LMatrix := CGAffineTransformTranslate(LMatrix, w, 0);
                                            LMatrix := CGAffineTransformScale(LMatrix, -1, 1);
                                          end;

    //UIImageOrientationLeftMirrored: The image has been rotated 90° clockwise and flipped horizontally from the orientation of its original pixel data.
    TalExifOrientationInfo.TRANSPOSE: begin
                                        //LMatrix.setRotate(90);
                                        //LMatrix.postScale(-1, 1);
                                        LMatrix := CGAffineTransformTranslate(LMatrix, w, 0);
                                        LMatrix := CGAffineTransformRotate(LMatrix, degToRad(90));
                                        LMatrix := CGAffineTransformTranslate(LMatrix, h, 0);
                                        LMatrix := CGAffineTransformScale(LMatrix, -1, 1);
                                      end;

    //UIImageOrientationRight: The image has been rotated 90° clockwise from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_90: begin
                                        //LMatrix.setRotate(90);
                                        LMatrix := CGAffineTransformTranslate(LMatrix, 0, h);
                                        LMatrix := CGAffineTransformRotate(LMatrix, -degToRad(90));
                                      end;

    //UIImageOrientationRightMirrored: The image has been rotated 90° COUNTERclockwise and flipped horizontally from the orientation of its original pixel data.
    TalExifOrientationInfo.TRANSVERSE: begin
                                         //LMatrix.setRotate(-90);
                                         //LMatrix.postScale(-1, 1);
                                         LMatrix := CGAffineTransformTranslate(LMatrix, 0, h);
                                         LMatrix := CGAffineTransformRotate(LMatrix, -degToRad(90));
                                         LMatrix := CGAffineTransformTranslate(LMatrix, h, 0);
                                         LMatrix := CGAffineTransformScale(LMatrix, -1, 1);
                                       end;

    //UIImageOrientationLeft: The image has been rotated 90° COUNTERclockwise from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_270: begin
                                         //LMatrix.setRotate(-90);
                                         LMatrix := CGAffineTransformTranslate(LMatrix, w, 0);
                                         LMatrix := CGAffineTransformRotate(LMatrix, degToRad(90));
                                       end;

    //UNDEFINED
    TalExifOrientationInfo.UNDEFINED:;

    //Error
    else
      raise exception.Create('Error 6205CE05-058D-46C0-A3C8-5491134178D6');

  end;

  Result := ALCreateCGContextRef(W, H);
  CGContextConcatCTM(Result, LMatrix);
  if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                              TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                              TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                              TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then
    CGContextDrawImage(
      Result, // c: The graphics context in which to draw the image.
      CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
      ALOSImageGetCgImage(AImage)) // image The image to draw.
  else
    CGContextDrawImage(
      Result, // c: The graphics context in which to draw the image.
      CGRectMake(0, 0, w, h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
      ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndNormalizeOrientationToCGContextRef(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageAndNormalizeOrientationToCGContextRef(LImage, AExifOrientationInfo);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndNormalizeOrientationToCGContextRef(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToCGContextRef(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndNormalizeOrientationToCGContextRef(const AFileName: String): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageAndNormalizeOrientationToCGContextRef(LImage, AlGetExifOrientationInfo(aFileName));
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamAndNormalizeOrientationToCGImageRef(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamAndNormalizeOrientationToCGContextRef(AStream, AExifOrientationInfo);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceAndNormalizeOrientationToCGImageRef(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToCGImageRef(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileAndNormalizeOrientationToCGImageRef(const AFileName: String): CGImageRef;
begin
  var LContextRef := ALLoadFromFileAndNormalizeOrientationToCGContextRef(AFileName);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{********************************************************************************************************************************************}
function ALLoadFromStreamAndNormalizeOrientationToBitmap(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): TBitmap;
begin
  Result := Tbitmap.CreateFromStream(aStream);
  case aExifOrientationInfo of
    TalExifOrientationInfo.NORMAL: exit;
    TalExifOrientationInfo.FLIP_HORIZONTAL: Result.FlipHorizontal;
    TalExifOrientationInfo.ROTATE_180: Result.Rotate(180);
    TalExifOrientationInfo.FLIP_VERTICAL: Result.FlipVertical;
    TalExifOrientationInfo.TRANSPOSE: begin
                                        Result.Rotate(90);
                                        Result.FlipHorizontal;
                                      end;
    TalExifOrientationInfo.ROTATE_90: Result.Rotate(90);
    TalExifOrientationInfo.TRANSVERSE: begin
                                         Result.Rotate(-90);
                                         Result.FlipHorizontal;
                                       end;
    TalExifOrientationInfo.ROTATE_270: Result.Rotate(270);
    TalExifOrientationInfo.UNDEFINED: exit;
    else
      raise exception.Create('Error 1C368047-00C4-4F68-8C77-56956FABCF92');
  end;
end;

{**********************************************************************************************************************************************}
function ALLoadFromResourceAndNormalizeOrientationToBitmap(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamAndNormalizeOrientationToBitmap(LStream, AExifOrientationInfo);
  finally
    ALfreeandNil(LStream);
  end;
end;

{***************************************************************************************}
function ALLoadFromFileAndNormalizeOrientationToBitmap(const AFileName: String): TBitmap;
begin
  Result := Tbitmap.CreateFromFile(AFileName);
  case AlGetExifOrientationInfo(aFileName) of
    TalExifOrientationInfo.NORMAL: exit;
    TalExifOrientationInfo.FLIP_HORIZONTAL: Result.FlipHorizontal;
    TalExifOrientationInfo.ROTATE_180: Result.Rotate(180);
    TalExifOrientationInfo.FLIP_VERTICAL: Result.FlipVertical;
    TalExifOrientationInfo.TRANSPOSE: begin
                                        Result.Rotate(90);
                                        Result.FlipHorizontal;
                                      end;
    TalExifOrientationInfo.ROTATE_90: Result.Rotate(90);
    TalExifOrientationInfo.TRANSVERSE: begin
                                         Result.Rotate(-90);
                                         Result.FlipHorizontal;
                                       end;
    TalExifOrientationInfo.ROTATE_270: Result.Rotate(270);
    TalExifOrientationInfo.UNDEFINED: exit;
    else
      raise exception.Create('Error 2F09739F-4CB7-46DC-A665-C64D626DD1D7');
  end;
end;

{**************************************************************************************************************************************************}
function ALLoadFromStreamAndNormalizeOrientationToDrawable(const AStream: TStream; const AExifOrientationInfo: TALExifOrientationInfo): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamAndNormalizeOrientationToSkImage(AStream, AExifOrientationInfo);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamAndNormalizeOrientationToSkSurface(AStream, AExifOrientationInfo);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamAndNormalizeOrientationToSkSurface(AStream, AExifOrientationInfo);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamAndNormalizeOrientationToJBitmap(AStream, AExifOrientationInfo);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamAndNormalizeOrientationToCGContextRef(AStream, AExifOrientationInfo);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamAndNormalizeOrientationToBitmap(AStream, AExifOrientationInfo);
  {$ENDIF}
end;

{****************************************************************************************************************************************************}
function ALLoadFromResourceAndNormalizeOrientationToDrawable(const AResName: String; const AExifOrientationInfo: TALExifOrientationInfo): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceAndNormalizeOrientationToSkImage(AResName, AExifOrientationInfo);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceAndNormalizeOrientationToSkSurface(AResName, AExifOrientationInfo);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceAndNormalizeOrientationToSkSurface(AResName, AExifOrientationInfo);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceAndNormalizeOrientationToJBitmap(AResName, AExifOrientationInfo);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceAndNormalizeOrientationToCGContextRef(AResName, AExifOrientationInfo);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceAndNormalizeOrientationToBitmap(AResName, AExifOrientationInfo);
  {$ENDIF}
end;

{*********************************************************************************************}
function ALLoadFromFileAndNormalizeOrientationToDrawable(const AFileName: String): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileAndNormalizeOrientationToSkImage(AFileName);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileAndNormalizeOrientationToSkSurface(AFileName);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileAndNormalizeOrientationToSkSurface(AFileName);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileAndNormalizeOrientationToJBitmap(AFileName);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileAndNormalizeOrientationToCGContextRef(AFileName);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileAndNormalizeOrientationToBitmap(AFileName);
  {$ENDIF}
end;

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromSkImageToSkSurface(const AImage: sk_image_t): sk_surface_t;
begin
  var LRect := Trect.Create(0, 0, sk4d_image_get_width(AImage), sk4d_image_get_Height(AImage));
  var LRectF := TRectF.Create(LRect);

  Result := ALCreateSkSurface(LRect.Width, LRect.Height);

  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  try
    sk4d_paint_set_antialias(LPaint, true);
    sk4d_paint_set_dither(LPaint, true);

    var LCanvas := ALSkCheckHandle(sk4d_surface_get_canvas(Result));

    var LSamplingoptions := ALGetCubicMitchellNetravaliSkSamplingoptions;
    sk4d_canvas_draw_image_rect(
      LCanvas, // self: sk_canvas_t;
      AImage, // const image: sk_image_t;
      @LRectF, // const src: psk_rect_t;
      @LRectF,  // const dest: psk_rect_t;
      @LSamplingoptions, // const sampling: psk_samplingoptions_t;
      LPaint, // const paint: sk_paint_t;
      FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
  finally
    sk4d_paint_destroy(LPaint);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamToSkSurface(const AStream: TStream): sk_surface_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
    try
      Result := ALLoadFromSkImageToSkSurface(LImage);
    finally
      sk4d_refcnt_unref(LImage);
    end;
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceToSkSurface(const AResName: String): sk_surface_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToSkSurface(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileToSkSurface(const AFileName: String): sk_surface_t;
begin
  var LImage := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
  try
    Result := ALLoadFromSkImageToSkSurface(LImage);
  finally
    sk4d_refcnt_unref(LImage);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromStreamToSkImage(const AStream: TStream): sk_image_t;
begin
  var LStream := ALSkCheckHandle(sk4d_streamadapter_create(AStream));
  try
    var LStreamadapterProcs: sk_streamadapter_procs_t;
    LStreamadapterProcs.get_length := ALSkStreamAdapterGetLengthProc;
    LStreamadapterProcs.get_position := ALSkStreamAdapterGetPositionProc;
    LStreamadapterProcs.read := ALSkStreamAdapterReadProc;
    LStreamadapterProcs.seek := ALSkStreamAdapterSeekProc;
    sk4d_streamadapter_set_procs(@LStreamadapterProcs);
    Result := ALSkCheckHandle(sk4d_image_make_from_encoded_stream(LStream));
  finally
    sk4d_streamadapter_destroy(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromResourceToSkImage(const AResName: String): sk_image_t;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToSkImage(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{****************************}
{$IF defined(ALSkiaAvailable)}
function ALLoadFromFileToSkImage(const AFileName: String): sk_image_t;
begin
  Result := ALSkCheckHandle(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromStreamToJBitmap(const AStream: TStream): JBitmap;
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
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromResourceToJBitmap(const AResName: String): JBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToJBitmap(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALLoadFromFileToJBitmap(const AFileName: String): JBitmap;
begin
  var LOptions := TJBitmapFactory_Options.Javaclass.Init;
  if TOSVersion.Check(8, 0) then LOptions.inPreferredColorSpace := ALGetGlobalJColorSpace;
  Result := TJBitmapFactory.JavaClass.decodeFile(StringToJString(AFileName), LOptions);
  if Result = nil then raise Exception.create('Failed to load bitmap from file');
  LOptions := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromOSImageToCGContextRef(const AImage: ALOSImage): CGContextRef;
begin
  var LRect := Trect.Create(0, 0, ALOSImageGetWidth(AImage), ALOSImageGetHeight(AImage));
  var LRectF := TRectF.Create(LRect);
  //-----
  Result := ALCreateCGContextRef(LRect.Width, LRect.Height);
  CGContextDrawImage(
    Result, // c: The graphics context in which to draw the image.
    CGRectMake(LRectF), // rect The location and dimensions in user space of the bounding box in which to draw the image.
    ALOSImageGetCgImage(AImage)); // image The image to draw.
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamToCGContextRef(const AStream: TStream): CGContextRef;
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
      var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithData(LData));
      if LImage = nil then raise Exception.create('Failed to decode image from stream');
      try
        result := ALLoadFromOSImageToCGContextRef(LImage);
      finally
        LImage.release;
      end;
    finally
      LData.release;
    end;
  finally
    ALFreeAndNil(LMemoryStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceToCGContextRef(const AResName: String): CGContextRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToCGContextRef(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileToCGContextRef(const AFileName: String): CGContextRef;
begin
  var LImage := TALOSImage.Wrap(TALOSImage.alloc.initWithContentsOfFile(StrToNSStr(AFilename)));
  if LImage = nil then raise Exception.create('Failed to load image from file');
  try
    result := ALLoadFromOSImageToCGContextRef(LImage);
  finally
    LImage.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromStreamToCGImageRef(const AStream: TStream): CGImageRef;
begin
  var LContextRef := ALLoadFromStreamToCGContextRef(AStream);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromResourceToCGImageRef(const AResName: String): CGImageRef;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToCGImageRef(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLoadFromFileToCGImageRef(const AFileName: String): CGImageRef;
begin
  var LContextRef := ALLoadFromFileToCGContextRef(AFileName);
  try
    // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
    // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
    // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
    // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
    // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
    // you can avoid the actual physical copy of the data.
    result := CGBitmapContextCreateImage(LContextRef);
    if result = nil then raise Exception.Create('Failed to create CGImageRef from CGContextRef');
  finally
    CGContextRelease(LContextRef);
  end;
end;
{$ENDIF}

{*****************************************************************}
function ALLoadFromStreamToBitmap(const AStream: TStream): TBitmap;
begin
  Result := Tbitmap.CreateFromStream(aStream);
end;

{*******************************************************************}
function ALLoadFromResourceToBitmap(const AResName: String): TBitmap;
begin
  var LStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    result := ALLoadFromStreamToBitmap(LStream);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************}
function ALLoadFromFileToBitmap(const AFileName: String): TBitmap;
begin
  Result := Tbitmap.CreateFromFile(AFileName);
end;

{***********************************************************************}
function ALLoadFromStreamToDrawable(const AStream: TStream): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromStreamToSkImage(AStream);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromStreamToSkSurface(AStream);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromStreamToSkSurface(AStream);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromStreamToJBitmap(AStream);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromStreamToCGContextRef(AStream);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromStreamToBitmap(AStream);
  {$ENDIF}
end;

{*************************************************************************}
function ALLoadFromResourceToDrawable(const AResName: String): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromResourceToSkImage(AResName);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromResourceToSkSurface(AResName);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromResourceToSkSurface(AResName);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromResourceToJBitmap(AResName);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromResourceToCGContextRef(AResName);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromResourceToBitmap(AResName);
  {$ENDIF}
end;

{**********************************************************************}
function ALLoadFromFileToDrawable(const AFileName: String): TALDrawable;
begin
  {$IF defined(ALSkiaEngine)}
    {$IF defined(ALSkiaCanvas)}
    Result := ALLoadFromFileToSkImage(AFileName);
    {$ELSEIF defined(ALGPUCanvas)}
    var LSurface := ALLoadFromFileToSkSurface(AFileName);
    try
      result := ALCreateTextureFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ELSE}
    var LSurface := ALLoadFromFileToSkSurface(AFileName);
    try
      result := ALCreateBitmapFromSkSurface(LSurface);
    finally
      sk4d_refcnt_unref(LSurface);
    end;
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  var LBitmap := ALLoadFromFileToJBitmap(AFileName);
  try
    result := ALCreateTextureFromJBitmap(LBitmap);
  finally
    LBitmap.recycle;
    LBitmap := nil;
  end;
  {$ELSEIF defined(ALAppleOS)}
  var LCGContextRef := ALLoadFromFileToCGContextRef(AFileName);
  try
    {$IF defined(ALGPUCanvas)}
    result := ALCreateTextureFromCGContextRef(LCGContextRef);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(LCGContextRef);
    {$ENDIF}
  finally
    CGContextRelease(LCGContextRef);
  end;
  {$ELSE}
  Result := ALLoadFromFileToBitmap(AFileName);
  {$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
/// THE CODE ABOVE (INTERFACE + IMPLEMENTATION) WAS AUTO-GENERATED FROM      ///
/// <ALCINOE>\References\FMXGraphicsBuilder.                                 ///
////////////////////////////////////////////////////////////////////////////////


{*****************************************************}
function ALGetImageSize(const aStream: TStream): TSize;
begin

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
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
  var LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    Result := TSize.Create(LBitmap.Width, LBitmap.height);
  finally
    AlFreeAndNil(LBitmap);
  end;
  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************************************************}
function AlGetExifOrientationInfo(const aFilename: String): TalExifOrientationInfo;
begin

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
  result := TalExifOrientationInfo.NORMAL; // << todo - https://stackoverflow.com/questions/18622152/read-exif-gps-info-using-delphi
  {$ENDIF}
  {$ENDREGION}

end;

{*************************************************************************************************}
function AlGetImageSignature(const aStream: TStream; const aSignatureLength: integer = 12): Tbytes;
begin
  aStream.Position := 0;
  SetLength(result, aSignatureLength);
  aStream.ReadBuffer(result[0], min(length(result),aStream.Size));
  if aStream.Size < length(Result) then
    for var I := aStream.Size to High(result) do
      result[i] := $00;
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
  if (AFillResourceName <> '') then begin
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
           var ARect: TrectF;
           const AFill: TALBrush;
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
    ALClearCanvas(LCanvas, TAlphaColors.Null);
    Result := ALCreateDrawableFromSurface(LSurface);
  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;
end;

{************************}
procedure ALDrawRectangle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFillColor: TAlphaColor;
            const AFillGradientStyle: TGradientStyle;
            const AFillGradientColors: TArray<TAlphaColor>;
            const AFillGradientOffsets: TArray<Single>;
            const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
            const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
            const AFillResourceName: String;
            Const AFillWrapMode: TALImageWrapMode;
            Const AFillBackgroundMarginsRect: TRectF;
            Const AFillImageMarginsRect: TRectF;
            const AStateLayerOpacity: Single;
            const AStateLayerColor: TAlphaColor;
            Const AStateLayerMarginsRect: TRectF;
            const AStateLayerXRadius: Single;
            const AStateLayerYRadius: Single;
            const ADrawStateLayerOnTop: Boolean;
            const AStrokeColor: TalphaColor;
            const AStrokeThickness: Single;
            const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
            const AShadowBlur: Single;
            const AShadowOffsetX: Single;
            const AShadowOffsetY: Single;
            const ASides: TSides;
            const ACorners: TCorners;
            const AXRadius: Single;
            const AYRadius: Single);

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
              const aCanvas: sk_canvas_t;
              const aPaint: sk_Paint_t;
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean);

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
    var LScaledXRadius: Single := AXRadius;
    var LScaledYRadius: Single := AYRadius;
    if LScaledXRadius < 0 then LScaledXRadius := (LRect.Width/100)*abs(LScaledXRadius)
    else LScaledXRadius := LScaledXRadius * AScale;
    if LScaledYRadius < 0 then LScaledYRadius := (LRect.Height/100)*abs(LScaledYRadius)
    else LScaledYRadius := LScaledYRadius* AScale;
    var LMaxRadius := Min(LRect.Width / 2, LRect.Height / 2);
    LScaledXRadius := min(LScaledXRadius, LMaxRadius);
    LScaledYRadius := min(LScaledYRadius, LMaxRadius);

    // use drawRoundRect
    if ((compareValue(LScaledXRadius, 0, TEpsilon.Position) > 0) and
        (compareValue(LScaledYRadius, 0, TEpsilon.position) > 0)) and
       (ACorners<>[]) and
       (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      var LRRect :=  ALSkCheckHandle(sk4d_rrect_create);
      try
        var LRadii: array[0..4] of TPointF;
        if TCorner.TopLeft in ACorners then LRadii[0] := TPointF.Create(LScaledXRadius, LScaledYRadius)
        else LRadii[0] := TPointF.Create(0, 0);

        if TCorner.TopRight in ACorners then LRadii[1] := TPointF.Create(LScaledXRadius, LScaledYRadius)
        else LRadii[1] := TPointF.Create(0, 0);

        if TCorner.BottomRight in ACorners then LRadii[2] := TPointF.Create(LScaledXRadius, LScaledYRadius)
        else LRadii[2] := TPointF.Create(0, 0);

        if TCorner.BottomLeft in ACorners then LRadii[3] := TPointF.Create(LScaledXRadius, LScaledYRadius)
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
             (ACorners=[])) and
            (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
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
          var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
          if (aDrawOnlyBorder) or
             ((LRectIsEqualsToStrokeRect) and
              (LShadowcolor = TalphaColorRec.Null) and
              (ACorners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
              (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right])) then begin
            LHalfStrokeThickness := LScaledStrokeThickness / 2;
            LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
          end
          else if (LRectIsEqualsToStrokeRect) and
                  (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) and
                  (ACorners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
                  (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
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
           (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := ACorners
        else LCorners := [];
        //--
        If aDrawOnlyBorder then begin
          If (TCorner.TopRight in LCorners)    and (not (TSide.Top in ASides))    and (not (Tside.Right in ASides)) then LCorners := LCorners - [TCorner.TopRight];
          If (TCorner.TopLeft in LCorners)     and (not (TSide.Top in ASides))    and (not (Tside.Left in ASides))  then LCorners := LCorners - [TCorner.TopLeft];
          If (TCorner.BottomRight in LCorners) and (not (TSide.Bottom in ASides)) and (not (Tside.Right in ASides)) then LCorners := LCorners - [TCorner.BottomRight];
          If (TCorner.BottomLeft in LCorners)  and (not (TSide.Bottom in ASides)) and (not (Tside.Left in ASides))  then LCorners := LCorners - [TCorner.BottomLeft];
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
             (TSide.right in ASides) then begin
            _RLineTo(LPathBuilder, 0, -LYradius);
            if (aDrawOnlyBorder) and
               (TSide.right in ASides) and
               (not (TSide.top in ASides)) then begin
              _RLineTo(LPathBuilder, 0, -LHalfStrokeThickness);
              _RMoveTo(LPathBuilder, 0, LHalfStrokeThickness);
            end;
          end
          else _RMoveTo(LPathBuilder, 0, -LYradius); // aDrawOnlyBorder AND not TSide.right
          //----
          if (not aDrawOnlyBorder) or
             (TSide.top in ASides) then begin
            if (aDrawOnlyBorder) and
               (TSide.top in ASides) and
               (not (TSide.right in ASides)) then begin
              _RMoveTo(LPathBuilder, LHalfStrokeThickness, 0);
              _RLineTo(LPathBuilder, -LHalfStrokeThickness, 0);
            end;
            _RLineTo(LPathBuilder, -LXRadius,0);
          end
          else _RMoveTo(LPathBuilder, -LXRadius,0); // aDrawOnlyBorder AND not TSide.top
        end;
        //-----
        if (not aDrawOnlyBorder) or
           (TSide.Top in ASides) then _RLineTo(LPathBuilder, -LWidthMinusCorners, 0)
        else _RMoveTo(LPathBuilder, -LWidthMinusCorners, 0);

        //----- TopLeft
        if (TCorner.TopLeft in LCorners) then begin
          _RQuadTo(LPathBuilder, -LXRadius, 0, -LXRadius, LYradius);
        end
        else begin
          if (not aDrawOnlyBorder) or
             (TSide.top in ASides) then begin
            _RLineTo(LPathBuilder, -LXRadius, 0);
            if (aDrawOnlyBorder) and
               (TSide.top in ASides) and
               (not (TSide.left in ASides)) then begin
              _RLineTo(LPathBuilder, -LHalfStrokeThickness, 0);
              _RMoveTo(LPathBuilder, LHalfStrokeThickness, 0);
            end;
          end
          else _RMoveTo(LPathBuilder, -LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
          //----
          if (not aDrawOnlyBorder) or
             (TSide.left in ASides) then begin
            if (aDrawOnlyBorder) and
               (TSide.left in ASides) and
               (not (TSide.top in ASides)) then begin
              _RMoveTo(LPathBuilder, 0, -LHalfStrokeThickness);
              _RLineTo(LPathBuilder, 0, LHalfStrokeThickness);
            end;
            _RLineTo(LPathBuilder, 0,LYradius);
          end
          else _RMoveTo(LPathBuilder, 0,LYradius); // aDrawOnlyBorder AND not TSide.left
        end;
        //-----
        if (not aDrawOnlyBorder) or
           (TSide.left in ASides) then _RLineTo(LPathBuilder, 0, LHeightMinusCorners)
        else _RMoveTo(LPathBuilder, 0, LHeightMinusCorners);

        //----- BottomLeft
        if (TCorner.BottomLeft in LCorners) then begin
          _RQuadTo(LPathBuilder, 0, LYradius, LXRadius, LYradius);
        end
        else begin
          if (not aDrawOnlyBorder) or
             (TSide.left in ASides) then begin
            _RLineTo(LPathBuilder, 0, LYradius);
            if (aDrawOnlyBorder) and
               (TSide.left in ASides) and
               (not (TSide.bottom in ASides)) then begin
              _RLineTo(LPathBuilder, 0, LHalfStrokeThickness);
              _RMoveTo(LPathBuilder, 0, -LHalfStrokeThickness);
            end;
          end
          else _RMoveTo(LPathBuilder, 0, LYradius); // aDrawOnlyBorder AND not TSide.left
          //----
          if (not aDrawOnlyBorder) or
             (TSide.bottom in ASides) then begin
            if (aDrawOnlyBorder) and
               (TSide.bottom in ASides) and
               (not (TSide.left in ASides)) then begin
              _RMoveTo(LPathBuilder, -LHalfStrokeThickness, 0);
              _RLineTo(LPathBuilder, LHalfStrokeThickness, 0);
            end;
            _RLineTo(LPathBuilder, LXRadius,0);
          end
          else _RMoveTo(LPathBuilder, LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        end;
        //-----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in ASides) then _RLineTo(LPathBuilder, LWidthMinusCorners, 0)
        else _RMoveTo(LPathBuilder, LWidthMinusCorners, 0);

        //----- BottomRight
        if (TCorner.BottomRight in LCorners) then begin
          _RQuadTo(LPathBuilder, LXRadius, 0, LXRadius, -LYradius);
        end
        else begin
          if (not aDrawOnlyBorder) or
             (TSide.bottom in ASides) then begin
            _RLineTo(LPathBuilder, LXRadius,0);
            if (aDrawOnlyBorder) and
               (TSide.bottom in ASides) and
               (not (TSide.right in ASides)) then begin
              _RLineTo(LPathBuilder, LHalfStrokeThickness, 0);
              _RMoveTo(LPathBuilder, -LHalfStrokeThickness, 0);
            end;
          end
          else _RMoveTo(LPathBuilder, LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
          //----
          if (not aDrawOnlyBorder) or
             (TSide.right in ASides) then begin
            if (aDrawOnlyBorder) and
               (TSide.right in ASides) and
               (not (TSide.bottom in ASides)) then begin
              _RMoveTo(LPathBuilder, 0, LHalfStrokeThickness);
              _RLineTo(LPathBuilder, 0, -LHalfStrokeThickness);
            end;
            _RLineTo(LPathBuilder, 0,-LYradius);
          end
          else _RMoveTo(LPathBuilder, 0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        end;
        //-----
        if (not aDrawOnlyBorder) or
           (TSide.right in ASides) then _RLineTo(LPathBuilder, 0, -LHeightMinusCorners)
        else _RMoveTo(LPathBuilder, 0, -LHeightMinusCorners);
        //-----
        if (TSide.right in ASides) and
           (TSide.top in ASides) then sk4d_pathbuilder_close(LPathBuilder);


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
              const aCanvas: Jcanvas;
              const aPaint: JPaint;
              const aRect: TrectF;
              const aDrawOnlyBorder: Boolean;
              const aForceDrawPath: Boolean;
              const aClipPath: Boolean);
  begin

    // Init LRect
    var LRect := aRect;

    // Init LScaledXRadius and LScaledYRadius
    var LScaledXRadius: Single := AXRadius;
    var LScaledYRadius: Single := AYRadius;
    if LScaledXRadius < 0 then LScaledXRadius := (LRect.Width/100)*abs(LScaledXRadius)
    else LScaledXRadius := LScaledXRadius * AScale;
    if LScaledYRadius < 0 then LScaledYRadius := (LRect.Height/100)*abs(LScaledYRadius)
    else LScaledYRadius := LScaledYRadius* AScale;
    var LMaxRadius := Min(LRect.Width / 2, LRect.Height / 2);
    LScaledXRadius := min(LScaledXRadius, LMaxRadius);
    LScaledYRadius := min(LScaledYRadius, LMaxRadius);

    // use drawRoundRect
    if (not aForceDrawPath) and
       (not aClipPath) and
       ((compareValue(LScaledXRadius, 0, TEpsilon.Position) > 0) and
        (compareValue(LScaledYRadius, 0, TEpsilon.position) > 0)) and
       (ACorners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
       (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      var LJRect := TJRectf.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);
      aCanvas.drawRoundRect(
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
             (ACorners=[])) and
            (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      aCanvas.drawRect(
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
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if (aDrawOnlyBorder) or
           ((LRectIsEqualsToStrokeRect) and
            (LShadowcolor = TalphaColorRec.Null) and
            (ACorners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
            (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right])) then begin
          LHalfStrokeThickness := LScaledStrokeThickness / 2;
          LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
        end
        else if (LRectIsEqualsToStrokeRect) and
                (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) and
                (ACorners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
                (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
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
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := ACorners
      else LCorners := [];
      //--
      If aDrawOnlyBorder then begin
        If (TCorner.TopRight in LCorners)    and (not (TSide.Top in ASides))    and (not (Tside.Right in ASides)) then LCorners := LCorners - [TCorner.TopRight];
        If (TCorner.TopLeft in LCorners)     and (not (TSide.Top in ASides))    and (not (Tside.Left in ASides))  then LCorners := LCorners - [TCorner.TopLeft];
        If (TCorner.BottomRight in LCorners) and (not (TSide.Bottom in ASides)) and (not (Tside.Right in ASides)) then LCorners := LCorners - [TCorner.BottomRight];
        If (TCorner.BottomLeft in LCorners)  and (not (TSide.Bottom in ASides)) and (not (Tside.Left in ASides))  then LCorners := LCorners - [TCorner.BottomLeft];
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
           (TSide.right in ASides) then begin
          LPath.rLineTo(0, -LYradius);
          if (aDrawOnlyBorder) and
             (TSide.right in ASides) and
             (not (TSide.top in ASides)) then begin
            LPath.rLineTo(0, -LHalfStrokeThickness);
            LPath.rMoveTo(0, LHalfStrokeThickness);
          end;
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in ASides) then begin
          if (aDrawOnlyBorder) and
             (TSide.top in ASides) and
             (not (TSide.right in ASides)) then begin
            LPath.rMoveTo(LHalfStrokeThickness, 0);
            LPath.rLineTo(-LHalfStrokeThickness, 0);
          end;
          LPath.rLineTo(-LXRadius,0);
        end
        else LPath.rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in ASides) then LPath.rLineTo(-LWidthMinusCorners, 0)
      else LPath.rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        LPath.rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in ASides) then begin
          LPath.rLineTo(-LXRadius, 0);
          if (aDrawOnlyBorder) and
             (TSide.top in ASides) and
             (not (TSide.left in ASides)) then begin
            LPath.rLineTo(-LHalfStrokeThickness, 0);
            LPath.rMoveTo(LHalfStrokeThickness, 0);
          end;
        end
        else LPath.rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in ASides) then begin
          if (aDrawOnlyBorder) and
             (TSide.left in ASides) and
             (not (TSide.top in ASides)) then begin
            LPath.rMoveTo(0, -LHalfStrokeThickness);
            LPath.rLineTo(0, LHalfStrokeThickness);
          end;
          LPath.rLineTo(0,LYradius);
        end
        else LPath.rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in ASides) then LPath.rLineTo(0, LHeightMinusCorners)
      else LPath.rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        LPath.rQuadTo(0, LYradius, LXRadius, LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in ASides) then begin
          LPath.rLineTo(0, LYradius);
          if (aDrawOnlyBorder) and
             (TSide.left in ASides) and
             (not (TSide.bottom in ASides)) then begin
            LPath.rLineTo(0, LHalfStrokeThickness);
            LPath.rMoveTo(0, -LHalfStrokeThickness);
          end;
        end
        else LPath.rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in ASides) then begin
          if (aDrawOnlyBorder) and
             (TSide.bottom in ASides) and
             (not (TSide.left in ASides)) then begin
            LPath.rMoveTo(-LHalfStrokeThickness, 0);
            LPath.rLineTo(LHalfStrokeThickness, 0);
          end;
          LPath.rLineTo(LXRadius,0);
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in ASides) then LPath.rLineTo(LWidthMinusCorners, 0)
      else LPath.rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        LPath.rQuadTo(LXRadius, 0, LXRadius, -LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in ASides) then begin
          LPath.rLineTo(LXRadius,0);
          if (aDrawOnlyBorder) and
             (TSide.bottom in ASides) and
             (not (TSide.right in ASides)) then begin
            LPath.rLineTo(LHalfStrokeThickness, 0);
            LPath.rMoveTo(-LHalfStrokeThickness, 0);
          end;
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in ASides) then begin
          if (aDrawOnlyBorder) and
             (TSide.right in ASides) and
             (not (TSide.bottom in ASides)) then begin
            LPath.rMoveTo(0, LHalfStrokeThickness);
            LPath.rLineTo(0, -LHalfStrokeThickness);
          end;
          LPath.rLineTo(0,-LYradius);
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in ASides) then LPath.rLineTo(0, -LHeightMinusCorners)
      else LPath.rMoveTo(0, -LHeightMinusCorners);
      //-----
      if (TSide.right in ASides) and
         (TSide.top in ASides) then LPath.close;


      if aPaint <> nil then aCanvas.drawPath(LPath,aPaint);
      if aClipPath then begin
        aCanvas.save;
        aCanvas.clipPath(LPath);
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
              const aCanvas: CGContextRef;
              const aGridHeight: Integer;
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean;
              const aClipPath: Boolean);

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
    var LScaledXRadius: Single := AXRadius;
    var LScaledYRadius: Single := AYRadius;
    if LScaledXRadius < 0 then LScaledXRadius := (LRect.Width/100)*abs(LScaledXRadius)
    else LScaledXRadius := LScaledXRadius * AScale;
    if LScaledYRadius < 0 then LScaledYRadius := (LRect.Height/100)*abs(LScaledYRadius)
    else LScaledYRadius := LScaledYRadius* AScale;
    var LMaxRadius := Min(LRect.Width / 2, LRect.Height / 2);
    LScaledXRadius := min(LScaledXRadius, LMaxRadius);
    LScaledYRadius := min(LScaledYRadius, LMaxRadius);

    // use drawRect
    if ((compareValue(LScaledXRadius, 0, TEpsilon.Position) = 0) or
        (compareValue(LScaledYRadius, 0, TEpsilon.position) = 0) or
        (ACorners=[])) and
       (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
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
      CGContextClosePath(ACanvas);
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
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if (aDrawOnlyBorder) or
           ((LRectIsEqualsToStrokeRect) and
            (LShadowcolor = TalphaColorRec.Null) and
            (ACorners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
            (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right])) then begin
          LHalfStrokeThickness := LScaledStrokeThickness / 2;
          LRect.Inflate(-LHalfStrokeThickness, -LHalfStrokeThickness);
        end
        else if (LRectIsEqualsToStrokeRect) and
                (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) and
                (ACorners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
                (ASides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
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
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := ACorners
      else LCorners := [];
      //--
      If aDrawOnlyBorder then begin
        If (TCorner.TopRight in LCorners)    and (not (TSide.Top in ASides))    and (not (Tside.Right in ASides)) then LCorners := LCorners - [TCorner.TopRight];
        If (TCorner.TopLeft in LCorners)     and (not (TSide.Top in ASides))    and (not (Tside.Left in ASides))  then LCorners := LCorners - [TCorner.TopLeft];
        If (TCorner.BottomRight in LCorners) and (not (TSide.Bottom in ASides)) and (not (Tside.Right in ASides)) then LCorners := LCorners - [TCorner.BottomRight];
        If (TCorner.BottomLeft in LCorners)  and (not (TSide.Bottom in ASides)) and (not (Tside.Left in ASides))  then LCorners := LCorners - [TCorner.BottomLeft];
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
           (TSide.right in ASides) then begin
          _rLineTo(0, -LYradius);
          if (aDrawOnlyBorder) and
             (TSide.right in ASides) and
             (not (TSide.top in ASides)) then begin
            _rLineTo(0, -LHalfStrokeThickness);
            _rMoveTo(0, LHalfStrokeThickness);
          end;
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in ASides) then begin
          if (aDrawOnlyBorder) and
             (TSide.top in ASides) and
             (not (TSide.right in ASides)) then begin
            _rMoveTo(LHalfStrokeThickness, 0);
            _rLineTo(-LHalfStrokeThickness, 0);
          end;
          _rLineTo(-LXRadius,0);
        end
        else _rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in ASides) then _rLineTo(-LWidthMinusCorners, 0)
      else _rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        _rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in ASides) then begin
          _rLineTo(-LXRadius, 0);
          if (aDrawOnlyBorder) and
             (TSide.top in ASides) and
             (not (TSide.left in ASides)) then begin
            _rLineTo(-LHalfStrokeThickness, 0);
            _rMoveTo(LHalfStrokeThickness, 0);
          end;
        end
        else _rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in ASides) then begin
          if (aDrawOnlyBorder) and
             (TSide.left in ASides) and
             (not (TSide.top in ASides)) then begin
            _rMoveTo(0, -LHalfStrokeThickness);
            _rLineTo(0, LHalfStrokeThickness);
          end;
          _rLineTo(0,LYradius);
        end
        else _rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in ASides) then _rLineTo(0, LHeightMinusCorners)
      else _rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        _rQuadTo(0, LYradius, LXRadius, LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in ASides) then begin
          _rLineTo(0, LYradius);
          if (aDrawOnlyBorder) and
             (TSide.left in ASides) and
             (not (TSide.bottom in ASides)) then begin
            _rLineTo(0, LHalfStrokeThickness);
            _rMoveTo(0, -LHalfStrokeThickness);
          end;
        end
        else _rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in ASides) then begin
          if (aDrawOnlyBorder) and
             (TSide.bottom in ASides) and
             (not (TSide.left in ASides)) then begin
            _rMoveTo(-LHalfStrokeThickness, 0);
            _rLineTo(LHalfStrokeThickness, 0);
          end;
          _rLineTo(LXRadius,0);
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in ASides) then _rLineTo(LWidthMinusCorners, 0)
      else _rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        _rQuadTo(LXRadius, 0, LXRadius, -LYradius);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in ASides) then begin
          _rLineTo(LXRadius,0);
          if (aDrawOnlyBorder) and
             (TSide.bottom in ASides) and
             (not (TSide.right in ASides)) then begin
            _rLineTo(LHalfStrokeThickness, 0);
            _rMoveTo(-LHalfStrokeThickness, 0);
          end;
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in ASides) then begin
          if (aDrawOnlyBorder) and
             (TSide.right in ASides) and
             (not (TSide.bottom in ASides)) then begin
            _rMoveTo(0, LHalfStrokeThickness);
            _rLineTo(0, -LHalfStrokeThickness);
          end;
          _rLineTo(0,-LYradius);
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in ASides) then _rLineTo(0, -LHeightMinusCorners)
      else _rMoveTo(0, -LHeightMinusCorners);
      //-----
      if (TSide.right in ASides) and
         (TSide.top in ASides) then CGContextClosePath(ACanvas);


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
  procedure _SetShadow(const aCanvas: CGContextRef);
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
  procedure _ClearShadow(const aCanvas: CGContextRef);
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

    var LScaledStateLayerXRadius: Single := AStateLayerXRadius;
    var LScaledStateLayerYRadius: Single := AStateLayerYRadius;
    if LScaledStateLayerXRadius > 0 then LScaledStateLayerXRadius := LScaledStateLayerXRadius * AScale;
    if LScaledStateLayerYRadius > 0 then LScaledStateLayerYRadius := LScaledStateLayerYRadius* AScale;

    ALDrawRectangle(
      ACanvas, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      AAlignToPixel, // const AAlignToPixel: Boolean;
      LScaledStateLayerDstRect, // const ADstRect: TrectF;
      AStateLayerOpacity, // const AOpacity: Single;
      LStateLayerColor, // const AFillColor: TAlphaColor;
      TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
      [], // const AFillGradientColors: TArray<TAlphaColor>;
      [], // const AFillGradientOffsets: TArray<Single>;
      TPointF.Zero, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      TPointF.Zero, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      '', // const AFillResourceName: String;
      TALImageWrapMode.Fit, // Const AFillWrapMode: TALImageWrapMode;
      TRectF.Empty, // Const AFillBackgroundMarginsRect: TRectF;
      TRectF.Empty, // Const AFillImageMarginsRect: TRectF;
      0, // const AStateLayerOpacity: Single;
      TAlphaColors.Null, // const AStateLayerColor: TAlphaColor;
      TRectF.Empty, // Const AStateLayerMarginsRect: TRectF;
      0, // const AStateLayerXRadius: Single;
      0, // const AStateLayerYRadius: Single;
      False, // const ADrawStateLayerOnTop: Boolean;
      TAlphaColors.Null, // const AStrokeColor: TalphaColor;
      0, // const AStrokeThickness: Single;
      TAlphaColors.Null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
      0, // const AShadowBlur: Single;
      0, // onst AShadowOffsetX: Single;
      0, // const AShadowOffsetY: Single;
      AllSides, // const ASides: TSides;
      AllCorners, // const ACorners: TCorners;
      LScaledStateLayerXRadius, // const AXRadius: Single;
      LScaledStateLayerYRadius); // const AYRadius: Single)
  end;
  {$ENDREGION}

begin

  if CompareValue(AOpacity, 0, TEpsilon.Scale) <= 0 then exit;
  //--
  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AAlignToPixel then ALExtractMatrixFromCanvas(Acanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;
  //--
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}
  var LGridHeight := CGBitmapContextGetHeight(ACanvas);
  {$ENDIF}
  //--
  LScaledDstRect := ADstRect;
  LScaledDstRect.Top := LScaledDstRect.Top * AScale;
  LScaledDstRect.right := LScaledDstRect.right * AScale;
  LScaledDstRect.left := LScaledDstRect.left * AScale;
  LScaledDstRect.bottom := LScaledDstRect.bottom * AScale;
  if AAlignToPixel then
    LScaledDstRect := ALAlignToPixelRound(LScaledDstRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);
  //--
  LScaledStrokeThickness := AStrokeThickness * AScale;
  if AAlignToPixel then
    LScaledStrokeThickness := ALAlignDimensionToPixelRound(LScaledStrokeThickness, LCanvasScale, TEpsilon.Position);
  //--
  {$IF (defined(ANDROID)) or (defined(ALAppleOS)) or (defined(ALSkiaEngine))}
  LScaledShadowBlur := AShadowBlur * AScale;
  LScaledShadowOffsetX := AShadowOffsetX * AScale;
  LScaledShadowOffsetY := AShadowOffsetY * AScale;
  if AAlignToPixel then begin
    LScaledShadowBlur := ALAlignDimensionToPixelRound(LScaledShadowBlur, LCanvasScale, Tepsilon.Vector);
    LScaledShadowOffsetX := ALAlignDimensionToPixelRound(LScaledShadowOffsetX, LCanvasScale, TEpsilon.Position);
    LScaledShadowOffsetY := ALAlignDimensionToPixelRound(LScaledShadowOffsetY, LCanvasScale, TEpsilon.Position);
  end;
  LShadowColor := AShadowColor;
  if CompareValue(LScaledShadowBlur, 0, TEpsilon.position) <= 0 then
    LShadowColor := TAlphaColors.Null;
  {$ENDIF}
  //--
  LStrokeColor := AStrokeColor;
  if CompareValue(LScaledStrokeThickness, 0, TEpsilon.position) <= 0 then
    LStrokeColor := TAlphaColors.Null;
  //--
  if length(AFillGradientColors) = 1 then
    raise Exception.Create('Invalid gradient: A gradient requires at least two colors');
  var LFillGradientOffsets := AFillGradientOffsets;
  if (length(LFillGradientOffsets) = 0) and (length(AFillGradientColors) > 0) then begin
    setlength(LFillGradientOffsets, length(AFillGradientColors));
    for Var I := 0 to length(AFillGradientColors) - 1 do
      LFillGradientOffsets[i] := I * (1 / (length(AFillGradientColors) - 1));
  end
  else if (length(LFillGradientOffsets) <> length(AFillGradientColors)) then
    raise Exception.Create('Invalid gradient: The number of gradient offsets does not match the number of gradient colors');
  //--
  var LScaledFillGradientStartPoint := AFillGradientStartPoint;
  LScaledFillGradientStartPoint.X := LScaledFillGradientStartPoint.X * AScale;
  LScaledFillGradientStartPoint.Y := LScaledFillGradientStartPoint.Y * AScale;
  var LScaledFillGradientEndPoint := AFillGradientEndPoint;
  LScaledFillGradientEndPoint.X := LScaledFillGradientEndPoint.X * AScale;
  LScaledFillGradientEndPoint.Y := LScaledFillGradientEndPoint.Y * AScale;
  //--
  var LScaledFillBackgroundMarginsRect: TRectF;
  if (AFillColor <> TalphaColorRec.Null) or
     (length(AFillGradientColors) > 0) then begin
    LScaledFillBackgroundMarginsRect := AFillBackgroundMarginsRect;
    LScaledFillBackgroundMarginsRect.Top := LScaledFillBackgroundMarginsRect.Top * AScale;
    LScaledFillBackgroundMarginsRect.right := LScaledFillBackgroundMarginsRect.right * AScale;
    LScaledFillBackgroundMarginsRect.left := LScaledFillBackgroundMarginsRect.left * AScale;
    LScaledFillBackgroundMarginsRect.bottom := LScaledFillBackgroundMarginsRect.bottom * AScale;
    if AAlignToPixel then
      LScaledFillBackgroundMarginsRect := ALAlignEdgesToPixelRound(LScaledFillBackgroundMarginsRect, LCanvasScale, TEpsilon.Position);
  end
  else
    LScaledFillBackgroundMarginsRect := TRectF.Empty;
  //--
  var LScaledFillImageMarginsRect: TRectF;
  if (AFillResourceName <> '') then begin
    LScaledFillImageMarginsRect := AFillImageMarginsRect;
    LScaledFillImageMarginsRect.Top := LScaledFillImageMarginsRect.Top * AScale;
    LScaledFillImageMarginsRect.right := LScaledFillImageMarginsRect.right * AScale;
    LScaledFillImageMarginsRect.left := LScaledFillImageMarginsRect.left * AScale;
    LScaledFillImageMarginsRect.bottom := LScaledFillImageMarginsRect.bottom * AScale;
    if AAlignToPixel then
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
  var LFillColor := AFillColor;
  LFillGradientColors := AFillGradientColors;
  var LFillResourceName := AFillResourceName;
  if LScaledBackgroundDstRect.IsEmpty then begin
    LFillColor := TALphaColors.Null;
    setlength(LFillGradientColors, 0);
    setlength(LFillGradientOffsets, 0);
  end;
  if LScaledImageDstRect.IsEmpty then
    LFillResourceName := '';
  //--
  var LScaledStateLayerMarginsRect: TRectF;
  if (AStateLayerColor <> TalphaColorRec.Null) and
     (CompareValue(AStatelayerOpacity, 0, TEpsilon.Scale) > 0) then begin
    LScaledStateLayerMarginsRect := AStateLayerMarginsRect;
    LScaledStateLayerMarginsRect.Top := LScaledStateLayerMarginsRect.Top * AScale;
    LScaledStateLayerMarginsRect.right := LScaledStateLayerMarginsRect.right * AScale;
    LScaledStateLayerMarginsRect.left := LScaledStateLayerMarginsRect.left * AScale;
    LScaledStateLayerMarginsRect.bottom := LScaledStateLayerMarginsRect.bottom * AScale;
    if AAlignToPixel then
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
  LStateLayerColor := AStateLayerColor;
  if (LScaledStateLayerDstRect.IsEmpty) or
     (CompareValue(AStatelayerOpacity, 0, TEpsilon.Scale) <= 0) then
    LStateLayerColor := TALphaColors.Null;
  //--
  if (LStateLayerColor <> TALphaColors.Null) and
     (CompareValue(AStatelayerOpacity, 0, TEpsilon.Scale) > 0) and
     (LScaledStateLayerDstRect.EqualsTo(LScaledBackgroundDstRect)) and
     (LScaledStateLayerDstRect.EqualsTo(LScaledDstRect)) and
     (sameValue(AStateLayerXRadius, AXRadius, TEpsilon.Vector)) and
     (sameValue(AStateLayerYRadius, AYRadius, TEpsilon.Vector)) then begin
    LFillColor := ALblendColor(LfillColor, LStateLayerColor, AStatelayerOpacity);
    if (ADrawStateLayerOnTop) and
       (LStrokeColor <> TAlphaColors.Null) then
      LStrokeColor := ALblendColor(LStrokeColor, LStateLayerColor, AStatelayerOpacity);
    LStateLayerColor := TALphaColors.Null;
  end;
  //--
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}
  LPathClipped := False;
  {$ENDIF}

  if SameValue(LScaledDstRect.Width, LScaledDstRect.Height, TEpsilon.position) and
     SameValue(LScaledBackgroundDstRect.Width, LScaledBackgroundDstRect.Height, TEpsilon.position) and
     SameValue(LScaledImageDstRect.Width, LScaledImageDstRect.Height, TEpsilon.position) and
     SameValue(AXRadius, -50, TEpsilon.position) and
     SameValue(AYRadius, -50, TEpsilon.position) then begin
    ALDrawCircle(
      ACanvas, // const ACanvas: TALCanvas;
      AScale, // const AScale: Single;
      AAlignToPixel, // const AAlignToPixel: Boolean;
      ADstRect, // const ADstRect: TrectF;
      AOpacity, //const AOpacity: Single;
      AFillColor, // const AFillColor: TAlphaColor;
      AFillGradientStyle, // const AFillGradientStyle: TGradientStyle;
      AFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
      AFillGradientOffsets, // const AFillGradientOffsets: TArray<Single>;
      AFillGradientStartPoint, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      AFillGradientEndPoint, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      AFillResourceName, // const AFillResourceName: String;
      AFillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
      AFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
      AFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
      AStateLayerOpacity, // const AStateLayerOpacity: Single;
      AStateLayerColor, // const AStateLayerColor: TAlphaColor;
      AStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
      AStateLayerXRadius, // const AStateLayerXRadius: Single;
      AStateLayerYRadius, // const AStateLayerYRadius: Single;
      ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
      AStrokeColor, // const AStrokeColor: TalphaColor;
      AStrokeThickness, // const AStrokeThickness: Single;
      AShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
      AShadowBlur, // const AShadowBlur: Single;
      AShadowOffsetX, // const AShadowOffsetX: Single;
      AShadowOffsetY); // const AShadowOffsetY: Single)
    exit;
  end;

  {$REGION 'SKIA'}
  {$IF defined(ALSkiaEngine)}

  // Create the alpha layer
  if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        AStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(ACanvas, LLayerRect, AOpacity);
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
         (LFillResourceName <> '') or
         (LShadowColor <> TalphaColorRec.Null) then begin

        // FILL_SK_PAINTSTYLE
        sk4d_paint_set_style(LPaint, sk_paintstyle_t.FILL_SK_PAINTSTYLE);

        //Fill with bitmap
        //if AFill.Kind = TALBrushKind.Bitmap then begin
        //  if AFill.Bitmap.Bitmap.HandleAllocated then begin
        //    var LBitmapData: TBitmapData;
        //    if AFill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then begin
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
        //        AFill.Bitmap.Bitmap.Unmap(LBitmapData);
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
           ((LFillResourceName = '') or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
            (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
            (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
            (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

          // First pass draw the shadow
          sk4d_paint_set_color(LPaint, ALSetColorAlpha(LFillColor, 1{AOpacity}));
          _SetShadow(LPaint);
          _DrawRect(ACanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder});
          _ClearShadow(LPaint);

          // Second pass fill the rect
          var LBlender := ALSkCheckHandle(
                            sk4d_blender_make_mode(
                              sk_blendmode_t.SRC_SK_BLENDMODE));
          try
            sk4d_paint_set_blender(LPaint, LBlender);
            sk4d_paint_set_color(LPaint, LFillColor);
            _DrawRect(ACanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder});
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
          _DrawRect(ACanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder});
          _ClearShadow(LPaint);
          LDrawnWithSolidColor := True;
        end;

        // Fill with gradient
        if length(LFillGradientColors) > 0 then begin
          case AFillGradientStyle of
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
          if not LDrawnWithSolidColor then _SetShadow(LPaint);
          _DrawRect(ACanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder});
          if not LDrawnWithSolidColor then _ClearShadow(LPaint);
          sk4d_paint_set_shader(LPaint, 0);
          LDrawnWithSolidColor := True;
        end;

        // Draw the StateLayer
        if not ADrawStateLayerOnTop then
          _DrawStateLayer;

        // Fill with image
        if LFillResourceName <> '' then begin
          var LImage: sk_image_t;
          {$IFDEF ALDPK}
          try
          {$ENDIF}
            var LFileName := ALGetResourceFilename(LFillResourceName);
            if LFileName <> '' then LImage := ALLoadFromFileAndWrapToSkImage(LFileName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height)
            else LImage := {$IFDEF ALDPK}0{$ELSE}ALLoadFromResourceAndWrapToSkImage(LFillResourceName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height){$ENDIF};
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
              sk4d_paint_set_color(LPaint, $FFFFFFFF);
              if not LDrawnWithSolidColor then _SetShadow(LPaint);
              _DrawRect(ACanvas, LPaint, LScaledImageDstRect, false{aDrawOnlyBorder});
              if not LDrawnWithSolidColor then _ClearShadow(LPaint);
              sk4d_paint_set_shader(LPaint, 0);

            finally
              sk4d_refcnt_unref(LImage);
            end;
        end;

      end

      // Draw the StateLayer
      else if not ADrawStateLayerOnTop then
        _DrawStateLayer;

      // Stroke the rectangle
      if LStrokeColor <> TalphaColorRec.Null then begin
        sk4d_paint_set_style(LPaint, sk_paintstyle_t.STROKE_SK_PAINTSTYLE);
        sk4d_paint_set_stroke_width(LPaint, LScaledStrokeThickness);
        sk4d_paint_set_color(LPaint, LStrokeColor);
        _DrawRect(ACanvas, LPaint, LScaledDstRect, true{aDrawOnlyBorder});
      end;

      // Draw the StateLayer
      if ADrawStateLayerOnTop then
        _DrawStateLayer;

    finally
      sk4d_paint_destroy(LPaint);
    end;

  finally
    // Remove the alpha layer
    if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(ACanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ANDROID'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

  // Create the alpha layer
  if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        AStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(ACanvas, LLayerRect, AOpacity);
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
       (LFillResourceName <> '') or
       (LShadowColor <> TalphaColorRec.Null) then begin

      //init LPaint
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LScaledDstRect + StrokeThickness :( this result&ing in border if the fill is for exemple black and border white

      //fill with bitmap
      //if AFill.Kind = TALBrushKind.Bitmap then begin
      //  if not AFill.Bitmap.Bitmap.IsEmpty then begin
      //    if AFill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
      //      //--
      //      var LTmpBitmap := TJBitmap.JavaClass.createBitmap(AFill.Bitmap.Bitmap.Width, AFill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);
      //      //--
      //      var LPixelBuffer: Pointer;
      //      var LBitmapInfo: AndroidBitmapInfo;
      //      FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
      //      if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LBitmapInfo) = 0) and
      //         (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LPixelBuffer) = 0) then
      //      try
      //        var LBitmapData: TBitmapData;
      //        if AFill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
      //        try
      //          ALMove(LBitmapData.Data^, LPixelBuffer^, LBitmapData.Pitch * LBitmapData.Height);
      //        finally
      //          AFill.Bitmap.Bitmap.Unmap(LBitmapData);
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
         ((LFillResourceName = '') or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
          (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
          (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
          (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

        // First pass draw the shadow
        LPaint.setColor(integer(ALSetColorAlpha(LFillColor, 1{AOpacity})));
        _SetShadow(LPaint);
        _DrawRect(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, false{aClipPath});
        _ClearShadow(LPaint);

        // Second pass fill the rect
        var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC);
        LPaint.setXfermode(LPorterDuffXfermode);
        LPaint.setColor(integer(LFillColor));
        _DrawRect(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, (LFillResourceName <> '') and (length(LFillGradientColors) = 0) and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath});
        LPaint.setXfermode(nil);
        LPorterDuffXfermode := nil;
        LDrawnWithSolidColor := True;

      end

      // Fill with solid color
      else if (LFillColor <> TalphaColors.Null) then begin
        LPaint.setColor(integer(LFillColor));
        _SetShadow(LPaint);
        _DrawRect(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, (LFillResourceName <> '') and (length(LFillGradientColors) = 0) and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath});
        _ClearShadow(LPaint);
        LDrawnWithSolidColor := True;
      end;

      //fill with gradient
      if length(LFillGradientColors) > 0 then begin

        // First pass draw the shadow if not already drawn
        // We must do this because else the shadow will be drawn with
        // the color of the gradient :(
        if not LDrawnWithSolidColor then begin
          LPaint.setColor(integer(ALSetColorAlpha(LFillColor, 1{AOpacity})));
          _SetShadow(LPaint);
          _DrawRect(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, false{aClipPath});
          _ClearShadow(LPaint);
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
          case AFillGradientStyle of
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
          LPaint.setShader(LShader);
          _DrawRect(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, (LFillResourceName <> '') and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath});
          LPaint.setShader(nil);
          LShader := nil;
          LDrawnWithSolidColor := True;
        finally
          ALfreeandNil(LJColors);
          ALfreeandNil(LJOffsets);
        end;
      end;

      // Draw the StateLayer
      if not ADrawStateLayerOnTop then
        _DrawStateLayer;

      // Fill with image
      if LFillResourceName <> '' then begin
        var LBitmap: JBitmap;
        var LFileName := ALGetResourceFilename(LFillResourceName);
        if LFileName <> '' then LBitmap := ALLoadFromFileAndWrapToJBitmap(LFileName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height)
        else LBitmap := ALLoadFromResourceAndWrapToJBitmap(LFillResourceName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height);
        try

          // On android the bitmap is drawed with the opacity of the paint color
          // so set the color to black to make the bitmap fully opaque
          LPaint.setColor(integer(TAlphaColors.Black));

          // The shadow is made directly on the bitmap
          if (not LDrawnWithSolidColor) and (LShadowColor <> TalphaColorRec.Null) then begin

            // Their is corners so remove them from the LBitmap
            if (compareValue(AXRadius, 0, TEpsilon.Position) <> 0) or
               (compareValue(AYRadius, 0, TEpsilon.position) <> 0) then begin
              // Draw the shape of the rect in LDestBitmap
              var LDestRect := TRectF.Create(0,0,LScaledImageDstRect.Width,LScaledImageDstRect.height);
              var LDestBitmap := TJBitmap.JavaClass.createBitmap(Round(LDestRect.Width), round(LDestRect.Height), TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);
              var LDestCanvas := TJCanvas.JavaClass.init(LDestBitmap);
              _DrawRect(LDestCanvas, LPaint, LDestRect, false{aDrawOnlyBorder}, False{aForceDrawPath}, False{aClipPath});
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
            var LBlurMaskFilter := TJBlurMaskFilter.JavaClass.init(AShadowblur*2.5, TJBlurMaskFilter_Blur.JavaClass.NORMAL);
            LPaint.setColor(integer(LShadowColor));
            LPaint.setMaskFilter(LBlurMaskFilter);
            aCanvas.drawBitmap(LBitmap.extractAlpha, LJSrcRect, LJDestRectf, LPaint);
            LPaint.setMaskFilter(nil);
            LDestRect.Offset(-LScaledShadowOffsetX, -LScaledShadowOffsetY);
            LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
            LPaint.setColor(integer(TAlphaColors.Black));
            aCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);

          end

          // The shadow is made on the rectangle shape
          else begin
            If not LPathClipped then _DrawRect(aCanvas, nil{APaint}, LScaledImageDstRect, false{aDrawOnlyBorder}, true{aForceDrawPath}, true{aClipPath});
            var LDestRect := TrectF.Create(0,0, LBitmap.getWidth, LBitmap.getheight).CenterAt(LScaledImageDstRect);
            var LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
            var LJSrcRect := TJRect.JavaClass.init(0, 0, LBitmap.getWidth, LBitmap.getheight);
            aCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
            aCanvas.restore;
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
    else if not ADrawStateLayerOnTop then
      _DrawStateLayer;

    //stroke the rectangle
    if LStrokeColor <> TalphaColorRec.Null then begin
      LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
      LPaint.setStrokeWidth(LScaledStrokeThickness);
      LPaint.setColor(integer(LStrokeColor));
      _DrawRect(aCanvas, LPaint, LScaledDstRect, true{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, False{aClipPath});
    end;

    // Draw the StateLayer
    if ADrawStateLayerOnTop then
      _DrawStateLayer;

    //free the paint and the canvas
    LPaint := nil;

  finally
    // Remove the alpha layer
    if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(ACanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  // Create the alpha layer
  if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        AStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(ACanvas, LLayerRect, AOpacity);
  end;
  try

    // Fill the rectangle
    if (LFillColor <> TalphaColorRec.Null) or
       (length(LFillGradientColors) > 0) or
       (LFillResourceName <> '') or
       (LShadowColor <> TalphaColorRec.Null) then begin

      //fill with bitmap
      //if AFill.Kind = TALBrushKind.Bitmap then begin
      //  if not AFill.Bitmap.Bitmap.IsEmpty then begin
      //    if AFill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
      //      var LBitmapData: TBitmapData;
      //      if AFill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
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
      //        AFill.Bitmap.Bitmap.Unmap(LBitmapData);
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
         ((LFillResourceName = '') or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
          (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
          (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
          (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

        // First pass draw the shadow
        var LFillColorF := TAlphaColorCGFloat.Create(LFillColor);
        CGContextSetRGBFillColor(ACanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, 1{A});
        _SetShadow(aCanvas);
        _DrawRect(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath});
        _ClearShadow(aCanvas);

        // Second pass fill the rect
        CGContextSetBlendMode(ACanvas, kCGBlendModeCopy);
        CGContextSetRGBFillColor(ACanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, LFillColorF.A);
        _DrawRect(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath});
        CGContextSetBlendMode(ACanvas, kCGBlendModeNormal);
        LDrawnWithSolidColor := True;

      end

      // Fill with solid color
      else if (LFillColor <> TalphaColors.Null) then begin
        var LFillColorF := TAlphaColorCGFloat.Create(LFillColor);
        CGContextSetRGBFillColor(ACanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, LFillColorF.A);
        _SetShadow(aCanvas);
        _DrawRect(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath});
        _ClearShadow(aCanvas);
        LDrawnWithSolidColor := True;
      end;

      //fill with gradient
      if length(LFillGradientColors) > 0 then begin

        // First pass draw the shadow if not already drawn
        // We must do this because else the shadow will not be drawn
        if not LDrawnWithSolidColor then begin
          var LFillColorF := TAlphaColorCGFloat.Create(LFillColor);
          CGContextSetRGBFillColor(ACanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, 1{A});
          _SetShadow(aCanvas);
          _DrawRect(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath});
          _ClearShadow(aCanvas);
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
            case AFillGradientStyle of
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
              _DrawRect(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, true{aClipPath});
              CGContextDrawShading(ACanvas, LShading);
              CGContextRestoreGState(ACanvas);
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
      if not ADrawStateLayerOnTop then
        _DrawStateLayer;

      // Fill with image
      if LFillResourceName <> '' then begin
        var LImage: CGImageRef;
        var LFileName := ALGetResourceFilename(LFillResourceName);
        if LFileName <> '' then LImage := ALLoadFromFileAndWrapToCGImageRef(LFileName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height)
        else LImage := ALLoadFromResourceAndWrapToCGImageRef(LFillResourceName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height);
        try

          // The shadow is made directly on the bitmap
          if (not LDrawnWithSolidColor) and (LShadowColor <> TalphaColorRec.Null) then begin

            // Their is corners so remove them from the LBitmap
            if (compareValue(AXRadius, 0, TEpsilon.Position) <> 0) or
               (compareValue(AYRadius, 0, TEpsilon.position) <> 0) then begin
              var LTmpRect := TRectF.Create(0,0,LScaledImageDstRect.Width,LScaledImageDstRect.height).Round;
              var LTmpCGContextRef := ALCreateCGContextRef(LTmpRect.Width, LTmpRect.Height);
              try
                _DrawRect(LTmpCGContextRef, LTmpRect.Height, LTmpRect, false{aDrawOnlyBorder}, true{aClipPath});
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

            _SetShadow(aCanvas);
            var LDestRect := TrectF.Create(0,0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)).CenterAt(LScaledImageDstRect);
            CGContextDrawImage(
              ACanvas, // c: The graphics context in which to draw the image.
              ALLowerLeftCGRect(
                LDestRect.TopLeft,
                LDestRect.Width,
                LDestRect.Height,
                LGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              LImage); // image The image to draw.
             _ClearShadow(aCanvas);

          end

          // The shadow is made on the rectangle shape
          else begin
            _DrawRect(aCanvas, LGridHeight, LScaledImageDstRect, false{aDrawOnlyBorder}, true{aClipPath});
            var LDestRect := TrectF.Create(0,0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)).CenterAt(LScaledImageDstRect);
            CGContextDrawImage(
              ACanvas, // c: The graphics context in which to draw the image.
              ALLowerLeftCGRect(
                LDestRect.TopLeft,
                LDestRect.Width,
                LDestRect.Height,
                LGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              LImage); // image The image to draw.
            CGContextRestoreGState(ACanvas);
          end;

        finally
          CGImageRelease(LImage);
        end;
      end;

    end

    // Draw the StateLayer
    else if not ADrawStateLayerOnTop then
      _DrawStateLayer;

    //stroke the rectangle
    if LStrokeColor <> TalphaColorRec.Null then begin
      CGContextSetLineWidth(ACanvas, LScaledStrokeThickness);
      var LStrokeColorF := TAlphaColorCGFloat.Create(LStrokeColor);
      CGContextSetRGBStrokeColor(ACanvas, LStrokeColorF.R, LStrokeColorF.G, LStrokeColorF.B, LStrokeColorF.A);
      _DrawRect(aCanvas, LGridHeight, LScaledDstRect, True{aDrawOnlyBorder}, false);
    end;

    // Draw the StateLayer
    if ADrawStateLayerOnTop then
      _DrawStateLayer;

  finally
    // Remove the alpha layer
    if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(ACanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  var LSaveState := ACanvas.SaveState;
  try

    if LFillColor <> TAlphaColorRec.Null then begin
      ACanvas.Fill.Kind := TBrushKind.Solid;
      ACanvas.Fill.Color := LFillColor;
    end
    else ACanvas.Fill.Kind := TBrushKind.None;
    If LStrokeColor <> TalphaColorRec.Null then begin
      ACanvas.Stroke.Kind := TBrushKind.Solid;
      ACanvas.Stroke.Color := LStrokeColor;
      ACanvas.Stroke.Thickness := LScaledStrokeThickness;
    end
    else ACanvas.Stroke.Kind := TBrushKind.None;

    if LStrokeColor <> TalphaColorRec.Null then
      LScaledDstRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2));

    var LScaledXRadius: Single := AXRadius;
    var LScaledYRadius: Single := AYRadius;
    if LScaledXRadius < 0 then LScaledXRadius := (LScaledDstRect.Width/100)*abs(LScaledXRadius)
    else LScaledXRadius := LScaledXRadius * AScale;
    if LScaledYRadius < 0 then LScaledYRadius := (LScaledDstRect.Height/100)*abs(LScaledYRadius)
    else LScaledYRadius := LScaledYRadius* AScale;
    var LMaxRadius := Min(LScaledDstRect.Width / 2, LScaledDstRect.Height / 2);
    LScaledXRadius := min(LScaledXRadius, LMaxRadius);
    LScaledYRadius := min(LScaledYRadius, LMaxRadius);

    if LFillColor <> TAlphaColorRec.Null then begin
      aCanvas.FillRect(LScaledDstRect, LScaledXRadius, LScaledYRadius, ACorners, AOpacity, ACanvas.Fill, TCornerType.Round{CornerType});
    end;
    If LStrokeColor <> TalphaColorRec.Null then
      aCanvas.DrawRect(LScaledDstRect, LScaledXRadius, LScaledYRadius, ACorners, AOpacity, ACanvas.Stroke, TCornerType.Round{CornerType});

  finally
    ACanvas.RestoreState(LSaveState)
  end;

  {$ENDIF}
  {$ENDREGION}

end;

{************************}
procedure ALDrawRectangle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFillColor: TAlphaColor;
            const AFillGradientStyle: TGradientStyle;
            const AFillGradientColors: TArray<TAlphaColor>;
            const AFillGradientOffsets: TArray<Single>;
            const AFillGradientAngle: Single;
            const AFillResourceName: String;
            Const AFillWrapMode: TALImageWrapMode;
            Const AFillBackgroundMarginsRect: TRectF;
            Const AFillImageMarginsRect: TRectF;
            const AStateLayerOpacity: Single;
            const AStateLayerColor: TAlphaColor;
            Const AStateLayerMarginsRect: TRectF;
            const AStateLayerXRadius: Single;
            const AStateLayerYRadius: Single;
            const ADrawStateLayerOnTop: Boolean;
            const AStrokeColor: TalphaColor;
            const AStrokeThickness: Single;
            const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
            const AShadowBlur: Single;
            const AShadowOffsetX: Single;
            const AShadowOffsetY: Single;
            const ASides: TSides;
            const ACorners: TCorners;
            const AXRadius: Single;
            const AYRadius: Single);
begin
  var LFillGradientStartPoint: TPointF;
  var LFillGradientEndPoint: TPointF;
  case AFillGradientStyle of
    TGradientStyle.Linear: begin
      ALGetLinearGradientCoordinates(
        ADstRect.Size, // const ASize: TSizeF;
        AFillGradientAngle, // const AAngle: Single;
        LFillGradientStartPoint, // out AStartPoint: TPointF;
        LFillGradientEndPoint); // out AEndPoint: TPointF;
    end;
    TGradientStyle.Radial: begin
      LFillGradientStartPoint := ADstRect.CenterPoint;
      LFillGradientEndPoint := ADstRect.TopLeft;
    end
    else
      Raise Exception.Create('Error 69B128A0-83FC-4FF1-AC60-3BDF6044258D')
  end;
  ALDrawRectangle(
    ACanvas, // const ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    AAlignToPixel, // const AAlignToPixel: Boolean;
    ADstRect, // const ADstRect: TrectF;
    AOpacity, //const AOpacity: Single;
    AFillColor, // const AFillColor: TAlphaColor;
    AFillGradientStyle, // const AFillGradientStyle: TGradientStyle;
    AFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
    AFillGradientOffsets, // const AFillGradientOffsets: TArray<Single>;
    LFillGradientStartPoint, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
    LFillGradientEndPoint, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
    AFillResourceName, // const AFillResourceName: String;
    AFillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
    AFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
    AFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
    AStateLayerOpacity, // const AStateLayerOpacity: Single;
    AStateLayerColor, // const AStateLayerColor: TAlphaColor;
    AStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
    AStateLayerXRadius, // const AStateLayerXRadius: Single;
    AStateLayerYRadius, // const AStateLayerYRadius: Single;
    ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
    AStrokeColor, // const AStrokeColor: TalphaColor;
    AStrokeThickness, // const AStrokeThickness: Single;
    AShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
    AShadowBlur, // const AShadowBlur: Single;
    AShadowOffsetX, // const AShadowOffsetX: Single;
    AShadowOffsetY, // const AShadowOffsetY: Single;
    ASides, // const ASides: TSides;
    ACorners, // const ACorners: TCorners;
    AXRadius, // const AXRadius: Single;
    AYRadius); // const AYRadius: Single)
end;

{************************}
procedure ALDrawRectangle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow; // If shadow is not nil, then the Canvas must have enough space to draw the shadow (approximately Shadow.blur on each side of the rectangle)
            const ASides: TSides;
            const ACorners: TCorners;
            const AXRadius: Single;
            const AYRadius: Single);
begin
  // AFill
  var LFillColor: TAlphaColor;
  var LFillGradientStyle: TGradientStyle;
  var LFillGradientColors: TArray<TAlphaColor>;
  var LFillGradientOffsets: TArray<Single>;
  var LFillGradientAngle: Single;
  var LFillResourceName: String;
  var LFillWrapMode: TALImageWrapMode;
  var LFillBackgroundMarginsRect: TRectF;
  var LFillImageMarginsRect: TRectF;
  if AFill <> nil then begin
    LFillColor := AFill.Color;
    LFillGradientStyle := Afill.Gradient.Style;
    LFillGradientColors := Afill.Gradient.Colors;
    LFillGradientOffsets := Afill.Gradient.Offsets;
    LFillGradientAngle := Afill.Gradient.Angle;
    LFillResourceName := AFill.ResourceName;
    LFillWrapMode := AFill.WrapMode;
    LFillBackgroundMarginsRect := AFill.BackgroundMargins.Rect;
    LFillImageMarginsRect := AFill.ImageMargins.Rect;
  end
  else begin
    LFillColor := TAlphaColors.Null;
    LFillGradientStyle := TGradientStyle.Linear;
    LFillGradientColors := [];
    LFillGradientOffsets := [];
    LFillGradientAngle := 0;
    LFillResourceName := '';
    LFillWrapMode := TALImageWrapMode.Fit;
    LFillBackgroundMarginsRect := TRectF.Empty;
    LFillImageMarginsRect := TRectF.Empty;
  end;

  // AStateLayer
  var LStateLayerOpacity: Single;
  var LStateLayerColor: TAlphaColor;
  var LStateLayerMarginsRect: TRectF;
  var LStateLayerXRadius: Single;
  var LStateLayerYRadius: single;
  if AStateLayer <> nil then begin
    LStateLayerOpacity := AStateLayer.Opacity;
    if AStateLayer.UseContentColor then LStateLayerColor := AStateLayerContentColor
    else LStateLayerColor := AStateLayer.Color;
    LStateLayerMarginsRect := AStateLayer.Margins.Rect;
    LStateLayerXRadius := AStateLayer.XRadius;
    LStateLayerYRadius := AStateLayer.YRadius;
  end
  else begin
    LStateLayerOpacity := 0;
    LStateLayerColor := TAlphaColors.Null;
    LStateLayerMarginsRect := TRectF.Empty;
    LStateLayerXRadius := 0;
    LStateLayerYRadius := 0;
  end;

  // AStroke
  var LStrokeColor: TalphaColor;
  var LStrokeThickness: Single;
  if AStroke <> nil then begin
    LStrokeColor := AStroke.Color;
    LStrokeThickness := AStroke.Thickness;
  end
  else begin
    LStrokeColor := TalphaColors.Null;
    LStrokeThickness := 0;
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

  // DrawRectangle
  ALDrawRectangle(
    ACanvas, // const ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    AAlignToPixel, // const AAlignToPixel: Boolean;
    ADstRect, // const ADstRect: TrectF;
    AOpacity, //const AOpacity: Single;
    LFillColor, // const AFillColor: TAlphaColor;
    LfillGradientStyle, // const AFillGradientStyle: TGradientStyle;
    LfillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
    LfillGradientOffsets, // const AFillGradientOffsets: TArray<Single>;
    LfillGradientAngle, // const AFillGradientAngle: Single;
    LFillResourceName, // const AFillResourceName: String;
    LFillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
    LFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
    LFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
    LStateLayerOpacity, // const AStateLayerOpacity: Single;
    LStateLayerColor, // const AStateLayerColor: TAlphaColor;
    LStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
    LStateLayerXRadius, // const AStateLayerXRadius: Single;
    LStateLayerYRadius, // const AStateLayerYRadius: Single;
    ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
    LStrokeColor, // const AStrokeColor: TalphaColor;
    LStrokeThickness, // const AStrokeThickness: Single;
    LShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
    LShadowBlur, // const AShadowBlur: Single;
    LShadowOffsetX, // const AShadowOffsetX: Single;
    LShadowOffsetY, // const AShadowOffsetY: Single;
    ASides, // const ASides: TSides;
    ACorners, // const ACorners: TCorners;
    AXRadius, // const AXRadius: Single;
    AYRadius); // const AYRadius: Single)
end;

{*********************}
procedure ALDrawCircle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFillColor: TAlphaColor;
            const AFillGradientStyle: TGradientStyle;
            const AFillGradientColors: TArray<TAlphaColor>;
            const AFillGradientOffsets: TArray<Single>;
            const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
            const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
            const AFillResourceName: String;
            Const AFillWrapMode: TALImageWrapMode;
            Const AFillBackgroundMarginsRect: TRectF;
            Const AFillImageMarginsRect: TRectF;
            const AStateLayerOpacity: Single;
            const AStateLayerColor: TAlphaColor;
            Const AStateLayerMarginsRect: TRectF;
            const AStateLayerXRadius: Single;
            const AStateLayerYRadius: Single;
            const ADrawStateLayerOnTop: Boolean;
            const AStrokeColor: TalphaColor;
            const AStrokeThickness: Single;
            const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
            const AShadowBlur: Single;
            const AShadowOffsetX: Single;
            const AShadowOffsetY: Single);

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

  {$REGION '_DrawCircle (SKIA)'}
  {$IF defined(ALSkiaEngine)}
  procedure _DrawCircle(
              const aCanvas: sk_canvas_t;
              const aPaint: sk_Paint_t;
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean);
  begin

    var LRect := aRect;
    if (LStrokeColor <> TalphaColorRec.Null) then begin
      var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
      if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
        LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
      else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
        LRect.Inflate(-1, -1);
    end;
    //--
    var LCenterPoint := LRect.CenterPoint;
    sk4d_canvas_draw_circle(ACanvas, @LCenterPoint{center}, LRect.width / 2{radius}, aPaint);

  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_DrawCircle (ANDROID)'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}
  procedure _DrawCircle(
              const aCanvas: Jcanvas;
              const aPaint: JPaint;
              const aRect: TrectF;
              const aDrawOnlyBorder: Boolean;
              const aForceDrawPath: Boolean;
              const aClipPath: Boolean);
  begin

    // Init LRect
    var LRect := aRect;

    // use drawRoundRect
    if (not aForceDrawPath) and
       (not aClipPath) then begin

      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
        if aDrawOnlyBorder or (LRectIsEqualsToStrokeRect and (LShadowcolor = TalphaColorRec.Null)) then
          LRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2))
        else if (LRectIsEqualsToStrokeRect) and (compareValue(LScaledStrokeThickness, 1, TEpsilon.position) > 0) then
          LRect.Inflate(-1, -1);
      end;
      //--
      aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, aPaint);

    end

    // use drawPath
    else begin

      var LPath := TJPath.Create;
      //--
      if (LStrokeColor <> TalphaColorRec.Null) then begin
        var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
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
      LPath.addCircle(LRect.CenterPoint.x{x}, LRect.CenterPoint.y{y}, LRect.width / 2{radius}, TJPath_Direction.JavaClass.CW{dir});
      //--
      if aPaint <> nil then aCanvas.drawPath(LPath,aPaint);
      if aClipPath then begin
        aCanvas.save;
        aCanvas.clipPath(LPath);
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

  {$REGION '_DrawCircle (APPLEOS)'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}
  procedure _DrawCircle(
              const aCanvas: CGContextRef;
              const aGridHeight: Integer;
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean;
              const aClipPath: Boolean);
  begin
    var LRect := aRect;
    if (LStrokeColor <> TalphaColorRec.Null) then begin
      var LRectIsEqualsToStrokeRect := ARect.EqualsTo(LScaledDstRect, TEpsilon.position);
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
    CGContextClosePath(ACanvas);
    if aClipPath then CGContextClip(ACanvas)
    else if not aDrawOnlyBorder then CGContextFillPath(ACanvas)
    else CGContextStrokePath(ACanvas);
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
  procedure _SetShadow(const aCanvas: CGContextRef);
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
  procedure _ClearShadow(const aCanvas: CGContextRef);
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

    var LScaledStateLayerXRadius: Single := AStateLayerXRadius;
    var LScaledStateLayerYRadius: Single := AStateLayerYRadius;
    if LScaledStateLayerXRadius > 0 then LScaledStateLayerXRadius := LScaledStateLayerXRadius * AScale;
    if LScaledStateLayerYRadius > 0 then LScaledStateLayerYRadius := LScaledStateLayerYRadius* AScale;

    ALDrawRectangle(
      ACanvas, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      AAlignToPixel, // const AAlignToPixel: Boolean;
      LScaledStateLayerDstRect, // const ADstRect: TrectF;
      AStateLayerOpacity, // const AOpacity: Single;
      LStateLayerColor, // const AFillColor: TAlphaColor;
      TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
      [], // const AFillGradientColors: TArray<TAlphaColor>;
      [], // const AFillGradientOffsets: TArray<Single>;
      TPointF.Zero, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      TPointF.Zero, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      '', // const AFillResourceName: String;
      TALImageWrapMode.Fit, // Const AFillWrapMode: TALImageWrapMode;
      TRectF.Empty, // Const AFillBackgroundMarginsRect: TRectF;
      TRectF.Empty, // Const AFillImageMarginsRect: TRectF;
      0, // const AStateLayerOpacity: Single;
      TAlphaColors.Null, // const AStateLayerColor: TAlphaColor;
      TRectF.Empty, // Const AStateLayerMarginsRect: TRectF;
      0, // const AStateLayerXRadius: Single;
      0, // const AStateLayerYRadius: Single;
      False, // const ADrawStateLayerOnTop: Boolean;
      TAlphaColors.Null, // const AStrokeColor: TalphaColor;
      0, // const AStrokeThickness: Single;
      TAlphaColors.Null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
      0, // const AShadowBlur: Single;
      0, // onst AShadowOffsetX: Single;
      0, // const AShadowOffsetY: Single;
      AllSides, // const ASides: TSides;
      AllCorners, // const ACorners: TCorners;
      LScaledStateLayerXRadius, // const AXRadius: Single;
      LScaledStateLayerYRadius); // const AYRadius: Single)
  end;
  {$ENDREGION}

begin

  if CompareValue(AOpacity, 0, TEpsilon.Scale) <= 0 then exit;
  //--
  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AAlignToPixel then ALExtractMatrixFromCanvas(Acanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;
  //--
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}
  var LGridHeight := CGBitmapContextGetHeight(ACanvas);
  {$ENDIF}
  //--
  LScaledDstRect := ADstRect;
  LScaledDstRect.Top := LScaledDstRect.Top * AScale;
  LScaledDstRect.right := LScaledDstRect.right * AScale;
  LScaledDstRect.left := LScaledDstRect.left * AScale;
  LScaledDstRect.bottom := LScaledDstRect.bottom * AScale;
  LScaledDstRect := TRectF.Create(0, 0, 1, 1).FitInto(LScaledDstRect);
  if AAlignToPixel then
    LScaledDstRect := ALAlignToPixelRound(LScaledDstRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);
  //--
  LScaledStrokeThickness := AStrokeThickness * AScale;
  if AAlignToPixel then
    LScaledStrokeThickness := ALAlignDimensionToPixelRound(LScaledStrokeThickness, LCanvasScale, TEpsilon.Position);
  //--
  {$IF (defined(ANDROID)) or (defined(ALAppleOS)) or (defined(ALSkiaEngine))}
  LScaledShadowBlur := AShadowBlur * AScale;
  LScaledShadowOffsetX := AShadowOffsetX * AScale;
  LScaledShadowOffsetY := AShadowOffsetY * AScale;
  if AAlignToPixel then begin
    LScaledShadowBlur := ALAlignDimensionToPixelRound(LScaledShadowBlur, LCanvasScale, Tepsilon.Vector);
    LScaledShadowOffsetX := ALAlignDimensionToPixelRound(LScaledShadowOffsetX, LCanvasScale, TEpsilon.Position);
    LScaledShadowOffsetY := ALAlignDimensionToPixelRound(LScaledShadowOffsetY, LCanvasScale, TEpsilon.Position);
  end;
  LShadowColor := AShadowColor;
  if CompareValue(LScaledShadowBlur, 0, TEpsilon.position) <= 0 then
    LShadowColor := TAlphaColors.Null;
  {$ENDIF}
  //--
  LStrokeColor := AStrokeColor;
  if CompareValue(LScaledStrokeThickness, 0, TEpsilon.position) <= 0 then
    LStrokeColor := TAlphaColors.Null;
  //--
  if length(AFillGradientColors) = 1 then
    raise Exception.Create('Invalid gradient: A gradient requires at least two colors');
  var LFillGradientOffsets := AFillGradientOffsets;
  if (length(LFillGradientOffsets) = 0) and (length(AFillGradientColors) > 0) then begin
    setlength(LFillGradientOffsets, length(AFillGradientColors));
    for Var I := 0 to length(AFillGradientColors) - 1 do
      LFillGradientOffsets[i] := I * (1 / (length(AFillGradientColors) - 1));
  end
  else if (length(LFillGradientOffsets) <> length(AFillGradientColors)) then
    raise Exception.Create('Invalid gradient: The number of gradient offsets does not match the number of gradient colors');
  //--
  var LScaledFillGradientStartPoint := AFillGradientStartPoint;
  LScaledFillGradientStartPoint.X := LScaledFillGradientStartPoint.X * AScale;
  LScaledFillGradientStartPoint.Y := LScaledFillGradientStartPoint.Y * AScale;
  var LScaledFillGradientEndPoint := AFillGradientEndPoint;
  LScaledFillGradientEndPoint.X := LScaledFillGradientEndPoint.X * AScale;
  LScaledFillGradientEndPoint.Y := LScaledFillGradientEndPoint.Y * AScale;
  //--
  var LScaledFillBackgroundMarginsRect: TRectF;
  if (AFillColor <> TalphaColorRec.Null) or
     (length(AFillGradientColors) > 0) then begin
    LScaledFillBackgroundMarginsRect := AFillBackgroundMarginsRect;
    LScaledFillBackgroundMarginsRect.Top := LScaledFillBackgroundMarginsRect.Top * AScale;
    LScaledFillBackgroundMarginsRect.right := LScaledFillBackgroundMarginsRect.right * AScale;
    LScaledFillBackgroundMarginsRect.left := LScaledFillBackgroundMarginsRect.left * AScale;
    LScaledFillBackgroundMarginsRect.bottom := LScaledFillBackgroundMarginsRect.bottom * AScale;
    if AAlignToPixel then
      LScaledFillBackgroundMarginsRect := ALAlignEdgesToPixelRound(LScaledFillBackgroundMarginsRect, LCanvasScale, TEpsilon.Position);
  end
  else
    LScaledFillBackgroundMarginsRect := TRectF.Empty;
  //--
  var LScaledFillImageMarginsRect: TRectF;
  if (AFillResourceName <> '') then begin
    LScaledFillImageMarginsRect := AFillImageMarginsRect;
    LScaledFillImageMarginsRect.Top := LScaledFillImageMarginsRect.Top * AScale;
    LScaledFillImageMarginsRect.right := LScaledFillImageMarginsRect.right * AScale;
    LScaledFillImageMarginsRect.left := LScaledFillImageMarginsRect.left * AScale;
    LScaledFillImageMarginsRect.bottom := LScaledFillImageMarginsRect.bottom * AScale;
    if AAlignToPixel then
      LScaledFillImageMarginsRect := ALAlignEdgesToPixelRound(LScaledFillImageMarginsRect, LCanvasScale, TEpsilon.Position);
  end
  else
    LScaledFillImageMarginsRect := TRectF.Empty;
  //--
  var LScaledBackgroundDstRect := LScaledDstRect;
  LScaledBackgroundDstRect.Inflate(-LScaledFillBackgroundMarginsRect.Left, -LScaledFillBackgroundMarginsRect.Top, -LScaledFillBackgroundMarginsRect.Right, -LScaledFillBackgroundMarginsRect.Bottom);
  LScaledBackgroundDstRect := TRectF.Create(0, 0, 1, 1).FitInto(LScaledBackgroundDstRect);
  if (CompareValue(LScaledBackgroundDstRect.Width, 0, TEpsilon.Position) <= 0) or
     (CompareValue(LScaledBackgroundDstRect.height, 0, TEpsilon.Position) <= 0) then
    LScaledBackgroundDstRect := TRectf.Empty;
  //--
  var LScaledImageDstRect := LScaledDstRect;
  LScaledImageDstRect.Inflate(-LScaledFillImageMarginsRect.Left, -LScaledFillImageMarginsRect.Top, -LScaledFillImageMarginsRect.Right, -LScaledFillImageMarginsRect.Bottom);
  LScaledImageDstRect := TRectF.Create(0, 0, 1, 1).FitInto(LScaledImageDstRect);
  if (CompareValue(LScaledImageDstRect.Width, 0, TEpsilon.Position) <= 0) or
     (CompareValue(LScaledImageDstRect.height, 0, TEpsilon.Position) <= 0) then
    LScaledImageDstRect := TRectf.Empty;
  //--
  var LFillColor := AFillColor;
  LFillGradientColors := AFillGradientColors;
  var LFillResourceName := AFillResourceName;
  if LScaledBackgroundDstRect.IsEmpty then begin
    LFillColor := TALphaColors.Null;
    setlength(LFillGradientColors, 0);
    setlength(LFillGradientOffsets, 0);
  end;
  if LScaledImageDstRect.IsEmpty then
    LFillResourceName := '';
  //--
  var LScaledStateLayerMarginsRect: TRectF;
  if (AStateLayerColor <> TalphaColorRec.Null) and
     (CompareValue(AStatelayerOpacity, 0, TEpsilon.Scale) > 0) then begin
    LScaledStateLayerMarginsRect := AStateLayerMarginsRect;
    LScaledStateLayerMarginsRect.Top := LScaledStateLayerMarginsRect.Top * AScale;
    LScaledStateLayerMarginsRect.right := LScaledStateLayerMarginsRect.right * AScale;
    LScaledStateLayerMarginsRect.left := LScaledStateLayerMarginsRect.left * AScale;
    LScaledStateLayerMarginsRect.bottom := LScaledStateLayerMarginsRect.bottom * AScale;
    if AAlignToPixel then
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
  LStateLayerColor := AStateLayerColor;
  if (LScaledStateLayerDstRect.IsEmpty) or
     (CompareValue(AStatelayerOpacity, 0, TEpsilon.Scale) <= 0) then
    LStateLayerColor := TALphaColors.Null;
  //--
  if (LStateLayerColor <> TALphaColors.Null) and
     (CompareValue(AStatelayerOpacity, 0, TEpsilon.Scale) > 0) and
     (LScaledStateLayerDstRect.EqualsTo(LScaledBackgroundDstRect)) and
     (LScaledStateLayerDstRect.EqualsTo(LScaledDstRect)) and
     (sameValue(AStateLayerXRadius, -50, TEpsilon.Vector)) and
     (sameValue(AStateLayerYRadius, -50, TEpsilon.Vector)) then begin
    LFillColor := ALblendColor(LfillColor, LStateLayerColor, AStatelayerOpacity);
    if (ADrawStateLayerOnTop) and
       (LStrokeColor <> TAlphaColors.Null) then
      LStrokeColor := ALblendColor(LStrokeColor, LStateLayerColor, AStatelayerOpacity);
    LStateLayerColor := TALphaColors.Null;
  end;
  //--
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}
  LPathClipped := False;
  {$ENDIF}

  {$REGION 'SKIA'}
  {$IF defined(ALSkiaEngine)}

  // Create the alpha layer
  if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        AStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(ACanvas, LLayerRect, AOpacity);
  end;
  try

    // Create LPaint
    var LPaint := ALSkCheckHandle(sk4d_paint_create);
    try

      // Requests, but does not require, that edge pixels draw opaque or with partial transparency.
      sk4d_paint_set_antialias(LPaint, true);
      // Requests, but does not require, to distribute color error.
      sk4d_paint_set_dither(LPaint, true);

      // Fill the circle
      if (LFillColor <> TalphaColorRec.Null) or
         (length(LFillGradientColors) > 0) or
         (LFillResourceName <> '') or
         (LShadowColor <> TalphaColorRec.Null) then begin

        // FILL_SK_PAINTSTYLE
        sk4d_paint_set_style(LPaint, sk_paintstyle_t.FILL_SK_PAINTSTYLE);

        // Init LDrawnWithSolidColor
        var LDrawnWithSolidColor := False;

        // Fill with transparent solid color and shadow
        if (LShadowColor <> TalphaColorRec.Null) and // If null, skip drawing the shadow
           (TAlphaColorRec(LFillColor).A < 255) and // Else, fill with solid color and shadow in one pass
           (_FillGradientIsTransparent) and // Else, fill with gradient and shadow in one pass
           ((LFillResourceName = '') or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
            (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
            (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
            (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

          // First pass draw the shadow
          sk4d_paint_set_color(LPaint, ALSetColorAlpha(LFillColor, 1{AOpacity}));
          _SetShadow(LPaint);
          _DrawCircle(ACanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder});
          _ClearShadow(LPaint);

          // Second pass fill the rect
          var LBlender := ALSkCheckHandle(
                            sk4d_blender_make_mode(
                              sk_blendmode_t.SRC_SK_BLENDMODE));
          try
            sk4d_paint_set_blender(LPaint, LBlender);
            sk4d_paint_set_color(LPaint, LFillColor);
            _DrawCircle(ACanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder});
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
          _DrawCircle(ACanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder});
          _ClearShadow(LPaint);
          LDrawnWithSolidColor := True;
        end;

        // Fill with gradient
        if length(LFillGradientColors) > 0 then begin
          case AFillGradientStyle of
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
          if not LDrawnWithSolidColor then _SetShadow(LPaint);
          _DrawCircle(ACanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder});
          if not LDrawnWithSolidColor then _ClearShadow(LPaint);
          sk4d_paint_set_shader(LPaint, 0);
          LDrawnWithSolidColor := True;
        end;

        // Draw the StateLayer
        if not ADrawStateLayerOnTop then
          _DrawStateLayer;

        // Fill with image
        if LFillResourceName <> '' then begin
          var LImage: sk_image_t;
          {$IFDEF ALDPK}
          try
          {$ENDIF}
            var LFileName := ALGetResourceFilename(LFillResourceName);
            if LFileName <> '' then LImage := ALLoadFromFileAndWrapToSkImage(LFileName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height)
            else LImage := {$IFDEF ALDPK}0{$ELSE}ALLoadFromResourceAndWrapToSkImage(LFillResourceName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height){$ENDIF};
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
              sk4d_paint_set_color(LPaint, $FFFFFFFF);
              if not LDrawnWithSolidColor then _SetShadow(LPaint);
              _DrawCircle(ACanvas, LPaint, LScaledImageDstRect, false{aDrawOnlyBorder});
              if not LDrawnWithSolidColor then _ClearShadow(LPaint);
              sk4d_paint_set_shader(LPaint, 0);

            finally
              sk4d_refcnt_unref(LImage);
            end;
        end;

      end

      // Draw the StateLayer
      else if not ADrawStateLayerOnTop then
        _DrawStateLayer;

      // Stroke the circle
      if LStrokeColor <> TalphaColorRec.Null then begin
        sk4d_paint_set_style(LPaint, sk_paintstyle_t.STROKE_SK_PAINTSTYLE);
        sk4d_paint_set_stroke_width(LPaint, LScaledStrokeThickness);
        sk4d_paint_set_color(LPaint, LStrokeColor);
        _DrawCircle(ACanvas, LPaint, LScaledDstRect, true{aDrawOnlyBorder});
      end;

      // Draw the StateLayer
      if ADrawStateLayerOnTop then
        _DrawStateLayer;

    finally
      sk4d_paint_destroy(LPaint);
    end;

  finally
    // Remove the alpha layer
    if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(ACanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ANDROID'}
  {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

  // Create the alpha layer
  if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        AStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(ACanvas, LLayerRect, AOpacity);
  end;
  try

    //create the canvas and the paint
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    // Fill the circle
    if (LFillColor <> TalphaColorRec.Null) or
       (length(LFillGradientColors) > 0) or
       (LFillResourceName <> '') or
       (LShadowColor <> TalphaColorRec.Null) then begin

      //init LPaint
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LScaledDstRect + StrokeThickness :( this result&ing in border if the fill is for exemple black and border white

      // Init LDrawnWithSolidColor
      var LDrawnWithSolidColor := False;

      // Fill with transparent solid color and shadow
      if (LShadowColor <> TalphaColorRec.Null) and // If null, skip drawing the shadow
         (TAlphaColorRec(LFillColor).A < 255) and // Else, fill with solid color and shadow in one pass
         (_FillGradientIsTransparent) and // Else, fill with gradient and shadow in one pass
         ((LFillResourceName = '') or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
          (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
          (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
          (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

        // First pass draw the shadow
        LPaint.setColor(integer(ALSetColorAlpha(LFillColor, 1{AOpacity})));
        _SetShadow(LPaint);
        _DrawCircle(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, false{aClipPath});
        _ClearShadow(LPaint);

        // Second pass fill the rect
        var LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC);
        LPaint.setXfermode(LPorterDuffXfermode);
        LPaint.setColor(integer(LFillColor));
        _DrawCircle(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, (LFillResourceName <> '') and (length(LFillGradientColors) = 0) and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath});
        LPaint.setXfermode(nil);
        LPorterDuffXfermode := nil;
        LDrawnWithSolidColor := True;

      end

      // Fill with solid color
      else if (LFillColor <> TalphaColors.Null) then begin
        LPaint.setColor(integer(LFillColor));
        _SetShadow(LPaint);
        _DrawCircle(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, (LFillResourceName <> '') and (length(LFillGradientColors) = 0) and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath});
        _ClearShadow(LPaint);
        LDrawnWithSolidColor := True;
      end;

      //fill with gradient
      if length(LFillGradientColors) > 0 then begin

        // First pass draw the shadow if not already drawn
        // We must do this because else the shadow will be drawn with
        // the color of the gradient :(
        if not LDrawnWithSolidColor then begin
          LPaint.setColor(integer(ALSetColorAlpha(LFillColor, 1{AOpacity})));
          _SetShadow(LPaint);
          _DrawCircle(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, false{aClipPath});
          _ClearShadow(LPaint);
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
          case AFillGradientStyle of
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
          LPaint.setShader(LShader);
          _DrawCircle(aCanvas, LPaint, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, (LFillResourceName <> '') and (LScaledBackgroundDstRect.EqualsTo(LScaledImageDstRect, TEpsilon.position)){aClipPath});
          LPaint.setShader(nil);
          LShader := nil;
          LDrawnWithSolidColor := True;
        finally
          ALfreeandNil(LJColors);
          ALfreeandNil(LJOffsets);
        end;
      end;

      // Draw the StateLayer
      if not ADrawStateLayerOnTop then
        _DrawStateLayer;

      // Fill with image
      if LFillResourceName <> '' then begin
        var LBitmap: JBitmap;
        var LFileName := ALGetResourceFilename(LFillResourceName);
        if LFileName <> '' then LBitmap := ALLoadFromFileAndWrapToJBitmap(LFileName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height)
        else LBitmap := ALLoadFromResourceAndWrapToJBitmap(LFillResourceName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height);
        try

          // On android the bitmap is drawed with the opacity of the paint color
          // so set the color to black to make the bitmap fully opaque
          LPaint.setColor(integer(TAlphaColors.Black));

          // The shadow is made directly on the bitmap
          if (not LDrawnWithSolidColor) and (LShadowColor <> TalphaColorRec.Null) then begin

            // Draw the shape of the rect in LDestBitmap
            var LDestRect := TRectF.Create(0,0,LScaledImageDstRect.Width,LScaledImageDstRect.height);
            var LDestBitmap := TJBitmap.JavaClass.createBitmap(Round(LDestRect.Width), round(LDestRect.Height), TJBitmap_Config.JavaClass.ARGB_8888, true{hasAlpha}, ALGetGlobalJColorSpace);
            var LDestCanvas := TJCanvas.JavaClass.init(LDestBitmap);
            _DrawCircle(LDestCanvas, LPaint, LDestRect, false{aDrawOnlyBorder}, False{aForceDrawPath}, False{aClipPath});
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

            LDestRect := TrectF.Create(0,0, LBitmap.getWidth, LBitmap.getheight).CenterAt(LScaledImageDstRect);
            LDestRect.Offset(LScaledShadowOffsetX, LScaledShadowOffsetY);
            LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
            LJSrcRect := TJRect.JavaClass.init(0, 0, LBitmap.getWidth, LBitmap.getheight);
            //--
            var LBlurMaskFilter := TJBlurMaskFilter.JavaClass.init(AShadowblur*2.5, TJBlurMaskFilter_Blur.JavaClass.NORMAL);
            LPaint.setColor(integer(LShadowColor));
            LPaint.setMaskFilter(LBlurMaskFilter);
            aCanvas.drawBitmap(LBitmap.extractAlpha, LJSrcRect, LJDestRectf, LPaint);
            LPaint.setMaskFilter(nil);
            LDestRect.Offset(-LScaledShadowOffsetX, -LScaledShadowOffsetY);
            LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
            LPaint.setColor(integer(TAlphaColors.Black));
            aCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);

          end

          // The shadow is made on the circle shape
          else begin
            If not LPathClipped then _DrawCircle(aCanvas, nil{APaint}, LScaledImageDstRect, false{aDrawOnlyBorder}, true{aForceDrawPath}, true{aClipPath});
            var LDestRect := TrectF.Create(0,0, LBitmap.getWidth, LBitmap.getheight).CenterAt(LScaledImageDstRect);
            var LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
            var LJSrcRect := TJRect.JavaClass.init(0, 0, LBitmap.getWidth, LBitmap.getheight);
            aCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
            aCanvas.restore;
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
    else if not ADrawStateLayerOnTop then
      _DrawStateLayer;

    //stroke the circle
    if LStrokeColor <> TalphaColorRec.Null then begin
      LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
      LPaint.setStrokeWidth(LScaledStrokeThickness);
      LPaint.setColor(integer(LStrokeColor));
      _DrawCircle(aCanvas, LPaint, LScaledDstRect, true{aDrawOnlyBorder}, LFillResourceName <> ''{aForceDrawPath}, False{aClipPath});
    end;

    // Draw the StateLayer
    if ADrawStateLayerOnTop then
      _DrawStateLayer;

    //free the paint and the canvas
    LPaint := nil;

  finally
    // Remove the alpha layer
    if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(ACanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'APPLEOS'}
  {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  // Create the alpha layer
  if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then begin
    var LLayerRect := ALGetShapeSurfaceRect(
                        LScaledDstRect, // const ARect: TrectF;
                        LFillColor, // const AFillColor: TAlphaColor;
                        LFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                        LFillResourceName, // const AFillResourceName: String;
                        LScaledFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
                        LScaledFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
                        AStateLayerOpacity, // const AStateLayerOpacity: Single;
                        LStateLayerColor, // const AStateLayerColor: TAlphaColor;
                        false, // const AStateLayerUseContentColor: Boolean;
                        LScaledStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
                        LShadowColor, // const AShadowColor: TAlphaColor;
                        LScaledShadowBlur, // const AShadowBlur: Single;
                        LScaledShadowOffsetX, // const AShadowOffsetX: Single;
                        LScaledShadowOffsetY); // const AShadowOffsetY: Single);
    ALBeginTransparencyLayer(ACanvas, LLayerRect, AOpacity);
  end;
  try

    // Fill the circle
    if (LFillColor <> TalphaColorRec.Null) or
       (length(LFillGradientColors) > 0) or
       (LFillResourceName <> '') or
       (LShadowColor <> TalphaColorRec.Null) then begin

      // Init LDrawnWithSolidColor
      var LDrawnWithSolidColor := False;

      // Fill with transparent solid color and shadow
      if (LShadowColor <> TalphaColorRec.Null) and // If null, skip drawing the shadow
         (TAlphaColorRec(LFillColor).A < 255) and // Else, fill with solid color and shadow in one pass
         (_FillGradientIsTransparent) and // Else, fill with gradient and shadow in one pass
         ((LFillResourceName = '') or // If no image, no opaque fill color and no opaque gradient is present, then draw the shadow in two passes
          (LFillColor <> TalphaColors.Null) or // If there is an image and a transparent fill color, then draw the shadow in two passes
          (length(LFillGradientColors) > 0) or // If there is an image and a transparent gradient, draw the shadow in two passes
          (LStrokeColor <> TalphaColorRec.Null)) then begin // If there is an image and a stroke, draw the shadow in two passes

        // First pass draw the shadow
        var LFillColorF := TAlphaColorCGFloat.Create(LFillColor);
        CGContextSetRGBFillColor(ACanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, 1{A});
        _SetShadow(aCanvas);
        _DrawCircle(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath});
        _ClearShadow(aCanvas);

        // Second pass fill the rect
        CGContextSetBlendMode(ACanvas, kCGBlendModeCopy);
        CGContextSetRGBFillColor(ACanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, LFillColorF.A);
        _DrawCircle(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath});
        CGContextSetBlendMode(ACanvas, kCGBlendModeNormal);
        LDrawnWithSolidColor := True;

      end

      // Fill with solid color
      else if (LFillColor <> TalphaColors.Null) then begin
        var LFillColorF := TAlphaColorCGFloat.Create(LFillColor);
        CGContextSetRGBFillColor(ACanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, LFillColorF.A);
        _SetShadow(aCanvas);
        _DrawCircle(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath});
        _ClearShadow(aCanvas);
        LDrawnWithSolidColor := True;
      end;

      //fill with gradient
      if length(LFillGradientColors) > 0 then begin

        // First pass draw the shadow if not already drawn
        // We must do this because else the shadow will not be drawn
        if not LDrawnWithSolidColor then begin
          var LFillColorF := TAlphaColorCGFloat.Create(LFillColor);
          CGContextSetRGBFillColor(ACanvas, LFillColorF.R, LFillColorF.G, LFillColorF.B, 1{A});
          _SetShadow(aCanvas);
          _DrawCircle(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, false{aClipPath});
          _ClearShadow(aCanvas);
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
            case AFillGradientStyle of
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
              _DrawCircle(aCanvas, LGridHeight, LScaledBackgroundDstRect, false{aDrawOnlyBorder}, true{aClipPath});
              CGContextDrawShading(ACanvas, LShading);
              CGContextRestoreGState(ACanvas);
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
      if not ADrawStateLayerOnTop then
        _DrawStateLayer;

      // Fill with image
      if LFillResourceName <> '' then begin
        var LImage: CGImageRef;
        var LFileName := ALGetResourceFilename(LFillResourceName);
        if LFileName <> '' then LImage := ALLoadFromFileAndWrapToCGImageRef(LFileName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height)
        else LImage := ALLoadFromResourceAndWrapToCGImageRef(LFillResourceName, AFillWrapMode, LScaledImageDstRect.Width, LScaledImageDstRect.Height);
        try

          // The shadow is made directly on the bitmap
          if (not LDrawnWithSolidColor) and (LShadowColor <> TalphaColorRec.Null) then begin

            // Their is corners so remove them from the LBitmap
            var LTmpRect := TRectF.Create(0,0,LScaledImageDstRect.Width,LScaledImageDstRect.height).Round;
            var LTmpCGContextRef := ALCreateCGContextRef(LTmpRect.Width, LTmpRect.Height);
            try
              _DrawCircle(LTmpCGContextRef, LTmpRect.Height, LTmpRect, false{aDrawOnlyBorder}, true{aClipPath});
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

            _SetShadow(aCanvas);
            var LDestRect := TrectF.Create(0,0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)).CenterAt(LScaledImageDstRect);
            CGContextDrawImage(
              ACanvas, // c: The graphics context in which to draw the image.
              ALLowerLeftCGRect(
                LDestRect.TopLeft,
                LDestRect.Width,
                LDestRect.Height,
                LGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              LImage); // image The image to draw.
             _ClearShadow(aCanvas);

          end

          // The shadow is made on the circle shape
          else begin
            _DrawCircle(aCanvas, LGridHeight, LScaledImageDstRect, false{aDrawOnlyBorder}, true{aClipPath});
            var LDestRect := TrectF.Create(0,0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)).CenterAt(LScaledImageDstRect);
            CGContextDrawImage(
              ACanvas, // c: The graphics context in which to draw the image.
              ALLowerLeftCGRect(
                LDestRect.TopLeft,
                LDestRect.Width,
                LDestRect.Height,
                LGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              LImage); // image The image to draw.
            CGContextRestoreGState(ACanvas);
          end;

        finally
          CGImageRelease(LImage);
        end;
      end;

    end

    // Draw the StateLayer
    else if not ADrawStateLayerOnTop then
      _DrawStateLayer;

    //stroke the circle
    if LStrokeColor <> TalphaColorRec.Null then begin
      CGContextSetLineWidth(ACanvas, LScaledStrokeThickness);
      var LStrokeColorF := TAlphaColorCGFloat.Create(LStrokeColor);
      CGContextSetRGBStrokeColor(ACanvas, LStrokeColorF.R, LStrokeColorF.G, LStrokeColorF.B, LStrokeColorF.A);
      _DrawCircle(aCanvas, LGridHeight, LScaledDstRect, True{aDrawOnlyBorder}, false);
    end;

    // Draw the StateLayer
    if ADrawStateLayerOnTop then
      _DrawStateLayer;

  finally
    // Remove the alpha layer
    if compareValue(AOpacity, 1, Tepsilon.Scale) < 0 then
      ALEndTransparencyLayer(ACanvas);
  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWINDOWS'}
  {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

  var LSaveState := ACanvas.SaveState;
  try

    if LFillColor <> TAlphaColorRec.Null then begin
      ACanvas.Fill.Kind := TBrushKind.Solid;
      ACanvas.Fill.Color := LFillColor;
    end
    else ACanvas.Fill.Kind := TBrushKind.None;
    If LStrokeColor <> TalphaColorRec.Null then begin
      ACanvas.Stroke.Kind := TBrushKind.Solid;
      ACanvas.Stroke.Color := LStrokeColor;
      ACanvas.Stroke.Thickness := LScaledStrokeThickness;
    end
    else ACanvas.Stroke.Kind := TBrushKind.None;

    if LStrokeColor <> TalphaColorRec.Null then
      LScaledDstRect.Inflate(-(LScaledStrokeThickness / 2), -(LScaledStrokeThickness / 2));

    if LFillColor <> TAlphaColorRec.Null then begin
      aCanvas.FillEllipse(LScaledDstRect, AOpacity, ACanvas.Fill);
    end;
    If LStrokeColor <> TalphaColorRec.Null then
      aCanvas.DrawEllipse(LScaledDstRect, AOpacity, ACanvas.Stroke);

  finally
    ACanvas.RestoreState(LSaveState)
  end;

  {$ENDIF}
  {$ENDREGION}

end;

{*********************}
procedure ALDrawCircle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFillColor: TAlphaColor;
            const AFillGradientStyle: TGradientStyle;
            const AFillGradientColors: TArray<TAlphaColor>;
            const AFillGradientOffsets: TArray<Single>;
            const AFillGradientAngle: Single;
            const AFillResourceName: String;
            Const AFillWrapMode: TALImageWrapMode;
            Const AFillBackgroundMarginsRect: TRectF;
            Const AFillImageMarginsRect: TRectF;
            const AStateLayerOpacity: Single;
            const AStateLayerColor: TAlphaColor;
            Const AStateLayerMarginsRect: TRectF;
            const AStateLayerXRadius: Single;
            const AStateLayerYRadius: Single;
            const ADrawStateLayerOnTop: Boolean;
            const AStrokeColor: TalphaColor;
            const AStrokeThickness: Single;
            const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
            const AShadowBlur: Single;
            const AShadowOffsetX: Single;
            const AShadowOffsetY: Single); overload;
begin
  var LFillGradientStartPoint: TPointF;
  var LFillGradientEndPoint: TPointF;
  case AFillGradientStyle of
    TGradientStyle.Linear: begin
      ALGetLinearGradientCoordinates(
        ADstRect.Size, // const ASize: TSizeF;
        AFillGradientAngle, // const AAngle: Single;
        LFillGradientStartPoint, // out AStartPoint: TPointF;
        LFillGradientEndPoint); // out AEndPoint: TPointF;
    end;
    TGradientStyle.Radial: begin
      LFillGradientStartPoint := ADstRect.CenterPoint;
      LFillGradientEndPoint := ADstRect.TopLeft;
    end
    else
      Raise Exception.Create('Error 69B128A0-83FC-4FF1-AC60-3BDF6044258D')
  end;
  ALDrawCircle(
    ACanvas, // const ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    AAlignToPixel, // const AAlignToPixel: Boolean;
    ADstRect, // const ADstRect: TrectF;
    AOpacity, //const AOpacity: Single;
    AFillColor, // const AFillColor: TAlphaColor;
    AFillGradientStyle, // const AFillGradientStyle: TGradientStyle;
    AFillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
    AFillGradientOffsets, // const AFillGradientOffsets: TArray<Single>;
    LFillGradientStartPoint, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
    LFillGradientEndPoint, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
    AFillResourceName,// const AFillResourceName: String;
    AFillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
    AFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
    AFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
    AStateLayerOpacity, // const AStateLayerOpacity: Single;
    AStateLayerColor, // const AStateLayerColor: TAlphaColor;
    AStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
    AStateLayerXRadius, // const AStateLayerXRadius: Single;
    AStateLayerYRadius, // const AStateLayerYRadius: Single;
    ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
    AStrokeColor, // const AStrokeColor: TalphaColor;
    AStrokeThickness, // const AStrokeThickness: Single;
    AShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
    AShadowBlur, // const AShadowBlur: Single;
    AShadowOffsetX, // const AShadowOffsetX: Single;
    AShadowOffsetY); // const AShadowOffsetY: Single);
end;

{*********************}
procedure ALDrawCircle(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const AAlignToPixel: Boolean;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow); // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
begin
  // AFill
  var LFillColor: TAlphaColor;
  var LFillGradientStyle: TGradientStyle;
  var LFillGradientColors: TArray<TAlphaColor>;
  var LFillGradientOffsets: TArray<Single>;
  var LFillGradientAngle: Single;
  var LFillResourceName: String;
  var LFillWrapMode: TALImageWrapMode;
  var LFillBackgroundMarginsRect: TRectF;
  var LFillImageMarginsRect: TRectF;
  if AFill <> nil then begin
    LFillColor := AFill.Color;
    LFillGradientStyle := Afill.Gradient.Style;
    LFillGradientColors := Afill.Gradient.Colors;
    LFillGradientOffsets := Afill.Gradient.Offsets;
    LFillGradientAngle := Afill.Gradient.Angle;
    LFillResourceName := AFill.ResourceName;
    LFillWrapMode := AFill.WrapMode;
    LFillBackgroundMarginsRect := AFill.BackgroundMargins.Rect;
    LFillImageMarginsRect := AFill.ImageMargins.Rect;
  end
  else begin
    LFillColor := TAlphaColors.Null;
    LFillGradientStyle := TGradientStyle.Linear;
    LFillGradientColors := [];
    LFillGradientOffsets := [];
    LFillGradientAngle := 0;
    LFillResourceName := '';
    LFillWrapMode := TALImageWrapMode.Fit;
    LFillBackgroundMarginsRect := TRectF.Empty;
    LFillImageMarginsRect := TRectF.Empty;
  end;

  // AStateLayer
  var LStateLayerOpacity: Single;
  var LStateLayerColor: TAlphaColor;
  var LStateLayerMarginsRect: TRectF;
  var LStateLayerXRadius: Single;
  var LStateLayerYRadius: single;
  if AStateLayer <> nil then begin
    LStateLayerOpacity := AStateLayer.Opacity;
    if AStateLayer.UseContentColor then LStateLayerColor := AStateLayerContentColor
    else LStateLayerColor := AStateLayer.Color;
    LStateLayerMarginsRect := AStateLayer.Margins.Rect;
    LStateLayerXRadius := AStateLayer.XRadius;
    LStateLayerYRadius := AStateLayer.YRadius;
  end
  else begin
    LStateLayerOpacity := 0;
    LStateLayerColor := TAlphaColors.Null;
    LStateLayerMarginsRect := TRectF.Empty;
    LStateLayerXRadius := 0;
    LStateLayerYRadius := 0;
  end;

  // AStroke
  var LStrokeColor: TalphaColor;
  var LStrokeThickness: Single;
  if AStroke <> nil then begin
    LStrokeColor := AStroke.Color;
    LStrokeThickness := AStroke.Thickness;
  end
  else begin
    LStrokeColor := TalphaColors.Null;
    LStrokeThickness := 0;
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

  // DrawCircle
  ALDrawCircle(
    ACanvas, // const ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    AAlignToPixel, // const AAlignToPixel: Boolean;
    ADstRect, // const ADstRect: TrectF;
    AOpacity, //const AOpacity: Single;
    LFillColor, // const AFillColor: TAlphaColor;
    LfillGradientStyle, // const AFillGradientStyle: TGradientStyle;
    LfillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
    LfillGradientOffsets, // const AFillGradientOffsets: TArray<Single>;
    LfillGradientAngle, // const AFillGradientAngle: Single;
    LFillResourceName, // const AFillResourceName: String;
    LFillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
    LFillBackgroundMarginsRect, // Const AFillBackgroundMarginsRect: TRectF;
    LFillImageMarginsRect, // Const AFillImageMarginsRect: TRectF;
    LStateLayerOpacity, // const AStateLayerOpacity: Single;
    LStateLayerColor, // const AStateLayerColor: TAlphaColor;
    LStateLayerMarginsRect, // Const AStateLayerMarginsRect: TRectF;
    LStateLayerXRadius, // const AStateLayerXRadius: Single;
    LStateLayerYRadius, // const AStateLayerYRadius: Single;
    ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
    LStrokeColor, // const AStrokeColor: TalphaColor;
    LStrokeThickness, // const AStrokeThickness: Single;
    LShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
    LShadowBlur, // const AShadowBlur: Single;
    LShadowOffsetX, // const AShadowOffsetX: Single;
    LShadowOffsetY); // const AShadowOffsetY: Single;
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

  {$IF DEFINED(ALDPK)}

  var LBufferBitmap := ALCreateBitmapFromSkImage(ADrawable);
  try
    ACanvas.DrawBitmap(
      LBufferBitmap,
      LSrcRect, {SrcRect}
      LDstRect, {DestRect}
      AOpacity, {opacity}
      true{highSpeed});
  finally
    ALFreeAndNil(LBufferBitmap);
  end;

  {$ELSE}

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

  {$ENDIF}

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

  {$IF DEFINED(ALDPK)}

  var LBufferBitmap := ALCreateBitmapFromSkImage(ADrawable);
  try
    ACanvas.DrawBitmap(
      LBufferBitmap,
      ASrcRect, {SrcRect}
      LDstRect, {DestRect}
      AOpacity, {opacity}
      true{highSpeed});
  finally
    ALFreeAndNil(LBufferBitmap);
  end;

  {$ELSE}

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

  {$ENDIF}

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
    result := ALCreateBitmapFromSkSurface(ASurface);
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  result := ALCreateTextureFromJBitmap(ASurface);
  {$ELSEIF defined(ALAppleOS)}
    {$IF defined(ALGpuCanvas)}
    result := ALCreateTextureFromCGContextRef(ASurface);
    {$ELSE}
    result := ALCreateBitmapFromCGContextRef(ASurface);
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
    ALUpdateBitmapFromSkSurface(ASurface, aDrawable);
    {$ENDIF}
  {$ELSEIF defined(ANDROID)}
  ALUpdateTextureFromJBitmap(ASurface, aDrawable);
  {$ELSEIF defined(ALAppleOS)}
    {$IF defined(ALGpuCanvas)}
    ALUpdateTextureFromCGContextRef(ASurface, aDrawable);
    {$ELSE}
    ALUpdateBitmapFromCGContextRef(ASurface, aDrawable);
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

{******************************************************************************************************************}
procedure ALExtractMatrixFromCanvas(const ACanvas: TALCanvas; out ACanvasMatrix: TMatrix; out ACanvasScale: Single);
begin
  ACanvasMatrix := TMatrix.Identity;
  ACanvasScale := 1;
  {$IF defined(ALSkiaEngine)}
  if not ALIsCanvasNull(ACanvas) then begin
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
  {$ENDIF}
end;

{*******************************}
function  ALScaleAndCenterCanvas(
            Const ACanvas: TCanvas;
            Const AAbsoluteRect: TRectF;
            Const AScaleX: Single;
            Const AScaleY: Single;
            Const ASaveState: Boolean): TCanvasSaveState;
begin
  Result := nil;
  if (not samevalue(AScaleX, 1, TEpsilon.Scale)) or
     (not samevalue(AScaleY, 1, TEpsilon.Scale)) then begin
    if ASaveState then
      Result := ACanvas.SaveState;
    var LMatrix := TMatrix.CreateTranslation(-AAbsoluteRect.Left, -AAbsoluteRect.Top);
    LMatrix := LMatrix * TMatrix.CreateScaling(AScalex, AScaley);
    LMatrix := LMatrix * TMatrix.CreateTranslation(
                           AAbsoluteRect.Left - (((AAbsoluteRect.Width * AScalex) - AAbsoluteRect.Width) / 2),
                           AAbsoluteRect.Top - (((AAbsoluteRect.height * AScaley) - AAbsoluteRect.Height) / 2));
    ACanvas.SetMatrix(ACanvas.Matrix * LMatrix);
  end;
end;

{*************************}
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

{*************************}
{$IF defined(ALSkiaAvailable)}
function ALSkCheckHandle(const AHandle: sk_handle_t): sk_handle_t;
begin
  If AHandle = 0 then
    raise Exception.Create('Skia API call failed');
  result := AHandle;
end;
{$ENDIF}

{*************************}
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

{*************************}
{$IF defined(ALSkiaAvailable)}
function ALSkStreamAdapterGetLengthProc(context: Pointer): size_t;
begin
  Result := TStream(context).Size;
end;
{$ENDIF}

{*************************}
{$IF defined(ALSkiaAvailable)}
function ALSkStreamAdapterGetPositionProc(context: Pointer): size_t;
begin
  Result := TStream(context).Position;
end;
{$ENDIF}

{*************************}
{$IF defined(ALSkiaAvailable)}
function ALSkStreamAdapterReadProc(context: Pointer; buffer: Pointer; size: size_t): size_t;
begin
  Result := TStream(context).Read(buffer^, size);
end;
{$ENDIF}

{*************************}
{$IF defined(ALSkiaAvailable)}
function ALSkStreamAdapterSeekProc(context: Pointer; position: size_t): _bool;
begin
  TStream(context).Position := position;
  Result := True;
end;
{$ENDIF}

{*************************}
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

{*************************}
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

{*************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateBitmapFromSkPixmap(Const APixmap: sk_pixmap_t): TBitmap;
begin
  var LWidth := sk4d_pixmap_get_width(APixmap);
  var LHeight := sk4d_pixmap_get_Height(APixmap);
  Result := TBitmap.Create(LWidth, LHeight);
  Try
    ALUpdateBitmapFromSkPixmap(APixmap, Result);
  except
    ALFreeAndNil(Result);
    Raise;
  end;
end;
{$ENDIF}

{*************************}
{$IF defined(ALSkiaAvailable)}
function ALCreateBitmapFromSkSurface(Const ASurface: sk_surface_t): TBitmap;
begin
  var LPixmap := ALSkCheckHandle(sk4d_surface_peek_pixels(ASurface));
  try
    Result := ALCreateBitmapFromSkPixmap(LPixmap);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{*************************}
{$IF defined(ALSkiaAvailable)}
Function ALCreateBitmapFromSkImage(const AImage: sk_image_t): TBitmap;
begin
  var LPixmap := ALSkCheckHandle(sk4d_image_peek_pixels(AImage));
  try
    Result := ALCreateBitmapFromSkPixmap(LPixmap);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{*************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateBitmapFromSkPixmap(Const APixmap: sk_pixmap_t; const ABitmap: Tbitmap);
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

{*************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateBitmapFromSkSurface(Const ASurface: sk_surface_t; const ABitmap: TBitmap);
begin
  var LPixmap := ALSkCheckHandle(sk4d_surface_peek_pixels(ASurface));
  try
    ALUpdateBitmapFromSkPixmap(LPixmap, ABitmap);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{*************************}
{$IF defined(ALSkiaAvailable)}
procedure ALUpdateBitmapFromSkImage(const AImage: sk_image_t; const ABitmap: TBitmap);
begin
  var LPixmap := ALSkCheckHandle(sk4d_image_peek_pixels(AImage));
  try
    ALUpdateBitmapFromSkPixmap(LPixmap, ABitmap);
  finally
    sk4d_pixmap_destroy(LPixmap);
  end;
end;
{$ENDIF}

{*************************}
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

{*************************}
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

{*************************}
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

{*************************}
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
end;
{$ENDIF}

{*************************}
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

{*************************}
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

{*************************}
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

{*************************}
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

{*************************}
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

{*************************}
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

{*************************}
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

{****************}
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

{****************}
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

{****************}
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

{****************}
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

{****************}
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

{****************}
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

{****************}
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

{*****************************************}
procedure TALGraphicThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
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
        var LExtData := AExtData;
        Try
          if assigned(AProc) then aProc(LExtData);
        finally
          ALFreeAndNil(LExtData);
        end;
      except
        //hide the exception
      end;
    end);
  {$ELSE}
  inherited ExecuteProc(AProc, AExtData, APriority, AGetPriorityFunc, AAsync);
  {$endif}
end;

{*****************************************}
procedure TALGraphicThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
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
        var LExtData := AExtData;
        Try
          if assigned(AProc) then aProc(LExtData);
        finally
          ALFreeAndNil(LExtData);
        end;
      except
        //hide the exception
      end;
    end);
  {$ELSE}
  inherited ExecuteProc(AProc, AExtData, APriority, AGetPriorityFunc, AAsync);
  {$endif}
end;

initialization
  {$IF defined(ALSkiaAvailable)}
  ALGlobalUseRasterSkSurface := True;
  ALGlobalUseRasterSkImage := True;
  ALGlobalSkColorSpace := 0;
  ALGlobalSkColorSpaceInitialized := False;
  ALGlobalSkPaint := 0;
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
