unit Alcinoe.FMX.Graphics;

interface

{$I Alcinoe.inc}

uses
  system.classes,
  system.sysutils,
  system.types,
  system.uitypes,
  {$IF defined(ios)}
  iOSapi.CoreGraphics,
  iOSapi.CocoaTypes,
  fmx.surfaces,
  fmx.types3D,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  fmx.surfaces,
  fmx.types3D,
  {$ENDIF}
  FMX.types,
  FMX.graphics,
  Alcinoe.FMX.Common,
  Alcinoe.Common;

//get the oritation From Exif
Type

  TALNativeBitmap = {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
  TALRasterImage = {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

  TalExifOrientationInfo = (FLIP_HORIZONTAL,
                            FLIP_VERTICAL,
                            NORMAL,
                            ROTATE_180,
                            ROTATE_270,
                            ROTATE_90,
                            TRANSPOSE,
                            TRANSVERSE,
                            UNDEFINED);

function  ALGetImageSize(const aStream: TCustomMemoryStream): TSize;
function  AlGetExifOrientationInfo(const aFilename: String): TalExifOrientationInfo;
procedure ALNormalizeImageOrientationV1(const aBitmap: Tbitmap; const aExifOrientationInfo: TalExifOrientationInfo);
function  ALNormalizeImageOrientationV2(const aBitmap: TALNativeBitmap; const aExifOrientationInfo: TalExifOrientationInfo): TALNativeBitmap;
function  AlGetImageSignature(const aStream: TStream; const aSignatureLength: integer = 12): Tbytes; overload;
function  AlGetImageSignature(const aFileName: string; const aSignatureLength: integer = 12): Tbytes; overload;
function  AlDetectImageExtension(const aStream: TStream): String; overload;
function  AlDetectImageExtension(const aFileName: string): String; overload;
function  ALPrepareColor(const SrcColor: TAlphaColor; const Opacity: Single): TAlphaColor;
function  ALAlphaBlendColors(const aBackToFrontColors: array of TAlphaColor): TAlphaColor;

{$IF defined(ANDROID)}
function ALJBitmaptoTexture(const aBitmap: Jbitmap): TTexture;
{$ENDIF}
{$IFDEF ALUseTexture}
function ALBitmapSurfacetoTexture(const aBitmapSurface: TbitmapSurface): TTexture;
function ALTransformBitmaptoTexture(var aBitmap: Tbitmap): TTexture;
{$ENDIF}

type
  TALResizeImageGetDestSizeFunct = reference to function(const aOriginalSize: TPointF): TpointF;
  TALResizeAndBlurImageGetDestSizeFunct = reference to function(const aOriginalSize: TPointF; var aRadius: single): TpointF;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
//https://i.stack.imgur.com/CcESX.png - transparent pixel in the mask are removed from the resulting image
function  ALFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap): Tbitmap; overload;
function  ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap): TALNativeBitmap; overload;
function  ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap): TALRasterImage; overload;
//-----
function  ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): TALNativeBitmap; overload;
function  ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; aBlurRadius: single; const aBlurW, aBlurH: single): TALNativeBitmap; overload;
function  ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): TALRasterImage; overload;
function  ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; aBlurRadius: single; const aBlurW, aBlurH: single): TALRasterImage; overload;
//-----
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: TALNativeBitmap; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: TALNativeBitmap): TALNativeBitmap; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: TALNativeBitmap; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: TALNativeBitmap): TALRasterImage; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as round rect
function  ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): Tbitmap; overload;
function  ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): TALNativeBitmap; overload;
function  ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): TALRasterImage; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as circle
function  ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single): TALNativeBitmap; overload;
function  ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single): TALRasterImage; overload;
//-----
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single): TALNativeBitmap; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single): TALRasterImage; overload;
//-----
function  ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): TALNativeBitmap; overload;
function  ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): TALNativeBitmap; overload;
function  ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): TALRasterImage; overload;
function  ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): TALRasterImage; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as rect
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single): TALNativeBitmap; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single): TALRasterImage; overload;
//-----
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): TALNativeBitmap; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): TALNativeBitmap; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): TALRasterImage; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): TALRasterImage; overload;
//-----
function  ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single): TALNativeBitmap; overload;
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single): TALRasterImage; overload;
//-----
function  ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single): TALNativeBitmap; overload;
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single): TALRasterImage; overload;

//If any dimension of the image is greater than W or H then the image is scaled down to best fit W and H else the image is cropped with same ratio between W and H
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single): TALNativeBitmap; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single): TALRasterImage; overload;
//-----
function  ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single): TALNativeBitmap; overload;
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single): TALRasterImage; overload;
//-----
function  ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single; const aCropCenter: TPointF): TALNativeBitmap; overload;
function  ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single): TALNativeBitmap; overload;
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single; const aCropCenter: TPointF): TALRasterImage; overload;
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single): TALRasterImage; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or lower than w or h
function  ALFitIntoImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap; overload;
function  ALFitIntoImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): TALNativeBitmap; overload;
function  ALFitIntoImageV2(const aStream: TCustomMemoryStream; const W, H: single): TALNativeBitmap; overload;
function  ALFitIntoImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): TALRasterImage; overload;
function  ALFitIntoImageV3(const aStream: TCustomMemoryStream; const W, H: single): TALRasterImage; overload;
//-----
function  ALLoadFitIntoResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
function  ALLoadFitIntoResourceImageV2(const aResName: String; const W, H: single): TALNativeBitmap;
function  ALLoadFitIntoResourceImageV3(const aResName: String; const W, H: single): TALRasterImage;
//-----
function  ALLoadFitIntoFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
function  ALLoadFitIntoFileImageV2(const aFileName: String; const W, H: single): TALNativeBitmap;
function  ALLoadFitIntoFileImageV3(const aFileName: String; const W, H: single): TALRasterImage;

//resize the src image to make that width = w and height = h
function  ALStretchImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap; overload;
function  ALStretchImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALStretchImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): TALNativeBitmap; overload;
function  ALStretchImageV2(const aStream: TCustomMemoryStream; const W, H: single): TALNativeBitmap; overload;
function  ALStretchImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): TALRasterImage; overload;
function  ALStretchImageV3(const aStream: TCustomMemoryStream; const W, H: single): TALRasterImage; overload;
//-----
function  ALLoadStretchResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
function  ALLoadStretchResourceImageV2(const aResName: String; const W, H: single): TALNativeBitmap;
function  ALLoadStretchResourceImageV3(const aResName: String; const W, H: single): TALRasterImage;
//-----
function  ALLoadStretchFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
function  ALLoadStretchFileImageV2(const aFileName: String; const W, H: single): TALNativeBitmap;
function  ALLoadStretchFileImageV3(const aFileName: String; const W, H: single): TALRasterImage;
//-----
function  ALLoadNormalizeOrientationImageV1(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): Tbitmap;
function  ALLoadNormalizeOrientationImageV2(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): TALNativeBitmap;
function  ALLoadNormalizeOrientationImageV3(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): TALRasterImage;
//-----
function  ALLoadNormalizeOrientationFileImageV1(const aFileName: String): Tbitmap;
function  ALLoadNormalizeOrientationFileImageV2(const aFileName: String): TALNativeBitmap;
function  ALLoadNormalizeOrientationFileImageV3(const aFileName: String): TALRasterImage;

{$IF defined(IOS)}
type
  PAlphaColorCGFloat = ^TAlphaColorCGFloat;
  TAlphaColorCGFloat = record
  public
    R, G, B, A: CGFloat;
    class function Create(const R, G, B: CGFloat; const A: CGFloat = 1): TAlphaColorCGFloat; overload; static; inline;
    class function Create(const Color: TAlphaColor): TAlphaColorCGFloat; overload; static; inline;
    class function Create(const Color: TAlphaColorF): TAlphaColorCGFloat; overload; static; inline;
  end;

procedure ALGradientEvaluateCallback(info: Pointer; inData: PCGFloat; outData: PAlphaColorCGFloat); cdecl;
{$ENDIF}

{*************************}
procedure ALPaintRectangle(
            {$IF defined(ANDROID)}
            const aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            const aContext: CGContextRef;
            const aColorSpace: CGColorSpaceRef;
            const aGridHeight: Single;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            const aCanvas: Tcanvas;
            {$ENDIF}
            const dstRect: TrectF;
            const FillColor: TAlphaColor;
            const StrokeColor: TalphaColor;
            const StrokeThickness: Single;
            const ShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
            const shadowBlur: Single;
            const shadowOffsetX: Single;
            const shadowOffsetY: Single;
            const Sides: TSides;
            const Corners: TCorners;
            const XRadius: Single;
            const YRadius: Single); overload;
procedure ALPaintRectangle(
            {$IF defined(ANDROID)}
            const aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            const aContext: CGContextRef;
            const aColorSpace: CGColorSpaceRef;
            const aGridHeight: Single;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            const aCanvas: Tcanvas;
            {$ENDIF}
            const dstRect: TrectF;
            const Fill: TBrush;
            const Stroke: TStrokeBrush;
            const Shadow: TALShadow; // If shadow is not nil, then the Canvas must have enough space to draw the shadow (approximately Shadow.blur on each side of the rectangle)
            const Sides: TSides;
            const Corners: TCorners;
            const XRadius: Single;
            const YRadius: Single); overload;

{**********************}
procedure ALPaintCircle(
            {$IF defined(ANDROID)}
            const aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            const aContext: CGContextRef;
            const aColorSpace: CGColorSpaceRef;
            const aGridHeight: Single;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            const aCanvas: Tcanvas;
            {$ENDIF}
            const dstRect: TrectF;
            const FillColor: TAlphaColor;
            const StrokeColor: TalphaColor;
            const StrokeThickness: Single;
            const ShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
            const shadowBlur: Single;
            const shadowOffsetX: Single;
            const shadowOffsetY: Single); overload;
procedure ALPaintCircle(
            {$IF defined(ANDROID)}
            const aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            const aContext: CGContextRef;
            const aColorSpace: CGColorSpaceRef;
            const aGridHeight: Single;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            const aCanvas: Tcanvas;
            {$ENDIF}
            const dstRect: TrectF;
            const Fill: TBrush;
            const Stroke: TStrokeBrush;
            const Shadow: TALShadow); overload; // If shadow is not nil, then the Canvas must have enough space to draw the shadow (approximately Shadow.blur on each side of the circle)

{*******************************}
Procedure ALCreateDrawingSurface(
            {$IF defined(ANDROID)}
            Var aBitmap: Jbitmap;
            var aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            var aBitmapSurface: TbitmapSurface;
            Var aContext: CGContextRef;
            Var aColorSpace: CGColorSpaceRef;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            Var aBitmap: Tbitmap;
            const aClearBitmap: boolean;
            {$ENDIF}
            const w: integer;
            const h: integer);
procedure ALFreeDrawingSurface(
            {$IF defined(ANDROID)}
            Var aBitmap: Jbitmap;
            var aCanvas: Jcanvas
            {$ELSEIF defined(IOS)}
            var aBitmapSurface: TbitmapSurface;
            Var aContext: CGContextRef;
            Var aColorSpace: CGColorSpaceRef
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            Var aBitmap: Tbitmap
            {$ENDIF});
{$IF defined(IOS)}
Procedure ALCreateDrawingSurfaceV2(
            var aBitmapSurface: TbitmapSurface;
            Var aContext: CGContextRef;
            Var aColorSpace: CGColorSpaceRef;
            const w: integer;
            const h: integer);
procedure ALFreeDrawingSurfaceV2(
            var aBitmapSurface: TbitmapSurface;
            Var aContext: CGContextRef);
{$ENDIF}

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
  System.Math.Vectors,
  {$IF defined(ANDROID)}
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes, //[DCC Hint]
  Androidapi.Helpers,
  Androidapi.JNI.Media,
  Androidapi.Bitmap,
  Alcinoe.AndroidApi.Common,
  Alcinoe.FMX.Types3D,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.UIKit,
  iOSapi.Foundation,
  iOSapi.CoreImage,
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Alcinoe.iOSApi.ImageIO,
  Alcinoe.FMX.Types3D,
  {$ENDIF}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  FMX.Effects,
  {$ENDIF}
  {$IFDEF ALUseTexture}
  FMX.Canvas.GPU,
  {$ENDIF}
  System.UIConsts;

{********************}
{$IF defined(ANDROID)}
function ALJBitmaptoTexture(const aBitmap: Jbitmap): TTexture;
begin
  result := TALTexture.Create;
  try
    TALTexture(result).Assign(aBitmap);
  except
    ALFreeAndNil(result);
    raise;
  end;
end;
{$ENDIF}

{*******************}
{$IFDEF ALUseTexture}
function ALBitmapSurfacetoTexture(const aBitmapSurface: TbitmapSurface): TTexture;
begin
  result := TALTexture.Create;
  try
    result.Assign(aBitmapSurface);
  except
    ALFreeAndNil(result);
    raise;
  end;
end;
{$ENDIF}

{*******************}
{$IFDEF ALUseTexture}
function ALTransformBitmaptoTexture(var aBitmap: Tbitmap): TTexture;
var LBitmapSurface: TbitmapSurface;
    LPaintingTexture: TTexture;
begin

  //If TCustomCanvasGpu then simply move the textureID to the result
  if aBitmap.CanvasClass.InheritsFrom(TCustomCanvasGpu) then begin

    //TBitmap.image = TBitmapImage
    //TBitmap.image.handle = TBitmapCtx (but casted as THandle)
    LPaintingTexture := TBitmapCtx(aBitmap.Handle).PaintingTexture;
    Result := TalTexture.Create;
    try

      //assign LPaintingTexture to Result
      Result.assign(LPaintingTexture);

      //set the handle of aTmpTexture to 0 to avoid the
      //textureID to be deleted from OpenGL when we will free aBitmap
      ITextureAccess(LPaintingTexture).Handle := 0;

    except
      ALFreeAndNil(Result);
      raise;
    end;

  end

  //else use a LBitmapSurface to transfert the bitmap to the texture
  else begin

    LBitmapSurface := TbitmapSurface.create;
    try
      LBitmapSurface.Assign(aBitmap);
      result := ALBitmapSurfacetoTexture(LBitmapSurface);
    finally
      alfreeAndNil(LBitmapSurface);
    end;

  end;

  //free the aBitmap as we extract the texture from it
  alFreeAndNil(aBitmap);

end;
{$ENDIF}

{***********************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): Tbitmap;
var LBitmap: TBitmap;
begin

  LBitmap := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TbrushKind.Bitmap;
        Result.Canvas.FillRect(
          TRectF.Create(0,0, W, H),
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

{************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap;
var LBitmap: TBitmap;
    D, B, M: TBitmapData;
    w, h: single;
    C: TAlphaColor;
    ratio: single;
    I, J: Integer;
begin

  //init local var
  w := aMask.Width;
  h := aMask.height;

  LBitmap := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      if Result.Map(TMapAccess.Write, D) then
      try
        if LBitmap.Map(TMapAccess.Read, B) then
        try
          if aMask.Map(TMapAccess.Read, M) then
          try
            for J := 0 to Result.Height - 1 do
              for I := 0 to Result.Width - 1 do
              begin
                C := B.GetPixel(I, J);
                TAlphaColorRec(C).A := TAlphaColorRec(M.GetPixel(I, J)).A;
                if TAlphaColorRec(C).A < 255 then begin  // << don't ask me why we need to do this :(
                  ratio := TAlphaColorRec(C).A / 255;
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

{********************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap): Tbitmap;
begin
  result := ALFitIntoAndCropAsMaskImageV1(aStream, aMask, TpointF.Create(-50,-50));
end;

{****************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; const aCropCenter: TPointF): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    LCanvas: Jcanvas;
    LPaint: JPaint;
    LPorterDuffXfermode: jPorterDuffXfermode;
    w, h: Single;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try

      w := aMask.getWidth;
      h := aMask.getHeight;
      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter);
      LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
      LJSrcRect := TJRect.JavaClass.init(round(LSrcRect.left), round(LSrcRect.top), round(LSrcRect.right), round(LSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      LPaint := TJPaint.JavaClass.init;
      LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      LCanvas := TJCanvas.JavaClass.init(result);
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      LCanvas.drawBitmap(aMask, 0{left}, 0{top}, LPaint);
      LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      LPaint.setXfermode(LPorterDuffXfermode);
      LCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
      LPorterDuffXfermode := nil;
      LJSrcRect := nil;
      LJDestRectf := nil;
      LCanvas := nil;
      LPaint := nil;

    finally
      LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALFreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    w, h: Single;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          w := CGImageGetWidth(aMask);
          h := CGImageGetHeight(aMask);
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(W), // width: The width, in pixels, of the required bitmap.
                            round(H), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextClipToMask(
                    LContext,
                    ALLowerLeftCGRect(
                      TpointF.Create(0, 0),
                      w,
                      h,
                      h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap): TALNativeBitmap;
begin
  result := ALFitIntoAndCropAsMaskImageV2(aStream, aMask, TpointF.Create(-50,-50));
end;

{***************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; const aCropCenter: TPointF): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  LTmpBitmap := ALFitIntoAndCropAsMaskImageV2(aStream, aMask, aCropCenter);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
    w, h: Single;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            w := CGImageGetWidth(aMask);
            h := CGImageGetHeight(aMask);
            //-----
            LBitmapSurface.SetSize(round(W), round(H));
            //-----
            LDestRect := TrectF.Create(0, 0, W, H);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              round(W), // width: The width, in pixels, of the required bitmap.
                              round(H), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextClipToMask(
                      LContext,
                      ALLowerLeftCGRect(
                        TpointF.Create(0, 0),
                        w,
                        h,
                        h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(
                          0-(LSrcRect.Left*LRatio),
                          0-(LSrcRect.top*LRatio)),
                        w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                        h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                        h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.assign(LBitmapSurface);
                  except
                    AlFreeAndNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            AlFreeAndNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{***********************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap): TALRasterImage;
begin
  result := ALFitIntoAndCropAsMaskImageV3(aStream, aMask, TpointF.Create(-50,-50));
end;

{*******************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap;
var LBitmap: TBitmap;
    D, B, M: TBitmapData;
    w, h: single;
    C: TAlphaColor;
    ratio: single;
    I, J: Integer;
begin

  //init local var
  w := aMask.Width;
  h := aMask.height;

  LBitmap := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      if Result.Map(TMapAccess.Write, D) then
      try
        if LBitmap.Map(TMapAccess.Read, B) then
        try
          if aMask.Map(TMapAccess.Read, M) then
          try
            for J := 0 to Result.Height - 1 do
              for I := 0 to Result.Width - 1 do
              begin
                C := B.GetPixel(I, J);
                TAlphaColorRec(C).A := TAlphaColorRec(M.GetPixel(I, J)).A;
                if TAlphaColorRec(C).A < 255 then begin  // << don't ask me why we need to do this :(
                  ratio := TAlphaColorRec(C).A / 255;
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

{***************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap;
begin
  result := ALBlurFitIntoAndCropAsMaskImageV1(aStream, aMask, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{***********************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LBitmap: Jbitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    LCanvas: Jcanvas;
    LPaint: JPaint;
    LPorterDuffXfermode: jPorterDuffXfermode;
    w, h: Single;
begin
  result := nil;
  LBitmap := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if LBitmap <> nil then begin
    try

      w := aMask.getWidth;
      h := aMask.getHeight;
      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter);
      LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
      LJSrcRect := TJRect.JavaClass.init(round(LSrcRect.left), round(LSrcRect.top), round(LSrcRect.right), round(LSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      LPaint := TJPaint.JavaClass.init;
      LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      LCanvas := TJCanvas.JavaClass.init(result);
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      LCanvas.drawBitmap(aMask, 0{left}, 0{top}, LPaint);
      LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      LPaint.setXfermode(LPorterDuffXfermode);
      LCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
      LPorterDuffXfermode := nil;
      LJSrcRect := nil;
      LJDestRectf := nil;
      LCanvas := nil;
      LPaint := nil;

    finally
      LBitmap.recycle;
      LBitmap := nil;
    end;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    w, h: Single;
    LCGImageRef: CGImageRef;
begin

  result := nil;
  LCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if LCGImageRef <> nil then begin
    try
      //-----
      w := CGImageGetWidth(aMask);
      h := CGImageGetHeight(aMask);
      //-----
      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LCGImageRef), CGImageGetHeight(LCGImageRef)), aCropCenter, LRatio);
      //-----
      LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
      if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
        try
          LContext := CGBitmapContextCreate(
                        nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                             //       memory block should be at least (bytesPerRow*height) bytes.
                             //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                             //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                        round(W), // width: The width, in pixels, of the required bitmap.
                        round(H), // height: The height, in pixels, of the required bitmap.
                        8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                           //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                           //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                           //                   chapter of Quartz 2D Programming Guide.
                           //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                        0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                           //              a value of 0 causes the value to be calculated automatically.
                           //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                        LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
          if LContext <> nil then begin
            try
              CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
              CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
              CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
              CGContextClipToMask(
                LContext,
                ALLowerLeftCGRect(
                  TpointF.Create(0, 0),
                  w,
                  h,
                  h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
              CGContextDrawImage(
                LContext, // c: The graphics context in which to draw the image.
                ALLowerLeftCGRect(
                  TpointF.Create(
                    0-(LSrcRect.Left*LRatio),
                    0-(LSrcRect.top*LRatio)),
                  w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LCGImageRef)-LSrcRect.right)*LRatio),
                  h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LCGImageRef)-LSrcRect.bottom)*LRatio),
                  h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                LCGImageRef); // image The image to draw.
              result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                              // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                              // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                              // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                              // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                              // you can avoid the actual physical copy of the data.
            finally
              CGContextRelease(LContext);
            end;
          end;
        finally
          CGColorSpaceRelease(LColorSpace);
        end;
      end;
      //-----
    finally
      CGImageRelease(LCGImageRef);
    end;
  end
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALBlurFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter, aBlurRadius, aBlurW, aBlurH);
end;
{$ENDIF}
{$ENDREGION}

{*******************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; aBlurRadius: single; const aBlurW, aBlurH: single): TALNativeBitmap;
begin
  result := ALBlurFitIntoAndCropAsMaskImageV2(aStream, aMask, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{**********************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  LTmpBitmap := ALBlurFitIntoAndCropAsMaskImageV2(aStream, aMask, aCropCenter, aBlurRadius, aBlurW, aBlurH);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
    w, h: Single;
    LCGImageRef: CGImageRef;
begin

  result := nil;
  LCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if LCGImageRef <> nil then begin
    try
      LBitmapSurface := TbitmapSurface.Create;
      try
        //-----
        w := CGImageGetWidth(aMask);
        h := CGImageGetHeight(aMask);
        //-----
        LBitmapSurface.SetSize(round(W), round(H));
        //-----
        LDestRect := TrectF.Create(0, 0, W, H);
        LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LCGImageRef), CGImageGetHeight(LCGImageRef)), aCropCenter, LRatio);
        //-----
        LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
        if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
          try
            LContext := CGBitmapContextCreate(
                          LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                               //       memory block should be at least (bytesPerRow*height) bytes.
                                               //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                               //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                          round(W), // width: The width, in pixels, of the required bitmap.
                          round(H), // height: The height, in pixels, of the required bitmap.
                          8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                             //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                             //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                             //                   chapter of Quartz 2D Programming Guide.
                             //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                          LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                //              a value of 0 causes the value to be calculated automatically.
                                                //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                          LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
            if LContext <> nil then begin

              try
                CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextClipToMask(
                  LContext,
                  ALLowerLeftCGRect(
                    TpointF.Create(0, 0),
                    w,
                    h,
                    h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                  aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                CGContextDrawImage(
                  LContext, // c: The graphics context in which to draw the image.
                  ALLowerLeftCGRect(
                    TpointF.Create(
                      0-(LSrcRect.Left*LRatio),
                      0-(LSrcRect.top*LRatio)),
                    w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LCGImageRef)-LSrcRect.right)*LRatio),
                    h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LCGImageRef)-LSrcRect.bottom)*LRatio),
                    h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                  LCGImageRef); // image The image to draw.
              finally
                CGContextRelease(LContext);
              end;

              result := TALTexture.Create;
              try
                result.assign(LBitmapSurface);
              except
                AlFreeAndNil(result);
                raise;
              end;

            end;
          finally
            CGColorSpaceRelease(LColorSpace);
          end;
        end;
      finally
        AlFreeAndNil(LBitmapSurface);
      end;
    finally
      CGImageRelease(LCGImageRef);
    end;
  end
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALBlurFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter, aBlurRadius, aBlurW, aBlurH);
end;
{$ENDIF}
{$ENDREGION}

{******************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: TALNativeBitmap; aBlurRadius: single; const aBlurW, aBlurH: single): TALRasterImage;
begin
  result := ALBlurFitIntoAndCropAsMaskImageV3(aStream, aMask, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{*************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV1(LStream, aMask, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV1(LStream, aMask);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: TALNativeBitmap; const aCropCenter: TPointF): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV2(LStream, aMask, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: TALNativeBitmap): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV2(LStream, aMask);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: TALNativeBitmap; const aCropCenter: TPointF): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV3(LStream, aMask, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: TALNativeBitmap): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV3(LStream, aMask);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*******************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): Tbitmap;
begin
  result := ALFitIntoAndCropAsRoundRectImageV1(aStream, w, h, XRadius, YRadius, TpointF.Create(-50,-50));
end;

{*******************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    LCanvas: Jcanvas;
    LPaint: JPaint;
    LPorterDuffXfermode: jPorterDuffXfermode;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try

      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter);
      LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
      LJSrcRect := TJRect.JavaClass.init(round(LSrcRect.left), round(LSrcRect.top), round(LSrcRect.right), round(LSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      LPaint := TJPaint.JavaClass.init;
      LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      LCanvas := TJCanvas.JavaClass.init(result);
      LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      LCanvas.drawRoundRect(
        LJDestRectf{rect},
        xRadius {rx},
        yRadius {ry},
        LPaint);
      LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      LPaint.setXfermode(LPorterDuffXfermode);
      LCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
      LPorterDuffXfermode := nil;
      LJSrcRect := nil;
      LJDestRectf := nil;
      LCanvas := nil;
      LPaint := nil;

    finally
      LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALFreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LXRadius: single;
    LYRadius: Single;
    LWidthMinusCorners: single;
    LHeightMinusCorners: Single;
    LCurPoint: TpointF;
    LGridHeight: single;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(LContext, X, LGridHeight - Y);
      LCurPoint.X := x;
      LCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(
        LContext,
        LCurPoint.X + dx1{cpx},
        LGridHeight - (LCurPoint.Y + dy1){cpy},
        LCurPoint.X + dx2{x},
        LGridHeight - (LCurPoint.Y + dy2){y});
      LCurPoint.X := LCurPoint.X + dx2;
      LCurPoint.Y := LCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(LContext, LCurPoint.X + dx{x}, LGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
    end;

begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(W), // width: The width, in pixels, of the required bitmap.
                            round(H), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextBeginPath(LContext);  // Creates a new empty path in a graphics context.

                  LGridHeight := H;
                  LXRadius := xRadius;
                  LYRadius := yRadius;
                  if (LXRadius > LDestRect.width / 2) then LXRadius := LDestRect.width / 2;
                  if (LYRadius > LDestRect.height / 2) then LYRadius := LDestRect.height / 2;
                  LWidthMinusCorners := (LDestRect.width - (2 * LXRadius));
                  LHeightMinusCorners := (LDestRect.height - (2 * LYRadius));

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

                  CGContextClosePath(LContext); // Closes and terminates the current path’s subpath.
                  CGContextClip(LContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                           // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                           // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                           // save the graphics state before you clip and restore the graphics state after you’ve completed
                                           // any clipped drawing.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropAsRoundRectImageV1(aStream, W, H, XRadius, YRadius, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{***************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): TALNativeBitmap;
begin
  result := ALFitIntoAndCropAsRoundRectImageV2(aStream, w, h, XRadius, YRadius, TpointF.Create(-50,-50));
end;

{******************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  LTmpBitmap := ALFitIntoAndCropAsRoundRectImageV2(aStream, W, H, XRadius, YRadius, aCropCenter);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
    LXRadius: single;
    LYRadius: Single;
    LWidthMinusCorners: single;
    LHeightMinusCorners: Single;
    LCurPoint: TpointF;
    LGridHeight: single;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(LContext, X, LGridHeight - Y);
      LCurPoint.X := x;
      LCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(
        LContext,
        LCurPoint.X + dx1{cpx},
        LGridHeight - (LCurPoint.Y + dy1){cpy},
        LCurPoint.X + dx2{x},
        LGridHeight - (LCurPoint.Y + dy2){y});
      LCurPoint.X := LCurPoint.X + dx2;
      LCurPoint.Y := LCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(LContext, LCurPoint.X + dx{x}, LGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
    end;

begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LBitmapSurface.SetSize(round(W), round(H));
            //-----
            LDestRect := TrectF.Create(0, 0, W, H);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              round(W), // width: The width, in pixels, of the required bitmap.
                              round(H), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextBeginPath(LContext);  // Creates a new empty path in a graphics context.

                    LGridHeight := H;
                    LXRadius := xRadius;
                    LYRadius := yRadius;
                    if (LXRadius > LDestRect.width / 2) then LXRadius := LDestRect.width / 2;
                    if (LYRadius > LDestRect.height / 2) then LYRadius := LDestRect.height / 2;
                    LWidthMinusCorners := (LDestRect.width - (2 * LXRadius));
                    LHeightMinusCorners := (LDestRect.height - (2 * LYRadius));

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

                    CGContextClosePath(LContext); // Closes and terminates the current path’s subpath.
                    CGContextClip(LContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(
                          0-(LSrcRect.Left*LRatio),
                          0-(LSrcRect.top*LRatio)),
                        w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                        h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                        h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.assign(LBitmapSurface);
                  except
                    AlFreeAndNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            AlFreeAndNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropAsRoundRectImageV1(aStream, W, H, XRadius, YRadius, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{**************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): TALRasterImage;
begin
  result := ALFitIntoAndCropAsRoundRectImageV3(aStream, w, h, XRadius, YRadius, TpointF.Create(-50,-50));
end;

{************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var LBitmap: TBitmap;
begin

  LBitmap := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TbrushKind.Bitmap;
        Result.Canvas.FillEllipse(TRectF.Create(0,0, W, H), 1 {AOpacity});
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

{********************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap;
begin
  result := ALFitIntoAndCropAsCircleImageV1(aStream, w, h, TpointF.Create(-50,-50));
end;

{********************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    LCanvas: Jcanvas;
    LPaint: JPaint;
    LPorterDuffXfermode: jPorterDuffXfermode;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try

      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter);
      LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
      LJSrcRect := TJRect.JavaClass.init(round(LSrcRect.left), round(LSrcRect.top), round(LSrcRect.right), round(LSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      LPaint := TJPaint.JavaClass.init;
      LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      LCanvas := TJCanvas.JavaClass.init(result);

      LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      LCanvas.drawCircle(W/2, H/2, W/2, LPaint);
      LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      LPaint.setXfermode(LPorterDuffXfermode);
      LCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
      LPorterDuffXfermode := nil;
      LJSrcRect := nil;
      LJDestRectf := nil;
      LCanvas := nil;
      LPaint := nil;

    finally
      LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALFreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(W), // width: The width, in pixels, of the required bitmap.
                            round(H), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextBeginPath(LContext);  // Creates a new empty path in a graphics context.
                  CGContextAddEllipseInRect(
                    LContext,
                    ALLowerLeftCGRect(
                      TPointF.Create(LDestRect.Left, LDestRect.Top),
                      LDestRect.Width,
                      LDestRect.Height,
                      h)); // Adds an ellipse that fits inside the specified rectangle.
                  CGContextClosePath(LContext); // Closes and terminates the current path’s subpath.
                  CGContextClip(LContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                           // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                           // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                           // save the graphics state before you clip and restore the graphics state after you’ve completed
                                           // any clipped drawing.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{****************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single): TALNativeBitmap;
begin
  result := ALFitIntoAndCropAsCircleImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{*******************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  LTmpBitmap := ALFitIntoAndCropAsCircleImageV2(aStream, W, H, aCropCenter);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LBitmapSurface.SetSize(round(W), round(H));
            //-----
            LDestRect := TrectF.Create(0, 0, W, H);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              round(W), // width: The width, in pixels, of the required bitmap.
                              round(H), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextBeginPath(LContext);  // Creates a new empty path in a graphics context.
                    CGContextAddEllipseInRect(
                      LContext,
                      ALLowerLeftCGRect(
                        TPointF.Create(LDestRect.Left, LDestRect.Top),
                        LDestRect.Width,
                        LDestRect.Height,
                        LBitmapSurface.Height)); // Adds an ellipse that fits inside the specified rectangle.
                    CGContextClosePath(LContext); // Closes and terminates the current path’s subpath.
                    CGContextClip(LContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(
                          0-(LSrcRect.Left*LRatio),
                          0-(LSrcRect.top*LRatio)),
                        w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                        h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                        h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.assign(LBitmapSurface);
                  except
                    AlFreeAndNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            AlFreeAndNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{***************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single): TALRasterImage;
begin
  result := ALFitIntoAndCropAsCircleImageV3(aStream, w, h, TpointF.Create(-50,-50));
end;

{*************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV1(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV2(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{********************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV3(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*******************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap;
var LBitmap: TBitmap;
begin

  LBitmap := ALBlurFitIntoAndCropImageV1(aStream, W, H, aCropCenter, aBlurRadius);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(LBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TbrushKind.Bitmap;
        Result.Canvas.FillEllipse(TRectF.Create(0,0, W, H), 1 {AOpacity});
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

{***************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap;
begin
  result := ALBlurFitIntoAndCropAsCircleImageV1(aStream, w, h, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{***************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LBitmap: Jbitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    LCanvas: Jcanvas;
    LPaint: JPaint;
    LPorterDuffXfermode: jPorterDuffXfermode;
begin
  result := nil;
  LBitmap := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if LBitmap <> nil then begin
    try

      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter);
      LJDestRectf := TJRectf.JavaClass.init(LDestRect.left, LDestRect.top, LDestRect.right, LDestRect.bottom);
      LJSrcRect := TJRect.JavaClass.init(round(LSrcRect.left), round(LSrcRect.top), round(LSrcRect.right), round(LSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      LPaint := TJPaint.JavaClass.init;
      LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      LCanvas := TJCanvas.JavaClass.init(result);

      LPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      LCanvas.drawCircle(W/2, H/2, W/2, LPaint);
      LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      LPaint.setXfermode(LPorterDuffXfermode);
      LCanvas.drawBitmap(LBitmap, LJSrcRect, LJDestRectf, LPaint);
      LPorterDuffXfermode := nil;
      LJSrcRect := nil;
      LJDestRectf := nil;
      LCanvas := nil;
      LPaint := nil;

    finally
      LBitmap.recycle;
      LBitmap := nil;
    end;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LCGImageRef: CGImageRef;
begin

  result := nil;
  LCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if LCGImageRef <> nil then begin
    try
      //-----
      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LCGImageRef), CGImageGetHeight(LCGImageRef)), aCropCenter, LRatio);
      //-----
      LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
      if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
        try
          LContext := CGBitmapContextCreate(
                        nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                             //       memory block should be at least (bytesPerRow*height) bytes.
                             //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                             //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                        round(W), // width: The width, in pixels, of the required bitmap.
                        round(H), // height: The height, in pixels, of the required bitmap.
                        8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                           //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                           //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                           //                   chapter of Quartz 2D Programming Guide.
                           //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                        0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                           //              a value of 0 causes the value to be calculated automatically.
                           //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                        LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
          if LContext <> nil then begin
            try
              CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
              CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
              CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
              CGContextBeginPath(LContext);  // Creates a new empty path in a graphics context.
              CGContextAddEllipseInRect(
                LContext,
                ALLowerLeftCGRect(
                  TPointF.Create(LDestRect.Left, LDestRect.Top),
                  LDestRect.Width,
                  LDestRect.Height,
                  h)); // Adds an ellipse that fits inside the specified rectangle.
              CGContextClosePath(LContext); // Closes and terminates the current path’s subpath.
              CGContextClip(LContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                       // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                       // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                       // save the graphics state before you clip and restore the graphics state after you’ve completed
                                       // any clipped drawing.
              CGContextDrawImage(
                LContext, // c: The graphics context in which to draw the image.
                ALLowerLeftCGRect(
                  TpointF.Create(
                    0-(LSrcRect.Left*LRatio),
                    0-(LSrcRect.top*LRatio)),
                  w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LCGImageRef)-LSrcRect.right)*LRatio),
                  h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LCGImageRef)-LSrcRect.bottom)*LRatio),
                  h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                LCGImageRef); // image The image to draw.
              result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                              // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                              // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                              // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                              // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                              // you can avoid the actual physical copy of the data.
            finally
              CGContextRelease(LContext);
            end;
          end;
        finally
          CGColorSpaceRelease(LColorSpace);
        end;
      end;
      //-----
    finally
      CGImageRelease(LCGImageRef);
    end;
  end
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALBlurFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter, aBlurRadius, aBlurW, aBlurH);
end;
{$ENDIF}
{$ENDREGION}

{***********************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): TALNativeBitmap;
begin
  result := ALBlurFitIntoAndCropAsCircleImageV2(aStream, w, h, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{**************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  LTmpBitmap := ALBlurFitIntoAndCropAsCircleImageV2(aStream, W, H, aCropCenter, aBlurRadius, aBlurW, aBlurH);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
    LCGImageRef: CGImageRef;
begin

  result := nil;
  LCGImageRef := ALBlurFitIntoAndCropAsCircleImageV2(aStream, W, H, aCropCenter, aBlurRadius, aBlurW, aBlurH);
  if LCGImageRef <> nil then begin
    try
      LBitmapSurface := TbitmapSurface.Create;
      try
        //-----
        LBitmapSurface.SetSize(CGImageGetWidth(LCGImageRef), CGImageGetHeight(LCGImageRef));
        //-----
        LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
        if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
          try
            LContext := CGBitmapContextCreate(
                          LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                               //       memory block should be at least (bytesPerRow*height) bytes.
                                               //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                               //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                          LBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                          LBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                          8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                             //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                             //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                             //                   chapter of Quartz 2D Programming Guide.
                             //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                          LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                //              a value of 0 causes the value to be calculated automatically.
                                                //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                          LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
            if LContext <> nil then begin

              try
                CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(
                  LContext, // c: The graphics context in which to draw the image.
                  ALLowerLeftCGRect(
                    TpointF.Create(0,0),
                    LBitmapSurface.Width,
                    LBitmapSurface.Height,
                    LBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                  LCGImageRef); // image The image to draw.
              finally
                CGContextRelease(LContext);
              end;

              result := TALTexture.Create;
              try
                result.Assign(LBitmapSurface);
              except
                ALfreeandNil(result);
                raise;
              end;

            end;
          finally
            CGColorSpaceRelease(LColorSpace);
          end;
        end;
      finally
        ALfreeandNil(LBitmapSurface);
      end;
    finally
      CGImageRelease(LCGImageRef);
    end;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALBlurFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter, aBlurRadius, aBlurW, aBlurH);
end;
{$ENDIF}
{$ENDREGION}

{**********************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): TALRasterImage;
begin
  result := ALBlurFitIntoAndCropAsCircleImageV3(aStream, w, h, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{*****************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap;
var LBitmap: TBitmap;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.width, LBitmap.height));

    Result := TBitmap.Create(round(LDestSize.x),round(LDestSize.y));
    try

      LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.Width, LBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{****************************************************************************************************************************}
function ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var LBitmap: TBitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.Width, LBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{************************************************************************************************}
function ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap;
begin
  result := ALFitIntoAndCropImageV1(aStream, w, h, TpointF.Create(-50,-50));
end;

{*************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LRatio: single;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: Trect;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.getwidth, LBitmap.getheight));
      LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter, LRatio).round;
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
      LMatrix := nil;
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)));
          LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(LDestSize.x), // width: The width, in pixels, of the required bitmap.
                            round(LDestSize.y), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      LDestSize.x + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      LDestSize.y + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      LDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{************************************************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: Trect;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter, LRatio).round;
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
      LMatrix := nil;
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(W), // width: The width, in pixels, of the required bitmap.
                            round(H), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{********************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single): TALNativeBitmap;
begin
  result := ALFitIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALFitIntoAndCropImageV2(aStream, aGetDestSizeFunct, aCropCenter);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)));
            LBitmapSurface.SetSize(round(LDestSize.x), round(LDestSize.y));
            //-----
            LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              round(LDestSize.x), // width: The width, in pixels, of the required bitmap.
                              round(LDestSize.y), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(
                          0-(LSrcRect.Left*LRatio),
                          0-(LSrcRect.top*LRatio)),
                        LDestSize.x + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                        LDestSize.y + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                        LDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}


{***********************************************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALFitIntoAndCropImageV2(aStream, W, H, aCropCenter);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LBitmapSurface.SetSize(round(W), round(H));
            //-----
            LDestRect := TrectF.Create(0, 0, W, H);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              round(W), // width: The width, in pixels, of the required bitmap.
                              round(H), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(
                          0-(LSrcRect.Left*LRatio),
                          0-(LSrcRect.top*LRatio)),
                        w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                        h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                        h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{*******************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single): TALRasterImage;
begin
  result := ALFitIntoAndCropImageV3(aStream, w, h, TpointF.Create(-50,-50));
end;

{****************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap;
var LBitmap: TBitmap;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LRadius: single;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.width, LBitmap.height), LRadius);

    Result := TBitmap.Create(round(LDestSize.x),round(LDestSize.y));
    try

      LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.Width, LBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{*************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): Tbitmap;
var LBitmap: TBitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.Width, LBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{*********************************************************************************************************************}
function ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): Tbitmap;
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, w, h, TpointF.Create(-50,-50), aRadius);
end;

{************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LRatio: single;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: Trect;
    LRS: JRenderScript;
    LInput: JAllocation;
    LOutPut: JAllocation;
    LScript: JScriptIntrinsicBlur;
    LTmpRadius: Single;
    LRadius: single;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try

      LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.getwidth, LBitmap.getheight), LRadius);
      LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter, LRatio).round;
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
      LMatrix := nil;

      LRS := getRenderScript;
      while compareValue(LRadius, 0, Tepsilon.Vector) > 0 do begin
        LInput := TJAllocation.JavaClass.createFromBitmap(LRS, result);
        LOutPut := TJAllocation.JavaClass.createTyped(LRS, LInput.getType());
        LScript :=  TJScriptIntrinsicBlur.javaclass.create(LRS, TJElement.javaclass.U8_4(LRS));
        LTmpRadius := Min(25, LRadius);
        LRadius := LRadius - LTmpRadius;
        LScript.setRadius(LTmpRadius);
        LScript.setInput(LInput);
        LScript.forEach(LOutPut);
        LOutPut.copyTo(result);
        LScript := nil;
        LInput := nil;
        LOutPut := nil;
      end;
      LRS := nil;

    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LCGImageRef: CGImageRef;
    LCIContext: CIContext;
    LCIImage: CIImage;
    LClampFilter: CIFilter;
    LBlurFilter : CIFilter;
    LRadius: single;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), LRadius);
          LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(LDestSize.x), // width: The width, in pixels, of the required bitmap.
                            round(LDestSize.y), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      LDestSize.x + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      LDestSize.y + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      LDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  LCGImageRef := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                       // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                       // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                       // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                       // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                       // you can avoid the actual physical copy of the data.
                  try

                    LCIContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
                    LCIImage := TCIImage.Wrap(TCIImage.OCClass.imageWithCGImage(LCGImageRef));

                    LClampFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
                    LClampFilter.setDefaults;
                    LClampFilter.setValueforKey((LCIImage as ILocalObject).getObjectId, kCIInputImageKey);

                    LBlurFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
                    LBlurFilter.setValueforKey((LClampFilter.outputImage as ILocalObject).getObjectId, kCIInputImageKey);
                    LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(LRadius), kCIInputRadiusKey);

                    result := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);

                    LCIImage := nil; // no need to call LCIImage.release; (i try => exception)
                    LBlurFilter := nil; // no need to call LBlurFilter.release (i try => exception)
                    LClampFilter := nil; // no need to call LClampFilter.release (i try => exception)
                    LCIContext := nil; // no need to call LCIContext.release; (i try => exception)

                  finally
                    CGImageRelease(LCGImageRef);
                  end;
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{*********************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: Trect;
    LRS: JRenderScript;
    LInput: JAllocation;
    LOutPut: JAllocation;
    LScript: JScriptIntrinsicBlur;
    LTmpRadius: Single;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try

      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter, LRatio).round;
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
      LMatrix := nil;

      LRS := getRenderScript;
      while compareValue(aRadius, 0, Tepsilon.Vector) > 0 do begin
        LInput := TJAllocation.JavaClass.createFromBitmap(LRS, result);
        LOutPut := TJAllocation.JavaClass.createTyped(LRS, LInput.getType());
        LScript :=  TJScriptIntrinsicBlur.javaclass.create(LRS, TJElement.javaclass.U8_4(LRS));
        LTmpRadius := Min(25, aRadius);
        aRadius := aRadius - LTmpRadius;
        LScript.setRadius(LTmpRadius);
        LScript.setInput(LInput);
        LScript.forEach(LOutPut);
        LOutPut.copyTo(result);
        LScript := nil;
        LInput := nil;
        LOutPut := nil;
      end;
      LRS := nil;

    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LCGImageRef: CGImageRef;
    LCIContext: CIContext;
    LCIImage: CIImage;
    LClampFilter: CIFilter;
    LBlurFilter : CIFilter;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(W), // width: The width, in pixels, of the required bitmap.
                            round(H), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  LCGImageRef := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                       // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                       // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                       // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                       // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                       // you can avoid the actual physical copy of the data.
                  try

                    LCIContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
                    LCIImage := TCIImage.Wrap(TCIImage.OCClass.imageWithCGImage(LCGImageRef));

                    LClampFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
                    LClampFilter.setDefaults;
                    LClampFilter.setValueforKey((LCIImage as ILocalObject).getObjectId, kCIInputImageKey);

                    LBlurFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
                    LBlurFilter.setValueforKey((LClampFilter.outputImage as ILocalObject).getObjectId, kCIInputImageKey);
                    LBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aRadius), kCIInputRadiusKey);

                    result := LCIContext.createCGImage(LBlurFilter.outputImage, LCIImage.extent);

                    LCIImage := nil; // no need to call LCIImage.release; (i try => exception)
                    LBlurFilter := nil; // no need to call LBlurFilter.release (i try => exception)
                    LClampFilter := nil; // no need to call LClampFilter.release (i try => exception)
                    LCIContext := nil; // no need to call LCIContext.release; (i try => exception)

                  finally
                    CGImageRelease(LCGImageRef);
                  end;
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, W, H, aCropCenter, aRadius);
end;
{$ENDIF}
{$ENDREGION}

{*****************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): TALNativeBitmap;
begin
  result := ALBlurFitIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50), aRadius);
end;

{***********************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALBlurFitIntoAndCropImageV2(aStream, aGetDestSizeFunct, aCropCenter);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
    LCGImageRef: CGImageRef;
begin

  result := nil;
  LCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, aGetDestSizeFunct, aCropCenter);
  if LCGImageRef <> nil then begin
    try
      LBitmapSurface := TbitmapSurface.Create;
      try
        //-----
        LBitmapSurface.SetSize(CGImageGetWidth(LCGImageRef), CGImageGetHeight(LCGImageRef));
        //-----
        LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
        if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
          try
            LContext := CGBitmapContextCreate(
                          LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                               //       memory block should be at least (bytesPerRow*height) bytes.
                                               //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                               //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                          LBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                          LBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                          8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                             //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                             //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                             //                   chapter of Quartz 2D Programming Guide.
                             //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                          LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                //              a value of 0 causes the value to be calculated automatically.
                                                //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                          LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
            if LContext <> nil then begin

              try
                CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(
                  LContext, // c: The graphics context in which to draw the image.
                  ALLowerLeftCGRect(
                    TpointF.Create(0,0),
                    LBitmapSurface.Width,
                    LBitmapSurface.Height,
                    LBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                  LCGImageRef); // image The image to draw.
              finally
                CGContextRelease(LContext);
              end;

              result := TALTexture.Create;
              try
                result.Assign(LBitmapSurface);
              except
                ALfreeandNil(result);
                raise;
              end;

            end;
          finally
            CGColorSpaceRelease(LColorSpace);
          end;
        end;
      finally
        ALfreeandNil(LBitmapSurface);
      end;
    finally
      CGImageRelease(LCGImageRef);
    end;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}


{********************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALBlurFitIntoAndCropImageV2(aStream, W, H, aCropCenter, aRadius);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
    LCGImageRef: CGImageRef;
begin

  result := nil;
  LCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, W, H, aCropCenter, aRadius);
  if LCGImageRef <> nil then begin
    try
      LBitmapSurface := TbitmapSurface.Create;
      try
        //-----
        LBitmapSurface.SetSize(CGImageGetWidth(LCGImageRef), CGImageGetHeight(LCGImageRef));
        //-----
        LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
        if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
          try
            LContext := CGBitmapContextCreate(
                          LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                               //       memory block should be at least (bytesPerRow*height) bytes.
                                               //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                               //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                          LBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                          LBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                          8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                             //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                             //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                             //                   chapter of Quartz 2D Programming Guide.
                             //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                          LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                //              a value of 0 causes the value to be calculated automatically.
                                                //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                          LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
            if LContext <> nil then begin

              try
                CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(
                  LContext, // c: The graphics context in which to draw the image.
                  ALLowerLeftCGRect(
                    TpointF.Create(0,0),
                    LBitmapSurface.Width,
                    LBitmapSurface.Height,
                    LBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                  LCGImageRef); // image The image to draw.
              finally
                CGContextRelease(LContext);
              end;

              result := TALTexture.Create;
              try
                result.Assign(LBitmapSurface);
              except
                ALfreeandNil(result);
                raise;
              end;

            end;
          finally
            CGColorSpaceRelease(LColorSpace);
          end;
        end;
      finally
        ALfreeandNil(LBitmapSurface);
      end;
    finally
      CGImageRelease(LCGImageRef);
    end;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, W, H, aCropCenter, aRadius);
end;
{$ENDIF}
{$ENDREGION}

{****************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): TALRasterImage;
begin
  result := ALBlurFitIntoAndCropImageV3(aStream, w, h, TpointF.Create(-50,-50), aRadius);
end;

{****************************************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV1(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{************************************************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV2(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{********************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV3(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{********************************************************************************************************}
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************************************}
function ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV1(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************}
function ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************}
function ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single; const aCropCenter: TPointF): TALNativeBitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV2(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************}
function ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single): TALNativeBitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************}
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single; const aCropCenter: TPointF): TALRasterImage;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV3(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************}
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single): TALRasterImage;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*******************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap;
var LBitmap: TBitmap;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LRatio: Single;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.width, LBitmap.height));
    if (LDestSize.X > LBitmap.width) and (LDestSize.Y > LBitmap.height) then begin
      if (LDestSize.X / LBitmap.width) > (LDestSize.Y / LBitmap.height) then LRatio := LDestSize.X / LBitmap.width
      else LRatio := LDestSize.Y / LBitmap.height;
      LDestSize := LDestSize / LRatio;
    end;

    Result := TBitmap.Create(round(LDestSize.x),round(LDestSize.y));
    try

      LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.Width, LBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{************************************************************************************************************************}
function ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): Tbitmap;
var LBitmap: TBitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LRatio: Single;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    if (W > LBitmap.width) and (H > LBitmap.height) then begin
      if (W / LBitmap.width) > (H / LBitmap.height) then LRatio := W / LBitmap.width
      else LRatio := H / LBitmap.height;
      W := W / LRatio;
      H := H / LRatio;
    end;

    Result := TBitmap.Create(round(W),round(H));
    try

      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.Width, LBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{********************************************************************************************}
function ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single): Tbitmap;
begin
  result := ALPlaceIntoAndCropImageV1(aStream, w, h, TpointF.Create(-50,-50));
end;

{***************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LRatio: single;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: Trect;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.getwidth, LBitmap.getheight));
      if (LDestSize.X > LBitmap.getwidth) and (LDestSize.Y > LBitmap.getheight) then begin
        if (LDestSize.X / LBitmap.getwidth) > (LDestSize.Y / LBitmap.getheight) then LRatio := LDestSize.X / LBitmap.getwidth
        else LRatio := LDestSize.Y / LBitmap.getheight;
        LDestSize := LDestSize / LRatio;
      end;
      LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter, LRatio).round;
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
      LMatrix := nil;
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)));
          if (LDestSize.X > CGImageGetWidth(LImage.cgImage)) and (LDestSize.Y > CGImageGetHeight(LImage.cgImage)) then begin
            if (LDestSize.X / CGImageGetWidth(LImage.cgImage)) > (LDestSize.Y / CGImageGetHeight(LImage.cgImage)) then LRatio := LDestSize.X / CGImageGetWidth(LImage.cgImage)
            else LRatio := LDestSize.Y / CGImageGetHeight(LImage.cgImage);
            LDestSize := LDestSize / LRatio;
          end;
          LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(LDestSize.x), // width: The width, in pixels, of the required bitmap.
                            round(LDestSize.y), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      LDestSize.x + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      LDestSize.y + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      LDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALPlaceIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{********************************************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: Trect;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      if (W > LBitmap.getwidth) and (H > LBitmap.getheight) then begin
        if (W / LBitmap.getwidth) > (H / LBitmap.getheight) then LRatio := W / LBitmap.getwidth
        else LRatio := H / LBitmap.getheight;
        W := W / LRatio;
        H := H / LRatio;
      end;
      LDestRect := TrectF.Create(0, 0, W, H);
      LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight), aCropCenter, LRatio).round;
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, LSrcRect.Left{X}, LSrcRect.top{Y}, LSrcRect.width{Width}, LSrcRect.height{height}, LMatrix{m}, True{filter});
      LMatrix := nil;
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          if (W > CGImageGetWidth(LImage.cgImage)) and (H > CGImageGetHeight(LImage.cgImage)) then begin
            if (W / CGImageGetWidth(LImage.cgImage)) > (H / CGImageGetHeight(LImage.cgImage)) then LRatio := W / CGImageGetWidth(LImage.cgImage)
            else LRatio := H / CGImageGetHeight(LImage.cgImage);
            W := W / LRatio;
            H := H / LRatio;
          end;
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            round(W), // width: The width, in pixels, of the required bitmap.
                            round(H), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(
                        0-(LSrcRect.Left*LRatio),
                        0-(LSrcRect.top*LRatio)),
                      w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                      h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                      h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALPlaceIntoAndCropImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{****************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single): TALNativeBitmap;
begin
  result := ALPlaceIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{**************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALPlaceIntoAndCropImageV2(aStream, aGetDestSizeFunct, aCropCenter);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)));
            if (LDestSize.X > CGImageGetWidth(LImage.cgImage)) and (LDestSize.Y > CGImageGetHeight(LImage.cgImage)) then begin
              if (LDestSize.X / CGImageGetWidth(LImage.cgImage)) > (LDestSize.Y / CGImageGetHeight(LImage.cgImage)) then LRatio := LDestSize.X / CGImageGetWidth(LImage.cgImage)
              else LRatio := LDestSize.Y / CGImageGetHeight(LImage.cgImage);
              LDestSize := LDestSize / LRatio;
            end;
            LBitmapSurface.SetSize(round(LDestSize.x), round(LDestSize.y));
            //-----
            LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              round(LDestSize.x), // width: The width, in pixels, of the required bitmap.
                              round(LDestSize.y), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(
                          0-(LSrcRect.Left*LRatio),
                          0-(LSrcRect.top*LRatio)),
                        LDestSize.x + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                        LDestSize.y + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                        LDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALPlaceIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}


{*******************************************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALPlaceIntoAndCropImageV2(aStream, W, H, aCropCenter);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LRatio: single;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            if (W > CGImageGetWidth(LImage.cgImage)) and (H > CGImageGetHeight(LImage.cgImage)) then begin
              if (W / CGImageGetWidth(LImage.cgImage)) > (H / CGImageGetHeight(LImage.cgImage)) then LRatio := W / CGImageGetWidth(LImage.cgImage)
              else LRatio := H / CGImageGetHeight(LImage.cgImage);
              W := W / LRatio;
              H := H / LRatio;
            end;
            LBitmapSurface.SetSize(round(W), round(H));
            //-----
            LDestRect := TrectF.Create(0, 0, W, H);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              round(W), // width: The width, in pixels, of the required bitmap.
                              round(H), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(
                          0-(LSrcRect.Left*LRatio),
                          0-(LSrcRect.top*LRatio)),
                        w + (LSrcRect.Left*LRatio) + ((CGImageGetWidth(LImage.cgImage)-LSrcRect.right)*LRatio),
                        h + (LSrcRect.top*LRatio)  + ((CGImageGetHeight(LImage.cgImage)-LSrcRect.bottom)*LRatio),
                        h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALPlaceIntoAndCropImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{***************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single): TALRasterImage;
begin
  result := ALPlaceIntoAndCropImageV3(aStream, w, h, TpointF.Create(-50,-50));
end;

{************************************************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single; const aCropCenter: TPointF): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV1(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{********************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{********************************************************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single; const aCropCenter: TPointF): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV2(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{********************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single; const aCropCenter: TPointF): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV3(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************************************************}
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single; const aCropCenter: TPointF): Tbitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV1(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single): Tbitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single; const aCropCenter: TPointF): TALNativeBitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV2(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single): TALNativeBitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single; const aCropCenter: TPointF): TALRasterImage;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV3(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************}
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single): TALRasterImage;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{******************************************************************************************************************************}
function ALFitIntoImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap;
var LBitmap: TBitmap;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.width, LBitmap.height));
    LSrcRect := TrectF.Create(0, 0, LBitmap.width, LBitmap.height);
    LDestRect := LSrcRect.
                   FitInto(
                     TrectF.Create(0, 0, LDestSize.x, LDestSize.y));
    LDestRect.Offset(-LDestRect.TopLeft);

    Result := TBitmap.Create(ceil(LDestRect.Width),ceil(LDestRect.Height));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{*****************************************************************************************}
function ALFitIntoImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap;
var LBitmap: TBitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    LSrcRect := TrectF.Create(0, 0, LBitmap.width, LBitmap.height);
    LDestRect := LSrcRect.
                   FitInto(
                     TrectF.Create(0, 0, w, h));
    LDestRect.Offset(-LDestRect.TopLeft);

    Result := TBitmap.Create(ceil(LDestRect.Width),ceil(LDestRect.Height));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{**************************************************************************************************************************************}
function ALFitIntoImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: Trectf;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.getwidth, LBitmap.getheight));
      LSrcRect := TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
      LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, LDestSize.x, LDestSize.y));
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, round(LSrcRect.Left){X}, round(LSrcRect.top){Y}, round(LSrcRect.width){Width}, round(LSrcRect.height){height}, LMatrix{m}, True{filter});
      LMatrix := nil;
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)));
          LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage));
          LDestRect := LSrcRect.
                         FitInto(
                           TrectF.Create(0, 0, LDestSize.x, LDestSize.y));
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            ceil(LDestRect.width), // width: The width, in pixels, of the required bitmap.
                            ceil(LDestRect.height), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(0,0),
                      LDestRect.width,
                      LDestRect.Height,
                      ceil(LDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoImageV1(aStream, aGetDestSizeFunct);
end;
{$ENDIF}
{$ENDREGION}

{*************************************************************************************************}
function ALFitIntoImageV2(const aStream: TCustomMemoryStream; const W, H: single): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LDestRect: TrectF;
    LSrcRect: Trectf;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      LSrcRect := TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
      LDestRect := LSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, round(LSrcRect.Left){X}, round(LSrcRect.top){Y}, round(LSrcRect.width){Width}, round(LSrcRect.height){height}, LMatrix{m}, True{filter});
      LMatrix := nil;
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage));
          LDestRect := LSrcRect.
                         FitInto(
                           TrectF.Create(0, 0, W, H));
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            ceil(LDestRect.width), // width: The width, in pixels, of the required bitmap.
                            ceil(LDestRect.height), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(0,0),
                      LDestRect.width,
                      LDestRect.Height,
                      ceil(LDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoImageV1(aStream, W, H);
end;
{$ENDIF}
{$ENDREGION}

{*************************************************************************************************************************************}
function ALFitIntoImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALFitIntoImageV2(aStream, aGetDestSizeFunct);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)));
            LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage));
            LDestRect := LSrcRect.
                           FitInto(
                             TrectF.Create(0, 0, LDestSize.x, LDestSize.y));
            //-----
            LBitmapSurface.SetSize(ceil(LDestRect.width), ceil(LDestRect.height));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              ceil(LDestRect.width), // width: The width, in pixels, of the required bitmap.
                              ceil(LDestRect.height), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(0,0),
                        LDestRect.width,
                        LDestRect.Height,
                        ceil(LDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoImageV1(aStream, aGetDestSizeFunct);
end;
{$ENDIF}
{$ENDREGION}


{************************************************************************************************}
function ALFitIntoImageV3(const aStream: TCustomMemoryStream; const W, H: single): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALFitIntoImageV2(aStream, W, H);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage));
            LDestRect := LSrcRect.
                           FitInto(
                             TrectF.Create(0, 0, W, H));
            //-----
            LBitmapSurface.SetSize(ceil(LDestRect.width), ceil(LDestRect.height));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              ceil(LDestRect.width), // width: The width, in pixels, of the required bitmap.
                              ceil(LDestRect.height), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(0,0),
                        LDestRect.width,
                        LDestRect.Height,
                        ceil(LDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALFitIntoImageV1(aStream, W, H);
end;
{$ENDIF}
{$ENDREGION}

{*****************************************************************************************}
function ALLoadFitIntoResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************}
function ALLoadFitIntoResourceImageV2(const aResName: String; const W, H: single): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************}
function  ALLoadFitIntoResourceImageV3(const aResName: String; const W, H: single): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**************************************************************************************}
function ALLoadFitIntoFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************}
function ALLoadFitIntoFileImageV2(const aFileName: String; const W, H: single): TALNativeBitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************}
function  ALLoadFitIntoFileImageV3(const aFileName: String; const W, H: single): TALRasterImage;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALFitIntoImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{******************************************************************************************************************************}
function ALStretchImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap;
var LBitmap: TBitmap;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.width, LBitmap.height));
    LSrcRect := TrectF.Create(0, 0, LBitmap.width, LBitmap.height);
    LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);

    Result := TBitmap.Create(ceil(LDestRect.Width),ceil(LDestRect.Height));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{*****************************************************************************************}
function ALStretchImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap;
var LBitmap: TBitmap;
    LDestRect: TrectF;
    LSrcRect: TrectF;
begin

  LBitmap := Tbitmap.CreateFromStream(aStream);
  try

    LSrcRect := TrectF.Create(0, 0, LBitmap.width, LBitmap.height);
    LDestRect := TrectF.Create(0, 0, w, h);

    Result := TBitmap.Create(ceil(LDestRect.Width),ceil(LDestRect.Height));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(
          LBitmap, // const ABitmap: TBitmap;
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

  finally
    AlFreeAndNil(LBitmap);
  end;

end;

{**************************************************************************************************************************************}
function ALStretchImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: Trectf;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      LDestSize := aGetDestSizeFunct(TpointF.create(LBitmap.getwidth, LBitmap.getheight));
      LSrcRect := TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
      LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, round(LSrcRect.Left){X}, round(LSrcRect.top){Y}, round(LSrcRect.width){Width}, round(LSrcRect.height){height}, LMatrix{m}, True{filter});
      LMatrix := nil;
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)));
          LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage));
          LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            ceil(LDestRect.width), // width: The width, in pixels, of the required bitmap.
                            ceil(LDestRect.height), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(0,0),
                      LDestRect.width,
                      LDestRect.Height,
                      ceil(LDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALStretchImageV1(aStream, aGetDestSizeFunct);
end;
{$ENDIF}
{$ENDREGION}

{*************************************************************************************************}
function ALStretchImageV2(const aStream: TCustomMemoryStream; const W, H: single): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
    LMatrix: JMatrix;
    LDestRect: TrectF;
    LSrcRect: Trectf;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      LSrcRect := TrectF.Create(0, 0, LBitmap.getWidth, LBitmap.getHeight);
      LDestRect := TrectF.Create(0, 0, W, H);
      LMatrix := TJMatrix.JavaClass.init;
      LMatrix.postScale(LDestRect.width/LSrcRect.width, LDestRect.height/LSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(LBitmap{src}, round(LSrcRect.Left){X}, round(LSrcRect.top){Y}, round(LSrcRect.width){Width}, round(LSrcRect.height){height}, LMatrix{m}, True{filter});
      LMatrix := nil;
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage));
          LDestRect := TrectF.Create(0, 0, W, H);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(
                            nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                            ceil(LDestRect.width), // width: The width, in pixels, of the required bitmap.
                            ceil(LDestRect.height), // height: The height, in pixels, of the required bitmap.
                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                               //                   chapter of Quartz 2D Programming Guide.
                               //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                               //              a value of 0 causes the value to be calculated automatically.
                               //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                            LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
              if LContext <> nil then begin
                try
                  CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(
                    LContext, // c: The graphics context in which to draw the image.
                    ALLowerLeftCGRect(
                      TpointF.Create(0,0),
                      LDestRect.width,
                      LDestRect.Height,
                      ceil(LDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                    LImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(LContext);
                end;
              end;
            finally
              CGColorSpaceRelease(LColorSpace);
            end;
          end;
          //-----
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALStretchImageV1(aStream, W, H);
end;
{$ENDIF}
{$ENDREGION}

{*************************************************************************************************************************************}
function ALStretchImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALStretchImageV2(aStream, aGetDestSizeFunct);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LDestSize: TpointF;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LDestSize := aGetDestSizeFunct(TpointF.create(CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage)));
            LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage));
            LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
            //-----
            LBitmapSurface.SetSize(ceil(LDestRect.width), ceil(LDestRect.height));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              ceil(LDestRect.width), // width: The width, in pixels, of the required bitmap.
                              ceil(LDestRect.height), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(0,0),
                        LDestRect.width,
                        LDestRect.Height,
                        ceil(LDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALStretchImageV1(aStream, aGetDestSizeFunct);
end;
{$ENDIF}
{$ENDREGION}


{************************************************************************************************}
function ALStretchImageV3(const aStream: TCustomMemoryStream; const W, H: single): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALStretchImageV2(aStream, W, H);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LDestRect: TrectF;
    LSrcRect: TrectF;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            LSrcRect := TrectF.Create(0, 0, CGImageGetWidth(LImage.cgImage), CGImageGetHeight(LImage.cgImage));
            LDestRect := TrectF.Create(0, 0, W, H);
            //-----
            LBitmapSurface.SetSize(ceil(LDestRect.width), ceil(LDestRect.height));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              ceil(LDestRect.width), // width: The width, in pixels, of the required bitmap.
                              ceil(LDestRect.height), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin

                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(
                      LContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        TpointF.Create(0,0),
                        LDestRect.width,
                        LDestRect.Height,
                        ceil(LDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALStretchImageV1(aStream, W, H);
end;
{$ENDIF}
{$ENDREGION}

{*****************************************************************************************}
function ALLoadStretchResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALStretchImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************}
function ALLoadStretchResourceImageV2(const aResName: String; const W, H: single): TALNativeBitmap;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALStretchImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************}
function  ALLoadStretchResourceImageV3(const aResName: String; const W, H: single): TALRasterImage;
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALStretchImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**************************************************************************************}
function ALLoadStretchFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALStretchImageV1(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************}
function ALLoadStretchFileImageV2(const aFileName: String; const W, H: single): TALNativeBitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALStretchImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**********************************************************************************************}
function  ALLoadStretchFileImageV3(const aFileName: String; const W, H: single): TALRasterImage;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALStretchImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*******************************************************************************************************************************************}
function  ALLoadNormalizeOrientationImageV1(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): Tbitmap;
begin
  result := Tbitmap.CreateFromStream(aStream);
  try
    ALNormalizeImageOrientationV1(result, aExifOrientationInfo);
  except
    AlFreeAndNil(Result);
    raise;
  end;
end;

{***************************************************************************************************************************************************}
function  ALLoadNormalizeOrientationImageV2(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then Exit(nil);
    try
      result := ALNormalizeImageOrientationV2(LBitmap, aExifOrientationInfo);
    finally
      if not LBitmap.equals(result) then LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          result := ALNormalizeImageOrientationV2(LImage.CGImage, aExifOrientationInfo);
          if result = LImage.CGImage then CGImageRetain(result);
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALLoadNormalizeOrientationImageV1(aStream, aExifOrientationInfo);
end;
{$ENDIF}
{$ENDREGION}

{**************************************************************************************************************************************************}
function  ALLoadNormalizeOrientationImageV3(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): TALRasterImage;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
begin

  //create the LTmpBitmap
  LTmpBitmap := ALLoadNormalizeOrientationImageV2(aStream, aExifOrientationInfo);
  if LTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(LTmpBitmap);
  finally
    LTmpBitmap.recycle;
    LTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
    LMatrix: CGAffineTransform;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    LBitmapSurface: TBitmapSurface;
    w, h: Single;
begin
  result := nil;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          LBitmapSurface := TbitmapSurface.Create;
          try

            //-----
            w := CGImageGetWidth(LImage.CGImage);
            h := CGImageGetHeight(LImage.CGImage);
            LMatrix := CGAffineTransformIdentity;
            case aExifOrientationInfo of

              //UIImageOrientationUp: The original pixel data matches the image's intended display orientation.
              TalExifOrientationInfo.NORMAL: ;

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
              TalExifOrientationInfo.UNDEFINED: ;

            end;

            //-----
            if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                                        TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                                        TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                                        TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then LBitmapSurface.SetSize(ceil(h), ceil(w))
            else LBitmapSurface.SetSize(ceil(w), ceil(h));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(
                              LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                              round(W), // width: The width, in pixels, of the required bitmap.
                              round(H), // height: The height, in pixels, of the required bitmap.
                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                 //                   chapter of Quartz 2D Programming Guide.
                                 //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                              LBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                    //              a value of 0 causes the value to be calculated automatically.
                                                    //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                              LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
                if LContext <> nil then begin
                  try
                    CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextConcatCTM(LContext, LMatrix);
                    if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                                                TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                                                TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                                                TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then CGContextDrawImage(
                                                                                                                           LContext, // c: The graphics context in which to draw the image.
                                                                                                                           CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                                                                                                           LImage.CGImage) // image The image to draw.
                    else CGContextDrawImage(
                           LContext, // c: The graphics context in which to draw the image.
                           CGRectMake(0, 0, w, h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                           LImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(LContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(LBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(LColorSpace);
              end;
            end;
          finally
            ALfreeandNil(LBitmapSurface);
          end;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := ALLoadNormalizeOrientationImageV1(aStream, aExifOrientationInfo);
end;
{$ENDIF}
{$ENDREGION}

{********************************************************************************}
function  ALLoadNormalizeOrientationFileImageV1(const aFileName: String): Tbitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALLoadNormalizeOrientationImageV1(LStream, AlGetExifOrientationInfo(aFileName));
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************************************}
function  ALLoadNormalizeOrientationFileImageV2(const aFileName: String): TALNativeBitmap;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALLoadNormalizeOrientationImageV2(LStream, AlGetExifOrientationInfo(aFileName));
  finally
    ALfreeandNil(LStream);
  end;
end;

{***************************************************************************************}
function  ALLoadNormalizeOrientationFileImageV3(const aFileName: String): TALRasterImage;
var LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    result := ALLoadNormalizeOrientationImageV3(LStream, AlGetExifOrientationInfo(aFileName));
  finally
    ALfreeandNil(LStream);
  end;
end;

{******************************************************************}
function  ALGetImageSize(const aStream: TCustomMemoryStream): TSize;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
begin
  if aStream = nil then begin
    result := TSize.create(0,0);
    exit;
  end;
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    ALMove(aStream.Memory^, LArray.Data^, aStream.Size);
    LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(LArray, 0, aStream.Size);
    if LBitmap = nil then begin
      result := TSize.create(0,0);
      Exit;
    end;
    try
      result := TSize.Create(LBitmap.getWidth, LBitmap.getHeight);
    finally
      LBitmap.recycle;
      LBitmap := nil;
    end;
  finally
    ALfreeandNil(LArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImage: UIimage;
    LData: NSData;
begin
  if aStream = nil then begin
    result := TSize.create(0,0);
    exit;
  end;
  LData := TNSData.Wrap(
             TNSData.alloc.initWithBytesNoCopy(
               aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
               astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
               False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          Result := TSize.Create(
                      CGImageGetWidth(LImage.cgImage),
                      CGImageGetHeight(LImage.cgImage));
          exit;
        finally
          LImage.release;
        end;
      end
    end;
  finally
    LData.release;
  end;
  result := TSize.create(0,0);
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
var LBitmap: TBitmap;
begin
  if aStream = nil then begin
    result := TSize.create(0,0);
    exit;
  end;
  LBitmap := Tbitmap.CreateFromStream(aStream);
  try
    Result := TSize.Create(LBitmap.Width, LBitmap.height);
  finally
    AlFreeAndNil(LBitmap);
  end;
end;
{$ENDIF}
{$ENDREGION}

{**********************************************************************************}
function  AlGetExifOrientationInfo(const aFilename: String): TalExifOrientationInfo;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LExifInterface: JExifInterface;
    LOrientation: Integer;
begin
  LExifInterface := TJExifInterface.javaclass.init(StringToJString(aFilename));
  LOrientation := LExifInterface.getAttributeInt(TJExifInterface.JavaClass.TAG_ORIENTATION, TJExifInterface.JavaClass.ORIENTATION_NORMAL);
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
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LImgSourceRef: CGImageSourceRef;
    LPath: CFStringRef;
    LUrl: CFURLRef;
    LDictionaryRef: CFDictionaryRef;
    LOrientation: NSNumber;
begin
  result := TalExifOrientationInfo.UNDEFINED;
  LPath := CFStringCreateWithCString(nil{alloc}, MarshaledAString(UTF8Encode(AFileName)){cStr}, kCFStringEncodingUTF8{encoding});
  try
    LUrl := CFURLCreateWithFileSystemPath(nil{allocator}, LPath{filePath}, kCFURLPOSIXPathStyle{pathStyle}, False{isDirectory});
    try
      LImgSourceRef := CGImageSourceCreateWithURL(LUrl{url}, nil{options});
      if LImgSourceRef <> nil then
      try
        LDictionaryRef := CGImageSourceCopyPropertiesAtIndex(LImgSourceRef{isrc}, 0{index}, nil{options});
        if LDictionaryRef <> nil then
        try
          LOrientation := TNSNumber.Wrap(CFDictionaryGetValue(LDictionaryRef, NSStringToID(kCGImagePropertyOrientation)));
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
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := TalExifOrientationInfo.NORMAL; // << todo - https://stackoverflow.com/questions/18622152/read-exif-gps-info-using-delphi
end;
{$ENDIF}
{$ENDREGION}

{******************************************************************************************************************}
procedure ALNormalizeImageOrientationV1(const aBitmap: Tbitmap; const aExifOrientationInfo: TalExifOrientationInfo);
begin
  case aExifOrientationInfo of
    TalExifOrientationInfo.NORMAL: exit;
    TalExifOrientationInfo.FLIP_HORIZONTAL: aBitmap.FlipHorizontal;
    TalExifOrientationInfo.ROTATE_180: aBitmap.Rotate(180);
    TalExifOrientationInfo.FLIP_VERTICAL: aBitmap.FlipVertical;
    TalExifOrientationInfo.TRANSPOSE: begin
                                        aBitmap.Rotate(90);
                                        aBitmap.FlipHorizontal;
                                      end;
    TalExifOrientationInfo.ROTATE_90: aBitmap.Rotate(90);
    TalExifOrientationInfo.TRANSVERSE: begin
                                         aBitmap.Rotate(-90);
                                         aBitmap.FlipHorizontal;
                                       end;
    TalExifOrientationInfo.ROTATE_270: aBitmap.Rotate(270);
    TalExifOrientationInfo.UNDEFINED: exit;
    else exit;
  end;
end;

{*******************************************************************************************************************************************}
function  ALNormalizeImageOrientationV2(const aBitmap: TALNativeBitmap; const aExifOrientationInfo: TalExifOrientationInfo): TALNativeBitmap;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LMatrix: JMatrix;
begin
  LMatrix := TJMatrix.JavaClass.init;
  case aExifOrientationInfo of
    TalExifOrientationInfo.NORMAL: exit(aBitmap);
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
    TalExifOrientationInfo.UNDEFINED: exit(aBitmap);
    else exit(aBitmap);
  end;
  result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, 0{X}, 0{Y}, aBitmap.getwidth{Width}, aBitmap.getheight{height}, LMatrix{m}, True{filter});
  LMatrix := nil;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var LMatrix: CGAffineTransform;
    LContext: CGContextRef;
    LColorSpace: CGColorSpaceRef;
    W, H: integer;
begin

  //-----
  result := aBitmap;
  if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270,{UIImageOrientationLeft}
                              TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                              TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                              TalExifOrientationInfo.TRANSVERSE {UIImageOrientationRightMirrored}] then begin
    w := CGImageGetHeight(aBitmap);
    h := CGImageGetWidth(aBitmap);
  end
  else begin
    w := CGImageGetWidth(aBitmap);
    h := CGImageGetHeight(aBitmap);
  end;
  LMatrix := CGAffineTransformIdentity;
  case aExifOrientationInfo of

    //UIImageOrientationUp: The original pixel data matches the image's intended display orientation.
    TalExifOrientationInfo.NORMAL: exit(aBitmap);

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
    TalExifOrientationInfo.UNDEFINED: exit(aBitmap);
    else exit(aBitmap);

  end;

  //-----
  LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
  if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
    try
      LContext := CGBitmapContextCreate(
                    nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                         //       memory block should be at least (bytesPerRow*height) bytes.
                         //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                         //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                    W, // width: The width, in pixels, of the required bitmap.
                    H, // height: The height, in pixels, of the required bitmap.
                    8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                       //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                       //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                       //                   chapter of Quartz 2D Programming Guide.
                       //                   we can also use CGImageGetBitsPerComponent(LImage.CGImage) but 8 it's what we need
                    0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                       //              a value of 0 causes the value to be calculated automatically.
                       //              we could also use CGImageGetBytesPerRow(LImage.CGImage) or W * 4
                    LColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
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
      if LContext <> nil then begin
        try
          CGContextSetInterpolationQuality(LContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
          CGContextSetShouldAntialias(LContext, True); // Sets anti-aliasing on or off for a graphics context.
          CGContextSetAllowsAntialiasing(LContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
          CGContextConcatCTM(LContext, LMatrix);
          if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                                      TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                                      TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                                      TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then
            CGContextDrawImage(
              LContext, // c: The graphics context in which to draw the image.
              CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              abitmap) // image The image to draw.
          else
            CGContextDrawImage(
              LContext, // c: The graphics context in which to draw the image.
              CGRectMake(0, 0, w, h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              abitmap); // image The image to draw.
          result := CGBitmapContextCreateImage(LContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                          // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                          // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                          // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                          // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                          // you can avoid the actual physical copy of the data.
        finally
          CGContextRelease(LContext);
        end;
      end;
    finally
      CGColorSpaceRelease(LColorSpace);
    end;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
begin
  result := aBitmap;
  ALNormalizeImageOrientationV1(result, aExifOrientationInfo);
end;
{$ENDIF}
{$ENDREGION}

{**************************************************************************************************}
function  AlGetImageSignature(const aStream: TStream; const aSignatureLength: integer = 12): Tbytes;
var i: integer;
begin
  aStream.Position := 0;
  SetLength(result, aSignatureLength);
  aStream.ReadBuffer(result[0], min(length(result),aStream.Size));
  if aStream.Size < length(Result) then
    for I := aStream.Size to High(result) do
      result[i] := $00;
end;

{***************************************************************************************************}
function  AlGetImageSignature(const aFileName: string; const aSignatureLength: integer = 12): Tbytes;
var LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(aFileName, fmOpenRead);
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
function  AlDetectImageExtension(const aStream: Tstream): String;
var LFirstBytes: Tbytes;
begin

  LFirstBytes := AlGetImageSignature(aStream);
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

{****************************************************************}
function  AlDetectImageExtension(const aFileName: string): String;
var LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    result := AlDetectImageExtension(LFileStream);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{****************************************************************************************}
function  ALPrepareColor(const SrcColor: TAlphaColor; const Opacity: Single): TAlphaColor;
begin
  if Opacity < 1 then
  begin
    TAlphaColorRec(Result).R := Round(TAlphaColorRec(SrcColor).R * Opacity);
    TAlphaColorRec(Result).G := Round(TAlphaColorRec(SrcColor).G * Opacity);
    TAlphaColorRec(Result).B := Round(TAlphaColorRec(SrcColor).B * Opacity);
    TAlphaColorRec(Result).A := Round(TAlphaColorRec(SrcColor).A * Opacity);
  end
  else if (TAlphaColorRec(SrcColor).A < $FF) then
    Result := PremultiplyAlpha(SrcColor)
  else
    Result := SrcColor;
end;

{****************************************************************************************}
function  ALAlphaBlendColors(const aBackToFrontColors: array of TAlphaColor): TAlphaColor;
Var LBelow: TalphaColorF;
    LAbove: TalphaColorF;
    LMixed: TalphaColorF;
    i: integer;
begin
  if length(aBackToFrontColors) = 0 then exit(TalphaColorRec.Null);
  LMixed := TalphaColorF.Create(aBackToFrontColors[Low(aBackToFrontColors)]);
  for I := Low(aBackToFrontColors) + 1 to High(aBackToFrontColors) do begin
    LBelow := LMixed;
    LAbove := TalphaColorF.Create(aBackToFrontColors[I]);
    LMixed.a := (1-LAbove.a)*LBelow.a + LAbove.a;
    LMixed.r := ((1-LAbove.a)*LBelow.a*LBelow.r + LAbove.a*LAbove.r) / LMixed.a;
    LMixed.g := ((1-LAbove.a)*LBelow.a*LBelow.g + LAbove.a*LAbove.g) / LMixed.a;
    LMixed.b := ((1-LAbove.a)*LBelow.a*LBelow.b + LAbove.a*LAbove.b) / LMixed.a;
  end;
  result := LMixed.ToAlphaColor;
end;

{****************}
{$IF defined(IOS)}
class function TAlphaColorCGFloat.Create(const R, G, B: CGFloat; const A: CGFloat = 1): TAlphaColorCGFloat;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

{*************************************************************************************}
class function TAlphaColorCGFloat.Create(const Color: TAlphaColor): TAlphaColorCGFloat;
begin
  Result.R := TAlphaColorRec(Color).R / 255;
  Result.G := TAlphaColorRec(Color).G / 255;
  Result.B := TAlphaColorRec(Color).B / 255;
  Result.A := TAlphaColorRec(Color).A / 255;
end;

{**************************************************************************************}
class function TAlphaColorCGFloat.Create(const Color: TAlphaColorf): TAlphaColorCGFloat;
begin
  Result.R := Color.R;
  Result.G := Color.G;
  Result.B := Color.B;
  Result.A := Color.A;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}

(*
static void ALGradientEvaluateCallback(void *info, const float *in, float *out)
{
  /*
  The domain of this function is 0 - 1. For an input value of 0
  this function returns the color to paint at the start point
  of the shading. For an input value of 1 this function returns
  the color to paint at the end point of the shading. This
  is a 1 in, 4 out function where the output values correspond
  to an r,g,b,a color.

  This function evaluates to produce a blend from startColor to endColor.
  Note that the returned results are clipped to the range
  by Quartz so this function doesn't worry about values
  that are outside the range 0-1.
  */

  MyStartEndColor *startEndColorP = (MyStartEndColor * )info;
  float *startColor = startEndColorP->startColor;
  float *endColor = startEndColorP->endColor;
  float input = in[0];
  // Weight the starting and ending color components depending
  // on what position in the blend the input value specifies.
  out[0] = (startColor[0]*(1-input) + endColor[0]*input);
  out[1] = (startColor[1]*(1-input) + endColor[1]*input);
  out[2] = (startColor[2]*(1-input) + endColor[2]*input);
  // The alpha component is always 1, the shading is always opaque.
  out[3] = 1;
}
*)
procedure ALGradientEvaluateCallback(info: Pointer; inData: PCGFloat; outData: PAlphaColorCGFloat); cdecl;
begin
  if info <> nil then
    outData^ := TAlphaColorCGFloat.Create(TGradient(info).InterpolateColor(inData^));
end;
{$ENDIF}

{**********************************}
{$IFNDEF ALCompilerVersionSupported}
  {$MESSAGE WARN 'Check if FMX.Objects.GetDrawingShapeRectAndSetThickness still have the same implementation and adjust the IFDEF'}
{$ENDIF}
//duplicate of the private delphi function GetDrawingShapeRectAndSetThickness in FMX.Objects
function ALGetDrawingShapeRectAndSetThickness(
           const Rect: TrectF;
           const Fill: TBrush;
           const Stroke: TStrokeBrush;
           const Fit: Boolean;
           var FillShape, DrawShape: Boolean;
           var StrokeThicknessRestoreValue: Single): TRectF;
const
  MinRectAreaSize = 0.01;
begin
  FillShape := (Fill <> nil) and (Fill.Kind <> TBrushKind.None);
  DrawShape := (Stroke <> nil) and (Stroke.Kind <> TBrushKind.None);

  if Fit then
    Result := TRectF.Create(0, 0, 1, 1).FitInto(Rect)
  else
    Result := Rect;

  if DrawShape then
  begin
    if Result.Width < Stroke.Thickness then
    begin
      StrokeThicknessRestoreValue := Stroke.Thickness;
      FillShape := False;
      Stroke.Thickness := Min(Result.Width, Result.Height);
      Result.Left := (Result.Right + Result.Left) * 0.5;
      Result.Right := Result.Left + MinRectAreaSize;
    end
    else
      Result.Inflate(-Stroke.Thickness * 0.5, 0);

    if Result.Height < Stroke.Thickness then
    begin
      if StrokeThicknessRestoreValue < 0.0 then
        StrokeThicknessRestoreValue := Stroke.Thickness;
      FillShape := False;
      Stroke.Thickness := Min(Result.Width, Result.Height);
      Result.Top := (Result.Bottom + Result.Top) * 0.5;
      Result.Bottom := Result.Top + MinRectAreaSize;
    end
    else
      Result.Inflate(0, -Stroke.Thickness * 0.5);
  end;
end;

{*************************}
procedure ALPaintRectangle(
            {$IF defined(ANDROID)}
            const aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            const aContext: CGContextRef;
            const aColorSpace: CGColorSpaceRef;
            const aGridHeight: Single;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            const aCanvas: Tcanvas;
            {$ENDIF}
            const dstRect: TrectF;
            const FillColor: TAlphaColor;
            const StrokeColor: TalphaColor;
            const StrokeThickness: Single;
            const ShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
            const shadowBlur: Single;
            const shadowOffsetX: Single;
            const shadowOffsetY: Single;
            const Sides: TSides;
            const Corners: TCorners;
            const XRadius: Single;
            const YRadius: Single);

  {$REGION ' _drawRect (ANDROID)'}
  {$IF defined(ANDROID)}
  procedure _drawRect(
              const aCanvas: Jcanvas;
              const aPaint: JPaint;
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean);
  var LJRect: JRectF;
      LPath: JPath;
      LXRadius: single;
      LYradius: Single;
      LWidthMinusCorners: single;
      LHeightMinusCorners: Single;
      LCorners: TCorners;
      LHalfStrokeWidth: Single;
  begin

    // use drawRoundRect
    if ((compareValue(xRadius, 0, TEpsilon.position) > 0) and
        (compareValue(YRadius, 0, TEpsilon.position) > 0)) and
       (corners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
       (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
      //-----
      if (not aDrawOnlyBorder) and
         (ShadowColor <> TalphaColorRec.Null) then aPaint.setShadowLayer(ShadowBlur{radius}, ShadowOffsetX{dx}, ShadowOffsetY{dy}, integer(ShadowColor){shadowColor});

      LJRect := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
      aCanvas.drawRoundRect(
        LJRect{rect},
        xRadius {rx},
        yRadius {ry},
        apaint);
      LJRect := nil;

      if (not aDrawOnlyBorder) and
         (ShadowColor <> TalphaColorRec.Null) then aPaint.clearShadowLayer;
      //-----
    end

    // use drawRect
    else if ((compareValue(xRadius, 0, TEpsilon.position) = 0) or
             (compareValue(YRadius, 0, TEpsilon.position) = 0) or
             (corners=[])) and
            (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
      //-----
      if (not aDrawOnlyBorder) and
         (ShadowColor <> TalphaColorRec.Null) then aPaint.setShadowLayer(ShadowBlur{radius}, ShadowOffsetX{dx}, ShadowOffsetY{dy}, integer(ShadowColor){shadowColor});

      aCanvas.drawRect(
        aRect.left{left},
        aRect.top{top},
        aRect.right{right},
        aRect.bottom{bottom},
        apaint);

      if (not aDrawOnlyBorder) and
         (ShadowColor <> TalphaColorRec.Null) then aPaint.clearShadowLayer;
      //-----
    end

    // use drawPath
    else begin

      LPath := TJPath.Create;
      //----
      LXRadius := xRadius;
      LYradius := yRadius;
      if (LXRadius > aRect.width / 2) then LXRadius := aRect.width / 2;
      if (LYradius > aRect.height / 2) then LYradius := aRect.height / 2;
      //----
      if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := corners
      else LCorners := [];
      //----
      LWidthMinusCorners := (aRect.width - (2 * LXRadius));
      LHeightMinusCorners := (aRect.height - (2 * LYradius));
      //----
      if (StrokeColor <> TalphaColorRec.Null) then LHalfStrokeWidth := (StrokeThickness) / 2
      else LHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in LCorners) then begin
        LPath.moveTo(aRect.right, aRect.top + LYradius);
        LPath.rQuadTo(0, -LYradius, -LXRadius, -LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(0, -LHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then LPath.moveTo(aRect.right + LHalfStrokeWidth, aRect.top + LYradius)
        else LPath.moveTo(aRect.right, aRect.top + LYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           LPath.rLineTo(0, -LYradius -LHalfStrokeWidth);
           if aDrawOnlyBorder then LPath.rMoveTo(0, LHalfStrokeWidth);
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(-LXRadius -LHalfStrokeWidth,0)
          else begin
            LPath.rMoveTo(+LHalfStrokeWidth,0);
            LPath.rLineTo(-LXRadius -LHalfStrokeWidth,0);
          end;
        end
        else LPath.rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then LPath.rLineTo(-LWidthMinusCorners, 0)
      else LPath.rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(0, +LHalfStrokeWidth);
        LPath.rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(-LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          LPath.rLineTo(-LXRadius -LHalfStrokeWidth, 0);
          if aDrawOnlyBorder then LPath.rMoveTo(LHalfStrokeWidth, 0);
        end
        else LPath.rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(0,LYradius +LHalfStrokeWidth)
          else begin
            LPath.rMoveTo(0,-LHalfStrokeWidth);
            LPath.rLineTo(0,+LYradius +LHalfStrokeWidth);
          end;
        end
        else LPath.rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then LPath.rLineTo(0, LHeightMinusCorners)
      else LPath.rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(LHalfStrokeWidth, 0);
        LPath.rQuadTo(0, LYradius, LXRadius, LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(0, LHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          LPath.rLineTo(0, LYradius +LHalfStrokeWidth);
          if aDrawOnlyBorder then LPath.rMoveTo(0, -LHalfStrokeWidth);
        end
        else LPath.rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(LXRadius +LHalfStrokeWidth,0)
          else begin
            LPath.rMoveTo(-LHalfStrokeWidth,0);
            LPath.rLineTo(+LXRadius +LHalfStrokeWidth,0);
          end;
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then LPath.rLineTo(LWidthMinusCorners, 0)
      else LPath.rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(0, -LHalfStrokeWidth);
        LPath.rQuadTo(LXRadius, 0, LXRadius, -LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          LPath.rLineTo(LXRadius +LHalfStrokeWidth,0);
          if aDrawOnlyBorder then LPath.rMoveTo(-LHalfStrokeWidth, 0);
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(0, -LYradius -LHalfStrokeWidth)
          else begin
            LPath.rMoveTo(0,+LHalfStrokeWidth);
            LPath.rLineTo(0,-LYradius -LHalfStrokeWidth);
          end;
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then LPath.rLineTo(0, -LHeightMinusCorners)
      else LPath.rMoveTo(0, -LHeightMinusCorners);

      //-----
      if (not aDrawOnlyBorder) and
         (ShadowColor <> TalphaColorRec.Null) then aPaint.setShadowLayer(ShadowBlur{radius}, ShadowOffsetX{dx}, ShadowOffsetY{dy}, integer(ShadowColor){shadowColor});

      aCanvas.drawPath(LPath,aPaint);
      LPath := nil;

      if (not aDrawOnlyBorder) and
         (ShadowColor <> TalphaColorRec.Null) then aPaint.clearShadowLayer;
      //-----

    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' _DrawPath (IOS)'}
  {$IF defined(IOS)}
  procedure _DrawPath(
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean);

  var LXRadius: single;
      LYradius: Single;
      LWidthMinusCorners: single;
      LHeightMinusCorners: Single;
      LCorners: TCorners;
      LHalfStrokeWidth: Single;
      LCurPoint: TpointF;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(aContext, X, aGridHeight - Y);
      LCurPoint.X := x;
      LCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(
        aContext,
        LCurPoint.X + dx1{cpx},
        aGridHeight - (LCurPoint.Y + dy1){cpy},
        LCurPoint.X + dx2{x},
        aGridHeight - (LCurPoint.Y + dy2){y});
      LCurPoint.X := LCurPoint.X + dx2;
      LCurPoint.Y := LCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(aContext, LCurPoint.X + dx{x}, aGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rMoveTo(dx: Single; dy: Single);
    begin
      CGContextMoveToPoint(aContext, LCurPoint.X + dx{x}, aGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
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
     CGContextAddRect(
       aContext,
       ALLowerLeftCGRect(
         aRect.TopLeft,
         aRect.Width,
         aRect.Height,
         aGridHeight));
     //-----
    end

    // use drawPath
    else begin

      LXRadius := xRadius;
      LYradius := yRadius;
      if (LXRadius > aRect.width / 2) then LXRadius := aRect.width / 2;
      if (LYradius > aRect.height / 2) then LYradius := aRect.height / 2;
      //----
      if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := corners
      else LCorners := [];
      //----
      LWidthMinusCorners := (aRect.width - (2 * LXRadius));
      LHeightMinusCorners := (aRect.height - (2 * LYradius));
      //----
      if (StrokeColor <> TalphaColorRec.Null) then LHalfStrokeWidth := (StrokeThickness) / 2
      else LHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in LCorners) then begin
        _moveTo(aRect.right, aRect.top + LYradius);
        _rQuadTo(0, -LYradius, -LXRadius, -LYradius);
        if not aDrawOnlyBorder then _rlineTo(0, -LHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then _moveTo(aRect.right + LHalfStrokeWidth, aRect.top + LYradius)
        else _moveTo(aRect.right, aRect.top + LYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           _rLineTo(0, -LYradius -LHalfStrokeWidth);
           if aDrawOnlyBorder then _rMoveTo(0, LHalfStrokeWidth);
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(-LXRadius -LHalfStrokeWidth,0)
          else begin
            _rMoveTo(+LHalfStrokeWidth,0);
            _rLineTo(-LXRadius -LHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then _rLineTo(-LWidthMinusCorners, 0)
      else _rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, +LHalfStrokeWidth);
        _rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
        if not aDrawOnlyBorder then _rlineTo(-LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          _rLineTo(-LXRadius -LHalfStrokeWidth, 0);
          if aDrawOnlyBorder then _rMoveTo(LHalfStrokeWidth, 0);
        end
        else _rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0,LYradius +LHalfStrokeWidth)
          else begin
            _rMoveTo(0,-LHalfStrokeWidth);
            _rLineTo(0,+LYradius +LHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then _rLineTo(0, LHeightMinusCorners)
      else _rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(LHalfStrokeWidth, 0);
        _rQuadTo(0, LYradius, LXRadius, LYradius);
        if not aDrawOnlyBorder then _rlineTo(0, LHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          _rLineTo(0, LYradius +LHalfStrokeWidth);
          if aDrawOnlyBorder then _rMoveTo(0, -LHalfStrokeWidth);
        end
        else _rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(LXRadius +LHalfStrokeWidth,0)
          else begin
            _rMoveTo(-LHalfStrokeWidth,0);
            _rLineTo(+LXRadius +LHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then _rLineTo(LWidthMinusCorners, 0)
      else _rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, -LHalfStrokeWidth);
        _rQuadTo(LXRadius, 0, LXRadius, -LYradius);
        if not aDrawOnlyBorder then _rlineTo(LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          _rLineTo(LXRadius +LHalfStrokeWidth,0);
          if aDrawOnlyBorder then _rMoveTo(-LHalfStrokeWidth, 0);
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0, -LYradius -LHalfStrokeWidth)
          else begin
            _rMoveTo(0,+LHalfStrokeWidth);
            _rLineTo(0,-LYradius -LHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then _rLineTo(0, -LHeightMinusCorners)
      else _rMoveTo(0, -LHeightMinusCorners);

    end;

  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' _GetShapeRect (MSWINDOWS / ALMacOS)'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  function _GetShapeRect: TRectF;
  begin
    Result := DstRect;
    if StrokeColor <> TalphaColorRec.Null then
      InflateRect(Result, -(StrokeThickness / 2), -(StrokeThickness / 2));
  end;
  {$ENDIF}
  {$ENDREGION}

{$IF defined(ANDROID)}
var LRect: TrectF;
    LPaint: JPaint;
{$ELSEIF defined(IOS)}
var LRect: TrectF;
    LAlphaColor: TAlphaColorCGFloat;
    LColor: CGColorRef;
{$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
var LShapeRect: TRectF;
    LOff: Single;
    LFillKindRestoreValue: TBrushKind;
    LFillColorRestoreValue: TAlphacolor;
    LStrokeKindRestoreValue: TBrushKind;
    LStrokeColorRestoreValue: TAlphacolor;
    LStrokeThicknessRestoreValue: Single;
    LFillShape, LDrawShape: Boolean;
    LShadowEffect: TshadowEffect;
{$ENDIF}

begin

  {$IFDEF ANDROID}

  //create the canvas and the paint
  LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  //init LRect
  if StrokeColor <> TalphaColorRec.Null then begin
    LRect := TrectF.Create(
               dstRect.Left + (StrokeThickness / 2),
               dstRect.Top + (StrokeThickness / 2),
               dstRect.right - (StrokeThickness / 2),
               dstRect.bottom - (StrokeThickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := dstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the rectangle
  if FillColor <> TalphaColorRec.Null then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LRect + StrokeThickness :( this result&ing in border if the fill is for exemple black and border white

    //fill with solid color
    LPaint.setColor(integer(FillColor));
    _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});

  end;

  //stroke the rectangle
  if StrokeColor <> TalphaColorRec.Null then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    LPaint.setStrokeWidth(StrokeThickness);

    //stroke with solid color
    LPaint.setColor(integer(StrokeColor));
    _drawRect(aCanvas, LPaint, LRect, true{aDrawOnlyBorder});

  end;

  //free the paint and the canvas
  LPaint := nil;

  {$ELSEIF DEFINED(IOS)}

  //set the paint default properties
  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
  //-----
  CGContextSetShouldAntialias(aContext, True); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(aContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.

  //init LRect
  if StrokeColor <> TalphaColorRec.Null then begin
    LRect := TrectF.Create(
               DstRect.Left + (StrokeThickness / 2),
               DstRect.Top + (StrokeThickness / 2),
               DstRect.right - (StrokeThickness / 2),
               DstRect.bottom - (StrokeThickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := DstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the rectangle
  if FillColor <> TalphaColorRec.Null then begin

    //fill with solid color
    LAlphaColor := TAlphaColorCGFloat.Create(FillColor);
    CGContextSetRGBFillColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
    _DrawPath(LRect, false{aDrawOnlyBorder});
    //-----
    if (ShadowColor <> TalphaColorRec.Null) then begin
      LAlphaColor := TAlphaColorCGFloat.Create(ShadowColor);
      LColor := CGColorCreate(aColorSpace, @LAlphaColor);
      try
        CGContextSetShadowWithColor(
          aContext,
          CGSizeMake(ShadowOffsetX, ShadowOffsetY), // offset
          ShadowBlur, // blur
          LColor); // color
      finally
        CGColorRelease(LColor);
      end;
    end;
    //-----
    CGContextFillPath(aContext);
    //-----
    if (ShadowColor <> TalphaColorRec.Null) then begin
      CGContextSetShadowWithColor(
        aContext,
        CGSizeMake(0, 0), // offset
        0, // blur
        nil); // color
    end;

  end;

  //stroke the rectangle
  if StrokeColor <> TalphaColorRec.Null then begin

    //stroke with solid color
    LAlphaColor := TAlphaColorCGFloat.Create(StrokeColor);
    CGContextSetRGBStrokeColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
    CGContextSetLineWidth(aContext, StrokeThickness);
    _DrawPath(LRect, True{aDrawOnlyBorder});
    CGContextStrokePath(aContext);

  end;

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

  LFillKindRestoreValue := ACanvas.Fill.Kind;
  LFillColorRestoreValue := ACanvas.Fill.color;
  LStrokeKindRestoreValue := ACanvas.Stroke.kind;
  LStrokeColorRestoreValue := ACanvas.Stroke.Color;
  LStrokeThicknessRestoreValue := ACanvas.Stroke.Thickness;
  if FillColor <> TAlphaColorRec.Null then begin
    ACanvas.Fill.Kind := TBrushKind.Solid;
    ACanvas.Fill.Color := FillColor;
  end
  else ACanvas.Fill.Kind := TBrushKind.None;
  If StrokeColor <> TalphaColorRec.Null then begin
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Color := StrokeColor;
    ACanvas.Stroke.Thickness := StrokeThickness;
  end
  else ACanvas.Stroke.Kind := TBrushKind.None;
  try

    LShapeRect := ALGetDrawingShapeRectAndSetThickness(DstRect, ACanvas.Fill, ACanvas.Stroke, False, LFillShape, LDrawShape, LStrokeThicknessRestoreValue);

    if Sides <> AllSides then
    begin
      LOff := LShapeRect.Left;
      if not(TSide.Top in Sides) then
        LShapeRect.Top := LShapeRect.Top - LOff;
      if not(TSide.Left in Sides) then
        LShapeRect.Left := LShapeRect.Left - LOff;
      if not(TSide.Bottom in Sides) then
        LShapeRect.Bottom := LShapeRect.Bottom + LOff;
      if not(TSide.Right in Sides) then
        LShapeRect.Right := LShapeRect.Right + LOff;
      if LFillShape then
        aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, ACanvas.Fill, TCornerType.Round{CornerType});
      if LDrawShape then
        aCanvas.DrawRectSides(_GetShapeRect, XRadius, YRadius, Corners,  1{AbsoluteOpacity}, Sides, ACanvas.Stroke, TCornerType.Round{CornerType});
    end
    else
    begin
      if LFillShape then
        aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, ACanvas.Fill, TCornerType.Round{CornerType});
      if LDrawShape then
        aCanvas.DrawRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, ACanvas.Stroke, TCornerType.Round{CornerType});
    end;

    if (ShadowColor <> TalphaColorRec.Null) then begin

      LShadowEffect := TshadowEffect.Create(nil);
      try
        LShadowEffect.ShadowColor := ShadowColor;
        LShadowEffect.distance := 0; // Specifies the distance between the shadow and the visual object to which TShadowEffect is applied.
                                     // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
        LShadowEffect.Direction := 0;  // Specifies the direction (in degrees) of the shadow.
                                       // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
        LShadowEffect.Opacity := 1; // Opacity is a System.Single value that takes values in the range from 0 through 1.
                                    // we use the opacity of the color instead
        LShadowEffect.softness := ShadowBlur / 24; // Specifies the amount of blur applied to the shadow.
                                                   // Softness is a System.Single value that takes values in the range from 0 through 9.
                                                   // i calculate approximatly that 0.5 = around 12 for blur
        Acanvas.Flush;
        LShadowEffect.ProcessEffect(ACanvas, Acanvas.Bitmap, 1);
      finally
        ALFreeAndNil(LShadowEffect);
      end;

      if Sides <> AllSides then
      begin
        if LFillShape then
          aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, ACanvas.Fill, TCornerType.Round{CornerType});
        if LDrawShape then
          aCanvas.DrawRectSides(_GetShapeRect, XRadius, YRadius, Corners,  1{AbsoluteOpacity}, Sides, ACanvas.Stroke, TCornerType.Round{CornerType});
      end
      else
      begin
        if LFillShape then
          aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, ACanvas.Fill, TCornerType.Round{CornerType});
        if LDrawShape then
          aCanvas.DrawRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, ACanvas.Stroke, TCornerType.Round{CornerType});
      end;

    end;

  finally
    ACanvas.Fill.Kind := LFillKindRestoreValue;
    ACanvas.Fill.color := LFillColorRestoreValue;
    ACanvas.Stroke.kind := LStrokeKindRestoreValue;
    ACanvas.Stroke.Color := LStrokeColorRestoreValue;
    ACanvas.Stroke.Thickness := LStrokeThicknessRestoreValue;
  end;

  {$ENDIF}

end;

{*************************}
procedure ALPaintRectangle(
            {$IF defined(ANDROID)}
            const aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            const aContext: CGContextRef;
            const aColorSpace: CGColorSpaceRef;
            const aGridHeight: Single;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            const aCanvas: Tcanvas;
            {$ENDIF}
            const dstRect: TrectF;
            const Fill: TBrush;
            const Stroke: TStrokeBrush;
            const Shadow: TALShadow; // If shadow is not nil, then the Canvas must have enough space to draw the shadow (approximately Shadow.blur on each side of the rectangle)
            const Sides: TSides;
            const Corners: TCorners;
            const XRadius: Single;
            const YRadius: Single);

  {$REGION ' _drawRect (ANDROID)'}
  {$IF defined(ANDROID)}
  procedure _drawRect(
              const aCanvas: Jcanvas;
              const aPaint: JPaint;
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean);
  var LJRect: JRectF;
      LPath: JPath;
      LXRadius: single;
      LYradius: Single;
      LWidthMinusCorners: single;
      LHeightMinusCorners: Single;
      LCorners: TCorners;
      LHalfStrokeWidth: Single;
  begin

    // use drawRoundRect
    if ((compareValue(xRadius, 0, TEpsilon.position) > 0) and
        (compareValue(YRadius, 0, TEpsilon.position) > 0)) and
       (corners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
       (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
      //-----
      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.Color){shadowColor});

      LJRect := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
      aCanvas.drawRoundRect(
        LJRect{rect},
        xRadius {rx},
        yRadius {ry},
        apaint);
      LJRect := nil;

      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.clearShadowLayer;
      //-----
    end

    // use drawRect
    else if ((compareValue(xRadius, 0, TEpsilon.position) = 0) or
             (compareValue(YRadius, 0, TEpsilon.position) = 0) or
             (corners=[])) and
            (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
      //-----
      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.Color){shadowColor});

      aCanvas.drawRect(
        aRect.left{left},
        aRect.top{top},
        aRect.right{right},
        aRect.bottom{bottom},
        apaint);

      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.clearShadowLayer;
      //-----
    end

    // use drawPath
    else begin

      LPath := TJPath.Create;
      //----
      LXRadius := xRadius;
      LYradius := yRadius;
      if (LXRadius > aRect.width / 2) then LXRadius := aRect.width / 2;
      if (LYradius > aRect.height / 2) then LYradius := aRect.height / 2;
      //----
      if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := corners
      else LCorners := [];
      //----
      LWidthMinusCorners := (aRect.width - (2 * LXRadius));
      LHeightMinusCorners := (aRect.height - (2 * LYradius));
      //----
      if (Stroke.Kind <> TBrushKind.None) then LHalfStrokeWidth := (Stroke.Thickness) / 2
      else LHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in LCorners) then begin
        LPath.moveTo(aRect.right, aRect.top + LYradius);
        LPath.rQuadTo(0, -LYradius, -LXRadius, -LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(0, -LHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then LPath.moveTo(aRect.right + LHalfStrokeWidth, aRect.top + LYradius)
        else LPath.moveTo(aRect.right, aRect.top + LYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           LPath.rLineTo(0, -LYradius -LHalfStrokeWidth);
           if aDrawOnlyBorder then LPath.rMoveTo(0, LHalfStrokeWidth);
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(-LXRadius -LHalfStrokeWidth,0)
          else begin
            LPath.rMoveTo(+LHalfStrokeWidth,0);
            LPath.rLineTo(-LXRadius -LHalfStrokeWidth,0);
          end;
        end
        else LPath.rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then LPath.rLineTo(-LWidthMinusCorners, 0)
      else LPath.rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(0, +LHalfStrokeWidth);
        LPath.rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(-LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          LPath.rLineTo(-LXRadius -LHalfStrokeWidth, 0);
          if aDrawOnlyBorder then LPath.rMoveTo(LHalfStrokeWidth, 0);
        end
        else LPath.rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(0,LYradius +LHalfStrokeWidth)
          else begin
            LPath.rMoveTo(0,-LHalfStrokeWidth);
            LPath.rLineTo(0,+LYradius +LHalfStrokeWidth);
          end;
        end
        else LPath.rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then LPath.rLineTo(0, LHeightMinusCorners)
      else LPath.rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(LHalfStrokeWidth, 0);
        LPath.rQuadTo(0, LYradius, LXRadius, LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(0, LHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          LPath.rLineTo(0, LYradius +LHalfStrokeWidth);
          if aDrawOnlyBorder then LPath.rMoveTo(0, -LHalfStrokeWidth);
        end
        else LPath.rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(LXRadius +LHalfStrokeWidth,0)
          else begin
            LPath.rMoveTo(-LHalfStrokeWidth,0);
            LPath.rLineTo(+LXRadius +LHalfStrokeWidth,0);
          end;
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then LPath.rLineTo(LWidthMinusCorners, 0)
      else LPath.rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(0, -LHalfStrokeWidth);
        LPath.rQuadTo(LXRadius, 0, LXRadius, -LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          LPath.rLineTo(LXRadius +LHalfStrokeWidth,0);
          if aDrawOnlyBorder then LPath.rMoveTo(-LHalfStrokeWidth, 0);
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(0, -LYradius -LHalfStrokeWidth)
          else begin
            LPath.rMoveTo(0,+LHalfStrokeWidth);
            LPath.rLineTo(0,-LYradius -LHalfStrokeWidth);
          end;
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then LPath.rLineTo(0, -LHeightMinusCorners)
      else LPath.rMoveTo(0, -LHeightMinusCorners);

      //-----
      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.Color){shadowColor});

      aCanvas.drawPath(LPath,aPaint);
      LPath := nil;

      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.clearShadowLayer;
      //-----

    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' _DrawPath (IOS)'}
  {$IF defined(IOS)}
  procedure _DrawPath(
              const aRect: TrectF;
              Const aDrawOnlyBorder: Boolean);

  var LXRadius: single;
      LYradius: Single;
      LWidthMinusCorners: single;
      LHeightMinusCorners: Single;
      LCorners: TCorners;
      LHalfStrokeWidth: Single;
      LCurPoint: TpointF;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(aContext, X, aGridHeight - Y);
      LCurPoint.X := x;
      LCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(
        aContext,
        LCurPoint.X + dx1{cpx},
        aGridHeight - (LCurPoint.Y + dy1){cpy},
        LCurPoint.X + dx2{x},
        aGridHeight - (LCurPoint.Y + dy2){y});
      LCurPoint.X := LCurPoint.X + dx2;
      LCurPoint.Y := LCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(aContext, LCurPoint.X + dx{x}, aGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rMoveTo(dx: Single; dy: Single);
    begin
      CGContextMoveToPoint(aContext, LCurPoint.X + dx{x}, aGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
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
     CGContextAddRect(
       aContext,
       ALLowerLeftCGRect(
         aRect.TopLeft,
         aRect.Width,
         aRect.Height,
         aGridHeight));
     //-----
    end

    // use drawPath
    else begin

      LXRadius := xRadius;
      LYradius := yRadius;
      if (LXRadius > aRect.width / 2) then LXRadius := aRect.width / 2;
      if (LYradius > aRect.height / 2) then LYradius := aRect.height / 2;
      //----
      if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := corners
      else LCorners := [];
      //----
      LWidthMinusCorners := (aRect.width - (2 * LXRadius));
      LHeightMinusCorners := (aRect.height - (2 * LYradius));
      //----
      if (Stroke.Kind <> TBrushKind.None) then LHalfStrokeWidth := (Stroke.Thickness) / 2
      else LHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in LCorners) then begin
        _moveTo(aRect.right, aRect.top + LYradius);
        _rQuadTo(0, -LYradius, -LXRadius, -LYradius);
        if not aDrawOnlyBorder then _rlineTo(0, -LHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then _moveTo(aRect.right + LHalfStrokeWidth, aRect.top + LYradius)
        else _moveTo(aRect.right, aRect.top + LYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           _rLineTo(0, -LYradius -LHalfStrokeWidth);
           if aDrawOnlyBorder then _rMoveTo(0, LHalfStrokeWidth);
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(-LXRadius -LHalfStrokeWidth,0)
          else begin
            _rMoveTo(+LHalfStrokeWidth,0);
            _rLineTo(-LXRadius -LHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then _rLineTo(-LWidthMinusCorners, 0)
      else _rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, +LHalfStrokeWidth);
        _rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
        if not aDrawOnlyBorder then _rlineTo(-LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          _rLineTo(-LXRadius -LHalfStrokeWidth, 0);
          if aDrawOnlyBorder then _rMoveTo(LHalfStrokeWidth, 0);
        end
        else _rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0,LYradius +LHalfStrokeWidth)
          else begin
            _rMoveTo(0,-LHalfStrokeWidth);
            _rLineTo(0,+LYradius +LHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then _rLineTo(0, LHeightMinusCorners)
      else _rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(LHalfStrokeWidth, 0);
        _rQuadTo(0, LYradius, LXRadius, LYradius);
        if not aDrawOnlyBorder then _rlineTo(0, LHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          _rLineTo(0, LYradius +LHalfStrokeWidth);
          if aDrawOnlyBorder then _rMoveTo(0, -LHalfStrokeWidth);
        end
        else _rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(LXRadius +LHalfStrokeWidth,0)
          else begin
            _rMoveTo(-LHalfStrokeWidth,0);
            _rLineTo(+LXRadius +LHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then _rLineTo(LWidthMinusCorners, 0)
      else _rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, -LHalfStrokeWidth);
        _rQuadTo(LXRadius, 0, LXRadius, -LYradius);
        if not aDrawOnlyBorder then _rlineTo(LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          _rLineTo(LXRadius +LHalfStrokeWidth,0);
          if aDrawOnlyBorder then _rMoveTo(-LHalfStrokeWidth, 0);
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0, -LYradius -LHalfStrokeWidth)
          else begin
            _rMoveTo(0,+LHalfStrokeWidth);
            _rLineTo(0,-LYradius -LHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then _rLineTo(0, -LHeightMinusCorners)
      else _rMoveTo(0, -LHeightMinusCorners);

    end;

  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' _GetShapeRect (MSWINDOWS / ALMacOS)'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  function _GetShapeRect: TRectF;
  begin
    Result := DstRect;
    if Stroke.Kind <> TBrushKind.None then
      InflateRect(Result, -(Stroke.Thickness / 2), -(Stroke.Thickness / 2));
  end;
  {$ENDIF}
  {$ENDREGION}

{$IF defined(IOS)}
const DefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var LRect: TrectF;
    LTmpBitmap: Jbitmap;
    LRadialShader: JRadialGradient;
    LLinearShader: JLinearGradient;
    LPaint: JPaint;
    LColors: TJavaArray<Integer>;
    LStops: TJavaArray<Single>;
    LPorterDuffXfermode: jPorterDuffXfermode;
    LBitmapInfo: AndroidBitmapInfo;
    LPixelBuffer: Pointer;
    LBitmapData: TBitmapData;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    i: integer;
{$ELSEIF defined(IOS)}
var LRect: TrectF;
    LAlphaColor: TAlphaColorCGFloat;
    LColor: CGColorRef;
    LCallback: CGFunctionCallbacks;
    LShading: CGShadingRef;
    LFunc: CGFunctionRef;
    LBitmapData: TBitmapData;
    LTmpContext: CGContextRef;
    LImageRef: CGImageRef;
    LImage: UIImage;
{$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
var LShapeRect: TRectF;
    LOff: Single;
    LStrokeThicknessRestoreValue: Single;
    LFillShape, LDrawShape: Boolean;
    LShadowEffect: TshadowEffect;
{$ENDIF}

begin

  {$IFDEF ANDROID}

  //create the canvas and the paint
  LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  //init LRect
  if Stroke.Kind <> TBrushKind.None then begin
    LRect := TrectF.Create(
               dstRect.Left + (Stroke.Thickness / 2),
               dstRect.Top + (Stroke.Thickness / 2),
               dstRect.right - (Stroke.Thickness / 2),
               dstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := dstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the rectangle
  if Fill.Kind <> TBrushKind.None then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LRect + Stroke.Thickness :( this result&ing in border if the fill is for exemple black and border white

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        LColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
        LStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
        for i := 0 to Fill.Gradient.Points.Count - 1 do begin
          LColors[Fill.Gradient.Points.Count - 1 - i] := integer(Fill.Gradient.Points[i].Color);
          LStops[Fill.Gradient.Points.Count - 1 - i] := 1 - Fill.Gradient.Points[i].Offset;
        end;
        LRadialShader := TJRadialGradient.JavaClass.init(
                           LRect.CenterPoint.x{x},
                           LRect.CenterPoint.y{y},
                           LRect.width / 2{radius},
                           LColors,
                           LStops,
                           TJShader_TileMode.JavaClass.CLAMP{tile});
        LPaint.setShader(LRadialShader);
        _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});
        LPaint.setShader(nil);
        LRadialShader := nil;
        ALfreeandNil(LColors);
        ALfreeandNil(LStops);
      end
      else if Fill.Gradient.Style = TGradientStyle.Linear then begin
        LColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
        LStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
        for i := 0 to Fill.Gradient.Points.Count - 1 do begin
          LColors[i] := integer(Fill.Gradient.Points[i].Color);
          LStops[i] := Fill.Gradient.Points[i].Offset;
        end;
        LLinearShader := TJLinearGradient.JavaClass.init(
                           LRect.TopLeft.x + (Fill.Gradient.StartPosition.X * LRect.Width){x0},
                           LRect.TopLeft.y + (Fill.Gradient.StartPosition.Y * LRect.Height){y0},
                           LRect.BottomRight.x + (Fill.Gradient.StopPosition.X * LRect.Width){x1},
                           LRect.BottomRight.y + (Fill.Gradient.StopPosition.Y * LRect.Height){y1},
                           LColors,
                           LStops,
                           TJShader_TileMode.JavaClass.CLAMP{tile});
        LPaint.setShader(LLinearShader);
        _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});
        LPaint.setShader(nil);
        LLinearShader := nil;
        ALfreeandNil(LColors);
        ALfreeandNil(LStops);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          //-----
          LTmpBitmap := TJBitmap.JavaClass.createBitmap(fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888);
          //-----
          FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
          if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LBitmapInfo) = 0) and
             (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LPixelBuffer) = 0) then
          try
            if fill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
            try
              ALMove(LBitmapData.Data^, LPixelBuffer^, LBitmapData.Pitch * LBitmapData.Height);
            finally
              fill.Bitmap.Bitmap.Unmap(LBitmapData);
            end;
          finally
            AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID);
          end;
          //-----
          _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});
          LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
          LJDestRectf := TJRectf.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);
          LJSrcRect := TJRect.JavaClass.init(0, 0, fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height);
          LPaint.setXfermode(LPorterDuffXfermode);
          aCanvas.drawBitmap(LTmpBitmap, LJSrcRect, LJDestRectf, LPaint);
          LPaint.setXfermode(nil);
          LPorterDuffXfermode := nil;
          LJSrcRect := nil;
          LJDestRectf := nil;
          //-----
          LTmpBitmap.recycle;
          LTmpBitmap := nil;
          //-----
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      LPaint.setColor(integer(Fill.Color));
      _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});
    end;

  end;

  //stroke the rectangle
  if Stroke.Kind <> TBrushKind.None then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    LPaint.setStrokeWidth(Stroke.Thickness);

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      LPaint.setColor(integer(Stroke.Color));
      _drawRect(aCanvas, LPaint, LRect, true{aDrawOnlyBorder});
    end;

  end;

  //free the paint and the canvas
  LPaint := nil;

  {$ELSEIF DEFINED(IOS)}

  //set the paint default properties
  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
  //-----
  CGContextSetShouldAntialias(aContext, True); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(aContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.

  //init LRect
  if Stroke.Kind <> TBrushKind.None then begin
    LRect := TrectF.Create(
               DstRect.Left + (Stroke.Thickness / 2),
               DstRect.Top + (Stroke.Thickness / 2),
               DstRect.right - (Stroke.Thickness / 2),
               DstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := DstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the rectangle
  if Fill.Kind <> TBrushKind.None then begin

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style in [TGradientStyle.Radial,
                                 TGradientStyle.Linear] then begin
        CGContextSaveGState(aContext);
        //-----
        LCallback.version := 0;
        LCallback.evaluate := @ALGradientEvaluateCallback;
        LCallback.releaseInfo:= nil;
        LFunc := CGFunctionCreate(
                   fill.Gradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                   1, // domainDimension - The number of inputs.
                   @DefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                   4, // rangeDimension - The number of outputs.
                   nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                   @LCallback); // callbacks - A pointer to a callback function table.
        try
          if Fill.Gradient.Style = TGradientStyle.Radial then begin
            LShading := CGShadingCreateRadial(
                          aColorSpace, // colorspace
                          CGPoint.Create(TPointF.Create(LRect.Width / 2, aGridHeight - (LRect.height / 2))), // start - The center of the starting circle, in the shading's target coordinate space.
                          LRect.Width / 2, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                          CGPoint.Create(TPointF.Create(LRect.Width / 2, aGridHeight - (LRect.Height / 2))), // end - The center of the ending circle, in the shading's target coordinate space.
                          0, // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                          LFunc, // function
                          True, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                          True); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
          end
          else begin
            LShading := CGShadingCreateAxial(
                          aColorSpace, // colorspace
                          CGPointMake(
                            LRect.Left + (Fill.Gradient.StartPosition.X * LRect.Width),
                            aGridHeight - LRect.top - (Fill.Gradient.StartPosition.Y * LRect.Height)), // start - The starting point of the axis, in the shading's target coordinate space.
                          CGPointMake(
                            LRect.Left + (Fill.Gradient.StopPosition.X * LRect.Width),
                            aGridHeight - LRect.top - (Fill.Gradient.StopPosition.Y * LRect.Height)), // end - The ending point of the axis, in the shading's target coordinate space.
                          LFunc, // function
                          True, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting point of the axis.
                          True); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending point of the axis.
          end;
          try
            _DrawPath(LRect, false{aDrawOnlyBorder});
            CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                     // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                     // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                     // save the graphics state before you clip and restore the graphics state after you’ve completed
                                     // any clipped drawing.
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              LAlphaColor := TAlphaColorCGFloat.Create(Shadow.Color);
              LColor := CGColorCreate(aColorSpace, @LAlphaColor);
              try
                CGContextSetShadowWithColor(
                  aContext,
                  CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                  Shadow.blur, // blur
                  LColor); // color
              finally
                CGColorRelease(LColor);
              end;
            end;
            //-----
            CGContextDrawShading(aContext, LShading);
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              CGContextSetShadowWithColor(
                aContext,
                CGSizeMake(0, 0), // offset
                0, // blur
                nil); // color
            end;
            //-----
          finally
            CGShadingRelease(LShading);
          end;
        finally
          CGFunctionRelease(LFunc);
        end;
        //-----
        CGContextRestoreGState(aContext);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          if fill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
          try
            LTmpContext := CGBitmapContextCreate(
                             LBitmapData.Data, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                               //       memory block should be at least (bytesPerRow*height) bytes.
                                               //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                               //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                             LBitmapData.Width, // width: The width, in pixels, of the required bitmap.
                             LBitmapData.Height, // height: The height, in pixels, of the required bitmap.
                             8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                //                   chapter of Quartz 2D Programming Guide.
                             LBitmapData.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
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
                LImageRef := CGBitmapContextCreateImage(LTmpContext);
                if LImageRef <> nil then
                try
                  LImage := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(LImageRef));
                  if LImage <> nil then
                  try
                    CGContextSaveGState(aContext);
                    //-----
                    _DrawPath(LRect, false{aDrawOnlyBorder});
                    CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      LAlphaColor := TAlphaColorCGFloat.Create(Shadow.Color);
                      LColor := CGColorCreate(aColorSpace, @LAlphaColor);
                      try
                        CGContextSetShadowWithColor(
                          aContext,
                          CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                          Shadow.blur, // blur
                          LColor); // color
                      finally
                        CGColorRelease(LColor);
                      end;
                    end;
                    //-----
                    CGContextDrawImage(
                      aContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        LRect.TopLeft,
                        LRect.Width,
                        LRect.Height,
                        aGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      CGContextSetShadowWithColor(
                        aContext,
                        CGSizeMake(0, 0), // offset
                        0, // blur
                        nil); // color
                    end;
                    //-----
                    CGContextRestoreGState(aContext);
                  finally
                    LImage.release;
                  end;
                finally
                  CGImageRelease(LImageRef);
                end;
              finally
                CGContextRelease(LTmpContext);
              end;
            end;
          finally
            fill.Bitmap.Bitmap.Unmap(LBitmapData);
          end;
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      LAlphaColor := TAlphaColorCGFloat.Create(Fill.Color);
      CGContextSetRGBFillColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
      _DrawPath(LRect, false{aDrawOnlyBorder});
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        LAlphaColor := TAlphaColorCGFloat.Create(Shadow.Color);
        LColor := CGColorCreate(aColorSpace, @LAlphaColor);
        try
          CGContextSetShadowWithColor(
            aContext,
            CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
            Shadow.blur, // blur
            LColor); // color
        finally
          CGColorRelease(LColor);
        end;
      end;
      //-----
      CGContextFillPath(aContext);
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        CGContextSetShadowWithColor(
          aContext,
          CGSizeMake(0, 0), // offset
          0, // blur
          nil); // color
      end;
    end;

  end;

  //stroke the rectangle
  if Stroke.Kind <> TBrushKind.None then begin

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      LAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
      CGContextSetRGBStrokeColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
      CGContextSetLineWidth(aContext, Stroke.Thickness);
      _DrawPath(LRect, True{aDrawOnlyBorder});
      CGContextStrokePath(aContext);
    end;

  end;

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

  LStrokeThicknessRestoreValue := Stroke.Thickness;
  try

    LShapeRect := ALGetDrawingShapeRectAndSetThickness(DstRect, Fill, Stroke, False, LFillShape, LDrawShape, LStrokeThicknessRestoreValue);

    if Sides <> AllSides then
    begin
      LOff := LShapeRect.Left;
      if not(TSide.Top in Sides) then
        LShapeRect.Top := LShapeRect.Top - LOff;
      if not(TSide.Left in Sides) then
        LShapeRect.Left := LShapeRect.Left - LOff;
      if not(TSide.Bottom in Sides) then
        LShapeRect.Bottom := LShapeRect.Bottom + LOff;
      if not(TSide.Right in Sides) then
        LShapeRect.Right := LShapeRect.Right + LOff;
      if LFillShape then
        aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Fill, TCornerType.Round{CornerType});
      if LDrawShape then
        aCanvas.DrawRectSides(_GetShapeRect, XRadius, YRadius, Corners,  1{AbsoluteOpacity}, Sides, Stroke, TCornerType.Round{CornerType});
    end
    else
    begin
      if LFillShape then
        aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Fill, TCornerType.Round{CornerType});
      if LDrawShape then
        aCanvas.DrawRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Stroke, TCornerType.Round{CornerType});
    end;

    if (Shadow <> nil) and
       (Shadow.Enabled) then begin

      LShadowEffect := TshadowEffect.Create(nil);
      try
        LShadowEffect.ShadowColor := Shadow.Color;
        LShadowEffect.distance := 0; // Specifies the distance between the shadow and the visual object to which TShadowEffect is applied.
                                     // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
        LShadowEffect.Direction := 0;  // Specifies the direction (in degrees) of the shadow.
                                       // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
        LShadowEffect.Opacity := 1; // Opacity is a System.Single value that takes values in the range from 0 through 1.
                                    // we use the opacity of the color instead
        LShadowEffect.softness := Shadow.blur / 24; // Specifies the amount of blur applied to the shadow.
                                                    // Softness is a System.Single value that takes values in the range from 0 through 9.
                                                    // i calculate approximatly that 0.5 = around 12 for blur
        Acanvas.Flush;
        LShadowEffect.ProcessEffect(ACanvas, Acanvas.Bitmap, 1);
      finally
        ALFreeAndNil(LShadowEffect);
      end;

      if Sides <> AllSides then
      begin
        if LFillShape then
          aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Fill, TCornerType.Round{CornerType});
        if LDrawShape then
          aCanvas.DrawRectSides(_GetShapeRect, XRadius, YRadius, Corners,  1{AbsoluteOpacity}, Sides, Stroke, TCornerType.Round{CornerType});
      end
      else
      begin
        if LFillShape then
          aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Fill, TCornerType.Round{CornerType});
        if LDrawShape then
          aCanvas.DrawRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Stroke, TCornerType.Round{CornerType});
      end;

    end;

  finally
    Stroke.Thickness := LStrokeThicknessRestoreValue;
  end;

  {$ENDIF}

end;

{**********************}
procedure ALPaintCircle(
            {$IF defined(ANDROID)}
            const aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            const aContext: CGContextRef;
            const aColorSpace: CGColorSpaceRef;
            const aGridHeight: Single;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            const aCanvas: Tcanvas;
            {$ENDIF}
            const dstRect: TrectF;
            const FillColor: TAlphaColor;
            const StrokeColor: TalphaColor;
            const StrokeThickness: Single;
            const ShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
            const shadowBlur: Single;
            const shadowOffsetX: Single;
            const shadowOffsetY: Single);

{$IF defined(IOS)}
const DefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var LPaint: JPaint;
    LRect: TRectf;
{$ELSEIF defined(IOS)}
var LAlphaColor: TAlphaColorCGFloat;
    LColor: CGColorRef;
    LRect: TRectf;
{$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
var LShapeRect: TRectF;
    LFillKindRestoreValue: TBrushKind;
    LFillColorRestoreValue: TAlphacolor;
    LStrokeKindRestoreValue: TBrushKind;
    LStrokeColorRestoreValue: TAlphacolor;
    LStrokeThicknessRestoreValue: Single;
    LFillShape, LDrawShape: Boolean;
    LShadowEffect: TshadowEffect;
{$ENDIF}

begin

  {$IFDEF ANDROID}

  //create the canvas and the paint
  LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  //init LRect
  if StrokeColor <> TalphaColorRec.Null then begin
    LRect := TrectF.Create(
               dstRect.Left + (StrokeThickness / 2),
               dstRect.Top + (StrokeThickness / 2),
               dstRect.right - (StrokeThickness / 2),
               dstRect.bottom - (StrokeThickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := dstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the circle
  if FillColor <> TalphaColorRec.Null then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LRect + StrokeThickness :( this result&ing in border if the fill is for exemple black and border white

    //fill with solid color
    LPaint.setColor(integer(FillColor));
    if (ShadowColor <> TalphaColorRec.Null) then LPaint.setShadowLayer(ShadowBlur{radius}, ShadowOffsetX{dx}, ShadowOffsetY{dy}, integer(ShadowColor){shadowColor});
    aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
    if (ShadowColor <> TalphaColorRec.Null) then LPaint.clearShadowLayer;

  end;

  //stroke the circle
  if StrokeColor <> TalphaColorRec.Null then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    LPaint.setStrokeWidth(StrokeThickness);

    //stroke with solid color
    LPaint.setColor(integer(StrokeColor));
    aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);

  end;

  //free the paint and the canvas
  LPaint := nil;

  {$ELSEIF DEFINED(IOS)}

  //set the paint default properties
  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
  //-----
  CGContextSetShouldAntialias(aContext, True); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(aContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.

  //init LRect
  if StrokeColor <> TalphaColorRec.Null then begin
    LRect := TrectF.Create(
               DstRect.Left + (StrokeThickness / 2),
               DstRect.Top + (StrokeThickness / 2),
               DstRect.right - (StrokeThickness / 2),
               DstRect.bottom - (StrokeThickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := DstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the circle
  if FillColor <> TalphaColorRec.Null then begin

    //fill with solid color
    LAlphaColor := TAlphaColorCGFloat.Create(FillColor);
    CGContextSetRGBFillColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
    //-----
    if (ShadowColor <> TalphaColorRec.Null) then begin
      LAlphaColor := TAlphaColorCGFloat.Create(ShadowColor);
      LColor := CGColorCreate(aColorSpace, @LAlphaColor);
      try
        CGContextSetShadowWithColor(
          aContext,
          CGSizeMake(ShadowOffsetX, ShadowOffsetY), // offset
          Shadowblur, // blur
          LColor); // color
      finally
        CGColorRelease(LColor);
      end;
    end;
    //-----
    CGContextFillEllipseInRect(
      aContext,
      ALLowerLeftCGRect(
        LRect.TopLeft,
        LRect.Width,
        LRect.Height,
        aGridHeight));
    //-----
    if (ShadowColor <> TalphaColorRec.Null) then begin
      CGContextSetShadowWithColor(
        aContext,
        CGSizeMake(0, 0), // offset
        0, // blur
        nil); // color
    end;

  end;

  //stroke the circle
  if StrokeColor <> TalphaColorRec.Null then begin

    //stroke with solid color
    LAlphaColor := TAlphaColorCGFloat.Create(StrokeColor);
    CGContextSetRGBStrokeColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
    CGContextSetLineWidth(aContext, StrokeThickness);
    CGContextStrokeEllipseInRect(
      aContext,
      ALLowerLeftCGRect(
        LRect.TopLeft,
        LRect.Width,
        LRect.Height,
        aGridHeight));

  end;

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

  LFillKindRestoreValue := ACanvas.Fill.Kind;
  LFillColorRestoreValue := ACanvas.Fill.color;
  LStrokeKindRestoreValue := ACanvas.Stroke.kind;
  LStrokeColorRestoreValue := ACanvas.Stroke.Color;
  LStrokeThicknessRestoreValue := ACanvas.Stroke.Thickness;
  if FillColor <> TAlphaColorRec.Null then begin
    ACanvas.Fill.Kind := TBrushKind.Solid;
    ACanvas.Fill.Color := FillColor;
  end
  else ACanvas.Fill.Kind := TBrushKind.None;
  If StrokeColor <> TalphaColorRec.Null then begin
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Color := StrokeColor;
    ACanvas.Stroke.Thickness := StrokeThickness;
  end
  else ACanvas.Stroke.Kind := TBrushKind.None;
  try

    LShapeRect := ALGetDrawingShapeRectAndSetThickness(DstRect, ACanvas.Fill, ACanvas.Stroke, True, LFillShape, LDrawShape, LStrokeThicknessRestoreValue);

    if LFillShape then
      aCanvas.FillEllipse(LShapeRect, 1{AbsoluteOpacity}, ACanvas.Fill);
    if LDrawShape then
      aCanvas.DrawEllipse(LShapeRect, 1{AbsoluteOpacity}, ACanvas.Stroke);

    if (ShadowColor <> TalphaColorRec.Null) then begin

      LShadowEffect := TshadowEffect.Create(nil);
      try
        LShadowEffect.ShadowColor := ShadowColor;
        LShadowEffect.distance := 0; // Specifies the distance between the shadow and the visual object to which TShadowEffect is applied.
                                     // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
        LShadowEffect.Direction := 0;  // Specifies the direction (in degrees) of the shadow.
                                       // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
        LShadowEffect.Opacity := 1; // Opacity is a System.Single value that takes values in the range from 0 through 1.
                                    // we use the opacity of the color instead
        LShadowEffect.softness := ShadowBlur / 24; // Specifies the amount of blur applied to the shadow.
                                                    // Softness is a System.Single value that takes values in the range from 0 through 9.
                                                    // i calculate approximatly that 0.5 = around 12 for blur
        Acanvas.Flush;
        LShadowEffect.ProcessEffect(ACanvas, Acanvas.Bitmap, 1);
      finally
        ALFreeAndNil(LShadowEffect);
      end;

      if LFillShape then
        aCanvas.FillEllipse(LShapeRect, 1{AbsoluteOpacity}, ACanvas.Fill);
      if LDrawShape then
        aCanvas.DrawEllipse(LShapeRect, 1{AbsoluteOpacity}, ACanvas.Stroke);

    end;

  finally
    ACanvas.Fill.Kind := LFillKindRestoreValue;
    ACanvas.Fill.color := LFillColorRestoreValue;
    ACanvas.Stroke.kind := LStrokeKindRestoreValue;
    ACanvas.Stroke.Color := LStrokeColorRestoreValue;
    ACanvas.Stroke.Thickness := LStrokeThicknessRestoreValue;
  end;

  {$ENDIF}

end;

{**********************}
procedure ALPaintCircle(
            {$IF defined(ANDROID)}
            const aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            const aContext: CGContextRef;
            const aColorSpace: CGColorSpaceRef;
            const aGridHeight: Single;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            const aCanvas: Tcanvas;
            {$ENDIF}
            const dstRect: TrectF;
            const Fill: TBrush;
            const Stroke: TStrokeBrush;
            const Shadow: TALShadow); // If shadow is not nil, then the Canvas must have enough space to draw the shadow (approximately Shadow.blur on each side of the circle)

{$IF defined(IOS)}
const DefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var LTmpBitmap: Jbitmap;
    LShader: JRadialGradient;
    LPaint: JPaint;
    LRect: TRectf;
    LColors: TJavaArray<Integer>;
    LStops: TJavaArray<Single>;
    LPorterDuffXfermode: jPorterDuffXfermode;
    LBitmapInfo: AndroidBitmapInfo;
    LPixelBuffer: Pointer;
    LBitmapData: TBitmapData;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    i: integer;
{$ELSEIF defined(IOS)}
var LAlphaColor: TAlphaColorCGFloat;
    LColor: CGColorRef;
    LCallback: CGFunctionCallbacks;
    LShading: CGShadingRef;
    LFunc: CGFunctionRef;
    LRect: TRectf;
    LBitmapData: TBitmapData;
    LTmpContext: CGContextRef;
    LImageRef: CGImageRef;
    LImage: UIImage;
{$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
var LShapeRect: TRectF;
    LStrokeThicknessRestoreValue: Single;
    LFillShape, LDrawShape: Boolean;
    LShadowEffect: TshadowEffect;
{$ENDIF}

begin

  {$IFDEF ANDROID}

  //create the canvas and the paint
  LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  //init LRect
  if Stroke.Kind <> TBrushKind.None then begin
    LRect := TrectF.Create(
               dstRect.Left + (Stroke.Thickness / 2),
               dstRect.Top + (Stroke.Thickness / 2),
               dstRect.right - (Stroke.Thickness / 2),
               dstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := dstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the circle
  if Fill.Kind <> TBrushKind.None then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LRect + Stroke.Thickness :( this result&ing in border if the fill is for exemple black and border white

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        LColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
        LStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
        for i := 0 to Fill.Gradient.Points.Count - 1 do begin
          LColors[Fill.Gradient.Points.Count - 1 - i] := integer(Fill.Gradient.Points[i].Color);
          LStops[Fill.Gradient.Points.Count - 1 - i] := 1 - Fill.Gradient.Points[i].Offset;
        end;
        LShader := TJRadialGradient.JavaClass.init(LRect.CenterPoint.x{x}, LRect.CenterPoint.y{y}, LRect.width / 2{radius},  LColors, LStops, TJShader_TileMode.JavaClass.CLAMP{tile});
        LPaint.setShader(LShader);
        if (Shadow <> nil) and
           (Shadow.enabled) then LPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.Color){shadowColor});
        aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
        if (Shadow <> nil) and
           (Shadow.enabled) then LPaint.clearShadowLayer;
        LPaint.setShader(nil);
        LShader := nil;
        alfreeandNil(LColors);
        alfreeandNil(LStops);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          //-----
          LTmpBitmap := TJBitmap.JavaClass.createBitmap(fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888);
          //-----
          FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
          if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LBitmapInfo) = 0) and
             (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LPixelBuffer) = 0) then
          try
            if fill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
            try
              ALMove(LBitmapData.Data^, LPixelBuffer^, LBitmapData.Pitch * LBitmapData.Height);
            finally
              fill.Bitmap.Bitmap.Unmap(LBitmapData);
            end;
          finally
            AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID);
          end;
          //-----
          aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
          LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
          LJDestRectf := TJRectf.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);
          LJSrcRect := TJRect.JavaClass.init(0, 0, fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height);
          LPaint.setXfermode(LPorterDuffXfermode);
          if (Shadow <> nil) and
             (Shadow.enabled) then LPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.Color){shadowColor});
          aCanvas.drawBitmap(LTmpBitmap, LJSrcRect, LJDestRectf, LPaint);
          if (Shadow <> nil) and
             (Shadow.enabled) then LPaint.clearShadowLayer;
          LPaint.setXfermode(nil);
          LPorterDuffXfermode := nil;
          LJSrcRect := nil;
          LJDestRectf := nil;
          //-----
          LTmpBitmap.recycle;
          LTmpBitmap := nil;
          //-----
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      LPaint.setColor(integer(Fill.Color));
      if (Shadow <> nil) and
         (Shadow.enabled) then LPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.Color){shadowColor});
      aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
      if (Shadow <> nil) and
         (Shadow.enabled) then LPaint.clearShadowLayer;
    end;

  end;

  //stroke the circle
  if Stroke.Kind <> TBrushKind.None then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    LPaint.setStrokeWidth(Stroke.Thickness);

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      LPaint.setColor(integer(Stroke.Color));
      aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
    end;

  end;

  //free the paint and the canvas
  LPaint := nil;

  {$ELSEIF DEFINED(IOS)}

  //set the paint default properties
  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
  //-----
  CGContextSetShouldAntialias(aContext, True); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(aContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.

  //init LRect
  if Stroke.Kind <> TBrushKind.None then begin
    LRect := TrectF.Create(
               DstRect.Left + (Stroke.Thickness / 2),
               DstRect.Top + (Stroke.Thickness / 2),
               DstRect.right - (Stroke.Thickness / 2),
               DstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := DstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the circle
  if Fill.Kind <> TBrushKind.None then begin

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        CGContextSaveGState(aContext);
        //-----
        LCallback.version := 0;
        LCallback.evaluate := @ALGradientEvaluateCallback;
        LCallback.releaseInfo:= nil;
        LFunc := CGFunctionCreate(
                   fill.Gradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                   1, // domainDimension - The number of inputs.
                   @DefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                   4, // rangeDimension - The number of outputs.
                   nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                   @LCallback); // callbacks - A pointer to a callback function table.
        try
          LShading := CGShadingCreateRadial(
                        aColorSpace, // colorspace
                        CGPoint.Create(TPointF.Create(LRect.Width / 2, LRect.height / 2)), // start - The center of the starting circle, in the shading's target coordinate space.
                        LRect.Width / 2, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                        CGPoint.Create(TPointF.Create(LRect.Width / 2, LRect.Height / 2)), // end - The center of the ending circle, in the shading's target coordinate space.
                        0, // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                        LFunc, // function
                        True, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                        True); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
          try
            CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
            CGContextAddEllipseInRect(
              aContext,
              ALLowerLeftCGRect(
                LRect.TopLeft,
                LRect.Width,
                LRect.Height,
                aGridHeight));
            CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
            CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                     // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                     // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                     // save the graphics state before you clip and restore the graphics state after you’ve completed
                                     // any clipped drawing.
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              LAlphaColor := TAlphaColorCGFloat.Create(Shadow.Color);
              LColor := CGColorCreate(aColorSpace, @LAlphaColor);
              try
                CGContextSetShadowWithColor(
                  aContext,
                  CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                  Shadow.blur, // blur
                  LColor); // color
              finally
                CGColorRelease(LColor);
              end;
            end;
            //-----
            CGContextDrawShading(aContext, LShading);
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              CGContextSetShadowWithColor(
                aContext,
                CGSizeMake(0, 0), // offset
                0, // blur
                nil); // color
            end;
            //-----
          finally
            CGShadingRelease(LShading);
          end;
        finally
          CGFunctionRelease(LFunc);
        end;
        //-----
        CGContextRestoreGState(aContext);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          if fill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
          try
            LTmpContext := CGBitmapContextCreate(
                             LBitmapData.Data, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                               //       memory block should be at least (bytesPerRow*height) bytes.
                                               //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                               //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                             LBitmapData.Width, // width: The width, in pixels, of the required bitmap.
                             LBitmapData.Height, // height: The height, in pixels, of the required bitmap.
                             8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                //                   chapter of Quartz 2D Programming Guide.
                             LBitmapData.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
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
                LImageRef := CGBitmapContextCreateImage(LTmpContext);
                if LImageRef <> nil then
                try
                  LImage := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(LImageRef));
                  if LImage <> nil then
                  try
                    CGContextSaveGState(aContext);
                    //-----
                    CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
                    CGContextAddEllipseInRect(
                      aContext,
                      ALLowerLeftCGRect(
                        LRect.TopLeft,
                        LRect.Width,
                        LRect.Height,
                        aGridHeight)); // Adds an ellipse that fits inside the specified rectangle.
                    CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
                    CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      LAlphaColor := TAlphaColorCGFloat.Create(Shadow.Color);
                      LColor := CGColorCreate(aColorSpace, @LAlphaColor);
                      try
                        CGContextSetShadowWithColor(
                          aContext,
                          CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                          Shadow.blur, // blur
                          LColor); // color
                      finally
                        CGColorRelease(LColor);
                      end;
                    end;
                    //-----
                    CGContextDrawImage(
                      aContext, // c: The graphics context in which to draw the image.
                      ALLowerLeftCGRect(
                        LRect.TopLeft,
                        LRect.Width,
                        LRect.Height,
                        aGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                      LImage.CGImage); // image The image to draw.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      CGContextSetShadowWithColor(
                        aContext,
                        CGSizeMake(0, 0), // offset
                        0, // blur
                        nil); // color
                    end;
                    //-----
                    CGContextRestoreGState(aContext);
                  finally
                    LImage.release;
                  end;
                finally
                  CGImageRelease(LImageRef);
                end;
              finally
                CGContextRelease(LTmpContext);
              end;
            end;
          finally
            fill.Bitmap.Bitmap.Unmap(LBitmapData);
          end;
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      LAlphaColor := TAlphaColorCGFloat.Create(Fill.Color);
      CGContextSetRGBFillColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        LAlphaColor := TAlphaColorCGFloat.Create(Shadow.Color);
        LColor := CGColorCreate(aColorSpace, @LAlphaColor);
        try
          CGContextSetShadowWithColor(
            aContext,
            CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
            Shadow.blur, // blur
            LColor); // color
        finally
          CGColorRelease(LColor);
        end;
      end;
      //-----
      CGContextFillEllipseInRect(
        aContext,
        ALLowerLeftCGRect(
          LRect.TopLeft,
          LRect.Width,
          LRect.Height,
          aGridHeight));
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        CGContextSetShadowWithColor(
          aContext,
          CGSizeMake(0, 0), // offset
          0, // blur
          nil); // color
      end;
      //-----
    end;

  end;

  //stroke the circle
  if Stroke.Kind <> TBrushKind.None then begin

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      LAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
      CGContextSetRGBStrokeColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
      CGContextSetLineWidth(aContext, Stroke.Thickness);
      CGContextStrokeEllipseInRect(
        aContext,
        ALLowerLeftCGRect(
          LRect.TopLeft,
          LRect.Width,
          LRect.Height,
          aGridHeight));
    end;

  end;

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

  LStrokeThicknessRestoreValue := Stroke.Thickness;
  try

    LShapeRect := ALGetDrawingShapeRectAndSetThickness(DstRect, Fill, Stroke, True, LFillShape, LDrawShape, LStrokeThicknessRestoreValue);

    if LFillShape then
      aCanvas.FillEllipse(LShapeRect, 1{AbsoluteOpacity}, Fill);
    if LDrawShape then
      aCanvas.DrawEllipse(LShapeRect, 1{AbsoluteOpacity}, Stroke);

    if (Shadow <> nil) and
       (Shadow.Enabled) then begin

      LShadowEffect := TshadowEffect.Create(nil);
      try
        LShadowEffect.ShadowColor := Shadow.Color;
        LShadowEffect.distance := 0; // Specifies the distance between the shadow and the visual object to which TShadowEffect is applied.
                                     // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
        LShadowEffect.Direction := 0;  // Specifies the direction (in degrees) of the shadow.
                                       // i m too lazy to calculate this from fShadow.offsetX / fShadow.offsetY - if someone want to do it
        LShadowEffect.Opacity := 1; // Opacity is a System.Single value that takes values in the range from 0 through 1.
                                    // we use the opacity of the color instead
        LShadowEffect.softness := Shadow.blur / 24; // Specifies the amount of blur applied to the shadow.
                                                    // Softness is a System.Single value that takes values in the range from 0 through 9.
                                                    // i calculate approximatly that 0.5 = around 12 for blur
        Acanvas.Flush;
        LShadowEffect.ProcessEffect(ACanvas, Acanvas.Bitmap, 1);
      finally
        ALFreeAndNil(LShadowEffect);
      end;

      if LFillShape then
        aCanvas.FillEllipse(LShapeRect, 1{AbsoluteOpacity}, Fill);
      if LDrawShape then
        aCanvas.DrawEllipse(LShapeRect, 1{AbsoluteOpacity}, Stroke);

    end;

  finally
    Stroke.Thickness := LStrokeThicknessRestoreValue;
  end;

  {$ENDIF}

end;

{*******************************}
Procedure ALCreateDrawingSurface(
            {$IF defined(ANDROID)}
            Var aBitmap: Jbitmap;
            var aCanvas: Jcanvas;
            {$ELSEIF defined(IOS)}
            var aBitmapSurface: TbitmapSurface;
            Var aContext: CGContextRef;
            Var aColorSpace: CGColorSpaceRef;
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            Var aBitmap: Tbitmap;
            const aClearBitmap: boolean;
            {$ENDIF}
            const w: integer;
            const h: integer);
begin

  {$IFDEF ANDROID}

  //create the main bitmap on with we will draw
  aBitmap := TJBitmap.JavaClass.createBitmap(W, H, TJBitmap_Config.JavaClass.ARGB_8888);

  //create the canvas and the paint
  aCanvas := TJCanvas.JavaClass.init(aBitmap);

  {$ELSEIF DEFINED(IOS)}

  //create the bitmapSurface
  aBitmapSurface := TbitmapSurface.Create;
  try

    //init aBitmapSurface
    aBitmapSurface.SetSize(W,H);

    //init the color space
    aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
    if aColorSpace = nil then Raise Exception.Create('Call to CGColorSpaceCreateDeviceRGB failed'); // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
    try

      //create the context
      aContext := CGBitmapContextCreate(
                    aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
      if aContext = nil then Raise Exception.Create('Call to CGBitmapContextCreate failed');
      try

        //set the paint default properties
        CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
        //-----
        CGContextSetShouldAntialias(aContext, True); // default: ON
                                                                                                        // Sets anti-aliasing on or off for a graphics context.
        CGContextSetAllowsAntialiasing(aContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
        //-----
        //CGContextSetShouldSmoothFonts(aContext, True); // There are cases, such as rendering to a bitmap, when font smoothing is not appropriate and should be disabled.
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
        //CGContextSetAllowsFontSmoothing(aContext, True); // Sets whether or not to allow font smoothing for a graphics context.
        //-----
        CGContextSetShouldSubpixelPositionFonts(aContext, True); // default: ON
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
        CGContextSetAllowsFontSubpixelPositioning(aContext, True); // Sets whether or not to allow subpixel positioning for a graphics context
        //-----
        CGContextSetShouldSubpixelQuantizeFonts(aContext, True); // default: ON
                                                                                                                    // Enables or disables subpixel quantization in a graphics context.
                                                                                                                    // -----
                                                                                                                    // Subpixel quantization is only enabled if subpixel positioning is enabled. Subpixel
                                                                                                                    // quantization improves the rendering of fonts whose glyphs are at subpixel positions
                                                                                                                    // by more closely examining how the shapes that make up the glyphs cover an individual pixel.
                                                                                                                    // This improvement, requires additional processing so changing this value can affect text
                                                                                                                    // drawing performance.
        CGContextSetAllowsFontSubpixelQuantization(aContext, True);  // Sets whether or not to allow subpixel quantization for a graphics context

      except
        CGContextRelease(aContext);
        raise;
      end;

    Except
      CGColorSpaceRelease(aColorSpace);
      raise;
    end;

  except
    ALFreeAndNil(aBitmapSurface);
    raise;
  end;

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

  aBitmap := Tbitmap.Create(w, H);
  if aClearBitmap then aBitmap.Clear(TAlphaColorRec.Null);

  {$ENDIF}

end;

{*****************************}
procedure ALFreeDrawingSurface(
            {$IF defined(ANDROID)}
            Var aBitmap: Jbitmap;
            var aCanvas: Jcanvas
            {$ELSEIF defined(IOS)}
            var aBitmapSurface: TbitmapSurface;
            Var aContext: CGContextRef;
            Var aColorSpace: CGColorSpaceRef
            {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
            Var aBitmap: Tbitmap
            {$ENDIF});
begin

  {$IFDEF ANDROID}

  aCanvas := nil;
  aBitmap.recycle;
  aBitmap := nil;

  {$ELSEIF DEFINED(IOS)}

  CGContextRelease(aContext);
  CGColorSpaceRelease(aColorSpace);
  ALFreeAndNil(aBitmapSurface);

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

  ALFreeAndNil(aBitmap);

  {$ENDIF}

end;

{****************}
{$IF defined(IOS)}
Procedure ALCreateDrawingSurfaceV2(
            var aBitmapSurface: TbitmapSurface;
            Var aContext: CGContextRef;
            Var aColorSpace: CGColorSpaceRef;
            const w: integer;
            const h: integer);
begin

  //create the bitmapSurface
  aBitmapSurface := TbitmapSurface.Create;
  try

    //init aBitmapSurface
    aBitmapSurface.SetSize(W,H);

    //create the context
    aContext := CGBitmapContextCreate(
                  aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
    if aContext = nil then Raise Exception.Create('Call to CGBitmapContextCreate failed');
    try

      //set the paint default properties
      CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
      //-----
      CGContextSetShouldAntialias(aContext, True); // default: ON
                                                                                                      // Sets anti-aliasing on or off for a graphics context.
      CGContextSetAllowsAntialiasing(aContext, True); // Sets whether or not to allow anti-aliasing for a graphics context.
      //-----
      //CGContextSetShouldSmoothFonts(aContext, True); // There are cases, such as rendering to a bitmap, when font smoothing is not appropriate and should be disabled.
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
      //CGContextSetAllowsFontSmoothing(aContext, True); // Sets whether or not to allow font smoothing for a graphics context.
      //-----
      CGContextSetShouldSubpixelPositionFonts(aContext, True); // default: ON
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
      CGContextSetAllowsFontSubpixelPositioning(aContext, True); // Sets whether or not to allow subpixel positioning for a graphics context
      //-----
      CGContextSetShouldSubpixelQuantizeFonts(aContext, True); // default: ON
                                                                                                                  // Enables or disables subpixel quantization in a graphics context.
                                                                                                                  // -----
                                                                                                                  // Subpixel quantization is only enabled if subpixel positioning is enabled. Subpixel
                                                                                                                  // quantization improves the rendering of fonts whose glyphs are at subpixel positions
                                                                                                                  // by more closely examining how the shapes that make up the glyphs cover an individual pixel.
                                                                                                                  // This improvement, requires additional processing so changing this value can affect text
                                                                                                                  // drawing performance.
      CGContextSetAllowsFontSubpixelQuantization(aContext, True);  // Sets whether or not to allow subpixel quantization for a graphics context

    except
      CGContextRelease(aContext);
      raise;
    end;

  except
    ALFreeAndNil(aBitmapSurface);
    raise;
  end;

end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
procedure ALFreeDrawingSurfaceV2(
            var aBitmapSurface: TbitmapSurface;
            Var aContext: CGContextRef);
begin

  CGContextRelease(aContext);
  ALFreeAndNil(aBitmapSurface);

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
  {$IF defined(MSWINDOWS) or defined(_MACOS)}
  //TCanvas do not work from a background thread
  //under android/ios we use platform API functions
  //do draw images but under windows/macos we use Tcanvas :(
  {$IFNDEF ALCompilerVersionSupported}
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
  {$IF defined(MSWINDOWS) or defined(_MACOS)}
  //TCanvas do not work from a background thread
  //under android/ios we use platform API functions
  //do draw images but under windows/macos we use Tcanvas :(
  {$IFNDEF ALCompilerVersionSupported}
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
  TALGraphicThreadPool.FInstance := nil;
  TALGraphicThreadPool.CreateInstanceFunc := @TALGraphicThreadPool.CreateInstance;

finalization
  ALFreeAndNil(TALGraphicThreadPool.FInstance);

end.
