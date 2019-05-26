unit ALGraphics;

interface

{$IF Defined(ANDROID) or defined(IOS)}
  {$DEFINE _USE_TEXTURE}
{$ENDIF}

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE _MACOS}
{$ENDIF}

uses system.classes,
     system.sysutils,
     system.types,
     system.uitypes,
     {$IF defined(ios)}
     iOSapi.CocoaTypes,
     iOSapi.UIKit,
     fmx.surfaces,
     fmx.types3D,
     {$ENDIF}
     {$IF defined(ANDROID)}
     Androidapi.JNI.GraphicsContentViewText,
     fmx.surfaces,
     fmx.types3D,
     {$ENDIF}
     FMX.graphics;

//get the oritation From Exif
Type

  TalExifOrientationInfo = (FLIP_HORIZONTAL,
                            FLIP_VERTICAL,
                            NORMAL,
                            ROTATE_180,
                            ROTATE_270,
                            ROTATE_90,
                            TRANSPOSE,
                            TRANSVERSE,
                            UNDEFINED);

function  AlGetExifOrientationInfo(const aFilename: String): TalExifOrientationInfo;
procedure ALNormalizeImageOrientationV1(const aBitmap: Tbitmap; const aExifOrientationInfo: TalExifOrientationInfo);
function  ALNormalizeImageOrientationV2(const aBitmap: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aExifOrientationInfo: TalExifOrientationInfo): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  AlGetImageSignature(const aStream: TStream): Tbytes; overload;
function  AlGetImageSignature(const aFileName: string): Tbytes; overload;
function  AlDetectImageExtensionU(const aStream: TStream): String; overload;
function  AlDetectImageExtensionU(const aFileName: string): String; overload;
function  ALPrepareColor(const SrcColor: TAlphaColor; const Opacity: Single): TAlphaColor;

{$IF defined(ANDROID)}
function ALJBitmaptoTexture(const aBitmap: Jbitmap): TTexture;
{$ENDIF}
{$IF defined(ANDROID) or defined(IOS)}
function ALBitmapSurfacetoTexture(const aBitmapSurface: TbitmapSurface): TTexture;
{$ENDIF}
{$IFDEF _USE_TEXTURE}
function ALTransformBitmaptoTexture(var aBitmap: Tbitmap): TTexture;
{$ENDIF}

type

    TALResizeImageGetDestSizeFunct = reference to function(const aOriginalSize: TPointF): TpointF;
    TALResizeAndBlurImageGetDestSizeFunct = reference to function(const aOriginalSize: TPointF; var aRadius: single): TpointF;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
//https://i.stack.imgur.com/CcESX.png - transparent pixel in the mask are removed from the resulting image
function  ALFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap): Tbitmap; overload;
function  ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as round rect
function  ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): Tbitmap; overload;
function  ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as circle
function  ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as rect
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//If any dimension of the image is greater than W or H then the image is scaled down to best fit W and H else the image is cropped with same ratio between W and H
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or lower than w or h
function  ALFitIntoImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap; overload;
function  ALFitIntoImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
function  ALLoadFitIntoResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadFitIntoResourceImageV3(const aResName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
//-----
function  ALLoadFitIntoFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
function  ALLoadFitIntoFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadFitIntoFileImageV3(const aFileName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

//resize the src image to make that width = w and height = h
function  ALStretchImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap; overload;
function  ALStretchImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALStretchImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALStretchImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALStretchImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALStretchImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadStretchResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
function  ALLoadStretchResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadStretchResourceImageV3(const aResName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
//-----
function  ALLoadStretchFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
function  ALLoadStretchFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadStretchFileImageV3(const aFileName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
//-----
function  ALLoadNormalizeOrientationImageV1(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): Tbitmap;
function  ALLoadNormalizeOrientationImageV2(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadNormalizeOrientationImageV3(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
//-----
function  ALLoadNormalizeOrientationFileImageV1(const aFileName: String): Tbitmap;
function  ALLoadNormalizeOrientationFileImageV2(const aFileName: String): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadNormalizeOrientationFileImageV3(const aFileName: String): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

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

implementation

uses system.math,
     System.UIConsts,
     FMX.types,
     {$IF defined(ANDROID)}
     system.math.vectors,
     Androidapi.JNIBridge,
     Androidapi.JNI.JavaTypes,
     Androidapi.Helpers,
     Androidapi.Gles2,
     Androidapi.JNI.Media,
     FMX.Context.GLES,
     FMX.Context.GLES.Android,
     FMX.Helpers.Android,
     fmx.consts,
     ALAndroidApi,
     ALFmxTypes3D,
     ALFmxCommon,
     {$IFDEF DEBUG}
     alString,
     {$ENDIF}
     {$ENDIF}
     {$IF defined(IOS)}
     iOSapi.CoreGraphics,
     iOSapi.Foundation,
     iOSapi.CoreImage,
     Macapi.ObjectiveC,
     Macapi.CoreFoundation,
     Macapi.Helpers,
     ALIosImageIOApi,
     ALFmxTypes3D,
     alFmxCommon,
     {$ENDIF}
     {$IFDEF _USE_TEXTURE}
     FMX.Canvas.GPU,
     {$ENDIF}
     ALCommon;

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

{************************************}
{$IF defined(ANDROID) or defined(IOS)}
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
{$IFDEF _USE_TEXTURE}
function ALTransformBitmaptoTexture(var aBitmap: Tbitmap): TTexture;
var aBitmapSurface: TbitmapSurface;
    aPaintingTexture: TTexture;
begin

  //If TCustomCanvasGpu then simply move the textureID to the result
  if aBitmap.CanvasClass.InheritsFrom(TCustomCanvasGpu) then begin

    //TBitmap.image = TBitmapImage
    //TBitmap.image.handle = TBitmapCtx (but casted as THandle)
    aPaintingTexture := TBitmapCtx(aBitmap.Handle).PaintingTexture;
    Result := TalTexture.Create;
    try

      //assign aPaintingTexture to Result
      Result.assign(aPaintingTexture);

      //set the handle of aTmpTexture to 0 to avoid the
      //textureID to be deleted from OpenGL when we will free aBitmap
      ITextureAccess(aPaintingTexture).Handle := 0;

    except
      ALFreeAndNil(Result);
      raise;
    end;

  end

  //else use a aBitmapSurface to transfert the bitmap to the texture
  else begin

    aBitmapSurface := TbitmapSurface.create;
    try
      aBitmapSurface.Assign(aBitmap);
      result := ALBitmapSurfacetoTexture(aBitmapSurface);
    finally
      alfreeAndNil(aBitmapSurface);
    end;

  end;

  //free the aBitmap as we extract the texture from it
  alFreeAndNil(aBitmap);

end;
{$ENDIF}

{***********************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): Tbitmap;
var aBitmap: TBitmap;
begin

  aBitmap := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(aBitmap);
        Result.Canvas.Fill.bitmap.WrapMode := TWrapMode.TileStretch;
        Result.Canvas.Fill.Kind := TbrushKind.Bitmap;
        Result.Canvas.FillRect(TRectF.Create(0,0, W, H),
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
    AlFreeAndNil(aBitmap);
  end;

end;

{************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap;
var aBitmap: TBitmap;
    D, B, M: TBitmapData;
    w, h: single;
    C: TAlphaColor;
    ratio: single;
    I, J: Integer;
begin

  //init local var
  w := aMask.Width;
  h := aMask.height;

  aBitmap := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      if Result.Map(TMapAccess.Write, D) then
      try
        if aBitmap.Map(TMapAccess.Read, B) then
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
          aBitmap.Unmap(B);
        end;
      finally
        Result.Unmap(D);
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(aBitmap);
  end;

end;

{********************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap): Tbitmap;
begin
  result := ALFitIntoAndCropAsMaskImageV1(aStream, aMask, TpointF.Create(-50,-50));
end;

{********************************************************************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    aCanvas: Jcanvas;
    apaint: JPaint;
    aPorterDuffXfermode: jPorterDuffXfermode;
    w, h: Single;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try

      w := aMask.getWidth;
      h := aMask.getHeight;
      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter);
      aJDestRectf := TJRectf.JavaClass.init(aDestRect.left, aDestRect.top, aDestRect.right, aDestRect.bottom);
      aJSrcRect := TJRect.JavaClass.init(round(aSrcRect.left), round(aSrcRect.top), round(aSrcRect.right), round(aSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      aPaint := TJPaint.JavaClass.init;
      aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      aCanvas := TJCanvas.JavaClass.init(result);
      aPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      aCanvas.drawBitmap(aMask, 0{left}, 0{top}, apaint);
      aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      aPaint.setXfermode(aPorterDuffXfermode);
      aCanvas.drawBitmap(aBitmap, aJSrcRect, aJDestRectf, apaint);
      aPorterDuffXfermode := nil;
      aJSrcRect := nil;
      aJDestRectf := nil;
      aCanvas := nil;
      aPaint := nil;

    finally
      aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALFreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    w, h: Single;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          w := CGImageGetWidth(aMask);
          h := CGImageGetHeight(aMask);
          //-----
          aDestRect := TrectF.Create(0, 0, W, H);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(W), // width: The width, in pixels, of the required bitmap.
                                                round(H), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextClipToMask(aContext,
                                      ALLowerLeftCGRect(TpointF.Create(0, 0),
                                                       w,
                                                       h,
                                                       h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                      aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{****************************************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsMaskImageV2(aStream, aMask, TpointF.Create(-50,-50));
end;

{************************************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  aTmpBitmap := ALFitIntoAndCropAsMaskImageV2(aStream, aMask, aCropCenter);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
    w, h: Single;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            w := CGImageGetWidth(aMask);
            h := CGImageGetHeight(aMask);
            //-----
            aBitmapSurface.SetSize(round(W), round(H));
            //-----
            aDestRect := TrectF.Create(0, 0, W, H);
            aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  round(W), // width: The width, in pixels, of the required bitmap.
                                                  round(H), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextClipToMask(aContext,
                                        ALLowerLeftCGRect(TpointF.Create(0, 0),
                                                         w,
                                                         h,
                                                         h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                        aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                        0-(aSrcRect.top*aRatio)),
                                                         w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                         h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                         h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.assign(aBitmapSurface);
                  except
                    AlFreeAndNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            AlFreeAndNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{********************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsMaskImageV3(aStream, aMask, TpointF.Create(-50,-50));
end;

{*******************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap;
var aBitmap: TBitmap;
    D, B, M: TBitmapData;
    w, h: single;
    C: TAlphaColor;
     ratio: single;
     I, J: Integer;
begin

  //init local var
  w := aMask.Width;
  h := aMask.height;

  aBitmap := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      if Result.Map(TMapAccess.Write, D) then
      try
        if aBitmap.Map(TMapAccess.Read, B) then
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
          aBitmap.Unmap(B);
        end;
      finally
        Result.Unmap(D);
      end;

    except
      AlFreeAndNil(Result);
      raise;
    end;

  finally
    AlFreeAndNil(aBitmap);
  end;

end;

{***************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap;
begin
  result := ALBlurFitIntoAndCropAsMaskImageV1(aStream, aMask, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{***************************************************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aBitmap: Jbitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    aCanvas: Jcanvas;
    apaint: JPaint;
    aPorterDuffXfermode: jPorterDuffXfermode;
    w, h: Single;
begin
  result := nil;
  aBitmap := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if aBitmap <> nil then begin
    try

      w := aMask.getWidth;
      h := aMask.getHeight;
      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter);
      aJDestRectf := TJRectf.JavaClass.init(aDestRect.left, aDestRect.top, aDestRect.right, aDestRect.bottom);
      aJSrcRect := TJRect.JavaClass.init(round(aSrcRect.left), round(aSrcRect.top), round(aSrcRect.right), round(aSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      aPaint := TJPaint.JavaClass.init;
      aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      aCanvas := TJCanvas.JavaClass.init(result);
      aPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      aCanvas.drawBitmap(aMask, 0{left}, 0{top}, apaint);
      aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      aPaint.setXfermode(aPorterDuffXfermode);
      aCanvas.drawBitmap(aBitmap, aJSrcRect, aJDestRectf, apaint);
      aPorterDuffXfermode := nil;
      aJSrcRect := nil;
      aJDestRectf := nil;
      aCanvas := nil;
      aPaint := nil;

    finally
      aBitmap.recycle;
      aBitmap := nil;
    end;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    w, h: Single;
    aCGImageRef: CGImageRef;
begin

  result := nil;
  aCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if aCGImageRef <> nil then begin
    try
      //-----
      w := CGImageGetWidth(aMask);
      h := CGImageGetHeight(aMask);
      //-----
      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, CGImageGetWidth(aCGImageRef), CGImageGetHeight(aCGImageRef)), aCropCenter, ARatio);
      //-----
      aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
      if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
        try
          aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                            round(W), // width: The width, in pixels, of the required bitmap.
                                            round(H), // height: The height, in pixels, of the required bitmap.
                                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                               //                   chapter of Quartz 2D Programming Guide.
                                               //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                               //              a value of 0 causes the value to be calculated automatically.
                                               //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
              CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
              CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
              CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
              CGContextClipToMask(aContext,
                                  ALLowerLeftCGRect(TpointF.Create(0, 0),
                                                   w,
                                                   h,
                                                   h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                  aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
              CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                 ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                  0-(aSrcRect.top*aRatio)),
                                                   w + (aSrcRect.Left*aRatio) + ((CGImageGetWidth(aCGImageRef)-aSrcRect.right)*aRatio),
                                                   h + (aSrcRect.top*aRatio)  + ((CGImageGetHeight(aCGImageRef)-aSrcRect.bottom)*aRatio),
                                                   h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                 aCGImageRef); // image The image to draw.
              result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                              // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                              // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                              // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                              // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                              // you can avoid the actual physical copy of the data.
            finally
              CGContextRelease(aContext);
            end;
          end;
        finally
          CGColorSpaceRelease(aColorSpace);
        end;
      end;
      //-----
    finally
      CGImageRelease(aCGImageRef);
    end;
  end
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALBlurFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter, aBlurRadius, aBlurW, aBlurH);
end;
{$ENDIF}
{$ENDREGION}

{***********************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropAsMaskImageV2(aStream, aMask, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{*******************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  aTmpBitmap := ALBlurFitIntoAndCropAsMaskImageV2(aStream, aMask, aCropCenter, aBlurRadius, aBlurW, aBlurH);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
    w, h: Single;
    aCGImageRef: CGImageRef;
begin

  result := nil;
  aCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if aCGImageRef <> nil then begin
    try
      aBitmapSurface := TbitmapSurface.Create;
      try
        //-----
        w := CGImageGetWidth(aMask);
        h := CGImageGetHeight(aMask);
        //-----
        aBitmapSurface.SetSize(round(W), round(H));
        //-----
        aDestRect := TrectF.Create(0, 0, W, H);
        aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, CGImageGetWidth(aCGImageRef), CGImageGetHeight(aCGImageRef)), aCropCenter, ARatio);
        //-----
        aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
        if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
          try
            aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                              round(W), // width: The width, in pixels, of the required bitmap.
                                              round(H), // height: The height, in pixels, of the required bitmap.
                                              8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                 //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                 //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                 //                   chapter of Quartz 2D Programming Guide.
                                                 //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                              aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                    //              a value of 0 causes the value to be calculated automatically.
                                                                    //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextClipToMask(aContext,
                                    ALLowerLeftCGRect(TpointF.Create(0, 0),
                                                     w,
                                                     h,
                                                     h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                    aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                    0-(aSrcRect.top*aRatio)),
                                                     w + (aSrcRect.Left*aRatio) + ((CGImageGetWidth(aCGImageRef)-aSrcRect.right)*aRatio),
                                                     h + (aSrcRect.top*aRatio)  + ((CGImageGetHeight(aCGImageRef)-aSrcRect.bottom)*aRatio),
                                                     h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                   aCGImageRef); // image The image to draw.
              finally
                CGContextRelease(aContext);
              end;

              result := TALTexture.Create;
              try
                result.assign(aBitmapSurface);
              except
                AlFreeAndNil(result);
                raise;
              end;

            end;
          finally
            CGColorSpaceRelease(aColorSpace);
          end;
        end;
      finally
        AlFreeAndNil(aBitmapSurface);
      end;
    finally
      CGImageRelease(aCGImageRef);
    end;
  end
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALBlurFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter, aBlurRadius, aBlurW, aBlurH);
end;
{$ENDIF}
{$ENDREGION}

{***************************************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropAsMaskImageV3(aStream, aMask, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{*************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV1(aStream, aMask, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV1(aStream, aMask);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV2(aStream, aMask, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*****************************************************************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV2(aStream, aMask);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*************************************************************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV3(aStream, aMask, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV3(aStream, aMask);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*******************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): Tbitmap;
begin
  result := ALFitIntoAndCropAsRoundRectImageV1(aStream, w, h, XRadius, YRadius, TpointF.Create(-50,-50));
end;

{***************************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    aCanvas: Jcanvas;
    apaint: JPaint;
    aPorterDuffXfermode: jPorterDuffXfermode;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try

      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter);
      aJDestRectf := TJRectf.JavaClass.init(aDestRect.left, aDestRect.top, aDestRect.right, aDestRect.bottom);
      aJSrcRect := TJRect.JavaClass.init(round(aSrcRect.left), round(aSrcRect.top), round(aSrcRect.right), round(aSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      aPaint := TJPaint.JavaClass.init;
      aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      aCanvas := TJCanvas.JavaClass.init(result);
      aPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      aCanvas.drawRoundRect(aJDestRectf{rect},
                            xRadius {rx},
                            yRadius {ry},
                            apaint);
      aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      aPaint.setXfermode(aPorterDuffXfermode);
      aCanvas.drawBitmap(aBitmap, aJSrcRect, aJDestRectf, apaint);
      aPorterDuffXfermode := nil;
      aJSrcRect := nil;
      aJDestRectf := nil;
      aCanvas := nil;
      aPaint := nil;

    finally
      aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALFreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aXRadius: single;
    aYradius: Single;
    aWidthMinusCorners: single;
    aHeightMinusCorners: Single;
    aCurPoint: TpointF;
    aGridHeight: single;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(aContext, X, aGridHeight - Y);
      aCurPoint.X := x;
      aCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(aContext,
                                   aCurPoint.X + dx1{cpx},
                                   aGridHeight - (aCurPoint.Y + dy1){cpy},
                                   aCurPoint.X + dx2{x},
                                   aGridHeight - (aCurPoint.Y + dy2){y});
      aCurPoint.X := aCurPoint.X + dx2;
      aCurPoint.Y := aCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(aContext, aCurPoint.X + dx{x}, aGridHeight - (aCurPoint.Y + dy{y}));
      aCurPoint.X := aCurPoint.X + dx;
      aCurPoint.Y := aCurPoint.Y + dy;
    end;

begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestRect := TrectF.Create(0, 0, W, H);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(W), // width: The width, in pixels, of the required bitmap.
                                                round(H), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.

                  aGridHeight := H;
                  aXRadius := xRadius;
                  aYradius := yRadius;
                  if (aXRadius > aDestRect.width / 2) then aXRadius := aDestRect.width / 2;
                  if (aYradius > aDestRect.height / 2) then aYradius := aDestRect.height / 2;
                  aWidthMinusCorners := (aDestRect.width - (2 * aXRadius));
                  aHeightMinusCorners := (aDestRect.height - (2 * aYradius));

                  //----- TopRight
                  _moveTo(aDestRect.right, aDestRect.top + aYradius);
                  _rQuadTo(0, -aYradius, -aXRadius, -aYradius);
                  _rLineTo(-awidthMinusCorners, 0);

                  //----- TopLeft
                  _rQuadTo(-aXRadius, 0, -aXRadius, aYradius);
                  _rLineTo(0, aheightMinusCorners);

                  //----- BottomLeft
                  _rQuadTo(0, aYradius, aXRadius, aYradius);
                  _rLineTo(awidthMinusCorners, 0);

                  //----- BottomRight
                  _rQuadTo(aXRadius, 0, aXRadius, -aYradius);
                  _rLineTo(0, -aheightMinusCorners);

                  CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
                  CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                           // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                           // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                           // save the graphics state before you clip and restore the graphics state after you’ve completed
                                           // any clipped drawing.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropAsRoundRectImageV1(aStream, W, H, XRadius, YRadius, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{***********************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsRoundRectImageV2(aStream, w, h, XRadius, YRadius, TpointF.Create(-50,-50));
end;

{*******************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  aTmpBitmap := ALFitIntoAndCropAsRoundRectImageV2(aStream, W, H, XRadius, YRadius, aCropCenter);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
    aXRadius: single;
    aYradius: Single;
    aWidthMinusCorners: single;
    aHeightMinusCorners: Single;
    aCurPoint: TpointF;
    aGridHeight: single;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(aContext, X, aGridHeight - Y);
      aCurPoint.X := x;
      aCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(aContext,
                                   aCurPoint.X + dx1{cpx},
                                   aGridHeight - (aCurPoint.Y + dy1){cpy},
                                   aCurPoint.X + dx2{x},
                                   aGridHeight - (aCurPoint.Y + dy2){y});
      aCurPoint.X := aCurPoint.X + dx2;
      aCurPoint.Y := aCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(aContext, aCurPoint.X + dx{x}, aGridHeight - (aCurPoint.Y + dy{y}));
      aCurPoint.X := aCurPoint.X + dx;
      aCurPoint.Y := aCurPoint.Y + dy;
    end;

begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aBitmapSurface.SetSize(round(W), round(H));
            //-----
            aDestRect := TrectF.Create(0, 0, W, H);
            aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  round(W), // width: The width, in pixels, of the required bitmap.
                                                  round(H), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.

                    aGridHeight := H;
                    aXRadius := xRadius;
                    aYradius := yRadius;
                    if (aXRadius > aDestRect.width / 2) then aXRadius := aDestRect.width / 2;
                    if (aYradius > aDestRect.height / 2) then aYradius := aDestRect.height / 2;
                    aWidthMinusCorners := (aDestRect.width - (2 * aXRadius));
                    aHeightMinusCorners := (aDestRect.height - (2 * aYradius));

                    //----- TopRight
                    _moveTo(aDestRect.right, aDestRect.top + aYradius);
                    _rQuadTo(0, -aYradius, -aXRadius, -aYradius);
                    _rLineTo(-awidthMinusCorners, 0);

                    //----- TopLeft
                    _rQuadTo(-aXRadius, 0, -aXRadius, aYradius);
                    _rLineTo(0, aheightMinusCorners);

                    //----- BottomLeft
                    _rQuadTo(0, aYradius, aXRadius, aYradius);
                    _rLineTo(awidthMinusCorners, 0);

                    //----- BottomRight
                    _rQuadTo(aXRadius, 0, aXRadius, -aYradius);
                    _rLineTo(0, -aheightMinusCorners);

                    CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
                    CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                        0-(aSrcRect.top*aRatio)),
                                                         w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                         h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                         h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.assign(aBitmapSurface);
                  except
                    AlFreeAndNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            AlFreeAndNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropAsRoundRectImageV1(aStream, W, H, XRadius, YRadius, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{***************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsRoundRectImageV3(aStream, w, h, XRadius, YRadius, TpointF.Create(-50,-50));
end;

{************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var aBitmap: TBitmap;
begin

  aBitmap := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(aBitmap);
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
    AlFreeAndNil(aBitmap);
  end;

end;

{********************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap;
begin
  result := ALFitIntoAndCropAsCircleImageV1(aStream, w, h, TpointF.Create(-50,-50));
end;

{****************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    aCanvas: Jcanvas;
    apaint: JPaint;
    aPorterDuffXfermode: jPorterDuffXfermode;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try

      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter);
      aJDestRectf := TJRectf.JavaClass.init(aDestRect.left, aDestRect.top, aDestRect.right, aDestRect.bottom);
      aJSrcRect := TJRect.JavaClass.init(round(aSrcRect.left), round(aSrcRect.top), round(aSrcRect.right), round(aSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      aPaint := TJPaint.JavaClass.init;
      aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      aCanvas := TJCanvas.JavaClass.init(result);

      aPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      aCanvas.drawCircle(W/2, H/2, W/2, apaint);
      aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      aPaint.setXfermode(aPorterDuffXfermode);
      aCanvas.drawBitmap(aBitmap, aJSrcRect, aJDestRectf, apaint);
      aPorterDuffXfermode := nil;
      aJSrcRect := nil;
      aJDestRectf := nil;
      aCanvas := nil;
      aPaint := nil;

    finally
      aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALFreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestRect := TrectF.Create(0, 0, W, H);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(W), // width: The width, in pixels, of the required bitmap.
                                                round(H), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
                  CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(TPointF.Create(aDestRect.Left, aDestRect.Top),
                                                                        aDestRect.Width,
                                                                        aDestRect.Height,
                                                                        h)); // Adds an ellipse that fits inside the specified rectangle.
                  CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
                  CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                           // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                           // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                           // save the graphics state before you clip and restore the graphics state after you’ve completed
                                           // any clipped drawing.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsCircleImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{********************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  aTmpBitmap := ALFitIntoAndCropAsCircleImageV2(aStream, W, H, aCropCenter);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aBitmapSurface.SetSize(round(W), round(H));
            //-----
            aDestRect := TrectF.Create(0, 0, W, H);
            aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  round(W), // width: The width, in pixels, of the required bitmap.
                                                  round(H), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
                    CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(TPointF.Create(aDestRect.Left, aDestRect.Top),
                                                                          aDestRect.Width,
                                                                          aDestRect.Height,
                                                                          aBitmapSurface.Height)); // Adds an ellipse that fits inside the specified rectangle.
                    CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
                    CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                        0-(aSrcRect.top*aRatio)),
                                                         w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                         h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                         h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.assign(aBitmapSurface);
                  except
                    AlFreeAndNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            AlFreeAndNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{****************************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsCircleImageV3(aStream, w, h, TpointF.Create(-50,-50));
end;

{*************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*****************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV2(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV3(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*****************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{******************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap;
var aBitmap: TBitmap;
begin

  aBitmap := ALBlurFitIntoAndCropImageV1(aStream, W, H, aCropCenter, aBlurRadius);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.Fill.Bitmap.Bitmap.Assign(aBitmap);
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
    AlFreeAndNil(aBitmap);
  end;

end;

{***************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap;
begin
  result := ALBlurFitIntoAndCropAsCircleImageV1(aStream, w, h, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{***********************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aBitmap: Jbitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    aCanvas: Jcanvas;
    apaint: JPaint;
    aPorterDuffXfermode: jPorterDuffXfermode;
begin
  result := nil;
  aBitmap := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if aBitmap <> nil then begin
    try

      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter);
      aJDestRectf := TJRectf.JavaClass.init(aDestRect.left, aDestRect.top, aDestRect.right, aDestRect.bottom);
      aJSrcRect := TJRect.JavaClass.init(round(aSrcRect.left), round(aSrcRect.top), round(aSrcRect.right), round(aSrcRect.bottom));

      Result := TJBitmap.JavaClass.createBitmap(round(w), round(h), TJBitmap_Config.JavaClass.ARGB_8888);

      aPaint := TJPaint.JavaClass.init;
      aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
      aCanvas := TJCanvas.JavaClass.init(result);

      aPaint.setStyle(TJPaint_Style.JavaClass.FILL);
      aCanvas.drawCircle(W/2, H/2, W/2, apaint);
      aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
      aPaint.setXfermode(aPorterDuffXfermode);
      aCanvas.drawBitmap(aBitmap, aJSrcRect, aJDestRectf, apaint);
      aPorterDuffXfermode := nil;
      aJSrcRect := nil;
      aJDestRectf := nil;
      aCanvas := nil;
      aPaint := nil;

    finally
      aBitmap.recycle;
      aBitmap := nil;
    end;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aCGImageRef: CGImageRef;
begin

  result := nil;
  aCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, aBlurW, aBlurH, aCropCenter, aBlurRadius);
  if aCGImageRef <> nil then begin
    try
      //-----
      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, CGImageGetWidth(aCGImageRef), CGImageGetHeight(aCGImageRef)), aCropCenter, ARatio);
      //-----
      aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
      if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
        try
          aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                 //       memory block should be at least (bytesPerRow*height) bytes.
                                                 //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                 //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                            round(W), // width: The width, in pixels, of the required bitmap.
                                            round(H), // height: The height, in pixels, of the required bitmap.
                                            8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                               //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                               //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                               //                   chapter of Quartz 2D Programming Guide.
                                               //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                            0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                               //              a value of 0 causes the value to be calculated automatically.
                                               //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
              CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
              CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
              CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
              CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
              CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(TPointF.Create(aDestRect.Left, aDestRect.Top),
                                                                    aDestRect.Width,
                                                                    aDestRect.Height,
                                                                    h)); // Adds an ellipse that fits inside the specified rectangle.
              CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
              CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                       // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                       // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                       // save the graphics state before you clip and restore the graphics state after you’ve completed
                                       // any clipped drawing.
              CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                 ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                  0-(aSrcRect.top*aRatio)),
                                                   w + (aSrcRect.Left*aRatio) + ((CGImageGetWidth(aCGImageRef)-aSrcRect.right)*aRatio),
                                                   h + (aSrcRect.top*aRatio)  + ((CGImageGetHeight(aCGImageRef)-aSrcRect.bottom)*aRatio),
                                                   h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                 aCGImageRef); // image The image to draw.
              result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                              // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                              // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                              // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                              // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                              // you can avoid the actual physical copy of the data.
            finally
              CGContextRelease(aContext);
            end;
          end;
        finally
          CGColorSpaceRelease(aColorSpace);
        end;
      end;
      //-----
    finally
      CGImageRelease(aCGImageRef);
    end;
  end
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALBlurFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter, aBlurRadius, aBlurW, aBlurH);
end;
{$ENDIF}
{$ENDREGION}

{*******************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropAsCircleImageV2(aStream, w, h, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{***************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  aTmpBitmap := ALBlurFitIntoAndCropAsCircleImageV2(aStream, W, H, aCropCenter, aBlurRadius, aBlurW, aBlurH);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
    aCGImageRef: CGImageRef;
begin

  result := nil;
  aCGImageRef := ALBlurFitIntoAndCropAsCircleImageV2(aStream, W, H, aCropCenter, aBlurRadius, aBlurW, aBlurH);
  if aCGImageRef <> nil then begin
    try
      aBitmapSurface := TbitmapSurface.Create;
      try
        //-----
        aBitmapSurface.SetSize(CGImageGetWidth(aCGImageRef), CGImageGetHeight(aCGImageRef));
        //-----
        aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
        if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
          try
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
                                                 //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                              aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                    //              a value of 0 causes the value to be calculated automatically.
                                                                    //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TpointF.Create(0,0),
                                                     aBitmapSurface.Width,
                                                     aBitmapSurface.Height,
                                                     aBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                   aCGImageRef); // image The image to draw.
              finally
                CGContextRelease(aContext);
              end;

              result := TALTexture.Create;
              try
                result.Assign(aBitmapSurface);
              except
                ALfreeandNil(result);
                raise;
              end;

            end;
          finally
            CGColorSpaceRelease(aColorSpace);
          end;
        end;
      finally
        ALfreeandNil(aBitmapSurface);
      end;
    finally
      CGImageRelease(aCGImageRef);
    end;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALBlurFitIntoAndCropAsCircleImageV1(aStream, W, H, aCropCenter, aBlurRadius, aBlurW, aBlurH);
end;
{$ENDIF}
{$ENDREGION}

{***********************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropAsCircleImageV3(aStream, w, h, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{*****************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap;
var aBitmap: TBitmap;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.width, aBitmap.height));

    Result := TBitmap.Create(round(aDestSize.x),round(aDestSize.y));
    try

      aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.Width, aBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                 aSrcRect, //const SrcRect,
                                 aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{****************************************************************************************************************************}
function ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var aBitmap: TBitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.Width, aBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                 aSrcRect, //const SrcRect,
                                 aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{************************************************************************************************}
function ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap;
begin
  result := ALFitIntoAndCropImageV1(aStream, w, h, TpointF.Create(-50,-50));
end;

{*********************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    ARatio: single;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: Trect;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.getwidth, aBitmap.getheight));
      aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter, ARatio).round;
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, aSrcRect.Left{X}, aSrcRect.top{Y}, aSrcRect.width{Width}, aSrcRect.height{height}, aMatrix{m}, True{filter});
      aMatrix := nil;
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height));
          aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(aDestSize.x), // width: The width, in pixels, of the required bitmap.
                                                round(aDestSize.y), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       aDestSize.x + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       aDestSize.y + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       aDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{********************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: Trect;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter, ARatio).round;
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, aSrcRect.Left{X}, aSrcRect.top{Y}, aSrcRect.width{Width}, aSrcRect.height{height}, aMatrix{m}, True{filter});
      aMatrix := nil;
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestRect := TrectF.Create(0, 0, W, H);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(W), // width: The width, in pixels, of the required bitmap.
                                                round(H), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{****************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{*************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALFitIntoAndCropImageV2(aStream, aGetDestSizeFunct, aCropCenter);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height));
            aBitmapSurface.SetSize(round(aDestSize.x), round(aDestSize.y));
            //-----
            aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
            aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  round(aDestSize.x), // width: The width, in pixels, of the required bitmap.
                                                  round(aDestSize.y), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                        0-(aSrcRect.top*aRatio)),
                                                         aDestSize.x + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                         aDestSize.y + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                         aDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}


{************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALFitIntoAndCropImageV2(aStream, W, H, aCropCenter);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aBitmapSurface.SetSize(round(W), round(H));
            //-----
            aDestRect := TrectF.Create(0, 0, W, H);
            aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  round(W), // width: The width, in pixels, of the required bitmap.
                                                  round(H), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                        0-(aSrcRect.top*aRatio)),
                                                         w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                         h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                         h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{********************************************************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropImageV3(aStream, w, h, TpointF.Create(-50,-50));
end;

{****************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap;
var aBitmap: TBitmap;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aRadius: single;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.width, aBitmap.height), aRadius);

    Result := TBitmap.Create(round(aDestSize.x),round(aDestSize.y));
    try

      aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.Width, aBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                     aSrcRect, //const SrcRect,
                                     aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{*************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): Tbitmap;
var aBitmap: TBitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    Result := TBitmap.Create(round(W),round(H));
    try

      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.Width, aBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                     aSrcRect, //const SrcRect,
                                     aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{*********************************************************************************************************************}
function ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): Tbitmap;
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, w, h, TpointF.Create(-50,-50), aRadius);
end;

{********************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    ARatio: single;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: Trect;
    aRS: JRenderScript;
    aInput: JAllocation;
    aOutPut: JAllocation;
    aScript: JScriptIntrinsicBlur;
    aTmpRadius: Single;
    aRadius: single;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try

      aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.getwidth, aBitmap.getheight), aRadius);
      aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter, ARatio).round;
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, aSrcRect.Left{X}, aSrcRect.top{Y}, aSrcRect.width{Width}, aSrcRect.height{height}, aMatrix{m}, True{filter});
      aMatrix := nil;

      aRS := getRenderScript;
      while compareValue(aRadius, 0, Tepsilon.Vector) > 0 do begin
        aInput := TJAllocation.JavaClass.createFromBitmap(aRS, result);
        aOutPut := TJAllocation.JavaClass.createTyped(aRS, aInput.getType());
        aScript :=  TJScriptIntrinsicBlur.javaclass.create(aRS, TJElement.javaclass.U8_4(aRS));
        aTmpRadius := Min(25, aRadius);
        aRadius := aRadius - aTmpRadius;
        ascript.setRadius(aTmpRadius);
        ascript.setInput(aInput);
        ascript.forEach(aOutput);
        aoutput.copyTo(result);
        ascript := nil;
        aInput := nil;
        aOutPut := nil;
      end;
      aRS := nil;

    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aCGImageRef: CGImageRef;
    aCIContext: CIContext;
    aCIImage: CIImage;
    aClampFilter: CIFilter;
    aBlurFilter : CIFilter;
    aRadius: single;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height), aRadius);
          aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(aDestSize.x), // width: The width, in pixels, of the required bitmap.
                                                round(aDestSize.y), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       aDestSize.x + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       aDestSize.y + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       aDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  aCGImageRef := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                       // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                       // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                       // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                       // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                       // you can avoid the actual physical copy of the data.
                  try

                    aCIContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
                    aCIImage := TCIImage.Wrap(TCIImage.OCClass.imageWithCGImage(aCGImageRef));

                    aClampFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
                    aClampFilter.setDefaults;
                    aClampFilter.setValueforKey((aCIImage as ILocalObject).getObjectId, kCIInputImageKey);

                    aBlurFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
                    aBlurFilter.setValueforKey((aClampFilter.outputImage as ILocalObject).getObjectId, kCIInputImageKey);
                    aBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aRadius), kCIInputRadiusKey);

                    result := aCIContext.createCGImage(aBlurFilter.outputImage, aCIImage.extent);

                    aCIImage := nil; // no need to call aCIImage.release; (i try => exception)
                    aBlurFilter := nil; // no need to call aBlurFilter.release (i try => exception)
                    aClampFilter := nil; // no need to call aClampFilter.release (i try => exception)
                    aCIContext := nil; // no need to call aCIContext.release; (i try => exception)

                  finally
                    CGImageRelease(aCGImageRef);
                  end;
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{*****************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: Trect;
    aRS: JRenderScript;
    aInput: JAllocation;
    aOutPut: JAllocation;
    aScript: JScriptIntrinsicBlur;
    aTmpRadius: Single;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try

      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter, ARatio).round;
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, aSrcRect.Left{X}, aSrcRect.top{Y}, aSrcRect.width{Width}, aSrcRect.height{height}, aMatrix{m}, True{filter});
      aMatrix := nil;

      aRS := getRenderScript;
      while compareValue(aRadius, 0, Tepsilon.Vector) > 0 do begin
        aInput := TJAllocation.JavaClass.createFromBitmap(aRS, result);
        aOutPut := TJAllocation.JavaClass.createTyped(aRS, aInput.getType());
        aScript :=  TJScriptIntrinsicBlur.javaclass.create(aRS, TJElement.javaclass.U8_4(aRS));
        aTmpRadius := Min(25, aRadius);
        aRadius := aRadius - aTmpRadius;
        ascript.setRadius(aTmpRadius);
        ascript.setInput(aInput);
        ascript.forEach(aOutput);
        aoutput.copyTo(result);
        ascript := nil;
        aInput := nil;
        aOutPut := nil;
      end;
      aRS := nil;

    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aCGImageRef: CGImageRef;
    aCIContext: CIContext;
    aCIImage: CIImage;
    aClampFilter: CIFilter;
    aBlurFilter : CIFilter;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestRect := TrectF.Create(0, 0, W, H);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(W), // width: The width, in pixels, of the required bitmap.
                                                round(H), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  aCGImageRef := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                       // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                       // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                       // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                       // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                       // you can avoid the actual physical copy of the data.
                  try

                    aCIContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
                    aCIImage := TCIImage.Wrap(TCIImage.OCClass.imageWithCGImage(aCGImageRef));

                    aClampFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIAffineClamp')));
                    aClampFilter.setDefaults;
                    aClampFilter.setValueforKey((aCIImage as ILocalObject).getObjectId, kCIInputImageKey);

                    aBlurFilter := TCIFilter.Wrap(TCIFilter.OCClass.filterWithName(StrToNsStr('CIGaussianBlur')));
                    aBlurFilter.setValueforKey((aClampFilter.outputImage as ILocalObject).getObjectId, kCIInputImageKey);
                    aBlurFilter.setValueforKey(TNSNumber.OCClass.numberWithFloat(aRadius), kCIInputRadiusKey);

                    result := aCIContext.createCGImage(aBlurFilter.outputImage, aCIImage.extent);

                    aCIImage := nil; // no need to call aCIImage.release; (i try => exception)
                    aBlurFilter := nil; // no need to call aBlurFilter.release (i try => exception)
                    aClampFilter := nil; // no need to call aClampFilter.release (i try => exception)
                    aCIContext := nil; // no need to call aCIContext.release; (i try => exception)

                  finally
                    CGImageRelease(aCGImageRef);
                  end;
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, W, H, aCropCenter, aRadius);
end;
{$ENDIF}
{$ENDREGION}

{*************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50), aRadius);
end;

{************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALBlurFitIntoAndCropImageV2(aStream, aGetDestSizeFunct, aCropCenter);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
    aCGImageRef: CGImageRef;
begin

  result := nil;
  aCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, aGetDestSizeFunct, aCropCenter);
  if aCGImageRef <> nil then begin
    try
      aBitmapSurface := TbitmapSurface.Create;
      try
        //-----
        aBitmapSurface.SetSize(CGImageGetWidth(aCGImageRef), CGImageGetHeight(aCGImageRef));
        //-----
        aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
        if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
          try
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
                                                 //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                              aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                    //              a value of 0 causes the value to be calculated automatically.
                                                                    //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TpointF.Create(0,0),
                                                     aBitmapSurface.Width,
                                                     aBitmapSurface.Height,
                                                     aBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                   aCGImageRef); // image The image to draw.
              finally
                CGContextRelease(aContext);
              end;

              result := TALTexture.Create;
              try
                result.Assign(aBitmapSurface);
              except
                ALfreeandNil(result);
                raise;
              end;

            end;
          finally
            CGColorSpaceRelease(aColorSpace);
          end;
        end;
      finally
        ALfreeandNil(aBitmapSurface);
      end;
    finally
      CGImageRelease(aCGImageRef);
    end;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}


{*********************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALBlurFitIntoAndCropImageV2(aStream, W, H, aCropCenter, aRadius);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
    aCGImageRef: CGImageRef;
begin

  result := nil;
  aCGImageRef := ALBlurFitIntoAndCropImageV2(aStream, W, H, aCropCenter, aRadius);
  if aCGImageRef <> nil then begin
    try
      aBitmapSurface := TbitmapSurface.Create;
      try
        //-----
        aBitmapSurface.SetSize(CGImageGetWidth(aCGImageRef), CGImageGetHeight(aCGImageRef));
        //-----
        aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
        if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
          try
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
                                                 //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                              aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                    //              a value of 0 causes the value to be calculated automatically.
                                                                    //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TpointF.Create(0,0),
                                                     aBitmapSurface.Width,
                                                     aBitmapSurface.Height,
                                                     aBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                   aCGImageRef); // image The image to draw.
              finally
                CGContextRelease(aContext);
              end;

              result := TALTexture.Create;
              try
                result.Assign(aBitmapSurface);
              except
                ALfreeandNil(result);
                raise;
              end;

            end;
          finally
            CGColorSpaceRelease(aColorSpace);
          end;
        end;
      finally
        ALfreeandNil(aBitmapSurface);
      end;
    finally
      CGImageRelease(aCGImageRef);
    end;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALBlurFitIntoAndCropImageV1(aStream, W, H, aCropCenter, aRadius);
end;
{$ENDIF}
{$ENDREGION}

{*****************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropImageV3(aStream, w, h, TpointF.Create(-50,-50), aRadius);
end;

{****************************************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{********************************************************************************************************************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV2(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{****************************************************************************************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV3(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*************************************************************************************************************************}
function ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap;
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV1(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************}
function ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*****************************************************************************************************************************************************************************************************}
function ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV2(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*************************************************************************************************************************************************************************}
function ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{**********************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV3(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{******************************************************************************************************************************************}
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoAndCropImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*******************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap;
var aBitmap: TBitmap;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aRatio: Single;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.width, aBitmap.height));
    if (aDestSize.X > aBitmap.width) and (aDestSize.Y > aBitmap.height) then begin
      if (aDestSize.X / aBitmap.width) > (aDestSize.Y / aBitmap.height) then aRatio := aDestSize.X / aBitmap.width
      else aRatio := aDestSize.Y / aBitmap.height;
      aDestSize := aDestSize / aRatio;
    end;

    Result := TBitmap.Create(round(aDestSize.x),round(aDestSize.y));
    try

      aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.Width, aBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                     aSrcRect, //const SrcRect,
                                     aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{************************************************************************************************************************}
function ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): Tbitmap;
var aBitmap: TBitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aRatio: Single;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    if (W > aBitmap.width) and (H > aBitmap.height) then begin
      if (W / aBitmap.width) > (H / aBitmap.height) then aRatio := W / aBitmap.width
      else aRatio := H / aBitmap.height;
      W := W / aRatio;
      H := H / aRatio;
    end;

    Result := TBitmap.Create(round(W),round(H));
    try

      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.Width, aBitmap.height), aCropCenter);
      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                     aSrcRect, //const SrcRect,
                                     aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{********************************************************************************************}
function ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single): Tbitmap;
begin
  result := ALPlaceIntoAndCropImageV1(aStream, w, h, TpointF.Create(-50,-50));
end;

{***********************************************************************************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    ARatio: single;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: Trect;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.getwidth, aBitmap.getheight));
      if (aDestSize.X > aBitmap.getwidth) and (aDestSize.Y > aBitmap.getheight) then begin
        if (aDestSize.X / aBitmap.getwidth) > (aDestSize.Y / aBitmap.getheight) then aRatio := aDestSize.X / aBitmap.getwidth
        else aRatio := aDestSize.Y / aBitmap.getheight;
        aDestSize := aDestSize / aRatio;
      end;
      aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter, ARatio).round;
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, aSrcRect.Left{X}, aSrcRect.top{Y}, aSrcRect.width{Width}, aSrcRect.height{height}, aMatrix{m}, True{filter});
      aMatrix := nil;
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height));
          if (aDestSize.X > aImage.size.width) and (aDestSize.Y > aImage.size.height) then begin
            if (aDestSize.X / aImage.size.width) > (aDestSize.Y / aImage.size.height) then aRatio := aDestSize.X / aImage.size.width
            else aRatio := aDestSize.Y / aImage.size.height;
            aDestSize := aDestSize / aRatio;
          end;
          aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(aDestSize.x), // width: The width, in pixels, of the required bitmap.
                                                round(aDestSize.y), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       aDestSize.x + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       aDestSize.y + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       aDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALPlaceIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{****************************************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: Trect;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      if (W > aBitmap.getwidth) and (H > aBitmap.getheight) then begin
        if (W / aBitmap.getwidth) > (H / aBitmap.getheight) then aRatio := W / aBitmap.getwidth
        else aRatio := H / aBitmap.getheight;
        W := W / aRatio;
        H := H / aRatio;
      end;
      aDestRect := TrectF.Create(0, 0, W, H);
      aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight), aCropCenter, ARatio).round;
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, aSrcRect.Left{X}, aSrcRect.top{Y}, aSrcRect.width{Width}, aSrcRect.height{height}, aMatrix{m}, True{filter});
      aMatrix := nil;
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          if (W > aImage.size.width) and (H > aImage.size.height) then begin
            if (W / aImage.size.width) > (H / aImage.size.height) then aRatio := W / aImage.size.width
            else aRatio := H / aImage.size.height;
            W := W / aRatio;
            H := H / aRatio;
          end;
          //-----
          aDestRect := TrectF.Create(0, 0, W, H);
          aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                round(W), // width: The width, in pixels, of the required bitmap.
                                                round(H), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                      0-(aSrcRect.top*aRatio)),
                                                       w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                       h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                       h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALPlaceIntoAndCropImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALPlaceIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{***************************************************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALPlaceIntoAndCropImageV2(aStream, aGetDestSizeFunct, aCropCenter);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height));
            if (aDestSize.X > aImage.size.width) and (aDestSize.Y > aImage.size.height) then begin
              if (aDestSize.X / aImage.size.width) > (aDestSize.Y / aImage.size.height) then aRatio := aDestSize.X / aImage.size.width
              else aRatio := aDestSize.Y / aImage.size.height;
              aDestSize := aDestSize / aRatio;
            end;
            aBitmapSurface.SetSize(round(aDestSize.x), round(aDestSize.y));
            //-----
            aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
            aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  round(aDestSize.x), // width: The width, in pixels, of the required bitmap.
                                                  round(aDestSize.y), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                        0-(aSrcRect.top*aRatio)),
                                                         aDestSize.x + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                         aDestSize.y + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                         aDestSize.y), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALPlaceIntoAndCropImageV1(aStream, aGetDestSizeFunct, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}


{********************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALPlaceIntoAndCropImageV2(aStream, W, H, aCropCenter);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    ARatio: single;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            if (W > aImage.size.width) and (H > aImage.size.height) then begin
              if (W / aImage.size.width) > (H / aImage.size.height) then aRatio := W / aImage.size.width
              else aRatio := H / aImage.size.height;
              W := W / aRatio;
              H := H / aRatio;
            end;
            aBitmapSurface.SetSize(round(W), round(H));
            //-----
            aDestRect := TrectF.Create(0, 0, W, H);
            aSrcRect := ALRectFitInto(aDestRect, TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height), aCropCenter, ARatio);
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  round(W), // width: The width, in pixels, of the required bitmap.
                                                  round(H), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(aSrcRect.Left*aRatio),
                                                                        0-(aSrcRect.top*aRatio)),
                                                         w + (aSrcRect.Left*aRatio) + ((aImage.size.Width-aSrcRect.right)*aRatio),
                                                         h + (aSrcRect.top*aRatio)  + ((aImage.size.Height-aSrcRect.bottom)*aRatio),
                                                         h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALPlaceIntoAndCropImageV1(aStream, W, H, aCropCenter);
end;
{$ENDIF}
{$ENDREGION}

{****************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALPlaceIntoAndCropImageV3(aStream, w, h, TpointF.Create(-50,-50));
end;

{************************************************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single; const aCropCenter: TPointF): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV1(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{********************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{****************************************************************************************************************************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV2(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{************************************************************************************************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV3(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*****************************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single; const aCropCenter: TPointF): Tbitmap;
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV1(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*****************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single): Tbitmap;
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*************************************************************************************************************************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV2(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{******************************************************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single; const aCropCenter: TPointF): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV3(aStream, W, H, aCropCenter);
  finally
    ALfreeandNil(aStream);
  end;
end;

{**************************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALPlaceIntoAndCropImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{******************************************************************************************************************************}
function ALFitIntoImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap;
var aBitmap: TBitmap;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.width, aBitmap.height));
    aSrcRect := TrectF.Create(0, 0, aBitmap.width, aBitmap.height);
    aDestRect := aSrcRect.
                   FitInto(
                     TrectF.Create(0, 0, aDestSize.x, aDestSize.y));
    aDestRect.Offset(-aDestRect.TopLeft);

    Result := TBitmap.Create(ceil(aDestRect.Width),ceil(aDestRect.Height));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                 aSrcRect, //const SrcRect,
                                 aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{*****************************************************************************************}
function ALFitIntoImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap;
var aBitmap: TBitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    aSrcRect := TrectF.Create(0, 0, aBitmap.width, aBitmap.height);
    aDestRect := aSrcRect.
                   FitInto(
                     TrectF.Create(0, 0, w, h));
    aDestRect.Offset(-aDestRect.TopLeft);

    Result := TBitmap.Create(ceil(aDestRect.Width),ceil(aDestRect.Height));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                 aSrcRect, //const SrcRect,
                                 aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{**********************************************************************************************************************************************************************************************************}
function ALFitIntoImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: Trectf;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.getwidth, aBitmap.getheight));
      aSrcRect := TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight);
      aDestRect := aSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, aDestSize.x, aDestSize.y));
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, round(aSrcRect.Left){X}, round(aSrcRect.top){Y}, round(aSrcRect.width){Width}, round(aSrcRect.height){height}, aMatrix{m}, True{filter});
      aMatrix := nil;
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height));
          aSrcRect := TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height);
          aDestRect := aSrcRect.
                         FitInto(
                           TrectF.Create(0, 0, aDestSize.x, aDestSize.y));
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                ceil(aDestRect.width), // width: The width, in pixels, of the required bitmap.
                                                ceil(aDestRect.height), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0,0),
                                                       aDestRect.width,
                                                       aDestRect.Height,
                                                       ceil(aDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoImageV1(aStream, aGetDestSizeFunct);
end;
{$ENDIF}
{$ENDREGION}

{*********************************************************************************************************************************************************************}
function ALFitIntoImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    aDestRect: TrectF;
    aSrcRect: Trectf;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      aSrcRect := TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight);
      aDestRect := aSrcRect.
                     FitInto(
                       TrectF.Create(0, 0, W, H));
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, round(aSrcRect.Left){X}, round(aSrcRect.top){Y}, round(aSrcRect.width){Width}, round(aSrcRect.height){height}, aMatrix{m}, True{filter});
      aMatrix := nil;
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aSrcRect := TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height);
          aDestRect := aSrcRect.
                         FitInto(
                           TrectF.Create(0, 0, W, H));
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                ceil(aDestRect.width), // width: The width, in pixels, of the required bitmap.
                                                ceil(aDestRect.height), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0,0),
                                                       aDestRect.width,
                                                       aDestRect.Height,
                                                       ceil(aDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoImageV1(aStream, W, H);
end;
{$ENDIF}
{$ENDREGION}

{**************************************************************************************************************************************************************************}
function ALFitIntoImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALFitIntoImageV2(aStream, aGetDestSizeFunct);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height));
            aSrcRect := TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height);
            aDestRect := aSrcRect.
                           FitInto(
                             TrectF.Create(0, 0, aDestSize.x, aDestSize.y));
            //-----
            aBitmapSurface.SetSize(ceil(aDestRect.width), ceil(aDestRect.height));
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  ceil(aDestRect.width), // width: The width, in pixels, of the required bitmap.
                                                  ceil(aDestRect.height), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0,0),
                                                         aDestRect.width,
                                                         aDestRect.Height,
                                                         ceil(aDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoImageV1(aStream, aGetDestSizeFunct);
end;
{$ENDIF}
{$ENDREGION}


{*************************************************************************************************************************************}
function ALFitIntoImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALFitIntoImageV2(aStream, W, H);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aSrcRect := TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height);
            aDestRect := aSrcRect.
                           FitInto(
                             TrectF.Create(0, 0, W, H));
            //-----
            aBitmapSurface.SetSize(ceil(aDestRect.width), ceil(aDestRect.height));
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  ceil(aDestRect.width), // width: The width, in pixels, of the required bitmap.
                                                  ceil(aDestRect.height), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0,0),
                                                         aDestRect.width,
                                                         aDestRect.Height,
                                                         ceil(aDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALFitIntoImageV1(aStream, W, H);
end;
{$ENDIF}
{$ENDREGION}

{*****************************************************************************************}
function ALLoadFitIntoResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************************************************************}
function ALLoadFitIntoResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{**************************************************************************************************************************************}
function  ALLoadFitIntoResourceImageV3(const aResName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{**************************************************************************************}
function ALLoadFitIntoFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{******************************************************************************************************************************************************************}
function ALLoadFitIntoFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{***********************************************************************************************************************************}
function  ALLoadFitIntoFileImageV3(const aFileName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALFitIntoImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{******************************************************************************************************************************}
function ALStretchImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap;
var aBitmap: TBitmap;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.width, aBitmap.height));
    aSrcRect := TrectF.Create(0, 0, aBitmap.width, aBitmap.height);
    aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);

    Result := TBitmap.Create(ceil(aDestRect.Width),ceil(aDestRect.Height));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                     aSrcRect, //const SrcRect,
                                     aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{*****************************************************************************************}
function ALStretchImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap;
var aBitmap: TBitmap;
    aDestRect: TrectF;
    aSrcRect: TrectF;
begin

  aBitmap := Tbitmap.CreateFromStream(aStream);
  try

    aSrcRect := TrectF.Create(0, 0, aBitmap.width, aBitmap.height);
    aDestRect := TrectF.Create(0, 0, w, h);

    Result := TBitmap.Create(ceil(aDestRect.Width),ceil(aDestRect.Height));
    try

      Result.Clear(TAlphaColorRec.Null);
      if Result.Canvas.BeginScene then
      try
        Result.Canvas.DrawBitmap(aBitmap, // const ABitmap: TBitmap;
                                     aSrcRect, //const SrcRect,
                                     aDestRect, //const DstRect: TRectF;
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
    AlFreeAndNil(aBitmap);
  end;

end;

{**********************************************************************************************************************************************************************************************************}
function ALStretchImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: Trectf;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      aDestSize := aGetDestSizeFunct(TpointF.create(aBitmap.getwidth, aBitmap.getheight));
      aSrcRect := TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight);
      aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, round(aSrcRect.Left){X}, round(aSrcRect.top){Y}, round(aSrcRect.width){Width}, round(aSrcRect.height){height}, aMatrix{m}, True{filter});
      aMatrix := nil;
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height));
          aSrcRect := TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height);
          aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                ceil(aDestRect.width), // width: The width, in pixels, of the required bitmap.
                                                ceil(aDestRect.height), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0,0),
                                                       aDestRect.width,
                                                       aDestRect.Height,
                                                       ceil(aDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALStretchImageV1(aStream, aGetDestSizeFunct);
end;
{$ENDIF}
{$ENDREGION}

{*********************************************************************************************************************************************************************}
function ALStretchImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
    aMatrix: JMatrix;
    aDestRect: TrectF;
    aSrcRect: Trectf;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      aSrcRect := TrectF.Create(0, 0, aBitmap.getWidth, aBitmap.getHeight);
      aDestRect := TrectF.Create(0, 0, W, H);
      aMatrix := TJMatrix.JavaClass.init;
      aMatrix.postScale(aDestRect.width/aSrcRect.width, aDestRect.height/aSrcRect.height);
      result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, round(aSrcRect.Left){X}, round(aSrcRect.top){Y}, round(aSrcRect.width){Width}, round(aSrcRect.height){height}, aMatrix{m}, True{filter});
      aMatrix := nil;
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          //-----
          aSrcRect := TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height);
          aDestRect := TrectF.Create(0, 0, W, H);
          //-----
          aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                     //       memory block should be at least (bytesPerRow*height) bytes.
                                                     //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                     //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                ceil(aDestRect.width), // width: The width, in pixels, of the required bitmap.
                                                ceil(aDestRect.height), // height: The height, in pixels, of the required bitmap.
                                                8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                   //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                   //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                   //                   chapter of Quartz 2D Programming Guide.
                                                   //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                   //              a value of 0 causes the value to be calculated automatically.
                                                   //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0,0),
                                                       aDestRect.width,
                                                       aDestRect.Height,
                                                       ceil(aDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                     aImage.CGImage); // image The image to draw.
                  result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                                  // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                                  // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                                  // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                                  // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                                  // you can avoid the actual physical copy of the data.
                finally
                  CGContextRelease(aContext);
                end;
              end;
            finally
              CGColorSpaceRelease(aColorSpace);
            end;
          end;
          //-----
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALStretchImageV1(aStream, W, H);
end;
{$ENDIF}
{$ENDREGION}

{**************************************************************************************************************************************************************************}
function ALStretchImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALStretchImageV2(aStream, aGetDestSizeFunct);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aDestSize: TpointF;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aDestSize := aGetDestSizeFunct(TpointF.create(aImage.size.width, aImage.size.height));
            aSrcRect := TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height);
            aDestRect := TrectF.Create(0, 0, aDestSize.x, aDestSize.y);
            //-----
            aBitmapSurface.SetSize(ceil(aDestRect.width), ceil(aDestRect.height));
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  ceil(aDestRect.width), // width: The width, in pixels, of the required bitmap.
                                                  ceil(aDestRect.height), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0,0),
                                                         aDestRect.width,
                                                         aDestRect.Height,
                                                         ceil(aDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALStretchImageV1(aStream, aGetDestSizeFunct);
end;
{$ENDIF}
{$ENDREGION}


{*************************************************************************************************************************************}
function ALStretchImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALStretchImageV2(aStream, W, H);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aDestRect: TrectF;
    aSrcRect: TrectF;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try
            //-----
            aSrcRect := TrectF.Create(0, 0, aImage.size.Width, aImage.size.Height);
            aDestRect := TrectF.Create(0, 0, W, H);
            //-----
            aBitmapSurface.SetSize(ceil(aDestRect.width), ceil(aDestRect.height));
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  ceil(aDestRect.width), // width: The width, in pixels, of the required bitmap.
                                                  ceil(aDestRect.height), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0,0),
                                                         aDestRect.width,
                                                         aDestRect.Height,
                                                         ceil(aDestRect.height)), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALStretchImageV1(aStream, W, H);
end;
{$ENDIF}
{$ENDREGION}

{*****************************************************************************************}
function ALLoadStretchResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALStretchImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{*********************************************************************************************************************************************************************}
function ALLoadStretchResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALStretchImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{**************************************************************************************************************************************}
function  ALLoadStretchResourceImageV3(const aResName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TResourceStream;
begin
  aStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALStretchImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{**************************************************************************************}
function ALLoadStretchFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALStretchImageV1(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{******************************************************************************************************************************************************************}
function ALLoadStretchFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALStretchImageV2(aStream, W, H);
  finally
    ALfreeandNil(aStream);
  end;
end;

{***********************************************************************************************************************************}
function  ALLoadStretchFileImageV3(const aFileName: String; const W, H: single): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALStretchImageV3(aStream, W, H);
  finally
    ALfreeandNil(aStream);
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

{***********************************************************************************************************************************************************************************************************************}
function  ALLoadNormalizeOrientationImageV2(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aArray: TJavaArray<Byte>;
    aBitmap: Jbitmap;
begin
  aArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, aArray.Data^, aStream.Size);
    aBitmap := TJBitmapFactory.JavaClass.decodeByteArray(aArray, 0, aStream.Size);
    if aBitmap = nil then Exit(nil);
    try
      result := ALNormalizeImageOrientationV2(aBitmap, aExifOrientationInfo);
    finally
      if not aBitmap.equals(result) then aBitmap.recycle;
      aBitmap := nil;
    end;
  finally
    ALfreeandNil(aArray);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          result := ALNormalizeImageOrientationV2(aImage.CGImage, aExifOrientationInfo);
          if result = aImage.CGImage then CGImageRetain(result);
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALLoadNormalizeOrientationImageV1(aStream, aExifOrientationInfo);
end;
{$ENDIF}
{$ENDREGION}

{***************************************************************************************************************************************************************************************}
function  ALLoadNormalizeOrientationImageV3(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
begin

  //create the aTmpBitmap
  aTmpBitmap := ALLoadNormalizeOrientationImageV2(aStream, aExifOrientationInfo);
  if aTmpBitmap = nil then exit(nil);
  try
    result := ALJBitmaptoTexture(aTmpBitmap);
  finally
    aTmpBitmap.recycle;
    aTmpBitmap := nil;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImage: UIimage;
    aData: NSData;
    aMatrix: CGAffineTransform;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    aBitmapSurface: TBitmapSurface;
    w, h: Single;
begin
  result := nil;
  aData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if aData.length > 0 then begin
      aImage := TUIImage.Wrap(TUIImage.alloc.initWithData(aData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if aImage <> nil then begin
        try
          aBitmapSurface := TbitmapSurface.Create;
          try

            //-----
            w := CGImageGetWidth(aImage.CGImage);
            h := CGImageGetHeight(aImage.CGImage);
            aMatrix := CGAffineTransformIdentity;
            case aExifOrientationInfo of

              //UIImageOrientationUp: The original pixel data matches the image's intended display orientation.
              TalExifOrientationInfo.NORMAL: ;

              //UIImageOrientationUpMirrored: The image has been horizontally flipped from the orientation of its original pixel data
              TalExifOrientationInfo.FLIP_HORIZONTAL: begin
                                                        //amatrix.setScale(-1, 1);
                                                        aMatrix := CGAffineTransformTranslate(aMatrix, w, 0);
                                                        aMatrix := CGAffineTransformScale(aMatrix, -1, 1);
                                                      end;

              //UIImageOrientationDown: The image has been rotated 180° from the orientation of its original pixel data.
              TalExifOrientationInfo.ROTATE_180: begin
                                                   //amatrix.setRotate(180);
                                                   aMatrix := CGAffineTransformTranslate(aMatrix, w, h);
                                                   aMatrix := CGAffineTransformRotate(aMatrix, degToRad(180));
                                                 end;

              //UIImageOrientationDownMirrored: The image has been vertically flipped from the orientation of its original pixel data.
              TalExifOrientationInfo.FLIP_VERTICAL: begin
                                                      //amatrix.setRotate(180);
                                                      //amatrix.postScale(-1, 1);
                                                      aMatrix := CGAffineTransformTranslate(aMatrix, w, h);
                                                      aMatrix := CGAffineTransformRotate(aMatrix, degToRad(180));
                                                      aMatrix := CGAffineTransformTranslate(aMatrix, w, 0);
                                                      aMatrix := CGAffineTransformScale(aMatrix, -1, 1);
                                                    end;

              //UIImageOrientationLeftMirrored: The image has been rotated 90° clockwise and flipped horizontally from the orientation of its original pixel data.
              TalExifOrientationInfo.TRANSPOSE: begin
                                                  //amatrix.setRotate(90);
                                                  //amatrix.postScale(-1, 1);
                                                  aMatrix := CGAffineTransformTranslate(aMatrix, w, 0);
                                                  aMatrix := CGAffineTransformRotate(aMatrix, degToRad(90));
                                                  aMatrix := CGAffineTransformTranslate(aMatrix, h, 0);
                                                  aMatrix := CGAffineTransformScale(aMatrix, -1, 1);
                                                end;

              //UIImageOrientationRight: The image has been rotated 90° clockwise from the orientation of its original pixel data.
              TalExifOrientationInfo.ROTATE_90: begin
                                                  //amatrix.setRotate(90);
                                                  aMatrix := CGAffineTransformTranslate(aMatrix, 0, h);
                                                  aMatrix := CGAffineTransformRotate(aMatrix, -degToRad(90));
                                                end;

              //UIImageOrientationRightMirrored: The image has been rotated 90° COUNTERclockwise and flipped horizontally from the orientation of its original pixel data.
              TalExifOrientationInfo.TRANSVERSE: begin
                                                   //amatrix.setRotate(-90);
                                                   //amatrix.postScale(-1, 1);
                                                   aMatrix := CGAffineTransformTranslate(aMatrix, 0, h);
                                                   aMatrix := CGAffineTransformRotate(aMatrix, -degToRad(90));
                                                   aMatrix := CGAffineTransformTranslate(aMatrix, h, 0);
                                                   aMatrix := CGAffineTransformScale(aMatrix, -1, 1);
                                                 end;

              //UIImageOrientationLeft: The image has been rotated 90° COUNTERclockwise from the orientation of its original pixel data.
              TalExifOrientationInfo.ROTATE_270: begin
                                                   //amatrix.setRotate(-90);
                                                   aMatrix := CGAffineTransformTranslate(aMatrix, w, 0);
                                                   aMatrix := CGAffineTransformRotate(aMatrix, degToRad(90));
                                                 end;

              //UNDEFINED
              TalExifOrientationInfo.UNDEFINED: ;

            end;

            //-----
            if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                                        TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                                        TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                                        TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then aBitmapSurface.SetSize(ceil(h), ceil(w))
            else aBitmapSurface.SetSize(ceil(w), ceil(h));
            //-----
            aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                       //       memory block should be at least (bytesPerRow*height) bytes.
                                                                       //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                       //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                  round(W), // width: The width, in pixels, of the required bitmap.
                                                  round(H), // height: The height, in pixels, of the required bitmap.
                                                  8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                     //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                     //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                     //                   chapter of Quartz 2D Programming Guide.
                                                     //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                                  aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                        //              a value of 0 causes the value to be calculated automatically.
                                                                        //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
                    CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
                    CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextConcatCTM(aContext, aMatrix);
                    if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                                                TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                                                TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                                                TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                                                                                                                            CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                                                                                                                            aImage.CGImage) // image The image to draw.
                    else CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                            CGRectMake(0, 0, w, h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                            aImage.CGImage); // image The image to draw.
                  finally
                    CGContextRelease(aContext);
                  end;

                  result := TALTexture.Create;
                  try
                    result.Assign(aBitmapSurface);
                  except
                    ALfreeandNil(result);
                    raise;
                  end;

                end;
              finally
                CGColorSpaceRelease(aColorSpace);
              end;
            end;
          finally
            ALfreeandNil(aBitmapSurface);
          end;
        finally
          aImage.release;
        end;
      end
    end;
  finally
    aData.release;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := ALLoadNormalizeOrientationImageV1(aStream, aExifOrientationInfo);
end;
{$ENDIF}
{$ENDREGION}

{********************************************************************************}
function  ALLoadNormalizeOrientationFileImageV1(const aFileName: String): Tbitmap;
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALLoadNormalizeOrientationImageV1(aStream, AlGetExifOrientationInfo(aFileName));
  finally
    ALfreeandNil(aStream);
  end;
end;

{************************************************************************************************************************************************************}
function  ALLoadNormalizeOrientationFileImageV2(const aFileName: String): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALLoadNormalizeOrientationImageV2(aStream, AlGetExifOrientationInfo(aFileName));
  finally
    ALfreeandNil(aStream);
  end;
end;

{****************************************************************************************************************************}
function  ALLoadNormalizeOrientationFileImageV3(const aFileName: String): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aStream: TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  try
    aStream.LoadFromFile(aFileName);
    result := ALLoadNormalizeOrientationImageV3(aStream, AlGetExifOrientationInfo(aFileName));
  finally
    ALfreeandNil(aStream);
  end;
end;

{**********************************************************************************}
function  AlGetExifOrientationInfo(const aFilename: String): TalExifOrientationInfo;

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aExifInterface: JExifInterface;
    aOrientation: Integer;
begin
  aExifInterface := TJExifInterface.javaclass.init(StringToJString(aFilename));
  aorientation := aExifInterface.getAttributeInt(TJExifInterface.JavaClass.TAG_ORIENTATION, TJExifInterface.JavaClass.ORIENTATION_NORMAL);
  if aorientation = TJExifInterface.JavaClass.ORIENTATION_FLIP_HORIZONTAL then result := TalExifOrientationInfo.FLIP_HORIZONTAL
  else if aorientation = TJExifInterface.JavaClass.ORIENTATION_FLIP_VERTICAL then result := TalExifOrientationInfo.FLIP_VERTICAL
  else if aorientation = TJExifInterface.JavaClass.ORIENTATION_NORMAL then result := TalExifOrientationInfo.NORMAL
  else if aorientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_180 then result := TalExifOrientationInfo.ROTATE_180
  else if aorientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_270 then result := TalExifOrientationInfo.ROTATE_270
  else if aorientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_90 then result := TalExifOrientationInfo.ROTATE_90
  else if aorientation = TJExifInterface.JavaClass.ORIENTATION_TRANSPOSE then result := TalExifOrientationInfo.TRANSPOSE
  else if aorientation = TJExifInterface.JavaClass.ORIENTATION_TRANSVERSE then result := TalExifOrientationInfo.TRANSVERSE
  else if aorientation = TJExifInterface.JavaClass.ORIENTATION_UNDEFINED then result := TalExifOrientationInfo.UNDEFINED
  else result := TalExifOrientationInfo.UNDEFINED;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aImgSourceRef: CGImageSourceRef;
    aPath: CFStringRef;
    aUrl: CFURLRef;
    aDictionaryRef: CFDictionaryRef;
    aOrientation: NSNumber;
begin
  result := TalExifOrientationInfo.UNDEFINED;
  aPath := CFStringCreateWithCString(nil{alloc}, MarshaledAString(UTF8Encode(AFileName)){cStr}, kCFStringEncodingUTF8{encoding});
  try
    aUrl := CFURLCreateWithFileSystemPath(nil{allocator}, aPath{filePath}, kCFURLPOSIXPathStyle{pathStyle}, False{isDirectory});
    try
      aImgSourceRef := CGImageSourceCreateWithURL(aUrl{url}, nil{options});
      if aImgSourceRef <> nil then
      try
        aDictionaryRef := CGImageSourceCopyPropertiesAtIndex(aImgSourceRef{isrc}, 0{index}, nil{options});
        if aDictionaryRef <> nil then
        try
          aOrientation := TNSNumber.Wrap(CFDictionaryGetValue(aDictionaryRef, NSStringToID(kCGImagePropertyOrientation)));
          if aOrientation <> nil then begin

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

            case aOrientation.integerValue of

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
          CGImageRelease(aDictionaryRef);
        end;
      finally
        CFRelease(aimgSourceRef);
      end;
    finally
      CFRelease(aUrl);
    end;
  finally
    CFRelease(aPath);
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
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

{***********************************************************************************************************************************************************************************************************************************************************************************}
function  ALNormalizeImageOrientationV2(const aBitmap: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aExifOrientationInfo: TalExifOrientationInfo): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var aMatrix: JMatrix;
begin
  aMatrix := TJMatrix.JavaClass.init;
  case aExifOrientationInfo of
    TalExifOrientationInfo.NORMAL: exit(aBitmap);
    TalExifOrientationInfo.FLIP_HORIZONTAL: amatrix.setScale(-1, 1);
    TalExifOrientationInfo.ROTATE_180: amatrix.setRotate(180);
    TalExifOrientationInfo.FLIP_VERTICAL: begin
                                            amatrix.setRotate(180);
                                            amatrix.postScale(-1, 1);
                                          end;
    TalExifOrientationInfo.TRANSPOSE: begin
                                        amatrix.setRotate(90);
                                        amatrix.postScale(-1, 1);
                                      end;
    TalExifOrientationInfo.ROTATE_90: amatrix.setRotate(90);
    TalExifOrientationInfo.TRANSVERSE: begin
                                         amatrix.setRotate(-90);
                                         amatrix.postScale(-1, 1);
                                       end;
    TalExifOrientationInfo.ROTATE_270: amatrix.setRotate(-90);
    TalExifOrientationInfo.UNDEFINED: exit(aBitmap);
    else exit(aBitmap);
  end;
  result := TJBitmap.JavaClass.createBitmap(aBitmap{src}, 0{X}, 0{Y}, aBitmap.getwidth{Width}, aBitmap.getheight{height}, aMatrix{m}, True{filter});
  aMatrix := nil;
end;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
var aMatrix: CGAffineTransform;
    aContext: CGContextRef;
    aColorSpace: CGColorSpaceRef;
    w, h: Single;
begin

  //-----
  result := aBitmap;
  w := CGImageGetWidth(aBitmap);
  h := CGImageGetHeight(aBitmap);
  aMatrix := CGAffineTransformIdentity;
  case aExifOrientationInfo of

    //UIImageOrientationUp: The original pixel data matches the image's intended display orientation.
    TalExifOrientationInfo.NORMAL: exit(aBitmap);

    //UIImageOrientationUpMirrored: The image has been horizontally flipped from the orientation of its original pixel data
    TalExifOrientationInfo.FLIP_HORIZONTAL: begin
                                              //amatrix.setScale(-1, 1);
                                              aMatrix := CGAffineTransformTranslate(aMatrix, w, 0);
                                              aMatrix := CGAffineTransformScale(aMatrix, -1, 1);
                                            end;

    //UIImageOrientationDown: The image has been rotated 180° from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_180: begin
                                         //amatrix.setRotate(180);
                                         aMatrix := CGAffineTransformTranslate(aMatrix, w, h);
                                         aMatrix := CGAffineTransformRotate(aMatrix, degToRad(180));
                                       end;

    //UIImageOrientationDownMirrored: The image has been vertically flipped from the orientation of its original pixel data.
    TalExifOrientationInfo.FLIP_VERTICAL: begin
                                            //amatrix.setRotate(180);
                                            //amatrix.postScale(-1, 1);
                                            aMatrix := CGAffineTransformTranslate(aMatrix, w, h);
                                            aMatrix := CGAffineTransformRotate(aMatrix, degToRad(180));
                                            aMatrix := CGAffineTransformTranslate(aMatrix, w, 0);
                                            aMatrix := CGAffineTransformScale(aMatrix, -1, 1);
                                          end;

    //UIImageOrientationLeftMirrored: The image has been rotated 90° clockwise and flipped horizontally from the orientation of its original pixel data.
    TalExifOrientationInfo.TRANSPOSE: begin
                                        //amatrix.setRotate(90);
                                        //amatrix.postScale(-1, 1);
                                        aMatrix := CGAffineTransformTranslate(aMatrix, w, 0);
                                        aMatrix := CGAffineTransformRotate(aMatrix, degToRad(90));
                                        aMatrix := CGAffineTransformTranslate(aMatrix, h, 0);
                                        aMatrix := CGAffineTransformScale(aMatrix, -1, 1);
                                      end;

    //UIImageOrientationRight: The image has been rotated 90° clockwise from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_90: begin
                                        //amatrix.setRotate(90);
                                        aMatrix := CGAffineTransformTranslate(aMatrix, 0, h);
                                        aMatrix := CGAffineTransformRotate(aMatrix, -degToRad(90));
                                      end;

    //UIImageOrientationRightMirrored: The image has been rotated 90° COUNTERclockwise and flipped horizontally from the orientation of its original pixel data.
    TalExifOrientationInfo.TRANSVERSE: begin
                                         //amatrix.setRotate(-90);
                                         //amatrix.postScale(-1, 1);
                                         aMatrix := CGAffineTransformTranslate(aMatrix, 0, h);
                                         aMatrix := CGAffineTransformRotate(aMatrix, -degToRad(90));
                                         aMatrix := CGAffineTransformTranslate(aMatrix, h, 0);
                                         aMatrix := CGAffineTransformScale(aMatrix, -1, 1);
                                       end;

    //UIImageOrientationLeft: The image has been rotated 90° COUNTERclockwise from the orientation of its original pixel data.
    TalExifOrientationInfo.ROTATE_270: begin
                                         //amatrix.setRotate(-90);
                                         aMatrix := CGAffineTransformTranslate(aMatrix, w, 0);
                                         aMatrix := CGAffineTransformRotate(aMatrix, degToRad(90));
                                       end;

    //UNDEFINED
    TalExifOrientationInfo.UNDEFINED: exit(aBitmap);
    else exit(aBitmap);

  end;

  //-----
  aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
  if aColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
    try
      aContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                             //       memory block should be at least (bytesPerRow*height) bytes.
                                             //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                             //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                        round(W), // width: The width, in pixels, of the required bitmap.
                                        round(H), // height: The height, in pixels, of the required bitmap.
                                        8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                           //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                           //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                           //                   chapter of Quartz 2D Programming Guide.
                                           //                   we can also use CGImageGetBitsPerComponent(aImage.CGImage) but 8 it's what we need
                                        0, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                           //              a value of 0 causes the value to be calculated automatically.
                                           //              we could also use CGImageGetBytesPerRow(aImage.CGImage) or W * 4
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
          CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context.
          CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
          CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
          CGContextConcatCTM(aContext, aMatrix);
          if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                                      TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                                      TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                                      TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                                                                                                                  CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                                                                                                                  abitmap) // image The image to draw.
          else CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                  CGRectMake(0, 0, w, h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                  abitmap); // image The image to draw.
          result := CGBitmapContextCreateImage(aContext); // The CGImage object returned by this function is created by a copy operation. Subsequent changes to the bitmap
                                                          // graphics context do not affect the contents of the returned image. In some cases the copy operation actually
                                                          // follows copy-on-write semantics, so that the actual physical copy of the bits occur only if the underlying
                                                          // data in the bitmap graphics context is modified. As a consequence, you may want to use the resulting
                                                          // image and release it before you perform additional drawing into the bitmap graphics context. In this way,
                                                          // you can avoid the actual physical copy of the data.
        finally
          CGContextRelease(aContext);
        end;
      end;
    finally
      CGColorSpaceRelease(aColorSpace);
    end;
  end;

end;
{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS / _MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
begin
  result := aBitmap;
  ALNormalizeImageOrientationV1(result, aExifOrientationInfo);
end;
{$ENDIF}
{$ENDREGION}

{************************************************************}
function  AlGetImageSignature(const aStream: TStream): Tbytes;
var i: integer;
begin
  aStream.Position := 0;
  SetLength(result, 12);
  if aStream.Size < 12 then begin
    for I := Low(result) to High(result) do
      result[i] := $00;
    exit;
  end;
  aStream.ReadBuffer(result[0], length(result));
end;

{*************************************************************}
function  AlGetImageSignature(const aFileName: string): Tbytes;
var aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    result := AlGetImageSignature(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

{******************************************************}
// https://en.wikipedia.org/wiki/List_of_file_signatures
// https://github.com/strukturag/libheif/issues/83
// https://nokiatech.github.io/heif/technical.html
function  AlDetectImageExtensionU(const aStream: Tstream): String;
var aFirstBytes: Tbytes;
begin

  aFirstBytes := AlGetImageSignature(aStream);
  if length(aFirstBytes) < 12 then exit('');

  if (aFirstBytes[0] = $FF) and
     (aFirstBytes[1] = $D8) then result := 'jpg'  // ÿØ
  else if (aFirstBytes[0] = $89) and
          (aFirstBytes[1] = $50) and
          (aFirstBytes[2] = $4E) and
          (aFirstBytes[3] = $47) and
          (aFirstBytes[4] = $0D) and
          (aFirstBytes[5] = $0A) and
          (aFirstBytes[6] = $1A ) and
          (aFirstBytes[7] = $0A) then result := 'png' // .PNG....
  else if (aFirstBytes[0] = $47) and
          (aFirstBytes[1] = $49) and
          (aFirstBytes[2] = $46) then result := 'gif' // GIF
  else if (aFirstBytes[0] = $42) and
          (aFirstBytes[1] = $4D) then result := 'bmp' // BM
  else if (aFirstBytes[0] = $00) and
          (aFirstBytes[1] = $00) and
          (aFirstBytes[2] = $00) and
          (aFirstBytes[3] = $18) and
          (aFirstBytes[4] = $66) and
          (aFirstBytes[5] = $74) and
          (aFirstBytes[6] = $79) and
          (aFirstBytes[7] = $70) and
          (aFirstBytes[8] = $68) and
          (aFirstBytes[9] = $65) and
          (aFirstBytes[10] = $69) and
          ((aFirstBytes[11] = $63{c}) or
           (aFirstBytes[11] = $78{x})) then result := 'heic' // ftypheic or ftypheix....
  else if (aFirstBytes[0] = $00) and
          (aFirstBytes[1] = $00) and
          (aFirstBytes[2] = $00) and
          (aFirstBytes[3] = $18) and
          (aFirstBytes[4] = $66) and
          (aFirstBytes[5] = $74) and
          (aFirstBytes[6] = $79) and
          (aFirstBytes[7] = $70) and
          (aFirstBytes[8] = $6D) and
          (aFirstBytes[9] = $69) and
          (aFirstBytes[10] = $66) and
          (aFirstBytes[11] = $31) then result := 'heif' // ftypmif1....
  else result := '';

end;

{*****************************************************************}
function  AlDetectImageExtensionU(const aFileName: string): String;
var aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    result := AlDetectImageExtensionU(aFileStream);
  finally
    aFileStream.Free;
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

end.
