unit ALGraphics;

interface

{$I Alcinoe.inc}

uses
  system.classes,
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
function  AlGetImageSignature(const aStream: TStream; const aSignatureLength: integer = 12): Tbytes; overload;
function  AlGetImageSignature(const aFileName: string; const aSignatureLength: integer = 12): Tbytes; overload;
function  AlDetectImageExtensionU(const aStream: TStream): String; overload;
function  AlDetectImageExtensionU(const aFileName: string): String; overload;
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
function  ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsMaskImageV1(const aStream: TCustomMemoryStream; const aMask: Tbitmap; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV1(const aResName: String; const aMask: Tbitmap): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as round rect
function  ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropAsRoundRectImageV1(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): Tbitmap; overload;
function  ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as circle
function  ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV1(const aResName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsCircleImageV1(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as rect
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV1(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): Tbitmap; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceImageV1(const aResName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadFitIntoAndCropFileImageV1(const aFileName: String; const W, H: single): Tbitmap; overload;
function  ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//If any dimension of the image is greater than W or H then the image is scaled down to best fit W and H else the image is cropped with same ratio between W and H
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV1(const aStream: TCustomMemoryStream; W, H: single): Tbitmap; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropResourceImageV1(const aResName: String; W, H: single): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single; const aCropCenter: TPointF): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropFileImageV1(const aFileName: String; W, H: single): Tbitmap; overload;
function  ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;

//resize the src image to make that one side fit w or h keeping the other side equal or lower than w or h
function  ALFitIntoImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap; overload;
function  ALFitIntoImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALFitIntoImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALFitIntoImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadFitIntoResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
function  ALLoadFitIntoResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadFitIntoResourceImageV3(const aResName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
//-----
function  ALLoadFitIntoFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
function  ALLoadFitIntoFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadFitIntoFileImageV3(const aFileName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

//resize the src image to make that width = w and height = h
function  ALStretchImageV1(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): Tbitmap; overload;
function  ALStretchImageV1(const aStream: TCustomMemoryStream; const W, H: single): Tbitmap; overload;
function  ALStretchImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALStretchImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALStretchImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALStretchImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
//-----
function  ALLoadStretchResourceImageV1(const aResName: String; const W, H: single): Tbitmap;
function  ALLoadStretchResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadStretchResourceImageV3(const aResName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
//-----
function  ALLoadStretchFileImageV1(const aFileName: String; const W, H: single): Tbitmap;
function  ALLoadStretchFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadStretchFileImageV3(const aFileName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
//-----
function  ALLoadNormalizeOrientationImageV1(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): Tbitmap;
function  ALLoadNormalizeOrientationImageV2(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadNormalizeOrientationImageV3(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
//-----
function  ALLoadNormalizeOrientationFileImageV1(const aFileName: String): Tbitmap;
function  ALLoadNormalizeOrientationFileImageV2(const aFileName: String): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
function  ALLoadNormalizeOrientationFileImageV3(const aFileName: String): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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

uses
  system.math,
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
  {$IFDEF ALUseTexture}
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

{********************************************************************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextClipToMask(LContext,
                                      ALLowerLeftCGRect(TpointF.Create(0, 0),
                                                       w,
                                                       h,
                                                       h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                      aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{****************************************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsMaskImageV2(aStream, aMask, TpointF.Create(-50,-50));
end;

{************************************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextClipToMask(LContext,
                                        ALLowerLeftCGRect(TpointF.Create(0, 0),
                                                         w,
                                                         h,
                                                         h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                        aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                        0-(LSrcRect.top*LRatio)),
                                                         w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                         h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{********************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{***************************************************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
          LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
              CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
              CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
              CGContextClipToMask(LContext,
                                  ALLowerLeftCGRect(TpointF.Create(0, 0),
                                                   w,
                                                   h,
                                                   h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                  aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
              CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                 ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
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

{***********************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV2(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropAsMaskImageV2(aStream, aMask, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{*******************************************************************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
            LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextClipToMask(LContext,
                                    ALLowerLeftCGRect(TpointF.Create(0, 0),
                                                     w,
                                                     h,
                                                     h), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                    aMask); // Maps a mask into the specified rectangle and intersects it with the current clipping area of the graphics context.
                CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
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

{***************************************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsMaskImageV3(const aStream: TCustomMemoryStream; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{*********************************************************************************************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV2(LStream, aMask, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV2(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV2(LStream, aMask);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsMaskImageV3(LStream, aMask, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsMaskImageV3(const aResName: String; const aMask: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{***************************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
      LCanvas.drawRoundRect(LJDestRectf{rect},
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
      CGContextAddQuadCurveToPoint(LContext,
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
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
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{***********************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV2(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsRoundRectImageV2(aStream, w, h, XRadius, YRadius, TpointF.Create(-50,-50));
end;

{*******************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
      CGContextAddQuadCurveToPoint(LContext,
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
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
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                        0-(LSrcRect.top*LRatio)),
                                                         w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                         h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{***************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsRoundRectImageV3(const aStream: TCustomMemoryStream; const W, H: single; const XRadius, YRadius: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{****************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextBeginPath(LContext);  // Creates a new empty path in a graphics context.
                  CGContextAddEllipseInRect(LContext, ALLowerLeftCGRect(TPointF.Create(LDestRect.Left, LDestRect.Top),
                                                                        LDestRect.Width,
                                                                        LDestRect.Height,
                                                                        h)); // Adds an ellipse that fits inside the specified rectangle.
                  CGContextClosePath(LContext); // Closes and terminates the current path’s subpath.
                  CGContextClip(LContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                           // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                           // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                           // save the graphics state before you clip and restore the graphics state after you’ve completed
                                           // any clipped drawing.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{************************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropAsCircleImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{********************************************************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextBeginPath(LContext);  // Creates a new empty path in a graphics context.
                    CGContextAddEllipseInRect(LContext, ALLowerLeftCGRect(TPointF.Create(LDestRect.Left, LDestRect.Top),
                                                                          LDestRect.Width,
                                                                          LDestRect.Height,
                                                                          LBitmapSurface.Height)); // Adds an ellipse that fits inside the specified rectangle.
                    CGContextClosePath(LContext); // Closes and terminates the current path’s subpath.
                    CGContextClip(LContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                        0-(LSrcRect.top*LRatio)),
                                                         w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                         h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{****************************************************************************************************************************************************}
function ALFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{*****************************************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV2(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV3(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceAsCircleImageV3(const aResName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropAsCircleImageV3(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{******************************************************************************************************************************************************************************************}
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

{***********************************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
          LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
              CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
              CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
              CGContextBeginPath(LContext);  // Creates a new empty path in a graphics context.
              CGContextAddEllipseInRect(LContext, ALLowerLeftCGRect(TPointF.Create(LDestRect.Left, LDestRect.Top),
                                                                    LDestRect.Width,
                                                                    LDestRect.Height,
                                                                    h)); // Adds an ellipse that fits inside the specified rectangle.
              CGContextClosePath(LContext); // Closes and terminates the current path’s subpath.
              CGContextClip(LContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                       // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                       // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                       // save the graphics state before you clip and restore the graphics state after you’ve completed
                                       // any clipped drawing.
              CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                 ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
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

{*******************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV2(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropAsCircleImageV2(aStream, w, h, TpointF.Create(-50,-50), aBlurRadius, aBlurW, aBlurH);
end;

{***************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
            LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TpointF.Create(0,0),
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

{***********************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropAsCircleImageV3(const aStream: TCustomMemoryStream; const W, H: single; aBlurRadius: single; const aBlurW, aBlurH: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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

{*********************************************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height));
          LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       LDestSize.x + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       LDestSize.y + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{********************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{****************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALFitIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{*************************************************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height));
            LBitmapSurface.SetSize(round(LDestSize.x), round(LDestSize.y));
            //-----
            LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                        0-(LSrcRect.top*LRatio)),
                                                         LDestSize.x + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                         LDestSize.y + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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


{************************************************************************************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                        0-(LSrcRect.top*LRatio)),
                                                         w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                         h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{********************************************************************************************************************************************}
function ALFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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

{********************************************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height), LRadius);
          LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       LDestSize.x + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       LDestSize.y + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{*****************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{*************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV2(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALBlurFitIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50), aRadius);
end;

{************************************************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeAndBlurImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
            LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TpointF.Create(0,0),
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


{*********************************************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; const aCropCenter: TPointF; aRadius: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
            LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TpointF.Create(0,0),
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

{*****************************************************************************************************************************************************************}
function ALBlurFitIntoAndCropImageV3(const aStream: TCustomMemoryStream; const W, H: single; aRadius: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{********************************************************************************************************************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV2(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{****************************************************************************************************************************************************************************}
function ALLoadFitIntoAndCropResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*************************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoAndCropImageV3(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************************}
function  ALLoadFitIntoAndCropResourceImageV3(const aResName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{*****************************************************************************************************************************************************************************************************}
function ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
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

{*************************************************************************************************************************************************************************}
function ALLoadFitIntoAndCropFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
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

{**********************************************************************************************************************************************************************}
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{******************************************************************************************************************************************}
function  ALLoadFitIntoAndCropFileImageV3(const aFileName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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

{***********************************************************************************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height));
          if (LDestSize.X > LImage.size.width) and (LDestSize.Y > LImage.size.height) then begin
            if (LDestSize.X / LImage.size.width) > (LDestSize.Y / LImage.size.height) then LRatio := LDestSize.X / LImage.size.width
            else LRatio := LDestSize.Y / LImage.size.height;
            LDestSize := LDestSize / LRatio;
          end;
          LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       LDestSize.x + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       LDestSize.y + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{****************************************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          if (W > LImage.size.width) and (H > LImage.size.height) then begin
            if (W / LImage.size.width) > (H / LImage.size.height) then LRatio := W / LImage.size.width
            else LRatio := H / LImage.size.height;
            W := W / LRatio;
            H := H / LRatio;
          end;
          //-----
          LDestRect := TrectF.Create(0, 0, W, H);
          LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                      0-(LSrcRect.top*LRatio)),
                                                       w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                       h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV2(const aStream: TCustomMemoryStream; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
begin
  result := ALPlaceIntoAndCropImageV2(aStream, w, h, TpointF.Create(-50,-50));
end;

{***************************************************************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height));
            if (LDestSize.X > LImage.size.width) and (LDestSize.Y > LImage.size.height) then begin
              if (LDestSize.X / LImage.size.width) > (LDestSize.Y / LImage.size.height) then LRatio := LDestSize.X / LImage.size.width
              else LRatio := LDestSize.Y / LImage.size.height;
              LDestSize := LDestSize / LRatio;
            end;
            LBitmapSurface.SetSize(round(LDestSize.x), round(LDestSize.y));
            //-----
            LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                        0-(LSrcRect.top*LRatio)),
                                                         LDestSize.x + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                         LDestSize.y + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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


{********************************************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            if (W > LImage.size.width) and (H > LImage.size.height) then begin
              if (W / LImage.size.width) > (H / LImage.size.height) then LRatio := W / LImage.size.width
              else LRatio := H / LImage.size.height;
              W := W / LRatio;
              H := H / LRatio;
            end;
            LBitmapSurface.SetSize(round(W), round(H));
            //-----
            LDestRect := TrectF.Create(0, 0, W, H);
            LSrcRect := ALRectFitInto(LDestRect, TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height), aCropCenter, LRatio);
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0-(LSrcRect.Left*LRatio),
                                                                        0-(LSrcRect.top*LRatio)),
                                                         w + (LSrcRect.Left*LRatio) + ((LImage.size.Width-LSrcRect.right)*LRatio),
                                                         h + (LSrcRect.top*LRatio)  + ((LImage.size.Height-LSrcRect.bottom)*LRatio),
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

{****************************************************************************************************************************************}
function ALPlaceIntoAndCropImageV3(const aStream: TCustomMemoryStream; W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{****************************************************************************************************************************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV2(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{************************************************************************************************************************************************************************}
function ALLoadPlaceIntoAndCropResourceImageV2(const aResName: String; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*********************************************************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALPlaceIntoAndCropImageV3(LStream, W, H, aCropCenter);
  finally
    ALfreeandNil(LStream);
  end;
end;

{*****************************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropResourceImageV3(const aResName: String; W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{*************************************************************************************************************************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single; const aCropCenter: TPointF): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
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

{*********************************************************************************************************************************************************************}
function ALLoadPlaceIntoAndCropFileImageV2(const aFileName: String; W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
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

{******************************************************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single; const aCropCenter: TPointF): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{**************************************************************************************************************************************}
function  ALLoadPlaceIntoAndCropFileImageV3(const aFileName: String; W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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

{**********************************************************************************************************************************************************************************************************}
function ALFitIntoImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height));
          LSrcRect := TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height);
          LDestRect := LSrcRect.
                         FitInto(
                           TrectF.Create(0, 0, LDestSize.x, LDestSize.y));
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0,0),
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

{*********************************************************************************************************************************************************************}
function ALFitIntoImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LSrcRect := TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height);
          LDestRect := LSrcRect.
                         FitInto(
                           TrectF.Create(0, 0, W, H));
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0,0),
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

{**************************************************************************************************************************************************************************}
function ALFitIntoImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height));
            LSrcRect := TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height);
            LDestRect := LSrcRect.
                           FitInto(
                             TrectF.Create(0, 0, LDestSize.x, LDestSize.y));
            //-----
            LBitmapSurface.SetSize(ceil(LDestRect.width), ceil(LDestRect.height));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0,0),
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


{*************************************************************************************************************************************}
function ALFitIntoImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LSrcRect := TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height);
            LDestRect := LSrcRect.
                           FitInto(
                             TrectF.Create(0, 0, W, H));
            //-----
            LBitmapSurface.SetSize(ceil(LDestRect.width), ceil(LDestRect.height));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0,0),
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

{*********************************************************************************************************************************************************************}
function ALLoadFitIntoResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALFitIntoImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**************************************************************************************************************************************}
function  ALLoadFitIntoResourceImageV3(const aResName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{******************************************************************************************************************************************************************}
function ALLoadFitIntoFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
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

{***********************************************************************************************************************************}
function  ALLoadFitIntoFileImageV3(const aFileName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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
        Result.Canvas.DrawBitmap(LBitmap, // const ABitmap: TBitmap;
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

{**********************************************************************************************************************************************************************************************************}
function ALStretchImageV2(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height));
          LSrcRect := TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height);
          LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0,0),
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

{*********************************************************************************************************************************************************************}
function ALStretchImageV2(const aStream: TCustomMemoryStream; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
                                                          astream.Size,   // length: The number of bytes to hold from bytes. This value must not exceed the length of bytes.
                                                          False));        // flag: If YES, the returned object takes ownership of the bytes pointer and frees it on deallocation.
  try
    if LData.length > 0 then begin
      LImage := TUIImage.Wrap(TUIImage.alloc.initWithData(LData)); // Return Value: An initialized UIImage object, or nil if the method could not initialize the image from the specified data.
      if LImage <> nil then begin
        try
          //-----
          LSrcRect := TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height);
          LDestRect := TrectF.Create(0, 0, W, H);
          //-----
          LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
          if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
            try
              LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                  CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                  CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                  CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                     ALLowerLeftCGRect(TpointF.Create(0,0),
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

{**************************************************************************************************************************************************************************}
function ALStretchImageV3(const aStream: TCustomMemoryStream; const aGetDestSizeFunct: TALResizeImageGetDestSizeFunct): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LDestSize := aGetDestSizeFunct(TpointF.create(LImage.size.width, LImage.size.height));
            LSrcRect := TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height);
            LDestRect := TrectF.Create(0, 0, LDestSize.x, LDestSize.y);
            //-----
            LBitmapSurface.SetSize(ceil(LDestRect.width), ceil(LDestRect.height));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0,0),
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


{*************************************************************************************************************************************}
function ALStretchImageV3(const aStream: TCustomMemoryStream; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
            LSrcRect := TrectF.Create(0, 0, LImage.size.Width, LImage.size.Height);
            LDestRect := TrectF.Create(0, 0, W, H);
            //-----
            LBitmapSurface.SetSize(ceil(LDestRect.width), ceil(LDestRect.height));
            //-----
            LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
            if LColorSpace <> nil then begin             // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
              try
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(TpointF.Create(0,0),
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

{*********************************************************************************************************************************************************************}
function ALLoadStretchResourceImageV2(const aResName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
var LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, aResName, RT_RCDATA);
  try
    result := ALStretchImageV2(LStream, W, H);
  finally
    ALfreeandNil(LStream);
  end;
end;

{**************************************************************************************************************************************}
function  ALLoadStretchResourceImageV3(const aResName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{******************************************************************************************************************************************************************}
function ALLoadStretchFileImageV2(const aFileName: String; const W, H: single): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
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

{***********************************************************************************************************************************}
function  ALLoadStretchFileImageV3(const aFileName: String; const W, H: single): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{***********************************************************************************************************************************************************************************************************************}
function  ALLoadNormalizeOrientationImageV2(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
var LArray: TJavaArray<Byte>;
    LBitmap: Jbitmap;
begin
  LArray := TJavaArray<Byte>.Create(aStream.Size);
  try
    system.Move(aStream.Memory^, LArray.Data^, aStream.Size);
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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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

{***************************************************************************************************************************************************************************************}
function  ALLoadNormalizeOrientationImageV3(const aStream: TCustomMemoryStream; const aExifOrientationInfo: TalExifOrientationInfo): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

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
  LData := TNSData.Wrap(TNSData.alloc.initWithBytesNoCopy(aStream.Memory, // bytes: A buffer containing data for the new object. If flag is YES, bytes must point to a memory block allocated with malloc.
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
                LContext := CGBitmapContextCreate(LBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
                    CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
                    CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
                    CGContextConcatCTM(LContext, LMatrix);
                    if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                                                TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                                                TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                                                TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                                                                                                                            CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                                                                                                                            LImage.CGImage) // image The image to draw.
                    else CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
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

{************************************************************************************************************************************************************}
function  ALLoadNormalizeOrientationFileImageV2(const aFileName: String): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
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

{****************************************************************************************************************************}
function  ALLoadNormalizeOrientationFileImageV3(const aFileName: String): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
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

{***********************************************************************************************************************************************************************************************************************************************************************************}
function  ALNormalizeImageOrientationV2(const aBitmap: {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF}; const aExifOrientationInfo: TalExifOrientationInfo): {$IF defined(ANDROID)}Jbitmap{$ELSEIF defined(IOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};

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
    w, h: Single;
begin

  //-----
  result := aBitmap;
  w := CGImageGetWidth(aBitmap);
  h := CGImageGetHeight(aBitmap);
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
      LContext := CGBitmapContextCreate(nil, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
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
          CGContextSetShouldAntialias(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
          CGContextSetAllowsAntialiasing(LContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
          CGContextConcatCTM(LContext, LMatrix);
          if aExifOrientationInfo in [TalExifOrientationInfo.ROTATE_270, {UIImageOrientationLeft}
                                      TalExifOrientationInfo.TRANSPOSE, {UIImageOrientationLeftMirrored}
                                      TalExifOrientationInfo.ROTATE_90, {UIImageOrientationRight}
                                      TalExifOrientationInfo.TRANSVERSE{UIImageOrientationRightMirrored}] then CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                                                                                                                  CGRectMake(0, 0, h, w), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                                                                                                                  abitmap) // image The image to draw.
          else CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
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
function  AlDetectImageExtensionU(const aStream: Tstream): String;
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

{*****************************************************************}
function  AlDetectImageExtensionU(const aFileName: string): String;
var LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    result := AlDetectImageExtensionU(LFileStream);
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

end.
