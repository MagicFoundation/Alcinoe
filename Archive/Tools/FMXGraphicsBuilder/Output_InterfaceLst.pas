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
function ALLoadFromJBitmapToJBitmap(const ABitmap: JBitmap): JBitmap;
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

function ALLoadFromBitmapToBitmap(const ABitmap: TBitmap): TBitmap;
function ALLoadFromStreamToBitmap(const AStream: TStream): TBitmap;
function ALLoadFromResourceToBitmap(const AResName: String): TBitmap;
function ALLoadFromFileToBitmap(const AFileName: String): TBitmap;
//--
function ALLoadFromStreamToDrawable(const AStream: TStream): TALDrawable;
function ALLoadFromResourceToDrawable(const AResName: String): TALDrawable;
function ALLoadFromFileToDrawable(const AFileName: String): TALDrawable;
{$ENDREGION}
